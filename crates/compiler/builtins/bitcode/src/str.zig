const utils = @import("utils.zig");
const RocList = @import("list.zig").RocList;
const grapheme = @import("helpers/grapheme.zig");
const UpdateMode = utils.UpdateMode;
const std = @import("std");
const mem = std.mem;
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expectError = testing.expectError;
const expect = testing.expect;

const InPlace = enum(u8) {
    InPlace,
    Clone,
};

const MASK_ISIZE: isize = std.math.minInt(isize);
const MASK: usize = @bitCast(usize, MASK_ISIZE);
const SEAMLESS_SLICE_BIT: usize = MASK;

const SMALL_STR_MAX_LENGTH = SMALL_STRING_SIZE - 1;
const SMALL_STRING_SIZE = @sizeOf(RocStr);

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
    str_capacity: usize,

    pub const alignment = @alignOf(usize);

    pub inline fn empty() RocStr {
        return RocStr{
            .str_len = 0,
            .str_bytes = null,
            .str_capacity = MASK,
        };
    }

    // This clones the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn init(bytes_ptr: [*]const u8, length: usize) RocStr {
        var result = RocStr.allocate(length);
        @memcpy(result.asU8ptrMut(), bytes_ptr, length);

        return result;
    }

    // This requires that the list is non-null.
    // It also requires that start and count define a slice that does not go outside the bounds of the list.
    pub fn fromSubListUnsafe(list: RocList, start: usize, count: usize, update_mode: UpdateMode) RocStr {
        const start_byte = @ptrCast([*]u8, list.bytes) + start;
        if (list.isSeamlessSlice()) {
            return RocStr{
                .str_bytes = start_byte,
                .str_len = count | SEAMLESS_SLICE_BIT,
                .str_capacity = list.capacity_or_ref_ptr & (~SEAMLESS_SLICE_BIT),
            };
        } else if (start == 0 and (update_mode == .InPlace or list.isUnique())) {
            // Rare case, we can take over the original list.
            return RocStr{
                .str_bytes = start_byte,
                .str_len = count,
                .str_capacity = list.capacity_or_ref_ptr, // This is guaranteed to be a proper capacity.
            };
        } else {
            // Create seamless slice pointing to the list.
            return RocStr{
                .str_bytes = start_byte,
                .str_len = count | SEAMLESS_SLICE_BIT,
                .str_capacity = @ptrToInt(list.bytes) >> 1,
            };
        }
    }

    pub fn isSeamlessSlice(self: RocStr) bool {
        return !self.isSmallStr() and @bitCast(isize, self.str_len) < 0;
    }

    pub fn fromSlice(slice: []const u8) RocStr {
        return RocStr.init(slice.ptr, slice.len);
    }

    fn allocateBig(length: usize, capacity: usize) RocStr {
        const first_element = utils.allocateWithRefcount(capacity, @sizeOf(usize));

        return RocStr{
            .str_bytes = first_element,
            .str_len = length,
            .str_capacity = capacity,
        };
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet.
    // May have a larger capacity than the length.
    pub fn allocate(length: usize) RocStr {
        const element_width = 1;
        const result_is_big = length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            const capacity = utils.calculateCapacity(0, length, element_width);
            return RocStr.allocateBig(length, capacity);
        } else {
            var string = RocStr.empty();

            string.asU8ptrMut()[@sizeOf(RocStr) - 1] = @intCast(u8, length) | 0b1000_0000;

            return string;
        }
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet.
    // Will have the exact same capacity as length if it is not a small string.
    pub fn allocateExact(length: usize) RocStr {
        const result_is_big = length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            return RocStr.allocateBig(length, length);
        } else {
            var string = RocStr.empty();

            string.asU8ptrMut()[@sizeOf(RocStr) - 1] = @intCast(u8, length) | 0b1000_0000;

            return string;
        }
    }

    // This returns all ones if the list is a seamless slice.
    // Otherwise, it returns all zeros.
    // This is done without branching for optimization purposes.
    pub fn seamlessSliceMask(self: RocStr) usize {
        return @bitCast(usize, @bitCast(isize, self.str_len) >> (@bitSizeOf(isize) - 1));
    }

    // returns a pointer to just after the refcount.
    // It is just after the refcount as an optimization for other shared code paths.
    // For regular list, it just returns their bytes pointer.
    // For seamless slices, it returns the pointer stored in capacity_or_ref_ptr.
    // This does not return a valid value if the input is a small string.
    pub fn getRefcountPtr(self: RocStr) ?[*]u8 {
        const str_ref_ptr = @ptrToInt(self.str_bytes);
        const slice_ref_ptr = self.str_capacity << 1;
        const slice_mask = self.seamlessSliceMask();
        const ref_ptr = (str_ref_ptr & ~slice_mask) | (slice_ref_ptr & slice_mask);
        return @intToPtr(?[*]u8, ref_ptr);
    }

    pub fn incref(self: RocStr, n: usize) void {
        if (!self.isSmallStr()) {
            const ref_ptr = self.getRefcountPtr();
            if (ref_ptr != null) {
                const isizes: [*]isize = @ptrCast([*]isize, @alignCast(@alignOf(isize), ref_ptr));
                utils.increfC(@ptrCast(*isize, isizes - 1), @intCast(isize, n));
            }
        }
    }

    pub fn decref(self: RocStr) void {
        if (!self.isSmallStr()) {
            utils.decref(self.getRefcountPtr(), self.str_capacity, RocStr.alignment);
        }
    }

    pub fn eq(self: RocStr, other: RocStr) bool {
        // If they are byte-for-byte equal, they're definitely equal!
        if (self.str_bytes == other.str_bytes and self.str_len == other.str_len and self.str_capacity == other.str_capacity) {
            return true;
        }

        const self_len = self.len();
        const other_len = other.len();

        // If their lengths are different, they're definitely unequal.
        if (self_len != other_len) {
            return false;
        }

        // Now we have to look at the string contents
        const self_bytes = self.asU8ptr();
        const other_bytes = other.asU8ptr();
        // TODO: we can make an optimization like memcmp does in glibc.
        // We can check the min shared alignment 1, 2, 4, or 8.
        // Then do a copy at that alignment before falling back on one byte at a time.
        // Currently we have to be unaligned because slices can be at any alignment.
        var b: usize = 0;
        while (b < self_len) : (b += 1) {
            if (self_bytes[b] != other_bytes[b]) {
                return false;
            }
        }

        return true;
    }

    pub fn clone(str: RocStr) RocStr {
        if (str.isSmallStr()) {
            // just return the bytes
            return str;
        } else {
            var new_str = RocStr.allocateBig(str.str_len, str.str_len);

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
        const old_capacity = self.getCapacity();

        if (self.isSmallStr() or self.isSeamlessSlice() or !self.isUnique()) {
            return self.reallocateFresh(new_length);
        }

        if (self.str_bytes) |source_ptr| {
            if (old_capacity > new_length) {
                var output = self;
                output.setLen(new_length);
                return output;
            }
            const new_capacity = utils.calculateCapacity(old_capacity, new_length, element_width);
            const new_source = utils.unsafeReallocate(
                source_ptr,
                RocStr.alignment,
                old_capacity,
                new_capacity,
                element_width,
            );

            return RocStr{ .str_bytes = new_source, .str_len = new_length, .str_capacity = new_capacity };
        }
        return self.reallocateFresh(new_length);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    fn reallocateFresh(
        self: RocStr,
        new_length: usize,
    ) RocStr {
        const old_length = self.len();
        const delta_length = new_length - old_length;

        var result = RocStr.allocate(new_length);

        // transfer the memory

        const source_ptr = self.asU8ptr();
        const dest_ptr = result.asU8ptrMut();

        @memcpy(dest_ptr, source_ptr, old_length);
        @memset(dest_ptr + old_length, 0, delta_length);

        self.decref();

        return result;
    }

    pub fn isSmallStr(self: RocStr) bool {
        return @bitCast(isize, self.str_capacity) < 0;
    }

    test "isSmallStr: returns true for empty string" {
        try expect(isSmallStr(RocStr.empty()));
    }

    fn asArray(self: RocStr) [@sizeOf(RocStr)]u8 {
        const as_ptr = @ptrCast([*]const u8, &self);
        const slice = as_ptr[0..@sizeOf(RocStr)];

        return slice.*;
    }

    pub fn len(self: RocStr) usize {
        if (self.isSmallStr()) {
            return self.asArray()[@sizeOf(RocStr) - 1] ^ 0b1000_0000;
        } else {
            return self.str_len & (~SEAMLESS_SLICE_BIT);
        }
    }

    pub fn setLen(self: *RocStr, length: usize) void {
        if (self.isSmallStr()) {
            self.asU8ptrMut()[@sizeOf(RocStr) - 1] = @intCast(u8, length) | 0b1000_0000;
        } else {
            self.str_len = length | (SEAMLESS_SLICE_BIT & self.str_len);
        }
    }

    pub fn getCapacity(self: RocStr) usize {
        if (self.isSmallStr()) {
            return SMALL_STR_MAX_LENGTH;
        } else if (self.isSeamlessSlice()) {
            return self.str_len & (~SEAMLESS_SLICE_BIT);
        } else {
            return self.str_capacity;
        }
    }

    // This does a small string check, but no bounds checking whatsoever!
    pub fn getUnchecked(self: RocStr, index: usize) u8 {
        if (self.isSmallStr()) {
            return self.asArray()[index];
        } else {
            const bytes = self.str_bytes orelse unreachable;

            return bytes[index];
        }
    }

    pub fn isEmpty(self: RocStr) bool {
        return self.len() == 0;
    }

    // If a string happens to be null-terminated already, then we can pass its
    // bytes directly to functions (e.g. for opening files) that require
    // null-terminated strings. Otherwise, we need to allocate and copy a new
    // null-terminated string, which has a much higher performance cost!
    fn isNullTerminated(self: RocStr) bool {
        const length = self.len();
        const longest_small_str = @sizeOf(RocStr) - 1;

        // NOTE: We want to compare length here, *NOT* check for isSmallStr!
        // This is because we explicitly want the empty string to be handled in
        // this branch, even though the empty string is not a small string.
        //
        // (The other branch dereferences the bytes pointer, which is not safe
        // to do for the empty string.)
        if (length <= longest_small_str) {
            // If we're a small string, then usually the next byte after the
            // end of the string will be zero. (Small strings set all their
            // unused bytes to 0, so that comparison for equality can be fast.)
            //
            // However, empty strings are *not* null terminated, so if this is
            // empty, it should return false.
            //
            // Also, if we are exactly a maximum-length small string,
            // then the next byte is off the end of the struct;
            // in that case, we are also not null-terminated!
            return length != 0 and length != longest_small_str;
        } else if (self.isSeamlessSlice()) {
            // Seamless slices can not use the character past the end even if it is null.
            return false;
        } else {
            // This is a big string, and it's not empty, so we can safely
            // dereference the pointer.
            const ptr: [*]usize = @ptrCast([*]usize, @alignCast(@alignOf(usize), self.str_bytes));
            const capacity_or_refcount: isize = (ptr - 1)[0];

            // If capacity_or_refcount is positive, then it's a capacity value.
            //
            // If we have excess capacity, then we can safely read the next
            // byte after the end of the string. Maybe it happens to be zero!
            if (capacity_or_refcount > @intCast(isize, length)) {
                return self.str_bytes[length] == 0;
            } else {
                // This string was refcounted or immortal; we can't safely read
                // the next byte, so assume the string is not null-terminated.
                return false;
            }
        }
    }

    pub fn isUnique(self: RocStr) bool {
        // small strings can be copied
        if (self.isSmallStr()) {
            return true;
        }

        // otherwise, check if the refcount is one
        return @call(.{ .modifier = always_inline }, RocStr.isRefcountOne, .{self});
    }

    fn isRefcountOne(self: RocStr) bool {
        return self.refcountMachine() == utils.REFCOUNT_ONE;
    }

    fn refcountMachine(self: RocStr) usize {
        if ((self.getCapacity() == 0 and !self.isSeamlessSlice()) or self.isSmallStr()) {
            return utils.REFCOUNT_ONE;
        }

        const ptr: [*]usize = @ptrCast([*]usize, @alignCast(@alignOf(usize), self.str_bytes));
        return (ptr - 1)[0];
    }

    fn refcountHuman(self: RocStr) usize {
        return self.refcountMachine() - utils.REFCOUNT_ONE + 1;
    }

    pub fn asSlice(self: *const RocStr) []const u8 {
        return self.asU8ptr()[0..self.len()];
    }

    pub fn asSliceWithCapacity(self: *const RocStr) []const u8 {
        return self.asU8ptr()[0..self.getCapacity()];
    }

    pub fn asSliceWithCapacityMut(self: *RocStr) []u8 {
        return self.asU8ptrMut()[0..self.getCapacity()];
    }

    pub fn asU8ptr(self: *const RocStr) [*]const u8 {
        if (self.isSmallStr()) {
            return @ptrCast([*]const u8, self);
        } else {
            return @ptrCast([*]const u8, self.str_bytes);
        }
    }

    pub fn asU8ptrMut(self: *RocStr) [*]u8 {
        if (self.isSmallStr()) {
            return @ptrCast([*]u8, self);
        } else {
            return @ptrCast([*]u8, self.str_bytes);
        }
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

    test "RocStr.eq: small, equal" {
        const str1_len = 3;
        var str1: [str1_len]u8 = "abc".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        try expect(roc_str1.eq(roc_str2));

        roc_str1.decref();
        roc_str2.decref();
    }

    test "RocStr.eq: small, not equal, different length" {
        const str1_len = 4;
        var str1: [str1_len]u8 = "abcd".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: small, not equal, same length" {
        const str1_len = 3;
        var str1: [str1_len]u8 = "acb".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, equal" {
        const content = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content, content.len);
        const roc_str2 = RocStr.init(content, content.len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, different lengths, unequal" {
        const content1 = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content1, content1.len);
        const content2 = "012345678901234567890";
        const roc_str2 = RocStr.init(content2, content2.len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, different content, unequal" {
        const content1 = "012345678901234567890123456789!!";
        const roc_str1 = RocStr.init(content1, content1.len);
        const content2 = "012345678901234567890123456789--";
        const roc_str2 = RocStr.init(content2, content2.len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, garbage after end, equal" {
        const content = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content, content.len);
        const roc_str2 = RocStr.init(content, content.len);
        try expect(roc_str1.str_bytes != roc_str2.str_bytes);

        // Insert garbage after the end of each string
        roc_str1.str_bytes.?[30] = '!';
        roc_str1.str_bytes.?[31] = '!';
        roc_str2.str_bytes.?[30] = '-';
        roc_str2.str_bytes.?[31] = '-';

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(roc_str1.eq(roc_str2));
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

// Str.toScalars
pub fn strToScalarsC(str: RocStr) callconv(.C) RocList {
    return @call(.{ .modifier = always_inline }, strToScalars, .{str});
}

fn strToScalars(string: RocStr) callconv(.C) RocList {
    const str_len = string.len();

    if (str_len == 0) {
        return RocList.empty();
    }

    var capacity = str_len;

    if (!string.isSmallStr()) {
        capacity = string.getCapacity();
    }

    // For purposes of preallocation, assume the number of code points is the same
    // as the number of bytes. This might be longer than necessary, but definitely
    // should not require a second allocation.
    var answer = RocList.allocate(@alignOf(u32), capacity, @sizeOf(u32));

    // `orelse unreachable` is fine here, because we already did an early
    // return to verify the string was nonempty.
    var answer_elems = answer.elements(u32) orelse unreachable;
    var src_index: usize = 0;
    var answer_index: usize = 0;

    while (src_index < str_len) {
        src_index += writeNextScalar(string, src_index, answer_elems, answer_index);
        answer_index += 1;
    }

    answer.length = answer_index;

    return answer;
}

// Given a non-empty RocStr, and a src_index byte index into that string,
// and a destination [*]u32, and an index into that destination,
// Parses the next scalar value out of the string (at the given byte index),
// writes it into the destination, and returns the number of bytes parsed.
inline fn writeNextScalar(non_empty_string: RocStr, src_index: usize, dest: [*]u32, dest_index: usize) usize {
    const utf8_byte = non_empty_string.getUnchecked(src_index);

    // How UTF-8 bytes work:
    // https://docs.teradata.com/r/Teradata-Database-International-Character-Set-Support/June-2017/Client-Character-Set-Options/UTF8-Client-Character-Set-Support/UTF8-Multibyte-Sequences
    if (utf8_byte <= 127) {
        // It's an ASCII character. Copy it over directly.
        dest[dest_index] = @intCast(u32, utf8_byte);

        return 1;
    } else if (utf8_byte >> 5 == 0b0000_0110) {
        // Its three high order bits are 110, so this is a two-byte sequence.

        // Example:
        //     utf-8:   1100 1111   1011 0001
        //     code pt: 0000 0011   1111 0001 (decimal: 1009)

        // Discard the first byte's high order bits of 110.
        var code_pt = @intCast(u32, utf8_byte & 0b0001_1111);

        // Discard the second byte's high order bits of 10.
        code_pt <<= 6;
        code_pt |= non_empty_string.getUnchecked(src_index + 1) & 0b0011_1111;

        dest[dest_index] = code_pt;

        return 2;
    } else if (utf8_byte >> 4 == 0b0000_1110) {
        // Its four high order bits are 1110, so this is a three-byte sequence.

        // Discard the first byte's high order bits of 1110.
        var code_pt = @intCast(u32, utf8_byte & 0b0000_1111);

        // Discard the second byte's high order bits of 10.
        code_pt <<= 6;
        code_pt |= non_empty_string.getUnchecked(src_index + 1) & 0b0011_1111;

        // Discard the third byte's high order bits of 10 (same as second byte).
        code_pt <<= 6;
        code_pt |= non_empty_string.getUnchecked(src_index + 2) & 0b0011_1111;

        dest[dest_index] = code_pt;

        return 3;
    } else {
        // This must be a four-byte sequence, so the five high order bits should be 11110.

        // Discard the first byte's high order bits of 11110.
        var code_pt = @intCast(u32, utf8_byte & 0b0000_0111);

        // Discard the second byte's high order bits of 10.
        code_pt <<= 6;
        code_pt |= non_empty_string.getUnchecked(src_index + 1) & 0b0011_1111;

        // Discard the third byte's high order bits of 10 (same as second byte).
        code_pt <<= 6;
        code_pt |= non_empty_string.getUnchecked(src_index + 2) & 0b0011_1111;

        // Discard the fourth byte's high order bits of 10 (same as second and third).
        code_pt <<= 6;
        code_pt |= non_empty_string.getUnchecked(src_index + 3) & 0b0011_1111;

        dest[dest_index] = code_pt;

        return 4;
    }
}

test "strToScalars: empty string" {
    const str = RocStr.fromSlice("");
    defer RocStr.decref(str);

    const expected = RocList.empty();
    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

test "strToScalars: One ASCII char" {
    const str = RocStr.fromSlice("R");
    defer RocStr.decref(str);

    const expected_array = [_]u32{82};
    const expected = RocList.fromSlice(u32, expected_array[0..expected_array.len]);
    defer expected.decref(@sizeOf(u32));

    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

test "strToScalars: Multiple ASCII chars" {
    const str = RocStr.fromSlice("Roc!");
    defer RocStr.decref(str);

    const expected_array = [_]u32{ 82, 111, 99, 33 };
    const expected = RocList.fromSlice(u32, expected_array[0..expected_array.len]);
    defer expected.decref(@sizeOf(u32));

    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

test "strToScalars: One 2-byte UTF-8 character" {
    const str = RocStr.fromSlice("é");
    defer RocStr.decref(str);

    const expected_array = [_]u32{233};
    const expected = RocList.fromSlice(u32, expected_array[0..expected_array.len]);
    defer expected.decref(@sizeOf(u32));

    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

test "strToScalars: Multiple 2-byte UTF-8 characters" {
    const str = RocStr.fromSlice("Cäfés");
    defer RocStr.decref(str);

    const expected_array = [_]u32{ 67, 228, 102, 233, 115 };
    const expected = RocList.fromSlice(u32, expected_array[0..expected_array.len]);
    defer expected.decref(@sizeOf(u32));

    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

test "strToScalars: One 3-byte UTF-8 character" {
    const str = RocStr.fromSlice("鹏");
    defer RocStr.decref(str);

    const expected_array = [_]u32{40527};
    const expected = RocList.fromSlice(u32, expected_array[0..expected_array.len]);
    defer expected.decref(@sizeOf(u32));

    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

test "strToScalars: Multiple 3-byte UTF-8 characters" {
    const str = RocStr.fromSlice("鹏很有趣");
    defer RocStr.decref(str);

    const expected_array = [_]u32{ 40527, 24456, 26377, 36259 };
    const expected = RocList.fromSlice(u32, expected_array[0..expected_array.len]);
    defer expected.decref(@sizeOf(u32));

    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

test "strToScalars: One 4-byte UTF-8 character" {
    // from https://design215.com/toolbox/utf8-4byte-characters.php
    const str = RocStr.fromSlice("𒀀");
    defer RocStr.decref(str);

    const expected_array = [_]u32{73728};
    const expected = RocList.fromSlice(u32, expected_array[0..expected_array.len]);
    defer expected.decref(@sizeOf(u32));

    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

test "strToScalars: Multiple 4-byte UTF-8 characters" {
    // from https://design215.com/toolbox/utf8-4byte-characters.php
    const str = RocStr.fromSlice("𒀀𒀁");
    defer RocStr.decref(str);

    const expected_array = [_]u32{ 73728, 73729 };
    const expected = RocList.fromSlice(u32, expected_array[0..expected_array.len]);
    defer expected.decref(@sizeOf(u32));

    const actual = strToScalars(str);
    defer actual.decref(@sizeOf(u32));

    try expect(RocList.eql(actual, expected));
}

// Str.fromInt
pub fn exportFromInt(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(int: T) callconv(.C) RocStr {
            return @call(.{ .modifier = always_inline }, strFromIntHelp, .{ T, int });
        }
    }.func;

    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

fn strFromIntHelp(comptime T: type, int: T) RocStr {
    // determine maximum size for this T
    const size = comptime blk: {
        // the string representation of the minimum i128 value uses at most 40 characters
        var buf: [40]u8 = undefined;
        var resultMin = std.fmt.bufPrint(&buf, "{}", .{std.math.minInt(T)}) catch unreachable;
        var resultMax = std.fmt.bufPrint(&buf, "{}", .{std.math.maxInt(T)}) catch unreachable;
        var result = if (resultMin.len > resultMax.len) resultMin.len else resultMax.len;
        break :blk result;
    };

    var buf: [size]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{}", .{int}) catch unreachable;

    return RocStr.init(&buf, result.len);
}

// Str.fromFloat
pub fn exportFromFloat(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(float: T) callconv(.C) RocStr {
            return @call(.{ .modifier = always_inline }, strFromFloatHelp, .{ T, float });
        }
    }.func;

    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

fn strFromFloatHelp(comptime T: type, float: T) RocStr {
    var buf: [400]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{d}", .{float}) catch unreachable;

    return RocStr.init(&buf, result.len);
}

// Str.split
pub fn strSplit(string: RocStr, delimiter: RocStr) callconv(.C) RocList {
    const segment_count = countSegments(string, delimiter);
    const list = RocList.allocate(@alignOf(RocStr), segment_count, @sizeOf(RocStr));

    if (list.bytes) |bytes| {
        const strings = @ptrCast([*]RocStr, @alignCast(@alignOf(RocStr), bytes));
        strSplitHelp(strings, string, delimiter);
    }

    return list;
}

fn initFromSmallStr(slice_bytes: [*]u8, len: usize, _: usize) RocStr {
    return RocStr.init(slice_bytes, len);
}

// The ref_ptr must already be shifted to be ready for storing in a seamless slice.
fn initFromBigStr(slice_bytes: [*]u8, len: usize, ref_ptr: usize) RocStr {
    // Here we can make seamless slices instead of copying to a new small str.
    return RocStr{
        .str_bytes = slice_bytes,
        .str_len = len | SEAMLESS_SLICE_BIT,
        .str_capacity = ref_ptr,
    };
}

// TODO: relpace this with @qualCast or @constCast in future version of zig
fn constCast(ptr: [*]const u8) [*]u8 {
    var result: [*]u8 = undefined;
    @memcpy(@ptrCast([*]u8, &result), @ptrCast([*]const u8, &ptr), @sizeOf([*]u8));
    return result;
}

fn strSplitHelp(array: [*]RocStr, string: RocStr, delimiter: RocStr) void {
    var ret_array_index: usize = 0;
    var slice_start_index: usize = 0;
    var str_index: usize = 0;

    const str_bytes = string.asU8ptr();
    const str_len = string.len();
    const ref_ptr = @ptrToInt(string.getRefcountPtr()) >> 1;
    const init_fn = if (string.isSmallStr())
        initFromSmallStr
    else
        initFromBigStr;

    const delimiter_bytes_ptrs = delimiter.asU8ptr();
    const delimiter_len = delimiter.len();

    if (str_len >= delimiter_len and delimiter_len > 0) {
        const end_index: usize = str_len - delimiter_len + 1;
        while (str_index <= end_index) {
            var delimiter_index: usize = 0;
            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                var delimiterChar = delimiter_bytes_ptrs[delimiter_index];

                if (str_index + delimiter_index >= str_len) {
                    matches_delimiter = false;
                    break;
                }

                var strChar = str_bytes[str_index + delimiter_index];

                if (delimiterChar != strChar) {
                    matches_delimiter = false;
                    break;
                }

                delimiter_index += 1;
            }

            if (matches_delimiter) {
                const segment_len: usize = str_index - slice_start_index;

                array[ret_array_index] = init_fn(constCast(str_bytes) + slice_start_index, segment_len, ref_ptr);
                slice_start_index = str_index + delimiter_len;
                ret_array_index += 1;
                str_index += delimiter_len;
            } else {
                str_index += 1;
            }
        }
    }

    array[ret_array_index] = init_fn(constCast(str_bytes) + slice_start_index, str_len - slice_start_index, ref_ptr);

    if (!string.isSmallStr()) {
        // Correct refcount for all of the splits made.
        string.incref(ret_array_index + 1);
    }
}

test "strSplitHelp: empty delimiter" {
    // Str.split "abc" "" == ["abc"]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitHelp(array_ptr, str, delimiter);

    var expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref();
        }

        for (expected) |roc_str| {
            roc_str.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
}

test "strSplitHelp: no delimiter" {
    // Str.split "abc" "!" == ["abc"]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitHelp(array_ptr, str, delimiter);

    var expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref();
        }

        for (expected) |roc_str| {
            roc_str.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
}

test "strSplitHelp: empty start" {
    const str_arr = "/a";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "/";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const array_len: usize = 2;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    strSplitHelp(array_ptr, str, delimiter);

    const one = RocStr.init("a", 1);

    var expected = [2]RocStr{
        RocStr.empty(), one,
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref();
        }

        for (expected) |rocStr| {
            rocStr.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: empty end" {
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

    strSplitHelp(array_ptr, str, delimiter);

    const one = RocStr.init("1", 1);
    const two = RocStr.init("2", 1);

    var expected = [3]RocStr{
        one, two, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref();
        }

        for (expected) |rocStr| {
            rocStr.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "strSplitHelp: string equals delimiter" {
    const str_delimiter_arr = "/";
    const str_delimiter = RocStr.init(str_delimiter_arr, str_delimiter_arr.len);

    const array_len: usize = 2;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    strSplitHelp(array_ptr, str_delimiter, str_delimiter);

    var expected = [2]RocStr{ RocStr.empty(), RocStr.empty() };

    defer {
        for (array) |rocStr| {
            rocStr.decref();
        }

        for (expected) |rocStr| {
            rocStr.decref();
        }

        str_delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: delimiter on sides" {
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
    strSplitHelp(array_ptr, str, delimiter);

    const ghi_arr = "ghi";
    const ghi = RocStr.init(ghi_arr, ghi_arr.len);

    var expected = [3]RocStr{
        RocStr.empty(), ghi, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref();
        }

        for (expected) |rocStr| {
            rocStr.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "strSplitHelp: three pieces" {
    // Str.split "a!b!c" "!" == ["a", "b", "c"]
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const array_len: usize = 3;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitHelp(array_ptr, str, delimiter);

    const a = RocStr.init("a", 1);
    const b = RocStr.init("b", 1);
    const c = RocStr.init("c", 1);

    var expected_array = [array_len]RocStr{
        a, b, c,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref();
        }

        for (expected_array) |roc_str| {
            roc_str.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(expected_array.len, array.len);
    try expect(array[0].eq(expected_array[0]));
    try expect(array[1].eq(expected_array[1]));
    try expect(array[2].eq(expected_array[2]));
}

test "strSplitHelp: overlapping delimiter 1" {
    // Str.split "aaa" "aa" == ["", "a"]
    const str_arr = "aaa";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "aa";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [2]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitHelp(array_ptr, str, delimiter);

    var expected = [2]RocStr{
        RocStr.empty(),
        RocStr.init("a", 1),
    };

    // strings are all small so we ignore freeing the memory

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: overlapping delimiter 2" {
    // Str.split "aaa" "aa" == ["", "a"]
    const str_arr = "aaaa";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "aa";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [3]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitHelp(array_ptr, str, delimiter);

    var expected = [3]RocStr{
        RocStr.empty(),
        RocStr.empty(),
        RocStr.empty(),
    };

    // strings are all small so we ignore freeing the memory

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
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

    if (str_len >= delimiter_len and delimiter_len > 0) {
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
                str_index += delimiter_len;
            } else {
                str_index += 1;
            }
        }
    }

    return count;
}

test "countSegments: long delimiter" {
    // Str.split "str" "delimiter" == ["str"]
    // 1 segment
    const str_arr = "str";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "delimiter";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.decref();
        delimiter.decref();
    }

    const segments_count = countSegments(str, delimiter);
    try expectEqual(segments_count, 1);
}

test "countSegments: delimiter at start" {
    // Str.split "hello there" "hello" == ["", " there"]
    // 2 segments
    const str_arr = "hello there";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "hello";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.decref();
        delimiter.decref();
    }

    const segments_count = countSegments(str, delimiter);

    try expectEqual(segments_count, 2);
}

test "countSegments: delimiter interspered" {
    // Str.split "a!b!c" "!" == ["a", "b", "c"]
    // 3 segments
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.decref();
        delimiter.decref();
    }

    const segments_count = countSegments(str, delimiter);

    try expectEqual(segments_count, 3);
}

test "countSegments: string equals delimiter" {
    // Str.split "/" "/" == ["", ""]
    // 2 segments
    const str_delimiter_arr = "/";
    const str_delimiter = RocStr.init(str_delimiter_arr, str_delimiter_arr.len);

    defer {
        str_delimiter.decref();
    }

    const segments_count = countSegments(str_delimiter, str_delimiter);

    try expectEqual(segments_count, 2);
}

test "countSegments: overlapping delimiter 1" {
    // Str.split "aaa" "aa" == ["", "a"]
    const segments_count = countSegments(RocStr.init("aaa", 3), RocStr.init("aa", 2));

    try expectEqual(segments_count, 2);
}

test "countSegments: overlapping delimiter 2" {
    // Str.split "aaa" "aa" == ["", "a"]
    const segments_count = countSegments(RocStr.init("aaaa", 4), RocStr.init("aa", 2));

    try expectEqual(segments_count, 3);
}

// Str.countGraphemeClusters
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

// Str.graphemes
pub fn strGraphemes(roc_str: RocStr) callconv(.C) RocList {
    var break_state: ?grapheme.BoundClass = null;
    var opt_last_codepoint: ?u21 = null;
    var index: usize = 0;
    var last_codepoint_len: u8 = 0;

    const ref_ptr = @ptrToInt(roc_str.getRefcountPtr()) >> 1;
    const init_fn = if (roc_str.isSmallStr())
        initFromSmallStr
    else
        initFromBigStr;

    var result = RocList.allocate(@alignOf(RocStr), countGraphemeClusters(roc_str), @sizeOf(RocStr));
    const graphemes = result.elements(RocStr) orelse return result;
    var slice = roc_str.asSlice();
    var iter = (unicode.Utf8View.init(slice) catch unreachable).iterator();

    while (iter.nextCodepoint()) |cur_codepoint| {
        const cur_codepoint_len = unicode.utf8CodepointSequenceLength(cur_codepoint) catch unreachable;
        if (opt_last_codepoint) |last_codepoint| {
            var did_break = grapheme.isGraphemeBreak(last_codepoint, cur_codepoint, &break_state);
            if (did_break) {
                graphemes[index] = init_fn(constCast(slice.ptr), last_codepoint_len, ref_ptr);
                slice = slice[last_codepoint_len..];
                index += 1;
                break_state = null;
                last_codepoint_len = 0;
            }
        }
        last_codepoint_len += cur_codepoint_len;
        opt_last_codepoint = cur_codepoint;
    }
    // Append last grapheme
    graphemes[index] = init_fn(constCast(slice.ptr), slice.len, ref_ptr);

    if (!roc_str.isSmallStr()) {
        // Correct refcount for all of the splits made.
        roc_str.incref(index + 1);
    }
    return result;
}

// these test both countGraphemeClusters() and strGraphemes()
fn graphemesTest(input: []const u8, expected: []const []const u8) !void {
    const rocstr = RocStr.fromSlice(input);
    defer rocstr.decref();
    const count = countGraphemeClusters(rocstr);
    try expectEqual(expected.len, count);

    const graphemes = strGraphemes(rocstr);
    defer graphemes.decref(@sizeOf(u8));
    if (input.len == 0) return; // empty string
    const elems = graphemes.elements(RocStr) orelse unreachable;
    for (expected) |g, i| {
        try std.testing.expectEqualStrings(g, elems[i].asSlice());
    }
}

test "graphemes: empty string" {
    try graphemesTest("", &.{});
}

test "graphemes: ascii characters" {
    try graphemesTest("abcd", &.{ "a", "b", "c", "d" });
}

test "graphemes: utf8 characters" {
    try graphemesTest("ãxā", &.{ "ã", "x", "ā" });
}

test "graphemes: emojis" {
    try graphemesTest("🤔🤔🤔", &.{ "🤔", "🤔", "🤔" });
}

test "graphemes: emojis and ut8 characters" {
    try graphemesTest("🤔å🤔¥🤔ç", &.{ "🤔", "å", "🤔", "¥", "🤔", "ç" });
}

test "graphemes: emojis, ut8, and ascii characters" {
    try graphemesTest("6🤔å🤔e¥🤔çpp", &.{ "6", "🤔", "å", "🤔", "e", "¥", "🤔", "ç", "p", "p" });
}

pub fn countUtf8Bytes(string: RocStr) callconv(.C) usize {
    return string.len();
}

pub fn getCapacity(string: RocStr) callconv(.C) usize {
    return string.getCapacity();
}

pub fn substringUnsafe(string: RocStr, start: usize, length: usize) callconv(.C) RocStr {
    const slice = string.asSlice()[start .. start + length];
    return RocStr.fromSlice(slice);
}

pub fn getUnsafe(string: RocStr, index: usize) callconv(.C) u8 {
    return string.getUnchecked(index);
}

test "substringUnsafe: start" {
    const str = RocStr.fromSlice("abcdef");
    defer str.decref();

    const expected = RocStr.fromSlice("abc");
    defer expected.decref();

    const actual = substringUnsafe(str, 0, 3);

    try expect(RocStr.eq(actual, expected));
}

test "substringUnsafe: middle" {
    const str = RocStr.fromSlice("abcdef");
    defer str.decref();

    const expected = RocStr.fromSlice("bcd");
    defer expected.decref();

    const actual = substringUnsafe(str, 1, 3);

    try expect(RocStr.eq(actual, expected));
}

test "substringUnsafe: end" {
    const str = RocStr.fromSlice("a string so long it is heap-allocated");
    defer str.decref();

    const expected = RocStr.fromSlice("heap-allocated");
    defer expected.decref();

    const actual = substringUnsafe(str, 23, 37 - 23);

    try expect(RocStr.eq(actual, expected));
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

// Str.repeat
pub fn repeat(string: RocStr, count: usize) callconv(.C) RocStr {
    const bytes_len = string.len();
    const bytes_ptr = string.asU8ptr();

    var ret_string = RocStr.allocate(count * bytes_len);
    var ret_string_ptr = ret_string.asU8ptrMut();

    var i: usize = 0;
    while (i < count) : (i += 1) {
        @memcpy(ret_string_ptr + (i * bytes_len), bytes_ptr, bytes_len);
    }

    return ret_string;
}

// Str.startsWithScalar
pub fn startsWithScalar(string: RocStr, prefix: u32) callconv(.C) bool {
    const str_len = string.len();

    if (str_len == 0) {
        return false;
    }

    // Write this (non-empty) string's first scalar into `first_scalar`
    var first_scalar: [1]u32 = undefined;

    _ = writeNextScalar(string, 0, &first_scalar, 0);

    // Return whether `first_scalar` equals `prefix`
    return @ptrCast(*u32, &first_scalar).* == prefix;
}

test "startsWithScalar: empty string" {
    const whole = RocStr.empty();
    const prefix: u32 = 'x';
    try expect(!startsWithScalar(whole, prefix));
}

test "startsWithScalar: ascii char" {
    const whole = RocStr.fromSlice("foobar");
    const prefix: u32 = 'f';
    try expect(startsWithScalar(whole, prefix));
}

test "startsWithScalar: emoji" {
    const yes = RocStr.fromSlice("💖foobar");
    const no = RocStr.fromSlice("foobar");
    const prefix: u32 = '💖';

    try expect(startsWithScalar(yes, prefix));
    try expect(!startsWithScalar(no, prefix));
}

test "startsWith: foo starts with fo" {
    const foo = RocStr.fromSlice("foo");
    const fo = RocStr.fromSlice("fo");
    try expect(startsWith(foo, fo));
}

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    const str = RocStr.fromSlice("123456789123456789");
    defer str.decref();
    try expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    const str = RocStr.fromSlice("12345678912345678910");
    defer str.decref();
    const prefix = RocStr.fromSlice("123456789123456789");
    defer prefix.decref();

    try expect(startsWith(str, prefix));
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
    defer foo.decref();
    defer oo.decref();

    try expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    const str = RocStr.init("123456789123456789", 18);
    defer str.decref();
    try expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    const str = RocStr.init("12345678912345678910", 20);
    const suffix = RocStr.init("345678912345678910", 18);
    defer str.decref();
    defer suffix.decref();

    try expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    const str = RocStr.init("hello world", 11);
    const suffix = RocStr.init("world", 5);
    defer str.decref();
    defer suffix.decref();

    try expect(endsWith(str, suffix));
}

// Str.concat
pub fn strConcatC(arg1: RocStr, arg2: RocStr) callconv(.C) RocStr {
    return @call(.{ .modifier = always_inline }, strConcat, .{ arg1, arg2 });
}

fn strConcat(arg1: RocStr, arg2: RocStr) RocStr {
    // NOTE: we don't special-case the first argument being empty. That is because it is owned and
    // may have sufficient capacity to store the rest of the list.
    if (arg2.isEmpty()) {
        // the first argument is owned, so we can return it without cloning
        return arg1;
    } else {
        const combined_length = arg1.len() + arg2.len();

        var result = arg1.reallocate(combined_length);
        @memcpy(result.asU8ptrMut() + arg1.len(), arg2.asU8ptr(), arg2.len());

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
        roc_str1.decref();
        roc_str2.decref();
        roc_str3.decref();
    }

    const result = strConcat(roc_str1, roc_str2);

    defer result.decref();

    try expect(roc_str3.eq(result));
}

pub const RocListStr = extern struct {
    list_elements: ?[*]RocStr,
    list_length: usize,
    list_capacity_or_ref_ptr: usize,
};

// Str.joinWith
pub fn strJoinWithC(list: RocList, separator: RocStr) callconv(.C) RocStr {
    const roc_list_str = RocListStr{
        .list_elements = @ptrCast(?[*]RocStr, @alignCast(@alignOf(usize), list.bytes)),
        .list_length = list.length,
        .list_capacity_or_ref_ptr = list.capacity_or_ref_ptr,
    };

    return @call(.{ .modifier = always_inline }, strJoinWith, .{ roc_list_str, separator });
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

        var result = RocStr.allocate(total_size);
        var result_ptr = result.asU8ptrMut();

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
    const list = RocListStr{
        .list_length = 3,
        .list_capacity_or_ref_ptr = 3,
        .list_elements = @ptrCast([*]RocStr, &elements),
    };

    defer {
        roc_sep.decref();
        roc_elem.decref();
        roc_result.decref();
    }

    const result = strJoinWith(list, roc_sep);

    defer result.decref();

    try expect(roc_result.eq(result));
}

// Str.toUtf8
pub fn strToUtf8C(arg: RocStr) callconv(.C) RocList {
    return strToBytes(arg);
}

inline fn strToBytes(arg: RocStr) RocList {
    const length = arg.len();
    if (length == 0) {
        return RocList.empty();
    } else if (arg.isSmallStr()) {
        const ptr = utils.allocateWithRefcount(length, RocStr.alignment);

        @memcpy(ptr, arg.asU8ptr(), length);

        return RocList{ .length = length, .bytes = ptr, .capacity_or_ref_ptr = length };
    } else {
        const is_seamless_slice = arg.str_len & SEAMLESS_SLICE_BIT;
        return RocList{ .length = length, .bytes = arg.str_bytes, .capacity_or_ref_ptr = arg.str_capacity | is_seamless_slice };
    }
}

const FromUtf8Result = extern struct {
    byte_index: usize,
    string: RocStr,
    is_ok: bool,
    problem_code: Utf8ByteProblem,
};

const CountAndStart = extern struct {
    count: usize,
    start: usize,
};

pub fn fromUtf8RangeC(
    output: *FromUtf8Result,
    list: RocList,
    start: usize,
    count: usize,
    update_mode: UpdateMode,
) callconv(.C) void {
    output.* = @call(.{ .modifier = always_inline }, fromUtf8Range, .{ list, start, count, update_mode });
}

pub fn fromUtf8Range(arg: RocList, start: usize, count: usize, update_mode: UpdateMode) FromUtf8Result {
    if (arg.len() == 0 or count == 0) {
        arg.decref(RocStr.alignment);
        return FromUtf8Result{
            .is_ok = true,
            .string = RocStr.empty(),
            .byte_index = 0,
            .problem_code = Utf8ByteProblem.InvalidStartByte,
        };
    }
    const bytes = @ptrCast([*]const u8, arg.bytes)[start .. start + count];

    if (isValidUnicode(bytes)) {
        // Make a seamless slice of the input.
        const string = RocStr.fromSubListUnsafe(arg, start, count, update_mode);
        return FromUtf8Result{
            .is_ok = true,
            .string = string,
            .byte_index = 0,
            .problem_code = Utf8ByteProblem.InvalidStartByte,
        };
    } else {
        const temp = errorToProblem(@ptrCast([*]u8, arg.bytes), arg.length);

        // decref the list
        arg.decref(RocStr.alignment);

        return FromUtf8Result{
            .is_ok = false,
            .string = RocStr.empty(),
            .byte_index = temp.index,
            .problem_code = temp.problem,
        };
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

pub fn isValidUnicode(buf: []const u8) bool {
    const size = @sizeOf(u64);
    // TODO: we should test changing the step on other platforms.
    // The general tradeoff is making extremely large strings potentially much faster
    // at the cost of small strings being slightly slower.
    const step = size;
    var i: usize = 0;
    while (i + step < buf.len) {
        var bytes: u64 = undefined;
        @memcpy(@ptrCast([*]u8, &bytes), @ptrCast([*]const u8, buf) + i, size);
        const unicode_bytes = bytes & 0x8080_8080_8080_8080;
        if (unicode_bytes == 0) {
            i += step;
            continue;
        }

        while (buf[i] < 0b1000_0000) : (i += 1) {}

        while (buf[i] >= 0b1000_0000) {
            // This forces prefetching, otherwise the loop can run at about half speed.
            if (i + 4 >= buf.len) break;
            var small_buf: [4]u8 = undefined;
            @memcpy(&small_buf, @ptrCast([*]const u8, buf) + i, 4);
            // TODO: Should we always inline these function calls below?
            if (std.unicode.utf8ByteSequenceLength(small_buf[0])) |cp_len| {
                if (std.meta.isError(std.unicode.utf8Decode(small_buf[0..cp_len]))) {
                    return false;
                }
                i += cp_len;
            } else |_| {
                return false;
            }
        }
    }

    if (i == buf.len) return true;
    while (buf[i] < 0b1000_0000) {
        i += 1;
        if (i == buf.len) return true;
    }

    return @call(.{ .modifier = always_inline }, unicode.utf8ValidateSlice, .{buf[i..]});
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
pub const Utf8ByteProblem = enum(u8) {
    CodepointTooLarge = 0,
    EncodesSurrogateHalf = 1,
    ExpectedContinuation = 2,
    InvalidStartByte = 3,
    OverlongEncoding = 4,
    UnexpectedEndOfSequence = 5,
};

fn validateUtf8Bytes(bytes: [*]u8, length: usize) FromUtf8Result {
    return fromUtf8Range(RocList{ .bytes = bytes, .length = length, .capacity_or_ref_ptr = length }, 0, length, .Immutable);
}

fn validateUtf8BytesX(str: RocList) FromUtf8Result {
    return fromUtf8Range(str, 0, str.len(), .Immutable);
}

fn expectOk(result: FromUtf8Result) !void {
    try expectEqual(result.is_ok, true);
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

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode œ" {
    const raw = "œ";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode ∆" {
    const raw = "∆";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

test "validateUtf8Bytes: emoji" {
    const raw = "💖";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode ∆ in middle of array" {
    const raw = "œb∆c¬";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

fn expectErr(list: RocList, index: usize, err: Utf8DecodeError, problem: Utf8ByteProblem) !void {
    const str_ptr = @ptrCast([*]u8, list.bytes);
    const str_len = list.length;

    try expectError(err, numberOfNextCodepointBytes(str_ptr, str_len, index));
    try expectEqual(toErrUtf8ByteResponse(index, problem), validateUtf8Bytes(str_ptr, str_len));
}

test "validateUtf8Bytes: invalid start byte" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "ab\x80c";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 2, error.Utf8InvalidStartByte, Utf8ByteProblem.InvalidStartByte);
}

test "validateUtf8Bytes: unexpected eof for 2 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 2 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: unexpected eof for 3 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 3 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\xa0\xc0";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: unexpected eof for 4 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 4 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x80\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: overlong" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L451
    const raw = "abc\xf0\x80\x80\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8OverlongEncoding, Utf8ByteProblem.OverlongEncoding);
}

test "validateUtf8Bytes: codepoint out too large" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L465
    const raw = "abc\xf4\x90\x80\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8CodepointTooLarge, Utf8ByteProblem.CodepointTooLarge);
}

test "validateUtf8Bytes: surrogate halves" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L468
    const raw = "abc\xed\xa0\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8EncodesSurrogateHalf, Utf8ByteProblem.EncodesSurrogateHalf);
}

fn isWhitespace(codepoint: u21) bool {
    // https://www.unicode.org/Public/UCD/latest/ucd/PropList.txt
    return switch (codepoint) {
        0x0009...0x000D => true, // control characters
        0x0020 => true, // space
        0x0085 => true, // control character
        0x00A0 => true, // no-break space
        0x1680 => true, // ogham space
        0x2000...0x200A => true, // en quad..hair space
        0x200E...0x200F => true, // left-to-right & right-to-left marks
        0x2028 => true, // line separator
        0x2029 => true, // paragraph separator
        0x202F => true, // narrow no-break space
        0x205F => true, // medium mathematical space
        0x3000 => true, // ideographic space

        else => false,
    };
}

test "isWhitespace" {
    try expect(isWhitespace(' '));
    try expect(isWhitespace('\u{00A0}'));
    try expect(!isWhitespace('x'));
}

pub fn strTrim(input_string: RocStr) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref();
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const leading_bytes = countLeadingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == leading_bytes) {
        string.decref();
        return RocStr.empty();
    }

    const trailing_bytes = countTrailingWhitespaceBytes(string);
    const new_len = original_len - leading_bytes - trailing_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr() + leading_bytes, new_len);
    } else if (leading_bytes == 0 and string.isUnique()) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.str_len = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .str_bytes = bytes_ptr + leading_bytes,
            .str_len = new_len | SEAMLESS_SLICE_BIT,
            .str_capacity = string.str_capacity,
        };
    } else {
        // Not unique or removing leading bytes, just make a slice.
        return RocStr{
            .str_bytes = bytes_ptr + leading_bytes,
            .str_len = new_len | SEAMLESS_SLICE_BIT,
            .str_capacity = @ptrToInt(bytes_ptr) >> 1,
        };
    }
}

pub fn strTrimLeft(input_string: RocStr) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref();
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const leading_bytes = countLeadingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == leading_bytes) {
        string.decref();
        return RocStr.empty();
    }

    const new_len = original_len - leading_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr() + leading_bytes, new_len);
    } else if (leading_bytes == 0 and string.isUnique()) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.str_len = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .str_bytes = bytes_ptr + leading_bytes,
            .str_len = new_len | SEAMLESS_SLICE_BIT,
            .str_capacity = string.str_capacity,
        };
    } else {
        // Not unique or removing leading bytes, just make a slice.
        return RocStr{
            .str_bytes = bytes_ptr + leading_bytes,
            .str_len = new_len | SEAMLESS_SLICE_BIT,
            .str_capacity = @ptrToInt(bytes_ptr) >> 1,
        };
    }
}

pub fn strTrimRight(input_string: RocStr) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref();
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const trailing_bytes = countTrailingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == trailing_bytes) {
        string.decref();
        return RocStr.empty();
    }

    const new_len = original_len - trailing_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr(), new_len);
    } else if (string.isUnique()) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.str_len = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .str_bytes = bytes_ptr,
            .str_len = new_len | SEAMLESS_SLICE_BIT,
            .str_capacity = string.str_capacity,
        };
    } else {
        // Not unique, just make a slice.
        return RocStr{
            .str_bytes = bytes_ptr,
            .str_len = new_len | SEAMLESS_SLICE_BIT,
            .str_capacity = @ptrToInt(bytes_ptr) >> 1,
        };
    }
}

fn countLeadingWhitespaceBytes(string: RocStr) usize {
    var byte_count: usize = 0;

    var bytes = string.asU8ptr()[0..string.len()];
    var iter = unicode.Utf8View.initUnchecked(bytes).iterator();
    while (iter.nextCodepoint()) |codepoint| {
        if (isWhitespace(codepoint)) {
            byte_count += unicode.utf8CodepointSequenceLength(codepoint) catch break;
        } else {
            break;
        }
    }

    return byte_count;
}

fn countTrailingWhitespaceBytes(string: RocStr) usize {
    var byte_count: usize = 0;

    var bytes = string.asU8ptr()[0..string.len()];
    var iter = ReverseUtf8View.initUnchecked(bytes).iterator();
    while (iter.nextCodepoint()) |codepoint| {
        if (isWhitespace(codepoint)) {
            byte_count += unicode.utf8CodepointSequenceLength(codepoint) catch break;
        } else {
            break;
        }
    }

    return byte_count;
}

/// A backwards version of Utf8View from std.unicode
const ReverseUtf8View = struct {
    bytes: []const u8,

    pub fn initUnchecked(s: []const u8) ReverseUtf8View {
        return ReverseUtf8View{ .bytes = s };
    }

    pub fn iterator(s: ReverseUtf8View) ReverseUtf8Iterator {
        return ReverseUtf8Iterator{
            .bytes = s.bytes,
            .i = if (s.bytes.len > 0) s.bytes.len - 1 else null,
        };
    }
};

/// A backwards version of Utf8Iterator from std.unicode
const ReverseUtf8Iterator = struct {
    bytes: []const u8,
    // NOTE null signifies complete/empty
    i: ?usize,

    pub fn nextCodepointSlice(it: *ReverseUtf8Iterator) ?[]const u8 {
        if (it.i) |index| {
            var i = index;

            // NOTE this relies on the string being valid utf8 to not run off the end
            while (!utf8BeginByte(it.bytes[i])) {
                i -= 1;
            }

            const cp_len = unicode.utf8ByteSequenceLength(it.bytes[i]) catch unreachable;
            const slice = it.bytes[i .. i + cp_len];

            it.i = if (i == 0) null else i - 1;

            return slice;
        } else {
            return null;
        }
    }

    pub fn nextCodepoint(it: *ReverseUtf8Iterator) ?u21 {
        const slice = it.nextCodepointSlice() orelse return null;

        return switch (slice.len) {
            1 => @as(u21, slice[0]),
            2 => unicode.utf8Decode2(slice) catch unreachable,
            3 => unicode.utf8Decode3(slice) catch unreachable,
            4 => unicode.utf8Decode4(slice) catch unreachable,
            else => unreachable,
        };
    }
};

fn utf8BeginByte(byte: u8) bool {
    return switch (byte) {
        0b1000_0000...0b1011_1111 => false,
        else => true,
    };
}

test "strTrim: empty" {
    const trimmedEmpty = strTrim(RocStr.empty());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrim: null byte" {
    const bytes = [_]u8{0};
    const original = RocStr.init(&bytes, 1);

    try expectEqual(@as(usize, 1), original.len());
    try expectEqual(@as(usize, SMALL_STR_MAX_LENGTH), original.getCapacity());

    const original_with_capacity = reserve(original, 40);
    defer original_with_capacity.decref();

    try expectEqual(@as(usize, 1), original_with_capacity.len());
    try expectEqual(@as(usize, 64), original_with_capacity.getCapacity());

    const trimmed = strTrim(original.clone());
    defer trimmed.decref();

    try expect(original.eq(trimmed));
}

test "strTrim: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    const trimmed = strTrim(original);
    defer trimmed.decref();

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrim: large to large" {
    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    try expect(!original.isSmallStr());

    const expected_bytes = "hello even more giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(!expected.isSmallStr());

    const trimmed = strTrim(original);
    defer trimmed.decref();

    try expect(trimmed.eq(expected));
}

test "strTrim: large to small sized slice" {
    const original_bytes = "             hello         ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    try expect(!original.isSmallStr());

    const expected_bytes = "hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    try expect(original.isUnique());
    const trimmed = strTrim(original);
    defer trimmed.decref();

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrim: small to small" {
    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(original.isSmallStr());

    const expected_bytes = "hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrim(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimLeft: empty" {
    const trimmedEmpty = strTrimLeft(RocStr.empty());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrimLeft: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    const trimmed = strTrimLeft(original);

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrimLeft: large to large" {
    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(!original.isSmallStr());

    const expected_bytes = "hello even more giant world ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(!expected.isSmallStr());

    const trimmed = strTrimLeft(original);

    try expect(trimmed.eq(expected));
}

test "strTrimLeft: large to small" {
    // `original` will be consumed by the concat; do not free explicitly
    const original_bytes = "                    hello ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    try expect(!original.isSmallStr());

    const expected_bytes = "hello ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrimLeft(original);
    defer trimmed.decref();

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrimLeft: small to small" {
    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(original.isSmallStr());

    const expected_bytes = "hello ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrimLeft(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimRight: empty" {
    const trimmedEmpty = strTrimRight(RocStr.empty());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrimRight: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    const trimmed = strTrimRight(original);

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrimRight: large to large" {
    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(!original.isSmallStr());

    const expected_bytes = " hello even more giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(!expected.isSmallStr());

    const trimmed = strTrimRight(original);

    try expect(trimmed.eq(expected));
}

test "strTrimRight: large to small" {
    // `original` will be consumed by the concat; do not free explicitly
    const original_bytes = " hello                    ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    try expect(!original.isSmallStr());

    const expected_bytes = " hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrimRight(original);
    defer trimmed.decref();

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrimRight: small to small" {
    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(original.isSmallStr());

    const expected_bytes = " hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrimRight(original);

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
    const data_bytes = "foobar";
    var data = RocStr.init(data_bytes, data_bytes.len);
    defer data.decref();

    try expectEqual(data.getCapacity(), SMALL_STR_MAX_LENGTH);
}

test "capacity: big string" {
    const data_bytes = "a string so large that it must be heap-allocated";
    var data = RocStr.init(data_bytes, data_bytes.len);
    defer data.decref();

    try expect(data.getCapacity() >= data_bytes.len);
}

pub fn appendScalar(string: RocStr, scalar_u32: u32) callconv(.C) RocStr {
    const scalar = @intCast(u21, scalar_u32);
    const width = std.unicode.utf8CodepointSequenceLength(scalar) catch unreachable;

    var output = string.reallocate(string.len() + width);
    var slice = output.asSliceWithCapacityMut();

    _ = std.unicode.utf8Encode(scalar, slice[string.len() .. string.len() + width]) catch unreachable;

    return output;
}

test "appendScalar: small A" {
    const A: []const u8 = "A";

    const data_bytes = "hello";
    var data = RocStr.init(data_bytes, data_bytes.len);

    const actual = appendScalar(data, A[0]);
    defer actual.decref();

    const expected_bytes = "helloA";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(actual.eq(expected));
}

test "appendScalar: small 😀" {
    const data_bytes = "hello";
    var data = RocStr.init(data_bytes, data_bytes.len);

    const actual = appendScalar(data, 0x1F600);
    defer actual.decref();

    const expected_bytes = "hello😀";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(actual.eq(expected));
}

test "appendScalar: big A" {
    const A: []const u8 = "A";

    const data_bytes = "a string so large that it must be heap-allocated";
    var data = RocStr.init(data_bytes, data_bytes.len);

    const actual = appendScalar(data, A[0]);
    defer actual.decref();

    const expected_bytes = "a string so large that it must be heap-allocatedA";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(actual.eq(expected));
}

test "appendScalar: big 😀" {
    const data_bytes = "a string so large that it must be heap-allocated";
    var data = RocStr.init(data_bytes, data_bytes.len);

    const actual = appendScalar(data, 0x1F600);
    defer actual.decref();

    const expected_bytes = "a string so large that it must be heap-allocated😀";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(actual.eq(expected));
}

pub fn reserve(string: RocStr, spare: usize) callconv(.C) RocStr {
    const old_length = string.len();
    if (string.getCapacity() >= old_length + spare) {
        return string;
    } else {
        var output = string.reallocate(old_length + spare);
        output.setLen(old_length);
        return output;
    }
}

pub fn withCapacity(capacity: usize) callconv(.C) RocStr {
    var str = RocStr.allocate(capacity);
    str.setLen(0);
    return str;
}

pub fn getScalarUnsafe(string: RocStr, index: usize) callconv(.C) extern struct { bytesParsed: usize, scalar: u32 } {
    const slice = string.asSlice();
    const bytesParsed = @intCast(usize, std.unicode.utf8ByteSequenceLength(slice[index]) catch unreachable);
    const scalar = std.unicode.utf8Decode(slice[index .. index + bytesParsed]) catch unreachable;

    return .{ .bytesParsed = bytesParsed, .scalar = @intCast(u32, scalar) };
}

test "getScalarUnsafe" {
    const data_bytes = "A";
    var data = RocStr.init(data_bytes, data_bytes.len);

    const result = getScalarUnsafe(data, 0);

    const expected = try std.unicode.utf8Decode("A");

    try expectEqual(result.scalar, @intCast(u32, expected));
    try expectEqual(result.bytesParsed, 1);
}

pub fn strCloneTo(
    string: RocStr,
    ptr: [*]u8,
    offset: usize,
    extra_offset: usize,
) callconv(.C) usize {
    const WIDTH: usize = @sizeOf(RocStr);
    if (string.isSmallStr()) {
        const array: [@sizeOf(RocStr)]u8 = @bitCast([@sizeOf(RocStr)]u8, string);

        var i: usize = 0;
        while (i < WIDTH) : (i += 1) {
            ptr[offset + i] = array[i];
        }

        return extra_offset;
    } else {
        const slice = string.asSlice();

        var relative = string;
        relative.str_bytes = @intToPtr(?[*]u8, extra_offset); // i.e. just after the string struct

        // write the string struct
        const array = relative.asArray();
        @memcpy(ptr + offset, &array, WIDTH);

        // write the string bytes just after the struct
        @memcpy(ptr + extra_offset, slice.ptr, slice.len);

        return extra_offset + slice.len;
    }
}

pub fn strRefcountPtr(
    string: RocStr,
) callconv(.C) ?[*]u8 {
    return string.getRefcountPtr();
}

pub fn strReleaseExcessCapacity(
    string: RocStr,
) callconv(.C) RocStr {
    const old_length = string.len();
    // We use the direct list.capacity_or_ref_ptr to make sure both that there is no extra capacity and that it isn't a seamless slice.
    if (string.isSmallStr()) {
        // SmallStr has no excess capacity.
        return string;
    } else if (string.isUnique() and !string.isSeamlessSlice() and string.getCapacity() == old_length) {
        return string;
    } else if (old_length == 0) {
        string.decref();
        return RocStr.empty();
    } else {
        var output = RocStr.allocateExact(old_length);
        const source_ptr = string.asU8ptr();
        const dest_ptr = output.asU8ptrMut();

        @memcpy(dest_ptr, source_ptr, old_length);
        string.decref();

        return output;
    }
}
