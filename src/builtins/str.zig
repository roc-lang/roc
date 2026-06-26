//! Builtin string operations and data structures for the Roc runtime.
//!
//! This module provides the core implementation of Roc's Str type, including
//! operations for string manipulation, Unicode handling, formatting, and
//! memory management. It defines the RocStr structure and associated functions
//! that are called from compiled Roc code to handle string operations efficiently.
const std = @import("std");
const builtin = @import("builtin");

const RocList = @import("list.zig").RocList;
const RocOps = @import("host_abi.zig").RocOps;
const UpdateMode = @import("utils.zig").UpdateMode;
const TestEnv = @import("utils.zig").TestEnv;

const utils = @import("utils.zig");
const compiler_rt_128 = @import("compiler_rt_128.zig");
const parse_float = @import("vendor_parse_float");
const ascii = std.ascii;
const mem = std.mem;
const unicode = std.unicode;
const testing = std.testing;
const rcNone = @import("utils.zig").rcNone;

const InPlace = enum(u8) {
    InPlace,
    Clone,
};

const MASK_ISIZE: isize = std.math.minInt(isize);
const SMALL_STR_BIT: usize = @as(usize, @bitCast(MASK_ISIZE));
const SEAMLESS_SLICE_TAG: usize = 1;

/// TODO
pub const SMALL_STR_MAX_LENGTH = SMALL_STRING_SIZE - 1;

const SMALL_STRING_SIZE = @sizeOf(RocStr);

/// Runtime representation of Roc's Str value.
///
/// Small strings store their bytes inline across the full struct and mark the
/// final byte with the high bit. Big strings keep `bytes` as a directly
/// dereferenceable pointer. We deliberately do not tag `bytes`: string
/// operations, hosts, glue, and object relocations all need the byte pointer to
/// be usable as-is, and masking it on every read would put tag handling in the
/// hottest path. Slice tagging lives in `capacity_or_alloc_ptr` instead, which is
/// only inspected by capacity and reference-counting operations.
///
/// Big-string capacities are shifted left by one to keep the low bit available
/// for slice tagging. That makes the maximum representable capacity the maximum
/// signed pointer-sized integer: about 2 GiB on 32-bit targets and 8 EiB on
/// 64-bit targets.
pub const RocStr = extern struct {
    bytes: ?[*]u8,
    // For big strs, contains the capacity shifted left by one.
    // For seamless slices contains the pointer to the original allocation, tagged with 1.
    // This pointer is to the first character of the original string.
    capacity_or_alloc_ptr: usize,
    length: usize,

    pub const alignment = @alignOf(usize);

    pub inline fn empty() RocStr {
        return RocStr{
            .bytes = null,
            .capacity_or_alloc_ptr = 0,
            .length = SMALL_STR_BIT,
        };
    }

    pub inline fn encodeCapacity(capacity: usize) usize {
        return capacity << 1;
    }

    pub inline fn decodeCapacity(encoded: usize) usize {
        return encoded >> 1;
    }

    pub inline fn encodeSliceAllocationPtr(ptr: [*]u8) usize {
        return @intFromPtr(ptr) | SEAMLESS_SLICE_TAG;
    }

    pub inline fn decodeSliceAllocationPtr(encoded: usize) usize {
        return encoded & ~SEAMLESS_SLICE_TAG;
    }

    // This clones the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn init(
        bytes_ptr: [*]const u8,
        length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        var result = RocStr.allocate(length, roc_ops);
        @memcpy(result.asU8ptrMut()[0..length], bytes_ptr[0..length]);

        return result;
    }

    // This requires that the list is non-null.
    // It also requires that start and count define a slice that does not go outside the bounds of the list.
    pub fn fromSubListUnsafe(list: RocList, start: usize, count: usize, update_mode: UpdateMode, roc_ops: *RocOps) RocStr {
        const start_byte = @as([*]u8, @ptrCast(list.bytes)) + start;
        if (list.isSeamlessSlice()) {
            const list_alloc_ptr = list.getAllocationDataPtr(roc_ops) orelse return RocStr.empty();
            return RocStr{
                .bytes = start_byte,
                .capacity_or_alloc_ptr = encodeSliceAllocationPtr(list_alloc_ptr),
                .length = count,
            };
        } else if (start == 0 and (update_mode == .InPlace or list.isUnique(roc_ops))) {
            // Rare case, we can take over the original list.
            return RocStr{
                .bytes = start_byte,
                .capacity_or_alloc_ptr = list.capacity_or_alloc_ptr, // RocStr and RocList share owned capacity encoding.
                .length = count,
            };
        } else {
            // Create seamless slice pointing to the list.
            return RocStr{
                .bytes = start_byte,
                .capacity_or_alloc_ptr = encodeSliceAllocationPtr(list.bytes orelse return RocStr.empty()),
                .length = count,
            };
        }
    }

    pub fn isSeamlessSlice(self: RocStr) bool {
        return !self.isSmallStr() and (self.capacity_or_alloc_ptr & SEAMLESS_SLICE_TAG) == SEAMLESS_SLICE_TAG;
    }

    pub fn fromSlice(
        slice: []const u8,
        roc_ops: *RocOps,
    ) RocStr {
        return RocStr.init(slice.ptr, slice.len, roc_ops);
    }

    /// Create a small string from a slice. The slice must fit in a small string
    /// (length < SMALL_STRING_SIZE). This does not require roc_ops since small
    /// strings are stored inline and don't need heap allocation.
    /// Asserts in debug mode if the slice is too large.
    pub fn fromSliceSmall(slice: []const u8) RocStr {
        std.debug.assert(slice.len < SMALL_STRING_SIZE);
        var result = RocStr.empty();
        @memcpy(result.asU8ptrMut()[0..slice.len], slice);
        result.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(slice.len)) | 0b1000_0000;
        return result;
    }

    /// Returns true if the given length would fit in a small string (stored inline).
    pub fn fitsInSmallStr(length: usize) bool {
        return length < SMALL_STRING_SIZE;
    }

    fn allocateBig(
        length: usize,
        capacity: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const first_element = @import("utils.zig").allocateWithRefcount(
            capacity,
            @sizeOf(usize),
            false,
            roc_ops,
        );

        return RocStr{
            .bytes = first_element,
            .capacity_or_alloc_ptr = encodeCapacity(capacity),
            .length = length,
        };
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet.
    // May have a larger capacity than the length.
    pub fn allocate(
        length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const element_width = 1;
        const result_is_big = length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            const capacity = utils.calculateCapacity(0, length, element_width);
            return RocStr.allocateBig(length, capacity, roc_ops);
        } else {
            var string = RocStr.empty();

            string.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(length)) | 0b1000_0000;

            return string;
        }
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet.
    // Will have the exact same capacity as length if it is not a small string.
    pub fn allocateExact(
        length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const result_is_big = length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            return RocStr.allocateBig(length, length, roc_ops);
        } else {
            var string = RocStr.empty();

            string.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(length)) | 0b1000_0000;

            return string;
        }
    }

    // This returns all ones if the string is a seamless slice.
    // Otherwise, it returns all zeros.
    // This is done without branching for optimization purposes.
    pub fn seamlessSliceMask(self: RocStr) usize {
        return 0 -% (self.capacity_or_alloc_ptr & SEAMLESS_SLICE_TAG);
    }

    // returns a pointer to the original allocation.
    // This pointer points to the first element of the allocation.
    // The pointer is to just after the refcount.
    // For big strings, it just returns their bytes pointer.
    // For seamless slices, it returns the pointer stored in capacity_or_alloc_ptr.
    // This does not return a valid value if the input is a small string.
    pub fn getAllocationPtr(self: RocStr) ?[*]u8 {
        const str_alloc_ptr = @intFromPtr(self.bytes);
        const slice_alloc_ptr = decodeSliceAllocationPtr(self.capacity_or_alloc_ptr);
        const slice_mask = self.seamlessSliceMask();
        const alloc_ptr = (str_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);

        // Verify the computed allocation pointer is properly aligned
        if (comptime builtin.mode == .Debug) {
            if (alloc_ptr != 0 and alloc_ptr % @alignOf(usize) != 0) {
                // This indicates memory corruption - the allocation pointer should always be aligned
                unreachable;
            }
        }

        return @as(?[*]u8, @ptrFromInt(alloc_ptr));
    }

    /// Increments the string's refcount using the given count-update atomicity.
    pub fn increfWithAtomicity(self: RocStr, n: usize, atomicity: utils.RcAtomicity, roc_ops: *RocOps) void {
        if (!self.isSmallStr()) {
            if (self.getAllocationPtr()) |alloc_ptr| {
                const isizes: [*]isize = utils.alignedPtrCast([*]isize, alloc_ptr, @src());
                utils.increfRcPtr(@as(*isize, @ptrCast(isizes - 1)), @as(isize, @intCast(n)), atomicity, roc_ops);
            }
        }
    }

    /// Increments the string's refcount with atomic count updates.
    pub fn incref(self: RocStr, n: usize, roc_ops: *RocOps) void {
        self.increfWithAtomicity(n, .atomic, roc_ops);
    }

    /// Decrements the string's refcount using the given count-update atomicity,
    /// freeing the allocation when the count reaches zero.
    pub fn decrefWithAtomicity(
        self: RocStr,
        atomicity: utils.RcAtomicity,
        roc_ops: *RocOps,
    ) void {
        if (!self.isSmallStr()) {
            utils.decref(self.getAllocationPtr(), self.capacity_or_alloc_ptr, RocStr.alignment, false, atomicity, roc_ops);
        }
    }

    /// Decrements the string's refcount with atomic count updates,
    /// freeing the allocation when the count reaches zero.
    pub fn decref(
        self: RocStr,
        roc_ops: *RocOps,
    ) void {
        self.decrefWithAtomicity(.atomic, roc_ops);
    }

    pub fn eql(self: RocStr, other: RocStr) bool {
        // For non-small strings, equal byte pointers and lengths imply equal
        // contents. Small strings store their payload across these same struct
        // fields, so they must always compare the inline bytes below.
        if (!self.isSmallStr() and !other.isSmallStr() and self.bytes == other.bytes and self.length == other.length) {
            return true;
        }

        const self_len = self.len();
        const other_len = other.len();

        // If their lengths are different, they're definitely unequal.
        if (self_len != other_len) {
            return false;
        }

        if (self.isSmallStr() and other.isSmallStr()) {
            const self_bytes: [@sizeOf(RocStr)]u8 = @bitCast(self);
            const other_bytes: [@sizeOf(RocStr)]u8 = @bitCast(other);
            return smallBytesEqual(self_bytes, other_bytes, self_len);
        }

        return bytesEqualFast(self.asU8ptr(), other.asU8ptr(), self_len);
    }

    /// Compare this RocStr with a byte slice for equality.
    pub fn eqlSlice(self: RocStr, slice: []const u8) bool {
        const self_len = self.len();

        if (self_len != slice.len) {
            return false;
        }

        return bytesEqualFast(self.asU8ptr(), slice.ptr, self_len);
    }

    /// Compare this RocStr against up to 24 static bytes packed little-endian
    /// into three u64 words. This is an internal compiler/runtime helper for
    /// generated record-field dispatch; it is not a user-facing primitive.
    ///
    /// The static side is already known by the compiler. The runtime side may be
    /// small, heap-backed, or a seamless slice, so all fixed-width loads stay
    /// inside the logical `self.len()` range. Short slices below one full word
    /// use a bounded byte build instead of over-reading past their allocation.
    pub fn eqlStaticSmall(self: RocStr, static_len: usize, word0: u64, word1: u64, word2: u64) bool {
        if (static_len > 24) return false;

        const self_len = self.len();
        if (self_len != static_len) return false;

        return bytesEqualStaticSmall(self.asU8ptr(), static_len, word0, word1, word2);
    }

    /// Compare one packed static word lane against this RocStr. This is an
    /// internal discriminator for generated field-name dispatch. It is separate
    /// from `eqlStaticSmall` so generated code can cheaply reject most fields
    /// with one selected lane before doing full verification on a rare hit.
    ///
    /// `active_len` says how many low byte lanes in `word` are meaningful. The
    /// runtime string may be small, heap-backed, or a seamless slice; every load
    /// is either proven in-bounds or assembled byte-by-byte.
    pub fn staticSmallWordEq(self: RocStr, offset: usize, active_len: usize, word: u64) bool {
        if (active_len > @sizeOf(u64)) return false;

        const self_len = self.len();
        if (offset > self_len) return false;
        if (active_len > self_len - offset) return false;

        const mask = lowBytesMask64(active_len);
        const runtime_word = staticSmallRuntimeWord(self, offset, active_len);
        return (runtime_word & mask) == (word & mask);
    }

    /// Compare one packed static word lane against this RocStr using the same
    /// ASCII-caseless semantics as `strCaselessAsciiEquals`.
    ///
    /// This is an internal discriminator for generated field-name dispatch. It
    /// does not lowercase, allocate, or normalize either side. Exact-equal byte
    /// lanes can contain any byte; differing lanes must differ by `0x20` and be
    /// ASCII letters.
    pub fn staticSmallWordCaselessEq(self: RocStr, offset: usize, active_len: usize, word: u64) bool {
        if (active_len > @sizeOf(u64)) return false;

        const self_len = self.len();
        if (offset > self_len) return false;
        if (active_len > self_len - offset) return false;

        const active = lowBytesMask64(active_len);
        const runtime_word = staticSmallRuntimeWord(self, offset, active_len);
        return wordCaselessAsciiEqualMasked(runtime_word, word, active);
    }

    pub fn clone(
        str: RocStr,
        roc_ops: *RocOps,
    ) RocStr {
        if (str.isSmallStr()) {
            // just return the bytes
            return str;
        } else {
            const length = str.len();
            const new_str = RocStr.allocateBig(length, length, roc_ops);

            const old_bytes: [*]u8 = @as([*]u8, @ptrCast(str.bytes));
            const new_bytes: [*]u8 = @as([*]u8, @ptrCast(new_str.bytes));

            @memcpy(new_bytes[0..length], old_bytes[0..length]);

            return new_str;
        }
    }

    /// An `.InPlace` update mode means the caller proved the string unique,
    /// so the runtime uniqueness check is skipped. Small strings and seamless
    /// slices still reallocate fresh: neither owns a growable big-string
    /// allocation, so the structural checks are not uniqueness checks.
    pub fn reallocate(
        self: RocStr,
        new_length: usize,
        update_mode: UpdateMode,
        roc_ops: *RocOps,
    ) RocStr {
        const element_width = 1;
        const old_capacity = self.getCapacity();

        if (self.isSmallStr() or self.isSeamlessSlice() or !(update_mode == .InPlace or self.isUnique())) {
            return self.reallocateFresh(new_length, roc_ops);
        }

        if (self.bytes) |source_ptr| {
            if (old_capacity > new_length) {
                var output = self;
                output.setLen(new_length);
                return output;
            }
            const new_capacity = @import("utils.zig").calculateCapacity(old_capacity, new_length, element_width);
            const new_source = @import("utils.zig").unsafeReallocate(
                source_ptr,
                RocStr.alignment,
                old_capacity,
                new_capacity,
                element_width,
                false,
                roc_ops,
            );

            return RocStr{ .bytes = new_source, .capacity_or_alloc_ptr = encodeCapacity(new_capacity), .length = new_length };
        }
        return self.reallocateFresh(new_length, roc_ops);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    fn reallocateFresh(
        self: RocStr,
        new_length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const old_length = self.len();

        const element_width = 1;
        const result_is_big = new_length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            const capacity = @import("utils.zig").calculateCapacity(0, new_length, element_width);
            var result = RocStr.allocateBig(new_length, capacity, roc_ops);

            // transfer the memory

            const source_ptr = self.asU8ptr();
            const dest_ptr = result.asU8ptrMut();

            std.mem.copyForwards(u8, dest_ptr[0..old_length], source_ptr[0..old_length]);
            @memset(dest_ptr[old_length..new_length], 0);

            self.decref(roc_ops);

            return result;
        } else {
            var string = RocStr.empty();

            // I believe taking this reference on the stack here is important for correctness.
            // Doing it via a method call seemed to cause issues
            const dest_ptr = @as([*]u8, @ptrCast(&string));
            dest_ptr[@sizeOf(RocStr) - 1] = @as(u8, @intCast(new_length)) | 0b1000_0000;

            const source_ptr = self.asU8ptr();

            std.mem.copyForwards(u8, dest_ptr[0..old_length], source_ptr[0..old_length]);
            @memset(dest_ptr[old_length..new_length], 0);

            self.decref(roc_ops);

            return string;
        }
    }

    pub fn isSmallStr(self: RocStr) bool {
        return @as(isize, @bitCast(self.length)) < 0;
    }

    fn asArray(self: RocStr) [@sizeOf(RocStr)]u8 {
        const as_ptr = @as([*]const u8, @ptrCast(&self));
        const slice = as_ptr[0..@sizeOf(RocStr)];

        return slice.*;
    }

    pub fn is_empty(self: RocStr) bool {
        return self.len() == 0;
    }

    pub fn len(self: RocStr) usize {
        if (self.isSmallStr()) {
            return self.asArray()[@sizeOf(RocStr) - 1] ^ 0b1000_0000;
        } else {
            return self.length;
        }
    }

    pub fn setLen(self: *RocStr, length: usize) void {
        if (self.isSmallStr()) {
            self.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(length)) | 0b1000_0000;
        } else {
            self.length = length;
        }
    }

    pub fn getCapacity(self: RocStr) usize {
        if (self.isSmallStr()) {
            return SMALL_STR_MAX_LENGTH;
        } else if (self.isSeamlessSlice()) {
            return self.length;
        } else {
            return decodeCapacity(self.capacity_or_alloc_ptr);
        }
    }

    // This does a small string check, but no bounds checking whatsoever!
    pub fn getUnchecked(self: RocStr, index: usize) u8 {
        if (self.isSmallStr()) {
            return self.asArray()[index];
        } else {
            const bytes = self.bytes orelse unreachable;

            return bytes[index];
        }
    }

    pub fn isEmpty(self: RocStr) bool {
        return self.len() == 0;
    }

    pub fn isUnique(self: RocStr) bool {
        // small strings can be copied
        if (self.isSmallStr()) {
            return true;
        }

        // otherwise, check if the refcount is one
        return @call(.always_inline, RocStr.isRefcountOne, .{self});
    }

    fn isRefcountOne(self: RocStr) bool {
        return @import("utils.zig").rcUnique(@bitCast(self.refcount()));
    }

    fn refcount(self: RocStr) usize {
        const is_seamless_slice = self.isSeamlessSlice();
        if ((self.getCapacity() == 0 and !is_seamless_slice) or self.isSmallStr()) {
            return 1;
        }

        const data_ptr = if (is_seamless_slice)
            self.getAllocationPtr()
        else
            self.bytes;

        if (data_ptr) |non_null_ptr| {
            const ptr: [*]usize = utils.alignedPtrCast([*]usize, non_null_ptr, @src());
            return (ptr - 1)[0];
        } else {
            // This should never happen - indicates corrupted RocStr structure
            unreachable;
        }
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
            return @as([*]const u8, @ptrCast(self));
        } else {
            return @as([*]const u8, @ptrCast(self.bytes));
        }
    }

    pub fn asU8ptrMut(self: *RocStr) [*]u8 {
        if (self.isSmallStr()) {
            return @as([*]u8, @ptrCast(self));
        } else {
            return @as([*]u8, @ptrCast(self.bytes));
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
        @memcpy(dest[0..self.len()], src[0..self.len()]);
    }
};

inline fn bytesEqualFast(left: [*]const u8, right: [*]const u8, len: usize) bool {
    const word_size = @sizeOf(u64);

    var index: usize = 0;
    while (index + word_size <= len) : (index += word_size) {
        const left_word = std.mem.readInt(u64, left[index..][0..word_size], .little);
        const right_word = std.mem.readInt(u64, right[index..][0..word_size], .little);
        if (left_word != right_word) return false;
    }

    const tail_len = len - index;
    if (tail_len != 0) {
        const mask = lowBytesMask64(tail_len);
        const left_tail = readTailU64(left, len, index, tail_len);
        const right_tail = readTailU64(right, len, index, tail_len);
        return (left_tail & mask) == (right_tail & mask);
    }

    return true;
}

inline fn bytesEqualStaticSmall(bytes: [*]const u8, len: usize, word0: u64, word1: u64, word2: u64) bool {
    const static_words = [3]u64{ word0, word1, word2 };
    const word_size = @sizeOf(u64);

    var index: usize = 0;
    while (index + word_size <= len) : (index += word_size) {
        const runtime_word = std.mem.readInt(u64, bytes[index..][0..word_size], .little);
        if (runtime_word != static_words[index / word_size]) return false;
    }

    const tail_len = len - index;
    if (tail_len == 0) return true;

    const mask = lowBytesMask64(tail_len);
    const runtime_tail = readTailU64(bytes, len, index, tail_len);
    return (runtime_tail & mask) == (static_words[index / word_size] & mask);
}

inline fn staticSmallRuntimeWord(str: RocStr, offset: usize, active_len: usize) u64 {
    if (str.isSmallStr()) {
        const bytes: [@sizeOf(RocStr)]u8 = @bitCast(str);
        return readSmallStringU64(bytes, offset);
    }

    const bytes = str.asU8ptr();
    const len = str.len();
    if (offset + @sizeOf(u64) <= len) {
        return std.mem.readInt(u64, bytes[offset..][0..@sizeOf(u64)], .little);
    }

    return readTailU64(bytes, len, offset, active_len);
}

inline fn smallBytesEqual(left: [@sizeOf(RocStr)]u8, right: [@sizeOf(RocStr)]u8, len: usize) bool {
    const word_size = @sizeOf(u64);

    var index: usize = 0;
    while (index + word_size <= len) : (index += word_size) {
        const left_word = std.mem.readInt(u64, left[index..][0..word_size], .little);
        const right_word = std.mem.readInt(u64, right[index..][0..word_size], .little);
        if (left_word != right_word) return false;
    }

    const tail_len = len - index;
    if (tail_len == 0) return true;

    const left_tail = readSmallStringU64(left, index);
    const right_tail = readSmallStringU64(right, index);
    const mask = lowBytesMask64(tail_len);
    return (left_tail & mask) == (right_tail & mask);
}

inline fn readSmallStringU64(bytes: [@sizeOf(RocStr)]u8, index: usize) u64 {
    // Roc small strings store their bytes inline in the RocStr value and zero
    // unused inline bytes. That gives the hot equality path a real fixed-width
    // source to load from even when the logical string length is not a multiple
    // of eight.
    //
    // On 64-bit targets the inline buffer is 24 bytes, so every 8-byte SSO lane
    // can be loaded directly. On 32-bit targets the inline buffer is 12 bytes:
    // a string with 9-11 bytes has its tail starting at byte 8, where an
    // ordinary 8-byte load would run past the RocStr value. In that rare case,
    // load the final in-bounds u64 and shift the requested lane down to byte 0.
    if (index + @sizeOf(u64) <= @sizeOf(RocStr)) {
        return std.mem.readInt(u64, bytes[index..][0..@sizeOf(u64)], .little);
    }

    const load_index = @sizeOf(RocStr) - @sizeOf(u64);
    const shift = (index - load_index) * 8;
    const word = std.mem.readInt(u64, bytes[load_index..][0..@sizeOf(u64)], .little);
    return word >> @intCast(shift);
}

inline fn lowBytesMask64(byte_count: usize) u64 {
    if (byte_count >= @sizeOf(u64)) return std.math.maxInt(u64);
    return (@as(u64, 1) << @intCast(byte_count * 8)) - 1;
}

pub fn init(
    bytes_ptr: [*]const u8,
    length: usize,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    return @call(.always_inline, RocStr.init, .{ bytes_ptr, length, roc_ops });
}

// Str.equal
/// TODO: Document strEqual.
pub fn strEqual(self: RocStr, other: RocStr) callconv(.c) bool {
    return self.eql(other);
}

/// Internal helper for compiler-generated static small string comparison.
pub fn strEqualStaticSmall(self: RocStr, static_len: u64, word0: u64, word1: u64, word2: u64) callconv(.c) bool {
    return self.eqlStaticSmall(@intCast(static_len), word0, word1, word2);
}

/// Internal helper for generated static small word-lane comparison.
pub fn strStaticSmallWordEq(self: RocStr, offset: u64, active_len: u64, word: u64) callconv(.c) bool {
    return self.staticSmallWordEq(@intCast(offset), @intCast(active_len), word);
}

/// Internal helper for generated static small ASCII-caseless word-lane comparison.
pub fn strStaticSmallWordCaselessEq(self: RocStr, offset: u64, active_len: u64, word: u64) callconv(.c) bool {
    return self.staticSmallWordCaselessEq(@intCast(offset), @intCast(active_len), word);
}

// Str.numberOfBytes
/// TODO: Document strNumberOfBytes.
pub fn strNumberOfBytes(string: RocStr) callconv(.c) usize {
    return string.len();
}

// Str.fromInt
/// TODO: Document exportFromInt.
pub fn exportFromInt(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            int: T,
            roc_ops: *RocOps,
        ) callconv(.c) RocStr {
            return @call(.always_inline, strFromIntHelp, .{ T, int, roc_ops });
        }
    }.func;

    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

fn strFromIntHelp(
    comptime T: type,
    int: T,
    roc_ops: *RocOps,
) RocStr {
    const size = compiler_rt_128.int_string_capacity(T);
    var buf: [size]u8 = undefined;
    const result = compiler_rt_128.int_to_str(T, &buf, int);

    return RocStr.init(result.ptr, result.len, roc_ops);
}

// Str.fromFloat
/// TODO: Document exportFromFloat.
pub fn exportFromFloat(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            float: T,
            roc_ops: *RocOps,
        ) callconv(.c) RocStr {
            return @call(.always_inline, strFromFloatHelp, .{ T, float, roc_ops });
        }
    }.func;

    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

fn strFromFloatHelp(
    comptime T: type,
    float: T,
    roc_ops: *RocOps,
) RocStr {
    var buf: [32]u8 = undefined;
    const val_bits: u64 = if (T == f32)
        @as(u64, @as(u32, @bitCast(float)))
    else
        @bitCast(float);
    const result = floatToStrBytes(&buf, val_bits, T == f32);

    return RocStr.init(result.ptr, result.len, roc_ops);
}

/// Format a Roc float into caller-owned scratch bytes.
pub fn floatToStrBytes(buf: []u8, val_bits: u64, is_f32: bool) []const u8 {
    return if (is_f32) blk: {
        const f32_val: f32 = @bitCast(@as(u32, @truncate(val_bits)));
        break :blk compiler_rt_128.f32_to_str(buf, f32_val);
    } else blk: {
        const f64_val: f64 = @bitCast(val_bits);
        break :blk compiler_rt_128.f64_to_str(buf, f64_val);
    };
}

const FloatToStrTestError = error{
    InvalidCharacter,
    TestExpectedEqual,
};

fn expectFloatToStr(comptime T: type, value: T, expected: []const u8) FloatToStrTestError!void {
    var buf: [400]u8 = undefined;
    const val_bits: u64 = if (T == f32)
        @as(u64, @as(u32, @bitCast(value)))
    else
        @bitCast(value);
    const actual = floatToStrBytes(&buf, val_bits, T == f32);
    try testing.expectEqualStrings(expected, actual);

    if (std.math.isFinite(value)) {
        const parsed = try parse_float.parseFloat(T, actual);
        const Bits = std.meta.Int(.unsigned, @bitSizeOf(T));
        try testing.expectEqual(@as(Bits, @bitCast(value)), @as(Bits, @bitCast(parsed)));
    }
}

test "floatToStrBytes emits stable shortest representations" {
    try expectFloatToStr(f32, @as(f32, 0.0), "0");
    try expectFloatToStr(f32, @as(f32, -0.0), "-0");
    try expectFloatToStr(f32, @as(f32, 1.5), "1.5");
    try expectFloatToStr(f32, @as(f32, 0.1), "0.1");
    try expectFloatToStr(f32, @as(f32, @bitCast(@as(u32, 0x00000001))), "1e-45");
    try expectFloatToStr(f32, std.math.floatMax(f32), "3.4028235e38");
    try expectFloatToStr(f32, std.math.inf(f32), "inf");
    try expectFloatToStr(f32, -std.math.inf(f32), "-inf");
    try expectFloatToStr(f32, std.math.nan(f32), "nan");

    try expectFloatToStr(f64, @as(f64, 0.0), "0");
    try expectFloatToStr(f64, @as(f64, -0.0), "-0");
    try expectFloatToStr(f64, @as(f64, 1.5), "1.5");
    try expectFloatToStr(f64, @as(f64, 0.1), "0.1");
    try expectFloatToStr(f64, @as(f64, @bitCast(@as(u64, 0x0000000000000001))), "5e-324");
    try expectFloatToStr(f64, std.math.floatMax(f64), "1.7976931348623157e308");
    try expectFloatToStr(f64, std.math.inf(f64), "inf");
    try expectFloatToStr(f64, -std.math.inf(f64), "-inf");
    try expectFloatToStr(f64, std.math.nan(f64), "nan");
}

/// Format a Roc float into a RocStr using the same implementation used by
/// generated builtin calls.
pub fn floatToStrFromBits(val_bits: u64, is_f32: bool, roc_ops: *RocOps) RocStr {
    var buf: [400]u8 = undefined;
    const result = floatToStrBytes(&buf, val_bits, is_f32);
    return RocStr.init(result.ptr, result.len, roc_ops);
}

// Str.splitOn
/// TODO: Document strSplitOn.
pub fn strSplitOn(
    string: RocStr,
    delimiter: RocStr,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const segment_count = countSegments(string, delimiter);
    const list = RocList.list_allocate(@alignOf(RocStr), segment_count, @sizeOf(RocStr), true, roc_ops);

    if (list.bytes) |bytes| {
        const strings: [*]RocStr = utils.alignedPtrCast([*]RocStr, bytes, @src());
        strSplitOnHelp(strings, string, delimiter, roc_ops);
    }

    return list;
}

/// TODO
pub fn strSplitOnHelp(
    array: [*]RocStr,
    string: RocStr,
    delimiter: RocStr,
    roc_ops: *RocOps,
) void {
    if (delimiter.len() == 0) {
        string.incref(1, roc_ops);
        array[0] = string;
        return;
    }

    var it = std.mem.splitSequence(u8, string.asSlice(), delimiter.asSlice());

    var i: usize = 0;
    var offset: usize = 0;

    while (it.next()) |zig_slice| : (i += 1) {
        const slice_offset = @intFromPtr(zig_slice.ptr) - @intFromPtr(string.asSlice().ptr);
        const roc_slice = substringUnsafe(string, slice_offset, zig_slice.len, roc_ops);
        array[i] = roc_slice;
        offset = slice_offset + zig_slice.len + delimiter.len();
    }

    // Correct refcount for all of the splits made.
    string.incref(i, roc_ops); // i == array.len()
}

// This is used for `Str.splitOn : Str, Str -> List Str
// It is used to count how many segments the input `_str`
// needs to be broken into, so that we can allocate a array
// of that size. It always returns at least 1.
/// TODO: Document countSegments.
pub fn countSegments(string: RocStr, delimiter: RocStr) callconv(.c) usize {
    if (delimiter.isEmpty()) {
        return 1;
    }

    var it = std.mem.splitSequence(u8, string.asSlice(), delimiter.asSlice());
    var count: usize = 0;

    while (it.next()) |_| : (count += 1) {}

    return count;
}

/// TODO: Document countUtf8Bytes.
pub fn countUtf8Bytes(string: RocStr) callconv(.c) u64 {
    return @intCast(string.len());
}

/// TODO: Document isEmpty.
pub fn isEmpty(string: RocStr) callconv(.c) bool {
    return string.isEmpty();
}

/// TODO: Document getCapacity.
pub fn getCapacity(string: RocStr) callconv(.c) usize {
    return string.getCapacity();
}

/// Str.substring - extracts a substring without bounds checking.
///
/// ## Ownership
/// - `string`: **borrows** - caller retains ownership
/// - Returns: **seamless-slice** - shares data with input string
///
/// **IMPORTANT**: This function does NOT call incref. The returned seamless
/// slice shares the input's allocation, but the caller is responsible for
/// ensuring the refcount is correct. This is typically used internally where
/// the caller handles refcount management.
///
/// For small strings: creates a new small string (copy).
/// For heap strings at start=0 with unique refcount: shrinks in place.
/// Otherwise: creates a seamless slice pointing into the original string.
pub fn substringUnsafeC(
    string: RocStr,
    start_u64: u64,
    length_u64: u64,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    const start: usize = @intCast(start_u64);
    const length: usize = @intCast(length_u64);

    return substringUnsafe(string, start, length, roc_ops);
}

/// Result layout for `Str.find_first`.
pub const FindFirstResult = struct {
    before: RocStr,
    found: bool,
    after: RocStr,
};

/// Result layout for `Str.drop_prefix_caseless_ascii`.
pub const DropPrefixCaselessAsciiResult = struct {
    after: RocStr,
    found: bool,
};

fn retainedSlice(source: RocStr, start: usize, length: usize, roc_ops: *RocOps) RocStr {
    if (length == 0) return RocStr.empty();

    source.incref(1, roc_ops);
    return substringUnsafe(source, start, length, roc_ops);
}

fn smallStringFromPtr(bytes: [*]const u8, length: usize) RocStr {
    std.debug.assert(length < SMALL_STRING_SIZE);

    var result = RocStr.empty();
    result.setLen(length);
    @memcpy(result.asU8ptrMut()[0..length], bytes[0..length]);
    return result;
}

/// Find the first delimiter occurrence and return seamless slices around it.
pub fn findFirst(source: RocStr, delimiter: RocStr, roc_ops: *RocOps) FindFirstResult {
    const source_bytes = source.asSlice();
    const delimiter_bytes = delimiter.asSlice();

    if (delimiter_bytes.len == 0) {
        return .{
            .before = RocStr.empty(),
            .found = true,
            .after = retainedSlice(source, 0, source_bytes.len, roc_ops),
        };
    }

    const index = std.mem.find(u8, source_bytes, delimiter_bytes) orelse {
        return .{
            .before = RocStr.empty(),
            .found = false,
            .after = RocStr.empty(),
        };
    };

    return .{
        .before = retainedSlice(source, 0, index, roc_ops),
        .found = true,
        .after = retainedSlice(source, index + delimiter_bytes.len, source_bytes.len - index - delimiter_bytes.len, roc_ops),
    };
}

/// See substringUnsafeC for ownership documentation.
pub fn substringUnsafe(
    string: RocStr,
    start: usize,
    length: usize,
    _: *RocOps,
) RocStr {
    if (string.isSmallStr()) {
        if (start == 0) {
            var output = string;
            output.setLen(length);
            return output;
        }
        return smallStringFromPtr(string.asU8ptr() + start, length);
    }
    if (string.bytes) |source_ptr| {
        if (start == 0 and string.isUnique()) {
            var output = string;
            output.setLen(length);
            return output;
        } else {
            const alloc_ptr = if (string.isSeamlessSlice())
                string.capacity_or_alloc_ptr
            else
                RocStr.encodeSliceAllocationPtr(source_ptr);
            return RocStr{
                .bytes = source_ptr + start,
                .capacity_or_alloc_ptr = alloc_ptr,
                .length = length,
            };
        }
    }
    return RocStr.empty();
}

/// TODO: Document getUnsafeC.
pub fn getUnsafeC(string: RocStr, index: u64) callconv(.c) u8 {
    return string.getUnchecked(@intCast(index));
}

// Str.startsWith
/// TODO: Document startsWith.
pub fn startsWith(string: RocStr, prefix: RocStr) callconv(.c) bool {
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

/// Str.drop_prefix - Returns string with prefix removed, or original if no match.
///
/// ## Ownership
/// - `string`: **borrows** - caller retains ownership
/// - `prefix`: **borrows** - caller retains ownership
/// - Returns: **seamless-slice** - shares data with input string
///
/// If prefix doesn't match, returns the original string.
/// If prefix matches, returns a seamless slice of the remaining portion.
/// The caller is responsible for retaining `string` when the result is owned.
pub fn strDropPrefix(
    string: RocStr,
    prefix: RocStr,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    if (!startsWith(string, prefix)) {
        return string;
    }

    const prefix_len = prefix.len();
    const new_len = string.len() - prefix_len;

    return substringUnsafe(string, prefix_len, new_len, roc_ops);
}

/// Drop a prefix using the same ASCII-caseless semantics as
/// `strCaselessAsciiEquals`.
///
/// On success, the returned `after` string is a retained seamless slice of
/// `string`. On failure, `after` is empty and the source string is not retained.
/// This shape is important for parser field dispatch: a miss must be only byte
/// comparison work, not temporary string construction or refcount traffic.
pub fn strDropPrefixCaselessAscii(
    string: RocStr,
    prefix: RocStr,
    roc_ops: *RocOps,
) DropPrefixCaselessAsciiResult {
    const string_len = string.len();
    const prefix_len = prefix.len();

    if (prefix_len > string_len) {
        return .{ .after = RocStr.empty(), .found = false };
    }

    if (!bytesCaselessAsciiEqualFast(string.asU8ptr(), prefix.asU8ptr(), prefix_len)) {
        return .{ .after = RocStr.empty(), .found = false };
    }

    const after_len = string_len - prefix_len;
    string.incref(1, roc_ops);
    return .{
        .after = substringUnsafe(string, prefix_len, after_len, roc_ops),
        .found = true,
    };
}

/// Str.drop_suffix - Returns string with suffix removed, or original if no match.
///
/// ## Ownership
/// - `string`: **borrows** - caller retains ownership
/// - `suffix`: **borrows** - caller retains ownership
/// - Returns: **seamless-slice** - shares data with input string
///
/// If suffix doesn't match, returns the original string.
/// If suffix matches, returns a seamless slice of the remaining portion.
/// The caller is responsible for retaining `string` when the result is owned.
pub fn strDropSuffix(
    string: RocStr,
    suffix: RocStr,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    if (!endsWith(string, suffix)) {
        return string;
    }

    const suffix_len = suffix.len();
    const new_len = string.len() - suffix_len;

    return substringUnsafe(string, 0, new_len, roc_ops);
}

// Str.repeat
/// TODO: Document repeatC.
pub fn repeatC(
    string: RocStr,
    count_u64: u64,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    const count: usize = @intCast(count_u64);
    const bytes_len = string.len();
    if (count == 0 or bytes_len == 0) {
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptr();

    var ret_string = RocStr.allocate(count * bytes_len, roc_ops);
    var ret_string_ptr = ret_string.asU8ptrMut();

    var i: usize = 0;
    while (i < count) : (i += 1) {
        @memcpy(ret_string_ptr[0..bytes_len], bytes_ptr[0..bytes_len]);
        ret_string_ptr += bytes_len;
    }

    return ret_string;
}

test "repeatC: empty string short-circuits" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const repeated = repeatC(RocStr.empty(), std.math.maxInt(u64), test_env.getOps());
    defer repeated.decref(test_env.getOps());

    try std.testing.expect(repeated.eql(RocStr.empty()));
}

/// Str.endsWith
pub fn endsWith(string: RocStr, suffix: RocStr) callconv(.c) bool {
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

/// Str.concat - concatenates two strings.
///
/// ## Ownership
/// - `arg1`: **consumes** - may be reallocated if capacity insufficient
/// - `arg2`: **borrows** - caller retains ownership (not decrefd here)
/// - Returns: **independent** or **copy-on-write** depending on arg1's capacity
///
/// Note: arg1 is owned and may be returned directly if arg2 is empty,
/// or reallocated to accommodate the combined content.
///
/// An `.InPlace` update mode means the caller proved arg1 unique, so the
/// runtime uniqueness check inside the reallocation is skipped. Small strings
/// have no refcount, so the mode only affects big strings.
pub fn strConcatC(
    arg1: RocStr,
    arg2: RocStr,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    return @call(.always_inline, strConcat, .{ arg1, arg2, update_mode, roc_ops });
}

/// See strConcatC for ownership documentation.
pub fn strConcat(
    arg1: RocStr,
    arg2: RocStr,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) RocStr {
    // NOTE: we don't special-case the first argument being empty. That is because it is owned and
    // may have sufficient capacity to store the rest of the list.
    if (arg2.isEmpty()) {
        // the first argument is owned, so we can return it without cloning
        return arg1;
    } else {
        const combined_length = arg1.len() + arg2.len();

        var result = arg1.reallocate(combined_length, update_mode, roc_ops);
        const src = arg2.asU8ptr()[0..arg2.len()];
        const dest = result.asU8ptrMut()[arg1.len()..combined_length];
        var i = src.len;
        while (i > 0) {
            i -= 1;
            dest[i] = src[i];
        }

        return result;
    }
}

/// Str.contains
pub fn strContains(haystack: RocStr, needle: RocStr) callconv(.c) bool {
    return std.mem.find(u8, haystack.asSlice(), needle.asSlice()) != null;
}

/// TODO: Document RocListStr.
pub const RocListStr = extern struct {
    list_elements: ?[*]RocStr,
    list_length: usize,
    list_capacity_or_alloc_ptr: usize,
};

/// Str.joinWith - joins a list of strings with a separator.
///
/// ## Ownership
/// - `list`: **consumes** - elements are borrowed, list is consumed
/// - `separator`: **borrows** - caller retains ownership
/// - Returns: **independent** - new allocation containing joined result
pub fn strJoinWithC(
    list: RocList,
    separator: RocStr,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    const list_elements: ?[*]RocStr = if (list.bytes) |bytes|
        utils.alignedPtrCast([*]RocStr, bytes, @src())
    else
        null;
    const roc_list_str = RocListStr{
        .list_elements = list_elements,
        .list_length = list.length,
        .list_capacity_or_alloc_ptr = list.capacity_or_alloc_ptr,
    };

    const result = @call(.always_inline, strJoinWith, .{ roc_list_str, separator, roc_ops });

    // Decref the consumed list. The list owns each element string, so when it is
    // unique we decref the elements before freeing the backing allocation.
    //
    // We do this inline with direct `RocStr.decref` calls rather than handing a
    // `&strDecref` callback to `RocList.decref`. Taking the address of that
    // internal builtin and calling it through a pointer is misresolved by the
    // COFF linker: the function pointer ends up aimed into `.rdata` instead of
    // `.text`, so the element loop never runs and every heap element string
    // leaks (verified on x64win). Direct calls relocate correctly, and this
    // mirrors `RocList.decref`'s own element-teardown logic for `List Str`.
    if (list.isUnique(roc_ops)) {
        if (list.getAllocationDataPtr(roc_ops)) |source| {
            const count = list.getAllocationElementCount(true, roc_ops);
            const elems: [*]RocStr = utils.alignedPtrCast([*]RocStr, source, @src());
            var i: usize = 0;
            while (i < count) : (i += 1) {
                elems[i].decref(roc_ops);
            }
        }
    }
    // Free the list's backing allocation. We pass elements_refcounted=true so the
    // allocation header is located the same way it was at allocation time; the
    // elements themselves were already decref'd above.
    utils.decref(
        list.getAllocationDataPtr(roc_ops),
        list.capacity_or_alloc_ptr,
        @alignOf(RocStr),
        true,
        .atomic,
        roc_ops,
    );

    return result;
}

/// See strJoinWithC for ownership documentation.
pub fn strJoinWith(
    list: RocListStr,
    separator: RocStr,
    roc_ops: *RocOps,
) RocStr {
    const len = list.list_length;

    if (len == 0) {
        return RocStr.empty();
    } else {
        const ptr = @as([*]RocStr, @ptrCast(list.list_elements));
        const slice: []RocStr = ptr[0..len];

        // determine the size of the result
        var total_size: usize = 0;
        for (slice) |substr| {
            total_size += substr.len();
        }

        // include size of the separator
        total_size += separator.len() * (len - 1);

        var result = RocStr.allocate(total_size, roc_ops);
        const result_ptr = result.asU8ptrMut();

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

/// Str.toUtf8 - converts a string to a list of UTF-8 bytes.
///
/// ## Ownership
/// - `arg`: **borrows** - caller retains ownership
/// - Returns: **seamless-slice** - shares underlying data with input string
///
/// For heap strings, the returned list shares the same underlying allocation
/// as the input string. This function calls `incref` on the allocation to
/// account for the new reference. Small strings are copied to a new allocation.
///
/// The caller must decref the argument after call (we borrowed it but added
/// a reference to its data via the returned list).
pub fn strToUtf8C(
    arg: RocStr,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    return strToBytes(arg, roc_ops);
}

inline fn strToBytes(
    arg: RocStr,
    roc_ops: *RocOps,
) RocList {
    const length = arg.len();
    if (length == 0) {
        return RocList.empty();
    } else if (arg.isSmallStr()) {
        const ptr = @import("utils.zig").allocateWithRefcount(length, RocStr.alignment, false, roc_ops);

        @memcpy(ptr[0..length], arg.asU8ptr()[0..length]);

        return RocList{ .length = length, .bytes = ptr, .capacity_or_alloc_ptr = RocList.encodeCapacity(length) };
    } else {
        // The returned list shares the same underlying allocation as the string.
        // We must incref the allocation since there's now an additional reference to it.
        arg.incref(1, roc_ops);
        const list_capacity_or_alloc_ptr = if (arg.isSeamlessSlice()) blk: {
            const alloc_ptr = arg.getAllocationPtr() orelse return RocList.empty();
            break :blk RocList.encodeSliceAllocationPtr(alloc_ptr);
        } else arg.capacity_or_alloc_ptr;
        return RocList{ .length = length, .bytes = arg.bytes, .capacity_or_alloc_ptr = list_capacity_or_alloc_ptr };
    }
}

/// TODO
pub const FromUtf8Try = extern struct {
    byte_index: u64,
    string: RocStr,
    is_ok: bool,
    problem_code: Utf8ByteProblem,
};

/// TODO: Document fromUtf8C.
pub fn fromUtf8C(
    list: RocList,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) FromUtf8Try {
    return fromUtf8(list, update_mode, roc_ops);
}

const UNICODE_REPLACEMENT: u21 = 0xfffd;

const Utf8Iterator = struct {
    bytes: []u8,
    i: usize,

    pub fn init(list: RocList) Utf8Iterator {
        const bytes = @as([*]u8, @ptrCast(list.bytes))[0..list.length];
        return Utf8Iterator{
            .bytes = bytes,
            .i = 0,
        };
    }

    pub fn nextLossy(it: *Utf8Iterator) ?u32 {
        if (it.bytes.len <= it.i) {
            return null;
        }

        const rest = it.bytes[it.i..];
        const n = unicode.utf8ByteSequenceLength(rest[0]) catch {
            // invalid start byte
            it.i += 1;
            it.skipAdjacentInvalidStartBytes();
            return UNICODE_REPLACEMENT;
        };

        for (1..n) |i| {
            if (rest.len == i) {
                // unexpected end
                it.i += i;
                return UNICODE_REPLACEMENT;
            }
            if ((rest[i] & 0xC0) != 0x80) {
                // expected continuation byte (0b10xx_xxxx)
                it.i += i;
                return UNICODE_REPLACEMENT;
            }
        }

        it.i += n;
        return unicode.utf8Decode(rest[0..n]) catch {
            return UNICODE_REPLACEMENT;
        };
    }

    fn skipAdjacentInvalidStartBytes(it: *Utf8Iterator) void {
        while (it.i < it.bytes.len) {
            const byte = it.bytes[it.i];
            if (byte < 0x80) return;
            _ = unicode.utf8ByteSequenceLength(byte) catch {
                it.i += 1;
                continue;
            };
            return;
        }
    }

    pub fn reset(it: *Utf8Iterator) void {
        it.i = 0;
    }
};

fn codepointSeqLengthLossy(c: u32) u3 {
    if (c < 0x110000) {
        if (unicode.utf8CodepointSequenceLength(@intCast(c))) |n| {
            return n;
        } else |_| {
            // fallthrough
        }
    }
    return unicode.utf8CodepointSequenceLength(UNICODE_REPLACEMENT) catch unreachable;
}

fn utf8EncodeLossy(c: u32, out: []u8) u3 {
    if (c < 0x110000) {
        if (unicode.utf8Encode(@intCast(c), out)) |n| {
            return n;
        } else |_| {
            // fallthrough
        }
    }
    return unicode.utf8Encode(UNICODE_REPLACEMENT, out) catch unreachable;
}

/// TODO: Document fromUtf8Lossy.
pub fn fromUtf8Lossy(
    list: RocList,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    if (list.len() == 0) {
        return RocStr.empty();
    }

    // PERF: we could try to reuse the input list if it's already valid utf-8, similar to fromUtf8

    var it = Utf8Iterator.init(list);

    var enc_len: usize = 0;
    while (it.nextLossy()) |c| {
        enc_len += codepointSeqLengthLossy(c);
    }

    var str = RocStr.allocate(enc_len, roc_ops);
    const ptr = str.asU8ptrMut()[0..enc_len];
    var end_index: usize = 0;
    it.reset();
    while (it.nextLossy()) |c| {
        end_index += utf8EncodeLossy(c, ptr[end_index..]);
    }
    str.setLen(end_index);

    return str;
}

/// TODO: Document fromUtf8.
pub fn fromUtf8(
    list: RocList,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) FromUtf8Try {
    if (list.len() == 0) {
        return FromUtf8Try{
            .is_ok = true,
            .string = RocStr.empty(),
            .byte_index = 0,
            .problem_code = Utf8ByteProblem.InvalidStartByte,
        };
    }
    const bytes = @as([*]const u8, @ptrCast(list.bytes))[0..list.len()];

    if (isValidUnicode(bytes)) {
        // Borrowed-call semantics: the returned string must own its bytes
        // independently of the caller's list value. Increment first so
        // `fromSubListUnsafe` cannot take over a unique list allocation.
        list.incref(1, false, roc_ops);
        const string = RocStr.fromSubListUnsafe(list, 0, list.len(), update_mode, roc_ops);
        return FromUtf8Try{
            .is_ok = true,
            .string = string,
            .byte_index = 0,
            .problem_code = Utf8ByteProblem.InvalidStartByte,
        };
    } else {
        const temp = errorToProblem(bytes);

        return FromUtf8Try{
            .is_ok = false,
            .string = RocStr.empty(),
            .byte_index = @intCast(temp.index),
            .problem_code = temp.problem,
        };
    }
}

fn errorToProblem(bytes: []const u8) struct { index: usize, problem: Utf8ByteProblem } {
    const len = bytes.len;
    var index: usize = 0;

    while (index < len) {
        const nextNumBytes = numberOfNextCodepointBytes(bytes, index) catch |err| {
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

/// Returns true if the given buffer contains valid Unicode data.
pub fn isValidUnicode(buf: []const u8) bool {
    const size = @sizeOf(u64);
    // TODO: we should test changing the step on other platforms.
    // The general tradeoff is making extremely large strings potentially much faster
    // at the cost of small strings being slightly slower.
    const step = size;
    var i: usize = 0;
    while (i + step < buf.len) {
        var bytes: u64 = undefined;
        @memcpy(@as([*]u8, @ptrCast(&bytes))[0..size], buf[i..(i + size)]);
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
            @memcpy(small_buf[0..4], buf[i..(i + 4)]);
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

    return @call(.always_inline, unicode.utf8ValidateSlice, .{buf[i..]});
}

/// TODO
pub const Utf8DecodeError = error{
    UnexpectedEof,
    Utf8InvalidStartByte,
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8EncodesSurrogateHalf,
    Utf8CodepointTooLarge,
};

/// Returns the number of bytes in the next codepoint starting at the given index in the byte slice.
/// Returns an error if the bytes are not valid UTF-8.
///
/// Essentially unicode.utf8ValidateSlice -> https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L156
/// but only for the next codepoint from the index. Then we return the number of bytes of that codepoint.
///
/// TODO: we only ever use the values 0-4, so can we use smaller int than `usize`?
pub fn numberOfNextCodepointBytes(bytes: []const u8, index: usize) Utf8DecodeError!usize {
    const codepoint_len = try unicode.utf8ByteSequenceLength(bytes[index]);
    const codepoint_end_index = index + codepoint_len;
    if (codepoint_end_index > bytes.len) {
        return error.UnexpectedEof;
    }
    const codepoint = try unicode.utf8Decode(bytes[index..codepoint_end_index]);
    if (codepoint > 0x10FFFF) {
        return error.Utf8CodepointTooLarge;
    }
    return codepoint_end_index - index;
}

/// Return types for validateUtf8Bytes
/// Values must be in alphabetical order. That is, lowest values are the first alphabetically.
pub const Utf8ByteProblem = enum(u8) {
    CodepointTooLarge = 0,
    EncodesSurrogateHalf = 1,
    ExpectedContinuation = 2,
    InvalidStartByte = 3,
    OverlongEncoding = 4,
    UnexpectedEndOfSequence = 5,
};

/// TODO
pub fn validateUtf8Bytes(
    bytes: [*]u8,
    length: usize,
    roc_ops: *RocOps,
) FromUtf8Try {
    return fromUtf8(RocList{ .bytes = bytes, .length = length, .capacity_or_alloc_ptr = RocList.encodeCapacity(length) }, .Immutable, roc_ops);
}

/// TODO
pub fn validateUtf8BytesX(
    str: RocList,
    roc_ops: *RocOps,
) FromUtf8Try {
    return fromUtf8(str, .Immutable, roc_ops);
}

/// TODO
pub fn sliceHelp(
    bytes: [*]const u8,
    length: usize,
    roc_ops: *RocOps,
) RocList {
    var list = RocList.list_allocate(RocStr.alignment, length, @sizeOf(u8), false, roc_ops);
    var list_bytes = list.bytes orelse unreachable;
    @memcpy(list_bytes[0..length], bytes[0..length]);
    list.length = length;

    return list;
}

/// TODO
pub fn toErrUtf8ByteResponse(index: usize, problem: Utf8ByteProblem) FromUtf8Try {
    return FromUtf8Try{ .is_ok = false, .string = RocStr.empty(), .byte_index = @as(u64, @intCast(index)), .problem_code = problem };
}

// NOTE on memory: the validate function consumes a RC token of the input. Since
// we freshly created it (in `sliceHelp`), it has only one RC token, and input list will be deallocated.
//
// If we tested with big strings, we'd have to deallocate the output string, but never the input list

/// TODO
pub fn isWhitespace(codepoint: u21) bool {
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

/// Str.trim - removes leading and trailing whitespace.
///
/// ## Ownership
/// - `input_string`: **consumes** - caller loses ownership
/// - Returns: **copy-on-write** or **seamless-slice** depending on input
///
/// Behavior depends on input state:
/// - Empty string: returns empty (decrefs input if heap-allocated)
/// - Small string: creates new small string with trimmed bytes
/// - Unique with no leading whitespace: shrinks in place (same allocation)
/// - Otherwise: creates seamless slice pointing to trimmed region
///
/// An `.InPlace` update mode means the caller proved the string unique, so
/// the big-string runtime uniqueness check is skipped.
pub fn strTrim(
    input_string: RocStr,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const leading_bytes = countLeadingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == leading_bytes) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const trailing_bytes = countTrailingWhitespaceBytes(string);
    const new_len = original_len - leading_bytes - trailing_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return smallStringFromPtr(string.asU8ptr() + leading_bytes, new_len);
    } else if (leading_bytes == 0 and (update_mode == .InPlace or string.isUnique())) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.length = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .bytes = bytes_ptr + leading_bytes,
            .capacity_or_alloc_ptr = string.capacity_or_alloc_ptr,
            .length = new_len,
        };
    } else {
        // Not unique or removing leading bytes, just make a slice.
        return RocStr{
            .bytes = bytes_ptr + leading_bytes,
            .capacity_or_alloc_ptr = RocStr.encodeSliceAllocationPtr(bytes_ptr),
            .length = new_len,
        };
    }
}

/// Str.trim_start - removes leading whitespace.
///
/// ## Ownership
/// - `input_string`: **consumes** - caller loses ownership
/// - Returns: **copy-on-write** or **seamless-slice** depending on input
///
/// Behavior depends on input state:
/// - Empty string: returns empty (decrefs input if heap-allocated)
/// - Small string: creates new small string with trimmed bytes
/// - Unique with no leading whitespace: returns same allocation unchanged
/// - Otherwise: creates seamless slice pointing to trimmed region
///
/// An `.InPlace` update mode means the caller proved the string unique, so
/// the big-string runtime uniqueness check is skipped.
pub fn strTrimStart(
    input_string: RocStr,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const leading_bytes = countLeadingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == leading_bytes) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const new_len = original_len - leading_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return smallStringFromPtr(string.asU8ptr() + leading_bytes, new_len);
    } else if (leading_bytes == 0 and (update_mode == .InPlace or string.isUnique())) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.length = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .bytes = bytes_ptr + leading_bytes,
            .capacity_or_alloc_ptr = string.capacity_or_alloc_ptr,
            .length = new_len,
        };
    } else {
        // Not unique or removing leading bytes, just make a slice.
        return RocStr{
            .bytes = bytes_ptr + leading_bytes,
            .capacity_or_alloc_ptr = RocStr.encodeSliceAllocationPtr(bytes_ptr),
            .length = new_len,
        };
    }
}

/// Str.trim_end - removes trailing whitespace.
///
/// ## Ownership
/// - `input_string`: **consumes** - caller loses ownership
/// - Returns: **copy-on-write** - may be same allocation if unique
///
/// Behavior depends on input state:
/// - Empty string: returns empty (decrefs input if heap-allocated)
/// - Small string: creates new small string with trimmed bytes
/// - Unique: shrinks length in place (same allocation)
/// - Shared: creates seamless slice pointing to trimmed region
///
/// An `.InPlace` update mode means the caller proved the string unique, so
/// the big-string runtime uniqueness check is skipped.
pub fn strTrimEnd(
    input_string: RocStr,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const trailing_bytes = countTrailingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == trailing_bytes) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const new_len = original_len - trailing_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return smallStringFromPtr(string.asU8ptr(), new_len);
    } else if (update_mode == .InPlace or string.isUnique()) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.length = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .bytes = bytes_ptr,
            .capacity_or_alloc_ptr = string.capacity_or_alloc_ptr,
            .length = new_len,
        };
    } else {
        // Not unique, just make a slice.
        return RocStr{
            .bytes = bytes_ptr,
            .capacity_or_alloc_ptr = RocStr.encodeSliceAllocationPtr(bytes_ptr),
            .length = new_len,
        };
    }
}

fn countLeadingWhitespaceBytes(string: RocStr) usize {
    var byte_count: usize = 0;

    const bytes = string.asU8ptr()[0..string.len()];
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

    const bytes = string.asU8ptr()[0..string.len()];
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

/// Str.with_ascii_lowercased
///
/// Returns a string with all ASCII letters converted to lowercase.
///
/// ## Ownership
/// - `string`: **consumes** - caller loses ownership
/// - Returns: **copy-on-write** - may be same allocation if input was unique
///
/// If the input string is unique, modifies in place and returns it.
/// If shared, decrefs the input and allocates a new string.
///
/// An `.InPlace` update mode means the caller proved the string unique, so
/// the runtime uniqueness check is skipped. Small strings always report
/// unique, so the mode changes nothing for them.
pub fn strWithAsciiLowercased(
    string: RocStr,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    var new_str = if (update_mode == .InPlace or string.isUnique())
        string
    else blk: {
        string.decref(roc_ops);
        break :blk RocStr.fromSlice(string.asSlice(), roc_ops);
    };

    const new_str_bytes = new_str.asU8ptrMut()[0..string.len()];
    for (new_str_bytes) |*c| {
        c.* = ascii.toLower(c.*);
    }
    return new_str;
}

/// Str.with_ascii_uppercased
///
/// Returns a string with all ASCII letters converted to uppercase.
///
/// ## Ownership
/// - `string`: **consumes** - caller loses ownership
/// - Returns: **copy-on-write** - may be same allocation if input was unique
///
/// If the input string is unique, modifies in place and returns it.
/// If shared, decrefs the input and allocates a new string.
///
/// An `.InPlace` update mode means the caller proved the string unique, so
/// the runtime uniqueness check is skipped. Small strings always report
/// unique, so the mode changes nothing for them.
pub fn strWithAsciiUppercased(
    string: RocStr,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    var new_str = if (update_mode == .InPlace or string.isUnique())
        string
    else blk: {
        string.decref(roc_ops);
        break :blk RocStr.fromSlice(string.asSlice(), roc_ops);
    };

    const new_str_bytes = new_str.asU8ptrMut()[0..string.len()];
    for (new_str_bytes) |*c| {
        c.* = ascii.toUpper(c.*);
    }
    return new_str;
}

/// TODO: Document strCaselessAsciiEquals.
pub fn strCaselessAsciiEquals(self: RocStr, other: RocStr) callconv(.c) bool {
    if (self.bytes == other.bytes and self.length == other.length) {
        return true;
    }

    const self_len = self.len();
    if (self_len != other.len()) return false;

    if (self_len > 0 and self_len < @sizeOf(u64) and self.isSmallStr() and other.isSmallStr()) {
        const left_word = std.mem.readInt(u64, self.asU8ptr()[0..@sizeOf(u64)], .little);
        const right_word = std.mem.readInt(u64, other.asU8ptr()[0..@sizeOf(u64)], .little);
        const active = (@as(u64, 1) << @intCast(self_len * 8)) - 1;
        return wordCaselessAsciiEqualMasked(left_word, right_word, active);
    }

    return bytesCaselessAsciiEqualFast(self.asU8ptr(), other.asU8ptr(), self_len);
}

inline fn bytesCaselessAsciiEqualFast(left: [*]const u8, right: [*]const u8, len: usize) bool {
    const word_size = @sizeOf(u64);

    var index: usize = 0;
    while (index + word_size <= len) : (index += word_size) {
        const left_word = std.mem.readInt(u64, left[index..][0..word_size], .little);
        const right_word = std.mem.readInt(u64, right[index..][0..word_size], .little);
        if (left_word != right_word and !wordCaselessAsciiEqual(left_word, right_word)) return false;
    }

    const tail_len = len - index;
    if (tail_len != 0) {
        const active = (@as(u64, 1) << @intCast(tail_len * 8)) - 1;
        const left_word = readTailU64(left, len, index, tail_len);
        const right_word = readTailU64(right, len, index, tail_len);
        return wordCaselessAsciiEqualMasked(left_word, right_word, active);
    }

    return true;
}

inline fn wordCaselessAsciiEqual(left: u64, right: u64) bool {
    return wordCaselessAsciiEqualMasked(left, right, std.math.maxInt(u64));
}

// SWAR = SIMD Within A Register. These routines compare eight independent byte
// lanes inside one u64. This is deliberately written in terms of fixed u64
// masks and shifts, not slice loops or higher-level ASCII helpers, because this
// is on a hot path for record-field dispatch in parsers such as HTTP headers.
//
// The machine shape we want is:
//   1. one unaligned 8-byte load from each string,
//   2. one xor to find differing bytes,
//   3. a small fixed sequence of integer mask operations,
//   4. no per-byte branch unless we are in the rare short non-small tail path.
//
// ASCII case-insensitive equality per byte is:
//   - equal bytes are always equal, even for punctuation, underscores, UTF-8
//     continuation bytes, or bytes with the high bit set;
//   - differing bytes must differ by exactly 0x20;
//   - bytes that differ by 0x20 are equal only when they are ASCII letters.
//
// This is intentionally narrower than "can I lowercase both whole words with
// bit 0x20?" We do not need to prove that exact-equal bytes are lowercase-safe.
// That matters for field names such as "x_auth_token" and for UTF-8 bytes that
// are identical on both sides.
const SWAR_ONES: u64 = 0x0101010101010101;
const SWAR_LOW7: u64 = 0x7f7f7f7f7f7f7f7f;
const SWAR_HIGHS: u64 = 0x8080808080808080;
const SWAR_ASCII_CASE: u64 = 0x2020202020202020;

inline fn swarSplat(byte: u8) u64 {
    return @as(u64, byte) * SWAR_ONES;
}

// Return 0x80 in each byte lane where that lane is zero, else 0x00.
//
// This is the classic has-zero-byte trick, expressed so the optimizer sees a
// fixed u64 dataflow. Masking with SWAR_LOW7 keeps carries inside each 7-bit
// lane from depending on the input high bit; OR-ing the original word back in
// makes non-ASCII bytes non-zero for this predicate.
inline fn swarZeroHigh(word: u64) u64 {
    return ~(((word & SWAR_LOW7) + SWAR_LOW7) | word) & SWAR_HIGHS;
}

// Return 0x80 in each byte lane equal to `byte`, else 0x00.
inline fn swarByteEq(word: u64, byte: u8) u64 {
    return swarZeroHigh(word ^ swarSplat(byte));
}

// Return 0x80 in each ASCII byte lane >= `lo`, else 0x00. High-bit bytes are
// always rejected. This is used only after the xor has proved that a lane differs
// by 0x20, so the only remaining question is whether that lane is a letter.
inline fn swarByteGeAscii(word: u64, lo: u8) u64 {
    return ((word & SWAR_LOW7) + swarSplat(0x80 - lo)) & ~word & SWAR_HIGHS;
}

// Return 0x80 in each ASCII byte lane <= `hi`, else 0x00. High-bit bytes are
// always rejected.
inline fn swarByteLeAscii(word: u64, hi: u8) u64 {
    return ~(((word & SWAR_LOW7) + swarSplat(0x7f - hi)) | word) & SWAR_HIGHS;
}

inline fn swarByteInAsciiRange(word: u64, lo: u8, hi: u8) u64 {
    return swarByteGeAscii(word, lo) & swarByteLeAscii(word, hi);
}

inline fn wordCaselessAsciiEqualMasked(left: u64, right: u64, active: u64) bool {
    // `active` has 0xff in the byte lanes that belong to the logical string and
    // 0x00 in ignored lanes. Full 8-byte chunks pass all ones; tails pass a low
    // byte mask. Masking the xor here means inactive tail bytes behave exactly
    // like equal bytes and disappear from all later predicates.
    const diff = (left ^ right) & active;
    if (diff == 0) return true;

    // Convert the active byte mask into one high bit per active lane. All SWAR
    // predicates below produce the same shape: 0x80 for "this lane passed."
    const active_highs = active & SWAR_HIGHS;

    // First prove that every active byte lane is either identical or differs by
    // exactly the ASCII case bit. If any lane differs by another amount, the
    // words cannot be caseless-ASCII-equal. This rejects cases such as "_" vs
    // "\x7f" before any alphabetic range test can accidentally accept them.
    const exact_bytes = swarZeroHigh(diff) & active_highs;
    const case_diff_bytes = swarByteEq(diff, 0x20) & active_highs;
    if ((exact_bytes | case_diff_bytes) != active_highs) return false;

    // Only lanes that differ by 0x20 need to be letters. Exact-equal lanes can
    // be anything at all, including underscores, dashes, digits, punctuation, or
    // non-ASCII UTF-8 bytes.
    //
    // It is enough to test `left | 0x20` because a byte pair whose xor is 0x20
    // has the same lowercase value on either side if it is an ASCII letter.
    const left_lower = left | SWAR_ASCII_CASE;
    const left_alpha = swarByteInAsciiRange(left_lower, 'a', 'z') & active_highs;
    return (case_diff_bytes & ~left_alpha) == 0;
}

inline fn readTailU64(bytes: [*]const u8, len: usize, index: usize, tail_len: usize) u64 {
    // For any string with at least one full u64, load the final u64 ending at
    // `len` and shift the requested tail down to byte lane 0. This avoids a
    // byte-building loop without reading past the logical end of an owned string
    // or seamless slice.
    if (len >= @sizeOf(u64)) {
        const load_index = len - @sizeOf(u64);
        const shift = (index - load_index) * 8;
        const word = std.mem.readInt(u64, bytes[load_index..][0..@sizeOf(u64)], .little);
        return word >> @intCast(shift);
    }

    // The only remaining case is a non-small string/slice shorter than 8 bytes.
    // A short seamless slice may sit at the end of its allocation, and RocStr
    // does not store the original allocation capacity for string slices, so do
    // not over-read here.
    return readTailU64ZeroPadded(bytes, index, tail_len);
}

inline fn readTailU64ZeroPadded(bytes: [*]const u8, index: usize, tail_len: usize) u64 {
    var word: u64 = 0;
    var i: usize = 0;
    while (i < tail_len) : (i += 1) {
        word |= @as(u64, bytes[index + i]) << @intCast(i * 8);
    }
    return word;
}

/// A backwards version of Utf8View from std.unicode
pub const ReverseUtf8View = struct {
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
            2 => unicode.utf8Decode2([2]u8{ slice[0], slice[1] }) catch unreachable,
            3 => unicode.utf8Decode3([3]u8{ slice[0], slice[1], slice[2] }) catch unreachable,
            4 => unicode.utf8Decode4([4]u8{ slice[0], slice[1], slice[2], slice[3] }) catch unreachable,
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

/// Ensures the RocStr has at least the specified spare capacity, reallocating if necessary.
///
/// An `.InPlace` update mode means the caller proved the string unique, so
/// the big-string runtime uniqueness check inside the reallocation is skipped.
pub fn reserveC(
    string: RocStr,
    spare_u64: u64,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    return reserve(string, @intCast(spare_u64), update_mode, roc_ops);
}

/// See reserveC.
pub fn reserve(
    string: RocStr,
    spare: usize,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) RocStr {
    const old_length = string.len();

    if (string.getCapacity() >= old_length + spare) {
        return string;
    } else {
        var output = string.reallocate(old_length + spare, update_mode, roc_ops);
        output.setLen(old_length);
        return output;
    }
}

/// Creates a new RocStr with the specified capacity.
pub fn withCapacityC(
    capacity: u64,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    var str = RocStr.allocate(@intCast(capacity), roc_ops);
    str.setLen(0);
    return str;
}

/// Clones the contents of the given RocStr into the provided pointer, starting at the given offset and extra_offset.
pub fn strCloneTo(
    string: RocStr,
    ptr: [*]u8,
    offset: usize,
    extra_offset: usize,
) callconv(.c) usize {
    const WIDTH: usize = @sizeOf(RocStr);
    if (string.isSmallStr()) {
        const array: [@sizeOf(RocStr)]u8 = @as([@sizeOf(RocStr)]u8, @bitCast(string));

        var i: usize = 0;
        while (i < WIDTH) : (i += 1) {
            ptr[offset + i] = array[i];
        }

        return extra_offset;
    } else {
        const slice = string.asSlice();

        var relative = string;
        relative.bytes = @as(?[*]u8, @ptrFromInt(extra_offset)); // i.e. just after the string struct

        // write the string struct
        const array = relative.asArray();
        @memcpy(ptr[offset..(offset + WIDTH)], array[0..WIDTH]);

        // write the string bytes just after the struct
        @memcpy(ptr[extra_offset..(extra_offset + slice.len)], slice);

        return extra_offset + slice.len;
    }
}

/// Returns a pointer to the allocation backing the given RocStr
pub fn strAllocationPtr(
    string: RocStr,
) callconv(.c) ?[*]u8 {
    return string.getAllocationPtr();
}

/// Release excess capacity
///
/// An `.InPlace` update mode means the caller proved the string unique, so
/// the big-string runtime uniqueness check is skipped. Small strings have no
/// excess capacity and return unchanged regardless of mode.
pub fn strReleaseExcessCapacity(
    string: RocStr,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    const old_length = string.len();
    // We use the direct list.capacity_or_alloc_ptr to make sure both that there is no extra capacity and that it isn't a seamless slice.
    if (string.isSmallStr()) {
        // SmallStr has no excess capacity.
        return string;
    } else if ((update_mode == .InPlace or string.isUnique()) and !string.isSeamlessSlice() and string.getCapacity() == old_length) {
        return string;
    } else if (old_length == 0) {
        string.decref(roc_ops);
        return RocStr.empty();
    } else {
        var output = RocStr.allocateExact(old_length, roc_ops);
        const source_ptr = string.asU8ptr();
        const dest_ptr = output.asU8ptrMut();

        @memcpy(dest_ptr[0..old_length], source_ptr[0..old_length]);
        string.decref(roc_ops);

        return output;
    }
}

fn expectOk(result: FromUtf8Try) error{TestExpectedEqual}!void {
    try std.testing.expectEqual(result.is_ok, true);
}

test "isSmallStr: returns true for empty string" {
    const str = RocStr.empty();
    try std.testing.expect(str.isSmallStr());
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

    try std.testing.expect(roc_str1.eql(roc_str2));

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

    try std.testing.expect(!roc_str1.eql(roc_str2));
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

    try std.testing.expect(!roc_str1.eql(roc_str2));
}

test "RocStr.eq: small, exact u64 chunk" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str1 = RocStr.init("abcdefgh", 8, test_env.getOps());
    const roc_str2 = RocStr.init("abcdefgh", 8, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
    }

    try std.testing.expect(roc_str1.isSmallStr());
    try std.testing.expectEqual(@as(usize, 8), roc_str1.len());
    try std.testing.expect(roc_str1.eql(roc_str2));
}

test "RocStr.eq: small, masked tail detects last byte difference" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const equal_text = if (SMALL_STR_MAX_LENGTH >= 23)
        "abcdefghijklmnopqrstuvw"
    else
        "abcdefghijk";
    const different_text = if (SMALL_STR_MAX_LENGTH >= 23)
        "abcdefghijklmnopqrstuvX"
    else
        "abcdefghijX";

    const roc_str1 = RocStr.init(equal_text, equal_text.len, test_env.getOps());
    const roc_str2 = RocStr.init(equal_text, equal_text.len, test_env.getOps());
    const roc_str3 = RocStr.init(different_text, different_text.len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
        roc_str3.decref(test_env.getOps());
    }

    try std.testing.expect(roc_str1.isSmallStr());
    try std.testing.expect(roc_str2.isSmallStr());
    try std.testing.expect(roc_str3.isSmallStr());
    try std.testing.expect(roc_str1.eql(roc_str2));
    try std.testing.expect(!roc_str1.eql(roc_str3));
}

test "RocStr.eq: all small lengths compare by bytes" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var left_buf: [SMALL_STR_MAX_LENGTH]u8 = undefined;
    var right_buf: [SMALL_STR_MAX_LENGTH]u8 = undefined;

    for (0..SMALL_STR_MAX_LENGTH) |i| {
        left_buf[i] = @as(u8, @intCast('a' + (i % 26)));
        right_buf[i] = left_buf[i];
    }

    for (0..SMALL_STR_MAX_LENGTH + 1) |len| {
        const equal_left = RocStr.init(&left_buf, len, test_env.getOps());
        const equal_right = RocStr.init(&right_buf, len, test_env.getOps());

        try std.testing.expect(equal_left.isSmallStr());
        try std.testing.expect(equal_right.isSmallStr());
        try std.testing.expect(equal_left.eql(equal_right));

        equal_left.decref(test_env.getOps());
        equal_right.decref(test_env.getOps());

        if (len == 0) continue;

        right_buf[len - 1] ^= 1;
        const unequal_left = RocStr.init(&left_buf, len, test_env.getOps());
        const unequal_right = RocStr.init(&right_buf, len, test_env.getOps());

        try std.testing.expect(unequal_left.isSmallStr());
        try std.testing.expect(unequal_right.isSmallStr());
        try std.testing.expect(!unequal_left.eql(unequal_right));

        unequal_left.decref(test_env.getOps());
        unequal_right.decref(test_env.getOps());
        right_buf[len - 1] = left_buf[len - 1];
    }
}

fn packStaticSmallForTest(text: []const u8) [3]u64 {
    var words = [3]u64{ 0, 0, 0 };
    for (text, 0..) |byte, index| {
        words[index / @sizeOf(u64)] |= @as(u64, byte) << @intCast((index % @sizeOf(u64)) * 8);
    }
    return words;
}

test "RocStr.eqStaticSmall: all static lengths compare by bytes" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var buf: [24]u8 = undefined;
    for (&buf, 0..) |*byte, index| {
        byte.* = @as(u8, @intCast('A' + (index % 26)));
    }

    for (0..25) |len| {
        const text = buf[0..len];
        const words = packStaticSmallForTest(text);
        const roc_str = RocStr.init(text.ptr, text.len, test_env.getOps());
        defer roc_str.decref(test_env.getOps());

        try std.testing.expect(roc_str.eqlStaticSmall(len, words[0], words[1], words[2]));

        if (len > 0) {
            var changed = buf;
            changed[len - 1] ^= 1;
            const changed_words = packStaticSmallForTest(changed[0..len]);
            try std.testing.expect(!roc_str.eqlStaticSmall(len, changed_words[0], changed_words[1], changed_words[2]));
        }
    }
}

test "RocStr.eqStaticSmall: short seamless slice at allocation end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const backing_text = "abcdefghijklmnopqrstuvxy";
    var backing = RocStr.init(backing_text, backing_text.len, test_env.getOps());
    defer backing.decref(test_env.getOps());

    const slice = substringUnsafe(backing, backing_text.len - 2, 2, test_env.getOps());

    const words = packStaticSmallForTest("xy");
    const mismatch = packStaticSmallForTest("xz");

    try std.testing.expect(slice.eqlStaticSmall(2, words[0], words[1], words[2]));
    try std.testing.expect(!slice.eqlStaticSmall(2, mismatch[0], mismatch[1], mismatch[2]));
}

test "RocStr.eqStaticSmall: rejects long static metadata" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const text = "abcdefghijklmnopqrstuvwx";
    const roc_str = RocStr.init(text, text.len, test_env.getOps());
    defer roc_str.decref(test_env.getOps());

    try std.testing.expect(!roc_str.eqlStaticSmall(25, 0, 0, 0));
}

test "RocStr.staticSmallWordEq: compares selected full lanes" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const text = "cacheControlUserIdValue";
    const roc_str = RocStr.init(text, text.len, test_env.getOps());
    defer roc_str.decref(test_env.getOps());

    const words = packStaticSmallForTest(text);
    try std.testing.expect(roc_str.staticSmallWordEq(0, 8, words[0]));
    try std.testing.expect(roc_str.staticSmallWordEq(8, 8, words[1]));
    try std.testing.expect(roc_str.staticSmallWordEq(16, text.len - 16, words[2]));
    try std.testing.expect(!roc_str.staticSmallWordEq(8, 8, words[1] ^ 0x01));
}

test "RocStr.staticSmallWordEq: short seamless slice at allocation end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const backing_text = "abcdefghijklmnopqrstuvxyz";
    var backing = RocStr.init(backing_text, backing_text.len, test_env.getOps());
    defer backing.decref(test_env.getOps());

    const slice = substringUnsafe(backing, backing_text.len - 3, 3, test_env.getOps());
    const words = packStaticSmallForTest("xyz");
    const mismatch = packStaticSmallForTest("xyZ");

    try std.testing.expect(slice.staticSmallWordEq(0, 3, words[0]));
    try std.testing.expect(!slice.staticSmallWordEq(0, 3, mismatch[0]));
}

test "RocStr.staticSmallWordEq: rejects out-of-range lanes" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("abc", 3, test_env.getOps());
    defer roc_str.decref(test_env.getOps());

    const words = packStaticSmallForTest("abc");
    try std.testing.expect(!roc_str.staticSmallWordEq(0, 9, words[0]));
    try std.testing.expect(!roc_str.staticSmallWordEq(4, 1, words[0]));
    try std.testing.expect(!roc_str.staticSmallWordEq(2, 2, words[0]));
}

test "RocStr.staticSmallWordCaselessEq: compares selected lanes" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.fromSlice("Content-Length", test_env.getOps());
    defer roc_str.decref(test_env.getOps());

    const words = packStaticSmallForTest("content-length");
    const mismatch = packStaticSmallForTest("lengxh");

    try std.testing.expect(roc_str.staticSmallWordCaselessEq(0, 8, words[0]));
    try std.testing.expect(roc_str.staticSmallWordCaselessEq(8, 6, words[1]));
    try std.testing.expect(!roc_str.staticSmallWordCaselessEq(8, 6, mismatch[0]));
}

test "RocStr.staticSmallWordCaselessEq: compares Cache-Control tail lane" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.fromSlice("Cache-Control", test_env.getOps());
    defer roc_str.decref(test_env.getOps());

    const words = packStaticSmallForTest("cache-control");
    const mismatch = packStaticSmallForTest("contrxl");

    try std.testing.expect(roc_str.staticSmallWordCaselessEq(0, 8, words[0]));
    try std.testing.expect(roc_str.staticSmallWordCaselessEq(8, 5, words[1]));
    try std.testing.expect(!roc_str.staticSmallWordCaselessEq(8, 5, mismatch[0]));
}

test "RocStr.staticSmallWordCaselessEq: exact ineligible bytes and unicode lanes" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.fromSlice("A_\x7Fé-Z9", test_env.getOps());
    defer roc_str.decref(test_env.getOps());

    const words = packStaticSmallForTest("a_\x7Fé-z9");
    try std.testing.expectEqual(@as(usize, 8), roc_str.len());
    try std.testing.expect(roc_str.staticSmallWordCaselessEq(0, 8, words[0]));
}

test "RocStr.staticSmallWordCaselessEq: rejects punctuation case-bit pairs" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const leading = RocStr.fromSlice("_abc", test_env.getOps());
    defer leading.decref(test_env.getOps());
    try std.testing.expect(!leading.staticSmallWordCaselessEq(0, 4, packStaticSmallForTest("\x7FABC")[0]));

    const trailing = RocStr.fromSlice("abc_", test_env.getOps());
    defer trailing.decref(test_env.getOps());
    try std.testing.expect(!trailing.staticSmallWordCaselessEq(0, 4, packStaticSmallForTest("ABC\x7F")[0]));
}

test "RocStr.staticSmallWordCaselessEq: short seamless slice at allocation end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const backing_text = "012345678901234567890123456789aBc";
    var backing = RocStr.init(backing_text, backing_text.len, test_env.getOps());
    defer backing.decref(test_env.getOps());

    const slice = substringUnsafe(backing, backing_text.len - 3, 3, test_env.getOps());
    const words = packStaticSmallForTest("AbC");
    const mismatch = packStaticSmallForTest("AbD");

    try std.testing.expect(slice.staticSmallWordCaselessEq(0, 3, words[0]));
    try std.testing.expect(!slice.staticSmallWordCaselessEq(0, 3, mismatch[0]));
}

test "RocStr.eq: embedded nul bytes use byte equality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const text = "ab\x00cd";
    const same = "ab\x00cd";
    const different = "ab\x00ce";

    const roc_str1 = RocStr.init(text, text.len, test_env.getOps());
    const roc_str2 = RocStr.init(same, same.len, test_env.getOps());
    const roc_str3 = RocStr.init(different, different.len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
        roc_str3.decref(test_env.getOps());
    }

    try std.testing.expect(roc_str1.eql(roc_str2));
    try std.testing.expect(!roc_str1.eql(roc_str3));
}

test "RocStr.eq: utf8 content uses exact byte equality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const composed = "caf\xc3\xa9";
    const same = "caf\xc3\xa9";
    const decomposed = "cafe\xcc\x81";

    const roc_str1 = RocStr.init(composed, composed.len, test_env.getOps());
    const roc_str2 = RocStr.init(same, same.len, test_env.getOps());
    const roc_str3 = RocStr.init(decomposed, decomposed.len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
        roc_str3.decref(test_env.getOps());
    }

    try std.testing.expect(roc_str1.eql(roc_str2));
    try std.testing.expect(!roc_str1.eql(roc_str3));
}

test "RocStr.eq: boundary between small and heap strings" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var small_buf: [SMALL_STR_MAX_LENGTH]u8 = undefined;
    var heap_buf: [SMALL_STRING_SIZE]u8 = undefined;

    for (&small_buf, 0..) |*byte, i| {
        byte.* = @as(u8, @intCast('a' + (i % 26)));
    }
    for (&heap_buf, 0..) |*byte, i| {
        byte.* = @as(u8, @intCast('a' + (i % 26)));
    }

    const small1 = RocStr.init(&small_buf, small_buf.len, test_env.getOps());
    const small2 = RocStr.init(&small_buf, small_buf.len, test_env.getOps());
    const heap1 = RocStr.init(&heap_buf, heap_buf.len, test_env.getOps());
    const heap2 = RocStr.init(&heap_buf, heap_buf.len, test_env.getOps());

    defer {
        small1.decref(test_env.getOps());
        small2.decref(test_env.getOps());
        heap1.decref(test_env.getOps());
        heap2.decref(test_env.getOps());
    }

    try std.testing.expect(small1.isSmallStr());
    try std.testing.expect(!heap1.isSmallStr());
    try std.testing.expect(small1.eql(small2));
    try std.testing.expect(heap1.eql(heap2));
    try std.testing.expect(!small1.eql(heap1));
}

test "RocStr.eq: short seamless slice at allocation end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const source = RocStr.fromSlice("012345678901234567890123456789abc", test_env.getOps());
    const str1 = substringUnsafeC(source, source.len() - 3, 3, test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("abc", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const str3 = RocStr.fromSlice("abX", test_env.getOps());
    defer str3.decref(test_env.getOps());

    try std.testing.expect(str1.isSeamlessSlice());
    try std.testing.expect(str1.eql(str2));
    try std.testing.expect(!str1.eql(str3));
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

    try std.testing.expect(roc_str1.eql(roc_str2));
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

    try std.testing.expect(!roc_str1.eql(roc_str2));
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

    try std.testing.expect(!roc_str1.eql(roc_str2));
}

test "RocStr.eq: large, garbage after end, equal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const content = "012345678901234567890123456789";
    const roc_str1 = RocStr.init(content, content.len, test_env.getOps());
    const roc_str2 = RocStr.init(content, content.len, test_env.getOps());
    try std.testing.expect(roc_str1.bytes != roc_str2.bytes);

    // Insert garbage after the end of each string
    roc_str1.bytes.?[30] = '!';
    roc_str1.bytes.?[31] = '!';
    roc_str2.bytes.?[30] = '-';
    roc_str2.bytes.?[31] = '-';

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
    }

    try std.testing.expect(roc_str1.eql(roc_str2));
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

    strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

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
    try std.testing.expect(array[0].eql(expected[0]));
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

    strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

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
    try std.testing.expect(array[0].eql(expected[0]));
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

    strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

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
    try std.testing.expect(array[0].eql(expected[0]));
    try std.testing.expect(array[1].eql(expected[1]));
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

    strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

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
    try std.testing.expect(array[0].eql(expected[0]));
    try std.testing.expect(array[1].eql(expected[1]));
    try std.testing.expect(array[2].eql(expected[2]));
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

    strSplitOnHelp(array_ptr, str_delimiter, str_delimiter, test_env.getOps());

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
    try std.testing.expect(array[0].eql(expected[0]));
    try std.testing.expect(array[1].eql(expected[1]));
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
    strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

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
    try std.testing.expect(array[0].eql(expected[0]));
    try std.testing.expect(array[1].eql(expected[1]));
    try std.testing.expect(array[2].eql(expected[2]));
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

    strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

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
    try std.testing.expect(array[0].eql(expected_array[0]));
    try std.testing.expect(array[1].eql(expected_array[1]));
    try std.testing.expect(array[2].eql(expected_array[2]));
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

    strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const expected = [2]RocStr{
        RocStr.empty(),
        RocStr.init("a", 1, test_env.getOps()),
    };

    // strings are all small so we ignore freeing the memory

    try std.testing.expectEqual(array.len, expected.len);
    try std.testing.expect(array[0].eql(expected[0]));
    try std.testing.expect(array[1].eql(expected[1]));
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

    strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const expected = [3]RocStr{
        RocStr.empty(),
        RocStr.empty(),
        RocStr.empty(),
    };

    // strings are all small so we ignore freeing the memory

    try std.testing.expectEqual(array.len, expected.len);
    try std.testing.expect(array[0].eql(expected[0]));
    try std.testing.expect(array[1].eql(expected[1]));
    try std.testing.expect(array[2].eql(expected[2]));
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

    const segments_count = countSegments(str, delimiter);
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

    const segments_count = countSegments(str, delimiter);

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

    const segments_count = countSegments(str, delimiter);

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

    const segments_count = countSegments(str_delimiter, str_delimiter);

    try std.testing.expectEqual(segments_count, 2);
}

test "countSegments: overlapping delimiter 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "aaa" "aa" == ["", "a"]
    const segments_count = countSegments(
        RocStr.init("aaa", 3, test_env.getOps()),
        RocStr.init("aa", 2, test_env.getOps()),
    );

    try std.testing.expectEqual(segments_count, 2);
}

test "countSegments: overlapping delimiter 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "aaa" "aa" == ["", "a"]
    const segments_count = countSegments(
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

    const actual = substringUnsafe(str, 0, 3, test_env.getOps());

    try std.testing.expect(RocStr.eql(actual, expected));
}

test "substringUnsafe: middle" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("abcdef", test_env.getOps());
    defer str.decref(test_env.getOps());

    const expected = RocStr.fromSlice("bcd", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const actual = substringUnsafe(str, 1, 3, test_env.getOps());

    try std.testing.expect(RocStr.eql(actual, expected));
}

test "substringUnsafe: end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("a string so long it is heap-allocated", test_env.getOps());
    defer str.decref(test_env.getOps());

    const expected = RocStr.fromSlice("heap-allocated", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const actual = substringUnsafe(str, 23, 37 - 23, test_env.getOps());

    try std.testing.expect(RocStr.eql(actual, expected));
}

test "startsWith: food starts with foo" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const food = RocStr.fromSlice("food", test_env.getOps());
    const foo = RocStr.fromSlice("foo", test_env.getOps());
    try std.testing.expect(startsWith(food, foo));
}

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("123456789123456789", test_env.getOps());
    defer str.decref(test_env.getOps());
    try std.testing.expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("12345678912345678910", test_env.getOps());
    defer str.decref(test_env.getOps());
    const prefix = RocStr.fromSlice("123456789123456789", test_env.getOps());
    defer prefix.decref(test_env.getOps());

    try std.testing.expect(startsWith(str, prefix));
}

test "endsWith: foo ends with oo" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const foo = RocStr.init("foo", 3, test_env.getOps());
    const oo = RocStr.init("oo", 2, test_env.getOps());
    defer foo.decref(test_env.getOps());
    defer oo.decref(test_env.getOps());

    try std.testing.expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.init("123456789123456789", 18, test_env.getOps());
    defer str.decref(test_env.getOps());
    try std.testing.expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.init("12345678912345678910", 20, test_env.getOps());
    const suffix = RocStr.init("345678912345678910", 18, test_env.getOps());
    defer str.decref(test_env.getOps());
    defer suffix.decref(test_env.getOps());

    try std.testing.expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.init("hello world", 11, test_env.getOps());
    const suffix = RocStr.init("world", 5, test_env.getOps());
    defer str.decref(test_env.getOps());
    defer suffix.decref(test_env.getOps());

    try std.testing.expect(endsWith(str, suffix));
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

    const result = strConcat(roc_str1, roc_str2, .Immutable, test_env.getOps());

    defer result.decref(test_env.getOps());

    try std.testing.expect(roc_str3.eql(result));
}

test "RocStr.concat: concat result fed into concat again" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const first_a = RocStr.fromSliceSmall("a");
    const first_b = RocStr.fromSliceSmall("b");
    const second = RocStr.fromSliceSmall("c");

    const first = strConcat(first_a, first_b, .Immutable, test_env.getOps());
    defer first.decref(test_env.getOps());

    const result = strConcat(first, second, .Immutable, test_env.getOps());
    defer result.decref(test_env.getOps());

    const expected = RocStr.fromSliceSmall("abc");
    try std.testing.expect(expected.eql(result));
}

test "RocStr.concat: big concat overlapping seamless suffix" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.init("hello wonderful", 15, test_env.getOps());
    const prefix = RocStr.fromSliceSmall("hello ");
    const suffix = strDropPrefix(original, prefix, test_env.getOps());
    defer suffix.decref(test_env.getOps());

    const result = strConcat(original, suffix, .Immutable, test_env.getOps());
    defer result.decref(test_env.getOps());

    const expected = RocStr.init("hello wonderfulwonderful", 24, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(expected.eql(result));
}

test "RocStr.concat: big concat with self alias" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var original = RocStr.init("hello wonderful", 15, test_env.getOps());
    original.incref(1, test_env.getOps());

    const result = strConcat(original, original, .Immutable, test_env.getOps());
    defer result.decref(test_env.getOps());

    const expected = RocStr.init("hello wonderfulhello wonderful", 30, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(expected.eql(result));
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
        .list_capacity_or_alloc_ptr = RocList.encodeCapacity(3),
        .list_elements = @as([*]RocStr, @ptrCast(&elements)),
    };

    defer {
        roc_sep.decref(test_env.getOps());
        roc_elem.decref(test_env.getOps());
        roc_result.decref(test_env.getOps());
    }

    const result = strJoinWith(list, roc_sep, test_env.getOps());

    defer result.decref(test_env.getOps());

    try std.testing.expect(roc_result.eql(result));
}

test "strJoinWithC: consuming a unique heap List(Str) frees its element strings" {
    // Regression test: `strJoinWithC` consumes the list and must decref each
    // (heap) element string. A previous implementation did this through a
    // `&strDecref` function-pointer callback handed to `RocList.decref`; on
    // x64win the COFF linker misresolved that pointer (into `.rdata`), so the
    // element loop never ran and every heap element string leaked. The leak
    // checker in `std.testing.allocator` (via TestEnv.deinit) catches a
    // regression here.
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // 33-char literals are longer than SMALL_STRING_SIZE, so they allocate on
    // the heap rather than living inline in the RocStr.
    const a = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    const b = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
    const s1 = RocStr.init(a.ptr, a.len, test_env.getOps());
    const s2 = RocStr.init(b.ptr, b.len, test_env.getOps());
    try std.testing.expect(!s1.isSmallStr());
    try std.testing.expect(!s2.isSmallStr());

    // Build a unique heap List(Str) that takes ownership of the two element
    // strings (fromSlice copies the RocStr structs without incref'ing).
    const list = RocList.fromSlice(RocStr, &.{ s1, s2 }, true, test_env.getOps());

    var sep_bytes: [2]u8 = ", ".*;
    const sep = RocStr.init(&sep_bytes, sep_bytes.len, test_env.getOps());
    defer sep.decref(test_env.getOps());

    // Consumes `list`: frees both element strings and the backing allocation.
    const result = strJoinWithC(list, sep, test_env.getOps());
    defer result.decref(test_env.getOps());

    const expected = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
    try std.testing.expectEqualSlices(u8, expected, result.asSlice());
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

    const list = RocList.fromSlice(u8, "r💖c", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, &rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r💖c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try std.testing.expect(expected.eql(res));
}

fn expectErr(
    list: RocList,
    index: usize,
    err: Utf8DecodeError,
    problem: Utf8ByteProblem,
    test_env: *TestEnv,
) error{ TestExpectedError, TestUnexpectedError, TestExpectedEqual }!void {
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

    const list = RocList.fromSlice(u8, "r\x80c", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, &rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try std.testing.expect(expected.eql(res));
}

test "fromUtf8Lossy: adjacent invalid start bytes" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = RocList.fromSlice(u8, "r\xFF\xFFc", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, &rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try std.testing.expect(expected.eql(res));
}

test "fromUtf8Lossy: overlong encoding" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = RocList.fromSlice(u8, "r\xF0\x9F\x92\x96\x80c", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, &rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r💖�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try std.testing.expect(expected.eql(res));
}

test "fromUtf8Lossy: expected continuation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = RocList.fromSlice(u8, "r\xCFc", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, &rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try std.testing.expect(expected.eql(res));
}

test "fromUtf8Lossy: unexpected end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = RocList.fromSlice(u8, "r\xCF", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, &rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try std.testing.expect(expected.eql(res));
}

test "fromUtf8Lossy: encodes surrogate" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // 0xd83d == 0b1101_1000_0011_1101
    //             wwww xxxx yyyy zzzz
    // becomes 0b1110_1101 0b10_1000_00 0b10_11_1101
    //           1110_wwww   10_xxxx_yy   10_yy_zzzz
    //         0xED        0x90         0xBD
    const list = RocList.fromSlice(u8, "r\xED\xA0\xBDc", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, &rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try std.testing.expect(expected.eql(res));
}

test "isWhitespace" {
    try std.testing.expect(isWhitespace(' '));
    try std.testing.expect(isWhitespace('\u{00A0}'));
    try std.testing.expect(!isWhitespace('x'));
}

test "withAsciiLowercased: small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.fromSlice("cOFFÉ", test_env.getOps());
    try std.testing.expect(original.isSmallStr());

    const expected = RocStr.fromSlice("coffÉ", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = strWithAsciiLowercased(original, .Immutable, test_env.getOps());
    defer str_result.decref(test_env.getOps());

    try std.testing.expect(str_result.isSmallStr());
    try std.testing.expect(str_result.eql(expected));
}

test "withAsciiLowercased: non small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.fromSlice("cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ", test_env.getOps());
    defer original.decref(test_env.getOps());
    try std.testing.expect(!original.isSmallStr());

    const expected = RocStr.fromSlice("coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = strWithAsciiLowercased(original, .Immutable, test_env.getOps());

    try std.testing.expect(!str_result.isSmallStr());
    try std.testing.expect(str_result.eql(expected));
}

test "withAsciiLowercased: seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const l = RocStr.fromSlice("cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ", test_env.getOps());
    const original = substringUnsafeC(l, 1, l.len() - 1, test_env.getOps());
    defer original.decref(test_env.getOps());

    try std.testing.expect(original.isSeamlessSlice());

    const expected = RocStr.fromSlice("offÉ coffÉ coffÉ coffÉ coffÉ coffÉ", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = strWithAsciiLowercased(original, .Immutable, test_env.getOps());

    try std.testing.expect(!str_result.isSmallStr());
    try std.testing.expect(str_result.eql(expected));
}

test "withAsciiUppercased: small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.fromSlice("coffé", test_env.getOps());
    try std.testing.expect(original.isSmallStr());

    const expected = RocStr.fromSlice("COFFé", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = strWithAsciiUppercased(original, .Immutable, test_env.getOps());
    defer str_result.decref(test_env.getOps());

    try std.testing.expect(str_result.isSmallStr());
    try std.testing.expect(str_result.eql(expected));
}

test "withAsciiUppercased: non small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.fromSlice("coffé coffé coffé coffé coffé coffé", test_env.getOps());
    defer original.decref(test_env.getOps());
    try std.testing.expect(!original.isSmallStr());

    const expected = RocStr.fromSlice("COFFé COFFé COFFé COFFé COFFé COFFé", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = strWithAsciiUppercased(original, .Immutable, test_env.getOps());

    try std.testing.expect(!str_result.isSmallStr());
    try std.testing.expect(str_result.eql(expected));
}

test "withAsciiUppercased: seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const l = RocStr.fromSlice("coffé coffé coffé coffé coffé coffé", test_env.getOps());
    const original = substringUnsafeC(l, 1, l.len() - 1, test_env.getOps());
    defer original.decref(test_env.getOps());

    try std.testing.expect(original.isSeamlessSlice());

    const expected = RocStr.fromSlice("OFFé COFFé COFFé COFFé COFFé COFFé", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = strWithAsciiUppercased(original, .Immutable, test_env.getOps());

    try std.testing.expect(!str_result.isSmallStr());
    try std.testing.expect(str_result.eql(expected));
}

test "caselessAsciiEquals: same str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("coFféÉ", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const are_equal = strCaselessAsciiEquals(str1, str1);
    try std.testing.expect(are_equal);
}

test "caselessAsciiEquals: differently capitalized non-ascii char" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("coffé", test_env.getOps());
    defer str1.decref(test_env.getOps());
    try std.testing.expect(str1.isSmallStr());

    const str2 = RocStr.fromSlice("coffÉ", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = strCaselessAsciiEquals(str1, str2);
    try std.testing.expect(!are_equal);
}

test "caselessAsciiEquals: small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("coffé", test_env.getOps());
    defer str1.decref(test_env.getOps());
    try std.testing.expect(str1.isSmallStr());

    const str2 = RocStr.fromSlice("COFFé", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = strCaselessAsciiEquals(str1, str2);
    try std.testing.expect(are_equal);
}

test "caselessAsciiEquals: non small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("coffé coffé coffé coffé coffé coffé", test_env.getOps());
    defer str1.decref(test_env.getOps());
    try std.testing.expect(!str1.isSmallStr());

    const str2 = RocStr.fromSlice("COFFé COFFé COFFé COFFé COFFé COFFé", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = strCaselessAsciiEquals(str1, str2);

    try std.testing.expect(are_equal);
}

test "caselessAsciiEquals: seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const l = RocStr.fromSlice("coffé coffé coffé coffé coffé coffé", test_env.getOps());
    const str1 = substringUnsafeC(l, 1, l.len() - 1, test_env.getOps());
    defer str1.decref(test_env.getOps());

    try std.testing.expect(str1.isSeamlessSlice());

    const str2 = RocStr.fromSlice("OFFé COFFé COFFé COFFé COFFé COFFé", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = strCaselessAsciiEquals(str1, str2);

    try std.testing.expect(are_equal);
}

test "caselessAsciiEquals: short seamless slice at allocation end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const source = RocStr.fromSlice("012345678901234567890123456789aBc", test_env.getOps());
    const str1 = substringUnsafeC(source, source.len() - 3, 3, test_env.getOps());
    defer str1.decref(test_env.getOps());

    try std.testing.expect(str1.isSeamlessSlice());

    const str2 = RocStr.fromSlice("AbC", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = strCaselessAsciiEquals(str1, str2);

    try std.testing.expect(are_equal);
}

test "caselessAsciiEquals: punctuation with ascii case bit is not equal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("@[\\]^_", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("`{|}~\x7F", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = strCaselessAsciiEquals(str1, str2);

    try std.testing.expect(!are_equal);
}

test "caselessAsciiEquals: exact u64 chunk with alnum dash" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("AbCd-123", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("aBcD-123", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expectEqual(@as(usize, 8), str1.len());
    try std.testing.expect(strCaselessAsciiEquals(str1, str2));
}

test "caselessAsciiEquals: masked tail with alnum dash" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("Content-Length", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("content-length", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expectEqual(@as(usize, 14), str1.len());
    try std.testing.expect(strCaselessAsciiEquals(str1, str2));
}

test "caselessAsciiEquals: different lengths are not equal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("Content-Length", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("content-lengths", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expect(!strCaselessAsciiEquals(str1, str2));
}

test "caselessAsciiEquals: underscore exact byte still matches with ascii case folding" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("x_auth_token", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("X_AUTH_TOKEN", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expect(strCaselessAsciiEquals(str1, str2));
}

test "caselessAsciiEquals: exact ineligible bytes do not block ascii case folding" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("A_\x7Fé-Z9", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("a_\x7Fé-z9", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expectEqual(@as(usize, 8), str1.len());
    try std.testing.expect(strCaselessAsciiEquals(str1, str2));
}

test "caselessAsciiEquals: underscore with ascii case-bit pair is not equal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("_abc", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("\x7FABC", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expect(!strCaselessAsciiEquals(str1, str2));
}

test "caselessAsciiEquals: trailing underscore with ascii case-bit pair is not equal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("abc_", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("ABC\x7F", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expect(!strCaselessAsciiEquals(str1, str2));
}

test "caselessAsciiEquals: unicode fallback inside u64 chunk" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("café-AB", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("CAFé-ab", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expectEqual(@as(usize, 8), str1.len());
    try std.testing.expect(strCaselessAsciiEquals(str1, str2));
}

test "caselessAsciiEquals: unicode capitalization still differs" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("café-AB", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const str2 = RocStr.fromSlice("CAFÉ-ab", test_env.getOps());
    defer str2.decref(test_env.getOps());

    try std.testing.expectEqual(@as(usize, 8), str1.len());
    try std.testing.expect(!strCaselessAsciiEquals(str1, str2));
}

test "dropPrefixCaselessAscii: small string success does not allocate" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const source = RocStr.fromSlice("Cache-Control: 0", test_env.getOps());
    const prefix = RocStr.fromSlice("cache-control", test_env.getOps());

    const result = strDropPrefixCaselessAscii(source, prefix, test_env.getOps());

    try std.testing.expect(result.found);
    try std.testing.expect(result.after.eql(RocStr.fromSlice(": 0", test_env.getOps())));
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "dropPrefixCaselessAscii: miss does not retain or allocate" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const source = RocStr.fromSlice("Cache-Control: 0", test_env.getOps());
    const prefix = RocStr.fromSlice("content-length", test_env.getOps());

    const result = strDropPrefixCaselessAscii(source, prefix, test_env.getOps());

    try std.testing.expect(!result.found);
    try std.testing.expect(result.after.eql(RocStr.empty()));
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "dropPrefixCaselessAscii: non-ascii bytes match exactly only" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const source = RocStr.fromSlice("café: 0", test_env.getOps());
    const same_accent = RocStr.fromSlice("CAFé", test_env.getOps());
    const different_accent = RocStr.fromSlice("CAFÉ", test_env.getOps());

    const found = strDropPrefixCaselessAscii(source, same_accent, test_env.getOps());
    const not_found = strDropPrefixCaselessAscii(source, different_accent, test_env.getOps());

    try std.testing.expect(found.found);
    try std.testing.expect(found.after.eql(RocStr.fromSlice(": 0", test_env.getOps())));
    try std.testing.expect(!not_found.found);
    try std.testing.expect(not_found.after.eql(RocStr.empty()));
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "dropPrefixCaselessAscii: punctuation case-bit pairs do not match" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const source = RocStr.fromSlice("X_Auth: 0", test_env.getOps());
    const exact_punctuation = RocStr.fromSlice("x_auth", test_env.getOps());
    const case_bit_punctuation = RocStr.fromSlice("x\x7Fauth", test_env.getOps());

    const found = strDropPrefixCaselessAscii(source, exact_punctuation, test_env.getOps());
    const not_found = strDropPrefixCaselessAscii(source, case_bit_punctuation, test_env.getOps());

    try std.testing.expect(found.found);
    try std.testing.expect(found.after.eql(RocStr.fromSlice(": 0", test_env.getOps())));
    try std.testing.expect(!not_found.found);
    try std.testing.expect(not_found.after.eql(RocStr.empty()));
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "strTrim: empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const trimmedEmpty = strTrim(RocStr.empty(), .Immutable, test_env.getOps());
    try std.testing.expect(trimmedEmpty.eql(RocStr.empty()));
}

test "strTrim: null byte" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const bytes = [_]u8{0};
    const original = RocStr.init(&bytes, 1, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), original.len());
    try std.testing.expectEqual(@as(usize, SMALL_STR_MAX_LENGTH), original.getCapacity());

    const original_with_capacity = reserve(original, 40, .Immutable, test_env.getOps());
    defer original_with_capacity.decref(test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), original_with_capacity.len());
    try std.testing.expectEqual(@as(usize, 41), original_with_capacity.getCapacity());

    const trimmed = strTrim(original.clone(test_env.getOps()), .Immutable, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try std.testing.expect(original.eql(trimmed));
}

test "strTrim: blank" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    const trimmed = strTrim(original, .Immutable, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try std.testing.expect(trimmed.eql(RocStr.empty()));
}

test "strTrim: large to large" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    try std.testing.expect(!original.isSmallStr());

    const expected_bytes = "hello even more giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(!expected.isSmallStr());

    const trimmed = strTrim(original, .Immutable, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
}

test "strTrim: large to small sized slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = "             hello         ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    try std.testing.expect(!original.isSmallStr());

    const expected_bytes = "hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(expected.isSmallStr());

    try std.testing.expect(original.isUnique());
    const trimmed = strTrim(original, .Immutable, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
    try std.testing.expect(!trimmed.isSmallStr());
}

test "strTrim: small to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try std.testing.expect(original.isSmallStr());

    const expected_bytes = "hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(expected.isSmallStr());

    const trimmed = strTrim(original, .Immutable, test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
    try std.testing.expect(trimmed.isSmallStr());
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "strTrimStart: empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const trimmedEmpty = strTrimStart(RocStr.empty(), .Immutable, test_env.getOps());
    try std.testing.expect(trimmedEmpty.eql(RocStr.empty()));
}

test "strTrimStart: blank" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    const trimmed = strTrimStart(original, .Immutable, test_env.getOps());

    try std.testing.expect(trimmed.eql(RocStr.empty()));
}

test "strTrimStart: large to large" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try std.testing.expect(!original.isSmallStr());

    const expected_bytes = "hello even more giant world ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(!expected.isSmallStr());

    const trimmed = strTrimStart(original, .Immutable, test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
}

test "strTrimStart: large to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // `original` will be consumed by the concat; do not free explicitly
    const original_bytes = "                    hello ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    try std.testing.expect(!original.isSmallStr());

    const expected_bytes = "hello ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(expected.isSmallStr());

    const trimmed = strTrimStart(original, .Immutable, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
    try std.testing.expect(!trimmed.isSmallStr());
}

test "strTrimStart: small to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try std.testing.expect(original.isSmallStr());

    const expected_bytes = "hello ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(expected.isSmallStr());

    const trimmed = strTrimStart(original, .Immutable, test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
    try std.testing.expect(trimmed.isSmallStr());
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "strTrimEnd: empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const trimmedEmpty = strTrimEnd(RocStr.empty(), .Immutable, test_env.getOps());
    try std.testing.expect(trimmedEmpty.eql(RocStr.empty()));
}

test "strTrimEnd: blank" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    const trimmed = strTrimEnd(original, .Immutable, test_env.getOps());

    try std.testing.expect(trimmed.eql(RocStr.empty()));
}

test "strTrimEnd: large to large" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try std.testing.expect(!original.isSmallStr());

    const expected_bytes = " hello even more giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(!expected.isSmallStr());

    const trimmed = strTrimEnd(original, .Immutable, test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
}

test "strTrimEnd: large to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // `original` will be consumed by the concat; do not free explicitly
    const original_bytes = " hello                    ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    try std.testing.expect(!original.isSmallStr());

    const expected_bytes = " hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(expected.isSmallStr());

    const trimmed = strTrimEnd(original, .Immutable, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
    try std.testing.expect(!trimmed.isSmallStr());
}

test "strTrimEnd: small to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try std.testing.expect(original.isSmallStr());

    const expected_bytes = " hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try std.testing.expect(expected.isSmallStr());

    const trimmed = strTrimEnd(original, .Immutable, test_env.getOps());

    try std.testing.expect(trimmed.eql(expected));
    try std.testing.expect(trimmed.isSmallStr());
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "ReverseUtf8View: hello world" {
    const original_bytes = "hello world";
    const expected_bytes = "dlrow olleh";

    var i: usize = 0;
    var iter = ReverseUtf8View.initUnchecked(original_bytes).iterator();
    while (iter.nextCodepoint()) |codepoint| {
        try std.testing.expect(expected_bytes[i] == codepoint);
        i += 1;
    }
}

test "ReverseUtf8View: empty" {
    const original_bytes = "";

    var iter = ReverseUtf8View.initUnchecked(original_bytes).iterator();
    while (iter.nextCodepoint()) |_| {
        try std.testing.expect(false);
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

    try std.testing.expect(data.getCapacity() >= data_bytes.len);
}

test "RocStr single-thread incref/decref pair frees on zero" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("a string long enough to require a heap allocation", test_env.getOps());
    try std.testing.expect(!str.isSmallStr());
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    str.increfWithAtomicity(2, .single_thread, test_env.getOps());
    str.decrefWithAtomicity(.single_thread, test_env.getOps());
    str.decrefWithAtomicity(.single_thread, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    str.decrefWithAtomicity(.single_thread, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "strConcat InPlace reuses the unique big allocation without a uniqueness check" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const base = RocStr.fromSlice("a string long enough to be heap allocated", test_env.getOps());
    try std.testing.expect(!base.isSmallStr());
    const with_capacity = reserve(base, 16, .Immutable, test_env.getOps());
    const original_bytes = with_capacity.bytes;

    const suffix = RocStr.fromSlice("!?", test_env.getOps());
    const result = strConcat(with_capacity, suffix, .InPlace, test_env.getOps());
    defer result.decref(test_env.getOps());

    try std.testing.expectEqual(original_bytes, result.bytes);
    try std.testing.expect(std.mem.eql(u8, result.asSlice(), "a string long enough to be heap allocated!?"));
}

test "strConcat Immutable copies a shared big allocation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const base = RocStr.fromSlice("a string long enough to be heap allocated", test_env.getOps());
    try std.testing.expect(!base.isSmallStr());
    const original_bytes = base.bytes;

    // Hold a second reference so the checked path must copy.
    base.incref(1, test_env.getOps());

    const suffix = RocStr.fromSlice("!?", test_env.getOps());
    const result = strConcat(base, suffix, .Immutable, test_env.getOps());
    defer result.decref(test_env.getOps());
    defer base.decref(test_env.getOps());

    try std.testing.expect(result.bytes != original_bytes);
    try std.testing.expect(std.mem.eql(u8, result.asSlice(), "a string long enough to be heap allocated!?"));
    try std.testing.expect(std.mem.eql(u8, base.asSlice(), "a string long enough to be heap allocated"));
}
