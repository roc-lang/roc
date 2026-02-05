//! Wrapper functions for the dev backend
//!
//! These wrappers decompose RocStr/RocList structs into individual fields
//! so all arguments fit in registers and avoid platform-specific struct-passing ABI issues.
//! They are used by both:
//! - Native execution (dev evaluator) via direct function pointers
//! - Object file generation (roc build --backend=dev) via symbol references

const str = @import("str.zig");
const list = @import("list.zig");
const utils = @import("utils.zig");
const dec = @import("dec.zig");

const RocStr = str.RocStr;
const RocList = list.RocList;
// Use a local opaque pointer type for RocOps to avoid importing host_abi.zig
// which has a tracy dependency. The actual struct layout is handled by utils.zig.
const RocOps = utils.RocOps;

// Re-export commonly used functions
pub const rcNone = utils.rcNone;
pub const copy_fallback = list.copy_fallback;
pub const allocateWithRefcountC = utils.allocateWithRefcountC;
pub const increfDataPtrC = utils.increfDataPtrC;
pub const decrefDataPtrC = utils.decrefDataPtrC;
pub const freeDataPtrC = utils.freeDataPtrC;

// Import builtin functions we wrap (using actual function names from str.zig and list.zig)
const strToUtf8C = str.strToUtf8C;
const strConcatC = str.strConcatC;
const strContains = str.strContains;
const startsWith = str.startsWith;
const endsWith = str.endsWith;
const strIsEmpty = str.isEmpty;
const strEqual = str.strEqual;
const countUtf8Bytes = str.countUtf8Bytes;
const strCaselessAsciiEquals = str.strCaselessAsciiEquals;
const repeatC = str.repeatC;
const strTrim = str.strTrim;
const strTrimStart = str.strTrimStart;
const strTrimEnd = str.strTrimEnd;
const strSplitOn = str.strSplitOn;
const strJoinWithC = str.strJoinWithC;
const reserveC = str.reserveC;
const strReleaseExcessCapacity = str.strReleaseExcessCapacity;
const withCapacityC = str.withCapacityC;
const strDropPrefix = str.strDropPrefix;
const strDropSuffix = str.strDropSuffix;
const strWithAsciiLowercased = str.strWithAsciiLowercased;
const strWithAsciiUppercased = str.strWithAsciiUppercased;
const fromUtf8Lossy = str.fromUtf8Lossy;

const listConcat = list.listConcat;
const listPrepend = list.listPrepend;
const listSublist = list.listSublist;
const listReplace = list.listReplace;
const listReserve = list.listReserve;
const listReleaseExcessCapacity = list.listReleaseExcessCapacity;
const listWithCapacity = list.listWithCapacity;
const listAppendUnsafe = list.listAppendUnsafe;

// ═══════════════════════════════════════════════════════════════════════════
// String Wrappers
// ═══════════════════════════════════════════════════════════════════════════

/// Wrapper: strToUtf8C(RocStr, *RocOps) -> RocList
pub fn roc_builtins_str_to_utf8(out: *RocList, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const arg = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strToUtf8C(arg, roc_ops);
}

/// Wrapper: strConcatC(RocStr, RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_concat(out: *RocStr, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strConcatC(a, b, roc_ops);
}

/// Wrapper: strContains(RocStr, RocStr) -> bool
pub fn roc_builtins_str_contains(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return strContains(a, b);
}

/// Wrapper: startsWith(RocStr, RocStr) -> bool
pub fn roc_builtins_str_starts_with(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return startsWith(a, b);
}

/// Wrapper: endsWith(RocStr, RocStr) -> bool
pub fn roc_builtins_str_ends_with(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return endsWith(a, b);
}

/// Wrapper: isEmpty(RocStr) -> bool
pub fn roc_builtins_str_is_empty(str_bytes: ?[*]u8, str_len: usize, str_cap: usize) callconv(.c) bool {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    return strIsEmpty(s);
}

/// Wrapper: strEqual(RocStr, RocStr) -> bool
pub fn roc_builtins_str_equal(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return strEqual(a, b);
}

/// Wrapper: countUtf8Bytes(RocStr) -> u64
pub fn roc_builtins_str_count_utf8_bytes(str_bytes: ?[*]u8, str_len: usize, str_cap: usize) callconv(.c) u64 {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    return countUtf8Bytes(s);
}

/// Wrapper: strCaselessAsciiEquals(RocStr, RocStr) -> bool
pub fn roc_builtins_str_caseless_ascii_equals(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return strCaselessAsciiEquals(a, b);
}

/// Wrapper: repeatC(RocStr, u64, *RocOps) -> RocStr
pub fn roc_builtins_str_repeat(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, count: u64, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = repeatC(s, count, roc_ops);
}

/// Wrapper: strTrim(RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_trim(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrim(s, roc_ops);
}

/// Wrapper: strTrimStart(RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_trim_start(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrimStart(s, roc_ops);
}

/// Wrapper: strTrimEnd(RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_trim_end(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrimEnd(s, roc_ops);
}

/// Wrapper: strSplitOn(RocStr, RocStr, *RocOps) -> RocList
pub fn roc_builtins_str_split(out: *RocList, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strSplitOn(a, b, roc_ops);
}

/// Wrapper: strJoinWithC(RocList, RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_join_with(out: *RocStr, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, sep_bytes: ?[*]u8, sep_len: usize, sep_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    const sep = RocStr{ .bytes = sep_bytes, .length = sep_len, .capacity_or_alloc_ptr = sep_cap };
    out.* = strJoinWithC(l, sep, roc_ops);
}

/// Wrapper: reserveC(RocStr, u64, *RocOps) -> RocStr
pub fn roc_builtins_str_reserve(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, spare: u64, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = reserveC(s, spare, roc_ops);
}

/// Wrapper: strReleaseExcessCapacity(*RocOps, RocStr) -> RocStr
pub fn roc_builtins_str_release_excess_capacity(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strReleaseExcessCapacity(roc_ops, s);
}

/// Wrapper: withCapacityC(u64, *RocOps) -> RocStr
pub fn roc_builtins_str_with_capacity(out: *RocStr, capacity: u64, roc_ops: *RocOps) callconv(.c) void {
    out.* = withCapacityC(capacity, roc_ops);
}

/// Wrapper: strDropPrefix(RocStr, RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_drop_prefix(out: *RocStr, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strDropPrefix(a, b, roc_ops);
}

/// Wrapper: strDropSuffix(RocStr, RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_drop_suffix(out: *RocStr, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strDropSuffix(a, b, roc_ops);
}

/// Wrapper: strWithAsciiLowercased(RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_with_ascii_lowercased(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strWithAsciiLowercased(s, roc_ops);
}

/// Wrapper: strWithAsciiUppercased(RocStr, *RocOps) -> RocStr
pub fn roc_builtins_str_with_ascii_uppercased(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strWithAsciiUppercased(s, roc_ops);
}

/// Wrapper: fromUtf8Lossy(RocList, *RocOps) -> RocStr
pub fn roc_builtins_str_from_utf8_lossy(out: *RocStr, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = fromUtf8Lossy(l, roc_ops);
}

/// Wrapper for str_with_prefix: strConcatC(prefix, string, *RocOps) -> RocStr
pub fn roc_builtins_str_with_prefix(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, pfx_bytes: ?[*]u8, pfx_len: usize, pfx_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    const pfx = RocStr{ .bytes = pfx_bytes, .length = pfx_len, .capacity_or_alloc_ptr = pfx_cap };
    out.* = strConcatC(pfx, s, roc_ops);
}

/// Wrapper: escape special characters and wrap in double quotes for Str.inspect
pub fn roc_builtins_str_escape_and_quote(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    const slice = s.asSlice();

    var extra: usize = 0;
    for (slice) |ch| {
        if (ch == '\\' or ch == '"') extra += 1;
    }

    const result_len = slice.len + extra + 2;
    const small_string_size = @sizeOf(RocStr);

    if (result_len < small_string_size) {
        var buf: [small_string_size]u8 = .{0} ** small_string_size;
        buf[0] = '"';
        var pos: usize = 1;
        for (slice) |ch| {
            if (ch == '\\' or ch == '"') {
                buf[pos] = '\\';
                pos += 1;
            }
            buf[pos] = ch;
            pos += 1;
        }
        buf[pos] = '"';
        buf[small_string_size - 1] = @intCast(result_len | 0x80);
        out.* = @bitCast(buf);
    } else {
        const heap_ptr = allocateWithRefcountC(result_len, 1, false, roc_ops);
        heap_ptr[0] = '"';
        var pos: usize = 1;
        for (slice) |ch| {
            if (ch == '\\' or ch == '"') {
                heap_ptr[pos] = '\\';
                pos += 1;
            }
            heap_ptr[pos] = ch;
            pos += 1;
        }
        heap_ptr[pos] = '"';
        out.* = .{ .bytes = heap_ptr, .length = result_len, .capacity_or_alloc_ptr = result_len };
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// List Wrappers
// ═══════════════════════════════════════════════════════════════════════════

/// Wrapper: listWithCapacity
pub fn roc_builtins_list_with_capacity(out: *RocList, capacity: u64, alignment: u32, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    // elements_refcounted=false, no inc function needed for basic allocation
    out.* = listWithCapacity(capacity, alignment, element_width, false, null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listAppendUnsafe
pub fn roc_builtins_list_append_unsafe(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, element: ?[*]const u8, element_width: usize, _: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listAppendUnsafe(l, @constCast(element), element_width, @ptrCast(&copy_fallback));
}

/// Wrapper: listConcat(RocList, RocList, alignment, element_width, ..., *RocOps) -> RocList
pub fn roc_builtins_list_concat(out: *RocList, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, alignment: u32, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocList{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocList{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = listConcat(a, b, alignment, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listPrepend(RocList, alignment, element, element_width, ..., *RocOps) -> RocList
pub fn roc_builtins_list_prepend(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element: ?[*]u8, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listPrepend(l, alignment, element, element_width, false, null, @ptrCast(&rcNone), @ptrCast(&copy_fallback), roc_ops);
}

/// Wrapper: listSublist for drop_first/drop_last/take_first/take_last
pub fn roc_builtins_list_sublist(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, start: u64, len: u64, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listSublist(l, alignment, element_width, false, start, len, null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listReplace for list_set
pub fn roc_builtins_list_replace(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, index: u64, element: ?[*]u8, element_width: usize, out_element: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReplace(l, alignment, index, element, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), out_element, @ptrCast(&copy_fallback), roc_ops);
}

/// Wrapper: listReserve
pub fn roc_builtins_list_reserve(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, spare: u64, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReserve(l, alignment, spare, element_width, false, null, @ptrCast(&rcNone), .Immutable, roc_ops);
}

/// Wrapper: listReleaseExcessCapacity
pub fn roc_builtins_list_release_excess_capacity(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReleaseExcessCapacity(l, alignment, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), .Immutable, roc_ops);
}

// ═══════════════════════════════════════════════════════════════════════════
// Memory/Refcounting Wrappers (re-export with dev_ prefix for consistency)
// ═══════════════════════════════════════════════════════════════════════════

/// Re-export allocateWithRefcountC
pub fn roc_builtins_allocate_with_refcount(data_bytes: usize, element_alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) [*]u8 {
    return allocateWithRefcountC(data_bytes, element_alignment, elements_refcounted, roc_ops);
}

/// Re-export increfDataPtrC
pub fn roc_builtins_incref_data_ptr(ptr: [*]u8, amount: isize, roc_ops: *RocOps) callconv(.c) void {
    increfDataPtrC(ptr, amount, roc_ops);
}

/// Re-export decrefDataPtrC
pub fn roc_builtins_decref_data_ptr(ptr: [*]u8, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops);
}

/// Re-export freeDataPtrC
pub fn roc_builtins_free_data_ptr(ptr: [*]u8, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops);
}

// ═══════════════════════════════════════════════════════════════════════════
// Numeric Wrappers
// ═══════════════════════════════════════════════════════════════════════════

/// Wrapper: decToStrC (decomposed i128)
pub fn roc_builtins_dec_to_str(out: *RocStr, value_low: u64, value_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const value: i128 = @bitCast((@as(u128, value_high) << 64) | @as(u128, value_low));
    const d = dec.RocDec{ .num = value };
    var buf: [dec.RocDec.max_str_length]u8 = undefined;
    const slice = d.format_to_buf(&buf);
    out.* = RocStr.init(&buf, slice.len, roc_ops);
}
