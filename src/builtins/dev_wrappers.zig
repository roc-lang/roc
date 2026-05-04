//! Wrapper functions for the dev backend
//!
//! These wrappers decompose RocStr/RocList structs into individual fields
//! so all arguments fit in registers and avoid platform-specific struct-passing ABI issues.
//! They are used by both:
//! - Native execution (dev evaluator) via direct function pointers
//! - Object file generation (roc build --opt=dev) via symbol references

const std = @import("std");
const str = @import("str.zig");
const list = @import("list.zig");
const num = @import("num.zig");
const utils = @import("utils.zig");
const dec = @import("dec.zig");
const i128h = @import("compiler_rt_128.zig");

const RocStr = str.RocStr;
const RocList = list.RocList;
const FromUtf8Try = str.FromUtf8Try;
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
const listAppendSafeC = list.listAppendSafeC;
const listDecref = list.listDecref;
const RcDropFn = *const fn (?[*]u8, *RocOps) callconv(.c) void;
const SortCmpFn = *const fn (?[*]u8, ?[*]u8, ?[*]u8) callconv(.c) u8;

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

/// Wrapper: fromUtf8C(RocList, UpdateMode, *RocOps) -> FromUtf8Try
pub fn roc_builtins_str_from_utf8(out: [*]u8, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    const result: FromUtf8Try = str.fromUtf8C(l, .Immutable, roc_ops);
    @as(*FromUtf8Try, @ptrCast(@alignCast(out))).* = result;
}

fn writeDiscriminant(out: [*]u8, offset: u32, size: u32, value: u64) void {
    switch (size) {
        0 => {},
        1 => utils.writeAs(u8, out + offset, @truncate(value), @src()),
        2 => utils.writeAs(u16, out + offset, @truncate(value), @src()),
        4 => utils.writeAs(u32, out + offset, @truncate(value), @src()),
        8 => utils.writeAs(u64, out + offset, value, @src()),
        else => unreachable,
    }
}

/// Converts a UTF-8 byte list to a RocStr, writing the full result union (string or error details) to an output buffer.
pub fn roc_builtins_str_from_utf8_result(
    out: [*]u8,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    ok_tag: u64,
    err_tag: u64,
    outer_disc_offset: u32,
    outer_disc_size: u32,
    err_index_offset: u32,
    err_problem_offset: u32,
    roc_ops: *RocOps,
) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    const result = str.fromUtf8C(l, .Immutable, roc_ops);

    if (result.is_ok) {
        utils.writeAs(RocStr, out, result.string, @src());
        writeDiscriminant(out, outer_disc_offset, outer_disc_size, ok_tag);
        return;
    }

    utils.writeAs(u64, out + err_index_offset, result.byte_index, @src());
    utils.writeAs(u8, out + err_problem_offset, @intFromEnum(result.problem_code), @src());
    writeDiscriminant(out, outer_disc_offset, outer_disc_size, err_tag);
}

/// Converts a UTF-8 byte list to a RocStr, returning the result components via separate out-pointers.
pub fn roc_builtins_str_from_utf8_parts(
    out_string: *RocStr,
    out_index: *u64,
    out_problem: *u8,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    roc_ops: *RocOps,
) callconv(.c) u8 {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    const result = str.fromUtf8C(l, .Immutable, roc_ops);
    out_string.* = result.string;
    out_index.* = result.byte_index;
    out_problem.* = @intFromEnum(result.problem_code);
    return @intFromBool(result.is_ok);
}

/// Wrapper: call roc_dbg with a formatted RocStr
pub fn roc_builtins_roc_dbg(str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    const slice = s.asSlice();
    roc_ops.dbg(slice);
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

fn strListElementDecref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    if (element == null) return;
    const ctx = context orelse unreachable;
    const str_ptr: *RocStr = utils.alignedPtrCast(*RocStr, element.?, @src());
    const roc_ops: *RocOps = utils.alignedPtrCast(*RocOps, @as([*]u8, @ptrCast(ctx)), @src());
    str_ptr.decref(roc_ops);
}

const FlatListElementDecrefContext = struct {
    inner_alignment: u32,
    inner_element_width: usize,
    roc_ops: *RocOps,
};

const CallbackElementDecrefContext = struct {
    callback: RcDropFn,
    roc_ops: *RocOps,
};

fn flatListElementDecref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    if (element == null) return;
    const ctx_ptr = context orelse unreachable;
    const ctx: *const FlatListElementDecrefContext = utils.alignedPtrCast(
        *const FlatListElementDecrefContext,
        @as([*]u8, @ptrCast(ctx_ptr)),
        @src(),
    );
    const inner_list: *RocList = utils.alignedPtrCast(*RocList, element.?, @src());
    inner_list.decref(
        ctx.inner_alignment,
        ctx.inner_element_width,
        false,
        null,
        &rcNone,
        ctx.roc_ops,
    );
}

fn callbackListElementDecref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    if (element == null) return;
    const ctx_ptr = context orelse unreachable;
    const ctx: *const CallbackElementDecrefContext = utils.alignedPtrCast(
        *const CallbackElementDecrefContext,
        @as([*]u8, @ptrCast(ctx_ptr)),
        @src(),
    );
    ctx.callback(element, ctx.roc_ops);
}

/// Wrapper: listWithCapacity
pub fn roc_builtins_list_with_capacity(out: *RocList, capacity: u64, alignment: u32, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    out.* = listWithCapacity(capacity, alignment, element_width, elements_refcounted, null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listSortWith using a backend-provided comparator trampoline.
pub fn roc_builtins_list_sort_with(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    cmp_fn_ptr: ?*const anyopaque,
    cmp_data: ?[*]u8,
    alignment: u32,
    element_width: usize,
    roc_ops: *RocOps,
) callconv(.c) void {
    if (list_len < 2 or element_width == 0) {
        out.* = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
        return;
    }

    const total_bytes = list_len * element_width;
    const sorted_bytes = allocateWithRefcountC(total_bytes, alignment, false, roc_ops);
    if (list_bytes) |src| {
        @memcpy(sorted_bytes[0..total_bytes], src[0..total_bytes]);
    }

    const cmp_fn: SortCmpFn = @ptrFromInt(@intFromPtr(cmp_fn_ptr orelse unreachable));
    const temp_ptr: [*]u8 = @ptrCast(roc_ops.alloc(@max(@as(usize, alignment), 1), element_width));
    defer roc_ops.dealloc(temp_ptr, @max(@as(usize, alignment), 1));

    var i: usize = 1;
    while (i < list_len) : (i += 1) {
        const elem_i = sorted_bytes + i * element_width;
        @memcpy(temp_ptr[0..element_width], elem_i[0..element_width]);

        var j: usize = i;
        while (j > 0) {
            const prev_elem = sorted_bytes + (j - 1) * element_width;
            const cmp_result = cmp_fn(cmp_data, temp_ptr, prev_elem);
            if (cmp_result != 2) break;

            const dst_elem = sorted_bytes + j * element_width;
            @memcpy(dst_elem[0..element_width], prev_elem[0..element_width]);
            j -= 1;
        }

        const insert_pos = sorted_bytes + j * element_width;
        @memcpy(insert_pos[0..element_width], temp_ptr[0..element_width]);
    }

    out.* = .{
        .bytes = sorted_bytes,
        .length = list_len,
        .capacity_or_alloc_ptr = list_len,
    };
}

/// Wrapper: listAppendUnsafe
pub fn roc_builtins_list_append_unsafe(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, element: ?[*]const u8, element_width: usize, _: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listAppendUnsafe(l, @constCast(element), element_width, @ptrCast(&copy_fallback));
}

/// Wrapper: listConcat(RocList, RocList, alignment, element_width, ..., *RocOps) -> RocList
pub fn roc_builtins_list_concat(out: *RocList, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, alignment: u32, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const a = RocList{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocList{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = listConcat(a, b, alignment, element_width, elements_refcounted, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listPrepend(RocList, alignment, element, element_width, ..., *RocOps) -> RocList
pub fn roc_builtins_list_prepend(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element: ?[*]u8, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listPrepend(l, alignment, element, element_width, elements_refcounted, null, @ptrCast(&rcNone), @ptrCast(&copy_fallback), roc_ops);
}

/// Wrapper: listSublist for drop_first/drop_last/take_first/take_last
pub fn roc_builtins_list_sublist(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, start: u64, len: u64, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listSublist(l, alignment, element_width, elements_refcounted, start, len, null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listReplace for list_set
pub fn roc_builtins_list_replace(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, index: u64, element: ?[*]u8, element_width: usize, out_element: ?[*]u8, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReplace(l, alignment, index, element, element_width, elements_refcounted, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), out_element, @ptrCast(&copy_fallback), roc_ops);
}

/// Wrapper: listReserve
pub fn roc_builtins_list_reserve(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, spare: u64, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReserve(l, alignment, spare, element_width, elements_refcounted, null, @ptrCast(&rcNone), .Immutable, roc_ops);
}

/// Wrapper: listReleaseExcessCapacity
pub fn roc_builtins_list_release_excess_capacity(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReleaseExcessCapacity(l, alignment, element_width, elements_refcounted, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), .Immutable, roc_ops);
}

/// Wrapper: decref a List(Str), including decref of each string element when unique
pub fn roc_builtins_list_decref_str(list_bytes: ?[*]u8, list_len: usize, list_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    listDecref(
        l,
        @alignOf(RocStr),
        @sizeOf(RocStr),
        true,
        @ptrCast(roc_ops),
        &strListElementDecref,
        roc_ops,
    );
}

/// Wrapper: decref a List(List a) where the inner lists do not themselves contain refcounted elements.
pub fn roc_builtins_list_decref_flat_list(
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    inner_alignment: u32,
    inner_element_width: usize,
    roc_ops: *RocOps,
) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    var ctx = FlatListElementDecrefContext{
        .inner_alignment = inner_alignment,
        .inner_element_width = inner_element_width,
        .roc_ops = roc_ops,
    };
    listDecref(
        l,
        @alignOf(RocList),
        @sizeOf(RocList),
        true,
        @ptrCast(&ctx),
        &flatListElementDecref,
        roc_ops,
    );
}

/// Decref a Roc list and optionally run an element decref callback when unique.
pub fn roc_builtins_list_decref_with(
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    element_width: usize,
    element_decref: ?RcDropFn,
    roc_ops: *RocOps,
) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (element_decref) |callback| {
        var ctx = CallbackElementDecrefContext{
            .callback = callback,
            .roc_ops = roc_ops,
        };
        listDecref(
            l,
            alignment,
            element_width,
            true,
            @ptrCast(&ctx),
            &callbackListElementDecref,
            roc_ops,
        );
    } else {
        decrefDataPtrC(l.getAllocationDataPtr(roc_ops), alignment, false, roc_ops);
    }
}

/// Wrapper: free a List(List a) where the inner lists do not themselves contain refcounted elements.
pub fn roc_builtins_list_free_flat_list(
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    inner_alignment: u32,
    inner_element_width: usize,
    roc_ops: *RocOps,
) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    var ctx = FlatListElementDecrefContext{
        .inner_alignment = inner_alignment,
        .inner_element_width = inner_element_width,
        .roc_ops = roc_ops,
    };

    if (l.getAllocationDataPtr(roc_ops)) |source| {
        const count = l.getAllocationElementCount(true, roc_ops);
        var i: usize = 0;
        while (i < count) : (i += 1) {
            flatListElementDecref(@ptrCast(&ctx), source + i * @sizeOf(RocList));
        }
    }

    freeDataPtrC(l.getAllocationDataPtr(roc_ops), @alignOf(RocList), true, roc_ops);
}

/// Free a Roc list and optionally run an element decref callback first.
pub fn roc_builtins_list_free_with(
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    element_width: usize,
    element_decref: ?RcDropFn,
    roc_ops: *RocOps,
) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };

    if (element_decref) |callback| {
        var ctx = CallbackElementDecrefContext{
            .callback = callback,
            .roc_ops = roc_ops,
        };

        if (l.getAllocationDataPtr(roc_ops)) |source| {
            const count = l.getAllocationElementCount(true, roc_ops);
            var i: usize = 0;
            while (i < count) : (i += 1) {
                callbackListElementDecref(@ptrCast(&ctx), source + i * element_width);
            }
        }

        freeDataPtrC(l.getAllocationDataPtr(roc_ops), alignment, true, roc_ops);
    } else {
        freeDataPtrC(l.getAllocationDataPtr(roc_ops), alignment, false, roc_ops);
    }
}

/// Decref a boxed payload and optionally run payload teardown when unique.
pub fn roc_builtins_box_decref_with(
    payload_ptr: ?[*]u8,
    payload_alignment: u32,
    payload_decref: ?RcDropFn,
    roc_ops: *RocOps,
) callconv(.c) void {
    if (payload_decref) |callback| {
        if (utils.isUnique(payload_ptr, roc_ops)) {
            callback(payload_ptr, roc_ops);
        }
    }

    decrefDataPtrC(payload_ptr, payload_alignment, false, roc_ops);
}

/// Free a boxed payload and optionally run payload teardown first.
pub fn roc_builtins_box_free_with(
    payload_ptr: ?[*]u8,
    payload_alignment: u32,
    payload_decref: ?RcDropFn,
    roc_ops: *RocOps,
) callconv(.c) void {
    if (payload_decref) |callback| {
        callback(payload_ptr, roc_ops);
    }

    freeDataPtrC(payload_ptr, payload_alignment, false, roc_ops);
}

// ═══════════════════════════════════════════════════════════════════════════
// Memory/Refcounting Wrappers (re-export with dev_ prefix for consistency)
// ═══════════════════════════════════════════════════════════════════════════

/// Re-export allocateWithRefcountC
pub fn roc_builtins_allocate_with_refcount(data_bytes: usize, element_alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) [*]u8 {
    return allocateWithRefcountC(data_bytes, element_alignment, elements_refcounted, roc_ops);
}

/// Re-export increfDataPtrC
pub fn roc_builtins_incref_data_ptr(ptr: ?[*]u8, amount: isize, roc_ops: *RocOps) callconv(.c) void {
    increfDataPtrC(ptr, amount, roc_ops);
}

/// Re-export decrefDataPtrC
pub fn roc_builtins_decref_data_ptr(ptr: ?[*]u8, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops);
}

/// Re-export freeDataPtrC
pub fn roc_builtins_free_data_ptr(ptr: ?[*]u8, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops);
}

// ═══════════════════════════════════════════════════════════════════════════
// Numeric Wrappers
// ═══════════════════════════════════════════════════════════════════════════

/// Wrapper: decToStrC (decomposed i128)
pub fn roc_builtins_dec_to_str(out: *RocStr, value_low: u64, value_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const value: i128 = @bitCast(i128h.from_u64_pair(value_low, value_high));
    const d = dec.RocDec{ .num = value };
    var buf: [dec.RocDec.max_str_length]u8 = undefined;
    const slice = d.format_to_buf(&buf);
    out.* = RocStr.init(&buf, slice.len, roc_ops);
}

// ── Numeric conversion wrappers ──

/// Dec (i128) → i64 by truncating division
pub fn roc_builtins_dec_to_i64_trunc(low: u64, high: u64) callconv(.c) i64 {
    const val: i128 = @bitCast(i128h.from_u64_pair(low, high));
    return @intCast(i128h.divTrunc_i128(val, dec.RocDec.one_point_zero_i128));
}

/// i64 → Dec (i128) via output pointers
pub fn roc_builtins_i64_to_dec(out_low: *u64, out_high: *u64, val: i64) callconv(.c) void {
    const result: i128 = if (dec.RocDec.fromWholeInt(@as(i128, val))) |d|
        d.num
    else if (val < 0)
        std.math.minInt(i128)
    else
        std.math.maxInt(i128);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

/// u64 → Dec (i128) via output pointers
pub fn roc_builtins_u64_to_dec(out_low: *u64, out_high: *u64, val: u64) callconv(.c) void {
    const result: i128 = dec.fromU64C(val);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

/// Dec (i128) → f64
pub fn roc_builtins_dec_to_f64(low: u64, high: u64) callconv(.c) f64 {
    const val: i128 = @bitCast(i128h.from_u64_pair(low, high));
    return dec.toF64(dec.RocDec{ .num = val });
}

/// i128 → f64
pub fn roc_builtins_i128_to_f64(low: u64, high: u64) callconv(.c) f64 {
    const val: i128 = @bitCast(i128h.from_u64_pair(low, high));
    return i128h.i128_to_f64(val);
}

/// u128 → f64
pub fn roc_builtins_u128_to_f64(low: u64, high: u64) callconv(.c) f64 {
    const val: u128 = i128h.from_u64_pair(low, high);
    return i128h.u128_to_f64(val);
}

/// f64 → i128 (saturating) via output pointers
pub fn roc_builtins_f64_to_i128_trunc(out_low: *u64, out_high: *u64, val: f64) callconv(.c) void {
    const result: i128 = i128h.f64_to_i128(val);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

/// f64 → u128 (saturating) via output pointers
pub fn roc_builtins_f64_to_u128_trunc(out_low: *u64, out_high: *u64, val: f64) callconv(.c) void {
    const result: u128 = i128h.f64_to_u128(val);
    out_low.* = @truncate(result);
    out_high.* = i128h.hi64(result);
}

// ── Try-conversion wrappers ──

fn i128InTargetRange(val: i128, target_bits: u32, target_signed: bool) bool {
    if (target_bits >= 128) {
        return if (target_signed) true else val >= 0;
    }
    if (target_signed) {
        const shift: u7 = @intCast(target_bits - 1);
        const min_val: i128 = -@as(i128, @bitCast(i128h.shl(1, shift)));
        const max_val: i128 = @as(i128, @bitCast(i128h.shl(1, shift))) - 1;
        return val >= min_val and val <= max_val;
    } else {
        if (val < 0) return false;
        const shift: u7 = @intCast(target_bits);
        const max_val: i128 = @as(i128, @bitCast(i128h.shl(1, shift))) - 1;
        return val <= max_val;
    }
}

fn u128InTargetRange(val: u128, target_bits: u32, target_signed: bool) bool {
    if (target_bits >= 128) {
        return if (target_signed) val <= @as(u128, @bitCast(@as(i128, std.math.maxInt(i128)))) else true;
    }
    if (target_signed) {
        const shift: u7 = @intCast(target_bits - 1);
        const max_val: u128 = i128h.shl(1, shift) - 1;
        return val <= max_val;
    } else {
        const shift: u7 = @intCast(target_bits);
        const max_val: u128 = i128h.shl(1, shift) - 1;
        return val <= max_val;
    }
}

/// i128 try convert
pub fn roc_builtins_i128_try_convert(out: [*]u8, val_low: u64, val_high: u64, target_bits: u32, target_is_signed: u32, payload_size: u32, disc_offset: u32) callconv(.c) void {
    const val: i128 = @bitCast(i128h.from_u64_pair(val_low, val_high));
    if (i128InTargetRange(val, target_bits, target_is_signed != 0)) {
        const payload_bytes: [16]u8 = @bitCast(@as(u128, @bitCast(val)));
        @memcpy(out[0..payload_size], payload_bytes[0..payload_size]);
        out[disc_offset] = 1;
    } else {
        out[disc_offset] = 0;
    }
}

/// u128 try convert
pub fn roc_builtins_u128_try_convert(out: [*]u8, val_low: u64, val_high: u64, target_bits: u32, target_is_signed: u32, payload_size: u32, disc_offset: u32) callconv(.c) void {
    const val: u128 = i128h.from_u64_pair(val_low, val_high);
    if (u128InTargetRange(val, target_bits, target_is_signed != 0)) {
        const payload_bytes: [16]u8 = @bitCast(val);
        @memcpy(out[0..payload_size], payload_bytes[0..payload_size]);
        out[disc_offset] = 1;
    } else {
        out[disc_offset] = 0;
    }
}

/// Signed integer try convert
pub fn roc_builtins_int_try_signed(out: [*]u8, val: i64, min_val: i64, max_val: i64, payload_size: u32, disc_offset: u32) callconv(.c) void {
    if (val >= min_val and val <= max_val) {
        const payload_bytes: [8]u8 = @bitCast(val);
        if (payload_size <= 8) {
            @memcpy(out[0..payload_size], payload_bytes[0..payload_size]);
        } else {
            @memcpy(out[0..8], &payload_bytes);
            @memset(out[8..payload_size], 0);
        }
        out[disc_offset] = 1;
    } else {
        out[disc_offset] = 0;
    }
}

/// Unsigned integer try convert
pub fn roc_builtins_int_try_unsigned(out: [*]u8, val: u64, max_val: u64, payload_size: u32, disc_offset: u32) callconv(.c) void {
    if (val <= max_val) {
        const payload_bytes: [8]u8 = @bitCast(@as(i64, @bitCast(val)));
        @memcpy(out[0..payload_size], payload_bytes[0..payload_size]);
        out[disc_offset] = 1;
    } else {
        out[disc_offset] = 0;
    }
}

/// Dec → integer try unsafe
pub fn roc_builtins_dec_to_int_try_unsafe(out: [*]u8, dec_low: u64, dec_high: u64, target_bits: u32, target_is_signed: u32, val_size: u32) callconv(.c) void {
    const dec_val: i128 = @bitCast(i128h.from_u64_pair(dec_low, dec_high));
    const one = dec.RocDec.one_point_zero_i128;

    const remainder = i128h.rem_i128(dec_val, one);
    const is_int: bool = remainder == 0;
    const int_val: i128 = i128h.divTrunc_i128(dec_val, one);

    const in_range: bool = blk: {
        if (target_is_signed != 0) {
            break :blk i128InTargetRange(int_val, target_bits, true);
        } else {
            if (int_val < 0) break :blk false;
            break :blk u128InTargetRange(@as(u128, @bitCast(int_val)), target_bits, false);
        }
    };

    if (is_int and in_range) {
        const v_bytes: [16]u8 = @bitCast(@as(u128, @bitCast(int_val)));
        @memcpy(out[0..val_size], v_bytes[0..val_size]);
    }

    out[val_size] = @intFromBool(is_int);
    out[val_size + 1] = @intFromBool(in_range);
}

/// f64 → integer try unsafe
pub fn roc_builtins_f64_to_int_try_unsafe(out: [*]u8, val: f64, target_bits: u32, target_is_signed: u32, val_size: u32) callconv(.c) void {
    const is_int: bool = !std.math.isNan(val) and !std.math.isInf(val) and @trunc(val) == val;

    const in_range: bool = blk: {
        if (target_is_signed != 0) {
            if (target_bits >= 128) {
                const min_f: f64 = comptime i128h.i128_to_f64(std.math.minInt(i128));
                const max_f: f64 = comptime i128h.i128_to_f64(std.math.maxInt(i128));
                break :blk val >= min_f and val <= max_f;
            }
            const shift: u6 = @intCast(target_bits - 1);
            const min_i: i64 = -(@as(i64, 1) << shift);
            const max_i: i64 = (@as(i64, 1) << shift) - 1;
            break :blk val >= @as(f64, @floatFromInt(min_i)) and val <= @as(f64, @floatFromInt(max_i));
        } else {
            if (val < 0) break :blk false;
            if (target_bits >= 128) {
                const max_f: f64 = comptime i128h.u128_to_f64(std.math.maxInt(u128));
                break :blk val <= max_f;
            }
            if (target_bits >= 64) {
                const max_f: f64 = @floatFromInt(@as(u64, std.math.maxInt(u64)));
                break :blk val <= max_f;
            }
            const shift: u6 = @intCast(target_bits);
            const max_u: u64 = (@as(u64, 1) << shift) - 1;
            break :blk val <= @as(f64, @floatFromInt(max_u));
        }
    };

    if (is_int and in_range) {
        if (target_is_signed != 0) {
            if (val_size <= 8) {
                const v: i64 = @intFromFloat(val);
                const v_bytes: [8]u8 = @bitCast(v);
                @memcpy(out[0..val_size], v_bytes[0..val_size]);
            } else {
                const v: i128 = i128h.f64_to_i128(val);
                const v_bytes: [16]u8 = @bitCast(@as(u128, @bitCast(v)));
                @memcpy(out[0..val_size], v_bytes[0..val_size]);
            }
        } else {
            if (val_size <= 8) {
                const v: u64 = @intFromFloat(val);
                const v_bytes: [8]u8 = @bitCast(v);
                @memcpy(out[0..val_size], v_bytes[0..val_size]);
            } else {
                const v: u128 = i128h.f64_to_u128(val);
                const v_bytes: [16]u8 = @bitCast(v);
                @memcpy(out[0..val_size], v_bytes[0..val_size]);
            }
        }
    }

    out[val_size] = @intFromBool(is_int);
    out[val_size + 1] = @intFromBool(in_range);
}

/// Dec → f32 try unsafe
pub fn roc_builtins_dec_to_f32_try_unsafe(out: [*]u8, dec_low: u64, dec_high: u64) callconv(.c) void {
    const dec_val: i128 = @bitCast(i128h.from_u64_pair(dec_low, dec_high));
    const f64_val: f64 = dec.toF64(dec.RocDec{ .num = dec_val });
    const f32_val: f32 = @floatCast(f64_val);
    const success: bool = !std.math.isInf(f32_val) and (!std.math.isNan(f64_val) or std.math.isNan(f32_val));
    const f32_bytes: [4]u8 = @bitCast(f32_val);
    @memcpy(out[0..4], &f32_bytes);
    out[4] = @intFromBool(success);
}

/// f64 → f32 try unsafe
pub fn roc_builtins_f64_to_f32_try_unsafe(out: [*]u8, val: f64) callconv(.c) void {
    const f32_val: f32 = @floatCast(val);
    const success: bool = !std.math.isInf(f32_val) and (!std.math.isNan(val) or std.math.isNan(f32_val));
    const f32_bytes: [4]u8 = @bitCast(f32_val);
    @memcpy(out[0..4], &f32_bytes);
    out[4] = @intFromBool(success);
}

/// i128 → Dec try unsafe
pub fn roc_builtins_i128_to_dec_try_unsafe(out: [*]u8, val_low: u64, val_high: u64) callconv(.c) void {
    const val: i128 = @bitCast(i128h.from_u64_pair(val_low, val_high));
    const result = dec.RocDec.fromWholeInt(val);
    const success = result != null;
    const dec_val: i128 = if (result) |d| d.num else 0;
    const dec_bytes: [16]u8 = @bitCast(@as(u128, @bitCast(dec_val)));
    @memcpy(out[0..16], &dec_bytes);
    out[16] = @intFromBool(success);
}

/// u128 → Dec try unsafe
pub fn roc_builtins_u128_to_dec_try_unsafe(out: [*]u8, val_low: u64, val_high: u64) callconv(.c) void {
    const val: u128 = i128h.from_u64_pair(val_low, val_high);
    const fits_i128 = val <= @as(u128, @bitCast(@as(i128, std.math.maxInt(i128))));
    const result: ?dec.RocDec = if (fits_i128) dec.RocDec.fromWholeInt(@as(i128, @bitCast(val))) else null;
    const success = result != null;
    const dec_val: i128 = if (result) |d| d.num else 0;
    const dec_bytes: [16]u8 = @bitCast(@as(u128, @bitCast(dec_val)));
    @memcpy(out[0..16], &dec_bytes);
    out[16] = @intFromBool(success);
}

// ── Dec arithmetic wrappers (decomposed i128) ──

/// Dec multiply saturated (decomposed)
pub fn roc_builtins_dec_mul_saturated(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    const b: i128 = @bitCast(i128h.from_u64_pair(b_low, b_high));
    const result = dec.mulSaturatedC(dec.RocDec{ .num = a }, dec.RocDec{ .num = b });
    out_low.* = @truncate(@as(u128, @bitCast(result.num)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result.num)));
}

/// Dec divide (decomposed)
pub fn roc_builtins_dec_div(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    const b: i128 = @bitCast(i128h.from_u64_pair(b_low, b_high));
    const result = dec.divC(dec.RocDec{ .num = a }, dec.RocDec{ .num = b }, roc_ops);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

/// Dec divide truncating (decomposed)
pub fn roc_builtins_dec_div_trunc(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    const b: i128 = @bitCast(i128h.from_u64_pair(b_low, b_high));
    const result = dec.divTruncC(dec.RocDec{ .num = a }, dec.RocDec{ .num = b }, roc_ops);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

// ── i128 div/rem wrappers (decomposed) ──

/// u128 div trunc (decomposed)
pub fn roc_builtins_num_div_trunc_u128(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: u128 = i128h.from_u64_pair(a_low, a_high);
    const b: u128 = i128h.from_u64_pair(b_low, b_high);
    const result = num.divTruncU128(a, b, roc_ops);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

/// i128 div trunc (decomposed)
pub fn roc_builtins_num_div_trunc_i128(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    const b: i128 = @bitCast(i128h.from_u64_pair(b_low, b_high));
    const result = num.divTruncI128(a, b, roc_ops);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

/// u128 rem trunc (decomposed)
pub fn roc_builtins_num_rem_trunc_u128(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: u128 = i128h.from_u64_pair(a_low, a_high);
    const b: u128 = i128h.from_u64_pair(b_low, b_high);
    const result = num.remTruncU128(a, b, roc_ops);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

/// i128 rem trunc (decomposed)
pub fn roc_builtins_num_rem_trunc_i128(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    const b: i128 = @bitCast(i128h.from_u64_pair(b_low, b_high));
    const result = num.remTruncI128(a, b, roc_ops);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

// ── List append safe wrapper ──

/// List append safe (simplified - copy=copy_fallback)
pub fn roc_builtins_list_append_safe(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, element: ?[*]const u8, alignment: u32, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    listAppendSafeC(out, list_bytes, list_len, list_cap, @constCast(element), alignment, element_width, elements_refcounted, @ptrCast(&copy_fallback), roc_ops);
}

// ── Numeric-to-string wrappers ──

/// Format a u128 to decimal string without using compiler_rt intrinsics.
/// Uses i128h.divTrunc_u128 and i128h.rem_u128 to avoid __udivti3/__umodti3.
fn u128ToStr(buf: []u8, val: u128) []u8 {
    if (val == 0) {
        buf[0] = '0';
        return buf[0..1];
    }
    var tmp: [40]u8 = undefined;
    var len: usize = 0;
    var v = val;
    while (v != 0) {
        const digit: u8 = @truncate(i128h.rem_u128(v, 10));
        tmp[len] = '0' + digit;
        len += 1;
        v = i128h.divTrunc_u128(v, 10);
    }
    // Reverse into output buffer
    for (0..len) |i| {
        buf[i] = tmp[len - 1 - i];
    }
    return buf[0..len];
}

/// Format an i128 to decimal string without using compiler_rt intrinsics.
fn i128ToStr(buf: []u8, val: i128) []u8 {
    if (val >= 0) {
        return u128ToStr(buf, @intCast(val));
    }
    buf[0] = '-';
    const abs: u128 = if (val == std.math.minInt(i128))
        @as(u128, @bitCast(val))
    else
        @intCast(-val);
    const digits = u128ToStr(buf[1..], abs);
    return buf[0 .. 1 + digits.len];
}

/// Unified integer-to-string wrapper: dispatches on int_width/is_signed
pub fn roc_builtins_int_to_str(out: *RocStr, val_low: u64, val_high: u64, int_width: u8, is_signed: bool, roc_ops: *RocOps) callconv(.c) void {
    var buf: [40]u8 = undefined;
    const result = switch (int_width) {
        1 => if (is_signed)
            std.fmt.bufPrint(&buf, "{}", .{@as(i8, @bitCast(@as(u8, @truncate(val_low))))}) catch unreachable
        else
            std.fmt.bufPrint(&buf, "{}", .{@as(u8, @truncate(val_low))}) catch unreachable,
        2 => if (is_signed)
            std.fmt.bufPrint(&buf, "{}", .{@as(i16, @bitCast(@as(u16, @truncate(val_low))))}) catch unreachable
        else
            std.fmt.bufPrint(&buf, "{}", .{@as(u16, @truncate(val_low))}) catch unreachable,
        4 => if (is_signed)
            std.fmt.bufPrint(&buf, "{}", .{@as(i32, @bitCast(@as(u32, @truncate(val_low))))}) catch unreachable
        else
            std.fmt.bufPrint(&buf, "{}", .{@as(u32, @truncate(val_low))}) catch unreachable,
        8 => if (is_signed)
            std.fmt.bufPrint(&buf, "{}", .{@as(i64, @bitCast(val_low))}) catch unreachable
        else
            std.fmt.bufPrint(&buf, "{}", .{val_low}) catch unreachable,
        16 => blk: {
            const val128: u128 = i128h.from_u64_pair(val_low, val_high);
            break :blk if (is_signed)
                i128ToStr(&buf, @as(i128, @bitCast(val128)))
            else
                u128ToStr(&buf, val128);
        },
        else => unreachable,
    };
    out.* = RocStr.init(&buf, result.len, roc_ops);
}

/// Unified float-to-string wrapper: dispatches on is_f32.
/// Uses Ryu's binaryToDecimal directly and formats manually to avoid
/// pulling in std.fmt.float.formatDecimal which references isPowerOf10
/// (u128 div/mod → __udivti3/__umodti3 compiler_rt symbols).
pub fn roc_builtins_float_to_str(out: *RocStr, val_bits: u64, is_f32: bool, roc_ops: *RocOps) callconv(.c) void {
    var buf: [400]u8 = undefined;
    const result = if (is_f32) blk: {
        const f32_val: f32 = @bitCast(@as(u32, @truncate(val_bits)));
        break :blk i128h.f32_to_str(&buf, f32_val);
    } else blk: {
        const f64_val: f64 = @bitCast(val_bits);
        break :blk i128h.f64_to_str(&buf, f64_val);
    };
    out.* = RocStr.init(&buf, result.len, roc_ops);
}

// ── Numeric-from-string wrappers ──

fn writeIntParseResult(comptime T: type, out: [*]u8, disc_offset: u32, roc_str: RocStr) void {
    const r = num.parseIntFromStr(T, roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(out[0..value_bytes.len], value_bytes);
    // Roc discriminants: Err=0, Ok=1 (alphabetically sorted)
    // parseIntFromStr errorcode: 0=success, 1=failure
    // So: Ok discriminant = 1 - errorcode
    out[disc_offset] = 1 - r.errorcode;
}

fn writeFloatParseResult(comptime T: type, out: [*]u8, disc_offset: u32, roc_str: RocStr) void {
    const r = num.parseFloatFromStr(T, roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(out[0..value_bytes.len], value_bytes);
    out[disc_offset] = 1 - r.errorcode;
}

/// Unified integer-from-string wrapper: parses a string into an integer of the given width.
/// Writes the result directly into the tag union memory at `out`:
///   - Payload (parsed value) at offset 0
///   - Discriminant (Ok=1, Err=0) at disc_offset
pub fn roc_builtins_int_from_str(
    out: [*]u8,
    str_bytes: ?[*]u8,
    str_len: usize,
    str_cap: usize,
    int_width: u8,
    is_signed: bool,
    disc_offset: u32,
) callconv(.c) void {
    const roc_str = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    if (is_signed) {
        switch (int_width) {
            1 => writeIntParseResult(i8, out, disc_offset, roc_str),
            2 => writeIntParseResult(i16, out, disc_offset, roc_str),
            4 => writeIntParseResult(i32, out, disc_offset, roc_str),
            8 => writeIntParseResult(i64, out, disc_offset, roc_str),
            16 => writeIntParseResult(i128, out, disc_offset, roc_str),
            else => unreachable,
        }
    } else {
        switch (int_width) {
            1 => writeIntParseResult(u8, out, disc_offset, roc_str),
            2 => writeIntParseResult(u16, out, disc_offset, roc_str),
            4 => writeIntParseResult(u32, out, disc_offset, roc_str),
            8 => writeIntParseResult(u64, out, disc_offset, roc_str),
            16 => writeIntParseResult(u128, out, disc_offset, roc_str),
            else => unreachable,
        }
    }
}

/// Dec-from-string wrapper: parses a string into a Dec (i128).
/// Writes the result directly into the tag union memory at `out`.
pub fn roc_builtins_dec_from_str(
    out: [*]u8,
    str_bytes: ?[*]u8,
    str_len: usize,
    str_cap: usize,
    disc_offset: u32,
) callconv(.c) void {
    const roc_str = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    const r = dec.fromStr(roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(out[0..value_bytes.len], value_bytes);
    out[disc_offset] = 1 - r.errorcode;
}

/// Float-from-string wrapper: parses a string into f32 or f64.
/// Writes the result directly into the tag union memory at `out`.
pub fn roc_builtins_float_from_str(
    out: [*]u8,
    str_bytes: ?[*]u8,
    str_len: usize,
    str_cap: usize,
    float_width: u8,
    disc_offset: u32,
) callconv(.c) void {
    const roc_str = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    switch (float_width) {
        4 => writeFloatParseResult(f32, out, disc_offset, roc_str),
        8 => writeFloatParseResult(f64, out, disc_offset, roc_str),
        else => unreachable,
    }
}
