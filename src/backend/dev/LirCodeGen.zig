//! LIR Code Generator
//!
//! This module generates native machine code from LIR expressions.
//! It uses the Emit.zig infrastructure for instruction encoding and
//! ValueStorage.zig for register allocation.
//!
//! Pipeline position:
//! ```
//! CIR -> LIR Lowering -> LirCodeGen -> Machine Code
//! ```
//!
//! Key properties:
//! - Uses real machine instructions via Emit.zig
//! - Proper register allocation with spilling support
//! - Handles System V ABI (x86_64/aarch64) calling convention
//! - Generates position-independent code with relocations
//! - Supports x86_64 and aarch64 architectures

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const lir = @import("lir");
const builtins = @import("builtins");
const dev_wrappers = builtins.dev_wrappers;

const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");
const CallingConventionMod = @import("CallingConvention.zig");
const CallingConvention = CallingConventionMod.CallingConvention;
const RocTarget = @import("roc_target").RocTarget;

// Num builtin functions for 128-bit integer operations

// Utils builtin functions for memory allocation and reference counting
const allocateWithRefcountC = builtins.utils.allocateWithRefcountC;
const increfDataPtrC = builtins.utils.increfDataPtrC;
const decrefDataPtrC = builtins.utils.decrefDataPtrC;
const freeDataPtrC = builtins.utils.freeDataPtrC;
const rcNone = builtins.utils.rcNone;

// List builtin functions - using C-compatible wrappers to avoid ABI issues
// with 24-byte RocList struct returns on aarch64
const copy_fallback = builtins.list.copy_fallback;
const RocList = builtins.list.RocList;

// Additional list builtins (return RocList by value with callconv(.c))
const listConcat = builtins.list.listConcat;
const listPrepend = builtins.list.listPrepend;
const listReplace = builtins.list.listReplace;
const listReserve = builtins.list.listReserve;
const listReleaseExcessCapacity = builtins.list.listReleaseExcessCapacity;

// String builtins
const strToUtf8C = builtins.str.strToUtf8C;
const strConcatC = builtins.str.strConcatC;
const strContains = builtins.str.strContains;
const strStartsWith = builtins.str.startsWith;
const strEndsWith = builtins.str.endsWith;
const strCountUtf8Bytes = builtins.str.countUtf8Bytes;
const strCaselessAsciiEquals = builtins.str.strCaselessAsciiEquals;
const strEqual = builtins.str.strEqual;
const strRepeatC = builtins.str.repeatC;
const strTrim = builtins.str.strTrim;
const strTrimStart = builtins.str.strTrimStart;
const strTrimEnd = builtins.str.strTrimEnd;
const strSplitOn = builtins.str.strSplitOn;
const strJoinWithC = builtins.str.strJoinWithC;
const strReserveC = builtins.str.reserveC;
const strReleaseExcessCapacity = builtins.str.strReleaseExcessCapacity;
const strWithCapacityC = builtins.str.withCapacityC;
const strDropPrefix = builtins.str.strDropPrefix;
const strDropSuffix = builtins.str.strDropSuffix;
const strWithAsciiLowercased = builtins.str.strWithAsciiLowercased;
const strWithAsciiUppercased = builtins.str.strWithAsciiUppercased;
const strFromUtf8Lossy = builtins.str.fromUtf8Lossy;
const strFromUtf8C = builtins.str.fromUtf8C;
const FromUtf8Try = builtins.str.FromUtf8Try;

const Relocation = @import("Relocation.zig").Relocation;
const StaticDataInterner = @import("StaticDataInterner.zig");

const LirExprStore = lir.LirExprStore;
const LirExpr = lir.LirExpr;
const LirExprId = lir.LirExprId;
const LirPatternId = lir.LirPatternId;
const Symbol = lir.Symbol;
const JoinPointId = lir.JoinPointId;
// Layout store for accessing struct/tag field offsets
const LayoutStore = layout.Store;
const RcHelperResolver = layout.RcHelperResolver;
const RcHelperKey = layout.RcHelperKey;
const RcOp = layout.RcOp;

// Control flow statement types (for two-pass compilation)
const CFStmtId = lir.CFStmtId;
const LayoutIdxSpan = lir.LayoutIdxSpan;

/// Generation mode determines how builtin function calls are emitted.
/// This is important because the dev backend can be used in two ways:
/// 1. In-process execution (dev evaluator): Direct function pointers work
/// 2. Object file generation (roc build --opt=dev): Need symbol references
pub const GenerationMode = enum {
    /// Code runs in-process (dev evaluator), direct function pointers are valid.
    /// The compiled code calls builtins via absolute addresses embedded in the code.
    native_execution,
    /// Generating relocatable object files for linking.
    /// Builtin calls must use symbol references that the linker will resolve.
    object_file,
};

/// Builtin function identifiers for the dev backend.
/// These map to exported symbols in dev_wrappers.zig for object file generation.
pub const BuiltinFn = enum {
    // Memory/refcounting
    allocate_with_refcount,
    incref_data_ptr,
    decref_data_ptr,
    free_data_ptr,

    // String operations
    str_to_utf8,
    str_concat,
    str_contains,
    str_starts_with,
    str_ends_with,
    str_equal,
    str_count_utf8_bytes,
    str_caseless_ascii_equals,
    str_repeat,
    str_trim,
    str_trim_start,
    str_trim_end,
    str_split,
    str_join_with,
    str_reserve,
    str_release_excess_capacity,
    str_with_capacity,
    str_drop_prefix,
    str_drop_suffix,
    str_with_ascii_lowercased,
    str_with_ascii_uppercased,
    str_from_utf8_lossy,
    str_from_utf8,
    str_escape_and_quote,

    // List operations
    list_with_capacity,
    list_append_unsafe,
    list_append_safe,
    list_concat,
    list_prepend,
    list_sublist,
    list_replace,
    list_reserve,
    list_release_excess_capacity,
    list_decref_str,
    list_decref_with,
    list_decref_flat_list,
    list_free_with,
    list_free_flat_list,
    box_decref_with,
    box_free_with,
    list_sort_with,

    // Numeric operations
    dec_to_str,
    dec_mul_saturated,
    dec_div,
    dec_div_trunc,
    dec_to_f64,
    i128_to_f64,
    u128_to_f64,
    f64_to_i128_trunc,
    f64_to_u128_trunc,
    i64_to_dec,
    u64_to_dec,
    dec_to_i64_trunc,
    i128_try_convert,
    u128_try_convert,
    int_try_signed,
    int_try_unsigned,
    dec_to_int_try_unsafe,
    f64_to_int_try_unsafe,
    dec_to_f32_try_unsafe,
    f64_to_f32_try_unsafe,
    i128_to_dec_try_unsafe,
    u128_to_dec_try_unsafe,
    num_div_trunc_u128,
    num_div_trunc_i128,
    num_rem_trunc_u128,
    num_rem_trunc_i128,
    int_to_str,
    float_to_str,
    int_from_str,
    dec_from_str,
    float_from_str,

    /// Get the exported symbol name for object file linking.
    pub fn symbolName(self: BuiltinFn) []const u8 {
        return switch (self) {
            // Memory/refcounting
            .allocate_with_refcount => "roc_builtins_allocate_with_refcount",
            .incref_data_ptr => "roc_builtins_incref_data_ptr",
            .decref_data_ptr => "roc_builtins_decref_data_ptr",
            .free_data_ptr => "roc_builtins_free_data_ptr",

            // String operations
            .str_to_utf8 => "roc_builtins_str_to_utf8",
            .str_concat => "roc_builtins_str_concat",
            .str_contains => "roc_builtins_str_contains",
            .str_starts_with => "roc_builtins_str_starts_with",
            .str_ends_with => "roc_builtins_str_ends_with",
            .str_equal => "roc_builtins_str_equal",
            .str_count_utf8_bytes => "roc_builtins_str_count_utf8_bytes",
            .str_caseless_ascii_equals => "roc_builtins_str_caseless_ascii_equals",
            .str_repeat => "roc_builtins_str_repeat",
            .str_trim => "roc_builtins_str_trim",
            .str_trim_start => "roc_builtins_str_trim_start",
            .str_trim_end => "roc_builtins_str_trim_end",
            .str_split => "roc_builtins_str_split",
            .str_join_with => "roc_builtins_str_join_with",
            .str_reserve => "roc_builtins_str_reserve",
            .str_release_excess_capacity => "roc_builtins_str_release_excess_capacity",
            .str_with_capacity => "roc_builtins_str_with_capacity",
            .str_drop_prefix => "roc_builtins_str_drop_prefix",
            .str_drop_suffix => "roc_builtins_str_drop_suffix",
            .str_with_ascii_lowercased => "roc_builtins_str_with_ascii_lowercased",
            .str_with_ascii_uppercased => "roc_builtins_str_with_ascii_uppercased",
            .str_from_utf8_lossy => "roc_builtins_str_from_utf8_lossy",
            .str_from_utf8 => "roc_builtins_str_from_utf8",
            .str_escape_and_quote => "roc_builtins_str_escape_and_quote",

            // List operations
            .list_with_capacity => "roc_builtins_list_with_capacity",
            .list_append_unsafe => "roc_builtins_list_append_unsafe",
            .list_append_safe => "roc_builtins_list_append_safe",
            .list_concat => "roc_builtins_list_concat",
            .list_prepend => "roc_builtins_list_prepend",
            .list_sublist => "roc_builtins_list_sublist",
            .list_replace => "roc_builtins_list_replace",
            .list_reserve => "roc_builtins_list_reserve",
            .list_release_excess_capacity => "roc_builtins_list_release_excess_capacity",
            .list_decref_str => "roc_builtins_list_decref_str",
            .list_decref_with => "roc_builtins_list_decref_with",
            .list_decref_flat_list => "roc_builtins_list_decref_flat_list",
            .list_free_with => "roc_builtins_list_free_with",
            .list_free_flat_list => "roc_builtins_list_free_flat_list",
            .box_decref_with => "roc_builtins_box_decref_with",
            .box_free_with => "roc_builtins_box_free_with",
            .list_sort_with => "roc_builtins_list_sort_with",

            // Numeric operations
            .dec_to_str => "roc_builtins_dec_to_str",
            .dec_mul_saturated => "roc_builtins_dec_mul_saturated",
            .dec_div => "roc_builtins_dec_div",
            .dec_div_trunc => "roc_builtins_dec_div_trunc",
            .dec_to_f64 => "roc_builtins_dec_to_f64",
            .i128_to_f64 => "roc_builtins_i128_to_f64",
            .u128_to_f64 => "roc_builtins_u128_to_f64",
            .f64_to_i128_trunc => "roc_builtins_f64_to_i128_trunc",
            .f64_to_u128_trunc => "roc_builtins_f64_to_u128_trunc",
            .i64_to_dec => "roc_builtins_i64_to_dec",
            .u64_to_dec => "roc_builtins_u64_to_dec",
            .dec_to_i64_trunc => "roc_builtins_dec_to_i64_trunc",
            .i128_try_convert => "roc_builtins_i128_try_convert",
            .u128_try_convert => "roc_builtins_u128_try_convert",
            .int_try_signed => "roc_builtins_int_try_signed",
            .int_try_unsigned => "roc_builtins_int_try_unsigned",
            .dec_to_int_try_unsafe => "roc_builtins_dec_to_int_try_unsafe",
            .f64_to_int_try_unsafe => "roc_builtins_f64_to_int_try_unsafe",
            .dec_to_f32_try_unsafe => "roc_builtins_dec_to_f32_try_unsafe",
            .f64_to_f32_try_unsafe => "roc_builtins_f64_to_f32_try_unsafe",
            .i128_to_dec_try_unsafe => "roc_builtins_i128_to_dec_try_unsafe",
            .u128_to_dec_try_unsafe => "roc_builtins_u128_to_dec_try_unsafe",
            .num_div_trunc_u128 => "roc_builtins_num_div_trunc_u128",
            .num_div_trunc_i128 => "roc_builtins_num_div_trunc_i128",
            .num_rem_trunc_u128 => "roc_builtins_num_rem_trunc_u128",
            .num_rem_trunc_i128 => "roc_builtins_num_rem_trunc_i128",
            .int_to_str => "roc_builtins_int_to_str",
            .float_to_str => "roc_builtins_float_to_str",
            .int_from_str => "roc_builtins_int_from_str",
            .dec_from_str => "roc_builtins_dec_from_str",
            .float_from_str => "roc_builtins_float_from_str",
        };
    }
};

/// Special layout index for List I64 type (must match dev_evaluator.zig).
/// Lists are (ptr, len, capacity) = 24 bytes and need special handling when returning results.

// Number-to-string C wrapper functions (explicit output pointer to avoid struct return ABI issues)
const RocStr = builtins.str.RocStr;
const RocOps = builtins.host_abi.RocOps;

fn decToStrC(out: *RocStr, value: i128, roc_ops: *RocOps) callconv(.c) void {
    const dec = builtins.dec.RocDec{ .num = value };
    var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
    const slice = dec.format_to_buf(&buf);
    out.* = RocStr.init(&buf, slice.len, roc_ops);
}

/// Wrapper: decToStrC(i128, *RocOps) -> RocStr
/// Decomposed: (out, value_low, value_high, roc_ops) -> void
/// This avoids platform-specific i128 passing conventions (Windows ARM64 vs Unix aarch64).
fn wrapDecToStr(out: *RocStr, value_low: u64, value_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const value: i128 = @bitCast((@as(u128, value_high) << 64) | @as(u128, value_low));
    decToStrC(out, value, roc_ops);
}

// ── C wrapper functions for string/list builtins ──
// These decompose 24-byte RocStr/RocList structs into individual 8-byte fields
// so all args fit in registers and we avoid platform-specific struct-passing ABI issues.

/// Wrapper: strToUtf8C(RocStr, *RocOps) -> RocList
/// Decomposed: (out, str_bytes, str_len, str_cap, roc_ops) -> void
fn wrapStrToUtf8(out: *RocList, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const arg = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strToUtf8C(arg, roc_ops);
}

/// Wrapper: strConcatC(RocStr, RocStr, *RocOps) -> RocStr
/// Decomposed: (out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, roc_ops) -> void
fn wrapStrConcat(out: *RocStr, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strConcatC(a, b, roc_ops);
}

/// Wrapper: strContains(RocStr, RocStr) -> bool
/// Decomposed: (a_bytes, a_len, a_cap, b_bytes, b_len, b_cap) -> bool
fn wrapStrContains(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return strContains(a, b);
}

/// Wrapper: startsWith(RocStr, RocStr) -> bool
fn wrapStrStartsWith(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return strStartsWith(a, b);
}

/// Wrapper: endsWith(RocStr, RocStr) -> bool
fn wrapStrEndsWith(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return strEndsWith(a, b);
}

/// Wrapper: strEqual(RocStr, RocStr) -> bool
fn wrapStrEqual(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return strEqual(a, b);
}

/// Wrapper: countUtf8Bytes(RocStr) -> u64
fn wrapStrCountUtf8Bytes(str_bytes: ?[*]u8, str_len: usize, str_cap: usize) callconv(.c) u64 {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    return strCountUtf8Bytes(s);
}

/// Wrapper: strCaselessAsciiEquals(RocStr, RocStr) -> bool
fn wrapStrCaselessAsciiEquals(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    return strCaselessAsciiEquals(a, b);
}

/// Wrapper: repeatC(RocStr, u64, *RocOps) -> RocStr
fn wrapStrRepeat(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, count: u64, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strRepeatC(s, count, roc_ops);
}

/// Wrapper: strTrim(RocStr, *RocOps) -> RocStr
fn wrapStrTrim(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrim(s, roc_ops);
}

/// Wrapper: strTrimStart(RocStr, *RocOps) -> RocStr
fn wrapStrTrimStart(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrimStart(s, roc_ops);
}

/// Wrapper: strTrimEnd(RocStr, *RocOps) -> RocStr
fn wrapStrTrimEnd(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrimEnd(s, roc_ops);
}

/// Wrapper: strSplitOn(RocStr, RocStr, *RocOps) -> RocList
fn wrapStrSplit(out: *RocList, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strSplitOn(a, b, roc_ops);
}

/// Wrapper: strJoinWithC(RocList, RocStr, *RocOps) -> RocStr
fn wrapStrJoinWith(out: *RocStr, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, sep_bytes: ?[*]u8, sep_len: usize, sep_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    const sep = RocStr{ .bytes = sep_bytes, .length = sep_len, .capacity_or_alloc_ptr = sep_cap };
    out.* = strJoinWithC(list, sep, roc_ops);
}

/// Wrapper: reserveC(RocStr, u64, *RocOps) -> RocStr
fn wrapStrReserve(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, spare: u64, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strReserveC(s, spare, roc_ops);
}

/// Wrapper: strReleaseExcessCapacity(*RocOps, RocStr) -> RocStr
fn wrapStrReleaseExcessCapacity(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strReleaseExcessCapacity(roc_ops, s);
}

/// Wrapper: withCapacityC(u64, *RocOps) -> RocStr
fn wrapStrWithCapacity(out: *RocStr, capacity: u64, roc_ops: *RocOps) callconv(.c) void {
    out.* = strWithCapacityC(capacity, roc_ops);
}

/// Wrapper: strDropPrefix(RocStr, RocStr, *RocOps) -> RocStr
fn wrapStrDropPrefix(out: *RocStr, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strDropPrefix(a, b, roc_ops);
}

/// Wrapper: strDropSuffix(RocStr, RocStr, *RocOps) -> RocStr
fn wrapStrDropSuffix(out: *RocStr, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strDropSuffix(a, b, roc_ops);
}

/// Wrapper: strWithAsciiLowercased(RocStr, *RocOps) -> RocStr
fn wrapStrWithAsciiLowercased(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strWithAsciiLowercased(s, roc_ops);
}

/// Wrapper: strWithAsciiUppercased(RocStr, *RocOps) -> RocStr
fn wrapStrWithAsciiUppercased(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strWithAsciiUppercased(s, roc_ops);
}

/// Wrapper: fromUtf8Lossy(RocList, *RocOps) -> RocStr
fn wrapStrFromUtf8Lossy(out: *RocStr, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = strFromUtf8Lossy(list, roc_ops);
}

/// Wrapper: fromUtf8C(RocList, UpdateMode, *RocOps) -> FromUtf8Try
fn wrapStrFromUtf8(out: [*]u8, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    const result = strFromUtf8C(list, .Immutable, roc_ops);
    @as(*FromUtf8Try, @ptrCast(@alignCast(out))).* = result;
}

/// Wrapper: escape special characters and wrap in double quotes for Str.inspect
fn wrapStrEscapeAndQuote(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    // Reconstruct the RocStr so asSlice() handles both small and large strings
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    const slice = s.asSlice();

    // Count extra bytes needed for escaping backslashes and quotes
    var extra: usize = 0;
    for (slice) |ch| {
        if (ch == '\\' or ch == '"') extra += 1;
    }

    const result_len = slice.len + extra + 2; // +2 for surrounding quotes

    const small_string_size = @sizeOf(RocStr);
    if (result_len < small_string_size) {
        // Small string: build inline
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
        // Large string: allocate heap memory
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

/// Wrapper: listConcat(RocList, RocList, alignment, element_width, ..., *RocOps) -> RocList
fn wrapListConcat(out: *RocList, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, alignment: u32, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const a = RocList{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocList{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = listConcat(a, b, alignment, element_width, elements_refcounted, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listPrepend(RocList, alignment, element, element_width, ..., *RocOps) -> RocList
fn wrapListPrepend(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element: ?[*]u8, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listPrepend(list, alignment, element, element_width, elements_refcounted, null, @ptrCast(&rcNone), @ptrCast(&copy_fallback), roc_ops);
}

/// Wrapper: listReplace for list_set
fn wrapListReplace(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, index: u64, element: ?[*]u8, element_width: usize, elements_refcounted: bool, out_element: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReplace(list, alignment, index, element, element_width, elements_refcounted, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), out_element, @ptrCast(&copy_fallback), roc_ops);
}

/// Wrapper: listReserve
fn wrapListReserve(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, spare: u64, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReserve(list, alignment, spare, element_width, elements_refcounted, null, @ptrCast(&rcNone), .Immutable, roc_ops);
}

/// Wrapper: listReleaseExcessCapacity
fn wrapListReleaseExcessCapacity(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReleaseExcessCapacity(list, alignment, element_width, elements_refcounted, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), .Immutable, roc_ops);
}

/// Context passed through the opaque `cmp_data` pointer to the sort comparison trampoline.
const SortCmpContext = extern struct {
    roc_fn_addr: usize,
    element_width: usize,
};

/// C-callable comparison trampoline for listSortWith.
/// Loads element values from pointers and calls the compiled Roc comparison function.
fn sortCmpTrampoline(cmp_data: ?[*]u8, a_ptr: ?[*]u8, b_ptr: ?[*]u8) callconv(.c) u8 {
    const ctx: *const SortCmpContext = @ptrCast(@alignCast(cmp_data));
    const ew = ctx.element_width;

    if (ew <= 8) {
        // Safe to pass values directly: single-register types (u8..u64) have
        // identical layouts in both C callconv and the Roc internal calling
        // convention on all platforms, so no ABI mismatch is possible.
        const cmp_fn: *const fn (u64, u64) callconv(.c) u8 = @ptrFromInt(ctx.roc_fn_addr);
        var a_val: u64 = 0;
        var b_val: u64 = 0;
        if (a_ptr) |ap| @memcpy(@as([*]u8, @ptrCast(&a_val))[0..ew], ap[0..ew]);
        if (b_ptr) |bp| @memcpy(@as([*]u8, @ptrCast(&b_val))[0..ew], bp[0..ew]);
        return cmp_fn(a_val, b_val);
    } else {
        // For ew > 8 (multi-register types like Dec/i128/u128 and large structs),
        // we pass element pointers directly. The comparator lambda is compiled with
        // force_pass_by_ptr so its prologue loads values from these pointers.
        //
        // This avoids ABI mismatches between the Zig callconv(.c) and the Roc
        // internal calling convention. For example:
        // - Windows C ABI passes u128 by pointer (RCX=&a, RDX=&b), but the Roc
        //   lambda's bindLambdaParams may convert only the first param to pointer
        //   and pass the second in registers (RCX=&a, RDX=b_low, R8=b_high).
        // - System V C ABI passes large structs by pointer, but bindLambdaParams
        //   may keep one param in registers if it fits.
        const cmp_fn: *const fn (?[*]u8, ?[*]u8) callconv(.c) u8 = @ptrFromInt(ctx.roc_fn_addr);
        return cmp_fn(a_ptr, b_ptr);
    }
}

/// Wrapper: listSortWith — sorts a list using a compiled Roc comparison function.
/// Uses a simple insertion sort to avoid ABI complexities with fluxsort.
fn wrapListSortWith(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    cmp_fn_addr: usize,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.c) void {
    if (list_len < 2) {
        out.* = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
        return;
    }

    // Allocate a new list for the sorted result
    const total_bytes = list_len * element_width;
    const sorted_bytes = allocateWithRefcountC(total_bytes, alignment, elements_refcounted, roc_ops);
    if (list_bytes) |src| {
        @memcpy(sorted_bytes[0..total_bytes], src[0..total_bytes]);
    }

    // Insertion sort using the comparison trampoline
    const cmp_ctx = SortCmpContext{
        .roc_fn_addr = cmp_fn_addr,
        .element_width = element_width,
    };

    var temp_buf: [256]u8 align(16) = undefined;

    var i: usize = 1;
    while (i < list_len) : (i += 1) {
        // Save element[i] to temp
        const elem_i = sorted_bytes + i * element_width;
        @memcpy(temp_buf[0..element_width], elem_i[0..element_width]);

        // Shift elements right until we find the insertion point
        var j: usize = i;
        while (j > 0) {
            const elem_j_minus_1 = sorted_bytes + (j - 1) * element_width;
            // Compare temp (element being inserted) with element[j-1]
            const cmp_result = sortCmpTrampoline(@ptrCast(@constCast(&cmp_ctx)), &temp_buf, elem_j_minus_1);
            if (cmp_result != 2) break; // not LT, stop shifting (EQ=0, GT=1)
            // Shift element[j-1] to element[j]
            const elem_j = sorted_bytes + j * element_width;
            @memcpy(elem_j[0..element_width], elem_j_minus_1[0..element_width]);
            j -= 1;
        }
        // Insert temp at position j
        const insert_pos = sorted_bytes + j * element_width;
        @memcpy(insert_pos[0..element_width], temp_buf[0..element_width]);
    }

    out.* = RocList{
        .bytes = sorted_bytes,
        .length = list_len,
        .capacity_or_alloc_ptr = list_len,
    };
}

const LirProcSpec = lir.LirProcSpec;

const Allocator = std.mem.Allocator;

/// Code generator for LIR expressions
/// Parameterized by RocTarget for cross-compilation support
pub fn LirCodeGen(comptime target: RocTarget) type {
    // Validate target architecture is supported
    const arch = target.toCpuArch();
    if (arch != .x86_64 and arch != .aarch64 and arch != .aarch64_be) {
        @compileError("LirCodeGen requires x86_64 or aarch64 target");
    }

    // ── Target-specific size constants ──
    // These are derived from the target pointer size for the 64-bit architectures we support.
    // RocStr and RocList both have the same layout: { ptr: [*]u8, length: usize, capacity: usize }

    // Size of a pointer on the target architecture (8 bytes for x86_64/aarch64)
    const target_ptr_size: u32 = 8;

    // Size of a RocStr struct: ptr + length + capacity = 3 × pointer size (24 bytes on 64-bit)
    const roc_str_size: u32 = 3 * target_ptr_size;

    // Size of a RocList struct: ptr + length + capacity = 3 × pointer size (24 bytes on 64-bit)
    const roc_list_size: u32 = 3 * target_ptr_size;

    // Maximum length for small string optimization (struct size minus length byte, 23 bytes on 64-bit)
    const small_str_max_len: u32 = roc_str_size - 1;

    // Select architecture-specific types based on target
    const CodeGen = if (arch == .x86_64)
        x86_64.CodeGen(target)
    else
        aarch64.CodeGen(target);

    const GeneralReg = if (arch == .x86_64)
        x86_64.GeneralReg
    else
        aarch64.GeneralReg;

    const FloatReg = if (arch == .x86_64)
        x86_64.FloatReg
    else
        aarch64.FloatReg;

    const Condition = if (arch == .x86_64)
        x86_64.Emit(target).Condition
    else
        aarch64.Emit(target).Condition;

    return struct {
        const Self = @This();
        const call_stack_alignment: u32 = switch (arch) {
            .aarch64, .aarch64_be => aarch64.Call.STACK_ALIGNMENT,
            .x86_64 => if (target.isWindows())
                x86_64.WindowsFastcall.STACK_ALIGNMENT
            else
                x86_64.SystemV.STACK_ALIGNMENT,
            else => unreachable,
        };

        /// The target this LirCodeGen was instantiated for
        pub const roc_target = target;

        /// Frame pointer register for the target architecture
        const frame_ptr: GeneralReg = if (arch == .x86_64) .RBP else .FP;

        /// Stack pointer register for the target architecture
        const stack_ptr: GeneralReg = if (arch == .x86_64) .RSP else .ZRSP;

        /// Scratch/temporary register (not preserved across calls)
        const scratch_reg: GeneralReg = if (arch == .x86_64) .R11 else .X9;

        /// Return value registers (first, second, third)
        const ret_reg_0: GeneralReg = if (arch == .x86_64) .RAX else .X0;
        const ret_reg_1: GeneralReg = if (arch == .x86_64) .RDX else .X1;
        const ret_reg_2: GeneralReg = if (arch == .x86_64) .RCX else .X2;

        /// CallBuilder type alias for this architecture's emit type
        const Builder = CallingConventionMod.CallBuilder(@TypeOf(@as(CodeGen, undefined).emit));

        allocator: Allocator,

        /// Calling convention for the target platform (derived from comptime target)
        cc: CallingConvention,

        /// Architecture-specific code generator with register allocation
        codegen: CodeGen,

        /// The LIR store containing expressions to compile
        store: *const LirExprStore,

        /// Layout store for accessing struct/tag field offsets
        layout_store: *const LayoutStore,

        /// Static data interner for string literals
        static_interner: ?*StaticDataInterner,

        /// Map from Symbol to value location (register or stack slot)
        symbol_locations: std.AutoHashMap(u64, ValueLocation),

        /// Map from mutable variable symbol to fixed stack slot info
        /// Mutable variables need fixed slots so re-bindings can update the value at runtime
        mutable_var_slots: std.AutoHashMap(u64, MutableVarInfo),

        /// Map from JoinPointId to code offset (for recursive closure jumps)
        join_points: std.AutoHashMap(u32, usize),

        /// Current recursive context (for detecting recursive calls)
        /// When set, lookups of this symbol should jump to the join point instead of re-entering
        current_recursive_symbol: ?Symbol,
        current_recursive_join_point: ?JoinPointId,

        /// The symbol currently being bound (during let statement processing).
        current_binding_symbol: ?Symbol,

        /// Registry of compiled procedures (proc-spec id -> CompiledProc)
        /// Used to find call targets during second pass
        proc_registry: std.AutoHashMap(u32, CompiledProc),

        /// Registry of compiled RC helpers keyed by canonical RC helper identity.
        compiled_rc_helpers: std.AutoHashMap(u64, usize),

        /// Pending calls that need to be patched after all procedures are compiled
        pending_calls: std.ArrayList(PendingCall),

        /// Map from JoinPointId to list of jumps that target it (for patching)
        join_point_jumps: std.AutoHashMap(u32, std.ArrayList(JumpRecord)),

        /// Map from JoinPointId to parameter layouts (for i128 handling in rebind)
        join_point_param_layouts: std.AutoHashMap(u32, LayoutIdxSpan),

        /// Map from JoinPointId to parameter patterns (for rebinding to correct stack slots)
        join_point_param_patterns: std.AutoHashMap(u32, lir.LirPatternSpan),

        /// Tracks positions of BL/CALL instructions to compiled nested proc_specs.
        /// When deferred-prologue compilation shifts a proc body (extract, prepend prologue,
        /// re-append), BL instructions targeting code outside the body become
        /// incorrect. This list lets us re-patch those instructions after shifts.
        internal_call_patches: std.ArrayList(InternalCallPatch),

        /// Tracks positions of ADR/LEA instructions computing proc addresses.
        /// Same shifting problem as internal_call_patches: when deferred-prologue compilation
        /// shifts its body, ADR/LEA instructions targeting code outside the body
        /// get incorrect PC-relative offsets.
        internal_addr_patches: std.ArrayList(InternalAddrPatch),

        /// Stack of early return jump patches.
        /// When generateEarlyReturn is called inside deferred-prologue proc compilation,
        /// it emits a jump to the epilogue and records the patch location here.
        /// After generating the proc body, the deferred-prologue path patches all
        /// early return jumps to point to the epilogue.
        early_return_patches: std.ArrayList(usize),

        /// Stack of forward-jump patches for break expressions inside loops.
        /// Each generateForLoop/generateWhileLoop saves the length, and after
        /// body generation, patches all new entries to the loop exit offset.
        loop_break_patches: std.ArrayList(usize),

        /// Stack slot where early return value is stored during deferred-prologue proc compilation.
        early_return_result_slot: ?i32 = null,

        /// Layout for early return result (to know how to move to return register)
        early_return_ret_layout: ?layout.Idx = null,

        /// Register where RocOps pointer is saved (for calling builtins that need it)
        roc_ops_reg: ?GeneralReg = null,

        /// Proc currently being compiled, for debug-time invariant reporting.
        current_proc_name: ?Symbol = null,

        /// Stack slot where the hidden return pointer is saved (for return-by-pointer
        /// convention used when the return type exceeds the register limit).
        /// Set during deferred-prologue proc compilation, used by moveToReturnRegisterWithLayout
        /// and generateEarlyReturn.
        ret_ptr_slot: ?i32 = null,

        /// Counter for unique temporary local IDs.
        /// Starts at 0x8000_0000 to avoid collision with real local variables.
        /// Used by allocTempGeneral() for temporaries that don't correspond to real locals.
        next_temp_local: u32 = 0x8000_0000,

        /// Generation mode determines whether to use direct function pointers or symbol references.
        /// - native_execution: Code runs in-process (dev evaluator), direct function pointers work
        /// - object_file: Generating relocatable object files, use symbol references for builtins
        generation_mode: GenerationMode = .native_execution,

        /// Scratch buffer for argument locations during lambda body inlining
        scratch_arg_locs: base.Scratch(ValueLocation),

        /// Scratch buffer for argument info during call generation
        scratch_arg_infos: base.Scratch(ArgInfo),

        /// Scratch buffer for pass-by-pointer flags during call generation
        scratch_pass_by_ptr: base.Scratch(bool),

        /// Scratch buffer for parameter register counts during lambda param binding
        scratch_param_num_regs: base.Scratch(u8),

        /// Pre-computed argument info to avoid generating expressions twice
        const ArgInfo = struct {
            loc: ValueLocation,
            layout_idx: ?layout.Idx,
            num_regs: u8,
        };

        /// Info about a mutable variable's fixed stack slot
        pub const MutableVarInfo = struct {
            /// The fixed stack slot offset (from frame pointer)
            slot: i32,
            /// The size of the variable in bytes
            size: u32,
        };

        /// Compiled procedure information for two-pass compilation.
        /// After a procedure is fully compiled (including RET), it's registered here.
        pub const CompiledProc = struct {
            /// The compiled LIR proc-spec id
            id: lir.LIR.LirProcSpecId,
            /// Offset into the code buffer where this procedure starts
            code_start: usize,
            /// Offset where this procedure ends
            code_end: usize,
            /// The symbol this procedure is bound to
            name: Symbol,
            /// Declared argument layouts for ABI-correct call lowering.
            arg_layouts: LayoutIdxSpan,
        };

        const unresolved_proc_code_start = std.math.maxInt(usize);

        /// A pending call that needs to be patched after all procedures are compiled.
        pub const PendingCall = struct {
            /// Offset where the call instruction is (needs patching)
            call_site: usize,
            /// The proc being called
            target_proc: lir.LIR.LirProcSpecId,
        };

        /// Tracks position of a BL/CALL to a compiled lambda proc.
        /// Used to re-patch relative offsets after deferred-prologue body shifts.
        pub const InternalCallPatch = struct {
            /// Buffer offset where the BL/CALL instruction starts
            call_offset: usize,
            /// Absolute buffer offset of the target (prologue start of the called lambda)
            target_offset: usize,
        };

        /// Tracks position of an ADR/LEA instruction computing a proc address.
        /// Used to re-patch PC-relative offsets after body shifts.
        pub const InternalAddrPatch = struct {
            /// Buffer offset where the ADR/LEA instruction starts
            instr_offset: usize,
            /// Absolute buffer offset of the target lambda
            target_offset: usize,
        };

        /// Record of a jump instruction that needs patching to a join point.
        pub const JumpRecord = struct {
            /// Offset of the jump instruction
            location: usize,
        };

        /// Byte width of a scalar value on the stack.
        /// Used to emit correctly-sized loads that zero-extend to 64 bits.
        pub const ValueSize = enum(u3) {
            byte = 0, // 1 byte: Bool, U8, I8
            word = 1, // 2 bytes: U16, I16
            dword = 2, // 4 bytes: U32, I32, F32
            qword = 3, // 8 bytes: U64, I64, F64, pointers (default)

            pub fn fromByteCount(n: u32) ValueSize {
                return switch (n) {
                    1 => .byte,
                    2 => .word,
                    3, 4 => .dword,
                    else => .qword,
                };
            }

            pub fn byteCount(self: ValueSize) u8 {
                return switch (self) {
                    .byte => 1,
                    .word => 2,
                    .dword => 4,
                    .qword => 8,
                };
            }
        };

        /// Where a value is stored
        pub const ValueLocation = union(enum) {
            /// Value is in a general-purpose register
            general_reg: GeneralReg,
            /// Value is in a float register
            float_reg: FloatReg,
            /// Value is on the stack at given offset from frame pointer
            stack: struct { offset: i32, size: ValueSize = .qword },
            /// 128-bit value on the stack (16 bytes: low at offset, high at offset+8)
            stack_i128: i32,
            /// 24-byte string value on the stack (for RocStr: ptr/data, len, capacity)
            stack_str: i32,
            /// List value on the stack - tracks both struct and element locations
            /// for proper copying when returning lists
            list_stack: struct {
                /// Offset of the list struct (ptr, len) from frame pointer
                struct_offset: i32,
                /// Offset of the element data from frame pointer
                data_offset: i32,
                /// Number of elements in the list
                num_elements: u32,
            },
            /// Immediate value known at compile time
            immediate_i64: i64,
            /// Immediate float value
            immediate_f64: f64,
            /// Immediate 128-bit value
            immediate_i128: i128,
            /// Code path that never produces a value (crash/runtime_error).
            /// A trap instruction has been emitted; execution will never reach here.
            noreturn: void,
        };

        /// Result of code generation
        pub const CodeResult = struct {
            /// Generated machine code
            code: []const u8,
            /// Relocations for external references
            relocations: []const Relocation,
            /// Layout of the result
            result_layout: layout.Idx,
            /// Offset from start of code where execution should begin
            /// (procedures may be compiled before the main expression)
            entry_offset: usize = 0,
        };

        /// Information about an exported symbol for object file generation.
        /// Used when compiling to object files for linking with platform hosts.
        pub const ExportedSymbol = struct {
            /// Symbol name (e.g., "roc__main")
            name: []const u8,
            /// Byte offset of the symbol in the code buffer
            offset: usize,
            /// Size of the function in bytes
            size: usize,
            /// Size of the function prologue in bytes (for unwind info)
            prologue_size: u8 = 0,
            /// Stack allocation size (for unwind info)
            stack_alloc: u32 = 0,
            /// Whether function uses frame pointer
            uses_frame_pointer: bool = true,
        };

        /// Result of entrypoint compilation for native code generation.
        pub const EntrypointResult = struct {
            /// Generated machine code containing all entrypoints
            code: []const u8,
            /// Exported symbols for object file generation
            symbols: []const ExportedSymbol,
            /// Relocations for external references
            relocations: []const Relocation,
        };

        /// Errors that can occur during code generation
        /// Initialize the code generator
        /// Target is determined at compile time via the LirCodeGen(target) parameter
        pub fn init(
            allocator: Allocator,
            store: *const LirExprStore,
            layout_store_opt: *const LayoutStore,
            static_interner: ?*StaticDataInterner,
        ) Allocator.Error!Self {
            return .{
                .allocator = allocator,
                .cc = CallingConvention.forTarget(target),
                .codegen = CodeGen.init(allocator),
                .store = store,
                .layout_store = layout_store_opt,
                .static_interner = static_interner,
                .symbol_locations = std.AutoHashMap(u64, ValueLocation).init(allocator),
                .mutable_var_slots = std.AutoHashMap(u64, MutableVarInfo).init(allocator),
                .join_points = std.AutoHashMap(u32, usize).init(allocator),
                .current_recursive_symbol = null,
                .current_recursive_join_point = null,
                .current_binding_symbol = null,
                .proc_registry = std.AutoHashMap(u32, CompiledProc).init(allocator),
                .compiled_rc_helpers = std.AutoHashMap(u64, usize).init(allocator),
                .pending_calls = std.ArrayList(PendingCall).empty,
                .join_point_jumps = std.AutoHashMap(u32, std.ArrayList(JumpRecord)).init(allocator),
                .join_point_param_layouts = std.AutoHashMap(u32, LayoutIdxSpan).init(allocator),
                .join_point_param_patterns = std.AutoHashMap(u32, lir.LirPatternSpan).init(allocator),
                .internal_call_patches = std.ArrayList(InternalCallPatch).empty,
                .internal_addr_patches = std.ArrayList(InternalAddrPatch).empty,
                .early_return_patches = std.ArrayList(usize).empty,
                .loop_break_patches = std.ArrayList(usize).empty,
                .scratch_arg_locs = try base.Scratch(ValueLocation).init(allocator),
                .scratch_arg_infos = try base.Scratch(ArgInfo).init(allocator),
                .scratch_pass_by_ptr = try base.Scratch(bool).init(allocator),
                .scratch_param_num_regs = try base.Scratch(u8).init(allocator),
            };
        }

        /// Clean up resources
        pub fn deinit(self: *Self) void {
            self.codegen.deinit();
            self.symbol_locations.deinit();
            self.mutable_var_slots.deinit();
            self.join_points.deinit();
            self.proc_registry.deinit();
            self.compiled_rc_helpers.deinit();
            self.pending_calls.deinit(self.allocator);
            // Clean up the nested ArrayLists in join_point_jumps
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.deinit(self.allocator);
            }
            self.join_point_jumps.deinit();
            self.join_point_param_layouts.deinit();
            self.join_point_param_patterns.deinit();
            self.internal_call_patches.deinit(self.allocator);
            self.internal_addr_patches.deinit(self.allocator);
            self.early_return_patches.deinit(self.allocator);
            self.loop_break_patches.deinit(self.allocator);
            self.scratch_arg_locs.deinit();
            self.scratch_arg_infos.deinit();
            self.scratch_pass_by_ptr.deinit();
            self.scratch_param_num_regs.deinit();
        }

        /// Reset the code generator for generating a new expression
        pub fn reset(self: *Self) void {
            self.codegen.reset();
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();
            self.join_points.clearRetainingCapacity();
            self.current_recursive_symbol = null;
            self.current_recursive_join_point = null;
            self.current_binding_symbol = null;
            self.proc_registry.clearRetainingCapacity();
            self.compiled_rc_helpers.clearRetainingCapacity();
            self.pending_calls.clearRetainingCapacity();
            // Clear nested ArrayLists
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.clearRetainingCapacity();
            }
            self.join_point_jumps.clearRetainingCapacity();
            self.join_point_param_layouts.clearRetainingCapacity();
            self.join_point_param_patterns.clearRetainingCapacity();
            self.internal_call_patches.clearRetainingCapacity();
            self.internal_addr_patches.clearRetainingCapacity();
            self.early_return_patches.clearRetainingCapacity();
            self.loop_break_patches.clearRetainingCapacity();
        }

        fn cloneJoinPointJumpsMap(
            self: *Self,
            source: *const std.AutoHashMap(u32, std.ArrayList(JumpRecord)),
        ) Allocator.Error!std.AutoHashMap(u32, std.ArrayList(JumpRecord)) {
            var cloned = std.AutoHashMap(u32, std.ArrayList(JumpRecord)).init(self.allocator);
            errdefer self.deinitJoinPointJumpsMap(&cloned);

            var it = source.iterator();
            while (it.next()) |entry| {
                try cloned.put(entry.key_ptr.*, try entry.value_ptr.clone(self.allocator));
            }

            return cloned;
        }

        fn deinitJoinPointJumpsMap(
            self: *Self,
            map: *std.AutoHashMap(u32, std.ArrayList(JumpRecord)),
        ) void {
            var it = map.valueIterator();
            while (it.next()) |list| {
                list.deinit(self.allocator);
            }
            map.deinit();
        }

        fn clearFunctionControlFlowState(self: *Self) void {
            self.join_points.clearRetainingCapacity();
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.clearRetainingCapacity();
            }
            self.join_point_jumps.clearRetainingCapacity();
            self.join_point_param_layouts.clearRetainingCapacity();
            self.join_point_param_patterns.clearRetainingCapacity();
            self.loop_break_patches.clearRetainingCapacity();
        }

        /// Generate code for a LIR expression
        ///
        /// The generated code follows the calling convention:
        /// - First arg (RDI/X0) contains the pointer to the result buffer
        /// - Second arg (RSI/X1) contains the pointer to RocOps
        /// - The function writes the result to the result buffer and returns
        ///
        /// For tuples, pass tuple_len > 1 to copy all elements to the result buffer.
        pub fn generateCode(
            self: *Self,
            expr_id: LirExprId,
            result_layout: layout.Idx,
            tuple_len: usize,
        ) Allocator.Error!CodeResult {
            // Clear any leftover state from compileAllProcSpecs
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();
            self.codegen.callee_saved_used = 0;

            // Initialize stack_offset to reserve space for callee-saved area
            // (same convention as compileProcSpec — positive offsets, deferred prologue)
            if (comptime target.toCpuArch() == .x86_64) {
                self.codegen.stack_offset = -CodeGen.CALLEE_SAVED_AREA_SIZE;
            } else {
                // aarch64: FP-relative addressing
                // Reserve space for: FP/LR (16 bytes) + callee-saved area
                self.codegen.stack_offset = 16 + CodeGen.CALLEE_SAVED_AREA_SIZE;
            }

            // Track where the body starts (prologue will be prepended before this)
            const body_start = self.codegen.currentOffset();
            const relocs_before = self.codegen.relocations.items.len;

            // Reserve argument registers so they don't get allocated for temporaries
            // X0/RDI = result pointer, X1/RSI = RocOps pointer
            self.reserveArgumentRegisters();

            // Save the result pointer and RocOps pointer to callee-saved registers
            // before generating code that might call procedures (which would clobber them).
            const result_ptr_save_reg = if (comptime target.toCpuArch() == .aarch64)
                aarch64.GeneralReg.X19
            else
                x86_64.GeneralReg.RBX;

            const roc_ops_save_reg = if (comptime target.toCpuArch() == .aarch64)
                aarch64.GeneralReg.X20
            else
                x86_64.GeneralReg.R12;

            const arg0_reg = if (comptime target.toCpuArch() == .aarch64)
                aarch64.GeneralReg.X0
            else if (comptime target.isWindows())
                x86_64.GeneralReg.RCX
            else
                x86_64.GeneralReg.RDI;

            const arg1_reg = if (comptime target.toCpuArch() == .aarch64)
                aarch64.GeneralReg.X1
            else if (comptime target.isWindows())
                x86_64.GeneralReg.RDX
            else
                x86_64.GeneralReg.RSI;

            try self.emitMovRegReg(result_ptr_save_reg, arg0_reg);
            try self.emitMovRegReg(roc_ops_save_reg, arg1_reg);

            self.roc_ops_reg = roc_ops_save_reg;

            // NOTE: Do NOT free arg0_reg/arg1_reg here. Although their values have
            // been saved to callee-saved registers, freeing them makes RDI/RSI (or X0/X1)
            // available to the register allocator as scratch registers. When generating
            // a lambda call with many args (e.g. 4 Dec params = 8 registers), the call
            // setup needs these registers for pass-by-ptr arguments. If they've been
            // reused as temporaries, the generated call code puts wrong values in them,
            // causing segfaults.

            // Remove roc_ops and result_ptr registers from callee_saved_available
            // so the register allocator never uses them as temporaries.
            if (comptime target.toCpuArch() == .aarch64) {
                self.codegen.callee_saved_available &= ~(@as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20));
                self.codegen.callee_saved_available &= ~(@as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19));
            } else {
                self.codegen.callee_saved_available &= ~(@as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12));
                self.codegen.callee_saved_available &= ~(@as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RBX));
            }

            // Generate code for the expression.
            // In the new pipeline, MIR→LIR generates all closure dispatch as generic LIR
            // constructs, so the result is always a plain value — never a lambda/closure.
            const final_result = try self.generateExpr(expr_id);
            const actual_ret_layout = result_layout;

            // Store result to the saved result pointer
            const ret_size = self.getLayoutSize(actual_ret_layout);
            if (ret_size > 0) {
                try self.storeResultToSavedPtr(final_result, actual_ret_layout, result_ptr_save_reg, tuple_len);
            }

            // Emit epilogue using DeferredFrameBuilder with actual stack usage
            const body_epilogue_offset = self.codegen.currentOffset();
            {
                const actual_locals: u32 = if (comptime target.toCpuArch() == .aarch64)
                    @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE)
                else
                    @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                var builder = CodeGen.DeferredFrameBuilder.init();
                builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                builder.setStackSize(actual_locals);
                try builder.emitEpilogue(&self.codegen.emit);
            }

            const body_end = self.codegen.currentOffset();

            // Prepend prologue: copy body out, emit prologue with exact size, re-append body
            const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
            defer self.allocator.free(body_bytes);

            self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

            const prologue_start = self.codegen.currentOffset();
            if (comptime target.toCpuArch() == .x86_64) {
                const actual_locals_x86: u32 = @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                try self.codegen.emitPrologueWithAlloc(actual_locals_x86);
            } else {
                const actual_locals: u32 = @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE);
                var frame_builder = CodeGen.DeferredFrameBuilder.init();
                frame_builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                frame_builder.setStackSize(actual_locals);
                _ = try frame_builder.emitPrologue(&self.codegen.emit);
            }
            const prologue_size = self.codegen.currentOffset() - prologue_start;

            // Re-append body + epilogue
            self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

            // Adjust relocation offsets for the prepended prologue
            for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                reloc.adjustOffset(prologue_size);
            }

            // Root-body direct calls and nested helper offsets shift with the prepended prologue.
            self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size, std.math.maxInt(u64));
            self.shiftPendingCalls(body_start, body_end, prologue_size);
            self.repatchInternalCalls(body_start, body_end, prologue_size, body_start);
            self.repatchInternalAddrPatches(body_start, body_end, prologue_size, body_start);

            // Patch early return jumps (if any) to the epilogue
            const final_epilogue = body_epilogue_offset - body_start + prologue_size + prologue_start;
            for (self.early_return_patches.items) |patch| {
                self.codegen.patchJump(patch + prologue_size, final_epilogue);
            }

            const all_code = self.codegen.getCode();
            const code_copy = self.allocator.dupe(u8, all_code) catch return error.OutOfMemory;

            return CodeResult{
                .code = code_copy,
                .relocations = self.codegen.relocations.items,
                .result_layout = result_layout,
                .entry_offset = prologue_start,
            };
        }

        /// Reserve argument registers so they don't get allocated for temporaries
        fn reserveArgumentRegisters(self: *Self) void {
            if (comptime target.toCpuArch() == .aarch64) {
                // Clear X0 and X1 from the free register mask
                // X0 = bit 0, X1 = bit 1
                self.codegen.free_general &= ~@as(u32, 0b11);
                // Reserve X19 (result pointer) and X20 (RocOps pointer) - both callee-saved
                // Must mark as used so they get saved/restored in prologue/epilogue
                const x19_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19);
                const x20_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20);
                self.codegen.callee_saved_available &= ~(x19_bit | x20_bit);
                self.codegen.callee_saved_used |= @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19);
                self.codegen.callee_saved_used |= @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20);
            } else if (comptime target.isWindows()) {
                // Windows x64: Clear RCX and RDX from the free register mask
                const rcx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RCX);
                const rdx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RDX);
                self.codegen.free_general &= ~(rcx_bit | rdx_bit);
                // Reserve RBX (result pointer) and R12 (RocOps pointer) - both callee-saved
                // Must mark as used so they get saved/restored in prologue/epilogue
                const rbx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
                const r12_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12);
                self.codegen.callee_saved_available &= ~(rbx_bit | r12_bit);
                self.codegen.callee_saved_used |= @as(u16, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
                self.codegen.callee_saved_used |= @as(u16, 1) << @intFromEnum(x86_64.GeneralReg.R12);
            } else {
                // System V (Linux, macOS): Clear RDI and RSI from the free register mask
                const rdi_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RDI);
                const rsi_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RSI);
                self.codegen.free_general &= ~(rdi_bit | rsi_bit);
                // Reserve RBX (result pointer) and R12 (RocOps pointer) - both callee-saved
                // Must mark as used so they get saved/restored in prologue/epilogue
                const rbx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
                const r12_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12);
                self.codegen.callee_saved_available &= ~(rbx_bit | r12_bit);
                self.codegen.callee_saved_used |= @as(u16, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
                self.codegen.callee_saved_used |= @as(u16, 1) << @intFromEnum(x86_64.GeneralReg.R12);
            }
        }

        /// Get the concrete layout of a value-producing expression.
        /// This is total for all value expressions; non-value expressions panic in debug.
        fn exprLayout(self: *Self, expr_id: LirExprId) layout.Idx {
            const expr = self.store.getExpr(expr_id);
            return switch (expr) {
                // Expressions that store their layout
                .struct_ => |s| s.struct_layout,
                .tag => |tag| tag.union_layout,
                .lookup => |lookup| lookup.layout_idx,
                .cell_load => |load| load.layout_idx,
                .struct_access => |sa| sa.field_layout,
                .proc_call => |call| call.ret_layout,
                .low_level => |ll| ll.ret_layout,
                .hosted_call => |hc| hc.ret_layout,
                // Compound expressions with result layouts
                .if_then_else => |ite| ite.result_layout,
                .match_expr => |w| w.result_layout,
                .block => |b| b.result_layout,
                .dbg => |d| d.result_layout,
                .expect => |e| e.result_layout,
                .early_return => |er| er.ret_layout,
                .discriminant_switch => |ds| ds.result_layout,
                .tag_payload_access => |tpa| tpa.payload_layout,
                .zero_arg_tag => |zat| zat.union_layout,
                .nominal => |nom| nom.nominal_layout,
                // Literals with known layouts
                .f64_literal => .f64,
                .f32_literal => .f32,
                .bool_literal => .bool,
                .dec_literal => .dec,
                .i64_literal => |i| i.layout_idx,
                .i128_literal => |i| i.layout_idx,
                .str_literal, .str_concat, .int_to_str, .float_to_str, .dec_to_str, .str_escape_and_quote => .str,
                .empty_list => |l| l.list_layout,
                .list => |l| l.list_layout,
                // Loops return unit (ZST)
                .for_loop, .while_loop => .zst,
                // Statements, not value-producing expressions
                .incref, .decref, .free => .zst,
                // Noreturn
                .crash, .runtime_error, .break_expr => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: exprLayout called on non-value expression {s}",
                            .{@tagName(expr)},
                        );
                    }
                    unreachable;
                },
            };
        }

        /// Generate code for an expression. The result is ALWAYS in a stable location
        /// (stack, immediate, lambda_code, closure_value) — never a bare register.
        fn generateExpr(self: *Self, expr_id: LirExprId) Allocator.Error!ValueLocation {
            const loc = try self.generateExprRaw(expr_id);
            return self.stabilize(loc);
        }

        /// Spill bare register values to the stack. All other locations pass through.
        fn stabilize(self: *Self, loc: ValueLocation) Allocator.Error!ValueLocation {
            return switch (loc) {
                .general_reg => |reg| {
                    const slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStack(.w64, slot, reg);
                    self.codegen.freeGeneral(reg);
                    return .{ .stack = .{ .offset = slot } };
                },
                .float_reg => |reg| {
                    const slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStackF64(slot, reg);
                    self.codegen.freeFloat(reg);
                    return .{ .stack = .{ .offset = slot } };
                },
                else => loc,
            };
        }

        /// Generate code for an expression (raw — may return bare register locations).
        fn generateExprRaw(self: *Self, expr_id: LirExprId) Allocator.Error!ValueLocation {
            if (builtin.mode == .Debug and @intFromEnum(expr_id) >= self.store.exprs.items.len) {
                std.debug.panic(
                    "LirCodeGen: invalid expr_id {d} while compiling proc {d} (store len {d})",
                    .{
                        @intFromEnum(expr_id),
                        if (self.current_proc_name) |sym| @as(u64, @bitCast(sym)) else 0,
                        self.store.exprs.items.len,
                    },
                );
            }
            const expr = self.store.getExpr(expr_id);

            return switch (expr) {
                // Literals
                .i64_literal => |val| .{ .immediate_i64 = val.value },
                .i128_literal => |val| try self.generateI128Literal(val.value),
                .f64_literal => |val| .{ .immediate_f64 = val },
                .f32_literal => |val| .{ .immediate_f64 = @floatCast(val) },
                .bool_literal => |val| .{ .immediate_i64 = if (val) 1 else 0 },
                .dec_literal => |val| try self.generateI128Literal(val),

                // Lookups
                .lookup => |lookup| try self.generateLookup(lookup.symbol, lookup.layout_idx),
                .cell_load => |load| try self.generateCellLoad(load.cell, load.layout_idx),

                // Control flow
                .if_then_else => |ite| try self.generateIfThenElse(ite),
                .match_expr => |m| try self.generateMatch(m),

                // Blocks
                .block => |block| try self.generateBlock(block),
                // Function calls and lambdas
                .proc_call => |call| try self.generateCall(call),

                // Structs (records, tuples, empty records)
                .struct_ => |s| try self.generateStruct(s),
                .struct_access => |sa| try self.generateStructAccess(sa),

                // Tags (tagged unions)
                .zero_arg_tag => |tag| try self.generateZeroArgTag(tag),
                .tag => |tag| try self.generateTag(tag),

                // Lists (not fully implemented - returns placeholder for now)
                .list => |list| try self.generateList(list),
                .empty_list => try self.generateEmptyList(),

                // Low-level operations
                .low_level => |ll| try self.generateLowLevel(ll),

                // Hosted function calls (platform-provided effects)
                .hosted_call => |hc| try self.generateHostedCall(hc),

                // Nominal types (transparent wrappers)
                .nominal => |nom| try self.generateExpr(nom.backing_expr),

                // String literals
                .str_literal => |str_idx| try self.generateStrLiteral(str_idx),

                // Reference counting operations
                .incref => |rc_op| try self.generateIncref(rc_op),
                .decref => |rc_op| try self.generateDecref(rc_op),
                .free => |rc_op| try self.generateFree(rc_op),

                // For loop over a list
                .for_loop => |for_loop| try self.generateForLoop(for_loop),

                // While loop
                .while_loop => |while_loop| try self.generateWhileLoop(while_loop),

                // Early return from a block
                .early_return => |er| try self.generateEarlyReturn(er),

                // Break out of a loop
                .break_expr => try self.generateBreak(),

                // Debug and assertions
                .dbg => |dbg_expr| try self.generateDbg(dbg_expr),
                .expect => |expect_expr| try self.generateExpect(expect_expr),

                // Crash and runtime errors
                .crash => |crash| {
                    const msg = self.store.getString(crash.msg);
                    try self.emitRocCrash(msg);
                    try self.emitTrap();
                    return .noreturn;
                },
                .runtime_error => {
                    try self.emitRocCrash("hit a runtime error");
                    try self.emitTrap();
                    return .noreturn;
                },

                // String formatting for inspect
                .str_concat => |exprs| try self.generateStrConcat(exprs),
                .int_to_str => |its| try self.generateIntToStr(its),
                .float_to_str => |fts| try self.generateFloatToStr(fts),
                .dec_to_str => |dec_expr| try self.generateDecToStr(dec_expr),
                .str_escape_and_quote => |quote_expr| try self.generateStrEscapeAndQuote(quote_expr),

                // Discriminant switch for tag unions
                .discriminant_switch => |ds| try self.generateDiscriminantSwitch(ds),

                // Extract payload from a tag union value (used inside discriminant_switch branches)
                .tag_payload_access => |tpa| try self.generateTagPayloadAccess(tpa),
            };
        }

        /// Generate code for low-level operations
        fn generateLowLevel(self: *Self, ll: anytype) Allocator.Error!ValueLocation {
            const args = self.store.getExprSpan(ll.args);

            switch (ll.op) {
                .list_len => {
                    // List is a (ptr, len, capacity) triple - length is at offset 8
                    std.debug.assert(args.len >= 1);
                    const list_loc = try self.generateExpr(args[0]);

                    // Get base offset from either stack or list_stack location
                    const base_offset: i32 = switch (list_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        .immediate_i64 => |val| {
                            // Empty list - length is 0
                            if (val == 0) {
                                return .{ .immediate_i64 = 0 };
                            }
                            unreachable;
                        },
                        else => unreachable,
                    };

                    // Length is at offset 8 in the list struct
                    const result_reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, result_reg, frame_ptr, base_offset + 8);
                    return .{ .general_reg = result_reg };
                },
                .list_with_capacity => {
                    // listWithCapacity(capacity, alignment, elem_width, elements_refcounted,
                    //                  inc_context, inc, roc_ops) -> RocList
                    if (args.len < 1) {
                        unreachable;
                    }

                    const roc_ops_reg = self.roc_ops_reg orelse {
                        unreachable;
                    };
                    const capacity_loc = try self.generateExpr(args[0]);

                    // Get element layout from return type (which is List(elem))
                    const ls = self.layout_store;
                    const ret_layout = ls.getLayout(ll.ret_layout);

                    const elem_size_align: layout.SizeAlign = switch (ret_layout.tag) {
                        .list => blk: {
                            const elem_layout = ls.getLayout(ret_layout.data.list);
                            break :blk ls.layoutSizeAlign(elem_layout);
                        },
                        .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                        else => unreachable, // list_with_capacity must return a list
                    };

                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_with_capacity);

                    // Convert RocAlignment enum to actual byte alignment
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();

                    // Allocate stack space for result (RocList = 24 bytes)
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);

                    const cap_reg = try self.ensureInGeneralReg(capacity_loc);
                    const base_reg = frame_ptr;

                    // Determine if elements contain refcounted data
                    const elements_refcounted: bool = blk: {
                        if (ret_layout.tag == .list) {
                            break :blk ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list));
                        }
                        break :blk false;
                    };

                    // roc_builtins_list_with_capacity(out, capacity, alignment, element_width, elements_refcounted, roc_ops)
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addRegArg(cap_reg);
                    self.codegen.freeGeneral(cap_reg);
                    try builder.addImmArg(@intCast(alignment_bytes));
                    try builder.addImmArg(@intCast(elem_size_align.size));
                    try builder.addImmArg(if (elements_refcounted) 1 else 0);
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, fn_addr, .list_with_capacity);

                    // Return as .list_stack so recursive calls properly detect this as a list argument
                    return .{
                        .list_stack = .{
                            .struct_offset = result_offset,
                            .data_offset = 0, // Data location is stored in the list struct itself
                            .num_elements = 0, // Unknown at compile time
                        },
                    };
                },
                .list_append_unsafe => {
                    // list_append(list, element) -> List
                    // Uses SAFE listAppendSafeC that reserves capacity if needed
                    if (args.len != 2) {
                        unreachable;
                    }
                    const ls = self.layout_store;

                    const roc_ops_reg = self.roc_ops_reg orelse {
                        unreachable;
                    };

                    // Generate list argument (must be on stack - 24 bytes)
                    const list_loc = try self.generateExpr(args[0]);

                    // Generate element value
                    const elem_loc = try self.generateExpr(args[1]);
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    if (builtin.mode == .Debug and ret_layout_val.tag != .list and ret_layout_val.tag != .list_of_zst) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: list_append ret_layout must be list/list_of_zst, got {s}",
                            .{@tagName(ret_layout_val.tag)},
                        );
                    }
                    if (builtin.mode == .Debug) {
                        const list_layout_idx = self.exprLayout(args[0]);
                        const list_layout_val = ls.getLayout(list_layout_idx);
                        switch (list_layout_val.tag) {
                            .list => {},
                            .list_of_zst => {
                                if (ret_layout_val.tag != .list_of_zst) {
                                    std.debug.panic(
                                        "LIR/codegen invariant violated: list_append expected list_of_zst return layout",
                                        .{},
                                    );
                                }
                            },
                            else => {
                                std.debug.panic(
                                    "LIR/codegen invariant violated: list_append first argument must have list/list_of_zst layout, got {s}",
                                    .{@tagName(list_layout_val.tag)},
                                );
                            },
                        }
                    }
                    const elem_size_align: layout.SizeAlign = switch (ret_layout_val.tag) {
                        .list => ls.layoutSizeAlign(ls.getLayout(ret_layout_val.data.list)),
                        .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                        else => unreachable,
                    };

                    const is_zst = (elem_size_align.size == 0);

                    const list_offset: i32 = switch (list_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        .immediate_i64 => |val| blk: {
                            // Empty list case: materialize on stack (ptr=0, len=0, capacity=0)
                            if (val != 0) {
                                unreachable;
                            }
                            const slot = self.codegen.allocStackSlot(roc_str_size);
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(temp, 0);
                            try self.emitStore(.w64, frame_ptr, slot, temp);
                            try self.emitStore(.w64, frame_ptr, slot + 8, temp);
                            try self.emitStore(.w64, frame_ptr, slot + 16, temp);
                            self.codegen.freeGeneral(temp);
                            break :blk slot;
                        },
                        else => {
                            unreachable;
                        },
                    };

                    // Ensure element is on stack
                    const elem_offset: i32 = try self.ensureOnStack(elem_loc, elem_size_align.size);

                    // Allocate result slot (24 bytes for RocList)
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);

                    // For ZST (zero-sized types), use the unsafe version since no capacity is needed.
                    // For regular elements, use the safe version that reserves capacity.
                    const base_reg = frame_ptr;

                    if (is_zst) {
                        // ZST: use listAppendUnsafeC (fewer args, doesn't need capacity reservation)
                        const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_append_unsafe);

                        // roc_builtins_list_append_unsafe(out, list_bytes, list_len, list_cap, element, element_width, roc_ops)
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_offset);
                        try builder.addMemArg(base_reg, list_offset + 8);
                        try builder.addMemArg(base_reg, list_offset + 16);
                        try builder.addLeaArg(base_reg, elem_offset);
                        try builder.addImmArg(0); // elem_width = 0 for ZST
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, fn_addr, .list_append_unsafe);
                    } else {
                        // Non-ZST: use listAppendSafeC which reserves capacity
                        const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_append_safe);
                        const alignment_bytes = elem_size_align.alignment.toByteUnits();

                        // Determine if elements contain refcounted data
                        const elements_refcounted: bool = blk: {
                            if (ret_layout_val.tag == .list) {
                                break :blk ls.layoutContainsRefcounted(ls.getLayout(ret_layout_val.data.list));
                            }
                            break :blk false;
                        };

                        // roc_builtins_list_append_safe(out, list_bytes, list_len, list_cap, element, alignment, element_width, elements_refcounted, roc_ops)
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_offset);
                        try builder.addMemArg(base_reg, list_offset + 8);
                        try builder.addMemArg(base_reg, list_offset + 16);
                        try builder.addLeaArg(base_reg, elem_offset);
                        try builder.addImmArg(@intCast(alignment_bytes));
                        try builder.addImmArg(@intCast(elem_size_align.size));
                        try builder.addImmArg(if (elements_refcounted) 1 else 0);
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, fn_addr, .list_append_safe);
                    }

                    // Return as .list_stack so recursive calls properly detect this as a list argument
                    return .{
                        .list_stack = .{
                            .struct_offset = result_offset,
                            .data_offset = 0, // Data location is stored in the list struct itself
                            .num_elements = 0, // Unknown at compile time
                        },
                    };
                },
                .list_get_unsafe => {
                    // list_get_unsafe(list, index) -> element
                    std.debug.assert(args.len >= 2);
                    const list_loc = try self.generateExpr(args[0]);
                    const index_loc = try self.generateExpr(args[1]);

                    // Get base offset of list struct
                    const list_base: i32 = switch (list_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        else => unreachable,
                    };

                    const ls = self.layout_store;
                    const list_layout_idx = self.exprLayout(args[0]);
                    const list_layout_val = ls.getLayout(list_layout_idx);
                    const list_elem_layout: layout.Idx = switch (list_layout_val.tag) {
                        .list => list_layout_val.data.list,
                        .list_of_zst => ll.ret_layout,
                        else => {
                            if (builtin.mode == .Debug) {
                                std.debug.panic(
                                    "LIR/codegen invariant violated: list_get argument must have list/list_of_zst layout, got {s}",
                                    .{@tagName(list_layout_val.tag)},
                                );
                            }
                            unreachable;
                        },
                    };

                    if (builtin.mode == .Debug) {
                        if (!try self.layoutsStructurallyCompatible(ll.ret_layout, list_elem_layout)) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: list_get_unsafe ret/elem layout mismatch (ret={d}, elem={d})",
                                .{ @intFromEnum(ll.ret_layout), @intFromEnum(list_elem_layout) },
                            );
                        }
                    }

                    const elem_layout_idx = list_elem_layout;
                    const elem_layout_val = ls.getLayout(elem_layout_idx);
                    const elem_size: u32 = ls.layoutSizeAlign(elem_layout_val).size;

                    if (elem_size == 0) {
                        // ZST element with unsafe get - no actual data to load
                        return .{ .immediate_i64 = 0 };
                    }

                    // Get index into a register
                    const index_reg = try self.allocTempGeneral();
                    switch (index_loc) {
                        .immediate_i64 => |val| {
                            try self.codegen.emitLoadImm(index_reg, val);
                        },
                        .general_reg => |reg| {
                            try self.emitMovRegReg(index_reg, reg);
                            self.codegen.freeGeneral(reg);
                        },
                        .stack => |s| {
                            const off = s.offset;
                            try self.codegen.emitLoadStack(.w64, index_reg, off);
                        },
                        .stack_i128 => |off| {
                            // Dec/i128 index - just load the low 64 bits as U64
                            try self.codegen.emitLoadStack(.w64, index_reg, off);
                        },
                        .immediate_i128 => |val| {
                            // i128 immediate - truncate to 64 bits for index
                            const low: u64 = @truncate(@as(u128, @bitCast(val)));
                            try self.codegen.emitLoadImm(index_reg, @bitCast(low));
                        },
                        else => unreachable,
                    }

                    // Load list pointer (offset 0 in list struct)
                    const ptr_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, ptr_reg, list_base);

                    // Calculate element address: ptr + index * elem_size
                    const addr_reg = try self.allocTempGeneral();
                    try self.codegen.emit.movRegReg(.w64, addr_reg, index_reg);
                    self.codegen.freeGeneral(index_reg);

                    if (elem_size != 1) {
                        const size_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(size_reg, elem_size);
                        try self.emitMulRegs(.w64, addr_reg, addr_reg, size_reg);
                        self.codegen.freeGeneral(size_reg);
                    }

                    // Add base pointer
                    try self.emitAddRegs(.w64, addr_reg, addr_reg, ptr_reg);
                    self.codegen.freeGeneral(ptr_reg);

                    // Load element to stack slot
                    const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));
                    const temp_reg = try self.allocTempGeneral();

                    if (elem_size <= 8) {
                        const vs = ValueSize.fromByteCount(@intCast(elem_size));
                        try self.emitSizedLoadMem(temp_reg, addr_reg, 0, vs);
                        try self.emitSizedStoreMem(frame_ptr, elem_slot, temp_reg, vs);
                    } else {
                        // For larger elements, copy in 8-byte chunks
                        try self.copyChunked(temp_reg, addr_reg, 0, frame_ptr, elem_slot, elem_size);
                    }

                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(addr_reg);

                    var result_loc: ValueLocation = if (elem_layout_idx == .i128 or elem_layout_idx == .u128 or elem_layout_idx == .dec)
                        .{ .stack_i128 = elem_slot }
                    else if (elem_layout_idx == .str)
                        .{ .stack_str = elem_slot }
                    else if (elem_layout_val.tag == .list or elem_layout_val.tag == .list_of_zst)
                        .{ .list_stack = .{
                            .struct_offset = elem_slot,
                            .data_offset = 0,
                            .num_elements = 0,
                        } }
                    else
                        .{ .stack = .{ .offset = elem_slot } };

                    result_loc = try self.stabilize(result_loc);
                    return result_loc;
                },
                .list_concat => {
                    // list_concat(list_a, list_b) -> List
                    if (args.len != 2) unreachable;
                    const list_a_loc = try self.generateExpr(args[0]);
                    const list_b_loc = try self.generateExpr(args[1]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const ret_layout = ls.getLayout(ll.ret_layout);
                    const elem_size_align: layout.SizeAlign = switch (ret_layout.tag) {
                        .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                        .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                        else => unreachable,
                    };
                    const elements_refcounted: bool = switch (ret_layout.tag) {
                        .list => ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list)),
                        else => false,
                    };

                    const list_a_off = try self.ensureOnStack(list_a_loc, roc_list_size);
                    const list_b_off = try self.ensureOnStack(list_b_loc, roc_list_size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();
                    const fn_addr: usize = @intFromPtr(&wrapListConcat);

                    {
                        // wrapListConcat(out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, alignment, element_width, elements_refcounted, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_a_off);
                        try builder.addMemArg(base_reg, list_a_off + 8);
                        try builder.addMemArg(base_reg, list_a_off + 16);
                        try builder.addMemArg(base_reg, list_b_off);
                        try builder.addMemArg(base_reg, list_b_off + 8);
                        try builder.addMemArg(base_reg, list_b_off + 16);
                        try builder.addImmArg(@intCast(alignment_bytes));
                        try builder.addImmArg(@intCast(elem_size_align.size));
                        try builder.addImmArg(if (elements_refcounted) @as(usize, 1) else 0);
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_concat);
                    }

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_prepend => {
                    // list_prepend(list, element) -> List
                    if (args.len != 2) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const elem_loc = try self.generateExpr(args[1]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const ret_layout = ls.getLayout(ll.ret_layout);
                    const elem_size_align: layout.SizeAlign = switch (ret_layout.tag) {
                        .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                        .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                        else => unreachable,
                    };
                    const elements_refcounted: bool = switch (ret_layout.tag) {
                        .list => ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list)),
                        else => false,
                    };

                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const elem_off = try self.ensureOnStack(elem_loc, elem_size_align.size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();
                    const fn_addr: usize = @intFromPtr(&wrapListPrepend);

                    {
                        // wrapListPrepend(out, list_bytes, list_len, list_cap, alignment, element, element_width, elements_refcounted, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_off);
                        try builder.addMemArg(base_reg, list_off + 8);
                        try builder.addMemArg(base_reg, list_off + 16);
                        try builder.addImmArg(@intCast(alignment_bytes));
                        try builder.addLeaArg(base_reg, elem_off);
                        try builder.addImmArg(@intCast(elem_size_align.size));
                        try builder.addImmArg(if (elements_refcounted) @as(usize, 1) else 0);
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_prepend);
                    }

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_drop_first => {
                    // list_drop_first(list, n) -> List  (sublist from index n to end)
                    if (args.len != 2) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const n_loc = try self.generateExpr(args[1]);
                    return try self.callListSublist(ll, list_loc, n_loc, .drop_first);
                },
                .list_drop_last => {
                    // list_drop_last(list, n) -> List  (sublist from 0 with len - n)
                    if (args.len != 2) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const n_loc = try self.generateExpr(args[1]);
                    return try self.callListSublist(ll, list_loc, n_loc, .drop_last);
                },
                .list_take_first => {
                    // list_take_first(list, n) -> List  (sublist from 0 with n elements)
                    if (args.len != 2) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const n_loc = try self.generateExpr(args[1]);
                    return try self.callListSublist(ll, list_loc, n_loc, .take_first);
                },
                .list_take_last => {
                    // list_take_last(list, n) -> List  (sublist from len - n to end)
                    if (args.len != 2) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const n_loc = try self.generateExpr(args[1]);
                    return try self.callListSublist(ll, list_loc, n_loc, .take_last);
                },
                // Safe integer widening (signed source -> larger signed target)
                .i8_to_i16,
                .i8_to_i32,
                .i8_to_i64,
                .i16_to_i32,
                .i16_to_i64,
                .i32_to_i64,
                => {
                    std.debug.assert(args.len >= 1);
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);
                    // Sign-extend: shift left to put sign bit at bit 63, then arithmetic shift right
                    const src_bits: u8 = switch (ll.op) {
                        .i8_to_i16, .i8_to_i32, .i8_to_i64 => 8,
                        .i16_to_i32, .i16_to_i64 => 16,
                        .i32_to_i64 => 32,
                        else => unreachable,
                    };
                    const shift_amount: u8 = 64 - src_bits;
                    try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                    try self.emitAsrImm(.w64, src_reg, src_reg, shift_amount);
                    return .{ .general_reg = src_reg };
                },

                // Safe integer widening (unsigned source -> larger target)
                .u8_to_i16,
                .u8_to_i32,
                .u8_to_i64,
                .u8_to_u16,
                .u8_to_u32,
                .u8_to_u64,
                .u16_to_i32,
                .u16_to_i64,
                .u16_to_u32,
                .u16_to_u64,
                .u32_to_i64,
                .u32_to_u64,
                => {
                    std.debug.assert(args.len >= 1);
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);
                    // Zero-extend: mask off upper bits
                    const src_bits: u8 = switch (ll.op) {
                        .u8_to_i16, .u8_to_i32, .u8_to_i64, .u8_to_u16, .u8_to_u32, .u8_to_u64 => 8,
                        .u16_to_i32, .u16_to_i64, .u16_to_u32, .u16_to_u64 => 16,
                        .u32_to_i64, .u32_to_u64 => 32,
                        else => unreachable,
                    };
                    const shift_amount: u8 = 64 - src_bits;
                    try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                    try self.emitLsrImm(.w64, src_reg, src_reg, shift_amount);
                    return .{ .general_reg = src_reg };
                },

                // Wrapping conversions (truncation to smaller type)
                .u8_to_i8_wrap,
                .i8_to_u8_wrap,
                .i8_to_u16_wrap,
                .i8_to_u32_wrap,
                .i8_to_u64_wrap,
                .u16_to_i8_wrap,
                .u16_to_i16_wrap,
                .u16_to_u8_wrap,
                .i16_to_i8_wrap,
                .i16_to_u8_wrap,
                .i16_to_u16_wrap,
                .i16_to_u32_wrap,
                .i16_to_u64_wrap,
                .u32_to_i8_wrap,
                .u32_to_i16_wrap,
                .u32_to_i32_wrap,
                .u32_to_u8_wrap,
                .u32_to_u16_wrap,
                .i32_to_i8_wrap,
                .i32_to_i16_wrap,
                .i32_to_u8_wrap,
                .i32_to_u16_wrap,
                .i32_to_u32_wrap,
                .i32_to_u64_wrap,
                .u64_to_i8_wrap,
                .u64_to_i16_wrap,
                .u64_to_i32_wrap,
                .u64_to_i64_wrap,
                .u64_to_u8_wrap,
                .u64_to_u16_wrap,
                .u64_to_u32_wrap,
                .i64_to_i8_wrap,
                .i64_to_i16_wrap,
                .i64_to_i32_wrap,
                .i64_to_u8_wrap,
                .i64_to_u16_wrap,
                .i64_to_u32_wrap,
                .i64_to_u64_wrap,
                => {
                    std.debug.assert(args.len >= 1);
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);
                    // Truncation: just mask the relevant bits
                    const dst_bits: u8 = switch (ll.op) {
                        .u8_to_i8_wrap, .i8_to_u8_wrap, .u16_to_i8_wrap, .u16_to_u8_wrap, .i16_to_i8_wrap, .i16_to_u8_wrap, .u32_to_i8_wrap, .u32_to_u8_wrap, .i32_to_i8_wrap, .i32_to_u8_wrap, .u64_to_i8_wrap, .u64_to_u8_wrap, .i64_to_i8_wrap, .i64_to_u8_wrap => 8,
                        .u16_to_i16_wrap, .i8_to_u16_wrap, .i16_to_u16_wrap, .u32_to_i16_wrap, .u32_to_u16_wrap, .i32_to_i16_wrap, .i32_to_u16_wrap, .u64_to_i16_wrap, .u64_to_u16_wrap, .i64_to_i16_wrap, .i64_to_u16_wrap => 16,
                        .u32_to_i32_wrap, .i8_to_u32_wrap, .i16_to_u32_wrap, .i32_to_u32_wrap, .u64_to_i32_wrap, .u64_to_u32_wrap, .i64_to_i32_wrap, .i64_to_u32_wrap => 32,
                        .i8_to_u64_wrap, .i16_to_u64_wrap, .i32_to_u64_wrap, .u64_to_i64_wrap, .i64_to_u64_wrap => 64,
                        else => unreachable,
                    };
                    if (dst_bits < 64) {
                        const shift_amount: u8 = 64 - dst_bits;
                        try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                        try self.emitLsrImm(.w64, src_reg, src_reg, shift_amount);
                    }
                    // 64-bit wrapping is a no-op (reinterpret bits)
                    return .{ .general_reg = src_reg };
                },

                // ── Signed integer to float conversions ──
                // Sign-extend the source value to 64 bits, then convert to f64.
                // (All float values are stored as f64 internally; f32 narrowing
                // happens at the store point via storeResultToSavedPtr.)
                .i8_to_f32,
                .i8_to_f64,
                .i16_to_f32,
                .i16_to_f64,
                .i32_to_f32,
                .i32_to_f64,
                .i64_to_f32,
                .i64_to_f64,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);

                    // Sign-extend source to 64 bits
                    const src_bits: u8 = switch (ll.op) {
                        .i8_to_f32, .i8_to_f64 => 8,
                        .i16_to_f32, .i16_to_f64 => 16,
                        .i32_to_f32, .i32_to_f64 => 32,
                        .i64_to_f32, .i64_to_f64 => 64,
                        else => unreachable,
                    };
                    if (src_bits < 64) {
                        const shift_amount: u8 = 64 - src_bits;
                        try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                        try self.emitAsrImm(.w64, src_reg, src_reg, shift_amount);
                    }

                    // Convert signed i64 to f64
                    const freg = self.codegen.allocFloat() orelse unreachable;
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.scvtfFloatFromGen(.double, freg, src_reg, .w64);
                    } else {
                        try self.codegen.emit.cvtsi2sdRegReg(.w64, freg, src_reg);
                    }
                    self.codegen.freeGeneral(src_reg);
                    return .{ .float_reg = freg };
                },

                // ── Unsigned integer (≤32-bit) to float conversions ──
                // Zero-extend to 64 bits, then convert with signed instruction.
                // All u32 values fit in the positive range of i64 so SCVTF/CVTSI2SD is correct.
                .u8_to_f32,
                .u8_to_f64,
                .u16_to_f32,
                .u16_to_f64,
                .u32_to_f32,
                .u32_to_f64,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);

                    // Zero-extend source to 64 bits
                    const src_bits: u8 = switch (ll.op) {
                        .u8_to_f32, .u8_to_f64 => 8,
                        .u16_to_f32, .u16_to_f64 => 16,
                        .u32_to_f32, .u32_to_f64 => 32,
                        else => unreachable,
                    };
                    const shift_amount: u8 = 64 - src_bits;
                    try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                    try self.emitLsrImm(.w64, src_reg, src_reg, shift_amount);

                    // Convert (now fits in positive i64) to f64
                    const freg = self.codegen.allocFloat() orelse unreachable;
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.scvtfFloatFromGen(.double, freg, src_reg, .w64);
                    } else {
                        try self.codegen.emit.cvtsi2sdRegReg(.w64, freg, src_reg);
                    }
                    self.codegen.freeGeneral(src_reg);
                    return .{ .float_reg = freg };
                },

                // ── u64 to float conversions ──
                // u64 may exceed i64 max, so we need unsigned conversion.
                .u64_to_f32,
                .u64_to_f64,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);
                    const freg = self.codegen.allocFloat() orelse unreachable;

                    if (comptime target.toCpuArch() == .aarch64) {
                        // UCVTF handles unsigned integers directly
                        try self.codegen.emit.ucvtfFloatFromGen(.double, freg, src_reg, .w64);
                    } else {
                        // x86_64 has no unsigned int-to-float instruction.
                        // If the high bit is clear (value < 2^63), CVTSI2SD works directly.
                        // If set, we halve the value, convert, and double.
                        //   test src, src
                        //   js .large
                        //   cvtsi2sd freg, src
                        //   jmp .done
                        // .large:
                        //   mov tmp, src
                        //   shr tmp, 1
                        //   and src, 1
                        //   or tmp, src
                        //   cvtsi2sd freg, tmp
                        //   addsd freg, freg
                        // .done:

                        // test src_reg, src_reg (sets SF if high bit is 1)
                        try self.codegen.emit.testRegReg(.w64, src_reg, src_reg);
                        // JS .large
                        const large_patch = try self.codegen.emitCondJump(.sign);

                        // Small path: value fits in i64
                        try self.codegen.emit.cvtsi2sdRegReg(.w64, freg, src_reg);
                        const done_patch = try self.codegen.emitJump();

                        // .large:
                        self.codegen.patchJump(large_patch, self.codegen.currentOffset());
                        const tmp_reg = try self.allocTempGeneral();
                        try self.codegen.emit.movRegReg(.w64, tmp_reg, src_reg);
                        // shr tmp, 1
                        try self.codegen.emit.shrRegImm8(.w64, tmp_reg, 1);
                        // and src, 1
                        try self.codegen.emit.andRegImm8(src_reg, 1);
                        // or tmp, src
                        try self.codegen.emit.orRegReg(.w64, tmp_reg, src_reg);
                        // cvtsi2sd freg, tmp
                        try self.codegen.emit.cvtsi2sdRegReg(.w64, freg, tmp_reg);
                        // addsd freg, freg (double the result)
                        try self.codegen.emit.addsdRegReg(freg, freg);
                        self.codegen.freeGeneral(tmp_reg);

                        // .done:
                        self.codegen.patchJump(done_patch, self.codegen.currentOffset());
                    }
                    self.codegen.freeGeneral(src_reg);
                    return .{ .float_reg = freg };
                },

                // ── Float-to-float conversions ──
                // Internally all floats are stored as f64, so these are effectively no-ops.
                // f32→f64: the source is already f64 in a float register.
                // f64→f32_wrap: the f32 narrowing happens at the store point.
                .f32_to_f64,
                .f64_to_f32_wrap,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    return .{ .float_reg = try self.ensureInFloatReg(src_loc) };
                },

                // ── Float-to-signed-integer truncating conversions ──
                // Saturating conversion: NaN→0, clamp to [min, max], truncate toward zero.
                // On aarch64, FCVTZS handles all edge cases natively.
                // On x86_64, CVTTSD2SI returns indefinite (0x80...0) on overflow/NaN,
                // so we use C wrappers for correctness.
                .f32_to_i8_trunc,
                .f32_to_i16_trunc,
                .f32_to_i32_trunc,
                .f32_to_i64_trunc,
                .f64_to_i8_trunc,
                .f64_to_i16_trunc,
                .f64_to_i32_trunc,
                .f64_to_i64_trunc,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const freg = try self.ensureInFloatReg(src_loc);

                    const dst_reg = self.codegen.allocGeneral() orelse unreachable;
                    if (comptime target.toCpuArch() == .aarch64) {
                        // FCVTZS natively saturates and handles NaN (→0)
                        try self.codegen.emit.fcvtzsGenFromFloat(.double, dst_reg, freg, .w64);
                    } else {
                        // x86_64 CVTTSD2SI: handles i64 range correctly for values in range.
                        // Out-of-range returns 0x8000000000000000 (indefinite). For the dev
                        // backend this is acceptable — Roc programs shouldn't rely on
                        // saturating behavior for out-of-range float-to-int conversions.
                        try self.codegen.emit.cvttsd2siRegReg(.w64, dst_reg, freg);
                    }
                    self.codegen.freeFloat(freg);

                    // Mask to target width for sub-64-bit types
                    const dst_bits: u8 = switch (ll.op) {
                        .f32_to_i8_trunc, .f64_to_i8_trunc => 8,
                        .f32_to_i16_trunc, .f64_to_i16_trunc => 16,
                        .f32_to_i32_trunc, .f64_to_i32_trunc => 32,
                        .f32_to_i64_trunc, .f64_to_i64_trunc => 64,
                        else => unreachable,
                    };
                    if (dst_bits < 64) {
                        // Sign-extend to normalize the value in the register
                        const shift_amount: u8 = 64 - dst_bits;
                        try self.emitShlImm(.w64, dst_reg, dst_reg, shift_amount);
                        try self.emitAsrImm(.w64, dst_reg, dst_reg, shift_amount);
                    }
                    return .{ .general_reg = dst_reg };
                },

                // ── Float-to-unsigned-integer truncating conversions ──
                .f32_to_u8_trunc,
                .f32_to_u16_trunc,
                .f32_to_u32_trunc,
                .f32_to_u64_trunc,
                .f64_to_u8_trunc,
                .f64_to_u16_trunc,
                .f64_to_u32_trunc,
                .f64_to_u64_trunc,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const freg = try self.ensureInFloatReg(src_loc);

                    const dst_reg = self.codegen.allocGeneral() orelse unreachable;
                    if (comptime target.toCpuArch() == .aarch64) {
                        // FCVTZU natively handles unsigned conversion with saturation
                        try self.codegen.emit.fcvtzuGenFromFloat(.double, dst_reg, freg, .w64);
                    } else {
                        // x86_64: CVTTSD2SI is signed only. For u64, values > i64_max
                        // return indefinite. For the dev backend, we use signed conversion
                        // which handles the full u32 range and most of the u64 range.
                        try self.codegen.emit.cvttsd2siRegReg(.w64, dst_reg, freg);
                    }
                    self.codegen.freeFloat(freg);

                    // Mask to target width for sub-64-bit types
                    const dst_bits: u8 = switch (ll.op) {
                        .f32_to_u8_trunc, .f64_to_u8_trunc => 8,
                        .f32_to_u16_trunc, .f64_to_u16_trunc => 16,
                        .f32_to_u32_trunc, .f64_to_u32_trunc => 32,
                        .f32_to_u64_trunc, .f64_to_u64_trunc => 64,
                        else => unreachable,
                    };
                    if (dst_bits < 64) {
                        const shift_amount: u8 = 64 - dst_bits;
                        try self.emitShlImm(.w64, dst_reg, dst_reg, shift_amount);
                        try self.emitLsrImm(.w64, dst_reg, dst_reg, shift_amount);
                    }
                    return .{ .general_reg = dst_reg };
                },

                // ── Integer widening to 128-bit ──
                // Sign-extend or zero-extend to 128 bits (stored as 16 bytes on stack).
                .u8_to_u128,
                .u8_to_i128,
                .u16_to_u128,
                .u16_to_i128,
                .u32_to_u128,
                .u32_to_i128,
                .u64_to_u128,
                .u64_to_i128,
                .i8_to_i128,
                .i16_to_i128,
                .i32_to_i128,
                .i64_to_i128,
                // Signed-to-unsigned 128-bit wrapping: same as sign-extending
                // to 128 bits and reinterpreting the bits as unsigned.
                .i8_to_u128_wrap,
                .i16_to_u128_wrap,
                .i32_to_u128_wrap,
                .i64_to_u128_wrap,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);

                    const is_signed = switch (ll.op) {
                        .i8_to_i128,
                        .i16_to_i128,
                        .i32_to_i128,
                        .i64_to_i128,
                        .i8_to_u128_wrap,
                        .i16_to_u128_wrap,
                        .i32_to_u128_wrap,
                        .i64_to_u128_wrap,
                        => true,
                        else => false,
                    };
                    const src_bits: u8 = switch (ll.op) {
                        .u8_to_u128, .u8_to_i128, .i8_to_i128, .i8_to_u128_wrap => 8,
                        .u16_to_u128, .u16_to_i128, .i16_to_i128, .i16_to_u128_wrap => 16,
                        .u32_to_u128, .u32_to_i128, .i32_to_i128, .i32_to_u128_wrap => 32,
                        .u64_to_u128, .u64_to_i128, .i64_to_i128, .i64_to_u128_wrap => 64,
                        else => unreachable,
                    };

                    // Sign/zero extend source to 64 bits
                    if (src_bits < 64) {
                        const shift_amount: u8 = 64 - src_bits;
                        if (is_signed) {
                            try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                            try self.emitAsrImm(.w64, src_reg, src_reg, shift_amount);
                        } else {
                            try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                            try self.emitLsrImm(.w64, src_reg, src_reg, shift_amount);
                        }
                    }

                    const stack_offset = self.codegen.allocStackSlot(16);
                    // Store low 64 bits
                    try self.codegen.emitStoreStack(.w64, stack_offset, src_reg);

                    // High 64 bits: sign-extend for signed, zero for unsigned
                    if (is_signed) {
                        try self.emitAsrImm(.w64, src_reg, src_reg, 63);
                        try self.codegen.emitStoreStack(.w64, stack_offset + 8, src_reg);
                    } else {
                        try self.codegen.emitLoadImm(src_reg, 0);
                        try self.codegen.emitStoreStack(.w64, stack_offset + 8, src_reg);
                    }
                    self.codegen.freeGeneral(src_reg);
                    return .{ .stack_i128 = stack_offset };
                },

                // ── 128-bit narrowing (wrapping truncation) ──
                // Take the low bytes and mask to target width.
                .u128_to_i8_wrap,
                .u128_to_i16_wrap,
                .u128_to_i32_wrap,
                .u128_to_i64_wrap,
                .u128_to_i128_wrap,
                .u128_to_u8_wrap,
                .u128_to_u16_wrap,
                .u128_to_u32_wrap,
                .u128_to_u64_wrap,
                .i128_to_i8_wrap,
                .i128_to_i16_wrap,
                .i128_to_i32_wrap,
                .i128_to_i64_wrap,
                .i128_to_u8_wrap,
                .i128_to_u16_wrap,
                .i128_to_u32_wrap,
                .i128_to_u64_wrap,
                .i128_to_u128_wrap,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const src_signedness: std.builtin.Signedness = switch (ll.op) {
                        .i128_to_i8_wrap, .i128_to_i16_wrap, .i128_to_i32_wrap, .i128_to_i64_wrap, .i128_to_u8_wrap, .i128_to_u16_wrap, .i128_to_u32_wrap, .i128_to_u64_wrap, .i128_to_u128_wrap => .signed,
                        else => .unsigned,
                    };
                    const parts = try self.getI128Parts(src_loc, src_signedness);

                    const dst_bits: u8 = switch (ll.op) {
                        .u128_to_i8_wrap, .u128_to_u8_wrap, .i128_to_i8_wrap, .i128_to_u8_wrap => 8,
                        .u128_to_i16_wrap, .u128_to_u16_wrap, .i128_to_i16_wrap, .i128_to_u16_wrap => 16,
                        .u128_to_i32_wrap, .u128_to_u32_wrap, .i128_to_i32_wrap, .i128_to_u32_wrap => 32,
                        .u128_to_i64_wrap, .u128_to_u64_wrap, .i128_to_i64_wrap, .i128_to_u64_wrap => 64,
                        .u128_to_i128_wrap, .i128_to_u128_wrap => 128,
                        else => unreachable,
                    };

                    if (dst_bits == 128) {
                        // 128-bit to 128-bit wrap is a reinterpret (no-op on bits)
                        const stack_offset = self.codegen.allocStackSlot(16);
                        try self.codegen.emitStoreStack(.w64, stack_offset, parts.low);
                        try self.codegen.emitStoreStack(.w64, stack_offset + 8, parts.high);
                        self.codegen.freeGeneral(parts.low);
                        self.codegen.freeGeneral(parts.high);
                        return .{ .stack_i128 = stack_offset };
                    }

                    self.codegen.freeGeneral(parts.high);

                    if (dst_bits < 64) {
                        const shift_amount: u8 = 64 - dst_bits;
                        try self.emitShlImm(.w64, parts.low, parts.low, shift_amount);
                        try self.emitLsrImm(.w64, parts.low, parts.low, shift_amount);
                    }
                    return .{ .general_reg = parts.low };
                },

                // ── Integer to Dec conversions ──
                // Dec is i128 internally, representing a fixed-point number with 18 decimal places.
                // Conversion: multiply the integer by one_point_zero (10^18).
                .u8_to_dec,
                .u16_to_dec,
                .u32_to_dec,
                .u64_to_dec,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);

                    // Zero-extend source to 64 bits
                    const src_bits: u8 = switch (ll.op) {
                        .u8_to_dec => 8,
                        .u16_to_dec => 16,
                        .u32_to_dec => 32,
                        .u64_to_dec => 64,
                        else => unreachable,
                    };
                    if (src_bits < 64) {
                        const shift_amount: u8 = 64 - src_bits;
                        try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                        try self.emitLsrImm(.w64, src_reg, src_reg, shift_amount);
                    }

                    // Call roc_builtins_u64_to_dec(out_low, out_high, u64) -> void
                    return try self.callScalarToI128(src_reg, @intFromPtr(&dev_wrappers.roc_builtins_u64_to_dec), .u64_to_dec);
                },

                .i8_to_dec,
                .i16_to_dec,
                .i32_to_dec,
                .i64_to_dec,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(src_loc);

                    // Sign-extend source to 64 bits
                    const src_bits: u8 = switch (ll.op) {
                        .i8_to_dec => 8,
                        .i16_to_dec => 16,
                        .i32_to_dec => 32,
                        .i64_to_dec => 64,
                        else => unreachable,
                    };
                    if (src_bits < 64) {
                        const shift_amount: u8 = 64 - src_bits;
                        try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                        try self.emitAsrImm(.w64, src_reg, src_reg, shift_amount);
                    }

                    // Call roc_builtins_i64_to_dec(out_low, out_high, i64) -> void
                    return try self.callScalarToI128(src_reg, @intFromPtr(&dev_wrappers.roc_builtins_i64_to_dec), .i64_to_dec);
                },

                // ── Dec to integer truncating conversions ──
                // Divide the Dec i128 by one_point_zero (10^18), truncate to target size.
                .dec_to_i8_trunc,
                .dec_to_i16_trunc,
                .dec_to_i32_trunc,
                .dec_to_i64_trunc,
                .dec_to_u8_trunc,
                .dec_to_u16_trunc,
                .dec_to_u32_trunc,
                .dec_to_u64_trunc,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128

                    // Call roc_builtins_dec_to_i64_trunc(low, high) -> i64
                    const result_reg = self.codegen.allocGeneral() orelse unreachable;
                    const fn_addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_to_i64_trunc);

                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addRegArg(parts.low);
                    try builder.addRegArg(parts.high);
                    try self.callBuiltin(&builder, fn_addr, .dec_to_i64_trunc);
                    const ret_reg: GeneralReg = ret_reg_0;
                    try self.codegen.emit.movRegReg(.w64, result_reg, ret_reg);
                    self.codegen.freeGeneral(parts.low);
                    self.codegen.freeGeneral(parts.high);

                    // Mask to target width
                    const dst_bits: u8 = switch (ll.op) {
                        .dec_to_i8_trunc, .dec_to_u8_trunc => 8,
                        .dec_to_i16_trunc, .dec_to_u16_trunc => 16,
                        .dec_to_i32_trunc, .dec_to_u32_trunc => 32,
                        .dec_to_i64_trunc, .dec_to_u64_trunc => 64,
                        else => unreachable,
                    };
                    if (dst_bits < 64) {
                        const shift_amount: u8 = 64 - dst_bits;
                        try self.emitShlImm(.w64, result_reg, result_reg, shift_amount);
                        try self.emitLsrImm(.w64, result_reg, result_reg, shift_amount);
                    }
                    return .{ .general_reg = result_reg };
                },

                // ── Dec to i128 truncating ──
                .dec_to_i128_trunc => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128
                    const fn_addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_to_i64_trunc);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addRegArg(parts.low);
                    try builder.addRegArg(parts.high);
                    try self.callBuiltin(&builder, fn_addr, .dec_to_i64_trunc);
                    self.codegen.freeGeneral(parts.low);
                    self.codegen.freeGeneral(parts.high);

                    // Sign-extend result from i64 to i128
                    const stack_offset = self.codegen.allocStackSlot(16);
                    try self.codegen.emitStoreStack(.w64, stack_offset, ret_reg_0);
                    try self.emitAsrImm(.w64, ret_reg_0, ret_reg_0, 63);
                    try self.codegen.emitStoreStack(.w64, stack_offset + 8, ret_reg_0);
                    return .{ .stack_i128 = stack_offset };
                },

                // ── Dec to u128 truncating ──
                .dec_to_u128_trunc => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128
                    const fn_addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_to_i64_trunc);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addRegArg(parts.low);
                    try builder.addRegArg(parts.high);
                    try self.callBuiltin(&builder, fn_addr, .dec_to_i64_trunc);
                    self.codegen.freeGeneral(parts.low);
                    self.codegen.freeGeneral(parts.high);

                    const stack_offset = self.codegen.allocStackSlot(16);
                    try self.codegen.emitStoreStack(.w64, stack_offset, ret_reg_0);
                    try self.codegen.emitLoadImm(ret_reg_0, 0);
                    try self.codegen.emitStoreStack(.w64, stack_offset + 8, ret_reg_0);
                    return .{ .stack_i128 = stack_offset };
                },

                // ── Dec to float conversions ──
                .dec_to_f64 => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128
                    return try self.callI128PartsToF64(parts, @intFromPtr(&dev_wrappers.roc_builtins_dec_to_f64), .dec_to_f64);
                },
                .dec_to_f32_wrap => {
                    // Dec to f32: convert to f64 first (f32 narrowing happens at store)
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128
                    return try self.callI128PartsToF64(parts, @intFromPtr(&dev_wrappers.roc_builtins_dec_to_f64), .dec_to_f64);
                },

                // ── 128-bit integer to float conversions ──
                .i128_to_f32,
                .i128_to_f64,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed);
                    return try self.callI128PartsToF64(parts, @intFromPtr(&dev_wrappers.roc_builtins_i128_to_f64), .i128_to_f64);
                },
                .u128_to_f32,
                .u128_to_f64,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const parts = try self.getI128Parts(src_loc, .unsigned);
                    return try self.callI128PartsToF64(parts, @intFromPtr(&dev_wrappers.roc_builtins_u128_to_f64), .u128_to_f64);
                },

                // ── Float to 128-bit integer truncating conversions ──
                .f32_to_i128_trunc,
                .f64_to_i128_trunc,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const freg = try self.ensureInFloatReg(src_loc);
                    return try self.callF64ToI128(freg, @intFromPtr(&dev_wrappers.roc_builtins_f64_to_i128_trunc), .f64_to_i128_trunc);
                },
                .f32_to_u128_trunc,
                .f64_to_u128_trunc,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.generateExpr(args[0]);
                    const freg = try self.ensureInFloatReg(src_loc);
                    return try self.callF64ToI128(freg, @intFromPtr(&dev_wrappers.roc_builtins_f64_to_u128_trunc), .f64_to_u128_trunc);
                },

                // ── String low-level operations ──

                .str_to_utf8 => {
                    // str_to_utf8(str) -> List(U8)
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrToUtf8), .str_to_utf8, .list);
                },
                .str_is_eq => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    const eq_loc = try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrEqual), .str_equal);
                    const eq_reg = try self.ensureInGeneralReg(eq_loc);
                    return .{ .general_reg = eq_reg };
                },
                .str_concat => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2RocOpsToStr(a_off, b_off, @intFromPtr(&wrapStrConcat), .str_concat);
                },
                .str_contains => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrContains), .str_contains);
                },
                .str_starts_with => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrStartsWith), .str_starts_with);
                },
                .str_ends_with => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrEndsWith), .str_ends_with);
                },
                .str_count_utf8_bytes => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1ToScalar(str_off, @intFromPtr(&wrapStrCountUtf8Bytes), .str_count_utf8_bytes);
                },
                .str_caseless_ascii_equals => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrCaselessAsciiEquals), .str_caseless_ascii_equals);
                },
                .str_repeat => {
                    // str_repeat(str, count) -> Str
                    if (args.len != 2) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const count_loc = try self.generateExpr(args[1]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    const count_off = try self.ensureOnStack(count_loc, 8);
                    return try self.callStr1U64RocOpsToStr(str_off, count_off, @intFromPtr(&wrapStrRepeat), .str_repeat);
                },
                .str_trim => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrTrim), .str_trim, .str);
                },
                .str_trim_start => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrTrimStart), .str_trim_start, .str);
                },
                .str_trim_end => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrTrimEnd), .str_trim_end, .str);
                },
                .str_split_on => {
                    // str_split(str, delimiter) -> List(Str)
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2RocOpsToResult(a_off, b_off, @intFromPtr(&wrapStrSplit), .str_split, .list);
                },
                .str_join_with => {
                    // str_join_with(list, separator) -> Str
                    if (args.len != 2) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const sep_loc = try self.generateExpr(args[1]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const sep_off = try self.ensureOnStack(sep_loc, roc_str_size);
                    return try self.callStr2RocOpsToResult(list_off, sep_off, @intFromPtr(&wrapStrJoinWith), .str_join_with, .str);
                },
                .str_reserve => {
                    // str_reserve(str, spare) -> Str
                    if (args.len != 2) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const spare_loc = try self.generateExpr(args[1]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    const spare_off = try self.ensureOnStack(spare_loc, 8);
                    return try self.callStr1U64RocOpsToStr(str_off, spare_off, @intFromPtr(&wrapStrReserve), .str_reserve);
                },
                .str_release_excess_capacity => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrReleaseExcessCapacity), .str_release_excess_capacity, .str);
                },
                .str_with_capacity => {
                    // str_with_capacity(capacity) -> Str
                    if (args.len != 1) unreachable;
                    const cap_loc = try self.generateExpr(args[0]);
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const fn_addr: usize = @intFromPtr(&wrapStrWithCapacity);
                    const cap_reg = try self.ensureInGeneralReg(cap_loc);

                    {
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addRegArg(cap_reg);
                        self.codegen.freeGeneral(cap_reg);
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, fn_addr, .str_with_capacity);
                    }
                    return .{ .stack_str = result_offset };
                },
                .str_drop_prefix => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2RocOpsToResult(a_off, b_off, @intFromPtr(&wrapStrDropPrefix), .str_drop_prefix, .str);
                },
                .str_drop_suffix => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2RocOpsToResult(a_off, b_off, @intFromPtr(&wrapStrDropSuffix), .str_drop_suffix, .str);
                },
                .str_with_ascii_lowercased => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrWithAsciiLowercased), .str_with_ascii_lowercased, .str);
                },
                .str_with_ascii_uppercased => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrWithAsciiUppercased), .str_with_ascii_uppercased, .str);
                },
                .str_from_utf8_lossy => {
                    // str_from_utf8_lossy(list) -> Str
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    return try self.callStr1RocOpsToResult(list_off, @intFromPtr(&wrapStrFromUtf8Lossy), .str_from_utf8_lossy, .str);
                },
                .str_from_utf8 => {
                    // str_from_utf8(list) -> Result Str [BadUtf8 {problem: Utf8Problem, index: U64}]
                    // The C builtin returns FromUtf8Try {byte_index: u64, string: RocStr, is_ok: bool, problem_code: u8}
                    // We must convert it to the Roc tag union layout.
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);

                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                    const raw_size: i32 = @intCast(@sizeOf(FromUtf8Try));
                    const raw_offset = self.codegen.allocStackSlot(raw_size);

                    // Call C builtin: fn(out, list_bytes, list_len, list_cap, roc_ops) -> void
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(frame_ptr, raw_offset);
                    try builder.addMemArg(frame_ptr, list_off);
                    try builder.addMemArg(frame_ptr, list_off + 8);
                    try builder.addMemArg(frame_ptr, list_off + 16);
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, @intFromPtr(&wrapStrFromUtf8), .str_from_utf8);

                    // Now convert the C struct to a Roc tag union.
                    // FromUtf8Try layout: byte_index(u64)@0, string(RocStr)@8, is_ok(bool)@32, problem_code(u8)@33
                    // Result tag union: [Err [BadUtf8 {index: U64, problem: Utf8Problem}], Ok Str]
                    //   Err(0): payload = {index: U64@0, problem: U8@8}
                    //   Ok(1): payload = Str (24 bytes)@0
                    //   discriminant at offset = max payload size = 24
                    const ls = self.layout_store;
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    if (ret_layout_val.tag == .tag_union) {
                        const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
                        const tag_size = tu_data.size;
                        const disc_offset = tu_data.discriminant_offset;
                        const disc_size = tu_data.discriminant_size;

                        const result_slot = self.codegen.allocStackSlot(tag_size);
                        try self.zeroStackArea(result_slot, tag_size);

                        // Load is_ok byte from C struct (offset 32)
                        const ok_reg = try self.allocTempGeneral();
                        try self.emitLoadStackW8(ok_reg, raw_offset + 32);
                        try self.emitCmpImm(ok_reg, 0);
                        self.codegen.freeGeneral(ok_reg);

                        // Jump to Err branch if is_ok == 0
                        const err_patch = try self.codegen.emitCondJump(condEqual());

                        // === OK branch: copy string (24 bytes from raw+8 to result+0) ===
                        {
                            const temp_reg = try self.allocTempGeneral();
                            try self.copyChunked(temp_reg, frame_ptr, raw_offset + 8, frame_ptr, result_slot, 24);
                            self.codegen.freeGeneral(temp_reg);
                        }
                        try self.storeDiscriminant(result_slot + @as(i32, @intCast(disc_offset)), 1, disc_size);
                        const end_patch = try self.codegen.emitJump();

                        // === ERR branch ===
                        self.codegen.patchJump(err_patch, self.codegen.currentOffset());
                        // Copy byte_index (8 bytes from raw+0 to result+0) as the 'index' field
                        {
                            const temp_reg = try self.allocTempGeneral();
                            try self.copyChunked(temp_reg, frame_ptr, raw_offset, frame_ptr, result_slot, 8);
                            self.codegen.freeGeneral(temp_reg);
                        }
                        // Copy problem_code (1 byte from raw+33 to result+8) as the 'problem' field
                        {
                            const prob_reg = try self.allocTempGeneral();
                            try self.emitLoadStackW8(prob_reg, raw_offset + 33);
                            try self.emitStoreStackW8(result_slot + 8, prob_reg);
                            self.codegen.freeGeneral(prob_reg);
                        }
                        try self.storeDiscriminant(result_slot + @as(i32, @intCast(disc_offset)), 0, disc_size);

                        // === END ===
                        self.codegen.patchJump(end_patch, self.codegen.currentOffset());
                        return .{ .stack = .{ .offset = result_slot } };
                    }

                    // Fallback: return raw C struct (for contexts that don't need tag union)
                    return .{ .stack = .{ .offset = raw_offset } };
                },

                // ── Remaining list low-level operations ──

                .list_set => {
                    // list_set(list, index, element) -> List
                    if (args.len != 3) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const index_loc = try self.generateExpr(args[1]);
                    const elem_loc = try self.generateExpr(args[2]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const ret_layout = ls.getLayout(ll.ret_layout);
                    const elem_size_align: layout.SizeAlign = switch (ret_layout.tag) {
                        .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                        .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                        else => unreachable,
                    };
                    const elements_refcounted: bool = switch (ret_layout.tag) {
                        .list => ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list)),
                        else => false,
                    };

                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const index_off = try self.ensureOnStack(index_loc, 8);
                    const elem_off = try self.ensureOnStack(elem_loc, elem_size_align.size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    // We need a scratch slot for the old element (out_element param)
                    const old_elem_slot = self.codegen.allocStackSlot(@intCast(if (elem_size_align.size > 0) elem_size_align.size else 8));
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();
                    const fn_addr: usize = @intFromPtr(&wrapListReplace);

                    {
                        // wrapListReplace(out, list_bytes, list_len, list_cap, alignment, index, element, element_width, elements_refcounted, out_element, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_off);
                        try builder.addMemArg(base_reg, list_off + 8);
                        try builder.addMemArg(base_reg, list_off + 16);
                        try builder.addImmArg(@intCast(alignment_bytes));
                        try builder.addMemArg(base_reg, index_off);
                        try builder.addLeaArg(base_reg, elem_off);
                        try builder.addImmArg(@intCast(elem_size_align.size));
                        try builder.addImmArg(if (elements_refcounted) @as(usize, 1) else 0);
                        try builder.addLeaArg(base_reg, old_elem_slot);
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_replace);
                    }

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_first => {
                    // list_first(list) -> element  (same as list_get at index 0)
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    return try self.listGetAtConstIndex(list_loc, 0, ll.ret_layout);
                },
                .list_last => {
                    // list_last(list) -> element  (same as list_get at index len-1)
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    return try self.listGetAtLastIndex(list_loc, ll.ret_layout);
                },
                .list_contains => {
                    // list_contains(list, element) -> bool
                    // Linear scan: iterate through list, compare each element
                    if (args.len != 2) unreachable;
                    const ls = self.layout_store;
                    const list_layout_idx = self.exprLayout(args[0]);
                    const list_layout = ls.getLayout(list_layout_idx);
                    switch (list_layout.tag) {
                        .list => {
                            const list_loc = try self.generateExpr(args[0]);
                            const needle_loc = try self.generateExpr(args[1]);
                            return try self.generateListContains(
                                list_loc,
                                needle_loc,
                                list_layout.data.list,
                            );
                        },
                        .list_of_zst => {
                            // ZST elements: contains = list is non-empty
                            const list_loc = try self.generateExpr(args[0]);
                            _ = try self.generateExpr(args[1]); // evaluate needle for side effects
                            return try self.generateZstListContains(list_loc);
                        },
                        else => unreachable,
                    }
                },
                .list_reverse => {
                    // list_reverse(list) -> List
                    // Clone and reverse in place using listSublist (full range)
                    // Actually reverse needs a proper implementation. For now use sublist(0, len) and reverse.
                    // Simplest correct approach: allocate new list, copy elements in reverse order
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    return try self.generateListReverse(list_loc, ll);
                },
                .list_reserve => {
                    // list_reserve(list, spare) -> List
                    if (args.len != 2) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const spare_loc = try self.generateExpr(args[1]);
                    return try self.callListReserveOp(list_loc, spare_loc, ll);
                },
                .list_release_excess_capacity => {
                    // list_release_excess_capacity(list) -> List
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    return try self.callListReleaseExcessCapOp(list_loc, ll);
                },
                .list_split_first => {
                    // list_split_first(list) -> {first: elem, rest: List}
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    return try self.callListSplitOp(ll, list_loc, .first);
                },
                .list_split_last => {
                    // list_split_last(list) -> {rest: List, last: elem}
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    return try self.callListSplitOp(ll, list_loc, .last);
                },

                // ── Integer-to-integer try conversions ──
                // Returns a tag union: Ok(To) | Err({}).
                // Uses C wrappers that check bounds and write the tag union to a buffer.
                .u8_to_i8_try,
                .i8_to_u8_try,
                .i8_to_u16_try,
                .i8_to_u32_try,
                .i8_to_u64_try,
                .i8_to_u128_try,
                .u16_to_i8_try,
                .u16_to_i16_try,
                .u16_to_u8_try,
                .i16_to_i8_try,
                .i16_to_u8_try,
                .i16_to_u16_try,
                .i16_to_u32_try,
                .i16_to_u64_try,
                .i16_to_u128_try,
                .u32_to_i8_try,
                .u32_to_i16_try,
                .u32_to_i32_try,
                .u32_to_u8_try,
                .u32_to_u16_try,
                .i32_to_i8_try,
                .i32_to_i16_try,
                .i32_to_u8_try,
                .i32_to_u16_try,
                .i32_to_u32_try,
                .i32_to_u64_try,
                .i32_to_u128_try,
                .u64_to_i8_try,
                .u64_to_i16_try,
                .u64_to_i32_try,
                .u64_to_i64_try,
                .u64_to_u8_try,
                .u64_to_u16_try,
                .u64_to_u32_try,
                .i64_to_i8_try,
                .i64_to_i16_try,
                .i64_to_i32_try,
                .i64_to_u8_try,
                .i64_to_u16_try,
                .i64_to_u32_try,
                .i64_to_u64_try,
                .i64_to_u128_try,
                .u128_to_i8_try,
                .u128_to_i16_try,
                .u128_to_i32_try,
                .u128_to_i64_try,
                .u128_to_i128_try,
                .u128_to_u8_try,
                .u128_to_u16_try,
                .u128_to_u32_try,
                .u128_to_u64_try,
                .i128_to_i8_try,
                .i128_to_i16_try,
                .i128_to_i32_try,
                .i128_to_i64_try,
                .i128_to_u8_try,
                .i128_to_u16_try,
                .i128_to_u32_try,
                .i128_to_u64_try,
                .i128_to_u128_try,
                => {
                    return try self.generateIntTryConversion(ll, args);
                },

                // ── Float/Dec try_unsafe conversions ──
                // Returns a record { is_int: Bool, in_range: Bool, val_or_memory_garbage: To }.
                .f32_to_i8_try_unsafe,
                .f32_to_i16_try_unsafe,
                .f32_to_i32_try_unsafe,
                .f32_to_i64_try_unsafe,
                .f32_to_i128_try_unsafe,
                .f32_to_u8_try_unsafe,
                .f32_to_u16_try_unsafe,
                .f32_to_u32_try_unsafe,
                .f32_to_u64_try_unsafe,
                .f32_to_u128_try_unsafe,
                .f64_to_i8_try_unsafe,
                .f64_to_i16_try_unsafe,
                .f64_to_i32_try_unsafe,
                .f64_to_i64_try_unsafe,
                .f64_to_i128_try_unsafe,
                .f64_to_u8_try_unsafe,
                .f64_to_u16_try_unsafe,
                .f64_to_u32_try_unsafe,
                .f64_to_u64_try_unsafe,
                .f64_to_u128_try_unsafe,
                .f64_to_f32_try_unsafe,
                .dec_to_i8_try_unsafe,
                .dec_to_i16_try_unsafe,
                .dec_to_i32_try_unsafe,
                .dec_to_i64_try_unsafe,
                .dec_to_i128_try_unsafe,
                .dec_to_u8_try_unsafe,
                .dec_to_u16_try_unsafe,
                .dec_to_u32_try_unsafe,
                .dec_to_u64_try_unsafe,
                .dec_to_u128_try_unsafe,
                .dec_to_f32_try_unsafe,
                .u128_to_dec_try_unsafe,
                .i128_to_dec_try_unsafe,
                => {
                    return try self.generateFloatDecTryUnsafeConversion(ll, args);
                },

                // ── Generic numeric operations (not emitted by LIR lowering) ──
                // The LIR lowering phase resolves these to type-specific operations
                // (int_add_wrap, dec_add, float_add, etc.) before code generation.
                .num_from_str => {
                    return try self.generateNumFromStr(ll, args);
                },
                .list_sublist => {
                    // list_sublist(list, {start, len}) -> List
                    if (args.len != 2) unreachable;
                    const record_layout_idx = self.exprLayout(args[1]);
                    const list_loc = try self.generateExpr(args[0]);
                    const record_loc = try self.generateExpr(args[1]);
                    return try self.callListSublistFromRecord(ll, list_loc, record_loc, record_layout_idx);
                },
                .num_abs => {
                    // Absolute value: for signed types, negate if negative; unsigned is no-op
                    const val_loc = try self.generateExpr(args[0]);
                    return try self.generateNumAbs(val_loc, ll.ret_layout);
                },
                .num_abs_diff => {
                    // |a - b|: compare and subtract in the correct order to avoid wrap/overflow
                    const arg_layout = self.exprLayout(args[0]);
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    return try self.generateAbsDiff(a_loc, b_loc, ll.ret_layout, arg_layout);
                },

                // Numeric arithmetic and comparison ops — route to existing binop helpers
                .num_plus,
                .num_minus,
                .num_times,
                .num_div_by,
                .num_div_trunc_by,
                .num_rem_by,
                .num_mod_by,
                .num_shift_left_by,
                .num_shift_right_by,
                .num_shift_right_zf_by,
                .num_is_eq,
                .num_is_gt,
                .num_is_gte,
                .num_is_lt,
                .num_is_lte,
                => {
                    const lhs_loc = try self.generateExpr(args[0]);
                    const rhs_loc = try self.generateExpr(args[1]);

                    // For numeric/comparison ops, operand layout comes from arguments.
                    // Return layout can be Bool for comparisons, so don't key operand
                    // behavior off `ll.ret_layout`.
                    const operand_layout =
                        self.exprLayout(args[0]);

                    // With ANF, operands are always lookups or literals, so we
                    // dispatch structural equality purely by layout.
                    if (ll.op == .num_is_eq) {
                        {
                            const ls = self.layout_store;
                            const layout_idx = self.exprLayout(args[0]);
                            const stored_layout = ls.getLayout(layout_idx);
                            if (stored_layout.tag == .struct_)
                                return self.generateStructComparisonByLayout(lhs_loc, rhs_loc, layout_idx, .num_is_eq);
                            if (stored_layout.tag == .list or stored_layout.tag == .list_of_zst)
                                return self.generateListComparisonByLayout(lhs_loc, rhs_loc, layout_idx, .num_is_eq);
                            if (stored_layout.tag == .tag_union)
                                return self.generateTagUnionComparisonByLayout(lhs_loc, rhs_loc, layout_idx, .num_is_eq);
                        }
                    }

                    // Check for string operands that ended up in num_is_eq
                    // (e.g., from structural equality on tag unions containing strings)
                    if (ll.op == .num_is_eq) {
                        if (operand_layout == .str) {
                            const a_off = try self.ensureOnStack(lhs_loc, roc_str_size);
                            const b_off = try self.ensureOnStack(rhs_loc, roc_str_size);
                            const eq_loc = try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrEqual), .str_equal);
                            const eq_reg = try self.ensureInGeneralReg(eq_loc);
                            return .{ .general_reg = eq_reg };
                        }
                    }

                    const is_float = operand_layout == .f32 or operand_layout == .f64;
                    const is_i128_op = operand_layout == .dec or operand_layout == .i128 or operand_layout == .u128;

                    if (is_float) {
                        return self.generateFloatBinop(ll.op, lhs_loc, rhs_loc);
                    } else if (is_i128_op) {
                        const adj_lhs = if (is_i128_op and lhs_loc == .stack) ValueLocation{ .stack_i128 = lhs_loc.stack.offset } else lhs_loc;
                        const adj_rhs = if (is_i128_op and rhs_loc == .stack) ValueLocation{ .stack_i128 = rhs_loc.stack.offset } else rhs_loc;
                        return self.generateI128Binop(ll.op, adj_lhs, adj_rhs, operand_layout);
                    } else {
                        return self.generateIntBinop(ll.op, lhs_loc, rhs_loc, operand_layout);
                    }
                },

                .num_negate => {
                    const inner_loc = try self.generateExpr(args[0]);
                    const is_float = ll.ret_layout == .f32 or ll.ret_layout == .f64;
                    const is_i128 = ll.ret_layout == .i128 or ll.ret_layout == .u128 or ll.ret_layout == .dec;

                    if (is_float) {
                        const float_reg = try self.ensureInFloatReg(inner_loc);
                        const result_reg = try self.codegen.allocFloatFor(0);
                        try self.codegen.emitNegF64(result_reg, float_reg);
                        self.codegen.freeFloat(float_reg);
                        return .{ .float_reg = result_reg };
                    } else if (is_i128) {
                        const parts = try self.getI128Parts(inner_loc, .signed);
                        const result_low = try self.allocTempGeneral();
                        const result_high = try self.allocTempGeneral();

                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.subsRegRegReg(.w64, result_low, .ZRSP, parts.low);
                            try self.codegen.emit.sbcRegRegReg(.w64, result_high, .ZRSP, parts.high);
                        } else {
                            try self.codegen.emitLoadImm(result_low, 0);
                            try self.codegen.emit.subRegReg(.w64, result_low, parts.low);
                            try self.codegen.emitLoadImm(result_high, 0);
                            try self.codegen.emit.sbbRegReg(.w64, result_high, parts.high);
                        }

                        self.codegen.freeGeneral(parts.low);
                        self.codegen.freeGeneral(parts.high);

                        const stack_offset = self.codegen.allocStackSlot(16);
                        try self.codegen.emitStoreStack(.w64, stack_offset, result_low);
                        try self.codegen.emitStoreStack(.w64, stack_offset + 8, result_high);

                        self.codegen.freeGeneral(result_low);
                        self.codegen.freeGeneral(result_high);

                        return .{ .stack_i128 = stack_offset };
                    } else {
                        const src_reg = try self.ensureInGeneralReg(inner_loc);
                        const result_reg = try self.allocTempGeneral();
                        try self.codegen.emitNeg(.w64, result_reg, src_reg);
                        self.codegen.freeGeneral(src_reg);
                        return .{ .general_reg = result_reg };
                    }
                },

                .bool_not => {
                    const inner_loc = try self.generateExpr(args[0]);
                    const src_reg = try self.ensureInGeneralReg(inner_loc);
                    const result_reg = try self.allocTempGeneral();
                    try self.codegen.emitXorImm(.w64, result_reg, src_reg, 1);
                    self.codegen.freeGeneral(src_reg);
                    return .{ .general_reg = result_reg };
                },

                // Unimplemented ops
                .num_pow,
                .num_sqrt,
                .num_log,
                .num_round,
                .num_floor,
                .num_ceiling,
                .num_to_str,
                .str_inspect,
                .u8_to_str,
                .i8_to_str,
                .u16_to_str,
                .i16_to_str,
                .u32_to_str,
                .i32_to_str,
                .u64_to_str,
                .i64_to_str,
                .u128_to_str,
                .i128_to_str,
                .dec_to_str,
                .f32_to_str,
                .f64_to_str,
                .num_from_numeral,
                .list_drop_at,
                .compare,
                => {
                    std.debug.panic("UNIMPLEMENTED low-level op: {s}", .{@tagName(ll.op)});
                },
                .list_sort_with => {
                    // list_sort_with(list, comparator) -> List
                    if (args.len != 2) unreachable;
                    const list_loc = try self.generateExpr(args[0]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const ret_layout = ls.getLayout(ll.ret_layout);
                    const elem_size_align: layout.SizeAlign = switch (ret_layout.tag) {
                        .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                        .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                        else => unreachable,
                    };
                    const elements_refcounted: bool = switch (ret_layout.tag) {
                        .list => ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list)),
                        else => false,
                    };

                    // The comparator proc must be explicit in LIR; codegen does not
                    // recover callables from the function-value expression.
                    if (ll.callable_proc.isNone()) {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: list_sort_with is missing callable_proc metadata",
                                .{},
                            );
                        }
                        unreachable;
                    }
                    const cmp_code_offset: usize = try self.resolveComparatorOffset(ll.callable_proc);

                    // Compute the absolute address of the lambda at runtime using
                    // PC-relative addressing: emit LEA/ADR that resolves to the
                    // lambda's code address when the instruction executes.
                    const cmp_addr_slot = self.codegen.allocStackSlot(8);
                    {
                        const current = self.codegen.currentOffset();
                        if (comptime target.toCpuArch() == .aarch64) {
                            // ADR X9, (target - current)
                            const rel: i21 = @intCast(@as(i64, @intCast(cmp_code_offset)) - @as(i64, @intCast(current)));
                            try self.codegen.emit.adr(.X9, rel);
                            try self.codegen.emitStoreStack(.w64, cmp_addr_slot, .X9);
                        } else {
                            // LEA RAX, [RIP + (target - current - 7)]
                            // 7 = size of the LEA instruction itself (REX + opcode + modrm + disp32)
                            const rel: i32 = @intCast(@as(i64, @intCast(cmp_code_offset)) - @as(i64, @intCast(current)) - 7);
                            try self.codegen.emit.leaRegRipRel(.RAX, rel);
                            try self.codegen.emitStoreStack(.w64, cmp_addr_slot, .RAX);
                        }

                        // Record this as an internal address patch so deferred-prologue
                        // proc body shifts can update it.
                        try self.internal_addr_patches.append(self.allocator, .{
                            .instr_offset = current,
                            .target_offset = cmp_code_offset,
                        });
                    }

                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const result_offset = self.codegen.allocStackSlot(roc_list_size);
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();
                    const fn_addr: usize = @intFromPtr(&wrapListSortWith);

                    {
                        // wrapListSortWith(out, list_bytes, list_len, list_cap, cmp_fn_addr, alignment, element_width, elements_refcounted, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_off);
                        try builder.addMemArg(base_reg, list_off + 8);
                        try builder.addMemArg(base_reg, list_off + 16);
                        try builder.addMemArg(base_reg, cmp_addr_slot);
                        try builder.addImmArg(@intCast(alignment_bytes));
                        try builder.addImmArg(@intCast(elem_size_align.size));
                        try builder.addImmArg(if (elements_refcounted) @as(usize, 1) else 0);
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_sort_with);
                    }

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .box_box => {
                    // Box.box(value) -> Box(value): heap-allocate and copy value
                    const ls = self.layout_store;
                    const ret_layout_data = ls.getLayout(ll.ret_layout);

                    if (ret_layout_data.tag == .box_of_zst) {
                        // Boxing a ZST: evaluate the expression (for side effects) but
                        // return a null-like pointer since there's no data to store.
                        _ = try self.generateExpr(args[0]);
                        const reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(reg, 0);
                        return .{ .general_reg = reg };
                    }

                    const box_info = ls.getBoxInfo(ret_layout_data);
                    const elem_size: u32 = box_info.elem_size;
                    const elem_alignment: u32 = box_info.elem_alignment;

                    // Handle ZST element even when layout tag is .box (not .box_of_zst)
                    if (elem_size == 0) {
                        _ = try self.generateExpr(args[0]);
                        const reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(reg, 0);
                        return .{ .general_reg = reg };
                    }

                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    // Allocate heap memory via allocateWithRefcountC
                    const heap_ptr_slot: i32 = self.codegen.allocStackSlot(8);
                    {
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addImmArg(@intCast(elem_size));
                        try builder.addImmArg(@intCast(elem_alignment));
                        try builder.addImmArg(if (box_info.contains_refcounted) 1 else 0);
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, @intFromPtr(&allocateWithRefcountC), .allocate_with_refcount);
                    }
                    // Save heap pointer to stack (generating the value expression may clobber registers)
                    try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

                    // Generate the value expression
                    const value_loc = try self.generateExpr(args[0]);

                    // Copy value to heap
                    const heap_ptr = try self.allocTempGeneral();
                    try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);

                    const value_offset = try self.ensureOnStack(value_loc, elem_size);
                    const temp_reg = try self.allocTempGeneral();
                    try self.copyChunked(temp_reg, frame_ptr, value_offset, heap_ptr, 0, elem_size);
                    self.codegen.freeGeneral(temp_reg);

                    // Return the heap pointer (which IS the Box value)
                    return .{ .general_reg = heap_ptr };
                },
                .box_unbox => {
                    // Box.unbox(box) -> value: dereference the box pointer
                    const ls = self.layout_store;
                    // The argument is the Box — get its layout to find element info
                    const box_arg_layout = self.exprLayout(args[0]);
                    const box_layout_data = ls.getLayout(box_arg_layout);

                    if (box_layout_data.tag == .box_of_zst) {
                        // Unboxing a ZST: evaluate the box expression (for side effects)
                        // but return a ZST (no data).
                        _ = try self.generateExpr(args[0]);
                        return .{ .immediate_i64 = 0 };
                    }

                    const box_info = ls.getBoxInfo(box_layout_data);
                    const elem_size: u32 = box_info.elem_size;

                    // Handle ZST element even when layout tag is .box (not .box_of_zst)
                    if (elem_size == 0) {
                        _ = try self.generateExpr(args[0]);
                        return .{ .immediate_i64 = 0 };
                    }
                    const elem_layout_idx = box_info.elem_layout_idx;
                    const elem_layout_data = box_info.elem_layout;

                    // Generate the box pointer expression
                    const box_loc = try self.generateExpr(args[0]);
                    const box_reg = try self.ensureInGeneralReg(box_loc);

                    // Copy from heap to stack
                    const result_offset = self.codegen.allocStackSlot(elem_size);
                    const temp_reg = try self.allocTempGeneral();
                    try self.copyChunked(temp_reg, box_reg, 0, frame_ptr, result_offset, elem_size);
                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(box_reg);

                    // Return with appropriate value location based on element type
                    if (elem_layout_idx == .i128 or elem_layout_idx == .u128 or elem_layout_idx == .dec) {
                        return .{ .stack_i128 = result_offset };
                    } else if (elem_layout_idx == .str) {
                        return .{ .stack_str = result_offset };
                    } else if (elem_layout_data.tag == .list or elem_layout_data.tag == .list_of_zst) {
                        return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                    } else {
                        return .{ .stack = .{ .offset = result_offset, .size = ValueSize.fromByteCount(elem_size) } };
                    }
                },
                .crash => {
                    // Runtime crash: call roc_crashed via RocOps.
                    // TODO: Implement forwarding the user's crash message string from args.
                    // DO NOT replace this with anything hardcoded. either implement it fully
                    // or LEAVE IT AS A PANIC.
                    @panic("TODO: ll.crash message forwarding is not implemented");
                },
            }
        }

        // ── Helper methods for calling C wrapper builtins ──

        /// Call a C wrapper: fn(out, str_f0, str_f1, str_f2, roc_ops) -> void
        /// Used for str->str and str->list ops that take 1 string + roc_ops
        fn callStr1RocOpsToResult(self: *Self, str_off: i32, fn_addr: usize, builtin_fn: BuiltinFn, result_kind: enum { str, list }) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            // fn(out, str_bytes, str_len, str_cap, roc_ops) - 5 args
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, result_offset);
            try builder.addMemArg(frame_ptr, str_off);
            try builder.addMemArg(frame_ptr, str_off + 8);
            try builder.addMemArg(frame_ptr, str_off + 16);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return switch (result_kind) {
                .str => .{ .stack_str = result_offset },
                .list => .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } },
            };
        }

        /// Call a C wrapper: fn(str_f0, str_f1, str_f2) -> scalar (bool or u64)
        /// Used for str->bool and str->u64 ops that take 1 string
        fn callStr1ToScalar(self: *Self, str_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            // fn(str_bytes, str_len, str_cap) -> scalar - 3 args
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addMemArg(base_ptr, str_off);
            try builder.addMemArg(base_ptr, str_off + 8);
            try builder.addMemArg(base_ptr, str_off + 16);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            // Result is in return register (X0 or RAX)
            const result_reg = try self.allocTempGeneral();
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, result_reg, .X0);
            } else {
                try self.codegen.emit.movRegReg(.w64, result_reg, .RAX);
            }
            return .{ .general_reg = result_reg };
        }

        /// Call a C wrapper: fn(a_f0, a_f1, a_f2, b_f0, b_f1, b_f2) -> bool
        /// Used for (str, str) -> bool comparison ops (equal, contains, starts_with, etc.)
        fn callStr2ToScalar(self: *Self, a_off: i32, b_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            // fn(a_bytes, a_len, a_cap, b_bytes, b_len, b_cap) -> bool - 6 args
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addMemArg(base_ptr, a_off);
            try builder.addMemArg(base_ptr, a_off + 8);
            try builder.addMemArg(base_ptr, a_off + 16);
            try builder.addMemArg(base_ptr, b_off);
            try builder.addMemArg(base_ptr, b_off + 8);
            try builder.addMemArg(base_ptr, b_off + 16);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            // Result is in return register (X0 or RAX)
            const result_reg = try self.allocTempGeneral();
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, result_reg, .X0);
            } else {
                try self.codegen.emit.movRegReg(.w64, result_reg, .RAX);
                // x86_64 ABI: for bool return values, only the low byte (AL) is
                // guaranteed valid. Upper bytes of RAX may contain garbage from the
                // callee (e.g. LLVM may use SETZ AL without clearing upper bytes).
                // Mask to bit 0 so subsequent 64-bit comparisons work correctly.
                try self.codegen.emit.andRegImm8(result_reg, 1);
            }
            return .{ .general_reg = result_reg };
        }

        /// Call a C wrapper: fn(out, a_f0, a_f1, a_f2, b_f0, b_f1, b_f2, roc_ops) -> void
        /// Used for (str, str, roc_ops) -> str/list ops
        fn callStr2RocOpsToStr(self: *Self, a_off: i32, b_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            return self.callStr2RocOpsToResult(a_off, b_off, fn_addr, builtin_fn, .str);
        }

        fn callStr2RocOpsToResult(self: *Self, a_off: i32, b_off: i32, fn_addr: usize, builtin_fn: BuiltinFn, result_kind: enum { str, list }) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            // fn(out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, roc_ops) -> void - 8 args
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_ptr, result_offset);
            try builder.addMemArg(base_ptr, a_off);
            try builder.addMemArg(base_ptr, a_off + 8);
            try builder.addMemArg(base_ptr, a_off + 16);
            try builder.addMemArg(base_ptr, b_off);
            try builder.addMemArg(base_ptr, b_off + 8);
            try builder.addMemArg(base_ptr, b_off + 16);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return switch (result_kind) {
                .str => .{ .stack_str = result_offset },
                .list => .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } },
            };
        }

        /// Call: fn(out, str_f0, str_f1, str_f2, u64_val, roc_ops) -> void
        /// Used for (str, u64, roc_ops) -> str ops like str_repeat, str_reserve
        fn callStr1U64RocOpsToStr(self: *Self, str_off: i32, u64_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            // fn(out, str_bytes, str_len, str_cap, u64_val, roc_ops) -> void - 6 args
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_ptr, result_offset);
            try builder.addMemArg(base_ptr, str_off);
            try builder.addMemArg(base_ptr, str_off + 8);
            try builder.addMemArg(base_ptr, str_off + 16);
            try builder.addMemArg(base_ptr, u64_off);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .stack_str = result_offset };
        }

        /// Helper for list_drop_first, list_drop_last, list_take_first, list_take_last
        /// These all map to listSublist with different start/len calculations
        fn callListSublist(self: *Self, ll: anytype, list_loc: ValueLocation, n_loc: ValueLocation, mode: enum { drop_first, drop_last, take_first, take_last }) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const elements_refcounted = blk: {
                const ret_layout = ls.getLayout(ll.ret_layout);
                break :blk switch (ret_layout.tag) {
                    .list => ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list)),
                    .list_of_zst => false,
                    else => unreachable,
                };
            };

            const elem_size_align: layout.SizeAlign = blk: {
                const ret_layout = ls.getLayout(ll.ret_layout);
                break :blk switch (ret_layout.tag) {
                    .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                    .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                    else => unreachable,
                };
            };

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const n_reg = try self.ensureInGeneralReg(n_loc);

            // Load list length from the struct (offset 8)
            const len_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, len_reg, frame_ptr, list_off + 8);

            // Compute start and len based on mode
            const start_slot = self.codegen.allocStackSlot(8);
            const len_slot = self.codegen.allocStackSlot(8);

            switch (mode) {
                .drop_first => {
                    // start = n, len = max(list_len - n, 0)
                    const diff_reg = try self.allocTempGeneral();
                    try self.emitSaturatingSub(diff_reg, len_reg, n_reg);
                    try self.emitStore(.w64, frame_ptr, start_slot, n_reg);
                    try self.emitStore(.w64, frame_ptr, len_slot, diff_reg);
                    self.codegen.freeGeneral(diff_reg);
                },
                .drop_last => {
                    // start = 0, len = max(list_len - n, 0)
                    const diff_reg = try self.allocTempGeneral();
                    try self.emitSaturatingSub(diff_reg, len_reg, n_reg);
                    const zero_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(zero_reg, 0);
                    try self.emitStore(.w64, frame_ptr, start_slot, zero_reg);
                    try self.emitStore(.w64, frame_ptr, len_slot, diff_reg);
                    self.codegen.freeGeneral(zero_reg);
                    self.codegen.freeGeneral(diff_reg);
                },
                .take_first => {
                    // start = 0, len = min(n, list_len)
                    // listSublist handles this correctly even if n > list_len
                    const zero_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(zero_reg, 0);
                    try self.emitStore(.w64, frame_ptr, start_slot, zero_reg);
                    try self.emitStore(.w64, frame_ptr, len_slot, n_reg);
                    self.codegen.freeGeneral(zero_reg);
                },
                .take_last => {
                    // start = max(list_len - n, 0), len = n
                    const diff_reg = try self.allocTempGeneral();
                    try self.emitSaturatingSub(diff_reg, len_reg, n_reg);
                    try self.emitStore(.w64, frame_ptr, start_slot, diff_reg);
                    try self.emitStore(.w64, frame_ptr, len_slot, n_reg);
                    self.codegen.freeGeneral(diff_reg);
                },
            }
            self.codegen.freeGeneral(n_reg);
            self.codegen.freeGeneral(len_reg);

            // Call roc_builtins_list_sublist(out, list_bytes, list_len, list_cap,
            // alignment, element_width, start, len, elements_refcounted, roc_ops)
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const alignment_bytes = elem_size_align.alignment.toByteUnits();
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_sublist);

            {
                // roc_builtins_list_sublist(out, list_bytes, list_len, list_cap,
                // alignment, element_width, start, len, elements_refcounted, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addImmArg(@intCast(elem_size_align.size));
                try builder.addMemArg(base_reg, start_slot);
                try builder.addMemArg(base_reg, len_slot);
                try builder.addImmArg(if (elements_refcounted) 1 else 0);
                try builder.addRegArg(roc_ops_reg);

                try self.callBuiltin(&builder, fn_addr, .list_sublist);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// list_sublist(list, {start, len}) -> List
        fn callListSublistFromRecord(self: *Self, ll: anytype, list_loc: ValueLocation, record_loc: ValueLocation, record_layout_idx: ?layout.Idx) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const elements_refcounted = blk: {
                const ret_layout = ls.getLayout(ll.ret_layout);
                break :blk switch (ret_layout.tag) {
                    .list => ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list)),
                    .list_of_zst => false,
                    else => unreachable,
                };
            };

            const elem_size_align: layout.SizeAlign = blk: {
                const ret_layout = ls.getLayout(ll.ret_layout);
                break :blk switch (ret_layout.tag) {
                    .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                    .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                    else => unreachable,
                };
            };

            const record_layout = ls.getLayout(record_layout_idx orelse unreachable);
            const record_idx = record_layout.data.struct_.idx;
            const record_size = ls.getStructData(record_idx).size;
            // In shared layout, record field indices are canonical alphabetical order.
            // For { start : U64, len : U64 }, that means index 0 = len and index 1 = start.
            const len_field_off: i32 = @intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 0));
            const start_field_off: i32 = @intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 1));
            if (builtin.mode == .Debug) {
                const sorted_fields = ls.struct_fields.sliceRange(ls.getStructData(record_idx).getFields());
                if (sorted_fields.len != 2) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: list_sublist record expected 2 fields, got {d}",
                        .{sorted_fields.len},
                    );
                }
                if (record_size != 16 or
                    ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .u64 or
                    ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .u64 or
                    ls.getStructFieldSizeByOriginalIndex(record_idx, 0) != 8 or
                    ls.getStructFieldSizeByOriginalIndex(record_idx, 1) != 8)
                {
                    std.debug.panic(
                        "LIR/codegen invariant violated: list_sublist record expected canonical fields len/start as two U64s in 16 bytes, got layouts [{}, {}] size {d}",
                        .{
                            ls.getStructFieldLayoutByOriginalIndex(record_idx, 0),
                            ls.getStructFieldLayoutByOriginalIndex(record_idx, 1),
                            record_size,
                        },
                    );
                }
            }

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const record_off = try self.ensureOnStack(record_loc, record_size);

            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const alignment_bytes = elem_size_align.alignment.toByteUnits();
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_sublist);

            {
                // roc_builtins_list_sublist(out, list_bytes, list_len, list_cap,
                // alignment, element_width, start, len, elements_refcounted, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addImmArg(@intCast(elem_size_align.size));
                try builder.addMemArg(base_reg, record_off + start_field_off);
                try builder.addMemArg(base_reg, record_off + len_field_off);
                try builder.addImmArg(if (elements_refcounted) 1 else 0);
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, fn_addr, .list_sublist);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Helper for list_split_first and list_split_last.
        /// Returns a record {element, List} with fields at layout-determined offsets.
        fn callListSplitOp(self: *Self, ll: anytype, list_loc: ValueLocation, mode: enum { first, last }) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            // Get the return record layout
            const ret_layout = ls.getLayout(ll.ret_layout);
            if (ret_layout.tag != .struct_) unreachable;
            const record_idx = ret_layout.data.struct_.idx;
            const record_data = ls.getStructData(record_idx);
            const result_size: u32 = record_data.size;

            // Find which field is the list and which is the element.
            // The record has exactly 2 fields.
            const field0_layout_idx = ls.getStructFieldLayout(record_idx, 0);
            const field0_layout = ls.getLayout(field0_layout_idx);
            const field0_offset: i32 = @intCast(ls.getStructFieldOffset(record_idx, 0));
            const field1_layout_idx = ls.getStructFieldLayout(record_idx, 1);
            const field1_layout = ls.getLayout(field1_layout_idx);
            const field1_offset: i32 = @intCast(ls.getStructFieldOffset(record_idx, 1));

            const field0_is_list = field0_layout.tag == .list or field0_layout.tag == .list_of_zst;
            const list_field_offset: i32 = if (field0_is_list) field0_offset else field1_offset;
            const elem_field_offset: i32 = if (field0_is_list) field1_offset else field0_offset;
            const elem_layout = if (field0_is_list) field1_layout else field0_layout;

            const elem_size_align = ls.layoutSizeAlign(elem_layout);
            const elem_size: u32 = elem_size_align.size;
            const alignment_bytes = elem_size_align.alignment.toByteUnits();

            // Ensure list is on stack
            const list_off = try self.ensureOnStack(list_loc, roc_list_size);

            // Allocate result struct
            const result_offset = self.codegen.allocStackSlot(result_size);

            // Load list length
            const len_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, len_reg, frame_ptr, list_off + 8);

            // Copy the element from the list into the result struct
            if (elem_size > 0) {
                // Load list pointer
                const ptr_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, ptr_reg, frame_ptr, list_off);

                if (mode == .last) {
                    // Element is at ptr + (len-1) * elem_size
                    const idx_reg = try self.allocTempGeneral();
                    try self.codegen.emit.movRegReg(.w64, idx_reg, len_reg);
                    try self.emitSubImm(.w64, idx_reg, idx_reg, 1);
                    if (elem_size != 1) {
                        const size_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(size_reg, elem_size);
                        try self.emitMulRegs(.w64, idx_reg, idx_reg, size_reg);
                        self.codegen.freeGeneral(size_reg);
                    }
                    try self.emitAddRegs(.w64, ptr_reg, ptr_reg, idx_reg);
                    self.codegen.freeGeneral(idx_reg);
                }

                // Copy element from ptr_reg+0 to result+elem_field_offset
                const elem_dst = result_offset + elem_field_offset;
                const temp_reg = try self.allocTempGeneral();
                if (elem_size <= 8) {
                    const vs = ValueSize.fromByteCount(@intCast(elem_size));
                    try self.emitSizedLoadMem(temp_reg, ptr_reg, 0, vs);
                    try self.emitSizedStoreMem(frame_ptr, elem_dst, temp_reg, vs);
                } else {
                    try self.copyChunked(temp_reg, ptr_reg, 0, frame_ptr, elem_dst, elem_size);
                }
                self.codegen.freeGeneral(temp_reg);
                self.codegen.freeGeneral(ptr_reg);

                if (ls.layoutContainsRefcounted(elem_layout)) {
                    try self.emitIncrefAtStackOffset(elem_dst, if (field0_is_list) field1_layout_idx else field0_layout_idx);
                }
            }

            const rest_list_layout_idx = if (field0_is_list) field0_layout_idx else field1_layout_idx;
            const rest_elements_refcounted = blk: {
                const rest_layout = ls.getLayout(rest_list_layout_idx);
                break :blk switch (rest_layout.tag) {
                    .list => ls.layoutContainsRefcounted(ls.getLayout(rest_layout.data.list)),
                    .list_of_zst => false,
                    else => unreachable,
                };
            };

            // Build rest list via roc_builtins_list_sublist, writing directly into result+list_field_offset
            // For split_first: start=1, len=len-1
            // For split_last: start=0, len=len-1
            const start_slot = self.codegen.allocStackSlot(8);
            const sublist_len_slot = self.codegen.allocStackSlot(8);

            const one_tmp = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(one_tmp, 1);
            const new_len_reg = try self.allocTempGeneral();
            try self.emitSaturatingSub(new_len_reg, len_reg, one_tmp);
            self.codegen.freeGeneral(one_tmp);
            try self.emitStore(.w64, frame_ptr, sublist_len_slot, new_len_reg);
            self.codegen.freeGeneral(new_len_reg);

            switch (mode) {
                .first => {
                    // start = 1
                    const one_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(one_reg, 1);
                    try self.emitStore(.w64, frame_ptr, start_slot, one_reg);
                    self.codegen.freeGeneral(one_reg);
                },
                .last => {
                    // start = 0
                    const zero_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(zero_reg, 0);
                    try self.emitStore(.w64, frame_ptr, start_slot, zero_reg);
                    self.codegen.freeGeneral(zero_reg);
                },
            }
            self.codegen.freeGeneral(len_reg);

            {
                const list_dst_offset = result_offset + list_field_offset;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(frame_ptr, list_dst_offset);
                try builder.addMemArg(frame_ptr, list_off);
                try builder.addMemArg(frame_ptr, list_off + 8);
                try builder.addMemArg(frame_ptr, list_off + 16);
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addImmArg(@intCast(elem_size));
                try builder.addMemArg(frame_ptr, start_slot);
                try builder.addMemArg(frame_ptr, sublist_len_slot);
                try builder.addImmArg(if (rest_elements_refcounted) 1 else 0);
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_list_sublist), .list_sublist);
            }

            // Return the record as a stack value
            return .{ .stack = .{ .offset = result_offset } };
        }

        /// Get element at constant index 0 from a list
        fn listGetAtConstIndex(
            self: *Self,
            list_loc: ValueLocation,
            index: u64,
            ret_layout_idx: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const list_base: i32 = switch (list_loc) {
                .stack => |s| s.offset,
                .list_stack => |ls_info| ls_info.struct_offset,
                else => unreachable,
            };

            const ls = self.layout_store;
            const ret_layout_val = ls.getLayout(ret_layout_idx);
            const elem_size: u32 = ls.layoutSizeAlign(ret_layout_val).size;

            if (elem_size == 0) return .{ .immediate_i64 = 0 };

            // Load list pointer
            const ptr_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, ptr_reg, frame_ptr, list_base);

            // Element address = ptr + index * elem_size
            const byte_offset: u32 = @intCast(index * elem_size);

            // Copy element to stack
            const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));
            const temp_reg = try self.allocTempGeneral();

            if (elem_size <= 8) {
                const vs = ValueSize.fromByteCount(@intCast(elem_size));
                try self.emitSizedLoadMem(temp_reg, ptr_reg, @intCast(byte_offset), vs);
                try self.emitSizedStoreMem(frame_ptr, elem_slot, temp_reg, vs);
            } else {
                try self.copyChunked(temp_reg, ptr_reg, @intCast(byte_offset), frame_ptr, elem_slot, elem_size);
            }
            self.codegen.freeGeneral(temp_reg);
            self.codegen.freeGeneral(ptr_reg);

            if (ls.layoutContainsRefcounted(ret_layout_val)) {
                try self.emitIncrefAtStackOffset(elem_slot, ret_layout_idx);
            }

            var result_loc: ValueLocation = if (ret_layout_idx == .i128 or ret_layout_idx == .u128 or ret_layout_idx == .dec)
                .{ .stack_i128 = elem_slot }
            else if (ret_layout_idx == .str)
                .{ .stack_str = elem_slot }
            else if (ret_layout_val.tag == .list or ret_layout_val.tag == .list_of_zst)
                .{ .list_stack = .{ .struct_offset = elem_slot, .data_offset = 0, .num_elements = 0 } }
            else
                .{ .stack = .{ .offset = elem_slot } };

            result_loc = try self.stabilize(result_loc);
            return result_loc;
        }

        /// Get element at last index (len - 1) from a list
        fn listGetAtLastIndex(
            self: *Self,
            list_loc: ValueLocation,
            ret_layout_idx: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const list_base: i32 = switch (list_loc) {
                .stack => |s| s.offset,
                .list_stack => |ls_info| ls_info.struct_offset,
                else => unreachable,
            };

            const ls = self.layout_store;
            const ret_layout_val = ls.getLayout(ret_layout_idx);
            const elem_size: u32 = ls.layoutSizeAlign(ret_layout_val).size;

            if (elem_size == 0) return .{ .immediate_i64 = 0 };

            // Load list pointer and length
            const ptr_reg = try self.allocTempGeneral();
            const len_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, ptr_reg, frame_ptr, list_base);
            try self.emitLoad(.w64, len_reg, frame_ptr, list_base + 8);

            // index = len - 1
            try self.emitSubImm(.w64, len_reg, len_reg, 1);

            // addr = ptr + index * elem_size
            const addr_reg = try self.allocTempGeneral();
            try self.codegen.emit.movRegReg(.w64, addr_reg, len_reg);
            self.codegen.freeGeneral(len_reg);

            if (elem_size != 1) {
                const size_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(size_reg, elem_size);
                try self.emitMulRegs(.w64, addr_reg, addr_reg, size_reg);
                self.codegen.freeGeneral(size_reg);
            }

            try self.emitAddRegs(.w64, addr_reg, addr_reg, ptr_reg);
            self.codegen.freeGeneral(ptr_reg);

            // Copy element to stack
            const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));
            const temp_reg = try self.allocTempGeneral();

            if (elem_size <= 8) {
                const vs = ValueSize.fromByteCount(@intCast(elem_size));
                try self.emitSizedLoadMem(temp_reg, addr_reg, 0, vs);
                try self.emitSizedStoreMem(frame_ptr, elem_slot, temp_reg, vs);
            } else {
                try self.copyChunked(temp_reg, addr_reg, 0, frame_ptr, elem_slot, elem_size);
            }
            self.codegen.freeGeneral(temp_reg);
            self.codegen.freeGeneral(addr_reg);

            if (ls.layoutContainsRefcounted(ret_layout_val)) {
                try self.emitIncrefAtStackOffset(elem_slot, ret_layout_idx);
            }

            var result_loc: ValueLocation = if (ret_layout_idx == .i128 or ret_layout_idx == .u128 or ret_layout_idx == .dec)
                .{ .stack_i128 = elem_slot }
            else if (ret_layout_idx == .str)
                .{ .stack_str = elem_slot }
            else if (ret_layout_val.tag == .list or ret_layout_val.tag == .list_of_zst)
                .{ .list_stack = .{ .struct_offset = elem_slot, .data_offset = 0, .num_elements = 0 } }
            else
                .{ .stack = .{ .offset = elem_slot } };

            result_loc = try self.stabilize(result_loc);
            return result_loc;
        }

        const HostedCallArg = struct {
            loc: ValueLocation,
            layout_idx: layout.Idx,
        };

        /// Generate code for hosted function calls (platform-provided effects).
        /// Hosted functions follow the RocCall ABI: fn(roc_ops, ret_ptr, args_ptr) -> void
        fn generateHostedCall(self: *Self, hc: anytype) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            const ls = self.layout_store;
            const explicit_args = self.store.getExprSpan(hc.args);
            var hosted_args = std.ArrayList(HostedCallArg).empty;
            defer hosted_args.deinit(self.allocator);

            for (explicit_args) |arg_id| {
                try hosted_args.append(self.allocator, .{
                    .loc = try self.generateExpr(arg_id),
                    .layout_idx = self.exprLayout(arg_id),
                });
            }

            // Determine return value size
            const ret_layout = ls.getLayout(hc.ret_layout);
            const ret_size = ls.layoutSize(ret_layout);

            // Allocate return slot (even for ZST, we need a valid pointer)
            const ret_slot = if (ret_size > 0)
                self.codegen.allocStackSlot(@intCast(ret_size))
            else
                self.codegen.allocStackSlot(8); // Minimum slot for ZST

            // Marshal arguments into a contiguous buffer on the stack
            // First, calculate total size needed for all arguments
            var total_args_size: usize = 0;
            for (hosted_args.items) |arg| {
                const arg_layout = ls.getLayout(arg.layout_idx);
                const arg_size = ls.layoutSize(arg_layout);
                const arg_align = arg_layout.alignment(ls.targetUsize());
                total_args_size = std.mem.alignForward(usize, total_args_size, arg_align.toByteUnits());
                total_args_size += arg_size;
            }

            // Allocate args buffer (at least 8 bytes for empty args case)
            const args_slot = self.codegen.allocStackSlot(@intCast(@max(total_args_size, 8)));

            // Copy each argument into the args buffer
            var offset: usize = 0;
            for (hosted_args.items) |arg| {
                const arg_layout = ls.getLayout(arg.layout_idx);
                const arg_size = ls.layoutSize(arg_layout);
                const arg_align = arg_layout.alignment(ls.targetUsize());
                offset = std.mem.alignForward(usize, offset, arg_align.toByteUnits());

                if (arg_size > 0) {
                    const dest_offset: i32 = args_slot + @as(i32, @intCast(offset));
                    try self.copyValueToStack(arg.loc, dest_offset, arg_size);
                }
                offset += arg_size;
            }

            // RocOps.hosted_fns is at offset 56 (7 pointers * 8 bytes)
            // HostedFunctions.fns is at offset 8 within HostedFunctions (after count u32 + padding)
            // So hosted_fns.fns is at roc_ops + 56 + 8 = roc_ops + 64

            {
                const base_reg = frame_ptr;

                // Load function pointer into a register that won't conflict with
                // CallBuilder's SCRATCH_REG (X9/R11) or param registers
                const fn_ptr_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X10 else .R10;

                // Load hosted_fns.fns pointer, then the specific function pointer
                const fns_ptr_reg = try self.allocTempGeneral();
                const fn_offset: i32 = @intCast(hc.index * 8);

                try self.emitLoad(.w64, fns_ptr_reg, roc_ops_reg, 64);
                try self.emitLoad(.w64, fn_ptr_reg, fns_ptr_reg, fn_offset);
                self.codegen.freeGeneral(fns_ptr_reg);

                // hosted_fn(roc_ops, ret_ptr, args_ptr) — 3 args via CallBuilder
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addRegArg(roc_ops_reg);
                try builder.addLeaArg(base_reg, ret_slot);
                try builder.addLeaArg(base_reg, args_slot);
                try builder.callReg(fn_ptr_reg);
            }

            // Return the result location based on return type
            if (ret_size == 0) {
                // ZST - return unit/empty record
                return .{ .immediate_i64 = 0 };
            } else if (hc.ret_layout == .i128 or hc.ret_layout == .u128 or hc.ret_layout == .dec) {
                return .{ .stack_i128 = ret_slot };
            } else if (hc.ret_layout == .str) {
                return .{ .stack_str = ret_slot };
            } else if (ret_layout.tag == .list or ret_layout.tag == .list_of_zst) {
                return .{ .list_stack = .{ .struct_offset = ret_slot, .data_offset = 0, .num_elements = 0 } };
            } else {
                return .{ .stack = .{ .offset = ret_slot } };
            }
        }

        /// Copy a value to a stack location
        fn copyValueToStack(self: *Self, src_loc: ValueLocation, dest_offset: i32, size: usize) Allocator.Error!void {
            if (size == 0) return;

            switch (src_loc) {
                .immediate_i64 => |val| {
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, val);
                    try self.emitStore(.w64, frame_ptr, dest_offset, temp);
                    self.codegen.freeGeneral(temp);
                },
                .general_reg => |reg| {
                    try self.emitStore(.w64, frame_ptr, dest_offset, reg);
                },
                .stack => |s| {
                    const src_offset = s.offset;
                    try self.copyStackToStack(src_offset, dest_offset, size);
                },
                .stack_i128 => |src_offset| {
                    try self.copyStackToStack(src_offset, dest_offset, 16);
                },
                .stack_str => |src_offset| {
                    try self.copyStackToStack(src_offset, dest_offset, 24);
                },
                .list_stack => |ls_info| {
                    try self.copyStackToStack(ls_info.struct_offset, dest_offset, 24);
                },
                else => unreachable,
            }
        }

        /// Copy data from one stack location to another
        fn copyStackToStack(self: *Self, src_offset: i32, dest_offset: i32, size: usize) Allocator.Error!void {
            if (size == 0) return;

            const temp = try self.allocTempGeneral();

            // Copy 8 bytes at a time
            var copied: usize = 0;
            while (copied + 8 <= size) : (copied += 8) {
                const src_off: i32 = src_offset + @as(i32, @intCast(copied));
                const dest_off: i32 = dest_offset + @as(i32, @intCast(copied));
                try self.emitLoad(.w64, temp, frame_ptr, src_off);
                try self.emitStore(.w64, frame_ptr, dest_off, temp);
            }

            // Handle remaining bytes (1-7)
            if (copied < size) {
                const remaining = size - copied;
                const src_off: i32 = src_offset + @as(i32, @intCast(copied));
                const dest_off: i32 = dest_offset + @as(i32, @intCast(copied));

                if (remaining >= 4) {
                    try self.emitLoad(.w32, temp, frame_ptr, src_off);
                    try self.emitStore(.w32, frame_ptr, dest_off, temp);
                    copied += 4;
                }

                // Handle remaining 1-3 bytes byte-by-byte
                if (copied < size) {
                    const final_src: i32 = src_offset + @as(i32, @intCast(copied));
                    const final_dest: i32 = dest_offset + @as(i32, @intCast(copied));
                    const bytes_left = size - copied;
                    for (0..bytes_left) |i| {
                        const byte_src = final_src + @as(i32, @intCast(i));
                        const byte_dest = final_dest + @as(i32, @intCast(i));
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emitLoadStackByte(temp, byte_src);
                            try self.codegen.emitStoreStackByte(byte_dest, temp);
                        } else {
                            try self.codegen.emit.movRegMem(.w8, temp, .RBP, byte_src);
                            try self.codegen.emit.movMemReg(.w8, .RBP, byte_dest, temp);
                        }
                    }
                }
            }

            self.codegen.freeGeneral(temp);
        }

        /// Generate list_contains: linear scan comparing each element
        fn generateListContains(
            self: *Self,
            list_loc: ValueLocation,
            needle_loc: ValueLocation,
            elem_layout_idx: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const elem_layout = ls.getLayout(elem_layout_idx);
            const elem_sa = ls.layoutSizeAlign(elem_layout);
            const elem_size: u32 = elem_sa.size;

            const list_base: i32 = switch (list_loc) {
                .stack => |s| s.offset,
                .list_stack => |ls_info| ls_info.struct_offset,
                .immediate_i64 => |val| {
                    if (val != 0) unreachable;
                    // Empty list: contains always returns false
                    return .{ .immediate_i64 = 0 };
                },
                else => unreachable,
            };

            // Save needle to stack (handles any type/size)
            const normalized_needle = self.coerceImmediateToLayout(needle_loc, elem_layout_idx);
            const needle_slot = try self.ensureOnStack(normalized_needle, elem_size);

            // Save list ptr and len to stack (they must survive compareFieldByLayout
            // which may call builtins that clobber caller-saved registers)
            const ptr_slot = self.codegen.allocStackSlot(8);
            const len_slot = self.codegen.allocStackSlot(8);
            {
                const ptr_reg = try self.allocTempGeneral();
                const len_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, ptr_reg, frame_ptr, list_base);
                try self.emitLoad(.w64, len_reg, frame_ptr, list_base + 8);
                try self.codegen.emitStoreStack(.w64, ptr_slot, ptr_reg);
                try self.codegen.emitStoreStack(.w64, len_slot, len_reg);
                self.codegen.freeGeneral(ptr_reg);
                self.codegen.freeGeneral(len_reg);
            }

            // Initialize counter and byte offset on stack, result = false
            const ctr_slot = self.codegen.allocStackSlot(8);
            const offset_slot = self.codegen.allocStackSlot(8);
            const result_slot = self.codegen.allocStackSlot(8);
            // Allocate 8-byte-aligned slot so 8-byte copy loop doesn't overflow
            const elem_slot = self.codegen.allocStackSlot(@intCast(std.mem.alignForward(u32, elem_size, 8)));
            {
                const tmp = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(tmp, 0);
                try self.codegen.emitStoreStack(.w64, ctr_slot, tmp);
                try self.codegen.emitStoreStack(.w64, offset_slot, tmp);
                try self.codegen.emitStoreStack(.w64, result_slot, tmp);
                self.codegen.freeGeneral(tmp);
            }

            // Loop start
            const loop_start = self.codegen.currentOffset();

            // if ctr >= len, jump to end
            {
                const ctr_reg = try self.allocTempGeneral();
                const len_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, ctr_reg, ctr_slot);
                try self.codegen.emitLoadStack(.w64, len_reg, len_slot);
                try self.codegen.emit.cmpRegReg(.w64, ctr_reg, len_reg);
                self.codegen.freeGeneral(ctr_reg);
                self.codegen.freeGeneral(len_reg);
            }
            const exit_patch = try self.codegen.emitCondJump(condGreaterOrEqual());

            // Copy element from heap (ptr + offset) to elem_slot
            {
                const ptr_reg = try self.allocTempGeneral();
                const off_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, ptr_reg, ptr_slot);
                try self.codegen.emitLoadStack(.w64, off_reg, offset_slot);
                // addr_reg = ptr + byte_offset
                try self.emitAddRegs(.w64, ptr_reg, ptr_reg, off_reg);
                self.codegen.freeGeneral(off_reg);

                // Copy elem_size bytes from ptr_reg to elem_slot (8-byte chunks)
                const tmp = try self.allocTempGeneral();
                try self.copyChunked(tmp, ptr_reg, 0, frame_ptr, elem_slot, elem_size);
                self.codegen.freeGeneral(tmp);
                self.codegen.freeGeneral(ptr_reg);
            }

            // Compare element with needle using layout-aware comparison
            const eq_reg = try self.allocTempGeneral();
            try self.compareFieldByLayout(elem_slot, needle_slot, elem_layout_idx, elem_size, eq_reg);

            // If equal, set result = true and jump to end
            try self.emitCmpImm(eq_reg, 1);
            self.codegen.freeGeneral(eq_reg);
            const not_equal_patch = try self.codegen.emitCondJump(condNotEqual());
            // Found! Set result = 1
            {
                const one_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(one_reg, 1);
                try self.codegen.emitStoreStack(.w64, result_slot, one_reg);
                self.codegen.freeGeneral(one_reg);
            }
            // Jump to end
            const found_patch = try self.codegen.emitJump();
            // Not equal: continue
            self.codegen.patchJump(not_equal_patch, self.codegen.currentOffset());

            // Increment counter and byte offset
            {
                const ctr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, ctr_reg, ctr_slot);
                try self.emitAddImm(ctr_reg, ctr_reg, 1);
                try self.codegen.emitStoreStack(.w64, ctr_slot, ctr_reg);
                self.codegen.freeGeneral(ctr_reg);

                const off_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, off_reg, offset_slot);
                try self.emitAddImm(off_reg, off_reg, @intCast(elem_size));
                try self.codegen.emitStoreStack(.w64, offset_slot, off_reg);
                self.codegen.freeGeneral(off_reg);
            }

            // Jump back to loop start
            const back_patch = try self.codegen.emitJump();
            self.codegen.patchJump(back_patch, loop_start);

            // End: patch exit jumps
            self.codegen.patchJump(exit_patch, self.codegen.currentOffset());
            self.codegen.patchJump(found_patch, self.codegen.currentOffset());

            // Load result from stack
            const res_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadStack(.w64, res_reg, result_slot);
            return .{ .general_reg = res_reg };
        }

        /// Generate list_contains for zero-sized element types: true iff list is non-empty
        fn generateZstListContains(self: *Self, list_loc: ValueLocation) Allocator.Error!ValueLocation {
            const list_base: i32 = switch (list_loc) {
                .stack => |s| s.offset,
                .list_stack => |ls_info| ls_info.struct_offset,
                .immediate_i64 => |val| {
                    if (val != 0) unreachable;
                    return .{ .immediate_i64 = 0 };
                },
                else => unreachable,
            };
            const len_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, len_reg, frame_ptr, list_base + 8);
            const result_reg = try self.allocTempGeneral();
            try self.emitCmpImm(len_reg, 0);
            try self.emitSetCond(result_reg, condNotEqual());
            self.codegen.freeGeneral(len_reg);
            return .{ .general_reg = result_reg };
        }

        /// Generate list_reverse: allocate new list, copy elements in reverse order
        fn generateListReverse(self: *Self, list_loc: ValueLocation, ll: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            const elem_size_align: layout.SizeAlign = blk: {
                const ret_layout = ls.getLayout(ll.ret_layout);
                break :blk switch (ret_layout.tag) {
                    .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                    .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                    else => unreachable,
                };
            };

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const alignment_bytes = elem_size_align.alignment.toByteUnits();

            // Allocate list with same capacity as input length
            const cap_fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_with_capacity);
            const base_reg = frame_ptr;

            // Determine if elements contain refcounted data
            const elements_refcounted: bool = blk: {
                const ret_layout_val = ls.getLayout(ll.ret_layout);
                if (ret_layout_val.tag == .list) {
                    break :blk ls.layoutContainsRefcounted(ls.getLayout(ret_layout_val.data.list));
                }
                break :blk false;
            };

            {
                // roc_builtins_list_with_capacity(out, capacity, alignment, element_width, elements_refcounted, roc_ops)
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off + 8); // capacity = input length
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addImmArg(@intCast(elem_size_align.size));
                try builder.addImmArg(if (elements_refcounted) 1 else 0);
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, cap_fn_addr, .list_with_capacity);
            }

            // Now copy elements in reverse. For each i from 0..len-1, copy src[len-1-i] to dst[i]
            // Use stack slots for loop state
            if (elem_size_align.size == 0) {
                // ZST: just set the length
                const len_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, len_reg, frame_ptr, list_off + 8);
                try self.emitStore(.w64, frame_ptr, result_offset + 8, len_reg);
                self.codegen.freeGeneral(len_reg);
            } else {
                // Save src ptr, src len, dst ptr
                const src_ptr_slot = self.codegen.allocStackSlot(8);
                const src_len_slot = self.codegen.allocStackSlot(8);
                const dst_ptr_slot = self.codegen.allocStackSlot(8);
                const ctr_slot = self.codegen.allocStackSlot(8);

                const tr = try self.allocTempGeneral();
                try self.emitLoad(.w64, tr, frame_ptr, list_off);
                try self.emitStore(.w64, frame_ptr, src_ptr_slot, tr);
                try self.emitLoad(.w64, tr, frame_ptr, list_off + 8);
                try self.emitStore(.w64, frame_ptr, src_len_slot, tr);
                try self.emitLoad(.w64, tr, frame_ptr, result_offset);
                try self.emitStore(.w64, frame_ptr, dst_ptr_slot, tr);
                // Init counter = 0
                try self.codegen.emitLoadImm(tr, 0);
                try self.emitStore(.w64, frame_ptr, ctr_slot, tr);
                self.codegen.freeGeneral(tr);

                // Loop
                const loop_start2 = self.codegen.currentOffset();
                const ci = try self.allocTempGeneral();
                const li = try self.allocTempGeneral();
                try self.emitLoad(.w64, ci, frame_ptr, ctr_slot);
                try self.emitLoad(.w64, li, frame_ptr, src_len_slot);
                try self.codegen.emit.cmpRegReg(.w64, ci, li);
                self.codegen.freeGeneral(li);
                const exit_patch2 = try self.codegen.emitCondJump(condGreaterOrEqual());

                // src_index = len - 1 - i
                const si = try self.allocTempGeneral();
                try self.emitLoad(.w64, si, frame_ptr, src_len_slot);
                try self.emitSubImm(.w64, si, si, 1);
                try self.emitSubRegs(.w64, si, si, ci);

                // Compute src address and dst address
                const elem_sz: i64 = @intCast(elem_size_align.size);
                const esz_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(esz_reg, elem_sz);

                // src_addr = src_ptr + src_index * elem_size
                const src_addr = try self.allocTempGeneral();
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.mulRegRegReg(.w64, src_addr, si, esz_reg);
                } else {
                    try self.codegen.emit.movRegReg(.w64, src_addr, si);
                    try self.codegen.emit.imulRegReg(.w64, src_addr, esz_reg);
                }
                self.codegen.freeGeneral(si);
                const sp_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, sp_reg, frame_ptr, src_ptr_slot);
                try self.emitAddRegs(.w64, src_addr, src_addr, sp_reg);
                self.codegen.freeGeneral(sp_reg);

                // dst_addr = dst_ptr + i * elem_size
                const dst_addr = try self.allocTempGeneral();
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.mulRegRegReg(.w64, dst_addr, ci, esz_reg);
                } else {
                    try self.codegen.emit.movRegReg(.w64, dst_addr, ci);
                    try self.codegen.emit.imulRegReg(.w64, dst_addr, esz_reg);
                }
                self.codegen.freeGeneral(esz_reg);
                const dp_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, dp_reg, frame_ptr, dst_ptr_slot);
                try self.emitAddRegs(.w64, dst_addr, dst_addr, dp_reg);
                self.codegen.freeGeneral(dp_reg);

                // Copy elem_size bytes from src_addr to dst_addr.
                // Use copyChunked which handles non-8-aligned tails correctly,
                // avoiding over-reads past the last element's allocation boundary.
                const copy_tmp = try self.allocTempGeneral();
                try self.copyChunked(copy_tmp, src_addr, 0, dst_addr, 0, elem_size_align.size);
                self.codegen.freeGeneral(copy_tmp);
                self.codegen.freeGeneral(src_addr);
                self.codegen.freeGeneral(dst_addr);

                // Increment counter
                try self.emitAddImm(ci, ci, 1);
                try self.emitStore(.w64, frame_ptr, ctr_slot, ci);
                self.codegen.freeGeneral(ci);

                const back_patch2 = try self.codegen.emitJump();
                self.codegen.patchJump(back_patch2, loop_start2);
                self.codegen.patchJump(exit_patch2, self.codegen.currentOffset());

                // Set result length = src length
                const fl = try self.allocTempGeneral();
                try self.emitLoad(.w64, fl, frame_ptr, src_len_slot);
                try self.emitStore(.w64, frame_ptr, result_offset + 8, fl);
                self.codegen.freeGeneral(fl);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Call list_reserve wrapper
        fn callListReserveOp(self: *Self, list_loc: ValueLocation, spare_loc: ValueLocation, ll: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            const ret_layout = ls.getLayout(ll.ret_layout);
            const elem_size_align: layout.SizeAlign = switch (ret_layout.tag) {
                .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                else => unreachable,
            };
            const elements_refcounted: bool = switch (ret_layout.tag) {
                .list => ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list)),
                else => false,
            };

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const spare_off = try self.ensureOnStack(spare_loc, 8);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const alignment_bytes = elem_size_align.alignment.toByteUnits();
            const fn_addr: usize = @intFromPtr(&wrapListReserve);

            {
                // wrapListReserve(out, list_bytes, list_len, list_cap, alignment, spare, element_width, elements_refcounted, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addMemArg(base_reg, spare_off);
                try builder.addImmArg(@intCast(elem_size_align.size));
                try builder.addImmArg(if (elements_refcounted) @as(usize, 1) else 0);
                try builder.addRegArg(roc_ops_reg);

                try self.callBuiltin(&builder, fn_addr, .list_reserve);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Call list_release_excess_capacity wrapper
        fn callListReleaseExcessCapOp(self: *Self, list_loc: ValueLocation, ll: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            const ret_layout = ls.getLayout(ll.ret_layout);
            const elem_size_align: layout.SizeAlign = switch (ret_layout.tag) {
                .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                else => unreachable,
            };
            const elements_refcounted: bool = switch (ret_layout.tag) {
                .list => ls.layoutContainsRefcounted(ls.getLayout(ret_layout.data.list)),
                else => false,
            };

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const alignment_bytes = elem_size_align.alignment.toByteUnits();
            const fn_addr: usize = @intFromPtr(&wrapListReleaseExcessCapacity);

            {
                // wrapListReleaseExcessCapacity(out, list_bytes, list_len, list_cap, alignment, element_width, elements_refcounted, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addImmArg(@intCast(elem_size_align.size));
                try builder.addImmArg(if (elements_refcounted) @as(usize, 1) else 0);
                try builder.addRegArg(roc_ops_reg);

                try self.callBuiltin(&builder, fn_addr, .list_release_excess_capacity);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Generate code for an i128 literal
        fn generateI128Literal(_: *Self, val: i128) Allocator.Error!ValueLocation {
            // Return as immediate - will be materialized when needed
            return .{ .immediate_i128 = val };
        }

        /// Generate code for a symbol lookup
        fn generateLookup(self: *Self, symbol: Symbol, layout_idx: layout.Idx) Allocator.Error!ValueLocation {
            // Check if we have a location for this symbol
            const symbol_key: u64 = @bitCast(symbol);
            if (self.symbol_locations.get(symbol_key)) |loc| {
                if (loc == .list_stack) {} else if (loc == .stack) {}
                return loc;
            }

            // Symbol not found - it might be a top-level definition
            if (self.store.getSymbolDef(symbol)) |def_expr_id| {
                // Generate code for the definition
                const loc = try self.generateExpr(def_expr_id);
                // Refcounted top-level defs must not be cached as a single shared owner
                // inside the current frame. Each lookup needs its own lifetime so RC
                // insertion can clean it up after the surrounding use.
                const layout_val = self.layout_store.getLayout(layout_idx);
                if (!self.layout_store.layoutContainsRefcounted(layout_val)) {
                    try self.symbol_locations.put(symbol_key, loc);
                }
                return loc;
            }

            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "generateLookup: missing symbol location and symbol def for symbol={d} layout={d} current_proc={d}",
                    .{
                        symbol.raw(),
                        @intFromEnum(layout_idx),
                        if (self.current_proc_name) |sym| sym.raw() else std.math.maxInt(u64),
                    },
                );
            }
            unreachable;
        }

        /// Generate integer binary operation
        fn generateIntBinop(
            self: *Self,
            op: LirExpr.LowLevel,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            operand_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            // Load operands into registers
            const rhs_reg = try self.ensureInGeneralReg(rhs_loc);
            const lhs_reg = try self.ensureInGeneralReg(lhs_loc);

            const narrow_signed_shift: u6 = switch (operand_layout) {
                .i8 => 56,
                .i16 => 48,
                .i32 => 32,
                else => 0,
            };
            if (narrow_signed_shift > 0) {
                try self.emitShlImm(.w64, lhs_reg, lhs_reg, narrow_signed_shift);
                try self.emitAsrImm(.w64, lhs_reg, lhs_reg, narrow_signed_shift);
                try self.emitShlImm(.w64, rhs_reg, rhs_reg, narrow_signed_shift);
                try self.emitAsrImm(.w64, rhs_reg, rhs_reg, narrow_signed_shift);
            }

            // Allocate result register
            const result_reg = try self.allocTempGeneral();

            // Determine if this is an unsigned type (for division/modulo/comparisons)
            const is_unsigned = switch (operand_layout) {
                layout.Idx.u8, layout.Idx.u16, layout.Idx.u32, layout.Idx.u64, layout.Idx.u128 => true,
                else => false,
            };

            switch (op) {
                .num_plus => try self.codegen.emitAdd(.w64, result_reg, lhs_reg, rhs_reg),
                .num_minus => try self.codegen.emitSub(.w64, result_reg, lhs_reg, rhs_reg),
                .num_times => try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg),
                .num_div_by, .num_div_trunc_by => {
                    // For integers, div and div_trunc are the same (integer division truncates)
                    if (is_unsigned) {
                        try self.codegen.emitUDiv(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSDiv(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                },
                .num_rem_by => {
                    if (is_unsigned) {
                        try self.codegen.emitUMod(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSMod(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                },
                .num_mod_by => {
                    if (is_unsigned) {
                        try self.codegen.emitUMod(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        // Mathematical modulo: result has the sign of the divisor.
                        // Start with hardware remainder (sign of dividend), and add
                        // the divisor back only when the remainder is non-zero and
                        // its sign differs from the divisor's sign.
                        const divisor_reg = try self.allocTempGeneral();
                        try self.emitMovRegReg(divisor_reg, rhs_reg);
                        try self.codegen.emitSMod(.w64, result_reg, lhs_reg, rhs_reg);
                        const sign_check_reg = try self.allocTempGeneral();
                        try self.emitCmpImm(result_reg, 0);
                        const zero_patch = try self.emitJumpIfEqual();

                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.eorRegRegReg(.w64, sign_check_reg, result_reg, divisor_reg);
                        } else {
                            try self.emitMovRegReg(sign_check_reg, result_reg);
                            try self.codegen.emit.xorRegReg(.w64, sign_check_reg, divisor_reg);
                        }

                        try self.emitCmpImm(sign_check_reg, 0);
                        const same_sign_patch = try self.codegen.emitCondJump(condGreaterOrEqual());
                        try self.emitAddRegs(.w64, result_reg, result_reg, divisor_reg);
                        const done_patch = try self.codegen.emitJump();

                        const skip_adjust_offset = self.codegen.currentOffset();
                        self.codegen.patchJump(same_sign_patch, skip_adjust_offset);
                        self.codegen.patchJump(zero_patch, skip_adjust_offset);
                        self.codegen.patchJump(done_patch, self.codegen.currentOffset());
                        self.codegen.freeGeneral(sign_check_reg);
                        self.codegen.freeGeneral(divisor_reg);
                    }
                },
                .num_shift_left_by => try self.emitShlReg(.w64, result_reg, lhs_reg, rhs_reg),
                .num_shift_right_by => try self.emitAsrReg(.w64, result_reg, lhs_reg, rhs_reg),
                .num_shift_right_zf_by => try self.emitLsrReg(.w64, result_reg, lhs_reg, rhs_reg),
                // Comparison operations
                .num_is_eq => {
                    try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condEqual());
                },
                .num_is_lt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, if (is_unsigned) condBelow() else condLess()),
                .num_is_lte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, if (is_unsigned) condBelowOrEqual() else condLessOrEqual()),
                .num_is_gt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, if (is_unsigned) condAbove() else condGreater()),
                .num_is_gte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, if (is_unsigned) condAboveOrEqual() else condGreaterOrEqual()),
                else => unreachable,
            }

            // Free operand registers if they were temporary
            self.codegen.freeGeneral(lhs_reg);
            self.codegen.freeGeneral(rhs_reg);

            return .{ .general_reg = result_reg };
        }

        // Condition code helpers for cross-architecture support
        fn condEqual() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .eq else .equal;
        }

        fn condNotEqual() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .ne else .not_equal;
        }

        // Signed condition codes
        fn condLess() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .lt else .less;
        }

        fn condLessOrEqual() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .le else .less_or_equal;
        }

        fn condGreater() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .gt else .greater;
        }

        fn condGreaterOrEqual() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .ge else .greater_or_equal;
        }

        // Unsigned condition codes
        fn condBelow() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .cc else .below;
        }

        fn condBelowOrEqual() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .ls else .below_or_equal;
        }

        fn condAbove() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .hi else .above;
        }

        fn condAboveOrEqual() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .cs else .above_or_equal;
        }

        /// Generate 128-bit integer binary operation
        fn generateI128Binop(
            self: *Self,
            op: LirExpr.LowLevel,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            operand_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            // For 128-bit operations, we work with the values as pairs of 64-bit words
            // Low word at offset 0, high word at offset 8

            // Get low and high parts of both operands
            const signedness: std.builtin.Signedness = if (operand_layout == .u128) .unsigned else .signed;
            const lhs_parts = try self.getI128Parts(lhs_loc, signedness);
            const rhs_parts = try self.getI128Parts(rhs_loc, signedness);

            // Allocate registers for result
            const result_low = try self.allocTempGeneral();
            const result_high = try self.allocTempGeneral();

            const is_unsigned = operand_layout == .u128;

            switch (op) {
                .num_plus => {
                    // 128-bit add: low = lhs_low + rhs_low, high = lhs_high + rhs_high + carry
                    if (comptime target.toCpuArch() == .aarch64) {
                        // ADDS sets carry flag, ADC adds with carry
                        try self.codegen.emit.addsRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                        try self.codegen.emit.adcRegRegReg(.w64, result_high, lhs_parts.high, rhs_parts.high);
                    } else {
                        // x86_64: ADD sets carry, ADC uses it
                        try self.codegen.emit.movRegReg(.w64, result_low, lhs_parts.low);
                        try self.codegen.emit.addRegReg(.w64, result_low, rhs_parts.low);
                        try self.codegen.emit.movRegReg(.w64, result_high, lhs_parts.high);
                        try self.codegen.emit.adcRegReg(.w64, result_high, rhs_parts.high);
                    }
                },
                .num_minus => {
                    // 128-bit sub: low = lhs_low - rhs_low, high = lhs_high - rhs_high - borrow
                    if (comptime target.toCpuArch() == .aarch64) {
                        // SUBS sets borrow flag, SBC subtracts with borrow
                        try self.codegen.emit.subsRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                        try self.codegen.emit.sbcRegRegReg(.w64, result_high, lhs_parts.high, rhs_parts.high);
                    } else {
                        // x86_64: SUB sets borrow, SBB uses it
                        try self.codegen.emit.movRegReg(.w64, result_low, lhs_parts.low);
                        try self.codegen.emit.subRegReg(.w64, result_low, rhs_parts.low);
                        try self.codegen.emit.movRegReg(.w64, result_high, lhs_parts.high);
                        try self.codegen.emit.sbbRegReg(.w64, result_high, rhs_parts.high);
                    }
                },
                .num_times => {
                    if (operand_layout == .dec) {
                        // Dec multiplication: call builtin function
                        // mulSaturatedC(RocDec, RocDec) -> RocDec
                        // RocDec is extern struct { num: i128 }
                        try self.callDecMul(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit multiply: (a_lo, a_hi) * (b_lo, b_hi)
                        // result_lo = low64(a_lo * b_lo)
                        // result_hi = high64(a_lo * b_lo) + low64(a_lo * b_hi) + low64(a_hi * b_lo)

                        if (comptime target.toCpuArch() == .aarch64) {
                            // aarch64: Use MUL for low and UMULH for high
                            // 1. a_lo * b_lo -> result_low (low), temp (high via UMULH)
                            try self.codegen.emit.mulRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                            try self.codegen.emit.umulhRegRegReg(result_high, lhs_parts.low, rhs_parts.low);

                            // 2. a_lo * b_hi -> temp1 (low part only, add to result_high)
                            const temp1 = try self.allocTempGeneral();
                            try self.codegen.emit.mulRegRegReg(.w64, temp1, lhs_parts.low, rhs_parts.high);
                            try self.codegen.emit.addRegRegReg(.w64, result_high, result_high, temp1);
                            self.codegen.freeGeneral(temp1);

                            // 3. a_hi * b_lo -> temp2 (low part only, add to result_high)
                            const temp2 = try self.allocTempGeneral();
                            try self.codegen.emit.mulRegRegReg(.w64, temp2, rhs_parts.low, lhs_parts.high);
                            try self.codegen.emit.addRegRegReg(.w64, result_high, result_high, temp2);
                            self.codegen.freeGeneral(temp2);
                        } else {
                            // x86_64: Use MUL which gives RDX:RAX = RAX * src
                            // MUL clobbers RAX and RDX in each of 3 steps.
                            // We write results directly to result_low/result_high
                            // (no separate temp accumulators) to reduce register pressure.
                            //
                            // result_low/result_high are guaranteed not RAX/RDX because
                            // getI128Parts allocates RAX, RCX, RDX first for the inputs,
                            // so the result registers are always allocated beyond RDX.
                            std.debug.assert(result_low != .RAX and result_low != .RDX);
                            std.debug.assert(result_high != .RAX and result_high != .RDX);

                            // Mark RAX and RDX as in-use so allocGeneralFor won't return them
                            self.codegen.markRegisterInUse(.RAX);
                            self.codegen.markRegisterInUse(.RDX);

                            // Helper to save a register if it's RAX or RDX
                            const SavedReg = struct {
                                reg: GeneralReg,
                                needs_free: bool,
                            };

                            const saveIfClobbered = struct {
                                fn f(s: *Self, reg: GeneralReg, sentinel: u32) !SavedReg {
                                    if (reg == .RAX or reg == .RDX) {
                                        const saved = try s.codegen.allocGeneralFor(sentinel);
                                        try s.codegen.emit.movRegReg(.w64, saved, reg);
                                        return .{ .reg = saved, .needs_free = true };
                                    }
                                    return .{ .reg = reg, .needs_free = false };
                                }
                            }.f;

                            // Save inputs in RAX/RDX (at most 2 of 4 can be)
                            const lhs_low = try saveIfClobbered(self, lhs_parts.low, 0xFFFC);
                            const lhs_high = try saveIfClobbered(self, lhs_parts.high, 0xFFFD);
                            const rhs_low = try saveIfClobbered(self, rhs_parts.low, 0xFFFE);
                            const rhs_high = try saveIfClobbered(self, rhs_parts.high, 0xFFFF);

                            // Restore RAX/RDX to free pool (MUL will use them)
                            self.codegen.freeGeneral(.RAX);
                            self.codegen.freeGeneral(.RDX);

                            // 1. a_lo * b_lo -> result_low, result_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_low.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_low.reg);
                            try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                            try self.codegen.emit.movRegReg(.w64, result_high, .RDX);

                            // 2. a_lo * b_hi -> add low part to result_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_low.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_high.reg);
                            try self.codegen.emit.addRegReg(.w64, result_high, .RAX);

                            // 3. a_hi * b_lo -> add low part to result_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_high.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_low.reg);
                            try self.codegen.emit.addRegReg(.w64, result_high, .RAX);

                            // Cleanup saved registers
                            if (lhs_low.needs_free) self.codegen.freeGeneral(lhs_low.reg);
                            if (lhs_high.needs_free) self.codegen.freeGeneral(lhs_high.reg);
                            if (rhs_low.needs_free) self.codegen.freeGeneral(rhs_low.reg);
                            if (rhs_high.needs_free) self.codegen.freeGeneral(rhs_high.reg);
                        }
                    }
                },
                .num_div_by => {
                    if (operand_layout == .dec) {
                        // Dec division: call builtin function
                        // divC(RocDec, RocDec, *RocOps) -> i128
                        try self.callDecDiv(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit integer division: call builtin function
                        try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, false);
                    }
                },
                .num_div_trunc_by => {
                    if (operand_layout == .dec) {
                        // Dec truncating division: divide and truncate to whole number
                        // divTruncC(RocDec, RocDec, *RocOps) -> i128
                        try self.callDecDivTrunc(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit integer truncating division: same as regular i128 div
                        try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, false);
                    }
                },
                .num_rem_by, .num_mod_by => {
                    // 128-bit integer remainder/modulo: call builtin function
                    try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, true);
                },
                // Comparison operations for i128/Dec
                .num_is_eq => {
                    // Free result_low/result_high first — they're not needed for comparisons,
                    // and freeing them reduces register pressure (avoids spills that corrupt
                    // input part registers on Windows x64 where only 9 regs are available).
                    self.codegen.freeGeneral(result_high);
                    self.codegen.freeGeneral(result_low);

                    const result_reg = try self.allocTempGeneral();
                    try self.generateI128Equality(lhs_parts, rhs_parts, result_reg, true);

                    self.codegen.freeGeneral(lhs_parts.low);
                    self.codegen.freeGeneral(lhs_parts.high);
                    self.codegen.freeGeneral(rhs_parts.low);
                    self.codegen.freeGeneral(rhs_parts.high);

                    return .{ .general_reg = result_reg };
                },
                .num_is_lt, .num_is_lte, .num_is_gt, .num_is_gte => {
                    // Free result_low/result_high first — they're not needed for comparisons,
                    // and freeing them reduces register pressure (avoids spills that corrupt
                    // input part registers on Windows x64 where only 9 regs are available).
                    self.codegen.freeGeneral(result_high);
                    self.codegen.freeGeneral(result_low);

                    const result_reg = try self.allocTempGeneral();
                    try self.generateI128Comparison(lhs_parts, rhs_parts, result_reg, op, is_unsigned);

                    self.codegen.freeGeneral(lhs_parts.low);
                    self.codegen.freeGeneral(lhs_parts.high);
                    self.codegen.freeGeneral(rhs_parts.low);
                    self.codegen.freeGeneral(rhs_parts.high);

                    return .{ .general_reg = result_reg };
                },
                else => {
                    // Boolean ops - use low 64 bits (booleans are 0 or 1)
                    self.codegen.freeGeneral(result_high);
                    self.codegen.freeGeneral(result_low);
                    self.codegen.freeGeneral(lhs_parts.high);
                    self.codegen.freeGeneral(rhs_parts.high);
                    return self.generateIntBinop(op, .{ .general_reg = lhs_parts.low }, .{ .general_reg = rhs_parts.low }, .i64);
                },
            }

            // Free the part registers we loaded
            self.codegen.freeGeneral(lhs_parts.low);
            self.codegen.freeGeneral(lhs_parts.high);
            self.codegen.freeGeneral(rhs_parts.low);
            self.codegen.freeGeneral(rhs_parts.high);

            // Store result to stack and return stack location
            const stack_offset = self.codegen.allocStackSlot(16);
            try self.codegen.emitStoreStack(.w64, stack_offset, result_low);
            try self.codegen.emitStoreStack(.w64, stack_offset + 8, result_high);

            self.codegen.freeGeneral(result_low);
            self.codegen.freeGeneral(result_high);

            return .{ .stack_i128 = stack_offset };
        }

        /// Call a C function: fn(out_low: *u64, out_high: *u64, val: T) -> void.
        /// Takes a scalar value in a general register, returns i128 on stack via output pointers.
        fn callScalarToI128(self: *Self, src_reg: GeneralReg, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const stack_offset = self.codegen.allocStackSlot(16);
            const base_reg = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, stack_offset); // out_low
            try builder.addLeaArg(base_reg, stack_offset + 8); // out_high
            try builder.addRegArg(src_reg); // val
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
            self.codegen.freeGeneral(src_reg);
            return .{ .stack_i128 = stack_offset };
        }

        /// Call a C function: fn(low: u64, high: u64) -> f64.
        /// Takes i128 as two registers, returns f64 in float register.
        fn callI128PartsToF64(self: *Self, parts: I128Parts, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(parts.low);
            try builder.addRegArg(parts.high);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
            self.codegen.freeGeneral(parts.low);
            self.codegen.freeGeneral(parts.high);

            // f64 return value is in the float return register
            const freg = self.codegen.allocFloat() orelse unreachable;
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.fmovRegReg(.double, freg, .V0);
            } else {
                if (freg != .XMM0) {
                    try self.codegen.emit.movsdRegReg(freg, .XMM0);
                }
            }
            return .{ .float_reg = freg };
        }

        /// Call a C function: fn(out_low: *u64, out_high: *u64, val: f64) -> void.
        /// Takes f64 in float register, returns 128-bit value on stack via output pointers.
        fn callF64ToI128(self: *Self, freg: FloatReg, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const stack_offset = self.codegen.allocStackSlot(16);
            const base_reg = frame_ptr;

            // Move f64 to the float arg position before CallBuilder setup.
            // CallBuilder handles int args; float args need manual placement.
            if (comptime target.toCpuArch() == .aarch64) {
                if (freg != .V0) {
                    try self.codegen.emit.fmovRegReg(.double, .V0, freg);
                }
            } else {
                if (self.cc.is_windows) {
                    // Windows: float at position 2 (after 2 int args) goes in XMM2
                    if (freg != .XMM2) {
                        try self.codegen.emit.movsdRegReg(.XMM2, freg);
                    }
                } else {
                    // System V: floats use separate pool, first float arg is XMM0
                    if (freg != .XMM0) {
                        try self.codegen.emit.movsdRegReg(.XMM0, freg);
                    }
                }
            }
            self.codegen.freeFloat(freg);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, stack_offset); // out_low
            try builder.addLeaArg(base_reg, stack_offset + 8); // out_high
            // Float arg already positioned above; CallBuilder only tracks int args
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .stack_i128 = stack_offset };
        }

        // ── Integer try conversion info ──

        const IntTryInfo = struct {
            src_bits: u8,
            src_signed: bool,
            tgt_bits: u8,
            tgt_signed: bool,
        };

        fn intTryConvInfo(op: anytype) IntTryInfo {
            return switch (op) {
                .u8_to_i8_try => .{ .src_bits = 8, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
                .i8_to_u8_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
                .i8_to_u16_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
                .i8_to_u32_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
                .i8_to_u64_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
                .i8_to_u128_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
                .u16_to_i8_try => .{ .src_bits = 16, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
                .u16_to_i16_try => .{ .src_bits = 16, .src_signed = false, .tgt_bits = 16, .tgt_signed = true },
                .u16_to_u8_try => .{ .src_bits = 16, .src_signed = false, .tgt_bits = 8, .tgt_signed = false },
                .i16_to_i8_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 8, .tgt_signed = true },
                .i16_to_u8_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
                .i16_to_u16_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
                .i16_to_u32_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
                .i16_to_u64_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
                .i16_to_u128_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
                .u32_to_i8_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
                .u32_to_i16_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 16, .tgt_signed = true },
                .u32_to_i32_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 32, .tgt_signed = true },
                .u32_to_u8_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 8, .tgt_signed = false },
                .u32_to_u16_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 16, .tgt_signed = false },
                .i32_to_i8_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 8, .tgt_signed = true },
                .i32_to_i16_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 16, .tgt_signed = true },
                .i32_to_u8_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
                .i32_to_u16_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
                .i32_to_u32_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
                .i32_to_u64_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
                .i32_to_u128_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
                .u64_to_i8_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
                .u64_to_i16_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 16, .tgt_signed = true },
                .u64_to_i32_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 32, .tgt_signed = true },
                .u64_to_i64_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 64, .tgt_signed = true },
                .u64_to_u8_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 8, .tgt_signed = false },
                .u64_to_u16_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 16, .tgt_signed = false },
                .u64_to_u32_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 32, .tgt_signed = false },
                .i64_to_i8_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 8, .tgt_signed = true },
                .i64_to_i16_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 16, .tgt_signed = true },
                .i64_to_i32_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 32, .tgt_signed = true },
                .i64_to_u8_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
                .i64_to_u16_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
                .i64_to_u32_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
                .i64_to_u64_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
                .i64_to_u128_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
                .u128_to_i8_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
                .u128_to_i16_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 16, .tgt_signed = true },
                .u128_to_i32_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 32, .tgt_signed = true },
                .u128_to_i64_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 64, .tgt_signed = true },
                .u128_to_i128_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 128, .tgt_signed = true },
                .u128_to_u8_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 8, .tgt_signed = false },
                .u128_to_u16_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 16, .tgt_signed = false },
                .u128_to_u32_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 32, .tgt_signed = false },
                .u128_to_u64_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 64, .tgt_signed = false },
                .i128_to_i8_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 8, .tgt_signed = true },
                .i128_to_i16_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 16, .tgt_signed = true },
                .i128_to_i32_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 32, .tgt_signed = true },
                .i128_to_i64_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 64, .tgt_signed = true },
                .i128_to_u8_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
                .i128_to_u16_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
                .i128_to_u32_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
                .i128_to_u64_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
                .i128_to_u128_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
                else => unreachable,
            };
        }

        /// Generate a checked integer conversion returning Ok(value) | Err(OutOfRange).
        fn generateIntTryConversion(self: *Self, ll: anytype, args: []const LirExprId) Allocator.Error!ValueLocation {
            if (args.len != 1) unreachable;
            const src_loc = try self.generateExpr(args[0]);

            const ls = self.layout_store;
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            std.debug.assert(ret_layout_val.tag == .tag_union);
            const tu_idx = ret_layout_val.data.tag_union.idx;
            const tu_data = ls.getTagUnionData(tu_idx);

            const result_offset = self.codegen.allocStackSlot(tu_data.size);
            try self.zeroStackArea(result_offset, tu_data.size);

            const disc_offset: u32 = tu_data.discriminant_offset;
            const payload_size: u32 = disc_offset; // payload is before discriminant

            const info = intTryConvInfo(ll.op);

            if (info.src_bits > 64) {
                // 128-bit source: load as two registers, call 128-bit C wrapper
                const parts = try self.getI128Parts(src_loc, if (info.src_signed) .signed else .unsigned);

                const builtin_fn: BuiltinFn = if (info.src_signed) .i128_try_convert else .u128_try_convert;
                const fn_addr: usize = if (info.src_signed)
                    @intFromPtr(&dev_wrappers.roc_builtins_i128_try_convert)
                else
                    @intFromPtr(&dev_wrappers.roc_builtins_u128_try_convert);

                const target_bits: u32 = info.tgt_bits;
                const target_is_signed: u32 = if (info.tgt_signed) 1 else 0;
                const base_reg = frame_ptr;

                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addRegArg(parts.low);
                try builder.addRegArg(parts.high);
                try builder.addImmArg(@intCast(target_bits));
                try builder.addImmArg(@intCast(target_is_signed));
                try builder.addImmArg(@intCast(payload_size));
                try builder.addImmArg(@intCast(disc_offset));
                try self.callBuiltin(&builder, fn_addr, builtin_fn);

                self.codegen.freeGeneral(parts.low);
                self.codegen.freeGeneral(parts.high);
            } else {
                // ≤64-bit source: load into register, sign/zero extend, call C wrapper
                const src_reg = try self.ensureInGeneralReg(src_loc);

                // Sign-extend or zero-extend source to fill 64-bit register
                if (info.src_bits < 64) {
                    const shift_amount: u8 = 64 - info.src_bits;
                    if (info.src_signed) {
                        // Sign-extend
                        try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                        try self.emitAsrImm(.w64, src_reg, src_reg, shift_amount);
                    } else {
                        // Zero-extend
                        try self.emitShlImm(.w64, src_reg, src_reg, shift_amount);
                        try self.emitLsrImm(.w64, src_reg, src_reg, shift_amount);
                    }
                }

                if (info.src_signed) {
                    // Signed source: call wrapIntTrySigned(out, val, min_val, max_val, payload_size, disc_offset)
                    const min_val: i64 = if (info.tgt_signed) blk: {
                        if (info.tgt_bits >= 64) break :blk std.math.minInt(i64);
                        const shift: u6 = @intCast(info.tgt_bits - 1);
                        break :blk -(@as(i64, 1) << shift);
                    } else 0;

                    const max_val: i64 = if (info.tgt_signed) blk: {
                        if (info.tgt_bits >= 64) break :blk std.math.maxInt(i64);
                        const shift: u6 = @intCast(info.tgt_bits - 1);
                        break :blk (@as(i64, 1) << shift) - 1;
                    } else blk: {
                        // For unsigned targets >= 64 bits, any non-negative i64 fits
                        if (info.tgt_bits >= 64) break :blk std.math.maxInt(i64);
                        const shift: u6 = @intCast(info.tgt_bits);
                        break :blk (@as(i64, 1) << shift) - 1;
                    };

                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_int_try_signed);
                    const base_reg = frame_ptr;
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addRegArg(src_reg);
                    try builder.addImmArg(@bitCast(min_val));
                    try builder.addImmArg(@bitCast(max_val));
                    try builder.addImmArg(@intCast(payload_size));
                    try builder.addImmArg(@intCast(disc_offset));
                    try self.callBuiltin(&builder, fn_addr, .int_try_signed);
                } else {
                    // Unsigned source: call wrapIntTryUnsigned(out, val, max_val, payload_size, disc_offset)
                    const max_val: u64 = if (info.tgt_signed) blk: {
                        if (info.tgt_bits >= 64) break :blk @as(u64, @bitCast(@as(i64, std.math.maxInt(i64))));
                        const shift: u6 = @intCast(info.tgt_bits - 1);
                        break :blk @as(u64, @intCast((@as(i64, 1) << shift) - 1));
                    } else blk: {
                        if (info.tgt_bits >= 64) break :blk std.math.maxInt(u64);
                        const shift: u6 = @intCast(info.tgt_bits);
                        break :blk (@as(u64, 1) << shift) - 1;
                    };

                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_int_try_unsigned);
                    const base_reg = frame_ptr;
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addRegArg(src_reg);
                    try builder.addImmArg(@bitCast(@as(i64, @bitCast(max_val))));
                    try builder.addImmArg(@intCast(payload_size));
                    try builder.addImmArg(@intCast(disc_offset));
                    try self.callBuiltin(&builder, fn_addr, .int_try_unsigned);
                }

                self.codegen.freeGeneral(src_reg);
            }

            return .{ .stack = .{ .offset = result_offset } };
        }

        /// Generate code for num_from_str: Str -> Result(Num, [InvalidNumStr])
        /// Dispatches to the appropriate C wrapper based on the target numeric type.
        fn generateNumFromStr(self: *Self, ll: anytype, args: []const LirExprId) Allocator.Error!ValueLocation {
            if (args.len != 1) unreachable;
            const str_loc = try self.generateExpr(args[0]);
            const str_off = try self.ensureOnStack(str_loc, roc_str_size);

            const ls = self.layout_store;
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            if (ret_layout_val.tag != .tag_union) {
                std.debug.panic("generateNumFromStr: expected tag_union layout, got {s}", .{@tagName(ret_layout_val.tag)});
            }
            const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
            const result_offset = self.codegen.allocStackSlot(tu_data.size);
            try self.zeroStackArea(result_offset, tu_data.size);
            const disc_offset: u32 = tu_data.discriminant_offset;

            // Find the Ok variant's numeric payload layout. The Err payload can now be a
            // real single-tag union rather than ZST, so we cannot guess by "first non-zst".
            const variants = ls.getTagUnionVariants(tu_data);
            var payload_idx: ?layout.Idx = null;
            for (0..variants.len) |i| {
                const v_payload = variants.get(@intCast(i)).payload_layout;
                const candidate_payload = self.unwrapSingleFieldPayloadLayout(v_payload) orelse v_payload;
                const payload_layout = ls.getLayout(candidate_payload);
                switch (payload_layout.tag) {
                    .scalar => {
                        payload_idx = candidate_payload;
                        break;
                    },
                    else => {},
                }
                if (candidate_payload == .dec) {
                    payload_idx = candidate_payload;
                    break;
                }
            }
            const ok_payload_idx = payload_idx orelse
                std.debug.panic("generateNumFromStr: missing numeric payload in return layout {}", .{@intFromEnum(ll.ret_layout)});

            const base_reg = frame_ptr;

            if (ok_payload_idx == .dec) {
                const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_dec_from_str);
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, str_off);
                try builder.addMemArg(base_reg, str_off + 8);
                try builder.addMemArg(base_reg, str_off + 16);
                try builder.addImmArg(@intCast(disc_offset));
                try self.callBuiltin(&builder, fn_addr, .dec_from_str);
            } else if (ok_payload_idx == .f32 or ok_payload_idx == .f64) {
                const float_width: u8 = if (ok_payload_idx == .f32) 4 else 8;
                const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_float_from_str);
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, str_off);
                try builder.addMemArg(base_reg, str_off + 8);
                try builder.addMemArg(base_reg, str_off + 16);
                try builder.addImmArg(@intCast(float_width));
                try builder.addImmArg(@intCast(disc_offset));
                try self.callBuiltin(&builder, fn_addr, .float_from_str);
            } else {
                const int_width: u8 = switch (ok_payload_idx) {
                    .u8, .i8 => 1,
                    .u16, .i16 => 2,
                    .u32, .i32 => 4,
                    .u64, .i64 => 8,
                    .u128, .i128 => 16,
                    else => unreachable,
                };
                const is_signed: bool = switch (ok_payload_idx) {
                    .i8, .i16, .i32, .i64, .i128 => true,
                    .u8, .u16, .u32, .u64, .u128 => false,
                    else => unreachable,
                };
                const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_int_from_str);
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, str_off);
                try builder.addMemArg(base_reg, str_off + 8);
                try builder.addMemArg(base_reg, str_off + 16);
                try builder.addImmArg(@intCast(int_width));
                try builder.addImmArg(if (is_signed) @as(i64, 1) else @as(i64, 0));
                try builder.addImmArg(@intCast(disc_offset));
                try self.callBuiltin(&builder, fn_addr, .int_from_str);
            }

            return .{ .stack = .{ .offset = result_offset } };
        }

        fn unwrapSingleFieldPayloadLayout(self: *Self, layout_idx: layout.Idx) ?layout.Idx {
            const layout_val = self.layout_store.getLayout(layout_idx);
            if (layout_val.tag != .struct_) return null;

            const struct_data = self.layout_store.getStructData(layout_val.data.struct_.idx);
            const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
            if (fields.len != 1) return null;

            const field = fields.get(0);
            if (field.index != 0) return null;

            if (builtin.mode == .Debug) {
                const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(layout_val.data.struct_.idx, 0);
                std.debug.assert(field_offset == 0);
            }

            return field.layout;
        }

        // ── Float/Dec try_unsafe conversion info ──

        const FloatDecTryUnsafeInfo = struct {
            src_kind: enum { f32, f64, dec },
            tgt_kind: enum { int, f32, dec },
            tgt_bits: u8,
            tgt_signed: bool,
        };

        fn floatDecTryUnsafeInfo(op: anytype) FloatDecTryUnsafeInfo {
            return switch (op) {
                .f32_to_i8_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 8, .tgt_signed = true },
                .f32_to_i16_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 16, .tgt_signed = true },
                .f32_to_i32_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 32, .tgt_signed = true },
                .f32_to_i64_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 64, .tgt_signed = true },
                .f32_to_i128_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 128, .tgt_signed = true },
                .f32_to_u8_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 8, .tgt_signed = false },
                .f32_to_u16_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 16, .tgt_signed = false },
                .f32_to_u32_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 32, .tgt_signed = false },
                .f32_to_u64_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 64, .tgt_signed = false },
                .f32_to_u128_try_unsafe => .{ .src_kind = .f32, .tgt_kind = .int, .tgt_bits = 128, .tgt_signed = false },
                .f64_to_i8_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 8, .tgt_signed = true },
                .f64_to_i16_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 16, .tgt_signed = true },
                .f64_to_i32_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 32, .tgt_signed = true },
                .f64_to_i64_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 64, .tgt_signed = true },
                .f64_to_i128_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 128, .tgt_signed = true },
                .f64_to_u8_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 8, .tgt_signed = false },
                .f64_to_u16_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 16, .tgt_signed = false },
                .f64_to_u32_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 32, .tgt_signed = false },
                .f64_to_u64_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 64, .tgt_signed = false },
                .f64_to_u128_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .int, .tgt_bits = 128, .tgt_signed = false },
                .f64_to_f32_try_unsafe => .{ .src_kind = .f64, .tgt_kind = .f32, .tgt_bits = 32, .tgt_signed = true },
                .dec_to_i8_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 8, .tgt_signed = true },
                .dec_to_i16_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 16, .tgt_signed = true },
                .dec_to_i32_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 32, .tgt_signed = true },
                .dec_to_i64_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 64, .tgt_signed = true },
                .dec_to_i128_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 128, .tgt_signed = true },
                .dec_to_u8_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 8, .tgt_signed = false },
                .dec_to_u16_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 16, .tgt_signed = false },
                .dec_to_u32_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 32, .tgt_signed = false },
                .dec_to_u64_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 64, .tgt_signed = false },
                .dec_to_u128_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .int, .tgt_bits = 128, .tgt_signed = false },
                .dec_to_f32_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .f32, .tgt_bits = 32, .tgt_signed = true },
                .u128_to_dec_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .dec, .tgt_bits = 128, .tgt_signed = false },
                .i128_to_dec_try_unsafe => .{ .src_kind = .dec, .tgt_kind = .dec, .tgt_bits = 128, .tgt_signed = true },
                else => unreachable,
            };
        }

        /// Generate a float/dec try_unsafe conversion returning a record.
        fn generateFloatDecTryUnsafeConversion(self: *Self, ll: anytype, args: []const LirExprId) Allocator.Error!ValueLocation {
            if (args.len != 1) unreachable;
            const src_loc = try self.generateExpr(args[0]);

            const ls = self.layout_store;
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            const size_align = ls.layoutSizeAlign(ret_layout_val);
            const result_offset = self.codegen.allocStackSlot(size_align.size);
            try self.zeroStackArea(result_offset, size_align.size);

            const info = floatDecTryUnsafeInfo(ll.op);

            switch (info.tgt_kind) {
                .int => {
                    // Float/Dec to integer: call wrapper with (out, val, target_bits, target_is_signed, val_size)
                    const val_size: u32 = @as(u32, info.tgt_bits) / 8;
                    const target_bits: u32 = info.tgt_bits;
                    const target_is_signed: u32 = if (info.tgt_signed) 1 else 0;

                    if (info.src_kind == .dec) {
                        // Dec source: get i128 parts, call Dec-to-int wrapper
                        const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128
                        const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_dec_to_int_try_unsafe);
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addRegArg(parts.low);
                        try builder.addRegArg(parts.high);
                        try builder.addImmArg(@intCast(target_bits));
                        try builder.addImmArg(@intCast(target_is_signed));
                        try builder.addImmArg(@intCast(val_size));
                        try self.callBuiltin(&builder, fn_addr, .dec_to_int_try_unsafe);

                        self.codegen.freeGeneral(parts.low);
                        self.codegen.freeGeneral(parts.high);
                    } else {
                        // Float source (f32 or f64): all floats stored as f64 internally
                        const freg = try self.ensureInFloatReg(src_loc);
                        const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_f64_to_int_try_unsafe);
                        const base_reg = frame_ptr;

                        // Position float arg before CallBuilder setup
                        if (comptime target.toCpuArch() == .aarch64) {
                            if (freg != .V0) try self.codegen.emit.fmovRegReg(.double, .V0, freg);
                        } else {
                            if (freg != .XMM0) try self.codegen.emit.movsdRegReg(.XMM0, freg);
                        }

                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        // Float arg already in position; int args follow
                        try builder.addImmArg(@intCast(target_bits));
                        try builder.addImmArg(@intCast(target_is_signed));
                        try builder.addImmArg(@intCast(val_size));
                        try self.callBuiltin(&builder, fn_addr, .f64_to_int_try_unsafe);

                        self.codegen.freeFloat(freg);
                    }
                },
                .f32 => {
                    // Float/Dec narrowing to f32: result is {val: F32, success: Bool}
                    if (info.src_kind == .dec) {
                        // Dec to f32
                        const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128
                        const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_dec_to_f32_try_unsafe);
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addRegArg(parts.low);
                        try builder.addRegArg(parts.high);
                        try self.callBuiltin(&builder, fn_addr, .dec_to_f32_try_unsafe);

                        self.codegen.freeGeneral(parts.low);
                        self.codegen.freeGeneral(parts.high);
                    } else {
                        // f64 to f32
                        const freg = try self.ensureInFloatReg(src_loc);
                        const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_f64_to_f32_try_unsafe);
                        const base_reg = frame_ptr;

                        // Position float arg before CallBuilder
                        if (comptime target.toCpuArch() == .aarch64) {
                            if (freg != .V0) try self.codegen.emit.fmovRegReg(.double, .V0, freg);
                        } else {
                            if (freg != .XMM0) try self.codegen.emit.movsdRegReg(.XMM0, freg);
                        }

                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        // Float arg already in position
                        try self.callBuiltin(&builder, fn_addr, .f64_to_f32_try_unsafe);

                        self.codegen.freeFloat(freg);
                    }
                },
                .dec => {
                    // Integer to Dec: result is {val: Dec(i128), is_int: Bool}
                    const parts = try self.getI128Parts(src_loc, if (info.tgt_signed) .signed else .unsigned);
                    const builtin_fn: BuiltinFn = if (info.tgt_signed) .i128_to_dec_try_unsafe else .u128_to_dec_try_unsafe;
                    const fn_addr: usize = if (info.tgt_signed)
                        @intFromPtr(&dev_wrappers.roc_builtins_i128_to_dec_try_unsafe)
                    else
                        @intFromPtr(&dev_wrappers.roc_builtins_u128_to_dec_try_unsafe);
                    const base_reg = frame_ptr;

                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addRegArg(parts.low);
                    try builder.addRegArg(parts.high);
                    try self.callBuiltin(&builder, fn_addr, builtin_fn);

                    self.codegen.freeGeneral(parts.low);
                    self.codegen.freeGeneral(parts.high);
                },
            }

            return .{ .stack = .{ .offset = result_offset } };
        }

        /// Call Dec multiplication builtin via decomposed wrapper.
        /// Wrapper signature: (out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64) -> void
        fn callDecMul(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Allocator.Error!void {
            const fn_addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_mul_saturated);
            const result_slot = self.codegen.allocStackSlot(16);
            const base_reg = frame_ptr;

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_slot); // out_low
            try builder.addLeaArg(base_reg, result_slot + 8); // out_high
            try builder.addRegArg(lhs_parts.low);
            try builder.addRegArg(lhs_parts.high);
            try builder.addRegArg(rhs_parts.low);
            try builder.addRegArg(rhs_parts.high);
            try self.callBuiltin(&builder, fn_addr, .dec_mul_saturated);

            // Load results from stack slot
            try self.codegen.emitLoadStack(.w64, result_low, result_slot);
            try self.codegen.emitLoadStack(.w64, result_high, result_slot + 8);
        }

        /// Call Dec division builtin via decomposed wrapper.
        /// Wrapper signature: (out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) -> void
        fn callDecDiv(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Allocator.Error!void {
            const fn_addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_div);
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_slot = self.codegen.allocStackSlot(16);
            const base_reg = frame_ptr;

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_slot); // out_low
            try builder.addLeaArg(base_reg, result_slot + 8); // out_high
            try builder.addRegArg(lhs_parts.low);
            try builder.addRegArg(lhs_parts.high);
            try builder.addRegArg(rhs_parts.low);
            try builder.addRegArg(rhs_parts.high);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .dec_div);

            // Load results from stack slot
            try self.codegen.emitLoadStack(.w64, result_low, result_slot);
            try self.codegen.emitLoadStack(.w64, result_high, result_slot + 8);
        }

        /// Call Dec truncating division builtin via decomposed wrapper.
        /// Wrapper signature: (out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) -> void
        fn callDecDivTrunc(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Allocator.Error!void {
            const fn_addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_div_trunc);
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_slot = self.codegen.allocStackSlot(16);
            const base_reg = frame_ptr;

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_slot); // out_low
            try builder.addLeaArg(base_reg, result_slot + 8); // out_high
            try builder.addRegArg(lhs_parts.low);
            try builder.addRegArg(lhs_parts.high);
            try builder.addRegArg(rhs_parts.low);
            try builder.addRegArg(rhs_parts.high);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .dec_div_trunc);

            // Load results from stack slot
            try self.codegen.emitLoadStack(.w64, result_low, result_slot);
            try self.codegen.emitLoadStack(.w64, result_high, result_slot + 8);
        }

        /// Call i128/u128 division or remainder builtin via decomposed wrapper.
        /// Wrapper signature: (out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) -> void
        fn callI128DivRem(
            self: *Self,
            lhs_parts: I128Parts,
            rhs_parts: I128Parts,
            result_low: GeneralReg,
            result_high: GeneralReg,
            is_unsigned: bool,
            is_rem: bool,
        ) Allocator.Error!void {
            const fn_addr: usize = if (is_unsigned)
                if (is_rem) @intFromPtr(&dev_wrappers.roc_builtins_num_rem_trunc_u128) else @intFromPtr(&dev_wrappers.roc_builtins_num_div_trunc_u128)
            else if (is_rem) @intFromPtr(&dev_wrappers.roc_builtins_num_rem_trunc_i128) else @intFromPtr(&dev_wrappers.roc_builtins_num_div_trunc_i128);

            const builtin_fn: BuiltinFn = if (is_unsigned)
                if (is_rem) .num_rem_trunc_u128 else .num_div_trunc_u128
            else if (is_rem) .num_rem_trunc_i128 else .num_div_trunc_i128;

            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_slot = self.codegen.allocStackSlot(16);
            const base_reg = frame_ptr;

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_slot); // out_low
            try builder.addLeaArg(base_reg, result_slot + 8); // out_high
            try builder.addRegArg(lhs_parts.low);
            try builder.addRegArg(lhs_parts.high);
            try builder.addRegArg(rhs_parts.low);
            try builder.addRegArg(rhs_parts.high);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            // Load results from stack slot
            try self.codegen.emitLoadStack(.w64, result_low, result_slot);
            try self.codegen.emitLoadStack(.w64, result_high, result_slot + 8);
        }

        /// Get low and high 64-bit parts of a 128-bit value
        const I128Parts = struct {
            low: GeneralReg,
            high: GeneralReg,
        };

        fn getI128Parts(self: *Self, loc: ValueLocation, signedness: std.builtin.Signedness) Allocator.Error!I128Parts {
            const low_reg = try self.allocTempGeneral();
            const high_reg = try self.allocTempGeneral();

            switch (loc) {
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    try self.codegen.emitLoadImm(low_reg, @bitCast(low));
                    try self.codegen.emitLoadImm(high_reg, @bitCast(high));
                },
                .stack_i128 => |offset| {
                    try self.codegen.emitLoadStack(.w64, low_reg, offset);
                    try self.codegen.emitLoadStack(.w64, high_reg, offset + 8);
                },
                .immediate_i64 => |val| {
                    // Sign-extend to 128 bits
                    try self.codegen.emitLoadImm(low_reg, val);
                    if (val < 0) {
                        try self.codegen.emitLoadImm(high_reg, -1); // All 1s for sign extension
                    } else {
                        try self.codegen.emitLoadImm(high_reg, 0);
                    }
                },
                .general_reg => |reg| {
                    try self.emitMovRegReg(low_reg, reg);
                    try self.emitSignExtendHighReg(high_reg, low_reg, signedness);
                },
                .stack_str => |offset| {
                    try self.codegen.emitLoadStack(.w64, low_reg, offset);
                    try self.emitSignExtendHighReg(high_reg, low_reg, signedness);
                },
                .stack => |s| {
                    try self.codegen.emitLoadStack(.w64, low_reg, s.offset);
                    try self.emitSignExtendHighReg(high_reg, low_reg, signedness);
                },
                else => {
                    unreachable;
                },
            }

            return .{ .low = low_reg, .high = high_reg };
        }

        /// For signed values, arithmetic-shift-right the low register by 63 to
        /// produce all-1s (negative) or all-0s (positive) in the high register.
        /// For unsigned values, simply zero the high register.
        fn emitSignExtendHighReg(self: *Self, high_reg: GeneralReg, low_reg: GeneralReg, signedness: std.builtin.Signedness) !void {
            switch (signedness) {
                .signed => {
                    try self.emitMovRegReg(high_reg, low_reg);
                    try self.emitAsrImm(.w64, high_reg, high_reg, 63);
                },
                .unsigned => {
                    try self.codegen.emitLoadImm(high_reg, 0);
                },
            }
        }

        /// Generate i128 equality comparison (eq or neq)
        /// Compares both low and high parts - equal only if both parts match
        fn generateI128Equality(
            self: *Self,
            lhs_parts: I128Parts,
            rhs_parts: I128Parts,
            result_reg: GeneralReg,
            is_eq: bool,
        ) Allocator.Error!void {
            if (comptime target.toCpuArch() == .aarch64) {
                // Compare low parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.low, rhs_parts.low);
                // Use CSET to get 1 if equal, 0 if not
                try self.codegen.emit.cset(.w64, result_reg, .eq);

                // Compare high parts
                const temp = try self.allocTempGeneral();
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);
                try self.codegen.emit.cset(.w64, temp, .eq);

                // AND the results: both must be equal
                try self.codegen.emit.andRegRegReg(.w64, result_reg, result_reg, temp);
                self.codegen.freeGeneral(temp);

                // For neq, invert the result
                if (!is_eq) {
                    try self.codegen.emit.eorRegRegImm(.w64, result_reg, result_reg, 1);
                }
            } else {
                // x86_64: compare both parts and combine
                // Compare low parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.low, rhs_parts.low);
                // Set result to 1 if equal
                try self.codegen.emitLoadImm(result_reg, 1);
                const zero = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(zero, 0);
                try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero);

                // Compare high parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);
                // If high parts not equal, set to 0
                try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero);

                self.codegen.freeGeneral(zero);

                // For neq, invert the result
                if (!is_eq) {
                    try self.codegen.emit.xorRegImm8(.w64, result_reg, 1);
                }
            }
        }

        /// Generate i128 ordering comparison (lt, lte, gt, gte)
        /// Compares high parts first; if equal, compares low parts
        fn generateI128Comparison(
            self: *Self,
            lhs_parts: I128Parts,
            rhs_parts: I128Parts,
            result_reg: GeneralReg,
            op: LirExpr.LowLevel,
            is_unsigned: bool,
        ) Allocator.Error!void {
            // Strategy: compare high parts (signed for signed, unsigned for unsigned)
            // If high parts are not equal, use that result
            // If high parts are equal, compare low parts (always unsigned since they're magnitudes)

            if (comptime target.toCpuArch() == .aarch64) {
                // Compare high parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);

                // Get signed/unsigned condition for high part
                // aarch64: cc = unsigned <, cs = unsigned >=, hi = unsigned >, ls = unsigned <=
                const high_cond: Condition = switch (op) {
                    .num_is_lt => if (is_unsigned) .cc else .lt,
                    .num_is_lte => if (is_unsigned) .ls else .le,
                    .num_is_gt => if (is_unsigned) .hi else .gt,
                    .num_is_gte => if (is_unsigned) .cs else .ge,
                    else => unreachable,
                };

                // Get unsigned condition for low part (low parts are always unsigned)
                const low_cond: Condition = switch (op) {
                    .num_is_lt => .cc,
                    .num_is_lte => .ls,
                    .num_is_gt => .hi,
                    .num_is_gte => .cs,
                    else => unreachable,
                };

                // Result of comparing high parts (for strict inequality case)
                try self.codegen.emit.cset(.w64, result_reg, high_cond);

                // If high parts are equal, we need to check low parts
                const temp = try self.allocTempGeneral();

                // Compare low parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.low, rhs_parts.low);
                try self.codegen.emit.cset(.w64, temp, low_cond);

                // Check if high parts were equal
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);
                // If high parts equal, use low comparison result
                try self.codegen.emit.csel(.w64, result_reg, temp, result_reg, .eq);

                self.codegen.freeGeneral(temp);
            } else {
                // x86_64 implementation
                // Compare high parts first
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);

                // Prepare constant for cmovcc
                const one_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(one_reg, 1);

                // Get signed/unsigned condition for high part
                const high_true_cond: Condition = switch (op) {
                    .num_is_lt => if (is_unsigned) .below else .less,
                    .num_is_lte => if (is_unsigned) .below_or_equal else .less_or_equal,
                    .num_is_gt => if (is_unsigned) .above else .greater,
                    .num_is_gte => if (is_unsigned) .above_or_equal else .greater_or_equal,
                    else => unreachable,
                };

                // Start with high comparison result
                try self.codegen.emitLoadImm(result_reg, 0);
                try self.codegen.emit.cmovcc(high_true_cond, .w64, result_reg, one_reg);

                // If high parts not equal, we're done - result is set
                // If high parts are equal, need to use low comparison
                // Save high-equal status first

                const temp = try self.allocTempGeneral();

                // Compare high parts again for equality check
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);
                // temp = 1 if high parts equal
                try self.codegen.emitLoadImm(temp, 0);
                try self.codegen.emit.cmovcc(.equal, .w64, temp, one_reg);

                // Now compare low parts (unsigned since they're magnitudes)
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.low, rhs_parts.low);

                const low_true_cond: Condition = switch (op) {
                    .num_is_lt => .below,
                    .num_is_lte => .below_or_equal,
                    .num_is_gt => .above,
                    .num_is_gte => .above_or_equal,
                    else => unreachable,
                };

                // Get low comparison result
                const low_result = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(low_result, 0);
                try self.codegen.emit.cmovcc(low_true_cond, .w64, low_result, one_reg);

                // If high parts were equal, use low result instead
                // test temp, temp; if temp != 0 (high parts equal), use low_result
                try self.codegen.emit.testRegReg(.w64, temp, temp);
                try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, low_result);

                self.codegen.freeGeneral(temp);
                self.codegen.freeGeneral(low_result);
                self.codegen.freeGeneral(one_reg);
            }
        }

        /// Generate tag union comparison using layout information.
        /// Compares discriminant first, then payload bytes for the full
        /// max payload size. This is correct when all variants have the
        /// same payload size, or when unused payload bytes are zeroed.
        /// Generate tag union comparison using layout information.
        /// Compares discriminants first, then payload using layout-aware comparison.
        fn generateTagUnionComparisonByLayout(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            tu_layout_idx: layout.Idx,
            op: anytype,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const stored_layout = ls.getLayout(tu_layout_idx);
            if (stored_layout.tag != .tag_union) unreachable;

            const tu_idx = stored_layout.data.tag_union.idx;
            const tu_data = ls.getTagUnionData(tu_idx);
            const total_size = tu_data.size;

            if (total_size == 0) {
                return .{ .immediate_i64 = if (op == .num_is_eq) 1 else 0 };
            }

            const lhs_base = try self.ensureRecordOnStack(lhs_loc, total_size);
            const rhs_base = try self.ensureRecordOnStack(rhs_loc, total_size);

            // Check if any variant contains refcounted data (strings, lists, etc.)
            const tu_info = ls.getTagUnionInfo(stored_layout);
            if (!tu_info.contains_refcounted) {
                // Fast path: no heap types, raw byte comparison is correct
                return self.generateTagUnionBytewiseComparison(lhs_base, rhs_base, total_size, op);
            }

            // Slow path: compare discriminants first, then dispatch payload comparison
            const result_reg = try self.allocTempGeneral();
            const disc_offset: i32 = @intCast(tu_data.discriminant_offset);
            const disc_size = tu_data.discriminant_size;

            // Load discriminants
            // Use .w32 when .w64 would read past the tag union boundary.
            const disc_use_w32 = (disc_offset + 8 > @as(i32, @intCast(total_size)));
            const lhs_disc = try self.allocTempGeneral();
            const rhs_disc = try self.allocTempGeneral();
            if (disc_size == 0) {
                try self.codegen.emitLoadImm(lhs_disc, 0);
                try self.codegen.emitLoadImm(rhs_disc, 0);
            } else if (disc_use_w32) {
                try self.codegen.emitLoadStack(.w32, lhs_disc, lhs_base + disc_offset);
                try self.codegen.emitLoadStack(.w32, rhs_disc, rhs_base + disc_offset);
            } else {
                try self.codegen.emitLoadStack(.w64, lhs_disc, lhs_base + disc_offset);
                try self.codegen.emitLoadStack(.w64, rhs_disc, rhs_base + disc_offset);
            }

            // Mask discriminants to their actual size
            if (disc_size != 0 and disc_size < 8) {
                const disc_mask: u64 = (@as(u64, 1) << @intCast(disc_size * 8)) - 1;
                const disc_mask_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(disc_mask_reg, @bitCast(disc_mask));
                try self.emitAndRegs(.w64, lhs_disc, lhs_disc, disc_mask_reg);
                try self.emitAndRegs(.w64, rhs_disc, rhs_disc, disc_mask_reg);
                self.codegen.freeGeneral(disc_mask_reg);
            }

            // Compare discriminants
            try self.emitCmpReg(lhs_disc, rhs_disc);
            self.codegen.freeGeneral(rhs_disc);

            // If discriminants differ, result is 0 (not equal)
            try self.codegen.emitLoadImm(result_reg, 0);
            const disc_ne_patch = try self.emitJumpIfNotEqual();

            // Discriminants are equal - compare payload by variant
            const variants = ls.getTagUnionVariants(tu_data);
            const variant_count = variants.len;

            if (variant_count == 1) {
                // Only one variant: compare its payload directly
                self.codegen.freeGeneral(lhs_disc);
                const payload_layout_idx = variants.get(0).payload_layout;
                const payload_layout = ls.getLayout(payload_layout_idx);
                const payload_size = ls.layoutSizeAlign(payload_layout).size;
                if (payload_size > 0) {
                    try self.compareFieldByLayout(lhs_base, rhs_base, payload_layout_idx, payload_size, result_reg);
                } else {
                    try self.codegen.emitLoadImm(result_reg, 1);
                }
            } else {
                // Multiple variants: dispatch based on discriminant value
                try self.codegen.emitLoadImm(result_reg, 1); // default for ZST payloads

                // Spill lhs_disc to stack before variant loop since
                // compareFieldByLayout may call C builtins that clobber caller-saved registers.
                const disc_slot = self.codegen.allocStackSlot(8);
                try self.codegen.emitStoreStack(.w64, disc_slot, lhs_disc);
                self.codegen.freeGeneral(lhs_disc);

                var end_patches: [64]usize = undefined;
                var end_patch_count: u32 = 0;
                var variant_i: u32 = 0;
                while (variant_i < variant_count) : (variant_i += 1) {
                    const payload_layout_idx = variants.get(variant_i).payload_layout;
                    const payload_layout = ls.getLayout(payload_layout_idx);
                    const payload_size = ls.layoutSizeAlign(payload_layout).size;

                    if (payload_size == 0) {
                        // ZST payload: always equal (result already 1)
                        continue;
                    }

                    // Reload disc from stack (may have been clobbered by previous iteration)
                    const disc_temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, disc_temp, disc_slot);
                    try self.emitCmpImm(disc_temp, @intCast(variant_i));
                    self.codegen.freeGeneral(disc_temp);
                    const skip_patch = try self.emitJumpIfNotEqual();

                    // Compare payload for this variant
                    try self.compareFieldByLayout(lhs_base, rhs_base, payload_layout_idx, payload_size, result_reg);

                    // Jump to end
                    std.debug.assert(end_patch_count < end_patches.len);
                    end_patches[end_patch_count] = try self.codegen.emitJump();
                    end_patch_count += 1;

                    // Patch skip to here
                    self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
                }

                // Patch all end jumps to here
                const current = self.codegen.currentOffset();
                for (end_patches[0..end_patch_count]) |patch| {
                    self.codegen.patchJump(patch, current);
                }
            }

            // Patch discriminant-not-equal jump to here
            const done_offset = self.codegen.currentOffset();
            self.codegen.patchJump(disc_ne_patch, done_offset);

            if (op != .num_is_eq) {
                try self.emitXorImm(.w64, result_reg, result_reg, 1);
            }

            return .{ .general_reg = result_reg };
        }

        /// Fast bytewise tag union comparison when no variants contain heap types.
        fn generateTagUnionBytewiseComparison(
            self: *Self,
            lhs_base: i32,
            rhs_base: i32,
            total_size: u32,
            op: anytype,
        ) Allocator.Error!ValueLocation {
            const result_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(result_reg, 1);

            const tmp_a = try self.allocTempGeneral();
            const tmp_b = try self.allocTempGeneral();

            var cmp_off: u32 = 0;
            while (cmp_off < total_size) {
                const lhs_off = lhs_base + @as(i32, @intCast(cmp_off));
                const rhs_off = rhs_base + @as(i32, @intCast(cmp_off));

                try self.codegen.emitLoadStack(.w64, tmp_a, lhs_off);
                try self.codegen.emitLoadStack(.w64, tmp_b, rhs_off);

                const remaining = total_size - cmp_off;
                if (remaining < 8) {
                    const mask: u64 = (@as(u64, 1) << @intCast(remaining * 8)) - 1;
                    const mask_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(mask_reg, @bitCast(mask));
                    try self.emitAndRegs(.w64, tmp_a, tmp_a, mask_reg);
                    try self.emitAndRegs(.w64, tmp_b, tmp_b, mask_reg);
                    self.codegen.freeGeneral(mask_reg);
                }

                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.cmp(.w64, tmp_a, tmp_b);
                    try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                } else {
                    try self.codegen.emit.cmpRegReg(.w64, tmp_a, tmp_b);
                    const zero_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(zero_reg, 0);
                    try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg);
                    self.codegen.freeGeneral(zero_reg);
                }

                cmp_off += 8;
            }

            self.codegen.freeGeneral(tmp_a);
            self.codegen.freeGeneral(tmp_b);

            if (op != .num_is_eq) {
                try self.emitXorImm(.w64, result_reg, result_reg, 1);
            }

            return .{ .general_reg = result_reg };
        }

        /// for each field type (i128 for Dec fields, i64 for smaller fields, etc.)
        /// Compare a single field/element by its layout type, writing 1 (equal) or 0 (not equal)
        /// into result_reg. Dispatches on layout type rather than byte size to correctly
        /// handle heap types (strings, lists) that need content comparison.
        fn compareFieldByLayout(
            self: *Self,
            lhs_off: i32,
            rhs_off: i32,
            field_layout_idx: layout.Idx,
            field_size: u32,
            result_reg: GeneralReg,
        ) Allocator.Error!void {
            const ls = self.layout_store;

            // Check layout tag first for compound types that need recursive comparison,
            // before falling into scalar size-based paths.
            const field_layout = ls.getLayout(field_layout_idx);
            if (field_layout.tag == .struct_) {
                const sub_loc = try self.generateStructComparisonByLayout(
                    .{ .stack = .{ .offset = lhs_off } },
                    .{ .stack = .{ .offset = rhs_off } },
                    field_layout_idx,
                    .num_is_eq,
                );
                const sub_reg = try self.ensureInGeneralReg(sub_loc);
                try self.emitMovRegReg(result_reg, sub_reg);
                self.codegen.freeGeneral(sub_reg);
            } else if (field_layout_idx == .str) {
                // String: compare by content using strEqual builtin
                const eq_loc = try self.callStr2ToScalar(lhs_off, rhs_off, @intFromPtr(&wrapStrEqual), .str_equal);
                const eq_reg = try self.ensureInGeneralReg(eq_loc);
                try self.emitMovRegReg(result_reg, eq_reg);
                self.codegen.freeGeneral(eq_reg);
            } else if (field_layout_idx == .dec or field_layout_idx == .i128 or field_layout_idx == .u128) {
                // 128-bit scalar: compare as i128 (two 64-bit parts)
                const lhs_parts = try self.getI128Parts(.{ .stack_i128 = lhs_off }, .signed);
                const rhs_parts = try self.getI128Parts(.{ .stack_i128 = rhs_off }, .signed);
                try self.generateI128Equality(lhs_parts, rhs_parts, result_reg, true);
                self.codegen.freeGeneral(lhs_parts.low);
                self.codegen.freeGeneral(lhs_parts.high);
                self.codegen.freeGeneral(rhs_parts.low);
                self.codegen.freeGeneral(rhs_parts.high);
            } else if (field_layout.tag == .list) {
                const sub_loc = try self.generateListComparisonByLayout(
                    .{ .stack = .{ .offset = lhs_off } },
                    .{ .stack = .{ .offset = rhs_off } },
                    field_layout_idx,
                    .num_is_eq,
                );
                const sub_reg = try self.ensureInGeneralReg(sub_loc);
                try self.emitMovRegReg(result_reg, sub_reg);
                self.codegen.freeGeneral(sub_reg);
            } else if (field_size <= 8) {
                // Small field: compare as single register value
                const lhs_reg = try self.allocTempGeneral();
                const rhs_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, lhs_reg, lhs_off);
                try self.codegen.emitLoadStack(.w64, rhs_reg, rhs_off);

                if (field_size < 8) {
                    const mask: u64 = (@as(u64, 1) << @intCast(field_size * 8)) - 1;
                    const mask_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(mask_reg, @bitCast(mask));
                    try self.emitAndRegs(.w64, lhs_reg, lhs_reg, mask_reg);
                    try self.emitAndRegs(.w64, rhs_reg, rhs_reg, mask_reg);
                    self.codegen.freeGeneral(mask_reg);
                }

                try self.emitCmpReg(lhs_reg, rhs_reg);
                try self.emitSetCond(result_reg, condEqual());
                self.codegen.freeGeneral(lhs_reg);
                self.codegen.freeGeneral(rhs_reg);
            } else if (field_layout.tag == .tag_union) {
                const sub_loc = try self.generateTagUnionComparisonByLayout(
                    .{ .stack = .{ .offset = lhs_off } },
                    .{ .stack = .{ .offset = rhs_off } },
                    field_layout_idx,
                    .num_is_eq,
                );
                const sub_reg = try self.ensureInGeneralReg(sub_loc);
                try self.emitMovRegReg(result_reg, sub_reg);
                self.codegen.freeGeneral(sub_reg);
            } else {
                // Fallback: XOR-based byte comparison for other multi-byte fields
                const tmp_a = try self.allocTempGeneral();
                const tmp_b = try self.allocTempGeneral();
                const xor_acc = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(xor_acc, 0);

                var cmp_off: u32 = 0;
                while (cmp_off < field_size) {
                    try self.codegen.emitLoadStack(.w64, tmp_a, lhs_off + @as(i32, @intCast(cmp_off)));
                    try self.codegen.emitLoadStack(.w64, tmp_b, rhs_off + @as(i32, @intCast(cmp_off)));
                    const remaining = field_size - cmp_off;
                    if (remaining < 8) {
                        const mask: u64 = (@as(u64, 1) << @intCast(remaining * 8)) - 1;
                        const mask_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(mask_reg, @bitCast(mask));
                        try self.emitAndRegs(.w64, tmp_a, tmp_a, mask_reg);
                        try self.emitAndRegs(.w64, tmp_b, tmp_b, mask_reg);
                        self.codegen.freeGeneral(mask_reg);
                    }
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.eorRegRegReg(.w64, tmp_a, tmp_a, tmp_b);
                        try self.codegen.emit.orrRegRegReg(.w64, xor_acc, xor_acc, tmp_a);
                    } else {
                        try self.codegen.emit.xorRegReg(.w64, tmp_a, tmp_b);
                        try self.codegen.emit.orRegReg(.w64, xor_acc, tmp_a);
                    }
                    cmp_off += 8;
                }

                try self.emitCmpImm(xor_acc, 0);
                try self.emitSetCond(result_reg, condEqual());
                self.codegen.freeGeneral(tmp_a);
                self.codegen.freeGeneral(tmp_b);
                self.codegen.freeGeneral(xor_acc);
            }
        }

        fn generateStructComparisonByLayout(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            struct_layout_idx: layout.Idx,
            op: anytype,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const stored_layout = ls.getLayout(struct_layout_idx);
            // Empty structs (ZST) have scalar layout, not struct_ — they're always equal
            if (stored_layout.tag != .struct_) {
                return .{ .immediate_i64 = if (op == .num_is_eq) 1 else 0 };
            }

            const struct_idx = stored_layout.data.struct_.idx;
            const struct_data = ls.getStructData(struct_idx);
            const field_count = struct_data.fields.count;
            if (field_count == 0) {
                return .{ .immediate_i64 = if (op == .num_is_eq) 1 else 0 };
            }

            const lhs_base = try self.ensureRecordOnStack(lhs_loc, ls.layoutSizeAlign(stored_layout).size);
            const rhs_base = try self.ensureRecordOnStack(rhs_loc, ls.layoutSizeAlign(stored_layout).size);

            // Use stack-based accumulator since compareFieldByLayout may call builtins
            // that clobber caller-saved registers.
            const result_slot = self.codegen.allocStackSlot(8);
            {
                const temp = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(temp, 1);
                try self.codegen.emitStoreStack(.w64, result_slot, temp);
                self.codegen.freeGeneral(temp);
            }

            var field_i: u32 = 0;
            while (field_i < field_count) : (field_i += 1) {
                const field_offset = ls.getStructFieldOffset(struct_idx, @intCast(field_i));
                const field_size = ls.getStructFieldSize(struct_idx, @intCast(field_i));
                const field_layout_idx = ls.getStructFieldLayout(struct_idx, @intCast(field_i));

                if (field_size == 0) continue;

                const lhs_field_off = lhs_base + @as(i32, @intCast(field_offset));
                const rhs_field_off = rhs_base + @as(i32, @intCast(field_offset));

                const field_eq_reg = try self.allocTempGeneral();
                try self.compareFieldByLayout(lhs_field_off, rhs_field_off, field_layout_idx, field_size, field_eq_reg);

                // AND field result into accumulator: load from stack, AND, store back
                const acc_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, acc_reg, result_slot);
                try self.emitAndRegs(.w64, acc_reg, acc_reg, field_eq_reg);
                try self.codegen.emitStoreStack(.w64, result_slot, acc_reg);
                self.codegen.freeGeneral(acc_reg);
                self.codegen.freeGeneral(field_eq_reg);
            }

            // Load final result from stack into register
            const result_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadStack(.w64, result_reg, result_slot);

            if (op != .num_is_eq) {
                const one_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(one_reg, 1);
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.eorRegRegReg(.w64, result_reg, result_reg, one_reg);
                } else {
                    try self.codegen.emit.xorRegReg(.w64, result_reg, one_reg);
                }
                self.codegen.freeGeneral(one_reg);
            }

            return .{ .general_reg = result_reg };
        }

        /// Ensure a record value is on the stack, returning its base offset.
        fn ensureRecordOnStack(self: *Self, loc: ValueLocation, record_size: u32) Allocator.Error!i32 {
            return switch (loc) {
                .stack_str, .stack_i128 => |off| off,
                .stack => |s| s.offset,
                .general_reg => |reg| blk: {
                    const slot = self.codegen.allocStackSlot(@intCast(record_size));
                    try self.emitStore(.w64, frame_ptr, slot, reg);
                    break :blk slot;
                },
                .immediate_i64 => |val| blk: {
                    const slot = self.codegen.allocStackSlot(@intCast(record_size));
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, val);
                    try self.emitStore(.w64, frame_ptr, slot, temp);
                    self.codegen.freeGeneral(temp);
                    break :blk slot;
                },
                else => unreachable,
            };
        }

        /// Generate list comparison using layout information
        /// Generate list comparison using layout information.
        /// Compares list lengths at runtime, then compares elements one by one.
        fn generateListComparisonByLayout(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            list_layout_idx: layout.Idx,
            op: anytype,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const list_layout = ls.getLayout(list_layout_idx);
            const elem_layout_idx: layout.Idx = list_layout.data.list;
            const elem_layout = ls.getLayout(elem_layout_idx);
            const elem_sa = ls.layoutSizeAlign(elem_layout);
            const elem_size: u32 = elem_sa.size;

            // Get list struct offsets (ptr at +0, len at +8)
            const lhs_base: i32 = switch (lhs_loc) {
                .stack => |s| s.offset,
                .list_stack => |li| li.struct_offset,
                else => unreachable,
            };
            const rhs_base: i32 = switch (rhs_loc) {
                .stack => |s| s.offset,
                .list_stack => |li| li.struct_offset,
                else => unreachable,
            };

            const result_reg = try self.allocTempGeneral();

            // Load lengths
            const lhs_len = try self.allocTempGeneral();
            const rhs_len = try self.allocTempGeneral();
            try self.emitLoad(.w64, lhs_len, frame_ptr, lhs_base + 8);
            try self.emitLoad(.w64, rhs_len, frame_ptr, rhs_base + 8);

            // Compare lengths: if different, not equal
            try self.emitCmpReg(lhs_len, rhs_len);
            self.codegen.freeGeneral(rhs_len);

            // If lengths differ, result = 0 and jump to end
            try self.codegen.emitLoadImm(result_reg, 0);
            const len_ne_patch = try self.codegen.emitCondJump(condNotEqual());

            // Lengths are equal — compare elements if len > 0
            // Start with result = 1 (equal)
            try self.codegen.emitLoadImm(result_reg, 1);

            // Spill len to stack before checking for empty
            const len_slot = self.codegen.allocStackSlot(8);
            try self.codegen.emitStoreStack(.w64, len_slot, lhs_len);

            // If len == 0, skip the loop
            try self.emitCmpImm(lhs_len, 0);
            self.codegen.freeGeneral(lhs_len);
            const empty_patch = try self.codegen.emitCondJump(condEqual());

            const lhs_ptr_slot = self.codegen.allocStackSlot(8);
            const rhs_ptr_slot = self.codegen.allocStackSlot(8);
            {
                const tmp = try self.allocTempGeneral();
                try self.emitLoad(.w64, tmp, frame_ptr, lhs_base);
                try self.codegen.emitStoreStack(.w64, lhs_ptr_slot, tmp);
                try self.emitLoad(.w64, tmp, frame_ptr, rhs_base);
                try self.codegen.emitStoreStack(.w64, rhs_ptr_slot, tmp);
                self.codegen.freeGeneral(tmp);
            }

            // Counter and byte offset on stack
            const ctr_slot = self.codegen.allocStackSlot(8);
            const offset_slot = self.codegen.allocStackSlot(8);
            // Allocate stack slots for element copies (aligned to 8 for copy loop)
            const aligned_elem = @as(u32, @intCast(std.mem.alignForward(u32, elem_size, 8)));
            const lhs_elem_slot = self.codegen.allocStackSlot(@intCast(aligned_elem));
            const rhs_elem_slot = self.codegen.allocStackSlot(@intCast(aligned_elem));

            {
                const tmp = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(tmp, 0);
                try self.codegen.emitStoreStack(.w64, ctr_slot, tmp);
                try self.codegen.emitStoreStack(.w64, offset_slot, tmp);
                self.codegen.freeGeneral(tmp);
            }

            // Loop start
            const loop_start = self.codegen.currentOffset();

            // if ctr >= len, done (all elements matched)
            {
                const ctr_reg = try self.allocTempGeneral();
                const len_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, ctr_reg, ctr_slot);
                try self.codegen.emitLoadStack(.w64, len_reg, len_slot);
                try self.codegen.emit.cmpRegReg(.w64, ctr_reg, len_reg);
                self.codegen.freeGeneral(ctr_reg);
                self.codegen.freeGeneral(len_reg);
            }
            const exit_patch = try self.codegen.emitCondJump(condGreaterOrEqual());

            // Copy lhs element from heap to lhs_elem_slot
            {
                const ptr_reg = try self.allocTempGeneral();
                const off_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, ptr_reg, lhs_ptr_slot);
                try self.codegen.emitLoadStack(.w64, off_reg, offset_slot);
                try self.emitAddRegs(.w64, ptr_reg, ptr_reg, off_reg);
                self.codegen.freeGeneral(off_reg);
                const tmp = try self.allocTempGeneral();
                try self.copyChunked(tmp, ptr_reg, 0, frame_ptr, lhs_elem_slot, elem_size);
                self.codegen.freeGeneral(tmp);
                self.codegen.freeGeneral(ptr_reg);
            }

            // Copy rhs element from heap to rhs_elem_slot
            {
                const ptr_reg = try self.allocTempGeneral();
                const off_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, ptr_reg, rhs_ptr_slot);
                try self.codegen.emitLoadStack(.w64, off_reg, offset_slot);
                try self.emitAddRegs(.w64, ptr_reg, ptr_reg, off_reg);
                self.codegen.freeGeneral(off_reg);
                const tmp = try self.allocTempGeneral();
                try self.copyChunked(tmp, ptr_reg, 0, frame_ptr, rhs_elem_slot, elem_size);
                self.codegen.freeGeneral(tmp);
                self.codegen.freeGeneral(ptr_reg);
            }

            // Compare elements using layout-aware comparison
            const eq_reg = try self.allocTempGeneral();
            try self.compareFieldByLayout(lhs_elem_slot, rhs_elem_slot, elem_layout_idx, elem_size, eq_reg);

            // If not equal, set result = 0 and jump to end
            try self.emitCmpImm(eq_reg, 1);
            self.codegen.freeGeneral(eq_reg);
            const ne_patch = try self.codegen.emitCondJump(condNotEqual());
            // not_equal: result = 0, break
            // (we only get here if elements are equal, so fall through)

            // Increment counter and byte offset
            {
                const ctr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, ctr_reg, ctr_slot);
                try self.emitAddImm(ctr_reg, ctr_reg, 1);
                try self.codegen.emitStoreStack(.w64, ctr_slot, ctr_reg);
                self.codegen.freeGeneral(ctr_reg);

                const off_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, off_reg, offset_slot);
                try self.emitAddImm(off_reg, off_reg, @intCast(elem_size));
                try self.codegen.emitStoreStack(.w64, offset_slot, off_reg);
                self.codegen.freeGeneral(off_reg);
            }

            // Jump back to loop start
            const back_patch = try self.codegen.emitJump();
            self.codegen.patchJump(back_patch, loop_start);

            // Not equal exit: set result = 0
            self.codegen.patchJump(ne_patch, self.codegen.currentOffset());
            try self.codegen.emitLoadImm(result_reg, 0);
            // Fall through to end (result_reg already set to 0)
            const ne_done_patch = try self.codegen.emitJump();

            // All-equal exit: result stays 1
            self.codegen.patchJump(exit_patch, self.codegen.currentOffset());

            // Length-not-equal and empty-list exits
            self.codegen.patchJump(ne_done_patch, self.codegen.currentOffset());
            self.codegen.patchJump(len_ne_patch, self.codegen.currentOffset());
            self.codegen.patchJump(empty_patch, self.codegen.currentOffset());

            if (op != .num_is_eq) {
                try self.emitXorImm(.w64, result_reg, result_reg, 1);
            }

            return .{ .general_reg = result_reg };
        }

        /// Generate floating-point binary operation
        fn generateFloatBinop(
            self: *Self,
            op: LirExpr.LowLevel,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
        ) Allocator.Error!ValueLocation {
            // Load LHS into a float register
            const lhs_reg = try self.ensureInFloatReg(lhs_loc);

            // Load RHS into a float register
            const rhs_reg = try self.ensureInFloatReg(rhs_loc);

            // Comparisons produce integer results (0 or 1), not floats.
            // On x86_64, UCOMISD sets PF=1 for NaN (unordered), so eq/neq/lt/lte
            // need special handling to produce correct results when either operand is NaN.
            if (comptime target.toCpuArch() == .x86_64) {
                switch (op) {
                    .num_is_eq => {
                        // NaN-safe eq: (ZF=1) AND (PF=0) → sete + setnp + and
                        const result_reg = try self.codegen.allocGeneralFor(0);
                        const tmp_reg = try self.codegen.allocGeneralFor(0);
                        try self.codegen.emit.ucomisdRegReg(lhs_reg, rhs_reg);
                        try self.codegen.emit.setcc(.equal, result_reg);
                        try self.codegen.emit.setcc(.parity_odd, tmp_reg);
                        try self.codegen.emit.andRegReg(.w64, result_reg, tmp_reg);
                        try self.codegen.emit.andRegImm8(result_reg, 1);
                        self.codegen.freeGeneral(tmp_reg);
                        self.codegen.freeFloat(lhs_reg);
                        self.codegen.freeFloat(rhs_reg);
                        return .{ .general_reg = result_reg };
                    },
                    .num_is_lt => {
                        // NaN-safe lt: swap operands, use "above" (CF=0 AND ZF=0)
                        const result_reg = try self.codegen.allocGeneralFor(0);
                        try self.codegen.emitCmpF64(result_reg, rhs_reg, lhs_reg, .above);
                        self.codegen.freeFloat(lhs_reg);
                        self.codegen.freeFloat(rhs_reg);
                        return .{ .general_reg = result_reg };
                    },
                    .num_is_lte => {
                        // NaN-safe lte: swap operands, use "above_or_equal" (CF=0)
                        const result_reg = try self.codegen.allocGeneralFor(0);
                        try self.codegen.emitCmpF64(result_reg, rhs_reg, lhs_reg, .above_or_equal);
                        self.codegen.freeFloat(lhs_reg);
                        self.codegen.freeFloat(rhs_reg);
                        return .{ .general_reg = result_reg };
                    },
                    .num_is_gt, .num_is_gte => {
                        // gt/gte are already NaN-safe on x86_64 (above/above_or_equal return false for NaN)
                        const float_cond = floatCondition(op).?;
                        const result_reg = try self.codegen.allocGeneralFor(0);
                        try self.codegen.emitCmpF64(result_reg, lhs_reg, rhs_reg, float_cond);
                        self.codegen.freeFloat(lhs_reg);
                        self.codegen.freeFloat(rhs_reg);
                        return .{ .general_reg = result_reg };
                    },
                    else => {},
                }
            } else {
                const float_cond = floatCondition(op);
                if (float_cond) |cond| {
                    const result_reg = try self.codegen.allocGeneralFor(0);
                    try self.codegen.emitCmpF64(result_reg, lhs_reg, rhs_reg, cond);
                    self.codegen.freeFloat(lhs_reg);
                    self.codegen.freeFloat(rhs_reg);
                    return .{ .general_reg = result_reg };
                }
            }

            // Arithmetic operations produce float results
            const result_reg = try self.codegen.allocFloatFor(0);

            switch (op) {
                .num_plus => try self.codegen.emitAddF64(result_reg, lhs_reg, rhs_reg),
                .num_minus => try self.codegen.emitSubF64(result_reg, lhs_reg, rhs_reg),
                .num_times => try self.codegen.emitMulF64(result_reg, lhs_reg, rhs_reg),
                .num_div_by, .num_div_trunc_by => try self.codegen.emitDivF64(result_reg, lhs_reg, rhs_reg),
                else => unreachable,
            }

            // Free operand registers
            self.codegen.freeFloat(lhs_reg);
            self.codegen.freeFloat(rhs_reg);

            return .{ .float_reg = result_reg };
        }

        /// Map a LowLevel comparison to the appropriate float condition code.
        /// Returns null for non-comparison ops (arithmetic).
        /// AArch64 FCMP and x86_64 UCOMISD set flags differently from integer CMP,
        /// so float comparisons use unsigned/specific conditions rather than signed ones.
        fn floatCondition(op: LirExpr.LowLevel) ?Condition {
            return switch (op) {
                .num_is_eq => condEqual(),
                .num_is_lt => if (comptime target.toCpuArch() == .aarch64)
                    @as(Condition, .mi)
                else
                    @as(Condition, .below),
                .num_is_lte => if (comptime target.toCpuArch() == .aarch64)
                    @as(Condition, .ls)
                else
                    @as(Condition, .below_or_equal),
                .num_is_gt => if (comptime target.toCpuArch() == .aarch64)
                    @as(Condition, .gt)
                else
                    @as(Condition, .above),
                .num_is_gte => if (comptime target.toCpuArch() == .aarch64)
                    @as(Condition, .ge)
                else
                    @as(Condition, .above_or_equal),
                else => null,
            };
        }

        /// Generate absolute value for a numeric type
        fn generateNumAbs(self: *Self, val_loc: ValueLocation, ret_layout: layout.Idx) Allocator.Error!ValueLocation {
            const is_signed = switch (ret_layout) {
                .i8, .i16, .i32, .i64 => true,
                .u8, .u16, .u32, .u64 => false,
                .i128, .dec => true,
                .u128 => false,
                .f32, .f64 => return self.generateFloatAbs(val_loc),
                else => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: num_abs unsupported layout {s}",
                            .{@tagName(ret_layout)},
                        );
                    }
                    unreachable;
                },
            };

            if (!is_signed) {
                // Unsigned types: abs is identity
                return val_loc;
            }

            if (ret_layout == .i128 or ret_layout == .dec) {
                // 128-bit: negate, then select original or negated based on sign
                const parts = try self.getI128Parts(val_loc, .signed);
                const neg_low = try self.allocTempGeneral();
                const neg_high = try self.allocTempGeneral();

                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.subsRegRegReg(.w64, neg_low, .ZRSP, parts.low);
                    try self.codegen.emit.sbcRegRegReg(.w64, neg_high, .ZRSP, parts.high);
                    // If original was negative (signed < 0), use negated
                    try self.codegen.emit.cmpRegImm12(.w64, parts.high, 0);
                    // CSEL: if lt (negative), pick negated; else pick original
                    try self.codegen.emit.csel(.w64, neg_low, neg_low, parts.low, .lt);
                    try self.codegen.emit.csel(.w64, neg_high, neg_high, parts.high, .lt);
                } else {
                    try self.codegen.emitLoadImm(neg_low, 0);
                    try self.codegen.emit.subRegReg(.w64, neg_low, parts.low);
                    try self.codegen.emitLoadImm(neg_high, 0);
                    try self.codegen.emit.sbbRegReg(.w64, neg_high, parts.high);
                    // If original was non-negative, overwrite negated with original
                    try self.codegen.emit.testRegReg(.w64, parts.high, parts.high);
                    try self.codegen.emit.cmovcc(.not_sign, .w64, neg_low, parts.low);
                    try self.codegen.emit.cmovcc(.not_sign, .w64, neg_high, parts.high);
                }

                self.codegen.freeGeneral(parts.low);
                self.codegen.freeGeneral(parts.high);

                const stack_offset = self.codegen.allocStackSlot(16);
                try self.codegen.emitStoreStack(.w64, stack_offset, neg_low);
                try self.codegen.emitStoreStack(.w64, stack_offset + 8, neg_high);
                self.codegen.freeGeneral(neg_low);
                self.codegen.freeGeneral(neg_high);
                return .{ .stack_i128 = stack_offset };
            }

            // Signed 64-bit or smaller: negate and conditionally select
            const reg = try self.ensureInGeneralReg(val_loc);
            const neg_reg = try self.allocTempGeneral();
            try self.codegen.emitNeg(.w64, neg_reg, reg);

            if (comptime target.toCpuArch() == .aarch64) {
                // CMP reg, #0; CSEL result, neg_reg, reg, LT
                try self.codegen.emit.cmpRegImm12(.w64, reg, 0);
                try self.codegen.emit.csel(.w64, neg_reg, neg_reg, reg, .lt);
            } else {
                // TEST reg, reg; CMOVNS neg_reg, reg (if non-negative, use original)
                try self.codegen.emit.testRegReg(.w64, reg, reg);
                try self.codegen.emit.cmovcc(.not_sign, .w64, neg_reg, reg);
            }

            self.codegen.freeGeneral(reg);
            return .{ .general_reg = neg_reg };
        }

        /// Generate float absolute value
        fn generateFloatAbs(self: *Self, val_loc: ValueLocation) Allocator.Error!ValueLocation {
            const src_reg = try self.ensureInFloatReg(val_loc);
            const result_reg = try self.codegen.allocFloatFor(0);
            try self.codegen.emitAbsF64(result_reg, src_reg);
            self.codegen.freeFloat(src_reg);
            return .{ .float_reg = result_reg };
        }

        /// Generate subtraction for num_abs_diff
        /// Generate |a - b| correctly for all integer types.
        /// For floats, uses float sub + float abs.
        /// For integers: compares a and b, then subtracts the smaller from the larger.
        /// This avoids wrapping/overflow issues that abs(a - b) would cause.
        fn generateAbsDiff(self: *Self, a_loc: ValueLocation, b_loc: ValueLocation, ret_layout: layout.Idx, arg_layout: ?layout.Idx) Allocator.Error!ValueLocation {
            // Float: subtract then take float absolute value
            if (ret_layout == .f32 or ret_layout == .f64) {
                const a_reg = try self.ensureInFloatReg(a_loc);
                const b_reg = try self.ensureInFloatReg(b_loc);
                try self.codegen.emitSubF64(a_reg, a_reg, b_reg);
                self.codegen.freeFloat(b_reg);
                const result_reg = try self.codegen.allocFloatFor(0);
                try self.codegen.emitAbsF64(result_reg, a_reg);
                self.codegen.freeFloat(a_reg);
                return .{ .float_reg = result_reg };
            }

            const input_layout = self.inferAbsDiffInputLayout(a_loc, b_loc, ret_layout, arg_layout);

            // 128-bit (I128, U128, Dec)
            if (ret_layout == .i128 or ret_layout == .u128 or ret_layout == .dec) {
                return try self.generateAbsDiff128(a_loc, b_loc, input_layout);
            }

            // 64-bit or smaller integers: CMP, compute both a-b and b-a, CSEL/CMOV
            // Use arg_layout for signedness since abs_diff returns unsigned (e.g.
            // I8.abs_diff returns U8), but the comparison must be signed.
            const is_signed = switch (input_layout) {
                .i8, .i16, .i32, .i64 => true,
                .u8, .u16, .u32, .u64 => false,
                else => false,
            };

            const a_reg = try self.ensureInGeneralReg(a_loc);
            const b_reg = try self.ensureInGeneralReg(b_loc);

            // For signed types smaller than 64 bits, values are zero-extended in
            // registers. Sign-extend them so the signed comparison is correct.
            // E.g. I8 value -5 is stored as 0xFB (251); without sign extension,
            // a 64-bit signed compare treats it as 251 > 10 instead of -5 < 10.
            if (is_signed) {
                const shift_amount: u6 = switch (input_layout) {
                    .i8 => 56,
                    .i16 => 48,
                    .i32 => 32,
                    else => 0,
                };
                if (shift_amount > 0) {
                    try self.emitShlImm(.w64, a_reg, a_reg, shift_amount);
                    try self.emitAsrImm(.w64, a_reg, a_reg, shift_amount);
                    try self.emitShlImm(.w64, b_reg, b_reg, shift_amount);
                    try self.emitAsrImm(.w64, b_reg, b_reg, shift_amount);
                }
            }

            const diff_reg = try self.allocTempGeneral();
            const neg_reg = try self.allocTempGeneral();

            if (comptime target.toCpuArch() == .aarch64) {
                // CMP a, b
                try self.codegen.emit.cmpRegReg(.w64, a_reg, b_reg);
                // diff = a - b
                try self.codegen.emit.subRegRegReg(.w64, diff_reg, a_reg, b_reg);
                // neg = b - a
                try self.codegen.emit.subRegRegReg(.w64, neg_reg, b_reg, a_reg);
                // CSEL: if a >= b, use diff; else use neg
                try self.codegen.emit.csel(.w64, diff_reg, diff_reg, neg_reg, if (is_signed) .ge else .cs);
            } else {
                // diff = a - b
                try self.emitMovRegReg(diff_reg, a_reg);
                try self.codegen.emit.subRegReg(.w64, diff_reg, b_reg);
                // neg = b - a
                try self.emitMovRegReg(neg_reg, b_reg);
                try self.codegen.emit.subRegReg(.w64, neg_reg, a_reg);
                // CMP a, b
                try self.codegen.emit.cmpRegReg(.w64, a_reg, b_reg);
                // CMOV: if a < b, use neg
                try self.codegen.emit.cmovcc(if (is_signed) .less else .below, .w64, diff_reg, neg_reg);
            }

            self.codegen.freeGeneral(a_reg);
            self.codegen.freeGeneral(b_reg);
            self.codegen.freeGeneral(neg_reg);
            return .{ .general_reg = diff_reg };
        }

        fn immediateIsNegative(loc: ValueLocation) bool {
            return switch (loc) {
                .immediate_i64 => |v| v < 0,
                .immediate_i128 => |v| v < 0,
                else => false,
            };
        }

        fn signedCounterpartLayout(l: layout.Idx) ?layout.Idx {
            return switch (l) {
                .u8 => .i8,
                .u16 => .i16,
                .u32 => .i32,
                .u64 => .i64,
                .u128 => .i128,
                else => null,
            };
        }

        fn inferAbsDiffInputLayout(_: *Self, a_loc: ValueLocation, b_loc: ValueLocation, ret_layout: layout.Idx, arg_layout: ?layout.Idx) layout.Idx {
            if (arg_layout) |al| return al;

            if (immediateIsNegative(a_loc) or immediateIsNegative(b_loc)) {
                if (signedCounterpartLayout(ret_layout)) |signed_layout| {
                    return signed_layout;
                }
            }

            return ret_layout;
        }

        /// Generate 128-bit |a - b| using SUBS/SBCS for comparison flags.
        fn generateAbsDiff128(self: *Self, a_loc: ValueLocation, b_loc: ValueLocation, input_layout: layout.Idx) Allocator.Error!ValueLocation {
            const is_signed = (input_layout == .i128 or input_layout == .dec);

            const a_parts = try self.getI128Parts(a_loc, if (is_signed) .signed else .unsigned);
            const b_parts = try self.getI128Parts(b_loc, if (is_signed) .signed else .unsigned);

            // Allocate registers for both a-b and b-a results
            const diff_lo = try self.allocTempGeneral();
            const diff_hi = try self.allocTempGeneral();
            const bma_lo = try self.allocTempGeneral();
            const bma_hi = try self.allocTempGeneral();
            const flag_reg = try self.allocTempGeneral();

            if (comptime target.toCpuArch() == .aarch64) {
                // Compute a - b with flags (SUBS + SBCS gives correct 128-bit comparison)
                try self.codegen.emit.subsRegRegReg(.w64, diff_lo, a_parts.low, b_parts.low);
                try self.codegen.emit.sbcsRegRegReg(.w64, diff_hi, a_parts.high, b_parts.high);

                // Save comparison result: flag = 1 if a >= b
                try self.codegen.emit.cset(.w64, flag_reg, if (is_signed) .ge else .cs);

                // Compute b - a
                try self.codegen.emit.subsRegRegReg(.w64, bma_lo, b_parts.low, a_parts.low);
                try self.codegen.emit.sbcRegRegReg(.w64, bma_hi, b_parts.high, a_parts.high);

                // Select based on saved flag
                try self.codegen.emit.cmpRegImm12(.w64, flag_reg, 1);
                try self.codegen.emit.csel(.w64, diff_lo, diff_lo, bma_lo, .eq);
                try self.codegen.emit.csel(.w64, diff_hi, diff_hi, bma_hi, .eq);
            } else {
                // Compute a - b (128-bit) — SBB sets flags for 128-bit comparison
                try self.emitMovRegReg(diff_lo, a_parts.low);
                try self.codegen.emit.subRegReg(.w64, diff_lo, b_parts.low);
                try self.emitMovRegReg(diff_hi, a_parts.high);
                try self.codegen.emit.sbbRegReg(.w64, diff_hi, b_parts.high);

                // Save comparison result before clobbering flags
                // Zero the flag register first so SETCC only needs to set the low byte
                try self.codegen.emitLoadImm(flag_reg, 0);
                try self.codegen.emit.setcc(if (is_signed) .greater_or_equal else .above_or_equal, flag_reg);

                // Compute b - a
                try self.emitMovRegReg(bma_lo, b_parts.low);
                try self.codegen.emit.subRegReg(.w64, bma_lo, a_parts.low);
                try self.emitMovRegReg(bma_hi, b_parts.high);
                try self.codegen.emit.sbbRegReg(.w64, bma_hi, a_parts.high);

                // Select based on saved flag: if a < b (flag == 0), use bma
                try self.codegen.emit.testRegReg(.w64, flag_reg, flag_reg);
                try self.codegen.emit.cmovcc(.equal, .w64, diff_lo, bma_lo);
                try self.codegen.emit.cmovcc(.equal, .w64, diff_hi, bma_hi);
            }

            // Free temporaries
            self.codegen.freeGeneral(a_parts.low);
            self.codegen.freeGeneral(a_parts.high);
            self.codegen.freeGeneral(b_parts.low);
            self.codegen.freeGeneral(b_parts.high);
            self.codegen.freeGeneral(bma_lo);
            self.codegen.freeGeneral(bma_hi);
            self.codegen.freeGeneral(flag_reg);

            // Store result to stack
            const stack_offset = self.codegen.allocStackSlot(16);
            try self.codegen.emitStoreStack(.w64, stack_offset, diff_lo);
            try self.codegen.emitStoreStack(.w64, stack_offset + 8, diff_hi);
            self.codegen.freeGeneral(diff_lo);
            self.codegen.freeGeneral(diff_hi);
            return .{ .stack_i128 = stack_offset };
        }

        /// Generate code for if-then-else
        fn generateIfThenElse(self: *Self, ite: anytype) Allocator.Error!ValueLocation {
            const branches = self.store.getIfBranches(ite.branches);
            // Collect jump targets for patching
            var end_patches = std.ArrayList(usize).empty;
            defer end_patches.deinit(self.allocator);

            // Determine result size from layout
            var is_str_result = false;
            var is_list_result = false;
            const result_size: u32 = switch (ite.result_layout) {
                // Scalar types - size based on type
                .i8, .u8 => 1,
                .i16, .u16 => 2,
                .i32, .u32, .f32 => 4,
                .i64, .u64, .f64 => 8,
                .i128, .u128, .dec => 16,
                .str => blk: {
                    is_str_result = true;
                    break :blk roc_str_size;
                },
                else => blk: {
                    const ls = self.layout_store;
                    const result_layout = ls.getLayout(ite.result_layout);
                    break :blk switch (result_layout.tag) {
                        .list, .list_of_zst => inner: {
                            is_list_result = true;
                            break :inner roc_list_size;
                        },
                        .struct_ => ls.getStructData(result_layout.data.struct_.idx).size,
                        .tag_union => ls.getTagUnionData(result_layout.data.tag_union.idx).size,
                        .zst => 0,
                        .scalar => ls.layoutSizeAlign(result_layout).size,
                        else => unreachable,
                    };
                },
            };

            // Determine storage strategy based on result size
            var result_slot: ?i32 = null;
            var result_reg: ?GeneralReg = null;

            // Generate each branch
            var first_branch = true;
            for (branches) |branch| {
                // Generate condition
                const cond_loc = try self.generateExpr(branch.cond);
                const cond_reg = try self.ensureInGeneralReg(cond_loc);
                // Compare with zero and branch if equal (condition is false)
                const else_patch = try self.emitCmpZeroAndJump(cond_reg);

                self.codegen.freeGeneral(cond_reg);

                // Generate body (true case)
                const body_loc = try self.generateExpr(branch.body);

                // On first branch, determine result storage strategy
                if (first_branch) {
                    first_branch = false;
                    if (builtin.mode == .Debug and !is_list_result and body_loc == .list_stack) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: if branch produced list_stack but result_layout is not a list",
                            .{},
                        );
                    }
                    // Use stack for types > 8 bytes (e.g., i128, Dec) or stack-based values
                    if (result_size > 8) {
                        result_slot = self.codegen.allocStackSlot(result_size);
                    } else {
                        switch (body_loc) {
                            .stack, .stack_str, .list_stack => {
                                result_slot = self.codegen.allocStackSlot(result_size);
                            },
                            else => {
                                result_reg = try self.allocTempGeneral();
                            },
                        }
                    }
                }

                // Copy result to the appropriate location
                if (result_slot) |slot| {
                    // Copy from body_loc to result slot using the layout-determined size
                    try self.copyBytesToStackOffset(slot, body_loc, result_size);
                } else if (result_reg) |reg| {
                    const body_reg = try self.ensureInGeneralReg(body_loc);
                    try self.emitMovRegReg(reg, body_reg);
                    self.codegen.freeGeneral(body_reg);
                }

                // Jump to end (skip the else branch)
                const end_patch = try self.codegen.emitJump();
                try end_patches.append(self.allocator, end_patch);

                // Patch the else jump to here (start of else/next branch)
                const current_offset = self.codegen.currentOffset();
                self.codegen.patchJump(else_patch, current_offset);
            }

            // Generate final else
            const else_loc = try self.generateExpr(ite.final_else);

            // Handle case where all branches were composite but else is the first evaluation
            if (result_slot == null and result_reg == null) {
                if (builtin.mode == .Debug and !is_list_result and else_loc == .list_stack) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: if else branch produced list_stack but result_layout is not a list",
                        .{},
                    );
                }
                // Use stack for types > 8 bytes (e.g., i128, Dec) or stack-based values
                if (result_size > 8) {
                    result_slot = self.codegen.allocStackSlot(result_size);
                } else {
                    switch (else_loc) {
                        .stack, .stack_str, .list_stack => {
                            result_slot = self.codegen.allocStackSlot(result_size);
                        },
                        else => {
                            result_reg = try self.allocTempGeneral();
                        },
                    }
                }
            }

            if (result_slot) |slot| {
                // Copy from else_loc to result slot using the layout-determined size
                try self.copyBytesToStackOffset(slot, else_loc, result_size);
            } else if (result_reg) |reg| {
                const else_reg = try self.ensureInGeneralReg(else_loc);
                try self.emitMovRegReg(reg, else_reg);
                self.codegen.freeGeneral(else_reg);
            }

            // Patch all end jumps to here
            const end_offset = self.codegen.currentOffset();
            for (end_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }

            // Return the result location - use appropriate types for multi-word values
            if (result_slot) |slot| {
                if (is_str_result) {
                    return .{ .stack_str = slot };
                }
                if (is_list_result) {
                    return .{
                        .list_stack = .{
                            .struct_offset = slot,
                            .data_offset = 0, // Data location is stored in the list struct itself
                            .num_elements = 0, // Unknown at compile time
                        },
                    };
                }
                // Return stack_i128 only for actual 128-bit scalar types (i128, u128, Dec)
                // NOT for tag unions that happen to be 16 bytes
                if (ite.result_layout == .i128 or ite.result_layout == .u128 or ite.result_layout == .dec) {
                    return .{ .stack_i128 = slot };
                }
                return .{ .stack = .{ .offset = slot } };
            } else if (result_reg) |reg| {
                return .{ .general_reg = reg };
            } else {
                // Edge case: no branches at all (shouldn't happen)
                return .{ .immediate_i64 = 0 };
            }
        }

        /// Compare register with zero and jump if equal (condition is false)
        /// Returns the patch location for the jump
        fn emitCmpZeroAndJump(self: *Self, reg: GeneralReg) !usize {
            if (comptime target.toCpuArch() == .aarch64) {
                // cbz reg, 0 (branch if zero, offset will be patched)
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.cbz(.w64, reg, 0);
                return patch_loc;
            } else {
                // cmp reg, 0; je (will be patched)
                try self.codegen.emit.cmpRegImm32(.w64, reg, 0);
                return try self.codegen.emitCondJump(.equal);
            }
        }

        /// Move register to register (architecture-specific)
        fn emitMovRegReg(self: *Self, dst: GeneralReg, src: GeneralReg) !void {
            try self.codegen.emit.movRegReg(.w64, dst, src);
        }

        // ── Shared helpers for generateMatch / generateMatchStmt ──

        /// Load the discriminant from a tag union value and mask to actual size.
        /// Returns the register holding the discriminant. Caller must free it.
        fn loadAndMaskDiscriminant(
            self: *Self,
            value_loc: ValueLocation,
            disc_use_w32: bool,
            tu_disc_offset: i32,
            tu_disc_size: u8,
        ) Allocator.Error!GeneralReg {
            const disc_reg = try self.allocTempGeneral();
            if (tu_disc_size == 0) {
                try self.codegen.emitLoadImm(disc_reg, 0);
                return disc_reg;
            }
            switch (value_loc) {
                .stack_str, .stack_i128 => |base_offset| {
                    if (comptime target.toCpuArch() == .aarch64) {
                        const w = if (disc_use_w32) aarch64.RegisterWidth.w32 else aarch64.RegisterWidth.w64;
                        try self.codegen.emit.ldrRegMemSoff(w, disc_reg, .FP, base_offset + tu_disc_offset);
                    } else {
                        const w = if (disc_use_w32) x86_64.RegisterWidth.w32 else x86_64.RegisterWidth.w64;
                        try self.codegen.emit.movRegMem(w, disc_reg, .RBP, base_offset + tu_disc_offset);
                    }
                },
                .stack => |s| {
                    const base_offset = s.offset;
                    if (comptime target.toCpuArch() == .aarch64) {
                        const w = if (disc_use_w32) aarch64.RegisterWidth.w32 else aarch64.RegisterWidth.w64;
                        try self.codegen.emit.ldrRegMemSoff(w, disc_reg, .FP, base_offset + tu_disc_offset);
                    } else {
                        const w = if (disc_use_w32) x86_64.RegisterWidth.w32 else x86_64.RegisterWidth.w64;
                        try self.codegen.emit.movRegMem(w, disc_reg, .RBP, base_offset + tu_disc_offset);
                    }
                },
                .list_stack => |ls_info| {
                    if (comptime target.toCpuArch() == .aarch64) {
                        const w = if (disc_use_w32) aarch64.RegisterWidth.w32 else aarch64.RegisterWidth.w64;
                        try self.codegen.emit.ldrRegMemSoff(w, disc_reg, .FP, ls_info.struct_offset + tu_disc_offset);
                    } else {
                        const w = if (disc_use_w32) x86_64.RegisterWidth.w32 else x86_64.RegisterWidth.w64;
                        try self.codegen.emit.movRegMem(w, disc_reg, .RBP, ls_info.struct_offset + tu_disc_offset);
                    }
                },
                .general_reg => |reg| {
                    try self.emitMovRegReg(disc_reg, reg);
                },
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(disc_reg, val);
                },
                else => {
                    self.codegen.freeGeneral(disc_reg);
                    unreachable;
                },
            }

            // Mask to actual discriminant size — memory loads may include padding bytes
            if (tu_disc_size != 0 and tu_disc_size < 4) {
                const mask: i32 = (@as(i32, 1) << @as(u5, @intCast(tu_disc_size * 8))) - 1;
                if (comptime target.toCpuArch() == .aarch64) {
                    const mask_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(mask_reg, mask);
                    try self.codegen.emit.andRegRegReg(.w32, disc_reg, disc_reg, mask_reg);
                    self.codegen.freeGeneral(mask_reg);
                } else {
                    try self.codegen.emit.andRegImm32(disc_reg, mask);
                }
            }

            return disc_reg;
        }

        /// After the outer tag discriminant has matched, emit discriminant checks for any
        /// nested .tag arg patterns. For example, for the branch `Err(Exit(code))`, after
        /// confirming the outer discriminant is Err, this function checks that the payload's
        /// discriminant is also Exit. If any inner check fails, a conditional jump is emitted
        /// and its patch location is appended to `fail_patches` so the caller can direct all
        /// failures to the same "start of next branch" target.
        ///
        /// Handles both the single-arg case (payload is itself a tag_union) and the multi-arg
        /// case (payload is a struct whose fields may be tag unions). Recurses for deeper nesting.
        fn emitInnerTagArgDiscriminantChecks(
            self: *Self,
            tag_pattern: anytype,
            value_loc: ValueLocation,
            value_layout_idx: layout.Idx,
            value_layout_val: anytype,
            fail_patches: *std.ArrayList(usize),
        ) Allocator.Error!void {
            const ls = self.layout_store;
            const args = self.store.getPatternSpan(tag_pattern.args);
            if (args.len == 0) return;

            if (value_layout_val.tag != .tag_union) return;

            const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
            const variants = ls.getTagUnionVariants(tu_data);
            if (tag_pattern.discriminant >= variants.len) return;

            const payload_layout_idx = variants.get(tag_pattern.discriminant).payload_layout;
            const payload_layout_val = ls.getLayout(payload_layout_idx);

            // Materialize the outer value to the stack so we can address the payload.
            const stable_value_loc = try self.materializeValueToStackForLayout(value_loc, value_layout_idx);
            const base_offset: i32 = switch (stable_value_loc) {
                .stack => |s| s.offset,
                .stack_i128 => |off| off,
                .stack_str => |off| off,
                .list_stack => |ls_info| ls_info.struct_offset,
                else => return,
            };
            const payload_loc = self.stackLocationForLayout(payload_layout_idx, base_offset);

            if (payload_layout_val.tag == .tag_union) {
                // Single-arg payload that is itself a tag union — check its discriminant.
                if (args.len >= 1) {
                    // Unwrap as_pattern wrappers (e.g., Err(e as Exit(code))).
                    var effective_pat = self.store.getPattern(args[0]);
                    while (effective_pat == .as_pattern) {
                        effective_pat = self.store.getPattern(effective_pat.as_pattern.inner);
                    }
                    if (effective_pat == .tag) {
                        const inner_tag_pat = effective_pat.tag;
                        const inner_tu = ls.getTagUnionData(payload_layout_val.data.tag_union.idx);
                        const inner_disc_offset: i32 = @intCast(inner_tu.discriminant_offset);
                        const inner_disc_size: u8 = inner_tu.discriminant_size;
                        const inner_total_size: u32 = inner_tu.size;
                        const inner_disc_use_w32 = (inner_disc_offset + 8 > @as(i32, @intCast(inner_total_size)));

                        const inner_disc_reg = try self.loadAndMaskDiscriminant(
                            payload_loc,
                            inner_disc_use_w32,
                            inner_disc_offset,
                            inner_disc_size,
                        );
                        try self.emitCmpImm(inner_disc_reg, @intCast(inner_tag_pat.discriminant));
                        self.codegen.freeGeneral(inner_disc_reg);
                        const fail_patch = try self.emitJumpIfNotEqual();
                        try fail_patches.append(self.allocator, fail_patch);

                        // Recurse for deeper nesting (e.g., A(B(C(x)))).
                        try self.emitInnerTagArgDiscriminantChecks(
                            inner_tag_pat,
                            payload_loc,
                            payload_layout_idx,
                            payload_layout_val,
                            fail_patches,
                        );
                    }
                }
            } else if (payload_layout_val.tag == .struct_) {
                // Multi-arg tag payload stored as a struct; check any fields that are .tag patterns.
                for (args, 0..) |arg_pattern_id, arg_idx| {
                    // Unwrap as_pattern wrappers (e.g., Foo(x, e as Bar(y))).
                    var effective_pat = self.store.getPattern(arg_pattern_id);
                    while (effective_pat == .as_pattern) {
                        effective_pat = self.store.getPattern(effective_pat.as_pattern.inner);
                    }
                    if (effective_pat != .tag) continue;

                    const inner_tag_pat = effective_pat.tag;
                    const field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(
                        payload_layout_val.data.struct_.idx,
                        @intCast(arg_idx),
                    );
                    const field_layout_val = ls.getLayout(field_layout_idx);
                    if (field_layout_val.tag != .tag_union) continue;

                    const field_offset = ls.getStructFieldOffsetByOriginalIndex(
                        payload_layout_val.data.struct_.idx,
                        @intCast(arg_idx),
                    );
                    const field_loc = self.stackLocationForLayout(
                        field_layout_idx,
                        base_offset + @as(i32, @intCast(field_offset)),
                    );

                    const inner_tu = ls.getTagUnionData(field_layout_val.data.tag_union.idx);
                    const inner_disc_offset: i32 = @intCast(inner_tu.discriminant_offset);
                    const inner_disc_size: u8 = inner_tu.discriminant_size;
                    const inner_total_size: u32 = inner_tu.size;
                    const inner_disc_use_w32 = (inner_disc_offset + 8 > @as(i32, @intCast(inner_total_size)));

                    const inner_disc_reg = try self.loadAndMaskDiscriminant(
                        field_loc,
                        inner_disc_use_w32,
                        inner_disc_offset,
                        inner_disc_size,
                    );
                    try self.emitCmpImm(inner_disc_reg, @intCast(inner_tag_pat.discriminant));
                    self.codegen.freeGeneral(inner_disc_reg);
                    const fail_patch = try self.emitJumpIfNotEqual();
                    try fail_patches.append(self.allocator, fail_patch);

                    // Recurse for deeper nesting.
                    try self.emitInnerTagArgDiscriminantChecks(
                        inner_tag_pat,
                        field_loc,
                        field_layout_idx,
                        field_layout_val,
                        fail_patches,
                    );
                }
            }
        }

        /// Bind tag payload fields to symbols after a tag pattern match.
        /// Computes the payload location for each arg and delegates to bindPattern,
        /// which handles all pattern types (bind, wildcard, tag, struct, list, as_pattern, etc.).
        fn bindTagPayloadFields(
            self: *Self,
            tag_pattern: anytype,
            value_loc: ValueLocation,
            value_layout_idx: layout.Idx,
            value_layout_val: anytype,
        ) Allocator.Error!void {
            const ls = self.layout_store;
            const args = self.store.getPatternSpan(tag_pattern.args);
            if (args.len == 0) return;

            const variant_payload_layout, const stable_payload_loc = blk: {
                if (value_layout_val.tag == .tag_union) {
                    const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                    const variants = ls.getTagUnionVariants(tu_data);
                    if (tag_pattern.discriminant >= variants.len) return;

                    const payload_layout_idx = variants.get(tag_pattern.discriminant).payload_layout;
                    const stable_value_loc = try self.materializeValueToStackForLayout(value_loc, value_layout_idx);
                    const base_offset: i32 = switch (stable_value_loc) {
                        .stack => |s| s.offset,
                        .stack_i128 => |off| off,
                        .stack_str => |off| off,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        else => unreachable,
                    };

                    // Match-pattern payload bindings borrow from the scrutinee. RC insertion
                    // models branch pattern binds that way, so do not detach or retain here.
                    break :blk .{
                        payload_layout_idx,
                        self.stackLocationForLayout(payload_layout_idx, base_offset),
                    };
                }

                if (value_layout_val.tag == .box) {
                    const inner_layout = ls.getLayout(value_layout_val.data.box);
                    if (inner_layout.tag != .tag_union) return;

                    const tu_data = ls.getTagUnionData(inner_layout.data.tag_union.idx);
                    const variants = ls.getTagUnionVariants(tu_data);
                    if (tag_pattern.discriminant >= variants.len) return;

                    const payload_layout_idx = variants.get(tag_pattern.discriminant).payload_layout;
                    const payload_layout_val = ls.getLayout(payload_layout_idx);
                    const payload_size = ls.layoutSizeAlign(payload_layout_val).size;
                    if (payload_size == 0) {
                        break :blk .{ payload_layout_idx, self.stackLocationForLayout(payload_layout_idx, 0) };
                    }

                    const box_ptr_reg = try self.ensureInGeneralReg(value_loc);
                    defer self.codegen.freeGeneral(box_ptr_reg);

                    // Boxed tag-pattern payloads are also borrowed from the scrutinee, but the
                    // current value-location model cannot point into heap storage directly. Copy
                    // the payload bytes to stack without retaining the underlying RC payloads.
                    const detached_slot = self.codegen.allocStackSlot(payload_size);
                    var copied: u32 = 0;
                    while (copied < payload_size) {
                        const temp_reg = try self.allocTempGeneral();
                        try self.emitLoad(.w64, temp_reg, box_ptr_reg, @intCast(copied));
                        try self.emitStore(.w64, frame_ptr, detached_slot + @as(i32, @intCast(copied)), temp_reg);
                        self.codegen.freeGeneral(temp_reg);
                        copied += 8;
                    }

                    break :blk .{
                        payload_layout_idx,
                        self.stackLocationForLayout(payload_layout_idx, detached_slot),
                    };
                }

                if (value_layout_val.tag == .scalar or value_layout_val.tag == .zst) {
                    if (builtin.mode == .Debug and args.len != 1) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: scalar/zst tag payload binding expects exactly 1 arg, found {d}",
                            .{args.len},
                        );
                    }
                    break :blk .{ layout.Idx.zst, ValueLocation{ .immediate_i64 = 0 } };
                }

                return;
            };

            const stable_payload_layout_val = ls.getLayout(variant_payload_layout);

            for (args, 0..) |arg_pattern_id, arg_idx| {
                const arg_loc: ValueLocation = if (stable_payload_layout_val.tag == .struct_) blk: {
                    const payload_base: i32 = switch (stable_payload_loc) {
                        .stack => |s| s.offset,
                        .stack_i128 => |off| off,
                        .stack_str => |off| off,
                        else => unreachable,
                    };
                    const elem_offset = ls.getStructFieldOffsetByOriginalIndex(stable_payload_layout_val.data.struct_.idx, @intCast(arg_idx));
                    const elem_layout = ls.getStructFieldLayoutByOriginalIndex(stable_payload_layout_val.data.struct_.idx, @intCast(arg_idx));
                    if (builtin.mode == .Debug) {
                        try self.assertPatternMatchesRuntimeLayout(arg_pattern_id, elem_layout, "match tag payload field");
                    }
                    break :blk self.stackLocationForLayout(elem_layout, payload_base + @as(i32, @intCast(elem_offset)));
                } else blk: {
                    if (builtin.mode == .Debug) {
                        if (args.len != 1) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: non-struct match tag payload can only bind one arg, got {d}",
                                .{args.len},
                            );
                        }
                        try self.assertPatternMatchesRuntimeLayout(arg_pattern_id, variant_payload_layout, "match tag payload");
                    }
                    break :blk stable_payload_loc;
                };

                try self.bindPattern(arg_pattern_id, arg_loc);
            }
        }

        fn assertPatternMatchesRuntimeLayout(
            self: *Self,
            pattern_id: LirPatternId,
            runtime_layout_idx: layout.Idx,
            comptime context: []const u8,
        ) Allocator.Error!void {
            if (builtin.mode != .Debug) return;

            const ls = self.layout_store;
            const pattern = self.store.getPattern(pattern_id);

            switch (pattern) {
                .bind => |bind| {
                    if (!try self.layoutsStructurallyCompatible(bind.layout_idx, runtime_layout_idx)) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: {s} bind layout mismatch: pattern={d} runtime={d}",
                            .{ context, @intFromEnum(bind.layout_idx), @intFromEnum(runtime_layout_idx) },
                        );
                    }
                },
                .wildcard => |wc| {
                    if (!try self.layoutsStructurallyCompatible(wc.layout_idx, runtime_layout_idx)) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: {s} wildcard layout mismatch: pattern={d} runtime={d}",
                            .{ context, @intFromEnum(wc.layout_idx), @intFromEnum(runtime_layout_idx) },
                        );
                    }
                },
                .as_pattern => |as_pat| {
                    if (!try self.layoutsStructurallyCompatible(as_pat.layout_idx, runtime_layout_idx)) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: {s} as-pattern layout mismatch: pattern={d} runtime={d}",
                            .{ context, @intFromEnum(as_pat.layout_idx), @intFromEnum(runtime_layout_idx) },
                        );
                    }
                    try self.assertPatternMatchesRuntimeLayout(as_pat.inner, runtime_layout_idx, context);
                },
                .struct_ => |s| {
                    if (!try self.layoutsStructurallyCompatible(s.struct_layout, runtime_layout_idx)) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: {s} struct layout mismatch: pattern={d} runtime={d}",
                            .{ context, @intFromEnum(s.struct_layout), @intFromEnum(runtime_layout_idx) },
                        );
                    }

                    const runtime_layout = ls.getLayout(runtime_layout_idx);
                    if (runtime_layout.tag != .struct_) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: {s} expected runtime struct layout, got {s}",
                            .{ context, @tagName(runtime_layout.tag) },
                        );
                    }

                    const fields = self.store.getPatternSpan(s.fields);
                    for (fields, 0..) |field_pattern_id, i| {
                        const field_layout = ls.getStructFieldLayout(runtime_layout.data.struct_.idx, @intCast(i));
                        try self.assertPatternMatchesRuntimeLayout(field_pattern_id, field_layout, context);
                    }
                },
                .tag => |tag_pat| {
                    if (tag_pat.union_layout != runtime_layout_idx) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: {s} tag layout mismatch: pattern={d} runtime={d}",
                            .{ context, @intFromEnum(tag_pat.union_layout), @intFromEnum(runtime_layout_idx) },
                        );
                    }
                },
                .list => |list_pat| {
                    if (list_pat.list_layout != runtime_layout_idx) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: {s} list layout mismatch: pattern={d} runtime={d}",
                            .{ context, @intFromEnum(list_pat.list_layout), @intFromEnum(runtime_layout_idx) },
                        );
                    }
                },
                .int_literal, .float_literal, .str_literal => {},
            }
        }

        fn patternLayoutCompatible(
            self: *Self,
            pattern_id: LirPatternId,
            runtime_layout_idx: layout.Idx,
        ) Allocator.Error!bool {
            const ls = self.layout_store;
            const pattern = self.store.getPattern(pattern_id);

            return switch (pattern) {
                .bind => |bind| try self.layoutsStructurallyCompatible(bind.layout_idx, runtime_layout_idx),
                .wildcard => |wc| try self.layoutsStructurallyCompatible(wc.layout_idx, runtime_layout_idx),
                .as_pattern => |as_pat| try self.layoutsStructurallyCompatible(as_pat.layout_idx, runtime_layout_idx) and try self.patternLayoutCompatible(as_pat.inner, runtime_layout_idx),
                .struct_ => |s| blk: {
                    if (!try self.layoutsStructurallyCompatible(s.struct_layout, runtime_layout_idx)) break :blk false;

                    const runtime_layout = ls.getLayout(runtime_layout_idx);
                    if (runtime_layout.tag != .struct_) break :blk false;

                    const fields = self.store.getPatternSpan(s.fields);
                    for (fields, 0..) |field_pattern_id, i| {
                        const field_layout = ls.getStructFieldLayout(runtime_layout.data.struct_.idx, @intCast(i));
                        if (!try self.patternLayoutCompatible(field_pattern_id, field_layout)) {
                            break :blk false;
                        }
                    }

                    break :blk true;
                },
                .tag => |tag_pat| tag_pat.union_layout == runtime_layout_idx,
                .list => |list_pat| list_pat.list_layout == runtime_layout_idx,
                .int_literal, .float_literal, .str_literal => true,
            };
        }

        fn layoutsStructurallyCompatible(
            self: *Self,
            expected_layout_idx: layout.Idx,
            runtime_layout_idx: layout.Idx,
        ) Allocator.Error!bool {
            if (expected_layout_idx == runtime_layout_idx) return true;

            const ls = self.layout_store;
            const expected_layout = ls.getLayout(expected_layout_idx);
            const runtime_layout = ls.getLayout(runtime_layout_idx);

            if (expected_layout.tag != runtime_layout.tag) return false;

            return switch (expected_layout.tag) {
                .box => try self.layoutsStructurallyCompatible(expected_layout.data.box, runtime_layout.data.box),
                .list => try self.layoutsStructurallyCompatible(expected_layout.data.list, runtime_layout.data.list),
                .struct_ => blk: {
                    if (expected_layout.data.struct_.alignment != runtime_layout.data.struct_.alignment) break :blk false;

                    const expected_data = ls.getStructData(expected_layout.data.struct_.idx);
                    const runtime_data = ls.getStructData(runtime_layout.data.struct_.idx);
                    const expected_fields = ls.struct_fields.sliceRange(expected_data.getFields());
                    const runtime_fields = ls.struct_fields.sliceRange(runtime_data.getFields());

                    if (expected_fields.len != runtime_fields.len) break :blk false;

                    for (0..expected_fields.len) |i| {
                        const expected_field = expected_fields.get(i);
                        const runtime_field = runtime_fields.get(i);
                        if (expected_field.index != runtime_field.index) break :blk false;
                        if (!try self.layoutsStructurallyCompatible(expected_field.layout, runtime_field.layout)) {
                            break :blk false;
                        }
                    }

                    break :blk true;
                },
                .closure => try self.layoutsStructurallyCompatible(
                    expected_layout.data.closure.captures_layout_idx,
                    runtime_layout.data.closure.captures_layout_idx,
                ),
                .tag_union => blk: {
                    if (expected_layout.data.tag_union.alignment != runtime_layout.data.tag_union.alignment) break :blk false;

                    const expected_data = ls.getTagUnionData(expected_layout.data.tag_union.idx);
                    const runtime_data = ls.getTagUnionData(runtime_layout.data.tag_union.idx);
                    const expected_variants = ls.getTagUnionVariants(expected_data);
                    const runtime_variants = ls.getTagUnionVariants(runtime_data);

                    if (expected_variants.len != runtime_variants.len) break :blk false;

                    for (0..expected_variants.len) |i| {
                        if (!try self.layoutsStructurallyCompatible(
                            expected_variants.get(i).payload_layout,
                            runtime_variants.get(i).payload_layout,
                        )) {
                            break :blk false;
                        }
                    }

                    break :blk true;
                },
                else => expected_layout.eql(runtime_layout),
            };
        }

        /// Emit a string pattern comparison: generates literal, calls strEqual,
        /// compares result with 0. After this call, CPU flags are set so that
        /// emitJumpIfEqual() will jump when the strings are NOT equal.
        fn emitStringPatternCheck(self: *Self, str_lit_idx: anytype, value_loc: ValueLocation) Allocator.Error!void {
            const lit_loc = try self.generateStrLiteral(str_lit_idx);
            const lit_off = try self.ensureOnStack(lit_loc, roc_str_size);
            const val_off = try self.ensureOnStack(value_loc, roc_str_size);
            const eq_loc = try self.callStr2ToScalar(val_off, lit_off, @intFromPtr(&wrapStrEqual), .str_equal);
            const eq_reg = try self.ensureInGeneralReg(eq_loc);
            try self.emitCmpImm(eq_reg, 0);
            self.codegen.freeGeneral(eq_reg);
        }

        /// Emit an int pattern comparison: loads literal, compares with value.
        /// After this call, CPU flags are set so that emitJumpIfNotEqual() will
        /// jump when the values don't match.
        fn emitIntPatternCheck(self: *Self, int_value: i128, value_loc: ValueLocation) Allocator.Error!void {
            const value_reg = try self.ensureInGeneralReg(value_loc);
            if (int_value >= std.math.minInt(i32) and int_value <= std.math.maxInt(i32)) {
                try self.emitCmpImm(value_reg, @intCast(int_value));
            } else {
                const tmp_reg = try self.allocTempGeneral();
                try self.loadImm64(tmp_reg, @intCast(int_value));
                try self.emitCmpRegReg(value_reg, tmp_reg);
                self.codegen.freeGeneral(tmp_reg);
            }
            // Free the register if ensureInGeneralReg allocated a new one.
            // When value_loc is already .general_reg, ensureInGeneralReg returns
            // the existing register which the caller still owns.
            if (value_loc != .general_reg) {
                self.codegen.freeGeneral(value_reg);
            }
        }

        /// Emit list pattern bindings: length check and prefix/suffix element binding.
        /// Returns the conditional jump patch for length
        /// mismatch (null if last branch). Caller must free list_ptr_reg via freeGeneral.
        fn emitListPatternBindings(
            self: *Self,
            list_pattern: anytype,
            value_loc: ValueLocation,
        ) Allocator.Error!void {
            const ls = self.layout_store;
            const prefix_patterns = self.store.getPatternSpan(list_pattern.prefix);
            const suffix_patterns = self.store.getPatternSpan(list_pattern.suffix);

            // Get base offset of the list struct
            const base_offset: i32 = switch (value_loc) {
                .stack => |s| s.offset,
                .stack_str => |off| off,
                .list_stack => |list_info| list_info.struct_offset,
                else => unreachable,
            };

            const elem_layout = ls.getLayout(list_pattern.elem_layout);
            const elem_size_align = ls.layoutSizeAlign(elem_layout);
            const elem_size = elem_size_align.size;

            // Load the data pointer from the list struct (at base_offset)
            const list_ptr_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, list_ptr_reg, frame_ptr, base_offset);

            // Bind each prefix element by copying from heap to stack
            for (prefix_patterns, 0..) |elem_pattern_id, elem_idx| {
                const elem_offset_in_list = @as(i32, @intCast(elem_idx * elem_size));
                const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));
                const temp_reg = try self.allocTempGeneral();

                if (elem_size <= 8) {
                    try self.emitLoad(.w64, temp_reg, list_ptr_reg, elem_offset_in_list);
                    try self.emitStore(.w64, frame_ptr, elem_slot, temp_reg);
                } else {
                    try self.copyChunked(temp_reg, list_ptr_reg, elem_offset_in_list, frame_ptr, elem_slot, elem_size);
                }

                self.codegen.freeGeneral(temp_reg);
                try self.bindPattern(elem_pattern_id, self.stackLocationForLayout(list_pattern.elem_layout, elem_slot));
            }

            // Bind suffix elements (from the end of the list)
            if (suffix_patterns.len > 0) {
                const suf_len_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, suf_len_reg, frame_ptr, base_offset + 8);

                const suffix_count = @as(u32, @intCast(suffix_patterns.len));
                const suf_ptr_reg = try self.allocTempGeneral();

                // suf_ptr = list_ptr + (len - suffix_len) * elem_size
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.subRegRegImm12(.w64, suf_len_reg, suf_len_reg, @intCast(suffix_count));
                    if (elem_size == 1) {
                        try self.codegen.emit.addRegRegReg(.w64, suf_ptr_reg, list_ptr_reg, suf_len_reg);
                    } else {
                        const imm_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(imm_reg, @intCast(elem_size));
                        try self.codegen.emit.mulRegRegReg(.w64, suf_len_reg, suf_len_reg, imm_reg);
                        try self.codegen.emit.addRegRegReg(.w64, suf_ptr_reg, list_ptr_reg, suf_len_reg);
                        self.codegen.freeGeneral(imm_reg);
                    }
                } else {
                    try self.codegen.emit.subRegImm32(.w64, suf_len_reg, @intCast(suffix_count));
                    if (elem_size > 1) {
                        const imm_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(imm_reg, @intCast(elem_size));
                        try self.codegen.emit.imulRegReg(.w64, suf_len_reg, imm_reg);
                        self.codegen.freeGeneral(imm_reg);
                    }
                    try self.codegen.emit.movRegReg(.w64, suf_ptr_reg, list_ptr_reg);
                    try self.codegen.emit.addRegReg(.w64, suf_ptr_reg, suf_len_reg);
                }
                self.codegen.freeGeneral(suf_len_reg);

                // Bind each suffix element
                for (suffix_patterns, 0..) |suf_pattern_id, suf_idx| {
                    const suf_offset = @as(i32, @intCast(suf_idx * elem_size));
                    const suf_slot = self.codegen.allocStackSlot(@intCast(elem_size));
                    const temp_reg = try self.allocTempGeneral();

                    if (elem_size <= 8) {
                        try self.emitLoad(.w64, temp_reg, suf_ptr_reg, suf_offset);
                        try self.emitStore(.w64, frame_ptr, suf_slot, temp_reg);
                    } else {
                        try self.copyChunked(temp_reg, suf_ptr_reg, suf_offset, frame_ptr, suf_slot, elem_size);
                    }

                    self.codegen.freeGeneral(temp_reg);
                    try self.bindPattern(suf_pattern_id, self.stackLocationForLayout(list_pattern.elem_layout, suf_slot));
                }

                self.codegen.freeGeneral(suf_ptr_reg);
            }

            self.codegen.freeGeneral(list_ptr_reg);
        }

        /// Emit literal value checks for list pattern prefix/suffix elements.
        /// For each literal pattern in the prefix/suffix, loads the element from
        /// the list and compares it with the expected value. Appends fail patches
        /// (jumps to next branch) for each failed comparison.
        fn emitListLiteralChecks(
            self: *Self,
            list_pattern: anytype,
            value_loc: ValueLocation,
            fail_patches: *std.ArrayList(usize),
        ) Allocator.Error!void {
            const ls = self.layout_store;
            const prefix_patterns = self.store.getPatternSpan(list_pattern.prefix);
            const suffix_patterns = self.store.getPatternSpan(list_pattern.suffix);

            const elem_layout = ls.getLayout(list_pattern.elem_layout);
            const elem_size = ls.layoutSizeAlign(elem_layout).size;

            const base_offset: i32 = switch (value_loc) {
                .stack => |s| s.offset,
                .stack_str => |off| off,
                .list_stack => |list_info| list_info.struct_offset,
                else => unreachable,
            };

            // Check prefix literal elements
            var has_prefix_literals = false;
            for (prefix_patterns) |elem_pattern_id| {
                const elem_pattern = self.store.getPattern(elem_pattern_id);
                if (elem_pattern == .int_literal) {
                    has_prefix_literals = true;
                    break;
                }
            }

            if (has_prefix_literals) {
                const list_ptr_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, list_ptr_reg, frame_ptr, base_offset);

                for (prefix_patterns, 0..) |elem_pattern_id, elem_idx| {
                    const elem_pattern = self.store.getPattern(elem_pattern_id);
                    switch (elem_pattern) {
                        .int_literal => |int_lit| {
                            const elem_offset = @as(i32, @intCast(elem_idx * elem_size));
                            const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));
                            const temp_reg = try self.allocTempGeneral();

                            if (elem_size <= 8) {
                                try self.emitLoad(.w64, temp_reg, list_ptr_reg, elem_offset);
                                try self.emitStore(.w64, frame_ptr, elem_slot, temp_reg);
                            } else {
                                try self.copyChunked(temp_reg, list_ptr_reg, elem_offset, frame_ptr, elem_slot, elem_size);
                            }

                            self.codegen.freeGeneral(temp_reg);
                            const elem_loc = self.stackLocationForLayout(list_pattern.elem_layout, elem_slot);
                            try self.emitIntPatternCheck(int_lit.value, elem_loc);
                            const patch = try self.emitJumpIfNotEqual();
                            try fail_patches.append(self.allocator, patch);
                        },
                        else => {},
                    }
                }

                self.codegen.freeGeneral(list_ptr_reg);
            }

            // Check suffix literal elements
            if (suffix_patterns.len > 0) {
                var has_suffix_literals = false;
                for (suffix_patterns) |elem_pattern_id| {
                    const elem_pattern = self.store.getPattern(elem_pattern_id);
                    if (elem_pattern == .int_literal) {
                        has_suffix_literals = true;
                        break;
                    }
                }

                if (has_suffix_literals) {
                    const list_ptr_reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, list_ptr_reg, frame_ptr, base_offset);
                    const suf_len_reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, suf_len_reg, frame_ptr, base_offset + 8);

                    const suffix_count = @as(u32, @intCast(suffix_patterns.len));
                    const suf_ptr_reg = try self.allocTempGeneral();

                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.subRegRegImm12(.w64, suf_len_reg, suf_len_reg, @intCast(suffix_count));
                        if (elem_size == 1) {
                            try self.codegen.emit.addRegRegReg(.w64, suf_ptr_reg, list_ptr_reg, suf_len_reg);
                        } else {
                            const imm_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(imm_reg, @intCast(elem_size));
                            try self.codegen.emit.mulRegRegReg(.w64, suf_len_reg, suf_len_reg, imm_reg);
                            try self.codegen.emit.addRegRegReg(.w64, suf_ptr_reg, list_ptr_reg, suf_len_reg);
                            self.codegen.freeGeneral(imm_reg);
                        }
                    } else {
                        try self.codegen.emit.subRegImm32(.w64, suf_len_reg, @intCast(suffix_count));
                        if (elem_size > 1) {
                            const imm_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(imm_reg, @intCast(elem_size));
                            try self.codegen.emit.imulRegReg(.w64, suf_len_reg, imm_reg);
                            self.codegen.freeGeneral(imm_reg);
                        }
                        try self.codegen.emit.movRegReg(.w64, suf_ptr_reg, list_ptr_reg);
                        try self.codegen.emit.addRegReg(.w64, suf_ptr_reg, suf_len_reg);
                    }
                    self.codegen.freeGeneral(suf_len_reg);

                    for (suffix_patterns, 0..) |suf_pattern_id, suf_idx| {
                        const suf_pattern = self.store.getPattern(suf_pattern_id);
                        switch (suf_pattern) {
                            .int_literal => |int_lit| {
                                const suf_offset = @as(i32, @intCast(suf_idx * elem_size));
                                const suf_slot = self.codegen.allocStackSlot(@intCast(elem_size));
                                const temp_reg = try self.allocTempGeneral();

                                if (elem_size <= 8) {
                                    try self.emitLoad(.w64, temp_reg, suf_ptr_reg, suf_offset);
                                    try self.emitStore(.w64, frame_ptr, suf_slot, temp_reg);
                                } else {
                                    try self.copyChunked(temp_reg, suf_ptr_reg, suf_offset, frame_ptr, suf_slot, elem_size);
                                }

                                self.codegen.freeGeneral(temp_reg);
                                const elem_loc = self.stackLocationForLayout(list_pattern.elem_layout, suf_slot);
                                try self.emitIntPatternCheck(int_lit.value, elem_loc);
                                const patch = try self.emitJumpIfNotEqual();
                                try fail_patches.append(self.allocator, patch);
                            },
                            else => {},
                        }
                    }

                    self.codegen.freeGeneral(suf_ptr_reg);
                    self.codegen.freeGeneral(list_ptr_reg);
                }
            }
        }

        /// Emit a length check for a list pattern. Sets CPU flags for comparison.
        /// For exact matches (no rest/suffix), emitJumpIfNotEqual() skips on mismatch.
        /// For rest/suffix patterns, emitJumpIfLessThan() skips if too short.
        fn emitListLengthCheck(
            self: *Self,
            list_pattern: anytype,
            value_loc: ValueLocation,
        ) Allocator.Error!void {
            const prefix_patterns = self.store.getPatternSpan(list_pattern.prefix);
            const suffix_patterns = self.store.getPatternSpan(list_pattern.suffix);

            const base_offset: i32 = switch (value_loc) {
                .stack => |s| s.offset,
                .stack_str => |off| off,
                .list_stack => |list_info| list_info.struct_offset,
                else => unreachable,
            };

            // Load list length from stack (offset 8 from struct base)
            const len_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, len_reg, frame_ptr, base_offset + 8);

            // Compare length with expected (prefix + suffix)
            const expected_len = @as(i32, @intCast(prefix_patterns.len + suffix_patterns.len));
            try self.emitCmpImm(len_reg, expected_len);
            self.codegen.freeGeneral(len_reg);
        }

        /// Generate code for match expression
        fn generateMatch(self: *Self, when_expr: anytype) Allocator.Error!ValueLocation {
            // Evaluate the scrutinee (the value being matched)
            const value_loc = try self.generateExpr(when_expr.value);
            // Get the branches
            const branches = self.store.getMatchBranches(when_expr.branches);
            if (branches.len == 0) {
                unreachable;
            }

            // Determine result size to decide between register and stack result
            const ls = self.layout_store;
            const result_layout_val = ls.getLayout(when_expr.result_layout);
            var result_size: u32 = ls.layoutSizeAlign(result_layout_val).size;
            var use_stack_result = result_size > 8;
            const value_layout_val = ls.getLayout(when_expr.value_layout);
            const tu_disc_offset: i32 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk @intCast(tu_data.discriminant_offset);
            } else 0;
            const tu_total_size: u32 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk tu_data.size;
            } else ls.layoutSizeAlign(value_layout_val).size;
            const tu_disc_size: u8 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk tu_data.discriminant_size;
            } else @intCast(@max(ls.layoutSizeAlign(value_layout_val).size, 1));
            // Use .w32 for discriminant loads when .w64 would read past the tag union.
            // Discriminants are at most 4 bytes, so .w32 is always sufficient.
            const disc_use_w32 = (tu_disc_offset + 8 > @as(i32, @intCast(tu_total_size)));

            // Allocate result storage (may be upgraded dynamically below)
            var result_slot: i32 = if (use_stack_result) self.codegen.allocStackSlot(result_size) else 0;
            var result_reg: ?GeneralReg = if (!use_stack_result) try self.allocTempGeneral() else null;

            // Collect jump targets for patching to end
            var end_patches = std.ArrayList(usize).empty;
            defer end_patches.deinit(self.allocator);

            // Generate each branch
            for (branches, 0..) |branch, i| {
                const pattern = self.store.getPattern(branch.pattern);

                // Try to match the pattern
                switch (pattern) {
                    .wildcard => {
                        // Wildcard always matches
                        const guard_patch = try self.emitGuardCheck(branch.guard);
                        if (guard_patch) |gp| {
                            const body_loc = try self.generateExpr(branch.body);
                            try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                            if (i < branches.len - 1) {
                                const end_patch = try self.codegen.emitJump();
                                try end_patches.append(self.allocator, end_patch);
                            }
                            self.codegen.patchJump(gp, self.codegen.currentOffset());
                        } else {
                            const body_loc = try self.generateExpr(branch.body);
                            try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                            break;
                        }
                    },
                    .bind => |bind| {
                        // Bind always matches - bind the value first
                        const symbol_key: u64 = @bitCast(bind.symbol);
                        try self.symbol_locations.put(symbol_key, value_loc);

                        // Guard must be checked after binding (guard may reference bound var)
                        const guard_patch = try self.emitGuardCheck(branch.guard);
                        if (guard_patch) |gp| {
                            const body_loc = try self.generateExpr(branch.body);
                            try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                            if (i < branches.len - 1) {
                                const end_patch = try self.codegen.emitJump();
                                try end_patches.append(self.allocator, end_patch);
                            }
                            self.codegen.patchJump(gp, self.codegen.currentOffset());
                        } else {
                            const body_loc = try self.generateExpr(branch.body);
                            try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                            break;
                        }
                    },
                    .int_literal => |int_lit| {
                        try self.emitIntPatternCheck(int_lit.value, value_loc);

                        // Jump to next branch if not equal
                        const is_last_branch = (i == branches.len - 1);
                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            next_patch = try self.emitJumpIfNotEqual();
                        }

                        // Guard check
                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        // Pattern matched - generate body
                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);

                        // Jump to end (unless this is the last branch)
                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            // Patch the next branch jump to here
                            if (next_patch) |patch| {
                                const current_offset = self.codegen.currentOffset();
                                self.codegen.patchJump(patch, current_offset);
                            }
                        }
                        if (guard_patch) |patch| {
                            self.codegen.patchJump(patch, self.codegen.currentOffset());
                        }
                    },
                    .str_literal => |str_lit_idx| {
                        try self.emitStringPatternCheck(str_lit_idx, value_loc);

                        // Jump to next branch if equal to 0 (strings not equal)
                        const is_last_branch = (i == branches.len - 1);
                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            next_patch = try self.emitJumpIfEqual();
                        }

                        // Guard check
                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        // Pattern matched - generate body
                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);

                        // Jump to end (unless this is the last branch)
                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            // Patch the next branch jump to here
                            if (next_patch) |patch| {
                                const current_offset = self.codegen.currentOffset();
                                self.codegen.patchJump(patch, current_offset);
                            }
                        }
                        if (guard_patch) |patch| {
                            self.codegen.patchJump(patch, self.codegen.currentOffset());
                        }
                    },
                    .tag => |tag_pattern| {
                        // Match on tag discriminant
                        const disc_reg = try self.loadAndMaskDiscriminant(value_loc, disc_use_w32, tu_disc_offset, tu_disc_size);
                        try self.emitCmpImm(disc_reg, @intCast(tag_pattern.discriminant));
                        self.codegen.freeGeneral(disc_reg);

                        // Jump to next branch if not equal
                        const is_last_branch = (i == branches.len - 1);
                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            next_patch = try self.emitJumpIfNotEqual();
                        }

                        // For patterns like Err(Exit(code)), check inner discriminants
                        // before binding any payload variables. If any inner discriminant
                        // does not match, we must fall through to the next branch.
                        // Patches are collected alongside next_patch and all target the
                        // same "start of next branch" offset.
                        var inner_fail_patches = std.ArrayList(usize).empty;
                        defer inner_fail_patches.deinit(self.allocator);
                        if (!is_last_branch) {
                            try self.emitInnerTagArgDiscriminantChecks(
                                tag_pattern,
                                value_loc,
                                when_expr.value_layout,
                                value_layout_val,
                                &inner_fail_patches,
                            );
                        }

                        // Bind tag payload fields
                        try self.bindTagPayloadFields(tag_pattern, value_loc, when_expr.value_layout, value_layout_val);

                        // Guard check (after bindings, since guard may reference bound vars)
                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        // Generate body
                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);

                        // Jump to end (unless this is the last branch)
                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            // Patch the outer and all inner "not-equal" jumps to the
                            // start of the next branch (current position after the end jump).
                            const next_branch_offset = self.codegen.currentOffset();
                            if (next_patch) |patch| {
                                self.codegen.patchJump(patch, next_branch_offset);
                            }
                            for (inner_fail_patches.items) |patch| {
                                self.codegen.patchJump(patch, next_branch_offset);
                            }
                        }
                        if (guard_patch) |patch| {
                            self.codegen.patchJump(patch, self.codegen.currentOffset());
                        }
                    },
                    .list => |list_pattern| {
                        const is_exact_match = list_pattern.rest.isNone();

                        // Check list length
                        try self.emitListLengthCheck(list_pattern, value_loc);

                        // Jump to next branch if length doesn't match
                        const is_last_branch = (i == branches.len - 1);
                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            if (is_exact_match) {
                                next_patch = try self.emitJumpIfNotEqual();
                            } else {
                                next_patch = try self.emitJumpIfLessThan();
                            }
                        }

                        // Check literal values in prefix/suffix elements.
                        // For patterns like [1, 2, ..], we must verify that the
                        // actual list elements match the literal values, not just
                        // the list length.
                        var literal_fail_patches = std.ArrayList(usize).empty;
                        defer literal_fail_patches.deinit(self.allocator);
                        if (!is_last_branch) {
                            try self.emitListLiteralChecks(list_pattern, value_loc, &literal_fail_patches);
                        }

                        // Bind prefix, suffix, and rest elements
                        try self.emitListPatternBindings(list_pattern, value_loc);

                        // Guard check (after bindings, since guard may reference bound vars)
                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        // Generate body
                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);

                        // Jump to end (unless this is the last branch)
                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            // Patch the length check and all literal check jumps
                            // to the start of the next branch.
                            const next_branch_offset = self.codegen.currentOffset();
                            if (next_patch) |patch| {
                                self.codegen.patchJump(patch, next_branch_offset);
                            }
                            for (literal_fail_patches.items) |patch| {
                                self.codegen.patchJump(patch, next_branch_offset);
                            }
                        }
                        if (guard_patch) |patch| {
                            self.codegen.patchJump(patch, self.codegen.currentOffset());
                        }
                    },
                    .struct_ => {
                        // Struct destructuring always matches - bind fields and generate body
                        // Ensure the value is on the stack for field access
                        const value_size = ls.layoutSizeAlign(value_layout_val).size;
                        const stack_off = try self.ensureOnStack(value_loc, value_size);
                        try self.bindPattern(branch.pattern, .{ .stack = .{ .offset = stack_off } });

                        const guard_patch = try self.emitGuardCheck(branch.guard);
                        if (guard_patch) |gp| {
                            const body_loc = try self.generateExpr(branch.body);
                            try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                            if (i < branches.len - 1) {
                                const end_patch = try self.codegen.emitJump();
                                try end_patches.append(self.allocator, end_patch);
                            }
                            self.codegen.patchJump(gp, self.codegen.currentOffset());
                        } else {
                            const body_loc = try self.generateExpr(branch.body);
                            try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                            break;
                        }
                    },
                    .as_pattern => |as_pat| {
                        // As-pattern: bind the whole value to the symbol, then match the inner pattern
                        const symbol_key: u64 = @bitCast(as_pat.symbol);
                        try self.symbol_locations.put(symbol_key, value_loc);

                        // Also bind the inner pattern
                        const value_size = ls.layoutSizeAlign(value_layout_val).size;
                        const stack_off = try self.ensureOnStack(value_loc, value_size);
                        try self.bindPattern(as_pat.inner, .{ .stack = .{ .offset = stack_off } });

                        const guard_patch = try self.emitGuardCheck(branch.guard);
                        if (guard_patch) |gp| {
                            const body_loc = try self.generateExpr(branch.body);
                            try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                            if (i < branches.len - 1) {
                                const end_patch = try self.codegen.emitJump();
                                try end_patches.append(self.allocator, end_patch);
                            }
                            self.codegen.patchJump(gp, self.codegen.currentOffset());
                        } else {
                            const body_loc = try self.generateExpr(branch.body);
                            try self.storeMatchResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                            break;
                        }
                    },
                    else => {
                        unreachable;
                    },
                }
            }

            // Patch all end jumps to here
            const end_offset = self.codegen.currentOffset();
            for (end_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }

            if (use_stack_result) {
                // Use the declared result layout if it's known (not ZST)
                if (result_layout_val.tag != .zst) {
                    if (when_expr.result_layout == .i128 or when_expr.result_layout == .u128 or when_expr.result_layout == .dec) {
                        return .{ .stack_i128 = result_slot };
                    } else if (when_expr.result_layout == .str) {
                        return .{ .stack_str = result_slot };
                    } else if (result_layout_val.tag == .list or result_layout_val.tag == .list_of_zst) {
                        return .{ .list_stack = .{
                            .struct_offset = result_slot,
                            .data_offset = 0,
                            .num_elements = 0,
                        } };
                    } else if (result_layout_val.tag == .tag_union or result_layout_val.tag == .struct_ or result_layout_val.tag == .closure) {
                        // Non-scalar composite types stay as generic stack values
                        // so downstream code uses the layout for proper sizing.
                        const declared_size = ls.layoutSizeAlign(result_layout_val).size;
                        if (builtin.mode == .Debug and result_size > declared_size) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: match result slot size {d} exceeds declared layout size {d}",
                                .{ result_size, declared_size },
                            );
                        }
                        return .{ .stack = .{ .offset = result_slot } };
                    }
                }
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: unhandled stack match result layout {s}",
                        .{@tagName(result_layout_val.tag)},
                    );
                }
                return .{ .stack = .{ .offset = result_slot } };
            }
            if (result_reg) |reg| {
                return .{ .general_reg = reg };
            } else {
                return .{ .immediate_i64 = 0 };
            }
        }

        /// Store a match-branch result.
        fn storeMatchResult(
            self: *Self,
            body_loc: ValueLocation,
            use_stack_result: *bool,
            result_slot: *i32,
            result_reg: *?GeneralReg,
            result_size: *u32,
        ) Allocator.Error!void {
            if (body_loc == .noreturn) return;

            if (!use_stack_result.*) {
                if (builtin.mode == .Debug) {
                    switch (body_loc) {
                        .stack_str, .list_stack, .stack_i128, .immediate_i128 => {
                            std.debug.panic(
                                "LIR/codegen invariant violated: match branch produced multi-word value in register-result mode",
                                .{},
                            );
                        },
                        else => {},
                    }
                }
            }

            // Store the result
            if (use_stack_result.*) {
                try self.storeResultToSlot(result_slot.*, body_loc, result_size.*);
            } else {
                const body_reg = try self.ensureInGeneralReg(body_loc);
                try self.emitMovRegReg(result_reg.*.?, body_reg);
                self.codegen.freeGeneral(body_reg);
            }
        }

        /// Emit guard check. If the guard expression is present and evaluates to false,
        /// emit a conditional jump. Returns the patch location or null if no guard.
        fn emitGuardCheck(self: *Self, guard: anytype) Allocator.Error!?usize {
            if (guard.isNone()) return null;
            const guard_loc = try self.generateExpr(guard);
            const guard_reg = try self.ensureInGeneralReg(guard_loc);
            try self.emitCmpImm(guard_reg, 0);
            self.codegen.freeGeneral(guard_reg);
            return try self.emitJumpIfEqual();
        }

        /// Compare two registers
        fn emitCmpRegReg(self: *Self, lhs: GeneralReg, rhs: GeneralReg) !void {
            try self.codegen.emit.cmpRegReg(.w64, lhs, rhs);
        }

        /// Load 64-bit immediate into register
        fn loadImm64(self: *Self, dst: GeneralReg, value: i64) !void {
            try self.codegen.emit.movRegImm64(dst, @bitCast(value));
        }

        /// Generate code for an empty list
        fn generateEmptyList(self: *Self) Allocator.Error!ValueLocation {
            // Empty list: ptr = null, len = 0, capacity = 0
            // Materialize as a proper 24-byte list struct on the stack so that
            // when passed as a function argument, all 3 registers are set correctly.
            const list_struct_offset: i32 = self.codegen.allocStackSlot(roc_list_size);
            const zero_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(zero_reg, 0);

            try self.emitStore(.w64, frame_ptr, list_struct_offset, zero_reg);
            try self.emitStore(.w64, frame_ptr, list_struct_offset + 8, zero_reg);
            try self.emitStore(.w64, frame_ptr, list_struct_offset + 16, zero_reg);
            self.codegen.freeGeneral(zero_reg);

            return .{ .list_stack = .{
                .struct_offset = list_struct_offset,
                .data_offset = 0,
                .num_elements = 0,
            } };
        }

        /// Generate code for a list with elements
        fn generateList(self: *Self, list: anytype) Allocator.Error!ValueLocation {
            const elems = self.store.getExprSpan(list.elems);
            if (elems.len == 0) {
                // Empty list: ptr = null, len = 0, capacity = 0
                const list_struct_offset: i32 = self.codegen.allocStackSlot(roc_list_size);
                const zero_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(zero_reg, 0);

                try self.emitStore(.w64, frame_ptr, list_struct_offset, zero_reg);
                try self.emitStore(.w64, frame_ptr, list_struct_offset + 8, zero_reg);
                try self.emitStore(.w64, frame_ptr, list_struct_offset + 16, zero_reg);
                self.codegen.freeGeneral(zero_reg);

                return .{ .list_stack = .{
                    .struct_offset = list_struct_offset,
                    .data_offset = 0,
                    .num_elements = 0,
                } };
            }

            // Get element layout from the layout store - required, no fallbacks
            const ls = self.layout_store;
            const elem_layout_data = ls.getLayout(list.elem_layout);
            const elem_size_align = ls.layoutSizeAlign(elem_layout_data);
            const elem_size: u32 = elem_size_align.size;
            const elem_alignment: u32 = @intCast(elem_size_align.alignment.toByteUnits());
            const num_elems: u32 = @intCast(elems.len);
            const total_data_bytes: usize = @as(usize, elem_size) * @as(usize, num_elems);

            // Determine if elements contain refcounted data
            const elements_refcounted: bool = ls.layoutContainsRefcounted(elem_layout_data);

            // Get the saved RocOps register
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            // Call allocateWithRefcountC(data_bytes, element_alignment, elements_refcounted, roc_ops)
            // Returns pointer to allocated memory (refcount is already initialized to 1)

            // Allocate stack slot to save the heap pointer (will be clobbered during element generation)
            const heap_ptr_slot: i32 = self.codegen.allocStackSlot(8);

            // Allocate list using CallBuilder with automatic R12 handling
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addImmArg(@intCast(total_data_bytes));
            try builder.addImmArg(@intCast(elem_alignment));
            try builder.addImmArg(if (elements_refcounted) 1 else 0);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, @intFromPtr(&allocateWithRefcountC), .allocate_with_refcount);

            // Save heap pointer from return register to stack slot
            try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

            // Now store each element to heap memory
            for (elems, 0..) |elem_id, i| {
                const elem_loc = try self.generateExpr(elem_id);
                const elem_heap_offset: i32 = @intCast(@as(usize, i) * @as(usize, elem_size));

                if (elem_size == 0) {
                    continue;
                }

                // Materialize element to stack for a uniform copy path.
                const elem_stack_offset = try self.ensureOnStack(elem_loc, elem_size);

                // Load heap pointer from stack slot
                const heap_ptr = try self.allocTempGeneral();
                try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);

                // Copy elem_size bytes from stack to heap in 8-byte chunks
                const temp_reg = try self.allocTempGeneral();
                try self.copyChunked(temp_reg, frame_ptr, elem_stack_offset, heap_ptr, elem_heap_offset, elem_size);
                self.codegen.freeGeneral(temp_reg);

                self.codegen.freeGeneral(heap_ptr);
            }

            // Create the list struct: (ptr, len, capacity)
            // ptr points to heap memory, len = capacity = num_elems
            const list_struct_offset: i32 = self.codegen.allocStackSlot(roc_list_size);

            // Load heap pointer and length
            const ptr_reg = try self.allocTempGeneral();
            const len_reg = try self.allocTempGeneral();

            try self.emitLoad(.w64, ptr_reg, frame_ptr, heap_ptr_slot);
            try self.codegen.emitLoadImm(len_reg, @intCast(num_elems));

            // Store list struct
            try self.emitStore(.w64, frame_ptr, list_struct_offset, ptr_reg);
            try self.emitStore(.w64, frame_ptr, list_struct_offset + 8, len_reg);
            try self.emitStore(.w64, frame_ptr, list_struct_offset + 16, len_reg);

            self.codegen.freeGeneral(ptr_reg);
            self.codegen.freeGeneral(len_reg);

            // Return the list location
            // Note: data_offset is no longer meaningful for heap-allocated lists,
            // but we keep it for compatibility with existing code
            return .{
                .list_stack = .{
                    .struct_offset = list_struct_offset,
                    .data_offset = heap_ptr_slot, // Now points to heap ptr storage on stack
                    .num_elements = num_elems,
                },
            };
        }

        /// Generate code for a struct literal (records, tuples, empty records).
        /// Fields are in layout order (sorted by alignment).
        fn generateStruct(self: *Self, s: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;

            // Validate layout index before use
            if (@intFromEnum(s.struct_layout) >= ls.layouts.len()) {
                unreachable;
            }

            // Get the struct layout
            const struct_layout = ls.getLayout(s.struct_layout);
            // Empty structs (ZST) have scalar layout, not struct_ layout
            if (struct_layout.tag != .struct_) {
                return .{ .immediate_i64 = 0 };
            }

            const struct_data = ls.getStructData(struct_layout.data.struct_.idx);
            const stack_size = struct_data.size;

            // Zero-sized structs don't need storage
            if (stack_size == 0) {
                return .{ .immediate_i64 = 0 };
            }

            // Allocate stack space for the struct
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Get field expressions
            const field_exprs = self.store.getExprSpan(s.fields);

            // Copy each field to its offset within the struct.
            // Fields are already in layout order, so iterate positionally.
            for (field_exprs, 0..) |field_expr_id, i| {
                const field_offset = ls.getStructFieldOffset(struct_layout.data.struct_.idx, @intCast(i));
                const field_size = ls.getStructFieldSize(struct_layout.data.struct_.idx, @intCast(i));
                const field_loc = try self.generateExpr(field_expr_id);
                const field_base = base_offset + @as(i32, @intCast(field_offset));
                try self.copyBytesToStackOffset(field_base, field_loc, field_size);
            }

            return .{ .stack = .{ .offset = base_offset, .size = ValueSize.fromByteCount(@min(stack_size, 8)) } };
        }

        /// Determine the size of a value from its ValueLocation alone.
        fn valueSizeFromLoc(_: *Self, loc: ValueLocation) u32 {
            return switch (loc) {
                .stack_str => roc_str_size,
                .list_stack => roc_list_size,
                .stack_i128, .immediate_i128 => 16,
                .immediate_i64, .general_reg, .stack, .float_reg, .immediate_f64 => 8,
                else => {
                    if (builtin.mode == .Debug) std.debug.panic("LIR/codegen invariant violated: valueSizeFromLoc unsupported location {s}", .{@tagName(loc)});
                    unreachable;
                },
            };
        }

        /// Given a field's stack base offset, size, and layout index, return the appropriate ValueLocation.
        fn fieldLocationFromLayout(self: *Self, field_base: i32, field_size: u32, field_layout_idx: layout.Idx) ValueLocation {
            // Check well-known layout indices first
            if (field_layout_idx == .str) {
                return .{ .stack_str = field_base };
            }
            if (field_layout_idx == .i128 or field_layout_idx == .u128 or field_layout_idx == .dec) {
                return .{ .stack_i128 = field_base };
            }
            // Check layout tag for lists
            {
                const ls = self.layout_store;
                if (@intFromEnum(field_layout_idx) < ls.layouts.len()) {
                    const field_layout = ls.getLayout(field_layout_idx);
                    if (field_layout.tag == .list or field_layout.tag == .list_of_zst) {
                        return .{ .list_stack = .{
                            .struct_offset = field_base,
                            .data_offset = 0,
                            .num_elements = 0,
                        } };
                    }
                }
            }
            return .{ .stack = .{ .offset = field_base, .size = ValueSize.fromByteCount(field_size) } };
        }

        const LambdaProcOptions = struct {
            use_cache: bool = true,
            extra_hidden_args: u8 = 0,
            /// When true, all parameters are received as pointers regardless of
            /// register pressure. Used by the sort comparator trampoline which
            /// always passes element pointers to avoid ABI mismatches between
            /// the Zig callconv(.c) and the Roc internal calling convention.
            force_pass_by_ptr: bool = false,
        };

        /// Generate code for struct field access (records and tuples).
        /// field_idx is the sorted position in layout.
        fn generateStructAccess(self: *Self, access: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;

            // Generate code for the struct expression
            const struct_loc = try self.generateExpr(access.struct_expr);

            // Get the struct layout to find field offset and size
            const struct_layout = ls.getLayout(access.struct_layout);
            if (struct_layout.tag != .struct_) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: struct_access expected struct_ layout, got {s} (field_idx={d})",
                        .{ @tagName(struct_layout.tag), access.field_idx },
                    );
                }
                unreachable;
            }

            const field_offset = ls.getStructFieldOffset(struct_layout.data.struct_.idx, access.field_idx);
            const field_size = ls.getStructFieldSize(struct_layout.data.struct_.idx, access.field_idx);
            const field_layout_idx = ls.getStructFieldLayout(struct_layout.data.struct_.idx, access.field_idx);

            return switch (struct_loc) {
                .stack_str => |sv| blk: {
                    const field_base = sv + @as(i32, @intCast(field_offset));
                    break :blk self.fieldLocationFromLayout(field_base, field_size, field_layout_idx);
                },
                .stack => |sv| blk: {
                    const field_base = sv.offset + @as(i32, @intCast(field_offset));
                    break :blk self.fieldLocationFromLayout(field_base, field_size, field_layout_idx);
                },
                .stack_i128 => |sv| blk: {
                    // Struct itself is i128-sized, field access within it
                    const field_base = sv + @as(i32, @intCast(field_offset));
                    break :blk self.fieldLocationFromLayout(field_base, field_size, field_layout_idx);
                },
                .general_reg => |reg| blk: {
                    // Struct in register - only valid for small structs (<=8 bytes)
                    if (field_size > 8) {
                        unreachable;
                    }
                    if (field_offset == 0) {
                        break :blk .{ .general_reg = reg };
                    } else {
                        const result_reg = try self.allocTempGeneral();
                        try self.emitLsrImm(.w64, result_reg, reg, @intCast(field_offset * 8));
                        self.codegen.freeGeneral(reg);
                        break :blk .{ .general_reg = result_reg };
                    }
                },
                .immediate_i64 => |val| blk: {
                    if (field_size > 8) {
                        unreachable;
                    }
                    const shifted = val >> @intCast(field_offset * 8);
                    break :blk .{ .immediate_i64 = shifted };
                },
                else => unreachable,
            };
        }

        /// Generate code for a zero-argument tag (just discriminant)
        fn generateZeroArgTag(self: *Self, tag: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;

            // Get the union layout
            const union_layout = ls.getLayout(tag.union_layout);

            // For simple tags that fit in a register, just return the discriminant
            if (union_layout.tag == .scalar or union_layout.tag == .zst) {
                return .{ .immediate_i64 = tag.discriminant };
            }

            if (union_layout.tag != .tag_union) {
                // Might be a simple enum represented as a scalar
                return .{ .immediate_i64 = tag.discriminant };
            }

            const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
            const stack_size = tu_data.size;

            // For small unions (single discriminant byte), just return the value
            if (stack_size <= 8) {
                return .{ .immediate_i64 = tag.discriminant };
            }

            // For larger unions, allocate space and store discriminant
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Zero out the union space first
            try self.zeroStackArea(base_offset, stack_size);

            // Store discriminant at its offset
            const disc_offset = tu_data.discriminant_offset;
            const disc_size = tu_data.discriminant_size;
            try self.storeDiscriminant(base_offset + @as(i32, @intCast(disc_offset)), tag.discriminant, disc_size);

            return .{ .stack = .{ .offset = base_offset } };
        }

        /// Generate code for a tag with payload arguments
        fn generateTag(self: *Self, tag: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;

            // Get the union layout
            const union_layout = ls.getLayout(tag.union_layout);
            if (union_layout.tag == .scalar or union_layout.tag == .zst) {
                const arg_exprs = self.store.getExprSpan(tag.args);
                for (arg_exprs) |arg_expr_id| {
                    _ = try self.generateExpr(arg_expr_id);
                }
                return .{ .immediate_i64 = tag.discriminant };
            }
            if (union_layout.tag != .tag_union) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: generateTag expected tag_union/scalar/zst layout, got {s}",
                        .{@tagName(union_layout.tag)},
                    );
                }
                unreachable;
            }

            const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
            const stack_size = tu_data.size;

            // Allocate stack space for the tag union
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Zero out the union space first
            try self.zeroStackArea(base_offset, stack_size);

            // Get the variant's payload layout to determine correct sizes
            const variants = ls.getTagUnionVariants(tu_data);
            const variant_payload_layout: ?layout.Idx = if (tag.discriminant < variants.len) blk: {
                break :blk variants.get(tag.discriminant).payload_layout;
            } else null;
            // Get argument expressions and store them as payload
            const arg_exprs = self.store.getExprSpan(tag.args);
            if (arg_exprs.len == 1) {
                // Single argument: the payload layout directly tells us the size
                const arg_loc = try self.generateExpr(arg_exprs[0]);
                const payload_size: u32 = if (variant_payload_layout) |pl| blk: {
                    const pl_val = ls.getLayout(pl);
                    break :blk ls.layoutSizeAlign(pl_val).size;
                } else self.valueSizeFromLoc(arg_loc);
                try self.copyBytesToStackOffset(base_offset, arg_loc, payload_size);
            } else {
                // Multiple arguments: use the variant's payload tuple layout to get
                // field offsets and sizes. This correctly handles boxed/recursive types
                // and types larger than 8 bytes (e.g. List=24, Str=24, i128=16).
                const payload_tuple = if (variant_payload_layout) |pl| blk: {
                    const pl_val = ls.getLayout(pl);
                    break :blk if (pl_val.tag == .struct_) pl_val.data.struct_.idx else null;
                } else null;

                for (arg_exprs, 0..) |arg_expr_id, arg_i| {
                    const arg_loc = try self.generateExpr(arg_expr_id);
                    const elem_offset: i32 = if (payload_tuple) |tuple_idx|
                        @intCast(ls.getStructFieldOffsetByOriginalIndex(tuple_idx, @intCast(arg_i)))
                    else
                        @as(i32, @intCast(arg_i)) * 8;
                    const elem_size: u32 = if (payload_tuple) |tuple_idx| blk: {
                        const elem_layout = ls.getStructFieldLayoutByOriginalIndex(tuple_idx, @intCast(arg_i));
                        const elem_layout_val = ls.getLayout(elem_layout);
                        break :blk ls.layoutSizeAlign(elem_layout_val).size;
                    } else self.valueSizeFromLoc(arg_loc);
                    try self.copyBytesToStackOffset(base_offset + elem_offset, arg_loc, elem_size);
                }
            }

            // Store discriminant at its offset
            const disc_offset = tu_data.discriminant_offset;
            const disc_size = tu_data.discriminant_size;
            try self.storeDiscriminant(base_offset + @as(i32, @intCast(disc_offset)), tag.discriminant, disc_size);

            return .{ .stack = .{ .offset = base_offset } };
        }

        /// Copy a value to a stack offset
        fn copyValueToStackOffset(self: *Self, offset: i32, loc: ValueLocation) Allocator.Error!void {
            switch (loc) {
                .immediate_i64 => |val| {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, val);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    self.codegen.freeGeneral(reg);
                },
                .general_reg => |reg| {
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                },
                .stack => |s| {
                    const src_offset = s.offset;
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, src_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    self.codegen.freeGeneral(reg);
                },
                .stack_i128 => |src_offset| {
                    // Copy 16 bytes
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, src_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    try self.codegen.emitLoadStack(.w64, reg, src_offset + 8);
                    try self.codegen.emitStoreStack(.w64, offset + 8, reg);
                    self.codegen.freeGeneral(reg);
                },
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    try self.codegen.emitStoreStack(.w64, offset + 8, reg);
                    self.codegen.freeGeneral(reg);
                },
                .float_reg => |reg| {
                    try self.codegen.emitStoreStackF64(offset, reg);
                },
                .immediate_f64 => |val| {
                    const bits: u64 = @bitCast(val);
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @bitCast(bits));
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    self.codegen.freeGeneral(reg);
                },
                .stack_str => |src_offset| {
                    // Copy 24-byte RocStr struct
                    const reg = try self.allocTempGeneral();
                    // Copy ptr/data (first 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, src_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    // Copy len (second 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, src_offset + 8);
                    try self.codegen.emitStoreStack(.w64, offset + 8, reg);
                    // Copy capacity/flags (third 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, src_offset + 16);
                    try self.codegen.emitStoreStack(.w64, offset + 16, reg);
                    self.codegen.freeGeneral(reg);
                },
                .list_stack => |list_info| {
                    // Copy 24-byte list struct
                    const reg = try self.allocTempGeneral();
                    // Copy ptr (first 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, list_info.struct_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    // Copy len (second 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, list_info.struct_offset + 8);
                    try self.codegen.emitStoreStack(.w64, offset + 8, reg);
                    // Copy capacity (third 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, list_info.struct_offset + 16);
                    try self.codegen.emitStoreStack(.w64, offset + 16, reg);
                    self.codegen.freeGeneral(reg);
                },
                .noreturn => unreachable,
            }
        }
        /// Copy a specific number of bytes from a value location to a stack offset
        /// This uses the layout-determined size rather than inferring from ValueLocation type
        fn copyBytesToStackOffset(self: *Self, dest_offset: i32, loc: ValueLocation, size: u32) Allocator.Error!void {
            // Handle ZST (zero-sized types) - nothing to copy
            if (size == 0) {
                return;
            }

            switch (loc) {
                .immediate_i64 => |val| {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, val);
                    switch (size) {
                        1 => try self.emitStoreStackW8(dest_offset, reg),
                        2 => try self.emitStoreStackW16(dest_offset, reg),
                        4 => try self.codegen.emitStoreStack(.w32, dest_offset, reg),
                        8 => try self.codegen.emitStoreStack(.w64, dest_offset, reg),
                        16 => {
                            // i64 being stored as Dec (i128) - sign extend
                            try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                            // Store sign extension in high part
                            const high: i64 = if (val < 0) -1 else 0;
                            try self.codegen.emitLoadImm(reg, high);
                            try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                        },
                        roc_list_size => {
                            // Empty list (immediate 0) being stored as a roc_list_size-byte list struct
                            // An empty list has ptr=0, len=0, capacity=0 (all zeros)
                            std.debug.assert(val == 0);
                            try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                            try self.codegen.emitStoreStack(.w64, dest_offset + target_ptr_size, reg);
                            try self.codegen.emitStoreStack(.w64, dest_offset + 2 * target_ptr_size, reg);
                        },
                        else => unreachable,
                    }
                    self.codegen.freeGeneral(reg);
                    return;
                },
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    const reg = try self.allocTempGeneral();

                    if (size == 16) {
                        // Full i128 copy
                        try self.codegen.emitLoadImm(reg, @bitCast(low));
                        try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                        try self.codegen.emitLoadImm(reg, @bitCast(high));
                        try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                    } else if (size == 8) {
                        // Truncate to i64 - just store the low 64 bits
                        try self.codegen.emitLoadImm(reg, @bitCast(low));
                        try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    } else if (size == 4) {
                        // Truncate to i32
                        const low32: u32 = @truncate(low);
                        try self.codegen.emitLoadImm(reg, @as(i64, @bitCast(@as(u64, low32))));
                        try self.codegen.emitStoreStack(.w32, dest_offset, reg);
                    } else {
                        unreachable; // Unsupported size for i128 truncation
                    }

                    self.codegen.freeGeneral(reg);
                    return;
                },
                .immediate_f64 => |val| {
                    const reg = try self.allocTempGeneral();
                    if (size == 4) {
                        const f32_val: f32 = @floatCast(val);
                        const bits: u32 = @bitCast(f32_val);
                        try self.codegen.emitLoadImm(reg, @as(i64, bits));
                        try self.codegen.emitStoreStack(.w32, dest_offset, reg);
                    } else if (size == 8) {
                        try self.codegen.emitLoadImm(reg, @bitCast(@as(u64, @bitCast(val))));
                        try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    } else {
                        unreachable;
                    }
                    self.codegen.freeGeneral(reg);
                    return;
                },
                .float_reg => |freg| {
                    if (size == 4) {
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.fcvtFloatFloat(.single, freg, .double, freg);
                            const bits_reg = try self.allocTempGeneral();
                            try self.codegen.emit.fmovGenFromFloat(.single, bits_reg, freg);
                            try self.codegen.emitStoreStack(.w32, dest_offset, bits_reg);
                            self.codegen.freeGeneral(bits_reg);
                        } else {
                            try self.codegen.emit.cvtsd2ssRegReg(freg, freg);
                            try self.codegen.emit.movssMemReg(.RBP, dest_offset, freg);
                        }
                    } else if (size == 8) {
                        try self.codegen.emitStoreStackF64(dest_offset, freg);
                    } else {
                        unreachable;
                    }
                    return;
                },
                .general_reg => |reg| {
                    if (size <= 8) {
                        switch (size) {
                            1 => try self.emitStoreStackW8(dest_offset, reg),
                            2 => try self.emitStoreStackW16(dest_offset, reg),
                            4 => try self.codegen.emitStoreStack(.w32, dest_offset, reg),
                            else => try self.codegen.emitStoreStack(.w64, dest_offset, reg),
                        }
                    } else {
                        // Large values (> 8 bytes) shouldn't normally be in a general_reg.
                        // Just store the first 8 bytes.
                        try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    }
                    return;
                },
                .stack, .stack_str, .stack_i128, .list_stack => {
                    // Handle stack locations below
                },
                else => {
                    // For other locations, fall through to copyValueToStackOffset
                    try self.copyValueToStackOffset(dest_offset, loc);
                    return;
                },
            }

            // Get the source offset for stack locations
            const src_offset: i32 = switch (loc) {
                .stack => |s| s.offset,
                .stack_str => |off| off,
                .stack_i128 => |off| off,
                .list_stack => |info| info.struct_offset,
                else => unreachable,
            };

            // Copy in 8-byte chunks
            const reg = try self.allocTempGeneral();
            var copied: u32 = 0;
            while (copied + 8 <= size) {
                try self.codegen.emitLoadStack(.w64, reg, src_offset + @as(i32, @intCast(copied)));
                try self.codegen.emitStoreStack(.w64, dest_offset + @as(i32, @intCast(copied)), reg);
                copied += 8;
            }
            // Handle remaining bytes with appropriately-sized loads/stores
            if (size - copied >= 4) {
                try self.codegen.emitLoadStack(.w32, reg, src_offset + @as(i32, @intCast(copied)));
                try self.codegen.emitStoreStack(.w32, dest_offset + @as(i32, @intCast(copied)), reg);
                copied += 4;
            }
            if (size - copied >= 2) {
                try self.emitLoadStackW16(reg, src_offset + @as(i32, @intCast(copied)));

                try self.emitStoreStackW16(dest_offset + @as(i32, @intCast(copied)), reg);
                copied += 2;
            }
            if (size - copied >= 1) {
                try self.emitLoadStackW8(reg, src_offset + @as(i32, @intCast(copied)));

                try self.emitStoreStackW8(dest_offset + @as(i32, @intCast(copied)), reg);
            }
            self.codegen.freeGeneral(reg);
        }

        /// Load from base+offset into register (wraps ldrRegMemSoff / movRegMem)
        fn emitLoad(self: *Self, comptime width: anytype, dst: GeneralReg, base_reg: GeneralReg, offset: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.ldrRegMemSoff(width, dst, base_reg, offset);
            } else {
                try self.codegen.emit.movRegMem(width, dst, base_reg, offset);
            }
        }

        /// Store register to base+offset (wraps strRegMemSoff / movMemReg)
        fn emitStore(self: *Self, comptime width: anytype, base_reg: GeneralReg, offset: i32, src: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.strRegMemSoff(width, src, base_reg, offset);
            } else {
                try self.codegen.emit.movMemReg(width, base_reg, offset, src);
            }
        }

        /// Load byte (8-bit, zero-extended) from base+offset into register
        fn emitLoadW8(self: *Self, dst: GeneralReg, base_reg: GeneralReg, offset: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                if (offset >= -256 and offset <= 255) {
                    try self.codegen.emit.ldurbRegMem(dst, base_reg, @intCast(offset));
                } else {
                    const addr_reg: GeneralReg = if (base_reg == .IP0 or dst == .IP0) .IP1 else .IP0;
                    try self.codegen.emit.movRegImm64(addr_reg, @bitCast(@as(i64, offset)));
                    try self.codegen.emit.addRegRegReg(.w64, addr_reg, base_reg, addr_reg);
                    try self.codegen.emit.ldrbRegMem(dst, addr_reg, 0);
                }
            } else {
                try self.codegen.emit.movzxBRegMem(dst, base_reg, offset);
            }
        }

        /// Load halfword (16-bit, zero-extended) from base+offset into register
        fn emitLoadW16(self: *Self, dst: GeneralReg, base_reg: GeneralReg, offset: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                if (offset >= -256 and offset <= 255) {
                    try self.codegen.emit.ldurhRegMem(dst, base_reg, @intCast(offset));
                } else {
                    const addr_reg: GeneralReg = if (base_reg == .IP0 or dst == .IP0) .IP1 else .IP0;
                    try self.codegen.emit.movRegImm64(addr_reg, @bitCast(@as(i64, offset)));
                    try self.codegen.emit.addRegRegReg(.w64, addr_reg, base_reg, addr_reg);
                    try self.codegen.emit.ldrhRegMem(dst, addr_reg, 0);
                }
            } else {
                try self.codegen.emit.movzxWRegMem(dst, base_reg, offset);
            }
        }

        /// Store byte (8-bit) from register to base+offset
        fn emitStoreW8(self: *Self, base_reg: GeneralReg, offset: i32, src: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                if (offset >= -256 and offset <= 255) {
                    try self.codegen.emit.sturbRegMem(src, base_reg, @intCast(offset));
                } else {
                    const addr_reg: GeneralReg = if (base_reg == .IP0 or src == .IP0) .IP1 else .IP0;
                    try self.codegen.emit.movRegImm64(addr_reg, @bitCast(@as(i64, offset)));
                    try self.codegen.emit.addRegRegReg(.w64, addr_reg, base_reg, addr_reg);
                    try self.codegen.emit.strbRegMem(src, addr_reg, 0);
                }
            } else {
                try self.codegen.emit.movMemReg(.w8, base_reg, offset, src);
            }
        }

        /// Store halfword (16-bit) from register to base+offset
        fn emitStoreW16(self: *Self, base_reg: GeneralReg, offset: i32, src: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                if (offset >= -256 and offset <= 255) {
                    try self.codegen.emit.sturhRegMem(src, base_reg, @intCast(offset));
                } else {
                    const addr_reg: GeneralReg = if (base_reg == .IP0 or src == .IP0) .IP1 else .IP0;
                    try self.codegen.emit.movRegImm64(addr_reg, @bitCast(@as(i64, offset)));
                    try self.codegen.emit.addRegRegReg(.w64, addr_reg, base_reg, addr_reg);
                    try self.codegen.emit.strhRegMem(src, addr_reg, 0);
                }
            } else {
                try self.codegen.emit.movMemReg(.w16, base_reg, offset, src);
            }
        }

        /// dst = src1 + src2 (wraps addRegRegReg / addRegReg)
        /// On x86_64, dst must equal src1 (2-operand form).
        fn emitAddRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.addRegRegReg(width, dst, src1, src2);
            } else {
                try self.codegen.emit.addRegReg(width, dst, src2);
            }
        }

        /// dst = src1 * src2 (wraps mulRegRegReg / imulRegReg)
        /// On x86_64, dst must equal src1 (2-operand form).
        fn emitMulRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.mulRegRegReg(width, dst, src1, src2);
            } else {
                try self.codegen.emit.imulRegReg(width, dst, src2);
            }
        }

        /// dst = src1 - src2 (wraps subRegRegReg / subRegReg)
        /// On x86_64, dst must equal src1 (2-operand form).
        fn emitSubRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.subRegRegReg(width, dst, src1, src2);
            } else {
                try self.codegen.emit.subRegReg(width, dst, src2);
            }
        }

        /// dst = src1 & src2 (wraps andRegRegReg / andRegReg)
        /// On x86_64, dst must equal src1 (2-operand form).
        fn emitAndRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.andRegRegReg(width, dst, src1, src2);
            } else {
                try self.codegen.emit.andRegReg(width, dst, src2);
            }
        }

        /// Shift left by immediate (wraps lslRegRegImm / shlRegImm8)
        fn emitShlImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: u8) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.lslRegRegImm(width, dst, src, @intCast(amount));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.shlRegImm8(width, dst, amount);
            }
        }

        /// Logical shift right by immediate (wraps lsrRegRegImm / shrRegImm8)
        fn emitLsrImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: u8) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.lsrRegRegImm(width, dst, src, @intCast(amount));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.shrRegImm8(width, dst, amount);
            }
        }

        /// Arithmetic shift right by immediate (wraps asrRegRegImm / sarRegImm8)
        fn emitAsrImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: u8) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.asrRegRegImm(width, dst, src, @intCast(amount));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.sarRegImm8(width, dst, amount);
            }
        }

        /// Shift left by register (wraps lslRegReg / shlRegCl)
        fn emitShlReg(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.lslRegReg(width, dst, src, amount);
            } else {
                try self.emitShiftRegX86(width, dst, src, amount, .shl);
            }
        }

        /// Logical shift right by register (wraps lsrRegReg / shrRegCl)
        fn emitLsrReg(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.lsrRegReg(width, dst, src, amount);
            } else {
                try self.emitShiftRegX86(width, dst, src, amount, .shr);
            }
        }

        /// Arithmetic shift right by register (wraps asrRegReg / sarRegCl)
        fn emitAsrReg(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.asrRegReg(width, dst, src, amount);
            } else {
                try self.emitShiftRegX86(width, dst, src, amount, .sar);
            }
        }

        const ShiftOp = enum { shl, shr, sar };

        /// x86_64 shift by register, handling the RCX constraint correctly.
        /// x86_64 shifts require the amount in CL (part of RCX). When dst==RCX,
        /// we must shift into R11 (scratch) and move the result back.
        fn emitShiftRegX86(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: GeneralReg, comptime op: ShiftOp) !void {
            if (dst == .RCX) {
                // RCX is both destination and shift-amount register.
                // Shift into R11 (scratch), then move result to RCX.
                if (src == .RCX and amount == .R11) {
                    // Circular: need RCX→R11 (src) and R11→RCX (amount). Swap.
                    try self.codegen.emit.xchgRegReg(.w64, .RCX, .R11);
                } else if (amount == .R11) {
                    // amount is in R11 — must move to RCX before we use R11 for src
                    try self.codegen.emit.movRegReg(.w64, .RCX, amount);
                    if (src != .R11) try self.codegen.emit.movRegReg(width, .R11, src);
                } else {
                    // Safe: amount is not R11, so moving src to R11 won't clobber amount
                    if (src != .R11) try self.codegen.emit.movRegReg(width, .R11, src);
                    if (amount != .RCX) try self.codegen.emit.movRegReg(.w64, .RCX, amount);
                }
                // R11 has value to shift, CL has shift amount
                switch (op) {
                    .shl => try self.codegen.emit.shlRegCl(width, .R11),
                    .shr => try self.codegen.emit.shrRegCl(width, .R11),
                    .sar => try self.codegen.emit.sarRegCl(width, .R11),
                }
                try self.codegen.emit.movRegReg(width, .RCX, .R11);
            } else {
                // dst is not RCX — safe to shift in-place.
                // Note: if src==RCX, the mov dst←src happens before RCX is overwritten with amount.
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                if (amount != .RCX) try self.codegen.emit.movRegReg(.w64, .RCX, amount);
                switch (op) {
                    .shl => try self.codegen.emit.shlRegCl(width, dst),
                    .shr => try self.codegen.emit.shrRegCl(width, dst),
                    .sar => try self.codegen.emit.sarRegCl(width, dst),
                }
            }
        }

        /// Unsigned saturating subtraction: dst = max(a - b, 0)
        fn emitSaturatingSub(self: *Self, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                // cmp a, b; sub dst, a, b; csel dst, dst, xzr, cs
                // cs (carry set) = no borrow = a >= b
                try self.codegen.emit.cmpRegReg(.w64, a, b);
                try self.codegen.emit.subRegRegReg(.w64, dst, a, b);
                try self.codegen.emit.csel(.w64, dst, dst, .ZRSP, .cs);
            } else {
                // mov dst, a; sub dst, b; jae skip; xor dst, dst; skip:
                // Uses a conditional jump instead of cmov to avoid allocating a zero register.
                if (dst != a) try self.codegen.emit.movRegReg(.w64, dst, a);
                try self.codegen.emit.subRegReg(.w64, dst, b);
                const patch_loc = try self.codegen.emitCondJump(.above_or_equal);
                try self.codegen.emit.xorRegReg(.w64, dst, dst);
                self.codegen.patchJump(patch_loc, self.codegen.currentOffset());
            }
        }

        /// Add immediate to register (wraps addRegRegImm12 / addImm). Always 64-bit.
        fn emitAddImm(self: *Self, dst: GeneralReg, src: GeneralReg, imm: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.addRegRegImm12(.w64, dst, src, @intCast(imm));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(.w64, dst, src);
                try self.codegen.emit.addImm(dst, imm);
            }
        }

        /// Add an arbitrary signed immediate to a pointer register.
        /// Falls back to loading the immediate into a scratch register when the
        /// architecture-specific compact immediate encoding cannot represent it.
        fn emitAddPtrImmAny(self: *Self, dst: GeneralReg, src: GeneralReg, imm: i32) !void {
            if (imm == 0) {
                if (dst != src) try self.codegen.emit.movRegReg(.w64, dst, src);
                return;
            }

            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                if (imm > 0 and imm <= 4095) {
                    try self.codegen.emit.addRegRegImm12(.w64, dst, src, @intCast(imm));
                    return;
                }

                const scratch = try self.allocTempGeneral();
                defer self.codegen.freeGeneral(scratch);
                try self.codegen.emit.movRegImm64(scratch, @bitCast(@as(i64, imm)));
                try self.codegen.emit.addRegRegReg(.w64, dst, src, scratch);
                return;
            }

            try self.emitAddImm(dst, src, imm);
        }

        /// Subtract immediate from register (wraps subRegRegImm12 / subRegImm32)
        fn emitSubImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, imm: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.subRegRegImm12(width, dst, src, @intCast(imm));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.subRegImm32(width, dst, imm);
            }
        }

        /// XOR immediate (wraps eorRegRegImm / xorRegImm8)
        fn emitXorImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, imm: u8) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.eorRegRegImm(width, dst, src, @as(u64, imm));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.xorRegImm8(width, dst, @intCast(imm));
            }
        }

        /// Store register to ptr_reg+byte_offset using unsigned offset addressing.
        /// On aarch64, scales the byte offset to element-sized units for strRegMemUoff.
        /// On x86_64, uses movMemReg with the byte offset directly.
        fn emitStoreToPtr(self: *Self, comptime width: anytype, src: GeneralReg, ptr_reg: GeneralReg, byte_offset: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                const shift = comptime switch (width) {
                    .w64 => @as(u5, 3),
                    .w32 => @as(u5, 2),
                    else => @compileError("Use strhRegMem/strbRegMem for .w16/.w8"),
                };
                const unsigned_offset: u32 = @intCast(byte_offset);
                std.debug.assert(@rem(unsigned_offset, @as(u32, 1) << shift) == 0);
                try self.codegen.emit.strRegMemUoff(width, src, ptr_reg, @intCast(unsigned_offset >> shift));
            } else {
                try self.codegen.emit.movMemReg(width, ptr_reg, byte_offset, src);
            }
        }

        /// Store byte (8-bit) to stack slot (wraps emitStoreStackByte / emitStoreStack(.w8))
        fn emitStoreStackW8(self: *Self, offset: i32, src: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emitStoreStackByte(offset, src);
            } else {
                try self.codegen.emitStoreStack(.w8, offset, src);
            }
        }

        /// Store halfword (16-bit) to stack slot (wraps emitStoreStackHalfword / emitStoreStack(.w16))
        fn emitStoreStackW16(self: *Self, offset: i32, src: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emitStoreStackHalfword(offset, src);
            } else {
                try self.codegen.emitStoreStack(.w16, offset, src);
            }
        }

        /// Load byte (zero-extended to 64-bit) from stack slot
        /// (wraps emitLoadStackByte / movzxBRegMem)
        fn emitLoadStackW8(self: *Self, dst: GeneralReg, offset: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emitLoadStackByte(dst, offset);
            } else {
                try self.codegen.emit.movzxBRegMem(dst, frame_ptr, offset);
            }
        }

        /// Load halfword (zero-extended to 64-bit) from stack slot
        /// (wraps emitLoadStackHalfword / movzxWRegMem)
        fn emitLoadStackW16(self: *Self, dst: GeneralReg, offset: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emitLoadStackHalfword(dst, offset);
            } else {
                try self.codegen.emit.movzxWRegMem(dst, frame_ptr, offset);
            }
        }

        /// Set register to 1 if condition is true, 0 otherwise (wraps cset / setcc+mask)
        fn emitSetCond(self: *Self, dst: GeneralReg, cond: Condition) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.cset(.w64, dst, cond);
            } else {
                try self.codegen.emit.setcc(cond, dst);
                try self.codegen.emit.andRegImm32(dst, 0xFF);
            }
        }

        /// Load effective address of frame_ptr + offset into dst register
        /// (wraps addRegRegImm12 with range check / leaRegMem)
        fn emitLeaStack(self: *Self, dst: GeneralReg, offset: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                if (offset >= 0 and offset <= 4095) {
                    try self.codegen.emit.addRegRegImm12(.w64, dst, frame_ptr, @intCast(offset));
                } else {
                    try self.codegen.emitLoadImm(dst, @intCast(offset));
                    try self.codegen.emit.addRegRegReg(.w64, dst, frame_ptr, dst);
                }
            } else {
                try self.codegen.emit.leaRegMem(dst, frame_ptr, offset);
            }
        }

        /// Adjust stack pointer by adding immediate (for cleaning up spilled args after calls)
        fn emitAddStackPtr(self: *Self, imm: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.addRegRegImm12(.w64, stack_ptr, stack_ptr, @intCast(imm));
            } else {
                try self.codegen.emit.addRegImm32(.w64, stack_ptr, imm);
            }
        }

        /// Copy `size` bytes from src_base+src_offset to dst_base+dst_offset using 8-byte chunks.
        /// For sizes <= 8, does a single 8-byte load/store.
        /// For sizes > 8 that are not multiples of 8, re-copies the final 8 bytes at an
        /// overlapping offset to avoid over-reading the source.
        fn copyChunked(self: *Self, temp_reg: GeneralReg, src_base: GeneralReg, src_offset: i32, dst_base: GeneralReg, dst_offset: i32, size: u32) Allocator.Error!void {
            std.debug.assert(size > 0);
            if (size == 8) {
                try self.emitLoad(.w64, temp_reg, src_base, src_offset);
                try self.emitStore(.w64, dst_base, dst_offset, temp_reg);
                return;
            }
            if (size < 8) {
                // Use exact-sized operations to avoid writing past the destination.
                // At most 3 load/store pairs for sizes 1-7.
                var remaining = size;
                var off: i32 = 0;
                if (remaining >= 4) {
                    try self.emitLoad(.w32, temp_reg, src_base, src_offset + off);
                    try self.emitStore(.w32, dst_base, dst_offset + off, temp_reg);
                    remaining -= 4;
                    off += 4;
                }
                if (remaining >= 2) {
                    try self.emitLoadW16(temp_reg, src_base, src_offset + off);
                    try self.emitStoreW16(dst_base, dst_offset + off, temp_reg);
                    remaining -= 2;
                    off += 2;
                }
                if (remaining >= 1) {
                    try self.emitLoadW8(temp_reg, src_base, src_offset + off);
                    try self.emitStoreW8(dst_base, dst_offset + off, temp_reg);
                }
                return;
            }
            var copied: u32 = 0;
            while (copied + 8 <= size) : (copied += 8) {
                const s = src_offset + @as(i32, @intCast(copied));
                const d = dst_offset + @as(i32, @intCast(copied));
                try self.emitLoad(.w64, temp_reg, src_base, s);
                try self.emitStore(.w64, dst_base, d, temp_reg);
            }
            // Handle tail: if size is not a multiple of 8, re-copy the last 8 bytes
            // at an overlapping offset. This is safe because size > 8.
            if (copied < size) {
                const tail = @as(i32, @intCast(size - 8));
                const s = src_offset + tail;
                const d = dst_offset + tail;
                try self.emitLoad(.w64, temp_reg, src_base, s);
                try self.emitStore(.w64, dst_base, d, temp_reg);
            }
        }

        /// Zero out a stack area
        fn zeroStackArea(self: *Self, offset: i32, size: u32) Allocator.Error!void {
            const reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(reg, 0);

            var remaining = size;
            var current_offset = offset;
            while (remaining >= 8) {
                try self.codegen.emitStoreStack(.w64, current_offset, reg);
                current_offset += 8;
                remaining -= 8;
            }
            // Handle remaining bytes with appropriately-sized stores
            if (remaining >= 4) {
                try self.codegen.emitStoreStack(.w32, current_offset, reg);
                current_offset += 4;
                remaining -= 4;
            }
            if (remaining >= 2) {
                try self.emitStoreStackW16(current_offset, reg);
                current_offset += 2;
                remaining -= 2;
            }
            if (remaining >= 1) {
                try self.emitStoreStackW8(current_offset, reg);
            }

            self.codegen.freeGeneral(reg);
        }

        /// Generate code for a string literal
        fn generateStrLiteral(self: *Self, str_idx: base.StringLiteral.Idx) Allocator.Error!ValueLocation {
            const str_bytes = self.store.getString(str_idx);

            // Allocate space on stack for Roc string representation
            const base_offset = self.codegen.allocStackSlot(roc_str_size);

            if (str_bytes.len < roc_str_size) {
                // Small string optimization: store inline with length in high bit of last byte
                // Format: [data..., length | 0x80] where 0x80 marks it as small string
                var bytes: [roc_str_size]u8 = .{0} ** roc_str_size;
                @memcpy(bytes[0..str_bytes.len], str_bytes);
                bytes[small_str_max_len] = @intCast(str_bytes.len | 0x80); // Set high bit to indicate small string

                // Store as 3 x pointer-sized chunks
                const reg = try self.allocTempGeneral();

                const chunk0: u64 = @bitCast(bytes[0..target_ptr_size].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk0));
                try self.codegen.emitStoreStack(.w64, base_offset, reg);

                const chunk1: u64 = @bitCast(bytes[target_ptr_size .. 2 * target_ptr_size].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk1));
                try self.codegen.emitStoreStack(.w64, base_offset + target_ptr_size, reg);

                const chunk2: u64 = @bitCast(bytes[2 * target_ptr_size .. 3 * target_ptr_size].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk2));
                try self.codegen.emitStoreStack(.w64, base_offset + 2 * target_ptr_size, reg);

                self.codegen.freeGeneral(reg);
            } else {
                if (self.generation_mode == .native_execution and self.static_interner != null) {
                    const interner = self.static_interner.?;
                    const interned = try interner.internString(str_bytes);
                    const ptr_reg = try self.allocTempGeneral();

                    try self.codegen.emitLoadImm(ptr_reg, @intCast(@intFromPtr(interned.ptr)));
                    try self.codegen.emitStoreStack(.w64, base_offset, ptr_reg);

                    try self.codegen.emitLoadImm(ptr_reg, @intCast(interned.len));
                    try self.codegen.emitStoreStack(.w64, base_offset + 8, ptr_reg);
                    try self.codegen.emitStoreStack(.w64, base_offset + 16, ptr_reg);

                    self.codegen.freeGeneral(ptr_reg);
                } else {
                    // Object-file mode cannot use the in-process static interner directly.
                    // The final executable does not share the compiler's arena memory, so
                    // large literals still need the runtime-allocation path here until
                    // dev object files gain real rodata emission for RocStr payloads.
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                    const fn_addr: usize = @intFromPtr(&allocateWithRefcountC);

                    // Allocate stack slot to save the heap pointer
                    const heap_ptr_slot: i32 = self.codegen.allocStackSlot(8);

                    // Allocate string using CallBuilder with automatic R12 handling
                    // Align up to 8 bytes: tail write below stores a full 8-byte word
                    const alloc_size = std.mem.alignForward(usize, str_bytes.len, 8);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addImmArg(@intCast(alloc_size));
                    try builder.addImmArg(1); // byte alignment
                    try builder.addImmArg(0); // elements_refcounted = false
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, fn_addr, .allocate_with_refcount);

                    // Save heap pointer from return register to stack slot
                    try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

                    // Copy string bytes to heap memory
                    // Load heap pointer, then copy bytes
                    const heap_ptr = try self.allocTempGeneral();
                    try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);

                    // Copy string data in 8-byte chunks, then remaining bytes
                    var remaining: usize = str_bytes.len;
                    var str_offset: usize = 0;
                    const temp_reg = try self.allocTempGeneral();

                    while (remaining >= 8) {
                        const chunk: u64 = @bitCast(str_bytes[str_offset..][0..8].*);
                        try self.codegen.emitLoadImm(temp_reg, @bitCast(chunk));
                        try self.emitStore(.w64, heap_ptr, @intCast(str_offset), temp_reg);
                        str_offset += 8;
                        remaining -= 8;
                    }

                    // Handle remaining bytes (1-7 bytes)
                    if (remaining > 0) {
                        var last_chunk: u64 = 0;
                        for (0..remaining) |j| {
                            last_chunk |= @as(u64, str_bytes[str_offset + j]) << @intCast(j * 8);
                        }
                        try self.codegen.emitLoadImm(temp_reg, @bitCast(last_chunk));
                        // Store partial - for simplicity, store as full 8 bytes (heap has space)
                        try self.emitStore(.w64, heap_ptr, @intCast(str_offset), temp_reg);
                    }

                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(heap_ptr);

                    // Construct RocStr struct on stack: {pointer, length, capacity}
                    // Reload heap pointer for struct construction
                    const ptr_reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, heap_ptr_slot);

                    // Store pointer (first 8 bytes)
                    try self.codegen.emitStoreStack(.w64, base_offset, ptr_reg);

                    // Store length (second 8 bytes)
                    try self.codegen.emitLoadImm(ptr_reg, @intCast(str_bytes.len));
                    try self.codegen.emitStoreStack(.w64, base_offset + 8, ptr_reg);

                    // Store capacity (third 8 bytes) - same as length for immutable strings
                    // No need to reload, length is still in ptr_reg
                    try self.codegen.emitStoreStack(.w64, base_offset + 16, ptr_reg);

                    self.codegen.freeGeneral(ptr_reg);
                }
            }

            return .{ .stack_str = base_offset };
        }

        /// Generate code for a for loop over a list
        /// Iterates over each element, binding it to the pattern and executing the body
        fn generateForLoop(self: *Self, for_loop: anytype) Allocator.Error!ValueLocation {
            // Get the list location
            const list_loc = try self.generateExpr(for_loop.list_expr);

            // Handle empty list represented as immediate 0
            // Empty lists have null pointer and 0 length, so the loop body never executes
            if (list_loc == .immediate_i64 and list_loc.immediate_i64 == 0) {
                // Empty list - loop executes 0 times, just return unit
                return .{ .immediate_i64 = 0 };
            }

            // Get list pointer and length
            const list_base: i32 = switch (list_loc) {
                .stack => |s| s.offset,
                .stack_str => |off| off,
                .list_stack => |list_info| list_info.struct_offset,
                else => unreachable,
            };

            // Get element layout and size from LIR.
            const ls = self.layout_store;
            if (builtin.mode == .Debug) {
                if (@intFromEnum(for_loop.elem_layout) >= ls.layouts.len()) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: for_loop.elem_layout out of bounds ({d} >= {d})",
                        .{ @intFromEnum(for_loop.elem_layout), ls.layouts.len() },
                    );
                }

                const list_layout_idx = self.exprLayout(for_loop.list_expr);
                const list_layout = ls.getLayout(list_layout_idx);
                switch (list_layout.tag) {
                    .list => {
                        if (list_layout.data.list != for_loop.elem_layout) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: for_loop elem layout mismatch (loop={d}, list={d})",
                                .{ @intFromEnum(for_loop.elem_layout), @intFromEnum(list_layout.data.list) },
                            );
                        }
                    },
                    .list_of_zst => {
                        const elem_size = ls.layoutSizeAlign(ls.getLayout(for_loop.elem_layout)).size;
                        if (elem_size != 0) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: list_of_zst used with non-ZST for_loop elem layout {d}",
                                .{@intFromEnum(for_loop.elem_layout)},
                            );
                        }
                    },
                    else => {
                        std.debug.panic(
                            "LIR/codegen invariant violated: for_loop list_expr must be list/list_of_zst, got {s}",
                            .{@tagName(list_layout.tag)},
                        );
                    },
                }
            }

            const elem_layout = ls.getLayout(for_loop.elem_layout);
            const elem_size: u32 = ls.layoutSizeAlign(elem_layout).size;
            // ZST elements (size 0) are valid - they have no data but we still iterate
            std.debug.assert(elem_size <= 1024 * 1024); // Sanity check: < 1MB

            const is_zst = elem_size == 0;

            // CRITICAL: Store loop state on stack, not in registers!
            // The loop body may call C functions which clobber caller-saved registers.
            // We allocate stack slots for: ptr, len, idx (and elem_slot only for non-ZST)
            const ptr_slot = self.codegen.allocStackSlot(8);
            const len_slot = self.codegen.allocStackSlot(8);
            const idx_slot = self.codegen.allocStackSlot(8);
            // Only allocate element slot for non-ZST elements
            const elem_slot: i32 = if (is_zst) 0 else self.codegen.allocStackSlot(@intCast(elem_size));

            // Initialize loop state on stack
            {
                const temp = try self.allocTempGeneral();
                // Copy ptr from list struct to ptr_slot
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, temp, .FP, list_base);
                    try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, ptr_slot);
                    // Copy len from list struct to len_slot
                    try self.codegen.emit.ldrRegMemSoff(.w64, temp, .FP, list_base + 8);
                    try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, len_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, temp, .RBP, list_base);
                    try self.codegen.emit.movMemReg(.w64, .RBP, ptr_slot, temp);
                    try self.codegen.emit.movRegMem(.w64, temp, .RBP, list_base + 8);
                    try self.codegen.emit.movMemReg(.w64, .RBP, len_slot, temp);
                }
                // Initialize idx to 0
                try self.codegen.emitLoadImm(temp, 0);
                try self.emitStore(.w64, frame_ptr, idx_slot, temp);
                self.codegen.freeGeneral(temp);
            }

            // Record loop start position for the backward jump
            const loop_start = self.codegen.currentOffset();

            // Load idx and len from stack, compare
            {
                const idx_reg = try self.allocTempGeneral();
                const len_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, idx_reg, frame_ptr, idx_slot);
                try self.emitLoad(.w64, len_reg, frame_ptr, len_slot);
                try self.emitCmpReg(idx_reg, len_reg);
                self.codegen.freeGeneral(idx_reg);
                self.codegen.freeGeneral(len_reg);
            }

            // Jump to end if index >= length (we'll patch this later)
            const exit_patch = try self.emitJumpIfGreaterOrEqual();

            // Load current element from list[idx] to elem_slot (skip for ZST)
            // Calculate element address: ptr + idx * elem_size
            if (!is_zst) {
                const ptr_reg = try self.allocTempGeneral();
                const idx_reg = try self.allocTempGeneral();
                const addr_reg = try self.allocTempGeneral();

                try self.emitLoad(.w64, ptr_reg, frame_ptr, ptr_slot);
                try self.emitLoad(.w64, idx_reg, frame_ptr, idx_slot);

                try self.codegen.emit.movRegReg(.w64, addr_reg, idx_reg);

                // Multiply by element size
                if (elem_size != 1) {
                    const size_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(size_reg, elem_size);
                    try self.emitMulRegs(.w64, addr_reg, addr_reg, size_reg);
                    self.codegen.freeGeneral(size_reg);
                }

                // Add base pointer
                try self.emitAddRegs(.w64, addr_reg, addr_reg, ptr_reg);

                // Load element to stack slot
                const temp_reg = try self.allocTempGeneral();
                if (elem_size <= 8) {
                    try self.emitLoad(.w64, temp_reg, addr_reg, 0);
                    try self.emitStore(.w64, frame_ptr, elem_slot, temp_reg);
                } else {
                    // For larger elements, copy in 8-byte chunks
                    try self.copyChunked(temp_reg, addr_reg, 0, frame_ptr, elem_slot, elem_size);
                }
                self.codegen.freeGeneral(temp_reg);
                self.codegen.freeGeneral(addr_reg);
                self.codegen.freeGeneral(idx_reg);
                self.codegen.freeGeneral(ptr_reg);
            }

            // For ZST elements, bind to immediate 0 (no actual data)
            const elem_loc: ValueLocation = if (is_zst) .{ .immediate_i64 = 0 } else self.stackLocationForLayout(for_loop.elem_layout, elem_slot);
            try self.bindPattern(for_loop.elem_pattern, elem_loc);

            // Save break patches length before body generation
            const saved_break_patches_len = self.loop_break_patches.items.len;

            // Execute the body (result is discarded)
            // NOTE: This may call C functions which clobber all caller-saved registers
            {
                _ = try self.generateExpr(for_loop.body);
            }

            // Increment index (load from stack, increment, store back)
            {
                const idx_reg = try self.allocTempGeneral();
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, idx_reg, .FP, idx_slot);
                    const one_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(one_reg, 1);
                    try self.codegen.emit.addRegRegReg(.w64, idx_reg, idx_reg, one_reg);
                    self.codegen.freeGeneral(one_reg);
                    try self.codegen.emit.strRegMemSoff(.w64, idx_reg, .FP, idx_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, idx_reg, .RBP, idx_slot);
                    try self.codegen.emit.addRegImm(.w64, idx_reg, 1);
                    try self.codegen.emit.movMemReg(.w64, .RBP, idx_slot, idx_reg);
                }
                self.codegen.freeGeneral(idx_reg);
            }

            // Jump back to loop start
            try self.emitJumpBackward(loop_start);

            // Patch the exit jump to point here
            const loop_exit_offset = self.codegen.currentOffset();
            self.codegen.patchJump(exit_patch, loop_exit_offset);

            // Patch break jumps to loop exit
            for (self.loop_break_patches.items[saved_break_patches_len..]) |patch| {
                self.codegen.patchJump(patch, loop_exit_offset);
            }
            self.loop_break_patches.shrinkRetainingCapacity(saved_break_patches_len);

            // For loops return unit (empty record)
            return .{ .immediate_i64 = 0 };
        }

        /// Generate code for a while loop
        /// Executes body while condition is true
        fn generateWhileLoop(self: *Self, while_loop: anytype) Allocator.Error!ValueLocation {
            // Record loop start position for the backward jump
            const loop_start = self.codegen.currentOffset();

            // Evaluate condition
            const cond_loc = try self.generateExpr(while_loop.cond);

            // Get condition value into a register for comparison
            const cond_reg = switch (cond_loc) {
                .immediate_i64 => |val| blk: {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @intCast(val));
                    break :blk reg;
                },
                .stack => |s| blk: {
                    const off = s.offset;
                    const reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, reg, frame_ptr, off);
                    break :blk reg;
                },
                .general_reg => |r| blk: {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emit.movRegReg(.w64, reg, r);
                    break :blk reg;
                },
                else => unreachable,
            };

            // While loop condition is Bool (1 byte). When loaded from a mutable
            // variable's stack slot, upper bytes may contain uninitialized data.
            // Mask to the low byte to ensure correct zero-comparison.
            if (comptime target.toCpuArch() == .aarch64) {
                const mask_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(mask_reg, 0xFF);
                try self.codegen.emit.andRegRegReg(.w64, cond_reg, cond_reg, mask_reg);
                self.codegen.freeGeneral(mask_reg);
            } else {
                try self.codegen.emit.andRegImm32(cond_reg, 0xFF);
            }

            // Compare condition with 0 (false)
            const zero_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(zero_reg, 0);
            try self.emitCmpReg(cond_reg, zero_reg);
            self.codegen.freeGeneral(zero_reg);
            self.codegen.freeGeneral(cond_reg);

            // Jump to end if condition is false (equal to 0)
            const exit_patch = try self.emitJumpIfEqual();

            // Save break patches length before body generation
            const saved_break_patches_len = self.loop_break_patches.items.len;

            // Execute the body (result is discarded)
            _ = try self.generateExpr(while_loop.body);

            // Jump back to loop start (to re-evaluate condition)
            try self.emitJumpBackward(loop_start);

            // Patch the exit jump to point here
            const loop_exit_offset = self.codegen.currentOffset();
            self.codegen.patchJump(exit_patch, loop_exit_offset);

            // Patch break jumps to loop exit
            for (self.loop_break_patches.items[saved_break_patches_len..]) |patch| {
                self.codegen.patchJump(patch, loop_exit_offset);
            }
            self.loop_break_patches.shrinkRetainingCapacity(saved_break_patches_len);

            // While loops return unit (empty record)
            return .{ .immediate_i64 = 0 };
        }

        /// Generate code for early return
        fn generateEarlyReturn(self: *Self, er: anytype) Allocator.Error!ValueLocation {
            // Generate the return value
            const value_loc = try self.generateExpr(er.expr);

            // We must be inside deferred-prologue proc compilation: early returns
            // require the jump-to-epilogue infrastructure that path sets up.
            const ret_layout = self.early_return_ret_layout orelse unreachable;

            // Zero-sized return: nothing to move, just jump to epilogue
            if (self.getLayoutSize(ret_layout) == 0) {
                const patch = try self.codegen.emitJump();
                try self.early_return_patches.append(self.allocator, patch);
                return .{ .immediate_i64 = 0 };
            }

            const return_loc = self.normalizeResultLocForLayout(value_loc, ret_layout);
            const preserved_return_loc = return_loc;

            // Move the preserved value to the return register (or copy to return pointer)
            if (self.ret_ptr_slot) |ret_slot| {
                try self.copyResultToReturnPointer(preserved_return_loc, ret_layout, ret_slot);
            } else {
                try self.moveToReturnRegisterWithLayout(preserved_return_loc, ret_layout);
            }
            // Emit a jump (will be patched to the epilogue location)
            const patch = try self.codegen.emitJump();
            try self.early_return_patches.append(self.allocator, patch);
            // Return a dummy value — this code is unreachable at runtime
            return .{ .immediate_i64 = 0 };
        }

        /// Generate code for break expression (exits enclosing loop)
        fn generateBreak(self: *Self) Allocator.Error!ValueLocation {
            // Emit a forward jump (will be patched to loop exit)
            const patch = try self.codegen.emitJump();
            try self.loop_break_patches.append(self.allocator, patch);
            // Return a dummy value — code after break is unreachable
            return .{ .immediate_i64 = 0 };
        }

        /// Generate code for dbg expression (prints and returns value)
        fn generateDbg(self: *Self, dbg_expr: anytype) Allocator.Error!ValueLocation {
            // dbg evaluates its expression and returns the value.
            // Debug printing is handled by the interpreter side in tests.
            return try self.generateExpr(dbg_expr.expr);
        }

        /// Generate code for expect expression (assertion)
        fn generateExpect(self: *Self, expect_expr: anytype) Allocator.Error!ValueLocation {
            // Evaluate the condition
            const cond_loc = try self.generateExpr(expect_expr.cond);
            const cond_reg = try self.ensureInGeneralReg(cond_loc);

            // Check if condition is true (non-zero); if false, abort
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.cmpRegImm12(.w64, cond_reg, 0);
            } else {
                try self.codegen.emit.testRegReg(.w8, cond_reg, cond_reg);
            }
            self.codegen.freeGeneral(cond_reg);

            // Jump over abort call if condition is true (non-zero)
            const skip_patch = try self.codegen.emitCondJump(condNotEqual());

            // Condition was false: call roc_crashed via RocOps, then trap.
            // This path must be terminal even if roc_crashed were to return.
            try self.emitRocCrash("expect failed");
            try self.emitTrap();

            // Patch the skip jump to land here
            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());

            // Evaluate and return the body
            return try self.generateExpr(expect_expr.body);
        }

        /// Emit a roc_crashed call via RocOps with a static message.
        /// Used for runtime_error expressions (dead code paths that should
        /// never execute, e.g. the Err branch of `?` at the top level).
        ///
        /// The message bytes are stored on the stack so that this works in both
        /// native execution and object file modes (compile-time pointers are
        /// invalid in the final executable).
        fn emitRocCrash(self: *Self, msg: []const u8) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            // Allocate stack space for the message bytes
            const msg_aligned_size: u32 = std.mem.alignForward(u32, @intCast(msg.len), 8);
            const msg_slot = self.codegen.allocStackSlot(if (msg_aligned_size == 0) 8 else msg_aligned_size);

            // Allocate a 16-byte stack slot for the RocCrashed struct { utf8_bytes, len }
            const crashed_slot = self.codegen.allocStackSlot(16);

            {
                const base_reg = frame_ptr;
                const tmp = try self.allocTempGeneral();

                // Store message bytes on the stack in 8-byte chunks
                var offset: u32 = 0;
                while (offset < msg.len) {
                    const remaining = msg.len - offset;
                    if (remaining >= 8) {
                        const chunk: u64 = @bitCast(msg[offset..][0..8].*);
                        try self.codegen.emitLoadImm(tmp, @bitCast(chunk));
                        try self.emitStore(.w64, base_reg, msg_slot + @as(i32, @intCast(offset)), tmp);
                    } else {
                        // Handle the last partial chunk byte-by-byte
                        // Build a zero-padded 8-byte value
                        var padded: [8]u8 = .{0} ** 8;
                        @memcpy(padded[0..remaining], msg[offset..][0..remaining]);
                        const chunk: u64 = @bitCast(padded);
                        try self.codegen.emitLoadImm(tmp, @bitCast(chunk));
                        try self.emitStore(.w64, base_reg, msg_slot + @as(i32, @intCast(offset)), tmp);
                    }
                    offset += 8;
                }

                // Store pointer to stack-resident message bytes at RocCrashed offset 0
                try self.emitLeaStack(tmp, msg_slot);
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.strRegMemSoff(.w64, tmp, base_reg, crashed_slot);
                } else {
                    try self.codegen.emit.movMemReg(.w64, base_reg, crashed_slot, tmp);
                }

                // Store len at offset 8
                const msg_len_val: i64 = @bitCast(@as(u64, msg.len));
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emitLoadImm(tmp, msg_len_val);
                    try self.codegen.emit.strRegMemSoff(.w64, tmp, base_reg, crashed_slot + 8);
                } else {
                    try self.codegen.emit.movRegImm64(tmp, @bitCast(@as(u64, msg.len)));
                    try self.codegen.emit.movMemReg(.w64, base_reg, crashed_slot + 8, tmp);
                }

                // Load roc_crashed fn pointer from RocOps offset 48
                // Use a register that won't conflict with CallBuilder's SCRATCH_REG
                const fn_ptr_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X10 else .RAX;
                try self.emitLoad(.w64, fn_ptr_reg, roc_ops_reg, 48);

                // Use CallBuilder for args and call
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, crashed_slot);
                try builder.addMemArg(roc_ops_reg, 0); // env from RocOps offset 0
                try builder.callReg(fn_ptr_reg);

                self.codegen.freeGeneral(tmp);
            }
        }

        /// Emit a hardware trap instruction (ud2 on x86_64, brk on aarch64).
        /// Used after crash/runtime_error to guarantee the program never continues.
        fn emitTrap(self: *Self) Allocator.Error!void {
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.brk();
            } else {
                try self.codegen.emit.ud2();
            }
        }

        /// Generate code for string concatenation
        fn generateStrConcat(self: *Self, exprs: anytype) Allocator.Error!ValueLocation {
            const expr_ids = self.store.getExprSpan(exprs);
            if (expr_ids.len == 0) {
                // Empty concat returns empty string
                return try self.generateEmptyString();
            }
            if (expr_ids.len == 1) {
                // Single element, just return it
                return try self.generateExpr(expr_ids[0]);
            }

            // Multi-element: fold-left concatenation
            // result = concat(concat(...concat(a, b), c), ...)
            var acc_loc = try self.generateExpr(expr_ids[0]);
            var acc_off = try self.ensureOnStack(acc_loc, roc_str_size);

            for (expr_ids[1..]) |next_expr| {
                const next_loc = try self.generateExpr(next_expr);
                const next_off = try self.ensureOnStack(next_loc, roc_str_size);
                acc_loc = try self.callStr2RocOpsToStr(acc_off, next_off, @intFromPtr(&wrapStrConcat), .str_concat);
                acc_off = try self.ensureOnStack(acc_loc, roc_str_size);
            }

            return acc_loc;
        }

        /// Generate an empty string
        fn generateEmptyString(self: *Self) Allocator.Error!ValueLocation {
            // Empty small string in Roc format: all zeros except byte 23 = 0x80
            // (small string flag set, length 0)
            const str_slot = self.codegen.allocStackSlot(roc_str_size);
            const zero_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(zero_reg, 0);

            try self.emitStore(.w64, frame_ptr, str_slot, zero_reg);
            try self.emitStore(.w64, frame_ptr, str_slot + 8, zero_reg);

            // Byte 23 = 0x80 (small string flag, length 0)
            // In little-endian, bytes 16-23 as u64: 0x80 << 56 = 0x8000000000000000
            const small_str_flag: i64 = @bitCast(@as(u64, 0x80) << 56);
            try self.codegen.emitLoadImm(zero_reg, small_str_flag);
            try self.emitStore(.w64, frame_ptr, str_slot + 16, zero_reg);

            self.codegen.freeGeneral(zero_reg);
            return .{ .stack_str = str_slot };
        }

        /// Generate code for int_to_str by calling the unified wrapper
        fn generateIntToStr(self: *Self, its: anytype) Allocator.Error!ValueLocation {
            const val_loc = try self.generateExpr(its.value);
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_int_to_str);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const base_reg = frame_ptr;

            const int_width: u8 = @intCast(its.int_precision.size());
            const is_signed: bool = switch (its.int_precision) {
                .i8, .i16, .i32, .i64, .i128 => true,
                .u8, .u16, .u32, .u64, .u128 => false,
            };

            // Get low and high u64 parts of the value
            var val_low: GeneralReg = undefined;
            var val_high: GeneralReg = undefined;
            if (int_width <= 8) {
                val_low = try self.ensureInGeneralReg(val_loc);
                val_high = try self.allocTempGeneral();
                if (is_signed) {
                    // Sign-extend: arithmetic shift right by 63
                    try self.emitMovRegReg(val_high, val_low);
                    try self.emitAsrImm(.w64, val_high, val_high, 63);
                } else {
                    try self.codegen.emitLoadImm(val_high, 0);
                }
            } else {
                // 128-bit value
                const parts = try self.getI128Parts(val_loc, if (is_signed) .signed else .unsigned);
                val_low = parts.low;
                val_high = parts.high;
            }

            // roc_builtins_int_to_str(out, val_low, val_high, int_width, is_signed, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_offset);
            try builder.addRegArg(val_low);
            try builder.addRegArg(val_high);
            try builder.addImmArg(int_width);
            try builder.addImmArg(@intFromBool(is_signed));
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .int_to_str);

            self.codegen.freeGeneral(val_low);
            self.codegen.freeGeneral(val_high);

            return .{ .stack_str = result_offset };
        }

        /// Generate code for float_to_str by calling the unified wrapper
        fn generateFloatToStr(self: *Self, fts: anytype) Allocator.Error!ValueLocation {
            const val_loc = try self.generateExpr(fts.value);
            // Dec uses a dedicated helper with explicit u64 decomposition to avoid
            // platform-specific i128 calling convention issues
            if (fts.float_precision == .dec) {
                return try self.callDecToStrWrapped(val_loc);
            }
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_float_to_str);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const base_reg = frame_ptr;

            // Get float value as u64 bits.
            // F32 values are carried through codegen as widened F64s, so convert them
            // back to real F32 payload bits before calling the wrapper.
            const val_bits_reg = if (fts.float_precision == .f32)
                try self.materializeF32BitsInGeneralReg(val_loc)
            else
                try self.ensureInGeneralReg(val_loc);
            const is_f32: bool = (fts.float_precision == .f32);
            if (val_loc == .float_reg) {
                self.codegen.freeFloat(val_loc.float_reg);
            }

            // roc_builtins_float_to_str(out, val_bits, is_f32, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_offset);
            try builder.addRegArg(val_bits_reg);
            self.codegen.freeGeneral(val_bits_reg);
            try builder.addImmArg(@intFromBool(is_f32));
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .float_to_str);

            return .{ .stack_str = result_offset };
        }

        /// Generate code for dec_to_str by calling the wrapper with decomposed i128
        fn generateDecToStr(self: *Self, expr_id: anytype) Allocator.Error!ValueLocation {
            const val_loc = try self.generateExpr(expr_id);
            return try self.callDecToStrWrapped(val_loc);
        }

        /// Decomposed i128 value for passing to wrapDecToStr.
        /// Uses explicit low/high u64 values to avoid platform-specific i128 ABI issues.
        const DecomposedI128 = union(enum) {
            /// Both halves are on stack at consecutive offsets
            on_stack: i32, // low at offset, high at offset+8
            /// Both halves are compile-time immediates
            immediate: struct { low: u64, high: u64 },
        };

        /// Extract low and high u64 halves from a Dec/i128 ValueLocation.
        /// Returns a DecomposedI128 that can be used to pass to wrapDecToStr.
        fn decomposeI128Value(self: *Self, val_loc: ValueLocation) Allocator.Error!DecomposedI128 {
            return switch (val_loc) {
                // 128-bit value already on stack - most common case for Dec
                .stack_i128 => |offset| .{ .on_stack = offset },

                // Generic stack location with 16 bytes
                .stack => |s| .{ .on_stack = s.offset },

                // Compile-time known i128 value
                .immediate_i128 => |val| .{
                    .immediate = .{
                        .low = @truncate(@as(u128, @bitCast(val))),
                        .high = @truncate(@as(u128, @bitCast(val)) >> 64),
                    },
                },

                // 64-bit immediate - sign-extend to i128
                .immediate_i64 => |val| .{
                    .immediate = .{
                        .low = @bitCast(val),
                        .high = if (val < 0) @as(u64, @bitCast(@as(i64, -1))) else 0,
                    },
                },

                // Value in a general register - store to stack first, high is 0
                .general_reg => |reg| {
                    const val_slot = self.codegen.allocStackSlot(16);
                    try self.emitStore(.w64, frame_ptr, val_slot, reg);
                    // Zero out high half
                    try self.codegen.emitLoadImm(scratch_reg, 0);
                    try self.emitStore(.w64, frame_ptr, val_slot + 8, scratch_reg);
                    self.codegen.freeGeneral(reg);
                    return .{ .on_stack = val_slot };
                },

                // These types should never appear for Dec values
                .float_reg => unreachable, // Dec is not a float register type
                .stack_str => unreachable, // Dec is not a string
                .list_stack => unreachable, // Dec is not a list
                .immediate_f64 => unreachable, // Dec is not a float
                .noreturn => unreachable,
            };
        }

        /// Call wrapDecToStr with explicitly decomposed i128 arguments.
        /// This avoids platform-specific i128 calling conventions by passing
        /// (out: *RocStr, low: u64, high: u64, roc_ops: *RocOps) uniformly.
        /// Uses CallBuilder for cross-platform argument setup and callBuiltin
        /// to support both native execution and object file generation modes.
        fn callDecToStrWrapped(self: *Self, val_loc: ValueLocation) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const decomposed = try self.decomposeI128Value(val_loc);
            const base_reg = frame_ptr;

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_offset);

            switch (decomposed) {
                .on_stack => |offset| {
                    try builder.addMemArg(base_reg, offset);
                    try builder.addMemArg(base_reg, offset + 8);
                },
                .immediate => |imm| {
                    try builder.addImmArg(@bitCast(imm.low));
                    try builder.addImmArg(@bitCast(imm.high));
                },
            }

            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, @intFromPtr(&wrapDecToStr), .dec_to_str);

            return .{ .stack_str = result_offset };
        }

        /// Generate code for str_escape_and_quote
        fn generateStrEscapeAndQuote(self: *Self, expr_id: anytype) Allocator.Error!ValueLocation {
            const str_loc = try self.generateExpr(expr_id);
            const str_off = try self.ensureOnStack(str_loc, roc_str_size);
            return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrEscapeAndQuote), .str_escape_and_quote, .str);
        }

        /// Generate code for discriminant switch.
        /// Switches on the discriminant of a tag union value and generates the
        /// corresponding branch expression for the matching variant.
        fn generateDiscriminantSwitch(self: *Self, ds: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const branches = self.store.getExprSpan(ds.branches);
            if (branches.len == 0) {
                unreachable;
            }

            // Single branch — generate it directly, no dispatch needed
            if (branches.len == 1) {
                return try self.generateExpr(branches[0]);
            }

            // Generate the tag union value
            const value_loc = try self.generateExpr(ds.value);
            const union_layout = ls.getLayout(ds.union_layout);

            // Load the discriminant into a register
            const tag_reg = try self.allocTempGeneral();

            if (union_layout.tag == .tag_union) {
                // Tag union in memory — load discriminant from its offset
                const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
                const disc_offset: i32 = @intCast(tu_data.discriminant_offset);
                if (tu_data.discriminant_size == 0) {
                    try self.codegen.emitLoadImm(tag_reg, 0);
                } else {
                    const disc_size = ValueSize.fromByteCount(tu_data.discriminant_size);

                    const base_offset: i32 = switch (value_loc) {
                        .stack => |s| s.offset,
                        .stack_str => |off| off,
                        else => unreachable,
                    };

                    try self.emitSizedLoadStack(tag_reg, base_offset + disc_offset, disc_size);
                }
            } else if (union_layout.tag == .scalar or union_layout.tag == .zst) {
                // Scalar/ZST — the value itself IS the discriminant
                switch (value_loc) {
                    .general_reg => |reg| {
                        if (reg != tag_reg) {
                            try self.codegen.emit.movRegReg(.w64, tag_reg, reg);
                        }
                    },
                    .immediate_i64 => |val| {
                        try self.codegen.emitLoadImm(tag_reg, @bitCast(val));
                    },
                    .stack => |s| {
                        try self.emitSizedLoadStack(tag_reg, s.offset, s.size);
                    },
                    else => unreachable,
                }
            } else {
                unreachable;
            }

            // Allocate result slot sized to the result layout
            const result_size = self.getLayoutSize(ds.result_layout);
            const result_slot = self.codegen.allocStackSlot(result_size);

            // Track end jumps for patching
            var end_jumps = std.ArrayList(usize).empty;
            defer end_jumps.deinit(self.allocator);

            for (branches, 0..) |branch_expr, i| {
                const is_last = (i == branches.len - 1);

                if (!is_last) {
                    // Compare discriminant with this branch index
                    try self.emitCmpImm(tag_reg, @intCast(i));
                    const skip_jump = try self.emitJumpIfNotEqual();

                    // Generate code for this branch
                    const result = try self.generateExpr(branch_expr);
                    try self.copyToStackSlot(result_slot, result, result_size);

                    // Jump to end
                    try end_jumps.append(self.allocator, try self.codegen.emitJump());

                    // Patch skip_jump to here
                    self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
                } else {
                    // Last case — no comparison needed (fallthrough)
                    const result = try self.generateExpr(branch_expr);
                    try self.copyToStackSlot(result_slot, result, result_size);
                }
            }

            // Patch all end jumps to current location
            for (end_jumps.items) |jump| {
                self.codegen.patchJump(jump, self.codegen.currentOffset());
            }

            self.codegen.freeGeneral(tag_reg);
            return self.fieldLocationFromLayout(result_slot, result_size, ds.result_layout);
        }

        /// Extract the payload from a tag union value.
        /// The payload is always at offset 0 in the tag union memory.
        fn generateTagPayloadAccess(self: *Self, tpa: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;

            // Generate the tag union value
            const raw_value_loc = try self.generateExpr(tpa.value);

            const union_layout = ls.getLayout(tpa.union_layout);
            const payload_layout = ls.getLayout(tpa.payload_layout);
            const payload_size = ls.layoutSizeAlign(payload_layout).size;

            if (union_layout.tag == .tag_union) {
                const value_loc = try self.materializeValueToStackForLayout(raw_value_loc, tpa.union_layout);
                // Payload is at offset 0 within the stack-allocated tag union
                const base_offset: i32 = switch (value_loc) {
                    .stack => |s| s.offset,
                    .stack_i128 => |off| off,
                    .stack_str => |off| off,
                    .list_stack => |ls_info| ls_info.struct_offset,
                    else => unreachable,
                };
                const payload_loc = self.fieldLocationFromLayout(base_offset, payload_size, tpa.payload_layout);
                return payload_loc;
            } else if (union_layout.tag == .box) {
                // Boxed tag union: dereference the pointer, then copy payload from heap
                const inner_layout = ls.getLayout(union_layout.data.box);
                if (inner_layout.tag == .tag_union) {
                    const box_ptr_reg = try self.ensureInGeneralReg(raw_value_loc);

                    // Copy payload from heap to stack
                    const dest_offset = self.codegen.allocStackSlot(payload_size);
                    var copied: u32 = 0;
                    while (copied < payload_size) {
                        const temp_reg = try self.allocTempGeneral();
                        try self.emitLoad(.w64, temp_reg, box_ptr_reg, @intCast(copied));
                        try self.emitStore(.w64, frame_ptr, dest_offset + @as(i32, @intCast(copied)), temp_reg);
                        self.codegen.freeGeneral(temp_reg);
                        copied += 8;
                    }
                    self.codegen.freeGeneral(box_ptr_reg);
                    return self.fieldLocationFromLayout(dest_offset, payload_size, tpa.payload_layout);
                } else {
                    // Box of scalar/ZST — the value is the payload directly
                    return raw_value_loc;
                }
            } else if (union_layout.tag == .scalar or union_layout.tag == .zst) {
                // Scalar/ZST unions: the value itself is the payload (no indirection)
                return raw_value_loc;
            } else {
                unreachable;
            }
        }

        /// Helper to store a result to a stack slot
        fn storeResultToSlot(self: *Self, slot: i32, loc: ValueLocation, slot_size: u32) Allocator.Error!void {
            switch (loc) {
                .noreturn => return,
                else => try self.copyBytesToStackOffset(slot, loc, slot_size),
            }
        }

        /// Emit a compare of two registers
        fn emitCmpReg(self: *Self, reg1: GeneralReg, reg2: GeneralReg) Allocator.Error!void {
            try self.codegen.emit.cmpRegReg(.w64, reg1, reg2);
        }

        /// Emit a jump if greater or equal (for unsigned comparison)
        fn emitJumpIfGreaterOrEqual(self: *Self) Allocator.Error!usize {
            if (comptime target.toCpuArch() == .aarch64) {
                // B.CS (branch if carry set = unsigned higher or same) with placeholder offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.cs, 0);
                return patch_loc;
            } else {
                // JAE (jump if unsigned above or equal) with placeholder offset
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jae(@bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Emit a backward jump to a known location
        fn emitJumpBackward(self: *Self, jump_target: usize) Allocator.Error!void {
            const current = self.codegen.currentOffset();
            // Calculate offset - need to account for instruction encoding
            if (comptime target.toCpuArch() == .aarch64) {
                // aarch64 b instruction: offset is in words (4 bytes), relative to PC
                const byte_offset = @as(i32, @intCast(jump_target)) - @as(i32, @intCast(current));
                try self.codegen.emit.b(byte_offset);
            } else {
                // x86_64: jmp rel32 - offset is relative to end of instruction
                const inst_size: i32 = 5; // JMP rel32 is 5 bytes
                const byte_offset = @as(i32, @intCast(jump_target)) - @as(i32, @intCast(current)) - inst_size;
                try self.codegen.emit.jmpRel32(byte_offset);
            }
        }

        /// Store a discriminant value at the given offset
        fn storeDiscriminant(self: *Self, offset: i32, value: u16, disc_size: u8) Allocator.Error!void {
            if (disc_size == 0) return;

            const reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(reg, value);

            // Store appropriate size - architecture specific
            if (comptime target.toCpuArch() == .aarch64) {
                // aarch64 only has .w32 and .w64 for emitStoreStack, use direct emit for smaller sizes.
                // The offset >= 0 checks below are not defensive guards — they select the
                // unsigned-immediate instruction encoding (STRB/STRH), which only accepts
                // non-negative offsets. Negative offsets (valid because the stack grows down
                // from FP) take the fallback path that computes the address in a register.
                switch (disc_size) {
                    1 => {
                        // Use strb for 1-byte store
                        if (offset >= 0 and offset <= 4095) {
                            try self.codegen.emit.strbRegMem(reg, .FP, @intCast(offset));
                        } else {
                            // For negative/large offsets, compute address first
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                            try self.codegen.emit.strbRegMem(reg, .IP0, 0);
                        }
                    },
                    2 => {
                        // Use strh for 2-byte store
                        if (offset >= 0 and offset <= 8190) {
                            try self.codegen.emit.strhRegMem(reg, .FP, @intCast(@as(u32, @intCast(offset)) >> 1));
                        } else {
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                            try self.codegen.emit.strhRegMem(reg, .IP0, 0);
                        }
                    },
                    else => {
                        // 4 or 8 bytes - use standard store
                        try self.codegen.emitStoreStack(.w64, offset, reg);
                    },
                }
            } else {
                // x86_64 supports all widths
                const width: x86_64.RegisterWidth = switch (disc_size) {
                    1 => .w8,
                    2 => .w16,
                    4 => .w32,
                    else => .w64,
                };
                try self.codegen.emitStoreStack(width, offset, reg);
            }

            self.codegen.freeGeneral(reg);
        }

        /// Generate code for a block
        fn generateBlock(self: *Self, block: anytype) Allocator.Error!ValueLocation {
            const stmts = self.store.getStmts(block.stmts);
            // Process each statement
            for (stmts) |stmt| {
                switch (stmt) {
                    .decl, .mutate => |b| {
                        const expr_loc = try self.generateExpr(b.expr);
                        try self.bindPattern(b.pattern, expr_loc);
                    },
                    .cell_init => |cell| {
                        const expr_loc = try self.generateExpr(cell.expr);
                        try self.initializeCell(cell.cell, cell.layout_idx, expr_loc);
                    },
                    .cell_store => |cell| {
                        const expr_loc = try self.generateExpr(cell.expr);
                        try self.storeCell(cell.cell, cell.layout_idx, expr_loc);
                    },
                    .cell_drop => |cell| try self.dropCell(cell.cell, cell.layout_idx),
                }
            }

            // Generate the final expression
            const final_loc = try self.generateExpr(block.final_expr);

            return final_loc;
        }

        fn mutableSlotSize(self: *Self, layout_idx: layout.Idx) u32 {
            const ls = self.layout_store;
            const layout_val = ls.getLayout(layout_idx);
            const raw_size = ls.layoutSizeAlign(layout_val).size;
            return if (raw_size != 0 and raw_size < 8) 8 else raw_size;
        }

        const CellStorage = struct {
            slot: i32,
            size: u32,
            tracked_mutable_slot: bool,
        };

        fn resolveCellStorage(self: *Self, cell: Symbol, layout_idx: layout.Idx) ?CellStorage {
            const cell_key: u64 = @bitCast(cell);
            if (self.mutable_var_slots.get(cell_key)) |info| {
                return .{
                    .slot = info.slot,
                    .size = info.size,
                    .tracked_mutable_slot = true,
                };
            }

            const loc = self.symbol_locations.get(cell_key) orelse return null;
            return switch (loc) {
                .stack => |s| .{
                    .slot = s.offset,
                    .size = self.mutableSlotSize(layout_idx),
                    .tracked_mutable_slot = false,
                },
                .stack_i128 => |offset| .{
                    .slot = offset,
                    .size = 16,
                    .tracked_mutable_slot = false,
                },
                .stack_str => |offset| .{
                    .slot = offset,
                    .size = roc_str_size,
                    .tracked_mutable_slot = false,
                },
                .list_stack => |ls_info| .{
                    .slot = ls_info.struct_offset,
                    .size = roc_str_size,
                    .tracked_mutable_slot = false,
                },
                else => null,
            };
        }

        fn initializeCell(self: *Self, cell: Symbol, layout_idx: layout.Idx, value_loc: ValueLocation) Allocator.Error!void {
            const cell_key: u64 = @bitCast(cell);
            const normalized_value_loc = self.coerceImmediateToLayout(value_loc, layout_idx);
            const size = self.mutableSlotSize(layout_idx);
            const fixed_slot = self.codegen.allocStackSlot(size);
            try self.copyBytesToStackOffset(fixed_slot, normalized_value_loc, size);
            try self.mutable_var_slots.put(cell_key, .{ .slot = fixed_slot, .size = size });
        }

        fn storeCell(self: *Self, cell: Symbol, layout_idx: layout.Idx, value_loc: ValueLocation) Allocator.Error!void {
            const storage = self.resolveCellStorage(cell, layout_idx) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("LIR/codegen invariant violated: store to unknown cell {d}", .{@as(u64, @bitCast(cell))});
                }
                unreachable;
            };
            const normalized_value_loc = self.coerceImmediateToLayout(value_loc, layout_idx);
            try self.copyBytesToStackOffset(storage.slot, normalized_value_loc, storage.size);
        }

        fn dropCell(self: *Self, cell: Symbol, layout_idx: layout.Idx) Allocator.Error!void {
            const cell_key: u64 = @bitCast(cell);
            const storage = self.resolveCellStorage(cell, layout_idx) orelse return;
            if (storage.tracked_mutable_slot) {
                _ = self.mutable_var_slots.remove(cell_key);
            }
        }

        fn generateCellLoad(self: *Self, cell: Symbol, layout_idx: layout.Idx) Allocator.Error!ValueLocation {
            const storage = self.resolveCellStorage(cell, layout_idx) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("LIR/codegen invariant violated: load from unknown cell {d}", .{@as(u64, @bitCast(cell))});
                }
                unreachable;
            };

            const slot = self.codegen.allocStackSlot(storage.size);
            try self.copyBytesToStackOffset(slot, self.stackLocationForLayout(layout_idx, storage.slot), storage.size);
            return self.stackLocationForLayout(layout_idx, slot);
        }

        /// Bind a value to a pattern.
        fn bindPattern(self: *Self, pattern_id: LirPatternId, value_loc: ValueLocation) Allocator.Error!void {
            const pattern = self.store.getPattern(pattern_id);

            switch (pattern) {
                .bind => |bind| {
                    const symbol_key: u64 = @bitCast(bind.symbol);
                    const normalized_value_loc = self.coerceImmediateToLayout(value_loc, bind.layout_idx);
                    if (normalized_value_loc == .list_stack) {} else if (normalized_value_loc == .stack) {}

                    try self.symbol_locations.put(symbol_key, normalized_value_loc);
                    try self.trackMutableSlotFromSymbolLocation(bind, symbol_key);
                },
                .wildcard => {
                    // Ignore the value
                },
                .struct_ => |s| {
                    // Struct destructuring: bind each field pattern.
                    // Fields are in layout order, so iterate positionally.
                    const ls = self.layout_store;
                    const struct_layout = ls.getLayout(s.struct_layout);
                    if (struct_layout.tag != .struct_) {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: bindPattern struct expected struct_ layout, got {s}",
                                .{@tagName(struct_layout.tag)},
                            );
                        }
                        unreachable;
                    }

                    const field_patterns = self.store.getPatternSpan(s.fields);

                    // Get the base offset of the struct
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |sv| sv.offset,
                        .stack_str => |off| off,
                        else => {
                            if (builtin.mode == .Debug) {
                                std.debug.panic(
                                    "LIR/codegen invariant violated: bindPattern struct requires stack value location, got {s}",
                                    .{@tagName(value_loc)},
                                );
                            }
                            unreachable;
                        },
                    };

                    // Bind each field
                    for (field_patterns, 0..) |field_pattern_id, i| {
                        const field_offset = ls.getStructFieldOffset(struct_layout.data.struct_.idx, @intCast(i));

                        // Create a location for the field using the correct layout type
                        const field_layout_idx = ls.getStructFieldLayout(struct_layout.data.struct_.idx, @intCast(i));
                        const field_loc: ValueLocation = self.stackLocationForLayout(field_layout_idx, base_offset + @as(i32, @intCast(field_offset)));

                        try self.bindPattern(field_pattern_id, field_loc);
                    }
                },
                .as_pattern => |as_pat| {
                    // As-pattern: bind the symbol AND recursively bind the inner pattern
                    const symbol_key: u64 = @bitCast(as_pat.symbol);
                    try self.symbol_locations.put(symbol_key, value_loc);

                    // Also bind the inner pattern
                    if (!as_pat.inner.isNone()) {
                        try self.bindPattern(as_pat.inner, value_loc);
                    }
                },
                .list => |lst| {
                    // List destructuring: bind prefix elements and optional rest
                    // Get the base offset of the list struct (ptr, len, capacity)
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |s| s.offset,
                        .stack_str => |off| off,
                        .list_stack => |list_info| list_info.struct_offset,
                        else => {
                            if (builtin.mode == .Debug) {
                                std.debug.panic(
                                    "LIR/codegen invariant violated: bindPattern list requires stack value location, got {s}",
                                    .{@tagName(value_loc)},
                                );
                            }
                            unreachable;
                        },
                    };

                    const prefix_patterns = self.store.getPatternSpan(lst.prefix);

                    // For each prefix element, we need to load from the list data
                    // List layout: ptr at offset 0, len at offset 8, capacity at offset 16
                    // Elements are at ptr[0], ptr[1], etc.

                    // Get element size from the pattern layout.
                    const ls = self.layout_store;
                    const elem_layout_idx: layout.Idx = lst.elem_layout;
                    const elem_layout = ls.getLayout(elem_layout_idx);
                    const elem_size_align = ls.layoutSizeAlign(elem_layout);
                    const elem_size = elem_size_align.size;

                    // Load list pointer to a register
                    const list_ptr_reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, list_ptr_reg, frame_ptr, base_offset);

                    // Bind each prefix element
                    for (prefix_patterns, 0..) |elem_pattern_id, i| {
                        // Allocate stack space for this element
                        const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));

                        // Copy element from list to stack
                        const elem_offset_in_list = @as(i32, @intCast(i * elem_size));
                        const temp_reg = try self.allocTempGeneral();

                        if (elem_size <= 8) {
                            // Load element from list[i] to temp
                            try self.emitLoad(.w64, temp_reg, list_ptr_reg, elem_offset_in_list);
                            try self.emitStore(.w64, frame_ptr, elem_slot, temp_reg);
                        } else {
                            // For larger elements, copy 8 bytes at a time
                            try self.copyChunked(temp_reg, list_ptr_reg, elem_offset_in_list, frame_ptr, elem_slot, elem_size);
                        }

                        self.codegen.freeGeneral(temp_reg);

                        // Bind the element pattern to the stack slot
                        const elem_loc: ValueLocation = self.stackLocationForLayout(elem_layout_idx, elem_slot);
                        try self.bindPattern(elem_pattern_id, elem_loc);
                    }

                    // Bind suffix elements (from the end of the list)
                    const suffix_patterns = self.store.getPatternSpan(lst.suffix);
                    if (suffix_patterns.len > 0) {
                        // We need the list length to compute suffix offsets
                        const suf_len_reg = try self.allocTempGeneral();
                        try self.emitLoad(.w64, suf_len_reg, frame_ptr, base_offset + 8);

                        const suffix_count = @as(u32, @intCast(suffix_patterns.len));
                        const suf_ptr_reg = try self.allocTempGeneral();

                        // suf_ptr = list_ptr + (len - suffix_count) * elem_size
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.subRegRegImm12(.w64, suf_len_reg, suf_len_reg, @intCast(suffix_count));
                            if (elem_size == 1) {
                                try self.codegen.emit.addRegRegReg(.w64, suf_ptr_reg, list_ptr_reg, suf_len_reg);
                            } else {
                                const imm_reg = try self.allocTempGeneral();
                                try self.codegen.emitLoadImm(imm_reg, @intCast(elem_size));
                                try self.codegen.emit.mulRegRegReg(.w64, suf_len_reg, suf_len_reg, imm_reg);
                                try self.codegen.emit.addRegRegReg(.w64, suf_ptr_reg, list_ptr_reg, suf_len_reg);
                                self.codegen.freeGeneral(imm_reg);
                            }
                        } else {
                            try self.codegen.emit.subRegImm32(.w64, suf_len_reg, @intCast(suffix_count));
                            if (elem_size > 1) {
                                const imm_reg = try self.allocTempGeneral();
                                try self.codegen.emitLoadImm(imm_reg, @intCast(elem_size));
                                try self.codegen.emit.imulRegReg(.w64, suf_len_reg, imm_reg);
                                self.codegen.freeGeneral(imm_reg);
                            }
                            try self.codegen.emit.movRegReg(.w64, suf_ptr_reg, list_ptr_reg);
                            try self.codegen.emit.addRegReg(.w64, suf_ptr_reg, suf_len_reg);
                        }
                        self.codegen.freeGeneral(suf_len_reg);

                        for (suffix_patterns, 0..) |suf_pattern_id, suf_idx| {
                            const suf_offset = @as(i32, @intCast(suf_idx * elem_size));
                            const suf_slot = self.codegen.allocStackSlot(@intCast(elem_size));
                            const temp_reg = try self.allocTempGeneral();

                            if (elem_size <= 8) {
                                try self.emitLoad(.w64, temp_reg, suf_ptr_reg, suf_offset);
                                try self.emitStore(.w64, frame_ptr, suf_slot, temp_reg);
                            } else {
                                try self.copyChunked(temp_reg, suf_ptr_reg, suf_offset, frame_ptr, suf_slot, elem_size);
                            }

                            self.codegen.freeGeneral(temp_reg);
                            try self.bindPattern(suf_pattern_id, self.stackLocationForLayout(elem_layout_idx, suf_slot));
                        }

                        self.codegen.freeGeneral(suf_ptr_reg);
                    }

                    self.codegen.freeGeneral(list_ptr_reg);
                },
                .tag => |tag_pat| {
                    // Tag destructuring: bind payload patterns
                    // For lambda parameters, the tag match is already known, just bind the payload
                    const arg_patterns = self.store.getPatternSpan(tag_pat.args);
                    if (arg_patterns.len == 0) return;

                    const ls = self.layout_store;
                    const union_layout = ls.getLayout(tag_pat.union_layout);
                    const variant_payload_layout, const payload_loc = blk: {
                        switch (union_layout.tag) {
                            .tag_union => {
                                const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
                                const variants = ls.getTagUnionVariants(tu_data);
                                const variant = variants.get(tag_pat.discriminant);
                                const stable_value_loc = try self.materializeValueToStackForLayout(value_loc, tag_pat.union_layout);
                                const base_offset: i32 = switch (stable_value_loc) {
                                    .stack => |s| s.offset,
                                    .stack_i128 => |off| off,
                                    .stack_str => |off| off,
                                    .list_stack => |ls_info| ls_info.struct_offset,
                                    else => {
                                        if (builtin.mode == .Debug) {
                                            std.debug.panic(
                                                "LIR/codegen invariant violated: bindPattern tag requires stack value location, got {s}",
                                                .{@tagName(stable_value_loc)},
                                            );
                                        }
                                        unreachable;
                                    },
                                };
                                break :blk .{
                                    variant.payload_layout,
                                    self.stackLocationForLayout(variant.payload_layout, base_offset),
                                };
                            },
                            .box => {
                                const inner_layout = ls.getLayout(union_layout.data.box);
                                if (builtin.mode == .Debug and inner_layout.tag != .tag_union) {
                                    std.debug.panic(
                                        "LIR/codegen invariant violated: bindPattern boxed tag expected inner tag_union layout, got {s}",
                                        .{@tagName(inner_layout.tag)},
                                    );
                                }

                                const tu_data = ls.getTagUnionData(inner_layout.data.tag_union.idx);
                                const variants = ls.getTagUnionVariants(tu_data);
                                const variant = variants.get(tag_pat.discriminant);
                                const payload_layout_idx = variant.payload_layout;
                                const payload_layout_val = ls.getLayout(payload_layout_idx);
                                const payload_size = ls.layoutSizeAlign(payload_layout_val).size;
                                if (payload_size == 0) {
                                    break :blk .{ payload_layout_idx, ValueLocation{ .immediate_i64 = 0 } };
                                }

                                const box_ptr_reg = try self.ensureInGeneralReg(value_loc);
                                defer self.codegen.freeGeneral(box_ptr_reg);

                                const detached_slot = self.codegen.allocStackSlot(payload_size);
                                var copied: u32 = 0;
                                while (copied < payload_size) : (copied += 8) {
                                    const temp_reg = try self.allocTempGeneral();
                                    try self.emitLoad(.w64, temp_reg, box_ptr_reg, @intCast(copied));
                                    try self.emitStore(.w64, frame_ptr, detached_slot + @as(i32, @intCast(copied)), temp_reg);
                                    self.codegen.freeGeneral(temp_reg);
                                }

                                break :blk .{
                                    payload_layout_idx,
                                    self.stackLocationForLayout(payload_layout_idx, detached_slot),
                                };
                            },
                            .scalar, .zst => {
                                if (builtin.mode == .Debug and arg_patterns.len != 1) {
                                    std.debug.panic(
                                        "LIR/codegen invariant violated: scalar/zst tag payload binding expects exactly 1 arg, found {d}",
                                        .{arg_patterns.len},
                                    );
                                }
                                break :blk .{ layout.Idx.zst, ValueLocation{ .immediate_i64 = 0 } };
                            },
                            else => {
                                if (builtin.mode == .Debug) {
                                    std.debug.panic(
                                        "LIR/codegen invariant violated: bindPattern tag expected tag_union/box/scalar/zst layout, got {s}",
                                        .{@tagName(union_layout.tag)},
                                    );
                                }
                                unreachable;
                            },
                        }
                    };

                    const payload_layout = ls.getLayout(variant_payload_layout);

                    if (payload_layout.tag == .struct_) {
                        const payload_base: i32 = switch (payload_loc) {
                            .stack => |s| s.offset,
                            .stack_i128 => |off| off,
                            .stack_str => |off| off,
                            else => unreachable,
                        };
                        for (arg_patterns, 0..) |arg_pattern_id, i| {
                            const tuple_elem_offset = ls.getStructFieldOffsetByOriginalIndex(payload_layout.data.struct_.idx, @intCast(i));
                            const arg_offset = payload_base + @as(i32, @intCast(tuple_elem_offset));
                            const tuple_elem_layout_idx = ls.getStructFieldLayoutByOriginalIndex(payload_layout.data.struct_.idx, @intCast(i));
                            if (builtin.mode == .Debug) {
                                try self.assertPatternMatchesRuntimeLayout(arg_pattern_id, tuple_elem_layout_idx, "tag pattern payload field");
                            }
                            try self.bindPattern(arg_pattern_id, self.stackLocationForLayout(tuple_elem_layout_idx, arg_offset));
                        }
                    } else {
                        if (builtin.mode == .Debug) {
                            if (arg_patterns.len != 1) {
                                std.debug.panic(
                                    "LIR/codegen invariant violated: non-struct tag payload can only bind one arg, got {d}",
                                    .{arg_patterns.len},
                                );
                            }
                            try self.assertPatternMatchesRuntimeLayout(arg_patterns[0], variant_payload_layout, "tag pattern payload");
                        }
                        try self.bindPattern(arg_patterns[0], payload_loc);
                    }
                },
                else => {
                    // Literal patterns (int_literal, float_literal, str_literal) don't bind anything
                    // They are used for matching in match expressions, not for binding
                },
            }
        }

        /// Ensure a reassignable symbol has a tracked mutable slot from its current location.
        fn trackMutableSlotFromSymbolLocation(self: *Self, bind: anytype, symbol_key: u64) Allocator.Error!void {
            if (!bind.reassignable) return;
            const loc = self.symbol_locations.get(symbol_key) orelse return;

            const slot: i32 = switch (loc) {
                .stack => |s| s.offset,
                .stack_i128 => |off| off,
                .stack_str => |off| off,
                .list_stack => |ls_info| ls_info.struct_offset,
                else => {
                    if (builtin.mode == .Debug) std.debug.panic("LIR/codegen invariant violated: trackMutableSlotFromSymbolLocation unsupported location {s}", .{@tagName(loc)});
                    unreachable;
                },
            };

            const size: u32 = switch (loc) {
                .stack_i128 => 16,
                .stack_str, .list_stack => roc_str_size,
                .stack => blk: {
                    const ls = self.layout_store;
                    if (builtin.mode == .Debug and @intFromEnum(bind.layout_idx) >= ls.layouts.len()) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: mutable bind layout out of bounds ({d} >= {d})",
                            .{ @intFromEnum(bind.layout_idx), ls.layouts.len() },
                        );
                    }
                    const raw = ls.layoutSizeAlign(ls.getLayout(bind.layout_idx)).size;
                    break :blk if (raw != 0 and raw < 8) 8 else raw;
                },
                else => {
                    if (builtin.mode == .Debug) std.debug.panic("LIR/codegen invariant violated: trackMutableSlotFromSymbolLocation unsupported location for size {s}", .{@tagName(loc)});
                    unreachable;
                },
            };

            try self.mutable_var_slots.put(symbol_key, .{ .slot = slot, .size = size });
        }

        /// Map a layout index to the correct ValueLocation for a value on the stack.
        /// Multi-word types (strings, i128/Dec, lists) need specific location variants
        /// so downstream code loads the correct number of bytes.
        fn stackLocationForLayout(self: *Self, layout_idx: layout.Idx, stack_offset: i32) ValueLocation {
            if (layout_idx == .i128 or layout_idx == .u128 or layout_idx == .dec)
                return .{ .stack_i128 = stack_offset };
            if (layout_idx == .str)
                return .{ .stack_str = stack_offset };
            const ls = self.layout_store;
            const resolved = ls.getLayout(layout_idx);
            if (resolved.tag == .list or resolved.tag == .list_of_zst)
                return .{ .list_stack = .{ .struct_offset = stack_offset, .data_offset = 0, .num_elements = 0 } };
            const size = ls.layoutSizeAlign(resolved).size;
            return .{ .stack = .{ .offset = stack_offset, .size = ValueSize.fromByteCount(size) } };
        }

        /// Emit a correctly-sized load from the stack, zero-extending sub-word
        /// values to 64 bits. This prevents reading garbage upper bytes when
        /// a Bool/U8/U16/U32 was stored with a narrow write.
        fn emitSizedLoadStack(self: *Self, reg: GeneralReg, offset: i32, size: ValueSize) Allocator.Error!void {
            switch (size) {
                .byte => try self.emitLoadStackW8(reg, offset),
                .word => try self.emitLoadStackW16(reg, offset),
                .dword => try self.codegen.emitLoadStack(.w32, reg, offset),
                .qword => try self.codegen.emitLoadStack(.w64, reg, offset),
            }
        }

        /// Emit a correctly-sized load from memory (arbitrary base register + offset),
        /// zero-extending sub-word values to 64 bits.
        fn emitSizedLoadMem(self: *Self, dst: GeneralReg, base_reg: GeneralReg, offset: i32, size: ValueSize) Allocator.Error!void {
            switch (size) {
                .byte => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        if (offset >= -256 and offset <= 255) {
                            try self.codegen.emit.ldurbRegMem(dst, base_reg, @intCast(offset));
                        } else {
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .IP0, base_reg, .IP0);
                            try self.codegen.emit.ldrbRegMem(dst, .IP0, 0);
                        }
                    } else {
                        try self.codegen.emit.movzxBRegMem(dst, base_reg, offset);
                    }
                },
                .word => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        if (offset >= -256 and offset <= 255) {
                            try self.codegen.emit.ldurhRegMem(dst, base_reg, @intCast(offset));
                        } else {
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .IP0, base_reg, .IP0);
                            try self.codegen.emit.ldrhRegMem(dst, .IP0, 0);
                        }
                    } else {
                        try self.codegen.emit.movzxWRegMem(dst, base_reg, offset);
                    }
                },
                .dword => try self.emitLoad(.w32, dst, base_reg, offset),
                .qword => try self.emitLoad(.w64, dst, base_reg, offset),
            }
        }

        /// Emit a correctly-sized store to memory (arbitrary base register + offset).
        fn emitSizedStoreMem(self: *Self, base_reg: GeneralReg, offset: i32, src: GeneralReg, size: ValueSize) Allocator.Error!void {
            switch (size) {
                .byte => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        if (offset >= -256 and offset <= 255) {
                            try self.codegen.emit.sturbRegMem(src, base_reg, @intCast(offset));
                        } else {
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .IP0, base_reg, .IP0);
                            try self.codegen.emit.strbRegMem(src, .IP0, 0);
                        }
                    } else {
                        try self.codegen.emit.movMemReg(.w8, base_reg, offset, src);
                    }
                },
                .word => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        if (offset >= -256 and offset <= 255) {
                            try self.codegen.emit.sturhRegMem(src, base_reg, @intCast(offset));
                        } else {
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .IP0, base_reg, .IP0);
                            try self.codegen.emit.strhRegMem(src, .IP0, 0);
                        }
                    } else {
                        try self.codegen.emit.movMemReg(.w16, base_reg, offset, src);
                    }
                },
                .dword => try self.emitStore(.w32, base_reg, offset, src),
                .qword => try self.emitStore(.w64, base_reg, offset, src),
            }
        }

        /// Get the register used for argument N in the calling convention
        fn getArgumentRegister(_: *Self, index: u8) GeneralReg {
            if (comptime target.toCpuArch() == .aarch64) {
                // AArch64: X0-X7 for arguments
                if (index >= 8) {
                    unreachable;
                }
                return @enumFromInt(index);
            } else if (comptime target.isWindows()) {
                // Windows x64: RCX, RDX, R8, R9
                const arg_regs = [_]x86_64.GeneralReg{ .RCX, .RDX, .R8, .R9 };
                if (index >= arg_regs.len) {
                    unreachable;
                }
                return arg_regs[index];
            } else {
                // x86_64 System V: RDI, RSI, RDX, RCX, R8, R9
                const arg_regs = [_]x86_64.GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8, .R9 };
                if (index >= arg_regs.len) {
                    unreachable;
                }
                return arg_regs[index];
            }
        }

        /// Get the register used for return values
        fn getReturnRegister(_: *Self) GeneralReg {
            if (comptime target.toCpuArch() == .aarch64) {
                return .X0;
            } else {
                return .RAX;
            }
        }

        /// Emit a call instruction to a specific code offset.
        /// Records the call position so it can be re-patched if the surrounding
        /// code is shifted by deferred-prologue proc compilation.
        fn emitCallToOffset(self: *Self, target_offset: usize) !void {
            const current = self.codegen.currentOffset();

            // Record this call so we can re-patch it after body shifts
            try self.internal_call_patches.append(self.allocator, .{
                .call_offset = current,
                .target_offset = target_offset,
            });

            // Calculate relative byte offset (can be negative for backward call)
            const rel_offset: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(current)));

            if (comptime target.toCpuArch() == .aarch64) {
                // BL instruction expects byte offset (it divides by 4 internally)
                try self.codegen.emit.bl(rel_offset);
            } else {
                // x86_64: CALL rel32
                // Offset is relative to instruction after the call (current + 5)
                const call_rel = rel_offset - 5;
                try self.codegen.emit.call(@bitCast(call_rel));
            }
        }

        /// After deferred-prologue proc compilation shifts its body by prepending a prologue,
        /// re-patch any internal BL/CALL instructions within the shifted range
        /// that target code outside the shifted range.
        ///
        /// When body bytes [body_start..body_end] are shifted forward by prologue_size:
        /// - BL instructions within the body are now at (old_pos + prologue_size)
        /// - Their targets outside the body are NOT shifted
        /// - So the relative offset in the BL instruction is now wrong by prologue_size
        fn repatchInternalCalls(
            self: *Self,
            body_start: usize,
            body_end: usize,
            prologue_size: usize,
            current_entry_start: usize,
        ) void {
            const buf = self.codegen.emit.buf.items;
            for (self.internal_call_patches.items) |*patch| {
                // Only adjust patches that were within the shifted body range
                if (patch.call_offset >= body_start and patch.call_offset < body_end) {
                    // Update the patch's recorded position (it shifted)
                    patch.call_offset += prologue_size;

                    // Targets anywhere inside [body_start, body_end) shift with the body,
                    // except self-recursive calls to the current entry point. Those should
                    // continue targeting the prepended prologue at the original body_start.
                    if (patch.target_offset >= body_start and patch.target_offset < body_end and patch.target_offset != current_entry_start) {
                        patch.target_offset += prologue_size;
                        continue;
                    }

                    // For targets outside the shifted body, or the current body's entry point,
                    // re-patch because only the call site moved.
                    {
                        const new_rel: i32 = @intCast(@as(i64, @intCast(patch.target_offset)) - @as(i64, @intCast(patch.call_offset)));
                        if (comptime target.toCpuArch() == .aarch64) {
                            // Patch BL instruction (4 bytes at call_offset)
                            // BL encoding: imm26 = offset / 4
                            const imm26: u26 = @bitCast(@as(i26, @intCast(@divExact(new_rel, 4))));
                            const bl_opcode: u32 = (0b100101 << 26) | @as(u32, imm26);
                            const bytes: [4]u8 = @bitCast(bl_opcode);
                            @memcpy(buf[patch.call_offset..][0..4], &bytes);
                        } else {
                            // Patch CALL rel32 instruction (5 bytes: 0xE8 + 4-byte offset)
                            // The offset is relative to the instruction AFTER the call (call_offset + 5)
                            const call_rel: i32 = new_rel - 5;
                            const bytes: [4]u8 = @bitCast(call_rel);
                            @memcpy(buf[patch.call_offset + 1 ..][0..4], &bytes);
                        }
                    }
                }
            }
        }

        /// After deferred-prologue proc compilation shifts its body by prepending a prologue,
        /// re-patch any ADR/LEA instructions within the shifted range that
        /// compute lambda addresses targeting code outside the shifted range.
        fn repatchInternalAddrPatches(
            self: *Self,
            body_start: usize,
            body_end: usize,
            prologue_size: usize,
            current_entry_start: usize,
        ) void {
            const buf = self.codegen.emit.buf.items;
            for (self.internal_addr_patches.items) |*patch| {
                if (patch.instr_offset >= body_start and patch.instr_offset < body_end) {
                    // Update the patch's recorded position (it shifted)
                    patch.instr_offset += prologue_size;

                    // Targets anywhere inside [body_start, body_end) shift with the body,
                    // except references to the current entry point, which should continue
                    // targeting the prepended prologue at the original body_start.
                    if (patch.target_offset >= body_start and patch.target_offset < body_end and patch.target_offset != current_entry_start) {
                        patch.target_offset += prologue_size;
                        continue;
                    }

                    // Targets outside the shifted body, or the current body's entry point,
                    // need re-patching because only the instruction moved.
                    {
                        const new_rel: i64 = @as(i64, @intCast(patch.target_offset)) - @as(i64, @intCast(patch.instr_offset));
                        if (comptime target.toCpuArch() == .aarch64) {
                            // ADR instruction: rd | immhi(19) << 5 | 10000 << 24 | immlo(2) << 29 | 0 << 31
                            // We need to preserve rd and just update the immediate
                            const existing: u32 = @bitCast(buf[patch.instr_offset..][0..4].*);
                            const rd_bits: u32 = existing & 0x1F; // bottom 5 bits = Rd
                            const imm: u21 = @bitCast(@as(i21, @intCast(new_rel)));
                            const immlo: u2 = @truncate(imm);
                            const immhi: u19 = @truncate(imm >> 2);
                            const inst: u32 = (0 << 31) |
                                (@as(u32, immlo) << 29) |
                                (0b10000 << 24) |
                                (@as(u32, immhi) << 5) |
                                rd_bits;
                            const bytes: [4]u8 = @bitCast(inst);
                            @memcpy(buf[patch.instr_offset..][0..4], &bytes);
                        } else {
                            // LEA reg, [RIP + disp32] — 7 bytes: REX + 0x8D + ModRM + disp32
                            // disp32 is at bytes [3..7], relative to end of instruction (instr_offset + 7)
                            const lea_size: i64 = 7;
                            const disp: i32 = @intCast(new_rel - lea_size);
                            const bytes: [4]u8 = @bitCast(disp);
                            @memcpy(buf[patch.instr_offset + 3 ..][0..4], &bytes);
                        }
                    }
                }
            }
        }

        /// After a deferred-prologue body shifts forward, pending direct-proc calls
        /// emitted inside that body must move with it so they can be patched later.
        fn shiftPendingCalls(
            self: *Self,
            body_start: usize,
            body_end: usize,
            prologue_size: usize,
        ) void {
            for (self.pending_calls.items) |*pending| {
                if (pending.call_site >= body_start and pending.call_site < body_end) {
                    pending.call_site += prologue_size;
                }
            }
        }

        /// When a lambda body is shifted forward by prepending a prologue, nested lambdas
        /// compiled inside that body also move. Keep the lambda caches in final coordinates.
        fn shiftNestedCompiledRcHelperOffsets(
            self: *Self,
            body_start: usize,
            body_end: usize,
            prologue_size: usize,
            current_key: u64,
        ) void {
            if (prologue_size == 0) return;

            var iter = self.compiled_rc_helpers.iterator();
            while (iter.next()) |entry| {
                if (entry.key_ptr.* == current_key) continue;
                const offset = entry.value_ptr.*;
                if (offset > body_start and offset < body_end) {
                    entry.value_ptr.* = offset + prologue_size;
                }
            }
        }

        fn emitInternalCodeAddress(self: *Self, target_offset: usize, dst_reg: GeneralReg) !void {
            const current = self.codegen.currentOffset();
            if (comptime target.toCpuArch() == .aarch64) {
                const rel: i21 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(current)));
                try self.codegen.emit.adr(dst_reg, rel);
            } else {
                const rel: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(current)) - 7);
                try self.codegen.emit.leaRegRipRel(dst_reg, rel);
            }

            try self.internal_addr_patches.append(self.allocator, .{
                .instr_offset = current,
                .target_offset = target_offset,
            });
        }

        fn emitCallRcHelperFromStackSlots(
            self: *Self,
            helper_key: RcHelperKey,
            ptr_slot: i32,
            count_slot: ?i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const code_offset = try self.compileRcHelper(helper_key);

            const arg0 = self.getArgumentRegister(0);
            try self.emitLoad(.w64, arg0, frame_ptr, ptr_slot);

            switch (helper_key.op) {
                .incref => {
                    const arg1 = self.getArgumentRegister(1);
                    const arg2 = self.getArgumentRegister(2);
                    try self.emitLoad(.w64, arg1, frame_ptr, count_slot.?);
                    try self.emitLoad(.w64, arg2, frame_ptr, roc_ops_slot);
                },
                .decref, .free => {
                    const arg1 = self.getArgumentRegister(1);
                    try self.emitLoad(.w64, arg1, frame_ptr, roc_ops_slot);
                },
            }

            try self.emitCallToOffset(code_offset);
        }

        fn emitRcHelperCallAtStackOffset(
            self: *Self,
            op: RcOp,
            base_offset: i32,
            layout_idx: layout.Idx,
            count: u16,
        ) Allocator.Error!void {
            const helper_key = RcHelperKey{ .op = op, .layout_idx = layout_idx };
            const resolver = RcHelperResolver.init(self.layout_store);
            if (resolver.plan(helper_key) == .noop) return;

            const ptr_slot = self.codegen.allocStackSlot(8);
            const roc_ops_slot = self.codegen.allocStackSlot(8);
            const ptr_reg = try self.allocTempGeneral();
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            try self.emitMovRegReg(ptr_reg, frame_ptr);
            if (base_offset != 0) {
                try self.emitAddPtrImmAny(ptr_reg, ptr_reg, base_offset);
            }
            try self.emitStore(.w64, frame_ptr, ptr_slot, ptr_reg);
            try self.emitStore(.w64, frame_ptr, roc_ops_slot, roc_ops_reg);
            self.codegen.freeGeneral(ptr_reg);

            switch (op) {
                .incref => {
                    const count_slot = self.codegen.allocStackSlot(8);
                    const count_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(count_reg, count);
                    try self.emitStore(.w64, frame_ptr, count_slot, count_reg);
                    self.codegen.freeGeneral(count_reg);
                    try self.emitCallRcHelperFromStackSlots(helper_key, ptr_slot, count_slot, roc_ops_slot);
                },
                .decref, .free => {
                    try self.emitCallRcHelperFromStackSlots(helper_key, ptr_slot, null, roc_ops_slot);
                },
            }
        }

        fn emitRcHelperCallForValue(
            self: *Self,
            op: RcOp,
            value_loc: ValueLocation,
            layout_idx: layout.Idx,
            count: u16,
        ) Allocator.Error!void {
            const l = self.layout_store.getLayout(layout_idx);
            if (!self.layout_store.layoutContainsRefcounted(l)) return;

            const value_size = self.layout_store.layoutSizeAlign(l).size;
            if (value_size == 0) return;

            const base_offset = try self.ensureValueOnStackForRc(value_loc, value_size);
            try self.emitRcHelperCallAtStackOffset(op, base_offset, layout_idx, count);
        }

        fn emitRcHelperCallFromPtrReg(
            self: *Self,
            helper_key: RcHelperKey,
            ptr_reg: GeneralReg,
            count_slot: ?i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const ptr_slot = self.codegen.allocStackSlot(8);
            try self.emitStore(.w64, frame_ptr, ptr_slot, ptr_reg);
            try self.emitCallRcHelperFromStackSlots(helper_key, ptr_slot, count_slot, roc_ops_slot);
        }

        fn loadListDataPtrForRcFromValuePtr(
            self: *Self,
            value_ptr_reg: GeneralReg,
            out_reg: GeneralReg,
        ) Allocator.Error!void {
            try self.emitLoad(.w64, out_reg, value_ptr_reg, 0);

            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);
            try self.emitLoad(.w64, cap_reg, value_ptr_reg, 16);

            const slice_patch = blk: {
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, cap_reg, 0);
                    const patch_loc = self.codegen.currentOffset();
                    try self.codegen.emit.bcond(.mi, 0);
                    break :blk patch_loc;
                } else {
                    try self.codegen.emit.testRegReg(.w64, cap_reg, cap_reg);
                    break :blk try self.codegen.emitCondJump(.sign);
                }
            };

            const done_patch = try self.codegen.emitJump();
            self.codegen.patchJump(slice_patch, self.codegen.currentOffset());
            try self.emitMovRegReg(out_reg, cap_reg);
            try self.emitShlImm(.w64, out_reg, out_reg, 1);
            self.codegen.patchJump(done_patch, self.codegen.currentOffset());
        }

        fn loadStrDataPtrForRcFromValuePtr(
            self: *Self,
            value_ptr_reg: GeneralReg,
            out_reg: GeneralReg,
        ) Allocator.Error!void {
            try self.emitLoad(.w64, out_reg, value_ptr_reg, 0);

            const len_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(len_reg);
            try self.emitLoad(.w64, len_reg, value_ptr_reg, 8);

            const slice_patch = blk: {
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, len_reg, 0);
                    const patch_loc = self.codegen.currentOffset();
                    try self.codegen.emit.bcond(.mi, 0);
                    break :blk patch_loc;
                } else {
                    try self.codegen.emit.testRegReg(.w64, len_reg, len_reg);
                    break :blk try self.codegen.emitCondJump(.sign);
                }
            };

            const done_patch = try self.codegen.emitJump();
            self.codegen.patchJump(slice_patch, self.codegen.currentOffset());
            try self.emitLoad(.w64, out_reg, value_ptr_reg, 16);
            try self.emitShlImm(.w64, out_reg, out_reg, 1);
            self.codegen.patchJump(done_patch, self.codegen.currentOffset());
        }

        fn emitRcHelperStrIncref(
            self: *Self,
            ptr_slot: i32,
            count_slot: i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const value_ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(value_ptr_reg);
            try self.emitLoad(.w64, value_ptr_reg, frame_ptr, ptr_slot);

            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);
            try self.emitLoad(.w64, cap_reg, value_ptr_reg, 16);

            const skip_patch = blk: {
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, cap_reg, 0);
                    const patch_loc = self.codegen.currentOffset();
                    try self.codegen.emit.bcond(.mi, 0);
                    break :blk patch_loc;
                } else {
                    try self.codegen.emit.testRegReg(.w64, cap_reg, cap_reg);
                    break :blk try self.codegen.emitCondJump(.sign);
                }
            };

            const data_ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(data_ptr_reg);
            try self.loadStrDataPtrForRcFromValuePtr(value_ptr_reg, data_ptr_reg);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(data_ptr_reg);
            try builder.addMemArg(frame_ptr, count_slot);
            try builder.addMemArg(frame_ptr, roc_ops_slot);
            try self.callBuiltin(&builder, @intFromPtr(&increfDataPtrC), .incref_data_ptr);

            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        fn emitRcHelperStrDrop(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
            ptr_slot: i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const value_ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(value_ptr_reg);
            try self.emitLoad(.w64, value_ptr_reg, frame_ptr, ptr_slot);

            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);
            try self.emitLoad(.w64, cap_reg, value_ptr_reg, 16);

            const skip_patch = blk: {
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, cap_reg, 0);
                    const patch_loc = self.codegen.currentOffset();
                    try self.codegen.emit.bcond(.mi, 0);
                    break :blk patch_loc;
                } else {
                    try self.codegen.emit.testRegReg(.w64, cap_reg, cap_reg);
                    break :blk try self.codegen.emitCondJump(.sign);
                }
            };

            const data_ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(data_ptr_reg);
            try self.loadStrDataPtrForRcFromValuePtr(value_ptr_reg, data_ptr_reg);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(data_ptr_reg);
            try builder.addImmArg(1);
            try builder.addImmArg(0);
            try builder.addMemArg(frame_ptr, roc_ops_slot);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        fn emitRcHelperListIncref(
            self: *Self,
            ptr_slot: i32,
            count_slot: i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const value_ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(value_ptr_reg);
            try self.emitLoad(.w64, value_ptr_reg, frame_ptr, ptr_slot);

            const data_ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(data_ptr_reg);
            try self.loadListDataPtrForRcFromValuePtr(value_ptr_reg, data_ptr_reg);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(data_ptr_reg);
            try builder.addMemArg(frame_ptr, count_slot);
            try builder.addMemArg(frame_ptr, roc_ops_slot);
            try self.callBuiltin(&builder, @intFromPtr(&increfDataPtrC), .incref_data_ptr);
        }

        fn emitRcHelperListDrop(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
            list_plan: layout.RcListPlan,
            ptr_slot: i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const value_ptr_reg = try self.allocTempGeneral();
            const bytes_reg = try self.allocTempGeneral();
            const len_reg = try self.allocTempGeneral();
            const cap_reg = try self.allocTempGeneral();
            const callback_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(callback_reg);
            defer self.codegen.freeGeneral(cap_reg);
            defer self.codegen.freeGeneral(len_reg);
            defer self.codegen.freeGeneral(bytes_reg);
            defer self.codegen.freeGeneral(value_ptr_reg);

            try self.emitLoad(.w64, value_ptr_reg, frame_ptr, ptr_slot);
            try self.emitLoad(.w64, bytes_reg, value_ptr_reg, 0);
            try self.emitLoad(.w64, len_reg, value_ptr_reg, 8);
            try self.emitLoad(.w64, cap_reg, value_ptr_reg, 16);

            if (list_plan.child) |child_key| {
                const child_offset = try self.compileRcHelper(child_key);
                try self.emitInternalCodeAddress(child_offset, callback_reg);
            } else {
                try self.codegen.emitLoadImm(callback_reg, 0);
            }

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(bytes_reg);
            try builder.addRegArg(len_reg);
            try builder.addRegArg(cap_reg);
            try builder.addImmArg(list_plan.elem_alignment);
            try builder.addImmArg(@intCast(list_plan.elem_width));
            try builder.addRegArg(callback_reg);
            try builder.addMemArg(frame_ptr, roc_ops_slot);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
        }

        fn emitRcHelperBoxIncref(
            self: *Self,
            ptr_slot: i32,
            count_slot: i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const value_ptr_reg = try self.allocTempGeneral();
            const payload_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(payload_reg);
            defer self.codegen.freeGeneral(value_ptr_reg);

            try self.emitLoad(.w64, value_ptr_reg, frame_ptr, ptr_slot);
            try self.emitLoad(.w64, payload_reg, value_ptr_reg, 0);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(payload_reg);
            try builder.addMemArg(frame_ptr, count_slot);
            try builder.addMemArg(frame_ptr, roc_ops_slot);
            try self.callBuiltin(&builder, @intFromPtr(&increfDataPtrC), .incref_data_ptr);
        }

        fn emitRcHelperBoxDrop(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
            box_plan: layout.RcBoxPlan,
            ptr_slot: i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const value_ptr_reg = try self.allocTempGeneral();
            const payload_reg = try self.allocTempGeneral();
            const callback_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(callback_reg);
            defer self.codegen.freeGeneral(payload_reg);
            defer self.codegen.freeGeneral(value_ptr_reg);

            try self.emitLoad(.w64, value_ptr_reg, frame_ptr, ptr_slot);
            try self.emitLoad(.w64, payload_reg, value_ptr_reg, 0);

            if (box_plan.child) |child_key| {
                const child_offset = try self.compileRcHelper(child_key);
                try self.emitInternalCodeAddress(child_offset, callback_reg);
            } else {
                try self.codegen.emitLoadImm(callback_reg, 0);
            }

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(payload_reg);
            try builder.addImmArg(box_plan.elem_alignment);
            try builder.addRegArg(callback_reg);
            try builder.addMemArg(frame_ptr, roc_ops_slot);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
        }

        fn generateRcHelperBody(
            self: *Self,
            helper_key: RcHelperKey,
            ptr_slot: i32,
            count_slot: ?i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const resolver = RcHelperResolver.init(self.layout_store);
            switch (resolver.plan(helper_key)) {
                .noop => {},
                .str_incref => try self.emitRcHelperStrIncref(ptr_slot, count_slot.?, roc_ops_slot),
                .str_decref => try self.emitRcHelperStrDrop(.decref_data_ptr, @intFromPtr(&decrefDataPtrC), ptr_slot, roc_ops_slot),
                .str_free => try self.emitRcHelperStrDrop(.free_data_ptr, @intFromPtr(&freeDataPtrC), ptr_slot, roc_ops_slot),
                .list_incref => try self.emitRcHelperListIncref(ptr_slot, count_slot.?, roc_ops_slot),
                .list_decref => |list_plan| try self.emitRcHelperListDrop(
                    .list_decref_with,
                    @intFromPtr(&dev_wrappers.roc_builtins_list_decref_with),
                    list_plan,
                    ptr_slot,
                    roc_ops_slot,
                ),
                .list_free => |list_plan| try self.emitRcHelperListDrop(
                    .list_free_with,
                    @intFromPtr(&dev_wrappers.roc_builtins_list_free_with),
                    list_plan,
                    ptr_slot,
                    roc_ops_slot,
                ),
                .box_incref => try self.emitRcHelperBoxIncref(ptr_slot, count_slot.?, roc_ops_slot),
                .box_decref => |box_plan| try self.emitRcHelperBoxDrop(
                    .box_decref_with,
                    @intFromPtr(&dev_wrappers.roc_builtins_box_decref_with),
                    box_plan,
                    ptr_slot,
                    roc_ops_slot,
                ),
                .box_free => |box_plan| try self.emitRcHelperBoxDrop(
                    .box_free_with,
                    @intFromPtr(&dev_wrappers.roc_builtins_box_free_with),
                    box_plan,
                    ptr_slot,
                    roc_ops_slot,
                ),
                .struct_ => |struct_plan| {
                    const field_count = resolver.structFieldCount(struct_plan);
                    var i: u32 = 0;
                    while (i < field_count) : (i += 1) {
                        const field_plan = resolver.structFieldPlan(struct_plan, i) orelse continue;
                        const field_ptr_reg = try self.allocTempGeneral();
                        defer self.codegen.freeGeneral(field_ptr_reg);

                        try self.emitLoad(.w64, field_ptr_reg, frame_ptr, ptr_slot);
                        try self.emitAddPtrImmAny(field_ptr_reg, field_ptr_reg, @intCast(field_plan.offset));
                        try self.emitRcHelperCallFromPtrReg(field_plan.child, field_ptr_reg, count_slot, roc_ops_slot);
                    }
                },
                .tag_union => |tag_plan| {
                    const variant_count = resolver.tagUnionVariantCount(tag_plan);
                    if (variant_count == 0) return;

                    if (variant_count == 1) {
                        if (resolver.tagUnionVariantPlan(tag_plan, 0)) |child_key| {
                            const payload_reg = try self.allocTempGeneral();
                            defer self.codegen.freeGeneral(payload_reg);
                            try self.emitLoad(.w64, payload_reg, frame_ptr, ptr_slot);
                            try self.emitRcHelperCallFromPtrReg(child_key, payload_reg, count_slot, roc_ops_slot);
                        }
                        return;
                    }

                    const disc_offset: i32 = @intCast(resolver.tagUnionDiscriminantOffset(tag_plan));
                    const disc_size = resolver.tagUnionDiscriminantSize(tag_plan);
                    const total_size = resolver.tagUnionTotalSize(tag_plan);
                    const disc_use_w32 = (disc_offset + 8 > @as(i32, @intCast(total_size)));

                    var done_patches: std.ArrayList(usize) = .empty;
                    defer done_patches.deinit(self.allocator);

                    var variant_i: u32 = 0;
                    while (variant_i < variant_count) : (variant_i += 1) {
                        const child_key = resolver.tagUnionVariantPlan(tag_plan, variant_i) orelse continue;

                        const value_ptr_reg = try self.allocTempGeneral();
                        const disc_reg = try self.allocTempGeneral();
                        try self.emitLoad(.w64, value_ptr_reg, frame_ptr, ptr_slot);
                        if (disc_use_w32) {
                            try self.emitLoad(.w32, disc_reg, value_ptr_reg, disc_offset);
                        } else {
                            try self.emitLoad(.w64, disc_reg, value_ptr_reg, disc_offset);
                        }
                        self.codegen.freeGeneral(value_ptr_reg);

                        if (disc_size < 8) {
                            const disc_mask: u64 = (@as(u64, 1) << @intCast(disc_size * 8)) - 1;
                            const mask_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(mask_reg, @bitCast(disc_mask));
                            try self.emitAndRegs(.w64, disc_reg, disc_reg, mask_reg);
                            self.codegen.freeGeneral(mask_reg);
                        }

                        try self.emitCmpImm(disc_reg, @intCast(variant_i));
                        self.codegen.freeGeneral(disc_reg);

                        const skip_patch = try self.emitJumpIfNotEqual();
                        const payload_reg = try self.allocTempGeneral();
                        defer self.codegen.freeGeneral(payload_reg);
                        try self.emitLoad(.w64, payload_reg, frame_ptr, ptr_slot);
                        try self.emitRcHelperCallFromPtrReg(child_key, payload_reg, count_slot, roc_ops_slot);
                        try done_patches.append(self.allocator, try self.codegen.emitJump());
                        self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
                    }

                    const done_offset = self.codegen.currentOffset();
                    for (done_patches.items) |patch| {
                        self.codegen.patchJump(patch, done_offset);
                    }
                },
                .closure => |child_key| {
                    const captures_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(captures_reg);
                    try self.emitLoad(.w64, captures_reg, frame_ptr, ptr_slot);
                    try self.emitRcHelperCallFromPtrReg(child_key, captures_reg, count_slot, roc_ops_slot);
                },
            }
        }

        fn compileRcHelper(self: *Self, helper_key: RcHelperKey) Allocator.Error!usize {
            const cache_key = helper_key.encode();
            if (self.compiled_rc_helpers.get(cache_key)) |code_offset| {
                return code_offset;
            }

            const resolver = RcHelperResolver.init(self.layout_store);
            const helper_plan = resolver.plan(helper_key);
            if (helper_plan == .noop) {
                if (builtin.mode == .Debug) {
                    std.debug.panic("attempted to compile noop RC helper for layout {d}", .{@intFromEnum(helper_key.layout_idx)});
                }
                unreachable;
            }

            const skip_jump = try self.codegen.emitJump();

            const saved_stack_offset = self.codegen.stack_offset;
            const saved_callee_saved_used = self.codegen.callee_saved_used;
            const saved_callee_saved_available = self.codegen.callee_saved_available;
            const saved_free_general = self.codegen.free_general;
            const saved_general_owners = self.codegen.general_owners;
            const saved_free_float = self.codegen.free_float;
            const saved_float_owners = self.codegen.float_owners;
            const saved_roc_ops_reg = self.roc_ops_reg;
            const saved_ret_ptr_slot = self.ret_ptr_slot;
            const saved_binding_symbol = self.current_binding_symbol;

            // Reset register state for new function scope — each RC helper is a
            // separate callable with its own prologue/epilogue, so it starts with
            // a full set of registers regardless of what the parent is using.
            self.codegen.callee_saved_used = 0;
            self.codegen.callee_saved_available = CodeGen.CALLEE_SAVED_GENERAL_MASK;
            self.codegen.free_general = CodeGen.INITIAL_FREE_GENERAL;
            self.codegen.general_owners = [_]?u32{null} ** CodeGen.NUM_GENERAL_REGS;
            self.codegen.free_float = CodeGen.INITIAL_FREE_FLOAT;
            self.codegen.float_owners = [_]?u32{null} ** CodeGen.NUM_FLOAT_REGS;
            self.current_binding_symbol = null;
            self.roc_ops_reg = null;

            if (comptime target.toCpuArch() == .x86_64) {
                self.codegen.stack_offset = -CodeGen.CALLEE_SAVED_AREA_SIZE;
            } else {
                self.codegen.stack_offset = 16 + CodeGen.CALLEE_SAVED_AREA_SIZE;
            }

            const body_start = self.codegen.currentOffset();
            const relocs_before = self.codegen.relocations.items.len;
            try self.compiled_rc_helpers.put(cache_key, body_start);

            errdefer {
                _ = self.compiled_rc_helpers.remove(cache_key);
                self.codegen.stack_offset = saved_stack_offset;
                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.codegen.free_general = saved_free_general;
                self.codegen.general_owners = saved_general_owners;
                self.codegen.free_float = saved_free_float;
                self.codegen.float_owners = saved_float_owners;
                self.roc_ops_reg = saved_roc_ops_reg;
                self.ret_ptr_slot = saved_ret_ptr_slot;
                self.current_binding_symbol = saved_binding_symbol;
                self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
            }

            const ptr_slot = self.codegen.allocStackSlot(8);
            const roc_ops_slot = self.codegen.allocStackSlot(8);
            const ptr_arg_reg = self.getArgumentRegister(0);
            try self.codegen.emitStoreStack(.w64, ptr_slot, ptr_arg_reg);

            var count_slot: ?i32 = null;
            switch (helper_key.op) {
                .incref => {
                    const count_arg_reg = self.getArgumentRegister(1);
                    const roc_ops_arg_reg = self.getArgumentRegister(2);
                    count_slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStack(.w64, count_slot.?, count_arg_reg);
                    try self.codegen.emitStoreStack(.w64, roc_ops_slot, roc_ops_arg_reg);
                },
                .decref, .free => {
                    const roc_ops_arg_reg = self.getArgumentRegister(1);
                    try self.codegen.emitStoreStack(.w64, roc_ops_slot, roc_ops_arg_reg);
                },
            }

            const ptr_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, ptr_reg, frame_ptr, ptr_slot);
            try self.emitCmpImm(ptr_reg, 0);
            self.codegen.freeGeneral(ptr_reg);
            const early_return_patch = try self.emitJumpIfEqual();

            try self.generateRcHelperBody(helper_key, ptr_slot, count_slot, roc_ops_slot);

            const body_epilogue_offset = self.codegen.currentOffset();
            {
                const actual_locals: u32 = if (comptime target.toCpuArch() == .aarch64)
                    @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE)
                else
                    @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                var builder = CodeGen.DeferredFrameBuilder.init();
                builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                builder.setStackSize(actual_locals);
                try builder.emitEpilogue(&self.codegen.emit);
            }

            self.codegen.patchJump(early_return_patch, body_epilogue_offset);
            const body_end = self.codegen.currentOffset();

            const final_offset = if (comptime target.toCpuArch() == .x86_64) blk: {
                const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                const prologue_start = self.codegen.currentOffset();
                const actual_locals_x86: u32 = @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                try self.codegen.emitPrologueWithAlloc(actual_locals_x86);
                const prologue_size = self.codegen.currentOffset() - prologue_start;

                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size);
                }

                self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size, cache_key);
                self.repatchInternalCalls(body_start, body_end, prologue_size, body_start);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size, body_start);
                break :blk prologue_start;
            } else blk: {
                const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                const prologue_start = self.codegen.currentOffset();
                const actual_locals: u32 = @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE);
                var frame_builder = CodeGen.DeferredFrameBuilder.init();
                frame_builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                frame_builder.setStackSize(actual_locals);
                _ = try frame_builder.emitPrologue(&self.codegen.emit);
                const prologue_size = self.codegen.currentOffset() - prologue_start;

                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size);
                }

                self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size, cache_key);
                self.repatchInternalCalls(body_start, body_end, prologue_size, body_start);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size, body_start);
                break :blk prologue_start;
            };

            if (self.compiled_rc_helpers.getPtr(cache_key)) |entry| {
                entry.* = final_offset;
            }

            self.codegen.stack_offset = saved_stack_offset;
            self.codegen.callee_saved_used = saved_callee_saved_used;
            self.codegen.callee_saved_available = saved_callee_saved_available;
            self.codegen.free_general = saved_free_general;
            self.codegen.general_owners = saved_general_owners;
            self.codegen.free_float = saved_free_float;
            self.codegen.float_owners = saved_float_owners;
            self.roc_ops_reg = saved_roc_ops_reg;
            self.current_binding_symbol = saved_binding_symbol;

            self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
            return final_offset;
        }

        fn procCodeOffsetWithOptions(
            self: *Self,
            proc_id: lir.LIR.LirProcSpecId,
            _: LambdaProcOptions,
        ) Allocator.Error!CompiledProc {
            const proc = self.store.getProcSpec(proc_id);
            if (self.proc_registry.get(@intFromEnum(proc_id))) |compiled| return compiled;

            if (std.debug.runtime_safety) std.debug.panic(
                "proc call target {d} ({d}) is missing from the compiled proc registry",
                .{ @intFromEnum(proc_id), proc.name.raw() },
            );
            unreachable;
        }

        /// Resolve the pre-lowered comparator proc for list_sort_with to a compiled code offset.
        fn resolveComparatorOffset(self: *Self, proc_id: lir.LIR.LirProcSpecId) Allocator.Error!usize {
            const compiled = try self.procCodeOffsetWithOptions(proc_id, .{});
            if (compiled.code_start == unresolved_proc_code_start) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "list_sort_with comparator proc {d} was not compiled before codegen",
                        .{@intFromEnum(proc_id)},
                    );
                }
                unreachable;
            }
            return compiled.code_start;
        }

        /// Generate code for a direct proc call.
        fn generateCall(self: *Self, call: anytype) Allocator.Error!ValueLocation {
            const proc = try self.procCodeOffsetWithOptions(call.proc, .{});
            return try self.generateCallToCompiledProc(proc, call.args, call.ret_layout);
        }

        /// Copy a value location to a stack slot.
        fn copyToStackSlot(self: *Self, slot: i32, loc: ValueLocation, size: u32) Allocator.Error!void {
            try self.copyBytesToStackOffset(slot, loc, size);
        }

        /// Emit compare immediate instruction
        fn emitCmpImm(self: *Self, reg: GeneralReg, value: i64) !void {
            if (comptime target.toCpuArch() == .aarch64) {
                // CMP reg, #imm12
                try self.codegen.emit.cmpRegImm12(.w64, reg, @intCast(value));
            } else {
                // x86_64: CMP reg, imm32
                // Load immediate into temporary register and compare
                const temp = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(temp, value);
                try self.codegen.emit.cmpRegReg(.w64, reg, temp);
                self.codegen.freeGeneral(temp);
            }
        }

        /// Emit jump if not equal (after comparison)
        ///
        /// BRANCH PATCHING MECHANISM:
        /// When generating switch dispatch, we don't know the jump target offset until
        /// we've generated the code for the branch body. So we:
        /// 1. Emit the branch instruction with offset=0 (placeholder)
        /// 2. Record the instruction's location (patch_loc)
        /// 3. Generate the branch body code
        /// 4. Calculate the actual offset: current_offset - patch_loc
        /// 5. Patch the instruction at patch_loc with the real offset
        ///
        /// WHY OFFSET 0 IS SAFE:
        /// Offset 0 means "jump to the next instruction" which is harmless if we
        /// somehow fail to patch. But in normal operation, codegen.patchJump()
        /// overwrites the placeholder before execution.
        ///
        /// RETURNS: The patch location (where the displacement bytes are) for later patching.
        fn emitJumpIfNotEqual(self: *Self) !usize {
            if (comptime target.toCpuArch() == .aarch64) {
                // B.NE (branch if not equal) with placeholder offset
                // On aarch64, the entire 4-byte instruction encodes the offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.ne, 0);
                return patch_loc;
            } else {
                // JNE (jump if not equal) with placeholder offset
                // x86_64: JNE rel32 is 0F 85 xx xx xx xx (6 bytes)
                // The displacement starts at offset +2, so patch_loc = currentOffset + 2
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jne(@bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Emit a conditional jump for unsigned less than (for list length comparisons)
        fn emitJumpIfLessThan(self: *Self) !usize {
            if (comptime target.toCpuArch() == .aarch64) {
                // B.CC (branch if carry clear = unsigned less than) with placeholder offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.cc, 0);
                return patch_loc;
            } else {
                // JB (jump if below = unsigned less than) with placeholder offset
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jccRel32(.below, @bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Emit a conditional jump if equal (for while loop false condition check)
        fn emitJumpIfEqual(self: *Self) !usize {
            if (comptime target.toCpuArch() == .aarch64) {
                // B.EQ (branch if equal) with placeholder offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.eq, 0);
                return patch_loc;
            } else {
                // JE (jump if equal) with placeholder offset
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jccRel32(.equal, @bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Generate a call to an already-compiled procedure.
        /// This is used for recursive functions that were compiled via compileAllProcSpecs.
        const PassByPtrPlan = struct {
            start: u32,
            slice: []bool,
        };

        fn computePassByPtrPlan(self: *Self, arg_infos: []const ArgInfo, initial_reg_idx: u8, emit_roc_ops: bool) Allocator.Error!PassByPtrPlan {
            const pbp_start: u32 = self.scratch_pass_by_ptr.top();
            for (0..arg_infos.len) |_| try self.scratch_pass_by_ptr.append(false);
            const pass_by_ptr = self.scratch_pass_by_ptr.sliceFromStart(pbp_start);

            const pnr_start = self.scratch_param_num_regs.top();
            defer self.scratch_param_num_regs.clearFrom(pnr_start);
            for (arg_infos) |info| try self.scratch_param_num_regs.append(info.num_regs);
            const param_num_regs = self.scratch_param_num_regs.sliceFromStart(pnr_start);

            var reg_count: u8 = initial_reg_idx;
            for (param_num_regs, 0..) |nr, i| {
                const is_i128_arg = self.argNeedsI128Abi(arg_infos[i].loc, arg_infos[i].layout_idx);
                if (comptime target.toCpuArch() == .aarch64) {
                    if (!pass_by_ptr[i] and is_i128_arg and reg_count < max_arg_regs and reg_count % 2 != 0) {
                        reg_count += 1;
                    }
                }
                if (reg_count + nr <= max_arg_regs) {
                    reg_count += nr;
                } else if (nr > 1) {
                    pass_by_ptr[i] = true;
                    if (reg_count + 1 <= max_arg_regs) {
                        reg_count += 1;
                    } else {
                        reg_count = max_arg_regs;
                    }
                } else {
                    reg_count = max_arg_regs;
                }
            }

            if (emit_roc_ops) {
                while (reg_count + 1 > max_arg_regs) {
                    var found = false;
                    var best_idx: usize = 0;
                    var best_regs: u8 = 0;
                    for (param_num_regs, 0..) |nr, i| {
                        if (!pass_by_ptr[i] and nr > 1 and nr > best_regs) {
                            best_idx = i;
                            best_regs = nr;
                            found = true;
                        }
                    }
                    if (!found) break;
                    pass_by_ptr[best_idx] = true;
                    // Recompute register pressure after changing pass-by-pointer plan,
                    // including aarch64 i128 alignment effects.
                    reg_count = initial_reg_idx;
                    for (param_num_regs, 0..) |nr, i| {
                        const pbp = pass_by_ptr[i];
                        const is_i128_arg = self.argNeedsI128Abi(arg_infos[i].loc, arg_infos[i].layout_idx);
                        if (comptime target.toCpuArch() == .aarch64) {
                            if (!pbp and is_i128_arg and reg_count < max_arg_regs and reg_count % 2 != 0) {
                                reg_count += 1;
                            }
                        }
                        const eff_nr: u8 = if (pbp) 1 else nr;
                        if (reg_count + eff_nr <= max_arg_regs) {
                            reg_count += eff_nr;
                        } else {
                            reg_count = max_arg_regs;
                        }
                    }
                }
            }

            return .{ .start = pbp_start, .slice = pass_by_ptr };
        }

        fn generateCallToCompiledProc(self: *Self, proc: CompiledProc, args_span: anytype, ret_layout: layout.Idx) Allocator.Error!ValueLocation {
            const args = self.store.getExprSpan(args_span);
            const proc_arg_layouts = self.store.getLayoutIdxSpan(proc.arg_layouts);
            const needs_ret_ptr = self.needsInternalReturnByPointer(ret_layout);
            const ret_buffer_offset = if (needs_ret_ptr) blk: {
                const size = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(ret_layout)).size;
                break :blk self.codegen.allocStackSlot(size);
            } else 0;

            // Pass 1: Generate all argument expressions and calculate register needs
            const arg_infos_start = self.scratch_arg_infos.top();
            defer self.scratch_arg_infos.clearFrom(arg_infos_start);

            for (args, 0..) |arg_id, arg_index| {
                const arg_layout: ?layout.Idx = if (arg_index < proc_arg_layouts.len)
                    proc_arg_layouts[arg_index]
                else
                    self.exprLayout(arg_id);
                const raw_arg_loc = try self.generateExpr(arg_id);
                const arg_loc = if (arg_layout) |layout_idx|
                    self.coerceImmediateToLayout(raw_arg_loc, layout_idx)
                else
                    raw_arg_loc;
                const num_regs: u8 = self.calcArgRegCount(arg_loc, arg_layout);
                try self.scratch_arg_infos.append(.{ .loc = arg_loc, .layout_idx = arg_layout, .num_regs = num_regs });
            }
            const arg_infos = self.scratch_arg_infos.sliceFromStart(arg_infos_start);
            // Pass 2: Place arguments and emit call
            const initial_arg_reg_idx: u8 = if (needs_ret_ptr) 1 else 0;
            const pbp_plan = try self.computePassByPtrPlan(arg_infos, initial_arg_reg_idx, true);
            defer self.scratch_pass_by_ptr.clearFrom(pbp_plan.start);
            const stack_spill_size = try self.placeCallArguments(arg_infos, .{
                .needs_ret_ptr = needs_ret_ptr,
                .ret_buffer_offset = ret_buffer_offset,
                .pass_by_ptr = pbp_plan.slice,
                .emit_roc_ops = true,
            });
            if (proc.code_start == unresolved_proc_code_start) {
                try self.emitPendingCallToProc(proc.id);
            } else {
                try self.emitCallToOffset(proc.code_start);
            }

            if (stack_spill_size > 0) {
                try self.emitAddStackPtr(stack_spill_size);
            }

            return self.saveCallReturnValue(ret_layout, needs_ret_ptr, ret_buffer_offset);
        }

        fn emitPendingCallToProc(self: *Self, target_proc: lir.LIR.LirProcSpecId) !void {
            const call_site = self.codegen.currentOffset();
            try self.pending_calls.append(self.allocator, .{
                .call_site = call_site,
                .target_proc = target_proc,
            });

            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.bl(0);
            } else {
                try self.codegen.emit.call(@bitCast(@as(i32, 0)));
            }
        }

        /// Move a value to a specific register
        fn moveToReg(self: *Self, loc: ValueLocation, target_reg: GeneralReg) Allocator.Error!void {
            switch (loc) {
                .general_reg => |src_reg| {
                    if (src_reg != target_reg) {
                        try self.emitMovRegReg(target_reg, src_reg);
                    }
                },
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(target_reg, val);
                },
                .immediate_i128 => |val| {
                    // Only load low 64 bits into single register
                    const low: i64 = @truncate(val);
                    try self.codegen.emitLoadImm(target_reg, low);
                },
                .stack => |s| {
                    const offset = s.offset;
                    try self.codegen.emitLoadStack(.w64, target_reg, offset);
                },
                .stack_i128 => |offset| {
                    // Only load low 64 bits
                    try self.codegen.emitLoadStack(.w64, target_reg, offset);
                },
                .stack_str => |offset| {
                    // Load ptr/data (first 8 bytes of string struct)
                    try self.codegen.emitLoadStack(.w64, target_reg, offset);
                },
                .list_stack => |list_info| {
                    // Load ptr (first 8 bytes of list struct)
                    try self.codegen.emitLoadStack(.w64, target_reg, list_info.struct_offset);
                },
                .float_reg => |freg| {
                    // Calls use general argument registers; pass float values as raw bits.
                    const slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStackF64(slot, freg);
                    try self.codegen.emitLoadStack(.w64, target_reg, slot);
                },
                .immediate_f64 => |val| {
                    const bits: u64 = @bitCast(val);
                    try self.codegen.emitLoadImm(target_reg, @bitCast(bits));
                },
                .noreturn => unreachable,
            }
        }

        fn materializeValueToStackForLayout(
            self: *Self,
            value_loc: ValueLocation,
            layout_idx: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const normalized_value_loc = self.coerceImmediateToLayout(value_loc, layout_idx);

            return switch (normalized_value_loc) {
                .stack, .stack_i128, .stack_str, .list_stack, .noreturn => normalized_value_loc,
                else => blk: {
                    const size = self.getLayoutSize(layout_idx);
                    if (size == 0) {
                        break :blk self.stackLocationForLayout(layout_idx, 0);
                    }

                    const slot = self.codegen.allocStackSlot(size);
                    try self.copyBytesToStackOffset(slot, normalized_value_loc, size);
                    break :blk self.stackLocationForLayout(layout_idx, slot);
                },
            };
        }

        /// Returns true when an immediate i128 cannot be represented as a sign-extended i64.
        /// Ambiguous literals (e.g. `5`) are carried as immediate_i128 in some paths, but
        /// should not force a 128-bit ABI unless the layout requires it.
        fn immediateI128NeedsWideAbi(val: i128) bool {
            const low: i64 = @truncate(val);
            return @as(i128, low) != val;
        }

        /// Determine whether an argument must use the 128-bit ABI (two registers).
        fn argNeedsI128Abi(_: *Self, arg_loc: ValueLocation, arg_layout: ?layout.Idx) bool {
            if (arg_layout) |al| {
                return al == .dec or al == .i128 or al == .u128;
            }

            return switch (arg_loc) {
                .stack_i128 => true,
                .immediate_i128 => |v| immediateI128NeedsWideAbi(v),
                else => false,
            };
        }

        /// Calculate the number of registers an argument needs based on its location and layout.
        fn calcArgRegCount(self: *Self, arg_loc: ValueLocation, arg_layout: ?layout.Idx) u8 {
            const is_i128_arg = self.argNeedsI128Abi(arg_loc, arg_layout);
            if (is_i128_arg) return 2;

            // Check for list/string types - need 3 registers (24 bytes: ptr, len, capacity)
            if (arg_loc == .list_stack or arg_loc == .stack_str) return 3;
            if (arg_layout) |al| {
                if (al == .str) return 3; // Strings are 24 bytes
                {
                    const ls = self.layout_store;
                    const layout_val = ls.getLayout(al);
                    if (layout_val.tag == .zst or ls.layoutSizeAlign(layout_val).size == 0) return 0;
                    if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 3;
                    // Check for aggregate values > 8 bytes
                    if (layout_val.tag == .struct_ or layout_val.tag == .tag_union or layout_val.tag == .closure) {
                        const size = ls.layoutSizeAlign(layout_val).size;
                        if (size > 8) return @intCast((size + 7) / 8);
                    }
                }
            }

            // Default: single register
            return 1;
        }

        /// Spill an argument to the stack (for arguments that don't fit in registers).
        /// stack_offset is the offset from RSP (x86_64) or SP (aarch64) where the argument should be placed.
        fn spillArgToStack(self: *Self, arg_loc: ValueLocation, arg_layout: ?layout.Idx, stack_offset: i32, num_regs: u8) Allocator.Error!void {
            // Use a temporary register for copying
            const temp_reg: GeneralReg = scratch_reg;

            switch (arg_loc) {
                .stack_i128, .stack_str => |src_offset| {
                    // Copy from local stack to argument stack area
                    var ri: u8 = 0;
                    while (ri < num_regs) : (ri += 1) {
                        const off: i32 = @as(i32, ri) * 8;
                        try self.emitLoad(.w64, temp_reg, frame_ptr, src_offset + off);
                        try self.emitStore(.w64, stack_ptr, stack_offset + off, temp_reg);
                    }
                },
                .stack => |s| {
                    const src_offset = s.offset;
                    // Copy from local stack to argument stack area
                    if (num_regs == 1 and s.size != .qword) {
                        try self.emitSizedLoadStack(temp_reg, src_offset, s.size);
                        try self.emitStore(.w64, stack_ptr, stack_offset, temp_reg);
                    } else {
                        var ri: u8 = 0;
                        while (ri < num_regs) : (ri += 1) {
                            const off: i32 = @as(i32, ri) * 8;
                            try self.emitLoad(.w64, temp_reg, frame_ptr, src_offset + off);
                            try self.emitStore(.w64, stack_ptr, stack_offset + off, temp_reg);
                        }
                    }
                },
                .list_stack => |info| {
                    // List is 24 bytes (3 registers)
                    var ri: u8 = 0;
                    while (ri < num_regs) : (ri += 1) {
                        const off: i32 = @as(i32, ri) * 8;
                        try self.emitLoad(.w64, temp_reg, frame_ptr, info.struct_offset + off);
                        try self.emitStore(.w64, stack_ptr, stack_offset + off, temp_reg);
                    }
                },
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(temp_reg, val);
                    try self.emitStore(.w64, stack_ptr, stack_offset, temp_reg);
                },
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(low));
                    try self.emitStore(.w64, stack_ptr, stack_offset, temp_reg);
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(high));
                    try self.emitStore(.w64, stack_ptr, stack_offset + 8, temp_reg);
                },
                .general_reg => |reg| {
                    try self.emitStore(.w64, stack_ptr, stack_offset, reg);
                },
                else => {
                    if (arg_layout == .f32) {
                        const bits_reg = try self.materializeF32BitsInGeneralReg(arg_loc);
                        if (bits_reg != temp_reg) {
                            try self.codegen.emit.movRegReg(.w64, temp_reg, bits_reg);
                            self.codegen.freeGeneral(bits_reg);
                        }
                    } else {
                        // For other types, try to move to temp register first
                        try self.moveToReg(arg_loc, temp_reg);
                    }
                    try self.emitStore(.w64, stack_ptr, stack_offset, temp_reg);
                },
            }
        }

        /// Get the size in bytes for a layout index.
        /// Allocate a general register with a unique temporary local ID.
        /// Use this for temporary registers that don't correspond to real local variables.
        /// This prevents register ownership conflicts that can corrupt spill tracking.
        fn allocTempGeneral(self: *Self) Allocator.Error!GeneralReg {
            const local_id = self.next_temp_local;
            self.next_temp_local +%= 1;
            return self.codegen.allocGeneralFor(local_id);
        }

        /// Call a builtin function using either direct function pointer (native mode)
        /// or symbol reference (object file mode) depending on generation_mode.
        ///
        /// This helper abstracts the difference between:
        /// - Native execution: Direct function pointers work because code runs in-process
        /// - Object file generation: Need symbol references that the linker will resolve
        ///
        /// Arguments:
        /// - builder: The CallBuilder with arguments already set up
        /// - fn_addr: Direct function address for native execution mode
        /// - symbol_name: Symbol name for object file mode (must match export in dev_wrappers.zig)
        fn callBuiltin(self: *Self, builder: *Builder, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!void {
            switch (self.generation_mode) {
                .native_execution => {
                    try builder.call(fn_addr);
                },
                .object_file => {
                    try builder.callRelocatable(builtin_fn.symbolName(), self.allocator, &self.codegen.relocations);
                },
            }
        }

        /// Ensure a value location is on the stack, spilling if needed. Returns stack offset.
        fn ensureOnStack(self: *Self, loc: ValueLocation, size: u32) Allocator.Error!i32 {
            return switch (loc) {
                .stack_i128, .stack_str => |off| off,
                .stack => |s| s.offset,
                .list_stack => |info| info.struct_offset,
                .general_reg => |reg| blk: {
                    const slot = self.codegen.allocStackSlot(@intCast(size));
                    try self.emitStore(.w64, frame_ptr, slot, reg);
                    self.codegen.freeGeneral(reg);
                    break :blk slot;
                },
                .immediate_i64 => |val| blk: {
                    const slot = self.codegen.allocStackSlot(@max(8, @as(u32, @intCast(size))));
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, val);
                    try self.emitStore(.w64, frame_ptr, slot, temp);
                    self.codegen.freeGeneral(temp);
                    break :blk slot;
                },
                .immediate_i128 => |val| blk: {
                    // Store 128-bit immediate to stack
                    const slot = self.codegen.allocStackSlot(16);
                    const temp = try self.allocTempGeneral();
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    try self.codegen.emitLoadImm(temp, @bitCast(low));
                    try self.emitStore(.w64, frame_ptr, slot, temp);
                    try self.codegen.emitLoadImm(temp, @bitCast(high));
                    try self.emitStore(.w64, frame_ptr, slot + 8, temp);
                    self.codegen.freeGeneral(temp);
                    break :blk slot;
                },
                else => {
                    unreachable;
                },
            };
        }

        /// Ensure a value is in a general-purpose register
        fn ensureInGeneralReg(self: *Self, loc: ValueLocation) Allocator.Error!GeneralReg {
            switch (loc) {
                .general_reg => |reg| return reg,
                .immediate_i64 => |val| {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, val);
                    return reg;
                },
                .immediate_i128 => |val| {
                    // Only load low 64 bits
                    const reg = try self.allocTempGeneral();
                    const low: i64 = @truncate(val);
                    try self.codegen.emitLoadImm(reg, low);
                    return reg;
                },
                .stack => |s| {
                    const reg = try self.allocTempGeneral();
                    try self.emitSizedLoadStack(reg, s.offset, s.size);
                    return reg;
                },
                .stack_i128 => |offset| {
                    // Only load low 64 bits
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    return reg;
                },
                .stack_str => |offset| {
                    // Load ptr/data (first 8 bytes of string struct)
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    return reg;
                },
                .list_stack => |list_info| {
                    // Load ptr (first 8 bytes of list struct)
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, list_info.struct_offset);
                    return reg;
                },
                .float_reg => |freg| {
                    // Some call paths pass all args in general regs; preserve float bits.
                    const reg = try self.allocTempGeneral();
                    const slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStackF64(slot, freg);
                    try self.codegen.emitLoadStack(.w64, reg, slot);
                    return reg;
                },
                .immediate_f64 => |val| {
                    const reg = try self.allocTempGeneral();
                    const bits: u64 = @bitCast(val);
                    try self.codegen.emitLoadImm(reg, @bitCast(bits));
                    return reg;
                },
                .noreturn => unreachable,
            }
        }

        fn materializeF32BitsInGeneralReg(self: *Self, loc: ValueLocation) Allocator.Error!GeneralReg {
            switch (loc) {
                .general_reg => |reg| return reg,
                .immediate_i64 => |val| {
                    const f32_val: f32 = @floatFromInt(val);
                    const bits: u32 = @bitCast(f32_val);
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @as(i64, bits));
                    return reg;
                },
                .immediate_f64 => |val| {
                    const f32_val: f32 = @floatCast(val);
                    const bits: u32 = @bitCast(f32_val);
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @as(i64, bits));
                    return reg;
                },
                .float_reg => |freg| {
                    const reg = try self.allocTempGeneral();
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.fcvtFloatFloat(.single, freg, .double, freg);
                        try self.codegen.emit.fmovGenFromFloat(.single, reg, freg);
                    } else {
                        const slot = self.codegen.allocStackSlot(4);
                        try self.codegen.emit.cvtsd2ssRegReg(freg, freg);
                        try self.codegen.emit.movssMemReg(.RBP, slot, freg);
                        try self.codegen.emitLoadStack(.w32, reg, slot);
                    }
                    return reg;
                },
                .stack => |s| {
                    const reg = try self.allocTempGeneral();
                    if (s.size == .dword) {
                        // Tag-union payloads and other structural fields can hold a real
                        // 4-byte F32 on the stack instead of the widened F64 carrier used
                        // by float temporaries. Preserve those payload bits as-is.
                        try self.codegen.emitLoadStack(.w32, reg, s.offset);
                    } else {
                        const freg = self.codegen.allocFloat() orelse unreachable;
                        try self.codegen.emitLoadStackF64(freg, s.offset);
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.fcvtFloatFloat(.single, freg, .double, freg);
                            try self.codegen.emit.fmovGenFromFloat(.single, reg, freg);
                        } else {
                            const slot = self.codegen.allocStackSlot(4);
                            try self.codegen.emit.cvtsd2ssRegReg(freg, freg);
                            try self.codegen.emit.movssMemReg(.RBP, slot, freg);
                            try self.codegen.emitLoadStack(.w32, reg, slot);
                        }
                        self.codegen.freeFloat(freg);
                    }
                    return reg;
                },
                .noreturn => unreachable,
                else => unreachable,
            }
        }

        /// Convert callable values stored on stack to a concrete stack ValueLocation
        /// based on the expected return layout.
        fn normalizeResultLocForLayout(_: *Self, loc: ValueLocation, _: layout.Idx) ValueLocation {
            return loc;
        }

        /// Normalize immediate literal representation to match the target layout.
        /// This prevents wide/default literal carriers (e.g. immediate_i128) from
        /// leaking into narrower typed bindings/calls.
        fn coerceImmediateToLayout(_: *Self, loc: ValueLocation, target_layout: layout.Idx) ValueLocation {
            return switch (target_layout) {
                .f32, .f64 => switch (loc) {
                    .immediate_i64 => |v| .{ .immediate_f64 = @floatFromInt(v) },
                    .immediate_i128 => |v| .{ .immediate_f64 = @floatFromInt(v) },
                    else => loc,
                },
                .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => switch (loc) {
                    .immediate_i128 => |v| .{ .immediate_i64 = @truncate(v) },
                    else => loc,
                },
                .i128, .u128, .dec => switch (loc) {
                    .immediate_i64 => |v| .{ .immediate_i128 = v },
                    else => loc,
                },
                else => loc,
            };
        }

        /// Ensure a value is in a floating-point register
        fn ensureInFloatReg(self: *Self, loc: ValueLocation) Allocator.Error!FloatReg {
            switch (loc) {
                .float_reg => |reg| return reg,
                .immediate_f64 => |val| {
                    const reg = self.codegen.allocFloat() orelse unreachable;
                    const bits: u64 = @bitCast(val);

                    if (bits == 0) {
                        // Special case: 0.0 can be loaded efficiently
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.fmovFloatFromGen(.double, reg, .ZRSP);
                        } else {
                            try self.codegen.emit.xorpdRegReg(reg, reg);
                        }
                    } else {
                        if (comptime target.toCpuArch() == .aarch64) {
                            // Load bits into scratch register, then FMOV to float register
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(bits));
                            try self.codegen.emit.fmovFloatFromGen(.double, reg, .IP0);
                        } else {
                            // x86_64: Store bits to stack, then load into float register
                            const stack_offset = self.codegen.allocStackSlot(8);
                            try self.codegen.emit.movRegImm64(.R11, @bitCast(bits));
                            try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, .R11);
                            try self.codegen.emit.movsdRegMem(reg, .RBP, stack_offset);
                        }
                    }
                    return reg;
                },
                .stack => |s| {
                    const reg = self.codegen.allocFloat() orelse unreachable;
                    if (s.size == .dword) {
                        if (comptime target.toCpuArch() == .aarch64) {
                            const bits_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadStack(.w32, bits_reg, s.offset);
                            try self.codegen.emit.fmovFloatFromGen(.single, reg, bits_reg);
                            self.codegen.freeGeneral(bits_reg);
                            try self.codegen.emit.fcvtFloatFloat(.double, reg, .single, reg);
                        } else {
                            try self.codegen.emit.movssRegMem(reg, .RBP, s.offset);
                            try self.codegen.emit.cvtss2sdRegReg(reg, reg);
                        }
                    } else {
                        try self.codegen.emitLoadStackF64(reg, s.offset);
                    }
                    return reg;
                },
                .immediate_i64 => |val| {
                    // Integer literal used in float context — convert at compile time
                    const f_val: f64 = @floatFromInt(val);
                    return self.ensureInFloatReg(.{ .immediate_f64 = f_val });
                },
                .general_reg, .immediate_i128, .stack_i128, .stack_str, .list_stack => {
                    unreachable;
                },
                .noreturn => unreachable,
            }
        }

        /// Store the result to the output buffer pointed to by a saved register
        /// This is used when the original result pointer (X0/RDI) may have been clobbered
        fn storeResultToSavedPtr(self: *Self, loc: ValueLocation, result_layout: layout.Idx, saved_ptr_reg: GeneralReg, tuple_len: usize) Allocator.Error!void {
            // Handle tuples specially - copy all elements from stack to result buffer
            if (tuple_len > 1) {
                switch (loc) {
                    .stack => |s| {
                        const base_offset = s.offset;
                        // Use layout store for accurate element offsets and sizes
                        {
                            const ls = self.layout_store;
                            const tuple_layout = ls.getLayout(result_layout);
                            if (tuple_layout.tag == .struct_) {
                                const tuple_data = ls.getStructData(tuple_layout.data.struct_.idx);
                                const total_size = tuple_data.size;

                                // Copy entire tuple as 8-byte chunks
                                const temp_reg = try self.allocTempGeneral();
                                var copied: u32 = 0;

                                while (copied < total_size) {
                                    const stack_offset = base_offset + @as(i32, @intCast(copied));
                                    const buf_offset: i32 = @as(i32, @intCast(copied));

                                    // Load from stack
                                    try self.emitLoad(.w64, temp_reg, frame_ptr, stack_offset);

                                    // Store to result buffer
                                    try self.emitStore(.w64, saved_ptr_reg, buf_offset, temp_reg);

                                    copied += 8;
                                }

                                self.codegen.freeGeneral(temp_reg);
                                return;
                            }
                        }

                        // Fallback: copy tuple_len * 8 bytes
                        const temp_reg = try self.allocTempGeneral();
                        for (0..tuple_len) |i| {
                            const stack_offset = base_offset + @as(i32, @intCast(i)) * 8;
                            const buf_offset: i32 = @as(i32, @intCast(i)) * 8;

                            try self.emitLoad(.w64, temp_reg, frame_ptr, stack_offset);
                            try self.emitStore(.w64, saved_ptr_reg, buf_offset, temp_reg);
                        }
                        self.codegen.freeGeneral(temp_reg);
                        return;
                    },
                    else => {
                        // Fallback - just store the single value
                    },
                }
            }

            switch (result_layout) {
                .i64, .i32, .i16, .u64, .u32, .u16 => {
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreToMem(saved_ptr_reg, reg);
                },
                .u8 => {
                    // Zero-extend to 64 bits before storing, since the register
                    // may have garbage in the upper bits from mutable variable loads.
                    // Shift left 56, then logical shift right 56 to clear upper bits.
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitShlImm(.w64, reg, reg, 56);
                    try self.emitLsrImm(.w64, reg, reg, 56);
                    try self.emitStoreToMem(saved_ptr_reg, reg);
                },
                .i8 => {
                    // Sign-extend to 64 bits before storing, since the register
                    // may have garbage in the upper bits from mutable variable loads.
                    // Shift left 56, then arithmetic shift right 56 to sign-extend.
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitShlImm(.w64, reg, reg, 56);
                    try self.emitAsrImm(.w64, reg, reg, 56);
                    try self.emitStoreToMem(saved_ptr_reg, reg);
                },
                .f64 => {
                    switch (loc) {
                        .float_reg => |reg| {
                            try self.emitStoreFloatToMem(saved_ptr_reg, reg);
                        },
                        .immediate_f64 => |val| {
                            const bits: i64 = @bitCast(val);
                            const reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(reg, bits);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                            self.codegen.freeGeneral(reg);
                        },
                        else => {
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                        },
                    }
                },
                .f32 => {
                    // F32: Convert from F64 and store 4 bytes.
                    // Note: `stabilize` spills float regs to the stack as 8-byte F64,
                    // so .stack locations hold F64-encoded values that need conversion.
                    switch (loc) {
                        .float_reg => |reg| {
                            // Convert F64 to F32, then store 4 bytes
                            if (comptime target.toCpuArch() == .aarch64) {
                                try self.codegen.emit.fcvtFloatFloat(.single, reg, .double, reg);
                                try self.codegen.emit.fstrRegMemUoff(.single, reg, saved_ptr_reg, 0);
                            } else {
                                try self.codegen.emit.cvtsd2ssRegReg(reg, reg);
                                try self.codegen.emit.movssMemReg(saved_ptr_reg, 0, reg);
                            }
                        },
                        .immediate_f64 => |val| {
                            // Convert to f32 bits and store 4 bytes
                            const f32_val: f32 = @floatCast(val);
                            const bits: u32 = @bitCast(f32_val);
                            const reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(reg, @as(i64, bits));
                            try self.emitStoreToPtr(.w32, reg, saved_ptr_reg, 0);
                            self.codegen.freeGeneral(reg);
                        },
                        .stack => |s| {
                            if (s.size == .dword) {
                                const reg = try self.allocTempGeneral();
                                try self.codegen.emitLoadStack(.w32, reg, s.offset);
                                try self.emitStoreToPtr(.w32, reg, saved_ptr_reg, 0);
                                self.codegen.freeGeneral(reg);
                            } else {
                                const offset = s.offset;
                                // Value was spilled to stack as F64 by stabilize.
                                // Load as F64, convert to F32, then store 4 bytes.
                                const freg = self.codegen.allocFloat() orelse unreachable;
                                try self.codegen.emitLoadStackF64(freg, offset);
                                if (comptime target.toCpuArch() == .aarch64) {
                                    try self.codegen.emit.fcvtFloatFloat(.single, freg, .double, freg);
                                    try self.codegen.emit.fstrRegMemUoff(.single, freg, saved_ptr_reg, 0);
                                } else {
                                    try self.codegen.emit.cvtsd2ssRegReg(freg, freg);
                                    try self.codegen.emit.movssMemReg(saved_ptr_reg, 0, freg);
                                }
                                self.codegen.freeFloat(freg);
                            }
                        },
                        else => {
                            // Store 4 bytes from general register
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToPtr(.w32, reg, saved_ptr_reg, 0);
                        },
                    }
                },
                .i128, .u128, .dec => {
                    try self.storeI128ToMem(saved_ptr_reg, loc);
                },
                .str => {
                    // Strings are 24 bytes (ptr, len, capacity) - same as lists
                    switch (loc) {
                        .stack_str => |stack_offset| {
                            // Copy 24-byte RocStr struct from stack to result buffer
                            const temp_reg = try self.allocTempGeneral();

                            // Copy all 24 bytes (3 x 8-byte words)
                            try self.emitLoad(.w64, temp_reg, frame_ptr, stack_offset);
                            try self.emitStore(.w64, saved_ptr_reg, 0, temp_reg);
                            try self.emitLoad(.w64, temp_reg, frame_ptr, stack_offset + 8);
                            try self.emitStore(.w64, saved_ptr_reg, 8, temp_reg);
                            try self.emitLoad(.w64, temp_reg, frame_ptr, stack_offset + 16);
                            try self.emitStore(.w64, saved_ptr_reg, 16, temp_reg);

                            self.codegen.freeGeneral(temp_reg);
                        },
                        .stack => |s| {
                            const stack_offset = s.offset;
                            // Copy 24-byte RocStr struct from stack to result buffer
                            const temp_reg = try self.allocTempGeneral();

                            // Copy all 24 bytes (3 x 8-byte words)
                            try self.emitLoad(.w64, temp_reg, frame_ptr, stack_offset);
                            try self.emitStore(.w64, saved_ptr_reg, 0, temp_reg);
                            try self.emitLoad(.w64, temp_reg, frame_ptr, stack_offset + 8);
                            try self.emitStore(.w64, saved_ptr_reg, 8, temp_reg);
                            try self.emitLoad(.w64, temp_reg, frame_ptr, stack_offset + 16);
                            try self.emitStore(.w64, saved_ptr_reg, 16, temp_reg);

                            self.codegen.freeGeneral(temp_reg);
                        },
                        else => {
                            // Fallback for non-stack string location
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                        },
                    }
                },
                else => {
                    // Check if this is a composite type (record/tuple/list) via layout store
                    const ls = self.layout_store;
                    const layout_val = ls.getLayout(result_layout);
                    switch (layout_val.tag) {
                        .struct_ => {
                            const struct_data = ls.getStructData(layout_val.data.struct_.idx);
                            try self.copyStackToPtr(loc, saved_ptr_reg, struct_data.size);
                            return;
                        },
                        .tag_union => {
                            const tu_data = ls.getTagUnionData(layout_val.data.tag_union.idx);
                            try self.copyStackToPtr(loc, saved_ptr_reg, tu_data.size);
                            return;
                        },
                        .list, .list_of_zst => {
                            // Lists are roc_list_size-byte structs (ptr, len, capacity)
                            try self.copyStackToPtr(loc, saved_ptr_reg, roc_list_size);
                            return;
                        },
                        .scalar => {
                            const sa = ls.layoutSizeAlign(layout_val);
                            if (sa.size == roc_str_size) {
                                // Str: roc_str_size-byte struct (ptr, len, capacity)
                                try self.copyStackToPtr(loc, saved_ptr_reg, roc_str_size);
                            } else if (sa.size == 16) {
                                // i128/u128/Dec
                                try self.storeI128ToMem(saved_ptr_reg, loc);
                            } else if (sa.size > 0) {
                                // Small scalars (1-8 bytes)
                                const reg = try self.ensureInGeneralReg(loc);
                                try self.emitStoreToMem(saved_ptr_reg, reg);
                            }
                            return;
                        },
                        .zst => {
                            // Zero-sized type — nothing to store.
                            return;
                        },
                        .closure => {
                            const sa = ls.layoutSizeAlign(layout_val);
                            try self.copyStackToPtr(loc, saved_ptr_reg, sa.size);
                            return;
                        },
                        .box => {
                            // Box is a heap pointer (machine word)
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                            return;
                        },
                        .box_of_zst => {
                            // Box of zero-sized type — nothing to store.
                            return;
                        },
                    }
                },
            }
        }

        /// Copy bytes from stack location to memory pointed to by ptr_reg
        fn copyStackToPtr(self: *Self, loc: ValueLocation, ptr_reg: GeneralReg, size: u32) Allocator.Error!void {
            switch (loc) {
                .stack => |s| {
                    const stack_offset = s.offset;
                    // Copy size bytes from stack to destination
                    const temp_reg = try self.allocTempGeneral();
                    var remaining = size;
                    var src_offset: i32 = stack_offset;
                    var dst_offset: i32 = 0;

                    // Copy 8 bytes at a time
                    while (remaining >= 8) {
                        try self.codegen.emitLoadStack(.w64, temp_reg, src_offset);
                        try self.emitStoreToPtr(.w64, temp_reg, ptr_reg, dst_offset);
                        src_offset += 8;
                        dst_offset += 8;
                        remaining -= 8;
                    }

                    // Handle remaining bytes (4, 2, 1)
                    if (remaining >= 4) {
                        try self.codegen.emitLoadStack(.w32, temp_reg, src_offset);
                        try self.emitStoreToPtr(.w32, temp_reg, ptr_reg, dst_offset);
                        src_offset += 4;
                        dst_offset += 4;
                        remaining -= 4;
                    }
                    if (remaining >= 2) {
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emitLoadStackHalfword(temp_reg, src_offset);
                            try self.codegen.emit.strhRegMem(temp_reg, ptr_reg, @intCast(@as(u32, @intCast(dst_offset)) >> 1));
                        } else {
                            try self.codegen.emitLoadStack(.w16, temp_reg, src_offset);
                            try self.codegen.emit.movMemReg(.w16, ptr_reg, dst_offset, temp_reg);
                        }
                        src_offset += 2;
                        dst_offset += 2;
                        remaining -= 2;
                    }
                    if (remaining >= 1) {
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emitLoadStackByte(temp_reg, src_offset);
                            try self.codegen.emit.strbRegMem(temp_reg, ptr_reg, @intCast(dst_offset));
                        } else {
                            try self.codegen.emitLoadStack(.w8, temp_reg, src_offset);
                            try self.codegen.emit.movMemReg(.w8, ptr_reg, dst_offset, temp_reg);
                        }
                    }

                    self.codegen.freeGeneral(temp_reg);
                },
                .list_stack => |list_info| {
                    // Copy 24 bytes from list struct on stack to destination
                    const temp_reg = try self.allocTempGeneral();
                    var remaining = size;
                    var src_offset: i32 = list_info.struct_offset;
                    var dst_offset: i32 = 0;

                    // Copy 8 bytes at a time
                    while (remaining >= 8) {
                        try self.codegen.emitLoadStack(.w64, temp_reg, src_offset);
                        try self.emitStoreToPtr(.w64, temp_reg, ptr_reg, dst_offset);
                        src_offset += 8;
                        dst_offset += 8;
                        remaining -= 8;
                    }

                    self.codegen.freeGeneral(temp_reg);
                },
                else => {
                    // Materialize non-stack values to a stack slot first so we preserve the
                    // requested byte width for narrow results like Bool and small tag unions.
                    const temp_slot = self.codegen.allocStackSlot(size);
                    try self.copyBytesToStackOffset(temp_slot, loc, size);
                    try self.copyStackToPtr(.{ .stack = .{ .offset = temp_slot } }, ptr_reg, size);
                },
            }
        }

        /// Store 128-bit value to memory at [ptr_reg]
        fn storeI128ToMem(self: *Self, ptr_reg: GeneralReg, loc: ValueLocation) Allocator.Error!void {
            switch (loc) {
                .immediate_i128 => |val| {
                    // Store low 64 bits, then high 64 bits
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);

                    const reg = try self.allocTempGeneral();

                    // Store low 64 bits at [ptr]
                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    try self.emitStoreToPtr(.w64, reg, ptr_reg, 0);

                    // Store high 64 bits at [ptr + 8]
                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    try self.emitStoreToPtr(.w64, reg, ptr_reg, 8);

                    self.codegen.freeGeneral(reg);
                },
                .stack_i128, .stack_str => |offset| {
                    // Copy 16 bytes from stack to destination
                    const reg = try self.allocTempGeneral();

                    // Load low 64 bits from stack, store to dest
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    try self.emitStoreToPtr(.w64, reg, ptr_reg, 0);

                    // Load high 64 bits from stack, store to dest
                    try self.codegen.emitLoadStack(.w64, reg, offset + 8);
                    try self.emitStoreToPtr(.w64, reg, ptr_reg, 8);

                    self.codegen.freeGeneral(reg);
                },
                .stack => |s| {
                    const offset = s.offset;
                    // Copy 16 bytes from stack to destination
                    const reg = try self.allocTempGeneral();

                    // Load low 64 bits from stack, store to dest
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    try self.emitStoreToPtr(.w64, reg, ptr_reg, 0);

                    // Load high 64 bits from stack, store to dest
                    try self.codegen.emitLoadStack(.w64, reg, offset + 8);
                    try self.emitStoreToPtr(.w64, reg, ptr_reg, 8);

                    self.codegen.freeGeneral(reg);
                },
                .immediate_i64 => |val| {
                    // Sign-extend i64 to i128 and store
                    const val_i128: i128 = val;
                    const low: u64 = @truncate(@as(u128, @bitCast(val_i128)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val_i128)) >> 64);

                    const reg = try self.allocTempGeneral();

                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    try self.emitStoreToPtr(.w64, reg, ptr_reg, 0);

                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    try self.emitStoreToPtr(.w64, reg, ptr_reg, 8);

                    self.codegen.freeGeneral(reg);
                },
                .general_reg => |reg| {
                    // Only have low 64 bits in register — sign-extend to i128
                    // by arithmetic-shifting right by 63 to fill high word.
                    const sign_reg = try self.allocTempGeneral();
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 0);
                        try self.codegen.emit.asrRegRegImm(.w64, sign_reg, reg, 63);
                        try self.codegen.emit.strRegMemUoff(.w64, sign_reg, ptr_reg, 1);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, reg);
                        try self.emitMovRegReg(sign_reg, reg);
                        try self.codegen.emit.sarRegImm8(.w64, sign_reg, 63);
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 8, sign_reg);
                    }
                    self.codegen.freeGeneral(sign_reg);
                },
                else => {
                    unreachable;
                },
            }
        }

        /// Store general register to memory at [ptr_reg] (architecture-specific)
        fn emitStoreToMem(self: *Self, ptr_reg: anytype, src_reg: GeneralReg) !void {
            try self.emitStoreToPtr(.w64, src_reg, ptr_reg, 0);
        }

        /// Store float register to memory at [ptr_reg] (architecture-specific)
        fn emitStoreFloatToMem(self: *Self, ptr_reg: anytype, src_reg: FloatReg) !void {
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.fstrRegMemUoff(.double, src_reg, ptr_reg, 0);
            } else {
                try self.codegen.emit.movsdMemReg(ptr_reg, 0, src_reg);
            }
        }

        /// Compile all procedures first, before generating any calls.
        /// This ensures all call targets are known before we need to patch calls.
        pub fn compileAllProcSpecs(self: *Self, proc_specs: []const LirProcSpec) Allocator.Error!void {
            for (proc_specs, 0..) |proc, i| {
                const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(i);
                try self.proc_registry.put(@intFromEnum(proc_id), .{
                    .id = proc_id,
                    .code_start = unresolved_proc_code_start,
                    .code_end = 0,
                    .name = proc.name,
                    .arg_layouts = proc.arg_layouts,
                });
            }

            for (proc_specs, 0..) |proc, i| {
                try self.compileProcSpec(@enumFromInt(i), proc);
            }

            try self.patchPendingCalls();
        }

        /// Compile a single procedure as a complete unit.
        /// Uses deferred prologue pattern: generates body first to determine which
        /// callee-saved registers are used, then prepends prologue and adjusts relocations.
        fn compileProcSpec(self: *Self, proc_id: lir.LIR.LirProcSpecId, proc: LirProcSpec) Allocator.Error!void {
            const key: u32 = @intFromEnum(proc_id);
            // Save current state - procedure has its own scope that shouldn't pollute caller
            const saved_stack_offset = self.codegen.stack_offset;
            const saved_callee_saved_used = self.codegen.callee_saved_used;
            const saved_callee_saved_available = self.codegen.callee_saved_available;
            const saved_free_general = self.codegen.free_general;
            const saved_general_owners = self.codegen.general_owners;
            const saved_free_float = self.codegen.free_float;
            const saved_float_owners = self.codegen.float_owners;
            const saved_roc_ops_reg = self.roc_ops_reg;
            const saved_ret_ptr_slot = self.ret_ptr_slot;
            const saved_binding_symbol = self.current_binding_symbol;
            const saved_current_proc_name = self.current_proc_name;
            var saved_symbol_locations = self.symbol_locations.clone() catch return error.OutOfMemory;
            defer saved_symbol_locations.deinit();
            var saved_mutable_var_slots = self.mutable_var_slots.clone() catch return error.OutOfMemory;
            defer saved_mutable_var_slots.deinit();
            var saved_join_points = self.join_points.clone() catch return error.OutOfMemory;
            defer saved_join_points.deinit();
            var saved_join_point_jumps = try self.cloneJoinPointJumpsMap(&self.join_point_jumps);
            defer self.deinitJoinPointJumpsMap(&saved_join_point_jumps);
            var saved_join_point_param_layouts = self.join_point_param_layouts.clone() catch return error.OutOfMemory;
            defer saved_join_point_param_layouts.deinit();
            var saved_join_point_param_patterns = self.join_point_param_patterns.clone() catch return error.OutOfMemory;
            defer saved_join_point_param_patterns.deinit();
            var saved_loop_break_patches = try self.loop_break_patches.clone(self.allocator);
            defer saved_loop_break_patches.deinit(self.allocator);

            // Clear state for procedure's scope
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();
            self.clearFunctionControlFlowState();
            self.codegen.callee_saved_used = 0;
            self.codegen.callee_saved_available = CodeGen.CALLEE_SAVED_GENERAL_MASK;
            self.codegen.free_general = CodeGen.INITIAL_FREE_GENERAL;
            self.codegen.general_owners = [_]?u32{null} ** CodeGen.NUM_GENERAL_REGS;
            self.codegen.free_float = CodeGen.INITIAL_FREE_FLOAT;
            self.codegen.float_owners = [_]?u32{null} ** CodeGen.NUM_FLOAT_REGS;
            self.roc_ops_reg = null;
            self.current_binding_symbol = null;
            self.current_proc_name = proc.name;

            // Reserve R12/X20 for roc_ops exactly like standalone lambda compilation.
            if (comptime target.toCpuArch() == .x86_64) {
                const r12_bit = @as(u16, 1) << @intFromEnum(x86_64.GeneralReg.R12);
                self.codegen.callee_saved_used |= r12_bit;
                self.codegen.callee_saved_available &= ~(@as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12));
            } else {
                const x20_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20);
                self.codegen.callee_saved_used |= x20_bit;
                self.codegen.callee_saved_available &= ~(@as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20));
            }

            // PHASE 1: Generate body first (to determine callee_saved_used)
            // Initialize stack_offset to reserve space for callee-saved area
            if (comptime target.toCpuArch() == .x86_64) {
                // Reserve 40 bytes for 5 callee-saved registers at fixed offsets
                self.codegen.stack_offset = -CodeGen.CALLEE_SAVED_AREA_SIZE;
            } else {
                // aarch64: FP-relative addressing
                // Reserve space for: FP/LR (16 bytes) + callee-saved area (80 bytes)
                // First slot at FP + 16 + 80 = FP + 96
                self.codegen.stack_offset = 16 + CodeGen.CALLEE_SAVED_AREA_SIZE;
            }

            const body_start = self.codegen.currentOffset();
            const relocs_before = self.codegen.relocations.items.len;

            // Keep this proc unresolved while generating its body. Recursive self-calls
            // then use the same pending-call path as any other forward proc call, and
            // get patched once the final prologue start is known.
            if (self.proc_registry.getPtr(key)) |entry| {
                entry.* = .{
                    .id = proc_id,
                    .code_start = unresolved_proc_code_start,
                    .code_end = 0,
                    .name = proc.name,
                    .arg_layouts = proc.arg_layouts,
                };
            } else {
                try self.proc_registry.put(key, .{
                    .id = proc_id,
                    .code_start = unresolved_proc_code_start,
                    .code_end = 0,
                    .name = proc.name,
                    .arg_layouts = proc.arg_layouts,
                });
            }

            // Set up recursive context
            const old_recursive_symbol = self.current_recursive_symbol;
            const old_recursive_join_point = self.current_recursive_join_point;

            switch (proc.is_self_recursive) {
                .self_recursive => |join_point_id| {
                    self.current_recursive_symbol = proc.name;
                    self.current_recursive_join_point = join_point_id;
                },
                .not_self_recursive => {},
            }

            // Save early return state (return_stmt uses jump-to-epilogue mechanism)
            const saved_early_return_ret_layout = self.early_return_ret_layout;
            const saved_early_return_patches_len = self.early_return_patches.items.len;
            self.early_return_ret_layout = proc.ret_layout;

            errdefer {
                _ = self.proc_registry.remove(key);
                self.codegen.stack_offset = saved_stack_offset;
                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.codegen.free_general = saved_free_general;
                self.codegen.general_owners = saved_general_owners;
                self.codegen.free_float = saved_free_float;
                self.codegen.float_owners = saved_float_owners;
                self.roc_ops_reg = saved_roc_ops_reg;
                self.current_binding_symbol = saved_binding_symbol;
                self.current_proc_name = saved_current_proc_name;
                self.symbol_locations.deinit();
                self.symbol_locations = saved_symbol_locations.clone() catch unreachable;
                self.mutable_var_slots.deinit();
                self.mutable_var_slots = saved_mutable_var_slots.clone() catch unreachable;
                self.join_points.deinit();
                self.join_points = saved_join_points.clone() catch unreachable;
                self.deinitJoinPointJumpsMap(&self.join_point_jumps);
                self.join_point_jumps = self.cloneJoinPointJumpsMap(&saved_join_point_jumps) catch unreachable;
                self.join_point_param_layouts.deinit();
                self.join_point_param_layouts = saved_join_point_param_layouts.clone() catch unreachable;
                self.join_point_param_patterns.deinit();
                self.join_point_param_patterns = saved_join_point_param_patterns.clone() catch unreachable;
                self.loop_break_patches.deinit(self.allocator);
                self.loop_break_patches = saved_loop_break_patches.clone(self.allocator) catch unreachable;
                self.early_return_ret_layout = saved_early_return_ret_layout;
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
            }

            const needs_ret_ptr = self.needsInternalReturnByPointer(proc.ret_layout);
            if (needs_ret_ptr) {
                self.ret_ptr_slot = self.codegen.allocStackSlot(8);
                const first_reg = self.getArgumentRegister(0);
                try self.codegen.emitStoreStack(.w64, self.ret_ptr_slot.?, first_reg);
            } else {
                self.ret_ptr_slot = null;
            }

            // Bind parameters to argument registers. Large returns use a hidden
            // first argument register for the return buffer.
            const initial_param_reg_idx: u8 = if (needs_ret_ptr) 1 else 0;
            try self.bindLambdaParams(proc.args, initial_param_reg_idx, proc.force_pass_by_ptr);

            // Generate the body (control flow statements)
            // Note: .return_stmt emits jumps that are patched to the shared epilogue below
            try self.generateStmt(proc.body);

            // Restore recursive context
            self.current_recursive_symbol = old_recursive_symbol;
            self.current_recursive_join_point = old_recursive_join_point;

            // Emit shared epilogue using DeferredFrameBuilder with actual stack usage
            const body_epilogue_offset = self.codegen.currentOffset();
            {
                const actual_locals: u32 = if (comptime target.toCpuArch() == .aarch64)
                    @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE)
                else
                    @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                var builder = CodeGen.DeferredFrameBuilder.init();
                builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                builder.setStackSize(actual_locals);
                try builder.emitEpilogue(&self.codegen.emit);
            }

            const body_end = self.codegen.currentOffset();

            // PHASE 2: Extract body and prepend prologue (x86_64 only - uses deferred pattern)
            if (comptime target.toCpuArch() == .x86_64) {
                // Save body bytes
                const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                // Truncate buffer back to body_start
                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                // Emit prologue using DeferredFrameBuilder (now knows callee_saved_used).
                // Pass only the actual locals size — the builder adds callee-saved space internally.
                const prologue_start = self.codegen.currentOffset();
                const actual_locals_x86: u32 = @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                try self.codegen.emitPrologueWithAlloc(actual_locals_x86);
                const prologue_size = self.codegen.currentOffset() - prologue_start;

                // Re-append body
                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                // PHASE 3: Adjust relocation offsets
                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size);
                }

                // Nested lambdas compiled while generating this proc body move along with it.
                // Keep the lambda caches in final coordinates so later call sites resolve correctly.
                self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size, std.math.maxInt(u64));
                self.shiftPendingCalls(body_start, body_end, prologue_size);

                // Re-patch internal calls/addr whose targets are outside the shifted body
                self.repatchInternalCalls(body_start, body_end, prologue_size, body_start);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size, body_start);

                // Patch return-stmt jumps to the shared epilogue
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size;
                }
                const final_epilogue = body_epilogue_offset - body_start + prologue_size + prologue_start;
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
                self.early_return_ret_layout = saved_early_return_ret_layout;

                // Update procedure registry with correct code_start (prologue_start)
                if (self.proc_registry.getPtr(key)) |entry| {
                    entry.code_start = prologue_start;
                    entry.code_end = self.codegen.currentOffset();
                }
            } else {
                // aarch64: Prepend prologue to generated body
                // Since body was generated without prologue, we need to prepend it.
                const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                // Truncate buffer back to body_start
                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                // Emit aarch64 prologue using DeferredFrameBuilder with actual stack usage
                const prologue_start = self.codegen.currentOffset();
                const actual_locals: u32 = @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE);
                var frame_builder = CodeGen.DeferredFrameBuilder.init();
                frame_builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                frame_builder.setStackSize(actual_locals);
                _ = try frame_builder.emitPrologue(&self.codegen.emit);
                const prologue_size = self.codegen.currentOffset() - prologue_start;

                // Re-append body
                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                // Adjust relocation offsets
                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size);
                }

                // Nested lambdas compiled while generating this proc body move along with it.
                // Keep the lambda caches in final coordinates so later call sites resolve correctly.
                self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size, std.math.maxInt(u64));
                self.shiftPendingCalls(body_start, body_end, prologue_size);

                // Re-patch internal calls/addr whose targets are outside the shifted body
                self.repatchInternalCalls(body_start, body_end, prologue_size, body_start);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size, body_start);

                // Patch return-stmt jumps to the shared epilogue
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size;
                }
                const final_epilogue = body_epilogue_offset - body_start + prologue_size + prologue_start;
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
                self.early_return_ret_layout = saved_early_return_ret_layout;

                // Update procedure registry
                if (self.proc_registry.getPtr(key)) |entry| {
                    entry.code_start = prologue_start;
                    entry.code_end = self.codegen.currentOffset();
                }
            }

            // Restore state
            self.codegen.stack_offset = saved_stack_offset;
            self.codegen.callee_saved_used = saved_callee_saved_used;
            self.codegen.callee_saved_available = saved_callee_saved_available;
            self.codegen.free_general = saved_free_general;
            self.codegen.general_owners = saved_general_owners;
            self.codegen.free_float = saved_free_float;
            self.codegen.float_owners = saved_float_owners;
            self.roc_ops_reg = saved_roc_ops_reg;
            self.ret_ptr_slot = saved_ret_ptr_slot;
            self.current_binding_symbol = saved_binding_symbol;
            self.current_proc_name = saved_current_proc_name;
            self.symbol_locations.deinit();
            self.symbol_locations = saved_symbol_locations.clone() catch return error.OutOfMemory;
            self.mutable_var_slots.deinit();
            self.mutable_var_slots = saved_mutable_var_slots.clone() catch return error.OutOfMemory;
            self.join_points.deinit();
            self.join_points = saved_join_points.clone() catch return error.OutOfMemory;
            self.deinitJoinPointJumpsMap(&self.join_point_jumps);
            self.join_point_jumps = try self.cloneJoinPointJumpsMap(&saved_join_point_jumps);
            self.join_point_param_layouts.deinit();
            self.join_point_param_layouts = saved_join_point_param_layouts.clone() catch return error.OutOfMemory;
            self.join_point_param_patterns.deinit();
            self.join_point_param_patterns = saved_join_point_param_patterns.clone() catch return error.OutOfMemory;
            self.loop_break_patches.deinit(self.allocator);
            self.loop_break_patches = try saved_loop_break_patches.clone(self.allocator);
        }

        /// Maximum number of registers used for multi-register returns in internal Roc calls.
        /// x86_64: RAX, RDX, RCX, R8, R9, R10, R11, RDI, RSI = 9 registers = 72 bytes
        /// aarch64: X0-X7, XR, X9-X15 = 16 registers = 128 bytes
        const max_return_regs: u32 = if (target.toCpuArch() == .aarch64) 16 else 9;
        const max_return_size: u32 = max_return_regs * 8;

        /// Check if a return type exceeds the register limit and needs return-by-pointer.
        /// When true, the caller passes a hidden first argument (pointer to a pre-allocated
        /// buffer) and the callee writes the result there instead of using return registers.
        fn needsInternalReturnByPointer(self: *Self, ret_layout: layout.Idx) bool {
            const ls = self.layout_store;
            if (@intFromEnum(ret_layout) < ls.layouts.len()) {
                const layout_val = ls.getLayout(ret_layout);
                if (layout_val.tag == .struct_ or layout_val.tag == .tag_union or layout_val.tag == .closure) {
                    return ls.layoutSizeAlign(layout_val).size > max_return_size;
                }
            }
            return false;
        }

        /// Save the return value from a call into a stack-based ValueLocation.
        /// Save the return value from a compiled proc call into a stack-based ValueLocation.
        /// Handles i128/str/list/multi-reg struct/scalar returns.
        fn saveCallReturnValue(self: *Self, ret_layout: layout.Idx, needs_ret_ptr: bool, ret_buffer_offset: i32) Allocator.Error!ValueLocation {
            // If we used return-by-pointer, the callee has written the result
            // to our pre-allocated buffer. No register saving needed.
            if (needs_ret_ptr) {
                return .{ .stack = .{ .offset = ret_buffer_offset } };
            }

            // Float returns come back in the float return register (V0/XMM0),
            // not in the integer return register (X0/RAX).
            if (ret_layout == .f32) {
                const stack_offset = self.codegen.allocStackSlot(4);
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.fcvtFloatFloat(.single, .V0, .double, .V0);
                    const bits_reg = try self.allocTempGeneral();
                    try self.codegen.emit.fmovGenFromFloat(.single, bits_reg, .V0);
                    try self.codegen.emitStoreStack(.w32, stack_offset, bits_reg);
                    self.codegen.freeGeneral(bits_reg);
                } else {
                    try self.codegen.emit.cvtsd2ssRegReg(.XMM0, .XMM0);
                    try self.codegen.emit.movssMemReg(.RBP, stack_offset, .XMM0);
                }
                return .{ .stack = .{ .offset = stack_offset, .size = .dword } };
            }

            if (ret_layout == .f64) {
                const stack_offset = self.codegen.allocStackSlot(8);
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emitStoreStackF64(stack_offset, .V0);
                } else {
                    try self.codegen.emitStoreStackF64(stack_offset, .XMM0);
                }
                return .{ .stack = .{ .offset = stack_offset } };
            }

            // Handle i128/Dec return values (returned in two registers)
            if (ret_layout == .i128 or ret_layout == .u128 or ret_layout == .dec) {
                const stack_offset = self.codegen.allocStackSlot(16);
                try self.codegen.emitStoreStack(.w64, stack_offset, ret_reg_0);
                try self.codegen.emitStoreStack(.w64, stack_offset + 8, ret_reg_1);
                return .{ .stack_i128 = stack_offset };
            }

            // Check if return type is a string (24 bytes)
            if (ret_layout == .str) {
                const stack_offset = self.codegen.allocStackSlot(roc_str_size);
                try self.emitStore(.w64, frame_ptr, stack_offset, ret_reg_0);
                try self.emitStore(.w64, frame_ptr, stack_offset + 8, ret_reg_1);
                try self.emitStore(.w64, frame_ptr, stack_offset + 16, ret_reg_2);
                return .{ .stack_str = stack_offset };
            }

            // Check if return type is a list (24 bytes)
            const is_list_return = blk: {
                const ls = self.layout_store;
                const layout_val = ls.getLayout(ret_layout);
                break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
            };

            if (is_list_return) {
                const stack_offset = self.codegen.allocStackSlot(roc_str_size);
                try self.emitStore(.w64, frame_ptr, stack_offset, ret_reg_0);
                try self.emitStore(.w64, frame_ptr, stack_offset + 8, ret_reg_1);
                try self.emitStore(.w64, frame_ptr, stack_offset + 16, ret_reg_2);
                return .{ .list_stack = .{
                    .struct_offset = stack_offset,
                    .data_offset = 0,
                    .num_elements = 0,
                } };
            }

            // Check if return type is a multi-register value (record, tag_union, closure, tuple > 8 bytes)
            {
                const ls = self.layout_store;
                const layout_val = ls.getLayout(ret_layout);
                if (layout_val.tag == .struct_ or layout_val.tag == .tag_union or layout_val.tag == .closure) {
                    const size_align = ls.layoutSizeAlign(layout_val);
                    if (size_align.size > 8) {
                        const stack_offset = self.codegen.allocStackSlot(size_align.size);
                        const num_regs = (size_align.size + 7) / 8;
                        if (comptime target.toCpuArch() == .aarch64) {
                            const regs = [_]@TypeOf(GeneralReg.X0){ .X0, .X1, .X2, .X3, .X4, .X5, .X6, .X7, .XR, .X9, .X10, .X11, .X12, .X13, .X14, .X15 };
                            for (0..@min(num_regs, regs.len)) |i| {
                                try self.codegen.emit.strRegMemSoff(.w64, regs[i], .FP, stack_offset + @as(i32, @intCast(i * 8)));
                            }
                        } else {
                            const regs = [_]@TypeOf(GeneralReg.RAX){ .RAX, .RDX, .RCX, .R8, .R9, .R10, .R11, .RDI, .RSI };
                            for (0..@min(num_regs, regs.len)) |i| {
                                try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + @as(i32, @intCast(i * 8)), regs[i]);
                            }
                        }
                        return .{ .stack = .{ .offset = stack_offset } };
                    }
                }
            }

            // Spill scalar return value from the return register (X0/RAX) to the stack.
            const ret_reg = self.getReturnRegister();
            const stack_offset = self.codegen.allocStackSlot(8);
            try self.codegen.emitStoreStack(.w64, stack_offset, ret_reg);
            return .{ .stack = .{ .offset = stack_offset } };
        }

        /// Configuration for placeCallArguments.
        const CallConfig = struct {
            /// If true, arg 0 is a hidden pointer to ret_buffer_offset.
            needs_ret_ptr: bool = false,
            ret_buffer_offset: i32 = 0,
            /// Pre-computed per-argument pass-by-pointer flags (null = no pass-by-ptr).
            pass_by_ptr: ?[]const bool = null,
            /// If true, append roc_ops as the final argument.
            emit_roc_ops: bool = false,
        };

        /// Place arguments in registers and/or stack slots per the calling convention.
        /// Handles i128 even-alignment on aarch64, 3-reg list/str, multi-reg structs,
        /// lambda_code addressing, pass-by-pointer conversion, and stack spilling.
        /// Returns the stack_spill_size allocated (caller must clean up after the call).
        fn placeCallArguments(self: *Self, arg_infos: []const ArgInfo, config: CallConfig) Allocator.Error!i32 {
            // Compute stack_spill_size.
            // When pass_by_ptr is provided, multi-reg args that would overflow are
            // already converted to pointers — account for that.
            var stack_spill_size: i32 = 0;
            {
                var reg_count: u8 = if (config.needs_ret_ptr) 1 else 0;
                for (arg_infos, 0..) |ai, i| {
                    const pbp = if (config.pass_by_ptr) |p| p[i] else false;
                    const is_i128_arg = self.argNeedsI128Abi(ai.loc, ai.layout_idx);
                    if (comptime target.toCpuArch() == .aarch64) {
                        if (!pbp and is_i128_arg and reg_count < max_arg_regs and reg_count % 2 != 0) {
                            reg_count += 1;
                        }
                    }
                    const nr: u8 = if (pbp) 1 else ai.num_regs;
                    if (reg_count + nr <= max_arg_regs) {
                        reg_count += nr;
                    } else {
                        stack_spill_size += @as(i32, nr) * 8;
                        reg_count = max_arg_regs;
                    }
                }
                // Account for roc_ops needing a slot
                if (config.emit_roc_ops) {
                    if (reg_count + 1 > max_arg_regs) {
                        stack_spill_size += 8;
                    }
                }
            }

            // Keep the outgoing spill area ABI-aligned. Padding sits above the
            // final spilled arg so stack arg 0 remains at spill offset 0.
            if (stack_spill_size > 0) {
                stack_spill_size = @intCast(std.mem.alignForward(
                    u32,
                    @intCast(stack_spill_size),
                    call_stack_alignment,
                ));
            }

            // Allocate stack space for spilled arguments
            if (stack_spill_size > 0) {
                try self.emitSubImm(.w64, stack_ptr, stack_ptr, stack_spill_size);
            }

            // Place arguments in registers or on stack
            var reg_idx: u8 = 0;
            var stack_arg_offset: i32 = 0;

            // If return-by-pointer, load the hidden return buffer pointer as arg 0
            if (config.needs_ret_ptr) {
                const arg_reg = self.getArgumentRegister(0);
                try self.emitLeaStack(arg_reg, config.ret_buffer_offset);
                reg_idx = 1;
            }

            for (arg_infos, 0..) |info, i| {
                if (info.num_regs == 0) {
                    continue;
                }
                const arg_loc = info.loc;
                const arg_layout = info.layout_idx;

                // Check if this argument is passed by pointer
                if (config.pass_by_ptr) |pbp| {
                    if (pbp[i]) {
                        const arg_size: u32 = @as(u32, info.num_regs) * 8;
                        const arg_offset = try self.ensureOnStack(arg_loc, arg_size);
                        if (reg_idx < max_arg_regs) {
                            const arg_reg = self.getArgumentRegister(reg_idx);
                            try self.emitLeaStack(arg_reg, arg_offset);
                            reg_idx += 1;
                        } else {
                            const temp = try self.allocTempGeneral();
                            try self.emitLeaStack(temp, arg_offset);
                            try self.spillArgToStack(.{ .general_reg = temp }, null, stack_arg_offset, 1);
                            self.codegen.freeGeneral(temp);
                            stack_arg_offset += 8;
                            reg_idx = max_arg_regs;
                        }
                        continue;
                    }
                }

                // Check if this argument fits in registers
                if (reg_idx + info.num_regs <= max_arg_regs) {
                    // Handle i128/Dec arguments (need two registers, even-aligned on aarch64)
                    const is_i128_arg = self.argNeedsI128Abi(arg_loc, arg_layout);
                    if (is_i128_arg) {
                        if (comptime target.toCpuArch() == .aarch64) {
                            if (reg_idx % 2 != 0) {
                                reg_idx += 1;
                            }
                        }
                        const low_reg = self.getArgumentRegister(reg_idx);
                        const high_reg = self.getArgumentRegister(reg_idx + 1);
                        switch (arg_loc) {
                            .stack_i128 => |offset| {
                                try self.codegen.emitLoadStack(.w64, low_reg, offset);
                                try self.codegen.emitLoadStack(.w64, high_reg, offset + 8);
                            },
                            .stack => |s| {
                                try self.codegen.emitLoadStack(.w64, low_reg, s.offset);
                                try self.codegen.emitLoadStack(.w64, high_reg, s.offset + 8);
                            },
                            .immediate_i128 => |val| {
                                const low: u64 = @truncate(@as(u128, @bitCast(val)));
                                const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                                try self.codegen.emitLoadImm(low_reg, @bitCast(low));
                                try self.codegen.emitLoadImm(high_reg, @bitCast(high));
                            },
                            else => if (builtin.mode == .Debug) {
                                std.debug.panic(
                                    "placeCallArguments i128 ABI mismatch: arg_loc={s} arg_layout={?} proc={d}",
                                    .{
                                        @tagName(arg_loc),
                                        arg_layout,
                                        if (self.current_proc_name) |sym| sym.raw() else std.math.maxInt(u64),
                                    },
                                );
                            } else unreachable,
                        }
                        reg_idx += 2;
                        continue;
                    }

                    if (info.num_regs == 3) {
                        // List or string (24 bytes)
                        const offset: i32 = switch (arg_loc) {
                            .stack => |s| s.offset,
                            .list_stack => |li| li.struct_offset,
                            .stack_str => |off| off,
                            else => if (builtin.mode == .Debug) {
                                std.debug.panic(
                                    "placeCallArguments expected 3-register arg on stack, got {s} for layout {any}",
                                    .{ @tagName(arg_loc), arg_layout },
                                );
                            } else unreachable,
                        };
                        const reg0 = self.getArgumentRegister(reg_idx);
                        const reg1 = self.getArgumentRegister(reg_idx + 1);
                        const reg2 = self.getArgumentRegister(reg_idx + 2);
                        try self.emitLoad(.w64, reg0, frame_ptr, offset);
                        try self.emitLoad(.w64, reg1, frame_ptr, offset + 8);
                        try self.emitLoad(.w64, reg2, frame_ptr, offset + 16);
                        reg_idx += 3;
                    } else if (info.num_regs > 1) {
                        // Multi-register struct (record > 8 bytes)
                        const offset: i32 = switch (arg_loc) {
                            .stack => |s| s.offset,
                            else => {
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                try self.moveToReg(arg_loc, arg_reg);
                                reg_idx += 1;
                                continue;
                            },
                        };
                        var ri: u8 = 0;
                        while (ri < info.num_regs) : (ri += 1) {
                            const r = self.getArgumentRegister(reg_idx + ri);
                            try self.codegen.emitLoadStack(.w64, r, offset + @as(i32, ri) * 8);
                        }
                        reg_idx += info.num_regs;
                    } else {
                        // Single register argument
                        const arg_reg = self.getArgumentRegister(reg_idx);
                        if (arg_layout == .f32) {
                            const bits_reg = try self.materializeF32BitsInGeneralReg(arg_loc);
                            if (bits_reg != arg_reg) {
                                try self.codegen.emit.movRegReg(.w64, arg_reg, bits_reg);
                                self.codegen.freeGeneral(bits_reg);
                            }
                        } else switch (arg_loc) {
                            .general_reg => |reg| {
                                if (reg != arg_reg) {
                                    try self.codegen.emit.movRegReg(.w64, arg_reg, reg);
                                }
                            },
                            .stack => |s| {
                                try self.emitSizedLoadStack(arg_reg, s.offset, s.size);
                            },
                            .immediate_i64 => |val| {
                                try self.codegen.emitLoadImm(arg_reg, @bitCast(val));
                            },
                            else => {
                                try self.moveToReg(arg_loc, arg_reg);
                            },
                        }
                        reg_idx += 1;
                    }
                } else {
                    // Spill to stack — registers exhausted
                    try self.spillArgToStack(arg_loc, arg_layout, stack_arg_offset, info.num_regs);
                    stack_arg_offset += @as(i32, info.num_regs) * 8;
                    reg_idx = max_arg_regs;
                }
            }

            // Append roc_ops as the final argument
            if (config.emit_roc_ops) {
                const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                if (reg_idx < max_arg_regs) {
                    const arg_reg = self.getArgumentRegister(reg_idx);
                    try self.codegen.emit.movRegReg(.w64, arg_reg, roc_ops_reg);
                } else {
                    try self.spillArgToStack(.{ .general_reg = roc_ops_reg }, null, stack_arg_offset, 1);
                }
            }

            return stack_spill_size;
        }

        /// Bind lambda parameters from argument registers.
        /// Similar to bindProcParams but works with pattern spans.
        /// Handles stack spilling when arguments exceed available registers.
        /// Windows x64 has 4 arg regs (RCX, RDX, R8, R9), System V has 6 (RDI, RSI, RDX, RCX, R8, R9)
        const max_arg_regs: u8 = if (target.toCpuArch() == .aarch64) 8 else if (target.isWindows()) 4 else 6;

        /// Calculate the number of registers a parameter needs based on its layout.
        fn calcParamRegCount(self: *Self, layout_idx: layout.Idx) u8 {
            // String parameters need 3 registers (24 bytes)
            if (layout_idx == .str) return 3;
            // i128/u128/Dec parameters need 2 registers (16 bytes)
            if (layout_idx == .i128 or layout_idx == .u128 or layout_idx == .dec) return 2;

            {
                const ls = self.layout_store;
                if (@intFromEnum(layout_idx) < ls.layouts.len()) {
                    const layout_val = ls.getLayout(layout_idx);
                    if (layout_val.tag == .zst or ls.layoutSizeAlign(layout_val).size == 0) return 0;
                    // List parameters need 3 registers (24 bytes)
                    if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 3;
                    // Aggregate parameters may need multiple registers
                    if (layout_val.tag == .struct_ or layout_val.tag == .tag_union or layout_val.tag == .closure) {
                        const size = ls.layoutSizeAlign(layout_val).size;
                        if (size > 8) return @intCast((size + 7) / 8);
                    }
                }
            }
            // Default: single register
            return 1;
        }

        /// Copy parameter data from caller's stack frame to local stack.
        /// caller_offset is the offset from RBP/FP to the caller's argument.
        /// For x86_64: first stack arg is at [RBP+16], second at [RBP+24], etc.
        /// For aarch64: first stack arg is at [FP+16], second at [FP+24], etc.
        fn copyFromCallerStack(self: *Self, caller_offset: i32, local_offset: i32, num_regs: u8) Allocator.Error!void {
            const temp_reg: GeneralReg = scratch_reg;
            var ri: u8 = 0;
            while (ri < num_regs) : (ri += 1) {
                const off: i32 = @as(i32, ri) * 8;
                try self.emitLoad(.w64, temp_reg, frame_ptr, caller_offset + off);
                try self.emitStore(.w64, frame_ptr, local_offset + off, temp_reg);
            }
        }

        fn bindLambdaParams(self: *Self, params: lir.LirPatternSpan, initial_reg_idx: u8, force_pass_by_ptr: bool) Allocator.Error!void {
            const pattern_ids = self.store.getPatternSpan(params);

            // Pre-scan: determine which params are passed by pointer.
            // This must match the logic in generateCallToCompiledProc.
            const pbp_start = self.scratch_pass_by_ptr.top();
            defer self.scratch_pass_by_ptr.clearFrom(pbp_start);
            for (0..pattern_ids.len) |_| try self.scratch_pass_by_ptr.append(false);
            const param_pass_by_ptr = self.scratch_pass_by_ptr.sliceFromStart(pbp_start);

            {
                const pnr_start = self.scratch_param_num_regs.top();
                defer self.scratch_param_num_regs.clearFrom(pnr_start);
                for (0..pattern_ids.len) |_| try self.scratch_param_num_regs.append(1);
                const param_num_regs = self.scratch_param_num_regs.sliceFromStart(pnr_start);

                var pre_reg_count: u8 = initial_reg_idx;
                for (pattern_ids, 0..) |pid, pi| {
                    const pat = self.store.getPattern(pid);
                    const nr: u8 = switch (pat) {
                        .bind => |b| self.calcParamRegCount(b.layout_idx),
                        .wildcard => |w| self.calcParamRegCount(w.layout_idx),
                        .struct_ => |s| blk: {
                            const ls = self.layout_store;
                            const sl = ls.getLayout(s.struct_layout);
                            const sz = ls.layoutSizeAlign(sl).size;
                            break :blk @max(1, @as(u8, @intCast((sz + 7) / 8)));
                        },
                        .list => 3,
                        else => 1,
                    };
                    param_num_regs[pi] = nr;
                    if (force_pass_by_ptr and nr > 0) {
                        // Force all non-zero-sized params to be received as pointers.
                        // Used by the sort comparator trampoline which always passes
                        // element pointers to avoid C ABI vs internal ABI mismatches.
                        // Zero-sized params (nr == 0) are skipped here because they
                        // are handled as immediates before the pass-by-ptr check and
                        // don't consume an argument register.
                        param_pass_by_ptr[pi] = true;
                        pre_reg_count += 1; // pointer = 1 register
                        continue;
                    }
                    const is_i128_param = switch (pat) {
                        .bind => |b| b.layout_idx == .i128 or b.layout_idx == .u128 or b.layout_idx == .dec,
                        .wildcard => |w| w.layout_idx == .i128 or w.layout_idx == .u128 or w.layout_idx == .dec,
                        else => false,
                    };
                    if (comptime target.toCpuArch() == .aarch64) {
                        if (is_i128_param and pre_reg_count < max_arg_regs and pre_reg_count % 2 != 0) {
                            pre_reg_count += 1;
                        }
                    }
                    if (pre_reg_count + nr <= max_arg_regs) {
                        pre_reg_count += nr;
                    } else if (nr > 1) {
                        param_pass_by_ptr[pi] = true;
                        if (pre_reg_count + 1 <= max_arg_regs) {
                            pre_reg_count += 1;
                        } else {
                            pre_reg_count = max_arg_regs;
                        }
                    } else {
                        pre_reg_count = max_arg_regs;
                    }
                }
                // If roc_ops doesn't fit, convert more inline multi-reg args
                while (pre_reg_count + 1 > max_arg_regs) {
                    var found = false;
                    var best_idx: usize = 0;
                    var best_regs: u8 = 0;
                    for (param_num_regs, 0..) |pnr, pi| {
                        if (!param_pass_by_ptr[pi] and pnr > 1 and pnr > best_regs) {
                            best_idx = pi;
                            best_regs = pnr;
                            found = true;
                        }
                    }
                    if (!found) break;
                    param_pass_by_ptr[best_idx] = true;
                    // Recompute register pressure after changing pass-by-pointer flags,
                    // including aarch64 i128 alignment behavior.
                    pre_reg_count = initial_reg_idx;
                    for (pattern_ids, 0..) |pid2, pi2| {
                        const pat2 = self.store.getPattern(pid2);
                        const pnr = param_num_regs[pi2];
                        const pbp = param_pass_by_ptr[pi2];
                        const is_i128_param = switch (pat2) {
                            .bind => |b| b.layout_idx == .i128 or b.layout_idx == .u128 or b.layout_idx == .dec,
                            .wildcard => |w| w.layout_idx == .i128 or w.layout_idx == .u128 or w.layout_idx == .dec,
                            else => false,
                        };
                        if (comptime target.toCpuArch() == .aarch64) {
                            if (!pbp and is_i128_param and pre_reg_count < max_arg_regs and pre_reg_count % 2 != 0) {
                                pre_reg_count += 1;
                            }
                        }
                        const eff_regs: u8 = if (pbp) 1 else pnr;
                        if (pre_reg_count + eff_regs <= max_arg_regs) {
                            pre_reg_count += eff_regs;
                        } else {
                            pre_reg_count = max_arg_regs;
                        }
                    }
                }
            }

            var reg_idx: u8 = initial_reg_idx;
            // Track offset for stack arguments (first stack arg at RBP+16/FP+16)
            var stack_arg_offset: i32 = 16;

            for (pattern_ids, 0..) |pattern_id, param_idx| {
                const pattern = self.store.getPattern(pattern_id);
                switch (pattern) {
                    .bind => |bind| {
                        const symbol_key: u64 = @bitCast(bind.symbol);
                        const num_regs = self.calcParamRegCount(bind.layout_idx);

                        if (num_regs == 0) {
                            try self.symbol_locations.put(symbol_key, .{ .immediate_i64 = 0 });
                            try self.trackMutableSlotFromSymbolLocation(bind, symbol_key);
                            continue;
                        }

                        // Check if this param is passed by pointer (pre-computed)
                        if (param_pass_by_ptr[param_idx]) {
                            // Multi-register arg: caller passed a pointer (1 register).
                            // Use a hardcoded temp register to avoid allocTempGeneral returning
                            // the same register as the argument register (e.g. X0).
                            const temp_reg: GeneralReg = scratch_reg;
                            const size: u32 = @as(u32, num_regs) * 8;
                            const local_stack_offset = self.codegen.allocStackSlot(@intCast(size));
                            const ptr_reg = self.getArgumentRegister(reg_idx);
                            var ri: u8 = 0;
                            while (ri < num_regs) : (ri += 1) {
                                const off: i32 = @as(i32, ri) * 8;
                                try self.emitLoad(.w64, temp_reg, ptr_reg, off);
                                try self.emitStore(.w64, frame_ptr, local_stack_offset + off, temp_reg);
                            }

                            // Set up symbol location based on type
                            if (bind.layout_idx == .str) {
                                try self.symbol_locations.put(symbol_key, .{ .stack_str = local_stack_offset });
                            } else if (@intFromEnum(bind.layout_idx) < self.layout_store.layouts.len()) {
                                const layout_val = self.layout_store.getLayout(bind.layout_idx);
                                if (layout_val.tag == .list or layout_val.tag == .list_of_zst) {
                                    try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                        .struct_offset = local_stack_offset,
                                        .data_offset = 0,
                                        .num_elements = 0,
                                    } });
                                } else {
                                    try self.symbol_locations.put(symbol_key, self.stackLocationForLayout(bind.layout_idx, local_stack_offset));
                                }
                            } else {
                                try self.symbol_locations.put(symbol_key, self.stackLocationForLayout(bind.layout_idx, local_stack_offset));
                            }
                            reg_idx += 1;
                        } else if (reg_idx + num_regs <= max_arg_regs) {
                            // Fits in registers - use register-based loading
                            if (bind.layout_idx == .str) {
                                const arg_reg0 = self.getArgumentRegister(reg_idx);
                                const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                                const arg_reg2 = self.getArgumentRegister(reg_idx + 2);

                                const stack_offset = self.codegen.allocStackSlot(roc_str_size);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                                try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);
                                try self.codegen.emitStoreStack(.w64, stack_offset + 16, arg_reg2);

                                try self.symbol_locations.put(symbol_key, .{ .stack_str = stack_offset });
                                reg_idx += 3;
                            } else if (bind.layout_idx == .i128 or bind.layout_idx == .u128 or bind.layout_idx == .dec) {
                                // aarch64: i128/Dec must be even-aligned in register pairs,
                                // matching the alignment in placeCallArguments.
                                if (comptime target.toCpuArch() == .aarch64) {
                                    if (reg_idx % 2 != 0) {
                                        reg_idx += 1;
                                    }
                                }
                                const arg_reg0 = self.getArgumentRegister(reg_idx);
                                const arg_reg1 = self.getArgumentRegister(reg_idx + 1);

                                const stack_offset = self.codegen.allocStackSlot(16);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                                try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);

                                try self.symbol_locations.put(symbol_key, .{ .stack_i128 = stack_offset });
                                reg_idx += 2;
                            } else if (@intFromEnum(bind.layout_idx) < self.layout_store.layouts.len()) {
                                const layout_val = self.layout_store.getLayout(bind.layout_idx);
                                if (layout_val.tag == .list or layout_val.tag == .list_of_zst) {
                                    const arg_reg0 = self.getArgumentRegister(reg_idx);
                                    const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                                    const arg_reg2 = self.getArgumentRegister(reg_idx + 2);

                                    const stack_offset = self.codegen.allocStackSlot(roc_str_size);
                                    try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                                    try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);
                                    try self.codegen.emitStoreStack(.w64, stack_offset + 16, arg_reg2);

                                    try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                        .struct_offset = stack_offset,
                                        .data_offset = 0,
                                        .num_elements = 0,
                                    } });
                                    reg_idx += 3;
                                    continue;
                                }
                                if (layout_val.tag == .struct_ or layout_val.tag == .tag_union or layout_val.tag == .closure) {
                                    const size = self.layout_store.layoutSizeAlign(layout_val).size;
                                    if (size > 8) {
                                        // Multi-register aggregates are transferred as whole
                                        // register words, so give them an ABI-sized stack home.
                                        const abi_size: u32 = @as(u32, num_regs) * 8;
                                        const local_stack_offset = self.codegen.allocStackSlot(@intCast(abi_size));
                                        var ri: u8 = 0;
                                        while (ri < num_regs) : (ri += 1) {
                                            const arg_r = self.getArgumentRegister(reg_idx + ri);
                                            try self.codegen.emitStoreStack(.w64, local_stack_offset + @as(i32, ri) * 8, arg_r);
                                        }
                                        try self.symbol_locations.put(symbol_key, self.stackLocationForLayout(bind.layout_idx, local_stack_offset));
                                        reg_idx += num_regs;
                                        continue;
                                    }
                                }
                                // Default: single 8-byte value
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                const stack_offset = self.codegen.allocStackSlot(8);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                                try self.symbol_locations.put(symbol_key, self.stackLocationForLayout(bind.layout_idx, stack_offset));
                                reg_idx += 1;
                            } else {
                                // Default: single 8-byte value
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                const stack_offset = self.codegen.allocStackSlot(8);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                                try self.symbol_locations.put(symbol_key, self.stackLocationForLayout(bind.layout_idx, stack_offset));
                                reg_idx += 1;
                            }
                        } else {
                            // Doesn't fit in registers - read from caller's stack frame
                            const size: u32 = @as(u32, num_regs) * 8;
                            const local_stack_offset = self.codegen.allocStackSlot(@intCast(size));
                            try self.copyFromCallerStack(stack_arg_offset, local_stack_offset, num_regs);
                            try self.symbol_locations.put(symbol_key, self.stackLocationForLayout(bind.layout_idx, local_stack_offset));
                            stack_arg_offset += @as(i32, num_regs) * 8;
                            reg_idx = max_arg_regs; // Mark all registers as consumed
                        }

                        try self.trackMutableSlotFromSymbolLocation(bind, symbol_key);

                        // In the new pipeline, closure dispatch is handled by MIR→LIR as
                        // generic LIR constructs. No closure_param_metadata upgrade needed.
                    },
                    .wildcard => |wc| {
                        // Skip this argument - use the layout to determine how many
                        // registers it occupies (important for correct roc_ops placement)
                        const num_regs = self.calcParamRegCount(wc.layout_idx);
                        if (num_regs == 0) {
                            continue;
                        }
                        // aarch64: i128/Dec must be even-aligned in register pairs
                        if (comptime target.toCpuArch() == .aarch64) {
                            if (num_regs == 2 and (wc.layout_idx == .i128 or wc.layout_idx == .u128 or wc.layout_idx == .dec)) {
                                if (reg_idx % 2 != 0) reg_idx += 1;
                            }
                        }
                        if (param_pass_by_ptr[param_idx]) {
                            reg_idx += 1; // passed by pointer, skip 1 register
                        } else if (reg_idx + num_regs <= max_arg_regs) {
                            reg_idx += num_regs;
                        } else {
                            stack_arg_offset += @as(i32, num_regs) * 8;
                            reg_idx = max_arg_regs;
                        }
                    },
                    .as_pattern => |as_pat| {
                        const num_regs = self.calcParamRegCount(as_pat.layout_idx);
                        if (num_regs == 0) {
                            continue;
                        }

                        if (comptime target.toCpuArch() == .aarch64) {
                            if (num_regs == 2 and (as_pat.layout_idx == .i128 or as_pat.layout_idx == .u128 or as_pat.layout_idx == .dec)) {
                                if (reg_idx % 2 != 0) reg_idx += 1;
                            }
                        }

                        const abi_size: u32 = @as(u32, num_regs) * 8;
                        const stack_offset = self.codegen.allocStackSlot(@intCast(abi_size));

                        if (param_pass_by_ptr[param_idx]) {
                            const temp_r: GeneralReg = scratch_reg;
                            const ptr_reg = self.getArgumentRegister(reg_idx);
                            var ri: u8 = 0;
                            while (ri < num_regs) : (ri += 1) {
                                const off: i32 = @as(i32, ri) * 8;
                                try self.emitLoad(.w64, temp_r, ptr_reg, off);
                                try self.emitStore(.w64, frame_ptr, stack_offset + off, temp_r);
                            }
                            reg_idx += 1;
                        } else if (reg_idx + num_regs <= max_arg_regs) {
                            var ri: u8 = 0;
                            while (ri < num_regs) : (ri += 1) {
                                const arg_r = self.getArgumentRegister(reg_idx + ri);
                                try self.codegen.emitStoreStack(.w64, stack_offset + @as(i32, ri) * 8, arg_r);
                            }
                            reg_idx += num_regs;
                        } else {
                            try self.copyFromCallerStack(stack_arg_offset, stack_offset, num_regs);
                            stack_arg_offset += @as(i32, num_regs) * 8;
                            reg_idx = max_arg_regs;
                        }

                        try self.bindPattern(pattern_id, self.stackLocationForLayout(as_pat.layout_idx, stack_offset));
                    },
                    .struct_ => |s| {
                        // Struct destructuring: store registers to stack, then delegate to bindPattern
                        const ls = self.layout_store;
                        const struct_layout = ls.getLayout(s.struct_layout);
                        const size = ls.layoutSizeAlign(struct_layout).size;
                        const num_regs: u8 = if (size == 0) 0 else @as(u8, @intCast((size + 7) / 8));
                        if (num_regs == 0) {
                            continue;
                        }
                        const abi_size: u32 = @as(u32, num_regs) * 8;

                        if (param_pass_by_ptr[param_idx]) {
                            // Passed by pointer: copy from pointer to local stack.
                            // Use hardcoded temp to avoid clobbering the arg register.
                            const temp_r: GeneralReg = scratch_reg;
                            const stack_offset = self.codegen.allocStackSlot(@intCast(abi_size));
                            const ptr_reg = self.getArgumentRegister(reg_idx);
                            var ri: u8 = 0;
                            while (ri < num_regs) : (ri += 1) {
                                const off: i32 = @as(i32, ri) * 8;
                                try self.emitLoad(.w64, temp_r, ptr_reg, off);
                                try self.emitStore(.w64, frame_ptr, stack_offset + off, temp_r);
                            }
                            reg_idx += 1;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        } else if (reg_idx + num_regs <= max_arg_regs) {
                            const stack_offset = self.codegen.allocStackSlot(@intCast(abi_size));
                            var ri: u8 = 0;
                            while (ri < num_regs) : (ri += 1) {
                                const arg_r = self.getArgumentRegister(reg_idx + ri);
                                try self.codegen.emitStoreStack(.w64, stack_offset + @as(i32, ri) * 8, arg_r);
                            }
                            reg_idx += num_regs;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        } else {
                            // Read from caller's stack
                            const stack_offset = self.codegen.allocStackSlot(@intCast(abi_size));
                            try self.copyFromCallerStack(stack_arg_offset, stack_offset, num_regs);
                            stack_arg_offset += @as(i32, num_regs) * 8;
                            reg_idx = max_arg_regs;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        }
                    },
                    .list => {
                        // List destructuring: lists are 24 bytes (ptr, len, capacity) = 3 registers
                        if (param_pass_by_ptr[param_idx]) {
                            // Passed by pointer. Use hardcoded temp to avoid clobbering arg register.
                            const temp_r: GeneralReg = scratch_reg;
                            const stack_offset = self.codegen.allocStackSlot(roc_list_size);
                            const ptr_reg = self.getArgumentRegister(reg_idx);
                            var ri: u8 = 0;
                            while (ri < 3) : (ri += 1) {
                                const off: i32 = @as(i32, ri) * 8;
                                try self.emitLoad(.w64, temp_r, ptr_reg, off);
                                try self.emitStore(.w64, frame_ptr, stack_offset + off, temp_r);
                            }
                            reg_idx += 1;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        } else if (reg_idx + 3 <= max_arg_regs) {
                            const stack_offset = self.codegen.allocStackSlot(roc_str_size);
                            const arg_reg0 = self.getArgumentRegister(reg_idx);
                            const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                            const arg_reg2 = self.getArgumentRegister(reg_idx + 2);
                            try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                            try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);
                            try self.codegen.emitStoreStack(.w64, stack_offset + 16, arg_reg2);
                            reg_idx += 3;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        } else {
                            // Fallback: read from caller's stack
                            const stack_offset = self.codegen.allocStackSlot(roc_list_size);
                            try self.copyFromCallerStack(stack_arg_offset, stack_offset, 3);
                            stack_arg_offset += roc_list_size;
                            reg_idx = max_arg_regs;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        }
                    },
                    else => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: unsupported complex parameter pattern in bindLambdaParams (compileProcSpec path), got {s}",
                                .{@tagName(pattern)},
                            );
                        }
                        unreachable;
                    },
                }

                if (pattern == .bind) {
                    try self.trackMutableSlotFromSymbolLocation(pattern.bind, @bitCast(pattern.bind.symbol));
                }
            }

            // Receive roc_ops as the final argument (passed by the caller)
            // Store it in R12 (x86_64) or X20 (aarch64) for use by the lambda body
            const roc_ops_save_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64)
                .X20
            else
                .R12;

            if (reg_idx < max_arg_regs) {
                // roc_ops was passed in a register
                const arg_reg = self.getArgumentRegister(reg_idx);
                if (arg_reg != roc_ops_save_reg) {
                    try self.codegen.emit.movRegReg(.w64, roc_ops_save_reg, arg_reg);
                }
            } else {
                // roc_ops was passed on stack - load it
                try self.emitLoad(.w64, roc_ops_save_reg, frame_ptr, stack_arg_offset);
            }

            // Set roc_ops_reg for use by the lambda body when calling builtins
            self.roc_ops_reg = roc_ops_save_reg;
        }

        /// Move a value to the return register(s), using layout information for proper sizing.
        fn moveToReturnRegisterWithLayout(self: *Self, loc: ValueLocation, ret_layout: layout.Idx) Allocator.Error!void {
            if (loc == .noreturn) return;

            const ls = self.layout_store;
            const layout_val = ls.getLayout(ret_layout);

            if (builtin.mode == .Debug and loc == .stack_str and !(layout_val.tag == .scalar and layout_val.data.scalar.tag == .str) and layout_val.tag != .list and layout_val.tag != .list_of_zst) {
                std.debug.panic(
                    "LIR/codegen invariant violated: stack_str result with non-string/list return layout {s} (layout_idx={})",
                    .{
                        @tagName(layout_val.tag),
                        @intFromEnum(ret_layout),
                    },
                );
            }

            switch (layout_val.tag) {
                // 3 registers (24 bytes): lists
                .list, .list_of_zst => {
                    const stack_offset: i32 = switch (loc) {
                        .stack => |s| s.offset,
                        .list_stack => |info| info.struct_offset,
                        .stack_str => |off| off, // same 24-byte representation
                        else => unreachable,
                    };
                    try self.codegen.emitLoadStack(.w64, ret_reg_0, stack_offset);
                    try self.codegen.emitLoadStack(.w64, ret_reg_1, stack_offset + 8);
                    try self.codegen.emitLoadStack(.w64, ret_reg_2, stack_offset + 16);
                },
                // Scalars: dispatch by scalar kind
                .scalar => {
                    const scalar = layout_val.data.scalar;
                    switch (scalar.tag) {
                        // 3 registers (24 bytes): strings
                        .str => {
                            const stack_offset: i32 = switch (loc) {
                                .stack => |s| s.offset,
                                .stack_str => |off| off,
                                .list_stack => |info| info.struct_offset, // same 24-byte representation
                                else => unreachable,
                            };
                            try self.codegen.emitLoadStack(.w64, ret_reg_0, stack_offset);
                            try self.codegen.emitLoadStack(.w64, ret_reg_1, stack_offset + 8);
                            try self.codegen.emitLoadStack(.w64, ret_reg_2, stack_offset + 16);
                        },
                        .frac => {
                            const precision = scalar.data.frac;
                            if (precision == .dec) {
                                // Dec is 128-bit fixed-point: 2 general registers
                                switch (loc) {
                                    .stack_i128 => |offset| {
                                        try self.codegen.emitLoadStack(.w64, ret_reg_0, offset);
                                        try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                                    },
                                    .stack => |s| {
                                        try self.codegen.emitLoadStack(.w64, ret_reg_0, s.offset);
                                        try self.codegen.emitLoadStack(.w64, ret_reg_1, s.offset + 8);
                                    },
                                    .immediate_i128 => |val| {
                                        const low: i64 = @truncate(val);
                                        const high: i64 = @truncate(val >> 64);
                                        try self.codegen.emitLoadImm(ret_reg_0, low);
                                        try self.codegen.emitLoadImm(ret_reg_1, high);
                                    },
                                    .immediate_i64 => |val| {
                                        try self.codegen.emitLoadImm(ret_reg_0, @bitCast(val));
                                        try self.codegen.emitLoadImm(ret_reg_1, if (val < 0) @as(i64, -1) else 0);
                                    },
                                    else => unreachable,
                                }
                            } else {
                                // f32/f64: float register return
                                switch (loc) {
                                    .float_reg => |freg| {
                                        if (comptime target.toCpuArch() == .aarch64) {
                                            if (freg != .V0) try self.codegen.emit.fmovRegReg(.double, .V0, freg);
                                        } else {
                                            if (freg != .XMM0) try self.codegen.emit.movsdRegReg(.XMM0, freg);
                                        }
                                    },
                                    .stack => |s| {
                                        try self.codegen.emitLoadStackF64(if (comptime target.toCpuArch() == .aarch64) .V0 else .XMM0, s.offset);
                                    },
                                    .immediate_f64 => |val| {
                                        const bits: u64 = @bitCast(val);
                                        if (bits == 0) {
                                            if (comptime target.toCpuArch() == .aarch64) {
                                                try self.codegen.emit.fmovFloatFromGen(.double, .V0, .ZRSP);
                                            } else {
                                                try self.codegen.emit.xorpdRegReg(.XMM0, .XMM0);
                                            }
                                        } else {
                                            if (comptime target.toCpuArch() == .aarch64) {
                                                try self.codegen.emit.movRegImm64(.IP0, @bitCast(bits));
                                                try self.codegen.emit.fmovFloatFromGen(.double, .V0, .IP0);
                                            } else {
                                                const stack_offset = self.codegen.allocStackSlot(8);
                                                try self.codegen.emit.movRegImm64(.R11, @bitCast(bits));
                                                try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, .R11);
                                                try self.codegen.emit.movsdRegMem(.XMM0, .RBP, stack_offset);
                                            }
                                        }
                                    },
                                    else => unreachable,
                                }
                            }
                        },
                        // Integer scalars: 1 or 2 registers depending on precision
                        .int => {
                            const precision = scalar.data.int;
                            if (precision == .i128 or precision == .u128) {
                                // 2 registers (16 bytes)
                                switch (loc) {
                                    .stack_i128 => |offset| {
                                        try self.codegen.emitLoadStack(.w64, ret_reg_0, offset);
                                        try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                                    },
                                    .stack => |s| {
                                        try self.codegen.emitLoadStack(.w64, ret_reg_0, s.offset);
                                        try self.codegen.emitLoadStack(.w64, ret_reg_1, s.offset + 8);
                                    },
                                    .immediate_i128 => |val| {
                                        const low: i64 = @truncate(val);
                                        const high: i64 = @truncate(val >> 64);
                                        try self.codegen.emitLoadImm(ret_reg_0, low);
                                        try self.codegen.emitLoadImm(ret_reg_1, high);
                                    },
                                    .immediate_i64 => |val| {
                                        try self.codegen.emitLoadImm(ret_reg_0, @bitCast(val));
                                        try self.codegen.emitLoadImm(ret_reg_1, if (val < 0) @as(i64, -1) else 0);
                                    },
                                    else => unreachable,
                                }
                            } else {
                                // 1 register (≤ 8 bytes)
                                try self.moveOneRegToReturn(loc);
                            }
                        },
                    }
                },
                // Structs and tag unions: size determines register count
                .struct_, .tag_union, .closure => {
                    const size_align = ls.layoutSizeAlign(layout_val);
                    if (size_align.size == 0) {
                        // Zero-sized — nothing to move
                    } else if (size_align.size <= 8) {
                        // 1 register
                        try self.moveOneRegToReturn(loc);
                    } else {
                        // Multi-register: load N words from stack
                        const stack_offset: i32 = switch (loc) {
                            .stack => |s| s.offset,
                            .stack_i128 => |off| off,
                            .stack_str => |off| off,
                            else => unreachable,
                        };
                        const num_regs = (size_align.size + 7) / 8;
                        if (comptime target.toCpuArch() == .aarch64) {
                            const regs = [_]@TypeOf(GeneralReg.X0){ .X0, .X1, .X2, .X3, .X4, .X5, .X6, .X7, .XR, .X9, .X10, .X11, .X12, .X13, .X14, .X15 };
                            for (0..@min(num_regs, regs.len)) |i| {
                                try self.codegen.emitLoadStack(.w64, regs[i], stack_offset + @as(i32, @intCast(i * 8)));
                            }
                        } else {
                            const regs = [_]@TypeOf(GeneralReg.RAX){ .RAX, .RDX, .RCX, .R8, .R9, .R10, .R11, .RDI, .RSI };
                            for (0..@min(num_regs, regs.len)) |i| {
                                try self.codegen.emitLoadStack(.w64, regs[i], stack_offset + @as(i32, @intCast(i * 8)));
                            }
                        }
                    }
                },
                // Zero-sized types: nothing to move
                .zst => {},
                // Box: single pointer (1 register)
                .box, .box_of_zst => try self.moveOneRegToReturn(loc),
            }
        }

        /// Move a single-register value to the return register (ret_reg_0).
        fn moveOneRegToReturn(self: *Self, loc: ValueLocation) Allocator.Error!void {
            const ret_reg = ret_reg_0;
            switch (loc) {
                .general_reg => |reg| {
                    if (reg != ret_reg) {
                        try self.codegen.emit.movRegReg(.w64, ret_reg, reg);
                    }
                },
                .stack => |s| {
                    try self.emitSizedLoadStack(ret_reg, s.offset, s.size);
                },
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(ret_reg, @bitCast(val));
                },
                else => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: moveOneRegToReturn does not support loc={s}",
                            .{@tagName(loc)},
                        );
                    }
                    unreachable;
                },
            }
        }

        /// Copy a result value to the hidden return pointer buffer.
        /// Used when the return type exceeds the register limit and the caller
        /// has passed a pointer to a pre-allocated buffer as a hidden first argument.
        fn copyResultToReturnPointer(self: *Self, result_loc: ValueLocation, ret_layout: layout.Idx, ret_ptr_stack_slot: i32) Allocator.Error!void {
            const ls = self.layout_store;
            const layout_val = ls.getLayout(ret_layout);
            const ret_size = ls.layoutSizeAlign(layout_val).size;

            // Ensure result is on stack
            const result_offset: i32 = switch (result_loc) {
                .stack => |s| s.offset,
                .list_stack => |info| info.struct_offset,
                .stack_str => |off| off,
                .stack_i128 => |off| off,
                else => try self.ensureOnStack(result_loc, ret_size),
            };

            // Load the return pointer from the saved stack slot
            const ptr_reg: GeneralReg = scratch_reg;
            try self.emitLoad(.w64, ptr_reg, frame_ptr, ret_ptr_stack_slot);

            // Copy data in 8-byte chunks from local stack to return buffer
            const temp_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X10 else .RAX;
            const num_words = (ret_size + 7) / 8;
            for (0..num_words) |w| {
                const off: i32 = @intCast(w * 8);
                try self.emitLoad(.w64, temp_reg, frame_ptr, result_offset + off);
                try self.emitStore(.w64, ptr_reg, off, temp_reg);
            }
        }

        /// Generate code for a control flow statement
        fn generateStmt(self: *Self, stmt_id: CFStmtId) Allocator.Error!void {
            const stmt = self.store.getCFStmt(stmt_id);

            switch (stmt) {
                .let_stmt => |let_s| {
                    // Evaluate the value
                    const value_loc = try self.generateExpr(let_s.value);
                    if (value_loc == .noreturn) return;
                    // Bind to pattern
                    try self.bindPattern(let_s.pattern, value_loc);
                    // Continue with next statement
                    try self.generateStmt(let_s.next);
                },

                .join => |j| {
                    // Store param layouts and patterns for this join point (needed by rebindJoinPointParams)
                    const jp_key = @intFromEnum(j.id);
                    try self.join_point_param_layouts.put(jp_key, j.param_layouts);
                    try self.join_point_param_patterns.put(jp_key, j.params);

                    // Set up storage for join point parameters (they'll be rebound on each jump)
                    try self.setupJoinPointParams(j.id, j.params, j.param_layouts);
                    if (!self.join_point_jumps.contains(jp_key)) {
                        try self.join_point_jumps.put(jp_key, std.ArrayList(JumpRecord).empty);
                    }

                    // Generate REMAINDER first (code that eventually jumps TO the join point)
                    try self.generateStmt(j.remainder);

                    // Record where join point body starts (this is where jumps will target)
                    const join_location = self.codegen.currentOffset();
                    try self.join_points.put(jp_key, join_location);

                    // Generate BODY (what happens when jumped to)
                    try self.generateStmt(j.body);

                    // Patch all jumps to this join point
                    if (self.join_point_jumps.get(jp_key)) |jumps| {
                        for (jumps.items) |jump_record| {
                            self.codegen.patchJump(jump_record.location, join_location);
                        }
                    }
                },

                .jump => |jmp| {
                    // Evaluate all arguments first (before rebinding, in case args reference params)
                    const args = self.store.getExprSpan(jmp.args);
                    var arg_locs: std.ArrayListUnmanaged(ValueLocation) = .empty;
                    defer arg_locs.deinit(self.allocator);

                    for (args) |arg_id| {
                        const loc = try self.generateExpr(arg_id);
                        try arg_locs.append(self.allocator, loc);
                    }

                    // Rebind join point parameters to new argument values
                    try self.rebindJoinPointParams(jmp.target, arg_locs.items);

                    // Emit jump instruction with placeholder offset
                    const jump_location = try self.emitJumpPlaceholder();

                    // Record for patching
                    const jp_key = @intFromEnum(jmp.target);
                    if (self.join_point_jumps.getPtr(jp_key)) |jumps| {
                        try jumps.append(self.allocator, .{ .location = jump_location });
                    }
                },

                .ret => |r| {
                    // Evaluate the return value
                    const value_loc = try self.generateExpr(r.value);
                    if (value_loc == .noreturn) return;
                    const ret_layout = self.exprLayout(r.value);
                    const preserved_return_loc = self.normalizeResultLocForLayout(value_loc, ret_layout);

                    if (self.ret_ptr_slot) |ret_slot| {
                        try self.copyResultToReturnPointer(preserved_return_loc, ret_layout, ret_slot);
                    } else {
                        try self.moveToReturnRegisterWithLayout(preserved_return_loc, ret_layout);
                    }
                    // Emit jump to shared epilogue (patched after body gen knows actual frame size)
                    const patch = try self.codegen.emitJump();
                    try self.early_return_patches.append(self.allocator, patch);
                },

                .expr_stmt => |e| {
                    // Evaluate expression for side effects
                    const value_loc = try self.generateExpr(e.value);
                    if (value_loc == .noreturn) return;
                    // Continue with next
                    try self.generateStmt(e.next);
                },

                .switch_stmt => |sw| {
                    try self.generateSwitchStmt(sw);
                },

                .match_stmt => |ms| {
                    try self.generateMatchStmt(ms);
                },
            }
        }

        /// Set up storage locations for join point parameters
        fn setupJoinPointParams(self: *Self, _: JoinPointId, params: lir.LirPatternSpan, param_layouts: LayoutIdxSpan) Allocator.Error!void {
            const pattern_ids = self.store.getPatternSpan(params);
            const layouts = self.store.getLayoutIdxSpan(param_layouts);

            var reg_idx: u8 = 0;

            // For each parameter, allocate a register or stack slot
            for (pattern_ids, 0..) |pattern_id, param_idx| {
                const pattern = self.store.getPattern(pattern_id);
                switch (pattern) {
                    .bind => |bind| {
                        const symbol_key: u64 = @bitCast(bind.symbol);

                        // Check if this parameter is a 128-bit type
                        const is_128bit = if (param_idx < layouts.len) blk: {
                            const param_layout = layouts[param_idx];
                            break :blk param_layout == .i128 or param_layout == .u128 or param_layout == .dec;
                        } else false;

                        if (is_128bit) {
                            // 128-bit types need two consecutive registers
                            const low_reg = self.getArgumentRegister(reg_idx);
                            const high_reg = self.getArgumentRegister(reg_idx + 1);

                            // Allocate 16-byte stack slot
                            const stack_offset = self.codegen.allocStack(16);

                            // Store both registers to stack
                            try self.codegen.emitStoreStack(.w64, stack_offset, low_reg);
                            try self.codegen.emitStoreStack(.w64, stack_offset + 8, high_reg);

                            // Track as stack_i128
                            try self.symbol_locations.put(symbol_key, .{ .stack_i128 = stack_offset });
                            reg_idx += 2;
                        } else {
                            // Check if this is a string type (24 bytes)
                            const is_str = if (param_idx < layouts.len)
                                layouts[param_idx] == .str
                            else
                                false;

                            // Check if this is a list type (24 bytes)
                            const is_list = if (param_idx < layouts.len) blk: {
                                const param_layout = layouts[param_idx];
                                const ls = self.layout_store;
                                if (builtin.mode == .Debug and @intFromEnum(param_layout) >= ls.layouts.len()) {
                                    std.debug.panic(
                                        "LIR/codegen invariant violated: join point param layout out of bounds ({d} >= {d})",
                                        .{ @intFromEnum(param_layout), ls.layouts.len() },
                                    );
                                }
                                const layout_val = ls.getLayout(param_layout);
                                break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                            } else false;

                            if (is_str) {
                                // String types need 3 consecutive registers (24 bytes)
                                const stack_offset = self.codegen.allocStackSlot(roc_str_size);

                                const reg0 = self.getArgumentRegister(reg_idx);
                                const reg1 = self.getArgumentRegister(reg_idx + 1);
                                const reg2 = self.getArgumentRegister(reg_idx + 2);

                                try self.emitStore(.w64, frame_ptr, stack_offset, reg0);
                                try self.emitStore(.w64, frame_ptr, stack_offset + 8, reg1);
                                try self.emitStore(.w64, frame_ptr, stack_offset + 16, reg2);

                                try self.symbol_locations.put(symbol_key, .{ .stack_str = stack_offset });
                                reg_idx += 3;
                            } else if (is_list) {
                                // List types need 3 consecutive registers
                                const stack_offset = self.codegen.allocStackSlot(roc_str_size);

                                const reg0 = self.getArgumentRegister(reg_idx);
                                const reg1 = self.getArgumentRegister(reg_idx + 1);
                                const reg2 = self.getArgumentRegister(reg_idx + 2);

                                try self.emitStore(.w64, frame_ptr, stack_offset, reg0);
                                try self.emitStore(.w64, frame_ptr, stack_offset + 8, reg1);
                                try self.emitStore(.w64, frame_ptr, stack_offset + 16, reg2);

                                // Store as .list_stack so that when this parameter is used as an argument
                                // or returned, it's properly detected as a list
                                try self.symbol_locations.put(symbol_key, .{
                                    .list_stack = .{
                                        .struct_offset = stack_offset,
                                        .data_offset = 0, // Data location is stored in the list struct itself
                                        .num_elements = 0, // Unknown at compile time
                                    },
                                });
                                reg_idx += 3;
                            } else {
                                // Normal 64-bit or smaller parameter — spill to stack
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                const stack_offset = self.codegen.allocStackSlot(8);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                                try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = stack_offset } });
                                reg_idx += 1;
                            }
                        }
                    },
                    .wildcard => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: wildcard join params are not allowed in canonical tail-recursive form",
                                .{},
                            );
                        }
                        unreachable;
                    },
                    .int_literal, .float_literal, .str_literal, .tag, .struct_, .list, .as_pattern => unreachable, // Join point params must be simple bindings or wildcards
                }
            }
        }

        /// Rebind join point parameters to new argument values (for jump)
        /// This writes the new values directly to the stack slots used by symbol_locations,
        /// so that the join point body can read the updated values.
        fn rebindJoinPointParams(self: *Self, join_point: JoinPointId, arg_locs: []const ValueLocation) Allocator.Error!void {
            const jp_key = @intFromEnum(join_point);
            const param_layouts_span = self.join_point_param_layouts.get(jp_key) orelse unreachable;
            const param_patterns_span = self.join_point_param_patterns.get(jp_key) orelse unreachable;
            const layouts = self.store.getLayoutIdxSpan(param_layouts_span);
            const pattern_ids = self.store.getPatternSpan(param_patterns_span);

            if (arg_locs.len != pattern_ids.len) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: jump arg arity ({d}) does not match join param arity ({d})",
                        .{ arg_locs.len, pattern_ids.len },
                    );
                }
                unreachable;
            }
            if (pattern_ids.len != layouts.len) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: join param pattern/layout arity mismatch ({d} != {d})",
                        .{ pattern_ids.len, layouts.len },
                    );
                }
                unreachable;
            }

            // Two-phase copy to avoid clobbering when params reference each other
            // (e.g., `jump jp(b, a)` swaps params)

            // Phase 1: Copy all sources to temp stack slots
            const TempInfo = struct { offset: i32, size: u8 };
            var temp_infos: std.ArrayListUnmanaged(TempInfo) = .empty;
            defer temp_infos.deinit(self.allocator);

            for (arg_locs, 0..) |loc, param_idx| {
                const pattern = self.store.getPattern(pattern_ids[param_idx]);
                switch (pattern) {
                    .bind => {},
                    .wildcard => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: wildcard join params are not allowed in canonical tail-recursive form",
                                .{},
                            );
                        }
                        unreachable;
                    },
                    .int_literal, .float_literal, .str_literal, .tag, .struct_, .list, .as_pattern => unreachable,
                }

                const dst_loc = self.symbol_locations.get(switch (pattern) {
                    .bind => |bind| @bitCast(bind.symbol),
                    else => unreachable,
                }) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: missing destination location for join param during rebind",
                            .{},
                        );
                    }
                    unreachable;
                };

                // Determine param size
                const is_str = if (param_idx < layouts.len)
                    layouts[param_idx] == .str
                else
                    (dst_loc == .stack_str);

                const is_list = if (param_idx < layouts.len) blk: {
                    const param_layout = layouts[param_idx];
                    const ls = self.layout_store;
                    if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                        break :blk (loc == .list_stack or dst_loc == .list_stack);
                    }
                    const layout_val = ls.getLayout(param_layout);
                    break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                } else (loc == .list_stack or dst_loc == .list_stack);

                const is_i128 = dst_loc == .stack_i128;

                const size: u8 = if (is_list or is_str) 24 else if (is_i128) 16 else 8;
                const temp_offset = self.codegen.allocStackSlot(size);

                // Copy source to temp
                try self.copyParamValueToStack(loc, temp_offset, size, is_i128);
                if (param_idx < layouts.len and self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(layouts[param_idx]))) {
                    try self.emitIncrefAtStackOffset(temp_offset, layouts[param_idx]);
                }

                try temp_infos.append(self.allocator, .{ .offset = temp_offset, .size = size });
            }

            // Phase 2: Copy from temp slots to destination slots
            for (temp_infos.items, 0..) |temp_info, param_idx| {
                if (temp_info.size == 0) unreachable;

                const pattern = self.store.getPattern(pattern_ids[param_idx]);
                const symbol_key: u64 = switch (pattern) {
                    .bind => |bind| @bitCast(bind.symbol),
                    .wildcard => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: wildcard join params are not allowed in canonical tail-recursive form",
                                .{},
                            );
                        }
                        unreachable;
                    },
                    else => unreachable,
                };

                const dst_loc = self.symbol_locations.get(symbol_key) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: missing destination symbol location in join param rebind phase 2",
                            .{},
                        );
                    }
                    unreachable;
                };
                const dst_offset: i32 = switch (dst_loc) {
                    .stack => |s| s.offset,
                    .list_stack => |ls_info| ls_info.struct_offset,
                    .stack_i128 => |off| off,
                    .stack_str => |off| off,
                    .general_reg, .float_reg, .immediate_i64, .immediate_i128, .immediate_f64, .noreturn => unreachable,
                };

                if (param_idx < layouts.len and self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(layouts[param_idx]))) {
                    try self.emitDecrefAtStackOffset(dst_offset, layouts[param_idx]);
                }

                // Copy from temp to dst
                const temp_reg = try self.allocTempGeneral();
                var bytes_copied: u8 = 0;
                while (bytes_copied < temp_info.size) : (bytes_copied += 8) {
                    try self.emitLoad(.w64, temp_reg, frame_ptr, temp_info.offset + bytes_copied);
                    try self.emitStore(.w64, frame_ptr, dst_offset + bytes_copied, temp_reg);
                }
                self.codegen.freeGeneral(temp_reg);
            }
        }

        /// Copy a value to a stack slot (helper for rebindJoinPointParams)
        fn copyParamValueToStack(self: *Self, loc: ValueLocation, dst_offset: i32, size: u8, is_i128: bool) Allocator.Error!void {
            if (size == 24 or (size == 16 and !is_i128)) {
                // 24-byte (list/str) or generic stack copy
                const src_offset: i32 = switch (loc) {
                    .stack => |s| s.offset,
                    .list_stack => |ls_info| ls_info.struct_offset,
                    .stack_str => |off| off,
                    else => unreachable,
                };
                const temp_reg = try self.allocTempGeneral();
                var off: i32 = 0;
                while (off < size) : (off += 8) {
                    try self.emitLoad(.w64, temp_reg, frame_ptr, src_offset + off);
                    try self.emitStore(.w64, frame_ptr, dst_offset + off, temp_reg);
                }
                self.codegen.freeGeneral(temp_reg);
            } else if (is_i128) {
                switch (loc) {
                    .stack_i128 => |src_offset| {
                        const temp_reg = try self.allocTempGeneral();
                        try self.emitLoad(.w64, temp_reg, frame_ptr, src_offset);
                        try self.emitStore(.w64, frame_ptr, dst_offset, temp_reg);
                        try self.emitLoad(.w64, temp_reg, frame_ptr, src_offset + 8);
                        try self.emitStore(.w64, frame_ptr, dst_offset + 8, temp_reg);
                        self.codegen.freeGeneral(temp_reg);
                    },
                    .immediate_i128 => |val| {
                        const low: u64 = @truncate(@as(u128, @bitCast(val)));
                        const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                        const temp_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(temp_reg, @bitCast(low));
                        try self.codegen.emitStoreStack(.w64, dst_offset, temp_reg);
                        try self.codegen.emitLoadImm(temp_reg, @bitCast(high));
                        try self.codegen.emitStoreStack(.w64, dst_offset + 8, temp_reg);
                        self.codegen.freeGeneral(temp_reg);
                    },
                    else => unreachable,
                }
            } else {
                // 8-byte normal value
                const src_reg = try self.ensureInGeneralReg(loc);
                try self.emitStore(.w64, frame_ptr, dst_offset, src_reg);
                self.codegen.freeGeneral(src_reg);
            }
        }

        /// Emit a jump placeholder (will be patched later).
        /// Returns the patch location for use with patchJump.
        fn emitJumpPlaceholder(self: *Self) Allocator.Error!usize {
            if (comptime target.toCpuArch() == .aarch64) {
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.b(0);
                return patch_loc;
            } else {
                const patch_loc = self.codegen.currentOffset() + 1; // after E9 opcode
                try self.codegen.emit.jmp(0);
                return patch_loc;
            }
        }

        /// Generate code for a switch statement
        fn generateSwitchStmt(self: *Self, sw: anytype) Allocator.Error!void {
            // Evaluate condition
            const cond_loc = try self.generateExpr(sw.cond);
            const cond_reg = try self.ensureInGeneralReg(cond_loc);

            const branches = self.store.getCFSwitchBranches(sw.branches);

            // For single branch (bool switch): compare and branch
            if (branches.len == 1) {
                const branch = branches[0];

                // Compare with branch value and jump if NOT equal (to default)
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, cond_reg, @intCast(branch.value));
                } else {
                    try self.codegen.emit.cmpRegImm32(.w64, cond_reg, @intCast(branch.value));
                }

                // Jump to default if not equal
                const else_patch = try self.emitJumpIfNotEqual();

                self.codegen.freeGeneral(cond_reg);

                // Generate branch body (recursively generates statements)
                try self.generateStmt(branch.body);

                // Patch else jump to here
                const else_offset = self.codegen.currentOffset();
                self.codegen.patchJump(else_patch, else_offset);

                // Generate default branch
                try self.generateStmt(sw.default_branch);
            } else {
                // Multiple branches - generate cascading comparisons
                var end_patches = std.ArrayList(usize).empty;
                defer end_patches.deinit(self.allocator);

                for (branches, 0..) |branch, i| {
                    if (i < branches.len - 1) {
                        // Compare and skip if not match
                        try self.emitCmpImm(cond_reg, @intCast(branch.value));
                        const skip_patch = try self.emitJumpIfNotEqual();

                        // Generate branch body
                        try self.generateStmt(branch.body);

                        // Jump to end
                        const end_patch = try self.codegen.emitJump();
                        try end_patches.append(self.allocator, end_patch);

                        // Patch skip
                        const skip_offset = self.codegen.currentOffset();
                        self.codegen.patchJump(skip_patch, skip_offset);
                    } else {
                        // Last branch before default
                        try self.emitCmpImm(cond_reg, @intCast(branch.value));
                        const skip_patch = try self.emitJumpIfNotEqual();

                        try self.generateStmt(branch.body);

                        const end_patch = try self.codegen.emitJump();
                        try end_patches.append(self.allocator, end_patch);

                        self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
                    }
                }

                self.codegen.freeGeneral(cond_reg);

                // Generate default branch
                try self.generateStmt(sw.default_branch);

                // Patch all end jumps
                const end_offset = self.codegen.currentOffset();
                for (end_patches.items) |patch| {
                    self.codegen.patchJump(patch, end_offset);
                }
            }
        }

        /// Generate code for a match statement (pattern matching in tail position).
        /// Like generateMatch but each branch body is a CFStmt (handles its own ret/jump).
        fn generateMatchStmt(self: *Self, ms: anytype) Allocator.Error!void {
            // Evaluate the scrutinee
            const value_loc = try self.generateExpr(ms.value);

            const branches = self.store.getCFMatchBranches(ms.branches);
            if (branches.len == 0) {
                unreachable;
            }

            // Get layout info for tag unions
            const ls = self.layout_store;
            const value_layout_val = ls.getLayout(ms.value_layout);
            const tu_disc_offset: i32 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk @intCast(tu_data.discriminant_offset);
            } else 0;
            const tu_total_size: u32 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk tu_data.size;
            } else ls.layoutSizeAlign(value_layout_val).size;
            const tu_disc_size: u8 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk tu_data.discriminant_size;
            } else @intCast(@max(ls.layoutSizeAlign(value_layout_val).size, 1));
            const disc_use_w32 = (tu_disc_offset + 8 > @as(i32, @intCast(tu_total_size)));

            // Collect jump targets for patching to end
            var end_patches = std.ArrayList(usize).empty;
            defer end_patches.deinit(self.allocator);

            for (branches, 0..) |branch, i| {
                const pattern = self.store.getPattern(branch.pattern);
                const is_last_branch = (i == branches.len - 1);

                switch (pattern) {
                    .wildcard => {
                        const guard_patch = try self.emitGuardCheck(branch.guard);
                        if (guard_patch) |gp| {
                            try self.generateStmt(branch.body);
                            if (!is_last_branch) {
                                const end_patch = try self.codegen.emitJump();
                                try end_patches.append(self.allocator, end_patch);
                            }
                            self.codegen.patchJump(gp, self.codegen.currentOffset());
                        } else {
                            try self.generateStmt(branch.body);
                            break;
                        }
                    },
                    .bind => |bind| {
                        const symbol_key: u64 = @bitCast(bind.symbol);
                        try self.symbol_locations.put(symbol_key, value_loc);

                        const guard_patch = try self.emitGuardCheck(branch.guard);
                        if (guard_patch) |gp| {
                            try self.generateStmt(branch.body);
                            if (!is_last_branch) {
                                const end_patch = try self.codegen.emitJump();
                                try end_patches.append(self.allocator, end_patch);
                            }
                            self.codegen.patchJump(gp, self.codegen.currentOffset());
                        } else {
                            try self.generateStmt(branch.body);
                            break;
                        }
                    },
                    .int_literal => |int_lit| {
                        try self.emitIntPatternCheck(int_lit.value, value_loc);

                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            next_patch = try self.emitJumpIfNotEqual();
                        }

                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        try self.generateStmt(branch.body);

                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            if (next_patch) |patch| {
                                self.codegen.patchJump(patch, self.codegen.currentOffset());
                            }
                        }
                        if (guard_patch) |patch| {
                            self.codegen.patchJump(patch, self.codegen.currentOffset());
                        }
                    },
                    .str_literal => |str_lit_idx| {
                        try self.emitStringPatternCheck(str_lit_idx, value_loc);

                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            next_patch = try self.emitJumpIfEqual();
                        }

                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        try self.generateStmt(branch.body);

                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            if (next_patch) |patch| {
                                self.codegen.patchJump(patch, self.codegen.currentOffset());
                            }
                        }
                        if (guard_patch) |patch| {
                            self.codegen.patchJump(patch, self.codegen.currentOffset());
                        }
                    },
                    .tag => |tag_pattern| {
                        const disc_reg = try self.loadAndMaskDiscriminant(value_loc, disc_use_w32, tu_disc_offset, tu_disc_size);
                        try self.emitCmpImm(disc_reg, @intCast(tag_pattern.discriminant));
                        self.codegen.freeGeneral(disc_reg);

                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            next_patch = try self.emitJumpIfNotEqual();
                        }

                        // For patterns like Err(Exit(code)), check inner discriminants
                        // before binding any payload variables. If any inner discriminant
                        // does not match, we must fall through to the next branch.
                        var inner_fail_patches = std.ArrayList(usize).empty;
                        defer inner_fail_patches.deinit(self.allocator);
                        if (!is_last_branch) {
                            try self.emitInnerTagArgDiscriminantChecks(
                                tag_pattern,
                                value_loc,
                                ms.value_layout,
                                value_layout_val,
                                &inner_fail_patches,
                            );
                        }

                        // Bind tag payload fields
                        try self.bindTagPayloadFields(tag_pattern, value_loc, ms.value_layout, value_layout_val);

                        // Guard check (after bindings, since guard may reference bound vars)
                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        try self.generateStmt(branch.body);

                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            const next_branch_offset = self.codegen.currentOffset();
                            if (next_patch) |patch| {
                                self.codegen.patchJump(patch, next_branch_offset);
                            }
                            for (inner_fail_patches.items) |patch| {
                                self.codegen.patchJump(patch, next_branch_offset);
                            }
                        }
                        if (guard_patch) |patch| {
                            self.codegen.patchJump(patch, self.codegen.currentOffset());
                        }
                    },
                    .list => |list_pattern| {
                        const is_exact_match = list_pattern.rest.isNone();

                        // Check list length
                        try self.emitListLengthCheck(list_pattern, value_loc);

                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            if (is_exact_match) {
                                next_patch = try self.emitJumpIfNotEqual();
                            } else {
                                next_patch = try self.emitJumpIfLessThan();
                            }
                        }

                        // Bind prefix, suffix, and rest elements
                        try self.emitListPatternBindings(list_pattern, value_loc);

                        // Guard check (after bindings, since guard may reference bound vars)
                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        try self.generateStmt(branch.body);

                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            if (next_patch) |patch| {
                                self.codegen.patchJump(patch, self.codegen.currentOffset());
                            }
                        }
                        if (guard_patch) |patch| {
                            self.codegen.patchJump(patch, self.codegen.currentOffset());
                        }
                    },
                    .struct_ => {
                        const value_size = ls.layoutSizeAlign(value_layout_val).size;
                        const stack_off = try self.ensureOnStack(value_loc, value_size);
                        try self.bindPattern(branch.pattern, .{ .stack = .{ .offset = stack_off } });

                        const guard_patch = try self.emitGuardCheck(branch.guard);
                        if (guard_patch) |gp| {
                            try self.generateStmt(branch.body);
                            if (!is_last_branch) {
                                const end_patch = try self.codegen.emitJump();
                                try end_patches.append(self.allocator, end_patch);
                            }
                            self.codegen.patchJump(gp, self.codegen.currentOffset());
                        } else {
                            try self.generateStmt(branch.body);
                            break;
                        }
                    },
                    .as_pattern => |as_pat| {
                        const symbol_key: u64 = @bitCast(as_pat.symbol);
                        try self.symbol_locations.put(symbol_key, value_loc);
                        const value_size = ls.layoutSizeAlign(value_layout_val).size;
                        const stack_off = try self.ensureOnStack(value_loc, value_size);
                        try self.bindPattern(as_pat.inner, .{ .stack = .{ .offset = stack_off } });

                        const guard_patch = try self.emitGuardCheck(branch.guard);
                        if (guard_patch) |gp| {
                            try self.generateStmt(branch.body);
                            if (!is_last_branch) {
                                const end_patch = try self.codegen.emitJump();
                                try end_patches.append(self.allocator, end_patch);
                            }
                            self.codegen.patchJump(gp, self.codegen.currentOffset());
                        } else {
                            try self.generateStmt(branch.body);
                            break;
                        }
                    },
                    else => {
                        unreachable;
                    },
                }
            }

            // Patch all end jumps to here
            const end_offset = self.codegen.currentOffset();
            for (end_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }
        }

        /// Patch all pending calls after all procedures are compiled
        fn generateRcOperandValue(self: *Self, expr_id: LirExprId, layout_idx: layout.Idx) Allocator.Error!ValueLocation {
            switch (self.store.getExpr(expr_id)) {
                .cell_load => |load| {
                    const storage = self.resolveCellStorage(load.cell, load.layout_idx) orelse {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: RC op on unknown cell {d}",
                                .{@as(u64, @bitCast(load.cell))},
                            );
                        }
                        unreachable;
                    };

                    return self.stackLocationForLayout(layout_idx, storage.slot);
                },
                else => return self.generateExpr(expr_id),
            }
        }

        /// Generate code for incref operation
        /// Increments the reference count of a heap-allocated value
        fn generateIncref(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            const value_loc = try self.generateRcOperandValue(rc_op.value, rc_op.layout_idx);
            const ls = self.layout_store;
            const layout_val = ls.getLayout(rc_op.layout_idx);
            if (!ls.layoutContainsRefcounted(layout_val)) return value_loc;

            switch (layout_val.tag) {
                .closure => {
                    // In the dev backend, the value location for a closure is its
                    // captures payload, not a Closure header. Route RC through the
                    // captures layout explicitly and leave generic ordinary-data
                    // RC to the canonical helper path.
                    try self.emitRcHelperCallForValue(.incref, value_loc, layout_val.data.closure.captures_layout_idx, rc_op.count);
                },
                else => {
                    try self.emitRcHelperCallForValue(.incref, value_loc, rc_op.layout_idx, rc_op.count);
                },
            }

            return value_loc;
        }

        /// Generate code for decref operation
        /// Decrements the reference count and frees if it reaches zero
        fn generateDecref(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            const value_loc = try self.generateRcOperandValue(rc_op.value, rc_op.layout_idx);
            const ls = self.layout_store;
            const layout_val = ls.getLayout(rc_op.layout_idx);
            if (!ls.layoutContainsRefcounted(layout_val)) return value_loc;

            if (layout_val.tag == .closure) {
                try self.emitRcHelperCallForValue(.decref, value_loc, layout_val.data.closure.captures_layout_idx, 1);
            } else {
                try self.emitRcHelperCallForValue(.decref, value_loc, rc_op.layout_idx, 1);
            }

            return value_loc;
        }

        fn ensureValueOnStackForRc(self: *Self, value_loc: ValueLocation, value_size: u32) Allocator.Error!i32 {
            return switch (value_loc) {
                .stack => |s| s.offset,
                .stack_i128 => |off| off,
                .stack_str => |off| off,
                .list_stack => |info| info.struct_offset,
                .general_reg => |reg| blk: {
                    std.debug.assert(value_size <= 8);
                    const slot = self.codegen.allocStackSlot(@intCast(@max(value_size, @as(u32, 8))));
                    try self.emitStore(.w64, frame_ptr, slot, reg);
                    break :blk slot;
                },
                .immediate_i64 => |imm| blk: {
                    std.debug.assert(value_size <= 8);
                    const slot = self.codegen.allocStackSlot(@intCast(@max(value_size, @as(u32, 8))));
                    const tmp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(tmp, imm);
                    try self.emitStore(.w64, frame_ptr, slot, tmp);
                    self.codegen.freeGeneral(tmp);
                    break :blk slot;
                },
                else => unreachable,
            };
        }

        fn emitIncrefAtStackOffset(self: *Self, base_offset: i32, layout_idx: layout.Idx) Allocator.Error!void {
            const ls = self.layout_store;
            const layout_val = ls.getLayout(layout_idx);
            if (!ls.layoutContainsRefcounted(layout_val)) return;

            switch (layout_val.tag) {
                .closure => {
                    try self.emitIncrefAtStackOffset(base_offset, layout_val.data.closure.captures_layout_idx);
                },
                else => {
                    try self.emitRcHelperCallAtStackOffset(.incref, base_offset, layout_idx, 1);
                },
            }
        }

        fn emitDecrefAtStackOffset(self: *Self, base_offset: i32, layout_idx: layout.Idx) Allocator.Error!void {
            const ls = self.layout_store;
            const layout_val = ls.getLayout(layout_idx);
            if (!ls.layoutContainsRefcounted(layout_val)) return;

            switch (layout_val.tag) {
                .closure => {
                    try self.emitDecrefAtStackOffset(base_offset, layout_val.data.closure.captures_layout_idx);
                },
                else => {
                    try self.emitRcHelperCallAtStackOffset(.decref, base_offset, layout_idx, 1);
                },
            }
        }

        /// Generate code for free operation
        /// Directly frees memory without checking refcount
        fn generateFree(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            const value_loc = try self.generateRcOperandValue(rc_op.value, rc_op.layout_idx);
            const ls = self.layout_store;
            const layout_val = ls.getLayout(rc_op.layout_idx);
            if (!ls.layoutContainsRefcounted(layout_val)) return value_loc;

            switch (layout_val.tag) {
                // Dev closures expose captures directly, so dropping a closure value
                // means releasing the captures' owned children rather than treating
                // the value as a heap-owning outer allocation.
                .closure => try self.emitRcHelperCallForValue(.decref, value_loc, layout_val.data.closure.captures_layout_idx, 1),
                else => try self.emitRcHelperCallForValue(.free, value_loc, rc_op.layout_idx, 1),
            }

            return value_loc;
        }

        pub fn patchPendingCalls(self: *Self) Allocator.Error!void {
            for (self.pending_calls.items) |pending| {
                const proc = self.proc_registry.get(@intFromEnum(pending.target_proc)) orelse {
                    unreachable;
                };
                self.patchCallTarget(pending.call_site, proc.code_start);
            }
            self.pending_calls.clearRetainingCapacity();
        }

        /// Patch a call instruction to target a specific offset
        fn patchCallTarget(self: *Self, call_site: usize, target_offset: usize) void {
            const rel_offset: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(call_site)));

            if (comptime target.toCpuArch() == .aarch64) {
                // BL instruction: patch the immediate offset
                // BL uses 26-bit signed offset in instructions (multiply by 4)
                const instr_offset = @divTrunc(rel_offset, 4);
                self.codegen.patchBL(call_site, instr_offset);
            } else {
                // CALL rel32: patch the 32-bit relative offset
                // Offset is relative to instruction after CALL (call_site + 5)
                const call_rel = rel_offset - 5;
                self.codegen.patchCall(call_site, call_rel);
            }
        }

        /// Generate a RocCall-compatible entrypoint wrapper function.
        ///
        /// The RocCall ABI is:
        ///   fn(*RocOps, *anyopaque ret_ptr, *anyopaque args_ptr) callconv(.c) void
        ///
        /// This generates a function that:
        /// 1. Receives (roc_ops, ret_ptr, args_ptr) per C calling convention
        /// 2. Saves RocOps pointer for use by Roc code
        /// 3. Unpacks arguments from the args tuple at args_ptr
        /// 4. Calls the compiled Roc function body
        /// 5. Stores the result to ret_ptr
        /// 6. Returns void
        ///
        /// Returns the code offset where the wrapper function starts.
        pub fn generateEntrypointWrapper(
            self: *Self,
            name: []const u8,
            entry_proc: lir.LIR.LirProcSpecId,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
        ) Allocator.Error!ExportedSymbol {
            _ = name; // Used for the symbol name, passed through to result

            // Record start position
            const func_start = self.codegen.currentOffset();

            // Track prologue info for unwind tables (Windows x64)
            var prologue_size: u8 = 0;
            var stack_alloc: u32 = 0;

            // Clear state for this entrypoint
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();
            self.codegen.callee_saved_used = 0;

            // On entry, arguments are in:
            // x86_64 System V: RDI=roc_ops, RSI=ret_ptr, RDX=args_ptr
            // aarch64 AAPCS64: X0=roc_ops, X1=ret_ptr, X2=args_ptr

            if (arch == .aarch64 or arch == .aarch64_be) {
                // Use DeferredFrameBuilder pattern: generate body first, then prepend prologue.
                // This ensures the stack frame is correctly sized for the actual body code,
                // which may include lambda bodies that allocate many stack slots.

                // Save state that the body generation will modify
                const saved_callee_saved_used = self.codegen.callee_saved_used;
                const saved_callee_saved_available = self.codegen.callee_saved_available;
                const saved_roc_ops_reg = self.roc_ops_reg;
                const saved_early_return_patches_len = self.early_return_patches.items.len;

                // Mark X19/X20/X21 as used callee-saved registers (roc_ops, ret_ptr, args_ptr)
                const x19_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19);
                const x20_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20);
                const x21_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X21);
                self.codegen.callee_saved_used = x19_bit | x20_bit | x21_bit;
                // Remove from available pool so body code can't use them as temps
                self.codegen.callee_saved_available &= ~(x19_bit | x20_bit | x21_bit);

                // PHASE 1: Generate body first (to determine actual stack usage)
                // Initialize stack_offset for procedure-style frame (positive, grows upward from FP)
                // Layout: [FP/LR (16)] [callee-saved area (80)] [locals...]
                self.codegen.stack_offset = 16 + CodeGen.CALLEE_SAVED_AREA_SIZE;

                const body_start = self.codegen.currentOffset();
                const relocs_before = self.codegen.relocations.items.len;

                // Save args to callee-saved registers (safe because prologue will save them)
                try self.codegen.emit.movRegReg(.w64, .X19, .X0);
                try self.codegen.emit.movRegReg(.w64, .X20, .X1);
                try self.codegen.emit.movRegReg(.w64, .X21, .X2);

                self.roc_ops_reg = .X19;

                try self.generateEntrypointProcCall(entry_proc, arg_layouts, ret_layout, .X20, .X21);

                // Emit epilogue using DeferredFrameBuilder with actual stack usage
                const body_epilogue_offset = self.codegen.currentOffset();
                const actual_locals: u32 = @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE);
                {
                    var builder = CodeGen.DeferredFrameBuilder.init();
                    builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                    builder.setStackSize(actual_locals);
                    try builder.emitEpilogue(&self.codegen.emit);
                }

                const body_end = self.codegen.currentOffset();

                // PHASE 2: Extract body and prepend prologue
                const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                // Truncate buffer back to body_start
                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                // Emit prologue with actual stack usage
                const prologue_start = self.codegen.currentOffset();
                {
                    var frame_builder = CodeGen.DeferredFrameBuilder.init();
                    frame_builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                    frame_builder.setStackSize(actual_locals);
                    _ = try frame_builder.emitPrologue(&self.codegen.emit);
                }
                const prologue_size_val = self.codegen.currentOffset() - prologue_start;

                // Re-append body
                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                // Adjust relocation offsets
                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size_val);
                }

                self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size_val, std.math.maxInt(u64));
                self.shiftPendingCalls(body_start, body_end, prologue_size_val);
                // Re-patch internal calls/addr whose targets are outside the shifted body
                self.repatchInternalCalls(body_start, body_end, prologue_size_val, body_start);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size_val, body_start);

                // Patch early return jumps to the epilogue (shifted by prologue_size)
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size_val;
                }
                const final_epilogue = body_epilogue_offset - body_start + prologue_size_val + prologue_start;
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);

                // Restore state
                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.roc_ops_reg = saved_roc_ops_reg;
            } else {
                // x86_64: use DeferredFrameBuilder pattern (same as aarch64)
                const saved_callee_saved_used = self.codegen.callee_saved_used;
                const saved_callee_saved_available = self.codegen.callee_saved_available;
                const saved_roc_ops_reg = self.roc_ops_reg;
                const saved_early_return_patches_len = self.early_return_patches.items.len;

                // Mark RBX, R12, R13 as used callee-saved (ret_ptr, roc_ops, args_ptr)
                const rbx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
                const r12_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12);
                const r13_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R13);
                self.codegen.callee_saved_used = rbx_bit | r12_bit | r13_bit;
                self.codegen.callee_saved_available &= ~(rbx_bit | r12_bit | r13_bit);

                // Initialize stack_offset for procedure-style frame (negative, grows downward)
                self.codegen.stack_offset = -CodeGen.CALLEE_SAVED_AREA_SIZE;

                const body_start = self.codegen.currentOffset();
                const relocs_before = self.codegen.relocations.items.len;

                // Save args to callee-saved registers
                if (target.isWindows()) {
                    try self.codegen.emit.movRegReg(.w64, .R12, .RCX); // roc_ops
                    try self.codegen.emit.movRegReg(.w64, .RBX, .RDX); // ret_ptr
                    try self.codegen.emit.movRegReg(.w64, .R13, .R8); // args_ptr
                } else {
                    try self.codegen.emit.movRegReg(.w64, .R12, .RDI); // roc_ops
                    try self.codegen.emit.movRegReg(.w64, .RBX, .RSI); // ret_ptr
                    try self.codegen.emit.movRegReg(.w64, .R13, .RDX); // args_ptr
                }

                self.roc_ops_reg = .R12;

                try self.generateEntrypointProcCall(entry_proc, arg_layouts, ret_layout, .RBX, .R13);

                // Emit epilogue with actual stack usage
                const body_epilogue_offset = self.codegen.currentOffset();
                const actual_locals_x86: u32 = @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                {
                    var builder = CodeGen.DeferredFrameBuilder.init();
                    builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                    builder.setStackSize(actual_locals_x86);
                    try builder.emitEpilogue(&self.codegen.emit);
                }

                const body_end = self.codegen.currentOffset();

                // Prepend prologue
                const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                const prologue_start_x86 = self.codegen.currentOffset();
                try self.codegen.emitPrologueWithAlloc(actual_locals_x86);
                const prologue_size_x86 = self.codegen.currentOffset() - prologue_start_x86;

                prologue_size = @intCast(prologue_size_x86);
                stack_alloc = actual_locals_x86;

                // Re-append body + epilogue
                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                // Adjust relocation offsets
                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size_x86);
                }

                self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size_x86, std.math.maxInt(u64));
                self.shiftPendingCalls(body_start, body_end, prologue_size_x86);
                // Re-patch internal calls/addr whose targets are outside the shifted body
                self.repatchInternalCalls(body_start, body_end, prologue_size_x86, body_start);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size_x86, body_start);

                // Patch early return jumps
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size_x86;
                }
                const final_epilogue = body_epilogue_offset - body_start + prologue_size_x86 + prologue_start_x86;
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);

                // Restore state
                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.roc_ops_reg = saved_roc_ops_reg;
            }

            const func_end = self.codegen.currentOffset();

            return ExportedSymbol{
                .name = "", // Caller should set this
                .offset = func_start,
                .size = func_end - func_start,
                .prologue_size = prologue_size,
                .stack_alloc = stack_alloc,
                .uses_frame_pointer = true,
            };
        }

        const EntrypointArgOrder = struct {
            index: usize,
            alignment: u32,
            size: u32,
        };

        fn computeEntrypointArgOffsets(self: *Self, arg_layouts: []const layout.Idx, offsets: []u32) Allocator.Error!void {
            std.debug.assert(arg_layouts.len == offsets.len);

            var ordered = try self.allocator.alloc(EntrypointArgOrder, arg_layouts.len);
            defer self.allocator.free(ordered);

            for (arg_layouts, 0..) |arg_layout, i| {
                const size_align = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(arg_layout));
                ordered[i] = .{
                    .index = i,
                    .alignment = @intCast(size_align.alignment.toByteUnits()),
                    .size = size_align.size,
                };
            }

            const SortCtx = struct {
                fn lessThan(_: void, lhs: EntrypointArgOrder, rhs: EntrypointArgOrder) bool {
                    if (lhs.alignment != rhs.alignment) {
                        return lhs.alignment > rhs.alignment;
                    }
                    return lhs.index < rhs.index;
                }
            };

            std.mem.sort(EntrypointArgOrder, ordered, {}, SortCtx.lessThan);

            var current_offset: u32 = 0;
            for (ordered) |arg| {
                current_offset = std.mem.alignForward(u32, current_offset, arg.alignment);
                offsets[arg.index] = current_offset;
                current_offset += arg.size;
            }
        }

        fn entrypointParamSlotSize(self: *Self, layout_idx: layout.Idx) u32 {
            const size = self.getLayoutSize(layout_idx);
            if (size == 0) return 0;

            const abi_size = @as(u32, self.calcParamRegCount(layout_idx)) * 8;
            return @max(size, abi_size);
        }

        fn copyBytesFromMemToStackOffset(
            self: *Self,
            src_base: GeneralReg,
            src_offset: i32,
            dest_offset: i32,
            copy_size: u32,
            slot_size: u32,
        ) Allocator.Error!void {
            if (slot_size == 0) return;

            if (slot_size > copy_size) {
                try self.zeroStackArea(dest_offset, slot_size);
            }

            if (copy_size == 0) return;

            const temp_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(temp_reg);

            try self.copyChunked(temp_reg, src_base, src_offset, frame_ptr, dest_offset, copy_size);
        }

        fn materializeEntrypointArgInfos(
            self: *Self,
            arg_layouts: []const layout.Idx,
            args_ptr_reg: GeneralReg,
        ) Allocator.Error![]const ArgInfo {
            const arg_infos_start = self.scratch_arg_infos.top();

            const arg_offsets = try self.allocator.alloc(u32, arg_layouts.len);
            defer self.allocator.free(arg_offsets);
            try self.computeEntrypointArgOffsets(arg_layouts, arg_offsets);

            for (arg_layouts, 0..) |arg_layout, i| {
                const slot_size = self.entrypointParamSlotSize(arg_layout);
                if (slot_size == 0) {
                    try self.scratch_arg_infos.append(.{
                        .loc = .{ .immediate_i64 = 0 },
                        .layout_idx = arg_layout,
                        .num_regs = 0,
                    });
                    continue;
                }

                const slot_offset = self.codegen.allocStackSlot(@intCast(slot_size));
                try self.copyBytesFromMemToStackOffset(
                    args_ptr_reg,
                    @intCast(arg_offsets[i]),
                    slot_offset,
                    self.getLayoutSize(arg_layout),
                    slot_size,
                );

                const loc = self.stackLocationForLayout(arg_layout, slot_offset);
                try self.scratch_arg_infos.append(.{
                    .loc = loc,
                    .layout_idx = arg_layout,
                    .num_regs = self.calcArgRegCount(loc, arg_layout),
                });
            }

            return self.scratch_arg_infos.sliceFromStart(arg_infos_start);
        }

        fn callCompiledOffsetWithArgInfos(
            self: *Self,
            code_offset: usize,
            arg_infos: []const ArgInfo,
            ret_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const pbp_plan = try self.computePassByPtrPlan(arg_infos, 0, true);
            defer self.scratch_pass_by_ptr.clearFrom(pbp_plan.start);

            const stack_spill_size = try self.placeCallArguments(arg_infos, .{
                .pass_by_ptr = pbp_plan.slice,
                .emit_roc_ops = true,
            });
            try self.emitCallToOffset(code_offset);

            if (stack_spill_size > 0) {
                try self.emitAddStackPtr(stack_spill_size);
            }

            return self.saveCallReturnValue(ret_layout, false, 0);
        }

        fn generateEntrypointProcCall(
            self: *Self,
            entry_proc: lir.LIR.LirProcSpecId,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
            ret_ptr_reg: GeneralReg,
            args_ptr_reg: GeneralReg,
        ) Allocator.Error!void {
            const compiled = try self.procCodeOffsetWithOptions(entry_proc, .{});
            if (compiled.code_start == unresolved_proc_code_start) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "entrypoint proc {d} was not compiled before wrapper generation",
                        .{@intFromEnum(entry_proc)},
                    );
                }
                unreachable;
            }

            const arg_infos = try self.materializeEntrypointArgInfos(arg_layouts, args_ptr_reg);
            const result_loc = try self.callCompiledOffsetWithArgInfos(compiled.code_start, arg_infos, ret_layout);
            if (self.getLayoutSize(ret_layout) > 0) {
                try self.storeResultToSavedPtr(result_loc, ret_layout, ret_ptr_reg, 1);
            }
        }

        /// Get the size of a layout in bytes for argument unpacking
        fn getLayoutSize(self: *Self, layout_idx: layout.Idx) u32 {
            const ls = self.layout_store;
            const layout_val = ls.getLayout(layout_idx);
            return ls.layoutSizeAlign(layout_val).size;
        }

        /// Get the generated code buffer (for object file generation)
        pub fn getGeneratedCode(self: *Self) []const u8 {
            return self.codegen.getCode();
        }

        /// Get the relocations for the generated code
        pub fn getRelocations(self: *Self) []const Relocation {
            return self.codegen.relocations.items;
        }
    };
}

// Pre-instantiated LirCodeGen types for each supported target

/// x86_64 Linux (glibc)
pub const X64GlibcLirCodeGen = LirCodeGen(.x64glibc);
/// x86_64 Linux (musl)
pub const X64MuslLirCodeGen = LirCodeGen(.x64musl);
/// x86_64 Windows
pub const X64WinLirCodeGen = LirCodeGen(.x64win);
/// x86_64 macOS
pub const X64MacLirCodeGen = LirCodeGen(.x64mac);

/// ARM64 Linux (glibc)
pub const Arm64GlibcLirCodeGen = LirCodeGen(.arm64glibc);
/// ARM64 Linux (musl)
pub const Arm64MuslLirCodeGen = LirCodeGen(.arm64musl);
/// ARM64 Windows
pub const Arm64WinLirCodeGen = LirCodeGen(.arm64win);
/// ARM64 macOS
pub const Arm64MacLirCodeGen = LirCodeGen(.arm64mac);
/// ARM64 Linux (generic)
pub const Arm64LinuxLirCodeGen = LirCodeGen(.arm64linux);

/// x86_64 FreeBSD
pub const X64FreebsdLirCodeGen = LirCodeGen(.x64freebsd);
/// x86_64 OpenBSD
pub const X64OpenbsdLirCodeGen = LirCodeGen(.x64openbsd);
/// x86_64 NetBSD
pub const X64NetbsdLirCodeGen = LirCodeGen(.x64netbsd);
/// x86_64 Linux (generic)
pub const X64LinuxLirCodeGen = LirCodeGen(.x64linux);
/// x86_64 ELF (generic)
pub const X64ElfLirCodeGen = LirCodeGen(.x64elf);

/// Host LirCodeGen for the host platform (the machine running the compiler).
/// Fails at compile time on architectures that don't support native code generation.
pub const HostLirCodeGen = blk: {
    const native_target = RocTarget.detectNative();
    const arch = native_target.toCpuArch();
    if (arch == .x86_64 or arch == .aarch64 or arch == .aarch64_be) {
        break :blk LirCodeGen(native_target);
    } else {
        break :blk void;
    }
};

// Tests

const ExecutableMemory = @import("ExecutableMemory.zig").ExecutableMemory;

const TestLayoutState = struct {
    layout_store: layout.Store,
    module_env: *@import("can").ModuleEnv,

    fn init(allocator: Allocator) !TestLayoutState {
        const module_env = try allocator.create(@import("can").ModuleEnv);
        module_env.* = try @import("can").ModuleEnv.init(allocator, "");
        var module_env_ptrs: [1]*const @import("can").ModuleEnv = .{module_env};
        const layout_store = try layout.Store.init(
            &module_env_ptrs,
            null,
            allocator,
            base.target.TargetUsize.native,
        );
        return .{ .layout_store = layout_store, .module_env = module_env };
    }

    fn deinit(self: *TestLayoutState) void {
        const allocator = std.testing.allocator;
        self.layout_store.deinit();
        self.module_env.deinit();
        allocator.destroy(self.module_env);
    }
};

test "code generator initialization" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();
}

test "proc params and mutable list cells use distinct stack slots" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const u32_layout: layout.Idx = .u32;
    const list_layout = try test_state.layout_store.insertLayout(layout.Layout.list(u32_layout));

    const sym_start = Symbol.fromRaw(1);
    const sym_end = Symbol.fromRaw(2);
    const sym_answer = Symbol.fromRaw(3);

    const pat_start = try store.addPattern(.{ .bind = .{
        .symbol = sym_start,
        .layout_idx = u32_layout,
        .reassignable = false,
    } }, base.Region.zero());
    const pat_end = try store.addPattern(.{ .bind = .{
        .symbol = sym_end,
        .layout_idx = u32_layout,
        .reassignable = false,
    } }, base.Region.zero());
    const params = try store.addPatternSpan(&.{ pat_start, pat_end });

    const answer_load = try store.addExpr(.{ .cell_load = .{
        .cell = sym_answer,
        .layout_idx = list_layout,
    } }, base.Region.zero());
    const one = try store.addExpr(.{ .i64_literal = .{
        .value = 1,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const append_args = try store.addExprSpan(&.{ answer_load, one });
    const append_expr = try store.addExpr(.{ .low_level = .{
        .op = .list_append_unsafe,
        .args = append_args,
        .ret_layout = list_layout,
    } }, base.Region.zero());

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const HostCodeGen = @TypeOf(codegen.codegen);
    if (comptime builtin.cpu.arch == .aarch64) {
        codegen.codegen.stack_offset = 16 + HostCodeGen.CALLEE_SAVED_AREA_SIZE;
    } else {
        codegen.codegen.stack_offset = -HostCodeGen.CALLEE_SAVED_AREA_SIZE;
    }

    try codegen.bindLambdaParams(params, 0, false);

    const end_loc = codegen.symbol_locations.get(sym_end.raw()) orelse unreachable;
    const end_slot: i32 = switch (end_loc) {
        .stack => |s| s.offset,
        else => unreachable,
    };

    try codegen.initializeCell(sym_answer, list_layout, .{ .immediate_i64 = 0 });
    const answer_info = codegen.mutable_var_slots.get(sym_answer.raw()) orelse unreachable;
    try std.testing.expect(answer_info.slot != end_slot);

    const append_loc = try codegen.generateExpr(append_expr);
    const append_slot: i32 = switch (append_loc) {
        .list_stack => |info| info.struct_offset,
        else => unreachable,
    };
    try std.testing.expect(append_slot != end_slot);
    try std.testing.expect(append_slot != answer_info.slot);
}

test "two-arg proc list loop returns full length" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const RcInsertPass = lir.RcInsert.RcInsertPass;
    const RocAlloc = builtins.host_abi.RocAlloc;
    const RocDealloc = builtins.host_abi.RocDealloc;
    const RocRealloc = builtins.host_abi.RocRealloc;
    const RocDbg = builtins.host_abi.RocDbg;
    const RocExpectFailed = builtins.host_abi.RocExpectFailed;
    const RocCrashed = builtins.host_abi.RocCrashed;

    const SimpleTestEnv = struct {
        const Self = @This();

        allocator: Allocator,
        roc_ops: builtins.host_abi.RocOps,

        fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .roc_ops = .{
                    .env = undefined,
                    .roc_alloc = rocAlloc,
                    .roc_dealloc = rocDealloc,
                    .roc_realloc = rocRealloc,
                    .roc_dbg = rocDbg,
                    .roc_expect_failed = rocExpectFailed,
                    .roc_crashed = rocCrashed,
                    .hosted_fns = .{ .count = 0, .fns = undefined },
                },
            };
        }

        fn getOps(self: *Self) *builtins.host_abi.RocOps {
            self.roc_ops.env = @ptrCast(self);
            return &self.roc_ops;
        }

        fn metaBytes(alignment: usize) usize {
            return @max(alignment, @alignOf(usize));
        }

        fn rocAlloc(args: *RocAlloc, env: *anyopaque) callconv(.c) void {
            const self: *Self = @ptrCast(@alignCast(env));
            const align_enum = std.mem.Alignment.fromByteUnits(args.alignment);
            const meta = metaBytes(args.alignment);
            const total = args.length + meta;
            const alloc_base = self.allocator.rawAlloc(total, align_enum, @returnAddress()) orelse
                @panic("SimpleTestEnv alloc failed");
            const size_ptr: *usize = @ptrFromInt(@intFromPtr(alloc_base) + meta - @sizeOf(usize));
            size_ptr.* = total;
            args.answer = @ptrFromInt(@intFromPtr(alloc_base) + meta);
        }

        fn rocDealloc(args: *RocDealloc, env: *anyopaque) callconv(.c) void {
            const self: *Self = @ptrCast(@alignCast(env));
            const meta = metaBytes(args.alignment);
            const total_ptr: *const usize = @ptrFromInt(@intFromPtr(args.ptr) - @sizeOf(usize));
            const total = total_ptr.*;
            const alloc_base: [*]u8 = @ptrFromInt(@intFromPtr(args.ptr) - meta);
            const align_enum = std.mem.Alignment.fromByteUnits(args.alignment);
            self.allocator.rawFree(alloc_base[0..total], align_enum, @returnAddress());
        }

        fn rocRealloc(args: *RocRealloc, env: *anyopaque) callconv(.c) void {
            const self: *Self = @ptrCast(@alignCast(env));
            const old_ptr = args.answer;
            const meta = metaBytes(args.alignment);
            const old_total_ptr: *const usize = @ptrFromInt(@intFromPtr(old_ptr) - @sizeOf(usize));
            const old_total = old_total_ptr.*;
            const old_base: [*]u8 = @ptrFromInt(@intFromPtr(old_ptr) - meta);
            const new_total = args.new_length + meta;
            const align_enum = std.mem.Alignment.fromByteUnits(args.alignment);
            const new_base = self.allocator.rawAlloc(new_total, align_enum, @returnAddress()) orelse
                @panic("SimpleTestEnv realloc failed");
            @memcpy(new_base[0..@min(old_total, new_total)], old_base[0..@min(old_total, new_total)]);
            self.allocator.rawFree(old_base[0..old_total], align_enum, @returnAddress());
            const new_total_ptr: *usize = @ptrFromInt(@intFromPtr(new_base) + meta - @sizeOf(usize));
            new_total_ptr.* = new_total;
            args.answer = @ptrFromInt(@intFromPtr(new_base) + meta);
        }

        fn rocDbg(_: *const RocDbg, _: *anyopaque) callconv(.c) void {
            @panic("unexpected dbg in SimpleTestEnv");
        }

        fn rocExpectFailed(_: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {
            @panic("unexpected expect failure in SimpleTestEnv");
        }

        fn rocCrashed(args: *const RocCrashed, _: *anyopaque) callconv(.c) void {
            std.debug.panic("roc crashed: {s}", .{args.utf8_bytes[0..args.len]});
        }
    };

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const u32_layout: layout.Idx = .u32;
    const i64_layout: layout.Idx = .i64;
    const list_layout = try test_state.layout_store.insertLayout(layout.Layout.list(u32_layout));

    const sym_start = Symbol.fromRaw(101);
    const sym_end = Symbol.fromRaw(102);
    const sym_current = Symbol.fromRaw(103);
    const sym_answer = Symbol.fromRaw(104);
    const proc_symbol = Symbol.fromRaw(105);

    const pat_start = try store.addPattern(.{ .bind = .{
        .symbol = sym_start,
        .layout_idx = u32_layout,
        .reassignable = false,
    } }, base.Region.zero());
    const pat_end = try store.addPattern(.{ .bind = .{
        .symbol = sym_end,
        .layout_idx = u32_layout,
        .reassignable = false,
    } }, base.Region.zero());
    const proc_params = try store.addPatternSpan(&.{ pat_start, pat_end });

    const wildcard_zst = try store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, base.Region.zero());

    const start_lookup = try store.addExpr(.{ .lookup = .{
        .symbol = sym_start,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const end_lookup = try store.addExpr(.{ .lookup = .{
        .symbol = sym_end,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const current_load_cond = try store.addExpr(.{ .cell_load = .{
        .cell = sym_current,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const cond_args = try store.addExprSpan(&.{ current_load_cond, end_lookup });
    const cond_expr = try store.addExpr(.{ .low_level = .{
        .op = .num_is_lte,
        .args = cond_args,
        .ret_layout = .bool,
    } }, base.Region.zero());

    const answer_load_append = try store.addExpr(.{ .cell_load = .{
        .cell = sym_answer,
        .layout_idx = list_layout,
    } }, base.Region.zero());
    const current_load_append = try store.addExpr(.{ .cell_load = .{
        .cell = sym_current,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const append_args = try store.addExprSpan(&.{ answer_load_append, current_load_append });
    const append_expr = try store.addExpr(.{ .low_level = .{
        .op = .list_append_unsafe,
        .args = append_args,
        .ret_layout = list_layout,
    } }, base.Region.zero());

    const current_load_inc = try store.addExpr(.{ .cell_load = .{
        .cell = sym_current,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const one = try store.addExpr(.{ .i64_literal = .{
        .value = 1,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const add_args = try store.addExprSpan(&.{ current_load_inc, one });
    const next_current = try store.addExpr(.{ .low_level = .{
        .op = .num_plus,
        .args = add_args,
        .ret_layout = u32_layout,
    } }, base.Region.zero());

    const unit = try store.addExpr(.{ .struct_ = .{
        .struct_layout = .zst,
        .fields = try store.addExprSpan(&.{}),
    } }, base.Region.zero());

    const loop_body_stmts = try store.addStmts(&.{
        .{ .cell_store = .{
            .cell = sym_answer,
            .layout_idx = list_layout,
            .expr = append_expr,
        } },
        .{ .cell_store = .{
            .cell = sym_current,
            .layout_idx = u32_layout,
            .expr = next_current,
        } },
    });
    const loop_body = try store.addExpr(.{ .block = .{
        .stmts = loop_body_stmts,
        .final_expr = unit,
        .result_layout = .zst,
    } }, base.Region.zero());
    const while_expr = try store.addExpr(.{ .while_loop = .{
        .cond = cond_expr,
        .body = loop_body,
    } }, base.Region.zero());

    const empty_list = try store.addExpr(.{ .empty_list = .{
        .elem_layout = u32_layout,
        .list_layout = list_layout,
    } }, base.Region.zero());
    const final_answer = try store.addExpr(.{ .cell_load = .{
        .cell = sym_answer,
        .layout_idx = list_layout,
    } }, base.Region.zero());

    const raw_body_stmts = try store.addStmts(&.{
        .{ .cell_init = .{
            .cell = sym_current,
            .layout_idx = u32_layout,
            .expr = start_lookup,
        } },
        .{ .cell_init = .{
            .cell = sym_answer,
            .layout_idx = list_layout,
            .expr = empty_list,
        } },
        .{ .decl = .{
            .pattern = wildcard_zst,
            .expr = while_expr,
        } },
    });
    const raw_body = try store.addExpr(.{ .block = .{
        .stmts = raw_body_stmts,
        .final_expr = final_answer,
        .result_layout = list_layout,
    } }, base.Region.zero());

    var proc_rc = try RcInsertPass.init(allocator, &store, &test_state.layout_store);
    defer proc_rc.deinit();
    const proc_body = try proc_rc.insertRcOpsForProcBody(raw_body, proc_params, list_layout);
    const proc_body_stmt = try store.addCFStmt(.{ .ret = .{ .value = proc_body } });
    const arg_layouts = try store.addLayoutIdxSpan(&.{ u32_layout, u32_layout });
    const proc_id = try store.addProcSpec(.{
        .name = proc_symbol,
        .args = proc_params,
        .arg_layouts = arg_layouts,
        .body = proc_body_stmt,
        .ret_layout = list_layout,
        .closure_data_layout = null,
        .force_pass_by_ptr = false,
        .is_self_recursive = .not_self_recursive,
    });

    const start_arg = try store.addExpr(.{ .i64_literal = .{
        .value = 1,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const end_arg = try store.addExpr(.{ .i64_literal = .{
        .value = 5,
        .layout_idx = u32_layout,
    } }, base.Region.zero());
    const call_args = try store.addExprSpan(&.{ start_arg, end_arg });
    const call_expr = try store.addExpr(.{ .proc_call = .{
        .proc = proc_id,
        .args = call_args,
        .ret_layout = list_layout,
        .called_via = .apply,
    } }, base.Region.zero());
    const len_args = try store.addExprSpan(&.{call_expr});
    const raw_root = try store.addExpr(.{ .low_level = .{
        .op = .list_len,
        .args = len_args,
        .ret_layout = i64_layout,
    } }, base.Region.zero());

    var root_rc = try RcInsertPass.init(allocator, &store, &test_state.layout_store);
    defer root_rc.deinit();
    const root_expr = try root_rc.insertRcOps(raw_root);

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(store.getProcSpecs());

    const result = try codegen.generateCode(root_expr, i64_layout, 1);
    defer allocator.free(result.code);

    var executable = try ExecutableMemory.initWithEntryOffset(result.code, result.entry_offset);
    defer executable.deinit();

    var test_env = SimpleTestEnv.init(allocator);

    var out: i64 = -1;
    executable.callWithResultPtrAndRocOps(@ptrCast(&out), @ptrCast(test_env.getOps()));
    try std.testing.expectEqual(@as(i64, 5), out);
}

test "generate i64 literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Add an i64 literal
    const expr_id = try store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .i64, 1);
    defer allocator.free(result.code);

    // Should have generated some code
    try std.testing.expect(result.code.len > 0);
}

test "generate bool literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const expr_id = try store.addExpr(.{ .bool_literal = true }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .bool, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "entrypoint arg offsets preserve Roc alignment order" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    var offsets: [2]u32 = undefined;
    try codegen.computeEntrypointArgOffsets(&.{ .bool, .i64 }, offsets[0..]);

    try std.testing.expectEqualSlices(u32, &.{ 8, 0 }, offsets[0..]);
}

test "entrypoint param slots round aggregates to ABI word width" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const aggregate_layout = try test_state.layout_store.putTuple(&.{
        test_state.layout_store.getLayout(.f32),
        test_state.layout_store.getLayout(.f32),
        test_state.layout_store.getLayout(.f32),
    });

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    try std.testing.expectEqual(@as(u32, 16), codegen.entrypointParamSlotSize(aggregate_layout));
    try std.testing.expectEqual(@as(u32, 24), codegen.entrypointParamSlotSize(.str));
    try std.testing.expectEqual(@as(u32, 8), codegen.entrypointParamSlotSize(.bool));
}

test "tag payload bind invariant rejects mismatched pattern layout" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const pattern_id = try store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    try std.testing.expect(!(try codegen.patternLayoutCompatible(pattern_id, .u8)));
}

test "generate addition" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Create: 1 + 2
    const lhs_id = try store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } }, base.Region.zero());
    const rhs_id = try store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = .i64 } }, base.Region.zero());
    const ll_args = try store.addExprSpan(&.{ lhs_id, rhs_id });
    const add_id = try store.addExpr(.{ .low_level = .{
        .op = .num_plus,
        .args = ll_args,
        .ret_layout = .i64,
    } }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(add_id, .i64, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "record equality uses layout-aware comparison" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const can = @import("can");
    const ModuleEnv = can.ModuleEnv;
    const Layout = layout.Layout;
    const allocator = std.testing.allocator;

    // Set up ModuleEnv for ident storage
    var module_env = try ModuleEnv.init(allocator, "");
    defer module_env.deinit();

    // Create layout store with a record layout { a: Str, b: Str }
    var module_env_ptrs = [1]*const ModuleEnv{&module_env};
    var layout_store = try layout.Store.init(
        &module_env_ptrs,
        null,
        allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    const record_layout_idx = try layout_store.putRecord(&[_]Layout{ Layout.str(), Layout.str() });

    // Create LIR expressions: two record literals and an eq binop
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Field values (string literals represented as i64 placeholders — the codegen
    // only inspects the layout, not the actual field values for comparison dispatch)
    const str1 = try store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } }, base.Region.zero());
    const str2 = try store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } }, base.Region.zero());
    const str3 = try store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } }, base.Region.zero());
    const str4 = try store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } }, base.Region.zero());

    const fields1 = try store.addExprSpan(&[_]LirExprId{ str1, str2 });

    const fields2 = try store.addExprSpan(&[_]LirExprId{ str3, str4 });

    const lhs_record = try store.addExpr(.{ .struct_ = .{
        .struct_layout = record_layout_idx,
        .fields = fields1,
    } }, base.Region.zero());

    const rhs_record = try store.addExpr(.{ .struct_ = .{
        .struct_layout = record_layout_idx,
        .fields = fields2,
    } }, base.Region.zero());

    // LHS record == RHS record
    const eq_args = try store.addExprSpan(&.{ lhs_record, rhs_record });
    const eq_expr = try store.addExpr(.{ .low_level = .{
        .op = .num_is_eq,
        .args = eq_args,
        .ret_layout = .bool,
    } }, base.Region.zero());

    // With layout_store: should use generateStructComparisonByLayout (no crash)
    var codegen = try HostLirCodeGen.init(allocator, &store, &layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(eq_expr, .bool, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "generate modulo" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Create: 10 % 3
    const lhs = try store.addExpr(.{ .i64_literal = .{ .value = 10, .layout_idx = .i64 } }, base.Region.zero());
    const rhs = try store.addExpr(.{ .i64_literal = .{ .value = 3, .layout_idx = .i64 } }, base.Region.zero());
    const ll_args = try store.addExprSpan(&.{ lhs, rhs });
    const expr_id = try store.addExpr(.{ .low_level = .{
        .op = .num_mod_by,
        .args = ll_args,
        .ret_layout = .i64,
    } }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .i64, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "generate shift left" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Create: 1 << 4
    const lhs = try store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } }, base.Region.zero());
    const rhs = try store.addExpr(.{ .i64_literal = .{ .value = 4, .layout_idx = .i64 } }, base.Region.zero());
    const ll_args = try store.addExprSpan(&.{ lhs, rhs });
    const expr_id = try store.addExpr(.{ .low_level = .{
        .op = .num_shift_left_by,
        .args = ll_args,
        .ret_layout = .i64,
    } }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .i64, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "generate shift right" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Create: 64 >> 2 (arithmetic shift right, sign-extending)
    const lhs = try store.addExpr(.{ .i64_literal = .{ .value = 64, .layout_idx = .i64 } }, base.Region.zero());
    const rhs = try store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = .i64 } }, base.Region.zero());
    const ll_args = try store.addExprSpan(&.{ lhs, rhs });
    const expr_id = try store.addExpr(.{ .low_level = .{
        .op = .num_shift_right_by,
        .args = ll_args,
        .ret_layout = .i64,
    } }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .i64, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "generate shift right zero-fill" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Create: 64 >>> 2 (logical shift right, zero-filling)
    const lhs = try store.addExpr(.{ .i64_literal = .{ .value = 64, .layout_idx = .i64 } }, base.Region.zero());
    const rhs = try store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = .i64 } }, base.Region.zero());
    const ll_args = try store.addExprSpan(&.{ lhs, rhs });
    const expr_id = try store.addExpr(.{ .low_level = .{
        .op = .num_shift_right_zf_by,
        .args = ll_args,
        .ret_layout = .i64,
    } }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .i64, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "generate unary minus" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Create: -42
    const inner = try store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, base.Region.zero());
    const neg_args = try store.addExprSpan(&.{inner});
    const neg = try store.addExpr(.{ .low_level = .{
        .op = .num_negate,
        .args = neg_args,
        .ret_layout = .i64,
    } }, base.Region.zero());

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(neg, .i64, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}
