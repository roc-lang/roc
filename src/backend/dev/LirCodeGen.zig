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
const FrameBuilderMod = @import("FrameBuilder.zig");
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
const listSublist = builtins.list.listSublist;
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
const strIsEmpty = builtins.str.isEmpty;
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
const LambdaSetMember = lir.LambdaSetMember;
const LambdaSetMemberSpan = lir.LambdaSetMemberSpan;

// Layout store for accessing record/tuple/tag field offsets
const LayoutStore = layout.Store;

// Control flow statement types (for two-pass compilation)
const CFStmtId = lir.CFStmtId;
const LayoutIdxSpan = lir.LayoutIdxSpan;

/// Generation mode determines how builtin function calls are emitted.
/// This is important because the dev backend can be used in two ways:
/// 1. In-process execution (dev evaluator): Direct function pointers work
/// 2. Object file generation (roc build --backend=dev): Need symbol references
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
    str_is_empty,
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
    str_with_prefix,
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
            .str_is_empty => "roc_builtins_str_is_empty",
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
            .str_with_prefix => "roc_builtins_str_with_prefix",
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

/// Wrapper: isEmpty(RocStr) -> bool
fn wrapStrIsEmpty(str_bytes: ?[*]u8, str_len: usize, str_cap: usize) callconv(.c) bool {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    return strIsEmpty(s);
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

/// Wrapper for str_with_prefix: strConcatC(prefix, string, *RocOps) -> RocStr
/// str_with_prefix(string, prefix) = concat(prefix, string) — prefix goes first!
fn wrapStrWithPrefix(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, pfx_bytes: ?[*]u8, pfx_len: usize, pfx_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    const pfx = RocStr{ .bytes = pfx_bytes, .length = pfx_len, .capacity_or_alloc_ptr = pfx_cap };
    // with_prefix(string, prefix) is concat(prefix, string)
    out.* = strConcatC(pfx, s, roc_ops);
}

/// Wrapper: listConcat(RocList, RocList, alignment, element_width, ..., *RocOps) -> RocList
fn wrapListConcat(out: *RocList, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, alignment: u32, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    const a = RocList{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocList{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = listConcat(a, b, alignment, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listPrepend(RocList, alignment, element, element_width, ..., *RocOps) -> RocList
fn wrapListPrepend(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element: ?[*]u8, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listPrepend(list, alignment, element, element_width, false, null, @ptrCast(&rcNone), @ptrCast(&copy_fallback), roc_ops);
}

/// Wrapper: listSublist for drop_first/drop_last/take_first/take_last
fn wrapListSublist(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, start: u64, len: u64, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listSublist(list, alignment, element_width, false, start, len, null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listReplace for list_set
fn wrapListReplace(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, index: u64, element: ?[*]u8, element_width: usize, out_element: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReplace(list, alignment, index, element, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), out_element, @ptrCast(&copy_fallback), roc_ops);
}

/// Wrapper: listReserve
fn wrapListReserve(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, spare: u64, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReserve(list, alignment, spare, element_width, false, null, @ptrCast(&rcNone), .Immutable, roc_ops);
}

/// Wrapper: listReleaseExcessCapacity
fn wrapListReleaseExcessCapacity(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listReleaseExcessCapacity(list, alignment, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), .Immutable, roc_ops);
}

const LirProc = lir.LirProc;

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

        /// ForwardFrameBuilder for emitMainPrologue/emitMainEpilogue (push-based, prologue first)
        const ForwardFrameBuilder = FrameBuilderMod.ForwardFrameBuilder(@TypeOf(@as(CodeGen, undefined).emit));

        allocator: Allocator,

        /// Calling convention for the target platform (derived from comptime target)
        cc: CallingConvention,

        /// Architecture-specific code generator with register allocation
        codegen: CodeGen,

        /// The LIR store containing expressions to compile
        store: *const LirExprStore,

        /// Layout store for accessing record/tuple/tag field offsets
        layout_store: ?*const LayoutStore,

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

        /// Registry of compiled procedures (symbol -> CompiledProc)
        /// Used to find call targets during second pass
        proc_registry: std.AutoHashMap(u64, CompiledProc),

        /// Registry of compiled lambdas by expression ID.
        /// Used when a lambda is called - we compile it once and reuse.
        /// Key is @intFromEnum(LirExprId), value is code start offset.
        compiled_lambdas: std.AutoHashMap(u32, usize),

        /// Pending calls that need to be patched after all procedures are compiled
        pending_calls: std.ArrayList(PendingCall),

        /// Map from JoinPointId to list of jumps that target it (for patching)
        join_point_jumps: std.AutoHashMap(u32, std.ArrayList(JumpRecord)),

        /// Map from JoinPointId to parameter layouts (for i128 handling in rebind)
        join_point_param_layouts: std.AutoHashMap(u32, LayoutIdxSpan),

        /// Map from JoinPointId to parameter patterns (for rebinding to correct stack slots)
        join_point_param_patterns: std.AutoHashMap(u32, lir.LirPatternSpan),

        /// Tracks positions of BL/CALL instructions to compiled lambda procs.
        /// When compileLambdaAsProc shifts its body (extract, prepend prologue,
        /// re-append), BL instructions targeting code outside the body become
        /// incorrect. This list lets us re-patch those instructions after shifts.
        internal_call_patches: std.ArrayList(InternalCallPatch),

        /// Tracks positions of ADR/LEA instructions computing lambda addresses.
        /// Same shifting problem as internal_call_patches: when compileLambdaAsProc
        /// shifts its body, ADR/LEA instructions targeting code outside the body
        /// get incorrect PC-relative offsets.
        internal_addr_patches: std.ArrayList(InternalAddrPatch),

        /// Stack of early return jump patches.
        /// When generateEarlyReturn is called inside compileLambdaAsProc,
        /// it emits a jump to the epilogue and records the patch location here.
        /// After generating the lambda body, compileLambdaAsProc patches all
        /// early return jumps to point to the epilogue.
        early_return_patches: std.ArrayList(usize),

        /// Stack of forward-jump patches for break expressions inside loops.
        /// Each generateForLoop/generateWhileLoop saves the length, and after
        /// body generation, patches all new entries to the loop exit offset.
        loop_break_patches: std.ArrayList(usize),

        /// Stack slot where early return value is stored (for compileLambdaAsProc).
        /// Set by compileLambdaAsProc before generating the body.
        early_return_result_slot: ?i32 = null,

        /// Layout for early return result (to know how to move to return register)
        early_return_ret_layout: ?layout.Idx = null,

        /// Register where RocOps pointer is saved (for calling builtins that need it)
        roc_ops_reg: ?GeneralReg = null,

        /// Stack slot where the hidden return pointer is saved (for return-by-pointer
        /// convention used when the return type exceeds the register limit).
        /// Set during compileLambdaAsProc, used by moveToReturnRegisterWithLayout
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
            /// Offset into the code buffer where this procedure starts
            code_start: usize,
            /// Offset where this procedure ends
            code_end: usize,
            /// The symbol this procedure is bound to
            name: Symbol,
        };

        /// A pending call that needs to be patched after all procedures are compiled.
        pub const PendingCall = struct {
            /// Offset where the call instruction is (needs patching)
            call_site: usize,
            /// The function being called
            target_symbol: Symbol,
        };

        /// Tracks position of a BL/CALL to a compiled lambda proc.
        /// Used to re-patch relative offsets after body shifts in compileLambdaAsProc.
        pub const InternalCallPatch = struct {
            /// Buffer offset where the BL/CALL instruction starts
            call_offset: usize,
            /// Absolute buffer offset of the target (prologue start of the called lambda)
            target_offset: usize,
        };

        /// Tracks position of an ADR/LEA instruction computing a lambda address.
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
            /// Compiled lambda code location (for first-class functions)
            lambda_code: struct {
                /// Offset into code buffer where the procedure starts
                code_offset: usize,
                /// Layout of the function's return type
                ret_layout: layout.Idx,
            },
            /// Closure value on stack - for lambda set dispatch at call sites.
            /// Used when a closure is passed as an argument to a higher-order function,
            /// or when a single-function closure captures variables.
            closure_value: struct {
                /// Stack offset where closure data (captures) is stored
                stack_offset: i32,
                /// The closure representation (contains lambda set info for dispatch)
                representation: lir.ClosureRepresentation,
                /// The lambda body expression (for single-function closures)
                lambda: lir.LirExprId,
                /// Capture specifications (symbols and layouts)
                captures: lir.LIR.LirCaptureSpan,
            },
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
            layout_store_opt: ?*const LayoutStore,
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
                .proc_registry = std.AutoHashMap(u64, CompiledProc).init(allocator),
                .compiled_lambdas = std.AutoHashMap(u32, usize).init(allocator),
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
            self.compiled_lambdas.deinit();
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
            self.compiled_lambdas.clearRetainingCapacity();
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
            // Clear any leftover state from compileAllProcs to ensure clean slate
            // for the main expression. This is critical because procedure compilation
            // uses positive stack offsets while main expression uses negative offsets.
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();

            // Track where the main expression code starts
            // (procedures may have been compiled before this, at the start of the buffer)
            const main_code_start = self.codegen.currentOffset();

            // Reserve argument registers so they don't get allocated for temporaries
            // X0/RDI = result pointer, X1/RSI = RocOps pointer
            self.reserveArgumentRegisters();

            // Emit prologue to save callee-saved registers we'll use (X19 for result ptr)
            try self.emitMainPrologue();

            // IMPORTANT: Save the result pointer and RocOps pointer to callee-saved registers
            // before generating code that might call procedures (which would clobber them).
            // On aarch64: save X0 to X19, X1 to X20 (callee-saved)
            // On x86_64: save RDI to RBX, RSI to R12 (callee-saved)
            const result_ptr_save_reg = if (comptime target.toCpuArch() == .aarch64)
                aarch64.GeneralReg.X19
            else
                x86_64.GeneralReg.RBX;

            const roc_ops_save_reg = if (comptime target.toCpuArch() == .aarch64)
                aarch64.GeneralReg.X20
            else
                x86_64.GeneralReg.R12;

            // Get the argument registers for the current platform's calling convention
            // aarch64: X0, X1
            // x86_64 System V (Linux, macOS): RDI, RSI
            // x86_64 Windows: RCX, RDX
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

            // Store RocOps save reg for use by Dec operations
            self.roc_ops_reg = roc_ops_save_reg;

            // Generate code for the expression - result ends up in a register
            const result_loc = try self.generateExpr(expr_id);

            // Track the actual return layout (may differ from result_layout if body is a closure/lambda)
            var actual_ret_layout = result_layout;

            const final_result = switch (result_loc) {
                .lambda_code => |lc| blk: {
                    // The lambda's return layout is the actual return type
                    actual_ret_layout = lc.ret_layout;
                    // Call the lambda
                    const current_offset = self.codegen.currentOffset();
                    if (comptime target.toCpuArch() == .aarch64) {
                        const rel_offset: i28 = @intCast(@as(i32, @intCast(lc.code_offset)) - @as(i32, @intCast(current_offset)));
                        try self.codegen.emit.bl(rel_offset);
                    } else {
                        // x86_64: emit relative call
                        const rel_offset: i32 = @intCast(@as(i32, @intCast(lc.code_offset)) - @as(i32, @intCast(current_offset)) - 5);
                        try self.codegen.emit.callRel32(rel_offset);
                    }
                    // Result is in X0/RAX
                    break :blk if (comptime target.toCpuArch() == .aarch64)
                        ValueLocation{ .general_reg = .X0 }
                    else
                        ValueLocation{ .general_reg = .RAX };
                },
                .closure_value => |cv| blk: {
                    // Dispatch the closure call with no arguments
                    // The closure's return layout is the actual return type, not the closure layout
                    const lambda_expr = self.store.getExpr(cv.lambda);
                    const lambda = switch (lambda_expr) {
                        .lambda => |l| l,
                        .closure => |c_id| inner: {
                            const c = self.store.getClosureData(c_id);
                            const inner = self.store.getExpr(c.lambda);
                            if (inner == .lambda) break :inner inner.lambda;
                            unreachable;
                        },
                        else => unreachable,
                    };
                    actual_ret_layout = lambda.ret_layout;
                    const empty_span = lir.LIR.LirExprSpan.empty();
                    break :blk try self.generateClosureDispatch(cv, empty_span, actual_ret_layout);
                },
                else => result_loc,
            };

            // Store result to the saved result pointer - but only if return type is non-zero-sized
            // Per RocCall ABI: "If the Roc function returns a zero-sized type like `{}`,
            // it will not write anything into this address."
            const ret_size = self.getLayoutSize(actual_ret_layout);
            if (ret_size > 0) {
                try self.storeResultToSavedPtr(final_result, actual_ret_layout, result_ptr_save_reg, tuple_len);
            }

            // Emit epilogue to restore callee-saved registers and return
            try self.emitMainEpilogue();

            // Patch all pending calls now that all procedures are compiled
            try self.patchPendingCalls();

            // Get ALL the generated code (including procedures at the start)
            // Execution will start at main_code_start via entry_offset
            const all_code = self.codegen.getCode();

            // Make a copy of the code since codegen buffer may be reused
            const code_copy = self.allocator.dupe(u8, all_code) catch return error.OutOfMemory;

            return CodeResult{
                .code = code_copy,
                .relocations = self.codegen.relocations.items,
                .result_layout = result_layout,
                .entry_offset = main_code_start,
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

        /// Get the layout of an expression (if available and valid for our layout store)
        fn getExprLayout(self: *Self, expr_id: LirExprId) ?layout.Idx {
            const expr = self.store.getExpr(expr_id);
            const raw_layout: ?layout.Idx = switch (expr) {
                // Expressions that store their layout
                .record => |rec| rec.record_layout,
                .tuple => |tup| tup.tuple_layout,
                .tag => |tag| tag.union_layout,
                .lookup => |lookup| lookup.layout_idx,
                .field_access => |fa| fa.field_layout,
                .binop => |binop| binop.result_layout,
                .unary_minus => |um| um.result_layout,
                .call => |call| call.ret_layout,
                .low_level => |ll| ll.ret_layout,
                .hosted_call => |hc| hc.ret_layout,
                // Compound expressions with result layouts
                .if_then_else => |ite| ite.result_layout,
                .match_expr => |w| w.result_layout,
                .block => |b| b.result_layout,
                .dbg => |d| d.result_layout,
                .expect => |e| e.result_layout,
                .early_return => |er| er.ret_layout,
                // Literals with known layouts
                .i64_literal => .i64,
                .f64_literal => .f64,
                .f32_literal => .f32,
                .bool_literal => .bool,
                .i128_literal => .i128,
                .dec_literal => .dec,
                .str_literal => .str,
                .str_concat => .str,
                .int_to_str => .str,
                .float_to_str => .str,
                .dec_to_str => .str,
                .str_escape_and_quote => .str,
                .discriminant_switch => null,
                // For other expressions, no layout available
                else => null,
            };

            if (raw_layout) |layout_idx| {
                return layout_idx;
            }
            return null;
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
            const expr = self.store.getExpr(expr_id);

            return switch (expr) {
                // Literals
                .i64_literal => |val| .{ .immediate_i64 = val },
                .i128_literal => |val| try self.generateI128Literal(val),
                .f64_literal => |val| .{ .immediate_f64 = val },
                .f32_literal => |val| .{ .immediate_f64 = @floatCast(val) },
                .bool_literal => |val| .{ .immediate_i64 = if (val) 1 else 0 },
                .dec_literal => |val| try self.generateI128Literal(val),

                // Lookups
                .lookup => |lookup| try self.generateLookup(lookup.symbol, lookup.layout_idx),

                // Binary operations
                .binop => |binop| try self.generateBinop(binop),

                // Unary operations
                .unary_minus => |unary| try self.generateUnaryMinus(unary),
                .unary_not => |unary| try self.generateUnaryNot(unary),

                // Control flow
                .if_then_else => |ite| try self.generateIfThenElse(ite),
                .match_expr => |m| try self.generateMatch(m),

                // Blocks
                .block => |block| try self.generateBlock(block),

                // Function calls and lambdas
                .call => |call| try self.generateCall(call),
                // Lambdas and closures as first-class values (stored to variables)
                .lambda => |lambda| {
                    const code_offset = try self.compileLambdaAsProc(expr_id, lambda);
                    return .{ .lambda_code = .{
                        .code_offset = code_offset,
                        .ret_layout = lambda.ret_layout,
                    } };
                },
                .closure => |closure_id| {
                    return try self.generateClosure(self.store.getClosureData(closure_id));
                },

                // Records
                .empty_record => .{ .immediate_i64 = 0 },
                .record => |record| try self.generateRecord(record),
                .field_access => |fa| try self.generateFieldAccess(fa),

                // Tuples
                .tuple => |tuple| try self.generateTuple(tuple),
                .tuple_access => |ta| try self.generateTupleAccess(ta),

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
                .list_is_empty => {
                    // List is empty if length is 0
                    std.debug.assert(args.len >= 1);
                    const list_loc = try self.generateExpr(args[0]);

                    const base_offset: i32 = switch (list_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        .immediate_i64 => |val| {
                            // Empty list - is_empty returns true (1)
                            if (val == 0) {
                                return .{ .immediate_i64 = 1 };
                            }
                            unreachable;
                        },
                        else => unreachable,
                    };

                    {
                        // Length is at offset 8 - check if zero
                        const len_reg = try self.allocTempGeneral();
                        try self.emitLoad(.w64, len_reg, frame_ptr, base_offset + 8);
                        // Compare with 0
                        try self.emitCmpImm(len_reg, 0);
                        // Set result to 1 if equal (empty), 0 otherwise
                        const result_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(result_reg, 0);
                        const one_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(one_reg, 1);
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.csel(.w64, result_reg, one_reg, result_reg, .eq);
                        } else {
                            try self.codegen.emit.cmovcc(.equal, .w64, result_reg, one_reg);
                        }
                        self.codegen.freeGeneral(one_reg);
                        self.codegen.freeGeneral(len_reg);
                        return .{ .general_reg = result_reg };
                    }
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
                    const ls = self.layout_store orelse {
                        unreachable;
                    };
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

                    // roc_builtins_list_with_capacity(out, capacity, alignment, element_width, roc_ops)
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addRegArg(cap_reg);
                    self.codegen.freeGeneral(cap_reg);
                    try builder.addImmArg(@intCast(alignment_bytes));
                    try builder.addImmArg(@intCast(elem_size_align.size));
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
                .list_append => {
                    // list_append(list, element) -> List
                    // Uses SAFE listAppendSafeC that reserves capacity if needed
                    if (args.len != 2) {
                        unreachable;
                    }

                    const ls = self.layout_store orelse {
                        unreachable;
                    };

                    const roc_ops_reg = self.roc_ops_reg orelse {
                        unreachable;
                    };

                    // Generate list argument (must be on stack - 24 bytes)
                    const list_loc = try self.generateExpr(args[0]);

                    // Generate element value
                    const elem_loc = try self.generateExpr(args[1]);
                    // Determine element size from layout.
                    // Cross-module layout resolution can produce incorrect ret_layout
                    // (e.g., list_of_zst instead of list(i64)) when the builtin function's
                    // layout index refers to a layout in the wrong module context.
                    // When this happens, derive the correct element size from the list
                    // argument's layout.
                    const elem_size_align: layout.SizeAlign = blk: {
                        const ret_layout_val = ls.getLayout(ll.ret_layout);
                        if (ret_layout_val.tag == .list) {
                            const sa = ls.layoutSizeAlign(ls.getLayout(ret_layout_val.data.list));
                            if (sa.size > 0) break :blk sa;
                        }
                        // ret_layout is list_of_zst or has ZST element — try deriving
                        // the correct element size from the list argument's layout
                        if (self.getExprLayout(args[0])) |list_layout_idx| {
                            const list_layout = ls.getLayout(list_layout_idx);
                            if (list_layout.tag == .list) {
                                const derived = ls.layoutSizeAlign(ls.getLayout(list_layout.data.list));
                                if (derived.size > 0) break :blk derived;
                            }
                        }
                        // Also check the element argument's layout
                        if (self.getExprLayout(args[1])) |elem_layout_idx| {
                            const sa = ls.layoutSizeAlign(ls.getLayout(elem_layout_idx));
                            if (sa.size > 0) break :blk sa;
                        }
                        // Truly ZST
                        break :blk .{ .size = 0, .alignment = .@"1" };
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

                        // roc_builtins_list_append_safe(out, list_bytes, list_len, list_cap, element, alignment, element_width, roc_ops)
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_offset);
                        try builder.addMemArg(base_reg, list_offset + 8);
                        try builder.addMemArg(base_reg, list_offset + 16);
                        try builder.addLeaArg(base_reg, elem_offset);
                        try builder.addImmArg(@intCast(alignment_bytes));
                        try builder.addImmArg(@intCast(elem_size_align.size));
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
                .list_get => {
                    // list_get(list, index) -> element or Try(element, [OutOfBounds])
                    //
                    // When ret_layout is a tag_union (from List.get method dispatch),
                    // this performs bounds checking and returns a proper Try result:
                    //   if index < list.len then Ok(element) else Err(OutOfBounds)
                    //
                    // When ret_layout is NOT a tag_union (from list_get_unsafe),
                    // this returns the bare element without bounds checking.
                    std.debug.assert(args.len >= 2);
                    const list_loc = try self.generateExpr(args[0]);
                    const index_loc = try self.generateExpr(args[1]);

                    // Get base offset of list struct
                    const list_base: i32 = switch (list_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        else => unreachable,
                    };

                    const ls = self.layout_store orelse unreachable;
                    const ret_layout_val = ls.getLayout(ll.ret_layout);

                    // Determine element layout: extract from the list arg's layout
                    // (not from ret_layout, which may be a Try tag union)
                    const elem_layout_idx: layout.Idx = blk: {
                        // Try to get the list layout from the first argument
                        if (self.getExprLayout(args[0])) |list_layout_idx| {
                            const list_layout_val = ls.getLayout(list_layout_idx);
                            if (list_layout_val.tag == .list) {
                                break :blk list_layout_val.data.list;
                            }
                        }
                        // Fallback: if ret_layout is a tag_union (Try), extract
                        // the Ok variant's payload layout as element layout
                        if (ret_layout_val.tag == .tag_union) {
                            const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
                            const variants = ls.getTagUnionVariants(tu_data);
                            // Ok is discriminant 1 (tags sorted alphabetically: Err=0, Ok=1)
                            if (variants.len > 1) {
                                break :blk variants.get(1).payload_layout;
                            }
                        }
                        // Last resort: use ret_layout directly (for list_get_unsafe)
                        break :blk ll.ret_layout;
                    };
                    const elem_layout_val = ls.getLayout(elem_layout_idx);
                    const elem_size: u32 = ls.layoutSizeAlign(elem_layout_val).size;

                    // Check if this is a safe List.get (ret_layout is a tag_union)
                    const is_safe_get = ret_layout_val.tag == .tag_union;

                    if (elem_size == 0 and !is_safe_get) {
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

                    if (is_safe_get) {
                        // Safe List.get: bounds check + Try result construction
                        const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
                        const tag_size = tu_data.size;
                        const disc_offset = tu_data.discriminant_offset;
                        const disc_size = tu_data.discriminant_size;

                        // Allocate result slot for the tag union
                        const result_slot = self.codegen.allocStackSlot(tag_size);
                        try self.zeroStackArea(result_slot, tag_size);

                        // Load list length (offset 8 in list struct)
                        const len_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadStack(.w64, len_reg, list_base + 8);

                        // Compare: index < length (unsigned)
                        try self.emitCmpRegReg(index_reg, len_reg);
                        self.codegen.freeGeneral(len_reg);

                        // Jump to else (Err) if index >= length (unsigned compare)
                        // aarch64: cs = carry set = unsigned >=
                        // x86_64: above_or_equal = unsigned >=
                        const unsigned_ge = if (comptime target.toCpuArch() == .aarch64) .cs else .above_or_equal;
                        const else_patch = try self.codegen.emitCondJump(unsigned_ge);

                        // === THEN branch: Ok(element) ===
                        // Load list pointer
                        const ptr_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadStack(.w64, ptr_reg, list_base);

                        // Calculate element address: ptr + index * elem_size
                        if (elem_size == 0) {
                            // ZST element - nothing to load, just store discriminant
                            self.codegen.freeGeneral(ptr_reg);
                        } else {
                            const addr_reg = try self.allocTempGeneral();
                            try self.codegen.emit.movRegReg(.w64, addr_reg, index_reg);

                            if (elem_size != 1) {
                                const size_reg = try self.allocTempGeneral();
                                try self.codegen.emitLoadImm(size_reg, elem_size);
                                try self.emitMulRegs(.w64, addr_reg, addr_reg, size_reg);
                                self.codegen.freeGeneral(size_reg);
                            }

                            try self.emitAddRegs(.w64, addr_reg, addr_reg, ptr_reg);
                            self.codegen.freeGeneral(ptr_reg);

                            // Copy element into payload area of tag union (at result_slot + 0)
                            const temp_reg = try self.allocTempGeneral();
                            try self.copyChunked(temp_reg, addr_reg, 0, frame_ptr, result_slot, elem_size);
                            self.codegen.freeGeneral(temp_reg);
                            self.codegen.freeGeneral(addr_reg);
                        }

                        // Store Ok discriminant (1)
                        try self.storeDiscriminant(result_slot + @as(i32, @intCast(disc_offset)), 1, disc_size);

                        // Jump to end
                        const end_patch = try self.codegen.emitJump();

                        // === ELSE branch: Err(OutOfBounds) ===
                        self.codegen.patchJump(else_patch, self.codegen.currentOffset());
                        // Result is already zeroed (payload = 0 for Err).
                        // Store Err discriminant (0) — already 0 from zeroing, but be explicit
                        try self.storeDiscriminant(result_slot + @as(i32, @intCast(disc_offset)), 0, disc_size);

                        // === END ===
                        self.codegen.patchJump(end_patch, self.codegen.currentOffset());

                        self.codegen.freeGeneral(index_reg);
                        return .{ .stack = .{ .offset = result_slot } };
                    }

                    // Unsafe list_get: no bounds checking, return bare element
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

                    // Return with appropriate value location based on element type
                    if (elem_layout_idx == .i128 or elem_layout_idx == .u128 or elem_layout_idx == .dec) {
                        return .{ .stack_i128 = elem_slot };
                    } else if (elem_layout_idx == .str) {
                        return .{ .stack_str = elem_slot };
                    } else if (elem_layout_val.tag == .list or elem_layout_val.tag == .list_of_zst) {
                        return .{ .list_stack = .{
                            .struct_offset = elem_slot,
                            .data_offset = 0,
                            .num_elements = 0,
                        } };
                    } else {
                        return .{ .stack = .{ .offset = elem_slot } };
                    }
                },
                .list_concat => {
                    // list_concat(list_a, list_b) -> List
                    if (args.len != 2) unreachable;
                    const list_a_loc = try self.generateExpr(args[0]);
                    const list_b_loc = try self.generateExpr(args[1]);

                    const ls = self.layout_store orelse unreachable;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const elem_size_align: layout.SizeAlign = blk: {
                        const ret_layout = ls.getLayout(ll.ret_layout);
                        break :blk switch (ret_layout.tag) {
                            .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                            .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                            else => unreachable,
                        };
                    };

                    const list_a_off = try self.ensureOnStack(list_a_loc, roc_list_size);
                    const list_b_off = try self.ensureOnStack(list_b_loc, roc_list_size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();
                    const fn_addr: usize = @intFromPtr(&wrapListConcat);

                    {
                        // wrapListConcat(out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, alignment, element_width, roc_ops)
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

                    const ls = self.layout_store orelse unreachable;
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
                    const elem_off = try self.ensureOnStack(elem_loc, elem_size_align.size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();
                    const fn_addr: usize = @intFromPtr(&wrapListPrepend);

                    {
                        // wrapListPrepend(out, list_bytes, list_len, list_cap, alignment, element, element_width, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_off);
                        try builder.addMemArg(base_reg, list_off + 8);
                        try builder.addMemArg(base_reg, list_off + 16);
                        try builder.addImmArg(@intCast(alignment_bytes));
                        try builder.addLeaArg(base_reg, elem_off);
                        try builder.addImmArg(@intCast(elem_size_align.size));
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
                .list_repeat => {
                    // list_repeat(element, count) -> List
                    // Implementation: create list with capacity, then append the element count times
                    if (args.len != 2) unreachable;
                    const elem_loc = try self.generateExpr(args[0]);
                    const count_loc = try self.generateExpr(args[1]);

                    const ls = self.layout_store orelse unreachable;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const elem_size_align: layout.SizeAlign = blk: {
                        const ret_layout = ls.getLayout(ll.ret_layout);
                        break :blk switch (ret_layout.tag) {
                            .list => ls.layoutSizeAlign(ls.getLayout(ret_layout.data.list)),
                            .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                            else => unreachable,
                        };
                    };

                    // Materialize count to a register then save to stack slot
                    const count_reg = try self.ensureInGeneralReg(count_loc);
                    const count_slot = self.codegen.allocStackSlot(8);
                    try self.emitStore(.w64, frame_ptr, count_slot, count_reg);
                    self.codegen.freeGeneral(count_reg);

                    // Ensure element is on the stack
                    const elem_off = try self.ensureOnStack(elem_loc, elem_size_align.size);

                    // First: allocate list with capacity
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const cap_fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_with_capacity);
                    const base_reg = frame_ptr;

                    {
                        // roc_builtins_list_with_capacity(out, capacity, alignment, element_width, roc_ops)
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, count_slot);
                        try builder.addImmArg(@intCast(alignment_bytes));
                        try builder.addImmArg(@intCast(elem_size_align.size));
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, cap_fn_addr, .list_with_capacity);
                    }

                    // Now loop: append element count times using counter on stack
                    const loop_counter_slot = self.codegen.allocStackSlot(8);
                    const temp_init = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp_init, 0);
                    try self.emitStore(.w64, frame_ptr, loop_counter_slot, temp_init);
                    self.codegen.freeGeneral(temp_init);

                    // Loop start: load counter, compare with count, branch if done
                    const loop_start = self.codegen.currentOffset();

                    const ctr_reg2 = try self.allocTempGeneral();
                    const cnt_reg2 = try self.allocTempGeneral();
                    try self.emitLoad(.w64, ctr_reg2, frame_ptr, loop_counter_slot);
                    try self.emitLoad(.w64, cnt_reg2, frame_ptr, count_slot);
                    try self.codegen.emit.cmpRegReg(.w64, ctr_reg2, cnt_reg2);
                    self.codegen.freeGeneral(cnt_reg2);

                    // Branch to end if counter >= count
                    const skip_patch = try self.codegen.emitCondJump(condGreaterOrEqual());

                    // Increment counter before the call (so it survives the call)
                    try self.emitAddImm(ctr_reg2, ctr_reg2, 1);
                    try self.emitStore(.w64, frame_ptr, loop_counter_slot, ctr_reg2);
                    self.codegen.freeGeneral(ctr_reg2);

                    // Call listAppendUnsafeC (capacity is already reserved)
                    const tmp_result = self.codegen.allocStackSlot(roc_str_size);
                    const append_fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_append_unsafe);

                    {
                        // roc_builtins_list_append_unsafe(out, list_bytes, list_len, list_cap, element, element_width, roc_ops)
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, tmp_result);
                        try builder.addMemArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, result_offset + 8);
                        try builder.addMemArg(base_reg, result_offset + 16);
                        try builder.addLeaArg(base_reg, elem_off);
                        try builder.addImmArg(@intCast(elem_size_align.size));
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, append_fn_addr, .list_append_unsafe);
                    }

                    // Copy tmp_result back to result_offset
                    const cp = try self.allocTempGeneral();
                    try self.emitLoad(.w64, cp, frame_ptr, tmp_result);
                    try self.emitStore(.w64, frame_ptr, result_offset, cp);
                    try self.emitLoad(.w64, cp, frame_ptr, tmp_result + 8);
                    try self.emitStore(.w64, frame_ptr, result_offset + 8, cp);
                    try self.emitLoad(.w64, cp, frame_ptr, tmp_result + 16);
                    try self.emitStore(.w64, frame_ptr, result_offset + 16, cp);
                    self.codegen.freeGeneral(cp);

                    // Jump back to loop start
                    const back_patch = try self.codegen.emitJump();
                    self.codegen.patchJump(back_patch, loop_start);

                    // Patch the forward branch to here
                    self.codegen.patchJump(skip_patch, self.codegen.currentOffset());

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
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
                .str_is_empty => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1ToScalar(str_off, @intFromPtr(&wrapStrIsEmpty), .str_is_empty);
                },
                .str_is_eq => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.generateExpr(args[0]);
                    const b_loc = try self.generateExpr(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrEqual), .str_equal);
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
                .str_split => {
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
                .str_with_prefix => {
                    // str_with_prefix(string, prefix) -> Str  (= concat(prefix, string))
                    if (args.len != 2) unreachable;
                    const str_loc = try self.generateExpr(args[0]);
                    const pfx_loc = try self.generateExpr(args[1]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    const pfx_off = try self.ensureOnStack(pfx_loc, roc_str_size);
                    return try self.callStr2RocOpsToResult(str_off, pfx_off, @intFromPtr(&wrapStrWithPrefix), .str_with_prefix, .str);
                },
                .str_from_utf8_lossy => {
                    // str_from_utf8_lossy(list) -> Str
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    return try self.callStr1RocOpsToResult(list_off, @intFromPtr(&wrapStrFromUtf8Lossy), .str_from_utf8_lossy, .str);
                },
                .str_from_utf8 => {
                    // str_from_utf8(list) -> FromUtf8Try {byte_index: u64, string: RocStr, is_ok: bool, problem_code: u8}
                    if (args.len != 1) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);

                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                    const result_size: i32 = @intCast(@sizeOf(FromUtf8Try));
                    const result_offset = self.codegen.allocStackSlot(result_size);

                    // fn(out, list_bytes, list_len, list_cap, roc_ops) -> void
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(frame_ptr, result_offset);
                    try builder.addMemArg(frame_ptr, list_off);
                    try builder.addMemArg(frame_ptr, list_off + 8);
                    try builder.addMemArg(frame_ptr, list_off + 16);
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, @intFromPtr(&wrapStrFromUtf8), .str_from_utf8);

                    return .{ .stack = .{ .offset = result_offset } };
                },

                // ── Remaining list low-level operations ──

                .list_set => {
                    // list_set(list, index, element) -> List
                    if (args.len != 3) unreachable;
                    const list_loc = try self.generateExpr(args[0]);
                    const index_loc = try self.generateExpr(args[1]);
                    const elem_loc = try self.generateExpr(args[2]);

                    const ls = self.layout_store orelse unreachable;
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
                    const index_off = try self.ensureOnStack(index_loc, 8);
                    const elem_off = try self.ensureOnStack(elem_loc, elem_size_align.size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    // We need a scratch slot for the old element (out_element param)
                    const old_elem_slot = self.codegen.allocStackSlot(@intCast(if (elem_size_align.size > 0) elem_size_align.size else 8));
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();
                    const fn_addr: usize = @intFromPtr(&wrapListReplace);

                    {
                        // wrapListReplace(out, list_bytes, list_len, list_cap, alignment, index, element, element_width, out_element, roc_ops)
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
                    const ls = self.layout_store orelse unreachable;
                    const list_layout_idx = self.getExprLayout(args[0]) orelse unreachable;
                    const list_layout = ls.getLayout(list_layout_idx);
                    switch (list_layout.tag) {
                        .list => {
                            const list_loc = try self.generateExpr(args[0]);
                            const needle_loc = try self.generateExpr(args[1]);
                            return try self.generateListContains(list_loc, needle_loc, list_layout.data.list);
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
                .num_add,
                .num_sub,
                .num_mul,
                .num_div,
                .num_mod,
                .num_neg,
                .num_abs,
                .num_pow,
                .num_sqrt,
                .num_log,
                .num_round,
                .num_floor,
                .num_ceiling,
                .num_to_str,
                .num_from_numeral,
                .num_is_zero,
                .num_abs_diff,
                .num_shift_left_by,
                .num_shift_right_by,
                .num_shift_right_zf_by,
                .str_inspekt,
                .list_sort_with,
                .list_drop_at,
                .list_sublist,
                .compare,
                => {
                    if (std.debug.runtime_safety) unreachable;
                    unreachable;
                },
                .box_box,
                .box_unbox,
                => {
                    unreachable;
                },
                .crash => {
                    // Runtime crash: call roc_crashed via RocOps.
                    // TODO: Pass the user's crash message string from the args
                    // instead of this static message.
                    try self.emitRocCrash("Roc crashed");
                    try self.emitTrap();
                    return .noreturn;
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

        /// Call a C wrapper: fn(a_f0, a_f1, a_f2, b_f0, b_f1, b_f2) -> scalar
        /// Used for (str, str) -> bool ops
        fn callStr2ToScalar(self: *Self, a_off: i32, b_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            // fn(a_bytes, a_len, a_cap, b_bytes, b_len, b_cap) -> scalar - 6 args
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
            const ls = self.layout_store orelse unreachable;
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

            // Call wrapListSublist(out, list_bytes, list_len, list_cap, alignment, element_width, start, len, roc_ops)
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const alignment_bytes = elem_size_align.alignment.toByteUnits();
            const fn_addr: usize = @intFromPtr(&wrapListSublist);

            {
                // wrapListSublist(out, list_bytes, list_len, list_cap, alignment, element_width, start, len, roc_ops)
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
                try builder.addRegArg(roc_ops_reg);

                try self.callBuiltin(&builder, fn_addr, .list_sublist);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Helper for list_split_first and list_split_last.
        /// Returns a record {element, List} with fields at layout-determined offsets.
        fn callListSplitOp(self: *Self, ll: anytype, list_loc: ValueLocation, mode: enum { first, last }) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            // Get the return record layout
            const ret_layout = ls.getLayout(ll.ret_layout);
            if (ret_layout.tag != .record) unreachable;
            const record_idx = ret_layout.data.record.idx;
            const record_data = ls.getRecordData(record_idx);
            const result_size: u32 = record_data.size;

            // Find which field is the list and which is the element.
            // The record has exactly 2 fields.
            const field0_layout_idx = ls.getRecordFieldLayout(record_idx, 0);
            const field0_layout = ls.getLayout(field0_layout_idx);
            const field0_offset: i32 = @intCast(ls.getRecordFieldOffset(record_idx, 0));
            const field1_layout_idx = ls.getRecordFieldLayout(record_idx, 1);
            const field1_layout = ls.getLayout(field1_layout_idx);
            const field1_offset: i32 = @intCast(ls.getRecordFieldOffset(record_idx, 1));

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
            }

            // Build rest list via wrapListSublist, writing directly into result+list_field_offset
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
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, @intFromPtr(&wrapListSublist), .list_sublist);
            }

            // Return the record as a stack value
            return .{ .stack = .{ .offset = result_offset } };
        }

        /// Get element at constant index 0 from a list
        fn listGetAtConstIndex(self: *Self, list_loc: ValueLocation, index: u64, ret_layout_idx: layout.Idx) Allocator.Error!ValueLocation {
            const list_base: i32 = switch (list_loc) {
                .stack => |s| s.offset,
                .list_stack => |ls_info| ls_info.struct_offset,
                else => unreachable,
            };

            const ls = self.layout_store orelse unreachable;
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

            if (ret_layout_idx == .i128 or ret_layout_idx == .u128 or ret_layout_idx == .dec) {
                return .{ .stack_i128 = elem_slot };
            } else if (ret_layout_idx == .str) {
                return .{ .stack_str = elem_slot };
            } else if (ret_layout_val.tag == .list or ret_layout_val.tag == .list_of_zst) {
                return .{ .list_stack = .{ .struct_offset = elem_slot, .data_offset = 0, .num_elements = 0 } };
            } else {
                return .{ .stack = .{ .offset = elem_slot } };
            }
        }

        /// Get element at last index (len - 1) from a list
        fn listGetAtLastIndex(self: *Self, list_loc: ValueLocation, ret_layout_idx: layout.Idx) Allocator.Error!ValueLocation {
            const list_base: i32 = switch (list_loc) {
                .stack => |s| s.offset,
                .list_stack => |ls_info| ls_info.struct_offset,
                else => unreachable,
            };

            const ls = self.layout_store orelse unreachable;
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

            if (ret_layout_idx == .i128 or ret_layout_idx == .u128 or ret_layout_idx == .dec) {
                return .{ .stack_i128 = elem_slot };
            } else if (ret_layout_idx == .str) {
                return .{ .stack_str = elem_slot };
            } else if (ret_layout_val.tag == .list or ret_layout_val.tag == .list_of_zst) {
                return .{ .list_stack = .{ .struct_offset = elem_slot, .data_offset = 0, .num_elements = 0 } };
            } else {
                return .{ .stack = .{ .offset = elem_slot } };
            }
        }

        /// Generate code for hosted function calls (platform-provided effects).
        /// Hosted functions follow the RocCall ABI: fn(roc_ops, ret_ptr, args_ptr) -> void
        fn generateHostedCall(self: *Self, hc: anytype) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            const ls = self.layout_store orelse unreachable;

            // Get the arguments
            const args = self.store.getExprSpan(hc.args);

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
            var max_alignment: usize = 1;
            for (args) |arg_id| {
                const maybe_layout = self.getExprLayout(arg_id);
                if (maybe_layout) |arg_layout_idx| {
                    const arg_layout = ls.getLayout(arg_layout_idx);
                    const arg_size = ls.layoutSize(arg_layout);
                    const arg_align = arg_layout.alignment(ls.targetUsize());
                    max_alignment = @max(max_alignment, arg_align.toByteUnits());
                    // Align to the argument's alignment
                    total_args_size = std.mem.alignForward(usize, total_args_size, arg_align.toByteUnits());
                    total_args_size += arg_size;
                }
            }

            // Allocate args buffer (at least 8 bytes for empty args case)
            const args_slot = self.codegen.allocStackSlot(@intCast(@max(total_args_size, 8)));

            // Copy each argument into the args buffer
            var offset: usize = 0;
            for (args) |arg_id| {
                const arg_loc = try self.generateExpr(arg_id);
                if (self.getExprLayout(arg_id)) |arg_layout_idx| {
                    const arg_layout = ls.getLayout(arg_layout_idx);
                    const arg_size = ls.layoutSize(arg_layout);
                    const arg_align = arg_layout.alignment(ls.targetUsize());
                    // Align offset
                    offset = std.mem.alignForward(usize, offset, arg_align.toByteUnits());

                    if (arg_size > 0) {
                        // Copy argument to args buffer at current offset
                        const dest_offset: i32 = args_slot + @as(i32, @intCast(offset));
                        try self.copyValueToStack(arg_loc, dest_offset, arg_size);
                    }
                    offset += arg_size;
                }
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
        fn generateListContains(self: *Self, list_loc: ValueLocation, needle_loc: ValueLocation, elem_layout_idx: layout.Idx) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;
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
            const needle_slot = try self.ensureOnStack(needle_loc, elem_size);

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
            const ls = self.layout_store orelse unreachable;
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

            {
                // roc_builtins_list_with_capacity(out, capacity, alignment, element_width, roc_ops)
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off + 8); // capacity = input length
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addImmArg(@intCast(elem_size_align.size));
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
            const ls = self.layout_store orelse unreachable;
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
            const spare_off = try self.ensureOnStack(spare_loc, 8);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const alignment_bytes = elem_size_align.alignment.toByteUnits();
            const fn_addr: usize = @intFromPtr(&wrapListReserve);

            {
                // wrapListReserve(out, list_bytes, list_len, list_cap, alignment, spare, element_width, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addMemArg(base_reg, spare_off);
                try builder.addImmArg(@intCast(elem_size_align.size));
                try builder.addRegArg(roc_ops_reg);

                try self.callBuiltin(&builder, fn_addr, .list_reserve);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Call list_release_excess_capacity wrapper
        fn callListReleaseExcessCapOp(self: *Self, list_loc: ValueLocation, ll: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;
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
            const fn_addr: usize = @intFromPtr(&wrapListReleaseExcessCapacity);

            {
                // wrapListReleaseExcessCapacity(out, list_bytes, list_len, list_cap, alignment, element_width, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(alignment_bytes));
                try builder.addImmArg(@intCast(elem_size_align.size));
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
        fn generateLookup(self: *Self, symbol: Symbol, _: layout.Idx) Allocator.Error!ValueLocation {
            // Check if we have a location for this symbol
            const symbol_key: u64 = @bitCast(symbol);
            if (self.symbol_locations.get(symbol_key)) |loc| {
                return loc;
            }

            // Symbol not found - it might be a top-level definition
            if (self.store.getSymbolDef(symbol)) |def_expr_id| {
                const def_expr = self.store.getExpr(def_expr_id);

                // For closures, compile the lambda as a proc and pre-register as lambda_code.
                // This breaks recursive capture cycles: if a closure captures itself (recursive
                // function), the self-reference is found in symbol_locations on the second lookup
                // instead of triggering infinite recursion through materializeCaptures.
                if (def_expr == .closure) {
                    const closure = self.store.getClosureData(def_expr.closure);
                    const inner = self.store.getExpr(closure.lambda);
                    if (inner == .lambda) {
                        const code_offset = try self.compileLambdaAsProc(closure.lambda, inner.lambda);
                        const lambda_loc: ValueLocation = .{ .lambda_code = .{
                            .code_offset = code_offset,
                            .ret_layout = inner.lambda.ret_layout,
                        } };
                        try self.symbol_locations.put(symbol_key, lambda_loc);
                        return lambda_loc;
                    }
                }

                // Generate code for the definition
                const loc = try self.generateExpr(def_expr_id);
                // Cache the location
                try self.symbol_locations.put(symbol_key, loc);
                return loc;
            }

            unreachable;
        }

        /// Generate code for a binary operation
        fn generateBinop(self: *Self, binop: anytype) Allocator.Error!ValueLocation {
            // Generate code for LHS first (always stable due to generateExpr wrapper)
            const lhs_loc = try self.generateExpr(binop.lhs);

            // Evaluate RHS (safe — LHS is in a stable location, never a bare register)
            const rhs_loc = try self.generateExpr(binop.rhs);

            // Check if this is a structural comparison (records/tuples/lists)
            // We need to check the layout, not the expression type, since the LHS
            // might be a function call that returns a record/tuple/list
            if (binop.op == .eq or binop.op == .neq) {
                const lhs_expr = self.store.getExpr(binop.lhs);
                const rhs_expr = self.store.getExpr(binop.rhs);

                // First try expression-based detection for direct literals (on either side).
                // Use layout-aware comparison when layout_store is available, since
                // bytewise comparison is incorrect for heap-allocated fields (strings, lists).
                if (lhs_expr == .record) {
                    if (self.layout_store != null) {
                        return self.generateRecordComparisonByLayout(lhs_loc, rhs_loc, lhs_expr.record.record_layout, binop.op);
                    }
                    return self.generateStructuralComparison(lhs_loc, rhs_loc, lhs_expr, binop.op);
                }
                if (rhs_expr == .record) {
                    if (self.layout_store != null) {
                        return self.generateRecordComparisonByLayout(lhs_loc, rhs_loc, rhs_expr.record.record_layout, binop.op);
                    }
                    return self.generateStructuralComparison(lhs_loc, rhs_loc, rhs_expr, binop.op);
                }
                if (lhs_expr == .tuple) {
                    if (self.layout_store != null) {
                        return self.generateTupleComparisonByLayout(lhs_loc, rhs_loc, lhs_expr.tuple.tuple_layout, binop.op);
                    }
                    return self.generateStructuralComparison(lhs_loc, rhs_loc, lhs_expr, binop.op);
                }
                if (rhs_expr == .tuple) {
                    if (self.layout_store != null) {
                        return self.generateTupleComparisonByLayout(lhs_loc, rhs_loc, rhs_expr.tuple.tuple_layout, binop.op);
                    }
                    return self.generateStructuralComparison(lhs_loc, rhs_loc, rhs_expr, binop.op);
                }
                if (lhs_expr == .list) {
                    return self.generateListComparison(lhs_loc, rhs_loc, lhs_expr, binop.op);
                }
                if (rhs_expr == .list) {
                    return self.generateListComparison(lhs_loc, rhs_loc, rhs_expr, binop.op);
                }
                // Tag union literals - use layout-based comparison only for
                // actual tag_union layouts (not scalar enums which compare as integers)
                if (lhs_expr == .tag or lhs_expr == .zero_arg_tag) {
                    if (self.layout_store) |ls| {
                        const tu_layout = switch (lhs_expr) {
                            .tag => |t| t.union_layout,
                            .zero_arg_tag => |t| t.union_layout,
                            else => unreachable,
                        };
                        if (ls.getLayout(tu_layout).tag == .tag_union) {
                            return self.generateTagUnionComparisonByLayout(lhs_loc, rhs_loc, tu_layout, binop.op);
                        }
                    }
                }
                if (rhs_expr == .tag or rhs_expr == .zero_arg_tag) {
                    if (self.layout_store) |ls| {
                        const tu_layout = switch (rhs_expr) {
                            .tag => |t| t.union_layout,
                            .zero_arg_tag => |t| t.union_layout,
                            else => unreachable,
                        };
                        if (ls.getLayout(tu_layout).tag == .tag_union) {
                            return self.generateTagUnionComparisonByLayout(lhs_loc, rhs_loc, tu_layout, binop.op);
                        }
                    }
                }

                // For calls/lookups/blocks, check the layout to detect composite types
                if (self.layout_store) |ls| {
                    // Try to get layout from LHS first, then RHS
                    const operand_layout: ?layout.Idx = switch (lhs_expr) {
                        .call => |call| call.ret_layout,
                        .lookup => |lookup| lookup.layout_idx,
                        .block => |block| block.result_layout,
                        else => switch (rhs_expr) {
                            .call => |call| call.ret_layout,
                            .lookup => |lookup| lookup.layout_idx,
                            .block => |block| block.result_layout,
                            else => null,
                        },
                    };

                    if (operand_layout) |op_layout| {
                        // Always check the layout tag to detect composite types
                        const stored_layout = ls.getLayout(op_layout);
                        if (stored_layout.tag == .record) {
                            return self.generateRecordComparisonByLayout(lhs_loc, rhs_loc, op_layout, binop.op);
                        } else if (stored_layout.tag == .tuple) {
                            return self.generateTupleComparisonByLayout(lhs_loc, rhs_loc, op_layout, binop.op);
                        } else if (stored_layout.tag == .list) {
                            return self.generateListComparisonByLayout(lhs_loc, rhs_loc, op_layout, binop.op);
                        } else if (stored_layout.tag == .tag_union) {
                            return self.generateTagUnionComparisonByLayout(lhs_loc, rhs_loc, op_layout, binop.op);
                        }
                    }
                }
            }

            // Check if operands are i128/Dec (need special handling even for comparisons that return bool)
            const operands_are_i128 = switch (lhs_loc) {
                .immediate_i128, .stack_i128 => true,
                else => switch (rhs_loc) {
                    .immediate_i128, .stack_i128 => true,
                    else => false,
                },
            };

            // Determine if this is an integer or float operation.
            // Use operand_layout which reliably carries the operand type,
            // even for comparisons where result_layout is .bool.
            const is_float = binop.operand_layout == .f32 or binop.operand_layout == .f64;

            if (is_float) {
                return self.generateFloatBinop(binop.op, lhs_loc, rhs_loc);
            } else if (operands_are_i128 or binop.operand_layout == .i128 or binop.operand_layout == .u128 or binop.operand_layout == .dec) {
                // Use i128 path for Dec/i128 operands (even for comparisons that return bool)
                // Convert .stack locations to .stack_i128 for Dec operations, since Dec values are 16 bytes
                // but may be stored with .stack location type (e.g., mutable variables)
                const is_i128_op = binop.operand_layout == .dec or binop.operand_layout == .i128 or binop.operand_layout == .u128;
                const adj_lhs = if (is_i128_op and lhs_loc == .stack) ValueLocation{ .stack_i128 = lhs_loc.stack.offset } else lhs_loc;
                const adj_rhs = if (is_i128_op and rhs_loc == .stack) ValueLocation{ .stack_i128 = rhs_loc.stack.offset } else rhs_loc;
                return self.generateI128Binop(binop.op, adj_lhs, adj_rhs, binop.operand_layout);
            } else {
                return self.generateIntBinop(binop.op, lhs_loc, rhs_loc, binop.operand_layout);
            }
        }

        /// Generate integer binary operation
        fn generateIntBinop(
            self: *Self,
            op: LirExpr.BinOp,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            operand_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            // Load operands into registers
            const rhs_reg = try self.ensureInGeneralReg(rhs_loc);
            const lhs_reg = try self.ensureInGeneralReg(lhs_loc);

            // Allocate result register
            const result_reg = try self.allocTempGeneral();

            // Determine if this is an unsigned type (for division/modulo/comparisons)
            const is_unsigned = switch (operand_layout) {
                layout.Idx.u8, layout.Idx.u16, layout.Idx.u32, layout.Idx.u64, layout.Idx.u128 => true,
                else => false,
            };

            switch (op) {
                .add => try self.codegen.emitAdd(.w64, result_reg, lhs_reg, rhs_reg),
                .sub => try self.codegen.emitSub(.w64, result_reg, lhs_reg, rhs_reg),
                .mul => try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg),
                .div, .div_trunc => {
                    // For integers, div and div_trunc are the same (integer division truncates)
                    if (is_unsigned) {
                        try self.codegen.emitUDiv(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSDiv(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                },
                .rem, .mod => {
                    // For integers: rem and mod differ for negative dividends.
                    // Roc's Num module uses unsigned types, so they're equivalent here.
                    if (is_unsigned) {
                        try self.codegen.emitUMod(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSMod(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                },
                .shl => try self.emitShlReg(.w64, result_reg, lhs_reg, rhs_reg),
                .shr => try self.emitAsrReg(.w64, result_reg, lhs_reg, rhs_reg),
                .shr_zf => try self.emitLsrReg(.w64, result_reg, lhs_reg, rhs_reg),
                // Comparison operations
                .eq => {
                    try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condEqual());
                },
                .neq => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condNotEqual()),
                .lt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, if (is_unsigned) condBelow() else condLess()),
                .lte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, if (is_unsigned) condBelowOrEqual() else condLessOrEqual()),
                .gt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, if (is_unsigned) condAbove() else condGreater()),
                .gte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, if (is_unsigned) condAboveOrEqual() else condGreaterOrEqual()),
                // Boolean operations - AND/OR two values
                // Boolean values in Roc are represented as 0 (false) or 1 (true).
                // Bitwise AND/OR work correctly for single-bit boolean values.
                .@"and" => try self.codegen.emitAnd(.w64, result_reg, lhs_reg, rhs_reg),
                .@"or" => try self.codegen.emitOr(.w64, result_reg, lhs_reg, rhs_reg),
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
            op: LirExpr.BinOp,
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
                .add => {
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
                .sub => {
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
                .mul => {
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
                            // IMPORTANT: MUL clobbers both RAX and RDX. We do 3 MUL operations.
                            // All input/output registers that are RAX or RDX will be clobbered!
                            //
                            // Usage pattern:
                            // Step 1: lhs_parts.low, rhs_parts.low -> clobbers RAX, RDX
                            // Step 2: lhs_parts.low, rhs_parts.high -> clobbers RAX, RDX
                            // Step 3: lhs_parts.high, rhs_parts.low -> clobbers RAX, RDX
                            //
                            // We must save any input in RAX/RDX before they get clobbered.

                            // Mark RAX and RDX as in-use so allocGeneralFor won't return them
                            self.codegen.markRegisterInUse(.RAX);
                            self.codegen.markRegisterInUse(.RDX);

                            // Allocate temp registers for accumulation (guaranteed not RAX/RDX)
                            const temp_low = try self.codegen.allocGeneralFor(0xFFFE);
                            const temp_high = try self.codegen.allocGeneralFor(0xFFFF);

                            // Helper to save a register if it's RAX or RDX
                            const SavedReg = struct {
                                reg: GeneralReg,
                                needs_free: bool,
                            };

                            const saveIfClobbered = struct {
                                fn f(s: *Self, reg: GeneralReg) !SavedReg {
                                    if (reg == .RAX or reg == .RDX) {
                                        const saved = try s.codegen.allocGeneralFor(0xFFFC);
                                        try s.codegen.emit.movRegReg(.w64, saved, reg);
                                        return .{ .reg = saved, .needs_free = true };
                                    }
                                    return .{ .reg = reg, .needs_free = false };
                                }
                            }.f;

                            // Save all inputs that are in RAX/RDX
                            // lhs_parts.low: used in steps 1, 2
                            const lhs_low = try saveIfClobbered(self, lhs_parts.low);
                            // lhs_parts.high: used in step 3
                            const lhs_high = try saveIfClobbered(self, lhs_parts.high);
                            // rhs_parts.low: used in steps 1, 3
                            const rhs_low = try saveIfClobbered(self, rhs_parts.low);
                            // rhs_parts.high: used in step 2
                            const rhs_high = try saveIfClobbered(self, rhs_parts.high);

                            // Restore RAX/RDX to free pool (MUL will use them)
                            self.codegen.freeGeneral(.RAX);
                            self.codegen.freeGeneral(.RDX);

                            // 1. a_lo * b_lo -> RAX (low), RDX (high)
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_low.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_low.reg);
                            try self.codegen.emit.movRegReg(.w64, temp_low, .RAX);
                            try self.codegen.emit.movRegReg(.w64, temp_high, .RDX);

                            // 2. a_lo * b_hi -> add low part to temp_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_low.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_high.reg);
                            try self.codegen.emit.addRegReg(.w64, temp_high, .RAX);

                            // 3. a_hi * b_lo -> add low part to temp_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_high.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_low.reg);
                            try self.codegen.emit.addRegReg(.w64, temp_high, .RAX);

                            // Move results to actual output registers
                            try self.codegen.emit.movRegReg(.w64, result_low, temp_low);
                            try self.codegen.emit.movRegReg(.w64, result_high, temp_high);

                            // Cleanup temp registers
                            self.codegen.freeGeneral(temp_low);
                            self.codegen.freeGeneral(temp_high);
                            if (lhs_low.needs_free) self.codegen.freeGeneral(lhs_low.reg);
                            if (lhs_high.needs_free) self.codegen.freeGeneral(lhs_high.reg);
                            if (rhs_low.needs_free) self.codegen.freeGeneral(rhs_low.reg);
                            if (rhs_high.needs_free) self.codegen.freeGeneral(rhs_high.reg);
                        }
                    }
                },
                .div => {
                    if (operand_layout == .dec) {
                        // Dec division: call builtin function
                        // divC(RocDec, RocDec, *RocOps) -> i128
                        try self.callDecDiv(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit integer division: call builtin function
                        try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, false);
                    }
                },
                .div_trunc => {
                    if (operand_layout == .dec) {
                        // Dec truncating division: divide and truncate to whole number
                        // divTruncC(RocDec, RocDec, *RocOps) -> i128
                        try self.callDecDivTrunc(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit integer truncating division: same as regular i128 div
                        try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, false);
                    }
                },
                .rem, .mod => {
                    // 128-bit integer remainder/modulo: call builtin function
                    try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, true);
                },
                // Comparison operations for i128/Dec
                .eq, .neq => {
                    // Compare both low and high parts
                    const result_reg = try self.allocTempGeneral();
                    try self.generateI128Equality(lhs_parts, rhs_parts, result_reg, op == .eq);

                    // Free the extra result_high we allocated
                    self.codegen.freeGeneral(result_high);
                    self.codegen.freeGeneral(result_low);
                    self.codegen.freeGeneral(lhs_parts.low);
                    self.codegen.freeGeneral(lhs_parts.high);
                    self.codegen.freeGeneral(rhs_parts.low);
                    self.codegen.freeGeneral(rhs_parts.high);

                    return .{ .general_reg = result_reg };
                },
                .lt, .lte, .gt, .gte => {
                    // i128 comparison: compare high parts first, if equal compare low parts
                    const result_reg = try self.allocTempGeneral();
                    try self.generateI128Comparison(lhs_parts, rhs_parts, result_reg, op, is_unsigned);

                    // Free the extra result_high we allocated
                    self.codegen.freeGeneral(result_high);
                    self.codegen.freeGeneral(result_low);
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

            const ls = self.layout_store orelse unreachable;
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

            const ls = self.layout_store orelse unreachable;
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            std.debug.assert(ret_layout_val.tag == .tag_union);
            const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
            const result_offset = self.codegen.allocStackSlot(tu_data.size);
            try self.zeroStackArea(result_offset, tu_data.size);
            const disc_offset: u32 = tu_data.discriminant_offset;

            // Find the Ok variant's payload layout to determine the target numeric type.
            // Tags are sorted alphabetically: Err=0, Ok=1
            const variants = ls.getTagUnionVariants(tu_data);
            const payload_idx = if (variants.len > 1) variants.get(1).payload_layout else variants.get(0).payload_layout;

            // Determine integer width and signedness from the layout index
            const int_width: u8 = switch (payload_idx) {
                .u8, .i8 => 1,
                .u16, .i16 => 2,
                .u32, .i32 => 4,
                .u64, .i64 => 8,
                else => unreachable, // Non-integer num_from_str not yet supported
            };
            const is_signed: bool = switch (payload_idx) {
                .i8, .i16, .i32, .i64 => true,
                .u8, .u16, .u32, .u64 => false,
                else => unreachable,
            };

            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_int_from_str);
            const base_reg = frame_ptr;

            // roc_builtins_int_from_str(out, str_bytes, str_len, str_cap, int_width, is_signed, disc_offset)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_offset);
            try builder.addMemArg(base_reg, str_off);
            try builder.addMemArg(base_reg, str_off + 8);
            try builder.addMemArg(base_reg, str_off + 16);
            try builder.addImmArg(@intCast(int_width));
            try builder.addImmArg(if (is_signed) @as(i64, 1) else @as(i64, 0));
            try builder.addImmArg(@intCast(disc_offset));
            try self.callBuiltin(&builder, fn_addr, .int_from_str);

            return .{ .stack = .{ .offset = result_offset } };
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

            const ls = self.layout_store orelse unreachable;
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
            op: LirExpr.BinOp,
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
                    .lt => if (is_unsigned) .cc else .lt,
                    .lte => if (is_unsigned) .ls else .le,
                    .gt => if (is_unsigned) .hi else .gt,
                    .gte => if (is_unsigned) .cs else .ge,
                    else => unreachable,
                };

                // Get unsigned condition for low part (low parts are always unsigned)
                const low_cond: Condition = switch (op) {
                    .lt => .cc,
                    .lte => .ls,
                    .gt => .hi,
                    .gte => .cs,
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

                // Prepare result values
                const one_reg = try self.allocTempGeneral();
                const zero_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(one_reg, 1);
                try self.codegen.emitLoadImm(zero_reg, 0);

                // Get signed/unsigned condition for high part
                const high_true_cond: Condition = switch (op) {
                    .lt => if (is_unsigned) .below else .less,
                    .lte => if (is_unsigned) .below_or_equal else .less_or_equal,
                    .gt => if (is_unsigned) .above else .greater,
                    .gte => if (is_unsigned) .above_or_equal else .greater_or_equal,
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
                    .lt => .below,
                    .lte => .below_or_equal,
                    .gt => .above,
                    .gte => .above_or_equal,
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
                self.codegen.freeGeneral(zero_reg);
            }
        }

        /// Generate structural comparison for records/tuples
        fn generateStructuralComparison(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            lhs_expr: LirExpr,
            op: LirExpr.BinOp,
        ) Allocator.Error!ValueLocation {
            // Get element expressions to determine sizes for nested structures
            const elem_exprs: []const LirExprId = switch (lhs_expr) {
                .record => |r| self.store.getExprSpan(r.fields),
                .tuple => |t| self.store.getExprSpan(t.elems),
                else => unreachable,
            };

            if (elem_exprs.len == 0) {
                // Empty records/tuples are always equal
                return .{ .immediate_i64 = if (op == .eq) 1 else 0 };
            }

            const result_reg = try self.allocTempGeneral();

            // Start with equality result = 1 (true for eq, will be inverted for neq)
            try self.codegen.emitLoadImm(result_reg, 1);

            // Calculate comparison byte offsets and sizes using the layout store
            // This must match how generateTuple/generateRecord place elements
            var cmp_offsets: std.ArrayList(i32) = .empty;
            defer cmp_offsets.deinit(self.allocator);
            var cmp_sizes: std.ArrayList(u32) = .empty;
            defer cmp_sizes.deinit(self.allocator);

            const ls = self.layout_store;

            switch (lhs_expr) {
                .record => |r| {
                    if (ls) |layout_store| {
                        const record_layout = layout_store.getLayout(r.record_layout);
                        if (record_layout.tag == .record) {
                            // Use layout store offsets and sizes to match generateRecord
                            for (0..elem_exprs.len) |i| {
                                const field_offset = layout_store.getRecordFieldOffset(record_layout.data.record.idx, @intCast(i));
                                const field_size = layout_store.getRecordFieldSize(record_layout.data.record.idx, @intCast(i));
                                const field_slots: usize = @max(1, (field_size + 7) / 8);
                                for (0..field_slots) |j| {
                                    try cmp_offsets.append(self.allocator, @as(i32, @intCast(field_offset)) + @as(i32, @intCast(j)) * 8);
                                    const remaining = field_size - @as(u32, @intCast(j)) * 8;
                                    try cmp_sizes.append(self.allocator, @min(remaining, 8));
                                }
                            }
                        } else {
                            // Fallback: 16-byte slots
                            for (0..elem_exprs.len) |i| {
                                try cmp_offsets.append(self.allocator, @as(i32, @intCast(i)) * 16);
                                try cmp_sizes.append(self.allocator, 8);
                            }
                        }
                    } else {
                        // No layout store: 16-byte slots
                        for (0..elem_exprs.len) |i| {
                            try cmp_offsets.append(self.allocator, @as(i32, @intCast(i)) * 16);
                            try cmp_sizes.append(self.allocator, 8);
                        }
                    }
                },
                .tuple => |t| {
                    if (ls) |layout_store| {
                        const tuple_layout = layout_store.getLayout(t.tuple_layout);
                        if (tuple_layout.tag == .tuple) {
                            // Use layout store offsets to match generateTuple
                            for (0..elem_exprs.len) |i| {
                                const elem_offset = layout_store.getTupleElementOffset(tuple_layout.data.tuple.idx, @intCast(i));
                                const elem_size = layout_store.getTupleElementSize(tuple_layout.data.tuple.idx, @intCast(i));

                                const elem_slots: usize = @max(1, (elem_size + 7) / 8);

                                for (0..elem_slots) |j| {
                                    try cmp_offsets.append(self.allocator, @as(i32, @intCast(elem_offset)) + @as(i32, @intCast(j)) * 8);
                                    const remaining = elem_size - @as(u32, @intCast(j)) * 8;
                                    try cmp_sizes.append(self.allocator, @min(remaining, 8));
                                }
                            }
                        } else {
                            // Fallback: 8-byte slots with nested tuple flattening
                            var current_offset: i32 = 0;
                            for (elem_exprs) |elem_id| {
                                const elem_expr = self.store.getExpr(elem_id);
                                const elem_slots: usize = switch (elem_expr) {
                                    .tuple => |inner_t| self.store.getExprSpan(inner_t.elems).len,
                                    else => 1,
                                };
                                for (0..elem_slots) |_| {
                                    try cmp_offsets.append(self.allocator, current_offset);
                                    try cmp_sizes.append(self.allocator, 8);
                                    current_offset += 8;
                                }
                            }
                        }
                    } else {
                        // No layout store: 8-byte slots with nested tuple flattening
                        var current_offset: i32 = 0;
                        for (elem_exprs) |elem_id| {
                            const elem_expr = self.store.getExpr(elem_id);
                            const elem_slots: usize = switch (elem_expr) {
                                .tuple => |inner_t| self.store.getExprSpan(inner_t.elems).len,
                                else => 1,
                            };
                            for (0..elem_slots) |_| {
                                try cmp_offsets.append(self.allocator, current_offset);
                                try cmp_sizes.append(self.allocator, 8);
                                current_offset += 8;
                            }
                        }
                    }
                },
                else => unreachable,
            }

            const temp_lhs = try self.allocTempGeneral();
            const temp_rhs = try self.allocTempGeneral();

            // Compare all elements at their respective offsets
            for (0..cmp_offsets.items.len) |i| {
                const offset: i32 = cmp_offsets.items[i];
                const cmp_size: u32 = cmp_sizes.items[i];

                // Load LHS element
                switch (lhs_loc) {
                    .stack_str => |base_offset| {
                        try self.emitLoad(.w64, temp_lhs, frame_ptr, base_offset + offset);
                    },
                    .stack => |s| {
                        const base_offset = s.offset;
                        try self.emitLoad(.w64, temp_lhs, frame_ptr, base_offset + offset);
                    },
                    else => {
                        // For single-element, the value IS the element
                        if (i == 0) {
                            const reg = try self.ensureInGeneralReg(lhs_loc);
                            try self.emitMovRegReg(temp_lhs, reg);
                        }
                    },
                }

                // Load RHS element
                switch (rhs_loc) {
                    .stack_str => |base_offset| {
                        try self.emitLoad(.w64, temp_rhs, frame_ptr, base_offset + offset);
                    },
                    .stack => |s| {
                        const base_offset = s.offset;
                        try self.emitLoad(.w64, temp_rhs, frame_ptr, base_offset + offset);
                    },
                    else => {
                        // For single-element, the value IS the element
                        if (i == 0) {
                            const reg = try self.ensureInGeneralReg(rhs_loc);
                            try self.emitMovRegReg(temp_rhs, reg);
                        }
                    },
                }

                // Mask to actual field size if less than 8 bytes
                if (cmp_size < 8) {
                    const mask: u64 = (@as(u64, 1) << @intCast(cmp_size * 8)) - 1;
                    if (comptime target.toCpuArch() == .aarch64) {
                        const mask_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(mask_reg, @bitCast(mask));
                        try self.codegen.emit.andRegRegReg(.w64, temp_lhs, temp_lhs, mask_reg);
                        try self.codegen.emit.andRegRegReg(.w64, temp_rhs, temp_rhs, mask_reg);
                        self.codegen.freeGeneral(mask_reg);
                    } else {
                        try self.codegen.emit.andRegImm32(temp_lhs, @intCast(mask));
                        try self.codegen.emit.andRegImm32(temp_rhs, @intCast(mask));
                    }
                }

                // Compare elements: if not equal, set result to 0
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.cmp(.w64, temp_lhs, temp_rhs);
                    // If not equal, clear result register
                    try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                } else {
                    try self.codegen.emit.cmpRegReg(.w64, temp_lhs, temp_rhs);
                    // Use CMOV to set result to 0 if not equal
                    const zero_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(zero_reg, 0);
                    try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg);
                    self.codegen.freeGeneral(zero_reg);
                }
            }

            self.codegen.freeGeneral(temp_lhs);
            self.codegen.freeGeneral(temp_rhs);

            // If neq, invert the result
            if (op == .neq) {
                try self.emitXorImm(.w64, result_reg, result_reg, 1);
            }

            return .{ .general_reg = result_reg };
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
            op: LirExpr.BinOp,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;
            const stored_layout = ls.getLayout(tu_layout_idx);
            if (stored_layout.tag != .tag_union) unreachable;

            const tu_idx = stored_layout.data.tag_union.idx;
            const tu_data = ls.getTagUnionData(tu_idx);
            const total_size = tu_data.size;

            if (total_size == 0) {
                return .{ .immediate_i64 = if (op == .eq) 1 else 0 };
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
            if (disc_use_w32) {
                try self.codegen.emitLoadStack(.w32, lhs_disc, lhs_base + disc_offset);
                try self.codegen.emitLoadStack(.w32, rhs_disc, rhs_base + disc_offset);
            } else {
                try self.codegen.emitLoadStack(.w64, lhs_disc, lhs_base + disc_offset);
                try self.codegen.emitLoadStack(.w64, rhs_disc, rhs_base + disc_offset);
            }

            // Mask discriminants to their actual size
            if (disc_size < 8) {
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

            if (op == .neq) {
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
            op: LirExpr.BinOp,
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

            if (op == .neq) {
                try self.emitXorImm(.w64, result_reg, result_reg, 1);
            }

            return .{ .general_reg = result_reg };
        }

        /// Generate list comparison (element by element)
        fn generateListComparison(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            lhs_expr: LirExpr,
            op: LirExpr.BinOp,
        ) Allocator.Error!ValueLocation {
            // Get list elements for element-by-element comparison
            const lhs_list = switch (lhs_expr) {
                .list => |l| l,
                else => unreachable,
            };
            const lhs_elems = self.store.getExprSpan(lhs_list.elems);

            // Determine element size (default to 8 bytes)
            const elem_layout = switch (lhs_expr) {
                .list => |l| l.elem_layout,
                else => .i64,
            };

            // Check if elements are themselves lists by examining the actual elements
            // (elem_layout may not correctly indicate nested lists)
            const is_nested_list = blk: {
                if (lhs_elems.len > 0) {
                    const first_elem = self.store.getExpr(lhs_elems[0]);
                    break :blk (first_elem == .list or first_elem == .empty_list);
                }
                break :blk false;
            };

            // For nested lists, elements are roc_list_size-byte structs regardless of elem_layout
            const elem_size: i32 = if (is_nested_list) roc_list_size else switch (elem_layout) {
                .i8, .u8 => 1,
                .i16, .u16 => 2,
                .i32, .u32, .f32 => 4,
                .i64, .u64, .f64, .str => 8,
                .i128, .u128, .dec => 16,
                else => unreachable,
            };

            const result_reg = try self.allocTempGeneral();

            if (lhs_elems.len == 0) {
                // Empty lists are equal
                try self.codegen.emitLoadImm(result_reg, if (op == .eq) 1 else 0);
                return .{ .general_reg = result_reg };
            }

            // Start with result = 1 (equal)
            try self.codegen.emitLoadImm(result_reg, 1);

            const temp_lhs = try self.allocTempGeneral();
            const temp_rhs = try self.allocTempGeneral();

            // The ptr in each list struct points to the element data
            // Load ptrs first, then compare elements through them

            // Load lhs ptr
            const lhs_ptr_reg = try self.allocTempGeneral();
            switch (lhs_loc) {
                .stack => |s| {
                    const base_offset = s.offset;
                    try self.emitLoad(.w64, lhs_ptr_reg, frame_ptr, base_offset);
                },
                .list_stack => |list_info| {
                    // Load ptr from the list struct
                    try self.emitLoad(.w64, lhs_ptr_reg, frame_ptr, list_info.struct_offset);
                },
                else => unreachable,
            }

            // Load rhs ptr
            const rhs_ptr_reg = try self.allocTempGeneral();
            switch (rhs_loc) {
                .stack => |s| {
                    const base_offset = s.offset;
                    try self.emitLoad(.w64, rhs_ptr_reg, frame_ptr, base_offset);
                },
                .list_stack => |list_info| {
                    // Load ptr from the list struct
                    try self.emitLoad(.w64, rhs_ptr_reg, frame_ptr, list_info.struct_offset);
                },
                else => unreachable,
            }

            // Compare each element through the pointers
            if (is_nested_list) {
                // For nested lists, we need to compare the inner list contents
                // Each inner list is a 24-byte struct (ptr, len, capacity)
                // We need to compare lengths first, then compare elements pointed to
                for (0..lhs_elems.len) |i| {
                    const offset: i32 = @as(i32, @intCast(i)) * elem_size;

                    // For nested lists, we need to compare the inner list contents,
                    // not just pointers. Inner lists are stored as (ptr, len) pairs.
                    // Load inner list pointers
                    try self.emitLoad(.w64, temp_lhs, lhs_ptr_reg, offset);
                    try self.emitLoad(.w64, temp_rhs, rhs_ptr_reg, offset);

                    // Load inner list lengths
                    const inner_len_lhs = try self.allocTempGeneral();
                    const inner_len_rhs = try self.allocTempGeneral();
                    try self.emitLoad(.w64, inner_len_lhs, lhs_ptr_reg, offset + 8);
                    try self.emitLoad(.w64, inner_len_rhs, rhs_ptr_reg, offset + 8);

                    // Compare lengths first
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.cmp(.w64, inner_len_lhs, inner_len_rhs);
                        try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                    } else {
                        try self.codegen.emit.cmpRegReg(.w64, inner_len_lhs, inner_len_rhs);
                        const zero_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(zero_reg, 0);
                        try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg);
                        self.codegen.freeGeneral(zero_reg);
                    }

                    // Now compare inner list elements
                    // Get the inner list's element info from the expression
                    const inner_list_expr = self.store.getExpr(lhs_elems[i]);
                    const inner_elem_count: usize = switch (inner_list_expr) {
                        .list => |l| self.store.getExprSpan(l.elems).len,
                        .empty_list => 0,
                        else => 0,
                    };
                    const inner_elem_layout = switch (inner_list_expr) {
                        .list => |l| l.elem_layout,
                        else => .i64,
                    };
                    const inner_elem_size: i32 = switch (inner_elem_layout) {
                        .i8, .u8 => 1,
                        .i16, .u16 => 2,
                        .i32, .u32, .f32 => 4,
                        .i64, .u64, .f64, .str => 8,
                        .i128, .u128, .dec => 16,
                        else => 8,
                    };

                    // Compare each inner element
                    // temp_lhs = inner lhs ptr, temp_rhs = inner rhs ptr
                    const inner_temp_lhs = try self.allocTempGeneral();
                    const inner_temp_rhs = try self.allocTempGeneral();
                    for (0..inner_elem_count) |j| {
                        const inner_offset: i32 = @as(i32, @intCast(j)) * inner_elem_size;

                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, inner_temp_lhs, temp_lhs, inner_offset);
                            try self.codegen.emit.ldrRegMemSoff(.w64, inner_temp_rhs, temp_rhs, inner_offset);
                            try self.codegen.emit.cmp(.w64, inner_temp_lhs, inner_temp_rhs);
                            try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, inner_temp_lhs, temp_lhs, inner_offset);
                            try self.codegen.emit.movRegMem(.w64, inner_temp_rhs, temp_rhs, inner_offset);
                            try self.codegen.emit.cmpRegReg(.w64, inner_temp_lhs, inner_temp_rhs);
                            const zero_reg2 = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(zero_reg2, 0);
                            try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg2);
                            self.codegen.freeGeneral(zero_reg2);
                        }
                    }
                    self.codegen.freeGeneral(inner_temp_lhs);
                    self.codegen.freeGeneral(inner_temp_rhs);
                    self.codegen.freeGeneral(inner_len_lhs);
                    self.codegen.freeGeneral(inner_len_rhs);
                }
            } else {
                // Simple flat list comparison
                for (0..lhs_elems.len) |i| {
                    const offset: i32 = @as(i32, @intCast(i)) * elem_size;

                    // Load lhs element: [lhs_ptr + offset]
                    try self.emitLoad(.w64, temp_lhs, lhs_ptr_reg, offset);

                    // Load rhs element: [rhs_ptr + offset]
                    try self.emitLoad(.w64, temp_rhs, rhs_ptr_reg, offset);

                    // Compare elements: if not equal, set result to 0
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.cmp(.w64, temp_lhs, temp_rhs);
                        try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                    } else {
                        try self.codegen.emit.cmpRegReg(.w64, temp_lhs, temp_rhs);
                        // Use CMOV to set result to 0 if not equal
                        const zero_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(zero_reg, 0);
                        try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg);
                        self.codegen.freeGeneral(zero_reg);
                    }
                }
            }

            self.codegen.freeGeneral(temp_lhs);
            self.codegen.freeGeneral(temp_rhs);
            self.codegen.freeGeneral(lhs_ptr_reg);
            self.codegen.freeGeneral(rhs_ptr_reg);

            // If neq, invert the result
            if (op == .neq) {
                try self.emitXorImm(.w64, result_reg, result_reg, 1);
            }

            return .{ .general_reg = result_reg };
        }

        /// Generate record comparison using layout information.
        /// Compares records field-by-field using the correct comparison method
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
            const ls = self.layout_store orelse unreachable;

            if (field_layout_idx == .str) {
                // String: compare by content using strEqual builtin
                const eq_loc = try self.callStr2ToScalar(lhs_off, rhs_off, @intFromPtr(&wrapStrEqual), .str_equal);
                const eq_reg = try self.ensureInGeneralReg(eq_loc);
                try self.emitMovRegReg(result_reg, eq_reg);
                self.codegen.freeGeneral(eq_reg);
            } else if (field_layout_idx == .dec or field_layout_idx == .i128 or field_layout_idx == .u128 or field_size == 16) {
                // 128-bit field: compare as i128 (two 64-bit parts)
                const lhs_parts = try self.getI128Parts(.{ .stack_i128 = lhs_off }, .signed);
                const rhs_parts = try self.getI128Parts(.{ .stack_i128 = rhs_off }, .signed);
                try self.generateI128Equality(lhs_parts, rhs_parts, result_reg, true);
                self.codegen.freeGeneral(lhs_parts.low);
                self.codegen.freeGeneral(lhs_parts.high);
                self.codegen.freeGeneral(rhs_parts.low);
                self.codegen.freeGeneral(rhs_parts.high);
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
            } else {
                // Check layout tag for compound types that need recursive comparison
                const field_layout = ls.getLayout(field_layout_idx);
                switch (field_layout.tag) {
                    .record => {
                        const sub_loc = try self.generateRecordComparisonByLayout(
                            .{ .stack = .{ .offset = lhs_off } },
                            .{ .stack = .{ .offset = rhs_off } },
                            field_layout_idx,
                            .eq,
                        );
                        const sub_reg = try self.ensureInGeneralReg(sub_loc);
                        try self.emitMovRegReg(result_reg, sub_reg);
                        self.codegen.freeGeneral(sub_reg);
                    },
                    .tuple => {
                        const sub_loc = try self.generateTupleComparisonByLayout(
                            .{ .stack = .{ .offset = lhs_off } },
                            .{ .stack = .{ .offset = rhs_off } },
                            field_layout_idx,
                            .eq,
                        );
                        const sub_reg = try self.ensureInGeneralReg(sub_loc);
                        try self.emitMovRegReg(result_reg, sub_reg);
                        self.codegen.freeGeneral(sub_reg);
                    },
                    .tag_union => {
                        const sub_loc = try self.generateTagUnionComparisonByLayout(
                            .{ .stack = .{ .offset = lhs_off } },
                            .{ .stack = .{ .offset = rhs_off } },
                            field_layout_idx,
                            .eq,
                        );
                        const sub_reg = try self.ensureInGeneralReg(sub_loc);
                        try self.emitMovRegReg(result_reg, sub_reg);
                        self.codegen.freeGeneral(sub_reg);
                    },
                    else => {
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
                    },
                }
            }
        }

        fn generateRecordComparisonByLayout(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            record_layout_idx: layout.Idx,
            op: LirExpr.BinOp,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;
            const stored_layout = ls.getLayout(record_layout_idx);
            if (stored_layout.tag != .record) unreachable;

            const record_idx = stored_layout.data.record.idx;
            const record_data = ls.getRecordData(record_idx);
            const field_count = record_data.fields.count;
            if (field_count == 0) {
                return .{ .immediate_i64 = if (op == .eq) 1 else 0 };
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
                const field_offset = ls.getRecordFieldOffset(record_idx, @intCast(field_i));
                const field_size = ls.getRecordFieldSize(record_idx, @intCast(field_i));
                const field_layout_idx = ls.getRecordFieldLayout(record_idx, @intCast(field_i));

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

            if (op == .neq) {
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

        /// Generate tuple comparison using layout information
        fn generateTupleComparisonByLayout(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            tuple_layout_idx: layout.Idx,
            op: LirExpr.BinOp,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;
            const stored_layout = ls.getLayout(tuple_layout_idx);
            if (stored_layout.tag != .tuple) unreachable;

            const tuple_idx = stored_layout.data.tuple.idx;
            const tuple_data = ls.getTupleData(tuple_idx);
            const elem_count = tuple_data.fields.count;
            if (elem_count == 0) {
                return .{ .immediate_i64 = if (op == .eq) 1 else 0 };
            }

            const lhs_base = try self.ensureRecordOnStack(lhs_loc, ls.layoutSizeAlign(stored_layout).size);
            const rhs_base = try self.ensureRecordOnStack(rhs_loc, ls.layoutSizeAlign(stored_layout).size);

            // Use stack-based accumulator since compareFieldByLayout may call builtins
            const result_slot = self.codegen.allocStackSlot(8);
            {
                const temp = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(temp, 1);
                try self.codegen.emitStoreStack(.w64, result_slot, temp);
                self.codegen.freeGeneral(temp);
            }

            var elem_i: u32 = 0;
            while (elem_i < elem_count) : (elem_i += 1) {
                const elem_offset = ls.getTupleElementOffset(tuple_idx, @intCast(elem_i));
                const elem_size = ls.getTupleElementSize(tuple_idx, @intCast(elem_i));
                const elem_layout_idx = ls.getTupleElementLayout(tuple_idx, @intCast(elem_i));

                if (elem_size == 0) continue;

                const lhs_elem_off = lhs_base + @as(i32, @intCast(elem_offset));
                const rhs_elem_off = rhs_base + @as(i32, @intCast(elem_offset));

                const elem_eq_reg = try self.allocTempGeneral();
                try self.compareFieldByLayout(lhs_elem_off, rhs_elem_off, elem_layout_idx, elem_size, elem_eq_reg);

                const acc_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadStack(.w64, acc_reg, result_slot);
                try self.emitAndRegs(.w64, acc_reg, acc_reg, elem_eq_reg);
                try self.codegen.emitStoreStack(.w64, result_slot, acc_reg);
                self.codegen.freeGeneral(acc_reg);
                self.codegen.freeGeneral(elem_eq_reg);
            }

            const result_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadStack(.w64, result_reg, result_slot);

            if (op == .neq) {
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

        /// Generate list comparison using layout information
        /// Generate list comparison using layout information.
        /// Compares list lengths at runtime, then compares elements one by one.
        fn generateListComparisonByLayout(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            list_layout_idx: layout.Idx,
            op: LirExpr.BinOp,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;
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

            if (op == .neq) {
                try self.emitXorImm(.w64, result_reg, result_reg, 1);
            }

            return .{ .general_reg = result_reg };
        }

        /// Generate floating-point binary operation
        fn generateFloatBinop(
            self: *Self,
            op: LirExpr.BinOp,
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
                    .eq => {
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
                    .neq => {
                        // NaN-safe neq: (ZF=0) OR (PF=1) → setne + setp + or
                        const result_reg = try self.codegen.allocGeneralFor(0);
                        const tmp_reg = try self.codegen.allocGeneralFor(0);
                        try self.codegen.emit.ucomisdRegReg(lhs_reg, rhs_reg);
                        try self.codegen.emit.setcc(.not_equal, result_reg);
                        try self.codegen.emit.setcc(.parity_even, tmp_reg);
                        try self.codegen.emit.orRegReg(.w64, result_reg, tmp_reg);
                        try self.codegen.emit.andRegImm8(result_reg, 1);
                        self.codegen.freeGeneral(tmp_reg);
                        self.codegen.freeFloat(lhs_reg);
                        self.codegen.freeFloat(rhs_reg);
                        return .{ .general_reg = result_reg };
                    },
                    .lt => {
                        // NaN-safe lt: swap operands, use "above" (CF=0 AND ZF=0)
                        const result_reg = try self.codegen.allocGeneralFor(0);
                        try self.codegen.emitCmpF64(result_reg, rhs_reg, lhs_reg, .above);
                        self.codegen.freeFloat(lhs_reg);
                        self.codegen.freeFloat(rhs_reg);
                        return .{ .general_reg = result_reg };
                    },
                    .lte => {
                        // NaN-safe lte: swap operands, use "above_or_equal" (CF=0)
                        const result_reg = try self.codegen.allocGeneralFor(0);
                        try self.codegen.emitCmpF64(result_reg, rhs_reg, lhs_reg, .above_or_equal);
                        self.codegen.freeFloat(lhs_reg);
                        self.codegen.freeFloat(rhs_reg);
                        return .{ .general_reg = result_reg };
                    },
                    .gt, .gte => {
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
                .add => try self.codegen.emitAddF64(result_reg, lhs_reg, rhs_reg),
                .sub => try self.codegen.emitSubF64(result_reg, lhs_reg, rhs_reg),
                .mul => try self.codegen.emitMulF64(result_reg, lhs_reg, rhs_reg),
                .div => try self.codegen.emitDivF64(result_reg, lhs_reg, rhs_reg),
                else => unreachable,
            }

            // Free operand registers
            self.codegen.freeFloat(lhs_reg);
            self.codegen.freeFloat(rhs_reg);

            return .{ .float_reg = result_reg };
        }

        /// Map a BinOp comparison to the appropriate float condition code.
        /// Returns null for non-comparison ops (arithmetic).
        /// AArch64 FCMP and x86_64 UCOMISD set flags differently from integer CMP,
        /// so float comparisons use unsigned/specific conditions rather than signed ones.
        fn floatCondition(op: LirExpr.BinOp) ?Condition {
            return switch (op) {
                .eq => condEqual(),
                .neq => condNotEqual(),
                .lt => if (comptime target.toCpuArch() == .aarch64)
                    // After FCMP: N=1 only when a < b
                    @as(Condition, .mi)
                else
                    // After UCOMISD: CF=1 when a < b
                    @as(Condition, .below),
                .lte => if (comptime target.toCpuArch() == .aarch64)
                    // After FCMP: (C=0 or Z=1) for a <= b
                    @as(Condition, .ls)
                else
                    // After UCOMISD: (CF=1 or ZF=1) for a <= b
                    @as(Condition, .below_or_equal),
                .gt => if (comptime target.toCpuArch() == .aarch64)
                    // After FCMP: (Z=0 and N=V) for a > b
                    @as(Condition, .gt)
                else
                    // After UCOMISD: (CF=0 and ZF=0) for a > b
                    @as(Condition, .above),
                .gte => if (comptime target.toCpuArch() == .aarch64)
                    // After FCMP: N=V for a >= b
                    @as(Condition, .ge)
                else
                    // After UCOMISD: CF=0 for a >= b
                    @as(Condition, .above_or_equal),
                else => null,
            };
        }

        /// Generate code for unary minus
        fn generateUnaryMinus(self: *Self, unary: anytype) Allocator.Error!ValueLocation {
            const inner_loc = try self.generateExpr(unary.expr);

            // Check if float
            const is_float = switch (unary.result_layout) {
                .f32, .f64 => true,
                else => false,
            };

            // Check if 128-bit type
            const is_i128 = switch (unary.result_layout) {
                .i128, .u128, .dec => true,
                else => false,
            };

            if (is_float) {
                const src_reg = try self.ensureInFloatReg(inner_loc);
                const result_reg = try self.codegen.allocFloatFor(0);
                try self.codegen.emitNegF64(result_reg, src_reg);
                self.codegen.freeFloat(src_reg);
                return .{ .float_reg = result_reg };
            } else if (is_i128) {
                // 128-bit negation: result = 0 - value (using SUBS/SBC or SUB/SBB)
                const parts = try self.getI128Parts(inner_loc, .signed); // negation is signed

                const result_low = try self.allocTempGeneral();
                const result_high = try self.allocTempGeneral();

                if (comptime target.toCpuArch() == .aarch64) {
                    // Negate using NEGS (NEG with flags) and NGC (negate with carry)
                    // NEGS is actually SUBS with XZR as first operand
                    try self.codegen.emit.subsRegRegReg(.w64, result_low, .ZRSP, parts.low);
                    // NGC is SBC with XZR as first operand
                    try self.codegen.emit.sbcRegRegReg(.w64, result_high, .ZRSP, parts.high);
                } else {
                    // x86_64: Load 0, then subtract
                    try self.codegen.emitLoadImm(result_low, 0);
                    try self.codegen.emit.subRegReg(.w64, result_low, parts.low);
                    try self.codegen.emitLoadImm(result_high, 0);
                    try self.codegen.emit.sbbRegReg(.w64, result_high, parts.high);
                }

                self.codegen.freeGeneral(parts.low);
                self.codegen.freeGeneral(parts.high);

                // Store result to stack
                const stack_offset = self.codegen.allocStackSlot(16);
                try self.codegen.emitStoreStack(.w64, stack_offset, result_low);
                try self.codegen.emitStoreStack(.w64, stack_offset + 8, result_high);

                self.codegen.freeGeneral(result_low);
                self.codegen.freeGeneral(result_high);

                return .{ .stack_i128 = stack_offset };
            } else {
                // For 64-bit integers, use NEG
                const reg = try self.ensureInGeneralReg(inner_loc);
                const result_reg = try self.allocTempGeneral();
                try self.codegen.emitNeg(.w64, result_reg, reg);
                self.codegen.freeGeneral(reg);
                return .{ .general_reg = result_reg };
            }
        }

        /// Generate code for unary not
        fn generateUnaryNot(self: *Self, unary: anytype) Allocator.Error!ValueLocation {
            const inner_loc = try self.generateExpr(unary.expr);

            const reg = try self.ensureInGeneralReg(inner_loc);
            const result_reg = try self.allocTempGeneral();

            // Boolean NOT: XOR with 1 to flip 0↔1
            // 0 XOR 1 = 1 (False -> True)
            // 1 XOR 1 = 0 (True -> False)
            try self.codegen.emitXorImm(.w64, result_reg, reg, 1);

            self.codegen.freeGeneral(reg);
            return .{ .general_reg = result_reg };
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
            var result_size: u32 = switch (ite.result_layout) {
                // Scalar types - size based on type
                .i8, .u8, .bool => 1,
                .i16, .u16 => 2,
                .i32, .u32, .f32 => 4,
                .i64, .u64, .f64 => 8,
                .i128, .u128, .dec => 16,
                .str => blk: {
                    is_str_result = true;
                    break :blk roc_str_size;
                },
                else => if (self.layout_store) |ls| blk: {
                    const result_layout = ls.getLayout(ite.result_layout);
                    break :blk switch (result_layout.tag) {
                        .list, .list_of_zst => inner: {
                            is_list_result = true;
                            break :inner roc_list_size;
                        },
                        .tuple => ls.getTupleData(result_layout.data.tuple.idx).size,
                        .record => ls.getRecordData(result_layout.data.record.idx).size,
                        .tag_union => ls.getTagUnionData(result_layout.data.tag_union.idx).size,
                        .zst => 0,
                        .scalar => ls.layoutSizeAlign(result_layout).size,
                        else => unreachable,
                    };
                } else unreachable,
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
                    // Detect list results from body_loc when layout check failed
                    // (cross-module layouts may be out of bounds)
                    if (!is_list_result and body_loc == .list_stack) {
                        is_list_result = true;
                        // Update result_size since layout check might have defaulted to target_ptr_size
                        result_size = roc_list_size;
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
            if (tu_disc_size < 4) {
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

        /// Bind tag payload fields to symbols after a tag pattern match.
        fn bindTagPayloadFields(
            self: *Self,
            tag_pattern: anytype,
            value_loc: ValueLocation,
            value_layout_val: anytype,
            tu_disc_offset: i32,
        ) Allocator.Error!void {
            const ls = self.layout_store orelse unreachable;
            const args = self.store.getPatternSpan(tag_pattern.args);
            if (args.len == 0) return;

            // Get variant payload layout for proper binding
            const variant_payload_layout: ?layout.Idx = if (value_layout_val.tag == .tag_union) vl_blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                const variants = ls.getTagUnionVariants(tu_data);
                if (tag_pattern.discriminant < variants.len) {
                    const variant = variants.get(tag_pattern.discriminant);
                    break :vl_blk variant.payload_layout;
                }
                break :vl_blk null;
            } else null;

            // Determine if payload is a tuple (multi-field payload)
            const payload_is_tuple = if (variant_payload_layout) |pl| blk_pt: {
                const pl_val = ls.getLayout(pl);
                break :blk_pt pl_val.tag == .tuple;
            } else false;

            for (args, 0..) |arg_pattern_id, arg_idx| {
                const arg_pattern = self.store.getPattern(arg_pattern_id);
                switch (arg_pattern) {
                    .bind => |arg_bind| {
                        const symbol_key: u64 = @bitCast(arg_bind.symbol);
                        switch (value_loc) {
                            .stack => |s| {
                                const base_offset = s.offset;
                                const arg_loc: ValueLocation = if (payload_is_tuple and variant_payload_layout != null) plblk: {
                                    // Multi-arg tag: payload is a tuple, use tuple element offsets/layouts
                                    const pl_val = ls.getLayout(variant_payload_layout.?);
                                    const elem_offset = ls.getTupleElementOffsetByOriginalIndex(pl_val.data.tuple.idx, @intCast(arg_idx));
                                    const elem_layout = ls.getTupleElementLayoutByOriginalIndex(pl_val.data.tuple.idx, @intCast(arg_idx));
                                    const arg_offset = base_offset + @as(i32, @intCast(elem_offset));
                                    break :plblk self.stackLocationForLayout(elem_layout, arg_offset);
                                } else if (variant_payload_layout) |pl| plblk: {
                                    // Single-arg tag: use variant payload layout directly
                                    const payload_offset = base_offset;
                                    if (pl == .i128 or pl == .u128 or pl == .dec) {
                                        break :plblk .{ .stack_i128 = payload_offset };
                                    } else if (pl == .str) {
                                        break :plblk .{ .stack_str = payload_offset };
                                    } else {
                                        const pl_val = ls.getLayout(pl);
                                        if (pl_val.tag == .list or pl_val.tag == .list_of_zst) {
                                            break :plblk .{ .list_stack = .{
                                                .struct_offset = payload_offset,
                                                .data_offset = 0,
                                                .num_elements = 0,
                                            } };
                                        }
                                        // For small payloads (< 8 bytes), the discriminant is adjacent.
                                        // We must extract only the payload bytes to avoid reading
                                        // discriminant data when the value is later used as 8 bytes.
                                        const pl_size = ls.layoutSizeAlign(pl_val).size;
                                        if (pl_size > 0 and pl_size < 8 and tu_disc_offset < 8) {
                                            const fresh_slot = self.codegen.allocStackSlot(8);
                                            const tmp_reg = try self.allocTempGeneral();
                                            // Zero the slot first
                                            try self.codegen.emitLoadImm(tmp_reg, 0);
                                            try self.codegen.emitStoreStack(.w64, fresh_slot, tmp_reg);
                                            // Load only payload bytes (w32 for 1-4 bytes, w64 for 5-7)
                                            if (pl_size <= 4) {
                                                try self.codegen.emitLoadStack(.w32, tmp_reg, payload_offset);
                                                // Mask to exact size if discriminant is within the 4 bytes
                                                if (pl_size < 4) {
                                                    const pl_mask: i64 = (@as(i64, 1) << @intCast(pl_size * 8)) - 1;
                                                    const mask_reg = try self.allocTempGeneral();
                                                    try self.codegen.emitLoadImm(mask_reg, pl_mask);
                                                    try self.emitAndRegs(.w64, tmp_reg, tmp_reg, mask_reg);
                                                    self.codegen.freeGeneral(mask_reg);
                                                }
                                            } else {
                                                try self.codegen.emitLoadStack(.w64, tmp_reg, payload_offset);
                                            }
                                            try self.codegen.emitStoreStack(.w64, fresh_slot, tmp_reg);
                                            self.codegen.freeGeneral(tmp_reg);
                                            break :plblk .{ .stack = .{ .offset = fresh_slot } };
                                        }
                                        break :plblk .{ .stack = .{ .offset = payload_offset } };
                                    }
                                } else .{ .stack = .{ .offset = base_offset + @as(i32, @intCast(arg_idx)) * 8 } };
                                try self.symbol_locations.put(symbol_key, arg_loc);
                            },
                            else => {
                                try self.symbol_locations.put(symbol_key, value_loc);
                            },
                        }
                    },
                    .wildcard => {},
                    .tag => |inner_tag| {
                        // Nested tag pattern (e.g., Err(ListWasEmpty) where ListWasEmpty is a zero-arg tag)
                        const inner_args = self.store.getPatternSpan(inner_tag.args);
                        for (inner_args) |inner_arg_id| {
                            const inner_arg = self.store.getPattern(inner_arg_id);
                            switch (inner_arg) {
                                .bind => |inner_bind| {
                                    const inner_key: u64 = @bitCast(inner_bind.symbol);
                                    const inner_loc: ValueLocation = if (variant_payload_layout) |pl| inner_blk: {
                                        const pl_val = ls.getLayout(pl);
                                        if (pl_val.tag == .tag_union) {
                                            break :inner_blk value_loc;
                                        }
                                        break :inner_blk value_loc;
                                    } else value_loc;
                                    try self.symbol_locations.put(inner_key, inner_loc);
                                },
                                .wildcard => {},
                                else => {},
                            }
                        }
                    },
                    else => unreachable,
                }
            }
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
        }

        /// Emit list pattern bindings: length check, prefix/suffix element binding,
        /// and rest-list extraction. Returns the conditional jump patch for length
        /// mismatch (null if last branch). Caller must free list_ptr_reg via freeGeneral.
        fn emitListPatternBindings(
            self: *Self,
            list_pattern: anytype,
            value_loc: ValueLocation,
        ) Allocator.Error!void {
            const ls = self.layout_store orelse unreachable;
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

            // Handle rest pattern (e.g. [first, .. as rest, last])
            if (!list_pattern.rest.isNone()) {
                const rest_slot = self.codegen.allocStackSlot(roc_str_size);

                const prefix_count = @as(u32, @intCast(prefix_patterns.len));
                const suffix_count = @as(u32, @intCast(suffix_patterns.len));
                const prefix_byte_offset = prefix_count * @as(u32, @intCast(elem_size));

                // Calculate rest pointer: original_ptr + prefix_len * elem_size
                const rest_ptr_reg = try self.allocTempGeneral();
                if (prefix_byte_offset == 0) {
                    try self.codegen.emit.movRegReg(.w64, rest_ptr_reg, list_ptr_reg);
                } else {
                    try self.emitAddImm(rest_ptr_reg, list_ptr_reg, @intCast(prefix_byte_offset));
                }

                // Store rest pointer at rest_slot + 0
                try self.emitStore(.w64, frame_ptr, rest_slot, rest_ptr_reg);
                self.codegen.freeGeneral(rest_ptr_reg);

                // Load original length from base_offset + 8
                const rest_len_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, rest_len_reg, frame_ptr, base_offset + 8);

                // Calculate rest length: original_length - prefix_count - suffix_count
                const total_fixed = prefix_count + suffix_count;
                if (total_fixed > 0) {
                    try self.emitSubImm(.w64, rest_len_reg, rest_len_reg, @intCast(total_fixed));
                }

                // Store rest length at rest_slot + 8
                try self.emitStore(.w64, frame_ptr, rest_slot + 8, rest_len_reg);

                // Store capacity = rest length at rest_slot + 16
                try self.emitStore(.w64, frame_ptr, rest_slot + 16, rest_len_reg);
                self.codegen.freeGeneral(rest_len_reg);

                // Bind the rest pattern to the new list slot
                try self.bindPattern(list_pattern.rest, .{ .list_stack = .{
                    .struct_offset = rest_slot,
                    .data_offset = 0,
                    .num_elements = 0,
                } });
            }

            self.codegen.freeGeneral(list_ptr_reg);
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
            const ls = self.layout_store orelse unreachable;
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
            } else 0;
            const tu_disc_size: u8 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk tu_data.discriminant_size;
            } else 4;
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

                        // Bind tag payload fields
                        try self.bindTagPayloadFields(tag_pattern, value_loc, value_layout_val, tu_disc_offset);

                        // Guard check (after bindings, since guard may reference bound vars)
                        const guard_patch = try self.emitGuardCheck(branch.guard);

                        // Generate body
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
                    .list => |list_pattern| {
                        const suffix_patterns = self.store.getPatternSpan(list_pattern.suffix);
                        const is_exact_match = list_pattern.rest.isNone() and suffix_patterns.len == 0;

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
                    .record => {
                        // Record destructuring always matches - bind fields and generate body
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
                    .tuple => {
                        // Tuple destructuring always matches - bind elements and generate body
                        // Ensure the value is on the stack for element access
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
                    } else if (result_layout_val.tag == .tag_union or result_layout_val.tag == .record or result_layout_val.tag == .tuple) {
                        // Non-scalar composite types stay as generic stack values
                        // so downstream code uses the layout for proper sizing.
                        // However, if result_size was dynamically upgraded (e.g., a branch
                        // produced a larger value than the declared layout, which happens when
                        // the ? operator's deferred type constraint munges the body type),
                        // fall through to size-based heuristics for correct ValueLocation type.
                        const declared_size = ls.layoutSizeAlign(result_layout_val).size;
                        if (result_size <= declared_size or result_size <= 8) {
                            return .{ .stack = .{ .offset = result_slot } };
                        }
                    }
                }
                // Fallback: use size-based heuristics (covers dynamic upgrade case)
                if (result_size >= roc_str_size) {
                    return .{ .stack_str = result_slot };
                } else if (result_size >= 16) {
                    return .{ .stack_i128 = result_slot };
                }
                return .{ .stack = .{ .offset = result_slot } };
            }
            if (result_reg) |reg| {
                return .{ .general_reg = reg };
            } else {
                return .{ .immediate_i64 = 0 };
            }
        }

        /// Store a match-branch result, dynamically upgrading from register to stack
        /// mode if the body produces a multi-register value (e.g., string, i128, list)
        /// but the declared result layout was ZST/small.
        fn storeMatchResult(
            self: *Self,
            body_loc: ValueLocation,
            use_stack_result: *bool,
            result_slot: *i32,
            result_reg: *?GeneralReg,
            result_size: *u32,
        ) Allocator.Error!void {
            // Check if we need to upgrade from register to stack mode
            if (!use_stack_result.*) {
                const needed_size: ?u32 = switch (body_loc) {
                    .stack_str => roc_str_size,
                    .list_stack => roc_list_size,
                    .stack_i128 => 16,
                    .immediate_i128 => 16,
                    else => null,
                };
                if (needed_size) |size| {
                    // Upgrade to stack mode
                    use_stack_result.* = true;
                    result_size.* = size;
                    result_slot.* = self.codegen.allocStackSlot(size);
                    if (result_reg.*) |reg| {
                        self.codegen.freeGeneral(reg);
                        result_reg.* = null;
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
            const ls = self.layout_store orelse unreachable;
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

                // Load heap pointer from stack slot
                const heap_ptr = try self.allocTempGeneral();
                try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);

                // Store element to heap based on its actual location type
                // We must handle different location types differently because the actual
                // size of the value may differ from elem_size (due to type variable resolution)
                switch (elem_loc) {
                    .stack_str => |src_offset| {
                        // Copy elem_size bytes from stack to heap in 8-byte chunks
                        const temp_reg = try self.allocTempGeneral();
                        try self.copyChunked(temp_reg, frame_ptr, src_offset, heap_ptr, elem_heap_offset, elem_size);
                        self.codegen.freeGeneral(temp_reg);
                    },
                    .stack => |s| {
                        const src_offset = s.offset;
                        // Copy elem_size bytes from stack to heap in 8-byte chunks
                        const temp_reg = try self.allocTempGeneral();
                        try self.copyChunked(temp_reg, frame_ptr, src_offset, heap_ptr, elem_heap_offset, elem_size);
                        self.codegen.freeGeneral(temp_reg);
                    },
                    .list_stack => |list_info| {
                        // For lists, copy the full RocList struct
                        const temp_reg = try self.allocTempGeneral();
                        var copied: u32 = 0;
                        while (copied < roc_list_size) : (copied += target_ptr_size) {
                            const chunk_src = list_info.struct_offset + @as(i32, @intCast(copied));
                            const chunk_dst = elem_heap_offset + @as(i32, @intCast(copied));
                            try self.emitLoad(.w64, temp_reg, frame_ptr, chunk_src);
                            try self.emitStore(.w64, heap_ptr, chunk_dst, temp_reg);
                        }
                        self.codegen.freeGeneral(temp_reg);
                    },
                    .immediate_i128 => |val| {
                        // For i128/Dec immediates, store the full 16 bytes
                        const low: u64 = @truncate(@as(u128, @bitCast(val)));
                        const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                        const temp_reg = try self.allocTempGeneral();

                        // Store low 8 bytes
                        try self.codegen.emitLoadImm(temp_reg, @bitCast(low));
                        try self.emitStore(.w64, heap_ptr, elem_heap_offset, temp_reg);

                        // Store high 8 bytes
                        try self.codegen.emitLoadImm(temp_reg, @bitCast(high));
                        try self.emitStore(.w64, heap_ptr, elem_heap_offset + 8, temp_reg);

                        self.codegen.freeGeneral(temp_reg);
                    },
                    .stack_i128 => |src_offset| {
                        // For i128/Dec stack values, copy the full 16 bytes
                        const temp_reg = try self.allocTempGeneral();

                        // Copy low 8 bytes
                        try self.emitLoad(.w64, temp_reg, frame_ptr, src_offset);
                        try self.emitStore(.w64, heap_ptr, elem_heap_offset, temp_reg);

                        // Copy high 8 bytes
                        try self.emitLoad(.w64, temp_reg, frame_ptr, src_offset + 8);
                        try self.emitStore(.w64, heap_ptr, elem_heap_offset + 8, temp_reg);

                        self.codegen.freeGeneral(temp_reg);
                    },
                    else => {
                        // For other immediates and register values:
                        // Store 8 bytes from the register, then zero-pad to elem_size if needed
                        const elem_reg = try self.ensureInGeneralReg(elem_loc);
                        try self.emitStore(.w64, heap_ptr, elem_heap_offset, elem_reg);
                        self.codegen.freeGeneral(elem_reg);

                        // Zero-pad remaining bytes if elem_size > 8
                        if (elem_size > 8) {
                            const zero_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(zero_reg, 0);
                            var padded: u32 = 8;
                            while (padded < elem_size) : (padded += 8) {
                                const pad_offset = elem_heap_offset + @as(i32, @intCast(padded));
                                try self.emitStore(.w64, heap_ptr, pad_offset, zero_reg);
                            }
                            self.codegen.freeGeneral(zero_reg);
                        }
                    },
                }

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

        /// Generate code for a record literal
        fn generateRecord(self: *Self, rec: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;

            // Validate layout index before use
            if (@intFromEnum(rec.record_layout) >= ls.layouts.len()) {
                unreachable;
            }

            // Get the record layout
            const record_layout = ls.getLayout(rec.record_layout);
            if (record_layout.tag != .record) {
                unreachable;
            }

            const record_data = ls.getRecordData(record_layout.data.record.idx);
            const stack_size = record_data.size;

            // Zero-sized records don't need storage
            if (stack_size == 0) {
                return .{ .immediate_i64 = 0 };
            }

            // Allocate stack space for the record
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Get field expressions
            const field_exprs = self.store.getExprSpan(rec.fields);

            // Copy each field to its offset within the record
            // Fields are sorted by alignment descending, then alphabetically - matching the layout
            for (field_exprs, 0..) |field_expr_id, i| {
                const field_offset = ls.getRecordFieldOffset(record_layout.data.record.idx, @intCast(i));
                const field_size = ls.getRecordFieldSize(record_layout.data.record.idx, @intCast(i));
                const field_loc = try self.generateExpr(field_expr_id);
                try self.copyBytesToStackOffset(base_offset + @as(i32, @intCast(field_offset)), field_loc, field_size);
            }

            return .{ .stack = .{ .offset = base_offset, .size = ValueSize.fromByteCount(@min(stack_size, 8)) } };
        }

        /// Generate code for field access
        /// Determine the size of a value from its ValueLocation alone.
        fn valueSizeFromLoc(_: *Self, loc: ValueLocation) u32 {
            return switch (loc) {
                .stack_str => roc_str_size,
                .list_stack => roc_list_size,
                .stack_i128, .immediate_i128 => 16,
                .immediate_i64, .general_reg, .stack, .float_reg, .immediate_f64 => 8,
                else => 8,
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
            if (self.layout_store) |ls| {
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
            if (field_size == 16) {
                return .{ .stack_i128 = field_base };
            }
            return .{ .stack = .{ .offset = field_base, .size = ValueSize.fromByteCount(field_size) } };
        }

        fn generateFieldAccess(self: *Self, access: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;

            // Generate code for the record expression
            const record_loc = try self.generateExpr(access.record_expr);

            // Get the record layout to find field offset and size
            const record_layout = ls.getLayout(access.record_layout);
            if (record_layout.tag != .record) {
                // Cross-module layout index mismatch: the record_layout index from
                // a builtin module may map to a different layout in the current module.
                // When field_idx is 0, just return the value as-is (first field = whole value).
                if (access.field_idx == 0) {
                    return record_loc;
                }
                // Any other field access on non-record is a compiler bug
                unreachable;
            }

            const field_offset = ls.getRecordFieldOffset(record_layout.data.record.idx, access.field_idx);
            const field_size = ls.getRecordFieldSize(record_layout.data.record.idx, access.field_idx);
            const field_layout_idx = ls.getRecordFieldLayout(record_layout.data.record.idx, access.field_idx);

            // Return location pointing to the field within the record
            return switch (record_loc) {
                .stack_str => |s| {
                    const field_base = s + @as(i32, @intCast(field_offset));
                    return self.fieldLocationFromLayout(field_base, field_size, field_layout_idx);
                },
                .stack => |s| {
                    const field_base = s.offset + @as(i32, @intCast(field_offset));
                    return self.fieldLocationFromLayout(field_base, field_size, field_layout_idx);
                },
                .stack_i128 => |s| {
                    // Record itself is i128-sized, field access within it
                    const field_base = s + @as(i32, @intCast(field_offset));
                    return self.fieldLocationFromLayout(field_base, field_size, field_layout_idx);
                },
                .general_reg => |reg| {
                    // Record in register - only valid for small records (<=8 bytes)
                    // A record with a 16-byte field cannot fit in a register
                    if (field_size > 8) {
                        unreachable;
                    }
                    if (field_offset == 0) {
                        return .{ .general_reg = reg };
                    } else {
                        const result_reg = try self.allocTempGeneral();
                        try self.emitLsrImm(.w64, result_reg, reg, @intCast(field_offset * 8));
                        self.codegen.freeGeneral(reg);
                        return .{ .general_reg = result_reg };
                    }
                },
                .immediate_i64 => |val| {
                    if (field_size > 8) {
                        unreachable;
                    }
                    const shifted = val >> @intCast(field_offset * 8);
                    return .{ .immediate_i64 = shifted };
                },
                else => unreachable,
            };
        }

        /// Generate code for a tuple literal
        fn generateTuple(self: *Self, tup: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;

            // Get the tuple layout
            const tuple_layout = ls.getLayout(tup.tuple_layout);
            if (tuple_layout.tag != .tuple) {
                unreachable;
            }

            const tuple_data = ls.getTupleData(tuple_layout.data.tuple.idx);
            const stack_size = tuple_data.size;

            // Zero-sized tuples don't need storage
            if (stack_size == 0) {
                return .{ .immediate_i64 = 0 };
            }

            // Allocate stack space for the tuple
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Get element expressions
            const elem_exprs = self.store.getExprSpan(tup.elems);

            // Copy each element to its offset within the tuple
            // Use ByOriginalIndex functions because elem_exprs is in source order,
            // but the layout store has elements sorted by alignment
            for (elem_exprs, 0..) |elem_expr_id, i| {
                const elem_offset = ls.getTupleElementOffsetByOriginalIndex(tuple_layout.data.tuple.idx, @intCast(i));
                const elem_size = ls.getTupleElementSizeByOriginalIndex(tuple_layout.data.tuple.idx, @intCast(i));
                const elem_loc = try self.generateExpr(elem_expr_id);
                try self.copyBytesToStackOffset(base_offset + @as(i32, @intCast(elem_offset)), elem_loc, elem_size);
            }

            return .{ .stack = .{ .offset = base_offset, .size = ValueSize.fromByteCount(@min(stack_size, 8)) } };
        }

        /// Generate code for tuple element access
        fn generateTupleAccess(self: *Self, access: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;

            // Generate code for the tuple expression
            const tuple_loc = try self.generateExpr(access.tuple_expr);

            // Get the tuple layout to find element offset and size
            const tuple_layout = ls.getLayout(access.tuple_layout);
            if (tuple_layout.tag != .tuple) {
                unreachable;
            }

            const elem_offset = ls.getTupleElementOffset(tuple_layout.data.tuple.idx, access.elem_idx);
            const elem_size = ls.getTupleElementSize(tuple_layout.data.tuple.idx, access.elem_idx);

            // Return location pointing to the element within the tuple
            return switch (tuple_loc) {
                .stack_str => |s| {
                    const elem_base = s + @as(i32, @intCast(elem_offset));
                    // Return stack_i128 for 16-byte elements (Dec/i128/u128)
                    if (elem_size == 16) {
                        return .{ .stack_i128 = elem_base };
                    }
                    return .{ .stack = .{ .offset = elem_base, .size = ValueSize.fromByteCount(elem_size) } };
                },
                .stack => |s| {
                    const elem_base = s.offset + @as(i32, @intCast(elem_offset));
                    // Return stack_i128 for 16-byte elements (Dec/i128/u128)
                    if (elem_size == 16) {
                        return .{ .stack_i128 = elem_base };
                    }
                    return .{ .stack = .{ .offset = elem_base, .size = ValueSize.fromByteCount(elem_size) } };
                },
                .stack_i128 => |s| {
                    // Tuple itself is i128-sized, element access within it
                    const elem_base = s + @as(i32, @intCast(elem_offset));
                    if (elem_size == 16) {
                        return .{ .stack_i128 = elem_base };
                    }
                    return .{ .stack = .{ .offset = elem_base, .size = ValueSize.fromByteCount(elem_size) } };
                },
                .general_reg => |reg| {
                    // Tuple in register - only valid for small tuples (<=8 bytes)
                    // A tuple with a 16-byte element cannot fit in a register
                    if (elem_size > 8) {
                        unreachable;
                    }
                    if (elem_offset == 0) {
                        return .{ .general_reg = reg };
                    } else {
                        const result_reg = try self.allocTempGeneral();
                        try self.emitLsrImm(.w64, result_reg, reg, @intCast(elem_offset * 8));
                        self.codegen.freeGeneral(reg);
                        return .{ .general_reg = result_reg };
                    }
                },
                .immediate_i64 => |val| {
                    if (elem_size > 8) {
                        unreachable;
                    }
                    const shifted = val >> @intCast(elem_offset * 8);
                    return .{ .immediate_i64 = shifted };
                },
                else => unreachable,
            };
        }

        /// Generate code for a zero-argument tag (just discriminant)
        fn generateZeroArgTag(self: *Self, tag: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;

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
            const ls = self.layout_store orelse unreachable;

            // Get the union layout
            const union_layout = ls.getLayout(tag.union_layout);
            if (union_layout.tag != .tag_union) {
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
                    break :blk if (pl_val.tag == .tuple) pl_val.data.tuple.idx else null;
                } else null;

                for (arg_exprs, 0..) |arg_expr_id, arg_i| {
                    const arg_loc = try self.generateExpr(arg_expr_id);
                    const elem_offset: i32 = if (payload_tuple) |tuple_idx|
                        @intCast(ls.getTupleElementOffsetByOriginalIndex(tuple_idx, @intCast(arg_i)))
                    else
                        @as(i32, @intCast(arg_i)) * 8;
                    const elem_size: u32 = if (payload_tuple) |tuple_idx| blk: {
                        const elem_layout = ls.getTupleElementLayoutByOriginalIndex(tuple_idx, @intCast(arg_i));
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
                .lambda_code => {
                    // lambda_code should not be copied to stack - it's only used for calling
                    unreachable;
                },
                .closure_value => |cv| {
                    // Copy the closure value from stack
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, cv.stack_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
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
                .lambda_code => |lc| {
                    // Store the code offset as an 8-byte pointer value
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @bitCast(@as(i64, @intCast(lc.code_offset))));
                    try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    self.codegen.freeGeneral(reg);
                    return;
                },
                .closure_value => |cv| {
                    // Copy the closure value from its stack location
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, cv.stack_offset);
                    try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    self.codegen.freeGeneral(reg);
                    return;
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
                try self.codegen.emit.ldurbRegMem(dst, base_reg, @intCast(offset));
            } else {
                try self.codegen.emit.movzxBRegMem(dst, base_reg, offset);
            }
        }

        /// Load halfword (16-bit, zero-extended) from base+offset into register
        fn emitLoadW16(self: *Self, dst: GeneralReg, base_reg: GeneralReg, offset: i32) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.ldurhRegMem(dst, base_reg, @intCast(offset));
            } else {
                try self.codegen.emit.movzxWRegMem(dst, base_reg, offset);
            }
        }

        /// Store byte (8-bit) from register to base+offset
        fn emitStoreW8(self: *Self, base_reg: GeneralReg, offset: i32, src: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.sturbRegMem(src, base_reg, @intCast(offset));
            } else {
                try self.codegen.emit.movMemReg(.w8, base_reg, offset, src);
            }
        }

        /// Store halfword (16-bit) from register to base+offset
        fn emitStoreW16(self: *Self, base_reg: GeneralReg, offset: i32, src: GeneralReg) !void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.sturhRegMem(src, base_reg, @intCast(offset));
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
                // mov dst, a; sub dst, b; mov zero, 0; cmov below, dst, zero
                if (dst != a) try self.codegen.emit.movRegReg(.w64, dst, a);
                try self.codegen.emit.subRegReg(.w64, dst, b);
                const zero_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(zero_reg, 0);
                try self.codegen.emit.cmovcc(.below, .w64, dst, zero_reg);
                self.codegen.freeGeneral(zero_reg);
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
                // Large string: needs heap allocation
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

            // Get element layout and size.
            // Cross-module layout resolution can produce incorrect elem_layout indices
            // (e.g., for builtin functions like List.fold where the for loop's elem_layout
            // refers to a layout index in the wrong module context). When this happens,
            // derive the correct element layout from the list expression's layout.
            const ls = self.layout_store orelse unreachable;
            const effective_elem_layout: layout.Idx = blk: {
                const stored_layout = ls.getLayout(for_loop.elem_layout);
                const stored_size = ls.layoutSizeAlign(stored_layout).size;
                if (stored_size > 0 or for_loop.elem_layout == .bool) {
                    // Stored layout has non-zero size, trust it
                    break :blk for_loop.elem_layout;
                }
                // Stored layout is ZST — try to derive the correct element layout
                if (self.getExprLayout(for_loop.list_expr)) |list_layout_idx| {
                    const list_layout = ls.getLayout(list_layout_idx);
                    if (list_layout.tag == .list) {
                        const derived = list_layout.data.list;
                        const derived_layout = ls.getLayout(derived);
                        const derived_size = ls.layoutSizeAlign(derived_layout).size;
                        if (derived_size > 0) {
                            break :blk derived;
                        }
                    }
                }
                // Also check the element pattern's layout_idx
                const elem_pattern = self.store.getPattern(for_loop.elem_pattern);
                switch (elem_pattern) {
                    .bind => |bind| {
                        const pat_layout = ls.getLayout(bind.layout_idx);
                        const pat_size = ls.layoutSizeAlign(pat_layout).size;
                        if (pat_size > 0) {
                            break :blk bind.layout_idx;
                        }
                    },
                    .wildcard => {}, // Fall through to use for_loop.elem_layout
                    // Other patterns not valid for for loop element
                    else => unreachable,
                }
                break :blk for_loop.elem_layout;
            };
            const elem_layout = ls.getLayout(effective_elem_layout);
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

            // Bind the element to the pattern, passing the element layout so list patterns
            // can use the correct inner element size (the pattern's stored elem_layout may be wrong)
            // For ZST elements, bind to immediate 0 (no actual data)
            const elem_loc: ValueLocation = if (is_zst) .{ .immediate_i64 = 0 } else self.stackLocationForLayout(effective_elem_layout, elem_slot);
            try self.bindPatternWithLayout(for_loop.elem_pattern, elem_loc, effective_elem_layout);

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
            // NOTE: This may call C functions which clobber all caller-saved registers
            // The body may contain reassignments that update mutable variables
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

            // We must be inside a compileLambdaAsProc — early returns require the
            // jump-to-epilogue infrastructure that compileLambdaAsProc sets up.
            const ret_layout = self.early_return_ret_layout orelse unreachable;
            // Move the value to the return register (or copy to return pointer)
            if (self.ret_ptr_slot) |ret_slot| {
                try self.copyResultToReturnPointer(value_loc, ret_layout, ret_slot);
            } else {
                try self.moveToReturnRegisterWithLayout(value_loc, ret_layout);
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

            // Condition was false: call roc_crashed via RocOps
            try self.emitRocCrash("expect failed");

            // Patch the skip jump to land here
            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());

            // Evaluate and return the body
            return try self.generateExpr(expect_expr.body);
        }

        /// Emit a roc_crashed call via RocOps with a static message.
        /// Used for runtime_error expressions (dead code paths that should
        /// never execute, e.g. the Err branch of `?` at the top level).
        fn emitRocCrash(self: *Self, msg: []const u8) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            // Allocate a 16-byte stack slot for the RocCrashed struct { utf8_bytes, len }
            const crashed_slot = self.codegen.allocStackSlot(16);

            const msg_ptr_val: i64 = @bitCast(@as(u64, @intFromPtr(msg.ptr)));
            const msg_len_val: i64 = @bitCast(@as(u64, msg.len));

            {
                const base_reg = frame_ptr;
                const tmp = try self.allocTempGeneral();

                // Store utf8_bytes pointer at offset 0
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emitLoadImm(tmp, msg_ptr_val);
                    try self.codegen.emit.strRegMemSoff(.w64, tmp, base_reg, crashed_slot);
                } else {
                    try self.codegen.emit.movRegImm64(tmp, @bitCast(@as(u64, @intFromPtr(msg.ptr))));
                    try self.codegen.emit.movMemReg(.w64, base_reg, crashed_slot, tmp);
                }

                // Store len at offset 8
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

            // Get float value as u64 bits
            const val_bits_reg = try self.ensureInGeneralReg(val_loc);
            const is_f32: bool = (fts.float_precision == .f32);

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
                .lambda_code => unreachable, // Dec is not a lambda
                .closure_value => unreachable, // Dec is not a closure
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

        /// Generate code for discriminant switch
        fn generateDiscriminantSwitch(self: *Self, ds: anytype) Allocator.Error!ValueLocation {
            // Get the value and read its discriminant
            const value_loc = try self.generateExpr(ds.value);

            const ls = self.layout_store orelse unreachable;
            const union_layout = ls.getLayout(ds.union_layout);

            // Load discriminant value into a register.
            // disc_use_w32: Use .w32 for discriminant loads when .w64 would read past the tag union.
            // Discriminants are at most 4 bytes, so .w32 is always sufficient.
            const disc_info: struct { reg: GeneralReg, use_w32: bool } = if (union_layout.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
                const disc_offset: i32 = @intCast(tu_data.discriminant_offset);
                const use_w32 = (disc_offset + 8 > @as(i32, @intCast(tu_data.size)));

                const base_offset: i32 = switch (value_loc) {
                    .stack => |s| s.offset,
                    .stack_str => |off| off,
                    else => unreachable,
                };

                const reg = try self.allocTempGeneral();
                if (use_w32) {
                    try self.emitLoad(.w32, reg, frame_ptr, base_offset + disc_offset);
                } else {
                    try self.emitLoad(.w64, reg, frame_ptr, base_offset + disc_offset);
                }
                break :blk .{ .reg = reg, .use_w32 = use_w32 };
            } else if (union_layout.tag == .scalar or union_layout.tag == .zst) blk: {
                // For scalar layouts (e.g., Bool, enums with no payloads),
                // the value itself IS the discriminant.
                break :blk .{ .reg = try self.ensureInGeneralReg(value_loc), .use_w32 = false };
            } else if (union_layout.tag == .box) blk: {
                // Boxed nominal type: check the inner layout to determine how to read
                // the discriminant.
                const inner_layout = ls.getLayout(union_layout.data.box);
                if (inner_layout.tag == .scalar or inner_layout.tag == .zst) {
                    // Box of scalar/ZST (e.g., Color := [Red, Green, Blue]):
                    // The value IS the discriminant directly — not a heap pointer.
                    // No-payload tag unions are never actually heap-allocated.
                    break :blk .{ .reg = try self.ensureInGeneralReg(value_loc), .use_w32 = false };
                } else if (inner_layout.tag == .tag_union) {
                    // Box of tag union with payloads: dereference the box pointer,
                    // then read the discriminant from heap memory.
                    const box_ptr_reg = try self.ensureInGeneralReg(value_loc);
                    const tu_data = ls.getTagUnionData(inner_layout.data.tag_union.idx);
                    const disc_offset: i32 = @intCast(tu_data.discriminant_offset);
                    const use_w32 = (disc_offset + 8 > @as(i32, @intCast(tu_data.size)));
                    const disc_reg = try self.allocTempGeneral();
                    if (use_w32) {
                        try self.emitLoad(.w32, disc_reg, box_ptr_reg, disc_offset);
                    } else {
                        try self.emitLoad(.w64, disc_reg, box_ptr_reg, disc_offset);
                    }
                    self.codegen.freeGeneral(box_ptr_reg);
                    break :blk .{ .reg = disc_reg, .use_w32 = use_w32 };
                } else {
                    unreachable;
                }
            } else {
                unreachable;
            };
            const disc_reg = disc_info.reg;
            const disc_use_w32 = disc_info.use_w32;

            // Get the branches
            const branches = self.store.getExprSpan(ds.branches);
            if (branches.len == 0) {
                self.codegen.freeGeneral(disc_reg);
                unreachable;
            }

            // For single branch, just return it
            if (branches.len == 1) {
                self.codegen.freeGeneral(disc_reg);
                return try self.generateExpr(branches[0]);
            }

            // TODO: Implement full switch with jump table for many branches
            // For now, use if-else chain for small number of branches

            var exit_patches = std.ArrayList(usize).empty;
            defer exit_patches.deinit(self.allocator);

            // Track result characteristics to determine slot size and return type
            var result_is_str = false;
            var result_is_i128 = false;

            // Determine result slot size. For tag unions, use the variant payloads.
            // For scalars, we defer allocation until the first branch is generated
            // so we can use the actual result ValueLocation to determine the size.
            var result_slot_size: u32 = 0;
            var result_slot: i32 = 0;

            if (union_layout.tag == .tag_union) {
                const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
                const variants = ls.getTagUnionVariants(tu_data);
                var max_size: u32 = 8;
                for (0..variants.len) |vi| {
                    const variant = variants.get(vi);
                    const vl = ls.getLayout(variant.payload_layout);
                    const vs = ls.layoutSizeAlign(vl).size;
                    if (vs > max_size) max_size = vs;
                }
                result_slot_size = max_size;
                result_slot = self.codegen.allocStackSlot(result_slot_size);
            }

            // Spill disc_reg to stack so branch bodies can freely use caller-saved regs.
            const disc_slot = self.codegen.allocStackSlot(if (disc_use_w32) 4 else 8);
            if (disc_use_w32) {
                try self.emitStore(.w32, frame_ptr, disc_slot, disc_reg);
            } else {
                try self.emitStore(.w64, frame_ptr, disc_slot, disc_reg);
            }
            self.codegen.freeGeneral(disc_reg);

            for (branches, 0..) |branch_expr, i| {
                if (i < branches.len - 1) {
                    // Reload discriminant — previous branch body may have clobbered it
                    const cmp_reg = try self.allocTempGeneral();
                    if (disc_use_w32) {
                        try self.emitLoad(.w32, cmp_reg, frame_ptr, disc_slot);
                    } else {
                        try self.emitLoad(.w64, cmp_reg, frame_ptr, disc_slot);
                    }

                    // Compare discriminant with branch index
                    try self.emitCmpImm(cmp_reg, @intCast(i));
                    self.codegen.freeGeneral(cmp_reg);
                    const skip_patch = try self.emitJumpIfNotEqual();

                    // Generate branch body
                    const branch_loc = try self.generateExpr(branch_expr);
                    if (branch_loc == .stack_i128 or branch_loc == .immediate_i128) result_is_i128 = true;
                    if (branch_loc == .stack_str) result_is_str = true;

                    // For scalar switches, allocate result slot after first branch
                    // so we know the actual result size from the ValueLocation.
                    if (result_slot_size == 0) {
                        result_slot_size = self.valueSizeFromLoc(branch_loc);
                        result_slot = self.codegen.allocStackSlot(result_slot_size);
                    }

                    try self.storeResultToSlot(result_slot, branch_loc, result_slot_size);

                    // Jump to end
                    const exit_patch = try self.emitJumpUnconditional();
                    try exit_patches.append(self.allocator, exit_patch);

                    // Patch the skip jump to current location
                    const skip_offset = self.codegen.currentOffset();
                    self.codegen.patchJump(skip_patch, skip_offset);
                } else {
                    // Last branch is the default
                    const branch_loc = try self.generateExpr(branch_expr);
                    if (branch_loc == .stack_i128 or branch_loc == .immediate_i128) result_is_i128 = true;
                    if (branch_loc == .stack_str) result_is_str = true;

                    // For scalar switches, allocate result slot after first branch
                    if (result_slot_size == 0) {
                        result_slot_size = self.valueSizeFromLoc(branch_loc);
                        result_slot = self.codegen.allocStackSlot(result_slot_size);
                    }

                    try self.storeResultToSlot(result_slot, branch_loc, result_slot_size);
                }
            }

            // Patch all exit jumps to current location
            const end_offset = self.codegen.currentOffset();
            for (exit_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }

            // Return with appropriate value location type
            if (result_is_str or result_slot_size == roc_str_size) {
                return .{ .stack_str = result_slot };
            } else if (result_is_i128 or result_slot_size == 16) {
                return .{ .stack_i128 = result_slot };
            } else {
                return .{ .stack = .{ .offset = result_slot } };
            }
        }

        /// Extract the payload from a tag union value.
        /// The payload is always at offset 0 in the tag union memory.
        fn generateTagPayloadAccess(self: *Self, tpa: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;

            // Generate the tag union value
            const value_loc = try self.generateExpr(tpa.value);

            const union_layout = ls.getLayout(tpa.union_layout);
            const payload_layout = ls.getLayout(tpa.payload_layout);
            const payload_size = ls.layoutSizeAlign(payload_layout).size;

            if (union_layout.tag == .tag_union) {
                // Payload is at offset 0 within the stack-allocated tag union
                const base_offset: i32 = switch (value_loc) {
                    .stack => |s| s.offset,
                    .stack_str => |off| off,
                    else => unreachable,
                };
                return self.fieldLocationFromLayout(base_offset, payload_size, tpa.payload_layout);
            } else if (union_layout.tag == .box) {
                // Boxed tag union: dereference the pointer, then copy payload from heap
                const inner_layout = ls.getLayout(union_layout.data.box);
                if (inner_layout.tag == .tag_union) {
                    const box_ptr_reg = try self.ensureInGeneralReg(value_loc);

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
                    return value_loc;
                }
            } else if (union_layout.tag == .scalar or union_layout.tag == .zst) {
                // Scalar/ZST unions: the value itself is the payload (no indirection)
                return value_loc;
            } else {
                unreachable;
            }
        }

        /// Helper to store a result to a stack slot
        fn storeResultToSlot(self: *Self, slot: i32, loc: ValueLocation, slot_size: u32) Allocator.Error!void {
            const temp_reg = try self.allocTempGeneral();
            switch (loc) {
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(val));
                    try self.emitStore(.w64, frame_ptr, slot, temp_reg);
                },
                .immediate_i128 => |val| {
                    // Store low 64 bits
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(@as(i64, @truncate(val))));
                    try self.emitStore(.w64, frame_ptr, slot, temp_reg);
                    // Store high 64 bits
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(@as(i64, @truncate(val >> 64))));
                    try self.emitStore(.w64, frame_ptr, slot + 8, temp_reg);
                },
                .stack => |s| {
                    const off = s.offset;
                    // Copy slot_size bytes (in 8-byte chunks) to handle tag unions
                    // and other multi-word stack values correctly.
                    var copied: u32 = 0;
                    while (copied < slot_size) {
                        const chunk_off: i32 = @intCast(copied);
                        try self.emitLoad(.w64, temp_reg, frame_ptr, off + chunk_off);
                        try self.emitStore(.w64, frame_ptr, slot + chunk_off, temp_reg);
                        copied += 8;
                    }
                },
                .stack_i128 => |off| {
                    // Copy 16 bytes (two 8-byte chunks)
                    try self.emitLoad(.w64, temp_reg, frame_ptr, off);
                    try self.emitStore(.w64, frame_ptr, slot, temp_reg);
                    try self.emitLoad(.w64, temp_reg, frame_ptr, off + 8);
                    try self.emitStore(.w64, frame_ptr, slot + 8, temp_reg);
                },
                .general_reg => |reg| {
                    try self.emitStore(.w64, frame_ptr, slot, reg);
                },
                .lambda_code, .closure_value => {
                    // Lambda/closure values must be called/dispatched before storing.
                    // Reaching here indicates a compiler bug.
                    unreachable;
                },
                .stack_str => |off| {
                    var offset: u32 = 0;
                    while (offset < slot_size) : (offset += 8) {
                        const src_off = off + @as(i32, @intCast(offset));
                        try self.emitLoad(.w64, temp_reg, frame_ptr, src_off);
                        try self.emitStore(.w64, frame_ptr, slot + @as(i32, @intCast(offset)), temp_reg);
                    }
                },
                .list_stack => |ls_info| {
                    var offset: u32 = 0;
                    while (offset < slot_size) : (offset += 8) {
                        const src_off = ls_info.struct_offset + @as(i32, @intCast(offset));
                        try self.emitLoad(.w64, temp_reg, frame_ptr, src_off);
                        try self.emitStore(.w64, frame_ptr, slot + @as(i32, @intCast(offset)), temp_reg);
                    }
                },
                else => unreachable,
            }
            self.codegen.freeGeneral(temp_reg);
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

        /// Emit an unconditional jump
        fn emitJumpUnconditional(self: *Self) Allocator.Error!usize {
            return try self.codegen.emitJump();
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
                const b = stmt.binding();
                // Generate code for the expression
                const expr_loc = try self.generateExpr(b.expr);
                // Get the expression's layout (for mutable variable binding with correct size)
                const expr_layout = self.getExprLayout(b.expr);
                // Bind the result to the pattern, using expr layout for mutable vars
                try self.bindPatternWithLayout(b.pattern, expr_loc, expr_layout);
            }

            // Generate the final expression
            return self.generateExpr(block.final_expr);
        }

        /// Bind a value to a pattern
        /// expr_layout_override: Optional layout from the expression being bound. If provided,
        /// this is used for mutable variables instead of the pattern's layout_idx (which may be wrong).
        fn bindPattern(self: *Self, pattern_id: LirPatternId, value_loc: ValueLocation) Allocator.Error!void {
            return self.bindPatternWithLayout(pattern_id, value_loc, null);
        }

        /// Bind a value to a pattern with an optional expression layout override
        fn bindPatternWithLayout(self: *Self, pattern_id: LirPatternId, value_loc: ValueLocation, expr_layout_override: ?layout.Idx) Allocator.Error!void {
            const pattern = self.store.getPattern(pattern_id);

            switch (pattern) {
                .bind => |bind| {
                    const symbol_key: u64 = @bitCast(bind.symbol);

                    // Check if this is a reassignable (mutable) variable
                    if (bind.symbol.ident_idx.attributes.reassignable) {
                        // Mutable variables need fixed stack slots for runtime updates
                        if (self.mutable_var_slots.get(symbol_key)) |var_info| {
                            // Re-binding: copy new value to the fixed slot at runtime
                            try self.copyBytesToStackOffset(var_info.slot, value_loc, var_info.size);
                            // symbol_locations already points to the fixed slot, don't change it
                        } else {
                            // First binding: allocate a fixed slot and copy value there
                            const ls = self.layout_store orelse unreachable;

                            // Use the pattern's layout for the mutable variable size.
                            // The pattern's layout_idx reflects the variable's declared/inferred type
                            // which includes all unification constraints (e.g., from both the initial
                            // assignment and subsequent rebindings). The expression's layout
                            // (expr_layout_override) can be wrong when the expression is wrapped in
                            // a call whose ret_layout wasn't properly resolved (e.g., List.with_capacity
                            // wrapped in a call with ret_layout=u8 instead of List).
                            const size: u32 = blk: {
                                const layout_val = ls.getLayout(bind.layout_idx);
                                break :blk ls.layoutSizeAlign(layout_val).size;
                            };

                            // Allocate a fixed stack slot for this mutable variable
                            const fixed_slot = self.codegen.allocStackSlot(size);

                            // Copy the initial value to the fixed slot
                            try self.copyBytesToStackOffset(fixed_slot, value_loc, size);

                            // Record the fixed slot info for future re-bindings
                            try self.mutable_var_slots.put(symbol_key, .{ .slot = fixed_slot, .size = size });

                            // Point symbol_locations to the fixed slot
                            // IMPORTANT: Preserve the correct ValueLocation type so that
                            // subsequent code correctly handles multi-register values
                            if (value_loc == .list_stack) {
                                try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                    .struct_offset = fixed_slot,
                                    .data_offset = 0,
                                    .num_elements = 0,
                                } });
                            } else if (value_loc == .stack_str or size == roc_str_size) {
                                // Strings are roc_str_size bytes - preserve .stack_str so return handling
                                // loads all 3 registers (ptr, len, capacity)
                                try self.symbol_locations.put(symbol_key, .{ .stack_str = fixed_slot });
                            } else if (value_loc == .stack_i128 or size == 16) {
                                // Dec/i128 values are 16 bytes - preserve .stack_i128
                                try self.symbol_locations.put(symbol_key, .{ .stack_i128 = fixed_slot });
                            } else {
                                try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = fixed_slot } });
                            }
                        }
                    } else {
                        // Non-mutable: just record the location as before
                        try self.symbol_locations.put(symbol_key, value_loc);
                    }
                },
                .wildcard => {
                    // Ignore the value
                },
                .record => |rec| {
                    // Record destructuring: bind each field pattern
                    const ls = self.layout_store orelse return;
                    const record_layout = ls.getLayout(rec.record_layout);
                    if (record_layout.tag != .record) return;

                    const field_patterns = self.store.getPatternSpan(rec.fields);

                    // Get the base offset of the record
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |s| s.offset,
                        .stack_str => |off| off,
                        else => return, // Can't destructure non-stack values
                    };

                    // Bind each field
                    for (field_patterns, 0..) |field_pattern_id, i| {
                        const field_offset = ls.getRecordFieldOffset(record_layout.data.record.idx, @intCast(i));

                        // Create a location for the field using the correct layout type
                        const field_layout_idx = ls.getRecordFieldLayout(record_layout.data.record.idx, @intCast(i));
                        const field_loc: ValueLocation = self.stackLocationForLayout(field_layout_idx, base_offset + @as(i32, @intCast(field_offset)));

                        try self.bindPattern(field_pattern_id, field_loc);
                    }
                },
                .tuple => |tup| {
                    // Tuple destructuring: bind each element pattern
                    const ls = self.layout_store orelse return;
                    const tuple_layout = ls.getLayout(tup.tuple_layout);
                    if (tuple_layout.tag != .tuple) return;

                    const elem_patterns = self.store.getPatternSpan(tup.elems);

                    // Get the base offset of the tuple
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |s| s.offset,
                        .stack_str => |off| off,
                        else => return, // Can't destructure non-stack values
                    };

                    // Bind each element (patterns are in source order, so use ByOriginalIndex)
                    for (elem_patterns, 0..) |elem_pattern_id, i| {
                        const elem_offset = ls.getTupleElementOffsetByOriginalIndex(tuple_layout.data.tuple.idx, @intCast(i));

                        // Create a location for the element using the correct layout type
                        const elem_layout_idx = ls.getTupleElementLayoutByOriginalIndex(tuple_layout.data.tuple.idx, @intCast(i));
                        const elem_loc: ValueLocation = self.stackLocationForLayout(elem_layout_idx, base_offset + @as(i32, @intCast(elem_offset)));

                        try self.bindPattern(elem_pattern_id, elem_loc);
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
                        else => return,
                    };

                    const prefix_patterns = self.store.getPatternSpan(lst.prefix);

                    // For each prefix element, we need to load from the list data
                    // List layout: ptr at offset 0, len at offset 8, capacity at offset 16
                    // Elements are at ptr[0], ptr[1], etc.

                    // Get element size from layout
                    // Use element layout from the expression override if it's a list,
                    // otherwise use the pattern's element layout.
                    const ls = self.layout_store orelse return;
                    const elem_layout = if (expr_layout_override) |override_idx| blk: {
                        const override_layout = ls.getLayout(override_idx);
                        if (override_layout.tag == .list) {
                            break :blk ls.getLayout(override_layout.data.list);
                        }
                        break :blk ls.getLayout(lst.elem_layout);
                    } else ls.getLayout(lst.elem_layout);
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
                        const elem_loc: ValueLocation = self.stackLocationForLayout(lst.elem_layout, elem_slot);
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
                            try self.bindPattern(suf_pattern_id, self.stackLocationForLayout(lst.elem_layout, suf_slot));
                        }

                        self.codegen.freeGeneral(suf_ptr_reg);
                    }

                    // Handle rest pattern (the remaining list after prefix, before suffix)
                    if (!lst.rest.isNone()) {
                        // Create a new RocList for the remaining elements
                        // RocList layout: bytes (ptr), length (usize), capacity_or_alloc_ptr (usize)
                        const rest_slot = self.codegen.allocStackSlot(roc_str_size);

                        const prefix_count = @as(u32, @intCast(prefix_patterns.len));
                        const suffix_count_rest = @as(u32, @intCast(suffix_patterns.len));
                        const prefix_byte_offset = prefix_count * elem_size;

                        // Calculate rest pointer: original_ptr + prefix_len * elem_size
                        const rest_ptr_reg = try self.allocTempGeneral();
                        if (prefix_byte_offset == 0) {
                            // No offset needed, just copy the pointer
                            try self.codegen.emit.movRegReg(.w64, rest_ptr_reg, list_ptr_reg);
                        } else {
                            // Add offset to pointer: rest_ptr = list_ptr + prefix_byte_offset
                            try self.emitAddImm(rest_ptr_reg, list_ptr_reg, @intCast(prefix_byte_offset));
                        }

                        // Store rest pointer at rest_slot + 0
                        try self.emitStore(.w64, frame_ptr, rest_slot, rest_ptr_reg);
                        self.codegen.freeGeneral(rest_ptr_reg);

                        // Load original length from base_offset + 8
                        const len_reg = try self.allocTempGeneral();
                        try self.emitLoad(.w64, len_reg, frame_ptr, base_offset + 8);

                        // Calculate rest length: original_length - prefix_count - suffix_count
                        const total_fixed = prefix_count + suffix_count_rest;
                        if (total_fixed > 0) {
                            try self.emitSubImm(.w64, len_reg, len_reg, @intCast(total_fixed));
                        }

                        // Store rest length at rest_slot + 8
                        try self.emitStore(.w64, frame_ptr, rest_slot + 8, len_reg);

                        // For capacity, use the same length (this is a slice view, not a copy)
                        try self.emitStore(.w64, frame_ptr, rest_slot + 16, len_reg);
                        self.codegen.freeGeneral(len_reg);

                        // Bind the rest pattern to the new list slot
                        try self.bindPattern(lst.rest, .{ .list_stack = .{
                            .struct_offset = rest_slot,
                            .data_offset = 0,
                            .num_elements = 0,
                        } });
                    }

                    self.codegen.freeGeneral(list_ptr_reg);
                },
                .tag => |tag_pat| {
                    // Tag destructuring: bind payload patterns
                    // For lambda parameters, the tag match is already known, just bind the payload
                    const arg_patterns = self.store.getPatternSpan(tag_pat.args);
                    if (arg_patterns.len == 0) return;

                    const ls = self.layout_store orelse return;
                    const union_layout = ls.getLayout(tag_pat.union_layout);
                    if (union_layout.tag != .tag_union) return;

                    const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
                    // Payload is always at offset 0, discriminant comes after
                    const payload_offset: i32 = 0;

                    // Get the base offset
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |s| s.offset,
                        .stack_str => |off| off,
                        else => return,
                    };

                    // Get the variant's payload layout to determine element offsets
                    const variants = ls.getTagUnionVariants(tu_data);
                    const variant = variants.get(tag_pat.discriminant);
                    const payload_layout = ls.getLayout(variant.payload_layout);

                    // For tags with single arg, bind directly at payload offset
                    // For tags with multiple args (tuples), need to use tuple element offsets
                    if (arg_patterns.len == 1) {
                        // Use the correct value location type based on payload layout
                        const payload_loc_offset = base_offset + payload_offset;
                        const arg_loc: ValueLocation = self.stackLocationForLayout(variant.payload_layout, payload_loc_offset);
                        try self.bindPattern(arg_patterns[0], arg_loc);
                    } else {
                        // Multiple args means payload is a tuple - get offsets from tuple layout
                        // Patterns are in source order, so use ByOriginalIndex
                        if (payload_layout.tag == .tuple) {
                            for (arg_patterns, 0..) |arg_pattern_id, i| {
                                const tuple_elem_offset = ls.getTupleElementOffsetByOriginalIndex(payload_layout.data.tuple.idx, @intCast(i));
                                const arg_offset = base_offset + payload_offset + @as(i32, @intCast(tuple_elem_offset));
                                const tuple_elem_layout_idx = ls.getTupleElementLayoutByOriginalIndex(payload_layout.data.tuple.idx, @intCast(i));
                                try self.bindPattern(arg_pattern_id, self.stackLocationForLayout(tuple_elem_layout_idx, arg_offset));
                            }
                        } else {
                            // Payload is not a tuple but we have multiple patterns - this shouldn't happen
                            // but handle gracefully by treating each pattern as having the same location
                            for (arg_patterns) |arg_pattern_id| {
                                const arg_loc: ValueLocation = .{ .stack = .{ .offset = base_offset + payload_offset } };
                                try self.bindPattern(arg_pattern_id, arg_loc);
                            }
                        }
                    }
                },
                else => {
                    // Literal patterns (int_literal, float_literal, str_literal) don't bind anything
                    // They are used for matching in match expressions, not for binding
                },
            }
        }

        /// Map a layout index to the correct ValueLocation for a value on the stack.
        /// Multi-word types (strings, i128/Dec, lists) need specific location variants
        /// so downstream code loads the correct number of bytes.
        fn stackLocationForLayout(self: *Self, layout_idx: layout.Idx, stack_offset: i32) ValueLocation {
            if (layout_idx == .i128 or layout_idx == .u128 or layout_idx == .dec)
                return .{ .stack_i128 = stack_offset };
            if (layout_idx == .str)
                return .{ .stack_str = stack_offset };
            const ls = self.layout_store orelse return .{ .stack = .{ .offset = stack_offset, .size = ValueSize.fromByteCount(self.getLayoutSize(layout_idx)) } };
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
        /// code is shifted by compileLambdaAsProc's deferred prologue pattern.
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

        /// After compileLambdaAsProc shifts its body by prepending a prologue,
        /// re-patch any internal BL/CALL instructions within the shifted range
        /// that target code outside the shifted range.
        ///
        /// When body bytes [body_start..body_end] are shifted forward by prologue_size:
        /// - BL instructions within the body are now at (old_pos + prologue_size)
        /// - Their targets outside the body are NOT shifted
        /// - So the relative offset in the BL instruction is now wrong by prologue_size
        fn repatchInternalCalls(self: *Self, body_start: usize, body_end: usize, prologue_size: usize) void {
            const buf = self.codegen.emit.buf.items;
            for (self.internal_call_patches.items) |*patch| {
                // Only adjust patches that were within the shifted body range
                if (patch.call_offset >= body_start and patch.call_offset < body_end) {
                    // Update the patch's recorded position (it shifted)
                    patch.call_offset += prologue_size;

                    // If the target is outside the shifted range (at or before body_start),
                    // we need to re-patch the instruction because the relative offset changed.
                    // Targets within the body shifted by the same amount, so they're fine.
                    // Note: target_offset == body_start covers self-recursive calls within
                    // compileLambdaAsProc, where the call targets the function's own entry point.
                    if (patch.target_offset <= body_start) {
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

        /// After compileLambdaAsProc shifts its body by prepending a prologue,
        /// re-patch any ADR/LEA instructions within the shifted range that
        /// compute lambda addresses targeting code outside the shifted range.
        fn repatchInternalAddrPatches(self: *Self, body_start: usize, body_end: usize, prologue_size: usize) void {
            const buf = self.codegen.emit.buf.items;
            for (self.internal_addr_patches.items) |*patch| {
                if (patch.instr_offset >= body_start and patch.instr_offset < body_end) {
                    // Update the patch's recorded position (it shifted)
                    patch.instr_offset += prologue_size;

                    // If the target is outside the shifted range (at or before body_start), re-patch.
                    // target_offset == body_start covers self-recursive calls.
                    if (patch.target_offset <= body_start) {
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

        /// Check if a lambda body returns a callable value (closure/lambda).
        /// Used to decide whether to inline or compile as a separate procedure.
        fn bodyReturnsCallable(self: *Self, body_expr_id: lir.LirExprId) bool {
            const body = self.store.getExpr(body_expr_id);
            return switch (body) {
                .lambda, .closure => true,
                .block => |block| self.bodyReturnsCallable(block.final_expr),
                .if_then_else => |ite| self.bodyReturnsCallable(ite.final_else),
                else => false,
            };
        }

        /// Check if any argument is a callable value (lambda, closure, or lookup
        /// that resolves to one). Higher-order function calls with callable args
        /// must be inlined because compiled procs can't dispatch opaque stack values.
        fn hasCallableArguments(self: *Self, args_span: anytype) bool {
            const args = self.store.getExprSpan(args_span);
            for (args) |arg_id| {
                const arg_expr = self.store.getExpr(arg_id);
                switch (arg_expr) {
                    .lambda, .closure => return true,
                    .lookup => |lk| {
                        const sk: u64 = @bitCast(lk.symbol);
                        if (self.symbol_locations.get(sk)) |loc| {
                            switch (loc) {
                                .lambda_code, .closure_value => return true,
                                else => {},
                            }
                        }
                        if (self.store.getSymbolDef(lk.symbol)) |def_id| {
                            const def = self.store.getExpr(def_id);
                            switch (def) {
                                .lambda, .closure => return true,
                                else => {},
                            }
                        }
                    },
                    else => {},
                }
            }
            return false;
        }

        /// Inline a lambda body at the call site, binding parameters directly.
        /// Used for direct lambda calls (e.g., `(|x| x + 1)(5)`) where the
        /// lambda may return closures whose capture data must remain on the
        /// current stack frame. Unlike compileLambdaAsProc, this does NOT
        /// create a separate procedure — the body runs in the caller's scope.
        fn callLambdaBodyDirect(self: *Self, lambda: anytype, args_span: anytype) Allocator.Error!ValueLocation {
            const args = self.store.getExprSpan(args_span);
            const params = self.store.getPatternSpan(lambda.params);
            const num_args = @min(params.len, args.len);

            // Evaluate ALL arguments before binding ANY patterns.
            // This prevents nested inlining (e.g., recursive map2 calls in
            // record builders) from clobbering parameter bindings that were
            // already set by earlier iterations. Without this, the inner call
            // overwrites shared symbol IDs (same lambda inlined multiple times).
            const arg_locs_start = self.scratch_arg_locs.top();
            defer self.scratch_arg_locs.clearFrom(arg_locs_start);
            for (0..num_args) |i| {
                try self.scratch_arg_locs.append(try self.generateExpr(args[i]));
            }
            const arg_locs = self.scratch_arg_locs.sliceFromStart(arg_locs_start);
            for (params[0..num_args], 0..) |pattern_id, i| {
                try self.bindPattern(pattern_id, arg_locs[i]);
            }

            // Set up early return context for inlined lambda.
            const saved_early_return_ret_layout = self.early_return_ret_layout;
            const saved_early_return_patches_len = self.early_return_patches.items.len;
            self.early_return_ret_layout = lambda.ret_layout;

            // Generate the lambda body inline
            const result_loc = try self.generateExpr(lambda.body);

            // Check if any early returns were generated
            if (self.early_return_patches.items.len > saved_early_return_patches_len) {
                // Patch early return jumps to the current position (merge point)
                const merge_point = self.codegen.currentOffset();
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, merge_point);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
            }

            // Restore early return state
            self.early_return_ret_layout = saved_early_return_ret_layout;

            return result_loc;
        }

        /// Generate code for a function call
        fn generateCall(self: *Self, call: anytype) Allocator.Error!ValueLocation {
            // Get the function expression
            const fn_expr = self.store.getExpr(call.fn_expr);

            return switch (fn_expr) {
                // Direct lambda call: inline the body in the current scope.
                // Inline lambdas are defined at a single call site, cannot be
                // recursive, and may return closures whose capture data must
                // remain on the current stack frame.
                .lambda => |lambda| {
                    return try self.callLambdaBodyDirect(lambda, call.args);
                },

                // Direct closure call: inline the inner lambda's body.
                .closure => |closure_id| {
                    const closure = self.store.getClosureData(closure_id);
                    const inner = self.store.getExpr(closure.lambda);
                    if (inner == .lambda) {
                        return try self.callLambdaBodyDirect(inner.lambda, call.args);
                    }
                    unreachable;
                },

                // Chained calls: evaluate inner call, then call result with outer args
                .call => |inner_call| {
                    const inner_result = try self.generateCall(inner_call);
                    if (inner_result == .lambda_code) {
                        return try self.generateCallToLambda(
                            inner_result.lambda_code.code_offset,
                            call.args,
                            call.ret_layout,
                        );
                    }
                    if (inner_result == .closure_value) {
                        return try self.generateClosureDispatch(inner_result.closure_value, call.args, call.ret_layout);
                    }
                    unreachable;
                },

                // Block calls: evaluate block, if it returns lambda_code or closure_value, call it
                .block => |block| {
                    const block_result = try self.generateBlock(block);
                    if (block_result == .lambda_code) {
                        return try self.generateCallToLambda(
                            block_result.lambda_code.code_offset,
                            call.args,
                            call.ret_layout,
                        );
                    }
                    if (block_result == .closure_value) {
                        return try self.generateClosureDispatch(block_result.closure_value, call.args, call.ret_layout);
                    }
                    unreachable;
                },

                // Lookup a function and call it
                .lookup => |lookup| {
                    const symbol_key: u64 = @bitCast(lookup.symbol);
                    // Check if the symbol is bound to a lambda_code or closure_value location
                    if (self.symbol_locations.get(symbol_key)) |loc| {
                        switch (loc) {
                            .lambda_code => |lc| {
                                return try self.generateCallToLambda(
                                    lc.code_offset,
                                    call.args,
                                    call.ret_layout,
                                );
                            },
                            .closure_value => |cv| {
                                return try self.generateClosureDispatch(cv, call.args, call.ret_layout);
                            },
                            // Other locations (registers, stack, immediates) fall through
                            // to generateLookupCall for function pointer handling
                            else => {},
                        }
                    }
                    return try self.generateLookupCall(lookup, call.args, call.ret_layout);
                },

                else => unreachable,
            };
        }

        /// Generate code for a closure expression.
        /// Handles different closure representations based on the lambda set.
        fn generateClosure(self: *Self, closure: anytype) Allocator.Error!ValueLocation {
            switch (closure.representation) {
                .enum_dispatch => |repr| {
                    // Multiple functions, no captures - just store the tag byte
                    // Use 8-byte slot for simplicity (tag is only 1 byte but stack alignment matters)
                    const slot = self.codegen.allocStackSlot(8);
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, repr.tag);
                    // Store as 64-bit value (low byte is the tag, rest is padding)
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    self.codegen.freeGeneral(temp);
                    return .{ .closure_value = .{
                        .stack_offset = slot,
                        .representation = closure.representation,
                        .lambda = closure.lambda,
                        .captures = closure.captures,
                    } };
                },
                .union_repr => |repr| {
                    // Multiple functions with captures - store tag + captures as tagged union
                    const ls = self.layout_store orelse unreachable;
                    const union_layout = ls.getLayout(repr.union_layout);
                    const union_size = ls.layoutSizeAlign(union_layout).size;
                    const slot = self.codegen.allocStackSlot(@intCast(union_size));
                    // Store tag at offset 0 as 64-bit (low 16 bits are the tag)
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, repr.tag);
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    self.codegen.freeGeneral(temp);
                    // Materialize captures at payload offset (+8 for tag with padding)
                    try self.materializeCaptures(closure.captures, slot + 8);
                    return .{ .closure_value = .{
                        .stack_offset = slot,
                        .representation = closure.representation,
                        .lambda = closure.lambda,
                        .captures = closure.captures,
                    } };
                },
                .unwrapped_capture => {
                    // Single function with one capture - materialize capture to stack
                    // and store as closure_value for dispatch at call sites.
                    const ls = self.layout_store orelse unreachable;
                    const capture_layout = ls.getLayout(closure.representation.unwrapped_capture.capture_layout);
                    const capture_size = ls.layoutSizeAlign(capture_layout).size;
                    const slot = self.codegen.allocStackSlot(@intCast(capture_size));
                    try self.materializeCaptures(closure.captures, slot);
                    return .{ .closure_value = .{
                        .stack_offset = slot,
                        .representation = closure.representation,
                        .lambda = closure.lambda,
                        .captures = closure.captures,
                    } };
                },
                .struct_captures => |sc| {
                    // Single function with multiple captures - materialize to struct on stack.
                    const ls = self.layout_store orelse unreachable;
                    const struct_layout = ls.getLayout(sc.struct_layout);
                    const struct_size = ls.layoutSizeAlign(struct_layout).size;
                    const slot = self.codegen.allocStackSlot(@intCast(struct_size));
                    try self.materializeCaptures(closure.captures, slot);
                    return .{ .closure_value = .{
                        .stack_offset = slot,
                        .representation = closure.representation,
                        .lambda = closure.lambda,
                        .captures = closure.captures,
                    } };
                },
                .direct_call => {
                    // Direct call - compile as procedure for direct calling
                    const inner = self.store.getExpr(closure.lambda);
                    if (inner != .lambda) unreachable;
                    const code_offset = try self.compileLambdaAsProc(closure.lambda, inner.lambda);
                    return .{ .lambda_code = .{
                        .code_offset = code_offset,
                        .ret_layout = inner.lambda.ret_layout,
                    } };
                },
            }
        }

        /// Materialize captured values to a stack location.
        /// Used when creating closure values with captures.
        fn materializeCaptures(self: *Self, captures_span: lir.LIR.LirCaptureSpan, base_offset: i32) Allocator.Error!void {
            const captures = self.store.getCaptures(captures_span);
            var offset: i32 = 0;
            for (captures) |capture| {
                const symbol_key: u64 = @bitCast(capture.symbol);
                if (self.symbol_locations.get(symbol_key)) |capture_loc| {
                    // Get size and alignment of this capture
                    const ls = self.layout_store orelse unreachable;
                    const capture_layout = ls.getLayout(capture.layout_idx);
                    const cap_sa = ls.layoutSizeAlign(capture_layout);
                    const capture_size = cap_sa.size;
                    // Align offset to match layout store's putCaptureStruct
                    const cap_align = cap_sa.alignment.toByteUnits();
                    offset = @intCast(std.mem.alignForward(usize, @intCast(offset), cap_align));
                    // Number of 8-byte words to copy (rounded up)
                    const num_words: u32 = (capture_size + 7) / 8;

                    // Copy value to the struct offset
                    switch (capture_loc) {
                        .immediate_i128 => |val| {
                            // Store 128-bit immediate as two 64-bit halves
                            const low: u64 = @truncate(@as(u128, @bitCast(val)));
                            const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(temp, @bitCast(low));
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            try self.codegen.emitLoadImm(temp, @bitCast(high));
                            try self.codegen.emitStoreStack(.w64, base_offset + offset + 8, temp);
                            self.codegen.freeGeneral(temp);
                        },
                        .stack_i128 => |src_offset| {
                            // Copy both 64-bit halves
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadStack(.w64, temp, src_offset);
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            try self.codegen.emitLoadStack(.w64, temp, src_offset + 8);
                            try self.codegen.emitStoreStack(.w64, base_offset + offset + 8, temp);
                            self.codegen.freeGeneral(temp);
                        },
                        .general_reg => |reg| {
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, reg);
                            if (num_words > 1) {
                                // Zero-extend remaining words
                                const temp = try self.allocTempGeneral();
                                try self.codegen.emitLoadImm(temp, 0);
                                var w: u32 = 1;
                                while (w < num_words) : (w += 1) {
                                    try self.codegen.emitStoreStack(.w64, base_offset + offset + @as(i32, @intCast(w * 8)), temp);
                                }
                                self.codegen.freeGeneral(temp);
                            }
                        },
                        .stack => |s| {
                            const src_offset = s.offset;
                            // Copy all words from source to destination
                            const temp = try self.allocTempGeneral();
                            var w: u32 = 0;
                            while (w < num_words) : (w += 1) {
                                const word_off: i32 = @intCast(w * 8);
                                try self.codegen.emitLoadStack(.w64, temp, src_offset + word_off);
                                try self.codegen.emitStoreStack(.w64, base_offset + offset + word_off, temp);
                            }
                            self.codegen.freeGeneral(temp);
                        },
                        .stack_str => |src_offset| {
                            // Copy all words from source to destination
                            const temp = try self.allocTempGeneral();
                            var w: u32 = 0;
                            while (w < num_words) : (w += 1) {
                                const word_off: i32 = @intCast(w * 8);
                                try self.codegen.emitLoadStack(.w64, temp, src_offset + word_off);
                                try self.codegen.emitStoreStack(.w64, base_offset + offset + word_off, temp);
                            }
                            self.codegen.freeGeneral(temp);
                        },
                        .immediate_i64 => |val| {
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(temp, @bitCast(val));
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            if (num_words > 1) {
                                // Sign-extend to remaining words
                                const sign_ext: i64 = if (val < 0) -1 else 0;
                                try self.codegen.emitLoadImm(temp, sign_ext);
                                var w: u32 = 1;
                                while (w < num_words) : (w += 1) {
                                    try self.codegen.emitStoreStack(.w64, base_offset + offset + @as(i32, @intCast(w * 8)), temp);
                                }
                            }
                            self.codegen.freeGeneral(temp);
                        },
                        .list_stack => |info| {
                            // Copy all 3 words of the list (ptr, len, capacity)
                            const temp = try self.allocTempGeneral();
                            var w: u32 = 0;
                            while (w < num_words) : (w += 1) {
                                const word_off: i32 = @intCast(w * 8);
                                try self.codegen.emitLoadStack(.w64, temp, info.struct_offset + word_off);
                                try self.codegen.emitStoreStack(.w64, base_offset + offset + word_off, temp);
                            }
                            self.codegen.freeGeneral(temp);
                        },
                        .lambda_code => |lc| {
                            // Capturing a lambda (function pointer) - store the code address
                            const temp = try self.allocTempGeneral();
                            const current = self.codegen.currentOffset();
                            const rel: i64 = @as(i64, @intCast(lc.code_offset)) - @as(i64, @intCast(current));
                            try self.internal_addr_patches.append(self.allocator, .{
                                .instr_offset = current,
                                .target_offset = lc.code_offset,
                            });
                            if (comptime target.toCpuArch() == .aarch64) {
                                try self.codegen.emit.adr(temp, @intCast(rel));
                            } else {
                                try self.codegen.emit.leaRegRipRel(temp, @intCast(rel));
                            }
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            self.codegen.freeGeneral(temp);
                        },
                        else => {
                            const slot = try self.ensureOnStack(capture_loc, capture_size);
                            const temp = try self.allocTempGeneral();
                            var w: u32 = 0;
                            while (w < num_words) : (w += 1) {
                                const word_off: i32 = @intCast(w * 8);
                                try self.codegen.emitLoadStack(.w64, temp, slot + word_off);
                                try self.codegen.emitStoreStack(.w64, base_offset + offset + word_off, temp);
                            }
                            self.codegen.freeGeneral(temp);
                        },
                    }
                    offset += @intCast(capture_size);
                } else {
                    // Symbol not found in symbol_locations. Try generateLookup which
                    // also checks top-level definitions via getSymbolDef.
                    if (self.generateLookup(capture.symbol, capture.layout_idx)) |resolved_loc| {
                        // Found via resolveSymbol - copy it to captures
                        const ls = self.layout_store orelse unreachable;
                        const capture_layout = ls.getLayout(capture.layout_idx);
                        const cap_sa_r = ls.layoutSizeAlign(capture_layout);
                        const capture_size = cap_sa_r.size;
                        const num_words_r: u32 = (capture_size + 7) / 8;
                        // Align offset to match layout store's putCaptureStruct
                        const cap_align_r = cap_sa_r.alignment.toByteUnits();
                        offset = @intCast(std.mem.alignForward(usize, @intCast(offset), cap_align_r));

                        switch (resolved_loc) {
                            .general_reg => |reg| {
                                try self.codegen.emitStoreStack(.w64, base_offset + offset, reg);
                                if (num_words_r > 1) {
                                    const temp = try self.allocTempGeneral();
                                    try self.codegen.emitLoadImm(temp, 0);
                                    var w: u32 = 1;
                                    while (w < num_words_r) : (w += 1) {
                                        try self.codegen.emitStoreStack(.w64, base_offset + offset + @as(i32, @intCast(w * 8)), temp);
                                    }
                                    self.codegen.freeGeneral(temp);
                                }
                            },
                            .lambda_code => |lc| {
                                // Capturing a lambda (function pointer) - store the code address
                                const temp = try self.allocTempGeneral();
                                try self.codegen.emitLoadImm(temp, @intCast(lc.code_offset));
                                try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                                self.codegen.freeGeneral(temp);
                            },
                            else => {
                                const slot = try self.ensureOnStack(resolved_loc, capture_size);
                                const temp = try self.allocTempGeneral();
                                var w: u32 = 0;
                                while (w < num_words_r) : (w += 1) {
                                    const word_off: i32 = @intCast(w * 8);
                                    try self.codegen.emitLoadStack(.w64, temp, slot + word_off);
                                    try self.codegen.emitStoreStack(.w64, base_offset + offset + word_off, temp);
                                }
                                self.codegen.freeGeneral(temp);
                            },
                        }
                        offset += @intCast(capture_size);
                    } else |_| {
                        unreachable;
                    }
                }
            }
        }

        /// Generate code for dispatching a closure call.
        /// Handles the different closure representations with appropriate dispatch.
        fn generateClosureDispatch(
            self: *Self,
            cv: anytype,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            switch (cv.representation) {
                .enum_dispatch => |repr| {
                    return try self.dispatchEnumClosure(cv.stack_offset, repr.lambda_set, args_span, ret_layout);
                },
                .union_repr => |repr| {
                    return try self.dispatchUnionClosure(cv.stack_offset, repr, args_span, ret_layout);
                },
                .unwrapped_capture => {
                    // Single function - call with the captured value
                    return try self.callSingleClosureWithCaptures(cv, args_span, ret_layout);
                },
                .struct_captures => {
                    // Single function - call with captures struct
                    return try self.callSingleClosureWithCaptures(cv, args_span, ret_layout);
                },
                .direct_call => {
                    // Inline the lambda body (no captures, no separate proc needed)
                    const lambda_expr = self.store.getExpr(cv.lambda);
                    switch (lambda_expr) {
                        .lambda => |l| {
                            return try self.callLambdaBodyDirect(l, args_span);
                        },
                        .closure => |c_id| {
                            const c = self.store.getClosureData(c_id);
                            const inner = self.store.getExpr(c.lambda);
                            if (inner == .lambda) {
                                return try self.callLambdaBodyDirect(inner.lambda, args_span);
                            }
                            unreachable;
                        },
                        else => unreachable,
                    }
                },
            }
        }

        /// Dispatch an enum closure (multiple functions, no captures).
        /// Generates a switch on the tag to dispatch to the correct function.
        fn dispatchEnumClosure(
            self: *Self,
            tag_offset: i32,
            lambda_set: lir.LambdaSetMemberSpan,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const members = self.store.getLambdaSetMembers(lambda_set);

            if (members.len == 0) {
                unreachable;
            }

            if (members.len == 1) {
                // Single function - no dispatch needed
                const member = members[0];
                return try self.compileLambdaAndCall(member.lambda_body, args_span, ret_layout);
            }

            // Load tag from stack (stored as 64-bit value, tag is in low byte)
            const tag_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadStack(.w64, tag_reg, tag_offset);

            // Allocate result slot sized to the return layout
            const result_size = self.getLayoutSize(ret_layout);
            const result_slot = self.codegen.allocStackSlot(result_size);

            // Track end jumps for patching
            var end_jumps = std.ArrayList(usize).empty;
            defer end_jumps.deinit(self.allocator);

            for (members, 0..) |member, i| {
                const is_last = (i == members.len - 1);

                if (!is_last) {
                    // Compare tag with this member's tag
                    try self.emitCmpImm(tag_reg, member.tag);
                    const skip_jump = try self.emitJumpIfNotEqual();

                    // Generate code for this branch
                    const result = try self.compileLambdaAndCall(member.lambda_body, args_span, ret_layout);
                    try self.copyToStackSlot(result_slot, result, result_size);

                    // Jump to end
                    try end_jumps.append(self.allocator, try self.codegen.emitJump());

                    // Patch skip_jump to here
                    self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
                } else {
                    // Last case - no comparison needed (fallthrough)
                    const result = try self.compileLambdaAndCall(member.lambda_body, args_span, ret_layout);
                    try self.copyToStackSlot(result_slot, result, result_size);
                }
            }

            // Patch all end jumps to current location
            for (end_jumps.items) |jump| {
                self.codegen.patchJump(jump, self.codegen.currentOffset());
            }

            self.codegen.freeGeneral(tag_reg);
            return .{ .stack = .{ .offset = result_slot } };
        }

        /// Dispatch a union closure (multiple functions, some with captures).
        fn dispatchUnionClosure(
            self: *Self,
            union_offset: i32,
            repr: anytype,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store orelse unreachable;
            const union_layout_val = ls.getLayout(repr.union_layout);
            std.debug.assert(union_layout_val.tag == .tag_union);
            const tu_data = ls.getTagUnionData(union_layout_val.data.tag_union.idx);
            const disc_offset: i32 = @intCast(tu_data.discriminant_offset);

            const members = self.store.getLambdaSetMembers(repr.lambda_set);

            if (members.len == 0) {
                unreachable;
            }

            if (members.len == 1) {
                // Single function - captures are the payload at offset 0
                const member = members[0];
                return try self.compileLambdaAndCallWithCaptures(member, union_offset, args_span);
            }

            // Load discriminant from its correct offset in the tag union
            // Layout: [payload at offset 0][discriminant at discriminant_offset]
            const disc_use_w32 = (disc_offset + 8 > @as(i32, @intCast(tu_data.size)));
            const tag_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadStack(if (disc_use_w32) .w32 else .w64, tag_reg, union_offset + disc_offset);

            // Allocate result slot sized to the return layout
            const result_size = self.getLayoutSize(ret_layout);
            const result_slot = self.codegen.allocStackSlot(result_size);

            // Track end jumps for patching
            var end_jumps = std.ArrayList(usize).empty;
            defer end_jumps.deinit(self.allocator);

            for (members, 0..) |member, i| {
                const is_last = (i == members.len - 1);

                if (!is_last) {
                    // Compare tag with this member's tag
                    try self.emitCmpImm(tag_reg, member.tag);
                    const skip_jump = try self.emitJumpIfNotEqual();

                    // Captures are the payload at offset 0
                    const result = try self.compileLambdaAndCallWithCaptures(member, union_offset, args_span);
                    try self.copyToStackSlot(result_slot, result, result_size);

                    // Jump to end
                    try end_jumps.append(self.allocator, try self.codegen.emitJump());

                    // Patch skip_jump to here
                    self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
                } else {
                    // Last case - no comparison needed (fallthrough)
                    const result = try self.compileLambdaAndCallWithCaptures(member, union_offset, args_span);
                    try self.copyToStackSlot(result_slot, result, result_size);
                }
            }

            // Patch all end jumps to current location
            for (end_jumps.items) |jump| {
                self.codegen.patchJump(jump, self.codegen.currentOffset());
            }

            self.codegen.freeGeneral(tag_reg);
            return .{ .stack = .{ .offset = result_slot } };
        }

        /// Call a single closure (unwrapped_capture or struct_captures) by binding
        /// its captures to symbol_locations and inlining the lambda body.
        fn callSingleClosureWithCaptures(
            self: *Self,
            cv: anytype,
            args_span: anytype,
            _: layout.Idx,
        ) Allocator.Error!ValueLocation {
            // Bind captures from the closure's stack data to their symbols
            const captures = self.store.getCaptures(cv.captures);
            var offset: i32 = 0;
            for (captures) |capture| {
                const symbol_key: u64 = @bitCast(capture.symbol);
                const ls = self.layout_store orelse unreachable;
                const capture_layout = ls.getLayout(capture.layout_idx);
                const cap_sa = ls.layoutSizeAlign(capture_layout);
                const capture_size = cap_sa.size;
                // Align offset to match layout store's putCaptureStruct
                const cap_align = cap_sa.alignment.toByteUnits();
                offset = @intCast(std.mem.alignForward(usize, @intCast(offset), cap_align));
                const capture_offset = cv.stack_offset + offset;
                // Use the appropriate ValueLocation based on the layout type
                if (capture.layout_idx == .i128 or capture.layout_idx == .u128 or capture.layout_idx == .dec) {
                    try self.symbol_locations.put(symbol_key, .{ .stack_i128 = capture_offset });
                } else if (capture.layout_idx == .str) {
                    try self.symbol_locations.put(symbol_key, .{ .stack_str = capture_offset });
                } else if (capture_layout.tag == .list or capture_layout.tag == .list_of_zst) {
                    try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                        .struct_offset = capture_offset,
                        .data_offset = 0,
                        .num_elements = 0,
                    } });
                } else {
                    try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = capture_offset } });
                }
                offset += @intCast(capture_size);
            }

            // Inline the lambda body (captures must stay in scope)
            const lambda_expr = self.store.getExpr(cv.lambda);
            switch (lambda_expr) {
                .lambda => |l| {
                    return try self.callLambdaBodyDirect(l, args_span);
                },
                .closure => |c_id| {
                    const c = self.store.getClosureData(c_id);
                    const inner = self.store.getExpr(c.lambda);
                    if (inner == .lambda) {
                        return try self.callLambdaBodyDirect(inner.lambda, args_span);
                    }
                    unreachable;
                },
                else => unreachable,
            }
        }

        // All lambdas are compiled as procs - no inlining.

        /// Compile a lambda body expression as a procedure and call it.
        fn compileLambdaAndCall(
            self: *Self,
            lambda_body: lir.LirExprId,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const lambda_expr = self.store.getExpr(lambda_body);
            switch (lambda_expr) {
                .lambda => |lambda| {
                    const code_offset = try self.compileLambdaAsProc(lambda_body, lambda);
                    return try self.generateCallToLambda(code_offset, args_span, ret_layout);
                },
                .closure => |closure_id| {
                    const closure = self.store.getClosureData(closure_id);
                    const inner = self.store.getExpr(closure.lambda);
                    if (inner == .lambda) {
                        const code_offset = try self.compileLambdaAsProc(closure.lambda, inner.lambda);
                        return try self.generateCallToLambda(code_offset, args_span, ret_layout);
                    }
                    unreachable;
                },
                else => unreachable,
            }
        }

        /// Compile a lambda and call it, binding captures from a stack location.
        /// Binds captures to symbol_locations, then evaluates the lambda body directly
        /// (captures need to stay in scope, so we don't compile as a separate procedure).
        fn compileLambdaAndCallWithCaptures(
            self: *Self,
            member: lir.LambdaSetMember,
            captures_offset: i32,
            args_span: anytype,
        ) Allocator.Error!ValueLocation {

            // Bind captures from the stack to their symbols
            const captures = self.store.getCaptures(member.captures);
            var offset: i32 = 0;
            for (captures) |capture| {
                const symbol_key: u64 = @bitCast(capture.symbol);
                const ls = self.layout_store orelse unreachable;
                const capture_layout = ls.getLayout(capture.layout_idx);
                const cap_sa = ls.layoutSizeAlign(capture_layout);
                const capture_size = cap_sa.size;
                // Align offset to match layout store's putCaptureStruct
                const cap_align = cap_sa.alignment.toByteUnits();
                offset = @intCast(std.mem.alignForward(usize, @intCast(offset), cap_align));
                try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = captures_offset + offset } });
                offset += @intCast(capture_size);
            }

            // Get the lambda and compile as a proc
            const lambda_expr = self.store.getExpr(member.lambda_body);
            switch (lambda_expr) {
                .lambda => |l| {
                    const code_offset = try self.compileLambdaAsProc(member.lambda_body, l);
                    return try self.generateCallToLambda(code_offset, args_span, l.ret_layout);
                },
                .closure => |c_id| {
                    const c = self.store.getClosureData(c_id);
                    const inner = self.store.getExpr(c.lambda);
                    if (inner == .lambda) {
                        const code_offset = try self.compileLambdaAsProc(c.lambda, inner.lambda);
                        return try self.generateCallToLambda(code_offset, args_span, inner.lambda.ret_layout);
                    }
                    unreachable;
                },
                else => unreachable,
            }
        }

        /// Copy a value location to a stack slot.
        fn copyToStackSlot(self: *Self, slot: i32, loc: ValueLocation, size: u32) Allocator.Error!void {
            switch (loc) {
                .general_reg => |reg| {
                    try self.codegen.emitStoreStack(.w64, slot, reg);
                },
                .stack => |s| {
                    const src_offset = s.offset;
                    const temp = try self.allocTempGeneral();
                    // Copy in 8-byte chunks for multi-word values
                    var offset: i32 = 0;
                    while (offset < @as(i32, @intCast(size))) : (offset += 8) {
                        try self.codegen.emitLoadStack(.w64, temp, src_offset + offset);
                        try self.codegen.emitStoreStack(.w64, slot + offset, temp);
                    }
                    self.codegen.freeGeneral(temp);
                },
                .immediate_i64 => |val| {
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, @bitCast(val));
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    self.codegen.freeGeneral(temp);
                },
                .stack_i128 => |src_offset| {
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, temp, src_offset);
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    try self.codegen.emitLoadStack(.w64, temp, src_offset + 8);
                    try self.codegen.emitStoreStack(.w64, slot + 8, temp);
                    self.codegen.freeGeneral(temp);
                },
                .stack_str => |src_offset| {
                    const temp = try self.allocTempGeneral();
                    var offset: i32 = 0;
                    while (offset < 24) : (offset += 8) {
                        try self.codegen.emitLoadStack(.w64, temp, src_offset + offset);
                        try self.codegen.emitStoreStack(.w64, slot + offset, temp);
                    }
                    self.codegen.freeGeneral(temp);
                },
                else => {
                    // For other types, ensure on stack and copy
                    const temp_slot = try self.ensureOnStack(loc, size);
                    const temp = try self.allocTempGeneral();
                    var offset: i32 = 0;
                    while (offset < @as(i32, @intCast(size))) : (offset += 8) {
                        try self.codegen.emitLoadStack(.w64, temp, temp_slot + offset);
                        try self.codegen.emitStoreStack(.w64, slot + offset, temp);
                    }
                    self.codegen.freeGeneral(temp);
                },
            }
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

        /// Generate code for calling a looked-up function definition.
        fn generateLookupCall(self: *Self, lookup: anytype, args_span: anytype, ret_layout: layout.Idx) Allocator.Error!ValueLocation {
            const symbol_key: u64 = @bitCast(lookup.symbol);

            // Check if the function was compiled as a procedure
            if (self.proc_registry.get(symbol_key)) |proc| {
                return try self.generateCallToCompiledProc(proc, args_span, ret_layout);
            }

            // Look up the function in top-level definitions
            if (self.store.getSymbolDef(lookup.symbol)) |def_expr_id| {
                const def_expr = self.store.getExpr(def_expr_id);

                return switch (def_expr) {
                    .lambda => |lambda| {
                        // Inline when:
                        // 1. Args contain callables (higher-order calls)
                        // 2. Body returns callable (closure-returning functions)
                        // 3. Polymorphic lambda with different ret_layout
                        if (self.hasCallableArguments(args_span) or self.bodyReturnsCallable(lambda.body) or lambda.ret_layout != ret_layout) {
                            return try self.callLambdaBodyDirect(lambda, args_span);
                        }
                        const offset = try self.compileLambdaAsProc(def_expr_id, lambda);
                        return try self.generateCallToLambda(offset, args_span, ret_layout);
                    },
                    .closure => |closure_id| {
                        const closure = self.store.getClosureData(closure_id);
                        const inner = self.store.getExpr(closure.lambda);
                        if (inner == .lambda) {
                            if (self.hasCallableArguments(args_span) or self.bodyReturnsCallable(inner.lambda.body) or inner.lambda.ret_layout != ret_layout) {
                                return try self.callLambdaBodyDirect(inner.lambda, args_span);
                            }
                            const offset = try self.compileLambdaAsProc(closure.lambda, inner.lambda);
                            return try self.generateCallToLambda(offset, args_span, ret_layout);
                        }
                        unreachable;
                    },
                    .block => {
                        // Definition is a block — evaluate the block which may produce a lambda/closure
                        const block_result = try self.generateExpr(def_expr_id);
                        if (block_result == .lambda_code) {
                            return try self.generateCallToLambda(
                                block_result.lambda_code.code_offset,
                                args_span,
                                ret_layout,
                            );
                        }
                        if (block_result == .closure_value) {
                            return try self.generateClosureDispatch(block_result.closure_value, args_span, ret_layout);
                        }
                        unreachable;
                    },
                    .nominal => |nom| {
                        // Unwrap nominal and retry with the inner expression
                        const inner = self.store.getExpr(nom.backing_expr);
                        if (inner == .lambda) {
                            if (self.hasCallableArguments(args_span) or self.bodyReturnsCallable(inner.lambda.body) or inner.lambda.ret_layout != ret_layout) {
                                return try self.callLambdaBodyDirect(inner.lambda, args_span);
                            }
                            const offset = try self.compileLambdaAsProc(nom.backing_expr, inner.lambda);
                            return try self.generateCallToLambda(offset, args_span, ret_layout);
                        }
                        if (inner == .closure) {
                            const inner_clo = self.store.getClosureData(inner.closure);
                            const closure_inner = self.store.getExpr(inner_clo.lambda);
                            if (closure_inner == .lambda) {
                                if (self.hasCallableArguments(args_span) or self.bodyReturnsCallable(closure_inner.lambda.body) or closure_inner.lambda.ret_layout != ret_layout) {
                                    return try self.callLambdaBodyDirect(closure_inner.lambda, args_span);
                                }
                                const offset = try self.compileLambdaAsProc(inner_clo.lambda, closure_inner.lambda);
                                return try self.generateCallToLambda(offset, args_span, ret_layout);
                            }
                        }
                        if (inner == .block) {
                            const block_result = try self.generateExpr(nom.backing_expr);
                            if (block_result == .lambda_code) {
                                return try self.generateCallToLambda(
                                    block_result.lambda_code.code_offset,
                                    args_span,
                                    ret_layout,
                                );
                            }
                            if (block_result == .closure_value) {
                                return try self.generateClosureDispatch(block_result.closure_value, args_span, ret_layout);
                            }
                        }
                        unreachable;
                    },
                    .runtime_error => {
                        // Dead code path in a call — emit roc_crashed and return dummy.
                        try self.emitRocCrash("hit a runtime error in call (dead code path)");
                        return .{ .immediate_i64 = 0 };
                    },
                    else => unreachable,
                };
            }

            // Check if the function is a locally-bound value (e.g., a lambda parameter
            // in a higher-order function like `apply = |f, x| f(x)`)
            if (self.symbol_locations.get(symbol_key)) |loc| {
                switch (loc) {
                    .lambda_code => |lc| {
                        return try self.generateCallToLambda(lc.code_offset, args_span, ret_layout);
                    },
                    .closure_value => |cv| {
                        return try self.generateClosureDispatch(cv, args_span, ret_layout);
                    },
                    // Function pointer: the value is an absolute address that we can call
                    // indirectly via BLR (aarch64) or CALL reg (x86_64).
                    // This happens when a lambda is passed as an argument to a higher-order
                    // function — the caller computes the address via ADR/LEA and passes it.
                    .general_reg, .stack, .immediate_i64 => {
                        return try self.generateIndirectCall(loc, args_span, ret_layout);
                    },
                    else => {},
                }
            }

            unreachable;
        }

        /// Generate a call to an already-compiled procedure.
        /// This is used for recursive functions that were compiled via compileAllProcs.
        fn generateCallToCompiledProc(self: *Self, proc: CompiledProc, args_span: anytype, ret_layout: layout.Idx) Allocator.Error!ValueLocation {
            const args = self.store.getExprSpan(args_span);

            // Pass 1: Generate all argument expressions and calculate register needs
            const arg_infos_start = self.scratch_arg_infos.top();
            defer self.scratch_arg_infos.clearFrom(arg_infos_start);

            for (args) |arg_id| {
                const arg_loc = try self.generateExpr(arg_id);
                const arg_layout = self.getExprLayout(arg_id);
                const num_regs: u8 = self.calcArgRegCount(arg_loc, arg_layout);
                try self.scratch_arg_infos.append(.{ .loc = arg_loc, .layout_idx = arg_layout, .num_regs = num_regs });
            }
            const arg_infos = self.scratch_arg_infos.sliceFromStart(arg_infos_start);

            // Pass 2: Place arguments and emit call
            const stack_spill_size = try self.placeCallArguments(arg_infos, .{});
            try self.emitCallToOffset(proc.code_start);

            if (stack_spill_size > 0) {
                try self.emitAddStackPtr(stack_spill_size);
            }

            return self.saveCallReturnValue(ret_layout, false, 0);
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
                .lambda_code => |lc| {
                    // Load the code offset into the register - used when passing lambdas as arguments
                    try self.codegen.emitLoadImm(target_reg, @bitCast(@as(i64, @intCast(lc.code_offset))));
                },
                .closure_value => |cv| {
                    // Load the closure value from stack
                    try self.codegen.emitLoadStack(.w64, target_reg, cv.stack_offset);
                },
                .float_reg, .immediate_f64 => {
                    unreachable;
                },
                .noreturn => unreachable,
            }
        }

        /// Calculate the number of registers an argument needs based on its location and layout.
        fn calcArgRegCount(self: *Self, arg_loc: ValueLocation, arg_layout: ?layout.Idx) u8 {
            // Check for 128-bit types (i128, u128, Dec) - need 2 registers
            const is_i128_arg = (arg_loc == .stack_i128 or arg_loc == .immediate_i128) or
                (arg_loc == .stack and arg_layout != null and
                    (arg_layout.? == .dec or arg_layout.? == .i128 or arg_layout.? == .u128));
            if (is_i128_arg) return 2;

            // Check for list/string types - need 3 registers (24 bytes: ptr, len, capacity)
            if (arg_loc == .list_stack or arg_loc == .stack_str) return 3;
            if (arg_layout) |al| {
                if (al == .str) return 3; // Strings are 24 bytes
                if (self.layout_store) |ls| {
                    const layout_val = ls.getLayout(al);
                    if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 3;
                    // Check for records/tuples > 8 bytes
                    if (layout_val.tag == .record or layout_val.tag == .tuple or layout_val.tag == .tag_union) {
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
        fn spillArgToStack(self: *Self, arg_loc: ValueLocation, stack_offset: i32, num_regs: u8) Allocator.Error!void {
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
                    var ri: u8 = 0;
                    while (ri < num_regs) : (ri += 1) {
                        const off: i32 = @as(i32, ri) * 8;
                        try self.emitLoad(.w64, temp_reg, frame_ptr, src_offset + off);
                        try self.emitStore(.w64, stack_ptr, stack_offset + off, temp_reg);
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
                    // For other types, try to move to temp register first
                    try self.moveToReg(arg_loc, temp_reg);
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
                .closure_value => |cv| cv.stack_offset,
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
                .lambda_code => |lc| blk: {
                    // Store the code address to a stack slot via ADR/LEA
                    const slot = self.codegen.allocStackSlot(8);
                    const temp = try self.allocTempGeneral();
                    const current = self.codegen.currentOffset();
                    const rel: i64 = @as(i64, @intCast(lc.code_offset)) - @as(i64, @intCast(current));
                    try self.internal_addr_patches.append(self.allocator, .{
                        .instr_offset = current,
                        .target_offset = lc.code_offset,
                    });
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.adr(temp, @intCast(rel));
                        try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot);
                    } else {
                        try self.codegen.emit.leaRegRipRel(temp, @intCast(rel));
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp);
                    }
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
                .closure_value => |cv| {
                    // Load the closure value from stack
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, cv.stack_offset);
                    return reg;
                },
                .float_reg, .immediate_f64, .lambda_code => {
                    // Convert float to int or lambda_code to register - this shouldn't happen in normal code
                    unreachable;
                },
                .noreturn => unreachable,
            }
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
                    const offset = s.offset;
                    const reg = self.codegen.allocFloat() orelse unreachable;
                    try self.codegen.emitLoadStackF64(reg, offset);
                    return reg;
                },
                .immediate_i64 => |val| {
                    // Integer literal used in float context — convert at compile time
                    const f_val: f64 = @floatFromInt(val);
                    return self.ensureInFloatReg(.{ .immediate_f64 = f_val });
                },
                .general_reg, .immediate_i128, .stack_i128, .stack_str, .list_stack, .lambda_code, .closure_value => {
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
                        if (self.layout_store) |ls| {
                            const tuple_layout = ls.getLayout(result_layout);
                            if (tuple_layout.tag == .tuple) {
                                const tuple_data = ls.getTupleData(tuple_layout.data.tuple.idx);
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
                .i64, .i32, .i16, .u64, .u32, .u16, .bool => {
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
                    if (self.layout_store) |ls| {
                        const layout_val = ls.getLayout(result_layout);
                        switch (layout_val.tag) {
                            .record => {
                                const record_data = ls.getRecordData(layout_val.data.record.idx);
                                try self.copyStackToPtr(loc, saved_ptr_reg, record_data.size);
                                return;
                            },
                            .tuple => {
                                const tuple_data = ls.getTupleData(layout_val.data.tuple.idx);
                                try self.copyStackToPtr(loc, saved_ptr_reg, tuple_data.size);
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
                            else => {
                                unreachable;
                            },
                        }
                    } else {
                        unreachable; // non-scalar layout must have layout store
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
                    // Not a stack location - try to store as single value
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreToMem(ptr_reg, reg);
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
        pub fn compileAllProcs(self: *Self, procs: []const LirProc) Allocator.Error!void {
            for (procs) |proc| {
                try self.compileProc(proc);
            }
        }

        /// Compile a single procedure as a complete unit.
        /// Uses deferred prologue pattern: generates body first to determine which
        /// callee-saved registers are used, then prepends prologue and adjusts relocations.
        fn compileProc(self: *Self, proc: LirProc) Allocator.Error!void {
            const key: u64 = @bitCast(proc.name);

            // Save current state - procedure has its own scope that shouldn't pollute caller
            const saved_stack_offset = self.codegen.stack_offset;
            const saved_callee_saved_used = self.codegen.callee_saved_used;
            var saved_symbol_locations = self.symbol_locations.clone() catch return error.OutOfMemory;
            defer saved_symbol_locations.deinit();
            var saved_mutable_var_slots = self.mutable_var_slots.clone() catch return error.OutOfMemory;
            defer saved_mutable_var_slots.deinit();

            // Clear state for procedure's scope
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();
            self.codegen.callee_saved_used = 0;

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

            // CRITICAL: Register the procedure BEFORE generating the body
            // so that recursive calls within the body can find this procedure.
            // We use body_start as a temporary code_start; will be updated after prologue prepend.
            try self.proc_registry.put(key, .{
                .code_start = body_start,
                .code_end = 0, // Placeholder, updated below
                .name = proc.name,
            });

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
            const saved_early_return_patches_len = self.early_return_patches.items.len;

            // Bind parameters to argument registers
            try self.bindProcParams(proc.args, proc.arg_layouts);

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
                var body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                // Truncate buffer back to body_start
                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                // Emit prologue using DeferredFrameBuilder (now knows callee_saved_used).
                // Pass only the actual locals size — the builder adds callee-saved space internally.
                const prologue_start = self.codegen.currentOffset();
                const actual_locals_x86: u32 = @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                try self.codegen.emitPrologueWithAlloc(actual_locals_x86);
                const prologue_size = self.codegen.currentOffset() - prologue_start;

                // PHASE 2.5: Patch self-calls in body_bytes
                // Self-calls target body_start (0) but after prepending prologue,
                // they need to target prologue_start (0) which means adjusting
                // the relative offset by -prologue_size.
                // x86_64 CALL rel32: E8 [4-byte signed offset]
                var i: usize = 0;
                while (i + 5 <= body_bytes.len) : (i += 1) {
                    if (body_bytes[i] == 0xE8) { // CALL opcode
                        // Read the 4-byte relative offset (little-endian)
                        const rel_bytes = body_bytes[i + 1 ..][0..4];
                        const rel_offset: i32 = @bitCast(rel_bytes.*);
                        // The call lands at: (body_offset + 5) + rel_offset
                        // where body_offset = i (offset within body)
                        // For self-calls, this should equal body_start (0)
                        const call_end_offset: i64 = @intCast(i + 5);
                        const call_target: i64 = call_end_offset + rel_offset;
                        if (call_target == @as(i64, @intCast(body_start))) {
                            // This is a self-call targeting body_start
                            // After prepending prologue, the call is at (prologue_size + i)
                            // and should still target prologue_start (0)
                            // New relative offset = 0 - (prologue_size + i + 5)
                            //                     = -(prologue_size + i + 5)
                            // Old relative offset = -(i + 5)
                            // Adjustment = new - old = -prologue_size
                            const new_rel: i32 = rel_offset - @as(i32, @intCast(prologue_size));
                            const new_bytes: [4]u8 = @bitCast(new_rel);
                            @memcpy(body_bytes[i + 1 ..][0..4], &new_bytes);
                        }
                    }
                }

                // Re-append body
                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                // PHASE 3: Adjust relocation offsets
                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size);
                }

                // Patch return-stmt jumps to the shared epilogue
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size;
                }
                const final_epilogue = body_epilogue_offset - body_start + prologue_size + prologue_start;
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);

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

                // PHASE 2.5: Patch self-calls in body_bytes
                // Self-calls target body_start but after prepending prologue,
                // they need to target prologue_start which means adjusting
                // the relative offset by -prologue_size.
                // aarch64 BL: 1 00101 imm26 (4-byte instruction, imm26 is signed offset in words)
                var i: usize = 0;
                while (i + 4 <= body_bytes.len) : (i += 4) {
                    const inst: u32 = @bitCast(body_bytes[i..][0..4].*);
                    // Check for BL opcode (bits 31-26 = 100101 = 37)
                    if ((inst >> 26) == 0b100101) {
                        // Extract 26-bit signed offset (in words)
                        const imm26: u26 = @truncate(inst);
                        const offset_words: i26 = @bitCast(imm26);
                        const offset_bytes: i32 = @as(i32, offset_words) * 4;
                        // For aarch64, offset is relative to instruction address (not end like x86_64)
                        const inst_offset: i64 = @intCast(i);
                        const call_target: i64 = inst_offset + offset_bytes;
                        if (call_target == @as(i64, @intCast(body_start))) {
                            // This is a self-call targeting body_start
                            // After prepending prologue, the instruction is at (prologue_size + i)
                            // and should still target prologue_start (body_start)
                            // New relative offset = body_start - (body_start + prologue_size + i)
                            //                     = -(prologue_size + i)
                            // Old relative offset = -(i)
                            // Adjustment = new - old = -prologue_size
                            const new_offset_bytes: i32 = offset_bytes - @as(i32, @intCast(prologue_size));
                            const new_offset_words: i26 = @intCast(@divExact(new_offset_bytes, 4));
                            const new_imm26: u26 = @bitCast(new_offset_words);
                            const new_inst: u32 = (@as(u32, 0b100101) << 26) | new_imm26;
                            const new_bytes: [4]u8 = @bitCast(new_inst);
                            @memcpy(body_bytes[i..][0..4], &new_bytes);
                        }
                    }
                }

                // Re-append body
                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                // Adjust relocation offsets
                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size);
                }

                // Patch return-stmt jumps to the shared epilogue
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size;
                }
                const final_epilogue = body_epilogue_offset - body_start + prologue_size + prologue_start;
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);

                // Update procedure registry
                if (self.proc_registry.getPtr(key)) |entry| {
                    entry.code_start = prologue_start;
                    entry.code_end = self.codegen.currentOffset();
                }
            }

            // Restore state
            self.codegen.stack_offset = saved_stack_offset;
            self.codegen.callee_saved_used = saved_callee_saved_used;
            self.symbol_locations.deinit();
            self.symbol_locations = saved_symbol_locations.clone() catch return error.OutOfMemory;
            self.mutable_var_slots.deinit();
            self.mutable_var_slots = saved_mutable_var_slots.clone() catch return error.OutOfMemory;
        }

        /// Compile a lambda expression as a standalone procedure.
        /// Returns the code offset where the procedure starts.
        /// If the lambda was already compiled, returns the cached offset.
        /// Uses deferred prologue pattern for x86_64 to properly save callee-saved registers.
        fn compileLambdaAsProc(self: *Self, lambda_expr_id: LirExprId, lambda: anytype) Allocator.Error!usize {
            const key = @intFromEnum(lambda_expr_id);

            // Check if already compiled
            if (self.compiled_lambdas.get(key)) |code_offset| {
                return code_offset;
            }

            // Emit a jump over the lambda code to prevent fall-through
            // The lambda code is emitted in the code stream, so we need to skip it during normal execution
            const skip_jump = try self.codegen.emitJump();

            // Save current state - both stack offset AND symbol locations
            // IMPORTANT: We must save symbol_locations because the procedure has its own
            // parameter bindings that shouldn't pollute the caller's symbol map
            const saved_stack_offset = self.codegen.stack_offset;
            const saved_callee_saved_used = self.codegen.callee_saved_used;
            const saved_callee_saved_available = self.codegen.callee_saved_available;
            const saved_roc_ops_reg = self.roc_ops_reg;
            var saved_symbol_locations = self.symbol_locations.clone() catch return error.OutOfMemory;
            defer saved_symbol_locations.deinit();
            var saved_mutable_var_slots = self.mutable_var_slots.clone() catch return error.OutOfMemory;
            defer saved_mutable_var_slots.deinit();

            // Clear state for the procedure's scope
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();
            self.codegen.callee_saved_used = 0;
            // NOTE: Do NOT reset callee_saved_available to the full mask here!
            // The main procedure has reserved R12 (roc_ops) and RBX (result_ptr) which
            // must remain protected. The lambda shares the same roc_ops register, so if
            // we made R12 available, the lambda might allocate it as a scratch register
            // and overwrite the roc_ops pointer, causing crashes when calling builtins.

            // Mark R12/X20 as used so the prologue will save/restore it.
            // The lambda uses R12/X20 to hold roc_ops (inherited from the caller).
            // Without this, the prologue wouldn't save R12/X20, and if anything inside
            // the lambda (e.g., a called function) clobbers it, roc_ops would be lost.
            //
            // CRITICAL: Also REMOVE R12/X20 from callee_saved_available!
            // Otherwise, allocTempGeneral could allocate R12 as a temp register,
            // overwriting roc_ops and causing crashes when calling builtins/hosted functions.
            if (comptime target.toCpuArch() == .x86_64) {
                const r12_bit = @as(u16, 1) << @intFromEnum(x86_64.GeneralReg.R12);
                self.codegen.callee_saved_used |= r12_bit;
                // Remove R12 from available pool so it can't be allocated
                self.codegen.callee_saved_available &= ~(@as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12));
            } else {
                const x20_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20);
                self.codegen.callee_saved_used |= x20_bit;
                // Remove X20 from available pool so it can't be allocated
                self.codegen.callee_saved_available &= ~(@as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20));
            }

            // Save early return state before generating body
            const saved_early_return_ret_layout = self.early_return_ret_layout;
            const saved_early_return_patches_len = self.early_return_patches.items.len;
            const saved_ret_ptr_slot = self.ret_ptr_slot;

            // PHASE 1: Generate body first (to determine callee_saved_used)
            // Initialize stack_offset to reserve space for callee-saved area
            if (comptime target.toCpuArch() == .x86_64) {
                self.codegen.stack_offset = -CodeGen.CALLEE_SAVED_AREA_SIZE;
            } else {
                // aarch64: FP-relative addressing
                // Reserve space for: FP/LR (16 bytes) + callee-saved area (80 bytes)
                // First slot at FP + 16 + 80 = FP + 96
                self.codegen.stack_offset = 16 + CodeGen.CALLEE_SAVED_AREA_SIZE;
            }

            const body_start = self.codegen.currentOffset();
            const relocs_before = self.codegen.relocations.items.len;

            // Register before generating (for potential recursive calls)
            // Use body_start as temporary; will be updated after prologue prepend
            try self.compiled_lambdas.put(key, body_start);

            // Restore state on error
            errdefer {
                self.codegen.stack_offset = saved_stack_offset;
                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.roc_ops_reg = saved_roc_ops_reg;
                self.ret_ptr_slot = saved_ret_ptr_slot;
                self.symbol_locations.deinit();
                self.symbol_locations = saved_symbol_locations.clone() catch unreachable;
                self.mutable_var_slots.deinit();
                self.mutable_var_slots = saved_mutable_var_slots.clone() catch unreachable;
                _ = self.compiled_lambdas.remove(key);
                self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
                self.early_return_ret_layout = saved_early_return_ret_layout;
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
            }

            // Check if return type needs return-by-pointer (exceeds register limit)
            const needs_ret_ptr = self.needsInternalReturnByPointer(lambda.ret_layout);
            if (needs_ret_ptr) {
                // The first argument register contains a hidden return pointer.
                // Save it to a local stack slot for use after body generation.
                self.ret_ptr_slot = self.codegen.allocStackSlot(8);
                const first_reg = self.getArgumentRegister(0);
                try self.codegen.emitStoreStack(.w64, self.ret_ptr_slot.?, first_reg);
            } else {
                self.ret_ptr_slot = null;
            }

            // Bind parameters from argument registers
            // When needs_ret_ptr, arg registers are shifted by 1 (hidden ptr is arg 0)
            try self.bindLambdaParams(lambda.params, if (needs_ret_ptr) 1 else 0);

            // Set early return state so generateEarlyReturn can emit jumps
            self.early_return_ret_layout = lambda.ret_layout;

            // Generate the body
            const result_loc = try self.generateExpr(lambda.body);

            // Move result to return register or copy to return pointer
            if (self.ret_ptr_slot) |ret_slot| {
                try self.copyResultToReturnPointer(result_loc, lambda.ret_layout, ret_slot);
            } else {
                try self.moveToReturnRegisterWithLayout(result_loc, lambda.ret_layout);
            }

            // Record epilogue location (relative to body, will adjust after prepending prologue)
            const body_epilogue_offset = self.codegen.currentOffset();

            // Emit epilogue using DeferredFrameBuilder with actual stack usage
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

            // PHASE 2: Extract body and prepend prologue
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

                // Re-patch internal calls/addr whose targets are outside the shifted body
                self.repatchInternalCalls(body_start, body_end, prologue_size);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size);

                // Adjust early return patches (they point to locations within the body)
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size;
                }

                // Update compiled lambda entry with correct code_start
                try self.compiled_lambdas.put(key, prologue_start);

                // Patch early return jumps to point to the epilogue (now at adjusted offset)
                const final_epilogue_offset = body_epilogue_offset - body_start + prologue_size + prologue_start;
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue_offset);
                }

                // Restore early return state (trim patches back)
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
                self.early_return_ret_layout = saved_early_return_ret_layout;

                // Restore state
                self.codegen.stack_offset = saved_stack_offset;
                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.roc_ops_reg = saved_roc_ops_reg;
                self.ret_ptr_slot = saved_ret_ptr_slot;
                self.symbol_locations.deinit();
                self.symbol_locations = saved_symbol_locations.clone() catch return error.OutOfMemory;
                self.mutable_var_slots.deinit();
                self.mutable_var_slots = saved_mutable_var_slots.clone() catch return error.OutOfMemory;

                // Patch the skip jump to point here (after the lambda code)
                const after_lambda = self.codegen.currentOffset();
                self.codegen.patchJump(skip_jump, after_lambda);

                return prologue_start;
            } else {
                // aarch64: Use deferred prologue pattern too for consistency
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

                // Re-patch internal calls/addr whose targets are outside the shifted body
                self.repatchInternalCalls(body_start, body_end, prologue_size);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size);

                // Adjust early return patches
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size;
                }

                // Update compiled lambda entry
                try self.compiled_lambdas.put(key, prologue_start);

                // Patch early return jumps
                const final_epilogue_offset = body_epilogue_offset - body_start + prologue_size + prologue_start;
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue_offset);
                }

                // Restore early return state
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
                self.early_return_ret_layout = saved_early_return_ret_layout;

                // Restore state
                self.codegen.stack_offset = saved_stack_offset;
                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.roc_ops_reg = saved_roc_ops_reg;
                self.ret_ptr_slot = saved_ret_ptr_slot;
                self.symbol_locations.deinit();
                self.symbol_locations = saved_symbol_locations.clone() catch return error.OutOfMemory;
                self.mutable_var_slots.deinit();
                self.mutable_var_slots = saved_mutable_var_slots.clone() catch return error.OutOfMemory;

                // Patch the skip jump to point here (after the lambda code)
                const after_lambda = self.codegen.currentOffset();
                self.codegen.patchJump(skip_jump, after_lambda);

                return prologue_start;
            }
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
            if (self.layout_store) |ls| {
                if (@intFromEnum(ret_layout) < ls.layouts.len()) {
                    const layout_val = ls.getLayout(ret_layout);
                    if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                        return ls.layoutSizeAlign(layout_val).size > max_return_size;
                    }
                }
            }
            return false;
        }

        /// Save the return value from a call into a stack-based ValueLocation.
        /// Shared by generateCallToCompiledProc, generateCallToLambda, and
        /// generateIndirectCall. Handles i128/str/list/multi-reg struct/scalar returns.
        fn saveCallReturnValue(self: *Self, ret_layout: layout.Idx, needs_ret_ptr: bool, ret_buffer_offset: i32) Allocator.Error!ValueLocation {
            // If we used return-by-pointer, the callee has written the result
            // to our pre-allocated buffer. No register saving needed.
            if (needs_ret_ptr) {
                return .{ .stack = .{ .offset = ret_buffer_offset } };
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
            const is_list_return = if (self.layout_store) |ls| blk: {
                const layout_val = ls.getLayout(ret_layout);
                break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
            } else false;

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

            // Check if return type is a multi-register struct (record, tag_union, tuple > 8 bytes)
            if (self.layout_store) |ls| {
                const layout_val = ls.getLayout(ret_layout);
                if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
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
                            try self.spillArgToStack(.{ .general_reg = temp }, stack_arg_offset, 1);
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
                    const is_i128_arg = (arg_loc == .stack_i128 or arg_loc == .immediate_i128) or
                        (arg_loc == .stack and arg_layout != null and
                            (arg_layout.? == .dec or arg_layout.? == .i128 or arg_layout.? == .u128));
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
                            else => unreachable,
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
                            else => unreachable,
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
                        switch (arg_loc) {
                            .general_reg => |reg| {
                                if (reg != arg_reg) {
                                    try self.codegen.emit.movRegReg(.w64, arg_reg, reg);
                                }
                            },
                            .stack => |s| {
                                try self.codegen.emitLoadStack(.w64, arg_reg, s.offset);
                            },
                            .immediate_i64 => |val| {
                                try self.codegen.emitLoadImm(arg_reg, @bitCast(val));
                            },
                            .lambda_code => |lc| {
                                const current = self.codegen.currentOffset();
                                const rel: i64 = @as(i64, @intCast(lc.code_offset)) - @as(i64, @intCast(current));
                                try self.internal_addr_patches.append(self.allocator, .{
                                    .instr_offset = current,
                                    .target_offset = lc.code_offset,
                                });
                                if (comptime target.toCpuArch() == .aarch64) {
                                    try self.codegen.emit.adr(arg_reg, @intCast(rel));
                                } else {
                                    const lea_size: i64 = 7;
                                    try self.codegen.emit.leaRegRipRel(arg_reg, @intCast(rel - lea_size));
                                }
                            },
                            .closure_value => |cv| {
                                try self.codegen.emitLoadStack(.w64, arg_reg, cv.stack_offset);
                            },
                            else => {
                                try self.moveToReg(arg_loc, arg_reg);
                            },
                        }
                        reg_idx += 1;
                    }
                } else {
                    // Spill to stack — registers exhausted
                    try self.spillArgToStack(arg_loc, stack_arg_offset, info.num_regs);
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
                    try self.spillArgToStack(.{ .general_reg = roc_ops_reg }, stack_arg_offset, 1);
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

            if (self.layout_store) |ls| {
                if (@intFromEnum(layout_idx) < ls.layouts.len()) {
                    const layout_val = ls.getLayout(layout_idx);
                    // List parameters need 3 registers (24 bytes)
                    if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 3;
                    // Record, tag union, and tuple parameters may need multiple registers
                    if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
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

        fn bindLambdaParams(self: *Self, params: lir.LirPatternSpan, initial_reg_idx: u8) Allocator.Error!void {
            const pattern_ids = self.store.getPatternSpan(params);

            // Pre-scan: determine which params are passed by pointer.
            // This must match the logic in generateCallToLambda.
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
                        .record => |r| blk: {
                            const ls = self.layout_store orelse break :blk 1;
                            const rl = ls.getLayout(r.record_layout);
                            const sz = ls.layoutSizeAlign(rl).size;
                            break :blk @max(1, @as(u8, @intCast((sz + 7) / 8)));
                        },
                        .tuple => |t| blk: {
                            const ls = self.layout_store orelse break :blk 1;
                            const tl = ls.getLayout(t.tuple_layout);
                            const sz = ls.layoutSizeAlign(tl).size;
                            break :blk @max(1, @as(u8, @intCast((sz + 7) / 8)));
                        },
                        .list => 3,
                        else => 1,
                    };
                    param_num_regs[pi] = nr;
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
                    pre_reg_count -= (best_regs - 1);
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
                            } else if (self.layout_store) |ls| {
                                if (@intFromEnum(bind.layout_idx) < ls.layouts.len()) {
                                    const layout_val = ls.getLayout(bind.layout_idx);
                                    if (layout_val.tag == .list or layout_val.tag == .list_of_zst) {
                                        try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                            .struct_offset = local_stack_offset,
                                            .data_offset = 0,
                                            .num_elements = 0,
                                        } });
                                    } else {
                                        try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = local_stack_offset } });
                                    }
                                } else {
                                    try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = local_stack_offset } });
                                }
                            } else {
                                try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = local_stack_offset } });
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
                                const arg_reg0 = self.getArgumentRegister(reg_idx);
                                const arg_reg1 = self.getArgumentRegister(reg_idx + 1);

                                const stack_offset = self.codegen.allocStackSlot(16);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                                try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);

                                try self.symbol_locations.put(symbol_key, .{ .stack_i128 = stack_offset });
                                reg_idx += 2;
                            } else if (self.layout_store) |ls| {
                                if (@intFromEnum(bind.layout_idx) < ls.layouts.len()) {
                                    const layout_val = ls.getLayout(bind.layout_idx);
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
                                    if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                                        const size = ls.layoutSizeAlign(layout_val).size;
                                        if (size > 8) {
                                            const local_stack_offset = self.codegen.allocStackSlot(@intCast(size));
                                            var ri: u8 = 0;
                                            while (ri < num_regs) : (ri += 1) {
                                                const arg_r = self.getArgumentRegister(reg_idx + ri);
                                                try self.codegen.emitStoreStack(.w64, local_stack_offset + @as(i32, ri) * 8, arg_r);
                                            }
                                            try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = local_stack_offset } });
                                            reg_idx += num_regs;
                                            continue;
                                        }
                                    }
                                }
                                // Default: single 8-byte value
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                const stack_offset = self.codegen.allocStackSlot(8);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                                try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = stack_offset } });
                                reg_idx += 1;
                            } else {
                                // Default: single 8-byte value
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                const stack_offset = self.codegen.allocStackSlot(8);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                                try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = stack_offset } });
                                reg_idx += 1;
                            }
                        } else {
                            // Doesn't fit in registers - read from caller's stack frame
                            const size: u32 = @as(u32, num_regs) * 8;
                            const local_stack_offset = self.codegen.allocStackSlot(@intCast(size));
                            try self.copyFromCallerStack(stack_arg_offset, local_stack_offset, num_regs);
                            try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = local_stack_offset } });
                            stack_arg_offset += @as(i32, num_regs) * 8;
                            reg_idx = max_arg_regs; // Mark all registers as consumed
                        }
                    },
                    .wildcard => |wc| {
                        // Skip this argument - use the layout to determine how many
                        // registers it occupies (important for correct roc_ops placement)
                        const num_regs = self.calcParamRegCount(wc.layout_idx);
                        if (param_pass_by_ptr[param_idx]) {
                            reg_idx += 1; // passed by pointer, skip 1 register
                        } else if (reg_idx + num_regs <= max_arg_regs) {
                            reg_idx += num_regs;
                        } else {
                            stack_arg_offset += @as(i32, num_regs) * 8;
                            reg_idx = max_arg_regs;
                        }
                    },
                    .record => |rec| {
                        // Record destructuring: store registers to stack, then delegate to bindPattern
                        const ls = self.layout_store orelse unreachable;
                        const record_layout = ls.getLayout(rec.record_layout);
                        const size = ls.layoutSizeAlign(record_layout).size;
                        const num_regs: u8 = @max(1, @as(u8, @intCast((size + 7) / 8)));

                        if (param_pass_by_ptr[param_idx]) {
                            // Passed by pointer: copy from pointer to local stack.
                            // Use hardcoded temp to avoid clobbering the arg register.
                            const temp_r: GeneralReg = scratch_reg;
                            const stack_offset = self.codegen.allocStackSlot(@intCast(size));
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
                            const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                            var ri: u8 = 0;
                            while (ri < num_regs) : (ri += 1) {
                                const arg_r = self.getArgumentRegister(reg_idx + ri);
                                try self.codegen.emitStoreStack(.w64, stack_offset + @as(i32, ri) * 8, arg_r);
                            }
                            reg_idx += num_regs;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        } else {
                            // Read from caller's stack
                            const stack_offset = self.codegen.allocStackSlot(@intCast(size));
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
                    .tuple => |tup| {
                        // Tuple destructuring: store registers to stack, then delegate to bindPattern
                        const ls = self.layout_store orelse unreachable;
                        const tuple_layout = ls.getLayout(tup.tuple_layout);
                        const size = ls.layoutSizeAlign(tuple_layout).size;
                        const num_regs: u8 = @max(1, @as(u8, @intCast((size + 7) / 8)));

                        if (param_pass_by_ptr[param_idx]) {
                            // Passed by pointer: copy from pointer to local stack.
                            // Use hardcoded temp to avoid clobbering the arg register.
                            const temp_r: GeneralReg = scratch_reg;
                            const stack_offset = self.codegen.allocStackSlot(@intCast(size));
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
                            const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                            var ri: u8 = 0;
                            while (ri < num_regs) : (ri += 1) {
                                const arg_r = self.getArgumentRegister(reg_idx + ri);
                                try self.codegen.emitStoreStack(.w64, stack_offset + @as(i32, ri) * 8, arg_r);
                            }
                            reg_idx += num_regs;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        } else {
                            // Read from caller's stack
                            const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                            try self.copyFromCallerStack(stack_arg_offset, stack_offset, num_regs);
                            stack_arg_offset += @as(i32, num_regs) * 8;
                            reg_idx = max_arg_regs;
                            try self.bindPattern(pattern_id, .{ .stack = .{ .offset = stack_offset } });
                        }
                    },
                    else => {
                        // For now, skip complex patterns - assume 1 register
                        if (reg_idx < max_arg_regs) {
                            reg_idx += 1;
                        } else {
                            stack_arg_offset += 8;
                        }
                    },
                }
            }

            // Receive roc_ops as the final argument (passed by generateCallToLambda)
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
            // First check if the layout tells us this is a multi-register type > 8 bytes
            if (self.layout_store) |ls| {
                const layout_val = ls.getLayout(ret_layout);

                // Lists and strings are always 24 bytes (3 registers)
                if (layout_val.tag == .list or layout_val.tag == .list_of_zst) {
                    const stack_offset: i32 = switch (loc) {
                        .stack => |s| s.offset,
                        .list_stack => |info| info.struct_offset,
                        else => return self.moveToReturnRegister(loc),
                    };
                    try self.codegen.emitLoadStack(.w64, ret_reg_0, stack_offset);
                    try self.codegen.emitLoadStack(.w64, ret_reg_1, stack_offset + 8);
                    try self.codegen.emitLoadStack(.w64, ret_reg_2, stack_offset + 16);
                    return;
                }

                if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                    const size_align = ls.layoutSizeAlign(layout_val);
                    if (size_align.size > 8) {
                        // Large struct - need to return in multiple registers
                        const stack_offset: i32 = switch (loc) {
                            .stack => |s| s.offset,
                            else => {
                                // For non-stack locations, fall through to regular handling
                                return self.moveToReturnRegister(loc);
                            },
                        };
                        const num_regs = (size_align.size + 7) / 8;
                        if (comptime target.toCpuArch() == .aarch64) {
                            // Use X0-X7 for first 8 words, then X9-X12 for overflow
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
                        return;
                    }
                }
            }
            // i128/u128/Dec need two registers (X0/X1 or RAX/RDX)
            if (ret_layout == .i128 or ret_layout == .u128 or ret_layout == .dec) {
                switch (loc) {
                    .stack_i128 => |offset| {
                        try self.codegen.emitLoadStack(.w64, ret_reg_0, offset);
                        try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                        return;
                    },
                    .stack => |s| {
                        const offset = s.offset;
                        try self.codegen.emitLoadStack(.w64, ret_reg_0, offset);
                        try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                        return;
                    },
                    // Other locations (immediates, etc.) fall through to regular handling
                    else => {},
                }
            }

            // Fall back to regular handling
            return self.moveToReturnRegister(loc);
        }

        /// Copy a result value to the hidden return pointer buffer.
        /// Used when the return type exceeds the register limit and the caller
        /// has passed a pointer to a pre-allocated buffer as a hidden first argument.
        fn copyResultToReturnPointer(self: *Self, result_loc: ValueLocation, ret_layout: layout.Idx, ret_ptr_stack_slot: i32) Allocator.Error!void {
            const ls = self.layout_store orelse unreachable;
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

        fn moveToReturnRegister(self: *Self, loc: ValueLocation) Allocator.Error!void {
            const ret_reg = self.getReturnRegister();
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
                .stack_str => |offset| {
                    // String return (24 bytes) - load into X0/X1/X2 or RAX/RDX/RCX
                    try self.codegen.emitLoadStack(.w64, ret_reg, offset);
                    try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                    try self.codegen.emitLoadStack(.w64, ret_reg_2, offset + 16);
                },
                .list_stack => |info| {
                    // List return (24 bytes) - load into X0/X1/X2 or RAX/RDX/RCX
                    try self.codegen.emitLoadStack(.w64, ret_reg, info.struct_offset);
                    try self.codegen.emitLoadStack(.w64, ret_reg_1, info.struct_offset + 8);
                    try self.codegen.emitLoadStack(.w64, ret_reg_2, info.struct_offset + 16);
                },
                .stack_i128 => |offset| {
                    // For i128/Dec return values, load both halves
                    // X0 = low 64 bits, X1 = high 64 bits
                    try self.codegen.emitLoadStack(.w64, ret_reg, offset);
                    try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                },
                .immediate_i128 => |val| {
                    // Load low 64 bits to X0, high 64 bits to X1
                    const low: i64 = @truncate(val);
                    const high: i64 = @truncate(val >> 64);
                    try self.codegen.emitLoadImm(ret_reg, low);
                    try self.codegen.emitLoadImm(ret_reg_1, high);
                },
                .lambda_code => |lc| {
                    // Return lambda code location: code_offset in X0/RAX, ret_layout in X1/RDX
                    try self.codegen.emitLoadImm(ret_reg, @bitCast(@as(i64, @intCast(lc.code_offset))));
                    try self.codegen.emitLoadImm(ret_reg_1, @intFromEnum(lc.ret_layout));
                },
                .closure_value => {
                    // Can't return a closure_value from a compiled procedure - the
                    // closure's capture data lives on this procedure's stack frame
                    // which is deallocated on return.
                    unreachable;
                },
                else => {
                    // For other types (like float_reg), try to handle appropriately
                },
            }
        }

        /// Generate a call to a compiled lambda procedure.
        /// Puts arguments in registers and emits a call instruction.
        ///
        /// Uses a two-pass approach to avoid register clobbering:
        /// 1. First generate all argument expressions (which may trigger nested calls
        ///    or allocate temp registers)
        /// 2. Then load the stable results into argument registers
        ///
        /// This prevents a bug where generating arg[1] could clobber argument
        /// registers (X2-X7) that were already loaded with arg[0]'s data.
        ///
        /// When registers are exhausted, spills remaining arguments to the stack.
        /// Call target for lambda calls: either a direct code offset or an
        /// indirect function pointer (for higher-order function arguments).
        const CallTarget = union(enum) {
            direct: usize,
            indirect: ValueLocation,
        };

        fn generateCallToLambda(self: *Self, code_offset: usize, args_span: anytype, ret_layout: layout.Idx) Allocator.Error!ValueLocation {
            return self.generateLambdaOrIndirectCall(.{ .direct = code_offset }, args_span, ret_layout);
        }

        fn generateIndirectCall(self: *Self, fn_ptr_loc: ValueLocation, args_span: anytype, ret_layout: layout.Idx) Allocator.Error!ValueLocation {
            return self.generateLambdaOrIndirectCall(.{ .indirect = fn_ptr_loc }, args_span, ret_layout);
        }

        /// Unified implementation for direct lambda calls and indirect (function pointer) calls.
        /// Both follow the same calling convention: args in registers with pass-by-pointer
        /// for overflowing multi-reg args, roc_ops as the final argument.
        fn generateLambdaOrIndirectCall(self: *Self, call_target: CallTarget, args_span: anytype, ret_layout: layout.Idx) Allocator.Error!ValueLocation {
            const args = self.store.getExprSpan(args_span);

            // For indirect calls, save the function pointer before arg setup
            // since arg register loading might clobber it.
            var fn_ptr_stack: i32 = 0;
            if (call_target == .indirect) {
                fn_ptr_stack = self.codegen.allocStackSlot(8);
                switch (call_target.indirect) {
                    .general_reg => |reg| {
                        try self.codegen.emitStoreStack(.w64, fn_ptr_stack, reg);
                    },
                    .stack => |s| {
                        const temp = try self.allocTempGeneral();
                        try self.codegen.emitLoadStack(.w64, temp, s.offset);
                        try self.codegen.emitStoreStack(.w64, fn_ptr_stack, temp);
                        self.codegen.freeGeneral(temp);
                    },
                    .immediate_i64 => |val| {
                        const temp = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(temp, @bitCast(val));
                        try self.codegen.emitStoreStack(.w64, fn_ptr_stack, temp);
                        self.codegen.freeGeneral(temp);
                    },
                    else => unreachable,
                }
            }

            // Pass 1: Generate all argument expressions and calculate register needs.
            // When a closure_value is passed as an argument to a higher-order function,
            // the callee will call it through a function pointer (BLR/CALL reg).
            // We compile the lambda as a proc and pass the code address,
            // not the raw closure data (tag bytes, captures, etc).
            const arg_infos_start = self.scratch_arg_infos.top();
            defer self.scratch_arg_infos.clearFrom(arg_infos_start);

            for (args) |arg_id| {
                var arg_loc = try self.generateExpr(arg_id);
                if (arg_loc == .closure_value) {
                    const cv = arg_loc.closure_value;
                    const lambda_expr = self.store.getExpr(cv.lambda);
                    const lambda = switch (lambda_expr) {
                        .lambda => |l| l,
                        .closure => |c_id| blk: {
                            const c = self.store.getClosureData(c_id);
                            const inner = self.store.getExpr(c.lambda);
                            break :blk inner.lambda;
                        },
                        else => unreachable,
                    };
                    const cv_code_offset = try self.compileLambdaAsProc(cv.lambda, lambda);
                    arg_loc = .{ .lambda_code = .{
                        .code_offset = cv_code_offset,
                        .ret_layout = lambda.ret_layout,
                    } };
                }
                const arg_layout = self.getExprLayout(arg_id);
                const num_regs = self.calcArgRegCount(arg_loc, arg_layout);
                try self.scratch_arg_infos.append(.{ .loc = arg_loc, .layout_idx = arg_layout, .num_regs = num_regs });
            }
            const arg_infos = self.scratch_arg_infos.sliceFromStart(arg_infos_start);

            // Check if return type exceeds register limit and needs return-by-pointer.
            // If so, the first argument register carries a hidden pointer to a caller-
            // allocated buffer, and all other args shift right by one register.
            const needs_ret_ptr = self.needsInternalReturnByPointer(ret_layout);
            var ret_buffer_offset: i32 = 0;
            if (needs_ret_ptr) {
                const ls = self.layout_store orelse unreachable;
                const ret_layout_val = ls.getLayout(ret_layout);
                const ret_size = ls.layoutSizeAlign(ret_layout_val).size;
                ret_buffer_offset = self.codegen.allocStackSlot(ret_size);
            }

            // Pre-compute which multi-register args should be passed by pointer.
            // The deferred prologue pattern makes FP-relative stack-arg offsets
            // unknowable at body-gen time, so we pass large args by pointer instead.
            // We convert overflowing multi-reg args first, then convert more
            // (largest first, scanning backwards) if roc_ops would still spill.
            const pbp_start = self.scratch_pass_by_ptr.top();
            defer self.scratch_pass_by_ptr.clearFrom(pbp_start);
            for (0..args.len) |_| try self.scratch_pass_by_ptr.append(false);
            const pass_by_ptr = self.scratch_pass_by_ptr.sliceFromStart(pbp_start);

            {
                var reg_count: u8 = if (needs_ret_ptr) 1 else 0;
                for (arg_infos, 0..) |ai, i| {
                    const nr = ai.num_regs;
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
                while (reg_count + 1 > max_arg_regs) {
                    var found = false;
                    var best_idx: usize = 0;
                    var best_regs: u8 = 0;
                    for (arg_infos, 0..) |ai, i| {
                        if (!pass_by_ptr[i] and ai.num_regs > 1 and ai.num_regs > best_regs) {
                            best_idx = i;
                            best_regs = ai.num_regs;
                            found = true;
                        }
                    }
                    if (!found) break;
                    pass_by_ptr[best_idx] = true;
                    reg_count -= (best_regs - 1);
                }
            }

            // Place arguments using shared helper
            const stack_spill_size = try self.placeCallArguments(arg_infos, .{
                .needs_ret_ptr = needs_ret_ptr,
                .ret_buffer_offset = ret_buffer_offset,
                .pass_by_ptr = pass_by_ptr,
                .emit_roc_ops = true,
            });

            // Emit call instruction
            switch (call_target) {
                .direct => |code_offset| try self.emitCallToOffset(code_offset),
                .indirect => {
                    // Load function pointer from saved stack slot and call indirectly.
                    // Use a dedicated scratch register that can never be an argument register,
                    // since the argument registers (X0-X7 / RDI,RSI,RDX,RCX,R8,R9) are
                    // already loaded with call arguments at this point.
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emitLoadStack(.w64, .IP0, fn_ptr_stack);
                        try self.codegen.emit.blrReg(.IP0);
                    } else {
                        try self.codegen.emitLoadStack(.w64, .R10, fn_ptr_stack);
                        try self.codegen.emit.callReg(.R10);
                    }
                },
            }

            // Clean up stack space for spilled arguments
            if (stack_spill_size > 0) {
                try self.emitAddStackPtr(stack_spill_size);
            }

            return self.saveCallReturnValue(ret_layout, needs_ret_ptr, ret_buffer_offset);
        }

        /// Stack size for main expression locals. Needs to be large enough for builtins
        /// like List.map which can use 400+ bytes.
        const MAIN_STACK_SIZE: u32 = 1024;

        /// Create a ForwardFrameBuilder configured for the main expression frame.
        /// Shared between prologue and epilogue to ensure they always match.
        fn initMainFrameBuilder(self: *Self) ForwardFrameBuilder {
            var frame = ForwardFrameBuilder.init(&self.codegen.emit);
            if (comptime target.toCpuArch() == .aarch64) {
                // Save X19 and X20 (callee-saved) which we use for result ptr and RocOps ptr
                frame.saveViaPush(.X19);
                frame.saveViaPush(.X20);
            } else {
                // Save RBX and R12 (callee-saved) which we use for result ptr and RocOps ptr
                frame.saveViaPush(.RBX);
                frame.saveViaPush(.R12);
            }
            frame.setStackSize(MAIN_STACK_SIZE);
            return frame;
        }

        /// Emit prologue for main expression code.
        /// Sets up frame pointer and saves callee-saved registers using ForwardFrameBuilder.
        fn emitMainPrologue(self: *Self) Allocator.Error!void {
            var frame = self.initMainFrameBuilder();
            self.codegen.stack_offset = try frame.emitPrologue();
        }

        /// Emit epilogue for main expression code.
        /// Restores callee-saved registers and frame pointer using ForwardFrameBuilder, then returns.
        fn emitMainEpilogue(self: *Self) Allocator.Error!void {
            var frame = self.initMainFrameBuilder();
            try frame.emitEpilogue();
        }

        /// Stack size for entrypoint wrapper locals.
        const ENTRYPOINT_STACK_SIZE: u32 = 64;

        /// Create a ForwardFrameBuilder configured for the entrypoint wrapper frame.
        /// Shared between prologue and epilogue to ensure they always match.
        /// Saves: roc_ops, ret_ptr, args_ptr into callee-saved registers.
        fn initEntrypointFrameBuilder(self: *Self) ForwardFrameBuilder {
            var frame = ForwardFrameBuilder.init(&self.codegen.emit);
            if (comptime target.toCpuArch() == .aarch64) {
                frame.saveViaPush(.X19); // roc_ops
                frame.saveViaPush(.X20); // ret_ptr
                frame.saveViaPush(.X21); // args_ptr
            } else {
                frame.saveViaPush(.RBX); // ret_ptr
                frame.saveViaPush(.R12); // roc_ops
                frame.saveViaPush(.R13); // args_ptr
            }
            frame.setStackSize(ENTRYPOINT_STACK_SIZE);
            return frame;
        }

        /// Bind procedure parameters to argument registers.
        /// Handles stack spilling when arguments exceed available registers.
        fn bindProcParams(self: *Self, params: lir.LirPatternSpan, param_layouts: LayoutIdxSpan) Allocator.Error!void {
            const pattern_ids = self.store.getPatternSpan(params);
            const layouts = self.store.getLayoutIdxSpan(param_layouts);

            // Track current register index separately from parameter index
            // because 128-bit parameters consume 2 registers
            var reg_idx: u8 = 0;
            // Track offset for stack arguments (first stack arg at RBP+16/FP+16)
            var stack_arg_offset: i32 = 16;

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

                        // Check if this is a string type (24 bytes)
                        const is_str = if (param_idx < layouts.len)
                            layouts[param_idx] == .str
                        else
                            false;

                        // Check if this is a list type (24 bytes)
                        const is_list = if (param_idx < layouts.len) blk: {
                            const param_layout = layouts[param_idx];
                            if (self.layout_store) |ls| {
                                if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                                    break :blk false;
                                }
                                const layout_val = ls.getLayout(param_layout);
                                break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                            }
                            break :blk false;
                        } else false;

                        // Determine number of registers needed
                        const num_regs: u8 = if (is_128bit) 2 else if (is_str or is_list) 3 else 1;

                        if (reg_idx + num_regs <= max_arg_regs) {
                            // Fits in registers - use register-based loading
                            if (is_128bit) {
                                if (comptime target.toCpuArch() == .aarch64) {
                                    if (reg_idx % 2 != 0) {
                                        reg_idx += 1; // Skip to even register for alignment
                                    }
                                }

                                const low_reg = self.getArgumentRegister(reg_idx);
                                const high_reg = self.getArgumentRegister(reg_idx + 1);
                                const stack_offset = self.codegen.allocStack(16);
                                try self.codegen.emitStoreStack(.w64, stack_offset, low_reg);
                                try self.codegen.emitStoreStack(.w64, stack_offset + 8, high_reg);
                                try self.symbol_locations.put(symbol_key, .{ .stack_i128 = stack_offset });
                                reg_idx += 2;
                            } else if (is_str) {
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
                                const stack_offset = self.codegen.allocStackSlot(roc_str_size);
                                const reg0 = self.getArgumentRegister(reg_idx);
                                const reg1 = self.getArgumentRegister(reg_idx + 1);
                                const reg2 = self.getArgumentRegister(reg_idx + 2);
                                try self.emitStore(.w64, frame_ptr, stack_offset, reg0);
                                try self.emitStore(.w64, frame_ptr, stack_offset + 8, reg1);
                                try self.emitStore(.w64, frame_ptr, stack_offset + 16, reg2);
                                try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                    .struct_offset = stack_offset,
                                    .data_offset = 0,
                                    .num_elements = 0,
                                } });
                                reg_idx += 3;
                            } else {
                                // Normal 64-bit or smaller parameter
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                const stack_offset = self.codegen.allocStackSlot(8);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                                try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = stack_offset } });
                                reg_idx += 1;
                            }
                        } else {
                            // Doesn't fit in registers - read from caller's stack frame
                            const size: u32 = @as(u32, num_regs) * 8;
                            const local_stack_offset = self.codegen.allocStackSlot(@intCast(size));
                            try self.copyFromCallerStack(stack_arg_offset, local_stack_offset, num_regs);

                            // Set up symbol location based on type
                            if (is_128bit) {
                                try self.symbol_locations.put(symbol_key, .{ .stack_i128 = local_stack_offset });
                            } else if (is_str) {
                                try self.symbol_locations.put(symbol_key, .{ .stack_str = local_stack_offset });
                            } else if (is_list) {
                                try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                    .struct_offset = local_stack_offset,
                                    .data_offset = 0,
                                    .num_elements = 0,
                                } });
                            } else {
                                try self.symbol_locations.put(symbol_key, .{ .stack = .{ .offset = local_stack_offset } });
                            }

                            stack_arg_offset += @as(i32, num_regs) * 8;
                            reg_idx = max_arg_regs; // Mark all registers as consumed
                        }
                    },
                    else => {
                        // Complex parameter patterns not yet supported
                        // Assume 1 register for now
                        if (reg_idx < max_arg_regs) {
                            reg_idx += 1;
                        } else {
                            stack_arg_offset += 8;
                        }
                    },
                }
            }
        }

        /// Generate code for a control flow statement
        fn generateStmt(self: *Self, stmt_id: CFStmtId) Allocator.Error!void {
            const stmt = self.store.getCFStmt(stmt_id);

            switch (stmt) {
                .let_stmt => |let_s| {
                    // Evaluate the value
                    const value_loc = try self.generateExpr(let_s.value);
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

                    // Handle i128/Dec return values specially (need two registers)
                    if (value_loc == .stack_i128 or value_loc == .immediate_i128) {
                        if (comptime target.toCpuArch() == .aarch64) {
                            // aarch64: return i128 in X0 (low), X1 (high)
                            switch (value_loc) {
                                .stack_i128 => |offset| {
                                    try self.codegen.emitLoadStack(.w64, .X0, offset);
                                    try self.codegen.emitLoadStack(.w64, .X1, offset + 8);
                                },
                                .immediate_i128 => |val| {
                                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                                    try self.codegen.emitLoadImm(.X0, @bitCast(low));
                                    try self.codegen.emitLoadImm(.X1, @bitCast(high));
                                },
                                else => unreachable,
                            }
                        } else {
                            // x86_64: return i128 in RAX (low), RDX (high)
                            switch (value_loc) {
                                .stack_i128 => |offset| {
                                    try self.codegen.emitLoadStack(.w64, .RAX, offset);
                                    try self.codegen.emitLoadStack(.w64, .RDX, offset + 8);
                                },
                                .immediate_i128 => |val| {
                                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                                    try self.codegen.emitLoadImm(.RAX, @bitCast(low));
                                    try self.codegen.emitLoadImm(.RDX, @bitCast(high));
                                },
                                else => unreachable,
                            }
                        }
                    } else if (value_loc == .stack_str) {
                        // String return (24 bytes) - return in X0, X1, X2 (aarch64) or RAX, RDX, RCX (x86_64)
                        const offset = value_loc.stack_str;
                        try self.codegen.emitLoadStack(.w64, ret_reg_0, offset);
                        try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                        try self.codegen.emitLoadStack(.w64, ret_reg_2, offset + 16);
                    } else if (value_loc == .list_stack) {
                        // List return (24 bytes) - return in X0, X1, X2 (aarch64) or RAX, RDX, RCX (x86_64)
                        const offset = value_loc.list_stack.struct_offset;
                        try self.codegen.emitLoadStack(.w64, ret_reg_0, offset);
                        try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                        try self.codegen.emitLoadStack(.w64, ret_reg_2, offset + 16);
                    } else if (value_loc == .stack) {
                        // Check expression layout for multi-register returns
                        var is_str = false;
                        var is_list = false;
                        var is_i128 = false;
                        var is_large_record = false;
                        var record_size: u32 = 0;
                        const expr_layout_opt = self.getExprLayout(r.value);
                        if (expr_layout_opt) |ret_layout| {
                            is_str = ret_layout == .str;
                        }
                        if (self.layout_store) |ls| {
                            if (expr_layout_opt) |ret_layout| {
                                const layout_val = ls.getLayout(ret_layout);
                                is_list = layout_val.tag == .list or layout_val.tag == .list_of_zst;
                                is_i128 = ret_layout == .i128 or ret_layout == .u128 or ret_layout == .dec;
                                if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                                    const sa = ls.layoutSizeAlign(layout_val);
                                    if (sa.size > 8) {
                                        is_large_record = true;
                                        record_size = sa.size;
                                    }
                                }
                            }
                        }

                        if (is_str or is_list) {
                            // String/List return (24 bytes) from .stack location
                            const offset = value_loc.stack.offset;
                            try self.codegen.emitLoadStack(.w64, ret_reg_0, offset);
                            try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                            try self.codegen.emitLoadStack(.w64, ret_reg_2, offset + 16);
                        } else if (is_i128) {
                            // i128/Dec return (16 bytes) from .stack location
                            const offset = value_loc.stack.offset;
                            try self.codegen.emitLoadStack(.w64, ret_reg_0, offset);
                            try self.codegen.emitLoadStack(.w64, ret_reg_1, offset + 8);
                        } else if (is_large_record) {
                            // Large record return - load into multiple registers
                            const offset = value_loc.stack.offset;
                            const num_regs = (record_size + 7) / 8;
                            if (comptime target.toCpuArch() == .aarch64) {
                                const regs = [_]@TypeOf(GeneralReg.X0){ .X0, .X1, .X2, .X3 };
                                for (0..@min(num_regs, 4)) |i| {
                                    try self.codegen.emitLoadStack(.w64, regs[i], offset + @as(i32, @intCast(i * 8)));
                                }
                            } else {
                                const regs = [_]@TypeOf(GeneralReg.RAX){ .RAX, .RDX, .RCX, .R8 };
                                for (0..@min(num_regs, 4)) |i| {
                                    try self.codegen.emitLoadStack(.w64, regs[i], offset + @as(i32, @intCast(i * 8)));
                                }
                            }
                        } else {
                            // Normal 64-bit return from stack
                            const return_reg = self.getReturnRegister();
                            const value_reg = try self.ensureInGeneralReg(value_loc);
                            if (value_reg != return_reg) {
                                try self.emitMovRegReg(return_reg, value_reg);
                            }
                        }
                    } else {
                        // Move to return register (64-bit values)
                        const return_reg = self.getReturnRegister();
                        const value_reg = try self.ensureInGeneralReg(value_loc);
                        if (value_reg != return_reg) {
                            try self.emitMovRegReg(return_reg, value_reg);
                        }
                    }
                    // Emit jump to shared epilogue (patched after body gen knows actual frame size)
                    const patch = try self.codegen.emitJump();
                    try self.early_return_patches.append(self.allocator, patch);
                },

                .expr_stmt => |e| {
                    // Evaluate expression for side effects
                    _ = try self.generateExpr(e.value);
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
                                if (self.layout_store) |ls| {
                                    // Bounds check for cross-module layouts
                                    if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                                        break :blk false;
                                    }
                                    const layout_val = ls.getLayout(param_layout);
                                    break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                                }
                                break :blk false;
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
                        // Consume register slots for this param to maintain correct
                        // register indexing, but don't bind any symbol.
                        const is_128bit = if (param_idx < layouts.len) blk: {
                            const param_layout = layouts[param_idx];
                            break :blk param_layout == .i128 or param_layout == .u128 or param_layout == .dec;
                        } else false;

                        if (is_128bit) {
                            reg_idx += 2;
                        } else {
                            const is_str = if (param_idx < layouts.len)
                                layouts[param_idx] == .str
                            else
                                false;

                            const is_list = if (param_idx < layouts.len) blk: {
                                const param_layout = layouts[param_idx];
                                if (self.layout_store) |ls| {
                                    if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                                        break :blk false;
                                    }
                                    const layout_val = ls.getLayout(param_layout);
                                    break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                                }
                                break :blk false;
                            } else false;

                            if (is_str or is_list) {
                                reg_idx += 3;
                            } else {
                                reg_idx += 1;
                            }
                        }
                    },
                    .int_literal, .float_literal, .str_literal, .tag, .record, .tuple, .list, .as_pattern => unreachable, // Join point params must be simple bindings or wildcards
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

            // Optimization: skip temp copy when there's only 1 param (no overlap possible)
            if (arg_locs.len <= 1) {
                try self.rebindSingleParam(arg_locs, pattern_ids, layouts);
                return;
            }

            // Two-phase copy to avoid clobbering when params reference each other
            // (e.g., `jump jp(b, a)` swaps params)

            // Phase 1: Copy all sources to temp stack slots
            const TempInfo = struct { offset: i32, size: u8 };
            var temp_infos: std.ArrayListUnmanaged(TempInfo) = .empty;
            defer temp_infos.deinit(self.allocator);

            for (arg_locs, 0..) |loc, param_idx| {
                if (param_idx >= pattern_ids.len) {
                    try temp_infos.append(self.allocator, .{ .offset = 0, .size = 0 });
                    continue;
                }

                const pattern = self.store.getPattern(pattern_ids[param_idx]);
                switch (pattern) {
                    .bind => {},
                    .wildcard => {
                        try temp_infos.append(self.allocator, .{ .offset = 0, .size = 0 });
                        continue;
                    },
                    .int_literal, .float_literal, .str_literal, .tag, .record, .tuple, .list, .as_pattern => unreachable,
                }

                const dst_loc = self.symbol_locations.get(switch (pattern) {
                    .bind => |bind| @bitCast(bind.symbol),
                    else => unreachable,
                }) orelse {
                    try temp_infos.append(self.allocator, .{ .offset = 0, .size = 0 });
                    continue;
                };

                // Determine param size
                const is_str = if (param_idx < layouts.len)
                    layouts[param_idx] == .str
                else
                    (dst_loc == .stack_str);

                const is_list = if (param_idx < layouts.len) blk: {
                    const param_layout = layouts[param_idx];
                    if (self.layout_store) |ls| {
                        if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                            break :blk (loc == .list_stack or dst_loc == .list_stack);
                        }
                        const layout_val = ls.getLayout(param_layout);
                        break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                    }
                    break :blk (loc == .list_stack or dst_loc == .list_stack);
                } else (loc == .list_stack or dst_loc == .list_stack);

                const is_i128 = dst_loc == .stack_i128;

                const size: u8 = if (is_list or is_str) 24 else if (is_i128) 16 else 8;
                const temp_offset = self.codegen.allocStackSlot(size);

                // Copy source to temp
                try self.copyParamValueToStack(loc, temp_offset, size, is_i128);

                try temp_infos.append(self.allocator, .{ .offset = temp_offset, .size = size });
            }

            // Phase 2: Copy from temp slots to destination slots
            for (temp_infos.items, 0..) |temp_info, param_idx| {
                if (temp_info.size == 0) continue;
                if (param_idx >= pattern_ids.len) continue;

                const pattern = self.store.getPattern(pattern_ids[param_idx]);
                const symbol_key: u64 = switch (pattern) {
                    .bind => |bind| @bitCast(bind.symbol),
                    else => continue,
                };

                const dst_loc = self.symbol_locations.get(symbol_key) orelse continue;
                const dst_offset: i32 = switch (dst_loc) {
                    .stack => |s| s.offset,
                    .list_stack => |ls_info| ls_info.struct_offset,
                    .stack_i128 => |off| off,
                    .stack_str => |off| off,
                    .general_reg, .float_reg, .immediate_i64, .immediate_i128, .immediate_f64, .closure_value, .lambda_code => unreachable,
                };

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

        /// Fast path for single-param rebind (no clobbering possible)
        fn rebindSingleParam(self: *Self, arg_locs: []const ValueLocation, pattern_ids: anytype, layouts: anytype) Allocator.Error!void {
            for (arg_locs, 0..) |loc, param_idx| {
                if (param_idx >= pattern_ids.len) continue;

                const pattern = self.store.getPattern(pattern_ids[param_idx]);
                const symbol_key: u64 = switch (pattern) {
                    .bind => |bind| @bitCast(bind.symbol),
                    .wildcard => continue,
                    .int_literal, .float_literal, .str_literal, .tag, .record, .tuple, .list, .as_pattern => unreachable,
                };

                const dst_loc = self.symbol_locations.get(symbol_key) orelse continue;

                const is_str = if (param_idx < layouts.len)
                    layouts[param_idx] == .str
                else
                    (dst_loc == .stack_str);

                const is_list = if (param_idx < layouts.len) blk: {
                    const param_layout = layouts[param_idx];
                    if (self.layout_store) |ls| {
                        if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                            break :blk (loc == .list_stack or dst_loc == .list_stack);
                        }
                        const layout_val = ls.getLayout(param_layout);
                        break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                    }
                    break :blk (loc == .list_stack or dst_loc == .list_stack);
                } else (loc == .list_stack or dst_loc == .list_stack);

                const dst_offset: i32 = switch (dst_loc) {
                    .stack => |s| s.offset,
                    .list_stack => |ls_info| ls_info.struct_offset,
                    .stack_i128 => |off| off,
                    .stack_str => |off| off,
                    .general_reg, .float_reg, .immediate_i64, .immediate_i128, .immediate_f64, .closure_value, .lambda_code => unreachable,
                };

                const is_i128 = dst_loc == .stack_i128;
                const size: u8 = if (is_list or is_str) 24 else if (is_i128) 16 else 8;
                try self.copyParamValueToStack(loc, dst_offset, size, is_i128);
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
            const ls = self.layout_store orelse unreachable;
            const value_layout_val = ls.getLayout(ms.value_layout);
            const tu_disc_offset: i32 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk @intCast(tu_data.discriminant_offset);
            } else 0;
            const tu_total_size: u32 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk tu_data.size;
            } else 0;
            const tu_disc_size: u8 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk tu_data.discriminant_size;
            } else 4;
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

                        // Bind tag payload fields
                        try self.bindTagPayloadFields(tag_pattern, value_loc, value_layout_val, tu_disc_offset);

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
                    .list => |list_pattern| {
                        const suffix_patterns = self.store.getPatternSpan(list_pattern.suffix);
                        const is_exact_match = list_pattern.rest.isNone() and suffix_patterns.len == 0;

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
                    .record => {
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
                    .tuple => {
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
        /// Generate code for incref operation
        /// Increments the reference count of a heap-allocated value
        fn generateIncref(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            // First generate the value expression
            const value_loc = try self.generateExpr(rc_op.value);

            // Check if we have a layout store to determine the type
            const ls = self.layout_store orelse return value_loc;

            // Get the layout to check if it's a heap-allocated type
            const layout_val = ls.getLayout(rc_op.layout_idx);

            // Only incref heap-allocated types: list, str (large), box
            switch (layout_val.tag) {
                .list, .list_of_zst => {
                    // Lists always have heap-allocated data
                    try self.emitListIncref(value_loc, rc_op.count);
                },
                .scalar => {
                    // Check if it's a string
                    if (layout_val.data.scalar.tag == .str) {
                        // Strings use SSO - only incref if large string
                        try self.emitStrIncref(value_loc, rc_op.count);
                    }
                    // Other scalars don't need incref
                },
                .box, .box_of_zst => {
                    // Boxes are always heap-allocated
                    try self.emitBoxIncref(value_loc, rc_op.count);
                },
                else => {
                    // Records, tuples, tag unions, closures, zst don't need RC at the top level
                    // (their heap-allocated fields are handled separately)
                },
            }

            return value_loc;
        }

        /// Generate code for decref operation
        /// Decrements the reference count and frees if it reaches zero
        fn generateDecref(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            // First generate the value expression
            const value_loc = try self.generateExpr(rc_op.value);

            // Check if we have a layout store to determine the type
            const ls = self.layout_store orelse return value_loc;

            // Get the layout to check if it's a heap-allocated type
            const layout_val = ls.getLayout(rc_op.layout_idx);

            // Only decref heap-allocated types: list, str (large), box
            switch (layout_val.tag) {
                .list, .list_of_zst => {
                    // Lists always have heap-allocated data
                    const list_info = ls.getListInfo(layout_val);
                    try self.emitListDecref(value_loc, list_info.elem_alignment);
                },
                .scalar => {
                    // Check if it's a string
                    if (layout_val.data.scalar.tag == .str) {
                        // Strings use SSO - only decref if large string
                        try self.emitStrDecref(value_loc);
                    }
                    // Other scalars don't need decref
                },
                .box, .box_of_zst => {
                    // Boxes are always heap-allocated
                    const box_info = ls.getBoxInfo(layout_val);
                    try self.emitBoxDecref(value_loc, box_info.elem_alignment);
                },
                else => {
                    // Records, tuples, tag unions, closures, zst don't need RC at the top level
                },
            }

            return value_loc;
        }

        /// Generate code for free operation
        /// Directly frees memory without checking refcount
        fn generateFree(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            // First generate the value expression
            const value_loc = try self.generateExpr(rc_op.value);

            // Check if we have a layout store to determine the type
            const ls = self.layout_store orelse return value_loc;

            // Get the layout to check if it's a heap-allocated type
            const layout_val = ls.getLayout(rc_op.layout_idx);

            // Only free heap-allocated types: list, str (large), box
            switch (layout_val.tag) {
                .list, .list_of_zst => {
                    const list_info = ls.getListInfo(layout_val);
                    try self.emitListFree(value_loc, list_info.elem_alignment);
                },
                .scalar => {
                    if (layout_val.data.scalar.tag == .str) {
                        try self.emitStrFree(value_loc);
                    }
                },
                .box, .box_of_zst => {
                    const box_info = ls.getBoxInfo(layout_val);
                    try self.emitBoxFree(value_loc, box_info.elem_alignment);
                },
                // Other layout types (scalars, tuples, records, etc.) are not heap-allocated
                // and don't need freeing
                else => {},
            }

            return value_loc;
        }

        /// Emit incref for a list value
        fn emitListIncref(self: *Self, value_loc: ValueLocation, count: u16) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&increfDataPtrC);

            // Get the data pointer from the list struct (offset 0)
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |s| {
                    const offset = s.offset;
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, offset);
                },
                .list_stack => |info| {
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, info.struct_offset);
                },
                else => return, // Can't incref non-stack values
            }

            // Call increfDataPtrC(ptr, count, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(@intCast(count));
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .incref_data_ptr);
        }

        /// Emit decref for a list value
        fn emitListDecref(self: *Self, value_loc: ValueLocation, elem_alignment: u32) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&decrefDataPtrC);

            // Get the data pointer from the list struct (offset 0)
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |s| {
                    const offset = s.offset;
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, offset);
                },
                .list_stack => |info| {
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, info.struct_offset);
                },
                else => return,
            }

            // Call decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(@intCast(elem_alignment));
            try builder.addImmArg(0); // elements_refcounted = false
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .decref_data_ptr);
        }

        /// Emit free for a list value
        fn emitListFree(self: *Self, value_loc: ValueLocation, elem_alignment: u32) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&freeDataPtrC);

            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |s| {
                    const offset = s.offset;
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, offset);
                },
                .list_stack => |info| {
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, info.struct_offset);
                },
                else => return,
            }

            // Call freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(@intCast(elem_alignment));
            try builder.addImmArg(0); // elements_refcounted = false
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .free_data_ptr);
        }

        /// Emit incref for a string value
        /// Strings use SSO, so we need to check if it's a large string first
        fn emitStrIncref(self: *Self, value_loc: ValueLocation, count: u16) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&increfDataPtrC);

            // String struct: bytes (offset 0), length (offset 8), capacity_or_alloc_ptr (offset 16)
            // Small string detection: capacity_or_alloc_ptr has high bit set (negative when signed)

            const base_offset: i32 = switch (value_loc) {
                .stack => |s| s.offset,
                .stack_str => |offset| offset,
                else => return,
            };

            // Load capacity_or_alloc_ptr to check for small string
            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);

            try self.emitLoad(.w64, cap_reg, frame_ptr, base_offset + 16);

            // Check if small string (high bit set = negative)
            // If negative, skip the incref
            const skip_patch = blk: {
                if (comptime target.toCpuArch() == .aarch64) {
                    // Compare with 0 and branch if negative (mi = minus/negative)
                    try self.codegen.emit.cmpRegImm12(.w64, cap_reg, 0);
                    const patch_loc = self.codegen.currentOffset();
                    try self.codegen.emit.bcond(.mi, 0);
                    break :blk patch_loc;
                } else {
                    // Test the sign bit and jump if sign flag set (negative)
                    try self.codegen.emit.testRegReg(.w64, cap_reg, cap_reg);
                    break :blk try self.codegen.emitCondJump(.sign);
                }
            };

            // Not a small string - load the bytes pointer and call incref
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            try self.emitLoad(.w64, ptr_reg, frame_ptr, base_offset);

            // Call increfDataPtrC(ptr, count, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(@intCast(count));
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .incref_data_ptr);

            // Patch the skip jump to here
            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        /// Emit decref for a string value
        fn emitStrDecref(self: *Self, value_loc: ValueLocation) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&decrefDataPtrC);

            const base_offset: i32 = switch (value_loc) {
                .stack => |s| s.offset,
                .stack_str => |offset| offset,
                else => return,
            };

            // Load capacity_or_alloc_ptr to check for small string
            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);

            try self.emitLoad(.w64, cap_reg, frame_ptr, base_offset + 16);

            // Check if small string (high bit set = negative)
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

            // Not a small string - load the bytes pointer and call decref
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            try self.emitLoad(.w64, ptr_reg, frame_ptr, base_offset);

            // Call decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            // Strings have 1-byte alignment for the data
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(1); // alignment
            try builder.addImmArg(0); // elements_refcounted = false
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .decref_data_ptr);

            // Patch the skip jump to here
            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        /// Emit free for a string value
        fn emitStrFree(self: *Self, value_loc: ValueLocation) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&freeDataPtrC);

            const base_offset: i32 = switch (value_loc) {
                .stack => |s| s.offset,
                .stack_str => |offset| offset,
                else => return,
            };

            // Load capacity_or_alloc_ptr to check for small string
            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);

            try self.emitLoad(.w64, cap_reg, frame_ptr, base_offset + 16);

            // Check if small string (high bit set = negative)
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

            // Not a small string - load the bytes pointer and call free
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            try self.emitLoad(.w64, ptr_reg, frame_ptr, base_offset);

            // Call freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(1); // alignment
            try builder.addImmArg(0); // elements_refcounted = false
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .free_data_ptr);

            // Patch the skip jump to here
            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        /// Emit incref for a box value
        fn emitBoxIncref(self: *Self, value_loc: ValueLocation, count: u16) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&increfDataPtrC);

            // Box is just a pointer
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |s| {
                    const offset = s.offset;
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, offset);
                },
                .general_reg => |r| {
                    try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                },
                else => return,
            }

            // Call increfDataPtrC(ptr, count, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(@intCast(count));
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .incref_data_ptr);
        }

        /// Emit decref for a box value
        fn emitBoxDecref(self: *Self, value_loc: ValueLocation, elem_alignment: u32) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&decrefDataPtrC);

            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |s| {
                    const offset = s.offset;
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, offset);
                },
                .general_reg => |r| {
                    try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                },
                else => return,
            }

            // Call decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(@intCast(elem_alignment));
            try builder.addImmArg(0); // elements_refcounted = false
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .decref_data_ptr);
        }

        /// Emit free for a box value
        fn emitBoxFree(self: *Self, value_loc: ValueLocation, elem_alignment: u32) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&freeDataPtrC);

            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |s| {
                    const offset = s.offset;
                    try self.emitLoad(.w64, ptr_reg, frame_ptr, offset);
                },
                .general_reg => |r| {
                    try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                },
                else => return,
            }

            // Call freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(ptr_reg);
            try builder.addImmArg(@intCast(elem_alignment));
            try builder.addImmArg(0); // elements_refcounted = false
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .free_data_ptr);
        }

        pub fn patchPendingCalls(self: *Self) Allocator.Error!void {
            for (self.pending_calls.items) |pending| {
                const key: u64 = @bitCast(pending.target_symbol);
                const proc = self.proc_registry.get(key) orelse {
                    unreachable;
                };
                self.patchCallTarget(pending.call_site, proc.code_start);
            }
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
            body_expr: LirExprId,
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

                // Unpack arguments from args_ptr (X21) to argument registers
                var args_offset: i32 = 0;
                for (arg_layouts, 0..) |arg_layout, i| {
                    const arg_size = self.getLayoutSize(arg_layout);
                    const dest_reg = self.getArgumentRegister(@intCast(i));

                    // Load from [X21 + args_offset]
                    try self.codegen.emit.ldrRegMemSoff(.w64, dest_reg, .X21, args_offset);
                    args_offset += @intCast(arg_size);
                }

                // Generate the body expression
                const result_loc = try self.generateExpr(body_expr);

                // If the body is a lambda or closure (function value), we need to CALL it, not return it.
                // This happens when the entrypoint is defined as `main_for_host! = main!` where
                // `main!` is a lambda/closure.
                // Track the actual return layout (may differ from ret_layout if body is a closure)
                var actual_ret_layout = ret_layout;

                const final_result = switch (result_loc) {
                    .lambda_code => |lc| blk: {
                        // The lambda's return layout is the actual return type
                        actual_ret_layout = lc.ret_layout;
                        // Call the lambda with BL instruction (aarch64)
                        // Calculate relative offset from current position
                        const current_offset = self.codegen.currentOffset();
                        const rel_offset: i28 = @intCast(@as(i32, @intCast(lc.code_offset)) - @as(i32, @intCast(current_offset)));
                        try self.codegen.emit.bl(rel_offset);

                        // Result is in X0
                        break :blk ValueLocation{ .general_reg = .X0 };
                    },
                    .closure_value => |cv| blk: {
                        // Dispatch the closure call with no arguments
                        // The closure's return layout is the actual return type, not the closure layout
                        const lambda_expr = self.store.getExpr(cv.lambda);
                        const lambda = switch (lambda_expr) {
                            .lambda => |l| l,
                            .closure => |c_id| inner: {
                                const c = self.store.getClosureData(c_id);
                                const inner = self.store.getExpr(c.lambda);
                                if (inner == .lambda) break :inner inner.lambda;
                                unreachable;
                            },
                            else => unreachable,
                        };
                        actual_ret_layout = lambda.ret_layout;
                        const empty_span = lir.LIR.LirExprSpan.empty();
                        break :blk try self.generateClosureDispatch(cv, empty_span, actual_ret_layout);
                    },
                    else => result_loc,
                };

                // Store result to ret_ptr (X20) - but only if return type is non-zero-sized
                // Per RocCall ABI: "If the Roc function returns a zero-sized type like `{}`,
                // it will not write anything into this address."
                const ret_size = self.getLayoutSize(actual_ret_layout);
                if (ret_size > 0) {
                    try self.storeResultToSavedPtr(final_result, actual_ret_layout, .X20, 1);
                }

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

                // Re-patch internal calls/addr whose targets are outside the shifted body
                self.repatchInternalCalls(body_start, body_end, prologue_size_val);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size_val);

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
                // x86_64: emit prologue using ForwardFrameBuilder
                var frame = self.initEntrypointFrameBuilder();
                self.codegen.stack_offset = try frame.emitPrologue();

                // Record prologue end for potential sub rsp patching.
                // The sub rsp, imm32 instruction is the last 7 bytes of the prologue;
                // the imm32 is at the last 4 bytes.
                const prologue_end = self.codegen.currentOffset();
                const initial_stack_alloc = frame.actual_stack_alloc;

                // Track prologue info for unwind tables
                prologue_size = @intCast(prologue_end - func_start);
                stack_alloc = frame.computeActualStackAlloc();

                // On entry, arguments are in different registers depending on ABI:
                // Windows x64 ABI: RCX=roc_ops, RDX=ret_ptr, R8=args_ptr
                // System V ABI (Linux/macOS): RDI=roc_ops, RSI=ret_ptr, RDX=args_ptr
                if (target.isWindows()) {
                    // Windows x64 ABI
                    // Save RocOps pointer (RCX) to R12
                    try self.codegen.emit.movRegReg(.w64, .R12, .RCX);
                    // Save ret_ptr (RDX) to RBX
                    try self.codegen.emit.movRegReg(.w64, .RBX, .RDX);
                    // Save args_ptr (R8) to R13
                    try self.codegen.emit.movRegReg(.w64, .R13, .R8);
                } else {
                    // System V ABI (Linux/macOS)
                    // Save RocOps pointer (RDI) to R12
                    try self.codegen.emit.movRegReg(.w64, .R12, .RDI);
                    // Save ret_ptr (RSI) to RBX
                    try self.codegen.emit.movRegReg(.w64, .RBX, .RSI);
                    // Save args_ptr (RDX) to R13
                    try self.codegen.emit.movRegReg(.w64, .R13, .RDX);
                }

                self.roc_ops_reg = .R12;

                // Unpack arguments from args_ptr (R13) to argument registers
                // System V: RDI, RSI, RDX, RCX, R8, R9 for first 6 args
                var args_offset: i32 = 0;
                for (arg_layouts, 0..) |arg_layout, i| {
                    const arg_size = self.getLayoutSize(arg_layout);
                    const dest_reg = self.getArgumentRegister(@intCast(i));

                    // Load from [R13 + args_offset]
                    try self.codegen.emit.movRegMem(.w64, dest_reg, .R13, args_offset);
                    args_offset += @intCast(arg_size);
                }

                // Generate the body expression
                const result_loc = try self.generateExpr(body_expr);

                // If the body is a lambda or closure (function value), we need to CALL it, not return it.
                // This happens when the entrypoint is defined as `main_for_host! = main!` where
                // `main!` is a lambda/closure. Evaluating the body gives us the function, but we need
                // to invoke it to get the actual result.
                // Track the actual return layout (may differ from ret_layout if body is a closure)
                var actual_ret_layout = ret_layout;

                const final_result = switch (result_loc) {
                    .lambda_code => |lc| blk: {
                        // The lambda's return layout is the actual return type
                        actual_ret_layout = lc.ret_layout;
                        // Call the lambda with roc_ops as first argument
                        // The lambda's code is at lc.code_offset and expects roc_ops in RCX (Windows)
                        // R12 holds roc_ops, so pass it to the lambda
                        if (target.isWindows()) {
                            try self.codegen.emit.movRegReg(.w64, .RCX, .R12);
                        } else {
                            try self.codegen.emit.movRegReg(.w64, .RDI, .R12);
                        }

                        const rel_offset = @as(i32, @intCast(lc.code_offset)) - @as(i32, @intCast(self.codegen.currentOffset() + 5));
                        try self.codegen.emit.callRel32(rel_offset);

                        // Result is in RAX (for small return values)
                        break :blk ValueLocation{ .general_reg = .RAX };
                    },
                    .closure_value => |cv| blk: {
                        // Dispatch the closure call with no arguments
                        // The closure's return layout is the actual return type, not the closure layout
                        const lambda_expr = self.store.getExpr(cv.lambda);
                        const lambda = switch (lambda_expr) {
                            .lambda => |l| l,
                            .closure => |c_id| inner: {
                                const c = self.store.getClosureData(c_id);
                                const inner = self.store.getExpr(c.lambda);
                                if (inner == .lambda) break :inner inner.lambda;
                                unreachable;
                            },
                            else => unreachable,
                        };
                        actual_ret_layout = lambda.ret_layout;
                        const empty_span = lir.LIR.LirExprSpan.empty();
                        break :blk try self.generateClosureDispatch(cv, empty_span, actual_ret_layout);
                    },
                    else => result_loc,
                };

                // Store result to ret_ptr (RBX) - but only if return type is non-zero-sized
                // Per RocCall ABI: "If the Roc function returns a zero-sized type like `{}`,
                // it will not write anything into this address."
                const ret_size = self.getLayoutSize(actual_ret_layout);
                if (ret_size > 0) {
                    try self.storeResultToSavedPtr(final_result, actual_ret_layout, .RBX, 1);
                }

                // Compute actual stack usage from body generation.
                // The body may have allocated more locals than ENTRYPOINT_STACK_SIZE.
                const push_bytes: u32 = @as(u32, frame.push_count) * 8;
                const actual_locals: u32 = @intCast(@as(i32, @intCast(push_bytes)) - self.codegen.stack_offset);
                var epilogue_frame = self.initEntrypointFrameBuilder();
                epilogue_frame.stack_size = actual_locals;
                const actual_alloc = epilogue_frame.computeActualStackAlloc();

                // Patch the prologue's sub rsp if the body needed more stack than initially allocated
                if (actual_alloc != initial_stack_alloc and initial_stack_alloc > 0) {
                    const patch_offset = prologue_end - 4;
                    std.mem.writeInt(u32, self.codegen.emit.buf.items[patch_offset..][0..4], actual_alloc, .little);
                    stack_alloc = actual_alloc;
                }

                // Epilogue using ForwardFrameBuilder (matches patched prologue)
                try epilogue_frame.emitEpilogue();
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

        /// Get the size of a layout in bytes for argument unpacking
        fn getLayoutSize(self: *Self, layout_idx: layout.Idx) u32 {
            if (self.layout_store) |ls| {
                const layout_val = ls.getLayout(layout_idx);
                return ls.layoutSizeAlign(layout_val).size;
            }
            // Default sizes for well-known layouts
            return switch (layout_idx) {
                .zst => 0, // Zero-sized type (empty records, empty tuples, etc.)
                .i8, .u8, .bool => 1,
                .i16, .u16 => 2,
                .i32, .u32, .f32 => 4,
                .i64, .u64, .f64 => 8,
                .i128, .u128, .dec => 16,
                .str => 24,
                else => 8, // Default to 8 bytes
            };
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
/// Falls back to UnsupportedArchCodeGen for architectures that don't support native code generation
/// (e.g., wasm32 when compiling the playground).
pub const HostLirCodeGen = blk: {
    const native_target = RocTarget.detectNative();
    const arch = native_target.toCpuArch();
    if (arch == .x86_64 or arch == .aarch64 or arch == .aarch64_be) {
        break :blk LirCodeGen(native_target);
    } else {
        break :blk UnsupportedArchCodeGen;
    }
};

/// Stub code generator for unsupported architectures.
/// This allows the code to compile for cross-compilation targets like 32-bit ARM/x86,
/// but will error at runtime if actually used.
pub const UnsupportedArchCodeGen = struct {
    const Self = @This();

    pub const CodeResult = struct {
        code: []const u8,
        entry_offset: usize,
    };

    pub const ExportedSymbol = struct {
        name: []const u8,
        offset: usize,
        size: usize,
    };

    allocator: Allocator,

    pub fn init(
        allocator: Allocator,
        _: *const LirExprStore,
        _: ?*const LayoutStore,
        _: ?*StaticDataInterner,
    ) Allocator.Error!Self {
        return .{ .allocator = allocator };
    }

    pub fn deinit(_: *Self) void {}

    pub fn compileAllProcs(_: *Self, _: anytype) Allocator.Error!void {
        unreachable;
    }

    pub fn generateCode(_: *Self, _: anytype, _: anytype, _: anytype) Allocator.Error!CodeResult {
        unreachable;
    }

    pub fn generateExpr(_: *Self, _: anytype) Allocator.Error!void {
        unreachable;
    }

    pub fn generateProc(_: *Self, _: anytype) Allocator.Error!void {
        unreachable;
    }

    pub fn generateEntrypointWrapper(_: *Self, _: []const u8, _: anytype, _: anytype, _: anytype) Allocator.Error!ExportedSymbol {
        unreachable;
    }

    pub fn finalize(_: *Self) Allocator.Error![]const u8 {
        unreachable;
    }

    pub fn getCode(_: *const Self) []const u8 {
        return &[_]u8{};
    }

    pub fn getGeneratedCode(_: *const Self) []const u8 {
        return &[_]u8{};
    }

    pub fn getRelocations(_: *const Self) []const Relocation {
        return &[_]Relocation{};
    }
};

// Tests

test "code generator initialization" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
    defer codegen.deinit();
}

test "generate i64 literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Add an i64 literal
    const expr_id = try store.addExpr(.{ .i64_literal = 42 }, base.Region.zero());

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
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

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .bool, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "generate addition" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Create: 1 + 2
    const lhs_id = try store.addExpr(.{ .i64_literal = 1 }, base.Region.zero());
    const rhs_id = try store.addExpr(.{ .i64_literal = 2 }, base.Region.zero());
    const add_id = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = lhs_id,
        .rhs = rhs_id,
        .result_layout = .i64,
        .operand_layout = .i64,
    } }, base.Region.zero());

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
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
    const Ident = base.Ident;
    const Layout = layout.Layout;
    const allocator = std.testing.allocator;

    // Set up ModuleEnv for ident storage
    var module_env = try ModuleEnv.init(allocator, "");
    defer module_env.deinit();

    // Insert field name identifiers
    const field_a = try module_env.insertIdent(Ident.for_text("a"));
    const field_b = try module_env.insertIdent(Ident.for_text("b"));

    // Create layout store with a record layout { a: Str, b: Str }
    var module_env_ptrs = [1]*const ModuleEnv{&module_env};
    var layout_store = try layout.Store.init(
        &module_env_ptrs,
        null,
        allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    const record_layout_idx = try layout_store.putRecord(
        &module_env,
        &[_]Layout{ Layout.str(), Layout.str() },
        &[_]Ident.Idx{ field_a, field_b },
    );

    // Create LIR expressions: two record literals and an eq binop
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    // Field values (string literals represented as i64 placeholders — the codegen
    // only inspects the layout, not the actual field values for comparison dispatch)
    const str1 = try store.addExpr(.{ .i64_literal = 0 }, base.Region.zero());
    const str2 = try store.addExpr(.{ .i64_literal = 0 }, base.Region.zero());
    const str3 = try store.addExpr(.{ .i64_literal = 0 }, base.Region.zero());
    const str4 = try store.addExpr(.{ .i64_literal = 0 }, base.Region.zero());

    const fields1 = try store.addExprSpan(&[_]LirExprId{ str1, str2 });
    const field_names1 = try store.addFieldNameSpan(&[_]Ident.Idx{ field_a, field_b });

    const fields2 = try store.addExprSpan(&[_]LirExprId{ str3, str4 });
    const field_names2 = try store.addFieldNameSpan(&[_]Ident.Idx{ field_a, field_b });

    const lhs_record = try store.addExpr(.{ .record = .{
        .record_layout = record_layout_idx,
        .fields = fields1,
        .field_names = field_names1,
    } }, base.Region.zero());

    const rhs_record = try store.addExpr(.{ .record = .{
        .record_layout = record_layout_idx,
        .fields = fields2,
        .field_names = field_names2,
    } }, base.Region.zero());

    // LHS record == RHS record
    const eq_expr = try store.addExpr(.{ .binop = .{
        .op = .eq,
        .lhs = lhs_record,
        .rhs = rhs_record,
        .result_layout = .bool,
        .operand_layout = .bool,
    } }, base.Region.zero());

    // With layout_store: should use generateRecordComparisonByLayout (no crash)
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
    const lhs = try store.addExpr(.{ .i64_literal = 10 }, base.Region.zero());
    const rhs = try store.addExpr(.{ .i64_literal = 3 }, base.Region.zero());
    const expr_id = try store.addExpr(.{ .binop = .{
        .op = .mod,
        .lhs = lhs,
        .rhs = rhs,
        .result_layout = .i64,
        .operand_layout = .i64,
    } }, base.Region.zero());

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
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
    const lhs = try store.addExpr(.{ .i64_literal = 1 }, base.Region.zero());
    const rhs = try store.addExpr(.{ .i64_literal = 4 }, base.Region.zero());
    const expr_id = try store.addExpr(.{ .binop = .{
        .op = .shl,
        .lhs = lhs,
        .rhs = rhs,
        .result_layout = .i64,
        .operand_layout = .i64,
    } }, base.Region.zero());

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
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
    const lhs = try store.addExpr(.{ .i64_literal = 64 }, base.Region.zero());
    const rhs = try store.addExpr(.{ .i64_literal = 2 }, base.Region.zero());
    const expr_id = try store.addExpr(.{ .binop = .{
        .op = .shr,
        .lhs = lhs,
        .rhs = rhs,
        .result_layout = .i64,
        .operand_layout = .i64,
    } }, base.Region.zero());

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
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
    const lhs = try store.addExpr(.{ .i64_literal = 64 }, base.Region.zero());
    const rhs = try store.addExpr(.{ .i64_literal = 2 }, base.Region.zero());
    const expr_id = try store.addExpr(.{ .binop = .{
        .op = .shr_zf,
        .lhs = lhs,
        .rhs = rhs,
        .result_layout = .i64,
        .operand_layout = .i64,
    } }, base.Region.zero());

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
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
    const inner = try store.addExpr(.{ .i64_literal = 42 }, base.Region.zero());
    const neg = try store.addExpr(.{ .unary_minus = .{
        .expr = inner,
        .result_layout = .i64,
    } }, base.Region.zero());

    var codegen = try HostLirCodeGen.init(allocator, &store, null, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(neg, .i64, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}
