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
const erased_callable = @import("erased_callable.zig");
const dec = @import("dec.zig");
const hash = @import("hash.zig");
const crypto = @import("crypto.zig");
const i128h = @import("compiler_rt_128.zig");
const float_tan = @import("float_math/tan.zig");
const numeric_conversions = @import("numeric_conversions.zig");

const RocStr = str.RocStr;
const RocList = list.RocList;
const FromUtf8Try = str.FromUtf8Try;
// Use a local opaque pointer type for RocOps to avoid importing host_abi.zig
// which has a tracy dependency. The actual struct layout is handled by utils.zig.
const RocOps = utils.RocOps;

/// Field offsets for the dev backend's `Str.find_first` result copy.
pub const StrFindFirstLayout = extern struct {
    after_offset: u32,
    before_offset: u32,
    found_offset: u32,
};

/// Field offsets for the dev backend's `Str.drop_prefix_caseless_ascii` result copy.
pub const StrDropPrefixCaselessAsciiLayout = extern struct {
    after_offset: u32,
    found_offset: u32,
};

// Re-export commonly used functions
pub const rcNone = utils.rcNone;
pub const copy_fallback = list.copy_fallback;
pub const allocateWithRefcountC = utils.allocateWithRefcountC;
pub const increfDataPtrC = utils.increfDataPtrC;
pub const increfDataPtrSingleThreadC = utils.increfDataPtrSingleThreadC;
pub const decrefDataPtrC = utils.decrefDataPtrC;
pub const decrefDataPtrSingleThreadC = utils.decrefDataPtrSingleThreadC;
pub const freeDataPtrC = utils.freeDataPtrC;
pub const erasedCallableIncref = erased_callable.incref;
pub const erasedCallableDecref = erased_callable.decref;
pub const erasedCallableFree = erased_callable.free;

/// C ABI wrapper for hashing integer-width scalar values.
pub fn roc_builtins_hasher_write_u64(seed: u64, domain: u8, value: u64, width: u8) callconv(.c) u64 {
    return hash.hasher_write_u64(seed, domain, value, width);
}

/// C ABI wrapper for hashing 128-bit scalar values.
pub fn roc_builtins_hasher_write_u128(seed: u64, domain: u8, low: u64, high: u64) callconv(.c) u64 {
    return hash.hasher_write_u128(seed, domain, low, high);
}

/// C ABI wrapper for hashing raw F32 bits.
pub fn roc_builtins_hasher_write_f32_bits(seed: u64, bits: u64) callconv(.c) u64 {
    return hash.hasher_write_f32_bits(seed, @truncate(bits));
}

/// C ABI wrapper for hashing raw F64 bits.
pub fn roc_builtins_hasher_write_f64_bits(seed: u64, bits: u64) callconv(.c) u64 {
    return hash.hasher_write_f64_bits(seed, bits);
}

/// C ABI wrapper for hashing byte-list contents.
pub fn roc_builtins_hasher_write_bytes(seed: u64, domain: u8, bytes: ?[*]const u8, length: usize) callconv(.c) u64 {
    return hash.hasher_write_bytes(seed, domain, bytes, length);
}

/// C ABI wrapper for hashing RocStr contents.
pub fn roc_builtins_hasher_write_str(seed: u64, str_bytes: ?[*]u8, str_len: usize, str_cap: usize) callconv(.c) u64 {
    const value = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    const bytes = value.asSlice();
    return hash.hasher_write_bytes(seed, @intFromEnum(hash.HasherDomain.str), bytes.ptr, bytes.len);
}

/// C ABI wrapper for finalizing a builtin Hasher state.
pub fn roc_builtins_hasher_finish(seed: u64) callconv(.c) u64 {
    return hash.hasher_finish(seed);
}

/// C ABI wrapper for the compiler-owned Dict hash seed.
pub fn roc_builtins_dict_pseudo_seed() callconv(.c) u64 {
    return utils.dictPseudoSeed();
}

/// C ABI wrapper for one-shot SHA-256 hashing.
pub fn roc_builtins_crypto_sha256_hash_bytes(out: *RocList, bytes: ?[*]const u8, len: usize, _: usize, roc_ops: *RocOps) callconv(.c) void {
    out.* = crypto.sha256HashBytes(bytes, len, roc_ops);
}

/// C ABI wrapper for creating an empty serialized SHA-256 state.
pub fn roc_builtins_crypto_sha256_hasher_empty(out: *RocList, roc_ops: *RocOps) callconv(.c) void {
    out.* = crypto.sha256HasherEmpty(roc_ops);
}

/// C ABI wrapper for updating serialized SHA-256 state.
pub fn roc_builtins_crypto_sha256_hasher_write(out: *RocList, state_bytes: ?[*]const u8, state_len: usize, _: usize, input_bytes: ?[*]const u8, input_len: usize, _: usize, roc_ops: *RocOps) callconv(.c) void {
    out.* = crypto.sha256HasherWrite(state_bytes, state_len, input_bytes, input_len, roc_ops);
}

/// C ABI wrapper for finishing serialized SHA-256 state.
pub fn roc_builtins_crypto_sha256_hasher_finish(out: *RocList, state_bytes: ?[*]const u8, state_len: usize, _: usize, roc_ops: *RocOps) callconv(.c) void {
    out.* = crypto.sha256HasherFinish(state_bytes, state_len, roc_ops);
}

/// C ABI wrapper for one-shot BLAKE3 hashing.
pub fn roc_builtins_crypto_blake3_hash_bytes(out: *RocList, bytes: ?[*]const u8, len: usize, _: usize, roc_ops: *RocOps) callconv(.c) void {
    out.* = crypto.blake3HashBytes(bytes, len, roc_ops);
}

/// C ABI wrapper for creating an empty serialized BLAKE3 state.
pub fn roc_builtins_crypto_blake3_hasher_empty(out: *RocList, roc_ops: *RocOps) callconv(.c) void {
    out.* = crypto.blake3HasherEmpty(roc_ops);
}

/// C ABI wrapper for updating serialized BLAKE3 state.
pub fn roc_builtins_crypto_blake3_hasher_write(out: *RocList, state_bytes: ?[*]const u8, state_len: usize, _: usize, input_bytes: ?[*]const u8, input_len: usize, _: usize, roc_ops: *RocOps) callconv(.c) void {
    out.* = crypto.blake3HasherWrite(state_bytes, state_len, input_bytes, input_len, roc_ops);
}

/// C ABI wrapper for finishing serialized BLAKE3 state.
pub fn roc_builtins_crypto_blake3_hasher_finish(out: *RocList, state_bytes: ?[*]const u8, state_len: usize, _: usize, roc_ops: *RocOps) callconv(.c) void {
    out.* = crypto.blake3HasherFinish(state_bytes, state_len, roc_ops);
}

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
const listDropAt = list.listDropAt;
const listReplace = list.listReplace;
const listSwap = list.listSwap;
const listReserve = list.listReserve;
const listReleaseExcessCapacity = list.listReleaseExcessCapacity;
const listWithCapacity = list.listWithCapacity;
const listAppendUnsafe = list.listAppendUnsafe;
const listDecref = list.listDecref;
const RcDropFn = *const fn (?[*]u8, *RocOps) callconv(.c) void;
const RcIncFn = *const fn (?[*]u8, isize, *RocOps) callconv(.c) void;

// ═══════════════════════════════════════════════════════════════════════════
// String Wrappers
// ═══════════════════════════════════════════════════════════════════════════

/// Wrapper: strToUtf8C(RocStr, *RocOps) -> RocList
pub fn roc_builtins_str_to_utf8(out: *RocList, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const arg = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strToUtf8C(arg, roc_ops);
}

/// Wrapper: strConcatC(RocStr, RocStr, UpdateMode, *RocOps) -> RocStr. The
/// update mode is forwarded to the builtin's uniqueness check; `.InPlace`
/// skips it.
pub fn roc_builtins_str_concat(out: *RocStr, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    out.* = strConcatC(a, b, update_mode, roc_ops);
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

/// Wrapper: strEqualStaticSmall(RocStr, u64, u64, u64, u64) -> bool
pub fn roc_builtins_str_equal_static_small(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, static_len: u64, word0: u64, word1: u64, word2: u64) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    return str.strEqualStaticSmall(a, static_len, word0, word1, word2);
}

/// Wrapper: strStaticSmallWordEq(RocStr, u64, u64, u64) -> bool
pub fn roc_builtins_str_static_small_word_eq(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, offset: u64, active_len: u64, word: u64) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    return str.strStaticSmallWordEq(a, offset, active_len, word);
}

/// Wrapper: strStaticSmallWordCaselessEq(RocStr, u64, u64, u64) -> bool
pub fn roc_builtins_str_static_small_word_caseless_eq(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, offset: u64, active_len: u64, word: u64) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    return str.strStaticSmallWordCaselessEq(a, offset, active_len, word);
}

/// Wrapper: countUtf8Bytes(RocStr) -> u64
pub fn roc_builtins_str_count_utf8_bytes(str_bytes: ?[*]u8, str_len: usize, str_cap: usize) callconv(.c) u64 {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    return countUtf8Bytes(s);
}

/// Wrapper: findFirst(RocStr, RocStr, *RocOps) -> { before, found, after }
pub fn roc_builtins_str_find_first(out: *anyopaque, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, layout: *const StrFindFirstLayout, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    const result = str.findFirst(a, b, roc_ops);
    const out_bytes: [*]u8 = @ptrCast(out);

    @as(*RocStr, @ptrCast(@alignCast(out_bytes + layout.after_offset))).* = result.after;
    @as(*RocStr, @ptrCast(@alignCast(out_bytes + layout.before_offset))).* = result.before;
    @as(*u8, @ptrCast(@alignCast(out_bytes + layout.found_offset))).* = if (result.found) 1 else 0;
}

/// Wrapper: strDropPrefixCaselessAscii(RocStr, RocStr, *RocOps) -> { after, found }
pub fn roc_builtins_str_drop_prefix_caseless_ascii(out: *anyopaque, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, layout: *const StrDropPrefixCaselessAsciiLayout, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    const result = str.strDropPrefixCaselessAscii(a, b, roc_ops);
    const out_bytes: [*]u8 = @ptrCast(out);

    @as(*RocStr, @ptrCast(@alignCast(out_bytes + layout.after_offset))).* = result.after;
    @as(*u8, @ptrCast(@alignCast(out_bytes + layout.found_offset))).* = if (result.found) 1 else 0;
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

/// Wrapper: strTrim(RocStr, UpdateMode, *RocOps) -> RocStr. The update mode
/// is forwarded to the builtin's uniqueness check; `.InPlace` skips it.
pub fn roc_builtins_str_trim(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrim(s, update_mode, roc_ops);
}

/// Wrapper: strTrimStart(RocStr, UpdateMode, *RocOps) -> RocStr. The update
/// mode is forwarded to the builtin's uniqueness check; `.InPlace` skips it.
pub fn roc_builtins_str_trim_start(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrimStart(s, update_mode, roc_ops);
}

/// Wrapper: strTrimEnd(RocStr, UpdateMode, *RocOps) -> RocStr. The update
/// mode is forwarded to the builtin's uniqueness check; `.InPlace` skips it.
pub fn roc_builtins_str_trim_end(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrimEnd(s, update_mode, roc_ops);
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

/// Wrapper: reserveC(RocStr, u64, UpdateMode, *RocOps) -> RocStr. The update
/// mode is forwarded to the builtin's uniqueness check; `.InPlace` skips it.
pub fn roc_builtins_str_reserve(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, spare: u64, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = reserveC(s, spare, update_mode, roc_ops);
}

/// Wrapper: strReleaseExcessCapacity(RocStr, UpdateMode, *RocOps) -> RocStr.
/// The update mode is forwarded to the builtin's uniqueness check; `.InPlace`
/// skips it.
pub fn roc_builtins_str_release_excess_capacity(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strReleaseExcessCapacity(s, update_mode, roc_ops);
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

/// Wrapper: strWithAsciiLowercased(RocStr, UpdateMode, *RocOps) -> RocStr.
/// The update mode is forwarded to the builtin's uniqueness check; `.InPlace`
/// skips it.
pub fn roc_builtins_str_with_ascii_lowercased(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strWithAsciiLowercased(s, update_mode, roc_ops);
}

/// Wrapper: strWithAsciiUppercased(RocStr, UpdateMode, *RocOps) -> RocStr.
/// The update mode is forwarded to the builtin's uniqueness check; `.InPlace`
/// skips it.
pub fn roc_builtins_str_with_ascii_uppercased(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strWithAsciiUppercased(s, update_mode, roc_ops);
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

/// Public value `StrFromUtf8Layout`.
pub const StrFromUtf8Layout = extern struct {
    ok_tag: u64,
    err_tag: u64,
    outer_disc_offset: u32,
    outer_disc_size: u32,
    err_index_offset: u32,
    err_problem_offset: u32,
    inner_disc_offset: u32,
    inner_disc_size: u32,
    inner_bad_utf8_tag: u32,
};

/// Converts a UTF-8 byte list to a RocStr, writing the full result union (string or error details) to an output buffer.
pub fn roc_builtins_str_from_utf8_result(
    out: [*]u8,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    layout: *const StrFromUtf8Layout,
    roc_ops: *RocOps,
) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    const result = str.fromUtf8C(l, .Immutable, roc_ops);

    if (result.is_ok) {
        utils.writeAs(RocStr, out, result.string, @src());
        writeDiscriminant(out, layout.outer_disc_offset, layout.outer_disc_size, layout.ok_tag);
        return;
    }

    utils.writeAs(u64, out + layout.err_index_offset, result.byte_index, @src());
    utils.writeAs(u8, out + layout.err_problem_offset, @intFromEnum(result.problem_code), @src());
    writeDiscriminant(out, layout.inner_disc_offset, layout.inner_disc_size, layout.inner_bad_utf8_tag);
    writeDiscriminant(out, layout.outer_disc_offset, layout.outer_disc_size, layout.err_tag);
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
        out.* = .{ .bytes = heap_ptr, .capacity_or_alloc_ptr = RocStr.encodeCapacity(result_len), .length = result_len };
    }
}

/// Wrapper: project a runtime RocStr to the host dbg ABI using the actual RocStr storage.
pub fn roc_builtins_dbg_str(str_ptr: *const RocStr, roc_ops: *RocOps) callconv(.c) void {
    roc_ops.dbg(str_ptr.asSlice());
}

/// Source region of the `?` whose Err most recently failed a top-level
/// expect via `roc_builtins_expect_err_str`. Compiled test roots run
/// in-process under the harness's crash boundary, so the harness reads this
/// back after the unwind via `takeExpectErrRegion` to point its failure
/// report at the `?` expression.
threadlocal var last_expect_err_region: ?ExpectErrRegion = null;

/// Byte offsets into the failing module's source for the `?` expression.
pub const ExpectErrRegion = struct {
    start: u32,
    end: u32,
};

/// Returns and clears the region recorded by the most recent
/// `roc_builtins_expect_err_str` call on this thread.
pub fn takeExpectErrRegion() ?ExpectErrRegion {
    const region = last_expect_err_region;
    last_expect_err_region = null;
    return region;
}

/// Fail a top-level expect whose `?` operator evaluated an Err, reporting the
/// runtime-built message (which includes the rendered Err value) and the
/// source region of the `?` expression. Terminates evaluation via the host's
/// crash callback; the message carries the expect-specific wording.
pub fn roc_builtins_expect_err_str(str_ptr: *const RocStr, region_start: u32, region_end: u32, roc_ops: *RocOps) callconv(.c) void {
    last_expect_err_region = .{ .start = region_start, .end = region_end };
    roc_ops.crash(str_ptr.asSlice());
}

/// Report a failed `expect` using static message bytes owned by generated code.
pub fn roc_builtins_roc_expect_failed(msg_bytes: [*]const u8, msg_len: usize, roc_ops: *RocOps) callconv(.c) void {
    roc_ops.expectFailed(msg_bytes[0..msg_len]);
}

/// Report a Roc crash using static message bytes owned by generated code.
pub fn roc_builtins_roc_crashed(msg_bytes: [*]const u8, msg_len: usize, roc_ops: *RocOps) callconv(.c) void {
    roc_ops.crash(msg_bytes[0..msg_len]);
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

const CallbackElementIncrefContext = struct {
    callback: RcIncFn,
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

fn callbackListElementIncref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    if (element == null) return;
    const ctx_ptr = context orelse unreachable;
    const ctx: *const CallbackElementIncrefContext = utils.alignedPtrCast(
        *const CallbackElementIncrefContext,
        @as([*]u8, @ptrCast(ctx_ptr)),
        @src(),
    );
    ctx.callback(element, 1, ctx.roc_ops);
}

/// Wrapper: listWithCapacity
pub fn roc_builtins_list_with_capacity(out: *RocList, capacity: u64, alignment: u32, element_width: usize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    out.* = listWithCapacity(capacity, alignment, element_width, elements_refcounted, null, @ptrCast(&rcNone), roc_ops);
}

/// Wrapper: listAppendUnsafe
pub fn roc_builtins_list_append_unsafe(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, element: ?[*]const u8, element_width: usize, _: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = listAppendUnsafe(l, @constCast(element), element_width, @ptrCast(&copy_fallback));
}

/// Wrapper: listMapCanReuse
pub fn roc_builtins_list_map_can_reuse(list_bytes: ?[*]u8, list_len: usize, list_cap: usize, roc_ops: *RocOps) callconv(.c) u8 {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    return @intFromBool(list.listMapCanReuse(l, roc_ops));
}

/// Wrapper: listConcat(RocList, RocList, alignment, element_width, ..., *RocOps) -> RocList.
/// `update_modes` carries one bit per list argument (bit 0 = a, bit 1 = b); a
/// set bit selects `.InPlace` for that argument's uniqueness check, skipping
/// it. The two modes travel as one 8-byte parameter because the dev call
/// builder writes every stack argument as an 8-byte slot, which only matches
/// the C ABI when no two sub-8-byte parameters are adjacent on the stack.
pub fn roc_builtins_list_concat(out: *RocList, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, alignment: u32, element_width: usize, elements_refcounted: bool, element_incref: ?RcIncFn, element_decref: ?RcDropFn, update_modes: u64, roc_ops: *RocOps) callconv(.c) void {
    const a = RocList{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocList{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    const update_mode_a: utils.UpdateMode = if (update_modes & 1 != 0) .InPlace else .Immutable;
    const update_mode_b: utils.UpdateMode = if (update_modes & 2 != 0) .InPlace else .Immutable;
    if (elements_refcounted) {
        var inc_ctx = CallbackElementIncrefContext{
            .callback = element_incref orelse unreachable,
            .roc_ops = roc_ops,
        };
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = listConcat(
            a,
            b,
            alignment,
            element_width,
            true,
            @ptrCast(&inc_ctx),
            &callbackListElementIncref,
            @ptrCast(&dec_ctx),
            &callbackListElementDecref,
            update_mode_a,
            update_mode_b,
            roc_ops,
        );
    } else {
        out.* = listConcat(a, b, alignment, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), update_mode_a, update_mode_b, roc_ops);
    }
}

/// Wrapper: listPrepend(RocList, alignment, element, element_width, ..., *RocOps) -> RocList.
/// The update mode is forwarded to the builtin's uniqueness check; `.InPlace`
/// skips it.
pub fn roc_builtins_list_prepend(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element: ?[*]u8, element_width: usize, elements_refcounted: bool, element_incref: ?RcIncFn, element_decref: ?RcDropFn, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (elements_refcounted) {
        var inc_ctx = CallbackElementIncrefContext{
            .callback = element_incref orelse unreachable,
            .roc_ops = roc_ops,
        };
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = listPrepend(l, alignment, element, element_width, true, @ptrCast(&inc_ctx), &callbackListElementIncref, @ptrCast(&dec_ctx), &callbackListElementDecref, update_mode, &copy_fallback, roc_ops);
    } else {
        out.* = listPrepend(l, alignment, element, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), update_mode, &copy_fallback, roc_ops);
    }
}

/// Wrapper: listSublist for sublist/drop_first/drop_last/take_first/take_last.
/// The update mode is forwarded to the builtin's uniqueness checks; `.InPlace`
/// skips them.
pub fn roc_builtins_list_sublist(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, start: u64, len: u64, elements_refcounted: bool, element_decref: ?RcDropFn, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (elements_refcounted) {
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = listSublist(l, alignment, element_width, true, start, len, @ptrCast(&dec_ctx), &callbackListElementDecref, update_mode, roc_ops);
    } else {
        out.* = listSublist(l, alignment, element_width, false, start, len, null, @ptrCast(&rcNone), update_mode, roc_ops);
    }
}

/// Wrapper: listDropAt(list, index) -> List. The update mode is forwarded to
/// the builtin's uniqueness check; `.InPlace` skips it.
pub fn roc_builtins_list_drop_at(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, index: u64, elements_refcounted: bool, element_incref: ?RcIncFn, element_decref: ?RcDropFn, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (elements_refcounted) {
        var inc_ctx = CallbackElementIncrefContext{
            .callback = element_incref orelse unreachable,
            .roc_ops = roc_ops,
        };
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = listDropAt(l, alignment, element_width, true, index, @ptrCast(&inc_ctx), &callbackListElementIncref, @ptrCast(&dec_ctx), &callbackListElementDecref, update_mode, roc_ops);
    } else {
        out.* = listDropAt(l, alignment, element_width, false, index, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), update_mode, roc_ops);
    }
}

/// Wrapper: listReplace for list_set. An `.InPlace` update mode means the
/// compiler proved the list unique, so the uniqueness-checked copy-on-write
/// entry is bypassed in favor of listReplaceInPlace.
pub fn roc_builtins_list_replace(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, index: u64, element: ?[*]u8, element_width: usize, out_element: ?[*]u8, elements_refcounted: bool, element_incref: ?RcIncFn, element_decref: ?RcDropFn, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (update_mode == .InPlace) {
        out.* = list.listReplaceInPlace(l, index, element, element_width, out_element, &copy_fallback);
        return;
    }
    if (elements_refcounted) {
        var inc_ctx = CallbackElementIncrefContext{
            .callback = element_incref orelse unreachable,
            .roc_ops = roc_ops,
        };
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = listReplace(l, alignment, index, element, element_width, true, @ptrCast(&inc_ctx), &callbackListElementIncref, @ptrCast(&dec_ctx), &callbackListElementDecref, out_element, &copy_fallback, roc_ops);
    } else {
        out.* = listReplace(l, alignment, index, element, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), out_element, &copy_fallback, roc_ops);
    }
}

/// Wrapper: listSwap for list_swap. The update mode is forwarded to the
/// builtin's uniqueness check; `.InPlace` skips it.
pub fn roc_builtins_list_swap(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, index_1: u64, index_2: u64, elements_refcounted: bool, element_incref: ?RcIncFn, element_decref: ?RcDropFn, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (elements_refcounted) {
        var inc_ctx = CallbackElementIncrefContext{
            .callback = element_incref orelse unreachable,
            .roc_ops = roc_ops,
        };
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = listSwap(l, alignment, element_width, index_1, index_2, true, @ptrCast(&inc_ctx), &callbackListElementIncref, @ptrCast(&dec_ctx), &callbackListElementDecref, update_mode, &copy_fallback, roc_ops);
    } else {
        out.* = listSwap(l, alignment, element_width, index_1, index_2, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), update_mode, &copy_fallback, roc_ops);
    }
}

/// Wrapper: listReserve. The update mode is forwarded to the builtin's
/// uniqueness check; `.InPlace` skips it.
pub fn roc_builtins_list_reserve(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, spare: u64, element_width: usize, elements_refcounted: bool, element_incref: ?RcIncFn, element_decref: ?RcDropFn, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (elements_refcounted) {
        var inc_ctx = CallbackElementIncrefContext{
            .callback = element_incref orelse unreachable,
            .roc_ops = roc_ops,
        };
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = listReserve(l, alignment, spare, element_width, true, @ptrCast(&inc_ctx), &callbackListElementIncref, @ptrCast(&dec_ctx), &callbackListElementDecref, update_mode, roc_ops);
    } else {
        out.* = listReserve(l, alignment, spare, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), update_mode, roc_ops);
    }
}

/// Wrapper: listReleaseExcessCapacity. The update mode is forwarded to the
/// builtin's uniqueness check; `.InPlace` skips it.
pub fn roc_builtins_list_release_excess_capacity(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, elements_refcounted: bool, element_incref: ?RcIncFn, element_decref: ?RcDropFn, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (elements_refcounted) {
        var inc_ctx = CallbackElementIncrefContext{
            .callback = element_incref orelse unreachable,
            .roc_ops = roc_ops,
        };
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = listReleaseExcessCapacity(l, alignment, element_width, true, @ptrCast(&inc_ctx), &callbackListElementIncref, @ptrCast(&dec_ctx), &callbackListElementDecref, update_mode, roc_ops);
    } else {
        out.* = listReleaseExcessCapacity(l, alignment, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), update_mode, roc_ops);
    }
}

test "roc_builtins_list_replace InPlace mutates the unique allocation without a uniqueness check" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();

    const data = [_]u8{ 10, 20, 30, 40 };
    const l = RocList.fromSlice(u8, data[0..], false, env.getOps());
    const original_bytes = l.bytes;

    const new_element: u8 = 99;
    var out_element: u8 = 0;
    var out: RocList = undefined;
    roc_builtins_list_replace(&out, l.bytes, l.length, l.capacity_or_alloc_ptr, @alignOf(u8), 2, @ptrCast(@constCast(&new_element)), @sizeOf(u8), @ptrCast(&out_element), false, null, null, .InPlace, env.getOps());
    defer out.decref(@alignOf(u8), @sizeOf(u8), false, null, list.rcNone, env.getOps());

    try std.testing.expectEqual(original_bytes, out.bytes);
    try std.testing.expectEqual(@as(u8, 30), out_element);
    const elements = out.elements(u8).?[0..out.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);
    try std.testing.expectEqual(@as(u8, 99), elements[2]);
    try std.testing.expectEqual(@as(u8, 40), elements[3]);
}

test "roc_builtins_list_swap InPlace mutates the unique allocation without a uniqueness check" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();

    const data = [_]u16{ 1, 2, 3 };
    const l = RocList.fromSlice(u16, data[0..], false, env.getOps());
    const original_bytes = l.bytes;

    var out: RocList = undefined;
    roc_builtins_list_swap(&out, l.bytes, l.length, l.capacity_or_alloc_ptr, @alignOf(u16), @sizeOf(u16), 0, 2, false, null, null, .InPlace, env.getOps());
    defer out.decref(@alignOf(u16), @sizeOf(u16), false, null, list.rcNone, env.getOps());

    try std.testing.expectEqual(original_bytes, out.bytes);
    const elements = out.elements(u16).?[0..out.len()];
    try std.testing.expectEqual(@as(u16, 3), elements[0]);
    try std.testing.expectEqual(@as(u16, 2), elements[1]);
    try std.testing.expectEqual(@as(u16, 1), elements[2]);
}

/// Wrapper: incref a list with refcounted elements.
pub fn roc_builtins_list_incref(
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    amount: isize,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    list.listIncref(l, amount, elements_refcounted, roc_ops);
}

/// Wrapper: incref a list whose allocation is proven thread-confined, so the
/// count update may use plain loads and stores.
pub fn roc_builtins_list_incref_single_thread(
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    amount: isize,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    l.increfWithAtomicity(amount, elements_refcounted, .single_thread, roc_ops);
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

/// Decref a list whose allocation is proven thread-confined: the list's own
/// count update uses plain loads and stores. Element cleanup runs through the
/// `element_decref` C function pointer, whose ABI carries no atomicity
/// parameter; the caller passes a callback whose body already matches the
/// single-thread statement. Visibility is containment-closed (design.md
/// "Thread-Confined Reference Counts"), so everything reachable from a
/// confined list is confined too.
pub fn roc_builtins_list_decref_with_single_thread(
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
        if (l.isUnique(roc_ops)) {
            if (l.getAllocationDataPtr(roc_ops)) |source| {
                const count = l.getAllocationElementCount(true, roc_ops);
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    callback(source + i * element_width, roc_ops);
                }
            }
        }
        utils.decref(l.getAllocationDataPtr(roc_ops), l.capacity_or_alloc_ptr, alignment, true, .single_thread, roc_ops);
    } else {
        decrefDataPtrSingleThreadC(l.getAllocationDataPtr(roc_ops), alignment, false, roc_ops);
    }
}

/// Test stand-in for a compiled single-thread string-element decref helper.
fn strElementDecrefSingleThread(element: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    const elem = element orelse return;
    const str_ptr: *RocStr = utils.alignedPtrCast(*RocStr, elem, @src());
    str_ptr.decrefWithAtomicity(.single_thread, roc_ops);
}

test "roc_builtins_list_decref_with_single_thread frees a unique list of strings and its elements exactly once" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const ops = env.getOps();

    const strs = [_]RocStr{
        RocStr.fromSlice("first heap-allocated element, long enough", ops),
        RocStr.fromSlice("second heap-allocated element, long enough", ops),
    };
    const l = RocList.fromSlice(RocStr, strs[0..], true, ops);
    try std.testing.expectEqual(@as(usize, 3), env.getAllocationCount());

    roc_builtins_list_decref_with_single_thread(
        l.bytes,
        l.length,
        l.capacity_or_alloc_ptr,
        @alignOf(RocStr),
        @sizeOf(RocStr),
        &strElementDecrefSingleThread,
        ops,
    );

    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "roc_builtins_list_decref_with_single_thread keeps an element alive while another handle shares it" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const ops = env.getOps();

    const shared = RocStr.fromSlice("shared heap-allocated element, long enough", ops);
    // A second handle held outside the list.
    shared.incref(1, ops);
    const strs = [_]RocStr{shared};
    const l = RocList.fromSlice(RocStr, strs[0..], true, ops);
    try std.testing.expectEqual(@as(usize, 2), env.getAllocationCount());

    roc_builtins_list_decref_with_single_thread(
        l.bytes,
        l.length,
        l.capacity_or_alloc_ptr,
        @alignOf(RocStr),
        @sizeOf(RocStr),
        &strElementDecrefSingleThread,
        ops,
    );

    // The list allocation is gone; the string allocation survives with the
    // outside handle holding its now-unique reference.
    try std.testing.expectEqual(@as(usize, 1), env.getAllocationCount());
    try std.testing.expect(shared.isUnique());
    try std.testing.expectEqualStrings("shared heap-allocated element, long enough", shared.asSlice());

    shared.decrefWithAtomicity(.single_thread, ops);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
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
    const payload_has_refcounted_children = payload_decref != null;

    if (payload_decref) |callback| {
        if (utils.isUnique(payload_ptr, roc_ops)) {
            callback(payload_ptr, roc_ops);
        }
    }

    decrefDataPtrC(payload_ptr, payload_alignment, payload_has_refcounted_children, roc_ops);
}

/// Decref a boxed payload whose allocation is proven thread-confined: the box's
/// own count update uses plain loads and stores. Payload teardown runs through
/// the `payload_decref` C function pointer, whose ABI carries no atomicity
/// parameter; the caller passes a callback whose body already matches the
/// single-thread statement (see roc_builtins_list_decref_with_single_thread).
pub fn roc_builtins_box_decref_with_single_thread(
    payload_ptr: ?[*]u8,
    payload_alignment: u32,
    payload_decref: ?RcDropFn,
    roc_ops: *RocOps,
) callconv(.c) void {
    const payload_has_refcounted_children = payload_decref != null;

    if (payload_decref) |callback| {
        if (utils.isUnique(payload_ptr, roc_ops)) {
            callback(payload_ptr, roc_ops);
        }
    }

    decrefDataPtrSingleThreadC(payload_ptr, payload_alignment, payload_has_refcounted_children, roc_ops);
}

/// Free a boxed payload and optionally run payload teardown first.
pub fn roc_builtins_box_free_with(
    payload_ptr: ?[*]u8,
    payload_alignment: u32,
    payload_decref: ?RcDropFn,
    roc_ops: *RocOps,
) callconv(.c) void {
    const payload_has_refcounted_children = payload_decref != null;

    if (payload_decref) |callback| {
        callback(payload_ptr, roc_ops);
    }

    freeDataPtrC(payload_ptr, payload_alignment, payload_has_refcounted_children, roc_ops);
}

/// Incref a boxed erased callable payload pointer.
pub fn roc_builtins_erased_callable_incref(payload_ptr: ?[*]u8, amount: isize, roc_ops: *RocOps) callconv(.c) void {
    erased_callable.incref(payload_ptr, amount, roc_ops);
}

/// Decref a boxed erased callable payload pointer, running the payload's
/// `on_drop` callback if the outer refcount reaches zero.
pub fn roc_builtins_erased_callable_decref(payload_ptr: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    erased_callable.decref(payload_ptr, roc_ops);
}

/// Decref a boxed erased callable whose allocation is proven thread-confined:
/// the callable's own count update uses plain loads and stores. The `on_drop`
/// callback is selected at closure creation, which is not an RC statement and
/// makes no thread-confinement claim, so capture-level count updates behind it
/// stay atomic (atomic is always sound).
pub fn roc_builtins_erased_callable_decref_single_thread(payload_ptr: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    if (payload_ptr) |ptr| {
        if (utils.isUnique(ptr, roc_ops)) {
            const payload = erased_callable.payloadPtr(ptr);
            if (payload.on_drop) |on_drop| {
                on_drop(erased_callable.capturePtr(ptr), roc_ops);
            }
        }
    }
    decrefDataPtrSingleThreadC(
        payload_ptr,
        erased_callable.payload_alignment,
        erased_callable.allocation_has_refcounted_children,
        roc_ops,
    );
}

/// Free a boxed erased callable payload pointer, running the payload's
/// `on_drop` callback unconditionally first.
pub fn roc_builtins_erased_callable_free(payload_ptr: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    erased_callable.free(payload_ptr, roc_ops);
}

/// Enter the loaded dev-shim code image that contains the generated callsite.
pub fn roc_builtins_hot_reload_enter(_: *RocOps) callconv(.c) ?*anyopaque {
    if (comptime @hasDecl(@import("root"), "roc_hot_reload_enter")) {
        return @import("root").roc_hot_reload_enter(@returnAddress());
    }
    return null;
}

/// Leave a loaded dev-shim code image previously returned by hot_reload_enter.
pub fn roc_builtins_hot_reload_leave(code_ref: ?*anyopaque) callconv(.c) void {
    if (comptime @hasDecl(@import("root"), "roc_hot_reload_leave")) {
        @import("root").roc_hot_reload_leave(code_ref);
    }
}

/// Retain the loaded dev-shim code image that created an erased-callable
/// payload. The retained reference is released by
/// roc_builtins_hot_reload_erased_callable_drop.
pub fn roc_builtins_hot_reload_retain_current(_: *RocOps) callconv(.c) ?*anyopaque {
    if (comptime @hasDecl(@import("root"), "roc_hot_reload_retain_current")) {
        return @import("root").roc_hot_reload_retain_current(@returnAddress());
    }
    return null;
}

/// Final-drop callback for shim-execution erased callables that carry a
/// hot-reload capture prefix.
pub fn roc_builtins_hot_reload_erased_callable_drop(capture_ptr: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    const header = erased_callable.hotReloadCaptureHeader(capture_ptr) orelse return;
    if (header.original_on_drop) |original_on_drop| {
        original_on_drop(erased_callable.hotReloadAdjustedCapturePtr(capture_ptr), roc_ops);
    }
    roc_builtins_hot_reload_leave(header.code_ref);
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

/// Re-export increfDataPtrSingleThreadC
pub fn roc_builtins_incref_data_ptr_single_thread(ptr: ?[*]u8, amount: isize, roc_ops: *RocOps) callconv(.c) void {
    increfDataPtrSingleThreadC(ptr, amount, roc_ops);
}

/// Re-export decrefDataPtrC
pub fn roc_builtins_decref_data_ptr(ptr: ?[*]u8, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops);
}

/// Re-export decrefDataPtrSingleThreadC
pub fn roc_builtins_decref_data_ptr_single_thread(ptr: ?[*]u8, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    decrefDataPtrSingleThreadC(ptr, alignment, elements_refcounted, roc_ops);
}

/// Re-export freeDataPtrC
pub fn roc_builtins_free_data_ptr(ptr: ?[*]u8, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops);
}

// ═══════════════════════════════════════════════════════════════════════════
// Numeric Wrappers
// ═══════════════════════════════════════════════════════════════════════════

fn writeRocStrFromSlice(out: *RocStr, slice: []const u8, roc_ops: *RocOps) void {
    const small_string_size = @sizeOf(RocStr);

    if (slice.len < small_string_size) {
        var buf: [small_string_size]u8 = .{0} ** small_string_size;
        @memcpy(buf[0..slice.len], slice);
        buf[small_string_size - 1] = @intCast(slice.len | 0x80);
        out.* = @bitCast(buf);
    } else {
        const heap_ptr = allocateWithRefcountC(slice.len, 1, false, roc_ops);
        @memcpy(heap_ptr[0..slice.len], slice);
        out.* = .{
            .bytes = heap_ptr,
            .capacity_or_alloc_ptr = RocStr.encodeCapacity(slice.len),
            .length = slice.len,
        };
    }
}

/// Build a RocStr from static literal bytes owned by generated code.
pub fn roc_builtins_str_from_literal(out: *RocStr, bytes: [*]const u8, len: usize, roc_ops: *RocOps) callconv(.c) void {
    writeRocStrFromSlice(out, bytes[0..len], roc_ops);
}

/// Wrapper: decToStrC (decomposed i128)
pub fn roc_builtins_dec_to_str(out: *RocStr, value_low: u64, value_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const value: i128 = @bitCast(i128h.from_u64_pair(value_low, value_high));
    const d = dec.RocDec{ .num = value };
    var buf: [dec.RocDec.max_str_length]u8 = undefined;
    const slice = d.format_to_buf(&buf);
    writeRocStrFromSlice(out, slice, roc_ops);
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
    return numeric_conversions.i128FitsTarget(val, target_bits, target_signed);
}

fn u128InTargetRange(val: u128, target_bits: u32, target_signed: bool) bool {
    return numeric_conversions.u128FitsTarget(val, target_bits, target_signed);
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
pub fn roc_builtins_dec_to_int_try_unsafe(out: [*]u8, dec_low: u64, dec_high: u64, target_bits: u32, target_is_signed: u32, val_size: u32, success_offset: u32, value_offset: u32) callconv(.c) void {
    const dec_val: i128 = @bitCast(i128h.from_u64_pair(dec_low, dec_high));
    const bits = numeric_conversions.decToIntTryBits(dec_val, target_bits, target_is_signed != 0);

    if (bits) |int_bits| {
        const v_bytes: [16]u8 = @bitCast(int_bits);
        @memcpy(out[value_offset..][0..val_size], v_bytes[0..val_size]);
    }

    out[success_offset] = @intFromBool(bits != null);
}

/// f64 → integer try unsafe
pub fn roc_builtins_f64_to_int_try_unsafe(out: [*]u8, val: f64, target_bits: u32, target_is_signed: u32, val_size: u32, success_offset: u32, value_offset: u32) callconv(.c) void {
    const bits = numeric_conversions.f64ToIntTryBits(val, target_bits, target_is_signed != 0);

    if (bits) |int_bits| {
        const v_bytes: [16]u8 = @bitCast(int_bits);
        @memcpy(out[value_offset..][0..val_size], v_bytes[0..val_size]);
    }

    out[success_offset] = @intFromBool(bits != null);
}

/// Dec → f32 try unsafe
pub fn roc_builtins_dec_to_f32_try_unsafe(out: [*]u8, dec_low: u64, dec_high: u64, success_offset: u32, value_offset: u32) callconv(.c) void {
    const dec_val: i128 = @bitCast(i128h.from_u64_pair(dec_low, dec_high));
    const f64_val: f64 = dec.toF64(dec.RocDec{ .num = dec_val });
    const f32_val: f32 = @floatCast(f64_val);
    const success: bool = !std.math.isInf(f32_val) and (!std.math.isNan(f64_val) or std.math.isNan(f32_val));
    const f32_bytes: [4]u8 = @bitCast(f32_val);
    @memcpy(out[value_offset..][0..4], &f32_bytes);
    out[success_offset] = @intFromBool(success);
}

/// f64 → f32 try unsafe
pub fn roc_builtins_f64_to_f32_try_unsafe(out: [*]u8, val: f64, success_offset: u32, value_offset: u32) callconv(.c) void {
    const f32_val: f32 = @floatCast(val);
    const success: bool = !std.math.isInf(f32_val) and (!std.math.isNan(val) or std.math.isNan(f32_val));
    const f32_bytes: [4]u8 = @bitCast(f32_val);
    @memcpy(out[value_offset..][0..4], &f32_bytes);
    out[success_offset] = @intFromBool(success);
}

/// i128 → Dec try unsafe
pub fn roc_builtins_i128_to_dec_try_unsafe(out: [*]u8, val_low: u64, val_high: u64, success_offset: u32, value_offset: u32) callconv(.c) void {
    const val: i128 = @bitCast(i128h.from_u64_pair(val_low, val_high));
    const result = dec.RocDec.fromWholeInt(val);
    const success = result != null;
    const dec_val: i128 = if (result) |d| d.num else 0;
    const dec_bytes: [16]u8 = @bitCast(@as(u128, @bitCast(dec_val)));
    @memcpy(out[value_offset..][0..16], &dec_bytes);
    out[success_offset] = @intFromBool(success);
}

/// u128 → Dec try unsafe
pub fn roc_builtins_u128_to_dec_try_unsafe(out: [*]u8, val_low: u64, val_high: u64, success_offset: u32, value_offset: u32) callconv(.c) void {
    const val: u128 = i128h.from_u64_pair(val_low, val_high);
    const fits_i128 = val <= @as(u128, @bitCast(@as(i128, std.math.maxInt(i128))));
    const result: ?dec.RocDec = if (fits_i128) dec.RocDec.fromWholeInt(@as(i128, @bitCast(val))) else null;
    const success = result != null;
    const dec_val: i128 = if (result) |d| d.num else 0;
    const dec_bytes: [16]u8 = @bitCast(@as(u128, @bitCast(dec_val)));
    @memcpy(out[value_offset..][0..16], &dec_bytes);
    out[success_offset] = @intFromBool(success);
}

// ── Dec arithmetic wrappers (decomposed i128) ──

/// Dec multiply (decomposed)
pub fn roc_builtins_dec_mul(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    const b: i128 = @bitCast(i128h.from_u64_pair(b_low, b_high));
    const result = dec.mulOrPanicC(dec.RocDec{ .num = a }, dec.RocDec{ .num = b }, roc_ops);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

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

/// Dec power (decomposed)
pub fn roc_builtins_dec_pow(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    const b: i128 = @bitCast(i128h.from_u64_pair(b_low, b_high));
    const result = dec.powC(dec.RocDec{ .num = a }, dec.RocDec{ .num = b }, roc_ops);
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

fn writeDecUnaryResult(out_low: *u64, out_high: *u64, result: i128) void {
    out_low.* = @truncate(@as(u128, @bitCast(result)));
    out_high.* = i128h.hi64(@as(u128, @bitCast(result)));
}

/// Dec square root (decomposed)
pub fn roc_builtins_dec_sqrt(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    writeDecUnaryResult(out_low, out_high, dec.sqrtC(dec.RocDec{ .num = a }, roc_ops));
}

/// Dec sine (decomposed)
pub fn roc_builtins_dec_sin(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    writeDecUnaryResult(out_low, out_high, dec.sinC(dec.RocDec{ .num = a }, roc_ops));
}

/// Dec cosine (decomposed)
pub fn roc_builtins_dec_cos(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    writeDecUnaryResult(out_low, out_high, dec.cosC(dec.RocDec{ .num = a }, roc_ops));
}

/// Dec tangent (decomposed)
pub fn roc_builtins_dec_tan(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    writeDecUnaryResult(out_low, out_high, dec.tanC(dec.RocDec{ .num = a }, roc_ops));
}

/// Dec arcsine (decomposed)
pub fn roc_builtins_dec_asin(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    writeDecUnaryResult(out_low, out_high, dec.asinC(dec.RocDec{ .num = a }, roc_ops));
}

/// Dec arccosine (decomposed)
pub fn roc_builtins_dec_acos(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    writeDecUnaryResult(out_low, out_high, dec.acosC(dec.RocDec{ .num = a }, roc_ops));
}

/// Dec arctangent (decomposed)
pub fn roc_builtins_dec_atan(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, roc_ops: *RocOps) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    writeDecUnaryResult(out_low, out_high, dec.atanC(dec.RocDec{ .num = a }, roc_ops));
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

// ── i128/u128 shift wrappers (decomposed) ──

/// u128 shift left (decomposed): out = a << shift_amount
pub fn roc_builtins_num_shl_u128(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, shift_amount: u8) callconv(.c) void {
    const a: u128 = i128h.from_u64_pair(a_low, a_high);
    const s: u7 = @intCast(shift_amount & 127);
    const result = i128h.shl(a, s);
    out_low.* = @truncate(result);
    out_high.* = i128h.hi64(result);
}

/// i128 arithmetic shift right (decomposed): out = a >> shift_amount (sign-extending)
pub fn roc_builtins_num_shr_i128(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, shift_amount: u8) callconv(.c) void {
    const a: i128 = @bitCast(i128h.from_u64_pair(a_low, a_high));
    const s: u7 = @intCast(shift_amount & 127);
    const result: u128 = @bitCast(i128h.shr_i128(a, s));
    out_low.* = @truncate(result);
    out_high.* = i128h.hi64(result);
}

/// u128 logical shift right (decomposed): out = a >> shift_amount (zero-fill)
pub fn roc_builtins_num_shr_u128(out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, shift_amount: u8) callconv(.c) void {
    const a: u128 = i128h.from_u64_pair(a_low, a_high);
    const s: u7 = @intCast(shift_amount & 127);
    const result = i128h.shr(a, s);
    out_low.* = @truncate(result);
    out_high.* = i128h.hi64(result);
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

fn signedIntToStr(comptime T: type, buf: []u8, val: T) []u8 {
    return i128ToStr(buf, @as(i128, @intCast(val)));
}

fn unsignedIntToStr(comptime T: type, buf: []u8, val: T) []u8 {
    return u128ToStr(buf, @as(u128, @intCast(val)));
}

/// Unified integer-to-string wrapper: dispatches on int_width/is_signed
pub fn roc_builtins_int_to_str(out: *RocStr, val_low: u64, val_high: u64, int_width: u8, is_signed: bool, roc_ops: *RocOps) callconv(.c) void {
    var buf: [40]u8 = undefined;
    const result = switch (int_width) {
        1 => if (is_signed)
            signedIntToStr(i8, &buf, @as(i8, @bitCast(@as(u8, @truncate(val_low)))))
        else
            unsignedIntToStr(u8, &buf, @as(u8, @truncate(val_low))),
        2 => if (is_signed)
            signedIntToStr(i16, &buf, @as(i16, @bitCast(@as(u16, @truncate(val_low)))))
        else
            unsignedIntToStr(u16, &buf, @as(u16, @truncate(val_low))),
        4 => if (is_signed)
            signedIntToStr(i32, &buf, @as(i32, @bitCast(@as(u32, @truncate(val_low)))))
        else
            unsignedIntToStr(u32, &buf, @as(u32, @truncate(val_low))),
        8 => if (is_signed)
            signedIntToStr(i64, &buf, @as(i64, @bitCast(val_low)))
        else
            unsignedIntToStr(u64, &buf, val_low),
        16 => blk: {
            const val128: u128 = i128h.from_u64_pair(val_low, val_high);
            break :blk if (is_signed)
                i128ToStr(&buf, @as(i128, @bitCast(val128)))
            else
                u128ToStr(&buf, val128);
        },
        else => unreachable,
    };
    writeRocStrFromSlice(out, result, roc_ops);
}

/// Unified float-to-string wrapper: dispatches on is_f32.
/// Uses Ryu's binaryToDecimal directly and formats manually to avoid
/// pulling in Zig's generic float formatter, which references isPowerOf10
/// (u128 div/mod → __udivti3/__umodti3 compiler_rt symbols).
pub fn roc_builtins_float_to_str(out: *RocStr, val_bits: u64, is_f32: bool, roc_ops: *RocOps) callconv(.c) void {
    out.* = str.floatToStrFromBits(val_bits, is_f32, roc_ops);
}

/// Return the floor of an F32 or F64 value passed as F64, preserving the requested width.
pub fn roc_builtins_float_floor(val: f64, float_width: u8) callconv(.c) f64 {
    return switch (float_width) {
        4 => @as(f64, @floatCast(@floor(@as(f32, @floatCast(val))))),
        8 => @floor(val),
        else => unreachable,
    };
}

/// Return the ceiling of an F32 or F64 value passed as F64, preserving the requested width.
pub fn roc_builtins_float_ceiling(val: f64, float_width: u8) callconv(.c) f64 {
    return switch (float_width) {
        4 => @as(f64, @floatCast(@ceil(@as(f32, @floatCast(val))))),
        8 => @ceil(val),
        else => unreachable,
    };
}

/// Raise an F32 or F64 base to an exponent, with both values passed as F64.
pub fn roc_builtins_float_pow(base: f64, exponent: f64, float_width: u8) callconv(.c) f64 {
    return switch (float_width) {
        4 => @as(f64, @floatCast(std.math.pow(f32, @as(f32, @floatCast(base)), @as(f32, @floatCast(exponent))))),
        8 => std.math.pow(f64, base, exponent),
        else => unreachable,
    };
}

const FloatUnaryMathOp = enum {
    sin,
    cos,
    tan,
    asin,
    acos,
    atan,
};

fn floatUnaryMath(val: f64, float_width: u8, comptime op: FloatUnaryMathOp) f64 {
    return switch (float_width) {
        4 => @as(f64, @floatCast(switch (op) {
            .sin => std.math.sin(@as(f32, @floatCast(val))),
            .cos => std.math.cos(@as(f32, @floatCast(val))),
            .tan => float_tan.tan32(@as(f32, @floatCast(val))),
            .asin => std.math.asin(@as(f32, @floatCast(val))),
            .acos => std.math.acos(@as(f32, @floatCast(val))),
            .atan => std.math.atan(@as(f32, @floatCast(val))),
        })),
        8 => switch (op) {
            .sin => std.math.sin(val),
            .cos => std.math.cos(val),
            .tan => float_tan.tan64(val),
            .asin => std.math.asin(val),
            .acos => std.math.acos(val),
            .atan => std.math.atan(val),
        },
        else => unreachable,
    };
}

/// Return the sine of an F32 or F64 value passed as F64.
pub fn roc_builtins_float_sin(val: f64, float_width: u8) callconv(.c) f64 {
    return floatUnaryMath(val, float_width, .sin);
}

/// Return the cosine of an F32 or F64 value passed as F64.
pub fn roc_builtins_float_cos(val: f64, float_width: u8) callconv(.c) f64 {
    return floatUnaryMath(val, float_width, .cos);
}

/// Return the tangent of an F32 or F64 value passed as F64.
pub fn roc_builtins_float_tan(val: f64, float_width: u8) callconv(.c) f64 {
    return floatUnaryMath(val, float_width, .tan);
}

/// Return the arcsine of an F32 or F64 value passed as F64.
pub fn roc_builtins_float_asin(val: f64, float_width: u8) callconv(.c) f64 {
    return floatUnaryMath(val, float_width, .asin);
}

/// Return the arccosine of an F32 or F64 value passed as F64.
pub fn roc_builtins_float_acos(val: f64, float_width: u8) callconv(.c) f64 {
    return floatUnaryMath(val, float_width, .acos);
}

/// Return the arctangent of an F32 or F64 value passed as F64.
pub fn roc_builtins_float_atan(val: f64, float_width: u8) callconv(.c) f64 {
    return floatUnaryMath(val, float_width, .atan);
}

test "float floor and ceiling wrappers" {
    try std.testing.expectEqual(@as(f64, 3.0), roc_builtins_float_floor(3.9, 4));
    try std.testing.expectEqual(@as(f64, -4.0), roc_builtins_float_floor(-3.2, 4));
    try std.testing.expectEqual(@as(f64, 4.0), roc_builtins_float_ceiling(3.2, 4));
    try std.testing.expectEqual(@as(f64, -3.0), roc_builtins_float_ceiling(-3.2, 4));

    try std.testing.expectEqual(@as(f64, 3.0), roc_builtins_float_floor(3.9, 8));
    try std.testing.expectEqual(@as(f64, -4.0), roc_builtins_float_floor(-3.2, 8));
    try std.testing.expectEqual(@as(f64, 4.0), roc_builtins_float_ceiling(3.2, 8));
    try std.testing.expectEqual(@as(f64, -3.0), roc_builtins_float_ceiling(-3.2, 8));
}

test "float pow wrapper" {
    try std.testing.expectEqual(@as(f64, 8.0), roc_builtins_float_pow(2.0, 3.0, 4));
    try std.testing.expectEqual(@as(f64, 3.0), roc_builtins_float_pow(9.0, 0.5, 4));

    try std.testing.expectEqual(@as(f64, 8.0), roc_builtins_float_pow(2.0, 3.0, 8));
    try std.testing.expectEqual(@as(f64, 3.0), roc_builtins_float_pow(9.0, 0.5, 8));
}

test "float trig wrappers" {
    inline for (.{ @as(u8, 4), @as(u8, 8) }) |width| {
        try std.testing.expectEqual(@as(f64, 0.0), roc_builtins_float_sin(0.0, width));
        try std.testing.expectEqual(@as(f64, 1.0), roc_builtins_float_cos(0.0, width));
        try std.testing.expectEqual(@as(f64, 0.0), roc_builtins_float_tan(0.0, width));
        try std.testing.expectEqual(@as(f64, 0.0), roc_builtins_float_asin(0.0, width));
        try std.testing.expectEqual(@as(f64, 0.0), roc_builtins_float_acos(1.0, width));
        try std.testing.expectEqual(@as(f64, 0.0), roc_builtins_float_atan(0.0, width));
    }
}

test "direct float wrapper f32" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();

    var out: RocStr = undefined;
    const bits: u32 = @bitCast(@as(f32, 3.14));
    roc_builtins_float_to_str(&out, bits, true, env.getOps());
    defer out.decref(env.getOps());

    try std.testing.expectEqualStrings("3.140000104904175", out.asSlice());
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

// ── List equality and reverse wrappers ──

/// Compare two lists of flat (non-refcounted) elements for equality.
/// Elements are compared byte-by-byte using the element width.
pub fn roc_builtins_list_eq(a_bytes: ?[*]u8, a_len: usize, _: usize, b_bytes: ?[*]u8, b_len: usize, _: usize, elem_width: usize) callconv(.c) bool {
    if (a_len != b_len) return false;
    if (a_len == 0) return true;
    if (a_bytes == b_bytes) return true;
    const a = a_bytes orelse return b_bytes == null;
    const b = b_bytes orelse return false;
    return std.mem.eql(u8, a[0 .. a_len * elem_width], b[0 .. b_len * elem_width]);
}

/// Compare two lists of strings for equality.
pub fn roc_builtins_list_str_eq(a_bytes: ?[*]u8, a_len: usize, _: usize, b_bytes: ?[*]u8, b_len: usize, _: usize) callconv(.c) bool {
    if (a_len != b_len) return false;
    if (a_len == 0) return true;
    if (a_bytes == b_bytes) return true;
    const a = a_bytes orelse return b_bytes == null;
    const b = b_bytes orelse return false;
    const str_size = @sizeOf(RocStr);
    for (0..a_len) |i| {
        const a_str: *const RocStr = @ptrCast(@alignCast(a + i * str_size));
        const b_str: *const RocStr = @ptrCast(@alignCast(b + i * str_size));
        if (!strEqual(a_str.*, b_str.*)) return false;
    }
    return true;
}

/// Compare two lists of lists for equality (inner elements are flat).
pub fn roc_builtins_list_list_eq(a_bytes: ?[*]u8, a_len: usize, _: usize, b_bytes: ?[*]u8, b_len: usize, _: usize, inner_elem_width: usize) callconv(.c) bool {
    if (a_len != b_len) return false;
    if (a_len == 0) return true;
    if (a_bytes == b_bytes) return true;
    const a = a_bytes orelse return b_bytes == null;
    const b = b_bytes orelse return false;
    const list_size = @sizeOf(RocList);
    for (0..a_len) |i| {
        const a_list: *const RocList = @ptrCast(@alignCast(a + i * list_size));
        const b_list: *const RocList = @ptrCast(@alignCast(b + i * list_size));
        if (a_list.length != b_list.length) return false;
        if (a_list.length == 0) continue;
        if (a_list.bytes == b_list.bytes) continue;
        const ab = a_list.bytes orelse return b_list.bytes == null;
        const bb = b_list.bytes orelse return false;
        if (!std.mem.eql(u8, ab[0 .. a_list.length * inner_elem_width], bb[0 .. b_list.length * inner_elem_width])) return false;
    }
    return true;
}

/// Wrapper: listReverse. The update mode is forwarded to the builtin's
/// uniqueness check; `.InPlace` skips it and reverses the elements in place.
pub fn roc_builtins_list_reverse(out: *RocList, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, alignment: u32, element_width: usize, elements_refcounted: bool, element_incref: ?RcIncFn, element_decref: ?RcDropFn, update_mode: utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const l = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (elements_refcounted) {
        var inc_ctx = CallbackElementIncrefContext{
            .callback = element_incref orelse unreachable,
            .roc_ops = roc_ops,
        };
        var dec_ctx = CallbackElementDecrefContext{
            .callback = element_decref orelse unreachable,
            .roc_ops = roc_ops,
        };
        out.* = list.listReverse(l, alignment, element_width, true, @ptrCast(&inc_ctx), &callbackListElementIncref, @ptrCast(&dec_ctx), &callbackListElementDecref, update_mode, &copy_fallback, roc_ops);
    } else {
        out.* = list.listReverse(l, alignment, element_width, false, null, @ptrCast(&rcNone), null, @ptrCast(&rcNone), update_mode, &copy_fallback, roc_ops);
    }
}

/// i32 modulo (floored division mod, not truncated remainder)
pub fn roc_builtins_i32_mod_by(a: i32, b: i32) callconv(.c) i32 {
    return @mod(a, b);
}

/// i8 modulo (floored division mod, not truncated remainder)
pub fn roc_builtins_i8_mod_by(a: i32, b: i32) callconv(.c) i32 {
    const lhs: i8 = @intCast(a);
    const rhs: i8 = @intCast(b);
    return @intCast(@mod(lhs, rhs));
}

/// u8 modulo (floored division mod, not truncated remainder)
pub fn roc_builtins_u8_mod_by(a: i32, b: i32) callconv(.c) i32 {
    const lhs: u8 = @intCast(a);
    const rhs: u8 = @intCast(b);
    return @intCast(@mod(lhs, rhs));
}

/// i16 modulo (floored division mod, not truncated remainder)
pub fn roc_builtins_i16_mod_by(a: i32, b: i32) callconv(.c) i32 {
    const lhs: i16 = @intCast(a);
    const rhs: i16 = @intCast(b);
    return @intCast(@mod(lhs, rhs));
}

/// u16 modulo (floored division mod, not truncated remainder)
pub fn roc_builtins_u16_mod_by(a: i32, b: i32) callconv(.c) i32 {
    const lhs: u16 = @intCast(a);
    const rhs: u16 = @intCast(b);
    return @intCast(@mod(lhs, rhs));
}

/// u32 modulo (floored division mod, not truncated remainder)
pub fn roc_builtins_u32_mod_by(a: u32, b: u32) callconv(.c) u32 {
    return @mod(a, b);
}

/// i64 modulo (floored division mod, not truncated remainder)
pub fn roc_builtins_i64_mod_by(a: i64, b: i64) callconv(.c) i64 {
    return @mod(a, b);
}

/// u64 modulo (floored division mod, not truncated remainder)
pub fn roc_builtins_u64_mod_by(a: u64, b: u64) callconv(.c) u64 {
    return @mod(a, b);
}
