//! LIR Code Generator
//!
//! This module generates native machine code from statement-only LIR procs.
//! It uses the Emit.zig infrastructure for instruction encoding and
//! ValueStorage.zig for register allocation.
//!
//! Pipeline position:
//! ```
//! checked modules -> post-check IRs -> LIR -> LirCodeGen -> Machine Code
//! ```
//!
//! Key properties:
//! - Uses real machine instructions via Emit.zig
//! - Proper register allocation with spilling support
//! - Handles System V ABI (x86_64/aarch64) calling convention
//! - Generates position-independent code with relocations
//! - Supports x86_64 and aarch64 architectures
//!
//! RC boundary:
//! - this backend may lower explicit LIR RC statements
//! - builtin helper implementations may perform primitive-internal RC
//! - ordinary codegen paths are forbidden from inventing RC policy

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
const increfDataPtrSingleThreadC = builtins.utils.increfDataPtrSingleThreadC;
const decrefDataPtrC = builtins.utils.decrefDataPtrC;
const decrefDataPtrSingleThreadC = builtins.utils.decrefDataPtrSingleThreadC;
const freeDataPtrC = builtins.utils.freeDataPtrC;

// List builtin functions - using C-compatible wrappers to avoid ABI issues
// with 24-byte RocList struct returns on aarch64
const RocList = builtins.list.RocList;

// String builtins
const strToUtf8C = builtins.str.strToUtf8C;
const strConcatC = builtins.str.strConcatC;
const strContains = builtins.str.strContains;
const strStartsWith = builtins.str.startsWith;
const strEndsWith = builtins.str.endsWith;
const strCountUtf8Bytes = builtins.str.countUtf8Bytes;
const strCaselessAsciiEquals = builtins.str.strCaselessAsciiEquals;
const strEqual = builtins.str.strEqual;
const strEqualStaticSmall = builtins.str.strEqualStaticSmall;
const strStaticSmallWordEq = builtins.str.strStaticSmallWordEq;
const strStaticSmallWordCaselessEq = builtins.str.strStaticSmallWordCaselessEq;
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
const strDropPrefixCaselessAscii = builtins.str.strDropPrefixCaselessAscii;
const strDropSuffix = builtins.str.strDropSuffix;
const strFindFirst = builtins.str.findFirst;
const strWithAsciiLowercased = builtins.str.strWithAsciiLowercased;
const strWithAsciiUppercased = builtins.str.strWithAsciiUppercased;
const strFromUtf8Lossy = builtins.str.fromUtf8Lossy;

const Relocation = @import("Relocation.zig").Relocation;

const StaticStringData = @import("StaticStringData.zig");

const LirStore = lir.LirStore;
const Symbol = lir.Symbol;
const JoinPointId = lir.JoinPointId;
const LocalId = lir.LocalId;
const LocalSpan = lir.LocalSpan;
// Layout store for accessing struct/tag field offsets
const LayoutStore = layout.Store;
const RcOp = layout.RcOp;
const RcHelperKey = layout.RcHelperKey;
const RcAtomicity = lir.LIR.RcAtomicity;

/// Identity of one compiled RC helper: the canonical layout plan plus the
/// count-update atomicity the helper's own updates use. Atomic and
/// single-thread helpers are compiled separately, so a helper's body never
/// has to branch on atomicity at runtime. The element/payload callback ABIs
/// carry no atomicity parameter, so the statement's atomicity is baked into
/// which helper variant a teardown callback pointer names; callbacks for
/// runtime-checked list ops and the erased-callable `on_drop` belong to no
/// RC statement and always use the atomic variant.
const RcHelperVariant = struct {
    key: RcHelperKey,
    atomicity: RcAtomicity,

    /// Pack the variant into a stable integer for the helper caches.
    /// `RcHelperKey.encode` occupies bits 0..33, so the atomicity bit is
    /// placed above it.
    fn encode(self: RcHelperVariant) u64 {
        return (@as(u64, @intFromEnum(self.atomicity)) << 34) | self.key.encode();
    }
};

// Control flow statement types (for two-pass compilation)
const CFStmtId = lir.CFStmtId;

const BuiltinListAbi = struct {
    elem_layout_idx: ?layout.Idx,
    elem_layout: layout.Layout,
    elem_size_align: layout.SizeAlign,
    alignment_bytes: u32,
    elements_refcounted: bool,
};

/// Immediate value for a builtin wrapper's update-mode parameter selected by
/// the statement's statically-proven-unique argument mask: `.InPlace` when bit
/// 0 says argument 0's runtime uniqueness check is redundant, `.Immutable`
/// (checked) otherwise.
fn updateModeImmForArg0(unique_args: u64) i64 {
    return @intFromEnum(if ((unique_args & 1) != 0) builtins.utils.UpdateMode.InPlace else builtins.utils.UpdateMode.Immutable);
}

fn builtinInternalListAbi(ls: *const LayoutStore, comptime _: []const u8, list_layout_idx: layout.Idx) BuiltinListAbi {
    const abi = ls.builtinListAbi(list_layout_idx);
    return .{
        .elem_layout_idx = abi.elem_layout_idx,
        .elem_layout = abi.elem_layout,
        .elem_size_align = .{
            .size = @intCast(abi.elem_size),
            .alignment = layout.RocAlignment.fromByteUnits(@intCast(abi.elem_alignment)),
        },
        .alignment_bytes = abi.elem_alignment,
        .elements_refcounted = abi.contains_refcounted,
    };
}

/// Generation mode determines how builtin function calls are emitted.
/// This is important because the dev backend can be used in two ways:
/// 1. In-process execution (dev evaluator): Direct function pointers work
/// 2. Object file generation (roc build --opt=dev): Need symbol references
/// One source line table entry: the code offset where a statement's machine
/// code begins, paired with the statement's source location.
pub const LineEntry = struct {
    offset: u32,
    loc: base.SourceLoc,
};

/// How generated code will be executed, which determines how calls to
/// builtins and other procs are encoded.
pub const GenerationMode = enum {
    /// Code runs in-process (dev evaluator), direct function pointers are valid.
    /// The compiled code calls builtins via absolute addresses embedded in the code.
    native_execution,
    /// Code runs in the linked shim child process from bytes produced by the
    /// parent compiler. Internal Roc procs receive RocOps, while builtin and
    /// readonly-data references are explicit relocation records.
    shim_execution,
    /// Generating relocatable object files for linking.
    /// Builtin calls must use symbol references that the linker will resolve.
    object_file,

    fn threadsRocOps(self: GenerationMode) bool {
        return self != .object_file;
    }
};

/// Compiler-internal callbacks emitted only for native compile-time evaluation.
pub const ComptimeHooks = struct {
    branch_taken: *const fn (*RocOps, u32, u32) callconv(.c) void,
    exhaustiveness_failed: *const fn (*RocOps, u32) callconv(.c) void,
    failure_region: *const fn (*RocOps, u32, u32) callconv(.c) void,
    call_enter: *const fn (*RocOps, u32, u32) callconv(.c) void,
    call_exit: *const fn (*RocOps) callconv(.c) void,
};

/// Builtin function identifiers for the dev backend.
/// These map to exported symbols in dev_wrappers.zig for object file generation.
pub const BuiltinFn = enum {
    // Memory/refcounting
    allocate_with_refcount,
    incref_data_ptr,
    incref_data_ptr_single_thread,
    decref_data_ptr,
    decref_data_ptr_single_thread,
    free_data_ptr,

    // String operations
    str_to_utf8,
    str_concat,
    str_contains,
    str_starts_with,
    str_ends_with,
    str_equal,
    str_equal_static_small,
    str_static_small_word_eq,
    str_static_small_word_caseless_eq,
    str_count_utf8_bytes,
    str_find_first,
    str_drop_prefix_caseless_ascii,
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
    dbg_str,
    expect_err_str,

    // List operations
    list_with_capacity,
    list_append_unsafe,
    list_concat,
    list_prepend,
    list_sublist,
    list_reverse,
    list_incref,
    list_incref_single_thread,
    list_drop_at,
    list_replace,
    list_swap,
    list_map_can_reuse,
    list_reserve,
    list_release_excess_capacity,
    list_decref_str,
    list_decref_with,
    list_decref_with_single_thread,
    list_decref_flat_list,
    list_free_with,
    list_free_flat_list,
    box_decref_with,
    box_decref_with_single_thread,
    box_free_with,
    erased_callable_incref,
    erased_callable_decref,
    erased_callable_decref_single_thread,
    erased_callable_free,
    hot_reload_enter,
    hot_reload_leave,
    hot_reload_retain_current,
    hot_reload_erased_callable_drop,

    // Numeric operations
    dec_to_str,
    dec_mul,
    dec_mul_saturated,
    dec_div,
    dec_div_trunc,
    dec_pow,
    dec_sqrt,
    dec_sin,
    dec_cos,
    dec_tan,
    dec_asin,
    dec_acos,
    dec_atan,
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
    num_shl_u128,
    num_shr_i128,
    num_shr_u128,
    int_to_str,
    float_to_str,
    float_floor,
    float_ceiling,
    float_pow,
    float_sin,
    float_cos,
    float_tan,
    float_asin,
    float_acos,
    float_atan,
    int_from_str,
    dec_from_str,
    float_from_str,

    // Hasher operations
    dict_pseudo_seed,
    hasher_finish,
    hasher_write_u64,
    hasher_write_u128,
    hasher_write_f32_bits,
    hasher_write_f64_bits,
    hasher_write_bytes,
    hasher_write_str,

    // Crypto operations
    crypto_sha256_hash_bytes,
    crypto_sha256_hasher_empty,
    crypto_sha256_hasher_write,
    crypto_sha256_hasher_finish,
    crypto_blake3_hash_bytes,
    crypto_blake3_hasher_empty,
    crypto_blake3_hasher_write,
    crypto_blake3_hasher_finish,

    /// Get the exported symbol name for object file linking.
    pub fn symbolName(self: BuiltinFn) []const u8 {
        return switch (self) {
            // Memory/refcounting
            .allocate_with_refcount => "roc_builtins_allocate_with_refcount",
            .incref_data_ptr => "roc_builtins_incref_data_ptr",
            .incref_data_ptr_single_thread => "roc_builtins_incref_data_ptr_single_thread",
            .decref_data_ptr => "roc_builtins_decref_data_ptr",
            .decref_data_ptr_single_thread => "roc_builtins_decref_data_ptr_single_thread",
            .free_data_ptr => "roc_builtins_free_data_ptr",

            // String operations
            .str_to_utf8 => "roc_builtins_str_to_utf8",
            .str_concat => "roc_builtins_str_concat",
            .str_contains => "roc_builtins_str_contains",
            .str_starts_with => "roc_builtins_str_starts_with",
            .str_ends_with => "roc_builtins_str_ends_with",
            .str_equal => "roc_builtins_str_equal",
            .str_equal_static_small => "roc_builtins_str_equal_static_small",
            .str_static_small_word_eq => "roc_builtins_str_static_small_word_eq",
            .str_static_small_word_caseless_eq => "roc_builtins_str_static_small_word_caseless_eq",
            .str_count_utf8_bytes => "roc_builtins_str_count_utf8_bytes",
            .str_find_first => "roc_builtins_str_find_first",
            .str_drop_prefix_caseless_ascii => "roc_builtins_str_drop_prefix_caseless_ascii",
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
            .str_from_utf8 => "roc_builtins_str_from_utf8_result",
            .str_escape_and_quote => "roc_builtins_str_escape_and_quote",
            .dbg_str => "roc_builtins_dbg_str",
            .expect_err_str => "roc_builtins_expect_err_str",

            // List operations
            .list_with_capacity => "roc_builtins_list_with_capacity",
            .list_append_unsafe => "roc_builtins_list_append_unsafe",
            .list_concat => "roc_builtins_list_concat",
            .list_prepend => "roc_builtins_list_prepend",
            .list_sublist => "roc_builtins_list_sublist",
            .list_reverse => "roc_builtins_list_reverse",
            .list_incref => "roc_builtins_list_incref",
            .list_incref_single_thread => "roc_builtins_list_incref_single_thread",
            .list_drop_at => "roc_builtins_list_drop_at",
            .list_replace => "roc_builtins_list_replace",
            .list_swap => "roc_builtins_list_swap",
            .list_map_can_reuse => "roc_builtins_list_map_can_reuse",
            .list_reserve => "roc_builtins_list_reserve",
            .list_release_excess_capacity => "roc_builtins_list_release_excess_capacity",
            .list_decref_str => "roc_builtins_list_decref_str",
            .list_decref_with => "roc_builtins_list_decref_with",
            .list_decref_with_single_thread => "roc_builtins_list_decref_with_single_thread",
            .list_decref_flat_list => "roc_builtins_list_decref_flat_list",
            .list_free_with => "roc_builtins_list_free_with",
            .list_free_flat_list => "roc_builtins_list_free_flat_list",
            .box_decref_with => "roc_builtins_box_decref_with",
            .box_decref_with_single_thread => "roc_builtins_box_decref_with_single_thread",
            .box_free_with => "roc_builtins_box_free_with",
            .erased_callable_incref => "roc_builtins_erased_callable_incref",
            .erased_callable_decref => "roc_builtins_erased_callable_decref",
            .erased_callable_decref_single_thread => "roc_builtins_erased_callable_decref_single_thread",
            .erased_callable_free => "roc_builtins_erased_callable_free",
            .hot_reload_enter => "roc_builtins_hot_reload_enter",
            .hot_reload_leave => "roc_builtins_hot_reload_leave",
            .hot_reload_retain_current => "roc_builtins_hot_reload_retain_current",
            .hot_reload_erased_callable_drop => "roc_builtins_hot_reload_erased_callable_drop",

            // Numeric operations
            .dec_to_str => "roc_builtins_dec_to_str",
            .dec_mul => "roc_builtins_dec_mul",
            .dec_mul_saturated => "roc_builtins_dec_mul_saturated",
            .dec_div => "roc_builtins_dec_div",
            .dec_div_trunc => "roc_builtins_dec_div_trunc",
            .dec_pow => "roc_builtins_dec_pow",
            .dec_sqrt => "roc_builtins_dec_sqrt",
            .dec_sin => "roc_builtins_dec_sin",
            .dec_cos => "roc_builtins_dec_cos",
            .dec_tan => "roc_builtins_dec_tan",
            .dec_asin => "roc_builtins_dec_asin",
            .dec_acos => "roc_builtins_dec_acos",
            .dec_atan => "roc_builtins_dec_atan",
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
            .num_shl_u128 => "roc_builtins_num_shl_u128",
            .num_shr_i128 => "roc_builtins_num_shr_i128",
            .num_shr_u128 => "roc_builtins_num_shr_u128",
            .int_to_str => "roc_builtins_int_to_str",
            .float_to_str => "roc_builtins_float_to_str",
            .float_floor => "roc_builtins_float_floor",
            .float_ceiling => "roc_builtins_float_ceiling",
            .float_pow => "roc_builtins_float_pow",
            .float_sin => "roc_builtins_float_sin",
            .float_cos => "roc_builtins_float_cos",
            .float_tan => "roc_builtins_float_tan",
            .float_asin => "roc_builtins_float_asin",
            .float_acos => "roc_builtins_float_acos",
            .float_atan => "roc_builtins_float_atan",
            .int_from_str => "roc_builtins_int_from_str",
            .dec_from_str => "roc_builtins_dec_from_str",
            .float_from_str => "roc_builtins_float_from_str",

            // Hasher operations
            .dict_pseudo_seed => "roc_builtins_dict_pseudo_seed",
            .hasher_finish => "roc_builtins_hasher_finish",
            .hasher_write_u64 => "roc_builtins_hasher_write_u64",
            .hasher_write_u128 => "roc_builtins_hasher_write_u128",
            .hasher_write_f32_bits => "roc_builtins_hasher_write_f32_bits",
            .hasher_write_f64_bits => "roc_builtins_hasher_write_f64_bits",
            .hasher_write_bytes => "roc_builtins_hasher_write_bytes",
            .hasher_write_str => "roc_builtins_hasher_write_str",

            // Crypto operations
            .crypto_sha256_hash_bytes => "roc_builtins_crypto_sha256_hash_bytes",
            .crypto_sha256_hasher_empty => "roc_builtins_crypto_sha256_hasher_empty",
            .crypto_sha256_hasher_write => "roc_builtins_crypto_sha256_hasher_write",
            .crypto_sha256_hasher_finish => "roc_builtins_crypto_sha256_hasher_finish",
            .crypto_blake3_hash_bytes => "roc_builtins_crypto_blake3_hash_bytes",
            .crypto_blake3_hasher_empty => "roc_builtins_crypto_blake3_hasher_empty",
            .crypto_blake3_hasher_write => "roc_builtins_crypto_blake3_hasher_write",
            .crypto_blake3_hasher_finish => "roc_builtins_crypto_blake3_hasher_finish",
        };
    }
};

/// Special layout index for List I64 type (must match dev_evaluator.zig).
/// Lists are (ptr, len, capacity) = 24 bytes and need special handling when returning results.

// Number-to-string C wrapper functions (explicit output pointer to avoid struct return ABI issues)
const RocStr = builtins.str.RocStr;
const RocOps = builtins.host_abi.RocOps;
const HostedFunctions = builtins.host_abi.HostedFunctions;

// ── C wrapper functions for string/list builtins ──
// These decompose 24-byte RocStr/RocList structs into individual 8-byte fields
// so all args fit in registers and we avoid platform-specific struct-passing ABI issues.

/// Wrapper: strToUtf8C(RocStr, *RocOps) -> RocList
/// Decomposed: (out, str_bytes, str_len, str_cap, roc_ops) -> void
fn wrapStrToUtf8(out: *RocList, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const arg = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strToUtf8C(arg, roc_ops);
}

/// Wrapper: strConcatC(RocStr, RocStr, UpdateMode, *RocOps) -> RocStr
/// Decomposed: (out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, update_mode, roc_ops) -> void
fn wrapStrConcat(out: *RocStr, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, update_mode: builtins.utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };

    if (builtin.mode == .Debug) {
        const debugAssertValidStr = struct {
            fn check(label: []const u8, s: RocStr) void {
                if (s.isSmallStr()) return;
                if (s.bytes != null) return;

                std.debug.print(
                    "wrapStrConcat invalid RocStr {s}: len={d} cap=0x{x} raw_bytes=0x{x}\n",
                    .{ label, s.len(), s.capacity_or_alloc_ptr, @intFromPtr(s.bytes) },
                );
                std.debug.panic(
                    "LIR/codegen invariant violated: wrapStrConcat received invalid RocStr {s} (len={d}, cap=0x{x})",
                    .{ label, s.len(), s.capacity_or_alloc_ptr },
                );
            }
        }.check;

        debugAssertValidStr("lhs", a);
        debugAssertValidStr("rhs", b);
    }

    out.* = strConcatC(a, b, update_mode, roc_ops);
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

/// Wrapper: strEqualStaticSmall(RocStr, u64, u64, u64, u64) -> bool
fn wrapStrEqualStaticSmall(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, static_len: u64, word0: u64, word1: u64, word2: u64) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    return strEqualStaticSmall(a, static_len, word0, word1, word2);
}

/// Wrapper: strStaticSmallWordEq(RocStr, u64, u64, u64) -> bool
fn wrapStrStaticSmallWordEq(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, offset: u64, active_len: u64, word: u64) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    return strStaticSmallWordEq(a, offset, active_len, word);
}

/// Wrapper: strStaticSmallWordCaselessEq(RocStr, u64, u64, u64) -> bool
fn wrapStrStaticSmallWordCaselessEq(a_bytes: ?[*]u8, a_len: usize, a_cap: usize, offset: u64, active_len: u64, word: u64) callconv(.c) bool {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    return strStaticSmallWordCaselessEq(a, offset, active_len, word);
}

/// Wrapper: countUtf8Bytes(RocStr) -> u64
fn wrapStrCountUtf8Bytes(str_bytes: ?[*]u8, str_len: usize, str_cap: usize) callconv(.c) u64 {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    return strCountUtf8Bytes(s);
}

fn wrapStrFindFirst(out: *anyopaque, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, find_layout: *const dev_wrappers.StrFindFirstLayout, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    const result = strFindFirst(a, b, roc_ops);
    const out_bytes: [*]u8 = @ptrCast(out);

    @as(*RocStr, @ptrCast(@alignCast(out_bytes + find_layout.after_offset))).* = result.after;
    @as(*RocStr, @ptrCast(@alignCast(out_bytes + find_layout.before_offset))).* = result.before;
    @as(*u8, @ptrCast(@alignCast(out_bytes + find_layout.found_offset))).* = if (result.found) 1 else 0;
}

fn wrapStrDropPrefixCaselessAscii(out: *anyopaque, a_bytes: ?[*]u8, a_len: usize, a_cap: usize, b_bytes: ?[*]u8, b_len: usize, b_cap: usize, prefix_layout: *const dev_wrappers.StrDropPrefixCaselessAsciiLayout, roc_ops: *RocOps) callconv(.c) void {
    const a = RocStr{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap };
    const b = RocStr{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap };
    const result = strDropPrefixCaselessAscii(a, b, roc_ops);
    const out_bytes: [*]u8 = @ptrCast(out);

    @as(*RocStr, @ptrCast(@alignCast(out_bytes + prefix_layout.after_offset))).* = result.after;
    @as(*u8, @ptrCast(@alignCast(out_bytes + prefix_layout.found_offset))).* = if (result.found) 1 else 0;
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

/// Wrapper: strTrim(RocStr, UpdateMode, *RocOps) -> RocStr
fn wrapStrTrim(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: builtins.utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrim(s, update_mode, roc_ops);
}

/// Wrapper: strTrimStart(RocStr, UpdateMode, *RocOps) -> RocStr
fn wrapStrTrimStart(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: builtins.utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrimStart(s, update_mode, roc_ops);
}

/// Wrapper: strTrimEnd(RocStr, UpdateMode, *RocOps) -> RocStr
fn wrapStrTrimEnd(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: builtins.utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strTrimEnd(s, update_mode, roc_ops);
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

/// Wrapper: reserveC(RocStr, u64, UpdateMode, *RocOps) -> RocStr
fn wrapStrReserve(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, spare: u64, update_mode: builtins.utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strReserveC(s, spare, update_mode, roc_ops);
}

/// Wrapper: strReleaseExcessCapacity(RocStr, UpdateMode, *RocOps) -> RocStr
fn wrapStrReleaseExcessCapacity(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: builtins.utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strReleaseExcessCapacity(s, update_mode, roc_ops);
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

/// Wrapper: strWithAsciiLowercased(RocStr, UpdateMode, *RocOps) -> RocStr
fn wrapStrWithAsciiLowercased(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: builtins.utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strWithAsciiLowercased(s, update_mode, roc_ops);
}

/// Wrapper: strWithAsciiUppercased(RocStr, UpdateMode, *RocOps) -> RocStr
fn wrapStrWithAsciiUppercased(out: *RocStr, str_bytes: ?[*]u8, str_len: usize, str_cap: usize, update_mode: builtins.utils.UpdateMode, roc_ops: *RocOps) callconv(.c) void {
    const s = RocStr{ .bytes = str_bytes, .length = str_len, .capacity_or_alloc_ptr = str_cap };
    out.* = strWithAsciiUppercased(s, update_mode, roc_ops);
}

/// Wrapper: fromUtf8Lossy(RocList, *RocOps) -> RocStr
fn wrapStrFromUtf8Lossy(out: *RocStr, list_bytes: ?[*]u8, list_len: usize, list_cap: usize, roc_ops: *RocOps) callconv(.c) void {
    const list = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    out.* = strFromUtf8Lossy(list, roc_ops);
}

const LIR = lir.LIR;
const LirProcSpec = lir.LirProcSpec;

const Allocator = std.mem.Allocator;

/// Code generator for statement-only LIR procs
/// Parameterized by RocTarget for cross-compilation support
pub fn LirCodeGen(comptime target: RocTarget) type {
    // Validate target architecture is supported
    const arch = target.toCpuArch();
    if (arch != .x86_64 and arch != .aarch64 and arch != .aarch64_be) {
        @compileError("LirCodeGen requires x86_64 or aarch64 target");
    }

    // ── Target-specific size constants ──
    // These are derived from the target pointer size for the 64-bit architectures we support.
    // RocStr and RocList are both three target words. RocStr stores bytes, encoded capacity, then length.

    // Size of a pointer on the target architecture (8 bytes for x86_64/aarch64)
    const target_ptr_size: u32 = 8;

    // Size of a RocStr struct: ptr + encoded capacity + length = 3 × pointer size (24 bytes on 64-bit)
    const roc_str_size: u32 = 3 * target_ptr_size;
    const small_str_max_len: u32 = roc_str_size - 1;

    // Size of a RocList struct: ptr + length + capacity = 3 × pointer size (24 bytes on 64-bit)
    const roc_list_size: u32 = 3 * target_ptr_size;

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

        /// AArch64 callee-saved register reserved for the caller's stack-argument
        /// base in internal procs that actually receive stack arguments.
        const caller_stack_arg_base_reg: GeneralReg = if (arch == .x86_64) frame_ptr else .X28;

        /// Return value registers (first, second, third)
        const ret_reg_0: GeneralReg = if (arch == .x86_64) .RAX else .X0;
        const ret_reg_1: GeneralReg = if (arch == .x86_64) .RDX else .X1;
        const ret_reg_2: GeneralReg = if (arch == .x86_64) .RCX else .X2;

        /// Emit/calling-convention types for this architecture.
        const EmitType = @TypeOf(@as(CodeGen, undefined).emit);
        const Builder = CallingConventionMod.CallBuilder(EmitType);
        const abi_shadow_space: i32 = @intCast(EmitType.CC.SHADOW_SPACE);
        const incoming_stack_arg_base_offset: i32 = if (arch == .x86_64) 16 + abi_shadow_space else 0;
        const outgoing_stack_arg_base_offset: i32 = abi_shadow_space;
        const max_arg_regs: u8 = @intCast(EmitType.CC.PARAM_REGS.len);
        const pass_by_ptr_arg_regs: u8 = @intCast(EmitType.CC.PARAM_REGS.len + 1);

        allocator: Allocator,

        /// Calling convention for the target platform (derived from comptime target)
        cc: CallingConvention,

        /// Architecture-specific code generator with register allocation
        codegen: CodeGen,

        /// The LIR store containing expressions to compile
        store: *const LirStore,

        /// Layout store for accessing struct/tag field offsets
        layout_store: *const LayoutStore,

        /// Readonly data symbols for non-SSO strings in object-file output.
        static_strings: []const StaticStringData.Entry,

        /// Map from LIR local id to value location (register or stack slot)
        local_locations: std.AutoHashMap(u32, ValueLocation),

        /// Current proc argument span, used only for debug invariant reporting.
        current_proc_args: ?LocalSpan = null,

        /// Map from JoinPointId to code offset (for recursive closure jumps)
        join_points: std.AutoHashMap(u32, usize),

        /// Map from CFStmtId to generated code offset.
        /// Dev codegen must treat LIR control flow as a graph, not a tree:
        /// shared continuations are emitted once and later paths jump to the
        /// already-generated block instead of regenerating it.
        stmt_locations: std.AutoHashMap(u32, usize),

        /// Source line entries in emission order (code offset paired with
        /// the statement's source location), consumed by DWARF line-table
        /// emission for object files.
        line_entries: std.ArrayList(LineEntry),

        /// Registry of compiled procedures (proc-spec id -> CompiledProc)
        /// Used to find call targets during second pass
        proc_registry: std.AutoHashMap(u32, CompiledProc),

        /// Registry of compiled RC helpers keyed by canonical RC helper identity.
        compiled_rc_helpers: std.AutoHashMap(u64, usize),

        /// Worklist of RC helpers awaiting compilation. RC helpers are compiled
        /// iteratively from this worklist (never via recursion): a helper body
        /// references its child helpers through pending refs and schedules them
        /// here, so deeply nested layouts never grow the native call stack.
        rc_helper_worklist: std.ArrayList(RcHelperVariant),
        /// RC helper keys already scheduled on the current worklist drain.
        rc_helper_scheduled: std.AutoHashMap(u64, void),
        /// Pending RC-helper address literals (ADR/LEA) awaiting offset resolution.
        pending_rc_addrs: std.ArrayList(PendingRcRef),
        /// Pending RC-helper calls (BL/CALL) awaiting offset resolution.
        pending_rc_calls: std.ArrayList(PendingRcRef),
        /// True while draining the RC-helper worklist, so nested requests only
        /// schedule rather than starting a second drain.
        compiling_rc_helpers: bool,

        /// Pending calls that need to be patched after all procedures are compiled
        pending_calls: std.ArrayList(PendingCall),

        /// Pending proc-address literals that need to be patched after all procedures are compiled.
        pending_proc_addrs: std.ArrayList(PendingProcAddr),

        /// Map from JoinPointId to list of jumps that target it (for patching)
        join_point_jumps: std.AutoHashMap(u32, std.ArrayList(JumpRecord)),

        /// Map from JoinPointId to parameters (for rebinding to correct stack slots)
        join_point_params: std.AutoHashMap(u32, LocalSpan),

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

        /// Stack of active loop continue targets.
        /// `loop_continue` lowers by jumping to the innermost active loop header.
        loop_continue_targets: std.ArrayList(usize),

        /// Stack markers into `loop_break_patches` for active loop exits.
        loop_break_patch_starts: std.ArrayList(usize),

        /// Jump patches emitted by `loop_break` that must target the active loop exit.
        loop_break_patches: std.ArrayList(usize),

        /// Stack slot where early return value is stored during deferred-prologue proc compilation.
        early_return_result_slot: ?i32 = null,

        /// Layout for early return result (to know how to move to return register)
        early_return_ret_layout: ?layout.Idx = null,

        /// Register where RocOps pointer is saved (for calling builtins that need it)
        roc_ops_reg: ?GeneralReg = null,

        /// Proc currently being compiled, for debug-time invariant reporting.
        current_proc_name: ?Symbol = null,

        /// Statement currently being generated, for debug-time invariant reporting.
        current_stmt_id: ?CFStmtId = null,

        /// Stack slot where the hidden return pointer is saved (for return-by-pointer
        /// convention used when the return type exceeds the register limit).
        /// Set during deferred-prologue proc compilation, used by moveToReturnRegisterWithLayout
        /// and generateEarlyReturn.
        ret_ptr_slot: ?i32 = null,

        /// The current AArch64 proc reads incoming stack arguments. Its prologue
        /// must initialize `caller_stack_arg_base_reg` to the callee-entry SP.
        uses_caller_stack_arg_base: bool = false,

        /// Per-proc shared stack slot for debug-assertion crash messages.
        /// Debug asserts each emit a unique msg, but only one fires at runtime
        /// (the program crashes after the first). Sharing one slot across all
        /// debug crash sites in a proc keeps the frame from growing linearly
        /// with the number of debug asserts. Lazily allocated on first use.
        proc_debug_msg_slot: ?i32 = null,

        /// Counter for unique temporary local IDs.
        /// Starts at 0x8000_0000 to avoid collision with real local variables.
        /// Used by allocTempGeneral() for temporaries that don't correspond to real locals.
        next_temp_local: u32 = 0x8000_0000,

        /// Generation mode determines whether to use direct function pointers or symbol references.
        /// - native_execution: Code runs in-process (dev evaluator), direct function pointers work
        /// - object_file: Generating relocatable object files, use symbol references for builtins
        generation_mode: GenerationMode = .native_execution,

        /// Whether shim-executed host-callable functions and boxed callbacks
        /// participate in the hot-reload code reference protocol.
        enable_hot_reload: bool = false,

        /// Whether object-file entrypoints should use the synthetic default
        /// platform runtime contract.
        enable_default_platform_runtime: bool = false,

        /// Compiler-internal hooks enabled only for native compile-time
        /// evaluation. Normal dev backend output leaves these null.
        comptime_hooks: ?ComptimeHooks = null,

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
            /// Declared arguments for ABI-correct call lowering.
            args: LocalSpan,
            /// Size of the emitted prologue, used for object unwind metadata.
            prologue_size: u32 = 0,
            /// Stack allocation size recorded in object unwind metadata.
            stack_alloc: u32 = 0,
            /// Full AArch64 frame size recorded in object unwind metadata.
            frame_size: u32 = 0,
            /// AArch64 callee-saved register mask for object unwind metadata.
            callee_saved_mask: u32 = 0,
            /// Offset of the shared epilogue from this procedure's code start.
            epilogue_offset: u32 = 0,
            /// Whether this procedure uses the platform frame pointer.
            uses_frame_pointer: bool = true,
        };

        /// Object-file symbol metadata for one compiled LIR procedure.
        pub const CompiledProcSymbol = struct {
            name: Symbol,
            code_start: usize,
            code_end: usize,
            prologue_size: u32,
            stack_alloc: u32,
            frame_size: u32,
            callee_saved_mask: u32,
            epilogue_offset: u32,
            uses_frame_pointer: bool,
        };

        const unresolved_proc_code_start = std.math.maxInt(usize);

        /// A pending call that needs to be patched after all procedures are compiled.
        pub const PendingCall = struct {
            /// Offset where the call instruction is (needs patching)
            call_site: usize,
            /// The proc being called
            target_proc: lir.LIR.LirProcSpecId,
        };

        /// A pending ADR/LEA proc-address literal that needs to be patched once the target proc is compiled.
        pub const PendingProcAddr = struct {
            /// Offset where the ADR/LEA instruction starts
            instr_offset: usize,
            /// The proc whose address should be materialized
            target_proc: lir.LIR.LirProcSpecId,
        };

        /// A pending RC-helper reference (call or address literal) whose target
        /// offset is resolved once the worklist drain has compiled every helper.
        pub const PendingRcRef = struct {
            /// Offset of the BL/CALL or ADR/LEA instruction (needs patching).
            instr_offset: usize,
            /// Encoded RC helper key whose compiled offset this reference targets.
            target_key: u64,
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
            /// Value is on the stack at given offset from frame pointer.
            /// `layout_idx` preserves the semantic interpretation for narrow integer loads.
            stack: struct {
                offset: i32,
                size: ValueSize = .qword,
                layout_idx: layout.Idx = .u64,
            },
            /// 128-bit value on the stack (16 bytes: low at offset, high at offset+8)
            stack_i128: i32,
            /// 24-byte string value on the stack (for RocStr: ptr/data, encoded capacity, length)
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
            prologue_size: u32 = 0,
            /// Stack allocation size (for unwind info)
            stack_alloc: u32 = 0,
            /// Full AArch64 frame size (for unwind info)
            frame_size: u32 = 0,
            /// AArch64 callee-saved register mask (for unwind info)
            callee_saved_mask: u32 = 0,
            /// Offset of the shared epilogue from the symbol start (for unwind info)
            epilogue_offset: u32 = 0,
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
            store: *const LirStore,
            layout_store_opt: *const LayoutStore,
            static_strings: []const StaticStringData.Entry,
        ) Allocator.Error!Self {
            return .{
                .allocator = allocator,
                .cc = CallingConvention.forTarget(target),
                .codegen = CodeGen.init(allocator),
                .store = store,
                .layout_store = layout_store_opt,
                .static_strings = static_strings,
                .local_locations = std.AutoHashMap(u32, ValueLocation).init(allocator),
                .join_points = std.AutoHashMap(u32, usize).init(allocator),
                .stmt_locations = std.AutoHashMap(u32, usize).init(allocator),
                .line_entries = .empty,
                .proc_registry = std.AutoHashMap(u32, CompiledProc).init(allocator),
                .compiled_rc_helpers = std.AutoHashMap(u64, usize).init(allocator),
                .rc_helper_worklist = std.ArrayList(RcHelperVariant).empty,
                .rc_helper_scheduled = std.AutoHashMap(u64, void).init(allocator),
                .pending_rc_addrs = std.ArrayList(PendingRcRef).empty,
                .pending_rc_calls = std.ArrayList(PendingRcRef).empty,
                .compiling_rc_helpers = false,
                .pending_calls = std.ArrayList(PendingCall).empty,
                .pending_proc_addrs = std.ArrayList(PendingProcAddr).empty,
                .join_point_jumps = std.AutoHashMap(u32, std.ArrayList(JumpRecord)).init(allocator),
                .join_point_params = std.AutoHashMap(u32, LocalSpan).init(allocator),
                .internal_call_patches = std.ArrayList(InternalCallPatch).empty,
                .internal_addr_patches = std.ArrayList(InternalAddrPatch).empty,
                .early_return_patches = std.ArrayList(usize).empty,
                .loop_continue_targets = std.ArrayList(usize).empty,
                .loop_break_patch_starts = std.ArrayList(usize).empty,
                .loop_break_patches = std.ArrayList(usize).empty,
                .scratch_arg_locs = try base.Scratch(ValueLocation).init(allocator),
                .scratch_arg_infos = try base.Scratch(ArgInfo).init(allocator),
                .scratch_pass_by_ptr = try base.Scratch(bool).init(allocator),
                .scratch_param_num_regs = try base.Scratch(u8).init(allocator),
            };
        }

        pub fn setComptimeHooks(self: *Self, hooks: ?ComptimeHooks) void {
            self.comptime_hooks = hooks;
        }

        /// Clean up resources
        pub fn deinit(self: *Self) void {
            self.codegen.deinit();
            self.local_locations.deinit();
            self.join_points.deinit();
            self.stmt_locations.deinit();
            self.line_entries.deinit(self.allocator);
            self.proc_registry.deinit();
            self.compiled_rc_helpers.deinit();
            self.rc_helper_worklist.deinit(self.allocator);
            self.rc_helper_scheduled.deinit();
            self.pending_rc_addrs.deinit(self.allocator);
            self.pending_rc_calls.deinit(self.allocator);
            self.pending_calls.deinit(self.allocator);
            self.pending_proc_addrs.deinit(self.allocator);
            // Clean up the nested ArrayLists in join_point_jumps
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.deinit(self.allocator);
            }
            self.join_point_jumps.deinit();
            self.join_point_params.deinit();
            self.internal_call_patches.deinit(self.allocator);
            self.internal_addr_patches.deinit(self.allocator);
            self.early_return_patches.deinit(self.allocator);
            self.loop_continue_targets.deinit(self.allocator);
            self.loop_break_patch_starts.deinit(self.allocator);
            self.loop_break_patches.deinit(self.allocator);
            self.scratch_arg_locs.deinit();
            self.scratch_arg_infos.deinit();
            self.scratch_pass_by_ptr.deinit();
            self.scratch_param_num_regs.deinit();
        }

        /// Reset the code generator for generating a new expression
        pub fn reset(self: *Self) void {
            self.codegen.reset();
            self.local_locations.clearRetainingCapacity();
            self.join_points.clearRetainingCapacity();
            self.stmt_locations.clearRetainingCapacity();
            self.proc_registry.clearRetainingCapacity();
            self.compiled_rc_helpers.clearRetainingCapacity();
            self.rc_helper_worklist.clearRetainingCapacity();
            self.rc_helper_scheduled.clearRetainingCapacity();
            self.pending_rc_addrs.clearRetainingCapacity();
            self.pending_rc_calls.clearRetainingCapacity();
            self.compiling_rc_helpers = false;
            self.pending_calls.clearRetainingCapacity();
            self.pending_proc_addrs.clearRetainingCapacity();
            // Clear nested ArrayLists
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.clearRetainingCapacity();
            }
            self.join_point_jumps.clearRetainingCapacity();
            self.join_point_params.clearRetainingCapacity();
            self.internal_call_patches.clearRetainingCapacity();
            self.internal_addr_patches.clearRetainingCapacity();
            self.early_return_patches.clearRetainingCapacity();
            self.loop_continue_targets.clearRetainingCapacity();
            self.loop_break_patch_starts.clearRetainingCapacity();
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

        const StmtEnvSnapshot = struct {
            local_locations: std.AutoHashMap(u32, ValueLocation),

            fn deinit(self: *StmtEnvSnapshot) void {
                self.local_locations.deinit();
            }
        };

        fn captureStmtEnv(self: *Self) Allocator.Error!StmtEnvSnapshot {
            return .{
                .local_locations = try self.local_locations.clone(),
            };
        }

        fn restoreStmtEnv(self: *Self, snapshot: *const StmtEnvSnapshot) Allocator.Error!void {
            self.local_locations.deinit();
            self.local_locations = try snapshot.local_locations.clone();
        }

        fn clearFunctionControlFlowState(self: *Self) void {
            self.join_points.clearRetainingCapacity();
            self.stmt_locations.clearRetainingCapacity();
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.clearRetainingCapacity();
            }
            self.join_point_jumps.clearRetainingCapacity();
            self.join_point_params.clearRetainingCapacity();
            self.loop_continue_targets.clearRetainingCapacity();
            self.loop_break_patch_starts.clearRetainingCapacity();
            self.loop_break_patches.clearRetainingCapacity();
        }

        /// Generate code for a compiled root proc.
        ///
        /// The generated code follows the calling convention:
        /// - First arg (RDI/X0) contains the pointer to the result buffer
        /// - Second arg (RSI/X1) contains the pointer to RocOps
        /// - The function writes the result to the result buffer and returns
        ///
        /// For tuples, pass tuple_len > 1 to copy all elements to the result buffer.
        pub fn generateCode(
            self: *Self,
            root_proc_id: lir.LIR.LirProcSpecId,
            result_layout: layout.Idx,
            tuple_len: usize,
        ) Allocator.Error!CodeResult {
            // Clear any leftover state from compileAllProcSpecs
            self.local_locations.clearRetainingCapacity();
            self.proc_debug_msg_slot = null;
            self.codegen.callee_saved_used = 0;
            self.uses_caller_stack_arg_base = false;

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

            const root_proc = try self.compiledProcForId(root_proc_id);
            const final_result = try self.generateCallToCompiledProc(root_proc, &.{}, &.{}, result_layout);
            const actual_ret_layout = result_layout;

            // If the root never returns, the trap path has already been emitted and
            // there is no value to store.
            if (final_result != .noreturn) {
                const ret_size = self.getLayoutSize(actual_ret_layout);
                if (ret_size > 0) {
                    try self.storeResultToSavedPtr(final_result, actual_ret_layout, result_ptr_save_reg, tuple_len);
                }
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
            self.shiftPendingProcAddrs(body_start, body_end, prologue_size);
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

        fn localMetadata(self: *Self, local: LocalId) lir.Local {
            return self.store.getLocal(local);
        }

        fn localLayout(self: *Self, local: LocalId) layout.Idx {
            return self.localMetadata(local).layout_idx;
        }

        fn localKey(local: LocalId) u32 {
            return @intFromEnum(local);
        }

        fn valueLayout(self: *Self, local: LocalId) layout.Idx {
            return self.localLayout(local);
        }

        /// Generate code for a local reference. The result is ALWAYS in a stable location.
        fn emitValueLocal(self: *Self, local: LocalId) Allocator.Error!ValueLocation {
            const loc = try self.emitValueLocalRaw(local);
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

        /// Generate code for a local reference (raw — may return bare register locations).
        fn emitValueLocalRaw(self: *Self, local: LocalId) Allocator.Error!ValueLocation {
            return try self.generateLookup(local);
        }

        /// Generate code for low-level operations
        fn generateLowLevel(self: *Self, ll: anytype) Allocator.Error!ValueLocation {
            const args = self.store.getLocalSpan(ll.args);

            switch (ll.op) {
                .list_len => {
                    // List is a (ptr, len, capacity) triple - length is at offset 8
                    std.debug.assert(args.len >= 1);
                    const list_loc = try self.emitValueLocal(args[0]);

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
                    const capacity_loc = try self.emitValueLocal(args[0]);

                    // Get element layout from return type (which is List(elem))
                    const ls = self.layout_store;
                    const list_abi = builtinInternalListAbi(ls, "dev.list_with_capacity.builtin_list_abi", ll.ret_layout);

                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_with_capacity);

                    // Allocate stack space for result (RocList = 24 bytes)
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);

                    const cap_reg = try self.ensureInGeneralReg(capacity_loc);
                    const base_reg = frame_ptr;

                    // roc_builtins_list_with_capacity(out, capacity, alignment, element_width, elements_refcounted, roc_ops)
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addRegArg(cap_reg);
                    self.codegen.freeGeneral(cap_reg);
                    try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                    try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                    try builder.addImmArg(if (list_abi.elements_refcounted) 1 else 0);
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
                    if (args.len != 2) {
                        unreachable;
                    }
                    const ls = self.layout_store;

                    const roc_ops_reg = self.roc_ops_reg orelse {
                        unreachable;
                    };

                    // Generate list argument (must be on stack - 24 bytes)
                    const list_loc = try self.emitValueLocal(args[0]);

                    // Generate element value
                    const elem_loc = try self.emitValueLocal(args[1]);
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    if (builtin.mode == .Debug and ret_layout_val.tag != .list and ret_layout_val.tag != .list_of_zst) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: list_append ret_layout must be list/list_of_zst, got {s}",
                            .{@tagName(ret_layout_val.tag)},
                        );
                    }
                    if (builtin.mode == .Debug) {
                        const list_layout_idx = self.valueLayout(args[0]);
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
                        .list => ls.layoutSizeAlign(ls.getLayout(ret_layout_val.getIdx())),
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
                    const elem_offset: i32 = if (is_zst)
                        self.codegen.allocStackSlot(1)
                    else
                        try self.ensureOnStack(elem_loc, elem_size_align.size);

                    // Allocate result slot (24 bytes for RocList)
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);

                    const base_reg = frame_ptr;
                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_append_unsafe);

                    // roc_builtins_list_append_unsafe(out, list_bytes, list_len, list_cap, element, element_width, roc_ops)
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addMemArg(base_reg, list_offset);
                    try builder.addMemArg(base_reg, list_offset + 8);
                    try builder.addMemArg(base_reg, list_offset + 16);
                    try builder.addLeaArg(base_reg, elem_offset);
                    try builder.addImmArg(if (is_zst) 0 else @as(i64, @intCast(elem_size_align.size)));
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, fn_addr, .list_append_unsafe);

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
                    const list_loc = try self.emitValueLocal(args[0]);
                    const index_loc = try self.emitValueLocal(args[1]);

                    // Get base offset of list struct
                    const list_base: i32 = switch (list_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        else => unreachable,
                    };

                    const ls = self.layout_store;
                    const list_layout_idx = self.valueLayout(args[0]);
                    const list_layout_val = ls.getLayout(list_layout_idx);
                    const list_elem_layout: layout.Idx = switch (list_layout_val.tag) {
                        .list => list_layout_val.getIdx(),
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

                    if (builtin.mode == .Debug and ll.ret_layout != list_elem_layout) {
                        const ret_layout_val = ls.getLayout(ll.ret_layout);
                        const elem_layout_val = ls.getLayout(list_elem_layout);
                        const ret_list_like = ret_layout_val.tag == .list or ret_layout_val.tag == .list_of_zst;
                        const elem_list_like = elem_layout_val.tag == .list or elem_layout_val.tag == .list_of_zst;
                        const ret_box_like = ret_layout_val.tag == .box or ret_layout_val.tag == .box_of_zst;
                        const elem_box_like = elem_layout_val.tag == .box or elem_layout_val.tag == .box_of_zst;

                        if (!(ret_list_like and elem_list_like) and !(ret_box_like and elem_box_like)) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: list_get_unsafe ret/elem layout mismatch (ret={d} {s}, elem={d} {s})",
                                .{ @intFromEnum(ll.ret_layout), @tagName(ret_layout_val.tag), @intFromEnum(list_elem_layout), @tagName(elem_layout_val.tag) },
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
                .list_map_can_reuse => {
                    // list_map_can_reuse(list, transform) -> U8; only the list is inspected.
                    std.debug.assert(args.len == 2);
                    // On a width where the element layouts are not
                    // interchangeable, the in-place branch is statically dead;
                    // resolve to a constant 0 so the runtime check never runs.
                    if (!ll.interchangeable.get(self.layout_store.targetUsize())) {
                        return .{ .immediate_i64 = 0 };
                    }
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);

                    // roc_builtins_list_map_can_reuse(bytes, len, cap, roc_ops) -> u8
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addMemArg(frame_ptr, list_off);
                    try builder.addMemArg(frame_ptr, list_off + 8);
                    try builder.addMemArg(frame_ptr, list_off + 16);
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_list_map_can_reuse), .list_map_can_reuse);

                    const result_reg = try self.allocTempGeneral();
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.movRegReg(.w64, result_reg, .X0);
                    } else {
                        try self.codegen.emit.movRegReg(.w64, result_reg, .RAX);
                        // x86_64 ABI: only the low byte is guaranteed valid for
                        // bool-like returns; mask so 64-bit compares work.
                        try self.codegen.emit.andRegImm8(result_reg, 1);
                    }
                    return .{ .general_reg = result_reg };
                },
                .list_map_cast_unsafe => {
                    // Same bits, new element type: copy the list struct through.
                    std.debug.assert(args.len == 1);
                    const list_loc = try self.emitValueLocal(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const tmp = try self.allocTempGeneral();
                    var off: i32 = 0;
                    while (off < roc_list_size) : (off += 8) {
                        try self.emitLoad(.w64, tmp, frame_ptr, list_off + off);
                        try self.emitStore(.w64, frame_ptr, result_offset + off, tmp);
                    }
                    self.codegen.freeGeneral(tmp);
                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_map_extract_unsafe => {
                    // list_map_extract_unsafe(list, index) -> element of the input
                    // type. The list local already carries the output element type,
                    // so the result layout supplies both the copy size and the
                    // stride. This op only executes on a width where the input and
                    // output element layouts are interchangeable (one stride); on a
                    // width where they are not, `list_map_can_reuse` resolves to a
                    // constant 0 and this op sits in a statically-dead branch.
                    std.debug.assert(args.len == 2);
                    const list_loc = try self.emitValueLocal(args[0]);
                    const index_loc = try self.emitValueLocal(args[1]);

                    const list_base: i32 = switch (list_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        else => unreachable,
                    };

                    const ls = self.layout_store;
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    const elem_size: u32 = ls.layoutSizeAlign(ret_layout_val).size;
                    if (elem_size == 0) {
                        return .{ .immediate_i64 = 0 };
                    }

                    const index_reg = try self.ensureInGeneralReg(index_loc);
                    const ptr_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, ptr_reg, list_base);

                    const addr_reg = try self.allocTempGeneral();
                    try self.codegen.emit.movRegReg(.w64, addr_reg, index_reg);
                    self.codegen.freeGeneral(index_reg);
                    if (elem_size != 1) {
                        const size_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(size_reg, elem_size);
                        try self.emitMulRegs(.w64, addr_reg, addr_reg, size_reg);
                        self.codegen.freeGeneral(size_reg);
                    }
                    try self.emitAddRegs(.w64, addr_reg, addr_reg, ptr_reg);
                    self.codegen.freeGeneral(ptr_reg);

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

                    var result_loc: ValueLocation = if (ll.ret_layout == .i128 or ll.ret_layout == .u128 or ll.ret_layout == .dec)
                        .{ .stack_i128 = elem_slot }
                    else if (ll.ret_layout == .str)
                        .{ .stack_str = elem_slot }
                    else if (ret_layout_val.tag == .list or ret_layout_val.tag == .list_of_zst)
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
                .list_map_write_unsafe => {
                    // list_map_write_unsafe(list, index, element) -> the same list,
                    // with the owned element's bytes stored into the vacated slot.
                    std.debug.assert(args.len == 3);
                    const list_loc = try self.emitValueLocal(args[0]);
                    const index_loc = try self.emitValueLocal(args[1]);
                    const elem_loc = try self.emitValueLocal(args[2]);

                    const ls = self.layout_store;
                    const elem_size: u32 = ls.layoutSizeAlign(ls.getLayout(self.valueLayout(args[2]))).size;

                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    if (elem_size == 0) {
                        const zst_result_offset = self.codegen.allocStackSlot(roc_str_size);
                        const zst_tmp = try self.allocTempGeneral();
                        var zst_off: i32 = 0;
                        while (zst_off < roc_list_size) : (zst_off += 8) {
                            try self.emitLoad(.w64, zst_tmp, frame_ptr, list_off + zst_off);
                            try self.emitStore(.w64, frame_ptr, zst_result_offset + zst_off, zst_tmp);
                        }
                        self.codegen.freeGeneral(zst_tmp);
                        return .{ .list_stack = .{ .struct_offset = zst_result_offset, .data_offset = 0, .num_elements = 0 } };
                    }
                    const elem_off = try self.ensureOnStack(elem_loc, elem_size);

                    const index_reg = try self.ensureInGeneralReg(index_loc);
                    const addr_reg = try self.allocTempGeneral();
                    try self.codegen.emit.movRegReg(.w64, addr_reg, index_reg);
                    self.codegen.freeGeneral(index_reg);
                    if (elem_size != 1) {
                        const size_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(size_reg, elem_size);
                        try self.emitMulRegs(.w64, addr_reg, addr_reg, size_reg);
                        self.codegen.freeGeneral(size_reg);
                    }
                    const ptr_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, ptr_reg, list_off);
                    try self.emitAddRegs(.w64, addr_reg, addr_reg, ptr_reg);
                    self.codegen.freeGeneral(ptr_reg);

                    const temp_reg = try self.allocTempGeneral();
                    if (elem_size <= 8) {
                        const vs = ValueSize.fromByteCount(@intCast(elem_size));
                        try self.emitSizedLoadMem(temp_reg, frame_ptr, elem_off, vs);
                        try self.emitSizedStoreMem(addr_reg, 0, temp_reg, vs);
                    } else {
                        try self.copyChunked(temp_reg, frame_ptr, elem_off, addr_reg, 0, elem_size);
                    }
                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(addr_reg);

                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const tmp = try self.allocTempGeneral();
                    var off: i32 = 0;
                    while (off < roc_list_size) : (off += 8) {
                        try self.emitLoad(.w64, tmp, frame_ptr, list_off + off);
                        try self.emitStore(.w64, frame_ptr, result_offset + off, tmp);
                    }
                    self.codegen.freeGeneral(tmp);
                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_concat => {
                    // list_concat(list_a, list_b) -> List
                    if (args.len != 2) unreachable;
                    const list_a_loc = try self.emitValueLocal(args[0]);
                    const list_b_loc = try self.emitValueLocal(args[1]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const list_abi = builtinInternalListAbi(ls, "dev.list_concat.builtin_list_abi", ll.ret_layout);

                    const list_a_off = try self.ensureOnStack(list_a_loc, roc_list_size);
                    const list_b_off = try self.ensureOnStack(list_b_loc, roc_list_size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_concat);
                    const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
                    defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
                    const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
                    defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

                    {
                        // wrapListConcat(out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, alignment, element_width, elements_refcounted, element_incref, element_decref, update_modes, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_a_off);
                        try builder.addMemArg(base_reg, list_a_off + 8);
                        try builder.addMemArg(base_reg, list_a_off + 16);
                        try builder.addMemArg(base_reg, list_b_off);
                        try builder.addMemArg(base_reg, list_b_off + 8);
                        try builder.addMemArg(base_reg, list_b_off + 16);
                        try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                        try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                        try builder.addImmArg(if (list_abi.elements_refcounted) @as(usize, 1) else 0);
                        if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        try builder.addImmArg(@intCast(ll.unique_args & 0b11));
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_concat);
                    }

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_prepend => {
                    // list_prepend(list, element) -> List
                    if (args.len != 2) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const elem_loc = try self.emitValueLocal(args[1]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const list_abi = builtinInternalListAbi(ls, "dev.list_prepend.builtin_list_abi", ll.ret_layout);

                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const elem_off = try self.ensureOnStack(elem_loc, list_abi.elem_size_align.size);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_prepend);
                    const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
                    defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
                    const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
                    defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

                    {
                        // wrapListPrepend(out, list_bytes, list_len, list_cap, alignment, element, element_width, elements_refcounted, element_incref, element_decref, update_mode, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_off);
                        try builder.addMemArg(base_reg, list_off + 8);
                        try builder.addMemArg(base_reg, list_off + 16);
                        try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                        try builder.addLeaArg(base_reg, elem_off);
                        try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                        try builder.addImmArg(if (list_abi.elements_refcounted) @as(usize, 1) else 0);
                        if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_prepend);
                    }

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_drop_first => {
                    // list_drop_first(list, n) -> List  (sublist from index n to end)
                    if (args.len != 2) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const n_loc = try self.emitValueLocal(args[1]);
                    return try self.callListSublist(ll, list_loc, n_loc, .drop_first);
                },
                .list_drop_last => {
                    // list_drop_last(list, n) -> List  (sublist from 0 with len - n)
                    if (args.len != 2) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const n_loc = try self.emitValueLocal(args[1]);
                    return try self.callListSublist(ll, list_loc, n_loc, .drop_last);
                },
                .list_take_first => {
                    // list_take_first(list, n) -> List  (sublist from 0 with n elements)
                    if (args.len != 2) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const n_loc = try self.emitValueLocal(args[1]);
                    return try self.callListSublist(ll, list_loc, n_loc, .take_first);
                },
                .list_take_last => {
                    // list_take_last(list, n) -> List  (sublist from len - n to end)
                    if (args.len != 2) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const n_loc = try self.emitValueLocal(args[1]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
                    return .{ .float_reg = try self.ensureInFloatReg(src_loc) };
                },

                // ── Float bit reinterpretations ──
                .f32_to_bits => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const reg = try self.materializeF32BitsInGeneralReg(src_loc);
                    return .{ .general_reg = reg };
                },
                .f32_from_bits => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const bits_reg = try self.ensureInGeneralReg(src_loc);
                    const stack_offset = self.codegen.allocStackSlot(4);
                    try self.codegen.emitStoreStack(.w32, stack_offset, bits_reg);
                    self.codegen.freeGeneral(bits_reg);
                    return .{ .stack = .{ .offset = stack_offset, .size = .dword, .layout_idx = .f32 } };
                },
                .f64_to_bits => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const reg = try self.ensureInGeneralReg(src_loc);
                    return .{ .general_reg = reg };
                },
                .f64_from_bits => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const bits_reg = try self.ensureInGeneralReg(src_loc);
                    const stack_offset = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStack(.w64, stack_offset, bits_reg);
                    self.codegen.freeGeneral(bits_reg);
                    return .{ .stack = .{ .offset = stack_offset, .size = .qword, .layout_idx = .f64 } };
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
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
                    const src_loc = try self.emitValueLocal(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128
                    return try self.callI128PartsToF64(parts, @intFromPtr(&dev_wrappers.roc_builtins_dec_to_f64), .dec_to_f64);
                },
                .dec_to_f32_wrap => {
                    // Dec to f32: convert to f64 first (f32 narrowing happens at store)
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed); // Dec is signed i128
                    return try self.callI128PartsToF64(parts, @intFromPtr(&dev_wrappers.roc_builtins_dec_to_f64), .dec_to_f64);
                },

                // ── 128-bit integer to float conversions ──
                .i128_to_f32,
                .i128_to_f64,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const parts = try self.getI128Parts(src_loc, .signed);
                    return try self.callI128PartsToF64(parts, @intFromPtr(&dev_wrappers.roc_builtins_i128_to_f64), .i128_to_f64);
                },
                .u128_to_f32,
                .u128_to_f64,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const parts = try self.getI128Parts(src_loc, .unsigned);
                    return try self.callI128PartsToF64(parts, @intFromPtr(&dev_wrappers.roc_builtins_u128_to_f64), .u128_to_f64);
                },

                // ── Float to 128-bit integer truncating conversions ──
                .f32_to_i128_trunc,
                .f64_to_i128_trunc,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const freg = try self.ensureInFloatReg(src_loc);
                    return try self.callF64ToI128(freg, @intFromPtr(&dev_wrappers.roc_builtins_f64_to_i128_trunc), .f64_to_i128_trunc);
                },
                .f32_to_u128_trunc,
                .f64_to_u128_trunc,
                => {
                    if (args.len < 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    const freg = try self.ensureInFloatReg(src_loc);
                    return try self.callF64ToI128(freg, @intFromPtr(&dev_wrappers.roc_builtins_f64_to_u128_trunc), .f64_to_u128_trunc);
                },

                // ── String low-level operations ──

                .str_to_utf8 => {
                    // str_to_utf8(str) -> List(U8)
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrToUtf8), .str_to_utf8, .list, null);
                },
                .str_is_eq => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    const eq_loc = try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrEqual), .str_equal);
                    const eq_reg = try self.ensureInGeneralReg(eq_loc);
                    return .{ .general_reg = eq_reg };
                },
                .str_is_eq_static_small => {
                    if (args.len != 5) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const len_loc = try self.emitValueLocal(args[1]);
                    const word0_loc = try self.emitValueLocal(args[2]);
                    const word1_loc = try self.emitValueLocal(args[3]);
                    const word2_loc = try self.emitValueLocal(args[4]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    const len_off = try self.ensureOnStack(len_loc, 8);
                    const word0_off = try self.ensureOnStack(word0_loc, 8);
                    const word1_off = try self.ensureOnStack(word1_loc, 8);
                    const word2_off = try self.ensureOnStack(word2_loc, 8);
                    const eq_loc = try self.callStrStaticSmallToScalar(str_off, len_off, word0_off, word1_off, word2_off, @intFromPtr(&wrapStrEqualStaticSmall), .str_equal_static_small);
                    const eq_reg = try self.ensureInGeneralReg(eq_loc);
                    return .{ .general_reg = eq_reg };
                },
                .str_static_small_word_eq => {
                    if (args.len != 4) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const offset_loc = try self.emitValueLocal(args[1]);
                    const active_len_loc = try self.emitValueLocal(args[2]);
                    const word_loc = try self.emitValueLocal(args[3]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    const offset_off = try self.ensureOnStack(offset_loc, 8);
                    const active_len_off = try self.ensureOnStack(active_len_loc, 8);
                    const word_off = try self.ensureOnStack(word_loc, 8);
                    const eq_loc = try self.callStrStaticSmallWordToScalar(str_off, offset_off, active_len_off, word_off, @intFromPtr(&wrapStrStaticSmallWordEq), .str_static_small_word_eq);
                    const eq_reg = try self.ensureInGeneralReg(eq_loc);
                    return .{ .general_reg = eq_reg };
                },
                .str_static_small_word_caseless_eq => {
                    if (args.len != 4) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const offset_loc = try self.emitValueLocal(args[1]);
                    const active_len_loc = try self.emitValueLocal(args[2]);
                    const word_loc = try self.emitValueLocal(args[3]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    const offset_off = try self.ensureOnStack(offset_loc, 8);
                    const active_len_off = try self.ensureOnStack(active_len_loc, 8);
                    const word_off = try self.ensureOnStack(word_loc, 8);
                    const eq_loc = try self.callStrStaticSmallWordToScalar(str_off, offset_off, active_len_off, word_off, @intFromPtr(&wrapStrStaticSmallWordCaselessEq), .str_static_small_word_caseless_eq);
                    const eq_reg = try self.ensureInGeneralReg(eq_loc);
                    return .{ .general_reg = eq_reg };
                },
                .str_concat => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    if (builtin.mode == .Debug and (a_loc != .stack_str or b_loc != .stack_str)) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: str_concat expects stack_str args, got lhs={s} rhs={s}",
                            .{ @tagName(a_loc), @tagName(b_loc) },
                        );
                    }
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2RocOpsToStr(a_off, b_off, @intFromPtr(&wrapStrConcat), .str_concat, updateModeImmForArg0(ll.unique_args));
                },
                .str_contains => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrContains), .str_contains);
                },
                .str_starts_with => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrStartsWith), .str_starts_with);
                },
                .str_ends_with => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrEndsWith), .str_ends_with);
                },
                .str_count_utf8_bytes => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1ToScalar(str_off, @intFromPtr(&wrapStrCountUtf8Bytes), .str_count_utf8_bytes);
                },
                .str_find_first => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const ls = self.layout_store;
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    if (ret_layout_val.tag != .struct_) {
                        std.debug.panic("LIR/codegen invariant violated: str_find_first expected record return layout", .{});
                    }
                    const record_idx = ret_layout_val.getStruct().idx;
                    const record_data = ls.getStructData(record_idx);
                    const fields = ls.struct_fields.sliceRange(record_data.getFields());
                    if (fields.len != 3 or
                        ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str or
                        ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .str or
                        ls.getStructFieldLayoutByOriginalIndex(record_idx, 2) != .bool)
                    {
                        std.debug.panic("LIR/codegen invariant violated: str_find_first expected fields after Str, before Str, found Bool", .{});
                    }

                    const record_size = record_data.size.get(ls.targetUsize());
                    const result_offset = self.codegen.allocStackSlot(record_size);
                    try self.zeroStackArea(result_offset, record_size);

                    const layout_slot = self.codegen.allocStackSlot(@sizeOf(dev_wrappers.StrFindFirstLayout));
                    const layout_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(layout_reg, @intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 0)));
                    try self.emitStore(.w32, frame_ptr, layout_slot, layout_reg);
                    try self.codegen.emitLoadImm(layout_reg, @intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 1)));
                    try self.emitStore(.w32, frame_ptr, layout_slot + 4, layout_reg);
                    try self.codegen.emitLoadImm(layout_reg, @intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 2)));
                    try self.emitStore(.w32, frame_ptr, layout_slot + 8, layout_reg);
                    self.codegen.freeGeneral(layout_reg);

                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(frame_ptr, result_offset);
                    try builder.addMemArg(frame_ptr, a_off);
                    try builder.addMemArg(frame_ptr, a_off + 16);
                    try builder.addMemArg(frame_ptr, a_off + 8);
                    try builder.addMemArg(frame_ptr, b_off);
                    try builder.addMemArg(frame_ptr, b_off + 16);
                    try builder.addMemArg(frame_ptr, b_off + 8);
                    try builder.addLeaArg(frame_ptr, layout_slot);
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, @intFromPtr(&wrapStrFindFirst), .str_find_first);

                    return self.stackLocationForLayout(ll.ret_layout, result_offset);
                },
                .str_drop_prefix_caseless_ascii => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const ls = self.layout_store;
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    if (ret_layout_val.tag != .struct_) {
                        std.debug.panic("LIR/codegen invariant violated: str_drop_prefix_caseless_ascii expected record return layout", .{});
                    }
                    const record_idx = ret_layout_val.getStruct().idx;
                    const record_data = ls.getStructData(record_idx);
                    const fields = ls.struct_fields.sliceRange(record_data.getFields());
                    if (fields.len != 2 or
                        ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str or
                        ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .bool)
                    {
                        std.debug.panic("LIR/codegen invariant violated: str_drop_prefix_caseless_ascii expected fields after Str, found Bool", .{});
                    }

                    const record_size = record_data.size.get(ls.targetUsize());
                    const result_offset = self.codegen.allocStackSlot(record_size);
                    try self.zeroStackArea(result_offset, record_size);

                    const layout_slot = self.codegen.allocStackSlot(@sizeOf(dev_wrappers.StrDropPrefixCaselessAsciiLayout));
                    const layout_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(layout_reg, @intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 0)));
                    try self.emitStore(.w32, frame_ptr, layout_slot, layout_reg);
                    try self.codegen.emitLoadImm(layout_reg, @intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 1)));
                    try self.emitStore(.w32, frame_ptr, layout_slot + 4, layout_reg);
                    self.codegen.freeGeneral(layout_reg);

                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(frame_ptr, result_offset);
                    try builder.addMemArg(frame_ptr, a_off);
                    try builder.addMemArg(frame_ptr, a_off + 16);
                    try builder.addMemArg(frame_ptr, a_off + 8);
                    try builder.addMemArg(frame_ptr, b_off);
                    try builder.addMemArg(frame_ptr, b_off + 16);
                    try builder.addMemArg(frame_ptr, b_off + 8);
                    try builder.addLeaArg(frame_ptr, layout_slot);
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, @intFromPtr(&wrapStrDropPrefixCaselessAscii), .str_drop_prefix_caseless_ascii);

                    return self.stackLocationForLayout(ll.ret_layout, result_offset);
                },
                .str_caseless_ascii_equals => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2ToScalar(a_off, b_off, @intFromPtr(&wrapStrCaselessAsciiEquals), .str_caseless_ascii_equals);
                },
                .str_repeat => {
                    // str_repeat(str, count) -> Str
                    if (args.len != 2) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const count_loc = try self.emitValueLocal(args[1]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    const count_off = try self.ensureOnStack(count_loc, 8);
                    return try self.callStr1U64RocOpsToStr(str_off, count_off, @intFromPtr(&wrapStrRepeat), .str_repeat, null);
                },
                .str_trim => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrTrim), .str_trim, .str, updateModeImmForArg0(ll.unique_args));
                },
                .str_trim_start => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrTrimStart), .str_trim_start, .str, updateModeImmForArg0(ll.unique_args));
                },
                .str_trim_end => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrTrimEnd), .str_trim_end, .str, updateModeImmForArg0(ll.unique_args));
                },
                .str_split_on => {
                    // str_split(str, delimiter) -> List(Str)
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2RocOpsToResult(a_off, b_off, @intFromPtr(&wrapStrSplit), .str_split, .list, null);
                },
                .str_join_with => {
                    // str_join_with(list, separator) -> Str
                    if (args.len != 2) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const sep_loc = try self.emitValueLocal(args[1]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const sep_off = try self.ensureOnStack(sep_loc, roc_str_size);
                    return try self.callListStrRocOpsToStr(list_off, sep_off, @intFromPtr(&wrapStrJoinWith), .str_join_with);
                },
                .str_reserve => {
                    // str_reserve(str, spare) -> Str
                    if (args.len != 2) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const spare_loc = try self.emitValueLocal(args[1]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    const spare_off = try self.ensureOnStack(spare_loc, 8);
                    return try self.callStr1U64RocOpsToStr(str_off, spare_off, @intFromPtr(&wrapStrReserve), .str_reserve, updateModeImmForArg0(ll.unique_args));
                },
                .str_release_excess_capacity => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrReleaseExcessCapacity), .str_release_excess_capacity, .str, updateModeImmForArg0(ll.unique_args));
                },
                .str_with_capacity => {
                    // str_with_capacity(capacity) -> Str
                    if (args.len != 1) unreachable;
                    const cap_loc = try self.emitValueLocal(args[0]);
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
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2RocOpsToResult(a_off, b_off, @intFromPtr(&wrapStrDropPrefix), .str_drop_prefix, .str, null);
                },
                .str_drop_suffix => {
                    if (args.len != 2) unreachable;
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    const a_off = try self.ensureOnStack(a_loc, roc_str_size);
                    const b_off = try self.ensureOnStack(b_loc, roc_str_size);
                    return try self.callStr2RocOpsToResult(a_off, b_off, @intFromPtr(&wrapStrDropSuffix), .str_drop_suffix, .str, null);
                },
                .str_with_ascii_lowercased => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrWithAsciiLowercased), .str_with_ascii_lowercased, .str, updateModeImmForArg0(ll.unique_args));
                },
                .str_with_ascii_uppercased => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return try self.callStr1RocOpsToResult(str_off, @intFromPtr(&wrapStrWithAsciiUppercased), .str_with_ascii_uppercased, .str, updateModeImmForArg0(ll.unique_args));
                },
                .str_from_utf8_lossy => {
                    // str_from_utf8_lossy(list) -> Str
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    return try self.callList1RocOpsToStr(list_off, @intFromPtr(&wrapStrFromUtf8Lossy), .str_from_utf8_lossy);
                },
                .str_from_utf8 => {
                    // str_from_utf8(list) -> Try(Str, [BadUtf8 {problem: Utf8Problem, index: U64}])
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);

                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                    const ls = self.layout_store;
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    if (ret_layout_val.tag == .tag_union) {
                        const tu_data = ls.getTagUnionData(ret_layout_val.getTagUnion().idx);
                        const variants = ls.getTagUnionVariants(tu_data);
                        var ok_disc: ?u16 = null;
                        var err_disc: ?u16 = null;
                        var err_record_idx: ?layout.StructIdx = null;
                        var inner_disc_offset: u32 = 0;
                        var inner_disc_size: u32 = 0;
                        var inner_bad_utf8_disc: u32 = 0;
                        for (0..variants.len) |i| {
                            const v_payload = variants.get(@intCast(i)).payload_layout;
                            const candidate = self.unwrapSingleFieldPayloadLayout(v_payload) orelse v_payload;
                            if (candidate == .str) {
                                ok_disc = @intCast(i);
                            } else {
                                err_disc = @intCast(i);
                                const err_layout = ls.getLayout(candidate);
                                switch (err_layout.tag) {
                                    .struct_ => err_record_idx = err_layout.getStruct().idx,
                                    .tag_union => {
                                        const inner_tu = ls.getTagUnionData(err_layout.getTagUnion().idx);
                                        if (self.findBadUtf8Variant(inner_tu)) |info| {
                                            err_record_idx = info.struct_idx;
                                            inner_disc_offset = inner_tu.discriminant_offset.get(self.layout_store.targetUsize());
                                            inner_disc_size = inner_tu.discriminant_size;
                                            inner_bad_utf8_disc = info.disc;
                                        }
                                    },
                                    else => {},
                                }
                            }
                        }

                        const resolved_ok = ok_disc orelse std.debug.panic(
                            "LIR/codegen invariant violated: str_from_utf8 had no Ok(Str) variant",
                            .{},
                        );
                        const resolved_err = err_disc orelse std.debug.panic(
                            "LIR/codegen invariant violated: str_from_utf8 had no Err variant",
                            .{},
                        );
                        const rec_idx = err_record_idx orelse std.debug.panic(
                            "LIR/codegen invariant violated: str_from_utf8 could not resolve error record layout",
                            .{},
                        );
                        const struct_data = ls.getStructData(rec_idx);
                        const fields = ls.struct_fields.sliceRange(struct_data.getFields());
                        var index_off: ?u32 = null;
                        var index_size: ?u32 = null;
                        var problem_off: ?u32 = null;
                        var problem_size: ?u32 = null;
                        for (0..fields.len) |i| {
                            const field = fields.get(i);
                            const field_layout = ls.getLayout(field.layout);
                            const field_size = ls.layoutSizeAlign(field_layout).size;
                            const field_off = ls.getStructFieldOffsetByOriginalIndex(rec_idx, field.index);
                            switch (field_size) {
                                8 => {
                                    index_off = field_off;
                                    index_size = field_size;
                                },
                                1 => {
                                    problem_off = field_off;
                                    problem_size = field_size;
                                },
                                else => {},
                            }
                        }
                        const resolved_index_off = index_off orelse std.debug.panic(
                            "LIR/codegen invariant violated: str_from_utf8 could not resolve index offset",
                            .{},
                        );
                        const resolved_index_size = index_size orelse std.debug.panic(
                            "LIR/codegen invariant violated: str_from_utf8 could not resolve index size",
                            .{},
                        );
                        const resolved_problem_off = problem_off orelse std.debug.panic(
                            "LIR/codegen invariant violated: str_from_utf8 could not resolve problem offset",
                            .{},
                        );
                        const resolved_problem_size = problem_size orelse std.debug.panic(
                            "LIR/codegen invariant violated: str_from_utf8 could not resolve problem size",
                            .{},
                        );
                        const tag_size = tu_data.size.get(self.layout_store.targetUsize());
                        const disc_offset = tu_data.discriminant_offset.get(self.layout_store.targetUsize());
                        const disc_size = tu_data.discriminant_size;
                        if (builtin.mode == .Debug and resolved_index_size != 8) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: str_from_utf8 index size {d} != 8",
                                .{resolved_index_size},
                            );
                        }
                        if (builtin.mode == .Debug and resolved_problem_size != 1) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: str_from_utf8 problem size {d} != 1",
                                .{resolved_problem_size},
                            );
                        }

                        const result_slot = self.codegen.allocStackSlot(tag_size);
                        try self.zeroStackArea(result_slot, tag_size);

                        const layout_slot = self.codegen.allocStackSlot(@sizeOf(dev_wrappers.StrFromUtf8Layout));
                        const layout_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(resolved_ok))));
                        try self.emitStore(.w64, frame_ptr, layout_slot, layout_reg);
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(resolved_err))));
                        try self.emitStore(.w64, frame_ptr, layout_slot + 8, layout_reg);
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(disc_offset))));
                        try self.emitStore(.w32, frame_ptr, layout_slot + 16, layout_reg);
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(disc_size))));
                        try self.emitStore(.w32, frame_ptr, layout_slot + 20, layout_reg);
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(resolved_index_off))));
                        try self.emitStore(.w32, frame_ptr, layout_slot + 24, layout_reg);
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(resolved_problem_off))));
                        try self.emitStore(.w32, frame_ptr, layout_slot + 28, layout_reg);
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(inner_disc_offset))));
                        try self.emitStore(.w32, frame_ptr, layout_slot + 32, layout_reg);
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(inner_disc_size))));
                        try self.emitStore(.w32, frame_ptr, layout_slot + 36, layout_reg);
                        try self.codegen.emitLoadImm(layout_reg, @bitCast(@as(i64, @intCast(inner_bad_utf8_disc))));
                        try self.emitStore(.w32, frame_ptr, layout_slot + 40, layout_reg);
                        self.codegen.freeGeneral(layout_reg);

                        // Call C builtin that writes the Roc tag union directly.
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(frame_ptr, result_slot);
                        try builder.addMemArg(frame_ptr, list_off);
                        try builder.addMemArg(frame_ptr, list_off + 8);
                        try builder.addMemArg(frame_ptr, list_off + 16);
                        try builder.addLeaArg(frame_ptr, layout_slot);
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_str_from_utf8_result), .str_from_utf8);

                        return self.stackLocationForLayout(ll.ret_layout, result_slot);
                    }

                    std.debug.panic(
                        "LIR/codegen invariant violated: str_from_utf8 expected tag union layout",
                        .{},
                    );
                },

                // ── Remaining list low-level operations ──

                .list_replace_unsafe => {
                    // list_replace_unsafe(list, index, element) -> { list : List(a), value : a }
                    // The result is a 2-field record. Reuse the same roc_builtins_list_replace
                    // wrapper but aim the (out_list, out_element) outputs directly at the
                    // list and value fields of the result record.
                    if (args.len != 3) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const index_loc = try self.emitValueLocal(args[1]);
                    const elem_loc = try self.emitValueLocal(args[2]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    // The list ABI must come from the INPUT list layout (args[0]); ll.ret_layout
                    // here is the result record, not a list.
                    const arg_list_layout = self.valueLayout(args[0]);
                    const list_abi = builtinInternalListAbi(ls, "dev.list_replace_unsafe.builtin_list_abi", arg_list_layout);

                    // Resolve the result record's field offsets. The record has exactly two
                    // fields; one is a list, the other is the element. Disambiguate by tag.
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    if (ret_layout_val.tag != .struct_) unreachable;
                    const rec_idx = ret_layout_val.getStruct().idx;
                    const f0_layout_idx = ls.getStructFieldLayoutByOriginalIndex(rec_idx, 0);
                    const f0_layout = ls.getLayout(f0_layout_idx);
                    const f0_offset: i32 = @intCast(ls.getStructFieldOffsetByOriginalIndex(rec_idx, 0));
                    const f1_offset: i32 = @intCast(ls.getStructFieldOffsetByOriginalIndex(rec_idx, 1));
                    const f0_is_list = f0_layout.tag == .list or f0_layout.tag == .list_of_zst;
                    const list_field_offset: i32 = if (f0_is_list) f0_offset else f1_offset;
                    const value_field_offset: i32 = if (f0_is_list) f1_offset else f0_offset;

                    const result_size = ls.layoutSizeAlign(ret_layout_val).size;
                    const result_offset = self.codegen.allocStackSlot(result_size);

                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const index_off = try self.ensureOnStack(index_loc, 8);
                    const elem_off = try self.ensureOnStack(elem_loc, list_abi.elem_size_align.size);

                    // For ZST elements, listReplace would dereference a NULL element pointer.
                    // Copy the input list bytes into the result's list field directly; the
                    // value field is zero-sized and needs no write.
                    if (list_abi.elem_size_align.size == 0) {
                        const tmp = try self.allocTempGeneral();
                        defer self.codegen.freeGeneral(tmp);
                        var off: i32 = 0;
                        while (off < roc_list_size) : (off += 8) {
                            try self.emitLoad(.w64, tmp, frame_ptr, list_off + off);
                            try self.emitStore(.w64, frame_ptr, result_offset + list_field_offset + off, tmp);
                        }
                        return .{ .stack = .{ .offset = result_offset } };
                    }

                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_replace);
                    const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
                    defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
                    const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
                    defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

                    {
                        // wrapListReplace(out_list, list_bytes, list_len, list_cap, alignment, index, element, element_width, out_element, elements_refcounted, element_incref, element_decref, update_mode, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset + list_field_offset);
                        try builder.addMemArg(base_reg, list_off);
                        try builder.addMemArg(base_reg, list_off + 8);
                        try builder.addMemArg(base_reg, list_off + 16);
                        try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                        try builder.addMemArg(base_reg, index_off);
                        try builder.addLeaArg(base_reg, elem_off);
                        try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                        try builder.addLeaArg(base_reg, result_offset + value_field_offset);
                        try builder.addImmArg(if (list_abi.elements_refcounted) @as(usize, 1) else 0);
                        if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_replace);
                    }

                    return .{ .stack = .{ .offset = result_offset } };
                },
                .list_set => {
                    // list_set(list, index, element) -> List
                    if (args.len != 3) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const index_loc = try self.emitValueLocal(args[1]);
                    const elem_loc = try self.emitValueLocal(args[2]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const list_abi = builtinInternalListAbi(ls, "dev.list_set.builtin_list_abi", ll.ret_layout);

                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const result_offset = self.codegen.allocStackSlot(roc_list_size);
                    if (list_abi.elem_size_align.size == 0) {
                        const tmp = try self.allocTempGeneral();
                        defer self.codegen.freeGeneral(tmp);
                        var off: i32 = 0;
                        while (off < roc_list_size) : (off += 8) {
                            try self.emitLoad(.w64, tmp, frame_ptr, list_off + off);
                            try self.emitStore(.w64, frame_ptr, result_offset + off, tmp);
                        }
                        return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                    }

                    const index_off = try self.ensureOnStack(index_loc, 8);
                    const elem_off = try self.ensureOnStack(elem_loc, list_abi.elem_size_align.size);
                    // We need a scratch slot for the old element (out_element param)
                    const old_elem_slot = self.codegen.allocStackSlot(@intCast(list_abi.elem_size_align.size));
                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_replace);
                    const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
                    defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
                    const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
                    defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

                    {
                        // wrapListReplace(out, list_bytes, list_len, list_cap, alignment, index, element, element_width, out_element, elements_refcounted, element_incref, element_decref, update_mode, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_off);
                        try builder.addMemArg(base_reg, list_off + 8);
                        try builder.addMemArg(base_reg, list_off + 16);
                        try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                        try builder.addMemArg(base_reg, index_off);
                        try builder.addLeaArg(base_reg, elem_off);
                        try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                        try builder.addLeaArg(base_reg, old_elem_slot);
                        try builder.addImmArg(if (list_abi.elements_refcounted) @as(usize, 1) else 0);
                        if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_replace);
                    }

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_swap => {
                    // list_swap(list, index_1, index_2) -> List
                    if (args.len != 3) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const index_1_loc = try self.emitValueLocal(args[1]);
                    const index_2_loc = try self.emitValueLocal(args[2]);

                    const ls = self.layout_store;
                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;

                    const list_abi = builtinInternalListAbi(ls, "dev.list_swap.builtin_list_abi", ll.ret_layout);

                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    const index_1_off = try self.ensureOnStack(index_1_loc, 8);
                    const index_2_off = try self.ensureOnStack(index_2_loc, 8);
                    const result_offset = self.codegen.allocStackSlot(roc_str_size);
                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_swap);
                    const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
                    defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
                    const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
                    defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

                    {
                        // wrapListSwap(out, list_bytes, list_len, list_cap, alignment, element_width, index_1, index_2, elements_refcounted, element_incref, element_decref, update_mode, roc_ops)
                        const base_reg = frame_ptr;
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                        try builder.addLeaArg(base_reg, result_offset);
                        try builder.addMemArg(base_reg, list_off);
                        try builder.addMemArg(base_reg, list_off + 8);
                        try builder.addMemArg(base_reg, list_off + 16);
                        try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                        try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                        try builder.addMemArg(base_reg, index_1_off);
                        try builder.addMemArg(base_reg, index_2_off);
                        try builder.addImmArg(if (list_abi.elements_refcounted) @as(usize, 1) else 0);
                        if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                        try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                        try builder.addRegArg(roc_ops_reg);

                        try self.callBuiltin(&builder, fn_addr, .list_swap);
                    }

                    return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                },
                .list_first => {
                    // list_first(list) -> element  (same as list_get at index 0)
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    return try self.listGetAtConstIndex(list_loc, 0, ll.ret_layout);
                },
                .list_last => {
                    // list_last(list) -> element  (same as list_get at index len-1)
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    return try self.listGetAtLastIndex(list_loc, ll.ret_layout);
                },
                .list_reverse => {
                    // list_reverse(list) -> List
                    // Clone and reverse in place using listSublist (full range)
                    // Actually reverse needs a proper implementation. For now use sublist(0, len) and reverse.
                    // Simplest correct approach: allocate new list, copy elements in reverse order
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    return try self.generateListReverse(list_loc, ll);
                },
                .list_reserve => {
                    // list_reserve(list, spare) -> List
                    if (args.len != 2) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const spare_loc = try self.emitValueLocal(args[1]);
                    return try self.callListReserveOp(list_loc, spare_loc, ll);
                },
                .list_release_excess_capacity => {
                    // list_release_excess_capacity(list) -> List
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    return try self.callListReleaseExcessCapOp(list_loc, ll);
                },
                .list_split_first => {
                    // list_split_first(list) -> {first: elem, rest: List}
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    return try self.callListSplitOp(ll, list_loc, .first);
                },
                .list_split_last => {
                    // list_split_last(list) -> {rest: List, last: elem}
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
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
                // Returns a record { success: U8, val_or_memory_garbage: To }.
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

                .u8_from_str,
                .i8_from_str,
                .u16_from_str,
                .i16_from_str,
                .u32_from_str,
                .i32_from_str,
                .u64_from_str,
                .i64_from_str,
                .u128_from_str,
                .i128_from_str,
                .dec_from_str,
                .f32_from_str,
                .f64_from_str,
                => {
                    return try self.generateNumFromStr(ll, args);
                },
                .list_sublist => {
                    // list_sublist(list, {start, len}) -> List
                    if (args.len != 2) unreachable;
                    const record_layout_idx = self.valueLayout(args[1]);
                    const list_loc = try self.emitValueLocal(args[0]);
                    const record_loc = try self.emitValueLocal(args[1]);
                    return try self.callListSublistFromRecord(ll, list_loc, record_loc, record_layout_idx);
                },
                .list_drop_at => {
                    if (args.len != 2) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const index_loc = try self.emitValueLocal(args[1]);
                    return try self.callListDropAt(ll, list_loc, index_loc);
                },
                .num_abs => {
                    // Absolute value: classify signedness from the operand layout.
                    // The result layout is not authoritative here because numeric methods
                    // can return a different signedness than their operand.
                    const val_loc = try self.emitValueLocal(args[0]);
                    return try self.generateNumAbs(val_loc, self.valueLayout(args[0]));
                },
                .num_abs_diff => {
                    // |a - b|: compare and subtract in the correct order to avoid wrap/overflow
                    if (args.len != 2) unreachable;
                    const lhs_layout = self.valueLayout(args[0]);
                    const rhs_layout = self.valueLayout(args[1]);
                    if (lhs_layout != rhs_layout) {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: num_abs_diff argument layouts differ: lhs={s} rhs={s}",
                                .{ @tagName(lhs_layout), @tagName(rhs_layout) },
                            );
                        }
                        unreachable;
                    }
                    const a_loc = try self.emitValueLocal(args[0]);
                    const b_loc = try self.emitValueLocal(args[1]);
                    return try self.generateAbsDiff(a_loc, b_loc, ll.ret_layout, lhs_layout);
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
                .num_bitwise_and,
                .num_bitwise_or,
                .num_bitwise_xor,
                .num_is_eq,
                .num_is_gt,
                .num_is_gte,
                .num_is_lt,
                .num_is_lte,
                => {
                    const lhs_loc = try self.emitValueLocal(args[0]);
                    const rhs_loc = try self.emitValueLocal(args[1]);

                    // For numeric/comparison ops, operand layout comes from arguments.
                    // Return layout can be Bool for comparisons, so don't key operand
                    // behavior off `ll.ret_layout`.
                    const operand_layout =
                        self.valueLayout(args[0]);

                    // With ANF, operands are always lookups or literals, so we
                    // dispatch structural equality purely by layout.
                    if (ll.op == .num_is_eq) {
                        {
                            const ls = self.layout_store;
                            const layout_idx = self.valueLayout(args[0]);
                            const stored_layout = ls.getLayout(layout_idx);
                            if (stored_layout.tag == .struct_ or
                                stored_layout.tag == .list or
                                stored_layout.tag == .list_of_zst or
                                stored_layout.tag == .tag_union)
                                return self.generateStructuralEquality(lhs_loc, rhs_loc, layout_idx);
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
                    const inner_loc = try self.emitValueLocal(args[0]);
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

                .num_bitwise_not => {
                    const inner_loc = try self.emitValueLocal(args[0]);
                    const is_i128 = ll.ret_layout == .i128 or ll.ret_layout == .u128;

                    if (is_i128) {
                        const signedness: std.builtin.Signedness = if (ll.ret_layout == .u128) .unsigned else .signed;
                        const parts = try self.getI128Parts(inner_loc, signedness);
                        const result_low = try self.allocTempGeneral();
                        const result_high = try self.allocTempGeneral();

                        try self.codegen.emitNot(.w64, result_low, parts.low);
                        try self.codegen.emitNot(.w64, result_high, parts.high);

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
                        try self.codegen.emitNot(.w64, result_reg, src_reg);
                        self.codegen.freeGeneral(src_reg);
                        return .{ .general_reg = result_reg };
                    }
                },

                .bool_not => {
                    const inner_loc = try self.emitValueLocal(args[0]);
                    const src_reg = try self.ensureInGeneralReg(inner_loc);
                    const result_reg = try self.allocTempGeneral();
                    try self.emitCmpImm(src_reg, 0);
                    try self.emitSetCond(result_reg, condEqual());
                    self.codegen.freeGeneral(src_reg);
                    return .{ .general_reg = result_reg };
                },

                .dict_pseudo_seed,
                .hasher_finish,
                .hasher_write_bool,
                .hasher_write_u8,
                .hasher_write_u16,
                .hasher_write_u32,
                .hasher_write_u64,
                .hasher_write_u128,
                .hasher_write_i8,
                .hasher_write_i16,
                .hasher_write_i32,
                .hasher_write_i64,
                .hasher_write_i128,
                .hasher_write_f32,
                .hasher_write_f64,
                .hasher_write_dec,
                .hasher_write_bytes,
                .hasher_write_str,
                => return try self.generateHasherLowLevel(ll, args),

                .crypto_sha256_hash_bytes,
                .crypto_sha256_hasher_empty,
                .crypto_sha256_hasher_write,
                .crypto_sha256_hasher_finish,
                .crypto_blake3_hash_bytes,
                .crypto_blake3_hasher_empty,
                .crypto_blake3_hasher_write,
                .crypto_blake3_hasher_finish,
                => return try self.generateCryptoLowLevel(ll, args),

                .u8_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 1, false);
                },
                .i8_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 1, true);
                },
                .u16_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 2, false);
                },
                .i16_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 2, true);
                },
                .u32_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 4, false);
                },
                .i32_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 4, true);
                },
                .u64_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 8, false);
                },
                .i64_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 8, true);
                },
                .u128_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 16, false);
                },
                .i128_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callIntToStr(value_loc, 16, true);
                },
                .dec_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callDecToStr(value_loc);
                },
                .f32_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callFloatToStr(value_loc, true);
                },
                .f64_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return self.callFloatToStr(value_loc, false);
                },
                .str_inspect => {
                    if (args.len != 1) unreachable;
                    const str_loc = try self.emitValueLocal(args[0]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    return self.callStr1RocOpsToResult(
                        str_off,
                        @intFromPtr(&dev_wrappers.roc_builtins_str_escape_and_quote),
                        .str_escape_and_quote,
                        .str,
                        null,
                    );
                },
                .num_to_str => {
                    const value_loc = try self.emitValueLocal(args[0]);
                    return switch (self.valueLayout(args[0])) {
                        .u8 => self.callIntToStr(value_loc, 1, false),
                        .i8 => self.callIntToStr(value_loc, 1, true),
                        .u16 => self.callIntToStr(value_loc, 2, false),
                        .i16 => self.callIntToStr(value_loc, 2, true),
                        .u32 => self.callIntToStr(value_loc, 4, false),
                        .i32 => self.callIntToStr(value_loc, 4, true),
                        .u64 => self.callIntToStr(value_loc, 8, false),
                        .i64 => self.callIntToStr(value_loc, 8, true),
                        .u128 => self.callIntToStr(value_loc, 16, false),
                        .i128 => self.callIntToStr(value_loc, 16, true),
                        .dec => self.callDecToStr(value_loc),
                        .f32 => self.callFloatToStr(value_loc, true),
                        .f64 => self.callFloatToStr(value_loc, false),
                        else => std.debug.panic(
                            "LirCodeGen invariant violated: num_to_str received non-numeric layout {s}",
                            .{@tagName(self.valueLayout(args[0]))},
                        ),
                    };
                },

                .num_sqrt => {
                    if (args.len != 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    if (ll.ret_layout == .dec) {
                        const adj_src = if (src_loc == .stack) ValueLocation{ .stack_i128 = src_loc.stack.offset } else src_loc;
                        return self.callDecUnaryMathBuiltin(
                            adj_src,
                            @intFromPtr(&dev_wrappers.roc_builtins_dec_sqrt),
                            .dec_sqrt,
                        );
                    }

                    const src_reg = try self.ensureInFloatReg(src_loc);
                    const result_reg = try self.codegen.allocFloatFor(0);

                    switch (ll.ret_layout) {
                        .f32 => {
                            if (comptime target.toCpuArch() == .aarch64) {
                                try self.codegen.emit.fcvtFloatFloat(.single, src_reg, .double, src_reg);
                                try self.codegen.emit.fsqrtRegReg(.single, result_reg, src_reg);
                                try self.codegen.emit.fcvtFloatFloat(.double, result_reg, .single, result_reg);
                            } else {
                                try self.codegen.emit.cvtsd2ssRegReg(src_reg, src_reg);
                                try self.codegen.emit.sqrtssRegReg(result_reg, src_reg);
                                try self.codegen.emit.cvtss2sdRegReg(result_reg, result_reg);
                            }
                        },
                        .f64 => {
                            if (comptime target.toCpuArch() == .aarch64) {
                                try self.codegen.emit.fsqrtRegReg(.double, result_reg, src_reg);
                            } else {
                                try self.codegen.emit.sqrtsdRegReg(result_reg, src_reg);
                            }
                        },
                        else => std.debug.panic(
                            "LirCodeGen invariant violated: num_sqrt received non-float return layout {s}",
                            .{@tagName(ll.ret_layout)},
                        ),
                    }

                    self.codegen.freeFloat(src_reg);
                    return .{ .float_reg = result_reg };
                },
                .num_pow => {
                    if (args.len != 2) unreachable;
                    const lhs_loc = try self.emitValueLocal(args[0]);
                    const rhs_loc = try self.emitValueLocal(args[1]);
                    if (ll.ret_layout == .dec) {
                        const adj_lhs = if (lhs_loc == .stack) ValueLocation{ .stack_i128 = lhs_loc.stack.offset } else lhs_loc;
                        const adj_rhs = if (rhs_loc == .stack) ValueLocation{ .stack_i128 = rhs_loc.stack.offset } else rhs_loc;
                        return self.callDecBinaryMathBuiltin(
                            adj_lhs,
                            adj_rhs,
                            @intFromPtr(&dev_wrappers.roc_builtins_dec_pow),
                            .dec_pow,
                        );
                    }

                    return self.callFloatBinaryBuiltin(
                        lhs_loc,
                        rhs_loc,
                        ll.ret_layout,
                        @intFromPtr(&dev_wrappers.roc_builtins_float_pow),
                        .float_pow,
                    );
                },
                .num_sin,
                .num_cos,
                .num_tan,
                .num_asin,
                .num_acos,
                .num_atan,
                => {
                    if (args.len != 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    if (ll.ret_layout == .dec) {
                        const math_builtin = decUnaryMathBuiltin(ll.op);
                        const adj_src = if (src_loc == .stack) ValueLocation{ .stack_i128 = src_loc.stack.offset } else src_loc;
                        return self.callDecUnaryMathBuiltin(
                            adj_src,
                            math_builtin.addr,
                            math_builtin.func,
                        );
                    }

                    const math_builtin = floatUnaryMathBuiltin(ll.op);
                    return self.callFloatUnaryBuiltin(
                        src_loc,
                        ll.ret_layout,
                        math_builtin.addr,
                        math_builtin.func,
                    );
                },
                .num_floor => {
                    if (args.len != 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    return self.callFloatUnaryBuiltin(
                        src_loc,
                        ll.ret_layout,
                        @intFromPtr(&dev_wrappers.roc_builtins_float_floor),
                        .float_floor,
                    );
                },
                .num_ceiling => {
                    if (args.len != 1) unreachable;
                    const src_loc = try self.emitValueLocal(args[0]);
                    return self.callFloatUnaryBuiltin(
                        src_loc,
                        ll.ret_layout,
                        @intFromPtr(&dev_wrappers.roc_builtins_float_ceiling),
                        .float_ceiling,
                    );
                },
                .compare => {
                    if (args.len != 2) unreachable;
                    const operand_layout = self.valueLayout(args[0]);

                    const gt_loc = if (operand_layout == .dec or operand_layout == .i128 or operand_layout == .u128) blk: {
                        const lhs_loc = try self.emitValueLocal(args[0]);
                        const rhs_loc = try self.emitValueLocal(args[1]);
                        const adj_lhs = if (lhs_loc == .stack) ValueLocation{ .stack_i128 = lhs_loc.stack.offset } else lhs_loc;
                        const adj_rhs = if (rhs_loc == .stack) ValueLocation{ .stack_i128 = rhs_loc.stack.offset } else rhs_loc;
                        break :blk try self.generateI128Binop(.num_is_gt, adj_lhs, adj_rhs, operand_layout);
                    } else blk: {
                        const lhs_loc = try self.emitValueLocal(args[0]);
                        const rhs_loc = try self.emitValueLocal(args[1]);
                        break :blk try self.generateIntBinop(.num_is_gt, lhs_loc, rhs_loc, operand_layout);
                    };

                    const lt_loc = if (operand_layout == .dec or operand_layout == .i128 or operand_layout == .u128) blk: {
                        const lhs_loc = try self.emitValueLocal(args[0]);
                        const rhs_loc = try self.emitValueLocal(args[1]);
                        const adj_lhs = if (lhs_loc == .stack) ValueLocation{ .stack_i128 = lhs_loc.stack.offset } else lhs_loc;
                        const adj_rhs = if (rhs_loc == .stack) ValueLocation{ .stack_i128 = rhs_loc.stack.offset } else rhs_loc;
                        break :blk try self.generateI128Binop(.num_is_lt, adj_lhs, adj_rhs, operand_layout);
                    } else blk: {
                        const lhs_loc = try self.emitValueLocal(args[0]);
                        const rhs_loc = try self.emitValueLocal(args[1]);
                        break :blk try self.generateIntBinop(.num_is_lt, lhs_loc, rhs_loc, operand_layout);
                    };

                    const gt_reg = try self.ensureInGeneralReg(gt_loc);
                    const lt_reg = try self.ensureInGeneralReg(lt_loc);
                    try self.emitShlImm(.w64, lt_reg, lt_reg, 1);
                    try self.emitAddRegs(.w64, gt_reg, gt_reg, lt_reg);
                    self.codegen.freeGeneral(lt_reg);
                    return .{ .general_reg = gt_reg };
                },

                // Resolved before backend codegen: builtin `from_numeral` is
                // folded to a constant during Monotype lowering, so the op must
                // never reach a backend. Reaching here is an invariant failure,
                // not a missing feature.
                .num_from_numeral => {
                    std.debug.panic("num_from_numeral reached the dev backend; it must be folded to a constant during Monotype lowering", .{});
                },
                // Unimplemented ops
                .num_log,
                .num_round,
                => {
                    std.debug.panic("UNIMPLEMENTED low-level op: {s}", .{@tagName(ll.op)});
                },
                .box_box => {
                    // Box.box(value) -> Box(value): heap-allocate and copy value
                    const ls = self.layout_store;
                    const ret_layout_data = ls.getLayout(ll.ret_layout);

                    if (ret_layout_data.tag == .box_of_zst) {
                        // Boxing a ZST: evaluate the expression (for side effects) but
                        // return a null-like pointer since there's no data to store.
                        _ = try self.emitValueLocal(args[0]);
                        const reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(reg, 0);
                        return .{ .general_reg = reg };
                    }

                    const box_abi = ls.builtinBoxAbi(ll.ret_layout);
                    const elem_size: u32 = box_abi.elem_size;
                    const elem_alignment: u32 = box_abi.elem_alignment;

                    // Handle ZST element even when layout tag is .box (not .box_of_zst)
                    if (elem_size == 0) {
                        _ = try self.emitValueLocal(args[0]);
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
                        try builder.addImmArg(if (box_abi.contains_refcounted) 1 else 0);
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, @intFromPtr(&allocateWithRefcountC), .allocate_with_refcount);
                    }
                    // Save heap pointer to stack (generating the value expression may clobber registers)
                    try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

                    // Generate the value expression
                    const value_loc = try self.emitValueLocal(args[0]);

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
                    const box_arg_layout = self.valueLayout(args[0]);
                    const box_layout_data = ls.getLayout(box_arg_layout);
                    const erased_box_ptr = box_layout_data.tag == .scalar and box_layout_data.getScalar().tag == .opaque_ptr;

                    if (box_layout_data.tag == .box_of_zst or
                        (erased_box_ptr and ls.isZeroSized(ls.getLayout(ll.ret_layout))))
                    {
                        // Unboxing a ZST: evaluate the box expression (for side effects)
                        // but return a ZST (no data).
                        _ = try self.emitValueLocal(args[0]);
                        return .{ .immediate_i64 = 0 };
                    }

                    const elem_layout_idx: layout.Idx = if (erased_box_ptr)
                        ll.ret_layout
                    else
                        (ls.builtinBoxAbi(box_arg_layout).elem_layout_idx orelse .zst);
                    const elem_layout_data = ls.getLayout(elem_layout_idx);
                    const elem_size: u32 = ls.layoutSize(elem_layout_data);

                    // Handle ZST element even when layout tag is .box (not .box_of_zst)
                    if (elem_size == 0) {
                        _ = try self.emitValueLocal(args[0]);
                        return .{ .immediate_i64 = 0 };
                    }

                    // Generate the box pointer expression
                    const box_loc = try self.emitValueLocal(args[0]);
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
                .erased_capture_load => {
                    const elem_layout_idx = ll.ret_layout;
                    const elem_layout_data = self.layout_store.getLayout(elem_layout_idx);
                    const elem_size: u32 = self.layout_store.layoutSize(elem_layout_data);

                    if (elem_size == 0) {
                        _ = try self.emitValueLocal(args[0]);
                        return .{ .immediate_i64 = 0 };
                    }

                    const capture_ptr_loc = try self.emitValueLocal(args[0]);
                    const capture_ptr_reg = try self.ensureInGeneralReg(capture_ptr_loc);

                    const result_offset = self.codegen.allocStackSlot(elem_size);
                    const temp_reg = try self.allocTempGeneral();
                    try self.copyChunked(temp_reg, capture_ptr_reg, 0, frame_ptr, result_offset, elem_size);
                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(capture_ptr_reg);

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
                .ptr_alloca => {
                    // ptr_alloca: () -> Ptr(T). Reserve a zeroed stack slot for T and
                    // yield its address. Target local layout is ptr(T).
                    const ls = self.layout_store;
                    const ret_layout_data = ls.getLayout(ll.ret_layout);
                    const elem_size: u32 = ls.layoutSize(ls.getLayout(ret_layout_data.getIdx()));

                    const slot = self.codegen.allocStackSlot(@max(elem_size, 1));
                    if (elem_size > 0) {
                        try self.zeroStackArea(slot, elem_size);
                    }
                    const reg = try self.allocTempGeneral();
                    try self.emitLeaStack(reg, slot);
                    return .{ .general_reg = reg };
                },
                .box_alloc_zeroed => {
                    // box_alloc_zeroed: () -> Box(T). Heap cell (rc=1) with a
                    // zero-filled payload, so box fields inside read as null.
                    const ls = self.layout_store;
                    const ret_layout_data = ls.getLayout(ll.ret_layout);

                    if (ret_layout_data.tag == .box_of_zst) {
                        const reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(reg, 0);
                        return .{ .general_reg = reg };
                    }

                    const box_abi = ls.builtinBoxAbi(ll.ret_layout);
                    const elem_size: u32 = box_abi.elem_size;
                    if (elem_size == 0) {
                        const reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(reg, 0);
                        return .{ .general_reg = reg };
                    }

                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                    const heap_ptr_slot: i32 = self.codegen.allocStackSlot(8);
                    {
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addImmArg(@intCast(elem_size));
                        try builder.addImmArg(@intCast(box_abi.elem_alignment));
                        try builder.addImmArg(if (box_abi.contains_refcounted) 1 else 0);
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, @intFromPtr(&allocateWithRefcountC), .allocate_with_refcount);
                    }
                    try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

                    const heap_ptr = try self.allocTempGeneral();
                    try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);
                    try self.zeroMemAt(heap_ptr, elem_size);
                    return .{ .general_reg = heap_ptr };
                },
                .ptr_store => {
                    // ptr_store: (Ptr(T), T) -> {}. Copy sizeOf(T) bytes into *ptr.
                    const ls = self.layout_store;
                    const value_layout = self.valueLayout(args[1]);
                    const elem_size: u32 = ls.layoutSize(ls.getLayout(value_layout));

                    if (elem_size == 0) {
                        _ = try self.emitValueLocal(args[0]);
                        _ = try self.emitValueLocal(args[1]);
                        return .{ .immediate_i64 = 0 };
                    }

                    const value_loc = try self.emitValueLocal(args[1]);
                    const value_offset = try self.ensureOnStack(value_loc, elem_size);
                    const ptr_loc = try self.emitValueLocal(args[0]);
                    const ptr_reg = try self.ensureInGeneralReg(ptr_loc);

                    const temp_reg = try self.allocTempGeneral();
                    try self.copyChunked(temp_reg, frame_ptr, value_offset, ptr_reg, 0, elem_size);
                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(ptr_reg);
                    return .{ .immediate_i64 = 0 };
                },
                .ptr_load => {
                    // ptr_load: (Ptr(T)) -> T. Copy sizeOf(T) bytes out of *ptr.
                    // Same shape as erased_capture_load above.
                    const ls = self.layout_store;
                    const elem_layout_data = ls.getLayout(ll.ret_layout);
                    const elem_size: u32 = ls.layoutSize(elem_layout_data);

                    if (elem_size == 0) {
                        _ = try self.emitValueLocal(args[0]);
                        return .{ .immediate_i64 = 0 };
                    }

                    const ptr_loc = try self.emitValueLocal(args[0]);
                    const ptr_reg = try self.ensureInGeneralReg(ptr_loc);

                    const result_offset = self.codegen.allocStackSlot(elem_size);
                    const temp_reg = try self.allocTempGeneral();
                    try self.copyChunked(temp_reg, ptr_reg, 0, frame_ptr, result_offset, elem_size);
                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(ptr_reg);

                    if (ll.ret_layout == .i128 or ll.ret_layout == .u128 or ll.ret_layout == .dec) {
                        return .{ .stack_i128 = result_offset };
                    } else if (ll.ret_layout == .str) {
                        return .{ .stack_str = result_offset };
                    } else if (elem_layout_data.tag == .list or elem_layout_data.tag == .list_of_zst) {
                        return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
                    } else {
                        return .{ .stack = .{ .offset = result_offset, .size = ValueSize.fromByteCount(elem_size) } };
                    }
                },
                .ptr_cast => {
                    // ptr_cast: identity bits (box(T) -> ptr(T) or ptr -> ptr).
                    const loc = try self.emitValueLocal(args[0]);
                    const src_reg = try self.ensureInGeneralReg(loc);
                    const dst_reg = try self.allocTempGeneral();
                    try self.emitMovRegReg(dst_reg, src_reg);
                    self.codegen.freeGeneral(src_reg);
                    return .{ .general_reg = dst_reg };
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

        fn hasherDomain(op: lir.LowLevel) u8 {
            return @intFromEnum(switch (op) {
                .hasher_write_bool => builtins.hash.HasherDomain.bool,
                .hasher_write_u8 => builtins.hash.HasherDomain.u8,
                .hasher_write_u16 => builtins.hash.HasherDomain.u16,
                .hasher_write_u32 => builtins.hash.HasherDomain.u32,
                .hasher_write_u64 => builtins.hash.HasherDomain.u64,
                .hasher_write_u128 => builtins.hash.HasherDomain.u128,
                .hasher_write_i8 => builtins.hash.HasherDomain.i8,
                .hasher_write_i16 => builtins.hash.HasherDomain.i16,
                .hasher_write_i32 => builtins.hash.HasherDomain.i32,
                .hasher_write_i64 => builtins.hash.HasherDomain.i64,
                .hasher_write_i128 => builtins.hash.HasherDomain.i128,
                .hasher_write_dec => builtins.hash.HasherDomain.dec,
                .hasher_write_bytes => builtins.hash.HasherDomain.bytes,
                else => unreachable,
            });
        }

        fn hasherWidth(op: lir.LowLevel) u8 {
            return switch (op) {
                .hasher_write_bool,
                .hasher_write_u8,
                .hasher_write_i8,
                => 1,
                .hasher_write_u16,
                .hasher_write_i16,
                => 2,
                .hasher_write_u32,
                .hasher_write_i32,
                => 4,
                .hasher_write_u64,
                .hasher_write_i64,
                => 8,
                else => unreachable,
            };
        }

        fn hasherStateReg(self: *Self, local: LocalId) Allocator.Error!GeneralReg {
            const loc = try self.emitValueLocal(local);
            return switch (loc) {
                .stack => |s| blk: {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, s.offset);
                    break :blk reg;
                },
                else => try self.ensureInGeneralReg(loc),
            };
        }

        fn scalarRetReg(self: *Self) Allocator.Error!ValueLocation {
            const result_reg = try self.allocTempGeneral();
            try self.codegen.emit.movRegReg(.w64, result_reg, ret_reg_0);
            return .{ .general_reg = result_reg };
        }

        fn callHasherWriteU64(
            self: *Self,
            seed_reg: GeneralReg,
            value_reg: GeneralReg,
            domain: u8,
            width: u8,
        ) Allocator.Error!ValueLocation {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(seed_reg);
            try builder.addImmArg(@intCast(domain));
            try builder.addRegArg(value_reg);
            try builder.addImmArg(@intCast(width));
            try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_hasher_write_u64), .hasher_write_u64);
            self.codegen.freeGeneral(seed_reg);
            if (value_reg != seed_reg) self.codegen.freeGeneral(value_reg);
            return try self.scalarRetReg();
        }

        fn callHasherWriteBits(self: *Self, seed_reg: GeneralReg, bits_reg: GeneralReg, comptime is_f32: bool) Allocator.Error!ValueLocation {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(seed_reg);
            try builder.addRegArg(bits_reg);
            try self.callBuiltin(
                &builder,
                if (is_f32) @intFromPtr(&dev_wrappers.roc_builtins_hasher_write_f32_bits) else @intFromPtr(&dev_wrappers.roc_builtins_hasher_write_f64_bits),
                if (is_f32) .hasher_write_f32_bits else .hasher_write_f64_bits,
            );
            self.codegen.freeGeneral(seed_reg);
            if (bits_reg != seed_reg) self.codegen.freeGeneral(bits_reg);
            return try self.scalarRetReg();
        }

        fn callHasherWriteU128(self: *Self, seed_reg: GeneralReg, parts: I128Parts, domain: u8) Allocator.Error!ValueLocation {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(seed_reg);
            try builder.addImmArg(@intCast(domain));
            try builder.addRegArg(parts.low);
            try builder.addRegArg(parts.high);
            try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_hasher_write_u128), .hasher_write_u128);
            self.codegen.freeGeneral(seed_reg);
            self.codegen.freeGeneral(parts.low);
            self.codegen.freeGeneral(parts.high);
            return try self.scalarRetReg();
        }

        fn generateHasherLowLevel(self: *Self, ll: anytype, args: []const LocalId) Allocator.Error!ValueLocation {
            switch (ll.op) {
                .dict_pseudo_seed => {
                    if (args.len != 0) unreachable;
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_dict_pseudo_seed), .dict_pseudo_seed);
                    return try self.scalarRetReg();
                },
                .hasher_finish => {
                    if (args.len != 1) unreachable;
                    const seed_reg = try self.hasherStateReg(args[0]);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addRegArg(seed_reg);
                    try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_hasher_finish), .hasher_finish);
                    self.codegen.freeGeneral(seed_reg);
                    return try self.scalarRetReg();
                },
                .hasher_write_bool,
                .hasher_write_u8,
                .hasher_write_u16,
                .hasher_write_u32,
                .hasher_write_u64,
                .hasher_write_i8,
                .hasher_write_i16,
                .hasher_write_i32,
                .hasher_write_i64,
                => {
                    if (args.len != 2) unreachable;
                    const seed_reg = try self.hasherStateReg(args[0]);
                    const value_loc = try self.emitValueLocal(args[1]);
                    const value_reg = try self.ensureInGeneralReg(value_loc);
                    return try self.callHasherWriteU64(seed_reg, value_reg, hasherDomain(ll.op), hasherWidth(ll.op));
                },
                .hasher_write_f32 => {
                    if (args.len != 2) unreachable;
                    const seed_reg = try self.hasherStateReg(args[0]);
                    const value_loc = try self.emitValueLocal(args[1]);
                    const bits_reg = try self.materializeF32BitsInGeneralReg(value_loc);
                    return try self.callHasherWriteBits(seed_reg, bits_reg, true);
                },
                .hasher_write_f64 => {
                    if (args.len != 2) unreachable;
                    const seed_reg = try self.hasherStateReg(args[0]);
                    const value_loc = try self.emitValueLocal(args[1]);
                    const bits_reg = try self.ensureInGeneralReg(value_loc);
                    return try self.callHasherWriteBits(seed_reg, bits_reg, false);
                },
                .hasher_write_u128,
                .hasher_write_i128,
                .hasher_write_dec,
                => {
                    if (args.len != 2) unreachable;
                    const seed_reg = try self.hasherStateReg(args[0]);
                    const value_loc = try self.emitValueLocal(args[1]);
                    const parts = try self.getI128Parts(value_loc, if (ll.op == .hasher_write_u128) .unsigned else .signed);
                    return try self.callHasherWriteU128(seed_reg, parts, hasherDomain(ll.op));
                },
                .hasher_write_bytes => {
                    if (args.len != 2) unreachable;
                    const seed_reg = try self.hasherStateReg(args[0]);
                    const list_loc = try self.emitValueLocal(args[1]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addRegArg(seed_reg);
                    try builder.addImmArg(@intCast(@intFromEnum(builtins.hash.HasherDomain.bytes)));
                    try builder.addMemArg(frame_ptr, list_off);
                    try builder.addMemArg(frame_ptr, list_off + 8);
                    try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_hasher_write_bytes), .hasher_write_bytes);
                    self.codegen.freeGeneral(seed_reg);
                    return try self.scalarRetReg();
                },
                .hasher_write_str => {
                    if (args.len != 2) unreachable;
                    const seed_reg = try self.hasherStateReg(args[0]);
                    const str_loc = try self.emitValueLocal(args[1]);
                    const str_off = try self.ensureOnStack(str_loc, roc_str_size);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addRegArg(seed_reg);
                    try builder.addMemArg(frame_ptr, str_off);
                    try builder.addMemArg(frame_ptr, str_off + 16);
                    try builder.addMemArg(frame_ptr, str_off + 8);
                    try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_hasher_write_str), .hasher_write_str);
                    self.codegen.freeGeneral(seed_reg);
                    return try self.scalarRetReg();
                },
                else => unreachable,
            }
        }

        fn generateCryptoLowLevel(self: *Self, ll: anytype, args: []const LocalId) Allocator.Error!ValueLocation {
            const Arity = enum { zero, one, two };
            const CryptoCall = struct {
                arity: Arity,
                addr: usize,
                builtin_fn: BuiltinFn,
            };
            const call: CryptoCall = switch (ll.op) {
                .crypto_sha256_hash_bytes => .{
                    .arity = Arity.one,
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_crypto_sha256_hash_bytes),
                    .builtin_fn = BuiltinFn.crypto_sha256_hash_bytes,
                },
                .crypto_sha256_hasher_empty => .{
                    .arity = Arity.zero,
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_crypto_sha256_hasher_empty),
                    .builtin_fn = BuiltinFn.crypto_sha256_hasher_empty,
                },
                .crypto_sha256_hasher_write => .{
                    .arity = Arity.two,
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_crypto_sha256_hasher_write),
                    .builtin_fn = BuiltinFn.crypto_sha256_hasher_write,
                },
                .crypto_sha256_hasher_finish => .{
                    .arity = Arity.one,
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_crypto_sha256_hasher_finish),
                    .builtin_fn = BuiltinFn.crypto_sha256_hasher_finish,
                },
                .crypto_blake3_hash_bytes => .{
                    .arity = Arity.one,
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_crypto_blake3_hash_bytes),
                    .builtin_fn = BuiltinFn.crypto_blake3_hash_bytes,
                },
                .crypto_blake3_hasher_empty => .{
                    .arity = Arity.zero,
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_crypto_blake3_hasher_empty),
                    .builtin_fn = BuiltinFn.crypto_blake3_hasher_empty,
                },
                .crypto_blake3_hasher_write => .{
                    .arity = Arity.two,
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_crypto_blake3_hasher_write),
                    .builtin_fn = BuiltinFn.crypto_blake3_hasher_write,
                },
                .crypto_blake3_hasher_finish => .{
                    .arity = Arity.one,
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_crypto_blake3_hasher_finish),
                    .builtin_fn = BuiltinFn.crypto_blake3_hasher_finish,
                },
                else => unreachable,
            };

            return switch (call.arity) {
                .zero => blk: {
                    if (args.len != 0) unreachable;
                    break :blk try self.callNoArgRocOpsToList(call.addr, call.builtin_fn);
                },
                .one => blk: {
                    if (args.len != 1) unreachable;
                    const list_loc = try self.emitValueLocal(args[0]);
                    const list_off = try self.ensureOnStack(list_loc, roc_list_size);
                    break :blk try self.callList1RocOpsToList(list_off, call.addr, call.builtin_fn);
                },
                .two => blk: {
                    if (args.len != 2) unreachable;
                    const first_loc = try self.emitValueLocal(args[0]);
                    const second_loc = try self.emitValueLocal(args[1]);
                    const first_off = try self.ensureOnStack(first_loc, roc_list_size);
                    const second_off = try self.ensureOnStack(second_loc, roc_list_size);
                    break :blk try self.callList2RocOpsToList(first_off, second_off, call.addr, call.builtin_fn);
                },
            };
        }

        // ── Helper methods for calling C wrapper builtins ──

        /// Call a C wrapper: fn(out, str_f0, str_f1, str_f2, roc_ops) -> void
        /// Used for str->str and str->list ops that take 1 string + roc_ops
        /// `update_mode_imm` is appended just before roc_ops for wrappers whose
        /// first argument carries the op's runtime uniqueness check.
        fn callStr1RocOpsToResult(self: *Self, str_off: i32, fn_addr: usize, builtin_fn: BuiltinFn, result_kind: enum { str, list }, update_mode_imm: ?i64) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            // fn(out, str_bytes, str_len, str_cap, [update_mode,] roc_ops)
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, result_offset);
            try builder.addMemArg(frame_ptr, str_off);
            try builder.addMemArg(frame_ptr, str_off + 16);
            try builder.addMemArg(frame_ptr, str_off + 8);
            if (update_mode_imm) |imm| try builder.addImmArg(imm);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return switch (result_kind) {
                .str => .{ .stack_str = result_offset },
                .list => .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } },
            };
        }

        /// Call a C wrapper: fn(out, list_bytes, list_len, list_cap, roc_ops) -> void
        /// Used for list->str ops that take 1 list + roc_ops.
        fn callList1RocOpsToStr(self: *Self, list_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, result_offset);
            try builder.addMemArg(frame_ptr, list_off);
            try builder.addMemArg(frame_ptr, list_off + 8);
            try builder.addMemArg(frame_ptr, list_off + 16);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .stack_str = result_offset };
        }

        /// Call a C wrapper: fn(out, roc_ops) -> void
        /// Used for no-arg ops returning List(U8).
        fn callNoArgRocOpsToList(self: *Self, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_list_size);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, result_offset);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Call a C wrapper: fn(out, list_bytes, list_len, list_cap, roc_ops) -> void
        /// Used for list->list ops that take 1 list + roc_ops.
        fn callList1RocOpsToList(self: *Self, list_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_list_size);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, result_offset);
            try builder.addMemArg(frame_ptr, list_off);
            try builder.addMemArg(frame_ptr, list_off + 8);
            try builder.addMemArg(frame_ptr, list_off + 16);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Call a C wrapper: fn(out, list1_bytes, list1_len, list1_cap, list2_bytes, list2_len, list2_cap, roc_ops) -> void
        /// Used for list+list->list ops.
        fn callList2RocOpsToList(self: *Self, first_off: i32, second_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_list_size);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, result_offset);
            try builder.addMemArg(frame_ptr, first_off);
            try builder.addMemArg(frame_ptr, first_off + 8);
            try builder.addMemArg(frame_ptr, first_off + 16);
            try builder.addMemArg(frame_ptr, second_off);
            try builder.addMemArg(frame_ptr, second_off + 8);
            try builder.addMemArg(frame_ptr, second_off + 16);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Call a C wrapper: fn(out, list_bytes, list_len, list_cap, str_bytes, str_len, str_cap, roc_ops) -> void
        /// Used for list+str->str ops.
        fn callListStrRocOpsToStr(self: *Self, list_off: i32, str_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_ptr, result_offset);
            try builder.addMemArg(base_ptr, list_off);
            try builder.addMemArg(base_ptr, list_off + 8);
            try builder.addMemArg(base_ptr, list_off + 16);
            try builder.addMemArg(base_ptr, str_off);
            try builder.addMemArg(base_ptr, str_off + 16);
            try builder.addMemArg(base_ptr, str_off + 8);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .stack_str = result_offset };
        }

        /// Call a C wrapper: fn(str_f0, str_f1, str_f2) -> scalar (bool or u64)
        /// Used for str->bool and str->u64 ops that take 1 string
        fn callStr1ToScalar(self: *Self, str_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            // fn(str_bytes, str_len, str_cap) -> scalar - 3 args
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addMemArg(base_ptr, str_off);
            try builder.addMemArg(base_ptr, str_off + 16);
            try builder.addMemArg(base_ptr, str_off + 8);
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
            try builder.addMemArg(base_ptr, a_off + 16);
            try builder.addMemArg(base_ptr, a_off + 8);
            try builder.addMemArg(base_ptr, b_off);
            try builder.addMemArg(base_ptr, b_off + 16);
            try builder.addMemArg(base_ptr, b_off + 8);
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

        /// Call a C wrapper: fn(str_f0, str_f1, str_f2, len, word0, word1, word2) -> bool
        /// Used by compiler-generated static small field dispatch.
        fn callStrStaticSmallToScalar(self: *Self, str_off: i32, len_off: i32, word0_off: i32, word1_off: i32, word2_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addMemArg(base_ptr, str_off);
            try builder.addMemArg(base_ptr, str_off + 16);
            try builder.addMemArg(base_ptr, str_off + 8);
            try builder.addMemArg(base_ptr, len_off);
            try builder.addMemArg(base_ptr, word0_off);
            try builder.addMemArg(base_ptr, word1_off);
            try builder.addMemArg(base_ptr, word2_off);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            const result_reg = try self.allocTempGeneral();
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, result_reg, .X0);
            } else {
                try self.codegen.emit.movRegReg(.w64, result_reg, .RAX);
                try self.codegen.emit.andRegImm8(result_reg, 1);
            }
            return .{ .general_reg = result_reg };
        }

        /// Call a C wrapper: fn(str_f0, str_f1, str_f2, offset, active_len, word) -> bool
        /// Used by compiler-generated static small field dispatch as a cheap discriminator.
        fn callStrStaticSmallWordToScalar(self: *Self, str_off: i32, offset_off: i32, active_len_off: i32, word_off: i32, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addMemArg(base_ptr, str_off);
            try builder.addMemArg(base_ptr, str_off + 16);
            try builder.addMemArg(base_ptr, str_off + 8);
            try builder.addMemArg(base_ptr, offset_off);
            try builder.addMemArg(base_ptr, active_len_off);
            try builder.addMemArg(base_ptr, word_off);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            const result_reg = try self.allocTempGeneral();
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, result_reg, .X0);
            } else {
                try self.codegen.emit.movRegReg(.w64, result_reg, .RAX);
                try self.codegen.emit.andRegImm8(result_reg, 1);
            }
            return .{ .general_reg = result_reg };
        }

        /// Call a C wrapper: fn(out, a_f0, a_f1, a_f2, b_f0, b_f1, b_f2, [update_mode,] roc_ops) -> void
        /// Used for (str, str, roc_ops) -> str/list ops
        fn callStr2RocOpsToStr(self: *Self, a_off: i32, b_off: i32, fn_addr: usize, builtin_fn: BuiltinFn, update_mode_imm: ?i64) Allocator.Error!ValueLocation {
            return self.callStr2RocOpsToResult(a_off, b_off, fn_addr, builtin_fn, .str, update_mode_imm);
        }

        /// `update_mode_imm` is appended just before roc_ops for wrappers whose
        /// first argument carries the op's runtime uniqueness check.
        fn callStr2RocOpsToResult(self: *Self, a_off: i32, b_off: i32, fn_addr: usize, builtin_fn: BuiltinFn, result_kind: enum { str, list }, update_mode_imm: ?i64) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            // fn(out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, [update_mode,] roc_ops) -> void
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_ptr, result_offset);
            try builder.addMemArg(base_ptr, a_off);
            try builder.addMemArg(base_ptr, a_off + 16);
            try builder.addMemArg(base_ptr, a_off + 8);
            try builder.addMemArg(base_ptr, b_off);
            try builder.addMemArg(base_ptr, b_off + 16);
            try builder.addMemArg(base_ptr, b_off + 8);
            if (update_mode_imm) |imm| try builder.addImmArg(imm);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return switch (result_kind) {
                .str => .{ .stack_str = result_offset },
                .list => .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } },
            };
        }

        /// Call: fn(out, str_f0, str_f1, str_f2, u64_val, [update_mode,] roc_ops) -> void
        /// Used for (str, u64, roc_ops) -> str ops like str_repeat, str_reserve.
        /// `update_mode_imm` is appended just before roc_ops for wrappers whose
        /// first argument carries the op's runtime uniqueness check.
        fn callStr1U64RocOpsToStr(self: *Self, str_off: i32, u64_off: i32, fn_addr: usize, builtin_fn: BuiltinFn, update_mode_imm: ?i64) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            // fn(out, str_bytes, str_len, str_cap, u64_val, [update_mode,] roc_ops) -> void
            const base_ptr = frame_ptr;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_ptr, result_offset);
            try builder.addMemArg(base_ptr, str_off);
            try builder.addMemArg(base_ptr, str_off + 16);
            try builder.addMemArg(base_ptr, str_off + 8);
            try builder.addMemArg(base_ptr, u64_off);
            if (update_mode_imm) |imm| try builder.addImmArg(imm);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .stack_str = result_offset };
        }

        fn normalizeIntegerWidthInReg(self: *Self, reg: GeneralReg, int_width_bytes: u8, is_signed: bool) Allocator.Error!void {
            const shift_amount: u6 = switch (int_width_bytes) {
                1 => 56,
                2 => 48,
                4 => 32,
                8, 16 => 0,
                else => unreachable,
            };
            if (shift_amount == 0) return;

            try self.emitShlImm(.w64, reg, reg, shift_amount);
            if (is_signed) {
                try self.emitAsrImm(.w64, reg, reg, shift_amount);
            } else {
                try self.emitLsrImm(.w64, reg, reg, shift_amount);
            }
        }

        fn callIntToStr(self: *Self, value_loc: ValueLocation, int_width_bytes: u8, is_signed: bool) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const builtin_fn: BuiltinFn = .int_to_str;
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_int_to_str);
            const base_reg = frame_ptr;

            const signedness: std.builtin.Signedness = if (is_signed) .signed else .unsigned;
            const low_reg, const high_reg = blk: {
                if (int_width_bytes == 16) {
                    const parts = try self.getI128Parts(value_loc, signedness);
                    break :blk .{ parts.low, parts.high };
                }

                const low = try self.ensureInGeneralReg(value_loc);
                try self.normalizeIntegerWidthInReg(low, int_width_bytes, is_signed);
                const high = try self.allocTempGeneral();
                try self.emitSignExtendHighReg(high, low, signedness);
                break :blk .{ low, high };
            };
            defer self.codegen.freeGeneral(low_reg);
            defer self.codegen.freeGeneral(high_reg);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_offset);
            try builder.addRegArg(low_reg);
            try builder.addRegArg(high_reg);
            try builder.addImmArg(int_width_bytes);
            try builder.addImmArg(if (is_signed) @as(u8, 1) else @as(u8, 0));
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            return .{ .stack_str = result_offset };
        }

        fn callDecToStr(self: *Self, value_loc: ValueLocation) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_dec_to_str);
            const base_reg = frame_ptr;
            const parts = try self.getI128Parts(value_loc, .signed);
            defer self.codegen.freeGeneral(parts.low);
            defer self.codegen.freeGeneral(parts.high);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_offset);
            try builder.addRegArg(parts.low);
            try builder.addRegArg(parts.high);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .dec_to_str);

            return .{ .stack_str = result_offset };
        }

        fn callFloatToStr(self: *Self, value_loc: ValueLocation, is_f32: bool) Allocator.Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_float_to_str);
            const base_reg = frame_ptr;
            const bits_reg = if (is_f32)
                try self.materializeF32BitsInGeneralReg(value_loc)
            else
                try self.ensureInGeneralReg(value_loc);
            defer self.codegen.freeGeneral(bits_reg);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_offset);
            try builder.addRegArg(bits_reg);
            try builder.addImmArg(if (is_f32) @as(u8, 1) else @as(u8, 0));
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, .float_to_str);

            return .{ .stack_str = result_offset };
        }

        /// Helper for list_drop_first, list_drop_last, list_take_first, list_take_last
        /// These all map to listSublist with different start/len calculations
        fn callListSublist(self: *Self, ll: anytype, list_loc: ValueLocation, n_loc: ValueLocation, mode: enum { drop_first, drop_last, take_first, take_last }) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const list_abi = builtinInternalListAbi(ls, "dev.callListSublist.builtin_list_abi", ll.ret_layout);

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
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_sublist);
            const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
            defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

            {
                // roc_builtins_list_sublist(out, list_bytes, list_len, list_cap,
                // alignment, element_width, start, len, elements_refcounted, element_decref, update_mode, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                try builder.addMemArg(base_reg, start_slot);
                try builder.addMemArg(base_reg, len_slot);
                try builder.addImmArg(if (list_abi.elements_refcounted) 1 else 0);
                if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                try builder.addRegArg(roc_ops_reg);

                try self.callBuiltin(&builder, fn_addr, .list_sublist);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// list_sublist(list, {start, len}) -> List
        fn callListSublistFromRecord(self: *Self, ll: anytype, list_loc: ValueLocation, record_loc: ValueLocation, record_layout_idx: ?layout.Idx) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const list_abi = builtinInternalListAbi(ls, "dev.callListSublistFromRecord.builtin_list_abi", ll.ret_layout);

            const record_layout = ls.getLayout(record_layout_idx orelse unreachable);
            const record_idx = record_layout.getStruct().idx;
            const record_size = ls.getStructData(record_idx).size.get(ls.targetUsize());
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
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_sublist);
            const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
            defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

            {
                // roc_builtins_list_sublist(out, list_bytes, list_len, list_cap,
                // alignment, element_width, start, len, elements_refcounted, element_decref, update_mode, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                try builder.addMemArg(base_reg, record_off + start_field_off);
                try builder.addMemArg(base_reg, record_off + len_field_off);
                try builder.addImmArg(if (list_abi.elements_refcounted) 1 else 0);
                if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, fn_addr, .list_sublist);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        fn callListDropAt(self: *Self, ll: anytype, list_loc: ValueLocation, index_loc: ValueLocation) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const list_abi = builtinInternalListAbi(ls, "dev.callListDropAt.builtin_list_abi", ll.ret_layout);

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const index_off = try self.ensureOnStack(index_loc, 8);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_drop_at);
            const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
            defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
            const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
            defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

            {
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                try builder.addMemArg(base_reg, index_off);
                try builder.addImmArg(if (list_abi.elements_refcounted) 1 else 0);
                if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, fn_addr, .list_drop_at);
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
            const record_idx = ret_layout.getStruct().idx;
            const record_data = ls.getStructData(record_idx);
            const result_size: u32 = record_data.size.get(ls.targetUsize());

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
            const list_field_layout_idx = if (field0_is_list) field0_layout_idx else field1_layout_idx;
            const list_abi = builtinInternalListAbi(ls, "dev.callListSplitOp.builtin_list_abi", list_field_layout_idx);

            const elem_size_align = ls.layoutSizeAlign(elem_layout);
            const elem_size: u32 = elem_size_align.size;

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

            const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
            defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

            {
                // roc_builtins_list_sublist(out, list_bytes, list_len, list_cap,
                // alignment, element_width, start, len, elements_refcounted, element_decref, update_mode, roc_ops)
                const list_dst_offset = result_offset + list_field_offset;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(frame_ptr, list_dst_offset);
                try builder.addMemArg(frame_ptr, list_off);
                try builder.addMemArg(frame_ptr, list_off + 8);
                try builder.addMemArg(frame_ptr, list_off + 16);
                try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                try builder.addImmArg(@intCast(elem_size));
                try builder.addMemArg(frame_ptr, start_slot);
                try builder.addMemArg(frame_ptr, sublist_len_slot);
                try builder.addImmArg(if (list_abi.elements_refcounted) 1 else 0);
                if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
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

        /// Generate list_reverse via the listReverse builtin: in place when the
        /// list is unique (or statically proven unique), copy-on-write otherwise.
        fn generateListReverse(self: *Self, list_loc: ValueLocation, ll: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const list_abi = builtinInternalListAbi(ls, "dev.generateListReverse.builtin_list_abi", ll.ret_layout);

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);

            if (list_abi.elem_size_align.size == 0) {
                // ZST: the reversed list is the input list (length unchanged, no data).
                const tr = try self.allocTempGeneral();
                try self.emitLoad(.w64, tr, frame_ptr, list_off);
                try self.emitStore(.w64, frame_ptr, result_offset, tr);
                try self.emitLoad(.w64, tr, frame_ptr, list_off + 8);
                try self.emitStore(.w64, frame_ptr, result_offset + 8, tr);
                try self.emitLoad(.w64, tr, frame_ptr, list_off + 16);
                try self.emitStore(.w64, frame_ptr, result_offset + 16, tr);
                self.codegen.freeGeneral(tr);
                return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
            }

            const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
            defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
            const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
            defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

            {
                // roc_builtins_list_reverse(out, list_bytes, list_len, list_cap,
                // alignment, element_width, elements_refcounted, element_incref, element_decref, update_mode, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                try builder.addImmArg(if (list_abi.elements_refcounted) 1 else 0);
                if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_list_reverse), .list_reverse);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Call list_reserve wrapper
        fn callListReserveOp(self: *Self, list_loc: ValueLocation, spare_loc: ValueLocation, ll: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const list_abi = builtinInternalListAbi(ls, "dev.callListReserveOp.builtin_list_abi", ll.ret_layout);

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const spare_off = try self.ensureOnStack(spare_loc, 8);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_reserve);
            const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
            defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
            const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
            defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

            {
                // wrapListReserve(out, list_bytes, list_len, list_cap, alignment, spare, element_width, elements_refcounted, element_incref, element_decref, update_mode, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                try builder.addMemArg(base_reg, spare_off);
                try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                try builder.addImmArg(if (list_abi.elements_refcounted) @as(usize, 1) else 0);
                if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
                try builder.addRegArg(roc_ops_reg);

                try self.callBuiltin(&builder, fn_addr, .list_reserve);
            }

            return .{ .list_stack = .{ .struct_offset = result_offset, .data_offset = 0, .num_elements = 0 } };
        }

        /// Call list_release_excess_capacity wrapper
        fn callListReleaseExcessCapOp(self: *Self, list_loc: ValueLocation, ll: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const list_abi = builtinInternalListAbi(ls, "dev.callListReleaseExcessCapOp.builtin_list_abi", ll.ret_layout);

            const list_off = try self.ensureOnStack(list_loc, roc_list_size);
            const result_offset = self.codegen.allocStackSlot(roc_str_size);
            const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_list_release_excess_capacity);
            const elem_incref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.incref, idx) else null;
            defer if (elem_incref_reg) |reg| self.codegen.freeGeneral(reg);
            const elem_decref_reg = if (list_abi.elem_layout_idx) |idx| try self.emitBuiltinInternalOptionalRcHelperAddress(.decref, idx) else null;
            defer if (elem_decref_reg) |reg| self.codegen.freeGeneral(reg);

            {
                // wrapListReleaseExcessCapacity(out, list_bytes, list_len, list_cap, alignment, element_width, elements_refcounted, element_incref, element_decref, update_mode, roc_ops)
                const base_reg = frame_ptr;
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

                try builder.addLeaArg(base_reg, result_offset);
                try builder.addMemArg(base_reg, list_off);
                try builder.addMemArg(base_reg, list_off + 8);
                try builder.addMemArg(base_reg, list_off + 16);
                try builder.addImmArg(@intCast(list_abi.alignment_bytes));
                try builder.addImmArg(@intCast(list_abi.elem_size_align.size));
                try builder.addImmArg(if (list_abi.elements_refcounted) @as(usize, 1) else 0);
                if (elem_incref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                if (elem_decref_reg) |reg| try builder.addRegArg(reg) else try builder.addImmArg(0);
                try builder.addImmArg(updateModeImmForArg0(ll.unique_args));
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

        /// Generate code for a local lookup.
        fn generateLookup(self: *Self, local: LocalId) Allocator.Error!ValueLocation {
            const layout_idx = self.localLayout(local);
            if (self.local_locations.get(localKey(local))) |loc| {
                if (loc == .list_stack) {} else if (loc == .stack) {}
                return loc;
            }

            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "generateLookup: missing local location for local={d} layout={d} current_proc={d} current_stmt={d} current_stmt_tag={s}",
                    .{
                        @intFromEnum(local),
                        @intFromEnum(layout_idx),
                        if (self.current_proc_name) |sym| sym.raw() else std.math.maxInt(u64),
                        if (self.current_stmt_id) |stmt_id| @intFromEnum(stmt_id) else std.math.maxInt(u32),
                        if (self.current_stmt_id) |stmt_id|
                            @tagName(self.store.getCFStmt(stmt_id))
                        else
                            "none",
                    },
                );
            }
            unreachable;
        }

        fn bindAssignedLocal(self: *Self, local: LocalId, value_loc: ValueLocation) Allocator.Error!void {
            const key = localKey(local);
            const local_layout = self.localLayout(local);
            if (self.local_locations.get(key)) |stable_loc| {
                try self.storeValueIntoStableLocation(stable_loc, value_loc, local_layout);
                try self.emitDebugAssertValidBoxLocal(local, stable_loc);
                try self.emitDebugAssertValidStrLocal(local, stable_loc);
                return;
            }

            const stable_loc = try self.materializeValueToStackForLayout(value_loc, local_layout);
            try self.local_locations.put(key, stable_loc);
            try self.emitDebugAssertValidBoxLocal(local, stable_loc);
            try self.emitDebugAssertValidStrLocal(local, stable_loc);
        }

        fn emitDebugAssertValidBoxLocal(
            self: *Self,
            local: LocalId,
            stable_loc: ValueLocation,
        ) Allocator.Error!void {
            if (comptime builtin.mode != .Debug) return;

            const local_layout = self.localLayout(local);
            const layout_val = self.layout_store.getLayout(local_layout);
            if (layout_val.tag != .box) return;

            const slot_offset: i32 = switch (stable_loc) {
                .stack => |s| s.offset,
                else => std.debug.panic(
                    "LIR/codegen invariant violated: box local {d} did not lower to a stack stable location",
                    .{@intFromEnum(local)},
                ),
            };

            const ptr_reg = try self.allocTempGeneral();
            const masked_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(masked_reg);
            defer self.codegen.freeGeneral(ptr_reg);

            try self.emitLoad(.w64, ptr_reg, frame_ptr, slot_offset);
            try self.emitCmpImm(ptr_reg, 0);
            const null_patch = try self.emitJumpIfEqual();

            try self.codegen.emitLoadImm(masked_reg, 7);
            try self.emitAndRegs(.w64, masked_reg, masked_reg, ptr_reg);
            try self.emitCmpImm(masked_reg, 0);
            const aligned_patch = try self.emitJumpIfEqual();

            const msg = try std.fmt.allocPrint(
                self.allocator,
                "LIR/codegen invariant violated: box local {d} received a non-aligned pointer at proc {d} stmt {d}",
                .{
                    @intFromEnum(local),
                    if (self.current_proc_name) |sym| sym.raw() else std.math.maxInt(u64),
                    if (self.current_stmt_id) |stmt_id| @intFromEnum(stmt_id) else std.math.maxInt(u32),
                },
            );
            defer self.allocator.free(msg);
            try self.emitRocCrashShared(msg);
            try self.emitTrap();

            const done = self.codegen.currentOffset();
            self.codegen.patchJump(aligned_patch, done);
            self.codegen.patchJump(null_patch, done);
        }

        fn emitDebugAssertValidStrLocal(
            self: *Self,
            local: LocalId,
            stable_loc: ValueLocation,
        ) Allocator.Error!void {
            if (comptime builtin.mode != .Debug) return;

            const runtime_layout = self.runtimeRepresentationLayoutIdx(self.localLayout(local));
            if (runtime_layout != .str) return;

            const slot_offset: i32 = switch (stable_loc) {
                .stack_str => |offset| offset,
                else => std.debug.panic(
                    "LIR/codegen invariant violated: str local {d} did not lower to a stack_str stable location",
                    .{@intFromEnum(local)},
                ),
            };

            const ptr_reg = try self.allocTempGeneral();
            const len_reg = try self.allocTempGeneral();
            const cap_reg = try self.allocTempGeneral();
            const tmp_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(tmp_reg);
            defer self.codegen.freeGeneral(cap_reg);
            defer self.codegen.freeGeneral(len_reg);
            defer self.codegen.freeGeneral(ptr_reg);

            try self.emitLoad(.w64, ptr_reg, frame_ptr, slot_offset);
            try self.emitLoad(.w64, cap_reg, frame_ptr, slot_offset + 8);
            try self.emitLoad(.w64, len_reg, frame_ptr, slot_offset + 16);

            // Small RocStrs are stored inline and identified by the sign bit of length.
            try self.codegen.emitLoadImm(tmp_reg, std.math.minInt(i64));
            try self.emitAndRegs(.w64, tmp_reg, tmp_reg, len_reg);
            try self.emitCmpImm(tmp_reg, 0);
            const small_patch = try self.emitJumpIfNotEqual();

            // Non-small RocStrs must have a non-null bytes pointer.
            try self.emitCmpImm(ptr_reg, 0);
            const ptr_non_null_patch = try self.emitJumpIfNotEqual();
            try self.emitDebugCrashInvalidStrLocal(local, "null bytes pointer");
            const after_null = self.codegen.currentOffset();
            self.codegen.patchJump(ptr_non_null_patch, after_null);

            // Seamless slices store an interior bytes pointer plus the original
            // allocation pointer in capacity_or_alloc_ptr. Their bytes pointer
            // may be arbitrarily offset, so validate the stored allocation
            // pointer instead of requiring bytes alignment.
            try self.codegen.emitLoadImm(tmp_reg, 1);
            try self.emitAndRegs(.w64, tmp_reg, tmp_reg, cap_reg);
            try self.emitCmpImm(tmp_reg, 0);
            const non_seamless_patch = try self.emitJumpIfEqual();

            try self.codegen.emitLoadImm(tmp_reg, -2);
            try self.emitAndRegs(.w64, tmp_reg, tmp_reg, cap_reg);
            try self.emitCmpImm(tmp_reg, 0);
            const alloc_non_null_patch = try self.emitJumpIfNotEqual();
            try self.emitDebugCrashInvalidStrLocal(local, "null allocation pointer");
            const after_alloc_null = self.codegen.currentOffset();
            self.codegen.patchJump(alloc_non_null_patch, after_alloc_null);

            try self.codegen.emitLoadImm(ptr_reg, @alignOf(usize) - 1);
            try self.emitAndRegs(.w64, ptr_reg, ptr_reg, tmp_reg);
            try self.emitCmpImm(ptr_reg, 0);
            const alloc_aligned_patch = try self.emitJumpIfEqual();
            try self.emitDebugCrashInvalidStrLocal(local, "misaligned allocation pointer");
            const after_alloc_align = self.codegen.currentOffset();
            self.codegen.patchJump(alloc_aligned_patch, after_alloc_align);

            const seamless_done_patch = try self.codegen.emitJump();
            const after_seamless = self.codegen.currentOffset();
            self.codegen.patchJump(non_seamless_patch, after_seamless);

            // Non-slice RocStrs must satisfy len <= decoded capacity.
            try self.codegen.emitLoadImm(tmp_reg, @alignOf(usize) - 1);
            try self.emitAndRegs(.w64, tmp_reg, tmp_reg, ptr_reg);
            try self.emitCmpImm(tmp_reg, 0);
            const ptr_aligned_patch = try self.emitJumpIfEqual();
            try self.emitDebugCrashInvalidStrLocal(local, "misaligned bytes pointer");
            const after_ptr_align = self.codegen.currentOffset();
            self.codegen.patchJump(ptr_aligned_patch, after_ptr_align);

            try self.emitLsrImm(.w64, tmp_reg, cap_reg, 1);
            try self.emitCmpReg(len_reg, tmp_reg);
            const len_ok_patch = try self.codegen.emitCondJump(condBelowOrEqual());
            try self.emitDebugCrashInvalidStrLocal(local, "length exceeds capacity");
            const done = self.codegen.currentOffset();
            self.codegen.patchJump(len_ok_patch, done);
            self.codegen.patchJump(seamless_done_patch, done);
            self.codegen.patchJump(small_patch, done);
        }

        fn emitDebugCrashInvalidStrLocal(self: *Self, local: LocalId, reason: []const u8) Allocator.Error!void {
            const msg = try std.fmt.allocPrint(
                self.allocator,
                "LIR/codegen invariant violated: str local {d} received an invalid RocStr ({s}) at proc {d} stmt {d}",
                .{
                    @intFromEnum(local),
                    reason,
                    if (self.current_proc_name) |sym| sym.raw() else std.math.maxInt(u64),
                    if (self.current_stmt_id) |stmt_id| @intFromEnum(stmt_id) else std.math.maxInt(u32),
                },
            );
            defer self.allocator.free(msg);
            try self.emitRocCrashShared(msg);
            try self.emitTrap();
        }

        fn storeValueIntoStableLocation(
            self: *Self,
            stable_loc: ValueLocation,
            value_loc: ValueLocation,
            layout_idx: layout.Idx,
        ) Allocator.Error!void {
            const normalized = self.coerceImmediateToLayout(value_loc, layout_idx);
            if (std.meta.eql(normalized, stable_loc)) return;

            const size = self.getLayoutSize(layout_idx);
            if (size == 0) return;

            switch (stable_loc) {
                .stack => |stack_loc| try self.copyBytesToStackOffset(stack_loc.offset, normalized, size),
                .stack_i128 => |offset| {
                    const runtime_layout_idx = self.runtimeRepresentationLayoutIdx(layout_idx);
                    try self.storeWideScalarToStackOffset(
                        offset,
                        normalized,
                        switch (runtime_layout_idx) {
                            .u128 => .unsigned,
                            .i128, .dec => .signed,
                            else => std.debug.panic(
                                "LirCodeGen invariant violated: stack_i128 stable location used for non-wide layout {d}",
                                .{@intFromEnum(runtime_layout_idx)},
                            ),
                        },
                    );
                },
                .stack_str => |offset| try self.copyBytesToStackOffset(offset, normalized, roc_str_size),
                .list_stack => |list_loc| try self.copyBytesToStackOffset(list_loc.struct_offset, normalized, roc_list_size),
                else => std.debug.panic(
                    "LirCodeGen invariant violated: assigned locals must use stack-backed stable locations, found {s}",
                    .{@tagName(stable_loc)},
                ),
            }
        }

        fn storeWideScalarToStackOffset(
            self: *Self,
            dest_offset: i32,
            loc: ValueLocation,
            signedness: std.builtin.Signedness,
        ) Allocator.Error!void {
            switch (loc) {
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    const reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(reg);
                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                },
                .immediate_i64 => |val| {
                    const low: u64 = @bitCast(val);
                    const high: u64 = switch (signedness) {
                        .signed => if (val < 0) std.math.maxInt(u64) else 0,
                        .unsigned => 0,
                    };
                    const reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(reg);
                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                },
                .general_reg => |reg| {
                    const high_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(high_reg);
                    try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    switch (signedness) {
                        .signed => {
                            if (comptime target.toCpuArch() == .aarch64) {
                                try self.codegen.emit.asrRegRegImm(.w64, high_reg, reg, 63);
                            } else {
                                try self.emitMovRegReg(high_reg, reg);
                                try self.codegen.emit.sarRegImm8(.w64, high_reg, 63);
                            }
                        },
                        .unsigned => try self.codegen.emitLoadImm(high_reg, 0),
                    }
                    try self.codegen.emitStoreStack(.w64, dest_offset + 8, high_reg);
                },
                .stack_i128 => |src_offset| {
                    const temp_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(temp_reg);
                    try self.codegen.emitLoadStack(.w64, temp_reg, src_offset);
                    try self.codegen.emitStoreStack(.w64, dest_offset, temp_reg);
                    try self.codegen.emitLoadStack(.w64, temp_reg, src_offset + 8);
                    try self.codegen.emitStoreStack(.w64, dest_offset + 8, temp_reg);
                },
                .stack => |stack_loc| {
                    const temp_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(temp_reg);
                    try self.codegen.emitLoadStack(.w64, temp_reg, stack_loc.offset);
                    try self.codegen.emitStoreStack(.w64, dest_offset, temp_reg);
                    try self.codegen.emitLoadStack(.w64, temp_reg, stack_loc.offset + 8);
                    try self.codegen.emitStoreStack(.w64, dest_offset + 8, temp_reg);
                },
                else => std.debug.panic(
                    "LirCodeGen invariant violated: unsupported wide scalar spill from {s}",
                    .{@tagName(loc)},
                ),
            }
        }

        fn ensureStableLocationForLocal(self: *Self, local: LocalId) Allocator.Error!void {
            const key = localKey(local);
            if (self.local_locations.contains(key)) return;

            const local_layout = self.localLayout(local);
            const size = self.getLayoutSize(local_layout);
            const stable_loc = if (size == 0)
                ValueLocation{ .immediate_i64 = 0 }
            else
                self.stackLocationForLayout(local_layout, self.codegen.allocStackSlot(size));
            try self.local_locations.put(key, stable_loc);
        }

        fn stableLocationStackOffset(stable_loc: ValueLocation) i32 {
            return switch (stable_loc) {
                .stack => |stack_loc| stack_loc.offset,
                .stack_i128 => |offset| offset,
                .stack_str => |offset| offset,
                .list_stack => |list_loc| list_loc.struct_offset,
                else => std.debug.panic(
                    "LirCodeGen invariant violated: uninitialized local used non-stack stable location {s}",
                    .{@tagName(stable_loc)},
                ),
            };
        }

        fn poisonUninitializedLocal(self: *Self, local: LocalId) Allocator.Error!void {
            const layout_idx = self.localLayout(local);
            const size = self.getLayoutSize(layout_idx);
            if (size == 0) return;

            try self.ensureStableLocationForLocal(local);
            const stable_loc = self.local_locations.get(localKey(local)) orelse unreachable;
            try self.poisonStackArea(stableLocationStackOffset(stable_loc), size);
        }

        fn collectStmtReadLocals(
            self: *Self,
            root_stmt_id: CFStmtId,
            locals: *std.AutoHashMap(u64, LocalId),
            visited: *std.AutoHashMap(u32, void),
        ) Allocator.Error!void {
            var sfa = std.heap.stackFallback(64 * @sizeOf(CFStmtId), self.allocator);
            const sa = sfa.get();
            var stack = std.ArrayList(CFStmtId).empty;
            defer stack.deinit(sa);
            try stack.append(sa, root_stmt_id);

            while (stack.pop()) |stmt_id| {
                const gop = try visited.getOrPut(@intFromEnum(stmt_id));
                if (gop.found_existing) continue;

                switch (self.store.getCFStmt(stmt_id)) {
                    .assign_ref => |assign| {
                        try locals.put(localKey(refOpSource(assign.op)), refOpSource(assign.op));
                        try stack.append(sa, assign.next);
                    },
                    .assign_literal => |assign| try stack.append(sa, assign.next),
                    .init_uninitialized => |uninit| try stack.append(sa, uninit.next),
                    .assign_call => |assign| {
                        for (self.store.getLocalSpan(assign.args)) |arg| {
                            try locals.put(localKey(arg), arg);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_call_erased => |assign| {
                        try locals.put(localKey(assign.closure), assign.closure);
                        for (self.store.getLocalSpan(assign.args)) |arg| {
                            try locals.put(localKey(arg), arg);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_packed_erased_fn => |assign| {
                        if (assign.capture) |capture| {
                            try locals.put(localKey(capture), capture);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_low_level => |assign| {
                        for (self.store.getLocalSpan(assign.args)) |arg| {
                            try locals.put(localKey(arg), arg);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_list => |assign| {
                        for (self.store.getLocalSpan(assign.elems)) |elem| {
                            try locals.put(localKey(elem), elem);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_struct => |assign| {
                        for (self.store.getLocalSpan(assign.fields)) |field| {
                            try locals.put(localKey(field), field);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_tag => |assign| {
                        if (assign.payload) |payload| try locals.put(localKey(payload), payload);
                        try stack.append(sa, assign.next);
                    },
                    .set_local => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        try locals.put(localKey(assign.value), assign.value);
                        try stack.append(sa, assign.next);
                    },
                    .debug => |debug_stmt| {
                        try locals.put(localKey(debug_stmt.message), debug_stmt.message);
                        try stack.append(sa, debug_stmt.next);
                    },
                    .expect_err => |expect_err_stmt| {
                        try locals.put(localKey(expect_err_stmt.message), expect_err_stmt.message);
                    },
                    .expect => |expect_stmt| {
                        try locals.put(localKey(expect_stmt.condition), expect_stmt.condition);
                        try stack.append(sa, expect_stmt.next);
                    },
                    .comptime_branch_taken => |marker| try stack.append(sa, marker.next),
                    .runtime_error, .comptime_exhaustiveness_failed => {},
                    .incref => |inc| {
                        try locals.put(localKey(inc.value), inc.value);
                        try stack.append(sa, inc.next);
                    },
                    .decref => |dec| {
                        try locals.put(localKey(dec.value), dec.value);
                        try stack.append(sa, dec.next);
                    },
                    .decref_if_initialized => |dec| {
                        try locals.put(localKey(dec.cond), dec.cond);
                        try locals.put(localKey(dec.value), dec.value);
                        try stack.append(sa, dec.next);
                    },
                    .free => |free_stmt| {
                        try locals.put(localKey(free_stmt.value), free_stmt.value);
                        try stack.append(sa, free_stmt.next);
                    },
                    .switch_stmt => |sw| {
                        try locals.put(localKey(sw.cond), sw.cond);
                        for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                            try stack.append(sa, branch.body);
                        }
                        try stack.append(sa, sw.default_branch);
                    },
                    .switch_initialized_payload => |sw| {
                        try locals.put(localKey(sw.cond), sw.cond);
                        try locals.put(localKey(sw.payload), sw.payload);
                        try stack.append(sa, sw.initialized_branch);
                        try stack.append(sa, sw.uninitialized_branch);
                    },
                    .str_match => |str_match| {
                        try locals.put(localKey(str_match.source), str_match.source);
                        try stack.append(sa, str_match.on_match);
                        try stack.append(sa, str_match.on_miss);
                    },
                    .str_match_set => |str_match_set| {
                        try locals.put(localKey(str_match_set.source), str_match_set.source);
                        for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                            try stack.append(sa, arm.on_match);
                        }
                        try stack.append(sa, str_match_set.on_miss);
                    },
                    .join => |join| {
                        try stack.append(sa, join.body);
                        try stack.append(sa, join.remainder);
                    },
                    .jump => {},
                    .ret => |ret_stmt| try locals.put(localKey(ret_stmt.value), ret_stmt.value),
                    .crash => {},
                    .loop_continue => {},
                    .loop_break => {},
                }
            }
        }

        fn collectStmtLocals(
            self: *Self,
            root_stmt_id: CFStmtId,
            locals: *std.AutoHashMap(u64, LocalId),
            visited: *std.AutoHashMap(u32, void),
        ) Allocator.Error!void {
            var sfa = std.heap.stackFallback(64 * @sizeOf(CFStmtId), self.allocator);
            const sa = sfa.get();
            var stack = std.ArrayList(CFStmtId).empty;
            defer stack.deinit(sa);
            try stack.append(sa, root_stmt_id);

            while (stack.pop()) |stmt_id| {
                const gop = try visited.getOrPut(@intFromEnum(stmt_id));
                if (gop.found_existing) continue;

                switch (self.store.getCFStmt(stmt_id)) {
                    .assign_ref => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        try locals.put(localKey(refOpSource(assign.op)), refOpSource(assign.op));
                        try stack.append(sa, assign.next);
                    },
                    .assign_literal => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        try stack.append(sa, assign.next);
                    },
                    .init_uninitialized => |uninit| {
                        try locals.put(localKey(uninit.target), uninit.target);
                        try stack.append(sa, uninit.next);
                    },
                    .assign_call => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        for (self.store.getLocalSpan(assign.args)) |arg| {
                            try locals.put(localKey(arg), arg);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_call_erased => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        try locals.put(localKey(assign.closure), assign.closure);
                        for (self.store.getLocalSpan(assign.args)) |arg| {
                            try locals.put(localKey(arg), arg);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_packed_erased_fn => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        if (assign.capture) |capture| try locals.put(localKey(capture), capture);
                        try stack.append(sa, assign.next);
                    },
                    .assign_low_level => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        for (self.store.getLocalSpan(assign.args)) |arg| {
                            try locals.put(localKey(arg), arg);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_list => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        for (self.store.getLocalSpan(assign.elems)) |elem| {
                            try locals.put(localKey(elem), elem);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_struct => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        for (self.store.getLocalSpan(assign.fields)) |field| {
                            try locals.put(localKey(field), field);
                        }
                        try stack.append(sa, assign.next);
                    },
                    .assign_tag => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        if (assign.payload) |payload| try locals.put(localKey(payload), payload);
                        try stack.append(sa, assign.next);
                    },
                    .set_local => |assign| {
                        try locals.put(localKey(assign.target), assign.target);
                        try locals.put(localKey(assign.value), assign.value);
                        try stack.append(sa, assign.next);
                    },
                    .debug => |debug_stmt| {
                        try locals.put(localKey(debug_stmt.message), debug_stmt.message);
                        try stack.append(sa, debug_stmt.next);
                    },
                    .expect_err => |expect_err_stmt| {
                        try locals.put(localKey(expect_err_stmt.message), expect_err_stmt.message);
                    },
                    .expect => |expect_stmt| {
                        try locals.put(localKey(expect_stmt.condition), expect_stmt.condition);
                        try stack.append(sa, expect_stmt.next);
                    },
                    .comptime_branch_taken => |marker| try stack.append(sa, marker.next),
                    .runtime_error, .comptime_exhaustiveness_failed => {},
                    .incref => |inc| {
                        try locals.put(localKey(inc.value), inc.value);
                        try stack.append(sa, inc.next);
                    },
                    .decref => |dec| {
                        try locals.put(localKey(dec.value), dec.value);
                        try stack.append(sa, dec.next);
                    },
                    .decref_if_initialized => |dec| {
                        try locals.put(localKey(dec.cond), dec.cond);
                        try locals.put(localKey(dec.value), dec.value);
                        try stack.append(sa, dec.next);
                    },
                    .free => |free_stmt| {
                        try locals.put(localKey(free_stmt.value), free_stmt.value);
                        try stack.append(sa, free_stmt.next);
                    },
                    .switch_stmt => |sw| {
                        try locals.put(localKey(sw.cond), sw.cond);
                        for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                            try stack.append(sa, branch.body);
                        }
                        try stack.append(sa, sw.default_branch);
                    },
                    .switch_initialized_payload => |sw| {
                        try locals.put(localKey(sw.cond), sw.cond);
                        try locals.put(localKey(sw.payload), sw.payload);
                        try stack.append(sa, sw.initialized_branch);
                        try stack.append(sa, sw.uninitialized_branch);
                    },
                    .str_match => |str_match| {
                        try locals.put(localKey(str_match.source), str_match.source);
                        for (self.store.getStrMatchSteps(str_match.steps)) |step| {
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| try locals.put(localKey(local), local),
                            }
                        }
                        try stack.append(sa, str_match.on_match);
                        try stack.append(sa, str_match.on_miss);
                    },
                    .str_match_set => |str_match_set| {
                        try locals.put(localKey(str_match_set.source), str_match_set.source);
                        for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                            for (self.store.getStrMatchSteps(arm.steps)) |step| {
                                switch (step.capture) {
                                    .discard => {},
                                    .view => |local| try locals.put(localKey(local), local),
                                }
                            }
                            try stack.append(sa, arm.on_match);
                        }
                        try stack.append(sa, str_match_set.on_miss);
                    },
                    .join => |join| {
                        for (self.store.getLocalSpan(join.params)) |param| {
                            try locals.put(localKey(param), param);
                        }
                        try stack.append(sa, join.body);
                        try stack.append(sa, join.remainder);
                    },
                    .jump => {},
                    .ret => |ret_stmt| try locals.put(localKey(ret_stmt.value), ret_stmt.value),
                    .crash => {},
                    .loop_continue => {},
                    .loop_break => {},
                }
            }
        }

        fn refOpSource(op: lir.RefOp) LocalId {
            return switch (op) {
                .local => |local| local,
                .discriminant => |disc| disc.source,
                .field => |field| field.source,
                .tag_payload => |payload| payload.source,
                .tag_payload_struct => |payload| payload.source,
                .list_reinterpret => |list_reinterpret| list_reinterpret.backing_ref,
                .nominal => |nominal| nominal.backing_ref,
            };
        }

        fn ensureStableLocationsForStmtReads(self: *Self, stmt_id: CFStmtId) Allocator.Error!void {
            var locals = std.AutoHashMap(u64, LocalId).init(self.allocator);
            defer locals.deinit();
            var visited = std.AutoHashMap(u32, void).init(self.allocator);
            defer visited.deinit();

            try self.collectStmtReadLocals(stmt_id, &locals, &visited);

            var it = locals.valueIterator();
            while (it.next()) |local| {
                try self.ensureStableLocationForLocal(local.*);
            }
        }

        fn ensureStableLocationsForStmtLocals(self: *Self, stmt_id: CFStmtId) Allocator.Error!void {
            var locals = std.AutoHashMap(u64, LocalId).init(self.allocator);
            defer locals.deinit();
            var visited = std.AutoHashMap(u32, void).init(self.allocator);
            defer visited.deinit();

            try self.collectStmtLocals(stmt_id, &locals, &visited);

            var it = locals.valueIterator();
            while (it.next()) |local| {
                try self.ensureStableLocationForLocal(local.*);
            }
        }

        fn generateRefOp(self: *Self, op: lir.RefOp, target_layout: layout.Idx) Allocator.Error!ValueLocation {
            return switch (op) {
                .local => |local| blk: {
                    const raw_loc = try self.emitValueLocal(local);
                    break :blk self.requireExactValueLocationToLayout(
                        raw_loc,
                        self.localLayout(local),
                        target_layout,
                        "assign_ref.local",
                    );
                },
                .discriminant => |disc| try self.generateDiscriminantAccess(.{
                    .source = disc.source,
                    .target_layout = target_layout,
                }),
                .field => |field| try self.generateStructAccess(.{
                    .source = field.source,
                    .field_idx = field.field_idx,
                    .target_layout = target_layout,
                }),
                .tag_payload => |payload| try self.generateTagPayloadAccess(.{
                    .source = payload.source,
                    .payload_idx = payload.payload_idx,
                    .variant_index = payload.variant_index,
                    .tag_discriminant = payload.tag_discriminant,
                    .target_layout = target_layout,
                }),
                .tag_payload_struct => |payload| try self.generateTagPayloadStructAccess(.{
                    .source = payload.source,
                    .variant_index = payload.variant_index,
                    .tag_discriminant = payload.tag_discriminant,
                    .target_layout = target_layout,
                }),
                .list_reinterpret => |list_reinterpret| self.requireExplicitListValueLocationToLayout(
                    try self.emitValueLocal(list_reinterpret.backing_ref),
                    self.localLayout(list_reinterpret.backing_ref),
                    target_layout,
                    "assign_ref.list_reinterpret",
                ),
                .nominal => |nominal| self.requireExplicitNominalValueLocationToLayout(
                    try self.emitValueLocal(nominal.backing_ref),
                    self.localLayout(nominal.backing_ref),
                    target_layout,
                    "assign_ref.nominal",
                ),
            };
        }

        /// Generate integer binary operation
        fn generateIntBinop(
            self: *Self,
            op: lir.LowLevel,
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
            // Determine if this is an unsigned type (for division/modulo/comparisons)
            const is_unsigned = switch (operand_layout) {
                layout.Idx.u8, layout.Idx.u16, layout.Idx.u32, layout.Idx.u64, layout.Idx.u128 => true,
                else => false,
            };

            const is_shift_op = switch (op) {
                .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by => true,
                else => false,
            };

            if (narrow_signed_shift > 0 and !is_unsigned) {
                if (op == .num_shift_right_zf_by) {
                    try self.emitShlImm(.w64, lhs_reg, lhs_reg, narrow_signed_shift);
                    try self.emitLsrImm(.w64, lhs_reg, lhs_reg, narrow_signed_shift);
                } else {
                    try self.emitShlImm(.w64, lhs_reg, lhs_reg, narrow_signed_shift);
                    try self.emitAsrImm(.w64, lhs_reg, lhs_reg, narrow_signed_shift);
                }

                if (is_shift_op) {
                    try self.emitShlImm(.w64, rhs_reg, rhs_reg, 56);
                    try self.emitLsrImm(.w64, rhs_reg, rhs_reg, 56);
                } else {
                    try self.emitShlImm(.w64, rhs_reg, rhs_reg, narrow_signed_shift);
                    try self.emitAsrImm(.w64, rhs_reg, rhs_reg, narrow_signed_shift);
                }
            } else if (is_shift_op) {
                try self.emitShlImm(.w64, rhs_reg, rhs_reg, 56);
                try self.emitLsrImm(.w64, rhs_reg, rhs_reg, 56);
            }

            // Allocate result register
            const result_reg = try self.allocTempGeneral();

            switch (op) {
                .num_plus => {
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.addsRegRegReg(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitAdd(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                    try self.emitCheckedIntAddSubOverflow(op, result_reg, operand_layout, is_unsigned);
                },
                .num_minus => {
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.subsRegRegReg(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSub(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                    try self.emitCheckedIntAddSubOverflow(op, result_reg, operand_layout, is_unsigned);
                },
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
                .num_bitwise_and => try self.codegen.emitAnd(.w64, result_reg, lhs_reg, rhs_reg),
                .num_bitwise_or => try self.codegen.emitOr(.w64, result_reg, lhs_reg, rhs_reg),
                .num_bitwise_xor => try self.codegen.emitXor(.w64, result_reg, lhs_reg, rhs_reg),
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

        fn emitCheckedIntAddSubOverflow(
            self: *Self,
            op: lir.LowLevel,
            result_reg: GeneralReg,
            operand_layout: layout.Idx,
            is_unsigned: bool,
        ) Allocator.Error!void {
            const message = switch (op) {
                .num_plus => "Integer addition overflowed!",
                .num_minus => "Integer subtraction overflowed!",
                else => unreachable,
            };

            switch (operand_layout) {
                .u8 => try self.emitUnsignedIntRangeCheck(result_reg, 255, message),
                .u16 => try self.emitUnsignedIntRangeCheck(result_reg, 65_535, message),
                .u32 => try self.emitUnsignedIntRangeCheck(result_reg, 4_294_967_295, message),
                .i8 => try self.emitSignedIntRangeCheck(result_reg, -128, 127, message),
                .i16 => try self.emitSignedIntRangeCheck(result_reg, -32_768, 32_767, message),
                .i32 => try self.emitSignedIntRangeCheck(result_reg, -2_147_483_648, 2_147_483_647, message),
                .u64 => try self.emitCrashOnCond(switch (op) {
                    .num_plus => condUnsignedAddOverflow(),
                    .num_minus => condUnsignedSubOverflow(),
                    else => unreachable,
                }, message),
                .i64 => try self.emitCrashOnCond(condOverflow(), message),
                else => {
                    if (builtin.mode == .Debug) {
                        std.debug.assert(!is_unsigned or operand_layout == .u128);
                    }
                },
            }
        }

        fn emitUnsignedIntRangeCheck(self: *Self, result_reg: GeneralReg, max_value: i64, message: []const u8) Allocator.Error!void {
            try self.emitCmpRegImm64(result_reg, max_value);
            try self.emitCrashOnCond(condAbove(), message);
        }

        fn emitSignedIntRangeCheck(self: *Self, result_reg: GeneralReg, min_value: i64, max_value: i64, message: []const u8) Allocator.Error!void {
            try self.emitCmpRegImm64(result_reg, min_value);
            try self.emitCrashOnCond(condLess(), message);
            try self.emitCmpRegImm64(result_reg, max_value);
            try self.emitCrashOnCond(condGreater(), message);
        }

        fn emitCmpRegImm64(self: *Self, reg: GeneralReg, value: i64) Allocator.Error!void {
            const temp = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(temp);
            try self.codegen.emitLoadImm(temp, value);
            try self.emitCmpReg(reg, temp);
        }

        fn emitCrashOnCond(self: *Self, cond: Condition, message: []const u8) Allocator.Error!void {
            const crash_patch = try self.codegen.emitCondJump(cond);
            const done_patch = try self.codegen.emitJump();
            self.codegen.patchJump(crash_patch, self.codegen.currentOffset());
            try self.emitRocCrash(message);
            try self.emitTrap();
            self.codegen.patchJump(done_patch, self.codegen.currentOffset());
        }

        fn emitCheckedI128AddSubOverflow(
            self: *Self,
            op: lir.LowLevel,
            operand_layout: layout.Idx,
        ) Allocator.Error!void {
            const message = switch (op) {
                .num_plus => "Integer addition overflowed!",
                .num_minus => "Integer subtraction overflowed!",
                else => unreachable,
            };

            switch (operand_layout) {
                .u128 => try self.emitCrashOnCond(switch (op) {
                    .num_plus => condUnsignedAddOverflow(),
                    .num_minus => condUnsignedSubOverflow(),
                    else => unreachable,
                }, message),
                .i128 => try self.emitCrashOnCond(condOverflow(), message),
                else => {},
            }
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

        fn condOverflow() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .vs else .overflow;
        }

        fn condUnsignedAddOverflow() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .cs else .below;
        }

        fn condUnsignedSubOverflow() Condition {
            return if (comptime target.toCpuArch() == .aarch64) .cc else .below;
        }

        /// Generate 128-bit integer binary operation
        fn generateI128Binop(
            self: *Self,
            op: lir.LowLevel,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            operand_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            // For 128-bit operations, we work with the values as pairs of 64-bit words
            // Low word at offset 0, high word at offset 8

            const signedness: std.builtin.Signedness = if (operand_layout == .u128) .unsigned else .signed;
            const lhs_parts = try self.getI128Parts(lhs_loc, signedness);

            const is_unsigned = operand_layout == .u128;

            if (op == .num_shift_left_by or op == .num_shift_right_by or op == .num_shift_right_zf_by) {
                const result_low = try self.allocTempGeneral();
                const result_high = try self.allocTempGeneral();

                try self.callI128Shift(lhs_parts, rhs_loc, result_low, result_high, operand_layout, op);
                self.codegen.freeGeneral(lhs_parts.low);
                self.codegen.freeGeneral(lhs_parts.high);

                const stack_offset = self.codegen.allocStackSlot(16);
                try self.codegen.emitStoreStack(.w64, stack_offset, result_low);
                try self.codegen.emitStoreStack(.w64, stack_offset + 8, result_high);

                self.codegen.freeGeneral(result_low);
                self.codegen.freeGeneral(result_high);

                return .{ .stack_i128 = stack_offset };
            }

            // Get low and high parts of the RHS for non-shift operations.
            const rhs_parts = try self.getI128Parts(rhs_loc, signedness);

            // Allocate result registers after both operands. On x86_64,
            // 128-bit multiply must reserve RAX/RDX for the widening MUL
            // instruction, so results must not be allocated into that pair.
            const result_low = try self.allocTempGeneral();
            const result_high = try self.allocTempGeneral();

            switch (op) {
                .num_plus => {
                    // 128-bit add: low = lhs_low + rhs_low, high = lhs_high + rhs_high + carry
                    if (comptime target.toCpuArch() == .aarch64) {
                        // ADDS sets carry; ADCS adds it into the high word and leaves final flags.
                        try self.codegen.emit.addsRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                        try self.codegen.emit.adcsRegRegReg(.w64, result_high, lhs_parts.high, rhs_parts.high);
                    } else {
                        // x86_64: ADD sets carry, ADC uses it
                        try self.codegen.emit.movRegReg(.w64, result_low, lhs_parts.low);
                        try self.codegen.emit.addRegReg(.w64, result_low, rhs_parts.low);
                        try self.codegen.emit.movRegReg(.w64, result_high, lhs_parts.high);
                        try self.codegen.emit.adcRegReg(.w64, result_high, rhs_parts.high);
                    }
                    try self.emitCheckedI128AddSubOverflow(op, operand_layout);
                },
                .num_minus => {
                    // 128-bit sub: low = lhs_low - rhs_low, high = lhs_high - rhs_high - borrow
                    if (comptime target.toCpuArch() == .aarch64) {
                        // SUBS sets borrow flag, SBC subtracts with borrow
                        try self.codegen.emit.subsRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                        try self.codegen.emit.sbcsRegRegReg(.w64, result_high, lhs_parts.high, rhs_parts.high);
                    } else {
                        // x86_64: SUB sets borrow, SBB uses it
                        try self.codegen.emit.movRegReg(.w64, result_low, lhs_parts.low);
                        try self.codegen.emit.subRegReg(.w64, result_low, rhs_parts.low);
                        try self.codegen.emit.movRegReg(.w64, result_high, lhs_parts.high);
                        try self.codegen.emit.sbbRegReg(.w64, result_high, rhs_parts.high);
                    }
                    try self.emitCheckedI128AddSubOverflow(op, operand_layout);
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
                                fn f(s: *Self, reg: GeneralReg, sentinel: u32) Allocator.Error!SavedReg {
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
                // Bitwise operations: apply independently to each 64-bit word.
                .num_bitwise_and => {
                    try self.codegen.emitAnd(.w64, result_low, lhs_parts.low, rhs_parts.low);
                    try self.codegen.emitAnd(.w64, result_high, lhs_parts.high, rhs_parts.high);
                },
                .num_bitwise_or => {
                    try self.codegen.emitOr(.w64, result_low, lhs_parts.low, rhs_parts.low);
                    try self.codegen.emitOr(.w64, result_high, lhs_parts.high, rhs_parts.high);
                },
                .num_bitwise_xor => {
                    try self.codegen.emitXor(.w64, result_low, lhs_parts.low, rhs_parts.low);
                    try self.codegen.emitXor(.w64, result_high, lhs_parts.high, rhs_parts.high);
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

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, stack_offset); // out_low
            try builder.addLeaArg(base_reg, stack_offset + 8); // out_high
            try builder.addF64RegArg(freg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
            self.codegen.freeFloat(freg);

            return .{ .stack_i128 = stack_offset };
        }

        fn callFloatUnaryBuiltin(self: *Self, src_loc: ValueLocation, ret_layout: layout.Idx, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const freg = try self.ensureInFloatReg(src_loc);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            if (comptime target.toCpuArch() == .aarch64) {
                if (freg != .V0) {
                    try self.codegen.emit.fmovRegReg(.double, .V0, freg);
                }
            } else {
                try builder.addF64RegArg(freg);
            }

            const float_width: i64 = switch (ret_layout) {
                .f32 => 4,
                .f64 => 8,
                else => std.debug.panic(
                    "LirCodeGen invariant violated: float unary builtin received non-float return layout {s}",
                    .{@tagName(ret_layout)},
                ),
            };
            try builder.addImmArg(float_width);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
            self.codegen.freeFloat(freg);

            const result_reg = self.codegen.allocFloat() orelse unreachable;
            if (comptime target.toCpuArch() == .aarch64) {
                if (result_reg != .V0) {
                    try self.codegen.emit.fmovRegReg(.double, result_reg, .V0);
                }
            } else {
                if (result_reg != .XMM0) {
                    try self.codegen.emit.movsdRegReg(result_reg, .XMM0);
                }
            }
            return .{ .float_reg = result_reg };
        }

        const FloatUnaryMathBuiltin = struct {
            addr: usize,
            func: BuiltinFn,
        };

        fn floatUnaryMathBuiltin(op: lir.LowLevel) FloatUnaryMathBuiltin {
            return switch (op) {
                .num_sin => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_float_sin), .func = .float_sin },
                .num_cos => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_float_cos), .func = .float_cos },
                .num_tan => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_float_tan), .func = .float_tan },
                .num_asin => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_float_asin), .func = .float_asin },
                .num_acos => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_float_acos), .func = .float_acos },
                .num_atan => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_float_atan), .func = .float_atan },
                else => unreachable,
            };
        }

        const DecUnaryMathBuiltin = struct {
            addr: usize,
            func: BuiltinFn,
        };

        fn decUnaryMathBuiltin(op: lir.LowLevel) DecUnaryMathBuiltin {
            return switch (op) {
                .num_sin => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_sin), .func = .dec_sin },
                .num_cos => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_cos), .func = .dec_cos },
                .num_tan => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_tan), .func = .dec_tan },
                .num_asin => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_asin), .func = .dec_asin },
                .num_acos => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_acos), .func = .dec_acos },
                .num_atan => .{ .addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_atan), .func = .dec_atan },
                else => unreachable,
            };
        }

        fn callFloatBinaryBuiltin(self: *Self, lhs_loc: ValueLocation, rhs_loc: ValueLocation, ret_layout: layout.Idx, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const lhs_reg = try self.ensureInFloatReg(lhs_loc);
            const rhs_reg = try self.ensureInFloatReg(rhs_loc);
            const lhs_slot = self.codegen.allocStackSlot(8);
            const rhs_slot = self.codegen.allocStackSlot(8);
            try self.codegen.emitStoreStackF64(lhs_slot, lhs_reg);
            try self.codegen.emitStoreStackF64(rhs_slot, rhs_reg);
            self.codegen.freeFloat(lhs_reg);
            if (rhs_reg != lhs_reg) {
                self.codegen.freeFloat(rhs_reg);
            }

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emitLoadStackF64(.V0, lhs_slot);
                try self.codegen.emitLoadStackF64(.V1, rhs_slot);
            } else {
                try builder.addF64MemArg(frame_ptr, lhs_slot);
                try builder.addF64MemArg(frame_ptr, rhs_slot);
            }

            const float_width: i64 = switch (ret_layout) {
                .f32 => 4,
                .f64 => 8,
                else => std.debug.panic(
                    "LirCodeGen invariant violated: float binary builtin received non-float return layout {s}",
                    .{@tagName(ret_layout)},
                ),
            };
            try builder.addImmArg(float_width);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            const result_reg = self.codegen.allocFloat() orelse unreachable;
            if (comptime target.toCpuArch() == .aarch64) {
                if (result_reg != .V0) {
                    try self.codegen.emit.fmovRegReg(.double, result_reg, .V0);
                }
            } else {
                if (result_reg != .XMM0) {
                    try self.codegen.emit.movsdRegReg(result_reg, .XMM0);
                }
            }
            return .{ .float_reg = result_reg };
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
        fn generateIntTryConversion(self: *Self, ll: anytype, args: []const LocalId) Allocator.Error!ValueLocation {
            if (args.len != 1) unreachable;
            const src_loc = try self.emitValueLocal(args[0]);

            const ls = self.layout_store;
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            std.debug.assert(ret_layout_val.tag == .tag_union);
            const tu_idx = ret_layout_val.getTagUnion().idx;
            const tu_data = ls.getTagUnionData(tu_idx);
            const tu_size = tu_data.size.get(ls.targetUsize());

            const result_offset = self.codegen.allocStackSlot(tu_size);
            try self.zeroStackArea(result_offset, tu_size);

            const disc_offset: u32 = tu_data.discriminant_offset.get(ls.targetUsize());
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

        /// Generate code for typed `*_from_str` low-levels:
        /// Str -> Result(Num, [InvalidNumStr])
        fn generateNumFromStr(self: *Self, ll: anytype, args: []const LocalId) Allocator.Error!ValueLocation {
            if (args.len != 1) unreachable;
            const str_loc = try self.emitValueLocal(args[0]);
            const str_off = try self.ensureOnStack(str_loc, roc_str_size);
            const parse_spec = ll.op.numericParseSpec() orelse
                std.debug.panic("generateNumFromStr: expected typed from_str op, got {s}", .{@tagName(ll.op)});

            const ls = self.layout_store;
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            if (ret_layout_val.tag != .tag_union) {
                std.debug.panic("generateNumFromStr: expected tag_union layout, got {s}", .{@tagName(ret_layout_val.tag)});
            }
            const tu_data = ls.getTagUnionData(ret_layout_val.getTagUnion().idx);
            const tu_size = tu_data.size.get(ls.targetUsize());
            const result_offset = self.codegen.allocStackSlot(tu_size);
            try self.zeroStackArea(result_offset, tu_size);
            const disc_offset: u32 = tu_data.discriminant_offset.get(ls.targetUsize());

            const base_reg = frame_ptr;

            switch (parse_spec) {
                .dec => {
                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_dec_from_str);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addMemArg(base_reg, str_off);
                    try builder.addMemArg(base_reg, str_off + 16);
                    try builder.addMemArg(base_reg, str_off + 8);
                    try builder.addImmArg(@intCast(disc_offset));
                    try self.callBuiltin(&builder, fn_addr, .dec_from_str);
                },
                .float => |float| {
                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_float_from_str);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addMemArg(base_reg, str_off);
                    try builder.addMemArg(base_reg, str_off + 16);
                    try builder.addMemArg(base_reg, str_off + 8);
                    try builder.addImmArg(@intCast(float.width_bytes));
                    try builder.addImmArg(@intCast(disc_offset));
                    try self.callBuiltin(&builder, fn_addr, .float_from_str);
                },
                .int => |int| {
                    const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_int_from_str);
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addLeaArg(base_reg, result_offset);
                    try builder.addMemArg(base_reg, str_off);
                    try builder.addMemArg(base_reg, str_off + 16);
                    try builder.addMemArg(base_reg, str_off + 8);
                    try builder.addImmArg(@intCast(int.width_bytes));
                    try builder.addImmArg(if (int.signed) @as(i64, 1) else @as(i64, 0));
                    try builder.addImmArg(@intCast(disc_offset));
                    try self.callBuiltin(&builder, fn_addr, .int_from_str);
                },
            }

            return .{ .stack = .{ .offset = result_offset } };
        }

        // ── Float/Dec try_unsafe conversion info ──

        const FloatDecTryUnsafeInfo = struct {
            src_kind: enum { f32, f64, dec },
            tgt_kind: enum { int, f32, dec },
            tgt_bits: u8,
            tgt_signed: bool,
        };

        const TryUnsafeOffsets = struct {
            success: u32,
            value: u32,
        };

        fn tryUnsafeOffsets(self: *Self, ret_layout: layout.Idx) TryUnsafeOffsets {
            const ret_layout_val = self.layout_store.getLayout(ret_layout);
            if (ret_layout_val.tag != .struct_) {
                std.debug.panic("try_unsafe result expected struct layout, got {s}", .{@tagName(ret_layout_val.tag)});
            }
            const struct_idx = ret_layout_val.getStruct().idx;
            return .{
                .success = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 0),
                .value = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 1),
            };
        }

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
        fn generateFloatDecTryUnsafeConversion(self: *Self, ll: anytype, args: []const LocalId) Allocator.Error!ValueLocation {
            if (args.len != 1) unreachable;
            const src_loc = try self.emitValueLocal(args[0]);

            const ls = self.layout_store;
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            const size_align = ls.layoutSizeAlign(ret_layout_val);
            const result_offset = self.codegen.allocStackSlot(size_align.size);
            try self.zeroStackArea(result_offset, size_align.size);

            const info = floatDecTryUnsafeInfo(ll.op);
            const offsets = self.tryUnsafeOffsets(ll.ret_layout);

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
                        try builder.addImmArg(@intCast(offsets.success));
                        try builder.addImmArg(@intCast(offsets.value));
                        try self.callBuiltin(&builder, fn_addr, .dec_to_int_try_unsafe);

                        self.codegen.freeGeneral(parts.low);
                        self.codegen.freeGeneral(parts.high);
                    } else {
                        // Float source (f32 or f64): all floats stored as f64 internally.
                        // The float `val` is argument position 1 (after `out`). On x86-64
                        // it must flow through the CallBuilder so it lands in the right
                        // register: Windows x64 shares positional slots between float and
                        // integer args (val -> XMM1, target_bits -> R8), and only the
                        // builder tracks that. On aarch64 (AAPCS) FP args use v0-v7
                        // independently, so the single float arg goes in V0 directly.
                        const freg = try self.ensureInFloatReg(src_loc);
                        const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_f64_to_int_try_unsafe);
                        const base_reg = frame_ptr;

                        if (comptime target.toCpuArch() == .aarch64) {
                            if (freg != .V0) try self.codegen.emit.fmovRegReg(.double, .V0, freg);
                        }

                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        if (comptime target.toCpuArch() != .aarch64) {
                            try builder.addF64RegArg(freg);
                        }
                        try builder.addImmArg(@intCast(target_bits));
                        try builder.addImmArg(@intCast(target_is_signed));
                        try builder.addImmArg(@intCast(val_size));
                        try builder.addImmArg(@intCast(offsets.success));
                        try builder.addImmArg(@intCast(offsets.value));
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
                        try builder.addImmArg(@intCast(offsets.success));
                        try builder.addImmArg(@intCast(offsets.value));
                        try self.callBuiltin(&builder, fn_addr, .dec_to_f32_try_unsafe);

                        self.codegen.freeGeneral(parts.low);
                        self.codegen.freeGeneral(parts.high);
                    } else {
                        // f64 to f32. `val` is argument position 1 (after `out`); route it
                        // through the CallBuilder on x86-64 to honor Windows x64 positional
                        // float/int slot sharing, and place it in V0 directly on aarch64
                        // (see the int branch above for details).
                        const freg = try self.ensureInFloatReg(src_loc);
                        const fn_addr: usize = @intFromPtr(&dev_wrappers.roc_builtins_f64_to_f32_try_unsafe);
                        const base_reg = frame_ptr;

                        if (comptime target.toCpuArch() == .aarch64) {
                            if (freg != .V0) try self.codegen.emit.fmovRegReg(.double, .V0, freg);
                        }

                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addLeaArg(base_reg, result_offset);
                        if (comptime target.toCpuArch() != .aarch64) {
                            try builder.addF64RegArg(freg);
                        }
                        try builder.addImmArg(@intCast(offsets.success));
                        try builder.addImmArg(@intCast(offsets.value));
                        try self.callBuiltin(&builder, fn_addr, .f64_to_f32_try_unsafe);

                        self.codegen.freeFloat(freg);
                    }
                },
                .dec => {
                    // Integer to Dec: result is { success: U8, val_or_memory_garbage: Dec }
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
                    try builder.addImmArg(@intCast(offsets.success));
                    try builder.addImmArg(@intCast(offsets.value));
                    try self.callBuiltin(&builder, fn_addr, builtin_fn);

                    self.codegen.freeGeneral(parts.low);
                    self.codegen.freeGeneral(parts.high);
                },
            }

            return .{ .stack = .{ .offset = result_offset } };
        }

        /// Call Dec multiplication builtin via decomposed wrapper.
        /// Wrapper signature: (out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, b_low: u64, b_high: u64, roc_ops: *RocOps) -> void
        fn callDecMul(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Allocator.Error!void {
            const fn_addr = @intFromPtr(&dev_wrappers.roc_builtins_dec_mul);
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
            try self.callBuiltin(&builder, fn_addr, .dec_mul);

            // Load results from stack slot
            try self.codegen.emitLoadStack(.w64, result_low, result_slot);
            try self.codegen.emitLoadStack(.w64, result_high, result_slot + 8);
        }

        fn callDecUnaryMathBuiltin(self: *Self, src_loc: ValueLocation, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const src_parts = try self.getI128Parts(src_loc, .signed);
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_slot = self.codegen.allocStackSlot(16);
            const base_reg = frame_ptr;

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_slot);
            try builder.addLeaArg(base_reg, result_slot + 8);
            try builder.addRegArg(src_parts.low);
            try builder.addRegArg(src_parts.high);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            self.codegen.freeGeneral(src_parts.low);
            self.codegen.freeGeneral(src_parts.high);

            return .{ .stack_i128 = result_slot };
        }

        fn callDecBinaryMathBuiltin(self: *Self, lhs_loc: ValueLocation, rhs_loc: ValueLocation, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!ValueLocation {
            const lhs_parts = try self.getI128Parts(lhs_loc, .signed);
            const rhs_parts = try self.getI128Parts(rhs_loc, .signed);
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const result_slot = self.codegen.allocStackSlot(16);
            const base_reg = frame_ptr;

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(base_reg, result_slot);
            try builder.addLeaArg(base_reg, result_slot + 8);
            try builder.addRegArg(lhs_parts.low);
            try builder.addRegArg(lhs_parts.high);
            try builder.addRegArg(rhs_parts.low);
            try builder.addRegArg(rhs_parts.high);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            self.codegen.freeGeneral(lhs_parts.low);
            self.codegen.freeGeneral(lhs_parts.high);
            self.codegen.freeGeneral(rhs_parts.low);
            self.codegen.freeGeneral(rhs_parts.high);

            return .{ .stack_i128 = result_slot };
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

        /// Call i128/u128 shift builtin via decomposed wrapper.
        /// Wrapper signature: (out_low: *u64, out_high: *u64, a_low: u64, a_high: u64, shift_amount: u8) -> void
        fn callI128Shift(
            self: *Self,
            lhs_parts: I128Parts,
            shift_loc: ValueLocation,
            result_low: GeneralReg,
            result_high: GeneralReg,
            operand_layout: layout.Idx,
            op: lir.LowLevel,
        ) Allocator.Error!void {
            if (builtin.mode == .Debug and operand_layout == .dec) {
                std.debug.panic(
                    "LirCodeGen invariant violated: decimal layout reached i128 shift lowering",
                    .{},
                );
            }

            const fn_info: struct { addr: usize, builtin_fn: BuiltinFn } = switch (op) {
                .num_shift_left_by => .{
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_num_shl_u128),
                    .builtin_fn = .num_shl_u128,
                },
                .num_shift_right_by => if (operand_layout == .u128)
                    .{
                        .addr = @intFromPtr(&dev_wrappers.roc_builtins_num_shr_u128),
                        .builtin_fn = .num_shr_u128,
                    }
                else
                    .{
                        .addr = @intFromPtr(&dev_wrappers.roc_builtins_num_shr_i128),
                        .builtin_fn = .num_shr_i128,
                    },
                .num_shift_right_zf_by => .{
                    .addr = @intFromPtr(&dev_wrappers.roc_builtins_num_shr_u128),
                    .builtin_fn = .num_shr_u128,
                },
                else => unreachable,
            };

            const shift_reg = try self.ensureInGeneralReg(shift_loc);
            defer self.codegen.freeGeneral(shift_reg);

            const result_slot = self.codegen.allocStackSlot(16);
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, result_slot);
            try builder.addLeaArg(frame_ptr, result_slot + 8);
            try builder.addRegArg(lhs_parts.low);
            try builder.addRegArg(lhs_parts.high);
            try builder.addRegArg(shift_reg);
            try self.callBuiltin(&builder, fn_info.addr, fn_info.builtin_fn);

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
        fn emitSignExtendHighReg(self: *Self, high_reg: GeneralReg, low_reg: GeneralReg, signedness: std.builtin.Signedness) Allocator.Error!void {
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
            op: lir.LowLevel,
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

        // Heap state shared by the explicit-stack continuations that emit a
        // multi-variant tag-union structural comparison. Freed by `tag_finish`.
        const EqTagState = struct {
            result_reg: GeneralReg,
            lhs_base: i32,
            rhs_base: i32,
            disc_slot: i32,
            disc_ne_patch: usize,
            end_patches: std.ArrayList(usize),
        };
        // Heap state for a list structural comparison loop. Freed by `list_finish`.
        const EqListState = struct {
            result_reg: GeneralReg,
            loop_start: usize,
            ctr_slot: i32,
            offset_slot: i32,
            elem_size: u32,
            eq_reg: GeneralReg,
            exit_patch: usize,
            len_ne_patch: usize,
            empty_patch: usize,
        };

        /// Structural equality (`num_is_eq`) for compound layouts, emitted with an
        /// explicit work stack so nested layouts never grow the native call stack.
        /// Every work item writes a 0/1 result into a caller-provided result reg.
        fn generateStructuralEquality(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            layout_idx: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const result_reg = try self.allocTempGeneral();

            const EqWork = union(enum) {
                // compareFieldByLayout: dispatch by layout tag/size on stack offsets.
                node_field: struct { lhs_off: i32, rhs_off: i32, layout_idx: layout.Idx, size: u32, result_reg: GeneralReg },
                // Structural comparators (operate on value locations).
                node_struct: struct { lhs_loc: ValueLocation, rhs_loc: ValueLocation, layout_idx: layout.Idx, result_reg: GeneralReg },
                node_tag: struct { lhs_loc: ValueLocation, rhs_loc: ValueLocation, layout_idx: layout.Idx, result_reg: GeneralReg },
                node_list: struct { lhs_loc: ValueLocation, rhs_loc: ValueLocation, layout_idx: layout.Idx, result_reg: GeneralReg },
                // Struct field continuations.
                struct_field: struct { result_slot: i32, lhs_off: i32, rhs_off: i32, field_layout_idx: layout.Idx, field_size: u32 },
                struct_and: struct { result_slot: i32, field_eq_reg: GeneralReg },
                struct_finish: struct { result_slot: i32, result_reg: GeneralReg },
                // Tag union continuations.
                tag_variant: struct { state: *EqTagState, variant_i: u32, payload_layout_idx: layout.Idx, payload_size: u32 },
                tag_after: struct { state: *EqTagState, skip_patch: usize },
                tag_finish: *EqTagState,
                tag_finish1: struct { result_reg: GeneralReg, disc_ne_patch: usize },
                // List continuation.
                list_finish: *EqListState,
            };

            var sfa = std.heap.stackFallback(32 * @sizeOf(EqWork), self.allocator);
            const wa = sfa.get();
            var work = std.ArrayList(EqWork).empty;
            defer work.deinit(wa);

            const top_layout = ls.getLayout(layout_idx);
            switch (top_layout.tag) {
                .list, .list_of_zst => try work.append(wa, .{ .node_list = .{ .lhs_loc = lhs_loc, .rhs_loc = rhs_loc, .layout_idx = layout_idx, .result_reg = result_reg } }),
                .tag_union => try work.append(wa, .{ .node_tag = .{ .lhs_loc = lhs_loc, .rhs_loc = rhs_loc, .layout_idx = layout_idx, .result_reg = result_reg } }),
                else => try work.append(wa, .{ .node_struct = .{ .lhs_loc = lhs_loc, .rhs_loc = rhs_loc, .layout_idx = layout_idx, .result_reg = result_reg } }),
            }

            while (work.pop()) |item| switch (item) {
                .node_field => |f| {
                    const rr = f.result_reg;
                    const field_layout = ls.getLayout(f.layout_idx);
                    if (field_layout.tag == .box) {
                        const inner_layout_idx = field_layout.getIdx();
                        const inner_layout = ls.getLayout(inner_layout_idx);
                        const lhs_norm = try self.normalizeValueLocationToLayout(.{ .stack = .{ .offset = f.lhs_off } }, f.layout_idx, inner_layout_idx);
                        const rhs_norm = try self.normalizeValueLocationToLayout(.{ .stack = .{ .offset = f.rhs_off } }, f.layout_idx, inner_layout_idx);
                        switch (inner_layout.tag) {
                            .struct_ => try work.append(wa, .{ .node_struct = .{ .lhs_loc = lhs_norm, .rhs_loc = rhs_norm, .layout_idx = inner_layout_idx, .result_reg = rr } }),
                            .tag_union => try work.append(wa, .{ .node_tag = .{ .lhs_loc = lhs_norm, .rhs_loc = rhs_norm, .layout_idx = inner_layout_idx, .result_reg = rr } }),
                            .list, .list_of_zst => try work.append(wa, .{ .node_list = .{ .lhs_loc = lhs_norm, .rhs_loc = rhs_norm, .layout_idx = inner_layout_idx, .result_reg = rr } }),
                            else => {
                                const inner_size = ls.layoutSizeAlign(inner_layout).size;
                                const lhs_stack = switch (lhs_norm) {
                                    .stack => |s| s.offset,
                                    .stack_str => |s| s,
                                    .stack_i128 => |s| s,
                                    .list_stack => |s| s.struct_offset,
                                    else => try self.ensureOnStack(lhs_norm, inner_size),
                                };
                                const rhs_stack = switch (rhs_norm) {
                                    .stack => |s| s.offset,
                                    .stack_str => |s| s,
                                    .stack_i128 => |s| s,
                                    .list_stack => |s| s.struct_offset,
                                    else => try self.ensureOnStack(rhs_norm, inner_size),
                                };
                                try work.append(wa, .{ .node_field = .{ .lhs_off = lhs_stack, .rhs_off = rhs_stack, .layout_idx = inner_layout_idx, .size = inner_size, .result_reg = rr } });
                            },
                        }
                    } else if (field_layout.tag == .struct_) {
                        try work.append(wa, .{ .node_struct = .{ .lhs_loc = .{ .stack = .{ .offset = f.lhs_off } }, .rhs_loc = .{ .stack = .{ .offset = f.rhs_off } }, .layout_idx = f.layout_idx, .result_reg = rr } });
                    } else if (f.layout_idx == .str) {
                        const eq_loc = try self.callStr2ToScalar(f.lhs_off, f.rhs_off, @intFromPtr(&wrapStrEqual), .str_equal);
                        const eq_reg = try self.ensureInGeneralReg(eq_loc);
                        try self.emitMovRegReg(rr, eq_reg);
                        self.codegen.freeGeneral(eq_reg);
                    } else if (f.layout_idx == .dec or f.layout_idx == .i128 or f.layout_idx == .u128) {
                        const lhs_parts = try self.getI128Parts(.{ .stack_i128 = f.lhs_off }, .signed);
                        const rhs_parts = try self.getI128Parts(.{ .stack_i128 = f.rhs_off }, .signed);
                        try self.generateI128Equality(lhs_parts, rhs_parts, rr, true);
                        self.codegen.freeGeneral(lhs_parts.low);
                        self.codegen.freeGeneral(lhs_parts.high);
                        self.codegen.freeGeneral(rhs_parts.low);
                        self.codegen.freeGeneral(rhs_parts.high);
                    } else if (field_layout.tag == .list or field_layout.tag == .list_of_zst) {
                        try work.append(wa, .{ .node_list = .{ .lhs_loc = .{ .stack = .{ .offset = f.lhs_off } }, .rhs_loc = .{ .stack = .{ .offset = f.rhs_off } }, .layout_idx = f.layout_idx, .result_reg = rr } });
                    } else if (f.size <= 8) {
                        const lhs_reg = try self.allocTempGeneral();
                        const rhs_reg = try self.allocTempGeneral();
                        switch (f.size) {
                            1 => {
                                try self.emitLoadStackW8(lhs_reg, f.lhs_off);
                                try self.emitLoadStackW8(rhs_reg, f.rhs_off);
                            },
                            2 => {
                                try self.emitLoadStackW16(lhs_reg, f.lhs_off);
                                try self.emitLoadStackW16(rhs_reg, f.rhs_off);
                            },
                            4 => {
                                try self.codegen.emitLoadStack(.w32, lhs_reg, f.lhs_off);
                                try self.codegen.emitLoadStack(.w32, rhs_reg, f.rhs_off);
                            },
                            8 => {
                                try self.codegen.emitLoadStack(.w64, lhs_reg, f.lhs_off);
                                try self.codegen.emitLoadStack(.w64, rhs_reg, f.rhs_off);
                            },
                            else => {
                                const mask: u64 = (@as(u64, 1) << @intCast(f.size * 8)) - 1;
                                const mask_reg = try self.allocTempGeneral();
                                try self.codegen.emitLoadStack(.w64, lhs_reg, f.lhs_off);
                                try self.codegen.emitLoadStack(.w64, rhs_reg, f.rhs_off);
                                try self.codegen.emitLoadImm(mask_reg, @bitCast(mask));
                                try self.emitAndRegs(.w64, lhs_reg, lhs_reg, mask_reg);
                                try self.emitAndRegs(.w64, rhs_reg, rhs_reg, mask_reg);
                                self.codegen.freeGeneral(mask_reg);
                            },
                        }
                        try self.emitCmpReg(lhs_reg, rhs_reg);
                        try self.emitSetCond(rr, condEqual());
                        self.codegen.freeGeneral(lhs_reg);
                        self.codegen.freeGeneral(rhs_reg);
                    } else if (field_layout.tag == .tag_union) {
                        try work.append(wa, .{ .node_tag = .{ .lhs_loc = .{ .stack = .{ .offset = f.lhs_off } }, .rhs_loc = .{ .stack = .{ .offset = f.rhs_off } }, .layout_idx = f.layout_idx, .result_reg = rr } });
                    } else {
                        const tmp_a = try self.allocTempGeneral();
                        const tmp_b = try self.allocTempGeneral();
                        const xor_acc = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(xor_acc, 0);
                        var cmp_off: u32 = 0;
                        while (cmp_off < f.size) {
                            try self.codegen.emitLoadStack(.w64, tmp_a, f.lhs_off + @as(i32, @intCast(cmp_off)));
                            try self.codegen.emitLoadStack(.w64, tmp_b, f.rhs_off + @as(i32, @intCast(cmp_off)));
                            const remaining = f.size - cmp_off;
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
                        try self.emitSetCond(rr, condEqual());
                        self.codegen.freeGeneral(tmp_a);
                        self.codegen.freeGeneral(tmp_b);
                        self.codegen.freeGeneral(xor_acc);
                    }
                },

                .node_struct => |s| {
                    const rr = s.result_reg;
                    const stored_layout = ls.getLayout(s.layout_idx);
                    if (stored_layout.tag == .box) {
                        const inner_layout_idx = stored_layout.getIdx();
                        const lhs_norm = try self.normalizeValueLocationToLayout(s.lhs_loc, s.layout_idx, inner_layout_idx);
                        const rhs_norm = try self.normalizeValueLocationToLayout(s.rhs_loc, s.layout_idx, inner_layout_idx);
                        try work.append(wa, .{ .node_struct = .{ .lhs_loc = lhs_norm, .rhs_loc = rhs_norm, .layout_idx = inner_layout_idx, .result_reg = rr } });
                        continue;
                    }
                    if (stored_layout.tag != .struct_) {
                        try self.codegen.emitLoadImm(rr, 1);
                        continue;
                    }
                    const struct_idx = stored_layout.getStruct().idx;
                    const struct_data = ls.getStructData(struct_idx);
                    const field_count = struct_data.fields.count;
                    if (field_count == 0) {
                        try self.codegen.emitLoadImm(rr, 1);
                        continue;
                    }
                    const lhs_base = try self.ensureRecordOnStack(s.lhs_loc, ls.layoutSizeAlign(stored_layout).size);
                    const rhs_base = try self.ensureRecordOnStack(s.rhs_loc, ls.layoutSizeAlign(stored_layout).size);
                    const result_slot = self.codegen.allocStackSlot(8);
                    {
                        const temp = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(temp, 1);
                        try self.codegen.emitStoreStack(.w64, result_slot, temp);
                        self.codegen.freeGeneral(temp);
                    }
                    try work.append(wa, .{ .struct_finish = .{ .result_slot = result_slot, .result_reg = rr } });
                    // Push fields in reverse so emission keeps source order.
                    var fi: u32 = field_count;
                    while (fi > 0) {
                        fi -= 1;
                        // Padding spacers hold uninitialized bytes; never compare them.
                        if (ls.getStructFieldIsPadding(struct_idx, @intCast(fi))) continue;
                        const field_size = ls.getStructFieldSize(struct_idx, @intCast(fi));
                        if (field_size == 0) continue;
                        const field_offset = ls.getStructFieldOffset(struct_idx, @intCast(fi));
                        const field_layout_idx = ls.getStructFieldLayout(struct_idx, @intCast(fi));
                        try work.append(wa, .{ .struct_field = .{
                            .result_slot = result_slot,
                            .lhs_off = lhs_base + @as(i32, @intCast(field_offset)),
                            .rhs_off = rhs_base + @as(i32, @intCast(field_offset)),
                            .field_layout_idx = field_layout_idx,
                            .field_size = field_size,
                        } });
                    }
                },

                .struct_field => |sf| {
                    const field_eq_reg = try self.allocTempGeneral();
                    try work.append(wa, .{ .struct_and = .{ .result_slot = sf.result_slot, .field_eq_reg = field_eq_reg } });
                    try work.append(wa, .{ .node_field = .{ .lhs_off = sf.lhs_off, .rhs_off = sf.rhs_off, .layout_idx = sf.field_layout_idx, .size = sf.field_size, .result_reg = field_eq_reg } });
                },

                .struct_and => |sa_| {
                    const acc_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, acc_reg, sa_.result_slot);
                    try self.emitAndRegs(.w64, acc_reg, acc_reg, sa_.field_eq_reg);
                    try self.codegen.emitStoreStack(.w64, sa_.result_slot, acc_reg);
                    self.codegen.freeGeneral(acc_reg);
                    self.codegen.freeGeneral(sa_.field_eq_reg);
                },

                .struct_finish => |sf| {
                    try self.codegen.emitLoadStack(.w64, sf.result_reg, sf.result_slot);
                },

                .node_tag => |t| {
                    const rr = t.result_reg;
                    const stored_layout = ls.getLayout(t.layout_idx);
                    if (stored_layout.tag == .box) {
                        const inner_layout_idx = stored_layout.getIdx();
                        const lhs_norm = try self.normalizeValueLocationToLayout(t.lhs_loc, t.layout_idx, inner_layout_idx);
                        const rhs_norm = try self.normalizeValueLocationToLayout(t.rhs_loc, t.layout_idx, inner_layout_idx);
                        try work.append(wa, .{ .node_tag = .{ .lhs_loc = lhs_norm, .rhs_loc = rhs_norm, .layout_idx = inner_layout_idx, .result_reg = rr } });
                        continue;
                    }
                    if (stored_layout.tag != .tag_union) unreachable;
                    const tu_idx = stored_layout.getTagUnion().idx;
                    const tu_data = ls.getTagUnionData(tu_idx);
                    const total_size = tu_data.size.get(ls.targetUsize());
                    if (total_size == 0) {
                        try self.codegen.emitLoadImm(rr, 1);
                        continue;
                    }
                    const lhs_base = try self.ensureRecordOnStack(t.lhs_loc, total_size);
                    const rhs_base = try self.ensureRecordOnStack(t.rhs_loc, total_size);
                    const disc_offset: i32 = @intCast(tu_data.discriminant_offset.get(self.layout_store.targetUsize()));
                    const disc_size = tu_data.discriminant_size;
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
                    if (disc_size != 0 and disc_size < 8) {
                        const disc_mask: u64 = (@as(u64, 1) << @intCast(disc_size * 8)) - 1;
                        const disc_mask_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(disc_mask_reg, @bitCast(disc_mask));
                        try self.emitAndRegs(.w64, lhs_disc, lhs_disc, disc_mask_reg);
                        try self.emitAndRegs(.w64, rhs_disc, rhs_disc, disc_mask_reg);
                        self.codegen.freeGeneral(disc_mask_reg);
                    }
                    try self.emitCmpReg(lhs_disc, rhs_disc);
                    self.codegen.freeGeneral(rhs_disc);
                    try self.codegen.emitLoadImm(rr, 0);
                    const disc_ne_patch = try self.emitJumpIfNotEqual();
                    const variants = ls.getTagUnionVariants(tu_data);
                    const variant_count = variants.len;
                    if (variant_count == 1) {
                        self.codegen.freeGeneral(lhs_disc);
                        const payload_layout_idx = variants.get(0).payload_layout;
                        const payload_layout = ls.getLayout(payload_layout_idx);
                        const payload_size = ls.layoutSizeAlign(payload_layout).size;
                        if (payload_size > 0) {
                            try work.append(wa, .{ .tag_finish1 = .{ .result_reg = rr, .disc_ne_patch = disc_ne_patch } });
                            try work.append(wa, .{ .node_field = .{ .lhs_off = lhs_base, .rhs_off = rhs_base, .layout_idx = payload_layout_idx, .size = payload_size, .result_reg = rr } });
                        } else {
                            try self.codegen.emitLoadImm(rr, 1);
                            self.codegen.patchJump(disc_ne_patch, self.codegen.currentOffset());
                        }
                    } else {
                        try self.codegen.emitLoadImm(rr, 1);
                        const disc_slot = self.codegen.allocStackSlot(8);
                        try self.codegen.emitStoreStack(.w64, disc_slot, lhs_disc);
                        self.codegen.freeGeneral(lhs_disc);
                        const state = try self.allocator.create(EqTagState);
                        state.* = .{
                            .result_reg = rr,
                            .lhs_base = lhs_base,
                            .rhs_base = rhs_base,
                            .disc_slot = disc_slot,
                            .disc_ne_patch = disc_ne_patch,
                            .end_patches = std.ArrayList(usize).empty,
                        };
                        try work.append(wa, .{ .tag_finish = state });
                        var vi: u32 = @intCast(variant_count);
                        while (vi > 0) {
                            vi -= 1;
                            const payload_layout_idx = variants.get(vi).payload_layout;
                            const payload_layout = ls.getLayout(payload_layout_idx);
                            const payload_size = ls.layoutSizeAlign(payload_layout).size;
                            if (payload_size == 0) continue;
                            try work.append(wa, .{ .tag_variant = .{ .state = state, .variant_i = vi, .payload_layout_idx = payload_layout_idx, .payload_size = payload_size } });
                        }
                    }
                },

                .tag_variant => |tv| {
                    const st = tv.state;
                    const disc_temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, disc_temp, st.disc_slot);
                    try self.emitCmpImm(disc_temp, @intCast(tv.variant_i));
                    self.codegen.freeGeneral(disc_temp);
                    const skip_patch = try self.emitJumpIfNotEqual();
                    try work.append(wa, .{ .tag_after = .{ .state = st, .skip_patch = skip_patch } });
                    try work.append(wa, .{ .node_field = .{ .lhs_off = st.lhs_base, .rhs_off = st.rhs_base, .layout_idx = tv.payload_layout_idx, .size = tv.payload_size, .result_reg = st.result_reg } });
                },

                .tag_after => |ta| {
                    const st = ta.state;
                    try st.end_patches.append(self.allocator, try self.codegen.emitJump());
                    self.codegen.patchJump(ta.skip_patch, self.codegen.currentOffset());
                },

                .tag_finish => |st| {
                    const current = self.codegen.currentOffset();
                    for (st.end_patches.items) |patch| {
                        self.codegen.patchJump(patch, current);
                    }
                    self.codegen.patchJump(st.disc_ne_patch, self.codegen.currentOffset());
                    st.end_patches.deinit(self.allocator);
                    self.allocator.destroy(st);
                },

                .tag_finish1 => |tf| {
                    self.codegen.patchJump(tf.disc_ne_patch, self.codegen.currentOffset());
                },

                .node_list => |l| {
                    const rr = l.result_reg;
                    const list_layout = ls.getLayout(l.layout_idx);
                    if (list_layout.tag == .box) {
                        const inner_layout_idx = list_layout.getIdx();
                        const lhs_norm = try self.normalizeValueLocationToLayout(l.lhs_loc, l.layout_idx, inner_layout_idx);
                        const rhs_norm = try self.normalizeValueLocationToLayout(l.rhs_loc, l.layout_idx, inner_layout_idx);
                        try work.append(wa, .{ .node_list = .{ .lhs_loc = lhs_norm, .rhs_loc = rhs_norm, .layout_idx = inner_layout_idx, .result_reg = rr } });
                        continue;
                    }
                    const elem_layout_idx: layout.Idx = switch (list_layout.tag) {
                        .list => list_layout.getIdx(),
                        .list_of_zst => .zst,
                        else => unreachable,
                    };
                    const elem_layout = ls.getLayout(elem_layout_idx);
                    const elem_size: u32 = ls.layoutSizeAlign(elem_layout).size;
                    const lhs_base: i32 = switch (l.lhs_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |li| li.struct_offset,
                        else => unreachable,
                    };
                    const rhs_base: i32 = switch (l.rhs_loc) {
                        .stack => |s| s.offset,
                        .list_stack => |li| li.struct_offset,
                        else => unreachable,
                    };
                    const lhs_len = try self.allocTempGeneral();
                    const rhs_len = try self.allocTempGeneral();
                    try self.emitLoad(.w64, lhs_len, frame_ptr, lhs_base + 8);
                    try self.emitLoad(.w64, rhs_len, frame_ptr, rhs_base + 8);
                    try self.emitCmpReg(lhs_len, rhs_len);
                    self.codegen.freeGeneral(rhs_len);
                    try self.codegen.emitLoadImm(rr, 0);
                    const len_ne_patch = try self.codegen.emitCondJump(condNotEqual());
                    try self.codegen.emitLoadImm(rr, 1);
                    const len_slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStack(.w64, len_slot, lhs_len);
                    try self.emitCmpImm(lhs_len, 0);
                    self.codegen.freeGeneral(lhs_len);
                    const empty_patch = try self.codegen.emitCondJump(condEqual());
                    if (elem_size == 0) {
                        const done_offset = self.codegen.currentOffset();
                        self.codegen.patchJump(len_ne_patch, done_offset);
                        self.codegen.patchJump(empty_patch, done_offset);
                        continue;
                    }
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
                    const ctr_slot = self.codegen.allocStackSlot(8);
                    const offset_slot = self.codegen.allocStackSlot(8);
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
                    const loop_start = self.codegen.currentOffset();
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
                    const eq_reg = try self.allocTempGeneral();
                    const state = try self.allocator.create(EqListState);
                    state.* = .{
                        .result_reg = rr,
                        .loop_start = loop_start,
                        .ctr_slot = ctr_slot,
                        .offset_slot = offset_slot,
                        .elem_size = elem_size,
                        .eq_reg = eq_reg,
                        .exit_patch = exit_patch,
                        .len_ne_patch = len_ne_patch,
                        .empty_patch = empty_patch,
                    };
                    try work.append(wa, .{ .list_finish = state });
                    try work.append(wa, .{ .node_field = .{ .lhs_off = lhs_elem_slot, .rhs_off = rhs_elem_slot, .layout_idx = elem_layout_idx, .size = elem_size, .result_reg = eq_reg } });
                },

                .list_finish => |st| {
                    try self.emitCmpImm(st.eq_reg, 1);
                    self.codegen.freeGeneral(st.eq_reg);
                    const ne_patch = try self.codegen.emitCondJump(condNotEqual());
                    {
                        const ctr_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadStack(.w64, ctr_reg, st.ctr_slot);
                        try self.emitAddImm(ctr_reg, ctr_reg, 1);
                        try self.codegen.emitStoreStack(.w64, st.ctr_slot, ctr_reg);
                        self.codegen.freeGeneral(ctr_reg);
                        const off_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadStack(.w64, off_reg, st.offset_slot);
                        try self.emitAddImm(off_reg, off_reg, @intCast(st.elem_size));
                        try self.codegen.emitStoreStack(.w64, st.offset_slot, off_reg);
                        self.codegen.freeGeneral(off_reg);
                    }
                    const back_patch = try self.codegen.emitJump();
                    self.codegen.patchJump(back_patch, st.loop_start);
                    self.codegen.patchJump(ne_patch, self.codegen.currentOffset());
                    try self.codegen.emitLoadImm(st.result_reg, 0);
                    const ne_done_patch = try self.codegen.emitJump();
                    self.codegen.patchJump(st.exit_patch, self.codegen.currentOffset());
                    self.codegen.patchJump(ne_done_patch, self.codegen.currentOffset());
                    self.codegen.patchJump(st.len_ne_patch, self.codegen.currentOffset());
                    self.codegen.patchJump(st.empty_patch, self.codegen.currentOffset());
                    self.allocator.destroy(st);
                },
            };

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

        /// Generate floating-point binary operation
        fn generateFloatBinop(
            self: *Self,
            op: lir.LowLevel,
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
        fn floatCondition(op: lir.LowLevel) ?Condition {
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
        fn generateNumAbs(self: *Self, val_loc: ValueLocation, operand_layout: layout.Idx) Allocator.Error!ValueLocation {
            const is_signed = switch (operand_layout) {
                .i8, .i16, .i32, .i64 => true,
                .u8, .u16, .u32, .u64 => false,
                .i128, .dec => true,
                .u128 => false,
                .f32, .f64 => return self.generateFloatAbs(val_loc),
                else => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: num_abs unsupported operand layout {s}",
                            .{@tagName(operand_layout)},
                        );
                    }
                    unreachable;
                },
            };

            if (!is_signed) {
                // Unsigned types: abs is identity
                return val_loc;
            }

            if (operand_layout == .i128 or operand_layout == .dec) {
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
        fn generateAbsDiff(self: *Self, a_loc: ValueLocation, b_loc: ValueLocation, ret_layout: layout.Idx, operand_layout: layout.Idx) Allocator.Error!ValueLocation {
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

            // 128-bit (I128, U128, Dec)
            if (ret_layout == .i128 or ret_layout == .u128 or ret_layout == .dec) {
                return try self.generateAbsDiff128(a_loc, b_loc, operand_layout);
            }

            // 64-bit or smaller integers: CMP, compute both a-b and b-a, CSEL/CMOV
            // Use operand layout for signedness since abs_diff returns unsigned (e.g.
            // I8.abs_diff returns U8), but the comparison must be signed.
            const is_signed = switch (operand_layout) {
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
                const shift_amount: u6 = switch (operand_layout) {
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

        /// Move register to register (architecture-specific)
        fn emitMovRegReg(self: *Self, dst: GeneralReg, src: GeneralReg) Allocator.Error!void {
            try self.codegen.emit.movRegReg(.w64, dst, src);
        }

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

        /// Bind tag payload fields to symbols after a tag pattern match.
        /// Computes the payload location for each arg and delegates to bindPattern,
        /// which handles all pattern types (bind, wildcard, tag, struct, list, as_pattern, etc.).
        /// Emit a compare of two registers
        fn emitCmpReg(self: *Self, reg1: GeneralReg, reg2: GeneralReg) Allocator.Error!void {
            try self.codegen.emit.cmpRegReg(.w64, reg1, reg2);
        }

        /// Store a discriminant value at the given offset
        fn storeDiscriminant(self: *Self, offset: i32, value: u16, disc_size: u8) Allocator.Error!void {
            if (disc_size == 0) return;

            const reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(reg, value);

            // Store appropriate size - architecture specific
            if (comptime target.toCpuArch() == .aarch64) {
                // aarch64 only has .w32 and .w64 for emitStoreStack, use direct emit for smaller sizes.
                switch (disc_size) {
                    1 => try self.codegen.emit.strbRegMemSoff(reg, .FP, offset),
                    2 => try self.codegen.emit.strhRegMemSoff(reg, .FP, offset),
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

        fn storeDiscriminantToPtr(self: *Self, ptr_reg: GeneralReg, offset: u32, value: u16, disc_size: u8) Allocator.Error!void {
            if (disc_size == 0) return;

            const reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(reg);
            try self.codegen.emitLoadImm(reg, value);

            switch (disc_size) {
                1 => {
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.strbRegMemSoff(reg, ptr_reg, @intCast(offset));
                    } else {
                        try self.codegen.emit.movMemReg(.w8, ptr_reg, @intCast(offset), reg);
                    }
                },
                2 => {
                    if (comptime target.toCpuArch() == .aarch64) {
                        try self.codegen.emit.strhRegMemSoff(reg, ptr_reg, @intCast(offset));
                    } else {
                        try self.codegen.emit.movMemReg(.w16, ptr_reg, @intCast(offset), reg);
                    }
                },
                4 => try self.emitStoreToPtr(.w32, reg, ptr_reg, @intCast(offset)),
                else => try self.emitStoreToPtr(.w64, reg, ptr_reg, @intCast(offset)),
            }
        }

        /// Map a layout index to the correct ValueLocation for a value on the stack.
        /// Multi-word types (strings, i128/Dec, lists) need specific location variants
        /// so downstream code loads the correct number of bytes.
        fn stackLocationForLayout(self: *Self, layout_idx: layout.Idx, stack_offset: i32) ValueLocation {
            const runtime_layout_idx = self.runtimeRepresentationLayoutIdx(layout_idx);
            if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec)
                return .{ .stack_i128 = stack_offset };
            if (runtime_layout_idx == .str)
                return .{ .stack_str = stack_offset };
            const ls = self.layout_store;
            const resolved = ls.getLayout(runtime_layout_idx);
            if (resolved.tag == .list or resolved.tag == .list_of_zst)
                return .{ .list_stack = .{ .struct_offset = stack_offset, .data_offset = 0, .num_elements = 0 } };
            const size = ls.layoutSizeAlign(resolved).size;
            return .{ .stack = .{
                .offset = stack_offset,
                .size = ValueSize.fromByteCount(size),
                .layout_idx = runtime_layout_idx,
            } };
        }

        fn normalizeValueLocationToLayout(
            self: *Self,
            loc: ValueLocation,
            actual_layout: layout.Idx,
            expected_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            if (actual_layout == expected_layout) return loc;

            const ls = self.layout_store;
            const actual_layout_val = ls.getLayout(actual_layout);
            switch (actual_layout_val.tag) {
                .box => {
                    if (actual_layout_val.getIdx() != expected_layout) return loc;

                    const expected_size = self.getLayoutSize(expected_layout);
                    if (expected_size == 0) return .{ .immediate_i64 = 0 };

                    const box_reg = try self.ensureInGeneralReg(loc);
                    defer self.codegen.freeGeneral(box_reg);

                    const result_offset = self.codegen.allocStackSlot(expected_size);
                    const temp_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(temp_reg);
                    try self.copyChunked(temp_reg, box_reg, 0, frame_ptr, result_offset, expected_size);

                    return self.stackLocationForLayout(expected_layout, result_offset);
                },
                .box_of_zst => {
                    if (expected_layout == .zst) return .{ .immediate_i64 = 0 };
                },
                else => {},
            }

            return loc;
        }

        fn requireExactValueLocationToLayout(
            self: *Self,
            loc: ValueLocation,
            actual_layout: layout.Idx,
            expected_layout: layout.Idx,
            comptime site: []const u8,
        ) ValueLocation {
            if (builtin.mode == .Debug and actual_layout != expected_layout) {
                const actual_layout_val = self.layout_store.getLayout(actual_layout);
                const expected_layout_val = self.layout_store.getLayout(expected_layout);
                const stmt_id: u32 = if (self.current_stmt_id) |current| @intFromEnum(current) else std.math.maxInt(u32);
                const stmt = if (self.current_stmt_id) |current| self.store.getCFStmt(current) else null;
                std.debug.panic(
                    "LIR/codegen invariant violated at {s} stmt {}: actual layout {} ({s}) did not match expected layout {} ({s}); stmt={any}",
                    .{
                        site,
                        stmt_id,
                        @intFromEnum(actual_layout),
                        @tagName(actual_layout_val.tag),
                        @intFromEnum(expected_layout),
                        @tagName(expected_layout_val.tag),
                        stmt,
                    },
                );
            }

            return self.coerceImmediateToLayout(loc, expected_layout);
        }

        fn requireExplicitNominalValueLocationToLayout(
            self: *Self,
            loc: ValueLocation,
            actual_layout: layout.Idx,
            expected_layout: layout.Idx,
            comptime site: []const u8,
        ) ValueLocation {
            if (builtin.mode == .Debug) {
                const actual_layout_val = self.layout_store.getLayout(actual_layout);
                const expected_layout_val = self.layout_store.getLayout(expected_layout);
                const actual_is_box = actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst;
                const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst;
                const actual_is_erased_ptr = actual_layout_val.tag == .scalar and actual_layout_val.getScalar().tag == .opaque_ptr;
                const expected_is_erased_ptr = expected_layout_val.tag == .scalar and expected_layout_val.getScalar().tag == .opaque_ptr;
                const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
                const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
                const boxing_compatible =
                    (actual_is_box == expected_is_box) or
                    (actual_is_box and expected_is_erased_ptr) or
                    (expected_is_box and actual_is_erased_ptr);
                if (!boxing_compatible or actual_is_list or expected_is_list) {
                    std.debug.panic(
                        "LIR/codegen invariant violated at {s}: explicit nominal reinterpret expected non-list layouts on the same side of layout boxing, got actual={} ({s}) expected={} ({s})",
                        .{
                            site,
                            @intFromEnum(actual_layout),
                            @tagName(actual_layout_val.tag),
                            @intFromEnum(expected_layout),
                            @tagName(expected_layout_val.tag),
                        },
                    );
                }
            }
            return self.coerceImmediateToLayout(loc, expected_layout);
        }

        fn requireExplicitListValueLocationToLayout(
            self: *Self,
            loc: ValueLocation,
            actual_layout: layout.Idx,
            expected_layout: layout.Idx,
            comptime site: []const u8,
        ) ValueLocation {
            if (builtin.mode == .Debug) {
                const actual_layout_val = self.layout_store.getLayout(actual_layout);
                const expected_layout_val = self.layout_store.getLayout(expected_layout);
                const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
                const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
                if (!actual_is_list or !expected_is_list) {
                    std.debug.panic(
                        "LIR/codegen invariant violated at {s}: explicit list reinterpret expected list layouts, got actual={} expected={}",
                        .{ site, @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                    );
                }
            }

            return self.coerceImmediateToLayout(loc, expected_layout);
        }

        /// Emit a correctly-sized raw load from the stack, zero-extending sub-word
        /// values to 64 bits. This is appropriate for structural byte copies.
        fn emitSizedLoadStack(self: *Self, reg: GeneralReg, offset: i32, size: ValueSize) Allocator.Error!void {
            switch (size) {
                .byte => try self.emitLoadStackW8(reg, offset),
                .word => try self.emitLoadStackW16(reg, offset),
                .dword => try self.codegen.emitLoadStack(.w32, reg, offset),
                .qword => try self.codegen.emitLoadStack(.w64, reg, offset),
            }
        }

        /// Emit a semantic load for a stack-backed value, restoring signed narrow
        /// integers to their proper 64-bit register representation.
        fn emitValueLoadStack(self: *Self, reg: GeneralReg, offset: i32, size: ValueSize, layout_idx: layout.Idx) Allocator.Error!void {
            try self.emitSizedLoadStack(reg, offset, size);

            const shift_amount: u8 = switch (layout_idx) {
                .i8 => 56,
                .i16 => 48,
                .i32 => 32,
                else => return,
            };

            try self.emitShlImm(.w64, reg, reg, shift_amount);
            try self.emitAsrImm(.w64, reg, reg, shift_amount);
        }

        /// Emit a correctly-sized load from memory (arbitrary base register + offset),
        /// zero-extending sub-word values to 64 bits.
        fn emitSizedLoadMem(self: *Self, dst: GeneralReg, base_reg: GeneralReg, offset: i32, size: ValueSize) Allocator.Error!void {
            switch (size) {
                .byte => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        try self.codegen.emit.ldrbRegMemSoff(dst, base_reg, offset);
                    } else {
                        try self.codegen.emit.movzxBRegMem(dst, base_reg, offset);
                    }
                },
                .word => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        try self.codegen.emit.ldrhRegMemSoff(dst, base_reg, offset);
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
                        try self.codegen.emit.strbRegMemSoff(src, base_reg, offset);
                    } else {
                        try self.codegen.emit.movMemReg(.w8, base_reg, offset, src);
                    }
                },
                .word => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        try self.codegen.emit.strhRegMemSoff(src, base_reg, offset);
                    } else {
                        try self.codegen.emit.movMemReg(.w16, base_reg, offset, src);
                    }
                },
                .dword => try self.emitStore(.w32, base_reg, offset, src),
                .qword => try self.emitStore(.w64, base_reg, offset, src),
            }
        }

        /// Get the register used for argument N in the calling convention.
        fn getArgumentRegister(_: *Self, index: u8) GeneralReg {
            if (index >= max_arg_regs) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "dev backend ABI invariant violated: argument register index {d} exceeds available register count {d}",
                        .{ index, max_arg_regs },
                    );
                }
                unreachable;
            }
            return EmitType.CC.PARAM_REGS[index];
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
        fn emitCallToOffset(self: *Self, target_offset: usize) Allocator.Error!void {
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
                    self.patchInternalCodeAddress(patch.instr_offset, patch.target_offset);
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

        fn shiftPendingProcAddrs(
            self: *Self,
            body_start: usize,
            body_end: usize,
            prologue_size: usize,
        ) void {
            for (self.pending_proc_addrs.items) |*pending| {
                if (pending.instr_offset >= body_start and pending.instr_offset < body_end) {
                    pending.instr_offset += prologue_size;
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

        fn emitInternalCodeAddress(self: *Self, target_offset: usize, dst_reg: GeneralReg) Allocator.Error!void {
            const current = self.codegen.currentOffset();
            if (comptime target.toCpuArch() == .aarch64) {
                // ADR alone has only ±1 MB range, which is exceeded by larger programs.
                // ADRP+ADD would extend that, but its page-relative form requires the
                // emit buffer's runtime base to be 4 KB-aligned — and the buffer here is
                // appended into the host's __TEXT at an unaligned offset. Stay purely
                // PC-relative by emitting ADR (anchor at this instruction) plus two
                // ADD/SUB imm12 instructions (hi shifted by LSL #12, then lo). This
                // supports offsets up to ±16 MB without any base-alignment assumption.
                try self.emitAarch64PcRelAddress(dst_reg, current, target_offset);
            } else {
                const rel: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(current)) - 7);
                try self.codegen.emit.leaRegRipRel(dst_reg, rel);
            }

            try self.internal_addr_patches.append(self.allocator, .{
                .instr_offset = current,
                .target_offset = target_offset,
            });
        }

        /// Emit a 3-instruction PC-relative address calculation on aarch64.
        /// Layout (12 bytes): ADR Xd, anchor (offset 0) — ADD/SUB Xd, Xd, #hi12, LSL #12 — ADD/SUB Xd, Xd, #lo12.
        /// `anchor` is the address of the ADR instruction. The final value of Xd equals
        /// `anchor + (target_off - anchor)`. Caller must ensure |target_off - anchor| ≤ 0xFFFFFF.
        fn emitAarch64PcRelAddress(self: *Self, dst: GeneralReg, anchor: usize, target_off: usize) Allocator.Error!void {
            const rel: i64 = @as(i64, @intCast(target_off)) - @as(i64, @intCast(anchor));
            const negative = rel < 0;
            const abs_rel: u64 = if (negative) @intCast(-rel) else @intCast(rel);
            const hi12: u12 = @truncate(abs_rel >> 12);
            const lo12: u12 = @truncate(abs_rel);

            try self.codegen.emit.adr(dst, 0);
            if (negative) {
                try self.codegen.emit.subRegRegImm12Shifted(.w64, dst, dst, hi12, true);
                try self.codegen.emit.subRegRegImm12(.w64, dst, dst, lo12);
            } else {
                try self.codegen.emit.addRegRegImm12Shifted(.w64, dst, dst, hi12, true);
                try self.codegen.emit.addRegRegImm12(.w64, dst, dst, lo12);
            }
        }

        fn emitPendingProcAddress(self: *Self, target_proc: lir.LIR.LirProcSpecId, dst_reg: GeneralReg) Allocator.Error!void {
            const current = self.codegen.currentOffset();
            if (comptime target.toCpuArch() == .aarch64) {
                // Reserve a 3-instruction (12-byte) PC-relative address sequence so the
                // patcher can rewrite ADR + ADD/SUB(hi) + ADD/SUB(lo) once the target
                // proc's code offset is known. See emitAarch64PcRelAddress.
                try self.codegen.emit.adr(dst_reg, 0);
                try self.codegen.emit.addRegRegImm12Shifted(.w64, dst_reg, dst_reg, 0, true);
                try self.codegen.emit.addRegRegImm12(.w64, dst_reg, dst_reg, 0);
            } else {
                try self.codegen.emit.leaRegRipRel(dst_reg, 0);
            }
            try self.pending_proc_addrs.append(self.allocator, .{
                .instr_offset = current,
                .target_proc = target_proc,
            });
        }

        /// Schedule an RC helper for compilation on the current worklist drain.
        /// No-op if it is already compiled or already scheduled.
        fn scheduleRcHelper(self: *Self, helper: RcHelperVariant) Allocator.Error!void {
            const cache_key = helper.encode();
            if (self.compiled_rc_helpers.contains(cache_key)) return;
            const gop = try self.rc_helper_scheduled.getOrPut(cache_key);
            if (gop.found_existing) return;
            try self.rc_helper_worklist.append(self.allocator, helper);
        }

        /// Emit a placeholder BL/CALL to an RC helper, resolved once the helper's
        /// compiled offset is known (see resolveRcHelperPending).
        fn emitPendingRcCall(self: *Self, helper: RcHelperVariant) Allocator.Error!void {
            const call_site = self.codegen.currentOffset();
            try self.pending_rc_calls.append(self.allocator, .{ .instr_offset = call_site, .target_key = helper.encode() });
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.bl(0);
            } else {
                try self.codegen.emit.call(@bitCast(@as(i32, 0)));
            }
        }

        /// Emit a placeholder PC-relative address literal for an RC helper.
        fn emitPendingRcAddr(self: *Self, helper: RcHelperVariant, dst_reg: GeneralReg) Allocator.Error!void {
            const current = self.codegen.currentOffset();
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.adr(dst_reg, 0);
                try self.codegen.emit.addRegRegImm12Shifted(.w64, dst_reg, dst_reg, 0, true);
                try self.codegen.emit.addRegRegImm12(.w64, dst_reg, dst_reg, 0);
            } else {
                try self.codegen.emit.leaRegRipRel(dst_reg, 0);
            }
            try self.pending_rc_addrs.append(self.allocator, .{ .instr_offset = current, .target_key = helper.encode() });
        }

        /// Move pending RC refs emitted inside a body that shifted forward by a
        /// prepended prologue so they can still be patched later.
        fn shiftPendingRcRefs(self: *Self, body_start: usize, body_end: usize, prologue_size: usize) void {
            for (self.pending_rc_calls.items) |*ref| {
                if (ref.instr_offset >= body_start and ref.instr_offset < body_end) ref.instr_offset += prologue_size;
            }
            for (self.pending_rc_addrs.items) |*ref| {
                if (ref.instr_offset >= body_start and ref.instr_offset < body_end) ref.instr_offset += prologue_size;
            }
        }

        /// Drain the RC-helper worklist iteratively (compiling each scheduled
        /// helper exactly once) and resolve every pending RC reference. Re-entrant
        /// calls (from within a helper body) only schedule; the outermost call
        /// drives the drain.
        fn maybeDrainRcHelpers(self: *Self) Allocator.Error!void {
            if (self.compiling_rc_helpers) return;
            self.compiling_rc_helpers = true;
            defer self.compiling_rc_helpers = false;

            while (self.rc_helper_worklist.pop()) |helper| {
                _ = try self.compileSingleRcHelper(helper);
            }

            for (self.pending_rc_calls.items) |ref| {
                const offset = self.compiled_rc_helpers.get(ref.target_key) orelse unreachable;
                self.patchCallTarget(ref.instr_offset, offset);
                try self.internal_call_patches.append(self.allocator, .{ .call_offset = ref.instr_offset, .target_offset = offset });
            }
            for (self.pending_rc_addrs.items) |ref| {
                const offset = self.compiled_rc_helpers.get(ref.target_key) orelse unreachable;
                self.patchInternalCodeAddress(ref.instr_offset, offset);
                try self.internal_addr_patches.append(self.allocator, .{ .instr_offset = ref.instr_offset, .target_offset = offset });
            }
            self.pending_rc_calls.clearRetainingCapacity();
            self.pending_rc_addrs.clearRetainingCapacity();
            self.rc_helper_scheduled.clearRetainingCapacity();
        }

        fn emitCallRcHelperFromStackSlots(
            self: *Self,
            helper: RcHelperVariant,
            ptr_slot: i32,
            count_slot: ?i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const arg0 = self.getArgumentRegister(0);
            try self.emitLoad(.w64, arg0, frame_ptr, ptr_slot);

            switch (helper.key.op) {
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

            try self.emitPendingRcCall(helper);
            try self.scheduleRcHelper(helper);
        }

        fn emitRcHelperCallAtStackOffset(
            self: *Self,
            helper: RcHelperVariant,
            base_offset: i32,
            count: u16,
        ) Allocator.Error!void {
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

            switch (helper.key.op) {
                .incref => {
                    const count_slot = self.codegen.allocStackSlot(8);
                    const count_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(count_reg, count);
                    try self.emitStore(.w64, frame_ptr, count_slot, count_reg);
                    self.codegen.freeGeneral(count_reg);
                    try self.emitCallRcHelperFromStackSlots(helper, ptr_slot, count_slot, roc_ops_slot);
                },
                .decref, .free => {
                    try self.emitCallRcHelperFromStackSlots(helper, ptr_slot, null, roc_ops_slot);
                },
            }

            try self.maybeDrainRcHelpers();
        }

        fn emitExplicitRcHelperCallForValue(
            self: *Self,
            helper: RcHelperVariant,
            value_loc: ValueLocation,
            count: u16,
        ) Allocator.Error!void {
            const helper_plan = self.layout_store.rcHelperPlan(helper.key);
            if (helper_plan == .noop) {
                if (builtin.mode == .Debug) {
                    std.debug.panic("LIR/codegen invariant violated: explicit RC statement used noop helper for layout {d}", .{@intFromEnum(helper.key.layout_idx)});
                }
                unreachable;
            }

            const layout_val = self.layout_store.getLayout(helper.key.layout_idx);
            const value_size = self.layout_store.layoutSizeAlign(layout_val).size;
            if (value_size == 0) {
                if (builtin.mode == .Debug) {
                    std.debug.panic("LIR/codegen invariant violated: explicit RC statement used zero-sized helper layout {d}", .{@intFromEnum(helper.key.layout_idx)});
                }
                unreachable;
            }

            const base_offset = try self.ensureValueOnStackForRc(value_loc, value_size);
            try self.emitRcHelperCallAtStackOffset(helper, base_offset, count);
        }

        fn emitRawRcHelperCallFromPtrReg(
            self: *Self,
            helper: RcHelperVariant,
            ptr_reg: GeneralReg,
            count_slot: ?i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const ptr_slot = self.codegen.allocStackSlot(8);
            try self.emitStore(.w64, frame_ptr, ptr_slot, ptr_reg);
            try self.emitCallRcHelperFromStackSlots(helper, ptr_slot, count_slot, roc_ops_slot);
        }

        /// Materialize the address of an RC helper destined for a
        /// runtime-checked list op's element callback. That RC is internal to
        /// the op, which serves both modes and makes no thread-confinement
        /// claim, so the helper is always the atomic variant.
        fn emitBuiltinInternalOptionalRcHelperAddress(
            self: *Self,
            op: RcOp,
            layout_idx: layout.Idx,
        ) Allocator.Error!?GeneralReg {
            const helper = RcHelperVariant{
                .key = .{ .op = op, .layout_idx = layout_idx },
                .atomicity = .atomic,
            };
            if (self.layout_store.rcHelperPlan(helper.key) == .noop) return null;

            const callback_reg = try self.allocTempGeneral();
            try self.emitPendingRcAddr(helper, callback_reg);
            try self.scheduleRcHelper(helper);
            try self.maybeDrainRcHelpers();
            return callback_reg;
        }

        fn loadStrDataPtrForRcFromValuePtr(
            self: *Self,
            value_ptr_reg: GeneralReg,
            out_reg: GeneralReg,
        ) Allocator.Error!void {
            try self.emitLoad(.w64, out_reg, value_ptr_reg, 0);

            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);
            try self.emitLoad(.w64, cap_reg, value_ptr_reg, 8);

            const tag_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(tag_reg);
            try self.codegen.emitLoadImm(tag_reg, 1);
            try self.emitAndRegs(.w64, tag_reg, tag_reg, cap_reg);
            try self.emitCmpImm(tag_reg, 0);
            const done_patch = try self.emitJumpIfEqual();

            try self.codegen.emitLoadImm(out_reg, -2);
            try self.emitAndRegs(.w64, out_reg, out_reg, cap_reg);
            self.codegen.patchJump(done_patch, self.codegen.currentOffset());
        }

        fn emitBuiltinInternalRcHelperStrIncref(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
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
            try self.callBuiltin(&builder, fn_addr, builtin_fn);

            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        fn emitBuiltinInternalRcHelperStrDrop(
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

        fn emitBuiltinInternalRcHelperListIncref(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
            list_plan: layout.RcListPlan,
            ptr_slot: i32,
            count_slot: i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            const value_ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(value_ptr_reg);
            try self.emitLoad(.w64, value_ptr_reg, frame_ptr, ptr_slot);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addMemArg(value_ptr_reg, 0);
            try builder.addMemArg(value_ptr_reg, 8);
            try builder.addMemArg(value_ptr_reg, 16);
            try builder.addMemArg(frame_ptr, count_slot);
            try builder.addImmArg(@intFromBool(list_plan.child != null));
            try builder.addMemArg(frame_ptr, roc_ops_slot);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
        }

        fn emitBuiltinInternalRcHelperListDrop(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
            list_plan: layout.RcListPlan,
            atomicity: RcAtomicity,
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
                // The element callback's C ABI carries no atomicity parameter,
                // so the statement's atomicity is baked into which helper
                // variant the pointer names. Visibility is containment-closed
                // (design.md "Thread-Confined Reference Counts"), so a
                // single-thread teardown covers the elements as well.
                const child = RcHelperVariant{ .key = child_key, .atomicity = atomicity };
                try self.emitPendingRcAddr(child, callback_reg);
                try self.scheduleRcHelper(child);
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

        fn emitBuiltinInternalRcHelperBoxIncref(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
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
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
        }

        fn emitBuiltinInternalRcHelperBoxDrop(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
            box_plan: layout.RcBoxPlan,
            atomicity: RcAtomicity,
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
                // The payload callback's C ABI carries no atomicity parameter,
                // so the statement's atomicity is baked into which helper
                // variant the pointer names (see emitBuiltinInternalRcHelperListDrop).
                const child = RcHelperVariant{ .key = child_key, .atomicity = atomicity };
                try self.emitPendingRcAddr(child, callback_reg);
                try self.scheduleRcHelper(child);
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

        fn emitBuiltinInternalRcHelperErasedCallableIncref(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
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
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
        }

        fn emitBuiltinInternalRcHelperErasedCallableDrop(
            self: *Self,
            builtin_fn: BuiltinFn,
            fn_addr: usize,
            ptr_slot: i32,
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
            try builder.addMemArg(frame_ptr, roc_ops_slot);
            try self.callBuiltin(&builder, fn_addr, builtin_fn);
        }

        fn generateBuiltinInternalRcHelperBody(
            self: *Self,
            helper: RcHelperVariant,
            ptr_slot: i32,
            count_slot: ?i32,
            roc_ops_slot: i32,
        ) Allocator.Error!void {
            // The helper's own count updates use the runtime entry matching its
            // atomicity. `free` leaves deallocate without touching the count,
            // so they have no single-thread counterpart. Child helpers reached
            // by direct call propagate the atomicity, and so do the element and
            // payload callbacks handed to the teardown builtins (see
            // emitBuiltinInternalRcHelperListDrop / BoxDrop); only the erased
            // callable's `on_drop` stays atomic because it is selected at
            // closure creation, where no RC statement exists.
            const single_thread = helper.atomicity == .single_thread;
            switch (self.layout_store.rcHelperPlan(helper.key)) {
                .noop => {},
                .str_incref => if (single_thread)
                    try self.emitBuiltinInternalRcHelperStrIncref(.incref_data_ptr_single_thread, @intFromPtr(&increfDataPtrSingleThreadC), ptr_slot, count_slot.?, roc_ops_slot)
                else
                    try self.emitBuiltinInternalRcHelperStrIncref(.incref_data_ptr, @intFromPtr(&increfDataPtrC), ptr_slot, count_slot.?, roc_ops_slot),
                .str_decref => if (single_thread)
                    try self.emitBuiltinInternalRcHelperStrDrop(.decref_data_ptr_single_thread, @intFromPtr(&decrefDataPtrSingleThreadC), ptr_slot, roc_ops_slot)
                else
                    try self.emitBuiltinInternalRcHelperStrDrop(.decref_data_ptr, @intFromPtr(&decrefDataPtrC), ptr_slot, roc_ops_slot),
                .str_free => try self.emitBuiltinInternalRcHelperStrDrop(.free_data_ptr, @intFromPtr(&freeDataPtrC), ptr_slot, roc_ops_slot),
                .list_incref => |list_plan| if (single_thread)
                    try self.emitBuiltinInternalRcHelperListIncref(.list_incref_single_thread, @intFromPtr(&dev_wrappers.roc_builtins_list_incref_single_thread), list_plan, ptr_slot, count_slot.?, roc_ops_slot)
                else
                    try self.emitBuiltinInternalRcHelperListIncref(.list_incref, @intFromPtr(&dev_wrappers.roc_builtins_list_incref), list_plan, ptr_slot, count_slot.?, roc_ops_slot),
                .list_decref => |list_plan| try self.emitBuiltinInternalRcHelperListDrop(
                    if (single_thread) .list_decref_with_single_thread else .list_decref_with,
                    if (single_thread) @intFromPtr(&dev_wrappers.roc_builtins_list_decref_with_single_thread) else @intFromPtr(&dev_wrappers.roc_builtins_list_decref_with),
                    list_plan,
                    helper.atomicity,
                    ptr_slot,
                    roc_ops_slot,
                ),
                .list_free => |list_plan| try self.emitBuiltinInternalRcHelperListDrop(
                    .list_free_with,
                    @intFromPtr(&dev_wrappers.roc_builtins_list_free_with),
                    list_plan,
                    helper.atomicity,
                    ptr_slot,
                    roc_ops_slot,
                ),
                .box_incref => if (single_thread)
                    try self.emitBuiltinInternalRcHelperBoxIncref(.incref_data_ptr_single_thread, @intFromPtr(&increfDataPtrSingleThreadC), ptr_slot, count_slot.?, roc_ops_slot)
                else
                    try self.emitBuiltinInternalRcHelperBoxIncref(.incref_data_ptr, @intFromPtr(&increfDataPtrC), ptr_slot, count_slot.?, roc_ops_slot),
                .box_decref => |box_plan| try self.emitBuiltinInternalRcHelperBoxDrop(
                    if (single_thread) .box_decref_with_single_thread else .box_decref_with,
                    if (single_thread) @intFromPtr(&dev_wrappers.roc_builtins_box_decref_with_single_thread) else @intFromPtr(&dev_wrappers.roc_builtins_box_decref_with),
                    box_plan,
                    helper.atomicity,
                    ptr_slot,
                    roc_ops_slot,
                ),
                .box_free => |box_plan| try self.emitBuiltinInternalRcHelperBoxDrop(
                    .box_free_with,
                    @intFromPtr(&dev_wrappers.roc_builtins_box_free_with),
                    box_plan,
                    helper.atomicity,
                    ptr_slot,
                    roc_ops_slot,
                ),
                // An erased callable's incref is exactly a data-pointer incref
                // of its single allocation.
                .erased_callable_incref => if (single_thread)
                    try self.emitBuiltinInternalRcHelperErasedCallableIncref(.incref_data_ptr_single_thread, @intFromPtr(&increfDataPtrSingleThreadC), ptr_slot, count_slot.?, roc_ops_slot)
                else
                    try self.emitBuiltinInternalRcHelperErasedCallableIncref(.erased_callable_incref, @intFromPtr(&dev_wrappers.roc_builtins_erased_callable_incref), ptr_slot, count_slot.?, roc_ops_slot),
                .erased_callable_decref => try self.emitBuiltinInternalRcHelperErasedCallableDrop(
                    if (single_thread) .erased_callable_decref_single_thread else .erased_callable_decref,
                    if (single_thread) @intFromPtr(&dev_wrappers.roc_builtins_erased_callable_decref_single_thread) else @intFromPtr(&dev_wrappers.roc_builtins_erased_callable_decref),
                    ptr_slot,
                    roc_ops_slot,
                ),
                .erased_callable_free => try self.emitBuiltinInternalRcHelperErasedCallableDrop(
                    .erased_callable_free,
                    @intFromPtr(&dev_wrappers.roc_builtins_erased_callable_free),
                    ptr_slot,
                    roc_ops_slot,
                ),
                .struct_ => |struct_plan| {
                    const field_count = self.layout_store.rcHelperStructFieldCount(struct_plan);
                    var i: u32 = 0;
                    while (i < field_count) : (i += 1) {
                        const field_plan = self.layout_store.rcHelperStructFieldPlan(struct_plan, i) orelse continue;
                        const field_ptr_reg = try self.allocTempGeneral();
                        defer self.codegen.freeGeneral(field_ptr_reg);

                        try self.emitLoad(.w64, field_ptr_reg, frame_ptr, ptr_slot);
                        try self.emitAddPtrImmAny(field_ptr_reg, field_ptr_reg, @intCast(field_plan.offset));
                        try self.emitRawRcHelperCallFromPtrReg(
                            .{ .key = field_plan.child, .atomicity = helper.atomicity },
                            field_ptr_reg,
                            count_slot,
                            roc_ops_slot,
                        );
                    }
                },
                .tag_union => |tag_plan| {
                    const variant_count = self.layout_store.rcHelperTagUnionVariantCount(tag_plan);
                    if (variant_count == 0) return;

                    if (variant_count == 1) {
                        if (self.layout_store.rcHelperTagUnionVariantPlan(tag_plan, 0)) |child_key| {
                            const payload_reg = try self.allocTempGeneral();
                            defer self.codegen.freeGeneral(payload_reg);
                            try self.emitLoad(.w64, payload_reg, frame_ptr, ptr_slot);
                            try self.emitRawRcHelperCallFromPtrReg(
                                .{ .key = child_key, .atomicity = helper.atomicity },
                                payload_reg,
                                count_slot,
                                roc_ops_slot,
                            );
                        }
                        return;
                    }

                    const disc_offset: i32 = @intCast(self.layout_store.rcHelperTagUnionDiscriminantOffset(tag_plan));
                    const disc_size = self.layout_store.rcHelperTagUnionDiscriminantSize(tag_plan);
                    const total_size = self.layout_store.rcHelperTagUnionTotalSize(tag_plan);
                    const disc_use_w32 = (disc_offset + 8 > @as(i32, @intCast(total_size)));

                    var done_patches: std.ArrayList(usize) = .empty;
                    defer done_patches.deinit(self.allocator);

                    var variant_i: u32 = 0;
                    while (variant_i < variant_count) : (variant_i += 1) {
                        const child_key = self.layout_store.rcHelperTagUnionVariantPlan(tag_plan, variant_i) orelse continue;

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
                        try self.emitRawRcHelperCallFromPtrReg(
                            .{ .key = child_key, .atomicity = helper.atomicity },
                            payload_reg,
                            count_slot,
                            roc_ops_slot,
                        );
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
                    try self.emitRawRcHelperCallFromPtrReg(
                        .{ .key = child_key, .atomicity = helper.atomicity },
                        captures_reg,
                        count_slot,
                        roc_ops_slot,
                    );
                },
            }
        }

        fn compileSingleRcHelper(self: *Self, helper: RcHelperVariant) Allocator.Error!usize {
            const cache_key = helper.encode();
            if (self.compiled_rc_helpers.get(cache_key)) |code_offset| {
                return code_offset;
            }

            const helper_plan = self.layout_store.rcHelperPlan(helper.key);
            if (helper_plan == .noop) {
                if (builtin.mode == .Debug) {
                    std.debug.panic("attempted to compile noop RC helper for layout {d}", .{@intFromEnum(helper.key.layout_idx)});
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
            const saved_uses_caller_stack_arg_base = self.uses_caller_stack_arg_base;
            // Reset register state for new function scope — each RC helper is a
            // separate callable with its own prologue/epilogue, so it starts with
            // a full set of registers regardless of what the parent is using.
            self.codegen.callee_saved_used = 0;
            self.codegen.callee_saved_available = CodeGen.CALLEE_SAVED_GENERAL_MASK;
            self.codegen.free_general = CodeGen.INITIAL_FREE_GENERAL;
            self.codegen.general_owners = [_]?u32{null} ** CodeGen.NUM_GENERAL_REGS;
            self.codegen.free_float = CodeGen.INITIAL_FREE_FLOAT;
            self.codegen.float_owners = [_]?u32{null} ** CodeGen.NUM_FLOAT_REGS;
            self.roc_ops_reg = null;
            self.ret_ptr_slot = null;
            self.uses_caller_stack_arg_base = false;

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
                self.uses_caller_stack_arg_base = saved_uses_caller_stack_arg_base;
                self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
            }

            const ptr_slot = self.codegen.allocStackSlot(8);
            const roc_ops_slot = self.codegen.allocStackSlot(8);
            const ptr_arg_reg = self.getArgumentRegister(0);
            try self.codegen.emitStoreStack(.w64, ptr_slot, ptr_arg_reg);

            var count_slot: ?i32 = null;
            switch (helper.key.op) {
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

            try self.generateBuiltinInternalRcHelperBody(helper, ptr_slot, count_slot, roc_ops_slot);

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
                self.shiftPendingRcRefs(body_start, body_end, prologue_size);
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
                self.shiftPendingRcRefs(body_start, body_end, prologue_size);
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
            self.ret_ptr_slot = saved_ret_ptr_slot;
            self.uses_caller_stack_arg_base = saved_uses_caller_stack_arg_base;

            self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
            return final_offset;
        }

        /// Given a field's stack base offset and semantic layout index, return the appropriate ValueLocation.
        fn fieldLocationFromLayout(self: *Self, field_base: i32, _: u32, field_layout_idx: layout.Idx) ValueLocation {
            return switch (field_layout_idx) {
                .zst => .{ .immediate_i64 = 0 },
                else => self.stackLocationForLayout(field_layout_idx, field_base),
            };
        }

        /// Generate code for struct field access (records and tuples).
        fn generateStructAccess(self: *Self, access: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const raw_struct_loc = try self.emitValueLocal(access.source);
            const source_layout_idx = self.localLayout(access.source);
            const source_layout = ls.getLayout(source_layout_idx);
            const base_layout_idx = switch (source_layout.tag) {
                .box => source_layout.getIdx(),
                else => source_layout_idx,
            };
            const struct_loc = try self.normalizeValueLocationToLayout(raw_struct_loc, source_layout_idx, base_layout_idx);
            const struct_layout = ls.getLayout(base_layout_idx);
            if (struct_layout.tag != .struct_) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: struct_access expected struct_ layout, got {s} (field_idx={d})",
                        .{ @tagName(struct_layout.tag), access.field_idx },
                    );
                }
                unreachable;
            }

            const field_offset = ls.getStructFieldOffsetByOriginalIndex(struct_layout.getStruct().idx, access.field_idx);
            const field_size = ls.getStructFieldSizeByOriginalIndex(struct_layout.getStruct().idx, access.field_idx);
            const actual_field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(struct_layout.getStruct().idx, access.field_idx);
            const raw_field_loc = switch (struct_loc) {
                .stack_str => |sv| blk: {
                    const field_base = sv + @as(i32, @intCast(field_offset));
                    break :blk self.fieldLocationFromLayout(field_base, field_size, actual_field_layout_idx);
                },
                .stack => |sv| blk: {
                    const field_base = sv.offset + @as(i32, @intCast(field_offset));
                    break :blk self.fieldLocationFromLayout(field_base, field_size, actual_field_layout_idx);
                },
                .stack_i128 => |sv| blk: {
                    const field_base = sv + @as(i32, @intCast(field_offset));
                    break :blk self.fieldLocationFromLayout(field_base, field_size, actual_field_layout_idx);
                },
                .general_reg => |reg| blk: {
                    if (field_size > 8) unreachable;
                    if (field_offset == 0) {
                        break :blk ValueLocation{ .general_reg = reg };
                    } else {
                        const result_reg = try self.allocTempGeneral();
                        try self.emitLsrImm(.w64, result_reg, reg, @intCast(field_offset * 8));
                        self.codegen.freeGeneral(reg);
                        break :blk ValueLocation{ .general_reg = result_reg };
                    }
                },
                .immediate_i64 => |val| blk: {
                    if (field_size > 8) unreachable;
                    if (field_offset == 0) {
                        break :blk ValueLocation{ .immediate_i64 = val };
                    }
                    const shifted = val >> @intCast(field_offset * 8);
                    break :blk ValueLocation{ .immediate_i64 = shifted };
                },
                else => unreachable,
            };
            return self.requireExactValueLocationToLayout(raw_field_loc, actual_field_layout_idx, access.target_layout, "struct_field_access");
        }

        /// Generate code for tag payload access.
        fn generateTagPayloadAccess(self: *Self, tpa: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const raw_value_loc = try self.emitValueLocal(tpa.source);
            const source_layout_idx = self.localLayout(tpa.source);
            const union_layout = ls.getLayout(source_layout_idx);
            const payload_layout_idx = switch (union_layout.tag) {
                .tag_union => blk: {
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(union_layout.getTagUnion().idx));
                    break :blk variants.get(tpa.variant_index).payload_layout;
                },
                .box => blk: {
                    const inner_layout = ls.getLayout(union_layout.getIdx());
                    if (inner_layout.tag != .tag_union) {
                        return raw_value_loc;
                    }
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(inner_layout.getTagUnion().idx));
                    break :blk variants.get(tpa.variant_index).payload_layout;
                },
                else => tpa.target_layout,
            };
            const payload_layout = ls.getLayout(payload_layout_idx);
            const PayloadSelection = struct {
                offset: u32,
                layout_idx: layout.Idx,
            };
            const selected: PayloadSelection = switch (payload_layout.tag) {
                .struct_ => .{
                    .offset = ls.getStructFieldOffsetByOriginalIndex(payload_layout.getStruct().idx, tpa.payload_idx),
                    .layout_idx = ls.getStructFieldLayoutByOriginalIndex(payload_layout.getStruct().idx, tpa.payload_idx),
                },
                else => blk: {
                    if (builtin.mode == .Debug and tpa.payload_idx != 0) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: scalar tag payload access requested payload_idx {d}",
                            .{tpa.payload_idx},
                        );
                    }
                    break :blk .{
                        .offset = @as(u32, 0),
                        .layout_idx = payload_layout_idx,
                    };
                },
            };
            const selected_size = ls.layoutSizeAlign(ls.getLayout(selected.layout_idx)).size;

            if (union_layout.tag == .tag_union) {
                const value_loc = try self.materializeValueToStackForLayout(raw_value_loc, source_layout_idx);
                const base_offset: i32 = switch (value_loc) {
                    .stack => |s| s.offset,
                    .stack_i128 => |off| off,
                    .stack_str => |off| off,
                    .list_stack => |ls_info| ls_info.struct_offset,
                    else => unreachable,
                };
                const raw_payload_loc = self.fieldLocationFromLayout(base_offset + @as(i32, @intCast(selected.offset)), selected_size, selected.layout_idx);
                return self.requireExactValueLocationToLayout(raw_payload_loc, selected.layout_idx, tpa.target_layout, "tag_payload_access.inline");
            } else if (union_layout.tag == .box) {
                const inner_layout = ls.getLayout(union_layout.getIdx());
                if (inner_layout.tag == .tag_union) {
                    const box_ptr_reg = try self.ensureInGeneralReg(raw_value_loc);
                    defer self.codegen.freeGeneral(box_ptr_reg);
                    if (selected_size == 0) {
                        const raw_payload_loc = self.fieldLocationFromLayout(0, selected_size, selected.layout_idx);
                        return self.requireExactValueLocationToLayout(raw_payload_loc, selected.layout_idx, tpa.target_layout, "tag_payload_access.boxed");
                    }
                    const dest_offset = self.codegen.allocStackSlot(selected_size);
                    const temp_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(temp_reg);
                    try self.copyChunked(
                        temp_reg,
                        box_ptr_reg,
                        @intCast(selected.offset),
                        frame_ptr,
                        dest_offset,
                        selected_size,
                    );
                    const raw_payload_loc = self.fieldLocationFromLayout(dest_offset, selected_size, selected.layout_idx);
                    return self.requireExactValueLocationToLayout(raw_payload_loc, selected.layout_idx, tpa.target_layout, "tag_payload_access.boxed");
                } else {
                    return raw_value_loc;
                }
            } else if (union_layout.tag == .scalar or union_layout.tag == .zst) {
                return raw_value_loc;
            } else {
                unreachable;
            }
        }

        fn generateTagPayloadStructAccess(self: *Self, tps: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const raw_value_loc = try self.emitValueLocal(tps.source);
            const source_layout_idx = self.localLayout(tps.source);
            const union_layout = ls.getLayout(source_layout_idx);
            const payload_layout_idx = switch (union_layout.tag) {
                .tag_union => blk: {
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(union_layout.getTagUnion().idx));
                    break :blk variants.get(tps.variant_index).payload_layout;
                },
                .box => blk: {
                    const inner_layout = ls.getLayout(union_layout.getIdx());
                    if (inner_layout.tag != .tag_union) {
                        return raw_value_loc;
                    }
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(inner_layout.getTagUnion().idx));
                    break :blk variants.get(tps.variant_index).payload_layout;
                },
                else => tps.target_layout,
            };
            const payload_size = ls.layoutSizeAlign(ls.getLayout(payload_layout_idx)).size;
            if (union_layout.tag == .tag_union) {
                const value_loc = try self.materializeValueToStackForLayout(raw_value_loc, source_layout_idx);
                const base_offset: i32 = switch (value_loc) {
                    .stack => |s| s.offset,
                    .stack_i128 => |off| off,
                    .stack_str => |off| off,
                    .list_stack => |ls_info| ls_info.struct_offset,
                    else => unreachable,
                };
                const raw_payload_loc = self.fieldLocationFromLayout(base_offset, payload_size, payload_layout_idx);
                return self.requireExactValueLocationToLayout(raw_payload_loc, payload_layout_idx, tps.target_layout, "tag_payload_struct_access.inline");
            } else if (union_layout.tag == .box) {
                const inner_layout = ls.getLayout(union_layout.getIdx());
                if (inner_layout.tag == .tag_union) {
                    const box_ptr_reg = try self.ensureInGeneralReg(raw_value_loc);
                    const dest_offset = self.codegen.allocStackSlot(payload_size);
                    var copied: u32 = 0;
                    while (copied < payload_size) : (copied += 8) {
                        const temp_reg = try self.allocTempGeneral();
                        try self.emitLoad(.w64, temp_reg, box_ptr_reg, @intCast(copied));
                        try self.emitStore(.w64, frame_ptr, dest_offset + @as(i32, @intCast(copied)), temp_reg);
                        self.codegen.freeGeneral(temp_reg);
                    }
                    self.codegen.freeGeneral(box_ptr_reg);
                    const raw_payload_loc = self.fieldLocationFromLayout(dest_offset, payload_size, payload_layout_idx);
                    return self.requireExactValueLocationToLayout(raw_payload_loc, payload_layout_idx, tps.target_layout, "tag_payload_struct_access.boxed");
                } else {
                    return raw_value_loc;
                }
            } else if (union_layout.tag == .scalar or union_layout.tag == .zst) {
                return raw_value_loc;
            } else {
                unreachable;
            }
        }

        /// Generate code for explicit tag-discriminant access.
        fn generateDiscriminantAccess(self: *Self, disc: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const raw_value_loc = try self.emitValueLocal(disc.source);
            const source_layout_idx = self.localLayout(disc.source);
            const source_layout = ls.getLayout(source_layout_idx);

            if (source_layout.tag == .tag_union) {
                const stable_value_loc = try self.materializeValueToStackForLayout(raw_value_loc, source_layout_idx);
                const tu_data = ls.getTagUnionData(source_layout.getTagUnion().idx);
                const disc_reg = try self.loadAndMaskDiscriminant(
                    stable_value_loc,
                    disc.target_layout != .u64,
                    @intCast(tu_data.discriminant_offset.get(ls.targetUsize())),
                    tu_data.discriminant_size,
                );
                return .{ .general_reg = disc_reg };
            }

            if (source_layout.tag == .box) {
                const inner_layout = ls.getLayout(source_layout.getIdx());
                if (inner_layout.tag == .tag_union) {
                    const tu_data = ls.getTagUnionData(inner_layout.getTagUnion().idx);
                    const box_ptr_reg = try self.ensureInGeneralReg(raw_value_loc);
                    const disc_reg = try self.allocTempGeneral();
                    if (disc.target_layout != .u64) {
                        try self.emitLoad(.w32, disc_reg, box_ptr_reg, @intCast(tu_data.discriminant_offset.get(self.layout_store.targetUsize())));
                    } else {
                        try self.emitLoad(.w64, disc_reg, box_ptr_reg, @intCast(tu_data.discriminant_offset.get(self.layout_store.targetUsize())));
                    }
                    self.codegen.freeGeneral(box_ptr_reg);

                    if (tu_data.discriminant_size != 0 and tu_data.discriminant_size < 4) {
                        const mask: i32 = (@as(i32, 1) << @as(u5, @intCast(tu_data.discriminant_size * 8))) - 1;
                        if (comptime target.toCpuArch() == .aarch64) {
                            const mask_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(mask_reg, mask);
                            try self.codegen.emit.andRegRegReg(.w32, disc_reg, disc_reg, mask_reg);
                            self.codegen.freeGeneral(mask_reg);
                        } else {
                            try self.codegen.emit.andRegImm32(disc_reg, mask);
                        }
                    }

                    return .{ .general_reg = disc_reg };
                }
            }

            return raw_value_loc;
        }

        fn generateList(self: *Self, list: anytype) Allocator.Error!ValueLocation {
            const elems = self.store.getLocalSpan(list.elems);
            if (elems.len == 0) {
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

            const ls = self.layout_store;
            const list_abi = builtinInternalListAbi(ls, "dev.generateList.builtin_list_abi", list.target_layout);
            const elem_layout_idx: layout.Idx = list_abi.elem_layout_idx orelse .zst;
            const elem_size: u32 = list_abi.elem_size_align.size;
            const num_elems: u32 = @intCast(elems.len);
            const total_data_bytes: usize = @as(usize, elem_size) * @as(usize, num_elems);
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const heap_ptr_slot: i32 = self.codegen.allocStackSlot(8);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addImmArg(@intCast(total_data_bytes));
            try builder.addImmArg(@intCast(list_abi.alignment_bytes));
            try builder.addImmArg(if (list_abi.elements_refcounted) 1 else 0);
            try builder.addRegArg(roc_ops_reg);
            try self.callBuiltin(&builder, @intFromPtr(&allocateWithRefcountC), .allocate_with_refcount);

            try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

            for (elems, 0..) |elem_id, i| {
                const elem_loc = self.requireExactValueLocationToLayout(
                    try self.emitValueLocal(elem_id),
                    self.valueLayout(elem_id),
                    elem_layout_idx,
                    "assign_list.elem",
                );
                const elem_heap_offset: i32 = @intCast(@as(usize, i) * @as(usize, elem_size));

                if (elem_size == 0) continue;

                const elem_stack_offset = try self.ensureOnStack(elem_loc, elem_size);
                const heap_ptr = try self.allocTempGeneral();
                try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);

                const temp_reg = try self.allocTempGeneral();
                try self.copyChunked(temp_reg, frame_ptr, elem_stack_offset, heap_ptr, elem_heap_offset, elem_size);
                self.codegen.freeGeneral(temp_reg);
                self.codegen.freeGeneral(heap_ptr);
            }

            const list_struct_offset: i32 = self.codegen.allocStackSlot(roc_list_size);
            const ptr_reg = try self.allocTempGeneral();
            const len_reg = try self.allocTempGeneral();
            const cap_reg = try self.allocTempGeneral();

            try self.emitLoad(.w64, ptr_reg, frame_ptr, heap_ptr_slot);
            try self.codegen.emitLoadImm(len_reg, @intCast(num_elems));
            try self.codegen.emitLoadImm(cap_reg, @intCast(@as(u64, num_elems) << 1));

            try self.emitStore(.w64, frame_ptr, list_struct_offset, ptr_reg);
            try self.emitStore(.w64, frame_ptr, list_struct_offset + 8, len_reg);
            try self.emitStore(.w64, frame_ptr, list_struct_offset + 16, cap_reg);

            self.codegen.freeGeneral(ptr_reg);
            self.codegen.freeGeneral(len_reg);
            self.codegen.freeGeneral(cap_reg);

            return .{
                .list_stack = .{
                    .struct_offset = list_struct_offset,
                    .data_offset = heap_ptr_slot,
                    .num_elements = num_elems,
                },
            };
        }

        fn generateStruct(self: *Self, s: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const target_layout = ls.getLayout(s.target_layout);
            switch (target_layout.tag) {
                .zst => return .{ .immediate_i64 = 0 },
                .box_of_zst => return .{ .immediate_i64 = 0 },
                .box => {
                    const box_abi = ls.builtinBoxAbi(s.target_layout);
                    const inner_layout = box_abi.elem_layout;
                    if (inner_layout.tag != .struct_) {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "LIR/codegen invariant violated: assign_struct target box layout {} did not box a struct",
                                .{@intFromEnum(s.target_layout)},
                            );
                        }
                        unreachable;
                    }

                    const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                    const heap_ptr_slot = self.codegen.allocStackSlot(8);
                    {
                        var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                        try builder.addImmArg(@intCast(box_abi.elem_size));
                        try builder.addImmArg(@intCast(box_abi.elem_alignment));
                        try builder.addImmArg(if (box_abi.contains_refcounted) 1 else 0);
                        try builder.addRegArg(roc_ops_reg);
                        try self.callBuiltin(&builder, @intFromPtr(&allocateWithRefcountC), .allocate_with_refcount);
                    }
                    try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

                    const field_exprs = self.store.getLocalSpan(s.fields);
                    for (field_exprs, 0..) |field_expr_id, i| {
                        const field_offset = ls.getStructFieldOffsetByOriginalIndex(inner_layout.getStruct().idx, @intCast(i));
                        const field_size = ls.getStructFieldSizeByOriginalIndex(inner_layout.getStruct().idx, @intCast(i));
                        const field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(inner_layout.getStruct().idx, @intCast(i));
                        const field_loc = self.requireExactValueLocationToLayout(
                            try self.emitValueLocal(field_expr_id),
                            self.valueLayout(field_expr_id),
                            field_layout_idx,
                            "assign_struct.boxed_field",
                        );
                        if (field_size == 0) continue;

                        const field_stack_offset = try self.ensureOnStack(field_loc, field_size);
                        const heap_ptr = try self.allocTempGeneral();
                        try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);

                        const temp_reg = try self.allocTempGeneral();
                        try self.copyChunked(temp_reg, frame_ptr, field_stack_offset, heap_ptr, @intCast(field_offset), field_size);
                        self.codegen.freeGeneral(temp_reg);
                        self.codegen.freeGeneral(heap_ptr);
                    }

                    const result_reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, result_reg, frame_ptr, heap_ptr_slot);
                    return .{ .general_reg = result_reg };
                },
                .struct_ => {
                    const struct_layout = target_layout;
                    const struct_data = ls.getStructData(struct_layout.getStruct().idx);
                    const stack_size = struct_data.size.get(ls.targetUsize());
                    if (stack_size == 0) {
                        return .{ .immediate_i64 = 0 };
                    }

                    const base_offset = self.codegen.allocStackSlot(stack_size);
                    const field_exprs = self.store.getLocalSpan(s.fields);

                    for (field_exprs, 0..) |field_expr_id, i| {
                        const field_offset = ls.getStructFieldOffsetByOriginalIndex(struct_layout.getStruct().idx, @intCast(i));
                        const field_size = ls.getStructFieldSizeByOriginalIndex(struct_layout.getStruct().idx, @intCast(i));
                        if (field_size == 0) continue;
                        const field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(struct_layout.getStruct().idx, @intCast(i));
                        const field_loc = self.requireExactValueLocationToLayout(
                            try self.emitValueLocal(field_expr_id),
                            self.valueLayout(field_expr_id),
                            field_layout_idx,
                            "assign_struct.field",
                        );
                        const field_base = base_offset + @as(i32, @intCast(field_offset));
                        try self.copyBytesToStackOffset(field_base, field_loc, field_size);
                    }

                    return self.stackLocationForLayout(s.target_layout, base_offset);
                },
                else => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: assign_struct target layout {s} is not runtime struct or zst layout",
                            .{@tagName(target_layout.tag)},
                        );
                    }
                    unreachable;
                },
            }
        }

        fn generateTag(self: *Self, tag: anytype) Allocator.Error!ValueLocation {
            const ls = self.layout_store;
            const union_layout = ls.getLayout(tag.target_layout);
            if (union_layout.tag == .zst) {
                if (tag.discriminant != 0) {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: zero-sized tag layout cannot encode discriminant {d}",
                            .{tag.discriminant},
                        );
                    }
                    unreachable;
                }
                if (tag.payload) |payload| _ = try self.emitValueLocal(payload);
                return .{ .immediate_i64 = 0 };
            }
            if (union_layout.tag == .scalar) {
                if (tag.payload) |payload| _ = try self.emitValueLocal(payload);
                return .{ .immediate_i64 = tag.discriminant };
            }
            if (union_layout.tag == .box_of_zst) {
                if (tag.payload) |payload| _ = try self.emitValueLocal(payload);
                return .{ .immediate_i64 = 0 };
            }
            if (union_layout.tag == .box) {
                const box_abi = ls.builtinBoxAbi(tag.target_layout);
                const inner_layout = box_abi.elem_layout;
                if (inner_layout.tag != .tag_union) {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: generateTag expected boxed tag union layout, got boxed {s}",
                            .{@tagName(inner_layout.tag)},
                        );
                    }
                    unreachable;
                }

                const roc_ops_reg = self.roc_ops_reg orelse unreachable;
                const heap_ptr_slot = self.codegen.allocStackSlot(8);
                {
                    var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                    try builder.addImmArg(@intCast(box_abi.elem_size));
                    try builder.addImmArg(@intCast(box_abi.elem_alignment));
                    try builder.addImmArg(if (box_abi.contains_refcounted) 1 else 0);
                    try builder.addRegArg(roc_ops_reg);
                    try self.callBuiltin(&builder, @intFromPtr(&allocateWithRefcountC), .allocate_with_refcount);
                }
                try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

                const inner_tu_data = ls.getTagUnionData(inner_layout.getTagUnion().idx);
                const variants = ls.getTagUnionVariants(inner_tu_data);
                if (@as(usize, tag.variant_index) >= variants.len) {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: tag assignment variant index {d} exceeded variant count {d}",
                            .{ tag.variant_index, variants.len },
                        );
                    }
                    unreachable;
                }
                const variant_payload_layout = variants.get(tag.variant_index).payload_layout;

                if (tag.payload) |payload_local| {
                    const arg_loc = self.requireExactValueLocationToLayout(
                        try self.emitValueLocal(payload_local),
                        self.valueLayout(payload_local),
                        variant_payload_layout,
                        "assign_tag.boxed_payload",
                    );
                    const payload_layout = ls.getLayout(variant_payload_layout);
                    const payload_size: u32 = ls.layoutSizeAlign(payload_layout).size;
                    if (payload_size > 0) {
                        const arg_stack_offset = try self.ensureOnStack(arg_loc, payload_size);
                        const heap_ptr = try self.allocTempGeneral();
                        try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);
                        const temp_reg = try self.allocTempGeneral();
                        try self.copyChunked(temp_reg, frame_ptr, arg_stack_offset, heap_ptr, 0, payload_size);
                        self.codegen.freeGeneral(temp_reg);
                        self.codegen.freeGeneral(heap_ptr);
                    }
                }

                {
                    const heap_ptr = try self.allocTempGeneral();
                    try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);
                    try self.storeDiscriminantToPtr(heap_ptr, inner_tu_data.discriminant_offset.get(self.layout_store.targetUsize()), tag.discriminant, inner_tu_data.discriminant_size);
                    self.codegen.freeGeneral(heap_ptr);
                }

                const result_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, result_reg, frame_ptr, heap_ptr_slot);
                return .{ .general_reg = result_reg };
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

            const tu_data = ls.getTagUnionData(union_layout.getTagUnion().idx);
            const stack_size = tu_data.size.get(self.layout_store.targetUsize());
            const variants = ls.getTagUnionVariants(tu_data);
            if (@as(usize, tag.variant_index) >= variants.len) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "LIR/codegen invariant violated: tag assignment variant index {d} exceeded variant count {d}",
                        .{ tag.variant_index, variants.len },
                    );
                }
                unreachable;
            }
            const variant_payload_layout = variants.get(tag.variant_index).payload_layout;

            if (tag.payload == null) {
                const base_offset = self.codegen.allocStackSlot(stack_size);
                try self.zeroStackArea(base_offset, stack_size);

                const disc_offset = tu_data.discriminant_offset.get(self.layout_store.targetUsize());
                const disc_size = tu_data.discriminant_size;
                try self.storeDiscriminant(base_offset + @as(i32, @intCast(disc_offset)), tag.discriminant, disc_size);

                return self.stackLocationForLayout(tag.target_layout, base_offset);
            } else if (tag.payload) |payload_local| {
                const base_offset = self.codegen.allocStackSlot(stack_size);
                try self.zeroStackArea(base_offset, stack_size);

                const arg_loc = self.requireExactValueLocationToLayout(
                    try self.emitValueLocal(payload_local),
                    self.valueLayout(payload_local),
                    variant_payload_layout,
                    "assign_tag.payload",
                );
                const payload_layout = ls.getLayout(variant_payload_layout);
                const payload_size: u32 = ls.layoutSizeAlign(payload_layout).size;
                try self.copyBytesToStackOffset(base_offset, arg_loc, payload_size);

                const disc_offset = tu_data.discriminant_offset.get(self.layout_store.targetUsize());
                const disc_size = tu_data.discriminant_size;
                try self.storeDiscriminant(base_offset + @as(i32, @intCast(disc_offset)), tag.discriminant, disc_size);

                return self.stackLocationForLayout(tag.target_layout, base_offset);
            }

            unreachable;
        }

        fn compiledProcForId(
            self: *Self,
            proc_id: lir.LIR.LirProcSpecId,
        ) Allocator.Error!CompiledProc {
            const proc = self.store.getProcSpec(proc_id);
            if (self.proc_registry.get(@intFromEnum(proc_id))) |compiled| return compiled;

            if (std.debug.runtime_safety) std.debug.panic(
                "proc call target {d} ({d}) is missing from the compiled proc registry",
                .{ @intFromEnum(proc_id), proc.name.raw() },
            );
            unreachable;
        }

        /// Generate code for a direct proc call.
        fn generateCall(self: *Self, call: anytype) Allocator.Error!ValueLocation {
            const proc_spec = self.store.getProcSpec(call.proc);
            const arg_refs = self.store.getLocalSpan(call.args);
            const param_refs = self.store.getLocalSpan(proc_spec.args);
            var args_sfa = std.heap.stackFallback(8 * (@sizeOf(ValueLocation) + @sizeOf(layout.Idx)), self.allocator);
            const args_alloc = args_sfa.get();
            var arg_locs = try args_alloc.alloc(ValueLocation, arg_refs.len);
            defer args_alloc.free(arg_locs);
            var arg_layouts = try args_alloc.alloc(layout.Idx, arg_refs.len);
            defer args_alloc.free(arg_layouts);

            if (builtin.mode == .Debug and param_refs.len != arg_refs.len) {
                std.debug.panic(
                    "Dev/codegen invariant violated: call to proc {d} passed {d} args but callee expects {d}",
                    .{ @intFromEnum(call.proc), arg_refs.len, param_refs.len },
                );
            }

            for (arg_refs, param_refs, 0..) |arg_ref, param_ref, i| {
                const actual_layout = self.localLayout(arg_ref);
                const expected_layout = self.localLayout(param_ref);
                const raw_arg_loc = try self.emitValueLocal(arg_ref);
                arg_locs[i] = self.requireExactValueLocationToLayout(raw_arg_loc, actual_layout, expected_layout, "direct_call.arg");
                arg_layouts[i] = expected_layout;
            }

            if (builtin.mode == .Debug and proc_spec.ret_layout != call.ret_layout) {
                std.debug.panic(
                    "Dev/codegen invariant violated: direct call target layout {} did not match callee ret layout {} for proc {d}",
                    .{ @intFromEnum(call.ret_layout), @intFromEnum(proc_spec.ret_layout), @intFromEnum(call.proc) },
                );
            }

            if (proc_spec.hosted) |hosted| {
                return try self.generateHostedCall(hosted, arg_locs, arg_layouts, call.ret_layout);
            }

            const proc = try self.compiledProcForId(call.proc);
            return try self.generateCallToCompiledProc(proc, arg_locs, arg_layouts, call.ret_layout);
        }

        fn generateErasedCall(
            self: *Self,
            closure_local: LocalId,
            call_args: LocalSpan,
            ret_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            const closure_layout = self.localLayout(closure_local);
            const runtime_closure_layout = self.runtimeRepresentationLayoutIdx(closure_layout);
            const closure_layout_val = self.layout_store.getLayout(runtime_closure_layout);
            if (builtin.mode == .Debug and closure_layout_val.tag != .erased_callable) {
                std.debug.panic(
                    "Dev/codegen invariant violated: erased call closure local {d} must have erased_callable layout, got {s}",
                    .{ @intFromEnum(closure_local), @tagName(closure_layout_val.tag) },
                );
            }

            const closure_loc = try self.emitValueLocal(closure_local);
            const closure_ptr_slot = self.codegen.allocStackSlot(8);
            {
                const closure_ptr_reg = try self.ensureInGeneralReg(closure_loc);
                try self.emitStore(.w64, frame_ptr, closure_ptr_slot, closure_ptr_reg);
                self.codegen.freeGeneral(closure_ptr_reg);
            }

            const arg_refs = self.store.getLocalSpan(call_args);
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            var args_sfa = std.heap.stackFallback(8 * (@sizeOf(ValueLocation) + @sizeOf(layout.Idx)), self.allocator);
            const args_alloc = args_sfa.get();
            var arg_layouts = try args_alloc.alloc(layout.Idx, arg_refs.len);
            defer args_alloc.free(arg_layouts);
            var arg_locs = try args_alloc.alloc(ValueLocation, arg_refs.len);
            defer args_alloc.free(arg_locs);

            for (arg_refs, 0..) |arg_ref, i| {
                const arg_layout = self.localLayout(arg_ref);
                const raw_arg_loc = try self.emitValueLocal(arg_ref);
                arg_locs[i] = self.requireExactValueLocationToLayout(raw_arg_loc, arg_layout, arg_layout, "erased_call.arg");
                arg_layouts[i] = arg_layout;
            }

            var total_args_size: u32 = 0;
            for (arg_layouts) |arg_layout| {
                const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
                const size_align = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_layout));
                total_args_size = std.mem.alignForward(u32, total_args_size, @intCast(@max(size_align.alignment.toByteUnits(), 1)));
                total_args_size += size_align.size;
            }
            const args_slot = if (arg_refs.len == 0)
                0
            else
                self.codegen.allocStackSlot(if (total_args_size == 0) 8 else total_args_size);

            var arg_offset: u32 = 0;
            for (arg_locs, arg_layouts) |arg_loc, arg_layout| {
                const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
                const size_align = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_layout));
                arg_offset = std.mem.alignForward(u32, arg_offset, @intCast(@max(size_align.alignment.toByteUnits(), 1)));
                if (size_align.size > 0) {
                    try self.copyBytesToStackOffset(args_slot + @as(i32, @intCast(arg_offset)), arg_loc, size_align.size);
                }
                arg_offset += size_align.size;
            }

            const capture_ptr_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, capture_ptr_reg, frame_ptr, closure_ptr_slot);
            try self.emitAddPtrImmAny(capture_ptr_reg, capture_ptr_reg, builtins.erased_callable.capture_offset);
            const capture_stack_offset = self.codegen.allocStackSlot(8);
            try self.emitStore(.w64, frame_ptr, capture_stack_offset, capture_ptr_reg);
            self.codegen.freeGeneral(capture_ptr_reg);
            const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
            const ret_size = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_ret_layout)).size;
            const ret_buffer_offset = if (ret_size == 0) 0 else self.codegen.allocStackSlot(ret_size);

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(roc_ops_reg);
            if (ret_size == 0) {
                try builder.addImmArg(0);
            } else {
                try builder.addLeaArg(frame_ptr, ret_buffer_offset);
            }
            if (arg_refs.len == 0) {
                try builder.addImmArg(0);
            } else {
                try builder.addLeaArg(frame_ptr, args_slot);
            }
            try builder.addMemArg(frame_ptr, capture_stack_offset);

            const comptime_call_entered = if (self.comptime_hooks) |hooks|
                try self.emitComptimeCallEnter(hooks)
            else
                false;
            const closure_ptr_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X11 else .R11;
            const fn_ptr_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X10 else .RAX;
            try self.emitLoad(.w64, closure_ptr_reg, frame_ptr, closure_ptr_slot);
            try self.emitLoad(.w64, fn_ptr_reg, closure_ptr_reg, 0);
            try builder.callReg(fn_ptr_reg);

            const result: ValueLocation = if (ret_size == 0)
                .{ .immediate_i64 = 0 }
            else
                self.stackLocationForLayout(runtime_ret_layout, ret_buffer_offset);
            if (comptime_call_entered) {
                try self.emitComptimeCallExit(self.comptime_hooks.?);
            }
            return result;
        }

        fn generatePackedErasedFn(
            self: *Self,
            proc_id: lir.LIR.LirProcSpecId,
            capture: ?LocalId,
            target_layout: layout.Idx,
            capture_layout: ?layout.Idx,
            on_drop: lir.LIR.ErasedCallableOnDrop,
        ) Allocator.Error!ValueLocation {
            const target_layout_val = self.layout_store.getLayout(target_layout);
            if (builtin.mode == .Debug and target_layout_val.tag != .erased_callable) {
                std.debug.panic(
                    "Dev/codegen invariant violated: packed erased fn target layout must be erased_callable, got {s}",
                    .{@tagName(target_layout_val.tag)},
                );
            }
            if ((capture != null) != (capture_layout != null)) {
                std.debug.panic("Dev/codegen invariant violated: packed erased fn capture/layout presence differed", .{});
            }

            const capture_size: u32 = if (capture_layout) |layout_idx| self.getLayoutSize(layout_idx) else 0;
            if (builtin.mode == .Debug) {
                if (capture_layout) |layout_idx| {
                    const capture_align = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(layout_idx)).alignment.toByteUnits();
                    if (capture_align > builtins.erased_callable.capture_alignment) {
                        std.debug.panic(
                            "Dev/codegen invariant violated: erased callable capture layout alignment {d} exceeds fixed capture alignment {d}",
                            .{ capture_align, builtins.erased_callable.capture_alignment },
                        );
                    }
                }
            }
            const capture_prefix_size: u32 = if (self.enable_hot_reload)
                builtins.erased_callable.hot_reload_capture_prefix_size
            else
                0;
            const payload_size = builtins.erased_callable.payloadSize(capture_prefix_size + capture_size);
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const heap_ptr_slot: i32 = self.codegen.allocStackSlot(8);

            {
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addImmArg(@intCast(payload_size));
                try builder.addImmArg(builtins.erased_callable.payload_alignment);
                try builder.addImmArg(if (builtins.erased_callable.allocation_has_refcounted_children) 1 else 0);
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, @intFromPtr(&allocateWithRefcountC), .allocate_with_refcount);
            }
            try self.emitStore(.w64, frame_ptr, heap_ptr_slot, ret_reg_0);

            const hot_reload_code_ref_slot: ?i32 = if (self.enable_hot_reload) blk: {
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_hot_reload_retain_current), .hot_reload_retain_current);

                const slot = self.codegen.allocStackSlot(8);
                try self.emitStore(.w64, frame_ptr, slot, ret_reg_0);
                break :blk slot;
            } else null;

            const heap_ptr = try self.allocTempGeneral();
            try self.emitLoad(.w64, heap_ptr, frame_ptr, heap_ptr_slot);

            const proc_addr = try self.allocTempGeneral();
            const proc = self.proc_registry.get(@intFromEnum(proc_id)) orelse unreachable;
            if (proc.code_start == unresolved_proc_code_start)
                try self.emitPendingProcAddress(proc_id, proc_addr)
            else
                try self.emitInternalCodeAddress(proc.code_start, proc_addr);
            try self.emitStore(.w64, heap_ptr, 0, proc_addr);
            self.codegen.freeGeneral(proc_addr);

            if (self.enable_hot_reload) {
                const hot_drop_reg = try self.allocTempGeneral();
                try self.emitBuiltinAddress(
                    hot_drop_reg,
                    @intFromPtr(&dev_wrappers.roc_builtins_hot_reload_erased_callable_drop),
                    .hot_reload_erased_callable_drop,
                );
                try self.emitStore(.w64, heap_ptr, @intCast(@sizeOf(usize)), hot_drop_reg);
                self.codegen.freeGeneral(hot_drop_reg);

                if (hot_reload_code_ref_slot) |slot| {
                    const code_ref_reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, code_ref_reg, frame_ptr, slot);
                    try self.emitStore(
                        .w64,
                        heap_ptr,
                        @intCast(builtins.erased_callable.capture_offset + @offsetOf(builtins.erased_callable.HotReloadCaptureHeader, "code_ref")),
                        code_ref_reg,
                    );
                    self.codegen.freeGeneral(code_ref_reg);
                }

                const original_on_drop_reg = try self.materializeErasedCallableOnDrop(on_drop);
                try self.emitStore(
                    .w64,
                    heap_ptr,
                    @intCast(builtins.erased_callable.capture_offset + @offsetOf(builtins.erased_callable.HotReloadCaptureHeader, "original_on_drop")),
                    original_on_drop_reg,
                );
                self.codegen.freeGeneral(original_on_drop_reg);
            } else {
                const on_drop_reg = try self.materializeErasedCallableOnDrop(on_drop);
                try self.emitStore(.w64, heap_ptr, @intCast(@sizeOf(usize)), on_drop_reg);
                self.codegen.freeGeneral(on_drop_reg);
            }

            if (capture) |capture_local| {
                const layout_idx = capture_layout orelse unreachable;
                const capture_loc = self.requireExactValueLocationToLayout(
                    try self.emitValueLocal(capture_local),
                    self.localLayout(capture_local),
                    layout_idx,
                    "packed_erased_fn.capture",
                );
                if (capture_size > 0) {
                    const capture_stack = try self.ensureOnStack(capture_loc, capture_size);
                    const temp = try self.allocTempGeneral();
                    try self.copyChunked(
                        temp,
                        frame_ptr,
                        capture_stack,
                        heap_ptr,
                        @intCast(builtins.erased_callable.capture_offset + capture_prefix_size),
                        capture_size,
                    );
                    self.codegen.freeGeneral(temp);
                }
            }

            return .{ .general_reg = heap_ptr };
        }

        fn materializeErasedCallableOnDrop(
            self: *Self,
            on_drop: lir.LIR.ErasedCallableOnDrop,
        ) Allocator.Error!GeneralReg {
            const on_drop_reg = try self.allocTempGeneral();
            switch (on_drop) {
                .none => try self.codegen.emitLoadImm(on_drop_reg, 0),
                .rc_helper => |helper_key| {
                    if (self.layout_store.rcHelperPlan(helper_key) == .noop) {
                        try self.codegen.emitLoadImm(on_drop_reg, 0);
                    } else {
                        // `on_drop` is selected here at closure creation, which
                        // is not an RC statement and makes no thread-confinement
                        // claim, so it is always the atomic variant (atomic is
                        // always sound).
                        const helper = RcHelperVariant{ .key = helper_key, .atomicity = .atomic };
                        try self.emitPendingRcAddr(helper, on_drop_reg);
                        try self.scheduleRcHelper(helper);
                        try self.maybeDrainRcHelpers();
                    }
                },
                .interpreter_context_drop => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "Dev/codegen invariant violated: interpreter_context_drop reached native backend",
                            .{},
                        );
                    }
                    unreachable;
                },
            }
            return on_drop_reg;
        }

        fn generateHostedCall(
            self: *Self,
            hosted: lir.LIR.HostedProc,
            args: []const ValueLocation,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
        ) Allocator.Error!ValueLocation {
            std.debug.assert(args.len == arg_layouts.len);

            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const ret_size = self.getLayoutSize(ret_layout);
            const ret_slot = self.codegen.allocStackSlot(if (ret_size == 0) 8 else ret_size);

            const arg_offsets = try self.allocator.alloc(u32, arg_layouts.len);
            defer self.allocator.free(arg_offsets);
            var total_args_size: u32 = 0;
            for (arg_layouts, arg_offsets) |arg_layout, *arg_offset| {
                const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
                const size_align = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_layout));
                total_args_size = std.mem.alignForward(u32, total_args_size, @intCast(@max(size_align.alignment.toByteUnits(), 1)));
                arg_offset.* = total_args_size;
                total_args_size += size_align.size;
            }

            // Pad by 8 bytes so register pieces loaded as full 64-bit words from the tail
            // argument never read past the slot.
            const args_slot = self.codegen.allocStackSlot(total_args_size + 8);

            const hosted_target_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X10 else .RAX;
            const hosted_table_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X11 else .R11;

            for (args, arg_layouts, arg_offsets) |arg_loc_raw, arg_layout, arg_offset| {
                const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
                const size_align = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_layout));
                if (size_align.size > 0) {
                    try self.copyBytesToStackOffset(args_slot + @as(i32, @intCast(arg_offset)), arg_loc_raw, size_align.size);
                }
            }

            const comptime_call_entered = if (self.comptime_hooks) |hooks|
                try self.emitComptimeCallEnter(hooks)
            else
                false;

            // Object-file output calls the host's linker symbol directly;
            // RocOps-threaded modes dispatch through the hosted table.
            if (self.generation_mode.threadsRocOps()) {
                const hosted_fns_offset: i32 = @intCast(@offsetOf(RocOps, "hosted_fns"));
                const hosted_fns_count_offset: i32 = hosted_fns_offset + @as(i32, @intCast(@offsetOf(HostedFunctions, "count")));
                const hosted_fns_ptr_offset: i32 = hosted_fns_offset + @as(i32, @intCast(@offsetOf(HostedFunctions, "fns")));
                const hosted_entry_offset: i32 = @intCast(@as(usize, hosted.dispatch_index) * @sizeOf(builtins.host_abi.HostedFn));

                if (builtin.mode == .Debug) {
                    const hosted_count_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(hosted_count_reg);
                    try self.emitLoad(.w32, hosted_count_reg, roc_ops_reg, hosted_fns_count_offset);
                    try self.emitCmpImm(hosted_count_reg, @intCast(hosted.dispatch_index));
                    const count_ok_patch = try self.codegen.emitCondJump(condAbove());
                    const msg = try std.fmt.allocPrint(
                        self.allocator,
                        "Dev/codegen invariant violated: hosted call index {d} out of bounds for proc {d}",
                        .{ hosted.dispatch_index, if (self.current_proc_name) |sym| sym.raw() else std.math.maxInt(u64) },
                    );
                    defer self.allocator.free(msg);
                    try self.emitRocCrashShared(msg);
                    try self.emitTrap();
                    self.codegen.patchJump(count_ok_patch, self.codegen.currentOffset());
                }

                try self.emitLoad(.w64, hosted_table_reg, roc_ops_reg, hosted_fns_ptr_offset);
                try self.emitLoad(.w64, hosted_target_reg, hosted_table_reg, hosted_entry_offset);
            }

            // Lower the call to the platform C ABI (shared classifier, same as the LLVM
            // backend and interpreter trampoline).
            const abi_target: layout.abi.Target = if (comptime target.toCpuArch() == .aarch64)
                .aarch64
            else if (comptime roc_target.isWindows())
                .x86_64_windows
            else
                .x86_64_sysv;
            var arena_state = std.heap.ArenaAllocator.init(self.allocator);
            defer arena_state.deinit();
            const lowered = layout.abi.lower(arena_state.allocator(), self.layout_store, abi_target, arg_layouts, ret_layout, false) catch return error.OutOfMemory;

            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);

            // sret: on x86 the indirect-result pointer is the first integer argument; on
            // aarch64 it is the dedicated x8 register, set just before the call.
            const aarch64_sret = lowered.ret == .indirect and comptime target.toCpuArch() == .aarch64;
            if (lowered.ret == .indirect and comptime target.toCpuArch() != .aarch64) {
                try builder.setReturnByPointer(ret_slot);
            }

            if (lowered.leading_ops) try builder.addRegArg(roc_ops_reg);

            for (lowered.args, arg_offsets, 0..) |placement, arg_offset, arg_i| {
                const slot_off = args_slot + @as(i32, @intCast(arg_offset));
                switch (placement) {
                    .none => {},
                    .indirect => {
                        const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layouts[arg_i]);
                        const size_align = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_layout));
                        if (abi_target == .x86_64_sysv) {
                            // SysV memory-class aggregates are copied into the outgoing
                            // stack argument area. AAPCS64 and Win64 pass a pointer.
                            try builder.addStackMemArg(frame_ptr, slot_off, size_align.size);
                        } else {
                            try builder.addLeaArg(frame_ptr, slot_off);
                        }
                    },
                    .registers => |pieces| {
                        for (pieces) |piece| {
                            const piece_off = slot_off + @as(i32, @intCast(piece.offset));
                            switch (piece.class) {
                                .integer => try builder.addMemArg(frame_ptr, piece_off),
                                .float => try builder.addFloatMemArg(frame_ptr, piece_off, piece.size == 8),
                            }
                        }
                    },
                }
            }

            if (aarch64_sret) {
                // AAPCS64 passes the indirect-result pointer in x8 (named XR here).
                try self.emitLeaStack(.XR, ret_slot);
            }

            if (self.generation_mode.threadsRocOps()) {
                try builder.callReg(hosted_target_reg);
            } else {
                try builder.callRelocatable(self.store.getString(hosted.symbol), self.allocator, &self.codegen.relocations);
            }

            // Register-class return: store each result register into the return slot.
            const hosted_ret_reg_0: GeneralReg = if (arch == .x86_64) .RAX else .X0;
            const hosted_ret_reg_1: GeneralReg = if (arch == .x86_64) .RDX else .X1;
            switch (lowered.ret) {
                .none, .indirect => {},
                .registers => |pieces| {
                    var gp_i: usize = 0;
                    var sse_i: usize = 0;
                    for (pieces) |piece| {
                        const dst_off = ret_slot + @as(i32, @intCast(piece.offset));
                        switch (piece.class) {
                            .integer => {
                                const reg = if (gp_i == 0) hosted_ret_reg_0 else hosted_ret_reg_1;
                                if (piece.size <= 4) {
                                    try self.emitStore(.w32, frame_ptr, dst_off, reg);
                                } else {
                                    try self.emitStore(.w64, frame_ptr, dst_off, reg);
                                }
                                gp_i += 1;
                            },
                            .float => {
                                try self.emitHostedFloatResultStore(dst_off, sse_i, piece.size == 8);
                                sse_i += 1;
                            },
                        }
                    }
                },
            }

            if (ret_size == 0) {
                if (comptime_call_entered) {
                    try self.emitComptimeCallExit(self.comptime_hooks.?);
                }
                return .{ .immediate_i64 = 0 };
            }

            const result = self.stackLocationForLayout(ret_layout, ret_slot);
            if (comptime_call_entered) {
                try self.emitComptimeCallExit(self.comptime_hooks.?);
            }
            return result;
        }

        /// Store hosted-call float result register `index` (0 or 1) into the return slot.
        fn emitHostedFloatResultStore(self: *Self, dst_off: i32, index: usize, is_f64: bool) Allocator.Error!void {
            const freg0: FloatReg = if (arch == .x86_64) .XMM0 else .V0;
            const freg1: FloatReg = if (arch == .x86_64) .XMM1 else .V1;
            const freg = if (index == 0) freg0 else freg1;
            if (comptime target.toCpuArch() == .aarch64) {
                if (is_f64) {
                    try self.codegen.emitStoreStackF64(dst_off, freg);
                } else {
                    try self.codegen.emitStoreStackF32(dst_off, freg);
                }
            } else if (is_f64) {
                try self.codegen.emit.movsdMemReg(frame_ptr, dst_off, freg);
            } else {
                try self.codegen.emit.movssMemReg(frame_ptr, dst_off, freg);
            }
        }

        /// Compare a general register against an immediate bit pattern.
        fn emitCmpImm(self: *Self, reg: GeneralReg, value: i64) Allocator.Error!void {
            if (comptime target.toCpuArch() == .aarch64) {
                if (value >= 0 and value <= std.math.maxInt(u12)) {
                    try self.codegen.emit.cmpRegImm12(.w64, reg, @intCast(value));
                } else {
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, value);
                    try self.codegen.emit.cmpRegReg(.w64, reg, temp);
                    self.codegen.freeGeneral(temp);
                }
            } else {
                if (value >= std.math.minInt(i32) and value <= std.math.maxInt(i32)) {
                    try self.codegen.emit.cmpRegImm32(.w64, reg, @intCast(value));
                } else {
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, value);
                    try self.codegen.emit.cmpRegReg(.w64, reg, temp);
                    self.codegen.freeGeneral(temp);
                }
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
        fn emitJumpIfNotEqual(self: *Self) Allocator.Error!usize {
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
        fn emitJumpIfEqual(self: *Self) Allocator.Error!usize {
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

        fn generateCallToCompiledProc(self: *Self, proc: CompiledProc, args: []const ValueLocation, arg_layouts: []const layout.Idx, ret_layout: layout.Idx) Allocator.Error!ValueLocation {
            std.debug.assert(args.len == arg_layouts.len);
            const needs_ret_ptr = self.needsInternalReturnByPointer(ret_layout);
            const ret_buffer_offset = if (needs_ret_ptr) blk: {
                const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
                const size = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_ret_layout)).size;
                break :blk self.codegen.allocStackSlot(size);
            } else 0;

            // Pass 1: Generate all argument expressions and calculate register needs
            const arg_infos_start = self.scratch_arg_infos.top();
            defer self.scratch_arg_infos.clearFrom(arg_infos_start);

            for (args, arg_layouts) |arg_loc_raw, arg_layout| {
                const arg_loc = self.coerceImmediateToLayout(arg_loc_raw, arg_layout);
                const num_regs: u8 = self.calcArgRegCount(arg_loc, arg_layout);
                try self.scratch_arg_infos.append(.{ .loc = arg_loc, .layout_idx = arg_layout, .num_regs = num_regs });
            }
            const arg_infos = self.scratch_arg_infos.sliceFromStart(arg_infos_start);
            // Pass 2: Place arguments and emit call
            const initial_arg_reg_idx: u8 = if (needs_ret_ptr) 1 else 0;
            // RocOps-threaded modes pass RocOps to procs; symbol-ABI output
            // carries none.
            const emit_roc_ops = self.generation_mode.threadsRocOps();
            const pbp_plan = try self.computePassByPtrPlan(arg_infos, initial_arg_reg_idx, emit_roc_ops);
            defer self.scratch_pass_by_ptr.clearFrom(pbp_plan.start);
            const placed_call = try self.placeCallArguments(arg_infos, .{
                .needs_ret_ptr = needs_ret_ptr,
                .ret_buffer_offset = ret_buffer_offset,
                .pass_by_ptr = pbp_plan.slice,
                .emit_roc_ops = emit_roc_ops,
                .comptime_call_hooks = self.comptime_hooks,
            });
            if (proc.code_start == unresolved_proc_code_start) {
                try self.emitPendingCallToProc(proc.id);
            } else {
                try self.emitCallToOffset(proc.code_start);
            }

            if (placed_call.stack_spill_size > 0) {
                try self.emitAddStackPtr(placed_call.stack_spill_size);
            }

            const result = try self.saveCallReturnValue(ret_layout, needs_ret_ptr, ret_buffer_offset);
            if (placed_call.comptime_call_entered) {
                try self.emitComptimeCallExit(self.comptime_hooks.?);
            }
            return result;
        }

        fn emitPendingCallToProc(self: *Self, target_proc: lir.LIR.LirProcSpecId) Allocator.Error!void {
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
                .stack => |stack_loc| blk: {
                    break :blk self.stackLocationForLayout(layout_idx, stack_loc.offset);
                },
                .stack_i128 => |offset| blk: {
                    break :blk self.stackLocationForLayout(layout_idx, offset);
                },
                .stack_str => |offset| blk: {
                    break :blk self.stackLocationForLayout(layout_idx, offset);
                },
                .list_stack => |info| blk: {
                    break :blk self.stackLocationForLayout(layout_idx, info.struct_offset);
                },
                .noreturn => normalized_value_loc,
                else => blk: {
                    const size = self.getLayoutSize(layout_idx);
                    if (size == 0) {
                        break :blk ValueLocation{ .immediate_i64 = 0 };
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

        fn aggregateArgRegisterPressure(size: u32) u8 {
            if (size <= 8) return 1;
            const regs = (size + 7) / 8;
            if (regs > max_arg_regs) return pass_by_ptr_arg_regs;
            return @intCast(regs);
        }

        /// Calculate the number of registers an argument needs based on its location and layout.
        fn calcArgRegCount(self: *Self, arg_loc: ValueLocation, arg_layout: ?layout.Idx) u8 {
            const is_i128_arg = self.argNeedsI128Abi(arg_loc, arg_layout);
            if (is_i128_arg) return 2;

            // Check for list/string types - need 3 registers (24 bytes: ptr, len, capacity)
            if (arg_loc == .list_stack or arg_loc == .stack_str) return 3;
            if (arg_layout) |al| {
                const runtime_layout_idx = self.runtimeRepresentationLayoutIdx(al);
                if (runtime_layout_idx == .str) return 3; // Strings are 24 bytes
                {
                    const ls = self.layout_store;
                    const layout_val = ls.getLayout(runtime_layout_idx);
                    if (layout_val.tag == .zst or ls.layoutSizeAlign(layout_val).size == 0) return 0;
                    if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 3;
                    // Check for aggregate values > 8 bytes
                    if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
                        const size = ls.layoutSizeAlign(layout_val).size;
                        return aggregateArgRegisterPressure(size);
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
        fn runtimeRepresentationLayoutIdx(self: *Self, layout_idx: layout.Idx) layout.Idx {
            const ls = self.layout_store;
            var current = layout_idx;
            while (true) {
                if (@intFromEnum(current) >= ls.layouts.len()) return current;

                const layout_val = ls.getLayout(current);
                switch (layout_val.tag) {
                    .closure => current = layout_val.getClosure().captures_layout_idx,
                    else => return current,
                }
            }
        }

        fn getLayoutSize(self: *Self, layout_idx: layout.Idx) u32 {
            const ls = self.layout_store;
            const layout_val = ls.getLayout(self.runtimeRepresentationLayoutIdx(layout_idx));
            return ls.layoutSizeAlign(layout_val).size;
        }

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
                .shim_execution, .object_file => {
                    try builder.callRelocatable(builtin_fn.symbolName(), self.allocator, &self.codegen.relocations);
                },
            }
        }

        fn emitBuiltinAddress(self: *Self, dst_reg: GeneralReg, fn_addr: usize, builtin_fn: BuiltinFn) Allocator.Error!void {
            switch (self.generation_mode) {
                .native_execution => {
                    try self.codegen.emitLoadImm(dst_reg, @bitCast(@as(u64, fn_addr)));
                },
                .shim_execution, .object_file => {
                    try self.codegen.emitLoadDataAddress(dst_reg, builtin_fn.symbolName());
                },
            }
        }

        fn emitHotReloadEnterForHostCallable(self: *Self) Allocator.Error!?i32 {
            if (!self.enable_hot_reload) return null;

            const roc_ops_reg = self.roc_ops_reg orelse unreachable;
            const code_ref_slot = self.codegen.allocStackSlot(8);
            {
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addRegArg(roc_ops_reg);
                try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_hot_reload_enter), .hot_reload_enter);
            }
            try self.emitStore(.w64, frame_ptr, code_ref_slot, ret_reg_0);
            return code_ref_slot;
        }

        fn emitHotReloadLeaveForHostCallable(self: *Self, code_ref_slot: i32) Allocator.Error!void {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addMemArg(frame_ptr, code_ref_slot);
            try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_hot_reload_leave), .hot_reload_leave);
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
                    if (builtin.mode == .Debug and size > 8) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: ensureOnStack cannot materialize immediate_i64 into {d}-byte value",
                            .{size},
                        );
                    }
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
                    try self.emitValueLoadStack(reg, s.offset, s.size, s.layout_idx);
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

        /// Normalize immediate literal representation to match the target layout.
        /// This prevents wide/default literal carriers (e.g. immediate_i128) from
        /// leaking into narrower typed bindings/calls.
        fn coerceImmediateToLayout(_: *Self, loc: ValueLocation, target_layout: layout.Idx) ValueLocation {
            return switch (target_layout) {
                .str => switch (loc) {
                    .immediate_i64, .immediate_i128 => if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: scalar immediate cannot stand in for RocStr layout",
                            .{},
                        );
                    } else unreachable,
                    else => loc,
                },
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

        fn unwrapSingleFieldPayloadLayout(self: *Self, layout_idx: layout.Idx) ?layout.Idx {
            const layout_val = self.layout_store.getLayout(layout_idx);
            if (layout_val.tag != .struct_) return null;

            const struct_data = self.layout_store.getStructData(layout_val.getStruct().idx);
            const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
            if (fields.len != 1) return null;

            const field = fields.get(0);
            if (field.index != 0) return null;
            return field.layout;
        }

        fn findBadUtf8Variant(self: *Self, inner_tu: *const layout.TagUnionData) ?struct { disc: u16, struct_idx: layout.StructIdx } {
            const variants = self.layout_store.getTagUnionVariants(inner_tu);
            for (0..variants.len) |i| {
                const payload = variants.get(@intCast(i)).payload_layout;
                const candidate = self.unwrapSingleFieldPayloadLayout(payload) orelse payload;
                const payload_layout = self.layout_store.getLayout(candidate);
                if (payload_layout.tag != .struct_) continue;

                const struct_idx = payload_layout.getStruct().idx;
                const struct_data = self.layout_store.getStructData(struct_idx);
                const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                if (fields.len != 2) continue;

                var has_index_field = false;
                var has_problem_field = false;
                for (0..fields.len) |field_i| {
                    const field = fields.get(field_i);
                    const field_layout = self.layout_store.getLayout(field.layout);
                    const field_size = self.layout_store.layoutSizeAlign(field_layout).size;
                    switch (field.index) {
                        0 => has_index_field = field_size == 8,
                        1 => has_problem_field = field_size == 1,
                        else => {},
                    }
                }
                if (has_index_field and has_problem_field) {
                    return .{ .disc = @intCast(i), .struct_idx = struct_idx };
                }
            }

            return null;
        }

        fn coerceImmediateForStackCopy(self: *Self, loc: ValueLocation) Allocator.Error!ValueLocation {
            return switch (loc) {
                .general_reg, .float_reg => loc,
                .immediate_f64 => .{ .float_reg = try self.ensureInFloatReg(loc) },
                .immediate_i64,
                .immediate_i128,
                .stack,
                .stack_i128,
                .stack_str,
                .list_stack,
                => .{ .general_reg = try self.ensureInGeneralReg(loc) },
                .noreturn => unreachable,
            };
        }

        /// Ensure a value is in a floating-point register
        fn ensureInFloatReg(self: *Self, loc_in: ValueLocation) Allocator.Error!FloatReg {
            var loc = loc_in;
            while (true) switch (loc) {
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
                    loc = .{ .immediate_f64 = f_val };
                },
                .general_reg, .immediate_i128, .stack_i128, .stack_str, .list_stack => {
                    unreachable;
                },
                .noreturn => unreachable,
            };
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
                                const tuple_data = ls.getStructData(tuple_layout.getStruct().idx);
                                const total_size = tuple_data.size.get(ls.targetUsize());

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
                .i64, .u64 => {
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreScalarToPtr(saved_ptr_reg, reg, 8);
                },
                .i32, .u32 => {
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreScalarToPtr(saved_ptr_reg, reg, 4);
                },
                .i16, .u16 => {
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreScalarToPtr(saved_ptr_reg, reg, 2);
                },
                .u8 => {
                    // Zero-extend to 64 bits before storing, since the register
                    // may have garbage in the upper bits from mutable variable loads.
                    // Shift left 56, then logical shift right 56 to clear upper bits.
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitShlImm(.w64, reg, reg, 56);
                    try self.emitLsrImm(.w64, reg, reg, 56);
                    try self.emitStoreScalarToPtr(saved_ptr_reg, reg, 1);
                },
                .i8 => {
                    // Sign-extend to 64 bits before storing, since the register
                    // may have garbage in the upper bits from mutable variable loads.
                    // Shift left 56, then arithmetic shift right 56 to sign-extend.
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitShlImm(.w64, reg, reg, 56);
                    try self.emitAsrImm(.w64, reg, reg, 56);
                    try self.emitStoreScalarToPtr(saved_ptr_reg, reg, 1);
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
                    // Strings are 24 bytes.
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
                    const layout_val = ls.getLayout(self.runtimeRepresentationLayoutIdx(result_layout));
                    switch (layout_val.tag) {
                        .struct_ => {
                            const struct_data = ls.getStructData(layout_val.getStruct().idx);
                            try self.copyStackToPtr(loc, saved_ptr_reg, struct_data.size.get(ls.targetUsize()));
                            return;
                        },
                        .tag_union => {
                            const tu_data = ls.getTagUnionData(layout_val.getTagUnion().idx);
                            try self.copyStackToPtr(loc, saved_ptr_reg, tu_data.size.get(ls.targetUsize()));
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
                                try self.emitStoreScalarToPtr(saved_ptr_reg, reg, sa.size);
                            }
                            return;
                        },
                        .zst => {
                            // Zero-sized type — nothing to store.
                            return;
                        },
                        .box, .erased_callable, .ptr => {
                            // Box is a heap pointer (machine word)
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                            return;
                        },
                        .box_of_zst => {
                            // Box of zero-sized type — nothing to store.
                            return;
                        },
                        .closure => {
                            if (builtin.mode == .Debug) {
                                std.debug.panic(
                                    "LIR/codegen invariant violated: runtimeRepresentationLayoutIdx returned closure for result layout {}",
                                    .{@intFromEnum(result_layout)},
                                );
                            }
                            unreachable;
                        },
                    }
                },
            }
        }

        /// Copy bytes from stack location to memory pointed to by ptr_reg
        fn copyStackToPtr(self: *Self, loc_in: ValueLocation, ptr_reg: GeneralReg, size: u32) Allocator.Error!void {
            var loc = loc_in;
            while (true) switch (loc) {
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
                            try self.codegen.emit.strhRegMemSoff(temp_reg, ptr_reg, dst_offset);
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
                            try self.codegen.emit.strbRegMemSoff(temp_reg, ptr_reg, dst_offset);
                        } else {
                            try self.codegen.emitLoadStack(.w8, temp_reg, src_offset);
                            try self.codegen.emit.movMemReg(.w8, ptr_reg, dst_offset, temp_reg);
                        }
                    }

                    self.codegen.freeGeneral(temp_reg);
                    return;
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
                    return;
                },
                else => {
                    // Materialize non-stack values to a stack slot first so we preserve the
                    // requested byte width for narrow results like Bool and small tag unions.
                    const temp_slot = self.codegen.allocStackSlot(size);
                    try self.copyBytesToStackOffset(temp_slot, loc, size);
                    loc = .{ .stack = .{ .offset = temp_slot } };
                },
            };
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
                        try self.emitStoreToPtr(.w64, reg, ptr_reg, 0);
                        try self.codegen.emit.asrRegRegImm(.w64, sign_reg, reg, 63);
                        try self.emitStoreToPtr(.w64, sign_reg, ptr_reg, 8);
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

        /// Copy a specific number of bytes from a value location to a stack offset.
        fn copyBytesToStackOffset(self: *Self, dest_offset: i32, loc: ValueLocation, size: u32) Allocator.Error!void {
            if (size == 0) return;

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
                            try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                            const high: i64 = if (val < 0) -1 else 0;
                            try self.codegen.emitLoadImm(reg, high);
                            try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                        },
                        roc_list_size => {
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
                    defer self.codegen.freeGeneral(reg);

                    switch (size) {
                        16 => {
                            try self.codegen.emitLoadImm(reg, @bitCast(low));
                            try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                            try self.codegen.emitLoadImm(reg, @bitCast(high));
                            try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                        },
                        8 => {
                            try self.codegen.emitLoadImm(reg, @bitCast(low));
                            try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                        },
                        4 => {
                            try self.codegen.emitLoadImm(reg, @intCast(@as(u32, @truncate(low))));
                            try self.codegen.emitStoreStack(.w32, dest_offset, reg);
                        },
                        else => unreachable,
                    }
                    return;
                },
                .stack_i128 => |src_offset| {
                    const temp_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(temp_reg);
                    try self.copyChunked(temp_reg, frame_ptr, src_offset, frame_ptr, dest_offset, size);
                    return;
                },
                .stack_str => |src_offset| {
                    const temp_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(temp_reg);
                    try self.copyChunked(temp_reg, frame_ptr, src_offset, frame_ptr, dest_offset, size);
                    return;
                },
                .stack => |stack_loc| {
                    const temp_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(temp_reg);
                    try self.copyChunked(temp_reg, frame_ptr, stack_loc.offset, frame_ptr, dest_offset, size);
                    return;
                },
                .list_stack => |info| {
                    const temp_reg = try self.allocTempGeneral();
                    defer self.codegen.freeGeneral(temp_reg);
                    try self.copyChunked(temp_reg, frame_ptr, info.struct_offset, frame_ptr, dest_offset, size);
                    return;
                },
                else => {},
            }

            const normalized_value_loc = try self.coerceImmediateForStackCopy(loc);
            switch (normalized_value_loc) {
                .general_reg => |reg| {
                    switch (size) {
                        1 => try self.emitStoreStackW8(dest_offset, reg),
                        2 => try self.emitStoreStackW16(dest_offset, reg),
                        4 => try self.codegen.emitStoreStack(.w32, dest_offset, reg),
                        8 => try self.codegen.emitStoreStack(.w64, dest_offset, reg),
                        else => unreachable,
                    }
                    self.codegen.freeGeneral(reg);
                },
                .float_reg => |freg| {
                    switch (size) {
                        4 => {
                            if (comptime target.toCpuArch() == .aarch64) {
                                try self.codegen.emit.fcvtFloatFloat(.single, freg, .double, freg);
                                try self.codegen.emitStoreStackF32(dest_offset, freg);
                            } else {
                                try self.codegen.emit.cvtsd2ssRegReg(freg, freg);
                                try self.codegen.emit.movssMemReg(frame_ptr, dest_offset, freg);
                            }
                            self.codegen.freeFloat(freg);
                        },
                        8 => {
                            if (comptime target.toCpuArch() == .aarch64) {
                                try self.codegen.emitStoreStackF64(dest_offset, freg);
                            } else {
                                try self.codegen.emit.movsdMemReg(frame_ptr, dest_offset, freg);
                            }
                            self.codegen.freeFloat(freg);
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        }

        fn emitLoad(self: *Self, comptime width: anytype, dst: GeneralReg, base_reg: GeneralReg, offset: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.ldrRegMemSoff(width, dst, base_reg, offset);
            } else {
                try self.codegen.emit.movRegMem(width, dst, base_reg, offset);
            }
        }

        fn emitStore(self: *Self, comptime width: anytype, base_reg: GeneralReg, offset: i32, src: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.strRegMemSoff(width, src, base_reg, offset);
            } else {
                try self.codegen.emit.movMemReg(width, base_reg, offset, src);
            }
        }

        fn emitLoadW8(self: *Self, dst: GeneralReg, base_reg: GeneralReg, offset: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.ldrbRegMemSoff(dst, base_reg, offset);
            } else {
                try self.codegen.emit.movzxBRegMem(dst, base_reg, offset);
            }
        }

        fn emitLoadW16(self: *Self, dst: GeneralReg, base_reg: GeneralReg, offset: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.ldrhRegMemSoff(dst, base_reg, offset);
            } else {
                try self.codegen.emit.movzxWRegMem(dst, base_reg, offset);
            }
        }

        fn emitStoreW8(self: *Self, base_reg: GeneralReg, offset: i32, src: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.strbRegMemSoff(src, base_reg, offset);
            } else {
                try self.codegen.emit.movMemReg(.w8, base_reg, offset, src);
            }
        }

        fn emitStoreW16(self: *Self, base_reg: GeneralReg, offset: i32, src: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.strhRegMemSoff(src, base_reg, offset);
            } else {
                try self.codegen.emit.movMemReg(.w16, base_reg, offset, src);
            }
        }

        fn emitAddRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) Allocator.Error!void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.addRegRegReg(width, dst, src1, src2);
            } else {
                try self.codegen.emit.addRegReg(width, dst, src2);
            }
        }

        fn emitSubRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) Allocator.Error!void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.subRegRegReg(width, dst, src1, src2);
            } else {
                if (dst != src1) try self.codegen.emit.movRegReg(width, dst, src1);
                try self.codegen.emit.subRegReg(width, dst, src2);
            }
        }

        fn emitMulRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) Allocator.Error!void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.mulRegRegReg(width, dst, src1, src2);
            } else {
                try self.codegen.emit.imulRegReg(width, dst, src2);
            }
        }

        fn emitAndRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) Allocator.Error!void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.andRegRegReg(width, dst, src1, src2);
            } else {
                try self.codegen.emit.andRegReg(width, dst, src2);
            }
        }

        fn emitOrRegs(self: *Self, comptime width: anytype, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) Allocator.Error!void {
            std.debug.assert(arch == .aarch64 or arch == .aarch64_be or dst == src1);
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.orrRegRegReg(width, dst, src1, src2);
            } else {
                if (dst != src1) try self.codegen.emit.movRegReg(width, dst, src1);
                try self.codegen.emit.orRegReg(width, dst, src2);
            }
        }

        fn emitLoadStackW8(self: *Self, dst: GeneralReg, offset: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emitLoadStackByte(dst, offset);
            } else {
                try self.codegen.emit.movzxBRegMem(dst, frame_ptr, offset);
            }
        }

        fn emitLoadStackW16(self: *Self, dst: GeneralReg, offset: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emitLoadStackHalfword(dst, offset);
            } else {
                try self.codegen.emit.movzxWRegMem(dst, frame_ptr, offset);
            }
        }

        fn emitShlImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: u8) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.lslRegRegImm(width, dst, src, @intCast(amount));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.shlRegImm8(width, dst, amount);
            }
        }

        fn emitLsrImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: u8) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.lsrRegRegImm(width, dst, src, @intCast(amount));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.shrRegImm8(width, dst, amount);
            }
        }

        fn emitAsrImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: u8) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.asrRegRegImm(width, dst, src, @intCast(amount));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.sarRegImm8(width, dst, amount);
            }
        }

        fn emitShlReg(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.lslRegReg(width, dst, src, amount);
            } else {
                try self.emitShiftRegX86(width, dst, src, amount, .shl);
            }
        }

        fn emitLsrReg(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.lsrRegReg(width, dst, src, amount);
            } else {
                try self.emitShiftRegX86(width, dst, src, amount, .shr);
            }
        }

        fn emitAsrReg(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.asrRegReg(width, dst, src, amount);
            } else {
                try self.emitShiftRegX86(width, dst, src, amount, .sar);
            }
        }

        const ShiftOp = enum { shl, shr, sar };

        fn emitShiftRegX86(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, amount: GeneralReg, comptime op: ShiftOp) Allocator.Error!void {
            if (dst == .RCX) {
                if (src == .RCX and amount == .R11) {
                    try self.codegen.emit.xchgRegReg(.w64, .RCX, .R11);
                } else if (amount == .R11) {
                    try self.codegen.emit.movRegReg(.w64, .RCX, amount);
                    if (src != .R11) try self.codegen.emit.movRegReg(width, .R11, src);
                } else {
                    if (src != .R11) try self.codegen.emit.movRegReg(width, .R11, src);
                    if (amount != .RCX) try self.codegen.emit.movRegReg(.w64, .RCX, amount);
                }
                switch (op) {
                    .shl => try self.codegen.emit.shlRegCl(width, .R11),
                    .shr => try self.codegen.emit.shrRegCl(width, .R11),
                    .sar => try self.codegen.emit.sarRegCl(width, .R11),
                }
                try self.codegen.emit.movRegReg(width, .RCX, .R11);
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                if (amount != .RCX) try self.codegen.emit.movRegReg(.w64, .RCX, amount);
                switch (op) {
                    .shl => try self.codegen.emit.shlRegCl(width, dst),
                    .shr => try self.codegen.emit.shrRegCl(width, dst),
                    .sar => try self.codegen.emit.sarRegCl(width, dst),
                }
            }
        }

        fn emitSaturatingSub(self: *Self, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.cmpRegReg(.w64, a, b);
                try self.codegen.emit.subRegRegReg(.w64, dst, a, b);
                try self.codegen.emit.csel(.w64, dst, dst, .ZRSP, .cs);
            } else {
                if (dst != a) try self.codegen.emit.movRegReg(.w64, dst, a);
                try self.codegen.emit.subRegReg(.w64, dst, b);
                const patch_loc = try self.codegen.emitCondJump(.above_or_equal);
                try self.codegen.emit.xorRegReg(.w64, dst, dst);
                self.codegen.patchJump(patch_loc, self.codegen.currentOffset());
            }
        }

        fn emitAddImm(self: *Self, dst: GeneralReg, src: GeneralReg, imm: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.addRegRegImm12(.w64, dst, src, @intCast(imm));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(.w64, dst, src);
                try self.codegen.emit.addImm(dst, imm);
            }
        }

        fn emitAddPtrImmAny(self: *Self, dst: GeneralReg, src: GeneralReg, imm: i32) Allocator.Error!void {
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

        fn emitSubImm(self: *Self, comptime width: anytype, dst: GeneralReg, src: GeneralReg, imm: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.subRegRegImm12(width, dst, src, @intCast(imm));
            } else {
                if (dst != src) try self.codegen.emit.movRegReg(width, dst, src);
                try self.codegen.emit.subRegImm32(width, dst, imm);
            }
        }

        fn emitSetCond(self: *Self, dst: GeneralReg, cond: Condition) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.cset(.w64, dst, cond);
            } else {
                try self.codegen.emit.setcc(cond, dst);
                try self.codegen.emit.andRegImm32(dst, 0xFF);
            }
        }

        fn emitLeaStack(self: *Self, dst: GeneralReg, offset: i32) Allocator.Error!void {
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

        fn emitAddStackPtr(self: *Self, imm: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emit.addRegRegImm12(.w64, stack_ptr, stack_ptr, @intCast(imm));
            } else {
                try self.codegen.emit.addRegImm32(.w64, stack_ptr, imm);
            }
        }

        fn copyChunked(self: *Self, temp_reg: GeneralReg, src_base: GeneralReg, src_offset: i32, dst_base: GeneralReg, dst_offset: i32, size: u32) Allocator.Error!void {
            std.debug.assert(size > 0);
            if (size == 8) {
                try self.emitLoad(.w64, temp_reg, src_base, src_offset);
                try self.emitStore(.w64, dst_base, dst_offset, temp_reg);
                return;
            }
            if (size < 8) {
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

            if (copied < size) {
                const tail = @as(i32, @intCast(size - 8));
                const s = src_offset + tail;
                const d = dst_offset + tail;
                try self.emitLoad(.w64, temp_reg, src_base, s);
                try self.emitStore(.w64, dst_base, d, temp_reg);
            }
        }

        fn zeroStackArea(self: *Self, offset: i32, size: u32) Allocator.Error!void {
            const reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(reg);
            try self.codegen.emitLoadImm(reg, 0);

            var remaining = size;
            var current_offset = offset;
            while (remaining >= 8) {
                try self.codegen.emitStoreStack(.w64, current_offset, reg);
                current_offset += 8;
                remaining -= 8;
            }
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
        }

        fn poisonStackArea(self: *Self, offset: i32, size: u32) Allocator.Error!void {
            const reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(reg);
            try self.codegen.emitLoadImm(reg, @as(i64, @bitCast(@as(u64, 0xAAAAAAAAAAAAAAAA))));

            var remaining = size;
            var current_offset = offset;
            while (remaining >= 8) {
                try self.codegen.emitStoreStack(.w64, current_offset, reg);
                current_offset += 8;
                remaining -= 8;
            }
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
        }

        /// Zero-fill `size` bytes through a base register (heap memory),
        /// mirroring zeroStackArea's chunking.
        fn zeroMemAt(self: *Self, base_reg: GeneralReg, size: u32) Allocator.Error!void {
            if (size == 0) return;
            const reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(reg);
            try self.codegen.emitLoadImm(reg, 0);

            var remaining = size;
            var off: i32 = 0;
            while (remaining >= 8) {
                try self.emitStore(.w64, base_reg, off, reg);
                off += 8;
                remaining -= 8;
            }
            if (remaining >= 4) {
                try self.emitStore(.w32, base_reg, off, reg);
                off += 4;
                remaining -= 4;
            }
            if (remaining >= 2) {
                try self.emitStoreW16(base_reg, off, reg);
                off += 2;
                remaining -= 2;
            }
            if (remaining >= 1) {
                try self.emitStoreW8(base_reg, off, reg);
            }
        }

        fn emitStoreToPtr(self: *Self, comptime width: anytype, src: GeneralReg, ptr_reg: GeneralReg, byte_offset: i32) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                switch (width) {
                    .w64, .w32 => try self.codegen.emit.strRegMemSoff(width, src, ptr_reg, byte_offset),
                    else => @compileError("Use strhRegMem/strbRegMem for .w16/.w8"),
                }
            } else {
                try self.codegen.emit.movMemReg(width, ptr_reg, byte_offset, src);
            }
        }

        fn emitStoreStackW8(self: *Self, offset: i32, src: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emitStoreStackByte(offset, src);
            } else {
                try self.codegen.emitStoreStack(.w8, offset, src);
            }
        }

        fn emitStoreStackW16(self: *Self, offset: i32, src: GeneralReg) Allocator.Error!void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                try self.codegen.emitStoreStackHalfword(offset, src);
            } else {
                try self.codegen.emitStoreStack(.w16, offset, src);
            }
        }

        /// Store general register to memory at [ptr_reg] (architecture-specific)
        fn emitStoreToMem(self: *Self, ptr_reg: anytype, src_reg: GeneralReg) Allocator.Error!void {
            try self.emitStoreToPtr(.w64, src_reg, ptr_reg, 0);
        }

        fn emitStoreScalarToPtr(self: *Self, ptr_reg: GeneralReg, src_reg: GeneralReg, size: u32) Allocator.Error!void {
            switch (size) {
                0 => {},
                1 => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        try self.codegen.emit.strbRegMem(src_reg, ptr_reg, 0);
                    } else {
                        try self.codegen.emit.movMemReg(.w8, ptr_reg, 0, src_reg);
                    }
                },
                2 => {
                    if (comptime arch == .aarch64 or arch == .aarch64_be) {
                        try self.codegen.emit.strhRegMem(src_reg, ptr_reg, 0);
                    } else {
                        try self.codegen.emit.movMemReg(.w16, ptr_reg, 0, src_reg);
                    }
                },
                4 => try self.emitStoreToPtr(.w32, src_reg, ptr_reg, 0),
                8 => try self.emitStoreToPtr(.w64, src_reg, ptr_reg, 0),
                else => if (builtin.mode == .Debug) {
                    std.debug.panic("LIR/codegen invariant violated: scalar result size {d} is not register-sized", .{size});
                } else unreachable,
            }
        }

        /// Store float register to memory at [ptr_reg] (architecture-specific)
        fn emitStoreFloatToMem(self: *Self, ptr_reg: anytype, src_reg: FloatReg) Allocator.Error!void {
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.fstrRegMemUoff(.double, src_reg, ptr_reg, 0);
            } else {
                try self.codegen.emit.movsdMemReg(ptr_reg, 0, src_reg);
            }
        }

        fn generateRcOperandValue(self: *Self, local: LocalId) Allocator.Error!ValueLocation {
            return self.emitValueLocal(local);
        }

        /// Generate code for incref operation.
        fn generateIncref(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            const value_loc = try self.generateRcOperandValue(rc_op.value);
            try self.emitExplicitRcHelperCallForValue(.{ .key = rc_op.rc, .atomicity = rc_op.atomicity }, value_loc, rc_op.count);
            return value_loc;
        }

        /// Generate code for decref operation.
        fn generateDecref(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            const value_loc = try self.generateRcOperandValue(rc_op.value);
            try self.emitExplicitRcHelperCallForValue(.{ .key = rc_op.rc, .atomicity = rc_op.atomicity }, value_loc, 1);
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

        /// Generate code for free operation.
        fn generateFree(self: *Self, rc_op: anytype) Allocator.Error!ValueLocation {
            const value_loc = try self.generateRcOperandValue(rc_op.value);
            try self.emitExplicitRcHelperCallForValue(.{ .key = rc_op.rc, .atomicity = rc_op.atomicity }, value_loc, 1);
            return value_loc;
        }

        pub fn patchPendingCalls(self: *Self) Allocator.Error!void {
            for (self.pending_calls.items) |pending| {
                const proc = self.proc_registry.get(@intFromEnum(pending.target_proc)) orelse unreachable;
                self.patchCallTarget(pending.call_site, proc.code_start);
            }
            self.pending_calls.clearRetainingCapacity();
        }

        pub fn patchPendingProcAddrs(self: *Self) Allocator.Error!void {
            for (self.pending_proc_addrs.items) |pending| {
                const proc = self.proc_registry.get(@intFromEnum(pending.target_proc)) orelse unreachable;
                self.patchInternalCodeAddress(pending.instr_offset, proc.code_start);
            }
            self.pending_proc_addrs.clearRetainingCapacity();
        }

        fn patchCallTarget(self: *Self, call_site: usize, target_offset: usize) void {
            const rel_offset: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(call_site)));

            if (comptime target.toCpuArch() == .aarch64) {
                const instr_offset = @divTrunc(rel_offset, 4);
                self.codegen.patchBL(call_site, instr_offset);
            } else {
                const call_rel = rel_offset - 5;
                self.codegen.patchCall(call_site, call_rel);
            }
        }

        fn patchInternalCodeAddress(self: *Self, instr_offset: usize, target_offset: usize) void {
            const buf = self.codegen.emit.buf.items;
            if (comptime target.toCpuArch() == .aarch64) {
                // The emit reserves a 3-instruction PC-relative sequence (ADR + two
                // ADD/SUB imm12) starting at instr_offset. Re-derive the relative offset
                // from the new instr_offset and rewrite all three instructions. The
                // existing ADR's Rd is preserved so the patched sequence targets the
                // same register the caller originally chose.
                const rel: i64 = @as(i64, @intCast(target_offset)) - @as(i64, @intCast(instr_offset));
                const negative = rel < 0;
                const abs_rel: u64 = if (negative) @intCast(-rel) else @intCast(rel);
                const hi12: u32 = @as(u32, @as(u12, @truncate(abs_rel >> 12)));
                const lo12: u32 = @as(u32, @as(u12, @truncate(abs_rel)));

                const adr_existing: u32 = @bitCast(buf[instr_offset..][0..4].*);
                const rd_bits: u32 = adr_existing & 0x1F;

                // Rewrite the ADR with offset 0 so Xd holds the anchor PC.
                const adr_inst: u32 = (0 << 31) |
                    (0b10000 << 24) |
                    rd_bits;
                @memcpy(buf[instr_offset..][0..4], &@as([4]u8, @bitCast(adr_inst)));

                // ADD/SUB Xd, Xd, #hi12, LSL #12
                const op_bits: u32 = if (negative) 0b1010001 else 0b0010001;
                const rn_rd_bits: u32 = (rd_bits << 5) | rd_bits;
                const hi_inst: u32 = (1 << 31) |
                    (op_bits << 24) |
                    (1 << 22) |
                    (hi12 << 10) |
                    rn_rd_bits;
                @memcpy(buf[instr_offset + 4 ..][0..4], &@as([4]u8, @bitCast(hi_inst)));

                // ADD/SUB Xd, Xd, #lo12
                const lo_inst: u32 = (1 << 31) |
                    (op_bits << 24) |
                    (0 << 22) |
                    (lo12 << 10) |
                    rn_rd_bits;
                @memcpy(buf[instr_offset + 8 ..][0..4], &@as([4]u8, @bitCast(lo_inst)));
            } else {
                const new_rel: i64 = @as(i64, @intCast(target_offset)) - @as(i64, @intCast(instr_offset));
                const lea_size: i64 = 7;
                const disp: i32 = @intCast(new_rel - lea_size);
                const bytes: [4]u8 = @bitCast(disp);
                @memcpy(buf[instr_offset + 3 ..][0..4], &bytes);
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
                    .args = proc.args,
                });
            }

            for (proc_specs, 0..) |proc, i| {
                try self.compileProcSpec(@enumFromInt(i), proc);
            }

            try self.patchPendingCalls();
            try self.patchPendingProcAddrs();
        }

        /// Returns object-file symbol metadata for a compiled LIR procedure.
        ///
        /// This is used by native object emission to let readonly static data
        /// relocate to erased-callable wrapper procedures. The procedure must
        /// already have been compiled by `compileAllProcSpecs`.
        pub fn compiledProcSymbol(self: *const Self, proc_id: lir.LIR.LirProcSpecId) ?CompiledProcSymbol {
            const proc = self.proc_registry.get(@intFromEnum(proc_id)) orelse return null;
            if (proc.code_start == unresolved_proc_code_start) return null;
            return .{
                .name = proc.name,
                .code_start = proc.code_start,
                .code_end = proc.code_end,
                .prologue_size = proc.prologue_size,
                .stack_alloc = proc.stack_alloc,
                .frame_size = proc.frame_size,
                .callee_saved_mask = proc.callee_saved_mask,
                .epilogue_offset = proc.epilogue_offset,
                .uses_frame_pointer = proc.uses_frame_pointer,
            };
        }

        /// Compile a single procedure as a complete unit.
        /// Uses deferred prologue pattern: generates body first to determine which
        /// callee-saved registers are used, then prepends prologue and adjusts relocations.
        fn compileProcSpec(self: *Self, proc_id: lir.LIR.LirProcSpecId, proc: LirProcSpec) Allocator.Error!void {
            const key: u32 = @intFromEnum(proc_id);
            const stack_probe_required = proc.stack_probe == .required;
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
            const saved_uses_caller_stack_arg_base = self.uses_caller_stack_arg_base;
            const saved_current_proc_name = self.current_proc_name;
            const saved_current_proc_args = self.current_proc_args;
            const saved_current_stmt_id = self.current_stmt_id;
            const saved_proc_debug_msg_slot = self.proc_debug_msg_slot;
            var saved_local_locations = self.local_locations.clone() catch return error.OutOfMemory;
            defer saved_local_locations.deinit();
            var saved_join_points = self.join_points.clone() catch return error.OutOfMemory;
            defer saved_join_points.deinit();
            var saved_stmt_locations = self.stmt_locations.clone() catch return error.OutOfMemory;
            defer saved_stmt_locations.deinit();
            var saved_join_point_jumps = try self.cloneJoinPointJumpsMap(&self.join_point_jumps);
            defer self.deinitJoinPointJumpsMap(&saved_join_point_jumps);
            var saved_join_point_params = self.join_point_params.clone() catch return error.OutOfMemory;
            defer saved_join_point_params.deinit();
            var saved_loop_continue_targets = try self.loop_continue_targets.clone(self.allocator);
            defer saved_loop_continue_targets.deinit(self.allocator);
            var saved_loop_break_patch_starts = try self.loop_break_patch_starts.clone(self.allocator);
            defer saved_loop_break_patch_starts.deinit(self.allocator);
            var saved_loop_break_patches = try self.loop_break_patches.clone(self.allocator);
            defer saved_loop_break_patches.deinit(self.allocator);

            // Clear state for procedure's scope
            self.local_locations.clearRetainingCapacity();
            self.clearFunctionControlFlowState();
            self.proc_debug_msg_slot = null;
            self.codegen.callee_saved_used = 0;
            self.codegen.callee_saved_available = CodeGen.CALLEE_SAVED_GENERAL_MASK;
            self.codegen.free_general = CodeGen.INITIAL_FREE_GENERAL;
            self.codegen.general_owners = [_]?u32{null} ** CodeGen.NUM_GENERAL_REGS;
            self.codegen.free_float = CodeGen.INITIAL_FREE_FLOAT;
            self.codegen.float_owners = [_]?u32{null} ** CodeGen.NUM_FLOAT_REGS;
            self.roc_ops_reg = null;
            self.uses_caller_stack_arg_base = false;
            self.current_proc_name = proc.name;
            self.current_proc_args = proc.args;
            self.current_stmt_id = null;

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
                    .args = proc.args,
                };
            } else {
                try self.proc_registry.put(key, .{
                    .id = proc_id,
                    .code_start = unresolved_proc_code_start,
                    .code_end = 0,
                    .name = proc.name,
                    .args = proc.args,
                });
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
                self.uses_caller_stack_arg_base = saved_uses_caller_stack_arg_base;
                self.current_proc_name = saved_current_proc_name;
                self.current_proc_args = saved_current_proc_args;
                self.current_stmt_id = saved_current_stmt_id;
                self.proc_debug_msg_slot = saved_proc_debug_msg_slot;
                // Restore the saved maps by swapping them back into place. This
                // cannot allocate (so it is safe in an errdefer that cannot
                // propagate errors): the mutated maps end up in the saved_*
                // locals, which their own defers deinit.
                std.mem.swap(@TypeOf(self.local_locations), &self.local_locations, &saved_local_locations);
                std.mem.swap(@TypeOf(self.join_points), &self.join_points, &saved_join_points);
                std.mem.swap(@TypeOf(self.stmt_locations), &self.stmt_locations, &saved_stmt_locations);
                std.mem.swap(@TypeOf(self.join_point_jumps), &self.join_point_jumps, &saved_join_point_jumps);
                std.mem.swap(@TypeOf(self.join_point_params), &self.join_point_params, &saved_join_point_params);
                std.mem.swap(@TypeOf(self.loop_continue_targets), &self.loop_continue_targets, &saved_loop_continue_targets);
                std.mem.swap(@TypeOf(self.loop_break_patch_starts), &self.loop_break_patch_starts, &saved_loop_break_patch_starts);
                std.mem.swap(@TypeOf(self.loop_break_patches), &self.loop_break_patches, &saved_loop_break_patches);
                self.early_return_ret_layout = saved_early_return_ret_layout;
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
            }

            const needs_ret_ptr = self.needsInternalReturnByPointer(proc.ret_layout);
            var hot_reload_code_ref_slot: ?i32 = null;
            if (proc.abi == .erased_callable) {
                self.ret_ptr_slot = self.codegen.allocStackSlot(8);
                const ret_ptr_reg = self.getArgumentRegister(1);
                try self.codegen.emitStoreStack(.w64, self.ret_ptr_slot.?, ret_ptr_reg);
                try self.bindErasedCallableAdapterParams(proc.args);
                hot_reload_code_ref_slot = try self.emitHotReloadEnterForHostCallable();
            } else {
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
                try self.bindProcParams(proc.args, initial_param_reg_idx);
            }

            if (proc.hosted) |hosted| {
                try self.generateHostedProcWrapper(hosted, proc);
            } else {
                const body = requireProcBody(proc);
                try self.ensureStableLocationsForStmtLocals(body);

                // Generate the body (control flow statements)
                // Note: .return_stmt emits jumps that are patched to the shared epilogue below
                try self.generateStmt(body);
            }

            // Emit shared epilogue using DeferredFrameBuilder with actual stack usage
            const body_epilogue_offset = self.codegen.currentOffset();
            if (hot_reload_code_ref_slot) |slot| {
                try self.emitHotReloadLeaveForHostCallable(slot);
            }
            {
                const actual_locals: u32 = if (comptime target.toCpuArch() == .aarch64)
                    @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE)
                else
                    @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                var builder = CodeGen.DeferredFrameBuilder.init();
                builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                builder.setStackSize(actual_locals);
                builder.setStackProbeRequired(stack_probe_required);
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
                try self.codegen.emitPrologueWithAllocAndStackProbe(actual_locals_x86, stack_probe_required);
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
                self.shiftPendingProcAddrs(body_start, body_end, prologue_size);

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
                    entry.prologue_size = @intCast(prologue_size);
                    entry.stack_alloc = actual_locals_x86;
                    entry.frame_size = actual_locals_x86;
                    entry.callee_saved_mask = self.codegen.callee_saved_used;
                    entry.epilogue_offset = @intCast(final_epilogue - prologue_start);
                    entry.uses_frame_pointer = true;
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
                frame_builder.setStackProbeRequired(stack_probe_required);
                if (self.uses_caller_stack_arg_base) {
                    frame_builder.setCallerStackArgBaseReg(caller_stack_arg_base_reg);
                }
                _ = try frame_builder.emitPrologue(&self.codegen.emit);
                const prologue_size = self.codegen.currentOffset() - prologue_start;
                const frame_size = frame_builder.actual_stack_alloc;

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
                self.shiftPendingProcAddrs(body_start, body_end, prologue_size);

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
                    entry.prologue_size = @intCast(prologue_size);
                    entry.stack_alloc = actual_locals;
                    entry.frame_size = frame_size;
                    entry.callee_saved_mask = self.codegen.callee_saved_used;
                    entry.epilogue_offset = @intCast(final_epilogue - prologue_start);
                    entry.uses_frame_pointer = true;
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
            self.uses_caller_stack_arg_base = saved_uses_caller_stack_arg_base;
            self.ret_ptr_slot = saved_ret_ptr_slot;
            self.current_proc_name = saved_current_proc_name;
            self.current_proc_args = saved_current_proc_args;
            self.current_stmt_id = saved_current_stmt_id;
            self.proc_debug_msg_slot = saved_proc_debug_msg_slot;
            self.local_locations.deinit();
            self.local_locations = saved_local_locations.clone() catch return error.OutOfMemory;
            self.join_points.deinit();
            self.join_points = saved_join_points.clone() catch return error.OutOfMemory;
            self.stmt_locations.deinit();
            self.stmt_locations = saved_stmt_locations.clone() catch return error.OutOfMemory;
            self.deinitJoinPointJumpsMap(&self.join_point_jumps);
            self.join_point_jumps = try self.cloneJoinPointJumpsMap(&saved_join_point_jumps);
            self.join_point_params.deinit();
            self.join_point_params = try saved_join_point_params.clone();
            self.loop_continue_targets.deinit(self.allocator);
            self.loop_continue_targets = try saved_loop_continue_targets.clone(self.allocator);
            self.loop_break_patch_starts.deinit(self.allocator);
            self.loop_break_patch_starts = try saved_loop_break_patch_starts.clone(self.allocator);
            self.loop_break_patches.deinit(self.allocator);
            self.loop_break_patches = try saved_loop_break_patches.clone(self.allocator);
        }

        fn requireProcBody(proc: LirProcSpec) lir.LIR.CFStmtId {
            return proc.body orelse std.debug.panic(
                "Dev/codegen invariant violated: non-hosted proc {d} missing statement body",
                .{proc.name.raw()},
            );
        }

        fn generateHostedProcWrapper(
            self: *Self,
            hosted: lir.LIR.HostedProc,
            proc: LirProcSpec,
        ) Allocator.Error!void {
            if (builtin.mode == .Debug and proc.body != null) {
                std.debug.panic(
                    "Dev/codegen invariant violated: hosted proc {d} unexpectedly carried a statement body",
                    .{proc.name.raw()},
                );
            }

            const params = self.store.getLocalSpan(proc.args);
            const arg_locs = try self.allocator.alloc(ValueLocation, params.len);
            defer self.allocator.free(arg_locs);
            const arg_layouts = try self.allocator.alloc(layout.Idx, params.len);
            defer self.allocator.free(arg_layouts);

            for (params, 0..) |param, i| {
                const arg_layout = self.localLayout(param);
                const raw_arg_loc = try self.emitValueLocal(param);
                arg_locs[i] = self.requireExactValueLocationToLayout(raw_arg_loc, arg_layout, arg_layout, "hosted_wrapper.arg");
                arg_layouts[i] = arg_layout;
            }

            const raw_result_loc = try self.generateHostedCall(hosted, arg_locs, arg_layouts, proc.ret_layout);
            const result_loc = self.requireExactValueLocationToLayout(raw_result_loc, proc.ret_layout, proc.ret_layout, "hosted_wrapper.ret");

            if (self.ret_ptr_slot) |ret_slot| {
                try self.copyResultToReturnPointer(result_loc, proc.ret_layout, ret_slot);
            } else {
                try self.moveToReturnRegisterWithLayout(result_loc, proc.ret_layout);
            }
        }

        /// Internal Roc proc calls use at most two general-purpose return registers.
        /// Larger runtime values use a hidden return pointer instead.
        const max_internal_return_words: u32 = 2;
        const max_internal_return_size: u32 = max_internal_return_words * 8;

        /// Check if a return type exceeds the register limit and needs return-by-pointer.
        /// When true, the caller passes a hidden first argument (pointer to a pre-allocated
        /// buffer) and the callee writes the result there instead of using return registers.
        fn needsInternalReturnByPointer(self: *Self, ret_layout: layout.Idx) bool {
            const ls = self.layout_store;
            const runtime_layout_idx = self.runtimeRepresentationLayoutIdx(ret_layout);
            const runtime_layout = ls.getLayout(runtime_layout_idx);
            return ls.layoutSizeAlign(runtime_layout).size > max_internal_return_size;
        }

        /// Save the return value from a call into a stack-based ValueLocation.
        /// Save the return value from a compiled proc call into a stack-based ValueLocation.
        /// Handles i128/str/list/multi-reg struct/scalar returns.
        fn saveCallReturnValue(self: *Self, ret_layout: layout.Idx, needs_ret_ptr: bool, ret_buffer_offset: i32) Allocator.Error!ValueLocation {
            const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
            // If we used return-by-pointer, the callee has written the result
            // to our pre-allocated buffer. No register saving needed.
            if (needs_ret_ptr) {
                return self.stackLocationForLayout(runtime_ret_layout, ret_buffer_offset);
            }

            // Float returns come back in the float return register (V0/XMM0),
            // not in the integer return register (X0/RAX).
            if (runtime_ret_layout == .f32) {
                const stack_offset = self.codegen.allocStackSlot(4);
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emitStoreStackF32(stack_offset, .V0);
                } else {
                    try self.codegen.emit.movssMemReg(.RBP, stack_offset, .XMM0);
                }
                return .{ .stack = .{ .offset = stack_offset, .size = .dword } };
            }

            if (runtime_ret_layout == .f64) {
                const stack_offset = self.codegen.allocStackSlot(8);
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emitStoreStackF64(stack_offset, .V0);
                } else {
                    try self.codegen.emitStoreStackF64(stack_offset, .XMM0);
                }
                return .{ .stack = .{ .offset = stack_offset } };
            }

            // Handle i128/Dec return values (returned in two registers)
            if (runtime_ret_layout == .i128 or runtime_ret_layout == .u128 or runtime_ret_layout == .dec) {
                const stack_offset = self.codegen.allocStackSlot(16);
                try self.codegen.emitStoreStack(.w64, stack_offset, ret_reg_0);
                try self.codegen.emitStoreStack(.w64, stack_offset + 8, ret_reg_1);
                return .{ .stack_i128 = stack_offset };
            }

            // Check if return type is a string (24 bytes)
            if (runtime_ret_layout == .str) {
                const stack_offset = self.codegen.allocStackSlot(roc_str_size);
                try self.emitStore(.w64, frame_ptr, stack_offset, ret_reg_0);
                try self.emitStore(.w64, frame_ptr, stack_offset + 8, ret_reg_1);
                try self.emitStore(.w64, frame_ptr, stack_offset + 16, ret_reg_2);
                return .{ .stack_str = stack_offset };
            }

            // Check if return type is a list (24 bytes)
            const is_list_return = blk: {
                const ls = self.layout_store;
                const layout_val = ls.getLayout(runtime_ret_layout);
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
                const layout_val = ls.getLayout(runtime_ret_layout);
                if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
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
            /// CTFE-only call-frame hooks. Emitted after argument values are
            /// frozen to stack slots, before ABI argument registers are loaded.
            comptime_call_hooks: ?ComptimeHooks = null,
        };

        const PlacedCall = struct {
            stack_spill_size: i32,
            comptime_call_entered: bool,
        };

        const FrozenCallArg = struct {
            stack_offset: i32,
            num_regs: u8,
            value_size: ValueSize = .qword,
            pass_by_ptr: bool,
        };

        fn freezeCallArg(self: *Self, info: ArgInfo, pass_by_ptr: bool) Allocator.Error!FrozenCallArg {
            std.debug.assert(info.num_regs > 0);

            if (pass_by_ptr or info.num_regs > 1) {
                const size: u32 = @as(u32, info.num_regs) * 8;
                const offset = switch (info.loc) {
                    .stack_i128, .stack_str => |off| off,
                    .stack => |s| s.offset,
                    .list_stack => |li| li.struct_offset,
                    .immediate_i128 => |val| blk: {
                        const slot = self.codegen.allocStackSlot(@intCast(size));
                        const low: u64 = @truncate(@as(u128, @bitCast(val)));
                        const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                        try self.codegen.emitLoadImm(scratch_reg, @bitCast(low));
                        try self.emitStore(.w64, frame_ptr, slot, scratch_reg);
                        try self.codegen.emitLoadImm(scratch_reg, @bitCast(high));
                        try self.emitStore(.w64, frame_ptr, slot + 8, scratch_reg);
                        break :blk slot;
                    },
                    else => if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "freezeCallArg expected stack-backed multi-reg arg, got {s} for layout {any}",
                            .{ @tagName(info.loc), info.layout_idx },
                        );
                    } else unreachable,
                };
                return .{
                    .stack_offset = offset,
                    .num_regs = info.num_regs,
                    .pass_by_ptr = pass_by_ptr,
                };
            }

            const arg_layout = info.layout_idx orelse .u64;
            if (arg_layout == .f32) {
                switch (info.loc) {
                    .stack => |s| {
                        if (s.size == .dword) {
                            return .{
                                .stack_offset = s.offset,
                                .num_regs = 1,
                                .value_size = .dword,
                                .pass_by_ptr = false,
                            };
                        }
                    },
                    .float_reg => |freg| {
                        const slot = self.codegen.allocStackSlot(4);
                        if (comptime target.toCpuArch() == .aarch64) {
                            const tmp = self.codegen.allocFloat() orelse unreachable;
                            try self.codegen.emit.fcvtFloatFloat(.single, tmp, .double, freg);
                            try self.codegen.emitStoreStackF32(slot, tmp);
                            self.codegen.freeFloat(tmp);
                        } else {
                            const tmp = self.codegen.allocFloat() orelse unreachable;
                            try self.codegen.emit.cvtsd2ssRegReg(tmp, freg);
                            try self.codegen.emitStoreStackF32(slot, tmp);
                            self.codegen.freeFloat(tmp);
                        }
                        return .{
                            .stack_offset = slot,
                            .num_regs = 1,
                            .value_size = .dword,
                            .pass_by_ptr = false,
                        };
                    },
                    else => {},
                }

                const slot = self.codegen.allocStackSlot(4);
                switch (info.loc) {
                    .general_reg => |reg| {
                        try self.emitStore(.w32, frame_ptr, slot, reg);
                    },
                    .immediate_i64 => |val| {
                        const f32_val: f32 = @floatFromInt(val);
                        const bits: u32 = @bitCast(f32_val);
                        try self.codegen.emitLoadImm(scratch_reg, @as(i64, bits));
                        try self.emitStore(.w32, frame_ptr, slot, scratch_reg);
                    },
                    .immediate_f64 => |val| {
                        const f32_val: f32 = @floatCast(val);
                        const bits: u32 = @bitCast(f32_val);
                        try self.codegen.emitLoadImm(scratch_reg, @as(i64, bits));
                        try self.emitStore(.w32, frame_ptr, slot, scratch_reg);
                    },
                    .stack => |s| {
                        const tmp = self.codegen.allocFloat() orelse unreachable;
                        try self.codegen.emitLoadStackF64(tmp, s.offset);
                        if (comptime target.toCpuArch() == .aarch64) {
                            try self.codegen.emit.fcvtFloatFloat(.single, tmp, .double, tmp);
                            try self.codegen.emitStoreStackF32(slot, tmp);
                        } else {
                            try self.codegen.emit.cvtsd2ssRegReg(tmp, tmp);
                            try self.codegen.emitStoreStackF32(slot, tmp);
                        }
                        self.codegen.freeFloat(tmp);
                    },
                    else => if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "freezeCallArg unsupported F32 source {s} for layout {any}",
                            .{ @tagName(info.loc), info.layout_idx },
                        );
                    } else unreachable,
                }
                return .{
                    .stack_offset = slot,
                    .num_regs = 1,
                    .value_size = .dword,
                    .pass_by_ptr = false,
                };
            }

            switch (info.loc) {
                .stack => |s| {
                    if (s.size == .qword) {
                        return .{
                            .stack_offset = s.offset,
                            .num_regs = 1,
                            .value_size = .qword,
                            .pass_by_ptr = false,
                        };
                    }
                },
                .stack_i128 => |offset| {
                    return .{
                        .stack_offset = offset,
                        .num_regs = 1,
                        .value_size = .qword,
                        .pass_by_ptr = false,
                    };
                },
                .stack_str => |offset| {
                    return .{
                        .stack_offset = offset,
                        .num_regs = 1,
                        .value_size = .qword,
                        .pass_by_ptr = false,
                    };
                },
                .list_stack => |li| {
                    return .{
                        .stack_offset = li.struct_offset,
                        .num_regs = 1,
                        .value_size = .qword,
                        .pass_by_ptr = false,
                    };
                },
                .float_reg => |freg| {
                    const slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStackF64(slot, freg);
                    return .{
                        .stack_offset = slot,
                        .num_regs = 1,
                        .value_size = .qword,
                        .pass_by_ptr = false,
                    };
                },
                else => {},
            }

            const slot = self.codegen.allocStackSlot(8);
            switch (info.loc) {
                .general_reg => |reg| {
                    try self.emitStore(.w64, frame_ptr, slot, reg);
                },
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(scratch_reg, val);
                    try self.emitStore(.w64, frame_ptr, slot, scratch_reg);
                },
                .immediate_f64 => |val| {
                    const bits: u64 = @bitCast(val);
                    try self.codegen.emitLoadImm(scratch_reg, @bitCast(bits));
                    try self.emitStore(.w64, frame_ptr, slot, scratch_reg);
                },
                .stack => |s| {
                    try self.emitValueLoadStack(scratch_reg, s.offset, s.size, s.layout_idx);
                    try self.emitStore(.w64, frame_ptr, slot, scratch_reg);
                },
                .noreturn => unreachable,
                else => if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "freezeCallArg unsupported scalar source {s} for layout {any}",
                        .{ @tagName(info.loc), info.layout_idx },
                    );
                } else unreachable,
            }
            return .{
                .stack_offset = slot,
                .num_regs = 1,
                .value_size = .qword,
                .pass_by_ptr = false,
            };
        }

        /// Place arguments in registers and/or stack slots per the calling convention.
        /// Handles i128 even-alignment on aarch64, 3-reg list/str, multi-reg structs,
        /// lambda_code addressing, pass-by-pointer conversion, and stack spilling.
        /// Returns the stack_spill_size allocated (caller must clean up after the call).
        fn placeCallArguments(self: *Self, arg_infos: []const ArgInfo, config: CallConfig) Allocator.Error!PlacedCall {
            // Compute stack_spill_size.
            // When pass_by_ptr is provided, multi-reg args that would overflow are
            // already converted to pointers — account for that.
            var stack_arg_bytes: i32 = 0;
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
                        stack_arg_bytes += @as(i32, nr) * 8;
                        reg_count = max_arg_regs;
                    }
                }
                // Account for roc_ops needing a slot
                if (config.emit_roc_ops) {
                    if (reg_count + 1 > max_arg_regs) {
                        stack_arg_bytes += 8;
                    }
                }
            }

            var stack_space_size = outgoing_stack_arg_base_offset + stack_arg_bytes;

            // Keep the outgoing argument area ABI-aligned. Padding sits above the
            // final spilled arg so stack arg 0 remains at its ABI-defined offset.
            if (stack_space_size > 0) {
                stack_space_size = @intCast(std.mem.alignForward(
                    u32,
                    @intCast(stack_space_size),
                    call_stack_alignment,
                ));
            }

            const frozen_args = try self.allocator.alloc(FrozenCallArg, arg_infos.len);
            defer self.allocator.free(frozen_args);
            for (arg_infos, 0..) |info, i| {
                if (info.num_regs == 0) {
                    frozen_args[i] = .{
                        .stack_offset = 0,
                        .num_regs = 0,
                        .pass_by_ptr = false,
                    };
                    continue;
                }

                const pbp = if (config.pass_by_ptr) |p| p[i] else false;
                frozen_args[i] = try self.freezeCallArg(info, pbp);
            }

            const comptime_call_entered = if (config.comptime_call_hooks) |hooks|
                try self.emitComptimeCallEnter(hooks)
            else
                false;

            // Allocate stack space for the callee's spilled arguments only after
            // CTFE hooks have run. The hook call has its own C ABI stack setup.
            if (stack_space_size > 0) {
                try self.emitSubImm(.w64, stack_ptr, stack_ptr, stack_space_size);
            }

            // Place arguments in registers or on stack
            var reg_idx: u8 = 0;
            var stack_arg_offset: i32 = outgoing_stack_arg_base_offset;

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
                const frozen = frozen_args[i];
                const arg_layout = info.layout_idx;

                // Check if this argument is passed by pointer
                if (frozen.pass_by_ptr) {
                    if (reg_idx < max_arg_regs) {
                        const arg_reg = self.getArgumentRegister(reg_idx);
                        try self.emitLeaStack(arg_reg, frozen.stack_offset);
                        reg_idx += 1;
                    } else {
                        try self.emitLeaStack(scratch_reg, frozen.stack_offset);
                        try self.spillArgToStack(.{ .general_reg = scratch_reg }, null, stack_arg_offset, 1);
                        stack_arg_offset += 8;
                        reg_idx = max_arg_regs;
                    }
                    continue;
                }

                // Check if this argument fits in registers
                if (reg_idx + info.num_regs <= max_arg_regs) {
                    // Handle i128/Dec arguments (need two registers, even-aligned on aarch64)
                    const is_i128_arg = self.argNeedsI128Abi(info.loc, arg_layout);
                    if (is_i128_arg) {
                        if (comptime target.toCpuArch() == .aarch64) {
                            if (reg_idx % 2 != 0) {
                                reg_idx += 1;
                            }
                        }
                        const low_reg = self.getArgumentRegister(reg_idx);
                        const high_reg = self.getArgumentRegister(reg_idx + 1);
                        try self.codegen.emitLoadStack(.w64, low_reg, frozen.stack_offset);
                        try self.codegen.emitLoadStack(.w64, high_reg, frozen.stack_offset + 8);
                        reg_idx += 2;
                        continue;
                    }

                    if (info.num_regs == 3) {
                        const reg0 = self.getArgumentRegister(reg_idx);
                        const reg1 = self.getArgumentRegister(reg_idx + 1);
                        const reg2 = self.getArgumentRegister(reg_idx + 2);
                        try self.emitLoad(.w64, reg0, frame_ptr, frozen.stack_offset);
                        try self.emitLoad(.w64, reg1, frame_ptr, frozen.stack_offset + 8);
                        try self.emitLoad(.w64, reg2, frame_ptr, frozen.stack_offset + 16);
                        reg_idx += 3;
                    } else if (info.num_regs > 1) {
                        // Multi-register struct (record > 8 bytes)
                        var ri: u8 = 0;
                        while (ri < info.num_regs) : (ri += 1) {
                            const r = self.getArgumentRegister(reg_idx + ri);
                            try self.codegen.emitLoadStack(.w64, r, frozen.stack_offset + @as(i32, ri) * 8);
                        }
                        reg_idx += info.num_regs;
                    } else {
                        // Single register argument
                        const arg_reg = self.getArgumentRegister(reg_idx);
                        try self.emitSizedLoadStack(arg_reg, frozen.stack_offset, frozen.value_size);
                        reg_idx += 1;
                    }
                } else {
                    // Spill to stack — registers exhausted
                    try self.spillArgToStack(
                        .{ .stack = .{ .offset = frozen.stack_offset, .size = frozen.value_size, .layout_idx = arg_layout orelse .u64 } },
                        arg_layout,
                        stack_arg_offset,
                        info.num_regs,
                    );
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

            return .{
                .stack_spill_size = stack_space_size,
                .comptime_call_entered = comptime_call_entered,
            };
        }

        /// Bind lambda parameters from argument registers.
        /// Similar to bindProcParams but works with pattern spans.
        /// Handles stack spilling when arguments exceed available registers.
        /// Calculate the number of registers a parameter needs based on its layout.
        fn calcParamRegCount(self: *Self, layout_idx: layout.Idx) u8 {
            const runtime_layout_idx = self.runtimeRepresentationLayoutIdx(layout_idx);
            // String parameters need 3 registers (24 bytes)
            if (runtime_layout_idx == .str) return 3;
            // i128/u128/Dec parameters need 2 registers (16 bytes)
            if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) return 2;

            {
                const ls = self.layout_store;
                if (@intFromEnum(runtime_layout_idx) < ls.layouts.len()) {
                    const layout_val = ls.getLayout(runtime_layout_idx);
                    if (layout_val.tag == .zst or ls.layoutSizeAlign(layout_val).size == 0) return 0;
                    // List parameters need 3 registers (24 bytes)
                    if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 3;
                    // Aggregate parameters may need multiple registers
                    if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
                        const size = ls.layoutSizeAlign(layout_val).size;
                        return aggregateArgRegisterPressure(size);
                    }
                }
            }
            // Default: single register
            return 1;
        }

        fn markCallerStackArgBaseUsed(self: *Self) void {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                const bit = @as(u32, 1) << @intFromEnum(caller_stack_arg_base_reg);
                if (std.debug.runtime_safety) {
                    std.debug.assert(self.codegen.general_owners[@intFromEnum(caller_stack_arg_base_reg)] == null);
                }
                self.uses_caller_stack_arg_base = true;
                self.codegen.callee_saved_used |= bit;
                self.codegen.callee_saved_available &= ~bit;
            }
        }

        fn callerStackArgBaseReg(self: *Self) GeneralReg {
            if (comptime arch == .aarch64 or arch == .aarch64_be) {
                self.markCallerStackArgBaseUsed();
                return caller_stack_arg_base_reg;
            } else {
                return frame_ptr;
            }
        }

        /// Copy parameter data from caller stack-argument slots to local stack.
        /// On x86_64 this uses FP plus the ABI's fixed stack-arg offset. On
        /// AArch64 it uses `caller_stack_arg_base_reg`, which the prologue
        /// initializes to the callee-entry SP when this proc has stack args.
        fn copyFromCallerStack(self: *Self, caller_base: GeneralReg, caller_offset: i32, local_offset: i32, num_regs: u8) Allocator.Error!void {
            const temp_reg: GeneralReg = scratch_reg;
            var ri: u8 = 0;
            while (ri < num_regs) : (ri += 1) {
                const off: i32 = @as(i32, ri) * 8;
                try self.emitLoad(.w64, temp_reg, caller_base, caller_offset + off);
                try self.emitStore(.w64, frame_ptr, local_offset + off, temp_reg);
            }
        }

        fn bindErasedCallableAdapterParams(self: *Self, params: LocalSpan) Allocator.Error!void {
            const locals = self.store.getLocalSpan(params);
            if (locals.len == 0) {
                if (builtin.mode == .Debug) {
                    std.debug.panic("Dev/codegen invariant violated: erased callable adapter has no hidden capture arg", .{});
                }
                unreachable;
            }

            const roc_ops_save_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64)
                .X20
            else
                .R12;
            const roc_ops_arg = self.getArgumentRegister(0);
            if (roc_ops_arg != roc_ops_save_reg) {
                try self.codegen.emit.movRegReg(.w64, roc_ops_save_reg, roc_ops_arg);
            }
            self.roc_ops_reg = roc_ops_save_reg;

            const args_ptr_slot = self.codegen.allocStackSlot(8);
            const capture_ptr_slot = self.codegen.allocStackSlot(8);
            try self.codegen.emitStoreStack(.w64, args_ptr_slot, self.getArgumentRegister(2));
            try self.codegen.emitStoreStack(.w64, capture_ptr_slot, self.getArgumentRegister(3));

            var arg_offset: u32 = 0;
            const explicit_count = locals.len - 1;
            for (locals[0..explicit_count]) |local| {
                const local_layout = self.localLayout(local);
                const runtime_layout = self.runtimeRepresentationLayoutIdx(local_layout);
                const size_align = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_layout));
                const arg_align: u32 = @intCast(@max(size_align.alignment.toByteUnits(), 1));
                arg_offset = std.mem.alignForward(u32, arg_offset, arg_align);
                if (size_align.size == 0) {
                    try self.local_locations.put(localKey(local), .{ .immediate_i64 = 0 });
                } else {
                    const local_offset = self.codegen.allocStackSlot(size_align.size);
                    const args_ptr_reg = try self.allocTempGeneral();
                    const temp_reg = try self.allocTempGeneral();
                    try self.emitLoad(.w64, args_ptr_reg, frame_ptr, args_ptr_slot);
                    try self.copyChunked(
                        temp_reg,
                        args_ptr_reg,
                        @intCast(arg_offset),
                        frame_ptr,
                        local_offset,
                        size_align.size,
                    );
                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(args_ptr_reg);
                    try self.local_locations.put(localKey(local), self.stackLocationForLayout(local_layout, local_offset));
                }
                arg_offset += size_align.size;
            }

            const capture_local = locals[explicit_count];
            const capture_stack = self.codegen.allocStackSlot(8);
            const capture_arg_reg = try self.allocTempGeneral();
            try self.emitLoad(.w64, capture_arg_reg, frame_ptr, capture_ptr_slot);
            if (self.enable_hot_reload) {
                try self.emitAddPtrImmAny(capture_arg_reg, capture_arg_reg, builtins.erased_callable.hot_reload_capture_prefix_size);
            }
            try self.emitStore(.w64, frame_ptr, capture_stack, capture_arg_reg);
            self.codegen.freeGeneral(capture_arg_reg);
            try self.local_locations.put(localKey(capture_local), self.stackLocationForLayout(.opaque_ptr, capture_stack));
        }

        fn bindProcParams(self: *Self, params: LocalSpan, initial_reg_idx: u8) Allocator.Error!void {
            const locals = self.store.getLocalSpan(params);

            const pbp_start = self.scratch_pass_by_ptr.top();
            defer self.scratch_pass_by_ptr.clearFrom(pbp_start);
            for (0..locals.len) |_| try self.scratch_pass_by_ptr.append(false);
            const param_pass_by_ptr = self.scratch_pass_by_ptr.sliceFromStart(pbp_start);

            const pnr_start = self.scratch_param_num_regs.top();
            defer self.scratch_param_num_regs.clearFrom(pnr_start);
            for (locals) |local| try self.scratch_param_num_regs.append(self.calcParamRegCount(self.localLayout(local)));
            const param_num_regs = self.scratch_param_num_regs.sliceFromStart(pnr_start);

            var pre_reg_count: u8 = initial_reg_idx;
            for (locals, 0..) |local, pi| {
                const nr = param_num_regs[pi];
                if (nr == 0) continue;

                const local_layout = self.localLayout(local);
                const runtime_layout_idx = self.runtimeRepresentationLayoutIdx(local_layout);
                const is_i128_param = runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec;
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

            // RocOps-threaded modes append RocOps as the final argument, so
            // their plan reserves one extra register slot; the symbol ABI
            // appends nothing. This must mirror the caller-side plan in
            // computePassByPtrPlan exactly.
            const ops_slots: u8 = if (self.generation_mode.threadsRocOps()) 1 else 0;
            while (pre_reg_count + ops_slots > max_arg_regs) {
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

                pre_reg_count = initial_reg_idx;
                for (locals, 0..) |local, pi| {
                    const pnr = param_num_regs[pi];
                    const pbp = param_pass_by_ptr[pi];
                    const runtime_layout_idx = self.runtimeRepresentationLayoutIdx(self.localLayout(local));
                    const is_i128_param = runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec;
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

            var reg_idx: u8 = initial_reg_idx;
            var stack_arg_offset: i32 = incoming_stack_arg_base_offset;

            for (locals, 0..) |local, param_idx| {
                const num_regs = param_num_regs[param_idx];

                if (num_regs == 0) {
                    try self.local_locations.put(localKey(local), .{ .immediate_i64 = 0 });
                    continue;
                }

                if (param_pass_by_ptr[param_idx]) {
                    var ptr_reg: GeneralReg = undefined;
                    if (reg_idx < max_arg_regs) {
                        ptr_reg = self.getArgumentRegister(reg_idx);
                        reg_idx += 1;
                    } else {
                        const caller_base = self.callerStackArgBaseReg();
                        ptr_reg = scratch_reg;
                        try self.emitLoad(.w64, ptr_reg, caller_base, stack_arg_offset);
                        stack_arg_offset += 8;
                        reg_idx = max_arg_regs;
                    }

                    const temp_reg: GeneralReg = if (ptr_reg == scratch_reg) ret_reg_0 else scratch_reg;
                    const size = self.getLayoutSize(self.localLayout(local));
                    const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                    try self.copyChunked(temp_reg, ptr_reg, 0, frame_ptr, stack_offset, size);
                    const stable_loc = self.stackLocationForLayout(self.localLayout(local), stack_offset);
                    try self.local_locations.put(localKey(local), stable_loc);
                    continue;
                }

                if (comptime target.toCpuArch() == .aarch64) {
                    const runtime_layout_idx = self.runtimeRepresentationLayoutIdx(self.localLayout(local));
                    if (num_regs == 2 and (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) and reg_idx % 2 != 0) {
                        reg_idx += 1;
                    }
                }

                if (reg_idx + num_regs <= max_arg_regs) {
                    const size: u32 = @as(u32, num_regs) * 8;
                    const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                    var ri: u8 = 0;
                    while (ri < num_regs) : (ri += 1) {
                        const arg_reg = self.getArgumentRegister(reg_idx + ri);
                        try self.codegen.emitStoreStack(.w64, stack_offset + @as(i32, ri) * 8, arg_reg);
                    }
                    const stable_loc = self.stackLocationForLayout(self.localLayout(local), stack_offset);
                    try self.local_locations.put(localKey(local), stable_loc);
                    reg_idx += num_regs;
                } else {
                    const caller_base = self.callerStackArgBaseReg();
                    const size: u32 = @as(u32, num_regs) * 8;
                    const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                    try self.copyFromCallerStack(caller_base, stack_arg_offset, stack_offset, num_regs);
                    const stable_loc = self.stackLocationForLayout(self.localLayout(local), stack_offset);
                    try self.local_locations.put(localKey(local), stable_loc);
                    stack_arg_offset += @as(i32, num_regs) * 8;
                    reg_idx = max_arg_regs;
                }
            }

            const roc_ops_save_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64)
                .X20
            else
                .R12;

            if (self.generation_mode.threadsRocOps()) {
                // RocOps-threaded modes append a real RocOps as the final
                // argument.
                if (reg_idx < max_arg_regs) {
                    const arg_reg = self.getArgumentRegister(reg_idx);
                    if (arg_reg != roc_ops_save_reg) {
                        try self.codegen.emit.movRegReg(.w64, roc_ops_save_reg, arg_reg);
                    }
                } else {
                    const caller_base = self.callerStackArgBaseReg();
                    try self.emitLoad(.w64, roc_ops_save_reg, caller_base, stack_arg_offset);
                }
            } else {
                // Symbol-ABI procs receive no RocOps. Builtins helper
                // signatures still carry an ops slot, which their extern
                // flavor ignores; feed those calls a null.
                try self.codegen.emitLoadImm(roc_ops_save_reg, 0);
            }

            self.roc_ops_reg = roc_ops_save_reg;
        }

        /// Move a value to the return register(s), using layout information for proper sizing.
        fn moveToReturnRegisterWithLayout(self: *Self, loc: ValueLocation, ret_layout: layout.Idx) Allocator.Error!void {
            if (loc == .noreturn) return;

            const ls = self.layout_store;
            const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
            const layout_val = ls.getLayout(runtime_ret_layout);

            if (builtin.mode == .Debug and loc == .stack_str and !(layout_val.tag == .scalar and layout_val.getScalar().tag == .str) and layout_val.tag != .list and layout_val.tag != .list_of_zst) {
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
                    const scalar = layout_val.getScalar();
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
                            const precision = scalar.getFrac();
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
                            const precision = scalar.getInt();
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
                        .opaque_ptr => try self.moveOneRegToReturn(loc),
                    }
                },
                // Structs and tag unions: size determines register count
                .struct_, .tag_union => {
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
                .box, .box_of_zst, .erased_callable, .ptr => try self.moveOneRegToReturn(loc),
                .closure => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "LIR/codegen invariant violated: runtimeRepresentationLayoutIdx returned closure for return layout {}",
                            .{@intFromEnum(ret_layout)},
                        );
                    }
                    unreachable;
                },
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
            const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
            const layout_val = ls.getLayout(runtime_ret_layout);
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
        // Per-switch state shared across the explicit-stack continuations that
        // emit a multi-arm switch. Heap-allocated so every continuation that
        // touches the same switch references one instance; freed by `switch_end`.
        const SwitchState = struct {
            owner: CFStmtId,
            cond_reg: GeneralReg,
            switch_env: StmtEnvSnapshot,
            end_patches: std.ArrayList(usize),
            branches: []const lir.CFSwitchBranch,
            default_branch: CFStmtId,
            index: usize,
        };
        // Per-switch state for the single-arm (boolean) switch shape.
        const SwitchState1 = struct {
            owner: CFStmtId,
            switch_env: StmtEnvSnapshot,
            else_patch: usize,
            default_branch: CFStmtId,
            end_patch: usize,
        };
        const StrMatchState = struct {
            owner: CFStmtId,
            before_env: StmtEnvSnapshot,
            miss_patches: std.ArrayList(usize),
            on_miss: CFStmtId,
            end_patch: usize,
        };
        const StrMatchSourceRegs = struct {
            bytes: GeneralReg,
            len: GeneralReg,
            is_small: GeneralReg,
            allocation: GeneralReg,
        };
        const StrMatchSetState = struct {
            owner: CFStmtId,
            before_env: StmtEnvSnapshot,
            source: StrMatchSourceRegs,
            arms: []const LIR.StrMatchArm,
            on_miss: CFStmtId,
            index: usize,
            miss_patches: std.ArrayList(usize),
            end_patches: std.ArrayList(usize),
        };
        // Work item for the explicit statement-generation stack. `node` generates
        // one statement; the remaining variants are continuations that emit the
        // glue code which originally lived after a recursive descent.
        const StmtWork = union(enum) {
            node: CFStmtId,
            join_body: struct { owner: CFStmtId, jp_key: u32, body: CFStmtId },
            join_patch: struct { owner: CFStmtId, jp_key: u32, skip_patch: usize, join_location: usize },
            switch_branch: *SwitchState,
            switch_branch_after: struct { state: *SwitchState, skip_patch: usize },
            switch_default: *SwitchState,
            switch_end: *SwitchState,
            switch1_after_branch: *SwitchState1,
            switch1_end: *SwitchState1,
            str_match_after_match: *StrMatchState,
            str_match_end: *StrMatchState,
            str_match_set_arm: *StrMatchSetState,
            str_match_set_after_arm: *StrMatchSetState,
            str_match_set_miss: *StrMatchSetState,
            str_match_set_end: *StrMatchSetState,
        };

        /// Generate code for a control flow statement and everything reachable
        /// from it. Uses an explicit work stack so deeply nested control flow and
        /// long statement chains never grow the native call stack.
        fn generateStmt(self: *Self, root_stmt_id: CFStmtId) Allocator.Error!void {
            const saved_outer_stmt_id = self.current_stmt_id;
            defer self.current_stmt_id = saved_outer_stmt_id;

            // Most control-flow graphs are shallow, so keep the work stack in a
            // small on-stack buffer and only spill to the heap for deep nesting.
            var sfa = std.heap.stackFallback(64 * @sizeOf(StmtWork), self.allocator);
            const wa = sfa.get();
            var work = std.ArrayList(StmtWork).empty;
            defer work.deinit(wa);
            try work.append(wa, .{ .node = root_stmt_id });

            while (work.pop()) |item| switch (item) {
                .node => |stmt_id| {
                    const stmt_key = @intFromEnum(stmt_id);
                    if (self.stmt_locations.get(stmt_key)) |stmt_location| {
                        const patch = try self.codegen.emitJump();
                        self.codegen.patchJump(patch, stmt_location);
                        continue;
                    }
                    try self.stmt_locations.put(stmt_key, self.codegen.currentOffset());

                    const stmt_loc = self.store.stmtLoc(stmt_id);
                    if (stmt_loc.hasLocation()) {
                        try self.line_entries.append(self.allocator, .{
                            .offset = @intCast(self.codegen.currentOffset()),
                            .loc = stmt_loc,
                        });
                    }

                    self.current_stmt_id = stmt_id;

                    const stmt = self.store.getCFStmt(stmt_id);
                    switch (stmt) {
                        .assign_ref => |assign| {
                            const value_loc = try self.generateRefOp(assign.op, self.localLayout(assign.target));
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .assign_literal => |assign| {
                            const value_loc: ValueLocation = switch (assign.value) {
                                .i64_literal => |lit| .{ .immediate_i64 = lit.value },
                                .i128_literal => |lit| try self.generateI128Literal(lit.value),
                                .f64_literal => |lit| .{ .immediate_f64 = lit },
                                .f32_literal => |lit| .{ .immediate_f64 = @floatCast(lit) },
                                .dec_literal => |lit| try self.generateI128Literal(lit),
                                .str_literal => |str_idx| try self.generateStrLiteral(str_idx),
                                .bytes_literal => |bytes_idx| try self.generateBytesLiteral(bytes_idx),
                                .null_ptr => .{ .immediate_i64 = 0 },
                                .proc_ref => |proc_id| blk: {
                                    const proc = self.proc_registry.get(@intFromEnum(proc_id)) orelse unreachable;
                                    const reg = try self.allocTempGeneral();
                                    if (proc.code_start == unresolved_proc_code_start)
                                        try self.emitPendingProcAddress(proc_id, reg)
                                    else
                                        try self.emitInternalCodeAddress(proc.code_start, reg);
                                    break :blk .{ .general_reg = reg };
                                },
                            };
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .init_uninitialized => |uninit| {
                            try self.poisonUninitializedLocal(uninit.target);
                            try work.append(wa, .{ .node = uninit.next });
                        },

                        .assign_call => |assign| {
                            const value_loc = try self.generateCall(.{
                                .proc = assign.proc,
                                .args = assign.args,
                                .ret_layout = self.localLayout(assign.target),
                            });
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .assign_call_erased => |assign| {
                            const value_loc = try self.generateErasedCall(
                                assign.closure,
                                assign.args,
                                self.localLayout(assign.target),
                            );
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .assign_packed_erased_fn => |assign| {
                            const value_loc = try self.generatePackedErasedFn(
                                assign.proc,
                                assign.capture,
                                self.localLayout(assign.target),
                                assign.capture_layout,
                                assign.on_drop,
                            );
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .assign_low_level => |assign| {
                            const value_loc = try self.generateLowLevel(.{
                                .op = assign.op,
                                .args = assign.args,
                                .ret_layout = self.localLayout(assign.target),
                                .unique_args = assign.unique_args,
                                .interchangeable = assign.interchangeable,
                            });
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .assign_list => |assign| {
                            const value_loc = try self.generateList(.{
                                .elems = assign.elems,
                                .target_layout = self.localLayout(assign.target),
                            });
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .assign_struct => |assign| {
                            const value_loc = try self.generateStruct(.{
                                .fields = assign.fields,
                                .target_layout = self.localLayout(assign.target),
                            });
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .assign_tag => |assign| {
                            const value_loc = try self.generateTag(.{
                                .target_layout = self.localLayout(assign.target),
                                .variant_index = assign.variant_index,
                                .discriminant = assign.discriminant,
                                .payload = assign.payload,
                            });
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .set_local => |assign| {
                            const value_loc = try self.emitValueLocal(assign.value);
                            try self.bindAssignedLocal(assign.target, value_loc);
                            try work.append(wa, .{ .node = assign.next });
                        },

                        .debug => |debug_stmt| {
                            const msg_loc = try self.emitValueLocal(debug_stmt.message);
                            const msg_offset = switch (msg_loc) {
                                .stack_str => |offset| offset,
                                else => std.debug.panic(
                                    "Dev/codegen invariant violated: debug message local {d} did not lower to a RocStr stack value",
                                    .{@intFromEnum(debug_stmt.message)},
                                ),
                            };
                            try self.emitRocDbgFromStackStr(msg_offset);
                            try work.append(wa, .{ .node = debug_stmt.next });
                        },

                        .expect => |expect_stmt| {
                            const cond_loc = try self.emitValueLocal(expect_stmt.condition);
                            const cond_reg = try self.ensureInGeneralReg(cond_loc);
                            try self.emitCmpImm(cond_reg, 0);
                            const skip_patch = try self.emitJumpIfNotEqual();
                            try self.emitRocExpectFailed();
                            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
                            try work.append(wa, .{ .node = expect_stmt.next });
                        },

                        .expect_err => |expect_err_stmt| {
                            const msg_loc = try self.emitValueLocal(expect_err_stmt.message);
                            const msg_offset = switch (msg_loc) {
                                .stack_str => |offset| offset,
                                else => std.debug.panic(
                                    "Dev/codegen invariant violated: expect_err message local {d} did not lower to a RocStr stack value",
                                    .{@intFromEnum(expect_err_stmt.message)},
                                ),
                            };
                            try self.emitRocExpectErrFromStackStr(msg_offset, expect_err_stmt.region);
                            try self.emitTrap();
                        },

                        .runtime_error => {
                            try self.emitRocCrash("hit a runtime error");
                            try self.emitTrap();
                        },

                        .comptime_exhaustiveness_failed => |marker| {
                            if (self.comptime_hooks) |hooks| {
                                try self.emitComptimeExhaustivenessFailed(hooks, marker.site);
                            } else {
                                try self.emitRocCrash("compile-time exhaustiveness failure reached runtime code");
                            }
                            try self.emitTrap();
                        },

                        .comptime_branch_taken => |marker| {
                            if (self.comptime_hooks) |hooks| {
                                try self.emitComptimeBranchTaken(hooks, marker.site, marker.branch_index);
                            }
                            try work.append(wa, .{ .node = marker.next });
                        },

                        .join => |j| {
                            const jp_key = @intFromEnum(j.id);
                            try self.setupJoinPointParams(j.id, j.params);
                            if (!self.join_point_jumps.contains(jp_key)) {
                                try self.join_point_jumps.put(jp_key, std.ArrayList(JumpRecord).empty);
                            }
                            try self.ensureStableLocationsForStmtReads(j.body);
                            // Emit the remainder first, then (via join_body) the join body,
                            // matching the original recursive order.
                            try work.append(wa, .{ .join_body = .{ .owner = stmt_id, .jp_key = jp_key, .body = j.body } });
                            try work.append(wa, .{ .node = j.remainder });
                        },

                        .jump => |jmp| {
                            const jump_location = try self.emitJumpPlaceholder();

                            const jp_key = @intFromEnum(jmp.target);
                            if (self.join_point_jumps.getPtr(jp_key)) |jumps| {
                                try jumps.append(self.allocator, .{ .location = jump_location });
                            }
                        },

                        .ret => |r| {
                            const value_loc = try self.emitValueLocal(r.value);
                            if (value_loc == .noreturn) return;
                            const value_layout = self.valueLayout(r.value);
                            const ret_layout = self.early_return_ret_layout orelse value_layout;
                            if (builtin.mode == .Debug and ret_layout != value_layout) {
                                std.debug.panic(
                                    "Dev/codegen invariant violated: proc return local layout {} did not match proc ret_layout {} at stmt {d}",
                                    .{
                                        @intFromEnum(value_layout),
                                        @intFromEnum(ret_layout),
                                        if (self.current_stmt_id) |current_stmt_id| @intFromEnum(current_stmt_id) else std.math.maxInt(u32),
                                    },
                                );
                            }
                            const preserved_return_loc = self.requireExactValueLocationToLayout(value_loc, value_layout, ret_layout, "ret");

                            if (self.ret_ptr_slot) |ret_slot| {
                                try self.copyResultToReturnPointer(preserved_return_loc, ret_layout, ret_slot);
                            } else {
                                try self.moveToReturnRegisterWithLayout(preserved_return_loc, ret_layout);
                            }
                            const patch = try self.codegen.emitJump();
                            try self.early_return_patches.append(self.allocator, patch);
                        },

                        .switch_stmt => |sw| {
                            // Evaluate condition
                            const cond_loc = try self.emitValueLocal(sw.cond);
                            const cond_reg = try self.ensureInGeneralReg(cond_loc);

                            const branches = self.store.getCFSwitchBranches(sw.branches);
                            const switch_env = try self.captureStmtEnv();

                            if (branches.len == 1) {
                                // Single branch (bool switch): compare and branch
                                const branch = branches[0];
                                try self.emitCmpImm(cond_reg, @bitCast(branch.value));
                                const else_patch = try self.emitJumpIfNotEqual();
                                self.codegen.freeGeneral(cond_reg);
                                try self.restoreStmtEnv(&switch_env);

                                const state = try self.allocator.create(SwitchState1);
                                state.* = .{
                                    .owner = stmt_id,
                                    .switch_env = switch_env,
                                    .else_patch = else_patch,
                                    .default_branch = sw.default_branch,
                                    .end_patch = 0,
                                };
                                try work.append(wa, .{ .switch1_after_branch = state });
                                try work.append(wa, .{ .node = branch.body });
                            } else {
                                const state = try self.allocator.create(SwitchState);
                                state.* = .{
                                    .owner = stmt_id,
                                    .cond_reg = cond_reg,
                                    .switch_env = switch_env,
                                    .end_patches = std.ArrayList(usize).empty,
                                    .branches = branches,
                                    .default_branch = sw.default_branch,
                                    .index = 0,
                                };
                                try work.append(wa, .{ .switch_branch = state });
                            }
                        },

                        .switch_initialized_payload => |sw| {
                            const cond_loc = try self.emitValueLocal(sw.cond);
                            const cond_reg = try self.ensureInGeneralReg(cond_loc);
                            const switch_env = try self.captureStmtEnv();

                            const cond_mask: i64 = @bitCast(sw.cond_mask);
                            const compare_reg = try self.allocTempGeneral();
                            try self.emitMovRegReg(compare_reg, cond_reg);
                            const mask_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(mask_reg, cond_mask);
                            try self.codegen.emitAnd(.w64, compare_reg, compare_reg, mask_reg);
                            self.codegen.freeGeneral(mask_reg);
                            try self.emitCmpImm(compare_reg, cond_mask);
                            const else_patch = try self.emitJumpIfNotEqual();
                            self.codegen.freeGeneral(compare_reg);
                            self.codegen.freeGeneral(cond_reg);
                            try self.restoreStmtEnv(&switch_env);

                            const state = try self.allocator.create(SwitchState1);
                            state.* = .{
                                .owner = stmt_id,
                                .switch_env = switch_env,
                                .else_patch = else_patch,
                                .default_branch = sw.uninitialized_branch,
                                .end_patch = 0,
                            };
                            try work.append(wa, .{ .switch1_after_branch = state });
                            try work.append(wa, .{ .node = sw.initialized_branch });
                        },

                        .str_match => |str_match| {
                            const before_env = try self.captureStmtEnv();
                            const miss_patches = try self.generateStrMatch(str_match);
                            const state = try self.allocator.create(StrMatchState);
                            state.* = .{
                                .owner = stmt_id,
                                .before_env = before_env,
                                .miss_patches = miss_patches,
                                .on_miss = str_match.on_miss,
                                .end_patch = 0,
                            };
                            try work.append(wa, .{ .str_match_after_match = state });
                            try work.append(wa, .{ .node = str_match.on_match });
                        },

                        .str_match_set => |str_match_set| {
                            const arms = self.store.getStrMatchArms(str_match_set.arms);
                            const before_env = try self.captureStmtEnv();
                            const source = try self.emitStrMatchSourceRegs(str_match_set.source);
                            const state = try self.allocator.create(StrMatchSetState);
                            state.* = .{
                                .owner = stmt_id,
                                .before_env = before_env,
                                .source = source,
                                .arms = arms,
                                .on_miss = str_match_set.on_miss,
                                .index = 0,
                                .miss_patches = std.ArrayList(usize).empty,
                                .end_patches = std.ArrayList(usize).empty,
                            };
                            try work.append(wa, .{ .str_match_set_arm = state });
                        },

                        .incref => |inc| {
                            _ = try self.generateIncref(.{
                                .value = inc.value,
                                .rc = inc.rc,
                                .count = inc.count,
                                .atomicity = inc.atomicity,
                            });
                            try work.append(wa, .{ .node = inc.next });
                        },

                        .decref => |dec| {
                            _ = try self.generateDecref(.{
                                .value = dec.value,
                                .rc = dec.rc,
                                .atomicity = dec.atomicity,
                            });
                            try work.append(wa, .{ .node = dec.next });
                        },

                        .decref_if_initialized => |dec| {
                            const cond_loc = try self.emitValueLocal(dec.cond);
                            const cond_reg = try self.ensureInGeneralReg(cond_loc);
                            const cond_mask: i64 = @bitCast(dec.cond_mask);
                            const compare_reg = try self.allocTempGeneral();
                            try self.emitMovRegReg(compare_reg, cond_reg);
                            const mask_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(mask_reg, cond_mask);
                            try self.codegen.emitAnd(.w64, compare_reg, compare_reg, mask_reg);
                            self.codegen.freeGeneral(mask_reg);
                            try self.emitCmpImm(compare_reg, cond_mask);
                            const skip_patch = try self.emitJumpIfNotEqual();
                            self.codegen.freeGeneral(compare_reg);
                            self.codegen.freeGeneral(cond_reg);

                            _ = try self.generateDecref(.{
                                .value = dec.value,
                                .rc = dec.rc,
                                .atomicity = dec.atomicity,
                            });
                            const done_patch = try self.codegen.emitJump();
                            const successor_offset = self.codegen.currentOffset();
                            self.codegen.patchJump(skip_patch, successor_offset);
                            self.codegen.patchJump(done_patch, successor_offset);
                            try work.append(wa, .{ .node = dec.next });
                        },

                        .free => |free_stmt| {
                            _ = try self.generateFree(.{
                                .value = free_stmt.value,
                                .rc = free_stmt.rc,
                                .atomicity = free_stmt.atomicity,
                            });
                            try work.append(wa, .{ .node = free_stmt.next });
                        },

                        .crash => |crash| {
                            try self.emitRocCrash(self.store.getString(crash.msg));
                            try self.emitTrap();
                        },

                        .loop_continue => {
                            if (builtin.mode == .Debug and self.loop_continue_targets.items.len == 0) {
                                std.debug.panic(
                                    "Dev/codegen invariant violated: loop_continue encountered outside a loop",
                                    .{},
                                );
                            }
                            const loop_target = self.loop_continue_targets.items[self.loop_continue_targets.items.len - 1];
                            const patch = try self.codegen.emitJump();
                            self.codegen.patchJump(patch, loop_target);
                        },

                        .loop_break => {
                            if (builtin.mode == .Debug and self.loop_break_patch_starts.items.len == 0) {
                                std.debug.panic(
                                    "Dev/codegen invariant violated: loop_break encountered outside a loop",
                                    .{},
                                );
                            }
                            try self.loop_break_patches.append(self.allocator, try self.emitJumpPlaceholder());
                        },
                    }
                },

                .join_body => |c| {
                    self.current_stmt_id = c.owner;
                    const skip_join_body_patch = try self.codegen.emitJump();
                    const join_location = self.codegen.currentOffset();
                    try self.join_points.put(c.jp_key, join_location);
                    try work.append(wa, .{ .join_patch = .{
                        .owner = c.owner,
                        .jp_key = c.jp_key,
                        .skip_patch = skip_join_body_patch,
                        .join_location = join_location,
                    } });
                    try work.append(wa, .{ .node = c.body });
                },

                .join_patch => |c| {
                    self.current_stmt_id = c.owner;
                    self.codegen.patchJump(c.skip_patch, self.codegen.currentOffset());
                    if (self.join_point_jumps.get(c.jp_key)) |jumps| {
                        for (jumps.items) |jump_record| {
                            self.codegen.patchJump(jump_record.location, c.join_location);
                        }
                    }
                },

                .switch_branch => |state| {
                    self.current_stmt_id = state.owner;
                    const branch = state.branches[state.index];
                    try self.emitCmpImm(state.cond_reg, @bitCast(branch.value));
                    const skip_patch = try self.emitJumpIfNotEqual();
                    try self.restoreStmtEnv(&state.switch_env);
                    try work.append(wa, .{ .switch_branch_after = .{ .state = state, .skip_patch = skip_patch } });
                    try work.append(wa, .{ .node = branch.body });
                },

                .switch_branch_after => |c| {
                    const state = c.state;
                    self.current_stmt_id = state.owner;
                    const end_patch = try self.codegen.emitJump();
                    try state.end_patches.append(self.allocator, end_patch);
                    self.codegen.patchJump(c.skip_patch, self.codegen.currentOffset());
                    state.index += 1;
                    if (state.index < state.branches.len) {
                        try work.append(wa, .{ .switch_branch = state });
                    } else {
                        try work.append(wa, .{ .switch_default = state });
                    }
                },

                .switch_default => |state| {
                    self.current_stmt_id = state.owner;
                    self.codegen.freeGeneral(state.cond_reg);
                    try self.restoreStmtEnv(&state.switch_env);
                    try work.append(wa, .{ .switch_end = state });
                    try work.append(wa, .{ .node = state.default_branch });
                },

                .switch_end => |state| {
                    self.current_stmt_id = state.owner;
                    const end_offset = self.codegen.currentOffset();
                    for (state.end_patches.items) |patch| {
                        self.codegen.patchJump(patch, end_offset);
                    }
                    try self.restoreStmtEnv(&state.switch_env);
                    state.switch_env.deinit();
                    state.end_patches.deinit(self.allocator);
                    self.allocator.destroy(state);
                },

                .switch1_after_branch => |state| {
                    self.current_stmt_id = state.owner;
                    state.end_patch = try self.codegen.emitJump();
                    self.codegen.patchJump(state.else_patch, self.codegen.currentOffset());
                    try self.restoreStmtEnv(&state.switch_env);
                    try work.append(wa, .{ .switch1_end = state });
                    try work.append(wa, .{ .node = state.default_branch });
                },

                .switch1_end => |state| {
                    self.current_stmt_id = state.owner;
                    self.codegen.patchJump(state.end_patch, self.codegen.currentOffset());
                    try self.restoreStmtEnv(&state.switch_env);
                    state.switch_env.deinit();
                    self.allocator.destroy(state);
                },

                .str_match_after_match => |state| {
                    self.current_stmt_id = state.owner;
                    state.end_patch = try self.codegen.emitJump();
                    const miss_offset = self.codegen.currentOffset();
                    for (state.miss_patches.items) |patch| {
                        self.codegen.patchJump(patch, miss_offset);
                    }
                    try self.restoreStmtEnv(&state.before_env);
                    try work.append(wa, .{ .str_match_end = state });
                    try work.append(wa, .{ .node = state.on_miss });
                },

                .str_match_end => |state| {
                    self.current_stmt_id = state.owner;
                    self.codegen.patchJump(state.end_patch, self.codegen.currentOffset());
                    try self.restoreStmtEnv(&state.before_env);
                    state.before_env.deinit();
                    state.miss_patches.deinit(self.allocator);
                    self.allocator.destroy(state);
                },

                .str_match_set_arm => |state| {
                    self.current_stmt_id = state.owner;
                    try self.restoreStmtEnv(&state.before_env);
                    if (builtin.mode == .Debug and state.index >= state.arms.len) {
                        std.debug.panic("Dev/codegen invariant violated: string-match-set arm index exceeded arm count", .{});
                    }
                    state.miss_patches = try self.generateStrMatchWithSource(state.arms[state.index], state.source);
                    try work.append(wa, .{ .str_match_set_after_arm = state });
                    try work.append(wa, .{ .node = state.arms[state.index].on_match });
                },

                .str_match_set_after_arm => |state| {
                    self.current_stmt_id = state.owner;
                    try state.end_patches.append(self.allocator, try self.codegen.emitJump());
                    const next_arm_offset = self.codegen.currentOffset();
                    for (state.miss_patches.items) |patch| {
                        self.codegen.patchJump(patch, next_arm_offset);
                    }
                    state.miss_patches.deinit(self.allocator);
                    state.miss_patches = std.ArrayList(usize).empty;
                    state.index += 1;
                    try self.restoreStmtEnv(&state.before_env);
                    if (state.index < state.arms.len) {
                        try work.append(wa, .{ .str_match_set_arm = state });
                    } else {
                        try work.append(wa, .{ .str_match_set_miss = state });
                    }
                },

                .str_match_set_miss => |state| {
                    self.current_stmt_id = state.owner;
                    try self.restoreStmtEnv(&state.before_env);
                    try work.append(wa, .{ .str_match_set_end = state });
                    try work.append(wa, .{ .node = state.on_miss });
                },

                .str_match_set_end => |state| {
                    self.current_stmt_id = state.owner;
                    const end_offset = self.codegen.currentOffset();
                    for (state.end_patches.items) |patch| {
                        self.codegen.patchJump(patch, end_offset);
                    }
                    try self.restoreStmtEnv(&state.before_env);
                    state.before_env.deinit();
                    state.miss_patches.deinit(self.allocator);
                    state.end_patches.deinit(self.allocator);
                    self.codegen.freeGeneral(state.source.allocation);
                    self.codegen.freeGeneral(state.source.is_small);
                    self.codegen.freeGeneral(state.source.len);
                    self.codegen.freeGeneral(state.source.bytes);
                    self.allocator.destroy(state);
                },
            };
        }

        fn generateStrMatch(self: *Self, str_match: anytype) Allocator.Error!std.ArrayList(usize) {
            const source = try self.emitStrMatchSourceRegs(str_match.source);
            defer self.codegen.freeGeneral(source.allocation);
            defer self.codegen.freeGeneral(source.is_small);
            defer self.codegen.freeGeneral(source.len);
            defer self.codegen.freeGeneral(source.bytes);

            return try self.generateStrMatchWithSource(str_match, source);
        }

        fn emitStrMatchSourceRegs(self: *Self, source: LIR.LocalId) Allocator.Error!StrMatchSourceRegs {
            const source_loc = try self.emitValueLocal(source);
            const source_offset = switch (source_loc) {
                .stack_str => |offset| offset,
                else => std.debug.panic(
                    "LIR/codegen invariant violated: str_match source local {d} did not lower to a RocStr stack value",
                    .{@intFromEnum(source)},
                ),
            };

            const regs = StrMatchSourceRegs{
                .bytes = try self.allocTempGeneral(),
                .len = try self.allocTempGeneral(),
                .is_small = try self.allocTempGeneral(),
                .allocation = try self.allocTempGeneral(),
            };

            try self.emitLoadStrSourceShape(source_offset, regs.bytes, regs.len, regs.is_small, regs.allocation);
            return regs;
        }

        fn generateStrMatchWithSource(self: *Self, str_match: anytype, source: StrMatchSourceRegs) Allocator.Error!std.ArrayList(usize) {
            var miss_patches = std.ArrayList(usize).empty;
            errdefer miss_patches.deinit(self.allocator);

            const steps = self.store.getStrMatchSteps(str_match.steps);
            var capture_offsets = std.ArrayList(?i32).empty;
            defer capture_offsets.deinit(self.allocator);
            try capture_offsets.ensureTotalCapacity(self.allocator, steps.len);
            for (steps) |step| {
                switch (step.capture) {
                    .discard => try capture_offsets.append(self.allocator, null),
                    .view => try capture_offsets.append(self.allocator, self.codegen.allocStackSlot(roc_str_size)),
                }
            }

            const cursor_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cursor_reg);
            try self.codegen.emitLoadImm(cursor_reg, 0);

            const prefix = self.store.getStringLiteral(str_match.prefix);
            if (prefix.len > 0) {
                try self.emitCheckBytesAvailable(cursor_reg, source.len, prefix.len, &miss_patches);
                try self.emitCompareLiteralAtPtr(source.bytes, prefix, &miss_patches);
                try self.emitAddUsizeImm(cursor_reg, cursor_reg, prefix.len);
            }

            for (steps, 0..) |step, step_i| {
                const capture_start_reg = try self.allocTempGeneral();
                defer self.codegen.freeGeneral(capture_start_reg);
                try self.emitMovRegReg(capture_start_reg, cursor_reg);

                const delimiter = self.store.getStringLiteral(step.delimiter);
                const is_final_tail_capture = str_match.end == .tail and step_i + 1 == steps.len and delimiter.len == 0;

                if (is_final_tail_capture) {
                    if (capture_offsets.items[step_i]) |capture_offset| {
                        try self.emitStoreStrCapture(capture_offset, source.bytes, source.allocation, source.is_small, capture_start_reg, source.len);
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| {
                                const loc = ValueLocation{ .stack_str = capture_offset };
                                try self.local_locations.put(localKey(local), loc);
                                try self.emitDebugAssertValidStrLocal(local, loc);
                            },
                        }
                    }
                    try self.emitMovRegReg(cursor_reg, source.len);
                    continue;
                }

                try self.emitFindDelimiter(source.bytes, source.len, cursor_reg, delimiter, &miss_patches);

                if (capture_offsets.items[step_i]) |capture_offset| {
                    try self.emitStoreStrCapture(capture_offset, source.bytes, source.allocation, source.is_small, capture_start_reg, cursor_reg);
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| {
                            const loc = ValueLocation{ .stack_str = capture_offset };
                            try self.local_locations.put(localKey(local), loc);
                            try self.emitDebugAssertValidStrLocal(local, loc);
                        },
                    }
                }

                if (delimiter.len > 0) {
                    try self.emitAddUsizeImm(cursor_reg, cursor_reg, delimiter.len);
                }
            }

            switch (str_match.end) {
                .exact => {
                    try self.emitCmpReg(cursor_reg, source.len);
                    try miss_patches.append(self.allocator, try self.codegen.emitCondJump(condNotEqual()));
                },
                .tail => {},
            }

            return miss_patches;
        }

        fn emitLoadStrSourceShape(
            self: *Self,
            source_offset: i32,
            bytes_reg: GeneralReg,
            len_reg: GeneralReg,
            source_is_small_reg: GeneralReg,
            source_alloc_reg: GeneralReg,
        ) Allocator.Error!void {
            const tmp_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(tmp_reg);

            try self.emitLoad(.w64, len_reg, frame_ptr, source_offset + 16);
            try self.codegen.emitLoadImm(tmp_reg, std.math.minInt(i64));
            try self.emitAndRegs(.w64, tmp_reg, tmp_reg, len_reg);
            try self.emitCmpImm(tmp_reg, 0);
            const small_patch = try self.codegen.emitCondJump(condNotEqual());

            try self.emitLoad(.w64, bytes_reg, frame_ptr, source_offset);
            try self.codegen.emitLoadImm(source_is_small_reg, 0);
            try self.emitLoad(.w64, source_alloc_reg, frame_ptr, source_offset + 8);
            try self.codegen.emitLoadImm(tmp_reg, 1);
            try self.emitAndRegs(.w64, tmp_reg, tmp_reg, source_alloc_reg);
            try self.emitCmpImm(tmp_reg, 0);
            const already_slice_patch = try self.codegen.emitCondJump(condNotEqual());
            try self.emitMovRegReg(source_alloc_reg, bytes_reg);
            try self.codegen.emitLoadImm(tmp_reg, 1);
            try self.emitOrRegs(.w64, source_alloc_reg, source_alloc_reg, tmp_reg);
            const alloc_done_patch = try self.codegen.emitJump();
            const already_slice_offset = self.codegen.currentOffset();
            self.codegen.patchJump(already_slice_patch, already_slice_offset);
            const alloc_done_offset = self.codegen.currentOffset();
            self.codegen.patchJump(alloc_done_patch, alloc_done_offset);
            const shape_done_patch = try self.codegen.emitJump();

            const small_offset = self.codegen.currentOffset();
            self.codegen.patchJump(small_patch, small_offset);
            try self.emitLeaStack(bytes_reg, source_offset);
            try self.codegen.emitLoadImm(source_is_small_reg, 1);
            try self.emitLsrImm(.w64, len_reg, len_reg, 56);
            try self.codegen.emitLoadImm(tmp_reg, 0x7F);
            try self.emitAndRegs(.w64, len_reg, len_reg, tmp_reg);
            try self.codegen.emitLoadImm(source_alloc_reg, 0);

            const shape_done_offset = self.codegen.currentOffset();
            self.codegen.patchJump(shape_done_patch, shape_done_offset);
        }

        fn emitCheckBytesAvailable(
            self: *Self,
            cursor_reg: GeneralReg,
            len_reg: GeneralReg,
            needed: usize,
            miss_patches: *std.ArrayList(usize),
        ) Allocator.Error!void {
            if (needed == 0) return;

            const needed_reg = try self.allocTempGeneral();
            const limit_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(limit_reg);
            defer self.codegen.freeGeneral(needed_reg);

            try self.codegen.emitLoadImm(needed_reg, @bitCast(@as(u64, @intCast(needed))));
            try self.emitCmpReg(len_reg, needed_reg);
            try miss_patches.append(self.allocator, try self.codegen.emitCondJump(condBelow()));

            try self.emitMovRegReg(limit_reg, len_reg);
            try self.emitSubRegs(.w64, limit_reg, limit_reg, needed_reg);
            try self.emitCmpReg(cursor_reg, limit_reg);
            try miss_patches.append(self.allocator, try self.codegen.emitCondJump(condAbove()));
        }

        fn emitFindDelimiter(
            self: *Self,
            bytes_reg: GeneralReg,
            len_reg: GeneralReg,
            cursor_reg: GeneralReg,
            delimiter: []const u8,
            miss_patches: *std.ArrayList(usize),
        ) Allocator.Error!void {
            if (delimiter.len == 0) return;

            const limit_reg = try self.allocTempGeneral();
            const candidate_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(candidate_reg);
            defer self.codegen.freeGeneral(limit_reg);

            {
                const needed_reg = try self.allocTempGeneral();
                defer self.codegen.freeGeneral(needed_reg);

                try self.codegen.emitLoadImm(needed_reg, @bitCast(@as(u64, @intCast(delimiter.len))));
                try self.emitCmpReg(len_reg, needed_reg);
                try miss_patches.append(self.allocator, try self.codegen.emitCondJump(condBelow()));

                try self.emitMovRegReg(limit_reg, len_reg);
                try self.emitSubRegs(.w64, limit_reg, limit_reg, needed_reg);
            }

            const loop_start = self.codegen.currentOffset();
            try self.emitCmpReg(cursor_reg, limit_reg);
            try miss_patches.append(self.allocator, try self.codegen.emitCondJump(condAbove()));

            try self.emitMovRegReg(candidate_reg, bytes_reg);
            try self.emitAddRegs(.w64, candidate_reg, candidate_reg, cursor_reg);

            const retry_patch = blk: {
                const loaded_reg = try self.allocTempGeneral();
                defer self.codegen.freeGeneral(loaded_reg);

                try self.emitLoadW8(loaded_reg, candidate_reg, 0);
                try self.emitCmpImm(loaded_reg, delimiter[0]);
                break :blk try self.codegen.emitCondJump(condNotEqual());
            };

            if (delimiter.len > 1) {
                try self.emitCompareLiteralAtPtr(candidate_reg, delimiter, miss_patches);
            }

            const found_patch = try self.codegen.emitJump();

            const retry_offset = self.codegen.currentOffset();
            self.codegen.patchJump(retry_patch, retry_offset);
            try self.emitAddUsizeImm(cursor_reg, cursor_reg, 1);
            const back_patch = try self.codegen.emitJump();
            self.codegen.patchJump(back_patch, loop_start);

            const found_offset = self.codegen.currentOffset();
            self.codegen.patchJump(found_patch, found_offset);
        }

        fn emitCompareLiteralAtPtr(
            self: *Self,
            ptr_reg: GeneralReg,
            literal: []const u8,
            mismatch_patches: *std.ArrayList(usize),
        ) Allocator.Error!void {
            if (literal.len == 0) return;

            const loaded_reg = try self.allocTempGeneral();
            const expected_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(expected_reg);
            defer self.codegen.freeGeneral(loaded_reg);

            var offset: usize = 0;
            while (offset < literal.len) {
                const remaining = literal.len - offset;
                const chunk_len: usize = if (remaining >= 8)
                    8
                else if (remaining >= 4)
                    4
                else if (remaining >= 2)
                    2
                else
                    1;

                const offset_i32: i32 = @intCast(offset);
                switch (chunk_len) {
                    8 => try self.emitLoad(.w64, loaded_reg, ptr_reg, offset_i32),
                    4 => try self.emitLoad(.w32, loaded_reg, ptr_reg, offset_i32),
                    2 => try self.emitLoadW16(loaded_reg, ptr_reg, offset_i32),
                    1 => try self.emitLoadW8(loaded_reg, ptr_reg, offset_i32),
                    else => unreachable,
                }

                var expected: u64 = 0;
                for (literal[offset..][0..chunk_len], 0..) |byte, byte_i| {
                    expected |= @as(u64, byte) << @intCast(byte_i * 8);
                }
                try self.codegen.emitLoadImm(expected_reg, @bitCast(expected));
                try self.emitCmpReg(loaded_reg, expected_reg);
                try mismatch_patches.append(self.allocator, try self.codegen.emitCondJump(condNotEqual()));

                offset += chunk_len;
            }
        }

        fn emitStoreStrCapture(
            self: *Self,
            dest_offset: i32,
            source_bytes_reg: GeneralReg,
            source_alloc_reg: GeneralReg,
            source_is_small_reg: GeneralReg,
            start_reg: GeneralReg,
            end_reg: GeneralReg,
        ) Allocator.Error!void {
            const capture_len_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(capture_len_reg);

            try self.emitMovRegReg(capture_len_reg, end_reg);
            try self.emitSubRegs(.w64, capture_len_reg, capture_len_reg, start_reg);

            try self.emitCmpImm(source_is_small_reg, 0);
            const heap_patch = try self.codegen.emitCondJump(condEqual());

            try self.emitStoreSmallStrCapture(dest_offset, source_bytes_reg, start_reg, capture_len_reg);
            const done_patch = try self.codegen.emitJump();

            const heap_offset = self.codegen.currentOffset();
            self.codegen.patchJump(heap_patch, heap_offset);
            try self.emitStoreHeapStrCapture(dest_offset, source_bytes_reg, source_alloc_reg, start_reg, capture_len_reg);

            const done_offset = self.codegen.currentOffset();
            self.codegen.patchJump(done_patch, done_offset);
        }

        fn emitStoreSmallStrCapture(
            self: *Self,
            dest_offset: i32,
            source_bytes_reg: GeneralReg,
            start_reg: GeneralReg,
            capture_len_reg: GeneralReg,
        ) Allocator.Error!void {
            try self.zeroStackArea(dest_offset, roc_str_size);

            const src_cur_reg = try self.allocTempGeneral();
            const dst_cur_reg = try self.allocTempGeneral();
            const remaining_reg = try self.allocTempGeneral();
            const byte_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(byte_reg);
            defer self.codegen.freeGeneral(remaining_reg);
            defer self.codegen.freeGeneral(dst_cur_reg);
            defer self.codegen.freeGeneral(src_cur_reg);

            try self.emitMovRegReg(src_cur_reg, source_bytes_reg);
            try self.emitAddRegs(.w64, src_cur_reg, src_cur_reg, start_reg);
            try self.emitLeaStack(dst_cur_reg, dest_offset);
            try self.emitMovRegReg(remaining_reg, capture_len_reg);

            const loop_start = self.codegen.currentOffset();
            try self.emitCmpImm(remaining_reg, 0);
            const done_patch = try self.codegen.emitCondJump(condEqual());
            try self.emitLoadW8(byte_reg, src_cur_reg, 0);
            try self.emitStoreW8(dst_cur_reg, 0, byte_reg);
            try self.emitAddUsizeImm(src_cur_reg, src_cur_reg, 1);
            try self.emitAddUsizeImm(dst_cur_reg, dst_cur_reg, 1);
            try self.emitSubImm(.w64, remaining_reg, remaining_reg, 1);
            const back_patch = try self.codegen.emitJump();
            self.codegen.patchJump(back_patch, loop_start);

            const done_offset = self.codegen.currentOffset();
            self.codegen.patchJump(done_patch, done_offset);

            try self.emitMovRegReg(byte_reg, capture_len_reg);
            try self.emitAddUsizeImm(byte_reg, byte_reg, 0x80);
            try self.emitStoreW8(frame_ptr, dest_offset + @as(i32, small_str_max_len), byte_reg);
        }

        fn emitStoreHeapStrCapture(
            self: *Self,
            dest_offset: i32,
            source_bytes_reg: GeneralReg,
            source_alloc_reg: GeneralReg,
            start_reg: GeneralReg,
            capture_len_reg: GeneralReg,
        ) Allocator.Error!void {
            const slice_bytes_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(slice_bytes_reg);

            try self.emitMovRegReg(slice_bytes_reg, source_bytes_reg);
            try self.emitAddRegs(.w64, slice_bytes_reg, slice_bytes_reg, start_reg);
            try self.emitStore(.w64, frame_ptr, dest_offset, slice_bytes_reg);
            try self.emitStore(.w64, frame_ptr, dest_offset + 8, source_alloc_reg);
            try self.emitStore(.w64, frame_ptr, dest_offset + 16, capture_len_reg);
        }

        /// Set up storage locations for join point parameters
        fn setupJoinPointParams(self: *Self, join_point: JoinPointId, params: LocalSpan) Allocator.Error!void {
            const jp_key = @intFromEnum(join_point);
            if (builtin.mode == .Debug and self.join_point_params.contains(jp_key)) {
                std.debug.panic(
                    "LIR/codegen invariant violated: duplicate join-point registration for id {d}",
                    .{jp_key},
                );
            }

            const locals = self.store.getLocalSpan(params);
            for (locals) |local| {
                try self.ensureStableLocationForLocal(local);
            }

            try self.join_point_params.put(jp_key, params);
        }

        /// Generate code for a string literal.
        fn generateStrLiteral(self: *Self, literal: LIR.StrLiteral) Allocator.Error!ValueLocation {
            const str_bytes = self.store.getStringLiteral(literal);
            const backing_bytes = self.store.getStringLiteralBacking(literal);
            const whole_backing = literal.offset == 0 and @as(usize, literal.len) == backing_bytes.len;
            const base_offset = self.codegen.allocStackSlot(roc_str_size);

            if (backing_bytes.len < roc_str_size and str_bytes.len < roc_str_size) {
                var bytes: [roc_str_size]u8 = .{0} ** roc_str_size;
                @memcpy(bytes[0..str_bytes.len], str_bytes);
                bytes[small_str_max_len] = @intCast(str_bytes.len | 0x80);

                const reg = try self.allocTempGeneral();
                defer self.codegen.freeGeneral(reg);

                const chunk0: u64 = @bitCast(bytes[0..target_ptr_size].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk0));
                try self.codegen.emitStoreStack(.w64, base_offset, reg);

                const chunk1: u64 = @bitCast(bytes[target_ptr_size .. 2 * target_ptr_size].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk1));
                try self.codegen.emitStoreStack(.w64, base_offset + target_ptr_size, reg);

                const chunk2: u64 = @bitCast(bytes[2 * target_ptr_size .. 3 * target_ptr_size].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk2));
                try self.codegen.emitStoreStack(.w64, base_offset + 2 * target_ptr_size, reg);
            } else {
                const ptr_reg = try self.allocTempGeneral();
                defer self.codegen.freeGeneral(ptr_reg);

                switch (self.generation_mode) {
                    .native_execution => {
                        verifyStaticStringBytes(backing_bytes);
                        try self.codegen.emitLoadImm(ptr_reg, @bitCast(@as(u64, @intFromPtr(str_bytes.ptr))));
                    },
                    .shim_execution, .object_file => {
                        const symbol_name = self.staticStringSymbol(literal.backing);
                        try self.codegen.emitLoadDataAddress(ptr_reg, symbol_name);
                        try self.emitAddUsizeImm(ptr_reg, ptr_reg, literal.offset);
                    },
                }
                try self.codegen.emitStoreStack(.w64, base_offset, ptr_reg);
                switch (self.generation_mode) {
                    .native_execution => {
                        const cap_or_alloc = if (whole_backing)
                            str_bytes.len << 1
                        else
                            @intFromPtr(backing_bytes.ptr) | 1;
                        try self.codegen.emitLoadImm(ptr_reg, @intCast(cap_or_alloc));
                    },
                    .shim_execution, .object_file => {
                        if (whole_backing) {
                            try self.codegen.emitLoadImm(ptr_reg, @intCast(str_bytes.len << 1));
                        } else {
                            const symbol_name = self.staticStringSymbol(literal.backing);
                            try self.codegen.emitLoadDataAddress(ptr_reg, symbol_name);
                            try self.emitAddUsizeImm(ptr_reg, ptr_reg, 1);
                        }
                    },
                }
                try self.codegen.emitStoreStack(.w64, base_offset + 8, ptr_reg);
                try self.codegen.emitLoadImm(ptr_reg, @intCast(str_bytes.len));
                try self.codegen.emitStoreStack(.w64, base_offset + 16, ptr_reg);
            }

            return .{ .stack_str = base_offset };
        }

        /// Generate code for a byte-list literal.
        fn generateBytesLiteral(self: *Self, literal: LIR.StrLiteral) Allocator.Error!ValueLocation {
            const bytes = self.store.getStringLiteral(literal);
            const backing_bytes = self.store.getStringLiteralBacking(literal);
            const whole_backing = literal.offset == 0 and @as(usize, literal.len) == backing_bytes.len;
            const base_offset = self.codegen.allocStackSlot(roc_list_size);

            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            if (bytes.len == 0) {
                try self.codegen.emitLoadImm(ptr_reg, 0);
                try self.codegen.emitStoreStack(.w64, base_offset, ptr_reg);
                try self.codegen.emitStoreStack(.w64, base_offset + 8, ptr_reg);
                try self.codegen.emitStoreStack(.w64, base_offset + 16, ptr_reg);
            } else {
                switch (self.generation_mode) {
                    .native_execution => {
                        verifyStaticStringBytes(backing_bytes);
                        try self.codegen.emitLoadImm(ptr_reg, @bitCast(@as(u64, @intFromPtr(bytes.ptr))));
                    },
                    .shim_execution, .object_file => {
                        const symbol_name = self.staticStringSymbol(literal.backing);
                        try self.codegen.emitLoadDataAddress(ptr_reg, symbol_name);
                        try self.emitAddUsizeImm(ptr_reg, ptr_reg, literal.offset);
                    },
                }
                try self.codegen.emitStoreStack(.w64, base_offset, ptr_reg);

                try self.codegen.emitLoadImm(ptr_reg, @intCast(bytes.len));
                try self.codegen.emitStoreStack(.w64, base_offset + 8, ptr_reg);

                switch (self.generation_mode) {
                    .native_execution => {
                        const cap_or_alloc = if (whole_backing)
                            bytes.len << 1
                        else
                            @intFromPtr(backing_bytes.ptr) | 1;
                        try self.codegen.emitLoadImm(ptr_reg, @intCast(cap_or_alloc));
                    },
                    .shim_execution, .object_file => {
                        if (whole_backing) {
                            try self.codegen.emitLoadImm(ptr_reg, @intCast(bytes.len << 1));
                        } else {
                            const symbol_name = self.staticStringSymbol(literal.backing);
                            try self.codegen.emitLoadDataAddress(ptr_reg, symbol_name);
                            try self.emitAddUsizeImm(ptr_reg, ptr_reg, 1);
                        }
                    },
                }
                try self.codegen.emitStoreStack(.w64, base_offset + 16, ptr_reg);
            }

            return .{ .list_stack = .{
                .struct_offset = base_offset,
                .data_offset = 0,
                .num_elements = @intCast(bytes.len),
            } };
        }

        fn emitAddUsizeImm(self: *Self, dst: GeneralReg, src: GeneralReg, imm: usize) Allocator.Error!void {
            if (imm == 0) {
                if (dst != src) try self.codegen.emit.movRegReg(.w64, dst, src);
                return;
            }
            if (imm <= @as(usize, @intCast(std.math.maxInt(i32)))) {
                try self.emitAddPtrImmAny(dst, src, @intCast(imm));
                return;
            }

            const scratch = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(scratch);
            try self.codegen.emitLoadImm(scratch, @intCast(imm));
            try self.emitAddRegs(.w64, dst, src, scratch);
        }

        fn staticStringSymbol(self: *Self, str_idx: base.StringLiteral.Idx) []const u8 {
            for (self.static_strings) |entry| {
                if (entry.id == str_idx) return entry.symbol_name;
            }
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "Dev/codegen invariant violated: non-SSO string literal {d} has no readonly data symbol",
                    .{@intFromEnum(str_idx)},
                );
            }
            unreachable;
        }

        fn verifyStaticStringBytes(str_bytes: []const u8) void {
            if (builtin.mode != .Debug) return;

            const data_addr = @intFromPtr(str_bytes.ptr);
            if (data_addr % @alignOf(isize) != 0) {
                std.debug.panic("Dev/codegen invariant violated: static string literal bytes are not refcount-aligned", .{});
            }
            const refcount_ptr: *const isize = @ptrCast(@alignCast(str_bytes.ptr - @sizeOf(isize)));
            if (refcount_ptr.* != builtins.utils.REFCOUNT_STATIC_DATA) {
                std.debug.panic("Dev/codegen invariant violated: static string literal missing static refcount", .{});
            }
        }

        /// Max size of any debug-assertion crash message (in bytes, aligned to 8).
        /// Used to size the per-proc shared msg slot. Generous to cover any
        /// future debug messages without re-tuning. Sharing this single slot
        /// across all debug crashes in a proc keeps the proc's frame from
        /// growing linearly with the number of debug asserts.
        const debug_msg_slot_size: u32 = 256;

        fn emitRocStaticMessageCall(self: *Self, field_offset: i32, msg: []const u8) Allocator.Error!void {
            return self.emitRocStaticMessageCallShared(field_offset, msg, false);
        }

        /// Like `emitRocStaticMessageCall`, but reuses a single per-proc stack
        /// slot for the message and args. Safe because only one debug crash
        /// can fire per program run — the program exits after the first.
        fn emitRocStaticDebugMessageCall(self: *Self, field_offset: i32, msg: []const u8) Allocator.Error!void {
            return self.emitRocStaticMessageCallShared(field_offset, msg, true);
        }

        fn emitRocStaticMessageCallShared(self: *Self, field_offset: i32, msg: []const u8, shared: bool) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse unreachable;

            const msg_aligned_size: u32 = std.mem.alignForward(u32, @intCast(msg.len), 8);
            const effective_size: u32 = if (msg_aligned_size == 0) 8 else msg_aligned_size;
            // If the message is unexpectedly larger than the shared slot we
            // fall back to a fresh per-call allocation. Keeps the safety
            // invariant even if a future caller passes a long string.
            const can_share = shared and effective_size <= debug_msg_slot_size;
            const msg_slot = if (can_share) blk: {
                if (self.proc_debug_msg_slot) |existing| break :blk existing;
                const slot = self.codegen.allocStackSlot(debug_msg_slot_size);
                self.proc_debug_msg_slot = slot;
                break :blk slot;
            } else self.codegen.allocStackSlot(effective_size);

            const base_reg = frame_ptr;
            const tmp = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(tmp);

            var offset: u32 = 0;
            while (offset < msg.len) : (offset += 8) {
                const remaining = msg.len - offset;
                if (remaining >= 8) {
                    const chunk: u64 = @bitCast(msg[offset..][0..8].*);
                    try self.codegen.emitLoadImm(tmp, @bitCast(chunk));
                    try self.emitStore(.w64, base_reg, msg_slot + @as(i32, @intCast(offset)), tmp);
                } else {
                    var padded: [8]u8 = .{0} ** 8;
                    @memcpy(padded[0..remaining], msg[offset..][0..remaining]);
                    const chunk: u64 = @bitCast(padded);
                    try self.codegen.emitLoadImm(tmp, @bitCast(chunk));
                    try self.emitStore(.w64, base_reg, msg_slot + @as(i32, @intCast(offset)), tmp);
                }
            }

            const msg_len_val: i64 = @bitCast(@as(u64, msg.len));

            if (self.generation_mode.threadsRocOps()) {
                // RocOps-threaded modes reach host callbacks through RocOps:
                // callback(ops: *RocOps, bytes: [*]const u8, len: usize).
                const fn_ptr_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X10 else .RAX;
                try self.emitLoad(.w64, fn_ptr_reg, roc_ops_reg, field_offset);

                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addRegArg(roc_ops_reg);
                try builder.addLeaArg(base_reg, msg_slot);
                try builder.addImmArg(msg_len_val);
                try builder.callReg(fn_ptr_reg);
            } else {
                // Object files call the host's fixed runtime symbol directly:
                // symbol(bytes: [*]const u8, len: usize).
                const symbol_name: []const u8 = if (field_offset == @offsetOf(RocOps, "roc_crashed"))
                    "roc_crashed"
                else if (field_offset == @offsetOf(RocOps, "roc_expect_failed"))
                    "roc_expect_failed"
                else
                    "roc_dbg";

                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addLeaArg(base_reg, msg_slot);
                try builder.addImmArg(msg_len_val);
                try builder.callRelocatable(symbol_name, self.allocator, &self.codegen.relocations);
            }
        }

        fn emitRocDbgFromStackStr(self: *Self, str_offset: i32) Allocator.Error!void {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, str_offset);
            try builder.addRegArg(self.roc_ops_reg orelse unreachable);
            try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_dbg_str), .dbg_str);
        }

        fn emitRocExpectErrFromStackStr(self: *Self, str_offset: i32, region: base.Region) Allocator.Error!void {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addLeaArg(frame_ptr, str_offset);
            try builder.addImmArg(@intCast(region.start.offset));
            try builder.addImmArg(@intCast(region.end.offset));
            try builder.addRegArg(self.roc_ops_reg orelse unreachable);
            try self.callBuiltin(&builder, @intFromPtr(&dev_wrappers.roc_builtins_expect_err_str), .expect_err_str);
        }

        fn emitRocExpectFailed(self: *Self) Allocator.Error!void {
            try self.emitRocStaticMessageCall(@offsetOf(RocOps, "roc_expect_failed"), "expect failed");
        }

        fn emitComptimeBranchTaken(
            self: *Self,
            hooks: ComptimeHooks,
            site: lir.LIR.ComptimeSiteId,
            branch_index: u32,
        ) Allocator.Error!void {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(self.roc_ops_reg orelse unreachable);
            try builder.addImmArg(@intCast(@intFromEnum(site)));
            try builder.addImmArg(@intCast(branch_index));
            try builder.call(@intFromPtr(hooks.branch_taken));
        }

        fn emitComptimeExhaustivenessFailed(
            self: *Self,
            hooks: ComptimeHooks,
            site: lir.LIR.ComptimeSiteId,
        ) Allocator.Error!void {
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(self.roc_ops_reg orelse unreachable);
            try builder.addImmArg(@intCast(@intFromEnum(site)));
            try builder.call(@intFromPtr(hooks.exhaustiveness_failed));
        }

        fn emitComptimeFailureRegion(
            self: *Self,
            hooks: ComptimeHooks,
        ) Allocator.Error!void {
            const stmt_id = self.current_stmt_id orelse return;
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const region = self.store.stmtRegion(stmt_id);
            if (region.isEmpty()) return;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(roc_ops_reg);
            try builder.addImmArg(@intCast(region.start.offset));
            try builder.addImmArg(@intCast(region.end.offset));
            try builder.call(@intFromPtr(hooks.failure_region));
        }

        fn emitComptimeCallEnter(
            self: *Self,
            hooks: ComptimeHooks,
        ) Allocator.Error!bool {
            const stmt_id = self.current_stmt_id orelse return false;
            const roc_ops_reg = self.roc_ops_reg orelse return false;
            const region = self.store.stmtRegion(stmt_id);
            if (region.isEmpty()) return false;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(roc_ops_reg);
            try builder.addImmArg(@intCast(region.start.offset));
            try builder.addImmArg(@intCast(region.end.offset));
            try builder.call(@intFromPtr(hooks.call_enter));
            return true;
        }

        fn emitComptimeCallExit(
            self: *Self,
            hooks: ComptimeHooks,
        ) Allocator.Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
            try builder.addRegArg(roc_ops_reg);
            try builder.call(@intFromPtr(hooks.call_exit));
        }

        /// Emit a roc_crashed call via RocOps with a static message.
        fn emitRocCrash(self: *Self, msg: []const u8) Allocator.Error!void {
            if (self.comptime_hooks) |hooks| try self.emitComptimeFailureRegion(hooks);
            try self.emitRocStaticMessageCall(@offsetOf(RocOps, "roc_crashed"), msg);
        }

        /// Same as `emitRocCrash`, but reuses a per-proc shared msg/args slot.
        /// Only safe from debug-assertion paths, where at most one fires per
        /// run (the program exits immediately after).
        fn emitRocCrashShared(self: *Self, msg: []const u8) Allocator.Error!void {
            if (self.comptime_hooks) |hooks| try self.emitComptimeFailureRegion(hooks);
            try self.emitRocStaticDebugMessageCall(@offsetOf(RocOps, "roc_crashed"), msg);
        }

        fn emitTrap(self: *Self) Allocator.Error!void {
            if (comptime target.toCpuArch() == .aarch64) {
                try self.codegen.emit.brk();
            } else {
                try self.codegen.emit.ud2();
            }
        }

        /// Generate an ABI-compliant entrypoint wrapper for calling a compiled Roc proc.
        ///
        /// The wrapper:
        /// 1. Receives `(roc_ops, ret_ptr, args_ptr)` in the platform C ABI
        /// 2. Saves the incoming pointers in callee-saved registers
        /// 3. Unpacks argument bytes from `args_ptr` according to Roc layout alignment
        /// 4. Calls the already-compiled Roc proc body
        /// 5. Stores the result into `ret_ptr`
        /// 6. Returns `void`
        pub fn generateEntrypointWrapper(
            self: *Self,
            symbol_name: []const u8,
            entry_proc: lir.LIR.LirProcSpecId,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
        ) Allocator.Error!ExportedSymbol {
            const func_start = self.codegen.currentOffset();
            var prologue_size: u32 = 0;
            var stack_alloc: u32 = 0;
            var frame_size: u32 = 0;
            var callee_saved_mask: u32 = 0;
            var epilogue_offset: u32 = 0;

            // Incoming C-ABI pieces that arrive on the caller's stack. On
            // x86_64 they are copied in the body ([rbp + fixed offset]); on
            // aarch64 the offset from the frame pointer depends on the final
            // frame size, so the copies are emitted right after the prologue,
            // once that size is known.
            var incoming_stack_copies = std.ArrayList(EntryStackCopy).empty;
            defer incoming_stack_copies.deinit(self.allocator);

            self.local_locations.clearRetainingCapacity();
            self.codegen.callee_saved_used = 0;

            if (arch == .aarch64 or arch == .aarch64_be) {
                const saved_callee_saved_used = self.codegen.callee_saved_used;
                const saved_callee_saved_available = self.codegen.callee_saved_available;
                const saved_roc_ops_reg = self.roc_ops_reg;
                const saved_early_return_patches_len = self.early_return_patches.items.len;

                const x19_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19);
                const x20_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20);
                const x21_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X21);
                self.codegen.callee_saved_used = x19_bit | x20_bit | x21_bit;
                self.codegen.callee_saved_available &= ~(x19_bit | x20_bit | x21_bit);
                self.codegen.stack_offset = 16 + CodeGen.CALLEE_SAVED_AREA_SIZE;

                const body_start = self.codegen.currentOffset();
                const relocs_before = self.codegen.relocations.items.len;

                if (self.generation_mode.threadsRocOps()) {
                    // RocOps-threaded modes call the wrapper with the
                    // (ops, ret_ptr, args_ptr) convention.
                    try self.codegen.emit.movRegReg(.w64, .X19, .X0);
                    try self.codegen.emit.movRegReg(.w64, .X20, .X1);
                    try self.codegen.emit.movRegReg(.w64, .X21, .X2);
                    self.roc_ops_reg = .X19;
                    try self.generateEntrypointProcCall(entry_proc, arg_layouts, ret_layout, .X20, .X21);
                } else {
                    // Object files export natural C-ABI entrypoints; the
                    // internal convention's RocOps is null under the symbol
                    // ABI. The incoming sret pointer (if any) is captured into
                    // X20 inside generateEntrypointBodyCAbi before X19 is
                    // written.
                    if (self.useDefaultPlatformLinuxStart(symbol_name)) {
                        try self.generateDefaultPlatformLinuxStartBody(entry_proc, arg_layouts, ret_layout);
                    } else {
                        try self.generateEntrypointBodyCAbi(entry_proc, arg_layouts, ret_layout, .X20, &incoming_stack_copies);
                    }
                }

                const body_epilogue_offset = self.codegen.currentOffset();
                const actual_locals: u32 = @intCast(self.codegen.stack_offset - 16 - CodeGen.CALLEE_SAVED_AREA_SIZE);
                {
                    var builder = CodeGen.DeferredFrameBuilder.init();
                    builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                    builder.setStackSize(actual_locals);
                    try builder.emitEpilogue(&self.codegen.emit);
                }

                const body_end = self.codegen.currentOffset();
                const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                const prologue_start = self.codegen.currentOffset();
                {
                    var frame_builder = CodeGen.DeferredFrameBuilder.init();
                    frame_builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                    frame_builder.setStackSize(actual_locals);
                    _ = try frame_builder.emitPrologue(&self.codegen.emit);
                    frame_size = frame_builder.actual_stack_alloc;
                }
                try self.emitEntryIncomingStackCopies(incoming_stack_copies.items, @intCast(frame_size));
                const prologue_size_val = self.codegen.currentOffset() - prologue_start;
                prologue_size = @intCast(prologue_size_val);
                stack_alloc = actual_locals;
                callee_saved_mask = self.codegen.callee_saved_used;

                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size_val);
                }

                self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size_val, std.math.maxInt(u64));
                self.shiftPendingCalls(body_start, body_end, prologue_size_val);
                self.shiftPendingProcAddrs(body_start, body_end, prologue_size_val);
                self.repatchInternalCalls(body_start, body_end, prologue_size_val, body_start);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size_val, body_start);

                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size_val;
                }
                const final_epilogue = body_epilogue_offset - body_start + prologue_size_val + prologue_start;
                epilogue_offset = @intCast(final_epilogue - prologue_start);
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);

                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.roc_ops_reg = saved_roc_ops_reg;
            } else {
                const saved_callee_saved_used = self.codegen.callee_saved_used;
                const saved_callee_saved_available = self.codegen.callee_saved_available;
                const saved_roc_ops_reg = self.roc_ops_reg;
                const saved_early_return_patches_len = self.early_return_patches.items.len;

                const rbx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
                const r12_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12);
                const r13_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R13);
                self.codegen.callee_saved_used = rbx_bit | r12_bit | r13_bit;
                self.codegen.callee_saved_available &= ~(rbx_bit | r12_bit | r13_bit);
                self.codegen.stack_offset = -CodeGen.CALLEE_SAVED_AREA_SIZE;

                const body_start = self.codegen.currentOffset();
                const relocs_before = self.codegen.relocations.items.len;

                if (self.generation_mode.threadsRocOps()) {
                    // RocOps-threaded modes call the wrapper with the
                    // (ops, ret_ptr, args_ptr) convention.
                    if (target.isWindows()) {
                        try self.codegen.emit.movRegReg(.w64, .R12, .RCX);
                        try self.codegen.emit.movRegReg(.w64, .RBX, .RDX);
                        try self.codegen.emit.movRegReg(.w64, .R13, .R8);
                    } else {
                        try self.codegen.emit.movRegReg(.w64, .R12, .RDI);
                        try self.codegen.emit.movRegReg(.w64, .RBX, .RSI);
                        try self.codegen.emit.movRegReg(.w64, .R13, .RDX);
                    }
                    self.roc_ops_reg = .R12;
                    try self.generateEntrypointProcCall(entry_proc, arg_layouts, ret_layout, .RBX, .R13);
                } else {
                    // Object files export natural C-ABI entrypoints; the
                    // internal convention's RocOps is null under the symbol
                    // ABI. The incoming sret pointer (if any) is captured into
                    // RBX inside generateEntrypointBodyCAbi before R12 is
                    // written.
                    if (self.useDefaultPlatformLinuxStart(symbol_name)) {
                        try self.generateDefaultPlatformLinuxStartBody(entry_proc, arg_layouts, ret_layout);
                    } else {
                        try self.generateEntrypointBodyCAbi(entry_proc, arg_layouts, ret_layout, .RBX, &incoming_stack_copies);
                    }
                }

                const body_epilogue_offset = self.codegen.currentOffset();
                const actual_locals_x86: u32 = @intCast(-self.codegen.stack_offset - CodeGen.CALLEE_SAVED_AREA_SIZE);
                {
                    var builder = CodeGen.DeferredFrameBuilder.init();
                    builder.setCalleeSavedMask(self.codegen.callee_saved_used);
                    builder.setStackSize(actual_locals_x86);
                    try builder.emitEpilogue(&self.codegen.emit);
                }

                const body_end = self.codegen.currentOffset();
                const body_bytes = self.allocator.dupe(u8, self.codegen.emit.buf.items[body_start..body_end]) catch return error.OutOfMemory;
                defer self.allocator.free(body_bytes);

                self.codegen.emit.buf.shrinkRetainingCapacity(body_start);

                const prologue_start_x86 = self.codegen.currentOffset();
                try self.codegen.emitPrologueWithAlloc(actual_locals_x86);
                const prologue_size_x86 = self.codegen.currentOffset() - prologue_start_x86;

                prologue_size = @intCast(prologue_size_x86);
                stack_alloc = actual_locals_x86;
                frame_size = actual_locals_x86;
                callee_saved_mask = self.codegen.callee_saved_used;

                self.codegen.emit.buf.appendSlice(self.allocator, body_bytes) catch return error.OutOfMemory;

                for (self.codegen.relocations.items[relocs_before..]) |*reloc| {
                    reloc.adjustOffset(prologue_size_x86);
                }

                self.shiftNestedCompiledRcHelperOffsets(body_start, body_end, prologue_size_x86, std.math.maxInt(u64));
                self.shiftPendingCalls(body_start, body_end, prologue_size_x86);
                self.shiftPendingProcAddrs(body_start, body_end, prologue_size_x86);
                self.repatchInternalCalls(body_start, body_end, prologue_size_x86, body_start);
                self.repatchInternalAddrPatches(body_start, body_end, prologue_size_x86, body_start);

                for (self.early_return_patches.items[saved_early_return_patches_len..]) |*patch| {
                    patch.* += prologue_size_x86;
                }
                const final_epilogue = body_epilogue_offset - body_start + prologue_size_x86 + prologue_start_x86;
                epilogue_offset = @intCast(final_epilogue - prologue_start_x86);
                for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                    self.codegen.patchJump(patch, final_epilogue);
                }
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);

                self.codegen.callee_saved_used = saved_callee_saved_used;
                self.codegen.callee_saved_available = saved_callee_saved_available;
                self.roc_ops_reg = saved_roc_ops_reg;
            }

            const func_end = self.codegen.currentOffset();

            return ExportedSymbol{
                .name = "",
                .offset = func_start,
                .size = func_end - func_start,
                .prologue_size = prologue_size,
                .stack_alloc = stack_alloc,
                .frame_size = frame_size,
                .callee_saved_mask = callee_saved_mask,
                .epilogue_offset = epilogue_offset,
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
                    if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
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
            const needs_ret_ptr = self.needsInternalReturnByPointer(ret_layout);
            const ret_buffer_offset = if (needs_ret_ptr) blk: {
                const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
                const size = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_ret_layout)).size;
                break :blk self.codegen.allocStackSlot(size);
            } else 0;

            const emit_roc_ops = self.generation_mode.threadsRocOps();
            const pbp_plan = try self.computePassByPtrPlan(arg_infos, if (needs_ret_ptr) 1 else 0, emit_roc_ops);
            defer self.scratch_pass_by_ptr.clearFrom(pbp_plan.start);

            const placed_call = try self.placeCallArguments(arg_infos, .{
                .needs_ret_ptr = needs_ret_ptr,
                .ret_buffer_offset = ret_buffer_offset,
                .pass_by_ptr = pbp_plan.slice,
                .emit_roc_ops = emit_roc_ops,
            });
            try self.emitCallToOffset(code_offset);

            if (placed_call.stack_spill_size > 0) {
                try self.emitAddStackPtr(placed_call.stack_spill_size);
            }

            return self.saveCallReturnValue(ret_layout, needs_ret_ptr, ret_buffer_offset);
        }

        /// A C-ABI piece of an entrypoint argument that arrives on the
        /// caller's stack. `incoming_slot` indexes the caller's 8-byte
        /// outgoing argument slots.
        const EntryStackCopy = struct {
            dest_off: i32,
            incoming_slot: u32,
            kind: union(enum) {
                /// Copy one slot's value into the destination, storing
                /// `width` bytes (4 or 8).
                value: u8,
                /// The slot holds a pointer; copy this many bytes from it.
                deref: u32,
            },
        };

        /// A C-ABI piece of an entrypoint argument that arrives in a register.
        const EntryRegCapture = struct {
            dest_off: i32,
            /// Store width in bytes (4 or 8).
            width: u8,
            reg_index: u16,
            is_float: bool,
        };

        /// An indirectly-passed entrypoint argument whose pointer arrives in
        /// a register; the pointer is spilled to `ptr_off` before any
        /// register can be clobbered, then dereferenced.
        const EntryIndirectCapture = struct {
            dest_off: i32,
            size: u32,
            ptr_off: i32,
        };

        fn useDefaultPlatformLinuxStart(self: *const Self, symbol_name: []const u8) bool {
            if (!self.enable_default_platform_runtime) return false;
            if (self.generation_mode != .object_file) return false;
            if (comptime target.toOsTag() != .linux) return false;
            return std.mem.eql(u8, symbol_name, "_start");
        }

        fn generateDefaultPlatformLinuxStartBody(
            self: *Self,
            entry_proc: lir.LIR.LirProcSpecId,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
        ) Allocator.Error!void {
            const compiled = try self.compiledProcForId(entry_proc);
            if (compiled.code_start == unresolved_proc_code_start) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "default platform entrypoint proc {d} was not compiled before wrapper generation",
                        .{@intFromEnum(entry_proc)},
                    );
                }
                unreachable;
            }

            for (arg_layouts) |arg_layout| {
                if (self.getLayoutSize(arg_layout) != 0) {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "default platform _start invariant violated: expected only zero-sized args, got layout {d}",
                            .{@intFromEnum(arg_layout)},
                        );
                    }
                    unreachable;
                }
            }

            const null_ops_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X19 else .R12;
            try self.codegen.emitLoadImm(null_ops_reg, 0);
            self.roc_ops_reg = null_ops_reg;

            {
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.callRelocatable("roc_default_runtime_init", self.allocator, &self.codegen.relocations);
            }

            const arg_infos_start = self.scratch_arg_infos.top();
            defer self.scratch_arg_infos.clearFrom(arg_infos_start);
            for (arg_layouts) |arg_layout| {
                try self.scratch_arg_infos.append(.{
                    .loc = .{ .immediate_i64 = 0 },
                    .layout_idx = arg_layout,
                    .num_regs = 0,
                });
            }

            const arg_infos = self.scratch_arg_infos.sliceFromStart(arg_infos_start);
            const result_loc = try self.callCompiledOffsetWithArgInfos(compiled.code_start, arg_infos, ret_layout);

            const exit_code_reg = self.getArgumentRegister(0);
            try self.moveToReg(result_loc, exit_code_reg);
            {
                var builder = try Builder.init(&self.codegen.emit, &self.codegen.stack_offset);
                try builder.addRegArg(exit_code_reg);
                try builder.callRelocatable("roc_default_exit", self.allocator, &self.codegen.relocations);
            }
            try self.emitTrap();
        }

        /// Generate the body of a natural C-ABI entrypoint wrapper: capture
        /// the incoming C-ABI arguments into stack slots, call the compiled
        /// proc through the internal convention with a null RocOps, and
        /// marshal the result back out per the C ABI.
        fn generateEntrypointBodyCAbi(
            self: *Self,
            entry_proc: lir.LIR.LirProcSpecId,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
            sret_reg: GeneralReg,
            incoming_stack_copies: *std.ArrayList(EntryStackCopy),
        ) Allocator.Error!void {
            const compiled = try self.compiledProcForId(entry_proc);
            if (compiled.code_start == unresolved_proc_code_start) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "entrypoint proc {d} was not compiled before wrapper generation",
                        .{@intFromEnum(entry_proc)},
                    );
                }
                unreachable;
            }

            const abi_target: layout.abi.Target = if (comptime target.toCpuArch() == .aarch64)
                .aarch64
            else if (comptime roc_target.isWindows())
                .x86_64_windows
            else
                .x86_64_sysv;
            var arena_state = std.heap.ArenaAllocator.init(self.allocator);
            defer arena_state.deinit();
            const lowered = layout.abi.lower(arena_state.allocator(), self.layout_store, abi_target, arg_layouts, ret_layout, false) catch return error.OutOfMemory;

            const int_param_regs = EmitType.CC.PARAM_REGS;
            const float_param_regs = EmitType.CC.FLOAT_PARAM_REGS;
            // Windows x64 shares one argument position counter between
            // integer and float registers; SysV and AAPCS64 count separately.
            const shared_arg_positions = comptime target.toCpuArch() == .x86_64 and roc_target.isWindows();

            // Capture the sret pointer before anything else can clobber it.
            if (lowered.ret == .indirect) {
                if (comptime target.toCpuArch() == .aarch64) {
                    try self.codegen.emit.movRegReg(.w64, sret_reg, .XR);
                } else {
                    try self.codegen.emit.movRegReg(.w64, sret_reg, int_param_regs[0]);
                }
            }

            var int_idx: usize = if (lowered.ret == .indirect and comptime target.toCpuArch() == .x86_64) 1 else 0;
            var float_idx: usize = 0;
            var stack_slot: u32 = 0;

            var reg_captures = std.ArrayList(EntryRegCapture).empty;
            defer reg_captures.deinit(self.allocator);
            var indirect_captures = std.ArrayList(EntryIndirectCapture).empty;
            defer indirect_captures.deinit(self.allocator);

            const arg_infos_start = self.scratch_arg_infos.top();

            for (lowered.args, arg_layouts) |placement, arg_layout| {
                switch (placement) {
                    .none => {
                        try self.scratch_arg_infos.append(.{
                            .loc = .{ .immediate_i64 = 0 },
                            .layout_idx = arg_layout,
                            .num_regs = 0,
                        });
                        continue;
                    },
                    .registers => |pieces| {
                        const slot_size = @max(self.entrypointParamSlotSize(arg_layout), 8);
                        const slot = self.codegen.allocStackSlot(slot_size);
                        for (pieces) |piece| {
                            const dest_off = slot + @as(i32, @intCast(piece.offset));
                            switch (piece.class) {
                                .integer => {
                                    const width: u8 = if (piece.size <= 4) 4 else 8;
                                    const pos = int_idx;
                                    int_idx += 1;
                                    if (shared_arg_positions) float_idx = int_idx;
                                    if (pos < int_param_regs.len) {
                                        try reg_captures.append(self.allocator, .{
                                            .dest_off = dest_off,
                                            .width = width,
                                            .reg_index = @intCast(pos),
                                            .is_float = false,
                                        });
                                    } else {
                                        try incoming_stack_copies.append(self.allocator, .{
                                            .dest_off = dest_off,
                                            .incoming_slot = stack_slot,
                                            .kind = .{ .value = width },
                                        });
                                        stack_slot += 1;
                                    }
                                },
                                .float => {
                                    const width: u8 = if (piece.size == 8) 8 else 4;
                                    const pos = if (shared_arg_positions) blk: {
                                        const taken = int_idx;
                                        int_idx += 1;
                                        float_idx = int_idx;
                                        break :blk taken;
                                    } else blk: {
                                        const taken = float_idx;
                                        float_idx += 1;
                                        break :blk taken;
                                    };
                                    if (pos < float_param_regs.len) {
                                        try reg_captures.append(self.allocator, .{
                                            .dest_off = dest_off,
                                            .width = width,
                                            .reg_index = @intCast(pos),
                                            .is_float = true,
                                        });
                                    } else {
                                        try incoming_stack_copies.append(self.allocator, .{
                                            .dest_off = dest_off,
                                            .incoming_slot = stack_slot,
                                            .kind = .{ .value = width },
                                        });
                                        stack_slot += 1;
                                    }
                                },
                            }
                        }
                        const loc = self.stackLocationForLayout(arg_layout, slot);
                        try self.scratch_arg_infos.append(.{
                            .loc = loc,
                            .layout_idx = arg_layout,
                            .num_regs = self.calcArgRegCount(loc, arg_layout),
                        });
                    },
                    .indirect => {
                        const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
                        const raw_size = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(runtime_layout)).size;
                        const slot_size = @max(self.entrypointParamSlotSize(arg_layout), std.mem.alignForward(u32, raw_size, 8));
                        const slot = self.codegen.allocStackSlot(slot_size);

                        if (abi_target == .x86_64_sysv) {
                            // SysV memory-class aggregates arrive by value in
                            // the caller's outgoing argument slots.
                            const slot_count = (raw_size + 7) / 8;
                            var k: u32 = 0;
                            while (k < slot_count) : (k += 1) {
                                try incoming_stack_copies.append(self.allocator, .{
                                    .dest_off = slot + @as(i32, @intCast(k * 8)),
                                    .incoming_slot = stack_slot,
                                    .kind = .{ .value = 8 },
                                });
                                stack_slot += 1;
                            }
                        } else {
                            // AAPCS64 and Win64 pass a pointer.
                            const pos = int_idx;
                            int_idx += 1;
                            if (shared_arg_positions) float_idx = int_idx;
                            if (pos < int_param_regs.len) {
                                const ptr_off = self.codegen.allocStackSlot(8);
                                try reg_captures.append(self.allocator, .{
                                    .dest_off = ptr_off,
                                    .width = 8,
                                    .reg_index = @intCast(pos),
                                    .is_float = false,
                                });
                                try indirect_captures.append(self.allocator, .{
                                    .dest_off = slot,
                                    .size = raw_size,
                                    .ptr_off = ptr_off,
                                });
                            } else {
                                try incoming_stack_copies.append(self.allocator, .{
                                    .dest_off = slot,
                                    .incoming_slot = stack_slot,
                                    .kind = .{ .deref = raw_size },
                                });
                                stack_slot += 1;
                            }
                        }
                        const loc = self.stackLocationForLayout(arg_layout, slot);
                        try self.scratch_arg_infos.append(.{
                            .loc = loc,
                            .layout_idx = arg_layout,
                            .num_regs = self.calcArgRegCount(loc, arg_layout),
                        });
                    },
                }
            }

            // Pass 1: spill every incoming register piece. These stores must
            // come before anything that could clobber an argument register.
            for (reg_captures.items) |cap| {
                if (cap.is_float) {
                    const freg = float_param_regs[cap.reg_index];
                    try self.emitEntryFloatStore(cap.dest_off, freg, cap.width == 8);
                } else {
                    const reg = int_param_regs[cap.reg_index];
                    if (cap.width <= 4) {
                        try self.emitStore(.w32, frame_ptr, cap.dest_off, reg);
                    } else {
                        try self.emitStore(.w64, frame_ptr, cap.dest_off, reg);
                    }
                }
            }

            // With every argument register captured, the RocOps register can
            // be claimed; the symbol ABI has no host vtable, so it is null.
            const null_ops_reg: GeneralReg = if (comptime target.toCpuArch() == .aarch64) .X19 else .R12;
            try self.codegen.emitLoadImm(null_ops_reg, 0);
            self.roc_ops_reg = null_ops_reg;

            // Pass 2: x86_64 reads the caller's stack slots at a fixed frame
            // offset, so its copies happen here in the body. (On aarch64 the
            // offset depends on the final frame size; those copies are emitted
            // by emitEntryIncomingStackCopies right after the prologue.)
            if (comptime target.toCpuArch() == .x86_64) {
                for (incoming_stack_copies.items) |copy| {
                    const src_off = incoming_stack_arg_base_offset + @as(i32, @intCast(copy.incoming_slot)) * 8;
                    switch (copy.kind) {
                        .value => |width| {
                            try self.emitLoad(.w64, scratch_reg, frame_ptr, src_off);
                            if (width <= 4) {
                                try self.emitStore(.w32, frame_ptr, copy.dest_off, scratch_reg);
                            } else {
                                try self.emitStore(.w64, frame_ptr, copy.dest_off, scratch_reg);
                            }
                        },
                        .deref => |size| {
                            try self.emitLoad(.w64, scratch_reg, frame_ptr, src_off);
                            const temp_reg = try self.allocTempGeneral();
                            try self.copyChunked(temp_reg, scratch_reg, 0, frame_ptr, copy.dest_off, size);
                            self.codegen.freeGeneral(temp_reg);
                        },
                    }
                }
                incoming_stack_copies.clearRetainingCapacity();
            }

            // Dereference indirectly-passed arguments whose pointers were
            // spilled in pass 1.
            for (indirect_captures.items) |cap| {
                if (cap.size == 0) continue;
                const ptr_reg = try self.allocTempGeneral();
                const temp_reg = try self.allocTempGeneral();
                try self.emitLoad(.w64, ptr_reg, frame_ptr, cap.ptr_off);
                try self.copyChunked(temp_reg, ptr_reg, 0, frame_ptr, cap.dest_off, cap.size);
                self.codegen.freeGeneral(temp_reg);
                self.codegen.freeGeneral(ptr_reg);
            }

            const arg_infos = self.scratch_arg_infos.sliceFromStart(arg_infos_start);
            const result_loc = try self.callCompiledOffsetWithArgInfos(compiled.code_start, arg_infos, ret_layout);

            switch (lowered.ret) {
                .none => {},
                .indirect => {
                    if (self.getLayoutSize(ret_layout) > 0) {
                        try self.storeResultToSavedPtr(result_loc, ret_layout, sret_reg, 1);
                    }
                    // Both SysV and Win64 require the sret pointer back in RAX.
                    if (comptime target.toCpuArch() == .x86_64) {
                        try self.codegen.emit.movRegReg(.w64, .RAX, sret_reg);
                    }
                },
                .registers => |pieces| {
                    const ret_size = self.getLayoutSize(ret_layout);
                    const ret_slot = try self.ensureOnStack(result_loc, ret_size);
                    var gp_i: usize = 0;
                    var fp_i: usize = 0;
                    for (pieces) |piece| {
                        const src_off = ret_slot + @as(i32, @intCast(piece.offset));
                        switch (piece.class) {
                            .integer => {
                                const reg: GeneralReg = if (gp_i == 0) ret_reg_0 else ret_reg_1;
                                gp_i += 1;
                                if (piece.size <= 4) {
                                    try self.emitLoad(.w32, reg, frame_ptr, src_off);
                                } else {
                                    try self.emitLoad(.w64, reg, frame_ptr, src_off);
                                }
                            },
                            .float => {
                                try self.emitEntryFloatLoad(src_off, fp_i, piece.size == 8);
                                fp_i += 1;
                            },
                        }
                    }
                },
            }
        }

        /// Store an incoming float argument register into the frame.
        fn emitEntryFloatStore(self: *Self, dest_off: i32, freg: FloatReg, is_f64: bool) Allocator.Error!void {
            if (comptime target.toCpuArch() == .aarch64) {
                if (is_f64) {
                    try self.codegen.emitStoreStackF64(dest_off, freg);
                } else {
                    try self.codegen.emitStoreStackF32(dest_off, freg);
                }
            } else if (is_f64) {
                try self.codegen.emit.movsdMemReg(frame_ptr, dest_off, freg);
            } else {
                try self.codegen.emit.movssMemReg(frame_ptr, dest_off, freg);
            }
        }

        /// Load C-ABI float return piece `index` from the frame into the
        /// float return register sequence (V0..V3 / XMM0..XMM1).
        fn emitEntryFloatLoad(self: *Self, src_off: i32, index: usize, is_f64: bool) Allocator.Error!void {
            if (comptime target.toCpuArch() == .aarch64) {
                const fregs = [_]FloatReg{ .V0, .V1, .V2, .V3 };
                const freg = fregs[index];
                if (is_f64) {
                    try self.codegen.emitLoadStackF64(freg, src_off);
                } else {
                    try self.codegen.emitLoadStackF32(freg, src_off);
                }
            } else {
                const fregs = [_]FloatReg{ .XMM0, .XMM1 };
                const freg = fregs[index];
                if (is_f64) {
                    try self.codegen.emit.movsdRegMem(freg, frame_ptr, src_off);
                } else {
                    try self.codegen.emit.movssRegMem(freg, frame_ptr, src_off);
                }
            }
        }

        /// Copy entrypoint argument pieces from the caller's outgoing stack
        /// slots into the frame. Emitted right after the prologue on aarch64,
        /// where the caller's slots sit at [fp + frame_total].
        fn emitEntryIncomingStackCopies(self: *Self, copies: []const EntryStackCopy, frame_total: i32) Allocator.Error!void {
            if (comptime target.toCpuArch() != .aarch64) {
                std.debug.assert(copies.len == 0);
                return;
            }
            for (copies) |copy| {
                const src_off = frame_total + @as(i32, @intCast(copy.incoming_slot)) * 8;
                switch (copy.kind) {
                    .value => |width| {
                        try self.emitLoad(.w64, .IP0, frame_ptr, src_off);
                        if (width <= 4) {
                            try self.emitStore(.w32, frame_ptr, copy.dest_off, .IP0);
                        } else {
                            try self.emitStore(.w64, frame_ptr, copy.dest_off, .IP0);
                        }
                    },
                    .deref => |size| {
                        if (size == 0) continue;
                        try self.emitLoad(.w64, .IP1, frame_ptr, src_off);
                        try self.copyChunked(.IP0, .IP1, 0, frame_ptr, copy.dest_off, size);
                    },
                }
            }
        }

        fn generateEntrypointProcCall(
            self: *Self,
            entry_proc: lir.LIR.LirProcSpecId,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
            ret_ptr_reg: GeneralReg,
            args_ptr_reg: GeneralReg,
        ) Allocator.Error!void {
            const compiled = try self.compiledProcForId(entry_proc);
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

        /// Get the generated code buffer for object-file emission.
        /// Source line entries recorded during code generation, in emission
        /// (and therefore code offset) order.
        pub fn getLineEntries(self: *const Self) []const LineEntry {
            return self.line_entries.items;
        }

        pub fn getGeneratedCode(self: *Self) []const u8 {
            return self.codegen.getCode();
        }

        /// Get relocations for the generated code buffer.
        pub fn getRelocations(self: *Self) []const Relocation {
            return self.codegen.relocations.items;
        }
    };
}

/// x86_64 OpenBSD
pub const X64OpenbsdLirCodeGen = LirCodeGen(.x64openbsd);
/// x86_64 NetBSD
pub const X64NetbsdLirCodeGen = LirCodeGen(.x64netbsd);
/// x86_64 Linux (generic)
pub const X64LinuxLirCodeGen = LirCodeGen(.x64linux);
/// x86_64 ELF (generic)
pub const X64ElfLirCodeGen = LirCodeGen(.x64elf);

const host_lir_codegen_target = RocTarget.detectNative();

/// Whether this compiler build has a fast native dev backend for the target it
/// is being compiled to run on.
///
/// 32-bit ARM builds intentionally report false for now. The long-term fix is
/// for dev builds on those targets to route through LLVM rather than trying to
/// instantiate a nonexistent fast native backend.
pub const host_lir_codegen_available =
    switch (host_lir_codegen_target.toCpuArch()) {
        .x86_64, .aarch64, .aarch64_be => true,
        else => false,
    };

/// Host LirCodeGen for the target this compiler build runs on.
pub const HostLirCodeGen = if (host_lir_codegen_available)
    LirCodeGen(host_lir_codegen_target)
else
    void;

// Tests

const ExecutableMemory = @import("ExecutableMemory.zig").ExecutableMemory;

const TestLayoutState = struct {
    layout_store: layout.Store,
    module_env: *@import("can").ModuleEnv,

    fn init(allocator: Allocator) Allocator.Error!TestLayoutState {
        const module_env = try allocator.create(@import("can").ModuleEnv);
        module_env.* = try @import("can").ModuleEnv.init(allocator, "");
        const layout_store = try layout.Store.init(allocator, base.target.TargetUsize.native);
        return .{ .layout_store = layout_store, .module_env = module_env };
    }

    fn deinit(self: *TestLayoutState) void {
        const allocator = std.testing.allocator;
        self.layout_store.deinit();
        self.module_env.deinit();
        allocator.destroy(self.module_env);
    }
};

const TestRocOps = struct {
    allocator: Allocator,
    roc_ops: RocOps,

    fn init(allocator: Allocator) TestRocOps {
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
                .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
            },
        };
    }

    fn getOps(self: *TestRocOps) *RocOps {
        self.roc_ops.env = @ptrCast(self);
        return &self.roc_ops;
    }

    fn metaBytes(alignment: usize) usize {
        return @max(alignment, @alignOf(usize));
    }

    fn rocAlloc(ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
        const self: *TestRocOps = @ptrCast(@alignCast(ops.env));
        const align_enum = std.mem.Alignment.fromByteUnits(alignment);
        const meta = metaBytes(alignment);
        const total = length + meta;
        const alloc_base = self.allocator.rawAlloc(total, align_enum, @returnAddress()) orelse
            @panic("TestRocOps alloc failed");
        const size_ptr: *usize = @ptrFromInt(@intFromPtr(alloc_base) + meta - @sizeOf(usize));
        size_ptr.* = total;
        return @ptrFromInt(@intFromPtr(alloc_base) + meta);
    }

    fn rocDealloc(ops: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
        const self: *TestRocOps = @ptrCast(@alignCast(ops.env));
        const meta = metaBytes(alignment);
        const total_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
        const total = total_ptr.*;
        const alloc_base: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - meta);
        const align_enum = std.mem.Alignment.fromByteUnits(alignment);
        self.allocator.rawFree(alloc_base[0..total], align_enum, @returnAddress());
    }

    fn rocRealloc(ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
        const self: *TestRocOps = @ptrCast(@alignCast(ops.env));
        const meta = metaBytes(alignment);
        const old_total_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
        const old_total = old_total_ptr.*;
        const old_base: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - meta);
        const new_total = new_length + meta;
        const align_enum = std.mem.Alignment.fromByteUnits(alignment);
        const new_base = self.allocator.rawAlloc(new_total, align_enum, @returnAddress()) orelse
            @panic("TestRocOps realloc failed");
        @memcpy(new_base[0..@min(old_total, new_total)], old_base[0..@min(old_total, new_total)]);
        self.allocator.rawFree(old_base[0..old_total], align_enum, @returnAddress());
        const new_total_ptr: *usize = @ptrFromInt(@intFromPtr(new_base) + meta - @sizeOf(usize));
        new_total_ptr.* = new_total;
        return @ptrFromInt(@intFromPtr(new_base) + meta);
    }

    fn rocDbg(_: *RocOps, _: [*]const u8, _: usize) callconv(.c) void {
        @panic("unexpected dbg in TestRocOps");
    }

    fn rocExpectFailed(_: *RocOps, _: [*]const u8, _: usize) callconv(.c) void {
        @panic("unexpected expect failure in TestRocOps");
    }

    fn rocCrashed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        std.debug.panic("roc crashed: {s}", .{bytes[0..len]});
    }
};

fn addLocal(store: *LirStore, layout_idx: layout.Idx) Allocator.Error!LocalId {
    return try store.addLocal(.{ .layout_idx = layout_idx });
}

fn addNoArgProc(store: *LirStore, body: CFStmtId, ret_layout: layout.Idx) Allocator.Error!lir.LIR.LirProcSpecId {
    return try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LocalSpan.empty(),
        .body = body,
        .ret_layout = ret_layout,
    });
}

fn addProc(store: *LirStore, args: []const LocalId, body: CFStmtId, ret_layout: layout.Idx) Allocator.Error!lir.LIR.LirProcSpecId {
    return try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(args),
        .body = body,
        .ret_layout = ret_layout,
    });
}

fn addLiteralProc(store: *LirStore, value: lir.LiteralValue, ret_layout: layout.Idx) Allocator.Error!lir.LIR.LirProcSpecId {
    const result = try addLocal(store, ret_layout);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = result,
        .value = value,
        .next = ret,
    } });
    return try addNoArgProc(store, assign, ret_layout);
}

fn addUnaryLowLevelProc(store: *LirStore, op: lir.LowLevel, operand_value: i64, operand_layout: layout.Idx, ret_layout: layout.Idx) Allocator.Error!lir.LIR.LirProcSpecId {
    const operand = try addLocal(store, operand_layout);
    const result = try addLocal(store, ret_layout);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const args = try store.addLocalSpan(&.{operand});
    const assign_op = try store.addCFStmt(.{ .assign_low_level = .{
        .target = result,
        .op = op,
        .rc_effect = op.rcEffect(),
        .args = args,
        .next = ret,
    } });
    const assign_operand = try store.addCFStmt(.{ .assign_literal = .{
        .target = operand,
        .value = .{ .i64_literal = .{ .value = operand_value, .layout_idx = operand_layout } },
        .next = assign_op,
    } });
    return try addNoArgProc(store, assign_operand, ret_layout);
}

fn addBinaryLowLevelProc(
    store: *LirStore,
    op: lir.LowLevel,
    lhs_value: i64,
    rhs_value: i64,
    operand_layout: layout.Idx,
    ret_layout: layout.Idx,
) Allocator.Error!lir.LIR.LirProcSpecId {
    const lhs = try addLocal(store, operand_layout);
    const rhs = try addLocal(store, operand_layout);
    const result = try addLocal(store, ret_layout);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const args = try store.addLocalSpan(&.{ lhs, rhs });
    const assign_op = try store.addCFStmt(.{ .assign_low_level = .{
        .target = result,
        .op = op,
        .rc_effect = op.rcEffect(),
        .args = args,
        .next = ret,
    } });
    const assign_rhs = try store.addCFStmt(.{ .assign_literal = .{
        .target = rhs,
        .value = .{ .i64_literal = .{ .value = rhs_value, .layout_idx = operand_layout } },
        .next = assign_op,
    } });
    const assign_lhs = try store.addCFStmt(.{ .assign_literal = .{
        .target = lhs,
        .value = .{ .i64_literal = .{ .value = lhs_value, .layout_idx = operand_layout } },
        .next = assign_rhs,
    } });
    return try addNoArgProc(store, assign_lhs, ret_layout);
}

fn compileRoot(store: *LirStore, layout_store: *layout.Store, root_proc: lir.LIR.LirProcSpecId, ret_layout: layout.Idx) Allocator.Error!struct {
    code: []const u8,
    entry_offset: usize,
} {
    const allocator = std.testing.allocator;
    var codegen = try HostLirCodeGen.init(allocator, store, layout_store, &.{});
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(store.getProcSpecs());

    const result = try codegen.generateCode(root_proc, ret_layout, 1);
    return .{ .code = result.code, .entry_offset = result.entry_offset };
}

fn runRootI64(store: *LirStore, layout_store: *layout.Store, root_proc: lir.LIR.LirProcSpecId) (Allocator.Error || error{ EmptyCode, MmapFailed, VirtualAllocFailed, MprotectFailed, VirtualProtectFailed, UnsupportedPlatform })!i64 {
    const allocator = std.testing.allocator;
    const compiled = try compileRoot(store, layout_store, root_proc, .i64);
    defer allocator.free(compiled.code);

    var executable = try ExecutableMemory.initWithEntryOffset(compiled.code, compiled.entry_offset);
    defer executable.deinit();

    var test_ops = TestRocOps.init(allocator);
    var out: i64 = undefined;
    const func: *const fn (*anyopaque, *anyopaque) callconv(.c) void = @ptrCast(@alignCast(executable.entryPtr()));
    func(@ptrCast(&out), @ptrCast(test_ops.getOps()));
    return out;
}

fn runRootU64(store: *LirStore, layout_store: *layout.Store, root_proc: lir.LIR.LirProcSpecId, ret_layout: layout.Idx) (Allocator.Error || error{ EmptyCode, MmapFailed, VirtualAllocFailed, MprotectFailed, VirtualProtectFailed, UnsupportedPlatform })!u64 {
    const allocator = std.testing.allocator;
    const compiled = try compileRoot(store, layout_store, root_proc, ret_layout);
    defer allocator.free(compiled.code);

    var executable = try ExecutableMemory.initWithEntryOffset(compiled.code, compiled.entry_offset);
    defer executable.deinit();

    var test_ops = TestRocOps.init(allocator);
    var out: u64 = undefined;
    const func: *const fn (*anyopaque, *anyopaque) callconv(.c) void = @ptrCast(@alignCast(executable.entryPtr()));
    func(@ptrCast(&out), @ptrCast(test_ops.getOps()));
    return out;
}

fn runRootU8(store: *LirStore, layout_store: *layout.Store, root_proc: lir.LIR.LirProcSpecId, ret_layout: layout.Idx) (Allocator.Error || error{ EmptyCode, MmapFailed, VirtualAllocFailed, MprotectFailed, VirtualProtectFailed, UnsupportedPlatform })!u8 {
    const allocator = std.testing.allocator;
    const compiled = try compileRoot(store, layout_store, root_proc, ret_layout);
    defer allocator.free(compiled.code);

    var executable = try ExecutableMemory.initWithEntryOffset(compiled.code, compiled.entry_offset);
    defer executable.deinit();

    var test_ops = TestRocOps.init(allocator);
    var out: u8 = undefined;
    const func: *const fn (*anyopaque, *anyopaque) callconv(.c) void = @ptrCast(@alignCast(executable.entryPtr()));
    func(@ptrCast(&out), @ptrCast(test_ops.getOps()));
    return out;
}

test "dev lowering: init_uninitialized writes poison pattern" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const value = try addLocal(&store, .u64);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = value } });
    const init_stmt = try store.addCFStmt(.{ .init_uninitialized = .{
        .target = value,
        .next = ret,
    } });
    const root_proc = try addNoArgProc(&store, init_stmt, .u64);

    try std.testing.expectEqual(@as(u64, 0xAAAAAAAAAAAAAAAA), try runRootU64(&store, &test_state.layout_store, root_proc, .u64));
}

fn stackOffsetOfTestLocation(loc: anytype) i32 {
    return switch (loc) {
        .stack => |s| s.offset,
        .list_stack => |s| s.struct_offset,
        .stack_i128, .stack_str => |off| off,
        else => unreachable,
    };
}

fn freshTestJoinPointId(next: *u32) lir.LIR.JoinPointId {
    const id: lir.LIR.JoinPointId = @enumFromInt(next.*);
    next.* += 1;
    return id;
}

test "code generator initialization" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, &.{});
    defer codegen.deinit();
}

test "proc params and mutable list cells use distinct stack slots" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const list_layout = try test_state.layout_store.insertLayout(layout.Layout.list(.u32));

    const start = try addLocal(&store, .u32);
    const end = try addLocal(&store, .u32);
    const answer = try addLocal(&store, list_layout);
    const one = try addLocal(&store, .u32);
    const appended = try addLocal(&store, list_layout);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = appended } });
    const append_args = try store.addLocalSpan(&.{ answer, one });
    const append_stmt = try store.addCFStmt(.{ .assign_low_level = .{
        .target = appended,
        .op = .list_append_unsafe,
        .rc_effect = lir.LowLevel.list_append_unsafe.rcEffect(),
        .args = append_args,
        .next = ret,
    } });
    const one_stmt = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u32 } },
        .next = append_stmt,
    } });
    const empty_elems = try store.addLocalSpan(&.{});
    const answer_stmt = try store.addCFStmt(.{ .assign_list = .{
        .target = answer,
        .elems = empty_elems,
        .next = one_stmt,
    } });
    const args = try store.addLocalSpan(&.{ start, end });

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, &.{});
    defer codegen.deinit();

    const HostCodeGen = @TypeOf(codegen.codegen);
    if (comptime builtin.cpu.arch == .aarch64) {
        codegen.codegen.stack_offset = 16 + HostCodeGen.CALLEE_SAVED_AREA_SIZE;
    } else {
        codegen.codegen.stack_offset = -HostCodeGen.CALLEE_SAVED_AREA_SIZE;
    }

    try codegen.bindProcParams(args, 0);
    try codegen.ensureStableLocationsForStmtLocals(answer_stmt);

    const end_slot = stackOffsetOfTestLocation(codegen.local_locations.get(@intFromEnum(end)).?);
    const answer_slot = stackOffsetOfTestLocation(codegen.local_locations.get(@intFromEnum(answer)).?);
    const appended_slot = stackOffsetOfTestLocation(codegen.local_locations.get(@intFromEnum(appended)).?);

    try std.testing.expect(answer_slot != end_slot);
    try std.testing.expect(appended_slot != end_slot);
    try std.testing.expect(appended_slot != answer_slot);
}

test "Windows internal proc ABI reads stack arguments after shadow space" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const WinCodeGen = LirCodeGen(.x64win);
    try std.testing.expectEqual(@as(i32, 48), WinCodeGen.incoming_stack_arg_base_offset);
    try std.testing.expectEqual(@as(i32, 32), WinCodeGen.outgoing_stack_arg_base_offset);

    const list_layout = try test_state.layout_store.insertLayout(layout.Layout.list(.u64));

    const a = try addLocal(&store, .u64);
    const b = try addLocal(&store, .u64);
    const c = try addLocal(&store, .u64);
    const d = try addLocal(&store, .u64);
    const list = try addLocal(&store, list_layout);
    const args = try store.addLocalSpan(&.{ a, b, c, d, list });

    var codegen = try WinCodeGen.init(allocator, &store, &test_state.layout_store, &.{});
    defer codegen.deinit();

    const InnerCodeGen = @TypeOf(codegen.codegen);
    codegen.codegen.stack_offset = -InnerCodeGen.CALLEE_SAVED_AREA_SIZE;

    try codegen.bindProcParams(args, 0);

    _ = codegen.local_locations.get(@intFromEnum(list)) orelse return error.TestUnexpectedResult;
    try std.testing.expect(codegen.codegen.getCode().len > 0);
}

test "AArch64 internal proc ABI uses caller stack arg base for stack arguments" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const ArmCodeGen = LirCodeGen(.arm64mac);
    try std.testing.expectEqual(@as(i32, 0), ArmCodeGen.incoming_stack_arg_base_offset);

    const a0 = try addLocal(&store, .u64);
    const a1 = try addLocal(&store, .u64);
    const a2 = try addLocal(&store, .u64);
    const a3 = try addLocal(&store, .u64);
    const a4 = try addLocal(&store, .u64);
    const a5 = try addLocal(&store, .u64);
    const a6 = try addLocal(&store, .u64);
    const a7 = try addLocal(&store, .u64);
    const stack_arg = try addLocal(&store, .u64);
    const args = try store.addLocalSpan(&.{ a0, a1, a2, a3, a4, a5, a6, a7, stack_arg });

    var codegen = try ArmCodeGen.init(allocator, &store, &test_state.layout_store, &.{});
    defer codegen.deinit();

    const InnerCodeGen = @TypeOf(codegen.codegen);
    codegen.codegen.stack_offset = 16 + InnerCodeGen.CALLEE_SAVED_AREA_SIZE;

    try codegen.bindProcParams(args, 0);

    const x28_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X28);
    try std.testing.expect(codegen.uses_caller_stack_arg_base);
    try std.testing.expect((codegen.codegen.callee_saved_used & x28_bit) != 0);
    try std.testing.expect((codegen.codegen.callee_saved_available & x28_bit) == 0);
    _ = codegen.local_locations.get(@intFromEnum(stack_arg)) orelse return error.TestUnexpectedResult;
    try std.testing.expect(codegen.codegen.getCode().len > 0);
}

test "AArch64 compare immediate accepts large bit masks" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const ArmCodeGen = LirCodeGen(.arm64mac);
    var codegen = try ArmCodeGen.init(allocator, &store, &test_state.layout_store, &.{});
    defer codegen.deinit();

    try codegen.emitCmpImm(aarch64.GeneralReg.X0, @bitCast(@as(u64, 1) << 63));
    try std.testing.expect(codegen.codegen.getCode().len > 4);
}

test "two-arg proc list join loop returns full length" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const list_layout = try test_state.layout_store.insertLayout(layout.Layout.list(.i64));

    const list_arg = try addLocal(&store, list_layout);
    const ignored_arg = try addLocal(&store, .i64);
    const len = try addLocal(&store, .u64);
    const index = try addLocal(&store, .u64);
    const acc = try addLocal(&store, .u64);
    const zero_index = try addLocal(&store, .u64);
    const zero_acc = try addLocal(&store, .u64);
    const one = try addLocal(&store, .u64);
    const cond = try addLocal(&store, .bool);
    const next_index = try addLocal(&store, .u64);
    const next_acc = try addLocal(&store, .u64);
    var next_join_point: u32 = 0;
    const join_id = freshTestJoinPointId(&next_join_point);

    const ret_acc = try store.addCFStmt(.{ .ret = .{ .value = acc } });
    const loop_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_acc = try store.addCFStmt(.{ .set_local = .{
        .target = acc,
        .value = next_acc,
        .mode = .initialize_join_param,
        .next = loop_jump,
    } });
    const set_index = try store.addCFStmt(.{ .set_local = .{
        .target = index,
        .value = next_index,
        .mode = .initialize_join_param,
        .next = set_acc,
    } });
    const next_index_args = try store.addLocalSpan(&.{ index, one });
    const add_next_index = try store.addCFStmt(.{ .assign_low_level = .{
        .target = next_index,
        .op = .num_plus,
        .rc_effect = lir.LowLevel.num_plus.rcEffect(),
        .args = next_index_args,
        .next = set_index,
    } });
    const next_acc_args = try store.addLocalSpan(&.{ acc, one });
    const add_next_acc = try store.addCFStmt(.{ .assign_low_level = .{
        .target = next_acc,
        .op = .num_plus,
        .rc_effect = lir.LowLevel.num_plus.rcEffect(),
        .args = next_acc_args,
        .next = add_next_index,
    } });
    const one_stmt = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = add_next_acc,
    } });
    const loop_branches = try store.addCFSwitchBranches(&.{.{
        .value = 1,
        .body = one_stmt,
    }});
    const loop_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = loop_branches,
        .default_branch = ret_acc,
    } });
    const cond_args = try store.addLocalSpan(&.{ index, len });
    const loop_body = try store.addCFStmt(.{ .assign_low_level = .{
        .target = cond,
        .op = .num_is_lt,
        .rc_effect = lir.LowLevel.num_is_lt.rcEffect(),
        .args = cond_args,
        .next = loop_switch,
    } });

    const initial_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const init_acc = try store.addCFStmt(.{ .set_local = .{
        .target = acc,
        .value = zero_acc,
        .mode = .initialize_join_param,
        .next = initial_jump,
    } });
    const init_index = try store.addCFStmt(.{ .set_local = .{
        .target = index,
        .value = zero_index,
        .mode = .initialize_join_param,
        .next = init_acc,
    } });
    const zero_acc_stmt = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero_acc,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = init_index,
    } });
    const zero_index_stmt = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero_index,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = zero_acc_stmt,
    } });
    const len_args = try store.addLocalSpan(&.{list_arg});
    const len_stmt = try store.addCFStmt(.{ .assign_low_level = .{
        .target = len,
        .op = .list_len,
        .rc_effect = lir.LowLevel.list_len.rcEffect(),
        .args = len_args,
        .next = zero_index_stmt,
    } });
    const join_params = try store.addLocalSpan(&.{ index, acc });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = join_params,
        .body = loop_body,
        .remainder = len_stmt,
    } });
    const len_proc = try addProc(&store, &.{ list_arg, ignored_arg }, join, .u64);

    const root_elems = [_]i64{ 10, 20, 30, 40, 50 };
    var elem_locals: [root_elems.len]LocalId = undefined;
    for (&elem_locals) |*local| local.* = try addLocal(&store, .i64);

    const root_list = try addLocal(&store, list_layout);
    const ignored = try addLocal(&store, .i64);
    const result = try addLocal(&store, .u64);

    const ret_result = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const drop_list = try store.addCFStmt(.{ .decref = .{
        .value = root_list,
        .rc = .{ .op = .decref, .layout_idx = list_layout },
        .next = ret_result,
    } });
    const call_args = try store.addLocalSpan(&.{ root_list, ignored });
    const call_len = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = len_proc,
        .args = call_args,
        .next = drop_list,
    } });
    const ignored_stmt = try store.addCFStmt(.{ .assign_literal = .{
        .target = ignored,
        .value = .{ .i64_literal = .{ .value = 123, .layout_idx = .i64 } },
        .next = call_len,
    } });
    const list_elems = try store.addLocalSpan(&elem_locals);
    const list_stmt = try store.addCFStmt(.{ .assign_list = .{
        .target = root_list,
        .elems = list_elems,
        .next = ignored_stmt,
    } });

    var current = list_stmt;
    var i: usize = root_elems.len;
    while (i > 0) {
        i -= 1;
        current = try store.addCFStmt(.{ .assign_literal = .{
            .target = elem_locals[i],
            .value = .{ .i64_literal = .{ .value = root_elems[i], .layout_idx = .i64 } },
            .next = current,
        } });
    }
    const root_proc = try addNoArgProc(&store, current, .u64);

    try std.testing.expectEqual(@as(u64, root_elems.len), try runRootU64(&store, &test_state.layout_store, root_proc, .u64));
}

test "ptr_alloca slot is zeroed and ptr_store/ptr_load round trip" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const ptr_u64 = try test_state.layout_store.insertPtr(.u64);

    const slot = try addLocal(&store, ptr_u64);
    const pre = try addLocal(&store, .u64);
    const v = try addLocal(&store, .u64);
    const st = try addLocal(&store, .zst);
    const post = try addLocal(&store, .u64);
    const sum = try addLocal(&store, .u64);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = sum } });
    const add_args = try store.addLocalSpan(&.{ pre, post });
    const add = try store.addCFStmt(.{ .assign_low_level = .{
        .target = sum,
        .op = .num_plus,
        .rc_effect = lir.LowLevel.num_plus.rcEffect(),
        .args = add_args,
        .next = ret,
    } });
    const load_post_args = try store.addLocalSpan(&.{slot});
    const load_post = try store.addCFStmt(.{ .assign_low_level = .{
        .target = post,
        .op = .ptr_load,
        .rc_effect = lir.LowLevel.ptr_load.rcEffect(),
        .args = load_post_args,
        .next = add,
    } });
    const store_args = try store.addLocalSpan(&.{ slot, v });
    const store_v = try store.addCFStmt(.{ .assign_low_level = .{
        .target = st,
        .op = .ptr_store,
        .rc_effect = lir.LowLevel.ptr_store.rcEffect(),
        .args = store_args,
        .next = load_post,
    } });
    const v_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = v,
        .value = .{ .i64_literal = .{ .value = 41, .layout_idx = .u64 } },
        .next = store_v,
    } });
    // Loading before any store proves the alloca slot was zero-initialized.
    const load_pre_args = try store.addLocalSpan(&.{slot});
    const load_pre = try store.addCFStmt(.{ .assign_low_level = .{
        .target = pre,
        .op = .ptr_load,
        .rc_effect = lir.LowLevel.ptr_load.rcEffect(),
        .args = load_pre_args,
        .next = v_lit,
    } });
    const alloca = try store.addCFStmt(.{ .assign_low_level = .{
        .target = slot,
        .op = .ptr_alloca,
        .rc_effect = lir.LowLevel.ptr_alloca.rcEffect(),
        .args = try store.addLocalSpan(&.{}),
        .next = load_pre,
    } });
    const root_proc = try addNoArgProc(&store, alloca, .u64);

    try std.testing.expectEqual(@as(u64, 41), try runRootU64(&store, &test_state.layout_store, root_proc, .u64));
}

test "box_alloc_zeroed cell is zeroed, writable through ptr_cast, and freed by decref" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const box_u64 = try test_state.layout_store.insertBox(.u64);
    const ptr_u64 = try test_state.layout_store.insertPtr(.u64);

    const cell = try addLocal(&store, box_u64);
    const p = try addLocal(&store, ptr_u64);
    const pre = try addLocal(&store, .u64);
    const v = try addLocal(&store, .u64);
    const st = try addLocal(&store, .zst);
    const post = try addLocal(&store, .u64);
    const sum = try addLocal(&store, .u64);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = sum } });
    // The testing allocator behind TestRocOps fails the test on leaks, so this
    // decref also verifies the cell came from allocateWithRefcount with rc=1.
    const drop_cell = try store.addCFStmt(.{ .decref = .{
        .value = cell,
        .rc = .{ .op = .decref, .layout_idx = box_u64 },
        .next = ret,
    } });
    const add_args = try store.addLocalSpan(&.{ pre, post });
    const add = try store.addCFStmt(.{ .assign_low_level = .{
        .target = sum,
        .op = .num_plus,
        .rc_effect = lir.LowLevel.num_plus.rcEffect(),
        .args = add_args,
        .next = drop_cell,
    } });
    const load_post_args = try store.addLocalSpan(&.{p});
    const load_post = try store.addCFStmt(.{ .assign_low_level = .{
        .target = post,
        .op = .ptr_load,
        .rc_effect = lir.LowLevel.ptr_load.rcEffect(),
        .args = load_post_args,
        .next = add,
    } });
    const store_args = try store.addLocalSpan(&.{ p, v });
    const store_v = try store.addCFStmt(.{ .assign_low_level = .{
        .target = st,
        .op = .ptr_store,
        .rc_effect = lir.LowLevel.ptr_store.rcEffect(),
        .args = store_args,
        .next = load_post,
    } });
    const v_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = v,
        .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .u64 } },
        .next = store_v,
    } });
    // Loading before any store proves the heap cell payload was zero-filled.
    const load_pre_args = try store.addLocalSpan(&.{p});
    const load_pre = try store.addCFStmt(.{ .assign_low_level = .{
        .target = pre,
        .op = .ptr_load,
        .rc_effect = lir.LowLevel.ptr_load.rcEffect(),
        .args = load_pre_args,
        .next = v_lit,
    } });
    const cast_args = try store.addLocalSpan(&.{cell});
    const cast = try store.addCFStmt(.{ .assign_low_level = .{
        .target = p,
        .op = .ptr_cast,
        .rc_effect = lir.LowLevel.ptr_cast.rcEffect(),
        .args = cast_args,
        .next = load_pre,
    } });
    const alloc = try store.addCFStmt(.{ .assign_low_level = .{
        .target = cell,
        .op = .box_alloc_zeroed,
        .rc_effect = lir.LowLevel.box_alloc_zeroed.rcEffect(),
        .args = try store.addLocalSpan(&.{}),
        .next = cast,
    } });
    const root_proc = try addNoArgProc(&store, alloc, .u64);

    try std.testing.expectEqual(@as(u64, 7), try runRootU64(&store, &test_state.layout_store, root_proc, .u64));
}

test "generate i64 literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const proc = try addLiteralProc(&store, .{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, .i64);
    try std.testing.expectEqual(@as(i64, 42), try runRootI64(&store, &test_state.layout_store, proc));
}

test "generate bool literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const result = try addLocal(&store, .bool);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const assign_true = try store.addCFStmt(.{ .assign_tag = .{
        .target = result,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = ret,
    } });
    const proc = try addNoArgProc(&store, assign_true, .bool);

    try std.testing.expectEqual(@as(u8, 1), try runRootU8(&store, &test_state.layout_store, proc, .bool));
}

test "tag payload bind invariant rejects mismatched pattern layout" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const tag_union_layout = try test_state.layout_store.putTagUnion(&.{.u64});
    const source = try addLocal(&store, tag_union_layout);
    const mismatched_target = try addLocal(&store, .u8);

    const tag_union = test_state.layout_store.getLayout(tag_union_layout);
    const variants = test_state.layout_store.getTagUnionVariants(
        test_state.layout_store.getTagUnionData(tag_union.getTagUnion().idx),
    );
    const runtime_payload_layout = variants.get(0).payload_layout;

    try std.testing.expectEqual(layout.Idx.u64, runtime_payload_layout);
    try std.testing.expect(runtime_payload_layout != store.getLocal(mismatched_target).layout_idx);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = mismatched_target } });
    _ = try store.addCFStmt(.{ .assign_ref = .{
        .target = mismatched_target,
        .op = .{ .tag_payload = .{
            .source = source,
            .payload_idx = 0,
            .variant_index = 0,
            .tag_discriminant = 0,
        } },
        .next = ret,
    } });
}

test "generate addition" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const proc = try addBinaryLowLevelProc(&store, .num_plus, 40, 2, .i64, .i64);
    try std.testing.expectEqual(@as(i64, 42), try runRootI64(&store, &test_state.layout_store, proc));
}

test "record equality uses layout-aware comparison" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const record_layout = try test_state.layout_store.putStructFields(&.{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .i64 },
    });
    const record_layout_val = test_state.layout_store.getLayout(record_layout);
    const record_idx = record_layout_val.getStruct().idx;
    const record_data = test_state.layout_store.getStructData(record_idx);
    const committed_fields = test_state.layout_store.struct_fields.sliceRange(record_data.getFields());
    try std.testing.expectEqual(layout.Idx.i64, committed_fields.get(0).layout);
    try std.testing.expectEqual(@as(u32, 8), test_state.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 0));
    try std.testing.expectEqual(@as(u32, 0), test_state.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 1));

    const lhs_small = try addLocal(&store, .u8);
    const lhs_large = try addLocal(&store, .i64);
    const rhs_small = try addLocal(&store, .u8);
    const rhs_large = try addLocal(&store, .i64);
    const lhs_record = try addLocal(&store, record_layout);
    const rhs_record = try addLocal(&store, record_layout);
    const eq_result = try addLocal(&store, .bool);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = eq_result } });
    const eq_args = try store.addLocalSpan(&.{ lhs_record, rhs_record });
    const eq_stmt = try store.addCFStmt(.{ .assign_low_level = .{
        .target = eq_result,
        .op = .num_is_eq,
        .rc_effect = lir.LowLevel.num_is_eq.rcEffect(),
        .args = eq_args,
        .next = ret,
    } });
    const rhs_fields = try store.addLocalSpan(&.{ rhs_small, rhs_large });
    const rhs_record_stmt = try store.addCFStmt(.{ .assign_struct = .{
        .target = rhs_record,
        .fields = rhs_fields,
        .next = eq_stmt,
    } });
    const lhs_fields = try store.addLocalSpan(&.{ lhs_small, lhs_large });
    const lhs_record_stmt = try store.addCFStmt(.{ .assign_struct = .{
        .target = lhs_record,
        .fields = lhs_fields,
        .next = rhs_record_stmt,
    } });
    const assign_rhs_large = try store.addCFStmt(.{ .assign_literal = .{
        .target = rhs_large,
        .value = .{ .i64_literal = .{ .value = 999, .layout_idx = .i64 } },
        .next = lhs_record_stmt,
    } });
    const assign_rhs_small = try store.addCFStmt(.{ .assign_literal = .{
        .target = rhs_small,
        .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .u8 } },
        .next = assign_rhs_large,
    } });
    const assign_lhs_large = try store.addCFStmt(.{ .assign_literal = .{
        .target = lhs_large,
        .value = .{ .i64_literal = .{ .value = 999, .layout_idx = .i64 } },
        .next = assign_rhs_small,
    } });
    const assign_lhs_small = try store.addCFStmt(.{ .assign_literal = .{
        .target = lhs_small,
        .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .u8 } },
        .next = assign_lhs_large,
    } });
    const proc = try addNoArgProc(&store, assign_lhs_small, .bool);

    try std.testing.expectEqual(@as(u8, 1), try runRootU8(&store, &test_state.layout_store, proc, .bool));
}

test "generate modulo" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const proc = try addBinaryLowLevelProc(&store, .num_mod_by, 10, 3, .i64, .i64);
    try std.testing.expectEqual(@as(i64, 1), try runRootI64(&store, &test_state.layout_store, proc));
}

test "generate shift left" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const proc = try addBinaryLowLevelProc(&store, .num_shift_left_by, 1, 4, .i64, .i64);
    try std.testing.expectEqual(@as(i64, 16), try runRootI64(&store, &test_state.layout_store, proc));
}

test "generate shift right" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const proc = try addBinaryLowLevelProc(&store, .num_shift_right_by, -8, 1, .i64, .i64);
    try std.testing.expectEqual(@as(i64, -4), try runRootI64(&store, &test_state.layout_store, proc));
}

test "generate shift right zero-fill" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const lhs_bits: i64 = @bitCast(@as(u64, 0x8000_0000_0000_0000));
    const proc = try addBinaryLowLevelProc(&store, .num_shift_right_zf_by, lhs_bits, 63, .u64, .u64);
    try std.testing.expectEqual(@as(u64, 1), try runRootU64(&store, &test_state.layout_store, proc, .u64));
}

test "generate unary minus" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const proc = try addUnaryLowLevelProc(&store, .num_negate, 42, .i64, .i64);
    try std.testing.expectEqual(@as(i64, -42), try runRootI64(&store, &test_state.layout_store, proc));
}

test "entrypoint arg offsets preserve Roc alignment order" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, &.{});
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
    var store = LirStore.init(allocator);
    defer store.deinit();

    var test_state = try TestLayoutState.init(allocator);
    defer test_state.deinit();

    const aggregate_layout = try test_state.layout_store.putTuple(&.{
        test_state.layout_store.getLayout(.f32),
        test_state.layout_store.getLayout(.f32),
        test_state.layout_store.getLayout(.f32),
    });

    var codegen = try HostLirCodeGen.init(allocator, &store, &test_state.layout_store, &.{});
    defer codegen.deinit();

    try std.testing.expectEqual(@as(u32, 16), codegen.entrypointParamSlotSize(aggregate_layout));
    try std.testing.expectEqual(@as(u32, 24), codegen.entrypointParamSlotSize(.str));
    try std.testing.expectEqual(@as(u32, 8), codegen.entrypointParamSlotSize(.bool));
}
