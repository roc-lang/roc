//! Static library version of builtins that provides minimal exports
//! This is a separate entry point to avoid circular imports with builtins module
//!
//! This library provides:
//! - Numeric overflow functions (for compiler-rt)
//! - Dev backend wrapper functions (for roc build --opt=dev)

const std = @import("std");
const shim_io = @import("shim_io");

pub const panic = std.debug.no_panic;

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal debug output override; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Disables stack-trace capture; see `shim_io.std_options_no_stack_tracing`.
pub const std_options = shim_io.std_options_no_stack_tracing;

// Export key functions that might need compiler-rt symbols
comptime {
    // Export overflow functions that might need compiler-rt symbols
    @import("num.zig").exportMulWithOverflow(i64, "roc__num_mul_with_overflow_");
    @import("num.zig").exportMulWithOverflow(i32, "roc__num_mul_with_overflow_");
    @import("num.zig").exportMulWithOverflow(i16, "roc__num_mul_with_overflow_");
    @import("num.zig").exportMulWithOverflow(i8, "roc__num_mul_with_overflow_");

    // Export other core functions that might be needed
    @import("num.zig").exportAddWithOverflow(i128, "roc__num_add_with_overflow_");
    @import("num.zig").exportSubWithOverflow(i128, "roc__num_sub_with_overflow_");
}

// Export dev backend wrapper functions - these are used by `roc build --opt=dev`
// to call builtin functions via symbol references instead of direct function pointers.
pub const dev_wrappers = @import("dev_wrappers.zig");

// Force export all dev_wrappers functions with explicit @export
comptime {
    const dw = dev_wrappers;
    @export(&dw.roc_builtins_str_to_utf8, .{ .name = "roc_builtins_str_to_utf8" });
    @export(&dw.roc_builtins_str_concat, .{ .name = "roc_builtins_str_concat" });
    @export(&dw.roc_builtins_str_contains, .{ .name = "roc_builtins_str_contains" });
    @export(&dw.roc_builtins_str_starts_with, .{ .name = "roc_builtins_str_starts_with" });
    @export(&dw.roc_builtins_str_ends_with, .{ .name = "roc_builtins_str_ends_with" });
    @export(&dw.roc_builtins_str_equal, .{ .name = "roc_builtins_str_equal" });
    @export(&dw.roc_builtins_str_equal_static_small, .{ .name = "roc_builtins_str_equal_static_small" });
    @export(&dw.roc_builtins_str_static_small_word_eq, .{ .name = "roc_builtins_str_static_small_word_eq" });
    @export(&dw.roc_builtins_str_static_small_word_caseless_eq, .{ .name = "roc_builtins_str_static_small_word_caseless_eq" });
    @export(&dw.roc_builtins_str_count_utf8_bytes, .{ .name = "roc_builtins_str_count_utf8_bytes" });
    @export(&dw.roc_builtins_str_find_first, .{ .name = "roc_builtins_str_find_first" });
    @export(&dw.roc_builtins_str_drop_prefix_caseless_ascii, .{ .name = "roc_builtins_str_drop_prefix_caseless_ascii" });
    @export(&dw.roc_builtins_str_caseless_ascii_equals, .{ .name = "roc_builtins_str_caseless_ascii_equals" });
    @export(&dw.roc_builtins_str_repeat, .{ .name = "roc_builtins_str_repeat" });
    @export(&dw.roc_builtins_str_trim, .{ .name = "roc_builtins_str_trim" });
    @export(&dw.roc_builtins_str_trim_start, .{ .name = "roc_builtins_str_trim_start" });
    @export(&dw.roc_builtins_str_trim_end, .{ .name = "roc_builtins_str_trim_end" });
    @export(&dw.roc_builtins_str_split, .{ .name = "roc_builtins_str_split" });
    @export(&dw.roc_builtins_str_join_with, .{ .name = "roc_builtins_str_join_with" });
    @export(&dw.roc_builtins_str_reserve, .{ .name = "roc_builtins_str_reserve" });
    @export(&dw.roc_builtins_str_release_excess_capacity, .{ .name = "roc_builtins_str_release_excess_capacity" });
    @export(&dw.roc_builtins_str_with_capacity, .{ .name = "roc_builtins_str_with_capacity" });
    @export(&dw.roc_builtins_str_drop_prefix, .{ .name = "roc_builtins_str_drop_prefix" });
    @export(&dw.roc_builtins_str_drop_suffix, .{ .name = "roc_builtins_str_drop_suffix" });
    @export(&dw.roc_builtins_str_with_ascii_lowercased, .{ .name = "roc_builtins_str_with_ascii_lowercased" });
    @export(&dw.roc_builtins_str_with_ascii_uppercased, .{ .name = "roc_builtins_str_with_ascii_uppercased" });
    @export(&dw.roc_builtins_str_from_utf8_lossy, .{ .name = "roc_builtins_str_from_utf8_lossy" });
    @export(&dw.roc_builtins_str_from_utf8, .{ .name = "roc_builtins_str_from_utf8" });
    @export(&dw.roc_builtins_str_from_utf8_result, .{ .name = "roc_builtins_str_from_utf8_result" });
    @export(&dw.roc_builtins_str_from_utf8_parts, .{ .name = "roc_builtins_str_from_utf8_parts" });
    @export(&dw.roc_builtins_str_escape_and_quote, .{ .name = "roc_builtins_str_escape_and_quote" });
    @export(&dw.roc_builtins_dbg_str, .{ .name = "roc_builtins_dbg_str" });
    @export(&dw.roc_builtins_expect_err_str, .{ .name = "roc_builtins_expect_err_str" });
    @export(&dw.roc_builtins_roc_expect_failed, .{ .name = "roc_builtins_roc_expect_failed" });
    @export(&dw.roc_builtins_roc_crashed, .{ .name = "roc_builtins_roc_crashed" });
    @export(&dw.roc_builtins_str_from_literal, .{ .name = "roc_builtins_str_from_literal" });
    @export(&dw.roc_builtins_list_with_capacity, .{ .name = "roc_builtins_list_with_capacity" });
    @export(&dw.roc_builtins_list_append_unsafe, .{ .name = "roc_builtins_list_append_unsafe" });
    @export(&dw.roc_builtins_list_map_can_reuse, .{ .name = "roc_builtins_list_map_can_reuse" });
    @export(&dw.roc_builtins_list_concat, .{ .name = "roc_builtins_list_concat" });
    @export(&dw.roc_builtins_list_prepend, .{ .name = "roc_builtins_list_prepend" });
    @export(&dw.roc_builtins_list_sublist, .{ .name = "roc_builtins_list_sublist" });
    @export(&dw.roc_builtins_list_incref, .{ .name = "roc_builtins_list_incref" });
    @export(&dw.roc_builtins_list_incref_single_thread, .{ .name = "roc_builtins_list_incref_single_thread" });
    @export(&dw.roc_builtins_list_drop_at, .{ .name = "roc_builtins_list_drop_at" });
    @export(&dw.roc_builtins_list_replace, .{ .name = "roc_builtins_list_replace" });
    @export(&dw.roc_builtins_list_swap, .{ .name = "roc_builtins_list_swap" });
    @export(&dw.roc_builtins_list_reserve, .{ .name = "roc_builtins_list_reserve" });
    @export(&dw.roc_builtins_list_release_excess_capacity, .{ .name = "roc_builtins_list_release_excess_capacity" });
    @export(&dw.roc_builtins_list_decref_str, .{ .name = "roc_builtins_list_decref_str" });
    @export(&dw.roc_builtins_list_decref_flat_list, .{ .name = "roc_builtins_list_decref_flat_list" });
    @export(&dw.roc_builtins_list_decref_with, .{ .name = "roc_builtins_list_decref_with" });
    @export(&dw.roc_builtins_list_decref_with_single_thread, .{ .name = "roc_builtins_list_decref_with_single_thread" });
    @export(&dw.roc_builtins_list_free_flat_list, .{ .name = "roc_builtins_list_free_flat_list" });
    @export(&dw.roc_builtins_list_free_with, .{ .name = "roc_builtins_list_free_with" });
    @export(&dw.roc_builtins_box_decref_with, .{ .name = "roc_builtins_box_decref_with" });
    @export(&dw.roc_builtins_box_decref_with_single_thread, .{ .name = "roc_builtins_box_decref_with_single_thread" });
    @export(&dw.roc_builtins_box_free_with, .{ .name = "roc_builtins_box_free_with" });
    @export(&dw.roc_builtins_erased_callable_incref, .{ .name = "roc_builtins_erased_callable_incref" });
    @export(&dw.roc_builtins_erased_callable_decref, .{ .name = "roc_builtins_erased_callable_decref" });
    @export(&dw.roc_builtins_erased_callable_decref_single_thread, .{ .name = "roc_builtins_erased_callable_decref_single_thread" });
    @export(&dw.roc_builtins_erased_callable_free, .{ .name = "roc_builtins_erased_callable_free" });
    @export(&dw.roc_builtins_allocate_with_refcount, .{ .name = "roc_builtins_allocate_with_refcount" });
    @export(&dw.roc_builtins_incref_data_ptr, .{ .name = "roc_builtins_incref_data_ptr" });
    @export(&dw.roc_builtins_incref_data_ptr_single_thread, .{ .name = "roc_builtins_incref_data_ptr_single_thread" });
    @export(&dw.roc_builtins_decref_data_ptr, .{ .name = "roc_builtins_decref_data_ptr" });
    @export(&dw.roc_builtins_decref_data_ptr_single_thread, .{ .name = "roc_builtins_decref_data_ptr_single_thread" });
    @export(&dw.roc_builtins_free_data_ptr, .{ .name = "roc_builtins_free_data_ptr" });
    @export(&dw.roc_builtins_dec_to_str, .{ .name = "roc_builtins_dec_to_str" });
    // Numeric conversion wrappers
    @export(&dw.roc_builtins_dec_to_i64_trunc, .{ .name = "roc_builtins_dec_to_i64_trunc" });
    @export(&dw.roc_builtins_i64_to_dec, .{ .name = "roc_builtins_i64_to_dec" });
    @export(&dw.roc_builtins_u64_to_dec, .{ .name = "roc_builtins_u64_to_dec" });
    @export(&dw.roc_builtins_dec_to_f64, .{ .name = "roc_builtins_dec_to_f64" });
    @export(&dw.roc_builtins_i128_to_f64, .{ .name = "roc_builtins_i128_to_f64" });
    @export(&dw.roc_builtins_u128_to_f64, .{ .name = "roc_builtins_u128_to_f64" });
    @export(&dw.roc_builtins_f64_to_i128_trunc, .{ .name = "roc_builtins_f64_to_i128_trunc" });
    @export(&dw.roc_builtins_f64_to_u128_trunc, .{ .name = "roc_builtins_f64_to_u128_trunc" });
    // Try-conversion wrappers
    @export(&dw.roc_builtins_i128_try_convert, .{ .name = "roc_builtins_i128_try_convert" });
    @export(&dw.roc_builtins_u128_try_convert, .{ .name = "roc_builtins_u128_try_convert" });
    @export(&dw.roc_builtins_int_try_signed, .{ .name = "roc_builtins_int_try_signed" });
    @export(&dw.roc_builtins_int_try_unsigned, .{ .name = "roc_builtins_int_try_unsigned" });
    @export(&dw.roc_builtins_dec_to_int_try_unsafe, .{ .name = "roc_builtins_dec_to_int_try_unsafe" });
    @export(&dw.roc_builtins_f64_to_int_try_unsafe, .{ .name = "roc_builtins_f64_to_int_try_unsafe" });
    @export(&dw.roc_builtins_dec_to_f32_try_unsafe, .{ .name = "roc_builtins_dec_to_f32_try_unsafe" });
    @export(&dw.roc_builtins_f64_to_f32_try_unsafe, .{ .name = "roc_builtins_f64_to_f32_try_unsafe" });
    @export(&dw.roc_builtins_i128_to_dec_try_unsafe, .{ .name = "roc_builtins_i128_to_dec_try_unsafe" });
    @export(&dw.roc_builtins_u128_to_dec_try_unsafe, .{ .name = "roc_builtins_u128_to_dec_try_unsafe" });
    // Dec arithmetic wrappers
    @export(&dw.roc_builtins_dec_mul, .{ .name = "roc_builtins_dec_mul" });
    @export(&dw.roc_builtins_dec_mul_saturated, .{ .name = "roc_builtins_dec_mul_saturated" });
    @export(&dw.roc_builtins_dec_div, .{ .name = "roc_builtins_dec_div" });
    @export(&dw.roc_builtins_dec_div_trunc, .{ .name = "roc_builtins_dec_div_trunc" });
    @export(&dw.roc_builtins_dec_pow, .{ .name = "roc_builtins_dec_pow" });
    @export(&dw.roc_builtins_dec_sqrt, .{ .name = "roc_builtins_dec_sqrt" });
    @export(&dw.roc_builtins_dec_sin, .{ .name = "roc_builtins_dec_sin" });
    @export(&dw.roc_builtins_dec_cos, .{ .name = "roc_builtins_dec_cos" });
    @export(&dw.roc_builtins_dec_tan, .{ .name = "roc_builtins_dec_tan" });
    @export(&dw.roc_builtins_dec_asin, .{ .name = "roc_builtins_dec_asin" });
    @export(&dw.roc_builtins_dec_acos, .{ .name = "roc_builtins_dec_acos" });
    @export(&dw.roc_builtins_dec_atan, .{ .name = "roc_builtins_dec_atan" });
    // i128 div/rem wrappers
    @export(&dw.roc_builtins_num_div_trunc_u128, .{ .name = "roc_builtins_num_div_trunc_u128" });
    @export(&dw.roc_builtins_num_div_trunc_i128, .{ .name = "roc_builtins_num_div_trunc_i128" });
    @export(&dw.roc_builtins_num_rem_trunc_u128, .{ .name = "roc_builtins_num_rem_trunc_u128" });
    @export(&dw.roc_builtins_num_rem_trunc_i128, .{ .name = "roc_builtins_num_rem_trunc_i128" });
    // Numeric-to-string wrappers
    @export(&dw.roc_builtins_int_to_str, .{ .name = "roc_builtins_int_to_str" });
    @export(&dw.roc_builtins_float_to_str, .{ .name = "roc_builtins_float_to_str" });
    @export(&dw.roc_builtins_float_pow, .{ .name = "roc_builtins_float_pow" });
    @export(&dw.roc_builtins_float_sin, .{ .name = "roc_builtins_float_sin" });
    @export(&dw.roc_builtins_float_cos, .{ .name = "roc_builtins_float_cos" });
    @export(&dw.roc_builtins_float_tan, .{ .name = "roc_builtins_float_tan" });
    @export(&dw.roc_builtins_float_asin, .{ .name = "roc_builtins_float_asin" });
    @export(&dw.roc_builtins_float_acos, .{ .name = "roc_builtins_float_acos" });
    @export(&dw.roc_builtins_float_atan, .{ .name = "roc_builtins_float_atan" });
    // Numeric-from-string wrapper
    @export(&dw.roc_builtins_int_from_str, .{ .name = "roc_builtins_int_from_str" });
    @export(&dw.roc_builtins_dec_from_str, .{ .name = "roc_builtins_dec_from_str" });
    @export(&dw.roc_builtins_float_from_str, .{ .name = "roc_builtins_float_from_str" });
    // List equality and reverse wrappers
    @export(&dw.roc_builtins_list_eq, .{ .name = "roc_builtins_list_eq" });
    @export(&dw.roc_builtins_list_str_eq, .{ .name = "roc_builtins_list_str_eq" });
    @export(&dw.roc_builtins_list_list_eq, .{ .name = "roc_builtins_list_list_eq" });
    @export(&dw.roc_builtins_list_reverse, .{ .name = "roc_builtins_list_reverse" });
    // Integer modulo wrappers
    @export(&dw.roc_builtins_i8_mod_by, .{ .name = "roc_builtins_i8_mod_by" });
    @export(&dw.roc_builtins_u8_mod_by, .{ .name = "roc_builtins_u8_mod_by" });
    @export(&dw.roc_builtins_i16_mod_by, .{ .name = "roc_builtins_i16_mod_by" });
    @export(&dw.roc_builtins_u16_mod_by, .{ .name = "roc_builtins_u16_mod_by" });
    @export(&dw.roc_builtins_i32_mod_by, .{ .name = "roc_builtins_i32_mod_by" });
    @export(&dw.roc_builtins_u32_mod_by, .{ .name = "roc_builtins_u32_mod_by" });
    @export(&dw.roc_builtins_i64_mod_by, .{ .name = "roc_builtins_i64_mod_by" });
    @export(&dw.roc_builtins_u64_mod_by, .{ .name = "roc_builtins_u64_mod_by" });
    // Hasher wrappers
    @export(&dw.roc_builtins_dict_pseudo_seed, .{ .name = "roc_builtins_dict_pseudo_seed" });
    @export(&dw.roc_builtins_hasher_finish, .{ .name = "roc_builtins_hasher_finish" });
    @export(&dw.roc_builtins_hasher_write_u64, .{ .name = "roc_builtins_hasher_write_u64" });
    @export(&dw.roc_builtins_hasher_write_u128, .{ .name = "roc_builtins_hasher_write_u128" });
    @export(&dw.roc_builtins_hasher_write_f32_bits, .{ .name = "roc_builtins_hasher_write_f32_bits" });
    @export(&dw.roc_builtins_hasher_write_f64_bits, .{ .name = "roc_builtins_hasher_write_f64_bits" });
    @export(&dw.roc_builtins_hasher_write_bytes, .{ .name = "roc_builtins_hasher_write_bytes" });
    @export(&dw.roc_builtins_hasher_write_str, .{ .name = "roc_builtins_hasher_write_str" });
    // Crypto wrappers
    @export(&dw.roc_builtins_crypto_sha256_hash_bytes, .{ .name = "roc_builtins_crypto_sha256_hash_bytes" });
    @export(&dw.roc_builtins_crypto_sha256_hasher_empty, .{ .name = "roc_builtins_crypto_sha256_hasher_empty" });
    @export(&dw.roc_builtins_crypto_sha256_hasher_write, .{ .name = "roc_builtins_crypto_sha256_hasher_write" });
    @export(&dw.roc_builtins_crypto_sha256_hasher_finish, .{ .name = "roc_builtins_crypto_sha256_hasher_finish" });
    @export(&dw.roc_builtins_crypto_blake3_hash_bytes, .{ .name = "roc_builtins_crypto_blake3_hash_bytes" });
    @export(&dw.roc_builtins_crypto_blake3_hasher_empty, .{ .name = "roc_builtins_crypto_blake3_hasher_empty" });
    @export(&dw.roc_builtins_crypto_blake3_hasher_write, .{ .name = "roc_builtins_crypto_blake3_hasher_write" });
    @export(&dw.roc_builtins_crypto_blake3_hasher_finish, .{ .name = "roc_builtins_crypto_blake3_hasher_finish" });
}
