//! Static library version of builtins that provides minimal exports
//! This is a separate entry point to avoid circular imports with builtins module
//!
//! This library provides:
//! - Numeric overflow functions (for compiler-rt)
//! - Dev backend wrapper functions (for roc build --backend=dev)

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

// Export dev backend wrapper functions - these are used by `roc build --backend=dev`
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
    @export(&dw.roc_builtins_str_is_empty, .{ .name = "roc_builtins_str_is_empty" });
    @export(&dw.roc_builtins_str_equal, .{ .name = "roc_builtins_str_equal" });
    @export(&dw.roc_builtins_str_count_utf8_bytes, .{ .name = "roc_builtins_str_count_utf8_bytes" });
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
    @export(&dw.roc_builtins_str_with_prefix, .{ .name = "roc_builtins_str_with_prefix" });
    @export(&dw.roc_builtins_str_from_utf8_lossy, .{ .name = "roc_builtins_str_from_utf8_lossy" });
    @export(&dw.roc_builtins_str_escape_and_quote, .{ .name = "roc_builtins_str_escape_and_quote" });
    @export(&dw.roc_builtins_list_with_capacity, .{ .name = "roc_builtins_list_with_capacity" });
    @export(&dw.roc_builtins_list_append_unsafe, .{ .name = "roc_builtins_list_append_unsafe" });
    @export(&dw.roc_builtins_list_concat, .{ .name = "roc_builtins_list_concat" });
    @export(&dw.roc_builtins_list_prepend, .{ .name = "roc_builtins_list_prepend" });
    @export(&dw.roc_builtins_list_sublist, .{ .name = "roc_builtins_list_sublist" });
    @export(&dw.roc_builtins_list_replace, .{ .name = "roc_builtins_list_replace" });
    @export(&dw.roc_builtins_list_reserve, .{ .name = "roc_builtins_list_reserve" });
    @export(&dw.roc_builtins_list_release_excess_capacity, .{ .name = "roc_builtins_list_release_excess_capacity" });
    @export(&dw.roc_builtins_allocate_with_refcount, .{ .name = "roc_builtins_allocate_with_refcount" });
    @export(&dw.roc_builtins_incref_data_ptr, .{ .name = "roc_builtins_incref_data_ptr" });
    @export(&dw.roc_builtins_decref_data_ptr, .{ .name = "roc_builtins_decref_data_ptr" });
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
    @export(&dw.roc_builtins_dec_mul_saturated, .{ .name = "roc_builtins_dec_mul_saturated" });
    @export(&dw.roc_builtins_dec_div, .{ .name = "roc_builtins_dec_div" });
    @export(&dw.roc_builtins_dec_div_trunc, .{ .name = "roc_builtins_dec_div_trunc" });
    // i128 div/rem wrappers
    @export(&dw.roc_builtins_num_div_trunc_u128, .{ .name = "roc_builtins_num_div_trunc_u128" });
    @export(&dw.roc_builtins_num_div_trunc_i128, .{ .name = "roc_builtins_num_div_trunc_i128" });
    @export(&dw.roc_builtins_num_rem_trunc_u128, .{ .name = "roc_builtins_num_rem_trunc_u128" });
    @export(&dw.roc_builtins_num_rem_trunc_i128, .{ .name = "roc_builtins_num_rem_trunc_i128" });
    // List append safe wrapper
    @export(&dw.roc_builtins_list_append_safe, .{ .name = "roc_builtins_list_append_safe" });
    // Numeric-to-string wrappers
    @export(&dw.roc_builtins_int_to_str, .{ .name = "roc_builtins_int_to_str" });
    @export(&dw.roc_builtins_float_to_str, .{ .name = "roc_builtins_float_to_str" });
}

// Ensure ___muloti4 symbol is available by using @mulWithOverflow
export fn force_muloti4_symbol() i128 {
    const a: i128 = 123456789;
    const b: i128 = 987654321;
    const result = @mulWithOverflow(a, b);
    return result[0];
}
