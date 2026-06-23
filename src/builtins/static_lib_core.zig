//! Core LLVM builtin bitcode for common string/list/refcount/debug operations.
//!
//! The full `static_lib.zig` payload includes decimal, float parsing/formatting,
//! and wide-integer helpers. LLVM builds link this smaller payload when the app
//! only declares roots from this explicit export set.

const std = @import("std");
const shim_io = @import("shim_io");

/// Builtin payloads must not pull in Zig's panic formatting machinery.
pub const panic = std.debug.no_panic;

/// Uses the same freestanding debug-info search path hook as the full builtins payload.
pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal debug output override; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Disables stack-trace capture; see `shim_io.std_options_no_stack_tracing`.
pub const std_options = shim_io.std_options_no_stack_tracing;

comptime {
    @import("num.zig").exportMulWithOverflow(i64, "roc__num_mul_with_overflow_");
    @import("num.zig").exportMulWithOverflow(i32, "roc__num_mul_with_overflow_");
    @import("num.zig").exportMulWithOverflow(i16, "roc__num_mul_with_overflow_");
    @import("num.zig").exportMulWithOverflow(i8, "roc__num_mul_with_overflow_");
    @import("num.zig").exportAddWithOverflow(i128, "roc__num_add_with_overflow_");
    @import("num.zig").exportSubWithOverflow(i128, "roc__num_sub_with_overflow_");
}

/// Wrapper namespace used by the explicit core export list below.
pub const dev_wrappers = @import("dev_wrappers.zig");

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
    @export(&dw.roc_builtins_list_eq, .{ .name = "roc_builtins_list_eq" });
    @export(&dw.roc_builtins_list_str_eq, .{ .name = "roc_builtins_list_str_eq" });
    @export(&dw.roc_builtins_list_list_eq, .{ .name = "roc_builtins_list_list_eq" });
    @export(&dw.roc_builtins_list_reverse, .{ .name = "roc_builtins_list_reverse" });

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
    @export(&dw.roc_builtins_int_to_str, .{ .name = "roc_builtins_int_to_str" });
    @export(&dw.roc_builtins_int_from_str, .{ .name = "roc_builtins_int_from_str" });

    @export(&dw.roc_builtins_i8_mod_by, .{ .name = "roc_builtins_i8_mod_by" });
    @export(&dw.roc_builtins_u8_mod_by, .{ .name = "roc_builtins_u8_mod_by" });
    @export(&dw.roc_builtins_i16_mod_by, .{ .name = "roc_builtins_i16_mod_by" });
    @export(&dw.roc_builtins_u16_mod_by, .{ .name = "roc_builtins_u16_mod_by" });
    @export(&dw.roc_builtins_i32_mod_by, .{ .name = "roc_builtins_i32_mod_by" });
    @export(&dw.roc_builtins_u32_mod_by, .{ .name = "roc_builtins_u32_mod_by" });
    @export(&dw.roc_builtins_i64_mod_by, .{ .name = "roc_builtins_i64_mod_by" });
    @export(&dw.roc_builtins_u64_mod_by, .{ .name = "roc_builtins_u64_mod_by" });
}
