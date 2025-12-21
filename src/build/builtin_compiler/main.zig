//! Build-time compiler for Roc builtin module (Builtin.roc).
//!
//! This executable runs during `zig build` on the host machine to:
//! 1. Parse and type-check the Builtin.roc module (which contains nested Bool, Try, Str, Dict, Set types)
//! 2. Serialize the resulting ModuleEnv to a binary file
//! 3. Output Builtin.bin to zig-out/builtins/ (which gets embedded in the roc binary)

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const types = @import("types");
const reporting = @import("reporting");

const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const Check = check.Check;
const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const Content = types.Content;
const Var = types.Var;

const max_builtin_bytes = 1024 * 1024;

// Stderr writer for diagnostic reporting
var stderr_buffer: [4096]u8 = undefined;
var stderr_writer: std.fs.File.Writer = undefined;
var stderr_initialized = false;

fn stderrWriter() *std.Io.Writer {
    if (!stderr_initialized) {
        stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
        stderr_initialized = true;
    }
    return &stderr_writer.interface;
}

fn flushStderr() void {
    if (stderr_initialized) {
        stderr_writer.interface.flush() catch {};
    }
}

// Use the canonical BuiltinIndices from CIR
const BuiltinIndices = CIR.BuiltinIndices;

/// Replace specific e_anno_only expressions with e_low_level_lambda operations.
/// This transforms standalone annotations into low-level builtin lambda operations
/// that will be recognized by the compiler backend.
/// Returns a list of new def indices created.
fn replaceStrIsEmptyWithLowLevel(env: *ModuleEnv) !std.ArrayList(CIR.Def.Idx) {
    const gpa = env.gpa;
    var new_def_indices = std.ArrayList(CIR.Def.Idx).empty;

    // Ensure types array has entries for all existing nodes
    // This is necessary because varFrom(node_idx) assumes type_var index == node index
    const current_nodes = env.store.nodes.len();
    const current_types = env.types.len();
    if (current_types < current_nodes) {
        // Fill the gap with fresh type variables
        var i: u64 = current_types;
        while (i < current_nodes) : (i += 1) {
            _ = env.types.fresh() catch unreachable;
        }
    }

    // Build a hashmap of (qualified name -> low-level operation)
    var low_level_map = std.AutoHashMap(base.Ident.Idx, CIR.Expr.LowLevel).init(gpa);
    defer low_level_map.deinit();

    // Add all low-level operations to the map using full qualified names
    // Associated items are stored as defs with qualified names like "Builtin.Str.is_empty"
    // We need to find the actual ident that was created during canonicalization
    if (env.common.findIdent("Builtin.Str.is_empty")) |str_is_empty_ident| {
        try low_level_map.put(str_is_empty_ident, .str_is_empty);
    }
    if (env.common.findIdent("Builtin.Str.is_eq")) |str_is_eq_ident| {
        try low_level_map.put(str_is_eq_ident, .str_is_eq);
    }
    if (env.common.findIdent("Builtin.Str.concat")) |str_concat_ident| {
        try low_level_map.put(str_concat_ident, .str_concat);
    }
    if (env.common.findIdent("Builtin.Str.contains")) |str_contains_ident| {
        try low_level_map.put(str_contains_ident, .str_contains);
    }
    if (env.common.findIdent("Builtin.Str.trim")) |str_trim_ident| {
        try low_level_map.put(str_trim_ident, .str_trim);
    }
    if (env.common.findIdent("Builtin.Str.trim_start")) |str_trim_start_ident| {
        try low_level_map.put(str_trim_start_ident, .str_trim_start);
    }
    if (env.common.findIdent("Builtin.Str.trim_end")) |str_trim_end_ident| {
        try low_level_map.put(str_trim_end_ident, .str_trim_end);
    }
    if (env.common.findIdent("Builtin.Str.caseless_ascii_equals")) |str_caseless_ascii_equals_ident| {
        try low_level_map.put(str_caseless_ascii_equals_ident, .str_caseless_ascii_equals);
    }
    if (env.common.findIdent("Builtin.Str.with_ascii_lowercased")) |str_with_ascii_lowercased_ident| {
        try low_level_map.put(str_with_ascii_lowercased_ident, .str_with_ascii_lowercased);
    }
    if (env.common.findIdent("Builtin.Str.with_ascii_uppercased")) |str_with_ascii_uppercased_ident| {
        try low_level_map.put(str_with_ascii_uppercased_ident, .str_with_ascii_uppercased);
    }
    if (env.common.findIdent("Builtin.Str.starts_with")) |str_starts_with_ident| {
        try low_level_map.put(str_starts_with_ident, .str_starts_with);
    }
    if (env.common.findIdent("Builtin.Str.ends_with")) |str_ends_with_ident| {
        try low_level_map.put(str_ends_with_ident, .str_ends_with);
    }
    if (env.common.findIdent("Builtin.Str.repeat")) |str_repeat_ident| {
        try low_level_map.put(str_repeat_ident, .str_repeat);
    }
    if (env.common.findIdent("Builtin.Str.with_prefix")) |str_with_prefix_ident| {
        try low_level_map.put(str_with_prefix_ident, .str_with_prefix);
    }
    if (env.common.findIdent("Builtin.Str.drop_prefix")) |str_drop_prefix_ident| {
        try low_level_map.put(str_drop_prefix_ident, .str_drop_prefix);
    }
    if (env.common.findIdent("Builtin.Str.drop_suffix")) |str_drop_suffix_ident| {
        try low_level_map.put(str_drop_suffix_ident, .str_drop_suffix);
    }
    if (env.common.findIdent("Builtin.Str.count_utf8_bytes")) |str_count_utf8_bytes_ident| {
        try low_level_map.put(str_count_utf8_bytes_ident, .str_count_utf8_bytes);
    }
    if (env.common.findIdent("Builtin.Str.with_capacity")) |str_with_capacity_ident| {
        try low_level_map.put(str_with_capacity_ident, .str_with_capacity);
    }
    if (env.common.findIdent("Builtin.Str.reserve")) |str_reserve_ident| {
        try low_level_map.put(str_reserve_ident, .str_reserve);
    }
    if (env.common.findIdent("Builtin.Str.release_excess_capacity")) |str_release_excess_capacity_ident| {
        try low_level_map.put(str_release_excess_capacity_ident, .str_release_excess_capacity);
    }
    if (env.common.findIdent("Builtin.Str.to_utf8")) |str_to_utf8_ident| {
        try low_level_map.put(str_to_utf8_ident, .str_to_utf8);
    }
    if (env.common.findIdent("Builtin.Str.from_utf8_lossy")) |str_from_utf8_lossy_ident| {
        try low_level_map.put(str_from_utf8_lossy_ident, .str_from_utf8_lossy);
    }
    if (env.common.findIdent("Builtin.Str.from_utf8")) |str_from_utf8_ident| {
        try low_level_map.put(str_from_utf8_ident, .str_from_utf8);
    }
    if (env.common.findIdent("Builtin.Str.split_on")) |str_split_on_ident| {
        try low_level_map.put(str_split_on_ident, .str_split_on);
    }
    if (env.common.findIdent("Builtin.Str.join_with")) |str_join_with_ident| {
        try low_level_map.put(str_join_with_ident, .str_join_with);
    }
    if (env.common.findIdent("Builtin.Str.inspect")) |str_inspekt_ident| {
        try low_level_map.put(str_inspekt_ident, .str_inspekt);
    }
    if (env.common.findIdent("Builtin.List.len")) |list_len_ident| {
        try low_level_map.put(list_len_ident, .list_len);
    }
    if (env.common.findIdent("Builtin.List.is_empty")) |list_is_empty_ident| {
        try low_level_map.put(list_is_empty_ident, .list_is_empty);
    }
    if (env.common.findIdent("Builtin.List.concat")) |list_concat_ident| {
        try low_level_map.put(list_concat_ident, .list_concat);
    }
    if (env.common.findIdent("Builtin.List.append")) |list_append_ident| {
        try low_level_map.put(list_append_ident, .list_append);
    }
    if (env.common.findIdent("Builtin.List.with_capacity")) |list_with_capacity_ident| {
        try low_level_map.put(list_with_capacity_ident, .list_with_capacity);
    }
    if (env.common.findIdent("Builtin.List.sort_with")) |list_sort_with_ident| {
        try low_level_map.put(list_sort_with_ident, .list_sort_with);
    }
    if (env.common.findIdent("list_get_unsafe")) |list_get_unsafe_ident| {
        try low_level_map.put(list_get_unsafe_ident, .list_get_unsafe);
    }
    if (env.common.findIdent("list_append_unsafe")) |list_append_unsafe_ident| {
        try low_level_map.put(list_append_unsafe_ident, .list_append_unsafe);
    }
    if (env.common.findIdent("Builtin.List.drop_at")) |list_drop_at_ident| {
        try low_level_map.put(list_drop_at_ident, .list_drop_at);
    }
    if (env.common.findIdent("Builtin.List.sublist")) |list_sublist_ident| {
        try low_level_map.put(list_sublist_ident, .list_sublist);
    }
    if (env.common.findIdent("Builtin.Bool.is_eq")) |bool_is_eq_ident| {
        try low_level_map.put(bool_is_eq_ident, .bool_is_eq);
    }

    // Numeric type checking operations (all numeric types)
    const numeric_types = [_][]const u8{ "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "Dec", "F32", "F64" };
    for (numeric_types) |num_type| {
        var buf: [256]u8 = undefined;

        // is_zero (all types)
        const is_zero = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.is_zero", .{num_type});
        if (env.common.findIdent(is_zero)) |ident| {
            try low_level_map.put(ident, .num_is_zero);
        }
    }

    // Numeric sign checking operations (signed types only)
    const signed_types = [_][]const u8{ "I8", "I16", "I32", "I64", "I128", "Dec", "F32", "F64" };
    for (signed_types) |num_type| {
        var buf: [256]u8 = undefined;

        // is_negative
        const is_negative = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.is_negative", .{num_type});
        if (env.common.findIdent(is_negative)) |ident| {
            try low_level_map.put(ident, .num_is_negative);
        }

        // is_positive
        const is_positive = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.is_positive", .{num_type});
        if (env.common.findIdent(is_positive)) |ident| {
            try low_level_map.put(ident, .num_is_positive);
        }
    }

    // Numeric equality operations (integer types + Dec only, NOT F32/F64)
    const eq_types = [_][]const u8{ "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "Dec" };
    for (eq_types) |num_type| {
        var buf: [256]u8 = undefined;

        // is_eq
        const is_eq = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.is_eq", .{num_type});
        if (env.common.findIdent(is_eq)) |ident| {
            try low_level_map.put(ident, .num_is_eq);
        }
    }

    // Numeric to_str operations (all numeric types)
    // Note: Types like Dec are nested under Num in Builtin.roc, so the canonical identifier is
    // "Builtin.Num.Dec.to_str". But Dec is auto-imported as "Dec", so user code
    // calling Dec.to_str looks up "Builtin.Dec.to_str". We need the canonical name here.
    for (numeric_types) |num_type| {
        var buf: [256]u8 = undefined;
        const to_str_name = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.to_str", .{num_type});
        if (env.common.findIdent(to_str_name)) |ident| {
            const low_level_op: CIR.Expr.LowLevel = if (std.mem.eql(u8, num_type, "U8"))
                .u8_to_str
            else if (std.mem.eql(u8, num_type, "I8"))
                .i8_to_str
            else if (std.mem.eql(u8, num_type, "U16"))
                .u16_to_str
            else if (std.mem.eql(u8, num_type, "I16"))
                .i16_to_str
            else if (std.mem.eql(u8, num_type, "U32"))
                .u32_to_str
            else if (std.mem.eql(u8, num_type, "I32"))
                .i32_to_str
            else if (std.mem.eql(u8, num_type, "U64"))
                .u64_to_str
            else if (std.mem.eql(u8, num_type, "I64"))
                .i64_to_str
            else if (std.mem.eql(u8, num_type, "U128"))
                .u128_to_str
            else if (std.mem.eql(u8, num_type, "I128"))
                .i128_to_str
            else if (std.mem.eql(u8, num_type, "Dec"))
                .dec_to_str
            else if (std.mem.eql(u8, num_type, "F32"))
                .f32_to_str
            else if (std.mem.eql(u8, num_type, "F64"))
                .f64_to_str
            else
                continue;
            try low_level_map.put(ident, low_level_op);
        }
    }

    // Numeric comparison operations (all numeric types)
    for (numeric_types) |num_type| {
        var buf: [256]u8 = undefined;

        // is_gt
        const is_gt = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.is_gt", .{num_type});
        if (env.common.findIdent(is_gt)) |ident| {
            try low_level_map.put(ident, .num_is_gt);
        }

        // is_gte
        const is_gte = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.is_gte", .{num_type});
        if (env.common.findIdent(is_gte)) |ident| {
            try low_level_map.put(ident, .num_is_gte);
        }

        // is_lt
        const is_lt = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.is_lt", .{num_type});
        if (env.common.findIdent(is_lt)) |ident| {
            try low_level_map.put(ident, .num_is_lt);
        }

        // is_lte
        const is_lte = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.is_lte", .{num_type});
        if (env.common.findIdent(is_lte)) |ident| {
            try low_level_map.put(ident, .num_is_lte);
        }
    }

    // Numeric parsing operations (all numeric types have from_int_digits)
    for (numeric_types) |num_type| {
        var buf: [256]u8 = undefined;

        // from_int_digits
        const from_int_digits = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.from_int_digits", .{num_type});
        if (env.common.findIdent(from_int_digits)) |ident| {
            try low_level_map.put(ident, .num_from_int_digits);
        }
    }

    // from_dec_digits (Dec, F32, F64 only)
    const dec_types = [_][]const u8{ "Dec", "F32", "F64" };
    for (dec_types) |num_type| {
        var buf: [256]u8 = undefined;

        // from_dec_digits
        const from_dec_digits = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.from_dec_digits", .{num_type});
        if (env.common.findIdent(from_dec_digits)) |ident| {
            try low_level_map.put(ident, .num_from_dec_digits);
        }
    }

    // from_numeral (all numeric types)
    for (numeric_types) |num_type| {
        var buf: [256]u8 = undefined;

        const from_numeral = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.from_numeral", .{num_type});
        if (env.common.findIdent(from_numeral)) |ident| {
            try low_level_map.put(ident, .num_from_numeral);
        }
    }

    // from_str (all numeric types)
    for (numeric_types) |num_type| {
        var buf: [256]u8 = undefined;

        const from_str = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.from_str", .{num_type});
        if (env.common.findIdent(from_str)) |ident| {
            try low_level_map.put(ident, .num_from_str);
        }
    }

    // Numeric arithmetic operations (all numeric types have plus, minus, times, div_by, rem_by)
    for (numeric_types) |num_type| {
        var buf: [256]u8 = undefined;

        // plus
        const plus = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.plus", .{num_type});
        if (env.common.findIdent(plus)) |ident| {
            try low_level_map.put(ident, .num_plus);
        }

        // minus
        const minus = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.minus", .{num_type});
        if (env.common.findIdent(minus)) |ident| {
            try low_level_map.put(ident, .num_minus);
        }

        // times
        const times = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.times", .{num_type});
        if (env.common.findIdent(times)) |ident| {
            try low_level_map.put(ident, .num_times);
        }

        // div_by
        const div_by = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.div_by", .{num_type});
        if (env.common.findIdent(div_by)) |ident| {
            try low_level_map.put(ident, .num_div_by);
        }

        // div_trunc_by
        const div_trunc_by = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.div_trunc_by", .{num_type});
        if (env.common.findIdent(div_trunc_by)) |ident| {
            try low_level_map.put(ident, .num_div_trunc_by);
        }

        // rem_by
        const rem_by = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.rem_by", .{num_type});
        if (env.common.findIdent(rem_by)) |ident| {
            try low_level_map.put(ident, .num_rem_by);
        }
    }

    // Numeric modulo operation (integer types only)
    const integer_types = [_][]const u8{ "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128" };
    for (integer_types) |num_type| {
        var buf: [256]u8 = undefined;

        // mod_by
        const mod_by = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.mod_by", .{num_type});
        if (env.common.findIdent(mod_by)) |ident| {
            try low_level_map.put(ident, .num_mod_by);
        }
    }

    // Numeric negate operation (signed types only)
    for (signed_types) |num_type| {
        var buf: [256]u8 = undefined;

        // negate
        const negate = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.negate", .{num_type});
        if (env.common.findIdent(negate)) |ident| {
            try low_level_map.put(ident, .num_negate);
        }

        // abs
        const abs = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.abs", .{num_type});
        if (env.common.findIdent(abs)) |ident| {
            try low_level_map.put(ident, .num_abs);
        }
    }

    // Numeric abs_diff operation (all numeric types)
    for (numeric_types) |num_type| {
        var buf: [256]u8 = undefined;

        const abs_diff = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.abs_diff", .{num_type});
        if (env.common.findIdent(abs_diff)) |ident| {
            try low_level_map.put(ident, .num_abs_diff);
        }
    }

    // Bitwise shift operations (integer types only);
    for (integer_types) |num_type| {
        var buf: [256]u8 = undefined;

        // shift_left_by
        const shift_left_by = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.shift_left_by", .{num_type});
        if (env.common.findIdent(shift_left_by)) |ident| {
            try low_level_map.put(ident, .num_shift_left_by);
        }

        // shift_right_by
        const shift_right_by = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.shift_right_by", .{num_type});
        if (env.common.findIdent(shift_right_by)) |ident| {
            try low_level_map.put(ident, .num_shift_right_by);
        }

        // shift_right_zf_by
        const shift_right_zf_by = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.shift_right_zf_by", .{num_type});
        if (env.common.findIdent(shift_right_zf_by)) |ident| {
            try low_level_map.put(ident, .num_shift_right_zf_by);
        }
    }

    // U8 conversion operations
    if (env.common.findIdent("Builtin.Num.U8.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .u8_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_i8_try")) |ident| {
        try low_level_map.put(ident, .u8_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_i16")) |ident| {
        try low_level_map.put(ident, .u8_to_i16);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_i32")) |ident| {
        try low_level_map.put(ident, .u8_to_i32);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_i64")) |ident| {
        try low_level_map.put(ident, .u8_to_i64);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_i128")) |ident| {
        try low_level_map.put(ident, .u8_to_i128);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_u16")) |ident| {
        try low_level_map.put(ident, .u8_to_u16);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_u32")) |ident| {
        try low_level_map.put(ident, .u8_to_u32);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_u64")) |ident| {
        try low_level_map.put(ident, .u8_to_u64);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_u128")) |ident| {
        try low_level_map.put(ident, .u8_to_u128);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_f32")) |ident| {
        try low_level_map.put(ident, .u8_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_f64")) |ident| {
        try low_level_map.put(ident, .u8_to_f64);
    }
    if (env.common.findIdent("Builtin.Num.U8.to_dec")) |ident| {
        try low_level_map.put(ident, .u8_to_dec);
    }

    // I8 conversion operations
    if (env.common.findIdent("Builtin.Num.I8.to_i16")) |ident| {
        try low_level_map.put(ident, .i8_to_i16);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_i32")) |ident| {
        try low_level_map.put(ident, .i8_to_i32);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_i64")) |ident| {
        try low_level_map.put(ident, .i8_to_i64);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_i128")) |ident| {
        try low_level_map.put(ident, .i8_to_i128);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .i8_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u8_try")) |ident| {
        try low_level_map.put(ident, .i8_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .i8_to_u16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u16_try")) |ident| {
        try low_level_map.put(ident, .i8_to_u16_try);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .i8_to_u32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u32_try")) |ident| {
        try low_level_map.put(ident, .i8_to_u32_try);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .i8_to_u64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u64_try")) |ident| {
        try low_level_map.put(ident, .i8_to_u64_try);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u128_wrap")) |ident| {
        try low_level_map.put(ident, .i8_to_u128_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_u128_try")) |ident| {
        try low_level_map.put(ident, .i8_to_u128_try);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_f32")) |ident| {
        try low_level_map.put(ident, .i8_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_f64")) |ident| {
        try low_level_map.put(ident, .i8_to_f64);
    }
    if (env.common.findIdent("Builtin.Num.I8.to_dec")) |ident| {
        try low_level_map.put(ident, .i8_to_dec);
    }

    // U16 conversion operations
    if (env.common.findIdent("Builtin.Num.U16.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .u16_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_i8_try")) |ident| {
        try low_level_map.put(ident, .u16_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .u16_to_i16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_i16_try")) |ident| {
        try low_level_map.put(ident, .u16_to_i16_try);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_i32")) |ident| {
        try low_level_map.put(ident, .u16_to_i32);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_i64")) |ident| {
        try low_level_map.put(ident, .u16_to_i64);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_i128")) |ident| {
        try low_level_map.put(ident, .u16_to_i128);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .u16_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_u8_try")) |ident| {
        try low_level_map.put(ident, .u16_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_u32")) |ident| {
        try low_level_map.put(ident, .u16_to_u32);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_u64")) |ident| {
        try low_level_map.put(ident, .u16_to_u64);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_u128")) |ident| {
        try low_level_map.put(ident, .u16_to_u128);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_f32")) |ident| {
        try low_level_map.put(ident, .u16_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_f64")) |ident| {
        try low_level_map.put(ident, .u16_to_f64);
    }
    if (env.common.findIdent("Builtin.Num.U16.to_dec")) |ident| {
        try low_level_map.put(ident, .u16_to_dec);
    }

    // I16 conversion operations
    if (env.common.findIdent("Builtin.Num.I16.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .i16_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_i8_try")) |ident| {
        try low_level_map.put(ident, .i16_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_i32")) |ident| {
        try low_level_map.put(ident, .i16_to_i32);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_i64")) |ident| {
        try low_level_map.put(ident, .i16_to_i64);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_i128")) |ident| {
        try low_level_map.put(ident, .i16_to_i128);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .i16_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u8_try")) |ident| {
        try low_level_map.put(ident, .i16_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .i16_to_u16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u16_try")) |ident| {
        try low_level_map.put(ident, .i16_to_u16_try);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .i16_to_u32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u32_try")) |ident| {
        try low_level_map.put(ident, .i16_to_u32_try);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .i16_to_u64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u64_try")) |ident| {
        try low_level_map.put(ident, .i16_to_u64_try);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u128_wrap")) |ident| {
        try low_level_map.put(ident, .i16_to_u128_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_u128_try")) |ident| {
        try low_level_map.put(ident, .i16_to_u128_try);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_f32")) |ident| {
        try low_level_map.put(ident, .i16_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_f64")) |ident| {
        try low_level_map.put(ident, .i16_to_f64);
    }
    if (env.common.findIdent("Builtin.Num.I16.to_dec")) |ident| {
        try low_level_map.put(ident, .i16_to_dec);
    }

    // U32 conversion operations
    if (env.common.findIdent("Builtin.Num.U32.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .u32_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_i8_try")) |ident| {
        try low_level_map.put(ident, .u32_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .u32_to_i16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_i16_try")) |ident| {
        try low_level_map.put(ident, .u32_to_i16_try);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_i32_wrap")) |ident| {
        try low_level_map.put(ident, .u32_to_i32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_i32_try")) |ident| {
        try low_level_map.put(ident, .u32_to_i32_try);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_i64")) |ident| {
        try low_level_map.put(ident, .u32_to_i64);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_i128")) |ident| {
        try low_level_map.put(ident, .u32_to_i128);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .u32_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_u8_try")) |ident| {
        try low_level_map.put(ident, .u32_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .u32_to_u16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_u16_try")) |ident| {
        try low_level_map.put(ident, .u32_to_u16_try);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_u64")) |ident| {
        try low_level_map.put(ident, .u32_to_u64);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_u128")) |ident| {
        try low_level_map.put(ident, .u32_to_u128);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_f32")) |ident| {
        try low_level_map.put(ident, .u32_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_f64")) |ident| {
        try low_level_map.put(ident, .u32_to_f64);
    }
    if (env.common.findIdent("Builtin.Num.U32.to_dec")) |ident| {
        try low_level_map.put(ident, .u32_to_dec);
    }

    // I32 conversion operations
    if (env.common.findIdent("Builtin.Num.I32.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .i32_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_i8_try")) |ident| {
        try low_level_map.put(ident, .i32_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .i32_to_i16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_i16_try")) |ident| {
        try low_level_map.put(ident, .i32_to_i16_try);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_i64")) |ident| {
        try low_level_map.put(ident, .i32_to_i64);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_i128")) |ident| {
        try low_level_map.put(ident, .i32_to_i128);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .i32_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u8_try")) |ident| {
        try low_level_map.put(ident, .i32_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .i32_to_u16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u16_try")) |ident| {
        try low_level_map.put(ident, .i32_to_u16_try);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .i32_to_u32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u32_try")) |ident| {
        try low_level_map.put(ident, .i32_to_u32_try);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .i32_to_u64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u64_try")) |ident| {
        try low_level_map.put(ident, .i32_to_u64_try);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u128_wrap")) |ident| {
        try low_level_map.put(ident, .i32_to_u128_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_u128_try")) |ident| {
        try low_level_map.put(ident, .i32_to_u128_try);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_f32")) |ident| {
        try low_level_map.put(ident, .i32_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_f64")) |ident| {
        try low_level_map.put(ident, .i32_to_f64);
    }
    if (env.common.findIdent("Builtin.Num.I32.to_dec")) |ident| {
        try low_level_map.put(ident, .i32_to_dec);
    }

    // U64 conversion operations
    if (env.common.findIdent("Builtin.Num.U64.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .u64_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_i8_try")) |ident| {
        try low_level_map.put(ident, .u64_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .u64_to_i16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_i16_try")) |ident| {
        try low_level_map.put(ident, .u64_to_i16_try);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_i32_wrap")) |ident| {
        try low_level_map.put(ident, .u64_to_i32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_i32_try")) |ident| {
        try low_level_map.put(ident, .u64_to_i32_try);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_i64_wrap")) |ident| {
        try low_level_map.put(ident, .u64_to_i64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_i64_try")) |ident| {
        try low_level_map.put(ident, .u64_to_i64_try);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_i128")) |ident| {
        try low_level_map.put(ident, .u64_to_i128);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .u64_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_u8_try")) |ident| {
        try low_level_map.put(ident, .u64_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .u64_to_u16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_u16_try")) |ident| {
        try low_level_map.put(ident, .u64_to_u16_try);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .u64_to_u32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_u32_try")) |ident| {
        try low_level_map.put(ident, .u64_to_u32_try);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_u128")) |ident| {
        try low_level_map.put(ident, .u64_to_u128);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_f32")) |ident| {
        try low_level_map.put(ident, .u64_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_f64")) |ident| {
        try low_level_map.put(ident, .u64_to_f64);
    }
    if (env.common.findIdent("Builtin.Num.U64.to_dec")) |ident| {
        try low_level_map.put(ident, .u64_to_dec);
    }

    // I64 conversion operations
    if (env.common.findIdent("Builtin.Num.I64.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .i64_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_i8_try")) |ident| {
        try low_level_map.put(ident, .i64_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .i64_to_i16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_i16_try")) |ident| {
        try low_level_map.put(ident, .i64_to_i16_try);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_i32_wrap")) |ident| {
        try low_level_map.put(ident, .i64_to_i32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_i32_try")) |ident| {
        try low_level_map.put(ident, .i64_to_i32_try);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_i128")) |ident| {
        try low_level_map.put(ident, .i64_to_i128);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .i64_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u8_try")) |ident| {
        try low_level_map.put(ident, .i64_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .i64_to_u16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u16_try")) |ident| {
        try low_level_map.put(ident, .i64_to_u16_try);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .i64_to_u32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u32_try")) |ident| {
        try low_level_map.put(ident, .i64_to_u32_try);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .i64_to_u64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u64_try")) |ident| {
        try low_level_map.put(ident, .i64_to_u64_try);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u128_wrap")) |ident| {
        try low_level_map.put(ident, .i64_to_u128_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_u128_try")) |ident| {
        try low_level_map.put(ident, .i64_to_u128_try);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_f32")) |ident| {
        try low_level_map.put(ident, .i64_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_f64")) |ident| {
        try low_level_map.put(ident, .i64_to_f64);
    }
    if (env.common.findIdent("Builtin.Num.I64.to_dec")) |ident| {
        try low_level_map.put(ident, .i64_to_dec);
    }

    // U128 conversion operations
    if (env.common.findIdent("Builtin.Num.U128.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i8_try")) |ident| {
        try low_level_map.put(ident, .u128_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_i16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i16_try")) |ident| {
        try low_level_map.put(ident, .u128_to_i16_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i32_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_i32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i32_try")) |ident| {
        try low_level_map.put(ident, .u128_to_i32_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i64_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_i64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i64_try")) |ident| {
        try low_level_map.put(ident, .u128_to_i64_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i128_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_i128_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_i128_try")) |ident| {
        try low_level_map.put(ident, .u128_to_i128_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_u8_try")) |ident| {
        try low_level_map.put(ident, .u128_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_u16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_u16_try")) |ident| {
        try low_level_map.put(ident, .u128_to_u16_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_u32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_u32_try")) |ident| {
        try low_level_map.put(ident, .u128_to_u32_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .u128_to_u64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_u64_try")) |ident| {
        try low_level_map.put(ident, .u128_to_u64_try);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_f32")) |ident| {
        try low_level_map.put(ident, .u128_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.U128.to_f64")) |ident| {
        try low_level_map.put(ident, .u128_to_f64);
    }
    if (env.common.findIdent("u128_to_dec_try_unsafe")) |ident| {
        try low_level_map.put(ident, .u128_to_dec_try_unsafe);
    }

    // I128 conversion operations
    if (env.common.findIdent("Builtin.Num.I128.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_i8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_i8_try")) |ident| {
        try low_level_map.put(ident, .i128_to_i8_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_i16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_i16_try")) |ident| {
        try low_level_map.put(ident, .i128_to_i16_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_i32_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_i32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_i32_try")) |ident| {
        try low_level_map.put(ident, .i128_to_i32_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_i64_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_i64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_i64_try")) |ident| {
        try low_level_map.put(ident, .i128_to_i64_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_u8_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u8_try")) |ident| {
        try low_level_map.put(ident, .i128_to_u8_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_u16_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u16_try")) |ident| {
        try low_level_map.put(ident, .i128_to_u16_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_u32_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u32_try")) |ident| {
        try low_level_map.put(ident, .i128_to_u32_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_u64_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u64_try")) |ident| {
        try low_level_map.put(ident, .i128_to_u64_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u128_wrap")) |ident| {
        try low_level_map.put(ident, .i128_to_u128_wrap);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_u128_try")) |ident| {
        try low_level_map.put(ident, .i128_to_u128_try);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_f32")) |ident| {
        try low_level_map.put(ident, .i128_to_f32);
    }
    if (env.common.findIdent("Builtin.Num.I128.to_f64")) |ident| {
        try low_level_map.put(ident, .i128_to_f64);
    }
    if (env.common.findIdent("i128_to_dec_try_unsafe")) |ident| {
        try low_level_map.put(ident, .i128_to_dec_try_unsafe);
    }

    // F32 conversion operations
    if (env.common.findIdent("Builtin.Num.F32.to_i8_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_i8_trunc);
    }
    if (env.common.findIdent("f32_to_i8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_i16_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_i16_trunc);
    }
    if (env.common.findIdent("f32_to_i16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_i32_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_i32_trunc);
    }
    if (env.common.findIdent("f32_to_i32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_i64_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_i64_trunc);
    }
    if (env.common.findIdent("f32_to_i64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_i128_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_i128_trunc);
    }
    if (env.common.findIdent("f32_to_i128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u8_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_u8_trunc);
    }
    if (env.common.findIdent("f32_to_u8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u16_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_u16_trunc);
    }
    if (env.common.findIdent("f32_to_u16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u32_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_u32_trunc);
    }
    if (env.common.findIdent("f32_to_u32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u64_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_u64_trunc);
    }
    if (env.common.findIdent("f32_to_u64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u128_trunc")) |ident| {
        try low_level_map.put(ident, .f32_to_u128_trunc);
    }
    if (env.common.findIdent("f32_to_u128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_f64")) |ident| {
        try low_level_map.put(ident, .f32_to_f64);
    }

    // F64 conversion operations
    if (env.common.findIdent("Builtin.Num.F64.to_i8_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_i8_trunc);
    }
    if (env.common.findIdent("f64_to_i8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_i16_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_i16_trunc);
    }
    if (env.common.findIdent("f64_to_i16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_i32_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_i32_trunc);
    }
    if (env.common.findIdent("f64_to_i32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_i64_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_i64_trunc);
    }
    if (env.common.findIdent("f64_to_i64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_i128_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_i128_trunc);
    }
    if (env.common.findIdent("f64_to_i128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u8_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_u8_trunc);
    }
    if (env.common.findIdent("f64_to_u8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u16_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_u16_trunc);
    }
    if (env.common.findIdent("f64_to_u16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u32_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_u32_trunc);
    }
    if (env.common.findIdent("f64_to_u32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u64_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_u64_trunc);
    }
    if (env.common.findIdent("f64_to_u64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u128_trunc")) |ident| {
        try low_level_map.put(ident, .f64_to_u128_trunc);
    }
    if (env.common.findIdent("f64_to_u128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_f32_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_f32_wrap);
    }
    if (env.common.findIdent("f64_to_f32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_f32_try_unsafe);
    }

    // Dec conversion functions
    if (env.common.findIdent("Builtin.Num.Dec.to_i8_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_i8_trunc);
    }
    if (env.common.findIdent("dec_to_i8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_i16_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_i16_trunc);
    }
    if (env.common.findIdent("dec_to_i16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_i32_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_i32_trunc);
    }
    if (env.common.findIdent("dec_to_i32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_i64_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_i64_trunc);
    }
    if (env.common.findIdent("dec_to_i64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_i128_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_i128_trunc);
    }
    if (env.common.findIdent("dec_to_i128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u8_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_u8_trunc);
    }
    if (env.common.findIdent("dec_to_u8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u16_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_u16_trunc);
    }
    if (env.common.findIdent("dec_to_u16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u32_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_u32_trunc);
    }
    if (env.common.findIdent("dec_to_u32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u64_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_u64_trunc);
    }
    if (env.common.findIdent("dec_to_u64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u128_trunc")) |ident| {
        try low_level_map.put(ident, .dec_to_u128_trunc);
    }
    if (env.common.findIdent("dec_to_u128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_f32_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_f32_wrap);
    }
    if (env.common.findIdent("dec_to_f32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_f32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_f64")) |ident| {
        try low_level_map.put(ident, .dec_to_f64);
    }

    // Iterate through all defs and replace matching anno-only defs with low-level implementations
    // NOTE: We copy def indices to a separate list first, because operations inside the loop
    // may reallocate extra_data, which would invalidate any slice taken from it.
    const all_defs_slice = env.store.sliceDefs(env.all_defs);
    var def_indices = std.ArrayList(CIR.Def.Idx).empty;
    defer def_indices.deinit(gpa);
    try def_indices.appendSlice(gpa, all_defs_slice);

    for (def_indices.items) |def_idx| {
        const def = env.store.getDef(def_idx);
        const expr = env.store.getExpr(def.expr);

        // Check if this is an anno-only def (e_anno_only expression)
        if (expr == .e_anno_only and def.annotation != null) {
            // Get the identifier from the pattern
            const pattern = env.store.getPattern(def.pattern);
            if (pattern == .assign) {
                const ident = pattern.assign.ident;

                // Check if this identifier matches a low-level operation
                if (low_level_map.fetchRemove(ident)) |entry| {
                    const low_level_op = entry.value;

                    // Get the number of parameters from the type annotation
                    // The annotation must be a function type for low-level operations
                    const annotation = env.store.getAnnotation(def.annotation.?);
                    const type_anno = env.store.getTypeAnno(annotation.anno);
                    const num_params: u32 = switch (type_anno) {
                        .@"fn" => |func| func.args.span.len,
                        else => std.debug.panic("Low-level operation {s} does not have a function type annotation", .{@tagName(low_level_op)}),
                    };

                    const patterns_start = env.store.scratchTop("patterns");
                    var i: u32 = 0;
                    while (i < num_params) : (i += 1) {
                        var arg_name_buf: [16]u8 = undefined;
                        const arg_name = try std.fmt.bufPrint(&arg_name_buf, "_arg{d}", .{i});
                        const arg_ident = env.common.findIdent(arg_name) orelse try env.common.insertIdent(gpa, base.Ident.for_text(arg_name));
                        const arg_pattern_idx = try env.addPattern(.{ .assign = .{ .ident = arg_ident } }, base.Region.zero());
                        try env.store.scratch.?.patterns.append(arg_pattern_idx);
                    }
                    const args_span = try env.store.patternSpanFrom(patterns_start);

                    // Create an e_runtime_error body that crashes when the function is called
                    const error_msg_lit = try env.insertString("Low-level builtin not yet implemented in interpreter");
                    const diagnostic_idx = try env.addDiagnostic(.{ .not_implemented = .{
                        .feature = error_msg_lit,
                        .region = base.Region.zero(),
                    } });
                    const body_idx = try env.addExpr(.{ .e_runtime_error = .{ .diagnostic = diagnostic_idx } }, base.Region.zero());

                    // Create e_low_level_lambda expression
                    const expr_idx = try env.addExpr(.{ .e_low_level_lambda = .{
                        .op = low_level_op,
                        .args = args_span,
                        .body = body_idx,
                    } }, base.Region.zero());

                    // Now replace the e_anno_only expression with the e_low_level_lambda
                    // Def structure is stored in extra_data:
                    // extra_data[0] = pattern, extra_data[1] = expr, ...
                    // node.data_1 points to the start index in extra_data
                    const def_node_idx = @as(@TypeOf(env.store.nodes).Idx, @enumFromInt(@intFromEnum(def_idx)));
                    const def_node = env.store.nodes.get(def_node_idx);
                    const extra_start = def_node.data_1;

                    // Update the expr field (at extra_start + 1)
                    env.store.extra_data.items.items[extra_start + 1] = @intFromEnum(expr_idx);

                    // Track this replaced def index
                    try new_def_indices.append(gpa, def_idx);
                }
            }
        }
    }

    // Verify all low-level operations were found in the builtins
    if (low_level_map.count() > 0) {
        std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
        std.debug.print("ERROR: Low-level operations not found in Builtin.roc\n", .{});
        std.debug.print("=" ** 80 ++ "\n\n", .{});

        std.debug.print("The following low-level operations were not found:\n", .{});
        var iter = low_level_map.iterator();
        while (iter.next()) |entry| {
            const ident_text = env.getIdentText(entry.key_ptr.*);
            const op_name = @tagName(entry.value_ptr.*);
            std.debug.print("  - {s} (mapped to .{s})\n", .{ ident_text, op_name });
        }
        std.debug.print("\nEither:\n", .{});
        std.debug.print("  1. Remove the obsolete entry from the low_level_map in builtin_compiler/main.zig, OR\n", .{});
        std.debug.print("  2. Add a standalone type annotation to Builtin.roc for it to match\n", .{});

        std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
        std.debug.print("Builtin compiler exiting with error code due to {d} missing operation(s)\n", .{low_level_map.count()});
        std.debug.print("=" ** 80 ++ "\n", .{});

        return error.LowLevelOperationsNotFound;
    }

    return new_def_indices;
}

fn readFileAllocPath(gpa: Allocator, path: []const u8) ![]u8 {
    if (std.fs.path.isAbsolute(path)) {
        var file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();
        return try file.readToEndAlloc(gpa, max_builtin_bytes);
    }
    return try std.fs.cwd().readFileAlloc(gpa, path, max_builtin_bytes);
}

/// Build-time compiler that compiles builtin .roc sources into serialized ModuleEnvs.
/// This runs during `zig build` on the host machine to generate .bin files
/// that get embedded into the final roc executable.
///
/// The build system passes the absolute path to Builtin.roc as the first argument for cache tracking;
/// we honor that when present so the compiler works regardless of the current working directory.
pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa_impl.deinit();
        if (leaked == .leak) {
            std.debug.print("WARNING: Memory leaked!\n", .{});
        }
    }
    const gpa = gpa_impl.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    // Prefer the absolute path provided by the build system, but fall back to the
    // project-relative path so manual runs (e.g. `zig build run`) still succeed.
    const builtin_src_path = if (args.len >= 2) args[1] else "src/build/roc/Builtin.roc";

    // Read the Builtin.roc source file at runtime
    // NOTE: We must free this source manually; CommonEnv.deinit() does not free the source.
    const builtin_roc_source = try readFileAllocPath(gpa, builtin_src_path);

    // Compile Builtin.roc (it's completely self-contained)
    const builtin_env = try compileModule(
        gpa,
        "Builtin",
        builtin_roc_source,
        builtin_src_path,
        &.{}, // No module dependencies
        null, // bool_stmt not available yet (will be found within Builtin)
        null, // try_stmt not available yet (will be found within Builtin)
        null, // str_stmt not available yet (will be found within Builtin)
    );
    defer {
        builtin_env.deinit();
        gpa.destroy(builtin_env);
        gpa.free(builtin_roc_source);
    }

    // Find nested type declarations in Builtin module
    // These are nested inside Builtin's record extension (Builtin := [].{...})
    const bool_type_idx = try findTypeDeclaration(builtin_env, "Bool");
    const try_type_idx = try findTypeDeclaration(builtin_env, "Try");
    const dict_type_idx = try findTypeDeclaration(builtin_env, "Dict");
    const set_type_idx = try findTypeDeclaration(builtin_env, "Set");
    const str_type_idx = try findTypeDeclaration(builtin_env, "Str");
    const list_type_idx = try findTypeDeclaration(builtin_env, "List");
    const box_type_idx = try findTypeDeclaration(builtin_env, "Box");

    // Find Utf8Problem nested inside Str (e.g., Builtin.Str.Utf8Problem)
    const utf8_problem_type_idx = try findNestedTypeDeclaration(builtin_env, "Str", "Utf8Problem");

    // Find numeric types nested inside Num (e.g., Builtin.Num.U8)
    const u8_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U8");
    const i8_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I8");
    const u16_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U16");
    const i16_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I16");
    const u32_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U32");
    const i32_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I32");
    const u64_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U64");
    const i64_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I64");
    const u128_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U128");
    const i128_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I128");
    const dec_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "Dec");
    const f32_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "F32");
    const f64_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "F64");
    const numeral_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "Numeral");

    // Look up idents for each type
    // All types use fully-qualified names for consistent member lookup
    // Top-level types: "Builtin.Bool", "Builtin.Str", etc.
    // Nested types under Num: "Builtin.Num.U8", etc.
    const bool_ident = builtin_env.common.findIdent("Builtin.Bool") orelse unreachable;
    const try_ident = builtin_env.common.findIdent("Builtin.Try") orelse unreachable;
    const dict_ident = builtin_env.common.findIdent("Builtin.Dict") orelse unreachable;
    const set_ident = builtin_env.common.findIdent("Builtin.Set") orelse unreachable;
    const str_ident = builtin_env.common.findIdent("Builtin.Str") orelse unreachable;
    const list_ident = builtin_env.common.findIdent("Builtin.List") orelse unreachable;
    const box_ident = builtin_env.common.findIdent("Builtin.Box") orelse unreachable;
    const utf8_problem_ident = builtin_env.common.findIdent("Builtin.Str.Utf8Problem") orelse unreachable;
    const u8_ident = builtin_env.common.findIdent("Builtin.Num.U8") orelse unreachable;
    const i8_ident = builtin_env.common.findIdent("Builtin.Num.I8") orelse unreachable;
    const u16_ident = builtin_env.common.findIdent("Builtin.Num.U16") orelse unreachable;
    const i16_ident = builtin_env.common.findIdent("Builtin.Num.I16") orelse unreachable;
    const u32_ident = builtin_env.common.findIdent("Builtin.Num.U32") orelse unreachable;
    const i32_ident = builtin_env.common.findIdent("Builtin.Num.I32") orelse unreachable;
    const u64_ident = builtin_env.common.findIdent("Builtin.Num.U64") orelse unreachable;
    const i64_ident = builtin_env.common.findIdent("Builtin.Num.I64") orelse unreachable;
    const u128_ident = builtin_env.common.findIdent("Builtin.Num.U128") orelse unreachable;
    const i128_ident = builtin_env.common.findIdent("Builtin.Num.I128") orelse unreachable;
    const dec_ident = builtin_env.common.findIdent("Builtin.Num.Dec") orelse unreachable;
    const f32_ident = builtin_env.common.findIdent("Builtin.Num.F32") orelse unreachable;
    const f64_ident = builtin_env.common.findIdent("Builtin.Num.F64") orelse unreachable;
    const numeral_ident = builtin_env.common.findIdent("Builtin.Num.Numeral") orelse unreachable;
    // Tag idents for Try type (Ok and Err)
    const ok_ident = builtin_env.common.findIdent("Ok") orelse unreachable;
    const err_ident = builtin_env.common.findIdent("Err") orelse unreachable;

    // Expose the types so they can be found by getExposedNodeIndexById (used for auto-imports)
    // Note: These types are already in exposed_items from canonicalization, we just set their node indices
    try builtin_env.common.setNodeIndexById(gpa, bool_ident, @intCast(@intFromEnum(bool_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, try_ident, @intCast(@intFromEnum(try_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, dict_ident, @intCast(@intFromEnum(dict_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, set_ident, @intCast(@intFromEnum(set_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, str_ident, @intCast(@intFromEnum(str_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, list_ident, @intCast(@intFromEnum(list_type_idx)));

    try builtin_env.common.setNodeIndexById(gpa, u8_ident, @intCast(@intFromEnum(u8_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i8_ident, @intCast(@intFromEnum(i8_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, u16_ident, @intCast(@intFromEnum(u16_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i16_ident, @intCast(@intFromEnum(i16_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, u32_ident, @intCast(@intFromEnum(u32_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i32_ident, @intCast(@intFromEnum(i32_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, u64_ident, @intCast(@intFromEnum(u64_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i64_ident, @intCast(@intFromEnum(i64_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, u128_ident, @intCast(@intFromEnum(u128_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i128_ident, @intCast(@intFromEnum(i128_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, dec_ident, @intCast(@intFromEnum(dec_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, f32_ident, @intCast(@intFromEnum(f32_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, f64_ident, @intCast(@intFromEnum(f64_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, numeral_ident, @intCast(@intFromEnum(numeral_type_idx)));

    // Create output directory
    try std.fs.cwd().makePath("zig-out/builtins");

    // Serialize the single Builtin module
    try serializeModuleEnv(gpa, builtin_env, "zig-out/builtins/Builtin.bin");

    // Create and serialize builtin indices
    const builtin_indices = BuiltinIndices{
        // Statement indices
        .bool_type = bool_type_idx,
        .try_type = try_type_idx,
        .dict_type = dict_type_idx,
        .set_type = set_type_idx,
        .str_type = str_type_idx,
        .list_type = list_type_idx,
        .box_type = box_type_idx,
        .utf8_problem_type = utf8_problem_type_idx,
        .u8_type = u8_type_idx,
        .i8_type = i8_type_idx,
        .u16_type = u16_type_idx,
        .i16_type = i16_type_idx,
        .u32_type = u32_type_idx,
        .i32_type = i32_type_idx,
        .u64_type = u64_type_idx,
        .i64_type = i64_type_idx,
        .u128_type = u128_type_idx,
        .i128_type = i128_type_idx,
        .dec_type = dec_type_idx,
        .f32_type = f32_type_idx,
        .f64_type = f64_type_idx,
        .numeral_type = numeral_type_idx,
        .bool_ident = bool_ident,
        .try_ident = try_ident,
        .dict_ident = dict_ident,
        .set_ident = set_ident,
        .str_ident = str_ident,
        .list_ident = list_ident,
        .box_ident = box_ident,
        .utf8_problem_ident = utf8_problem_ident,
        .u8_ident = u8_ident,
        .i8_ident = i8_ident,
        .u16_ident = u16_ident,
        .i16_ident = i16_ident,
        .u32_ident = u32_ident,
        .i32_ident = i32_ident,
        .u64_ident = u64_ident,
        .i64_ident = i64_ident,
        .u128_ident = u128_ident,
        .i128_ident = i128_ident,
        .dec_ident = dec_ident,
        .f32_ident = f32_ident,
        .f64_ident = f64_ident,
        .numeral_ident = numeral_ident,
        .ok_ident = ok_ident,
        .err_ident = err_ident,
    };

    // Validate that BuiltinIndices contains all type declarations under Builtin
    // This ensures BuiltinIndices stays in sync with the actual Builtin module content
    try validateBuiltinIndicesCompleteness(builtin_env, builtin_indices);

    try serializeBuiltinIndices(builtin_indices, "zig-out/builtins/builtin_indices.bin");
}

/// Validates that BuiltinIndices contains all nominal type declarations in the Builtin module.
/// Iterates through all statements and ensures every s_nominal_decl is present in BuiltinIndices,
/// with the exception of "Num" which is a container type, not an auto-imported type.
fn validateBuiltinIndicesCompleteness(env: *const ModuleEnv, indices: BuiltinIndices) !void {
    // Collect all statement indices from BuiltinIndices using reflection
    // Only check Statement.Idx fields (skip Ident.Idx fields)
    var indexed_stmts = std.AutoHashMap(CIR.Statement.Idx, void).init(std.heap.page_allocator);
    defer indexed_stmts.deinit();

    const fields = @typeInfo(BuiltinIndices).@"struct".fields;
    inline for (fields) |field| {
        if (field.type == CIR.Statement.Idx) {
            const stmt_idx = @field(indices, field.name);
            try indexed_stmts.put(stmt_idx, {});
        }
    }

    // Check all nominal type declarations in the Builtin module
    const all_stmts = env.store.sliceStatements(env.all_statements);
    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_nominal_decl => |decl| {
                const header = env.store.getTypeHeader(decl.header);
                const ident_text = env.getIdentText(header.name);

                // Skip container types that are not auto-imported types
                if (std.mem.eql(u8, ident_text, "Builtin") or
                    std.mem.eql(u8, ident_text, "Builtin.Num"))
                {
                    continue;
                }

                // Every other nominal type should be in BuiltinIndices
                if (!indexed_stmts.contains(stmt_idx)) {
                    std.debug.print("ERROR: Type '{s}' (stmt_idx={d}) is not in BuiltinIndices!\n", .{
                        ident_text,
                        @intFromEnum(stmt_idx),
                    });
                    std.debug.print("Add this type to BuiltinIndices in CIR.zig and builtin_compiler/main.zig\n", .{});
                    return error.BuiltinIndicesIncomplete;
                }
            },
            else => continue,
        }
    }
}

const ModuleDep = struct {
    name: []const u8,
    env: *const ModuleEnv,
};

fn compileModule(
    gpa: Allocator,
    module_name: []const u8,
    source: []const u8,
    source_path: []const u8,
    deps: []const ModuleDep,
    bool_stmt_opt: ?CIR.Statement.Idx,
    try_stmt_opt: ?CIR.Statement.Idx,
    str_stmt_opt: ?CIR.Statement.Idx,
) !*ModuleEnv {
    // This follows the pattern from TestEnv.init() in src/check/test/TestEnv.zig

    // 1. Create ModuleEnv
    var module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    // 2. Create common idents (needed for type checking)
    const module_ident = try module_env.insertIdent(base.Ident.for_text(module_name));

    // Use provided bool_stmt, try_stmt, and str_stmt if available, otherwise use undefined
    // For Builtin module, these will be found after canonicalization and updated before type checking
    var builtin_ctx: Check.BuiltinContext = .{
        .module_name = module_ident,
        .bool_stmt = bool_stmt_opt orelse undefined,
        .try_stmt = try_stmt_opt orelse undefined,
        .str_stmt = str_stmt_opt orelse undefined,
        .builtin_module = null,
        .builtin_indices = null,
    };

    // 3. Parse
    var parse_ast = try gpa.create(parse.AST);
    defer {
        parse_ast.deinit(gpa);
        gpa.destroy(parse_ast);
    }

    parse_ast.* = try parse.parse(&module_env.common, gpa);
    parse_ast.store.emptyScratch();

    // Check for parse errors
    if (parse_ast.hasErrors()) {
        const stderr = stderrWriter();
        const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
        const config = reporting.ReportingConfig.initColorTerminal();

        // Render tokenize diagnostics
        for (parse_ast.tokenize_diagnostics.items) |diag| {
            var report = parse_ast.tokenizeDiagnosticToReport(diag, gpa, source_path) catch |err| {
                std.debug.print("Error creating tokenize diagnostic report: {}\n", .{err});
                continue;
            };
            defer report.deinit();
            reporting.renderReportToTerminal(&report, stderr, palette, config) catch |err| {
                std.debug.print("Error rendering tokenize diagnostic: {}\n", .{err});
            };
        }

        // Render parse diagnostics
        for (parse_ast.parse_diagnostics.items) |diag| {
            var report = parse_ast.parseDiagnosticToReport(&module_env.common, diag, gpa, source_path) catch |err| {
                std.debug.print("Error creating parse diagnostic report: {}\n", .{err});
                continue;
            };
            defer report.deinit();
            reporting.renderReportToTerminal(&report, stderr, palette, config) catch |err| {
                std.debug.print("Error rendering parse diagnostic: {}\n", .{err});
            };
        }

        flushStderr();
        return error.ParseError;
    }

    // 4. Canonicalize
    try module_env.initCIRFields(module_name);

    var can_result = try gpa.create(Can);
    defer {
        can_result.deinit();
        gpa.destroy(can_result);
    }

    // When compiling Builtin itself, pass null for module_envs so setupAutoImportedBuiltinTypes doesn't run
    can_result.* = try Can.init(module_env, parse_ast, null);

    try can_result.canonicalizeFile();
    try can_result.validateForChecking();

    // Check for canonicalization errors
    const can_diagnostics = try module_env.getDiagnostics();
    defer gpa.free(can_diagnostics);
    if (can_diagnostics.len > 0) {
        const stderr = stderrWriter();
        const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
        const config = reporting.ReportingConfig.initColorTerminal();

        for (can_diagnostics) |diag| {
            var report = module_env.diagnosticToReport(diag, gpa, source_path) catch |err| {
                std.debug.print("Error creating canonicalization diagnostic report: {}\n", .{err});
                continue;
            };
            defer report.deinit();
            reporting.renderReportToTerminal(&report, stderr, palette, config) catch |err| {
                std.debug.print("Error rendering canonicalization diagnostic: {}\n", .{err});
            };
        }

        flushStderr();
        return error.CanonicalizeError;
    }

    // 5.5. Transform low-level operations (must happen before type checking)
    // For the Builtin module, transform annotation-only defs into low-level operations
    if (std.mem.eql(u8, module_name, "Builtin")) {
        // Transform annotation-only defs and get the list of new def indices
        var new_def_indices = try replaceStrIsEmptyWithLowLevel(module_env);
        defer new_def_indices.deinit(gpa);

        if (new_def_indices.items.len > 0) {
            // Rebuild the dependency graph and evaluation order to include the updated defs
            const DependencyGraph = @import("can").DependencyGraph;
            var graph = try DependencyGraph.buildDependencyGraph(
                module_env,
                module_env.all_defs,
                gpa,
            );
            defer graph.deinit();

            const eval_order = try DependencyGraph.computeSCCs(&graph, gpa);
            // Free the old evaluation order if it exists
            if (module_env.evaluation_order) |old_order| {
                old_order.deinit();
                gpa.destroy(old_order);
            }
            const eval_order_ptr = try gpa.create(DependencyGraph.EvaluationOrder);
            eval_order_ptr.* = eval_order;
            module_env.evaluation_order = eval_order_ptr;
        }

        // Find Bool, Try, and Str statements before type checking
        // When compiling Builtin, bool_stmt, try_stmt, and str_stmt are initially undefined,
        // but they must be set before type checking begins
        const found_bool_stmt = findTypeDeclaration(module_env, "Bool") catch {
            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
            std.debug.print("ERROR: Could not find Bool type in Builtin module\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("The Bool type declaration is required for type checking.\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            return error.TypeDeclarationNotFound;
        };
        const found_try_stmt = findTypeDeclaration(module_env, "Try") catch {
            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
            std.debug.print("ERROR: Could not find Try type in Builtin module\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("The Try type declaration is required for type checking.\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            return error.TypeDeclarationNotFound;
        };
        const found_str_stmt = findTypeDeclaration(module_env, "Str") catch {
            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
            std.debug.print("ERROR: Could not find Str type in Builtin module\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("The Str type declaration is required for type checking.\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            return error.TypeDeclarationNotFound;
        };

        // Update builtin_ctx with the found statement indices
        builtin_ctx.bool_stmt = found_bool_stmt;
        builtin_ctx.try_stmt = found_try_stmt;
        builtin_ctx.str_stmt = found_str_stmt;
    }

    // 6. Type check
    // Build the list of other modules for type checking
    var imported_envs = std.ArrayList(*const ModuleEnv).empty;
    defer imported_envs.deinit(gpa);

    // Add dependencies
    for (deps) |dep| {
        try imported_envs.append(gpa, dep.env);
    }

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        builtin_ctx,
    );
    defer checker.deinit();

    try checker.checkFile();

    // Check for type errors
    if (checker.problems.problems.items.len > 0) {
        const stderr = stderrWriter();
        const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
        const config = reporting.ReportingConfig.initColorTerminal();

        const problem = check.problem;
        var report_builder = problem.ReportBuilder.init(
            gpa,
            module_env,
            module_env,
            &checker.snapshots,
            source_path,
            imported_envs.items,
            &checker.import_mapping,
        );
        defer report_builder.deinit();

        for (0..checker.problems.len()) |i| {
            const problem_idx: problem.Problem.Idx = @enumFromInt(i);
            const prob = checker.problems.get(problem_idx);
            var report = report_builder.build(prob) catch |err| {
                std.debug.print("Error creating type problem report: {}\n", .{err});
                continue;
            };
            defer report.deinit();
            reporting.renderReportToTerminal(&report, stderr, palette, config) catch |err| {
                std.debug.print("Error rendering type problem: {}\n", .{err});
            };
        }

        flushStderr();
        return error.TypeCheckError;
    }

    return module_env;
}

fn serializeModuleEnv(
    gpa: Allocator,
    env: *const ModuleEnv,
    output_path: []const u8,
) !void {
    // This follows the pattern from module_env_test.zig

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    // Create output file
    const file = try std.fs.cwd().createFile(output_path, .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = collections.CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(env, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(arena_alloc, file);
}

/// Get the ident index from a type declaration statement
fn getTypeIdent(env: *const ModuleEnv, stmt_idx: CIR.Statement.Idx) base.Ident.Idx {
    const stmt = env.store.getStatement(stmt_idx);
    const header = env.store.getTypeHeader(stmt.s_nominal_decl.header);
    return header.name;
}

/// Find a type declaration by name in a compiled module
/// Returns the statement index of the type declaration
/// For builtin_compiler, types are always in all_statements (not builtin_statements)
/// because we're compiling Builtin.roc itself, not importing from it.
fn findTypeDeclaration(env: *const ModuleEnv, type_name: []const u8) !CIR.Statement.Idx {
    // Construct the qualified name (e.g., "Builtin.Bool")
    // Types in nested declarations are stored with their full qualified names
    var qualified_name_buf: [256]u8 = undefined;
    const qualified_name = try std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}", .{ env.module_name, type_name });

    // Search in all_statements (where Builtin.roc's own types are stored)
    const all_stmts = env.store.sliceStatements(env.all_statements);
    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_nominal_decl => |decl| {
                const header = env.store.getTypeHeader(decl.header);
                const ident_idx = header.name;
                const ident_text = env.getIdentText(ident_idx);
                if (std.mem.eql(u8, ident_text, qualified_name)) {
                    return stmt_idx;
                }
            },
            else => continue,
        }
    }

    return error.TypeDeclarationNotFound;
}

/// Find a nested type declaration by parent and type name in a compiled module
/// For example, findNestedTypeDeclaration(env, "Num", "U8") finds "Builtin.Num.U8"
/// Returns the statement index of the type declaration
fn findNestedTypeDeclaration(env: *const ModuleEnv, parent_name: []const u8, type_name: []const u8) !CIR.Statement.Idx {
    // Construct the qualified name (e.g., "Builtin.Num.U8")
    var qualified_name_buf: [256]u8 = undefined;
    const qualified_name = try std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}.{s}", .{ env.module_name, parent_name, type_name });

    // Search in all_statements (where Builtin.roc's own types are stored)
    const all_stmts = env.store.sliceStatements(env.all_statements);
    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_nominal_decl => |decl| {
                const header = env.store.getTypeHeader(decl.header);
                const ident_idx = header.name;
                const ident_text = env.getIdentText(ident_idx);
                if (std.mem.eql(u8, ident_text, qualified_name)) {
                    return stmt_idx;
                }
            },
            else => continue,
        }
    }

    return error.TypeDeclarationNotFound;
}

/// Serialize BuiltinIndices to a binary file
fn serializeBuiltinIndices(
    indices: BuiltinIndices,
    output_path: []const u8,
) !void {
    // Create output file
    const file = try std.fs.cwd().createFile(output_path, .{});
    defer file.close();

    // Write the struct directly as binary data
    // This is a simple struct with two u32 fields, so we can write it directly
    try file.writeAll(std.mem.asBytes(&indices));
}
