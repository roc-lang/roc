//! Builtin.roc low-level operation transform.
//!
//! The compiler-owned Builtin module declares primitive operations as annotations
//! in Roc source. This pass replaces those annotation-only definitions with CIR
//! lambdas that run the matching low-level operation before checking begins.

const std = @import("std");
const base = @import("base");

const CIR = @import("CIR.zig");
const DependencyGraph = @import("DependencyGraph.zig");
const ModuleEnv = @import("ModuleEnv.zig");

/// Returns whether this module is the compiler-owned Builtin module.
pub fn isBuiltinModule(env: *const ModuleEnv) bool {
    return env.display_module_name_idx.eql(env.idents.builtin_module);
}

/// Returns whether an annotation-only Builtin declaration is handled as an intrinsic wrapper.
pub fn isIntrinsicAnnotation(env: *const ModuleEnv, ident: base.Ident.Idx) bool {
    if (ident.eql(env.idents.builtin_str_inspect)) return true;

    const utf8_problem_eq = env.common.findIdent("Builtin.Str.Utf8Problem.is_eq") orelse return false;
    return ident.eql(utf8_problem_eq);
}

/// Replaces Builtin.roc annotation-only primitive declarations with low-level operation lambdas.
pub fn apply(env: *ModuleEnv) !void {
    var new_def_indices = try replaceProvidedByCompilerLowLevels(env);
    defer new_def_indices.deinit(env.gpa);

    if (new_def_indices.items.len == 0) return;

    var graph = try DependencyGraph.buildDependencyGraph(
        env,
        env.all_defs,
        env.gpa,
    );
    defer graph.deinit();

    const eval_order = try DependencyGraph.computeSCCs(&graph, env.gpa);
    if (env.evaluation_order) |old_order| {
        old_order.deinit();
        env.gpa.destroy(old_order);
    }
    const eval_order_ptr = try env.gpa.create(DependencyGraph.EvaluationOrder);
    eval_order_ptr.* = eval_order;
    env.evaluation_order = eval_order_ptr;
}

fn numericFromStrLowLevel(num_type: []const u8) CIR.Expr.LowLevel {
    if (std.mem.eql(u8, num_type, "U8")) return .u8_from_str;
    if (std.mem.eql(u8, num_type, "I8")) return .i8_from_str;
    if (std.mem.eql(u8, num_type, "U16")) return .u16_from_str;
    if (std.mem.eql(u8, num_type, "I16")) return .i16_from_str;
    if (std.mem.eql(u8, num_type, "U32")) return .u32_from_str;
    if (std.mem.eql(u8, num_type, "I32")) return .i32_from_str;
    if (std.mem.eql(u8, num_type, "U64")) return .u64_from_str;
    if (std.mem.eql(u8, num_type, "I64")) return .i64_from_str;
    if (std.mem.eql(u8, num_type, "U128")) return .u128_from_str;
    if (std.mem.eql(u8, num_type, "I128")) return .i128_from_str;
    if (std.mem.eql(u8, num_type, "Dec")) return .dec_from_str;
    if (std.mem.eql(u8, num_type, "F32")) return .f32_from_str;
    if (std.mem.eql(u8, num_type, "F64")) return .f64_from_str;

    unreachable;
}

/// Replace specific `e_anno_only` builtin declarations with `e_lambda` wrappers
/// around `e_run_low_level` operations.
///
/// This keeps compiler-provided builtins in one uniform shape so later
/// lowering can recognize them generically instead of carrying per-builtin
/// exceptions.
/// Returns a list of new def indices created.
fn replaceProvidedByCompilerLowLevels(env: *ModuleEnv) !std.ArrayList(CIR.Def.Idx) {
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
    if (env.common.findIdent("Builtin.Box.box")) |box_box_ident| {
        try low_level_map.put(box_box_ident, .box_box);
    }
    if (env.common.findIdent("Builtin.Box.unbox")) |box_unbox_ident| {
        try low_level_map.put(box_unbox_ident, .box_unbox);
    }
    if (env.common.findIdent("Builtin.List.len")) |list_len_ident| {
        try low_level_map.put(list_len_ident, .list_len);
    }
    if (env.common.findIdent("Builtin.List.concat")) |list_concat_ident| {
        try low_level_map.put(list_concat_ident, .list_concat);
    }
    if (env.common.findIdent("Builtin.List.with_capacity")) |list_with_capacity_ident| {
        try low_level_map.put(list_with_capacity_ident, .list_with_capacity);
    }
    if (env.common.findIdent("list_get_unsafe")) |list_get_unsafe_ident| {
        try low_level_map.put(list_get_unsafe_ident, .list_get_unsafe);
    }
    if (env.common.findIdent("list_append_unsafe")) |list_append_unsafe_ident| {
        try low_level_map.put(list_append_unsafe_ident, .list_append_unsafe);
    }
    if (env.common.findIdent("list_reserve")) |list_reserve_ident| {
        try low_level_map.put(list_reserve_ident, .list_reserve);
    }
    if (env.common.findIdent("list_release_excess_capacity")) |list_release_excess_capacity_ident| {
        try low_level_map.put(list_release_excess_capacity_ident, .list_release_excess_capacity);
    }
    if (env.common.findIdent("Builtin.List.drop_at")) |list_drop_at_ident| {
        try low_level_map.put(list_drop_at_ident, .list_drop_at);
    }
    if (env.common.findIdent("Builtin.List.sublist")) |list_sublist_ident| {
        try low_level_map.put(list_sublist_ident, .list_sublist);
    }
    if (env.common.findIdent("Builtin.List.prepend")) |list_prepend_ident| {
        try low_level_map.put(list_prepend_ident, .list_prepend);
    }
    if (env.common.findIdent("list_set_unsafe")) |list_set_unsafe_ident| {
        try low_level_map.put(list_set_unsafe_ident, .list_set);
    }
    if (env.common.findIdent("list_replace_unsafe")) |list_replace_unsafe_ident| {
        try low_level_map.put(list_replace_unsafe_ident, .list_replace_unsafe);
    }
    if (env.common.findIdent("list_swap_unsafe")) |list_swap_unsafe_ident| {
        try low_level_map.put(list_swap_unsafe_ident, .list_swap);
    }
    const numeric_types = [_][]const u8{ "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "Dec", "F32", "F64" };
    const signed_types = [_][]const u8{ "I8", "I16", "I32", "I64", "I128", "Dec", "F32", "F64" };
    // Numeric equality operations.
    // `num_is_eq` already lowers correctly for integers, Dec, and fractional types.
    const eq_types = [_][]const u8{ "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "Dec", "F32", "F64" };
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
            try low_level_map.put(ident, numericFromStrLowLevel(num_type));
        }
    }

    const internal_from_str_primitives = [_]struct {
        name: []const u8,
        op: CIR.Expr.LowLevel,
    }{
        .{ .name = "Builtin.u8_from_str", .op = .u8_from_str },
        .{ .name = "Builtin.i8_from_str", .op = .i8_from_str },
        .{ .name = "Builtin.u16_from_str", .op = .u16_from_str },
        .{ .name = "Builtin.i16_from_str", .op = .i16_from_str },
        .{ .name = "Builtin.u32_from_str", .op = .u32_from_str },
        .{ .name = "Builtin.i32_from_str", .op = .i32_from_str },
        .{ .name = "Builtin.u64_from_str", .op = .u64_from_str },
        .{ .name = "Builtin.i64_from_str", .op = .i64_from_str },
        .{ .name = "Builtin.u128_from_str", .op = .u128_from_str },
        .{ .name = "Builtin.i128_from_str", .op = .i128_from_str },
        .{ .name = "Builtin.dec_from_str", .op = .dec_from_str },
        .{ .name = "Builtin.f32_from_str", .op = .f32_from_str },
        .{ .name = "Builtin.f64_from_str", .op = .f64_from_str },
    };
    for (internal_from_str_primitives) |primitive| {
        if (env.common.findIdent(primitive.name)) |ident| {
            try low_level_map.put(ident, primitive.op);
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

    // Bitwise logical operations (integer types only)
    for (integer_types) |num_type| {
        var buf: [256]u8 = undefined;

        // bitwise_and
        const bitwise_and = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.bitwise_and", .{num_type});
        if (env.common.findIdent(bitwise_and)) |ident| {
            try low_level_map.put(ident, .num_bitwise_and);
        }

        // bitwise_or
        const bitwise_or = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.bitwise_or", .{num_type});
        if (env.common.findIdent(bitwise_or)) |ident| {
            try low_level_map.put(ident, .num_bitwise_or);
        }

        // bitwise_xor
        const bitwise_xor = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.bitwise_xor", .{num_type});
        if (env.common.findIdent(bitwise_xor)) |ident| {
            try low_level_map.put(ident, .num_bitwise_xor);
        }

        // bitwise_not
        const bitwise_not = try std.fmt.bufPrint(&buf, "Builtin.Num.{s}.bitwise_not", .{num_type});
        if (env.common.findIdent(bitwise_not)) |ident| {
            try low_level_map.put(ident, .num_bitwise_not);
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
    if (env.common.findIdent("Builtin.Num.F32.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_i8_trunc);
    }
    if (env.common.findIdent("f32_to_i8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_i16_trunc);
    }
    if (env.common.findIdent("f32_to_i16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_i32_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_i32_trunc);
    }
    if (env.common.findIdent("f32_to_i32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_i64_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_i64_trunc);
    }
    if (env.common.findIdent("f32_to_i64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_i128_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_i128_trunc);
    }
    if (env.common.findIdent("f32_to_i128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_i128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_u8_trunc);
    }
    if (env.common.findIdent("f32_to_u8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_u16_trunc);
    }
    if (env.common.findIdent("f32_to_u16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_u32_trunc);
    }
    if (env.common.findIdent("f32_to_u32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_u64_trunc);
    }
    if (env.common.findIdent("f32_to_u64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_u128_wrap")) |ident| {
        try low_level_map.put(ident, .f32_to_u128_trunc);
    }
    if (env.common.findIdent("f32_to_u128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f32_to_u128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F32.to_f64")) |ident| {
        try low_level_map.put(ident, .f32_to_f64);
    }

    // F64 conversion operations
    if (env.common.findIdent("Builtin.Num.F64.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_i8_trunc);
    }
    if (env.common.findIdent("f64_to_i8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_i16_trunc);
    }
    if (env.common.findIdent("f64_to_i16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_i32_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_i32_trunc);
    }
    if (env.common.findIdent("f64_to_i32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_i64_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_i64_trunc);
    }
    if (env.common.findIdent("f64_to_i64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_i128_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_i128_trunc);
    }
    if (env.common.findIdent("f64_to_i128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_i128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_u8_trunc);
    }
    if (env.common.findIdent("f64_to_u8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_u16_trunc);
    }
    if (env.common.findIdent("f64_to_u16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_u32_trunc);
    }
    if (env.common.findIdent("f64_to_u32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .f64_to_u64_trunc);
    }
    if (env.common.findIdent("f64_to_u64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .f64_to_u64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.F64.to_u128_wrap")) |ident| {
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
    if (env.common.findIdent("Builtin.Num.Dec.to_i8_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_i8_trunc);
    }
    if (env.common.findIdent("dec_to_i8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_i16_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_i16_trunc);
    }
    if (env.common.findIdent("dec_to_i16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_i32_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_i32_trunc);
    }
    if (env.common.findIdent("dec_to_i32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_i64_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_i64_trunc);
    }
    if (env.common.findIdent("dec_to_i64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_i128_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_i128_trunc);
    }
    if (env.common.findIdent("dec_to_i128_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_i128_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u8_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_u8_trunc);
    }
    if (env.common.findIdent("dec_to_u8_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u8_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u16_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_u16_trunc);
    }
    if (env.common.findIdent("dec_to_u16_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u16_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u32_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_u32_trunc);
    }
    if (env.common.findIdent("dec_to_u32_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u32_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u64_wrap")) |ident| {
        try low_level_map.put(ident, .dec_to_u64_trunc);
    }
    if (env.common.findIdent("dec_to_u64_try_unsafe")) |ident| {
        try low_level_map.put(ident, .dec_to_u64_try_unsafe);
    }
    if (env.common.findIdent("Builtin.Num.Dec.to_u128_wrap")) |ident| {
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
                const entry = low_level_map.fetchRemove(ident) orelse {
                    if (isIntrinsicAnnotation(env, ident)) continue;

                    return error.UnsupportedBuiltinAnnotationOnly;
                };
                const low_level_op = entry.value;

                // Get the number of parameters from the type annotation
                // The annotation must be a function type for low-level operations
                const annotation = env.store.getAnnotation(def.annotation.?);
                const type_anno = env.store.getTypeAnno(annotation.anno);
                const num_params: u32 = switch (type_anno) {
                    .@"fn" => |func| func.args.span.len,
                    else => return error.BuiltinLowLevelAnnotationMustBeFunction,
                };

                // Create parameter patterns for the lambda
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

                // Create e_lookup_local expressions for each parameter
                const exprs_start = env.store.scratchExprTop();
                const param_patterns = env.store.slicePatterns(args_span);
                for (param_patterns) |pat_idx| {
                    const lookup_idx = try env.addExpr(.{ .e_lookup_local = .{
                        .pattern_idx = pat_idx,
                    } }, base.Region.zero());
                    try env.store.addScratchExpr(lookup_idx);
                }
                const lookup_span = try env.store.exprSpanFrom(exprs_start);

                // Create e_run_low_level body expression
                const body_idx = try env.addExpr(.{ .e_run_low_level = .{
                    .op = low_level_op,
                    .args = lookup_span,
                } }, base.Region.zero());

                // Create e_lambda expression wrapping the low-level body
                const expr_idx = try env.addExpr(.{ .e_lambda = .{
                    .args = args_span,
                    .body = body_idx,
                } }, base.Region.zero());

                // Now replace the e_anno_only expression with the e_lambda
                // Def structure is stored in def_data list
                const def_node_idx = @as(@TypeOf(env.store.nodes).Idx, @enumFromInt(@intFromEnum(def_idx)));
                const def_node = env.store.nodes.get(def_node_idx);
                const def_data_idx = def_node.getPayload().def.def_data_idx;

                // Update the expr field in def_data
                env.store.def_data.items.items[def_data_idx].expr = @intFromEnum(expr_idx);

                // Track this replaced def index
                try new_def_indices.append(gpa, def_idx);
            }
        }
    }

    // Verify all low-level operations were found in the builtins
    if (low_level_map.count() > 0) {
        return error.LowLevelOperationsNotFound;
    }

    return new_def_indices;
}
