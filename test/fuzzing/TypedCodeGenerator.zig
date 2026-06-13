//! Small typed Roc source generator for the typecheck fuzzer.
//!
//! This intentionally starts with a narrow, easy-to-audit subset. It generates a
//! type module named `Main` with associated functions whose annotations make the
//! intended types explicit.

const std = @import("std");
const BuiltinSurface = @import("BuiltinSurface.zig");
const FuzzReader = @import("FuzzReader.zig");

const Self = @This();

allocator: std.mem.Allocator,
reader: *FuzzReader,
output: std.ArrayList(u8),
support_output: std.ArrayList(u8),
tools_output: std.ArrayList(u8),
name_counter: u32,

const BuiltinAdapterKind = enum {
    bool_not,
    bool_encode,
    bool_decode,
    str_is_empty,
    str_concat,
    str_contains,
    str_trim,
    str_trim_start,
    str_trim_end,
    str_caseless_ascii_equals,
    str_with_ascii_lowercased,
    str_with_ascii_uppercased,
    str_starts_with,
    str_ends_with,
    str_repeat,
    str_with_prefix,
    str_drop_prefix,
    str_drop_suffix,
    str_count_utf8_bytes,
    str_with_capacity,
    str_reserve,
    str_release_excess_capacity,
    str_to_utf8,
    str_from_utf8_lossy,
    str_from_utf8,
    str_split_on,
    str_join_with,
    str_is_eq,
    str_inspect,
    str_encode,
    str_decode,
    list_len,
    list_is_empty,
    list_iter,
    list_from_iter,
    list_with_capacity,
    list_reserve,
    list_release_excess_capacity,
    list_sort_with,
    list_is_eq,
    list_first,
    list_get,
    list_subscript,
    list_append,
    list_prepend,
    list_concat,
    list_map,
    list_map2,
    list_keep_if,
    list_drop_if,
    list_any,
    list_all,
    list_contains,
    list_starts_with,
    list_ends_with,
    list_set,
    list_update,
    list_replace,
    list_rev,
    list_last,
    list_single,
    list_repeat,
    list_map_with_index,
    list_fold,
    list_fold_with_index,
    list_fold_until,
    list_fold_with_index_until,
    list_fold_rev,
    list_count_if,
    list_find_first,
    list_find_last,
    list_find_first_index,
    list_find_last_index,
    list_sublist,
    list_take_first,
    list_take_last,
    list_drop_first,
    list_drop_last,
    list_drop_at,
    list_swap,
    list_for_each_bang,
    list_split_at,
    list_split_on,
    list_split_if,
    list_split_on_list,
    list_split_first,
    list_split_last,
    list_join_with,
    list_join_list_with,
    list_sum,
    list_min,
    list_max,
    list_encode,
    list_decode,
    iter_next,
    iter_custom,
    iter_iter,
    iter_map,
    iter_keep_if,
    iter_drop_if,
    iter_fold,
    iter_size_hint,
    iter_collect,
    iter_stream,
    iter_take_first,
    iter_drop_first,
    iter_take_last,
    iter_drop_last,
    stream_from_iter,
    stream_map,
    stream_map_bang,
    stream_next_bang,
    stream_size_hint,
    stream_collect_bang,
    try_is_ok,
    try_is_err,
    try_ok_or,
    try_err_or,
    try_map_ok,
    try_map_err,
    try_map_ok_bang,
    try_map_err_bang,
    try_is_eq,
    box_box,
    box_unbox,
    bool_is_eq,
    dict_empty,
    dict_is_eq,
    dict_single,
    dict_len,
    dict_is_empty,
    dict_get,
    dict_contains,
    dict_insert,
    dict_remove,
    dict_to_list,
    dict_keys,
    dict_values,
    dict_fold,
    dict_keep_if,
    dict_drop_if,
    dict_map,
    dict_join_map,
    dict_insert_all,
    dict_keep_shared,
    dict_remove_all,
    dict_update,
    dict_from_list,
    set_empty,
    set_is_eq,
    set_single,
    set_len,
    set_is_empty,
    set_contains,
    set_insert,
    set_remove,
    set_to_list,
    set_keep_if,
    set_drop_if,
    set_union,
    set_intersection,
    set_difference,
    set_map,
    set_from_list,
    numeral_is_negative,
    utf8_problem_is_eq,
};

const BuiltinAdapter = struct {
    owner: []const u8,
    name: []const u8,
    kind: BuiltinAdapterKind,
};

const NumericAdapterKind = enum {
    default,
    to_str,
    is_zero,
    is_negative,
    is_positive,
    is_eq,
    is_gt,
    is_gte,
    is_lt,
    is_lte,
    max,
    min,
    abs,
    negate,
    plus,
    minus,
    times,
    div_by,
    div_trunc_by,
    add_checked,
    sub_checked,
    mul_checked,
    div_checked,
    plus_saturated,
    rem_by,
    mod_by,
    abs_diff,
    shift_left_by,
    shift_right_by,
    shift_right_zf_by,
    bitwise_and,
    bitwise_or,
    bitwise_xor,
    bitwise_not,
    from_int_digits,
    from_dec_digits,
    from_numeral,
    from_str,
    encode,
    decode,
    to,
    until,
};

const NumericAdapterFamily = enum {
    all,
    signed_or_float,
    integer,
    float,
    decimal,
};

const NumericAdapter = struct {
    name: []const u8,
    kind: NumericAdapterKind,
    family: NumericAdapterFamily,
};

const NumericConversionMode = enum {
    direct,
    try_result,
};

const NumericConversionAdapter = struct {
    source: Type,
    name: []const u8,
    result: Type,
    mode: NumericConversionMode,
};

const builtin_adapters = [_]BuiltinAdapter{
    .{ .owner = "Bool", .name = "not", .kind = .bool_not },
    .{ .owner = "Bool", .name = "encode", .kind = .bool_encode },
    .{ .owner = "Bool", .name = "decode", .kind = .bool_decode },
    .{ .owner = "Bool", .name = "is_eq", .kind = .bool_is_eq },
    .{ .owner = "Str", .name = "is_empty", .kind = .str_is_empty },
    .{ .owner = "Str", .name = "concat", .kind = .str_concat },
    .{ .owner = "Str", .name = "contains", .kind = .str_contains },
    .{ .owner = "Str", .name = "trim", .kind = .str_trim },
    .{ .owner = "Str", .name = "trim_start", .kind = .str_trim_start },
    .{ .owner = "Str", .name = "trim_end", .kind = .str_trim_end },
    .{ .owner = "Str", .name = "caseless_ascii_equals", .kind = .str_caseless_ascii_equals },
    .{ .owner = "Str", .name = "with_ascii_lowercased", .kind = .str_with_ascii_lowercased },
    .{ .owner = "Str", .name = "with_ascii_uppercased", .kind = .str_with_ascii_uppercased },
    .{ .owner = "Str", .name = "starts_with", .kind = .str_starts_with },
    .{ .owner = "Str", .name = "ends_with", .kind = .str_ends_with },
    .{ .owner = "Str", .name = "repeat", .kind = .str_repeat },
    .{ .owner = "Str", .name = "with_prefix", .kind = .str_with_prefix },
    .{ .owner = "Str", .name = "drop_prefix", .kind = .str_drop_prefix },
    .{ .owner = "Str", .name = "drop_suffix", .kind = .str_drop_suffix },
    .{ .owner = "Str", .name = "count_utf8_bytes", .kind = .str_count_utf8_bytes },
    .{ .owner = "Str", .name = "with_capacity", .kind = .str_with_capacity },
    .{ .owner = "Str", .name = "reserve", .kind = .str_reserve },
    .{ .owner = "Str", .name = "release_excess_capacity", .kind = .str_release_excess_capacity },
    .{ .owner = "Str", .name = "to_utf8", .kind = .str_to_utf8 },
    .{ .owner = "Str", .name = "from_utf8_lossy", .kind = .str_from_utf8_lossy },
    .{ .owner = "Str", .name = "from_utf8", .kind = .str_from_utf8 },
    .{ .owner = "Str", .name = "split_on", .kind = .str_split_on },
    .{ .owner = "Str", .name = "join_with", .kind = .str_join_with },
    .{ .owner = "Str", .name = "is_eq", .kind = .str_is_eq },
    .{ .owner = "Str", .name = "inspect", .kind = .str_inspect },
    .{ .owner = "Str", .name = "encode", .kind = .str_encode },
    .{ .owner = "Str", .name = "decode", .kind = .str_decode },
    .{ .owner = "List", .name = "len", .kind = .list_len },
    .{ .owner = "List", .name = "is_empty", .kind = .list_is_empty },
    .{ .owner = "List", .name = "iter", .kind = .list_iter },
    .{ .owner = "List", .name = "from_iter", .kind = .list_from_iter },
    .{ .owner = "List", .name = "with_capacity", .kind = .list_with_capacity },
    .{ .owner = "List", .name = "reserve", .kind = .list_reserve },
    .{ .owner = "List", .name = "release_excess_capacity", .kind = .list_release_excess_capacity },
    .{ .owner = "List", .name = "sort_with", .kind = .list_sort_with },
    .{ .owner = "List", .name = "is_eq", .kind = .list_is_eq },
    .{ .owner = "List", .name = "first", .kind = .list_first },
    .{ .owner = "List", .name = "get", .kind = .list_get },
    .{ .owner = "List", .name = "subscript", .kind = .list_subscript },
    .{ .owner = "List", .name = "append", .kind = .list_append },
    .{ .owner = "List", .name = "prepend", .kind = .list_prepend },
    .{ .owner = "List", .name = "concat", .kind = .list_concat },
    .{ .owner = "List", .name = "map", .kind = .list_map },
    .{ .owner = "List", .name = "map2", .kind = .list_map2 },
    .{ .owner = "List", .name = "keep_if", .kind = .list_keep_if },
    .{ .owner = "List", .name = "drop_if", .kind = .list_drop_if },
    .{ .owner = "List", .name = "any", .kind = .list_any },
    .{ .owner = "List", .name = "all", .kind = .list_all },
    .{ .owner = "List", .name = "contains", .kind = .list_contains },
    .{ .owner = "List", .name = "starts_with", .kind = .list_starts_with },
    .{ .owner = "List", .name = "ends_with", .kind = .list_ends_with },
    .{ .owner = "List", .name = "set", .kind = .list_set },
    .{ .owner = "List", .name = "update", .kind = .list_update },
    .{ .owner = "List", .name = "replace", .kind = .list_replace },
    .{ .owner = "List", .name = "rev", .kind = .list_rev },
    .{ .owner = "List", .name = "last", .kind = .list_last },
    .{ .owner = "List", .name = "single", .kind = .list_single },
    .{ .owner = "List", .name = "repeat", .kind = .list_repeat },
    .{ .owner = "List", .name = "map_with_index", .kind = .list_map_with_index },
    .{ .owner = "List", .name = "fold", .kind = .list_fold },
    .{ .owner = "List", .name = "fold_with_index", .kind = .list_fold_with_index },
    .{ .owner = "List", .name = "fold_until", .kind = .list_fold_until },
    .{ .owner = "List", .name = "fold_with_index_until", .kind = .list_fold_with_index_until },
    .{ .owner = "List", .name = "fold_rev", .kind = .list_fold_rev },
    .{ .owner = "List", .name = "count_if", .kind = .list_count_if },
    .{ .owner = "List", .name = "find_first", .kind = .list_find_first },
    .{ .owner = "List", .name = "find_last", .kind = .list_find_last },
    .{ .owner = "List", .name = "find_first_index", .kind = .list_find_first_index },
    .{ .owner = "List", .name = "find_last_index", .kind = .list_find_last_index },
    .{ .owner = "List", .name = "sublist", .kind = .list_sublist },
    .{ .owner = "List", .name = "take_first", .kind = .list_take_first },
    .{ .owner = "List", .name = "take_last", .kind = .list_take_last },
    .{ .owner = "List", .name = "drop_first", .kind = .list_drop_first },
    .{ .owner = "List", .name = "drop_last", .kind = .list_drop_last },
    .{ .owner = "List", .name = "drop_at", .kind = .list_drop_at },
    .{ .owner = "List", .name = "swap", .kind = .list_swap },
    .{ .owner = "List", .name = "for_each!", .kind = .list_for_each_bang },
    .{ .owner = "List", .name = "split_at", .kind = .list_split_at },
    .{ .owner = "List", .name = "split_on", .kind = .list_split_on },
    .{ .owner = "List", .name = "split_if", .kind = .list_split_if },
    .{ .owner = "List", .name = "split_on_list", .kind = .list_split_on_list },
    .{ .owner = "List", .name = "split_first", .kind = .list_split_first },
    .{ .owner = "List", .name = "split_last", .kind = .list_split_last },
    .{ .owner = "List", .name = "join_with", .kind = .list_join_with },
    .{ .owner = "List", .name = "join_list_with", .kind = .list_join_list_with },
    .{ .owner = "List", .name = "sum", .kind = .list_sum },
    .{ .owner = "List", .name = "min", .kind = .list_min },
    .{ .owner = "List", .name = "max", .kind = .list_max },
    .{ .owner = "List", .name = "encode", .kind = .list_encode },
    .{ .owner = "List", .name = "decode", .kind = .list_decode },
    .{ .owner = "Iter", .name = "next", .kind = .iter_next },
    .{ .owner = "Iter", .name = "custom", .kind = .iter_custom },
    .{ .owner = "Iter", .name = "iter", .kind = .iter_iter },
    .{ .owner = "Iter", .name = "map", .kind = .iter_map },
    .{ .owner = "Iter", .name = "keep_if", .kind = .iter_keep_if },
    .{ .owner = "Iter", .name = "drop_if", .kind = .iter_drop_if },
    .{ .owner = "Iter", .name = "fold", .kind = .iter_fold },
    .{ .owner = "Iter", .name = "size_hint", .kind = .iter_size_hint },
    .{ .owner = "Iter", .name = "collect", .kind = .iter_collect },
    .{ .owner = "Iter", .name = "stream", .kind = .iter_stream },
    .{ .owner = "Iter", .name = "take_first", .kind = .iter_take_first },
    .{ .owner = "Iter", .name = "drop_first", .kind = .iter_drop_first },
    .{ .owner = "Iter", .name = "take_last", .kind = .iter_take_last },
    .{ .owner = "Iter", .name = "drop_last", .kind = .iter_drop_last },
    .{ .owner = "Stream", .name = "from_iter", .kind = .stream_from_iter },
    .{ .owner = "Stream", .name = "map", .kind = .stream_map },
    .{ .owner = "Stream", .name = "map!", .kind = .stream_map_bang },
    .{ .owner = "Stream", .name = "next!", .kind = .stream_next_bang },
    .{ .owner = "Stream", .name = "size_hint", .kind = .stream_size_hint },
    .{ .owner = "Stream", .name = "collect!", .kind = .stream_collect_bang },
    .{ .owner = "Try", .name = "is_ok", .kind = .try_is_ok },
    .{ .owner = "Try", .name = "is_err", .kind = .try_is_err },
    .{ .owner = "Try", .name = "ok_or", .kind = .try_ok_or },
    .{ .owner = "Try", .name = "err_or", .kind = .try_err_or },
    .{ .owner = "Try", .name = "map_ok", .kind = .try_map_ok },
    .{ .owner = "Try", .name = "map_err", .kind = .try_map_err },
    .{ .owner = "Try", .name = "map_ok!", .kind = .try_map_ok_bang },
    .{ .owner = "Try", .name = "map_err!", .kind = .try_map_err_bang },
    .{ .owner = "Try", .name = "is_eq", .kind = .try_is_eq },
    .{ .owner = "Box", .name = "box", .kind = .box_box },
    .{ .owner = "Box", .name = "unbox", .kind = .box_unbox },
    .{ .owner = "Dict", .name = "empty", .kind = .dict_empty },
    .{ .owner = "Dict", .name = "is_eq", .kind = .dict_is_eq },
    .{ .owner = "Dict", .name = "single", .kind = .dict_single },
    .{ .owner = "Dict", .name = "len", .kind = .dict_len },
    .{ .owner = "Dict", .name = "is_empty", .kind = .dict_is_empty },
    .{ .owner = "Dict", .name = "get", .kind = .dict_get },
    .{ .owner = "Dict", .name = "contains", .kind = .dict_contains },
    .{ .owner = "Dict", .name = "insert", .kind = .dict_insert },
    .{ .owner = "Dict", .name = "remove", .kind = .dict_remove },
    .{ .owner = "Dict", .name = "to_list", .kind = .dict_to_list },
    .{ .owner = "Dict", .name = "keys", .kind = .dict_keys },
    .{ .owner = "Dict", .name = "values", .kind = .dict_values },
    .{ .owner = "Dict", .name = "fold", .kind = .dict_fold },
    .{ .owner = "Dict", .name = "keep_if", .kind = .dict_keep_if },
    .{ .owner = "Dict", .name = "drop_if", .kind = .dict_drop_if },
    .{ .owner = "Dict", .name = "map", .kind = .dict_map },
    .{ .owner = "Dict", .name = "join_map", .kind = .dict_join_map },
    .{ .owner = "Dict", .name = "insert_all", .kind = .dict_insert_all },
    .{ .owner = "Dict", .name = "keep_shared", .kind = .dict_keep_shared },
    .{ .owner = "Dict", .name = "remove_all", .kind = .dict_remove_all },
    .{ .owner = "Dict", .name = "update", .kind = .dict_update },
    .{ .owner = "Dict", .name = "from_list", .kind = .dict_from_list },
    .{ .owner = "Set", .name = "empty", .kind = .set_empty },
    .{ .owner = "Set", .name = "is_eq", .kind = .set_is_eq },
    .{ .owner = "Set", .name = "single", .kind = .set_single },
    .{ .owner = "Set", .name = "len", .kind = .set_len },
    .{ .owner = "Set", .name = "is_empty", .kind = .set_is_empty },
    .{ .owner = "Set", .name = "contains", .kind = .set_contains },
    .{ .owner = "Set", .name = "insert", .kind = .set_insert },
    .{ .owner = "Set", .name = "remove", .kind = .set_remove },
    .{ .owner = "Set", .name = "to_list", .kind = .set_to_list },
    .{ .owner = "Set", .name = "keep_if", .kind = .set_keep_if },
    .{ .owner = "Set", .name = "drop_if", .kind = .set_drop_if },
    .{ .owner = "Set", .name = "union", .kind = .set_union },
    .{ .owner = "Set", .name = "intersection", .kind = .set_intersection },
    .{ .owner = "Set", .name = "difference", .kind = .set_difference },
    .{ .owner = "Set", .name = "map", .kind = .set_map },
    .{ .owner = "Set", .name = "from_list", .kind = .set_from_list },
    .{ .owner = "Numeral", .name = "is_negative", .kind = .numeral_is_negative },
    .{ .owner = "Utf8Problem", .name = "is_eq", .kind = .utf8_problem_is_eq },
};

const numeric_adapters = [_]NumericAdapter{
    .{ .name = "default", .kind = .default, .family = .all },
    .{ .name = "to_str", .kind = .to_str, .family = .all },
    .{ .name = "is_zero", .kind = .is_zero, .family = .all },
    .{ .name = "is_negative", .kind = .is_negative, .family = .signed_or_float },
    .{ .name = "is_positive", .kind = .is_positive, .family = .signed_or_float },
    .{ .name = "is_eq", .kind = .is_eq, .family = .all },
    .{ .name = "is_gt", .kind = .is_gt, .family = .all },
    .{ .name = "is_gte", .kind = .is_gte, .family = .all },
    .{ .name = "is_lt", .kind = .is_lt, .family = .all },
    .{ .name = "is_lte", .kind = .is_lte, .family = .all },
    .{ .name = "max", .kind = .max, .family = .all },
    .{ .name = "min", .kind = .min, .family = .all },
    .{ .name = "abs", .kind = .abs, .family = .signed_or_float },
    .{ .name = "negate", .kind = .negate, .family = .signed_or_float },
    .{ .name = "plus", .kind = .plus, .family = .all },
    .{ .name = "minus", .kind = .minus, .family = .all },
    .{ .name = "times", .kind = .times, .family = .all },
    .{ .name = "div_by", .kind = .div_by, .family = .all },
    .{ .name = "div_trunc_by", .kind = .div_trunc_by, .family = .all },
    .{ .name = "add_checked", .kind = .add_checked, .family = .integer },
    .{ .name = "sub_checked", .kind = .sub_checked, .family = .integer },
    .{ .name = "mul_checked", .kind = .mul_checked, .family = .integer },
    .{ .name = "div_checked", .kind = .div_checked, .family = .integer },
    .{ .name = "plus_saturated", .kind = .plus_saturated, .family = .integer },
    .{ .name = "rem_by", .kind = .rem_by, .family = .all },
    .{ .name = "mod_by", .kind = .mod_by, .family = .integer },
    .{ .name = "abs_diff", .kind = .abs_diff, .family = .all },
    .{ .name = "shift_left_by", .kind = .shift_left_by, .family = .integer },
    .{ .name = "shift_right_by", .kind = .shift_right_by, .family = .integer },
    .{ .name = "shift_right_zf_by", .kind = .shift_right_zf_by, .family = .integer },
    .{ .name = "bitwise_and", .kind = .bitwise_and, .family = .integer },
    .{ .name = "bitwise_or", .kind = .bitwise_or, .family = .integer },
    .{ .name = "bitwise_xor", .kind = .bitwise_xor, .family = .integer },
    .{ .name = "bitwise_not", .kind = .bitwise_not, .family = .integer },
    .{ .name = "from_int_digits", .kind = .from_int_digits, .family = .all },
    .{ .name = "from_dec_digits", .kind = .from_dec_digits, .family = .float },
    .{ .name = "from_numeral", .kind = .from_numeral, .family = .all },
    .{ .name = "from_str", .kind = .from_str, .family = .all },
    .{ .name = "encode", .kind = .encode, .family = .all },
    .{ .name = "decode", .kind = .decode, .family = .all },
    .{ .name = "to", .kind = .to, .family = .integer },
    .{ .name = "until", .kind = .until, .family = .integer },
    .{ .name = "add_checked", .kind = .add_checked, .family = .decimal },
    .{ .name = "sub_checked", .kind = .sub_checked, .family = .decimal },
    .{ .name = "plus_saturated", .kind = .plus_saturated, .family = .decimal },
    .{ .name = "from_dec_digits", .kind = .from_dec_digits, .family = .decimal },
    .{ .name = "to", .kind = .to, .family = .decimal },
    .{ .name = "until", .kind = .until, .family = .decimal },
};

const number_owner_names = [_][]const u8{ "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "Dec", "F32", "F64" };
const signed_or_float_number_owner_names = [_][]const u8{ "I8", "I16", "I32", "I64", "I128", "Dec", "F32", "F64" };
const integer_owner_names = [_][]const u8{ "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128" };
const float_owner_names = [_][]const u8{ "F32", "F64" };
const decimal_owner_names = [_][]const u8{"Dec"};

const max_numeric_conversion_adapters = 384;

const NumericConversionAdapterInventory = struct {
    items: [max_numeric_conversion_adapters]NumericConversionAdapter,
    len: usize,
};

const numeric_conversion_adapter_inventory = buildNumericConversionAdapters();
const numeric_conversion_adapters = numeric_conversion_adapter_inventory.items[0..numeric_conversion_adapter_inventory.len];

const ParsedNumericConversion = struct {
    result: Type,
    mode: NumericConversionMode,
};

fn buildNumericConversionAdapters() NumericConversionAdapterInventory {
    @setEvalBranchQuota(5_000_000);

    var result: NumericConversionAdapterInventory = .{ .items = undefined, .len = 0 };
    for (BuiltinSurface.functions) |function| {
        const source = typeFromNumberOwner(function.owner) orelse continue;
        if (!isNumericConversionName(function.name)) continue;
        const parsed = parseNumericConversionSignature(function.signature) orelse continue;

        if (result.len >= result.items.len) @compileError("numeric conversion builtin adapter inventory exceeded capacity");
        result.items[result.len] = .{
            .source = source,
            .name = function.name,
            .result = parsed.result,
            .mode = parsed.mode,
        };
        result.len += 1;
    }
    return result;
}

fn isNumericConversionName(name: []const u8) bool {
    if (!std.mem.startsWith(u8, name, "to_")) return false;
    if (std.mem.eql(u8, name, "to_str")) return false;
    return true;
}

fn parseNumericConversionSignature(signature: []const u8) ?ParsedNumericConversion {
    const arrow = std.mem.indexOf(u8, signature, "->") orelse return null;
    var rest = trimLeft(signature[arrow + "->".len ..]);

    const mode: NumericConversionMode = if (std.mem.startsWith(u8, rest, "Try(")) mode: {
        rest = trimLeft(rest["Try(".len..]);
        break :mode .try_result;
    } else .direct;

    const result_name_end = builtinIdentifierEnd(rest, 0);
    if (result_name_end == 0) return null;
    const result_type = typeFromBuiltinName(rest[0..result_name_end]) orelse return null;

    return .{ .result = result_type, .mode = mode };
}

comptime {
    BuiltinSurface.require("List", "iter");

    for (builtin_adapters) |adapter| {
        BuiltinSurface.require(adapter.owner, adapter.name);
    }
    for (numeric_adapters) |adapter| {
        switch (adapter.family) {
            .all => for (number_owner_names) |owner| {
                BuiltinSurface.require(owner, adapter.name);
            },
            .signed_or_float => for (signed_or_float_number_owner_names) |owner| {
                BuiltinSurface.require(owner, adapter.name);
            },
            .integer => for (integer_owner_names) |owner| {
                BuiltinSurface.require(owner, adapter.name);
            },
            .float => for (float_owner_names) |owner| {
                BuiltinSurface.require(owner, adapter.name);
            },
            .decimal => for (decimal_owner_names) |owner| {
                BuiltinSurface.require(owner, adapter.name);
            },
        }
    }
    for (numeric_conversion_adapters) |adapter| {
        BuiltinSurface.require(numberOwner(adapter.source), adapter.name);
    }
    requireCoveredBuiltinSurface();
}

fn requireCoveredBuiltinSurface() void {
    @setEvalBranchQuota(5_000_000);

    for (BuiltinSurface.functions) |function| {
        if (isGeneratedBuiltinFunction(function) or isUnsupportedBuiltinFunction(function)) continue;
        @compileError("typecheck fuzzer builtin surface is missing " ++ function.owner ++ "." ++ function.name);
    }
}

fn isGeneratedBuiltinFunction(function: BuiltinSurface.Function) bool {
    for (builtin_adapters) |adapter| {
        if (std.mem.eql(u8, function.owner, adapter.owner) and std.mem.eql(u8, function.name, adapter.name)) {
            return true;
        }
    }

    for (numeric_adapters) |adapter| {
        if (!std.mem.eql(u8, function.name, adapter.name)) continue;
        switch (adapter.family) {
            .all => if (ownerIn(function.owner, &number_owner_names)) return true,
            .signed_or_float => if (ownerIn(function.owner, &signed_or_float_number_owner_names)) return true,
            .integer => if (ownerIn(function.owner, &integer_owner_names)) return true,
            .float => if (ownerIn(function.owner, &float_owner_names)) return true,
            .decimal => if (ownerIn(function.owner, &decimal_owner_names)) return true,
        }
    }

    for (numeric_conversion_adapters) |adapter| {
        if (std.mem.eql(u8, function.owner, numberOwner(adapter.source)) and std.mem.eql(u8, function.name, adapter.name)) {
            return true;
        }
    }

    return false;
}

fn isUnsupportedBuiltinFunction(function: BuiltinSurface.Function) bool {
    if (std.mem.eql(u8, function.owner, "Builtin")) return true;

    return false;
}

fn ownerIn(owner: []const u8, owners: []const []const u8) bool {
    for (owners) |candidate| {
        if (std.mem.eql(u8, owner, candidate)) return true;
    }
    return false;
}

const Type = enum {
    main,
    support,
    tree,
    my_num,
    bool,
    str,
    record_bool,
    record_str_u64,
    holder_bool,
    holder_str,
    holder_u64,
    holder_record_bool,
    holder_list_u64,
    holder_try_u64_str,
    builder_bool,
    builder_str,
    builder_u64,
    builder_record_str_u64,
    builder_list_u64,
    builder_try_u64_str,
    wrap_bool,
    wrap_str,
    wrap_u64,
    wrap_record_str_u64,
    wrap_list_u64,
    wrap_try_u64_str,
    secret_bool,
    secret_str,
    secret_u64,
    secret_record_str_u64,
    secret_list_u64,
    secret_try_u64_str,
    box_bool,
    box_str,
    box_u64,
    box_record_str_u64,
    box_list_u64,
    box_try_u64_str,
    dict_str_u64,
    dict_u64_str,
    set_str,
    set_u64,
    fn_bool_bool,
    fn_u64_u64,
    fn_str_str,
    tuple_bool_u64,
    tuple_str_u64,
    tuple_u64_str,
    tuple_record_bool_u64,
    tuple_try_u64_str_bool,
    tuple_list_u64_str,
    try_bool_str,
    try_str_bool,
    try_u64_str,
    try_list_u64_str,
    list_bool,
    list_str,
    list_u64,
    list_record_bool,
    list_list_u64,
    list_tuple_bool_u64,
    list_tuple_str_u64,
    list_tuple_u64_str,
    list_try_u64_str,
    list_holder_u64,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    dec,
    f32,
    f64,

    fn isSupport(self: Type) bool {
        return self == .support;
    }

    fn isList(self: Type) bool {
        return switch (self) {
            .list_bool,
            .list_str,
            .list_u64,
            .list_record_bool,
            .list_list_u64,
            .list_tuple_bool_u64,
            .list_tuple_str_u64,
            .list_tuple_u64_str,
            .list_try_u64_str,
            .list_holder_u64,
            => true,
            else => false,
        };
    }

    fn listElement(self: Type) Type {
        return switch (self) {
            .list_bool => .bool,
            .list_str => .str,
            .list_u64 => .u64,
            .list_record_bool => .record_bool,
            .list_list_u64 => .list_u64,
            .list_tuple_bool_u64 => .tuple_bool_u64,
            .list_tuple_str_u64 => .tuple_str_u64,
            .list_tuple_u64_str => .tuple_u64_str,
            .list_try_u64_str => .try_u64_str,
            .list_holder_u64 => .holder_u64,
            else => unreachable,
        };
    }

    fn isRecord(self: Type) bool {
        return switch (self) {
            .record_bool, .record_str_u64 => true,
            else => false,
        };
    }

    fn isHolder(self: Type) bool {
        return switch (self) {
            .holder_bool,
            .holder_str,
            .holder_u64,
            .holder_record_bool,
            .holder_list_u64,
            .holder_try_u64_str,
            => true,
            else => false,
        };
    }

    fn isFunction(self: Type) bool {
        return switch (self) {
            .fn_bool_bool, .fn_u64_u64, .fn_str_str => true,
            else => false,
        };
    }

    fn isBuilder(self: Type) bool {
        return switch (self) {
            .builder_bool,
            .builder_str,
            .builder_u64,
            .builder_record_str_u64,
            .builder_list_u64,
            .builder_try_u64_str,
            => true,
            else => false,
        };
    }

    fn isWrap(self: Type) bool {
        return switch (self) {
            .wrap_bool,
            .wrap_str,
            .wrap_u64,
            .wrap_record_str_u64,
            .wrap_list_u64,
            .wrap_try_u64_str,
            => true,
            else => false,
        };
    }

    fn isSecret(self: Type) bool {
        return switch (self) {
            .secret_bool,
            .secret_str,
            .secret_u64,
            .secret_record_str_u64,
            .secret_list_u64,
            .secret_try_u64_str,
            => true,
            else => false,
        };
    }

    fn isBox(self: Type) bool {
        return switch (self) {
            .box_bool,
            .box_str,
            .box_u64,
            .box_record_str_u64,
            .box_list_u64,
            .box_try_u64_str,
            => true,
            else => false,
        };
    }

    fn isDict(self: Type) bool {
        return switch (self) {
            .dict_str_u64, .dict_u64_str => true,
            else => false,
        };
    }

    fn isSet(self: Type) bool {
        return switch (self) {
            .set_str, .set_u64 => true,
            else => false,
        };
    }

    fn isTuple(self: Type) bool {
        return switch (self) {
            .tuple_bool_u64,
            .tuple_str_u64,
            .tuple_u64_str,
            .tuple_record_bool_u64,
            .tuple_try_u64_str_bool,
            .tuple_list_u64_str,
            => true,
            else => false,
        };
    }

    fn isTry(self: Type) bool {
        return switch (self) {
            .try_bool_str, .try_str_bool, .try_u64_str, .try_list_u64_str => true,
            else => false,
        };
    }

    fn isNumber(self: Type) bool {
        return switch (self) {
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec, .f32, .f64 => true,
            else => false,
        };
    }

    fn isSimpleEquatable(self: Type) bool {
        return switch (self) {
            .main, .support, .my_num, .bool, .str => true,
            else => false,
        };
    }

    fn isTree(self: Type) bool {
        return self == .tree;
    }

    fn isFloat(self: Type) bool {
        return switch (self) {
            .f32, .f64 => true,
            else => false,
        };
    }

    fn isUnsignedInteger(self: Type) bool {
        return switch (self) {
            .u8, .u16, .u32, .u64, .u128 => true,
            else => false,
        };
    }

    fn functionResult(self: Type) Type {
        return switch (self) {
            .fn_bool_bool => .bool,
            .fn_u64_u64 => .u64,
            .fn_str_str => .str,
            else => unreachable,
        };
    }

    fn holderInner(self: Type) Type {
        return switch (self) {
            .holder_bool => .bool,
            .holder_str => .str,
            .holder_u64 => .u64,
            .holder_record_bool => .record_bool,
            .holder_list_u64 => .list_u64,
            .holder_try_u64_str => .try_u64_str,
            else => unreachable,
        };
    }

    fn builderInner(self: Type) Type {
        return switch (self) {
            .builder_bool => .bool,
            .builder_str => .str,
            .builder_u64 => .u64,
            .builder_record_str_u64 => .record_str_u64,
            .builder_list_u64 => .list_u64,
            .builder_try_u64_str => .try_u64_str,
            else => unreachable,
        };
    }

    fn wrapInner(self: Type) Type {
        return switch (self) {
            .wrap_bool => .bool,
            .wrap_str => .str,
            .wrap_u64 => .u64,
            .wrap_record_str_u64 => .record_str_u64,
            .wrap_list_u64 => .list_u64,
            .wrap_try_u64_str => .try_u64_str,
            else => unreachable,
        };
    }

    fn secretInner(self: Type) Type {
        return switch (self) {
            .secret_bool => .bool,
            .secret_str => .str,
            .secret_u64 => .u64,
            .secret_record_str_u64 => .record_str_u64,
            .secret_list_u64 => .list_u64,
            .secret_try_u64_str => .try_u64_str,
            else => unreachable,
        };
    }

    fn boxInner(self: Type) Type {
        return switch (self) {
            .box_bool => .bool,
            .box_str => .str,
            .box_u64 => .u64,
            .box_record_str_u64 => .record_str_u64,
            .box_list_u64 => .list_u64,
            .box_try_u64_str => .try_u64_str,
            else => unreachable,
        };
    }

    fn dictKey(self: Type) Type {
        return switch (self) {
            .dict_str_u64 => .str,
            .dict_u64_str => .u64,
            else => unreachable,
        };
    }

    fn dictValue(self: Type) Type {
        return switch (self) {
            .dict_str_u64 => .u64,
            .dict_u64_str => .str,
            else => unreachable,
        };
    }

    fn dictPairList(self: Type) Type {
        return switch (self) {
            .dict_str_u64 => .list_tuple_str_u64,
            .dict_u64_str => .list_tuple_u64_str,
            else => unreachable,
        };
    }

    fn setElement(self: Type) Type {
        return switch (self) {
            .set_str => .str,
            .set_u64 => .u64,
            else => unreachable,
        };
    }

    fn setList(self: Type) Type {
        return switch (self) {
            .set_str => .list_str,
            .set_u64 => .list_u64,
            else => unreachable,
        };
    }

    fn tryOk(self: Type) Type {
        return switch (self) {
            .try_bool_str => .bool,
            .try_str_bool => .str,
            .try_u64_str => .u64,
            .try_list_u64_str => .list_u64,
            else => unreachable,
        };
    }

    fn tryErr(self: Type) Type {
        return switch (self) {
            .try_bool_str, .try_u64_str, .try_list_u64_str => .str,
            .try_str_bool => .bool,
            else => unreachable,
        };
    }
};

const Expr = union(enum) {
    raw: []const u8,
    literal: Type,
    type_name: Type,
    bool_literal,
    string_literal,
    utf8_bytes_literal,
    numeral_literal,
    integer_literal,
    small_number_literal: Type,
};

const RecordField = struct {
    name: []const u8,
    value: Expr,
};

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .output = .empty,
        .support_output = .empty,
        .tools_output = .empty,
        .name_counter = 0,
    };
}

pub fn deinit(self: *Self) void {
    self.output.deinit(self.allocator);
    self.support_output.deinit(self.allocator);
    self.tools_output.deinit(self.allocator);
}

pub fn getOutput(self: *const Self) []const u8 {
    return self.output.items;
}

pub fn getSupportOutput(self: *const Self) []const u8 {
    return self.support_output.items;
}

pub fn getToolsOutput(self: *const Self) []const u8 {
    return self.tools_output.items;
}

fn generateSupportModule(self: *Self) std.mem.Allocator.Error!void {
    try self.writeSupport(
        \\Support := [Empty, Packet({ name : Str, count : U64 }), Many(List(U64))].{
        \\    make : U64 -> Support
        \\    make = |count| Packet({ name: "support", count: count })
        \\    from_list : List(U64) -> Support
        \\    from_list = |items| Many(items)
        \\    from_pair : (Str, U64) -> Support
        \\    from_pair = |pair| match pair {
        \\        (name, count) => Packet({ name: name, count: count })
        \\    }
        \\    to_pair : Support -> (Str, U64)
        \\    to_pair = |item| match item {
        \\        Empty => ("empty", 0)
        \\        Packet(record) => (record.name, record.count)
        \\        Many(items) => ("many", items.len())
        \\    }
        \\    count : Support -> U64
        \\    count = |item| match item {
        \\        Empty => 0
        \\        Packet(record) => record.count
        \\        Many(items) => items.len()
        \\    }
        \\    label : Support -> Str
        \\    label = |item| match item {
        \\        Empty => "empty"
        \\        Packet(record) => record.name
        \\        Many(_) => "many"
        \\    }
        \\    map_count : Support, (U64 -> U64) -> Support
        \\    map_count = |item, transform| match item {
        \\        Empty => Packet({ name: "empty", count: transform(0) })
        \\        Packet(record) => Packet({ name: record.name, count: transform(record.count) })
        \\        Many(items) => Packet({ name: "many", count: transform(items.len()) })
        \\    }
        \\    maybe : Support -> Try(U64, Str)
        \\    maybe = |item| match item {
        \\        Empty => Err("empty")
        \\        Packet(record) => Ok(record.count)
        \\        Many(items) => Ok(items.len())
        \\    }
        \\    is_eq : Support, Support -> Bool
        \\    is_eq = |left, right| if Support.count(left) == Support.count(right) Support.label(left) == Support.label(right) else False
        \\}
        \\
    );
}

fn generateToolsModule(self: *Self) std.mem.Allocator.Error!void {
    try self.writeTools(
        \\import Support exposing [Support]
        \\
        \\
        \\Tools := [Tool({ name : Str, count : U64 })].{
        \\    make : U64 -> Tools
        \\    make = |count| Tool({ name: "tool", count: count })
        \\    from_support : Support -> Tools
        \\    from_support = |item| Tool({ name: Support.label(item), count: Support.count(item) })
        \\    count : Tools -> U64
        \\    count = |item| match item {
        \\        Tool(record) => record.count
        \\    }
        \\    label : Tools -> Str
        \\    label = |item| match item {
        \\        Tool(record) => record.name
        \\    }
        \\    map_count : Tools, (U64 -> U64) -> Tools
        \\    map_count = |item, transform| match item {
        \\        Tool(record) => Tool({ name: record.name, count: transform(record.count) })
        \\    }
        \\    is_eq : Tools, Tools -> Bool
        \\    is_eq = |left, right| Tools.count(left) == Tools.count(right) and Tools.label(left) == Tools.label(right)
        \\}
        \\
    );
}

pub fn generateModule(self: *Self) std.mem.Allocator.Error!void {
    try self.generateSupportModule();
    try self.generateToolsModule();
    try self.write(
        \\import Support
        \\import Tools exposing [Tools]
        \\
        \\
        \\AliasBool : Bool
        \\U64s : List(U64)
        \\NameCount : { name : Str, count : U64 }
        \\Pair(a, b) : (a, b)
        \\Fallible(ok, err) : Try(ok, err)
        \\Color(others) : [Red, Green, Blue, Other(Str), ..others]
        \\
        \\
        \\MyNum := [MyNum(U64)].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(MyNum(1))
        \\    negate : MyNum -> MyNum
        \\    negate = |_| MyNum(0)
        \\    plus : MyNum, MyNum -> MyNum
        \\    plus = |_, _| MyNum(2)
        \\    is_eq : MyNum, MyNum -> Bool
        \\    is_eq = |_, _| True
        \\}
        \\
        \\
        \\Holder(a) := { value : a }.{
        \\    make : a -> Holder(a)
        \\    make = |value| { value: value }
        \\    get : Holder(a) -> a
        \\    get = |holder| holder.value
        \\    map : Holder(a), (a -> b) -> Holder(b)
        \\    map = |holder, f| { value: f(holder.value) }
        \\    map2 : Holder(a), Holder(b), (a, b -> c) -> Holder(c)
        \\    map2 = |left, right, f| { value: f(left.value, right.value) }
        \\}
        \\
        \\
        \\Wrap(a) := [Wrap(a)].{
        \\    make : a -> Wrap(a)
        \\    make = |value| Wrap(value)
        \\    get : Wrap(a) -> a
        \\    get = |wrapped| match wrapped {
        \\        Wrap(value) => value
        \\    }
        \\    map : Wrap(a), (a -> b) -> Wrap(b)
        \\    map = |wrapped, f| match wrapped {
        \\        Wrap(value) => Wrap(f(value))
        \\    }
        \\    map2 : Wrap(a), Wrap(b), (a, b -> c) -> Wrap(c)
        \\    map2 = |left, right, f| match (left, right) {
        \\        (Wrap(left_value), Wrap(right_value)) => Wrap(f(left_value, right_value))
        \\    }
        \\}
        \\
        \\
        \\Secret(a) :: { value : a }.{
        \\    make : a -> Secret(a)
        \\    make = |value| { value: value }
        \\    get : Secret(a) -> a
        \\    get = |secret| secret.value
        \\    map : Secret(a), (a -> b) -> Secret(b)
        \\    map = |secret, f| { value: f(secret.value) }
        \\    map2 : Secret(a), Secret(b), (a, b -> c) -> Secret(c)
        \\    map2 = |left, right, f| { value: f(left.value, right.value) }
        \\}
        \\
        \\
        \\Builder(a) := { value : a, help : Str }.{
        \\    map2 : Builder(a), Builder(b), (a, b -> c) -> Builder(c)
        \\    map2 = |left, right, f| { value: f(left.value, right.value), help: Str.concat(left.help, right.help) }
        \\    field : a -> Builder(a)
        \\    field = |value| { value: value, help: "h" }
        \\    run : Builder(a) -> a
        \\    run = |builder| builder.value
        \\}
        \\
        \\
        \\Fmt := [Fmt].{
        \\    encode_bool : Fmt, Bool -> Try(Str, Str)
        \\    encode_bool = |_, _| Ok("bool")
        \\    decode_bool : Fmt, Str -> (Try(Bool, Str), Str)
        \\    decode_bool = |_, source| (Ok(True), source)
        \\    encode_str : Fmt, Str -> Try(Str, Str)
        \\    encode_str = |_, value| Ok(value)
        \\    decode_str : Fmt, Str -> (Try(Str, Str), Str)
        \\    decode_str = |_, source| (Ok(source), source)
        \\    encode_list : Fmt, List(item), (item, Fmt -> Try(Str, Str)) -> Try(Str, Str)
        \\    encode_list = |_, _, _| Ok("list")
        \\    decode_list : Fmt, Str, (Str, Fmt -> (Try(item, Str), Str)) -> (Try(List(item), Str), Str)
        \\    decode_list = |_, source, _| (Ok([]), source)
        \\    encode_u8 : Fmt, U8 -> Try(Str, Str)
        \\    encode_u8 = |_, _| Ok("u8")
        \\    decode_u8 : Fmt, Str -> (Try(U8, Str), Str)
        \\    decode_u8 = |_, source| (Ok(1), source)
        \\    encode_i8 : Fmt, I8 -> Try(Str, Str)
        \\    encode_i8 = |_, _| Ok("i8")
        \\    decode_i8 : Fmt, Str -> (Try(I8, Str), Str)
        \\    decode_i8 = |_, source| (Ok(1), source)
        \\    encode_u16 : Fmt, U16 -> Try(Str, Str)
        \\    encode_u16 = |_, _| Ok("u16")
        \\    decode_u16 : Fmt, Str -> (Try(U16, Str), Str)
        \\    decode_u16 = |_, source| (Ok(1), source)
        \\    encode_i16 : Fmt, I16 -> Try(Str, Str)
        \\    encode_i16 = |_, _| Ok("i16")
        \\    decode_i16 : Fmt, Str -> (Try(I16, Str), Str)
        \\    decode_i16 = |_, source| (Ok(1), source)
        \\    encode_u32 : Fmt, U32 -> Try(Str, Str)
        \\    encode_u32 = |_, _| Ok("u32")
        \\    decode_u32 : Fmt, Str -> (Try(U32, Str), Str)
        \\    decode_u32 = |_, source| (Ok(1), source)
        \\    encode_i32 : Fmt, I32 -> Try(Str, Str)
        \\    encode_i32 = |_, _| Ok("i32")
        \\    decode_i32 : Fmt, Str -> (Try(I32, Str), Str)
        \\    decode_i32 = |_, source| (Ok(1), source)
        \\    encode_u64 : Fmt, U64 -> Try(Str, Str)
        \\    encode_u64 = |_, _| Ok("u64")
        \\    decode_u64 : Fmt, Str -> (Try(U64, Str), Str)
        \\    decode_u64 = |_, source| (Ok(1), source)
        \\    encode_i64 : Fmt, I64 -> Try(Str, Str)
        \\    encode_i64 = |_, _| Ok("i64")
        \\    decode_i64 : Fmt, Str -> (Try(I64, Str), Str)
        \\    decode_i64 = |_, source| (Ok(1), source)
        \\    encode_u128 : Fmt, U128 -> Try(Str, Str)
        \\    encode_u128 = |_, _| Ok("u128")
        \\    decode_u128 : Fmt, Str -> (Try(U128, Str), Str)
        \\    decode_u128 = |_, source| (Ok(1), source)
        \\    encode_i128 : Fmt, I128 -> Try(Str, Str)
        \\    encode_i128 = |_, _| Ok("i128")
        \\    decode_i128 : Fmt, Str -> (Try(I128, Str), Str)
        \\    decode_i128 = |_, source| (Ok(1), source)
        \\    encode_dec : Fmt, Dec -> Try(Str, Str)
        \\    encode_dec = |_, _| Ok("dec")
        \\    decode_dec : Fmt, Str -> (Try(Dec, Str), Str)
        \\    decode_dec = |_, source| (Ok(1.0), source)
        \\    encode_f32 : Fmt, F32 -> Try(Str, Str)
        \\    encode_f32 = |_, _| Ok("f32")
        \\    decode_f32 : Fmt, Str -> (Try(F32, Str), Str)
        \\    decode_f32 = |_, source| (Ok(1.0), source)
        \\    encode_f64 : Fmt, F64 -> Try(Str, Str)
        \\    encode_f64 = |_, _| Ok("f64")
        \\    decode_f64 : Fmt, Str -> (Try(F64, Str), Str)
        \\    decode_f64 = |_, source| (Ok(1.0), source)
        \\}
        \\
        \\
        \\Tree := [Leaf(Str), Node(List(Tree))].{
        \\    label : Tree -> Str
        \\    label = |tree| match tree {
        \\        Leaf(name) => name
        \\        Node(_) => "node"
        \\    }
        \\}
        \\
        \\
    );
    try self.write("Main := [Main, WithBool(Bool), WithStrU64(Str, U64)].{\n");
    try self.write(
        \\    is_eq : Main, Main -> Bool
        \\    is_eq = |left, right| match (left, right) {
        \\        (Main, Main) => True
        \\        (WithBool(a), WithBool(b)) => a == b
        \\        (WithStrU64(_, a), WithStrU64(_, b)) => a == b
        \\        _ => False
        \\    }
        \\    try_one : Main -> Try(U64, _)
        \\    try_one = |_| Ok(1)
        \\    try_chain : Main -> Try(U64, _)
        \\    try_chain = |_| {
        \\        value = Main.try_one(Main)?
        \\        Ok(value)
        \\    }
        \\    poly_record : Main -> { flag : Bool, count : U64 }
        \\    poly_record = |_| {
        \\        id = |value| value
        \\        { flag: id(True), count: id(1) }
        \\    }
        \\    poly_tuple : Main -> (Bool, U64)
        \\    poly_tuple = |_| {
        \\        id = |value| value
        \\        (id(False), id(2))
        \\    }
        \\    builder_wrapped : Main -> Builder({ a : U64, b : Bool })
        \\    builder_wrapped = |_| { a: Builder.field(1), b: Builder.field(True) }.Builder
        \\    builder_unwrapped : Main -> U64
        \\    builder_unwrapped = |_| { a: Builder.field(2), b: Builder.field(True) }.Builder.run().a
        \\    where_eq : a, a -> Bool where [a.is_eq : a, a -> Bool]
        \\    where_eq = |left, right| left == right
        \\    where_plus : a, a -> a where [a.plus : a, a -> a]
        \\    where_plus = |left, right| left + right
        \\    where_map : a, (b -> c) -> d where [a.map : a, (b -> c) -> d]
        \\    where_map = |container, transform| container.map(transform)
        \\    has_count : { count : U64, .. } -> U64
        \\    has_count = |record| record.count
    );
    try self.write("\n");

    try self.generateFunction(.main);
    try self.generateFunction(.support);
    try self.generateFunction(.tree);
    try self.generateFunction(.my_num);
    try self.generateFunction(.bool);
    try self.generateFunction(.str);
    try self.generateFunction(.record_bool);
    try self.generateFunction(.u64);
    try self.generateFunction(.list_u64);
    try self.generateFunction(.try_u64_str);
    try self.generateFunction(.list_try_u64_str);
    try self.generateFunction(.holder_list_u64);
    try self.generateFunction(.builder_u64);
    try self.generateFunction(.wrap_u64);
    try self.generateFunction(.secret_u64);
    try self.generateFunction(.box_u64);
    try self.generateFunction(.dict_str_u64);
    try self.generateFunction(.dict_u64_str);
    try self.generateFunction(.set_u64);
    try self.generateFunction(.set_str);

    const loop_count = self.reader.intRangeAtMost(u8, 1, 4);
    for (0..loop_count) |_| {
        try self.generateForLoopFunction();
    }

    const pattern_count = self.reader.intRangeAtMost(u8, 1, 4);
    for (0..pattern_count) |_| {
        try self.generatePatternFunction();
    }

    const recursion_count = self.reader.intRangeAtMost(u8, 1, 2);
    for (0..recursion_count) |_| {
        try self.generateRecursiveFunctionGroup();
    }

    const closure_count = self.reader.intRangeAtMost(u8, 1, 3);
    for (0..closure_count) |_| {
        try self.generateClosureFunction();
    }

    const open_union_count = self.reader.intRangeAtMost(u8, 1, 3);
    for (0..open_union_count) |_| {
        try self.generateOpenUnionFunction();
    }

    const rest_pattern_count = self.reader.intRangeAtMost(u8, 1, 4);
    for (0..rest_pattern_count) |_| {
        try self.generateRestPatternFunction();
    }

    const control_flow_count = self.reader.intRangeAtMost(u8, 1, 3);
    for (0..control_flow_count) |_| {
        try self.generateControlFlowFunction();
    }

    const statement_count = self.reader.intRangeAtMost(u8, 1, 3);
    for (0..statement_count) |_| {
        try self.generateStatementFunction();
    }

    const row_record_count = self.reader.intRangeAtMost(u8, 1, 3);
    for (0..row_record_count) |_| {
        try self.generateRowRecordFunction();
    }

    const tools_count = self.reader.intRangeAtMost(u8, 1, 4);
    for (0..tools_count) |_| {
        try self.generateToolsFunction();
    }

    const try_chain_count = self.reader.intRangeAtMost(u8, 1, 2);
    for (0..try_chain_count) |_| {
        try self.generateTryWildcardChainGroup();
    }

    const builtin_count = self.reader.intRangeAtMost(u8, 12, 36);
    for (0..builtin_count) |_| {
        try self.generateBuiltinAssociatedFunction();
    }

    const extra_count = self.reader.intRangeAtMost(u8, 12, 48);
    for (0..extra_count) |_| {
        try self.generateFunction(self.chooseType());
    }

    try self.write("}\n");
    try self.generateExpectations();
}

fn generateExpectations(self: *Self) std.mem.Allocator.Error!void {
    const count = self.reader.intRangeAtMost(u8, 0, 3);
    for (0..count) |_| {
        switch (self.reader.intRangeAtMost(u8, 0, 4)) {
            0 => try self.write("\nexpect Main == Main\n"),
            1 => try self.write("\nexpect WithBool(True) == WithBool(True)\n"),
            2 => try self.write("\nexpect Holder.get(Holder.make(1)) == 1\n"),
            3 => try self.write("\nexpect Tree.label(Leaf(\"x\")) == \"x\"\n"),
            4 => try self.write("\nexpect Main.try_chain(Main) == Ok(1)\n"),
            else => unreachable,
        }
    }
}

fn generateFunction(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    const max_style: u8 = if (typ == .my_num)
        8
    else if (typ.isSupport())
        9
    else if (typ.isHolder())
        9
    else if (typ.isBuilder())
        7
    else if (typ.isWrap())
        9
    else if (typ.isSecret())
        9
    else if (typ.isBox())
        7
    else if (typ.isDict())
        20
    else if (typ.isSet())
        14
    else if (typ.isRecord())
        if (typ == .record_str_u64) 8 else 7
    else if (typ.isList())
        23
    else if (typ.isFunction())
        5
    else if (typ.isTuple())
        6
    else if (typ.isTry())
        7
    else if (typ == .u64)
        8
    else if (typ.isNumber())
        7
    else if (typ == .str)
        7
    else if (typ.isSimpleEquatable())
        6
    else if (typ.isTree())
        5
    else
        4;
    const style = self.reader.intRangeAtMost(u8, 0, max_style);
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (style) {
        0 => try self.generateLiteralFunction(name_id, typ),
        1 => try self.generateIfFunction(name_id, typ),
        2 => try self.generateBlockFunction(name_id, typ),
        3 => try self.generateIdentityFunction(name_id, typ),
        4 => try self.generateMatchMainFunction(name_id, typ),
        5 => if (typ.isHolder())
            try self.generateHolderGetFunction(name_id, typ)
        else if (typ.isSupport())
            try self.generateSupportCountFunction(name_id)
        else if (typ.isBuilder())
            try self.generateBuilderRunFunction(name_id, typ)
        else if (typ.isWrap())
            try self.generateWrapGetFunction(name_id, typ)
        else if (typ.isSecret())
            try self.generateSecretGetFunction(name_id, typ)
        else if (typ.isBox())
            try self.generateBoxUnboxFunction(name_id, typ)
        else if (typ.isDict())
            try self.generateDictLenFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetLenFunction(name_id, typ)
        else if (typ.isList())
            try self.generateListLenFunction(name_id, typ)
        else if (typ.isFunction())
            try self.generateFunctionApplyFunction(name_id, typ)
        else if (typ.isTuple())
            try self.generateTupleDestructureFunction(name_id, typ)
        else if (typ.isTry())
            try self.generateTryMatchFunction(name_id, typ)
        else if (typ == .my_num)
            try self.generateCustomNumberUnwrapFunction(name_id)
        else if (typ.isNumber())
            try self.generateNumericCompareFunction(name_id, typ)
        else if (typ.isSimpleEquatable())
            try self.generateEqualityFunction(name_id, typ)
        else if (typ.isTree())
            try self.generateTreeLabelFunction(name_id, typ)
        else
            try self.generateRecordFieldFunction(name_id, typ),
        6 => if (typ.isHolder())
            try self.generateHolderMapFunction(name_id, typ)
        else if (typ.isSupport())
            try self.generateSupportMapFunction(name_id)
        else if (typ.isBuilder())
            try self.generateBuilderMap2Function(name_id, typ)
        else if (typ.isWrap())
            try self.generateWrapMapFunction(name_id, typ)
        else if (typ.isSecret())
            try self.generateSecretMapFunction(name_id, typ)
        else if (typ.isBox())
            try self.generateBoxRoundTripFunction(name_id, typ)
        else if (typ.isDict())
            try self.generateDictIsEmptyFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetIsEmptyFunction(name_id, typ)
        else if (typ.isRecord())
            try self.generateRecordFromLocalsFunction(name_id, typ)
        else if (typ.isList())
            try self.generateListMatchFunction(name_id, typ)
        else if (typ.isTuple())
            try self.generateTupleMatchFunction(name_id, typ)
        else if (typ.isTry())
            try self.generateTryMapFunction(name_id, typ)
        else if (typ == .my_num)
            try self.generateCustomNumberArithmeticFunction(name_id)
        else if (typ.isNumber())
            try self.generateNumericArithmeticFunction(name_id, typ)
        else if (typ == .str)
            try self.generateStrConcatFunction(name_id)
        else if (typ.isSimpleEquatable())
            try self.generateWhereEqFunction(name_id, typ)
        else
            unreachable,
        7 => if (typ.isHolder())
            try self.generateHolderMethodFunction(name_id, typ)
        else if (typ.isSupport())
            try self.generateSupportPairFunction(name_id)
        else if (typ.isBuilder())
            try self.generateBuilderMethodFunction(name_id, typ)
        else if (typ.isWrap())
            try self.generateWrapMethodFunction(name_id, typ)
        else if (typ.isSecret())
            try self.generateSecretMethodFunction(name_id, typ)
        else if (typ.isBox())
            try self.generateBoxMethodFunction(name_id, typ)
        else if (typ.isDict())
            try self.generateDictContainsFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetContainsFunction(name_id, typ)
        else if (typ.isList())
            try self.generateListFirstFunction(name_id, typ)
        else if (typ.isTry())
            try self.generateTryQuestionFunction(name_id, typ)
        else if (typ == .str)
            try self.generateStrInterpolationFunction(name_id)
        else if (typ == .my_num)
            try self.generateEqualityFunction(name_id, typ)
        else if (typ.isNumber())
            try self.generateWherePlusFunction(name_id, typ)
        else
            try self.generateRecordDestructureFunction(name_id, typ),
        8 => if (typ.isSupport())
            try self.generateSupportTryFunction(name_id)
        else if (typ.isHolder())
            try self.generateHolderMap2Function(name_id, typ)
        else if (typ.isWrap())
            try self.generateWrapMap2Function(name_id, typ)
        else if (typ.isSecret())
            try self.generateSecretMap2Function(name_id, typ)
        else if (typ.isDict())
            try self.generateDictInsertRemoveFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetInsertRemoveFunction(name_id, typ)
        else if (typ == .record_str_u64)
            try self.generateRecordBuilderFunction(name_id)
        else if (typ.isList())
            try self.generateListGetFunction(name_id, typ)
        else if (typ == .u64)
            try self.generateU64RangeForLoopFunction(name_id)
        else if (typ == .my_num)
            try self.generateWherePlusFunction(name_id, typ)
        else
            unreachable,
        9 => if (typ.isSupport())
            try self.generateSupportForLoopFunction(name_id)
        else if (typ.isDict())
            try self.generateDictGetFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetToListFunction(name_id, typ)
        else if (typ.isHolder() or typ.isWrap() or typ.isSecret())
            try self.generateWhereMapFunction(name_id, typ)
        else
            try self.generateListAppendFunction(name_id, typ),
        10 => if (typ.isDict())
            try self.generateDictToListFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetKeepDropFunction(name_id, typ)
        else
            try self.generateListConcatFunction(name_id, typ),
        11 => if (typ.isDict())
            try self.generateDictKeysFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetUnionFunction(name_id, typ)
        else
            try self.generateListMapFunction(name_id, typ),
        12 => if (typ.isDict())
            try self.generateDictValuesFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetIntersectionFunction(name_id, typ)
        else
            try self.generateListMap2Function(name_id, typ),
        13 => if (typ.isDict())
            try self.generateDictFoldFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetDifferenceFunction(name_id, typ)
        else
            try self.generateListSetFunction(name_id, typ),
        14 => if (typ.isDict())
            try self.generateDictKeepDropFunction(name_id, typ)
        else if (typ.isSet())
            try self.generateSetMapFunction(name_id, typ)
        else
            try self.generateListUpdateFunction(name_id, typ),
        15 => if (typ.isDict())
            try self.generateDictMapFunction(name_id, typ)
        else
            try self.generateListReplaceFunction(name_id, typ),
        16 => if (typ.isDict())
            try self.generateDictJoinMapFunction(name_id, typ)
        else
            try self.generateListRevFunction(name_id, typ),
        17 => if (typ.isDict())
            try self.generateDictInsertAllFunction(name_id, typ)
        else
            try self.generateListMapWithIndexFunction(name_id, typ),
        18 => if (typ.isDict())
            try self.generateDictKeepSharedFunction(name_id, typ)
        else
            try self.generateListFoldFunction(name_id, typ),
        19 => if (typ.isDict())
            try self.generateDictRemoveAllFunction(name_id, typ)
        else
            try self.generateListFoldRevFunction(name_id, typ),
        20 => if (typ.isDict())
            try self.generateDictUpdateFunction(name_id, typ)
        else
            try self.generateListCountIfFunction(name_id, typ),
        21 => try self.generateListSublistFunction(name_id, typ),
        22 => try self.generateListSwapFunction(name_id, typ),
        23 => try self.generateListForLoopFunction(name_id, typ),
        else => unreachable,
    }
}

fn generateForLoopFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => try self.generateListForLoopFunction(name_id, self.chooseListType()),
        1 => try self.generateU64RangeForLoopFunction(name_id),
        2 => try self.generateSupportForLoopFunction(name_id),
        else => unreachable,
    }
}

fn generatePatternFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (self.reader.intRangeAtMost(u8, 0, 4)) {
        0 => try self.generateTupleParamPatternFunction(name_id),
        1 => try self.generateRecordParamPatternFunction(name_id),
        2 => try self.generateSupportParamPatternFunction(name_id),
        3 => try self.generateNestedParamPatternFunction(name_id),
        4 => try self.generateMutableTupleParamPatternFunction(name_id),
        else => unreachable,
    }
}

fn generateTupleParamPatternFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main, (Str, U64)", "U64");
    try self.writeFunctionHeader(name_id);
    try self.write("|_, (_, count)| count\n");
}

fn generateRecordParamPatternFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main, { name : Str, count : U64 }", "Str");
    try self.writeFunctionHeader(name_id);
    try self.write("|_, { name, count: _ }| name\n");
}

fn generateSupportParamPatternFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main, Support", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_, Packet(record)| record.count\n");
}

fn generateNestedParamPatternFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main, (Support, { name : Str, count : U64 })", "Str");
    try self.writeFunctionHeader(name_id);
    try self.write("|_, (Packet(_), { name })| name\n");
}

fn generateMutableTupleParamPatternFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main, (Str, U64, U64)", "U64");
    try self.writeFunctionHeader(name_id);
    try self.write("|_, (_, count, var $index)| {\n");
    try self.writeMutableLocalAssignmentStart("index");
    try self.writeMutableLocalReference("index");
    try self.write(" + count\n");
    try self.write("        ");
    try self.writeMutableLocalReference("index");
    try self.write("\n    }\n");
}

fn generateRecursiveFunctionGroup(self: *Self) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.generateDirectRecursiveFunction();
    } else {
        try self.generateMutualRecursiveFunctions();
    }
}

fn generateDirectRecursiveFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    try self.writeRawFunctionSignature(name_id, "Main, U64", "U64");
    try self.writeFunctionHeader(name_id);
    try self.write("|_, n| if n == 0 0 else Main.");
    try self.writeFunctionName(name_id);
    try self.write("(Main, n - 1)\n");
}

fn generateMutualRecursiveFunctions(self: *Self) std.mem.Allocator.Error!void {
    const even_id = self.name_counter;
    const odd_id = self.name_counter + 1;
    self.name_counter += 2;

    try self.writeRawFunctionSignature(even_id, "Main, U64", "Bool");
    try self.writeFunctionHeader(even_id);
    try self.write("|_, n| if n == 0 True else Main.");
    try self.writeFunctionName(odd_id);
    try self.write("(Main, n - 1)\n");

    try self.writeRawFunctionSignature(odd_id, "Main, U64", "Bool");
    try self.writeFunctionHeader(odd_id);
    try self.write("|_, n| if n == 0 False else Main.");
    try self.writeFunctionName(even_id);
    try self.write("(Main, n - 1)\n");
}

fn generateClosureFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => try self.generateCapturedMapFunction(name_id),
        1 => try self.generateReturnedClosureFunction(name_id),
        2 => try self.generateCapturedSupportFoldFunction(name_id),
        else => unreachable,
    }
}

fn generateCapturedMapFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        offset = ");
    try self.writeIntegerLiteral();
    try self.write("\n        ");
    try self.writeNonEmptyListLiteral(.u64);
    try self.write(".map(|item| item + offset)\n    }\n");
}

fn generateReturnedClosureFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "(U64 -> U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        offset = ");
    try self.writeIntegerLiteral();
    try self.write("\n        |item| item + offset\n    }\n");
}

fn generateCapturedSupportFoldFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        support = ");
    try self.writeLiteral(.support);
    try self.write("\n        folder = |acc, item| acc + item + Support.count(support)\n        ");
    try self.writeNonEmptyListLiteral(.u64);
    try self.write(".fold(0, folder)\n    }\n");
}

fn generateOpenUnionFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => try self.generateOpenColorMatchFunction(name_id),
        1 => try self.generateOpenColorArgFunction(name_id),
        2 => try self.generateOpenErrorTryFunction(name_id),
        else => unreachable,
    }
}

fn generateOpenColorMatchFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        color : Color(_)\n        color = ");
    try self.writeOpenColorLiteral();
    try self.write("\n        ");
    try self.writeOpenColorMatch("color");
    try self.write("\n    }\n");
}

fn generateOpenColorArgFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main, [Red, Green, ..]", "Str");
    try self.writeFunctionHeader(name_id);
    try self.write("|_, color| match color {\n        Red => \"red\"\n        Green => \"green\"\n        _ => \"other\"\n    }\n");
}

fn generateOpenErrorTryFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        result : Try(U64, [Small, Big(Str), ..])\n        result = if ");
    try self.writeBoolLiteral();
    try self.write(" Ok(");
    try self.writeIntegerLiteral();
    try self.write(") else ");
    if (self.reader.boolean()) {
        try self.write("Err(Small)");
    } else {
        try self.writeCall("Err", &.{.{ .raw = "Big(\"open\")" }});
    }
    try self.write("\n        match result {\n            Ok(value) => value\n            Err(Small) => 0\n            Err(Big(_)) => 1\n            Err(_) => 2\n        }\n    }\n");
}

fn generateRestPatternFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => try self.generateRecordUpdateFunction(name_id),
        1 => try self.generateListRestPatternFunction(name_id),
        2 => try self.generateListBetweenPatternFunction(name_id),
        else => unreachable,
    }
}

fn generateRecordUpdateFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .record_str_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        record = ");
    try self.writeRecordStrU64Literal();
    try self.write("\n        { ..record, ");
    if (self.reader.boolean()) {
        try self.write("count: record.count + ");
        try self.writeIntegerLiteral();
    } else {
        try self.write("name: ");
        try self.writeStringLiteral();
    }
    try self.write(" }\n    }\n");
}

fn generateListRestPatternFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeNonEmptyListLiteral(.u64);
    try self.write(" {\n        [first, .. as rest] => first + rest.len()\n        [] => 0\n    }\n");
}

fn generateListBetweenPatternFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeNonEmptyListLiteral(.u64);
    try self.write(" {\n        [first, .., last] => first + last\n        [only] => only\n        [] => 0\n    }\n");
}

fn generateControlFlowFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    if (self.reader.boolean()) {
        try self.generateWhileFunction(name_id);
    } else {
        try self.generateEarlyReturnFunction(name_id);
    }
}

fn generateWhileFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    const upper = self.reader.intRangeAtMost(u8, 0, 8);
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n");
    try self.writeMutableLocalDeclaration("index", .{ .raw = "0" });
    try self.writeMutableLocalDeclaration("total", .{ .raw = "0" });
    try self.write("        while ");
    try self.writeMutableLocalReference("index");
    try self.output.print(self.allocator, " < {d} {{\n", .{upper});
    try self.writeMutableLocalAssignmentStart("total");
    try self.writeMutableLocalReference("total");
    try self.write(" + ");
    try self.writeMutableLocalReference("index");
    try self.write("\n");
    try self.writeMutableLocalAssignmentStart("index");
    try self.writeMutableLocalReference("index");
    try self.write(" + 1\n");
    try self.write("        }\n        ");
    try self.writeMutableLocalReference("total");
    try self.write("\n    }\n");
}

fn generateEarlyReturnFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        if ");
    try self.writeBoolLiteral();
    try self.write(" {\n            return ");
    try self.writeIntegerLiteral();
    try self.write("\n        }\n        ");
    try self.writeIntegerLiteral();
    try self.write("\n    }\n");
}

fn generateStatementFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => try self.generateDbgStatementFunction(name_id, .u64),
        1 => try self.generateDbgStatementFunction(name_id, .str),
        2 => try self.generateCrashStatementFunction(name_id),
        else => unreachable,
    }
}

fn generateDbgStatementFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        dbg ");
    try self.writeLiteral(typ);
    try self.write("\n        ");
    try self.writeLiteral(typ);
    try self.write("\n    }\n");
}

fn generateCrashStatementFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        if ");
    try self.writeBoolLiteral();
    try self.write(" {\n            crash ");
    try self.writeStringLiteral();
    try self.write("\n        }\n        ");
    try self.writeIntegerLiteral();
    try self.write("\n    }\n");
}

fn generateRowRecordFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => try self.generateRowRecordArgFunction(name_id),
        1 => try self.generateRowRecordLocalFunction(name_id),
        2 => try self.generateRowRecordPatternFunction(name_id),
        else => unreachable,
    }
}

fn generateRowRecordArgFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main, { count : U64, .. }", "U64");
    try self.writeFunctionHeader(name_id);
    try self.write("|_, record| Main.has_count(record)\n");
}

fn generateRowRecordLocalFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Main.has_count(");
    try self.writeWideRecordLiteral();
    try self.write(")\n");
}

fn generateRowRecordPatternFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeWideRecordLiteral();
    try self.write(" {\n        { count, .. } => count\n    }\n");
}

fn generateToolsFunction(self: *Self) std.mem.Allocator.Error!void {
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.generateToolsCountFunction(name_id),
        1 => try self.generateToolsMapFunction(name_id),
        2 => try self.generateToolsLabelFunction(name_id),
        3 => try self.generateToolsEqFunction(name_id),
        else => unreachable,
    }
}

fn generateToolsCountFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Tools", .{ .raw = "Tools.from_support(Support.make(1))" }, "count", &.{});
    try self.write("\n");
}

fn generateToolsMapFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "Tools");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Tools", .{ .raw = "Tools.from_support(Support.make(2))" }, "map_count", &.{.{ .raw = "|count| count + 1" }});
    try self.write("\n");
}

fn generateToolsLabelFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Tools.label(");
    try self.writeToolsLiteral();
    try self.write(")\n");
}

fn generateToolsEqFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Tools.is_eq(");
    try self.writeToolsLiteral();
    try self.write(", ");
    try self.writeToolsLiteral();
    try self.write(")\n");
}

fn generateTryWildcardChainGroup(self: *Self) std.mem.Allocator.Error!void {
    const first_id = self.name_counter;
    const second_id = self.name_counter + 1;
    const chain_id = self.name_counter + 2;
    self.name_counter += 3;

    try self.writeRawFunctionSignature(first_id, "Main, U64", "Try(U64, _)");
    try self.writeFunctionHeader(first_id);
    try self.write("|_, _| ");
    try self.writeCall("Ok", &.{.{ .integer_literal = {} }});
    try self.write("\n");

    try self.writeRawFunctionSignature(second_id, "Main, U64", "Try(U64, _)");
    try self.writeFunctionHeader(second_id);
    try self.write("|_, _| ");
    try self.writeCall("Ok", &.{.{ .integer_literal = {} }});
    try self.write("\n");

    try self.writeRawFunctionSignature(chain_id, "Main", "Try(U64, _)");
    try self.writeFunctionHeader(chain_id);
    try self.write("|_| {\n        _first = ");
    try self.writeGeneratedFunctionCall(first_id, &.{ .{ .raw = "Main" }, .{ .integer_literal = {} } });
    try self.write("?\n        second = ");
    try self.writeGeneratedFunctionCall(second_id, &.{ .{ .raw = "Main" }, .{ .integer_literal = {} } });
    try self.write("?\n        ");
    try self.writeCall("Ok", &.{.{ .raw = "second" }});
    try self.write("\n    }\n");
}

fn generateBuiltinAssociatedFunction(self: *Self) std.mem.Allocator.Error!void {
    const adapter_index = self.reader.intRangeLessThan(usize, 0, builtin_adapters.len + numeric_adapters.len + numeric_conversion_adapters.len);
    if (adapter_index >= builtin_adapters.len + numeric_adapters.len) {
        const conversion_adapter = numeric_conversion_adapters[adapter_index - builtin_adapters.len - numeric_adapters.len];
        const name_id = self.name_counter;
        self.name_counter += 1;
        return self.generateNumericConversionBuiltinFunction(name_id, conversion_adapter);
    }
    if (adapter_index >= builtin_adapters.len) {
        const numeric_adapter = numeric_adapters[adapter_index - builtin_adapters.len];
        const name_id = self.name_counter;
        self.name_counter += 1;
        return self.generateNumericBuiltinAssociatedFunction(name_id, numeric_adapter);
    }

    const adapter = builtin_adapters[adapter_index];
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (adapter.kind) {
        .bool_not => try self.generateBoolNotBuiltinFunction(name_id),
        .bool_encode => try self.generateBoolEncodeBuiltinFunction(name_id),
        .bool_decode => try self.generateBoolDecodeBuiltinFunction(name_id),
        .bool_is_eq => try self.generateBoolIsEqBuiltinFunction(name_id),
        .str_is_empty => try self.generateStrUnaryBuiltinFunction(name_id, "is_empty", .bool),
        .str_concat => try self.generateStrBinaryBuiltinFunction(name_id, "concat", .str),
        .str_contains => try self.generateStrBinaryBuiltinFunction(name_id, "contains", .bool),
        .str_trim => try self.generateStrUnaryBuiltinFunction(name_id, "trim", .str),
        .str_trim_start => try self.generateStrUnaryBuiltinFunction(name_id, "trim_start", .str),
        .str_trim_end => try self.generateStrUnaryBuiltinFunction(name_id, "trim_end", .str),
        .str_caseless_ascii_equals => try self.generateStrBinaryBuiltinFunction(name_id, "caseless_ascii_equals", .bool),
        .str_with_ascii_lowercased => try self.generateStrUnaryBuiltinFunction(name_id, "with_ascii_lowercased", .str),
        .str_with_ascii_uppercased => try self.generateStrUnaryBuiltinFunction(name_id, "with_ascii_uppercased", .str),
        .str_starts_with => try self.generateStrBinaryBuiltinFunction(name_id, "starts_with", .bool),
        .str_ends_with => try self.generateStrBinaryBuiltinFunction(name_id, "ends_with", .bool),
        .str_repeat => try self.generateStrRepeatBuiltinFunction(name_id),
        .str_with_prefix => try self.generateStrBinaryBuiltinFunction(name_id, "with_prefix", .str),
        .str_drop_prefix => try self.generateStrBinaryBuiltinFunction(name_id, "drop_prefix", .str),
        .str_drop_suffix => try self.generateStrBinaryBuiltinFunction(name_id, "drop_suffix", .str),
        .str_count_utf8_bytes => try self.generateStrUnaryBuiltinFunction(name_id, "count_utf8_bytes", .u64),
        .str_with_capacity => try self.generateStrWithCapacityBuiltinFunction(name_id),
        .str_reserve => try self.generateStrReserveBuiltinFunction(name_id),
        .str_release_excess_capacity => try self.generateStrUnaryBuiltinFunction(name_id, "release_excess_capacity", .str),
        .str_to_utf8 => try self.generateStrToUtf8BuiltinFunction(name_id),
        .str_from_utf8_lossy => try self.generateStrFromUtf8LossyBuiltinFunction(name_id),
        .str_from_utf8 => try self.generateStrFromUtf8BuiltinFunction(name_id),
        .str_split_on => try self.generateStrBinaryBuiltinFunction(name_id, "split_on", .list_str),
        .str_join_with => try self.generateStrJoinWithBuiltinFunction(name_id),
        .str_is_eq => try self.generateStrIsEqBuiltinFunction(name_id),
        .str_inspect => try self.generateStrInspectBuiltinFunction(name_id),
        .str_encode => try self.generateStrEncodeBuiltinFunction(name_id),
        .str_decode => try self.generateStrDecodeBuiltinFunction(name_id),
        .list_len => try self.generateListLenFunction(name_id, self.chooseListType()),
        .list_is_empty => try self.generateListIsEmptyBuiltinFunction(name_id, self.chooseListType()),
        .list_iter => try self.generateListIterBuiltinFunction(name_id),
        .list_from_iter => try self.generateListFromIterBuiltinFunction(name_id),
        .list_with_capacity => try self.generateListWithCapacityBuiltinFunction(name_id),
        .list_reserve => try self.generateListReserveBuiltinFunction(name_id, self.chooseListType()),
        .list_release_excess_capacity => try self.generateListReleaseExcessCapacityBuiltinFunction(name_id, self.chooseListType()),
        .list_sort_with => try self.generateListSortWithBuiltinFunction(name_id),
        .list_is_eq => try self.generateListIsEqBuiltinFunction(name_id, self.chooseEquatableListType()),
        .list_first => try self.generateListFirstFunction(name_id, self.chooseListType()),
        .list_get => try self.generateListGetFunction(name_id, self.chooseListType()),
        .list_subscript => try self.generateListSubscriptFunction(name_id, self.chooseListType()),
        .list_append => try self.generateListAppendFunction(name_id, self.chooseListType()),
        .list_prepend => try self.generateListPrependFunction(name_id, self.chooseListType()),
        .list_concat => try self.generateListConcatFunction(name_id, self.chooseListType()),
        .list_map => try self.generateListMapFunction(name_id, self.chooseListType()),
        .list_map2 => try self.generateListMap2Function(name_id, self.chooseListType()),
        .list_keep_if => try self.generateListKeepDropBuiltinFunction(name_id, self.chooseListType(), "keep_if"),
        .list_drop_if => try self.generateListKeepDropBuiltinFunction(name_id, self.chooseListType(), "drop_if"),
        .list_any => try self.generateListPredicateBuiltinFunction(name_id, self.chooseListType(), "any"),
        .list_all => try self.generateListPredicateBuiltinFunction(name_id, self.chooseListType(), "all"),
        .list_contains => try self.generateListContainsBuiltinFunction(name_id, self.chooseEquatableListType()),
        .list_starts_with => try self.generateListCompareBuiltinFunction(name_id, self.chooseEquatableListType(), "starts_with"),
        .list_ends_with => try self.generateListCompareBuiltinFunction(name_id, self.chooseEquatableListType(), "ends_with"),
        .list_set => try self.generateListSetFunction(name_id, self.chooseListType()),
        .list_update => try self.generateListUpdateFunction(name_id, self.chooseListType()),
        .list_replace => try self.generateListReplaceFunction(name_id, self.chooseListType()),
        .list_rev => try self.generateListRevFunction(name_id, self.chooseListType()),
        .list_last => try self.generateListLastBuiltinFunction(name_id, self.chooseListType()),
        .list_single => try self.generateListSingleBuiltinFunction(name_id, self.chooseListType()),
        .list_repeat => try self.generateListRepeatBuiltinFunction(name_id, self.chooseListType()),
        .list_map_with_index => try self.generateListMapWithIndexFunction(name_id, self.chooseListType()),
        .list_fold => try self.generateListFoldFunction(name_id, self.chooseListType()),
        .list_fold_with_index => try self.generateListFoldWithIndexFunction(name_id, self.chooseListType()),
        .list_fold_until => try self.generateListFoldUntilFunction(name_id),
        .list_fold_with_index_until => try self.generateListFoldWithIndexUntilFunction(name_id),
        .list_fold_rev => try self.generateListFoldRevFunction(name_id, self.chooseListType()),
        .list_count_if => try self.generateListCountIfFunction(name_id, self.chooseListType()),
        .list_find_first => try self.generateListFindElementBuiltinFunction(name_id, self.chooseListType(), "find_first"),
        .list_find_last => try self.generateListFindElementBuiltinFunction(name_id, self.chooseListType(), "find_last"),
        .list_find_first_index => try self.generateListFindIndexBuiltinFunction(name_id, self.chooseListType(), "find_first_index"),
        .list_find_last_index => try self.generateListFindIndexBuiltinFunction(name_id, self.chooseListType(), "find_last_index"),
        .list_sublist => try self.generateListSublistFunction(name_id, self.chooseListType()),
        .list_take_first => try self.generateListTakeDropBuiltinFunction(name_id, self.chooseListType(), "take_first"),
        .list_take_last => try self.generateListTakeDropBuiltinFunction(name_id, self.chooseListType(), "take_last"),
        .list_drop_first => try self.generateListTakeDropBuiltinFunction(name_id, self.chooseListType(), "drop_first"),
        .list_drop_last => try self.generateListTakeDropBuiltinFunction(name_id, self.chooseListType(), "drop_last"),
        .list_drop_at => try self.generateListTakeDropBuiltinFunction(name_id, self.chooseListType(), "drop_at"),
        .list_swap => try self.generateListSwapFunction(name_id, self.chooseListType()),
        .list_for_each_bang => try self.generateListForEachBangBuiltinFunction(name_id),
        .list_split_at => try self.generateListSplitAtBuiltinFunction(name_id),
        .list_split_on => try self.generateListSplitOnBuiltinFunction(name_id),
        .list_split_if => try self.generateListSplitIfBuiltinFunction(name_id),
        .list_split_on_list => try self.generateListSplitOnListBuiltinFunction(name_id),
        .list_split_first => try self.generateListSplitFirstLastBuiltinFunction(name_id, "split_first"),
        .list_split_last => try self.generateListSplitFirstLastBuiltinFunction(name_id, "split_last"),
        .list_join_with => try self.generateListJoinWithBuiltinFunction(name_id, .list_str),
        .list_join_list_with => try self.generateListJoinListWithBuiltinFunction(name_id),
        .list_sum => try self.generateListAggregateBuiltinFunction(name_id, "sum"),
        .list_min => try self.generateListMinMaxBuiltinFunction(name_id, "min"),
        .list_max => try self.generateListMinMaxBuiltinFunction(name_id, "max"),
        .list_encode => try self.generateListEncodeBuiltinFunction(name_id),
        .list_decode => try self.generateListDecodeBuiltinFunction(name_id),
        .iter_next => try self.generateIterNextBuiltinFunction(name_id),
        .iter_custom => try self.generateIterCustomBuiltinFunction(name_id),
        .iter_iter => try self.generateIterIterBuiltinFunction(name_id),
        .iter_map => try self.generateIterTransformCollectBuiltinFunction(name_id, "map"),
        .iter_keep_if => try self.generateIterPredicateCollectBuiltinFunction(name_id, "keep_if"),
        .iter_drop_if => try self.generateIterPredicateCollectBuiltinFunction(name_id, "drop_if"),
        .iter_fold => try self.generateIterFoldBuiltinFunction(name_id),
        .iter_size_hint => try self.generateIterSizeHintBuiltinFunction(name_id),
        .iter_collect => try self.generateIterCollectBuiltinFunction(name_id),
        .iter_stream => try self.generateIterStreamBuiltinFunction(name_id),
        .iter_take_first => try self.generateIterTakeDropCollectBuiltinFunction(name_id, "take_first"),
        .iter_drop_first => try self.generateIterTakeDropCollectBuiltinFunction(name_id, "drop_first"),
        .iter_take_last => try self.generateIterTakeDropCollectBuiltinFunction(name_id, "take_last"),
        .iter_drop_last => try self.generateIterTakeDropCollectBuiltinFunction(name_id, "drop_last"),
        .stream_from_iter => try self.generateStreamFromIterBuiltinFunction(name_id),
        .stream_map => try self.generateStreamMapBuiltinFunction(name_id),
        .stream_map_bang => try self.generateStreamMapBangBuiltinFunction(name_id),
        .stream_next_bang => try self.generateStreamNextBangBuiltinFunction(name_id),
        .stream_size_hint => try self.generateStreamSizeHintBuiltinFunction(name_id),
        .stream_collect_bang => try self.generateStreamCollectBangBuiltinFunction(name_id),
        .try_is_ok => try self.generateTryPredicateBuiltinFunction(name_id, self.chooseTryType(), "is_ok"),
        .try_is_err => try self.generateTryPredicateBuiltinFunction(name_id, self.chooseTryType(), "is_err"),
        .try_ok_or => try self.generateTryOkOrBuiltinFunction(name_id, self.chooseTryType()),
        .try_err_or => try self.generateTryErrOrBuiltinFunction(name_id, self.chooseTryType()),
        .try_map_ok => try self.generateTryMapOkErrBuiltinFunction(name_id, self.chooseTryType(), "map_ok"),
        .try_map_err => try self.generateTryMapOkErrBuiltinFunction(name_id, self.chooseTryType(), "map_err"),
        .try_map_ok_bang => try self.generateTryMapOkErrBangBuiltinFunction(name_id, self.chooseTryType(), "map_ok!"),
        .try_map_err_bang => try self.generateTryMapOkErrBangBuiltinFunction(name_id, self.chooseTryType(), "map_err!"),
        .try_is_eq => try self.generateTryIsEqBuiltinFunction(name_id, self.chooseTryType()),
        .box_box => try self.generateBoxBoxBuiltinFunction(name_id, self.chooseBoxType()),
        .box_unbox => try self.generateBoxUnboxFunction(name_id, self.chooseBoxType()),
        .dict_empty => try self.generateDictEmptyBuiltinFunction(name_id, self.chooseDictType()),
        .dict_is_eq => try self.generateDictIsEqBuiltinFunction(name_id, self.chooseDictType()),
        .dict_single => try self.generateDictSingleBuiltinFunction(name_id, self.chooseDictType()),
        .dict_len => try self.generateDictLenFunction(name_id, self.chooseDictType()),
        .dict_is_empty => try self.generateDictIsEmptyFunction(name_id, self.chooseDictType()),
        .dict_get => try self.generateDictGetFunction(name_id, self.chooseDictType()),
        .dict_contains => try self.generateDictContainsFunction(name_id, self.chooseDictType()),
        .dict_insert => try self.generateDictInsertBuiltinFunction(name_id, self.chooseDictType()),
        .dict_remove => try self.generateDictRemoveBuiltinFunction(name_id, self.chooseDictType()),
        .dict_to_list => try self.generateDictToListFunction(name_id, self.chooseDictType()),
        .dict_keys => try self.generateDictKeysFunction(name_id, self.chooseDictType()),
        .dict_values => try self.generateDictValuesFunction(name_id, self.chooseDictType()),
        .dict_fold => try self.generateDictFoldFunction(name_id, self.chooseDictType()),
        .dict_keep_if => try self.generateDictKeepDropSpecificFunction(name_id, self.chooseDictType(), "keep_if"),
        .dict_drop_if => try self.generateDictKeepDropSpecificFunction(name_id, self.chooseDictType(), "drop_if"),
        .dict_map => try self.generateDictMapFunction(name_id, self.chooseDictType()),
        .dict_join_map => try self.generateDictJoinMapFunction(name_id, self.chooseDictType()),
        .dict_insert_all => try self.generateDictInsertAllFunction(name_id, self.chooseDictType()),
        .dict_keep_shared => try self.generateDictKeepSharedFunction(name_id, self.chooseDictType()),
        .dict_remove_all => try self.generateDictRemoveAllFunction(name_id, self.chooseDictType()),
        .dict_update => try self.generateDictUpdateFunction(name_id, self.chooseDictType()),
        .dict_from_list => try self.generateDictFromListBuiltinFunction(name_id, self.chooseDictType()),
        .set_empty => try self.generateSetEmptyBuiltinFunction(name_id, self.chooseSetType()),
        .set_is_eq => try self.generateSetIsEqBuiltinFunction(name_id, self.chooseSetType()),
        .set_single => try self.generateSetSingleBuiltinFunction(name_id, self.chooseSetType()),
        .set_len => try self.generateSetLenFunction(name_id, self.chooseSetType()),
        .set_is_empty => try self.generateSetIsEmptyFunction(name_id, self.chooseSetType()),
        .set_contains => try self.generateSetContainsFunction(name_id, self.chooseSetType()),
        .set_insert => try self.generateSetInsertBuiltinFunction(name_id, self.chooseSetType()),
        .set_remove => try self.generateSetRemoveBuiltinFunction(name_id, self.chooseSetType()),
        .set_to_list => try self.generateSetToListFunction(name_id, self.chooseSetType()),
        .set_keep_if => try self.generateSetKeepDropSpecificFunction(name_id, self.chooseSetType(), "keep_if"),
        .set_drop_if => try self.generateSetKeepDropSpecificFunction(name_id, self.chooseSetType(), "drop_if"),
        .set_union => try self.generateSetUnionFunction(name_id, self.chooseSetType()),
        .set_intersection => try self.generateSetIntersectionFunction(name_id, self.chooseSetType()),
        .set_difference => try self.generateSetDifferenceFunction(name_id, self.chooseSetType()),
        .set_map => try self.generateSetMapFunction(name_id, self.chooseSetType()),
        .set_from_list => try self.generateSetFromListBuiltinFunction(name_id, self.chooseSetType()),
        .numeral_is_negative => try self.generateNumeralIsNegativeBuiltinFunction(name_id),
        .utf8_problem_is_eq => try self.generateUtf8ProblemIsEqBuiltinFunction(name_id),
    }
}

fn generateNumericBuiltinAssociatedFunction(self: *Self, name_id: u32, adapter: NumericAdapter) std.mem.Allocator.Error!void {
    const typ = switch (adapter.family) {
        .all => self.chooseNumberType(),
        .signed_or_float => self.chooseSignedOrFloatNumberType(),
        .integer => self.chooseIntegerType(),
        .float => self.chooseFloatType(),
        .decimal => .dec,
    };

    switch (adapter.kind) {
        .default => try self.generateNumericDefaultBuiltinFunction(name_id, typ),
        .to_str => try self.generateNumericUnaryBuiltinFunction(name_id, typ, "to_str", .str),
        .is_zero => try self.generateNumericUnaryBuiltinFunction(name_id, typ, "is_zero", .bool),
        .is_negative => try self.generateNumericUnaryBuiltinFunction(name_id, typ, "is_negative", .bool),
        .is_positive => try self.generateNumericUnaryBuiltinFunction(name_id, typ, "is_positive", .bool),
        .is_eq => try self.generateNumericAssociatedBinaryBuiltinFunction(name_id, typ, "is_eq", .bool),
        .is_gt => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "is_gt", .bool),
        .is_gte => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "is_gte", .bool),
        .is_lt => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "is_lt", .bool),
        .is_lte => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "is_lte", .bool),
        .max => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "max", typ),
        .min => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "min", typ),
        .abs => try self.generateNumericUnaryBuiltinFunction(name_id, typ, "abs", typ),
        .negate => try self.generateNumericUnaryBuiltinFunction(name_id, typ, "negate", typ),
        .plus => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "plus", typ),
        .minus => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "minus", typ),
        .times => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "times", typ),
        .div_by => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "div_by", typ),
        .div_trunc_by => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "div_trunc_by", typ),
        .add_checked => try self.generateNumericCheckedBinaryBuiltinFunction(name_id, typ, "add_checked"),
        .sub_checked => try self.generateNumericCheckedBinaryBuiltinFunction(name_id, typ, "sub_checked"),
        .mul_checked => try self.generateNumericCheckedBinaryBuiltinFunction(name_id, typ, "mul_checked"),
        .div_checked => try self.generateNumericCheckedBinaryBuiltinFunction(name_id, typ, "div_checked"),
        .plus_saturated => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "plus_saturated", typ),
        .rem_by => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "rem_by", typ),
        .mod_by => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "mod_by", typ),
        .abs_diff => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "abs_diff", absDiffReturnType(typ)),
        .shift_left_by => try self.generateNumericShiftBuiltinFunction(name_id, typ, "shift_left_by"),
        .shift_right_by => try self.generateNumericShiftBuiltinFunction(name_id, typ, "shift_right_by"),
        .shift_right_zf_by => try self.generateNumericShiftBuiltinFunction(name_id, typ, "shift_right_zf_by"),
        .bitwise_and => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "bitwise_and", typ),
        .bitwise_or => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "bitwise_or", typ),
        .bitwise_xor => try self.generateNumericBinaryBuiltinFunction(name_id, typ, "bitwise_xor", typ),
        .bitwise_not => try self.generateNumericUnaryBuiltinFunction(name_id, typ, "bitwise_not", typ),
        .from_int_digits => try self.generateNumericFromIntDigitsBuiltinFunction(name_id, typ),
        .from_dec_digits => try self.generateNumericFromDecDigitsBuiltinFunction(name_id, typ),
        .from_numeral => try self.generateNumericFromNumeralBuiltinFunction(name_id, typ),
        .from_str => try self.generateNumericFromStrBuiltinFunction(name_id, typ),
        .encode => try self.generateNumericEncodeBuiltinFunction(name_id, typ),
        .decode => try self.generateNumericDecodeBuiltinFunction(name_id, typ),
        .to => try self.generateNumericRangeBuiltinFunction(name_id, typ, "to"),
        .until => try self.generateNumericRangeBuiltinFunction(name_id, typ, "until"),
    }
}

fn generateNumericDefaultBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeQualifiedCall(numberOwner(typ), "default", &.{});
    try self.write("\n");
}

fn generateNumericUnaryBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8, return_type: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeTypedLocalFunctionStart(name_id, "number", typ, return_type);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall(numberOwner(typ), .{ .raw = "number" }, method, &.{});
    try self.write("\n    }\n");
}

fn generateNumericBinaryBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8, return_type: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeTypedLocalFunctionStart(name_id, "number", typ, return_type);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall(numberOwner(typ), .{ .raw = "number" }, method, &.{.{ .small_number_literal = typ }});
    try self.write("\n    }\n");
}

fn generateNumericAssociatedBinaryBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8, return_type: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeTypedLocalFunctionStart(name_id, "number", typ, return_type);
    try self.write("        ");
    try self.writeQualifiedCall(numberOwner(typ), method, &.{ .{ .raw = "number" }, .{ .small_number_literal = typ } });
    try self.write("\n    }\n");
}

fn generateNumericCheckedBinaryBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber() and !typ.isFloat());

    try self.writeNumericTrySignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        number : ");
    try self.writeType(typ);
    try self.write("\n        number = ");
    try self.writeSmallNumberLiteral(typ);
    try self.write("\n        ");
    try self.writeAssociatedOrMethodCall(numberOwner(typ), .{ .raw = "number" }, method, &.{.{ .small_number_literal = typ }});
    try self.write("\n    }\n");
}

fn generateNumericConversionBuiltinFunction(self: *Self, name_id: u32, adapter: NumericConversionAdapter) std.mem.Allocator.Error!void {
    std.debug.assert(adapter.source.isNumber());
    std.debug.assert(adapter.result.isNumber());

    switch (adapter.mode) {
        .direct => {
            try self.writeTypedLocalFunctionStart(name_id, "number", adapter.source, adapter.result);
            try self.write("        ");
            try self.writeAssociatedOrMethodCall(numberOwner(adapter.source), .{ .raw = "number" }, adapter.name, &.{});
            try self.write("\n    }\n");
        },
        .try_result => {
            try self.writeTrySignature(name_id, adapter.result);
            try self.writeFunctionHeader(name_id);
            try self.write("|_| {\n        number : ");
            try self.writeType(adapter.source);
            try self.write("\n        number = ");
            try self.writeSmallNumberLiteral(adapter.source);
            try self.write("\n        ");
            try self.writeAssociatedOrMethodCall(numberOwner(adapter.source), .{ .raw = "number" }, adapter.name, &.{});
            try self.write("\n    }\n");
        },
    }
}

fn generateNumericShiftBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber() and !typ.isFloat());

    try self.writeTypedLocalFunctionStart(name_id, "number", typ, typ);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall(numberOwner(typ), .{ .raw = "number" }, method, &.{.{ .small_number_literal = .u8 }});
    try self.write("\n    }\n");
}

fn generateNumericFromIntDigitsBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeNumericTrySignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeQualifiedCall(numberOwner(typ), "from_int_digits", &.{.{ .raw = "[1, 2, 3]" }});
    try self.write("\n");
}

fn generateNumericFromDecDigitsBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isFloat() or typ == .dec);

    try self.writeNumericTrySignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeQualifiedCall(numberOwner(typ), "from_dec_digits", &.{.{ .raw = "([1, 2], [3])" }});
    try self.write("\n");
}

fn generateNumericFromNumeralBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeNumericTrySignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeQualifiedCall(numberOwner(typ), "from_numeral", &.{.{ .numeral_literal = {} }});
    try self.write("\n");
}

fn generateNumericFromStrBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeNumericTrySignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeQualifiedCall(numberOwner(typ), "from_str", &.{.{ .string_literal = {} }});
    try self.write("\n");
}

fn generateNumericEncodeBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeFmtEncodeFunctionStart(name_id);
    try self.write("        ");
    try self.writeQualifiedCall(numberOwner(typ), "encode", &.{ .{ .small_number_literal = typ }, .{ .raw = "fmt" } });
    try self.write("\n    }\n");
}

fn generateNumericDecodeBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeFmtDecodeFunctionStart(name_id, typ);
    try self.write("        ");
    try self.writeQualifiedCall(numberOwner(typ), "decode", &.{ .{ .string_literal = {} }, .{ .raw = "fmt" } });
    try self.write("\n    }\n");
}

fn generateNumericRangeBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber() and !typ.isFloat());

    try self.writeNumberIterFunctionSignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall(numberOwner(typ), .{ .small_number_literal = typ }, method, &.{.{ .small_number_literal = typ }});
    try self.write("\n");
}

fn generateU64RangeForLoopFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.writeFunctionSignature(name_id, "Main", .u64);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| {\n");
        try self.writeMutableLocalDeclaration("total", .{ .raw = "0" });
        try self.writeForInPrefix("item");
        try self.writeU64RangeIterator();
        try self.writeForInBodyStart();
        try self.writeMutableLocalAssignmentStart("total");
        try self.writeMutableLocalReference("total");
        try self.write(" + item\n");
        try self.writeForInBodyEnd();
        try self.write("        ");
        try self.writeMutableLocalReference("total");
        try self.write("\n    }\n");
    } else {
        try self.writeFunctionSignature(name_id, "Main", .list_u64);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| {\n");
        try self.writeMutableLocalDeclaration("items", .{ .raw = "[]" });
        try self.writeForInPrefix("item");
        try self.writeU64RangeIterator();
        try self.writeForInBodyStart();
        try self.writeMutableLocalAssignmentStart("items");
        try self.writeMutableLocalReference("items");
        try self.write(".append(item)\n");
        try self.writeForInBodyEnd();
        try self.write("        ");
        try self.writeMutableLocalReference("items");
        try self.write("\n    }\n");
    }
}

fn generateBoolNotBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Bool.not", &.{.{ .bool_literal = {} }});
    try self.write("\n");
}

fn generateBoolIsEqBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Bool.is_eq", &.{ .{ .bool_literal = {} }, .{ .bool_literal = {} } });
    try self.write("\n");
}

fn generateBoolEncodeBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFmtEncodeFunctionStart(name_id);
    try self.write("        ");
    try self.writeCall("Bool.encode", &.{ .{ .bool_literal = {} }, .{ .raw = "fmt" } });
    try self.write("\n    }\n");
}

fn generateBoolDecodeBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFmtDecodeFunctionStart(name_id, .bool);
    try self.write("        ");
    try self.writeQualifiedCall("Bool", "decode", &.{ .{ .string_literal = {} }, .{ .raw = "fmt" } });
    try self.write("\n    }\n");
}

fn generateStrUnaryBuiltinFunction(self: *Self, name_id: u32, method: []const u8, return_type: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", return_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Str", .{ .string_literal = {} }, method, &.{});
    try self.write("\n");
}

fn generateStrBinaryBuiltinFunction(self: *Self, name_id: u32, method: []const u8, return_type: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", return_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Str", .{ .string_literal = {} }, method, &.{.{ .string_literal = {} }});
    try self.write("\n");
}

fn generateStrRepeatBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Str", .{ .string_literal = {} }, "repeat", &.{.{ .integer_literal = {} }});
    try self.write("\n");
}

fn generateStrWithCapacityBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Str.with_capacity", &.{.{ .integer_literal = {} }});
    try self.write("\n");
}

fn generateStrReserveBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Str", .{ .string_literal = {} }, "reserve", &.{.{ .integer_literal = {} }});
    try self.write("\n");
}

fn generateStrJoinWithBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Str.join_with", &.{ .{ .literal = .list_str }, .{ .string_literal = {} } });
    try self.write("\n");
}

fn generateStrIsEqBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Str.is_eq", &.{ .{ .string_literal = {} }, .{ .string_literal = {} } });
    try self.write("\n");
}

fn generateStrToUtf8BuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "List(U8)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Str", .{ .string_literal = {} }, "to_utf8", &.{});
    try self.write("\n");
}

fn generateStrFromUtf8LossyBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Str.from_utf8_lossy", &.{.{ .utf8_bytes_literal = {} }});
    try self.write("\n");
}

fn generateStrFromUtf8BuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeTrySignature(name_id, .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Str.from_utf8", &.{.{ .utf8_bytes_literal = {} }});
    try self.write("\n");
}

fn generateStrInspectBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    const typ = switch (self.reader.intRangeAtMost(u8, 0, 4)) {
        0 => Type.main,
        1 => Type.bool,
        2 => Type.str,
        3 => Type.u64,
        4 => Type.list_u64,
        else => unreachable,
    };

    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Str.inspect", &.{.{ .literal = typ }});
    try self.write("\n");
}

fn generateStrEncodeBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFmtEncodeFunctionStart(name_id);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall("Str", .{ .string_literal = {} }, "encode", &.{.{ .raw = "fmt" }});
    try self.write("\n    }\n");
}

fn generateStrDecodeBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFmtDecodeFunctionStart(name_id, .str);
    try self.write("        ");
    try self.writeQualifiedCall("Str", "decode", &.{ .{ .string_literal = {} }, .{ .raw = "fmt" } });
    try self.write("\n    }\n");
}

fn generateListIsEmptyBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeTypedLocalFunctionStart(name_id, "list", typ, .bool);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall("List", .{ .raw = "list" }, "is_empty", &.{});
    try self.write("\n    }\n");
}

fn generateTryPredicateBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    try self.writeTypedLocalFunctionStart(name_id, "fallible", typ, .bool);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall("Try", .{ .raw = "fallible" }, method, &.{});
    try self.write("\n    }\n");
}

fn generateTryOkOrBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    const ok_type = typ.tryOk();
    try self.writeTypedLocalFunctionStart(name_id, "fallible", typ, ok_type);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall("Try", .{ .raw = "fallible" }, "ok_or", &.{.{ .literal = ok_type }});
    try self.write("\n    }\n");
}

fn generateTryErrOrBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    const err_type = typ.tryErr();
    try self.writeTypedLocalFunctionStart(name_id, "fallible", typ, err_type);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall("Try", .{ .raw = "fallible" }, "err_or", &.{.{ .literal = err_type }});
    try self.write("\n    }\n");
}

fn generateTryMapOkErrBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    try self.writeTypedLocalFunctionStart(name_id, "fallible", typ, typ);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall("Try", .{ .raw = "fallible" }, method, &.{.{ .raw = "|value| value" }});
    try self.write("\n    }\n");
}

fn generateTryMapOkErrBangBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    try self.writeRawEffectfulFunctionSignature(name_id, "Main", tryTypeName(typ));
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        fallible : ");
    try self.writeType(typ);
    try self.write("\n        fallible = ");
    try self.writeLiteral(typ);
    try self.write("\n        Try.");
    try self.writeCall(method, &.{ .{ .raw = "fallible" }, .{ .raw = "|value| value" } });
    try self.write("\n    }\n");
}

fn generateTryIsEqBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    try self.writeTypedLocalFunctionStart(name_id, "fallible", typ, .bool);
    try self.write("        ");
    try self.writeCall("Try.is_eq", &.{ .{ .raw = "fallible" }, .{ .literal = typ } });
    try self.write("\n    }\n");
}

fn generateBoxBoxBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isBox());

    const inner = typ.boxInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Box.box", &.{.{ .literal = inner }});
    try self.write("\n");
}

fn generateLiteralFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeLiteral(typ);
    try self.write("\n");
}

fn generateIfFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| if ");
    try self.writeBoolLiteral();
    try self.write(" ");
    try self.writeLiteral(typ);
    try self.write(" else ");
    try self.writeLiteral(typ);
    try self.write("\n");
}

fn generateBlockFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n");
    if (self.reader.boolean()) {
        try self.write("        value : ");
        try self.writeType(typ);
        try self.write("\n");
    }
    try self.write("        value = ");
    try self.writeLiteral(typ);
    try self.write("\n        value\n    }\n");
}

fn generateIdentityFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.write("    value");
    try self.output.print(self.allocator, "{d}", .{name_id});
    try self.write(" : Main, ");
    try self.writeType(typ);
    try self.write(" -> ");
    try self.writeType(typ);
    try self.write("\n");

    try self.writeFunctionHeader(name_id);
    try self.write("|_, value| value\n");
}

fn generateMatchMainFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    if (self.reader.boolean()) {
        try self.write("|value| match value {\n");
        try self.write("        Main => ");
        try self.writeLiteral(typ);
        try self.write("\n        WithBool(flag) if flag => ");
        try self.writeBoolPayloadBranch(typ);
        try self.write("\n        WithBool(_) => ");
        try self.writeLiteral(typ);
        try self.write("\n        WithStrU64(name, count) if count >= 0 => ");
        try self.writeStrU64PayloadBranch(typ);
        try self.write("\n        WithStrU64(_, _) => ");
        try self.writeLiteral(typ);
        try self.write("\n    }\n");
    } else {
        try self.write("|value| match value {\n");
        try self.write("        Main => ");
        try self.writeLiteral(typ);
        try self.write("\n        WithBool(flag) => ");
        try self.writeBoolPayloadBranch(typ);
        try self.write("\n        WithStrU64(name, count) => ");
        try self.writeStrU64PayloadBranch(typ);
        try self.write("\n    }\n");
    }
}

fn generateSupportCountFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Support", .{ .literal = .support }, "count", &.{});
    try self.write("\n");
}

fn generateSupportMapFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .support);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Support", .{ .literal = .support }, "map_count", &.{.{ .raw = "|count| count + 1" }});
    try self.write("\n");
}

fn generateSupportPairFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.writeFunctionSignature(name_id, "Main", .tuple_str_u64);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| Support.to_pair(");
        try self.writeLiteral(.support);
        try self.write(")\n");
    } else {
        try self.writeFunctionSignature(name_id, "Main", .support);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| Support.from_pair(");
        try self.writeLiteral(.tuple_str_u64);
        try self.write(")\n");
    }
}

fn generateSupportTryFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match Support.maybe(");
    try self.writeLiteral(.support);
    try self.write(") {\n        Ok(count) => count\n        Err(_) => 0\n    }\n");
}

fn generateSupportForLoopFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n");
    try self.writeMutableLocalDeclaration("total", .{ .raw = "0" });
    try self.writeForInPrefix("item");
    try self.writeSupportListLiteral();
    try self.writeForInBodyStart();
    try self.writeMutableLocalAssignmentStart("total");
    try self.writeMutableLocalReference("total");
    try self.write(" + Support.count(item)\n");
    try self.writeForInBodyEnd();
    try self.write("        ");
    try self.writeMutableLocalReference("total");
    try self.write("\n    }\n");
}

fn generateRecordFieldFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isRecord());

    switch (typ) {
        .record_bool => {
            try self.writeFunctionSignature(name_id, "Main", .bool);
            try self.writeFunctionHeader(name_id);
            try self.write("|_| {\n");
            try self.write("        record = ");
            try self.writeLiteral(typ);
            try self.write("\n        record.flag\n    }\n");
        },
        .record_str_u64 => {
            const use_name = self.reader.boolean();
            try self.writeFunctionSignature(name_id, "Main", if (use_name) .str else .u64);
            try self.writeFunctionHeader(name_id);
            if (use_name) {
                try self.write("|_| {\n");
                try self.write("        record = ");
                try self.writeLiteral(typ);
                try self.write("\n        record.name\n    }\n");
            } else {
                try self.write("|_| {\n");
                try self.write("        record = ");
                try self.writeLiteral(typ);
                try self.write("\n        record.count\n    }\n");
            }
        },
        else => unreachable,
    }
}

fn generateRecordFromLocalsFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isRecord());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n");
    switch (typ) {
        .record_bool => {
            try self.write("        flag = ");
            try self.writeBoolLiteral();
            try self.write("\n        { flag: flag }\n    }\n");
        },
        .record_str_u64 => {
            try self.write("        name = ");
            try self.writeStringLiteral();
            try self.write("\n        count = ");
            try self.writeIntegerLiteral();
            try self.write("\n        { name: name, count: count }\n    }\n");
        },
        else => unreachable,
    }
}

fn generateRecordDestructureFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isRecord());

    switch (typ) {
        .record_bool => {
            try self.writeFunctionSignature(name_id, "Main", .bool);
            try self.writeFunctionHeader(name_id);
            try self.write("|_| {\n");
            try self.write("        record = ");
            try self.writeLiteral(typ);
            try self.write("\n        { flag } = record\n        flag\n    }\n");
        },
        .record_str_u64 => {
            const use_name = self.reader.boolean();
            try self.writeFunctionSignature(name_id, "Main", if (use_name) .str else .u64);
            try self.writeFunctionHeader(name_id);
            try self.write("|_| {\n");
            try self.write("        record = ");
            try self.writeLiteral(typ);
            if (use_name) {
                try self.write("\n        { name } = record\n        name\n    }\n");
            } else {
                try self.write("\n        { count } = record\n        count\n    }\n");
            }
        },
        else => unreachable,
    }
}

fn generateRecordBuilderFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .record_str_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| { name: Builder.field(");
    try self.writeStringLiteral();
    try self.write("), count: Builder.field(");
    try self.writeIntegerLiteral();
    try self.write(") }.Builder.run()\n");
}

fn generateListLenFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeLiteral(typ);
    try self.write(".len()\n");
}

fn generateListIterBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "Iter(U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_u64 }, "iter", &.{});
    try self.write("\n");
}

fn generateListFromIterBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.from_iter(");
    try self.writeListU64Iter();
    try self.write(")\n");
}

fn generateListWithCapacityBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("List.with_capacity", &.{.{ .integer_literal = {} }});
    try self.write("\n");
}

fn generateListReserveBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, "reserve", &.{.{ .integer_literal = {} }});
    try self.write("\n");
}

fn generateListReleaseExcessCapacityBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, "release_excess_capacity", &.{});
    try self.write("\n");
}

fn generateListSortWithBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.sort_with(");
    try self.writeLiteral(.list_u64);
    try self.write(", |left, right| if left < right LT else if left == right EQ else GT)\n");
}

fn generateListMatchFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeLiteral(typ);
    try self.write(" {\n");
    try self.write("        [] => ");
    try self.writeLiteral(elem_type);
    try self.write("\n        [value] => value\n        [value, _] => value\n        _ => ");
    try self.writeLiteral(elem_type);
    try self.write("\n    }\n");
}

fn generateListFirstFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match List.first(");
    try self.writeLiteral(typ);
    try self.write(") {\n        Ok(value) => value\n        Err(_) => ");
    try self.writeLiteral(elem_type);
    try self.write("\n    }\n");
}

fn generateListGetFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.generateListIndexLookupFunction(name_id, typ, "get");
}

fn generateListSubscriptFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    const index = self.reader.intRangeAtMost(u8, 0, 4);
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match List.subscript(");
    try self.writeLiteral(typ);
    try self.output.print(self.allocator, ", {d})", .{index});
    try self.write(" {\n        Ok(value) => value\n        Err(_) => ");
    try self.writeLiteral(elem_type);
    try self.write("\n    }\n");
}

fn generateListIndexLookupFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    const index = self.reader.intRangeAtMost(u8, 0, 4);
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    if (self.reader.boolean()) {
        try self.write("List.");
        try self.write(method);
        try self.write("(");
        try self.writeLiteral(typ);
        try self.output.print(self.allocator, ", {d}", .{index});
        try self.write(")");
    } else {
        try self.writeLiteral(typ);
        try self.write(".");
        try self.write(method);
        try self.output.print(self.allocator, "({d})", .{index});
    }
    try self.write(" {\n        Ok(value) => value\n        Err(_) => ");
    try self.writeLiteral(elem_type);
    try self.write("\n    }\n");
}

fn generateListAppendFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, "append", &.{.{ .literal = elem_type }});
    try self.write("\n");
}

fn generateListPrependFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, "prepend", &.{.{ .literal = elem_type }});
    try self.write("\n");
}

fn generateListConcatFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, "concat", &.{.{ .literal = typ }});
    try self.write("\n");
}

fn generateListMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    if (self.reader.boolean()) {
        try self.write("|_| List.map(");
        try self.writeLiteral(typ);
        try self.write(", |value| ");
        try self.writeListMapBody(typ.listElement());
        try self.write(")\n");
    } else {
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".map(|value| ");
        try self.writeListMapBody(typ.listElement());
        try self.write(")\n");
    }
}

fn generateListMap2Function(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    if (self.reader.boolean()) {
        try self.write("List.map2(");
        try self.writeLiteral(typ);
        try self.write(", ");
        try self.writeLiteral(typ);
        try self.write(", |left, right| ");
    } else {
        try self.writeLiteral(typ);
        try self.write(".map2(");
        try self.writeLiteral(typ);
        try self.write(", |left, right| ");
    }
    try self.writeListMap2Body(typ.listElement());
    try self.write(")\n");
}

fn generateListKeepDropBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, method, &.{.{ .raw = "|_| True" }});
    try self.write("\n");
}

fn generateListPredicateBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, method, &.{.{ .raw = "|_| True" }});
    try self.write("\n");
}

fn generateListContainsBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, "contains", &.{.{ .literal = typ.listElement() }});
    try self.write("\n");
}

fn generateListCompareBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, method, &.{.{ .literal = typ }});
    try self.write("\n");
}

fn generateListIsEqBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("List.is_eq", &.{ .{ .literal = typ }, .{ .literal = typ } });
    try self.write("\n");
}

fn generateListSetFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    const index = self.reader.intRangeAtMost(u8, 0, 4);
    try self.writeListTrySignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.set(");
    try self.writeLiteral(typ);
    try self.output.print(self.allocator, ", {d}, ", .{index});
    try self.writeLiteral(elem_type);
    try self.write(")\n");
}

fn generateListUpdateFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const index = self.reader.intRangeAtMost(u8, 0, 4);
    try self.writeListTrySignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.update(");
    try self.writeLiteral(typ);
    try self.output.print(self.allocator, ", {d}, |value| ", .{index});
    try self.writeListMapBody(typ.listElement());
    try self.write(")\n");
}

fn generateListReplaceFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    const index = self.reader.intRangeAtMost(u8, 0, 4);
    try self.write("    value");
    try self.output.print(self.allocator, "{d}", .{name_id});
    try self.write(" : Main -> Try({ list : ");
    try self.writeType(typ);
    try self.write(", prev : ");
    try self.writeType(elem_type);
    try self.write(" }, _)\n");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.replace(");
    try self.writeLiteral(typ);
    try self.output.print(self.allocator, ", {d}, ", .{index});
    try self.writeLiteral(elem_type);
    try self.write(")\n");
}

fn generateListRevFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    if (self.reader.boolean()) {
        try self.write("|_| List.rev(");
        try self.writeLiteral(typ);
        try self.write(")\n");
    } else {
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".rev()\n");
    }
}

fn generateListLastBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, "last", &.{});
    try self.write(" {\n        Ok(value) => value\n        Err(_) => ");
    try self.writeLiteral(elem_type);
    try self.write("\n    }\n");
}

fn generateListSingleBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("List.single", &.{.{ .literal = typ.listElement() }});
    try self.write("\n");
}

fn generateListRepeatBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("List.repeat", &.{ .{ .literal = typ.listElement() }, .{ .integer_literal = {} } });
    try self.write("\n");
}

fn generateListMapWithIndexFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.map_with_index(");
    try self.writeLiteral(typ);
    try self.write(", |value, index| ");
    if (typ.listElement() == .u64 and self.reader.boolean()) {
        try self.write("value + index");
    } else {
        try self.writeListMapBody(typ.listElement());
    }
    try self.write(")\n");
}

fn generateListFoldFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.fold(");
    try self.writeLiteral(typ);
    try self.write(", ");
    try self.writeLiteral(elem_type);
    try self.write(", |acc, _| ");
    if (self.reader.boolean()) {
        try self.write("acc");
    } else {
        try self.writeLiteral(elem_type);
    }
    try self.write(")\n");
}

fn generateListFoldWithIndexFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.fold_with_index(");
    try self.writeLiteral(typ);
    try self.write(", ");
    try self.writeLiteral(elem_type);
    try self.write(", |acc, _item, _index| ");
    if (self.reader.boolean()) {
        try self.write("acc");
    } else {
        try self.writeLiteral(elem_type);
    }
    try self.write(")\n");
}

fn generateListFoldUntilFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.fold_until(");
    try self.writeLiteral(.list_u64);
    try self.write(", 0, |acc, item| if item > 50 Break(acc) else Continue(acc + item))\n");
}

fn generateListFoldWithIndexUntilFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.fold_with_index_until(");
    try self.writeLiteral(.list_u64);
    try self.write(", 0, |acc, item, index| if index > 50 Break(acc) else Continue(acc + item + index))\n");
}

fn generateListFoldRevFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.fold_rev(");
    try self.writeLiteral(typ);
    try self.write(", ");
    try self.writeLiteral(elem_type);
    try self.write(", |_, acc| ");
    if (self.reader.boolean()) {
        try self.write("acc");
    } else {
        try self.writeLiteral(elem_type);
    }
    try self.write(")\n");
}

fn generateListCountIfFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    if (self.reader.boolean()) {
        try self.write("|_| List.count_if(");
        try self.writeLiteral(typ);
        try self.write(", |_| ");
        try self.writeBoolLiteral();
        try self.write(")\n");
    } else {
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".count_if(|_| ");
        try self.writeBoolLiteral();
        try self.write(")\n");
    }
}

fn generateListFindElementBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, method, &.{.{ .raw = "|_| True" }});
    try self.write(" {\n        Ok(value) => value\n        Err(_) => ");
    try self.writeLiteral(elem_type);
    try self.write("\n    }\n");
}

fn generateListFindIndexBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, method, &.{.{ .raw = "|_| True" }});
    try self.write(" {\n        Ok(index) => index\n        Err(_) => 0\n    }\n");
}

fn generateListSublistFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const start = self.reader.intRangeAtMost(u8, 0, 4);
    const len = self.reader.intRangeAtMost(u8, 0, 4);
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    if (self.reader.boolean()) {
        try self.write("|_| List.sublist(");
        try self.writeLiteral(typ);
        try self.output.print(self.allocator, ", {{ start: {d}, len: {d} }})\n", .{ start, len });
    } else {
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.output.print(self.allocator, ".sublist({{ start: {d}, len: {d} }})\n", .{ start, len });
    }
}

fn generateListTakeDropBuiltinFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, method, &.{.{ .integer_literal = {} }});
    try self.write("\n");
}

fn generateListSwapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const first = self.reader.intRangeAtMost(u8, 0, 4);
    const second = self.reader.intRangeAtMost(u8, 0, 4);
    try self.writeListTrySignature(name_id, typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.swap(");
    try self.writeLiteral(typ);
    try self.output.print(self.allocator, ", {d}, {d})\n", .{ first, second });
}

fn generateListForLoopFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n");
    try self.writeMutableLocalDeclaration("items", .{ .raw = "[]" });
    try self.writeForInPrefix("item");
    try self.writeNonEmptyListLiteral(typ.listElement());
    try self.writeForInBodyStart();
    try self.writeMutableLocalAssignmentStart("items");
    try self.writeMutableLocalReference("items");
    try self.write(".append(item)\n");
    try self.writeForInBodyEnd();
    try self.write("        ");
    try self.writeMutableLocalReference("items");
    try self.write("\n    }\n");
}

fn generateListForEachBangBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawEffectfulFunctionSignature(name_id, "Main", "{}");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| List.for_each!(");
    try self.writeLiteral(.list_u64);
    try self.write(", |_item| {})\n");
}

fn generateListSplitAtBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    const index = self.reader.intRangeAtMost(u8, 0, 4);
    try self.write("    value");
    try self.output.print(self.allocator, "{d}", .{name_id});
    try self.write(" : Main -> { before : List(U64), others : List(U64) }\n");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    if (self.reader.boolean()) {
        try self.write("List.split_at(");
        try self.writeLiteral(.list_u64);
        try self.output.print(self.allocator, ", {d})\n", .{index});
    } else {
        try self.writeLiteral(.list_u64);
        try self.output.print(self.allocator, ".split_at({d})\n", .{index});
    }
}

fn generateListSplitOnBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_u64 }, "split_on", &.{.{ .literal = .u64 }});
    try self.write("\n");
}

fn generateListSplitIfBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_u64 }, "split_if", &.{.{ .raw = "|_| True" }});
    try self.write("\n");
}

fn generateListSplitOnListBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_u64 }, "split_on_list", &.{.{ .literal = .list_u64 }});
    try self.write("\n");
}

fn generateListSplitFirstLastBuiltinFunction(self: *Self, name_id: u32, method: []const u8) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_u64 }, method, &.{.{ .literal = .u64 }});
    try self.write(" {\n        Ok(parts) => parts.before\n        Err(_) => []\n    }\n");
}

fn generateListJoinWithBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    const elem_type = typ.listElement();
    try self.writeFunctionSignature(name_id, "Main", elem_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = typ }, "join_with", &.{.{ .literal = elem_type }});
    try self.write("\n");
}

fn generateListJoinListWithBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_list_u64 }, "join_list_with", &.{.{ .literal = .list_u64 }});
    try self.write("\n");
}

fn generateListAggregateBuiltinFunction(self: *Self, name_id: u32, method: []const u8) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_u64 }, method, &.{});
    try self.write("\n");
}

fn generateListMinMaxBuiltinFunction(self: *Self, name_id: u32, method: []const u8) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_u64 }, method, &.{});
    try self.write(" {\n        Ok(value) => value\n        Err(_) => 0\n    }\n");
}

fn generateListEncodeBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFmtEncodeFunctionStart(name_id);
    try self.write("        ");
    try self.writeAssociatedOrMethodCall("List", .{ .literal = .list_u64 }, "encode", &.{.{ .raw = "fmt" }});
    try self.write("\n    }\n");
}

fn generateListDecodeBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFmtDecodeFunctionStart(name_id, .list_u64);
    try self.write("        ");
    try self.writeQualifiedCall("List", "decode", &.{ .{ .string_literal = {} }, .{ .raw = "fmt" } });
    try self.write("\n    }\n");
}

fn generateIterNextBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match Iter.next(");
    try self.writeListU64Iter();
    try self.write(") {\n        One(record) => record.item\n        Skip(_) => 0\n        Done => 0\n    }\n");
}

fn generateIterCustomBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "Iter(U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Iter.custom(0, Known(4), |state| if state < 4 Ok((state, state + 1)) else Err(NoMore))\n");
}

fn generateIterIterBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "Iter(U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeAssociatedOrMethodCall("Iter", .{ .raw = "List.iter([1, 2, 3])" }, "iter", &.{});
    try self.write("\n");
}

fn generateIterTransformCollectBuiltinFunction(self: *Self, name_id: u32, method: []const u8) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Iter.collect(Iter.");
    try self.write(method);
    try self.write("(");
    try self.writeListU64Iter();
    try self.write(", |item| item + 1))\n");
}

fn generateIterPredicateCollectBuiltinFunction(self: *Self, name_id: u32, method: []const u8) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Iter.collect(Iter.");
    try self.write(method);
    try self.write("(");
    try self.writeListU64Iter();
    try self.write(", |_| True))\n");
}

fn generateIterFoldBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Iter.fold(");
    try self.writeListU64Iter();
    try self.write(", 0, |acc, item| acc + item)\n");
}

fn generateIterSizeHintBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match Iter.size_hint(");
    try self.writeListU64Iter();
    try self.write(") {\n        Known(len) => len\n        Unknown => 0\n    }\n");
}

fn generateIterCollectBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Iter.collect(");
    try self.writeListU64Iter();
    try self.write(")\n");
}

fn generateIterStreamBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "Stream(U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Iter.stream(");
    try self.writeListU64Iter();
    try self.write(")\n");
}

fn generateIterTakeDropCollectBuiltinFunction(self: *Self, name_id: u32, method: []const u8) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .list_u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Iter.collect(Iter.");
    try self.write(method);
    try self.write("(");
    try self.writeListU64Iter();
    try self.write(", ");
    try self.writeIntegerLiteral();
    try self.write("))\n");
}

fn generateStreamFromIterBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "Stream(U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Stream.from_iter(");
    try self.writeListU64Iter();
    try self.write(")\n");
}

fn generateStreamMapBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "Stream(U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Stream.map(Stream.from_iter(");
    try self.writeListU64Iter();
    try self.write("), |item| item + 1)\n");
}

fn generateStreamMapBangBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawEffectfulFunctionSignature(name_id, "Main", "Stream(U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Stream.map!(Stream.from_iter(");
    try self.writeListU64Iter();
    try self.write("), |item| item + 1)\n");
}

fn generateStreamNextBangBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawEffectfulFunctionSignature(name_id, "Main", "U64");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match Stream.next!(Stream.from_iter(");
    try self.writeListU64Iter();
    try self.write(")) {\n        One(record) => record.item\n        Skip(_) => 0\n        Done => 0\n    }\n");
}

fn generateStreamSizeHintBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match Stream.size_hint(Stream.from_iter(");
    try self.writeListU64Iter();
    try self.write(")) {\n        Known(len) => len\n        Unknown => 0\n    }\n");
}

fn generateStreamCollectBangBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawEffectfulFunctionSignature(name_id, "Main", "List(U64)");
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Stream.collect!(Stream.from_iter(");
    try self.writeListU64Iter();
    try self.write("))\n");
}

fn writeListU64Iter(self: *Self) std.mem.Allocator.Error!void {
    try self.writeCall("List.iter", &.{.{ .literal = .list_u64 }});
}

fn writeU64RangeIterator(self: *Self) std.mem.Allocator.Error!void {
    try self.writeCall("U64.to", &.{ .{ .small_number_literal = .u64 }, .{ .small_number_literal = .u64 } });
}

fn generateTupleDestructureFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTuple());

    const use_first = self.reader.boolean();
    const result_type = tupleFieldType(typ, use_first);
    try self.writeFunctionSignature(name_id, "Main", result_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n");
    try self.write("        pair = ");
    try self.writeLiteral(typ);
    try self.write(if (use_first)
        "\n        (value, _) = pair\n        value\n    }\n"
    else
        "\n        (_, value) = pair\n        value\n    }\n");
}

fn generateTupleMatchFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTuple());

    const use_first = self.reader.boolean();
    const result_type = tupleFieldType(typ, use_first);
    try self.writeFunctionSignature(name_id, "Main", result_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeLiteral(typ);
    try self.write(if (use_first)
        " {\n        (value, _) => value\n    }\n"
    else
        " {\n        (_, value) => value\n    }\n");
}

fn generateFunctionApplyFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isFunction());

    const result_type = typ.functionResult();
    try self.writeFunctionSignature(name_id, "Main", result_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| (");
    try self.writeLiteral(typ);
    try self.write(")(");
    try self.writeLiteral(result_type);
    try self.write(")\n");
}

fn generateTryMatchFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    const ok_type = typ.tryOk();
    try self.writeFunctionSignature(name_id, "Main", ok_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeLiteral(typ);
    try self.write(" {\n        Ok(value) => value\n        Err(_) => ");
    try self.writeLiteral(ok_type);
    try self.write("\n    }\n");
}

fn generateTryMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    const ok_type = typ.tryOk();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeLiteral(typ);
    try self.write(" {\n        Ok(value) => Ok(");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(ok_type);
    }
    try self.write(")\n        Err(error) => Err(error)\n    }\n");
}

fn generateTryQuestionFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTry());

    const ok_type = typ.tryOk();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        value = ");
    try self.writeLiteral(typ);
    try self.write("?\n        Ok(");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(ok_type);
    }
    try self.write(")\n    }\n");
}

fn generateNumericCompareFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeSmallNumberLiteral(typ);
    try self.write(switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => " == ",
        1 => " != ",
        2 => " < ",
        3 => " >= ",
        else => unreachable,
    });
    try self.writeSmallNumberLiteral(typ);
    try self.write("\n");
}

fn generateEqualityFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSimpleEquatable());

    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeLiteral(typ);
    try self.write(if (self.reader.boolean()) " == " else " != ");
    try self.writeLiteral(typ);
    try self.write("\n");
}

fn generateWhereEqFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSimpleEquatable());

    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Main.where_eq(");
    try self.writeLiteral(typ);
    try self.write(", ");
    try self.writeLiteral(typ);
    try self.write(")\n");
}

fn generateTreeLabelFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isTree());

    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    if (self.reader.boolean()) {
        try self.write("|_| Tree.label(");
        try self.writeLiteral(typ);
        try self.write(")\n");
    } else {
        try self.write("|_| {\n        tree : Tree\n        tree = ");
        try self.writeLiteral(typ);
        try self.write("\n        tree.label()\n    }\n");
    }
}

fn generateStrConcatFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => {
            try self.write("|_| Str.concat(");
            try self.writeStringLiteral();
            try self.write(", ");
            try self.writeStringLiteral();
            try self.write(")\n");
        },
        1 => {
            try self.write("|_| ");
            try self.writeStringLiteral();
            try self.write(".concat(");
            try self.writeStringLiteral();
            try self.write(")\n");
        },
        2 => {
            try self.write("|_| {\n        prefix = ");
            try self.writeStringLiteral();
            try self.write("\n        suffix = ");
            try self.writeStringLiteral();
            try self.write("\n        prefix.concat(suffix)\n    }\n");
        },
        else => unreachable,
    }
}

fn generateStrInterpolationFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .str);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        left = ");
    try self.writeStringLiteral();
    try self.write("\n        right = ");
    try self.writeStringLiteral();
    try self.write("\n        \"");
    if (self.reader.boolean()) try self.write("prefix ");
    try self.write("${left}");
    if (self.reader.boolean()) try self.write(" ");
    try self.write("${right}");
    if (self.reader.boolean()) try self.write(" suffix");
    try self.write("\"\n    }\n");
}

fn generateNumericArithmeticFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeSmallNumberLiteral(typ);
    if (typ.isUnsignedInteger()) {
        try self.write(if (self.reader.boolean()) " + " else " * ");
    } else {
        try self.write(switch (self.reader.intRangeAtMost(u8, 0, 2)) {
            0 => " + ",
            1 => " - ",
            2 => " * ",
            else => unreachable,
        });
    }
    try self.writeSmallNumberLiteral(typ);
    try self.write("\n");
}

fn generateWherePlusFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber() or typ == .my_num);

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Main.where_plus(");
    try self.writeLiteral(typ);
    try self.write(", ");
    try self.writeLiteral(typ);
    try self.write(")\n");
}

fn generateCustomNumberUnwrapFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .u64);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| match ");
    try self.writeLiteral(.my_num);
    try self.write(" {\n        MyNum(value) => value\n    }\n");
}

fn generateCustomNumberArithmeticFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .my_num);
    try self.writeFunctionHeader(name_id);
    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => {
            try self.write("|_| ");
            try self.writeTypedCustomNumberLiteral();
            try self.write(" + ");
            try self.writeTypedCustomNumberLiteral();
            try self.write("\n");
        },
        1 => {
            try self.write("|_| -");
            try self.writeTypedCustomNumberLiteral();
            try self.write("\n");
        },
        2 => {
            try self.write("|_| ");
            try self.writeTypedCustomNumberLiteral();
            try self.write("\n");
        },
        else => unreachable,
    }
}

fn generateHolderGetFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isHolder());

    const inner = typ.holderInner();
    try self.writeFunctionSignature(name_id, "Main", inner);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Holder.get(");
    try self.writeLiteral(typ);
    try self.write(")\n");
}

fn generateHolderMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isHolder());

    const inner = typ.holderInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Holder.map(");
    try self.writeLiteral(typ);
    try self.write(", |value| ");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(inner);
    }
    try self.write(")\n");
}

fn generateHolderMethodFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isHolder());

    if (self.reader.boolean()) {
        const inner = typ.holderInner();
        try self.writeFunctionSignature(name_id, "Main", inner);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".get()\n");
    } else {
        const inner = typ.holderInner();
        try self.writeFunctionSignature(name_id, "Main", typ);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".map(|value| ");
        if (self.reader.boolean()) {
            try self.write("value");
        } else {
            try self.writeLiteral(inner);
        }
        try self.write(")\n");
    }
}

fn generateHolderMap2Function(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isHolder());

    const inner = typ.holderInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Holder.map2(");
    try self.writeLiteral(typ);
    try self.write(", Holder.make(");
    try self.writeBoolLiteral();
    try self.write("), |value, _| ");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(inner);
    }
    try self.write(")\n");
}

fn generateBuilderRunFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isBuilder());

    const inner = typ.builderInner();
    try self.writeFunctionSignature(name_id, "Main", inner);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Builder.run(");
    try self.writeLiteral(typ);
    try self.write(")\n");
}

fn generateBuilderMap2Function(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isBuilder());

    const inner = typ.builderInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Builder.map2(");
    try self.writeLiteral(typ);
    try self.write(", Builder.field(");
    try self.writeBoolLiteral();
    try self.write("), |value, _| ");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(inner);
    }
    try self.write(")\n");
}

fn generateBuilderMethodFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isBuilder());

    const inner = typ.builderInner();
    try self.writeFunctionSignature(name_id, "Main", inner);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeLiteral(typ);
    try self.write(".run()\n");
}

fn generateWrapGetFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isWrap());

    const inner = typ.wrapInner();
    try self.writeFunctionSignature(name_id, "Main", inner);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Wrap.get(");
    try self.writeLiteral(typ);
    try self.write(")\n");
}

fn generateWrapMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isWrap());

    const inner = typ.wrapInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Wrap.map(");
    try self.writeLiteral(typ);
    try self.write(", |value| ");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(inner);
    }
    try self.write(")\n");
}

fn generateWrapMethodFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isWrap());

    if (self.reader.boolean()) {
        const inner = typ.wrapInner();
        try self.writeFunctionSignature(name_id, "Main", inner);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".get()\n");
    } else {
        const inner = typ.wrapInner();
        try self.writeFunctionSignature(name_id, "Main", typ);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".map(|value| ");
        if (self.reader.boolean()) {
            try self.write("value");
        } else {
            try self.writeLiteral(inner);
        }
        try self.write(")\n");
    }
}

fn generateWrapMap2Function(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isWrap());

    const inner = typ.wrapInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Wrap.map2(");
    try self.writeLiteral(typ);
    try self.write(", Wrap.make(");
    try self.writeBoolLiteral();
    try self.write("), |value, _| ");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(inner);
    }
    try self.write(")\n");
}

fn generateSecretGetFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSecret());

    const inner = typ.secretInner();
    try self.writeFunctionSignature(name_id, "Main", inner);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Secret.get(");
    try self.writeLiteral(typ);
    try self.write(")\n");
}

fn generateSecretMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSecret());

    const inner = typ.secretInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Secret.map(");
    try self.writeLiteral(typ);
    try self.write(", |value| ");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(inner);
    }
    try self.write(")\n");
}

fn generateSecretMethodFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSecret());

    if (self.reader.boolean()) {
        const inner = typ.secretInner();
        try self.writeFunctionSignature(name_id, "Main", inner);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".get()\n");
    } else {
        const inner = typ.secretInner();
        try self.writeFunctionSignature(name_id, "Main", typ);
        try self.writeFunctionHeader(name_id);
        try self.write("|_| ");
        try self.writeLiteral(typ);
        try self.write(".map(|value| ");
        if (self.reader.boolean()) {
            try self.write("value");
        } else {
            try self.writeLiteral(inner);
        }
        try self.write(")\n");
    }
}

fn generateSecretMap2Function(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSecret());

    const inner = typ.secretInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Secret.map2(");
    try self.writeLiteral(typ);
    try self.write(", Secret.make(");
    try self.writeBoolLiteral();
    try self.write("), |value, _| ");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(inner);
    }
    try self.write(")\n");
}

fn generateWhereMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isHolder() or typ.isWrap() or typ.isSecret());

    const inner = if (typ.isHolder()) typ.holderInner() else if (typ.isWrap()) typ.wrapInner() else typ.secretInner();
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Main.where_map(");
    try self.writeLiteral(typ);
    try self.write(", |value| ");
    if (self.reader.boolean()) {
        try self.write("value");
    } else {
        try self.writeLiteral(inner);
    }
    try self.write(")\n");
}

fn generateBoxUnboxFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isBox());

    const inner = typ.boxInner();
    try self.writeFunctionSignature(name_id, "Main", inner);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Box.unbox(");
    try self.writeLiteral(typ);
    try self.write(")\n");
}

fn generateBoxRoundTripFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isBox());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| Box.box(Box.unbox(");
    try self.writeLiteral(typ);
    try self.write("))\n");
}

fn generateBoxMethodFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isBox());

    const inner = typ.boxInner();
    try self.writeFunctionSignature(name_id, "Main", inner);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeLiteral(typ);
    try self.write(".unbox()\n");
}

fn generateDictEmptyBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Dict.empty", &.{});
    try self.write("\n");
}

fn generateDictIsEqBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, .bool);
    try self.write("        ");
    try self.writeCall("Dict.is_eq", &.{ .{ .raw = "dict" }, .{ .literal = typ } });
    try self.write("\n    }\n");
}

fn generateDictSingleBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Dict.single", &.{ .{ .literal = typ.dictKey() }, .{ .literal = typ.dictValue() } });
    try self.write("\n");
}

fn generateDictLenFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, .u64);
    if (self.reader.boolean()) {
        try self.write("        Dict.len(dict)\n    }\n");
    } else {
        try self.write("        dict.len()\n    }\n");
    }
}

fn generateDictIsEmptyFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, .bool);
    if (self.reader.boolean()) {
        try self.write("        Dict.is_empty(dict)\n    }\n");
    } else {
        try self.write("        dict.is_empty()\n    }\n");
    }
}

fn generateDictContainsFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    const key = typ.dictKey();
    try self.writeDictLocalFunctionStart(name_id, typ, .bool);
    if (self.reader.boolean()) {
        try self.write("        Dict.contains(dict, ");
        try self.writeLiteral(key);
        try self.write(")\n    }\n");
    } else {
        try self.write("        dict.contains(");
        try self.writeLiteral(key);
        try self.write(")\n    }\n");
    }
}

fn generateDictInsertRemoveFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    const key = typ.dictKey();
    const value = typ.dictValue();
    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        if (self.reader.boolean()) {
            try self.write("        Dict.insert(dict, ");
            try self.writeLiteral(key);
            try self.write(", ");
            try self.writeLiteral(value);
            try self.write(")\n    }\n");
        } else {
            try self.write("        dict.insert(");
            try self.writeLiteral(key);
            try self.write(", ");
            try self.writeLiteral(value);
            try self.write(")\n    }\n");
        }
    } else {
        if (self.reader.boolean()) {
            try self.write("        Dict.remove(dict, ");
            try self.writeLiteral(key);
            try self.write(")\n    }\n");
        } else {
            try self.write("        dict.remove(");
            try self.writeLiteral(key);
            try self.write(")\n    }\n");
        }
    }
}

fn generateDictInsertBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    const key = typ.dictKey();
    const value = typ.dictValue();
    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        ");
        try self.writeCall("Dict.insert", &.{ .{ .raw = "dict" }, .{ .literal = key }, .{ .literal = value } });
        try self.write("\n    }\n");
    } else {
        try self.write("        ");
        try self.writeMethodCall(.{ .raw = "dict" }, "insert", &.{ .{ .literal = key }, .{ .literal = value } });
        try self.write("\n    }\n");
    }
}

fn generateDictRemoveBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    const key = typ.dictKey();
    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        ");
        try self.writeCall("Dict.remove", &.{ .{ .raw = "dict" }, .{ .literal = key } });
        try self.write("\n    }\n");
    } else {
        try self.write("        ");
        try self.writeMethodCall(.{ .raw = "dict" }, "remove", &.{.{ .literal = key }});
        try self.write("\n    }\n");
    }
}

fn generateDictGetFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    const key = typ.dictKey();
    const value = typ.dictValue();
    try self.writeDictLocalFunctionStart(name_id, typ, value);
    try self.write("        match ");
    if (self.reader.boolean()) {
        try self.write("Dict.get(dict, ");
        try self.writeLiteral(key);
        try self.write(")");
    } else {
        try self.write("dict.get(");
        try self.writeLiteral(key);
        try self.write(")");
    }
    try self.write(" {\n");
    try self.write("            Ok(found) => found\n            Err(_) => ");
    try self.writeLiteral(value);
    try self.write("\n        }\n    }\n");
}

fn generateDictToListFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, typ.dictPairList());
    if (self.reader.boolean()) {
        try self.write("        Dict.to_list(dict)\n    }\n");
    } else {
        try self.write("        dict.to_list()\n    }\n");
    }
}

fn generateDictKeysFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    const key_list = switch (typ.dictKey()) {
        .str => Type.list_str,
        .u64 => Type.list_u64,
        else => unreachable,
    };
    try self.writeDictLocalFunctionStart(name_id, typ, key_list);
    if (self.reader.boolean()) {
        try self.write("        Dict.keys(dict)\n    }\n");
    } else {
        try self.write("        dict.keys()\n    }\n");
    }
}

fn generateDictValuesFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    const value_list = switch (typ.dictValue()) {
        .str => Type.list_str,
        .u64 => Type.list_u64,
        else => unreachable,
    };
    try self.writeDictLocalFunctionStart(name_id, typ, value_list);
    if (self.reader.boolean()) {
        try self.write("        Dict.values(dict)\n    }\n");
    } else {
        try self.write("        dict.values()\n    }\n");
    }
}

fn generateDictFoldFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, .u64);
    if (self.reader.boolean()) {
        try self.write("        Dict.fold(dict, 0, |acc, _key, _value| acc + 1)\n    }\n");
    } else {
        try self.write("        dict.fold(0, |acc, _key, _value| acc + 1)\n    }\n");
    }
}

fn generateDictKeepDropFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        if (self.reader.boolean()) {
            try self.write("        Dict.keep_if(dict, |_| ");
        } else {
            try self.write("        dict.keep_if(|_| ");
        }
    } else {
        if (self.reader.boolean()) {
            try self.write("        Dict.drop_if(dict, |_| ");
        } else {
            try self.write("        dict.drop_if(|_| ");
        }
    }
    try self.writeBoolLiteral();
    try self.write(")\n    }\n");
}

fn generateDictKeepDropSpecificFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    try self.write("        ");
    if (self.reader.boolean()) {
        try self.write("Dict.");
        try self.writeCall(method, &.{ .{ .raw = "dict" }, .{ .raw = "|_| True" } });
    } else {
        try self.writeMethodCall(.{ .raw = "dict" }, method, &.{.{ .raw = "|_| True" }});
    }
    try self.write("\n    }\n");
}

fn generateDictMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        Dict.map(dict, |_key, value| value)\n    }\n");
    } else {
        try self.write("        dict.map(|_key, value| value)\n    }\n");
    }
}

fn generateDictJoinMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        Dict.join_map(dict, |key, value| Dict.single(key, value))\n    }\n");
    } else {
        try self.write("        dict.join_map(|key, value| Dict.single(key, value))\n    }\n");
    }
}

fn generateDictInsertAllFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        Dict.insert_all(dict, ");
        try self.writeLiteral(typ);
        try self.write(")\n    }\n");
    } else {
        try self.write("        dict.insert_all(");
        try self.writeLiteral(typ);
        try self.write(")\n    }\n");
    }
}

fn generateDictKeepSharedFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        Dict.keep_shared(dict, ");
        try self.writeLiteral(typ);
        try self.write(")\n    }\n");
    } else {
        try self.write("        dict.keep_shared(");
        try self.writeLiteral(typ);
        try self.write(")\n    }\n");
    }
}

fn generateDictRemoveAllFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        Dict.remove_all(dict, ");
        try self.writeLiteral(typ);
        try self.write(")\n    }\n");
    } else {
        try self.write("        dict.remove_all(");
        try self.writeLiteral(typ);
        try self.write(")\n    }\n");
    }
}

fn generateDictUpdateFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    const key = typ.dictKey();
    const value = typ.dictValue();
    try self.writeDictLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        Dict.update(dict, ");
        try self.writeLiteral(key);
        try self.write(", |possible| match possible {\n");
    } else {
        try self.write("        dict.update(");
        try self.writeLiteral(key);
        try self.write(", |possible| match possible {\n");
    }
    try self.write("            Ok(found) => Ok(found)\n            Err(Missing) => ");
    if (self.reader.boolean()) {
        try self.write("Ok(");
        try self.writeLiteral(value);
        try self.write(")");
    } else {
        try self.write("Err(Missing)");
    }
    try self.write("\n        })\n    }\n");
}

fn generateDictFromListBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Dict.from_list", &.{.{ .literal = typ.dictPairList() }});
    try self.write("\n");
}

fn writeDictLocalFunctionStart(self: *Self, name_id: u32, typ: Type, return_type: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isDict());

    try self.writeTypedLocalFunctionStart(name_id, "dict", typ, return_type);
}

fn generateSetEmptyBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Set.empty", &.{});
    try self.write("\n");
}

fn generateSetIsEqBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeSetLocalFunctionStart(name_id, typ, .bool);
    try self.write("        ");
    try self.writeCall("Set.is_eq", &.{ .{ .raw = "set" }, .{ .literal = typ } });
    try self.write("\n    }\n");
}

fn generateSetSingleBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Set.single", &.{.{ .literal = typ.setElement() }});
    try self.write("\n");
}

fn generateSetLenFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeSetLocalFunctionStart(name_id, typ, .u64);
    if (self.reader.boolean()) {
        try self.write("        Set.len(set)\n    }\n");
    } else {
        try self.write("        set.len()\n    }\n");
    }
}

fn generateSetIsEmptyFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeSetLocalFunctionStart(name_id, typ, .bool);
    if (self.reader.boolean()) {
        try self.write("        Set.is_empty(set)\n    }\n");
    } else {
        try self.write("        set.is_empty()\n    }\n");
    }
}

fn generateSetContainsFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    const elem = typ.setElement();
    try self.writeSetLocalFunctionStart(name_id, typ, .bool);
    if (self.reader.boolean()) {
        try self.write("        Set.contains(set, ");
        try self.writeLiteral(elem);
        try self.write(")\n    }\n");
    } else {
        try self.write("        set.contains(");
        try self.writeLiteral(elem);
        try self.write(")\n    }\n");
    }
}

fn generateSetInsertRemoveFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    const elem = typ.setElement();
    try self.writeSetLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        if (self.reader.boolean()) {
            try self.write("        Set.insert(set, ");
        } else {
            try self.write("        set.insert(");
        }
        try self.writeLiteral(elem);
        try self.write(")\n    }\n");
    } else {
        if (self.reader.boolean()) {
            try self.write("        Set.remove(set, ");
        } else {
            try self.write("        set.remove(");
        }
        try self.writeLiteral(elem);
        try self.write(")\n    }\n");
    }
}

fn generateSetInsertBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    const elem = typ.setElement();
    try self.writeSetLocalFunctionStart(name_id, typ, typ);
    try self.write("        ");
    if (self.reader.boolean()) {
        try self.writeCall("Set.insert", &.{ .{ .raw = "set" }, .{ .literal = elem } });
    } else {
        try self.writeMethodCall(.{ .raw = "set" }, "insert", &.{.{ .literal = elem }});
    }
    try self.write("\n    }\n");
}

fn generateSetRemoveBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    const elem = typ.setElement();
    try self.writeSetLocalFunctionStart(name_id, typ, typ);
    try self.write("        ");
    if (self.reader.boolean()) {
        try self.writeCall("Set.remove", &.{ .{ .raw = "set" }, .{ .literal = elem } });
    } else {
        try self.writeMethodCall(.{ .raw = "set" }, "remove", &.{.{ .literal = elem }});
    }
    try self.write("\n    }\n");
}

fn generateSetToListFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeSetLocalFunctionStart(name_id, typ, typ.setList());
    if (self.reader.boolean()) {
        try self.write("        Set.to_list(set)\n    }\n");
    } else {
        try self.write("        set.to_list()\n    }\n");
    }
}

fn generateSetKeepDropFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeSetLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        if (self.reader.boolean()) {
            try self.write("        Set.keep_if(set, |_| ");
        } else {
            try self.write("        set.keep_if(|_| ");
        }
    } else {
        if (self.reader.boolean()) {
            try self.write("        Set.drop_if(set, |_| ");
        } else {
            try self.write("        set.drop_if(|_| ");
        }
    }
    try self.writeBoolLiteral();
    try self.write(")\n    }\n");
}

fn generateSetKeepDropSpecificFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeSetLocalFunctionStart(name_id, typ, typ);
    try self.write("        ");
    if (self.reader.boolean()) {
        try self.write("Set.");
        try self.writeCall(method, &.{ .{ .raw = "set" }, .{ .raw = "|_| True" } });
    } else {
        try self.writeMethodCall(.{ .raw = "set" }, method, &.{.{ .raw = "|_| True" }});
    }
    try self.write("\n    }\n");
}

fn generateSetUnionFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.generateSetCombineFunction(name_id, typ, "union");
}

fn generateSetIntersectionFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.generateSetCombineFunction(name_id, typ, "intersection");
}

fn generateSetDifferenceFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.generateSetCombineFunction(name_id, typ, "difference");
}

fn generateSetCombineFunction(self: *Self, name_id: u32, typ: Type, method: []const u8) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeSetLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        Set.");
        try self.write(method);
        try self.write("(set, ");
        try self.writeLiteral(typ);
        try self.write(")\n    }\n");
    } else {
        try self.write("        set.");
        try self.write(method);
        try self.write("(");
        try self.writeLiteral(typ);
        try self.write(")\n    }\n");
    }
}

fn generateSetMapFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeSetLocalFunctionStart(name_id, typ, typ);
    if (self.reader.boolean()) {
        try self.write("        Set.map(set, |value| value)\n    }\n");
    } else {
        try self.write("        set.map(|value| value)\n    }\n");
    }
}

fn generateSetFromListBuiltinFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Set.from_list", &.{.{ .literal = typ.setList() }});
    try self.write("\n");
}

fn generateNumeralIsNegativeBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Numeral.is_negative", &.{.{ .numeral_literal = {} }});
    try self.write("\n");
}

fn generateUtf8ProblemIsEqBuiltinFunction(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", .bool);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeCall("Str.Utf8Problem.is_eq", &.{ .{ .raw = "InvalidStartByte" }, .{ .raw = "UnexpectedEndOfSequence" } });
    try self.write("\n");
}

fn writeSetLocalFunctionStart(self: *Self, name_id: u32, typ: Type, return_type: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isSet());

    try self.writeTypedLocalFunctionStart(name_id, "set", typ, return_type);
}

fn writeFunctionSignature(self: *Self, name_id: u32, arg_type: []const u8, return_type: Type) std.mem.Allocator.Error!void {
    try self.write("    ");
    try self.writeFunctionName(name_id);
    try self.write(" : ");
    try self.write(arg_type);
    try self.write(" -> ");
    try self.writeType(return_type);
    try self.write("\n");
}

fn writeRawFunctionSignature(self: *Self, name_id: u32, arg_type: []const u8, return_type: []const u8) std.mem.Allocator.Error!void {
    try self.write("    ");
    try self.writeFunctionName(name_id);
    try self.write(" : ");
    try self.write(arg_type);
    try self.write(" -> ");
    try self.write(return_type);
    try self.write("\n");
}

fn writeRawEffectfulFunctionSignature(self: *Self, name_id: u32, arg_type: []const u8, return_type: []const u8) std.mem.Allocator.Error!void {
    try self.write("    ");
    try self.writeFunctionName(name_id);
    try self.write(" : ");
    try self.write(arg_type);
    try self.write(" => ");
    try self.write(return_type);
    try self.write("\n");
}

fn writeFunctionName(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.write("value");
    try self.output.print(self.allocator, "{d}", .{name_id});
}

fn writeNumberIterFunctionSignature(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.write("    ");
    try self.writeFunctionName(name_id);
    try self.write(" : Main -> Iter(");
    try self.write(numberOwner(typ));
    try self.write(")\n");
}

fn writeFunctionHeader(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.write("    ");
    try self.writeFunctionName(name_id);
    try self.write(" = ");
}

fn writeListTrySignature(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isList());

    try self.writeTrySignature(name_id, typ);
}

fn writeNumericTrySignature(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    std.debug.assert(typ.isNumber());

    try self.writeTrySignature(name_id, typ);
}

fn writeTrySignature(self: *Self, name_id: u32, ok_type: Type) std.mem.Allocator.Error!void {
    try self.write("    ");
    try self.writeFunctionName(name_id);
    try self.write(" : Main -> Try(");
    try self.writeType(ok_type);
    try self.write(", _)\n");
}

fn writeFmtEncodeFunctionStart(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeRawFunctionSignature(name_id, "Main", "Try(Str, Str)");
    try self.writeFmtLocalFunctionHeader(name_id);
}

fn writeFmtDecodeFunctionStart(self: *Self, name_id: u32, decoded_type: Type) std.mem.Allocator.Error!void {
    try self.write("    ");
    try self.writeFunctionName(name_id);
    try self.write(" : Main -> (Try(");
    try self.writeType(decoded_type);
    try self.write(", Str), Str)\n");
    try self.writeFmtLocalFunctionHeader(name_id);
}

fn writeFmtLocalFunctionHeader(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        fmt : Fmt\n        fmt = Fmt\n");
}

fn chooseType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 78)) {
        0 => .main,
        1 => .support,
        2 => .tree,
        3 => .my_num,
        4 => .bool,
        5 => .str,
        6 => .record_bool,
        7 => .record_str_u64,
        8 => .holder_bool,
        9 => .holder_str,
        10 => .holder_u64,
        11 => .holder_record_bool,
        12 => .holder_list_u64,
        13 => .holder_try_u64_str,
        14 => .builder_bool,
        15 => .builder_str,
        16 => .builder_u64,
        17 => .builder_record_str_u64,
        18 => .builder_list_u64,
        19 => .builder_try_u64_str,
        20 => .wrap_bool,
        21 => .wrap_str,
        22 => .wrap_u64,
        23 => .wrap_record_str_u64,
        24 => .wrap_list_u64,
        25 => .wrap_try_u64_str,
        26 => .secret_bool,
        27 => .secret_str,
        28 => .secret_u64,
        29 => .secret_record_str_u64,
        30 => .secret_list_u64,
        31 => .secret_try_u64_str,
        32 => .box_bool,
        33 => .box_str,
        34 => .box_u64,
        35 => .box_record_str_u64,
        36 => .box_list_u64,
        37 => .box_try_u64_str,
        38 => .dict_str_u64,
        39 => .dict_u64_str,
        40 => .set_str,
        41 => .set_u64,
        42 => .fn_bool_bool,
        43 => .fn_u64_u64,
        44 => .fn_str_str,
        45 => .tuple_bool_u64,
        46 => .tuple_str_u64,
        47 => .tuple_u64_str,
        48 => .tuple_record_bool_u64,
        49 => .tuple_try_u64_str_bool,
        50 => .tuple_list_u64_str,
        51 => .try_bool_str,
        52 => .try_str_bool,
        53 => .try_u64_str,
        54 => .try_list_u64_str,
        55 => .list_bool,
        56 => .list_str,
        57 => .list_u64,
        58 => .list_record_bool,
        59 => .list_list_u64,
        60 => .list_tuple_bool_u64,
        61 => .list_tuple_str_u64,
        62 => .list_tuple_u64_str,
        63 => .list_try_u64_str,
        64 => .list_holder_u64,
        65 => .u8,
        66 => .i8,
        67 => .u16,
        68 => .i16,
        69 => .u32,
        70 => .i32,
        71 => .u64,
        72 => .i64,
        73 => .u128,
        74 => .i128,
        75 => .f32,
        76 => .f64,
        77 => .record_str_u64,
        78 => .dec,
        else => unreachable,
    };
}

fn chooseListType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 7)) {
        0 => .list_bool,
        1 => .list_str,
        2 => .list_u64,
        3 => .list_record_bool,
        4 => .list_list_u64,
        5 => .list_tuple_bool_u64,
        6 => .list_try_u64_str,
        7 => .list_holder_u64,
        else => unreachable,
    };
}

fn chooseEquatableListType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => .list_bool,
        1 => .list_str,
        2 => .list_u64,
        else => unreachable,
    };
}

fn chooseNumberType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 12)) {
        0 => .u8,
        1 => .i8,
        2 => .u16,
        3 => .i16,
        4 => .u32,
        5 => .i32,
        6 => .u64,
        7 => .i64,
        8 => .u128,
        9 => .i128,
        10 => .dec,
        11 => .f32,
        12 => .f64,
        else => unreachable,
    };
}

fn chooseSignedOrFloatNumberType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 7)) {
        0 => .i8,
        1 => .i16,
        2 => .i32,
        3 => .i64,
        4 => .i128,
        5 => .dec,
        6 => .f32,
        7 => .f64,
        else => unreachable,
    };
}

fn chooseIntegerType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 9)) {
        0 => .u8,
        1 => .i8,
        2 => .u16,
        3 => .i16,
        4 => .u32,
        5 => .i32,
        6 => .u64,
        7 => .i64,
        8 => .u128,
        9 => .i128,
        else => unreachable,
    };
}

fn chooseFloatType(self: *Self) Type {
    return if (self.reader.boolean()) .f32 else .f64;
}

fn chooseTryType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => .try_bool_str,
        1 => .try_str_bool,
        2 => .try_u64_str,
        3 => .try_list_u64_str,
        else => unreachable,
    };
}

fn tryTypeName(typ: Type) []const u8 {
    return switch (typ) {
        .try_bool_str => "Try(Bool, Str)",
        .try_str_bool => "Try(Str, Bool)",
        .try_u64_str => "Try(U64, Str)",
        .try_list_u64_str => "Try(List(U64), Str)",
        else => unreachable,
    };
}

fn chooseBoxType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 5)) {
        0 => .box_bool,
        1 => .box_str,
        2 => .box_u64,
        3 => .box_record_str_u64,
        4 => .box_list_u64,
        5 => .box_try_u64_str,
        else => unreachable,
    };
}

fn chooseDictType(self: *Self) Type {
    return if (self.reader.boolean()) .dict_str_u64 else .dict_u64_str;
}

fn chooseSetType(self: *Self) Type {
    return if (self.reader.boolean()) .set_str else .set_u64;
}

fn numberOwner(typ: Type) []const u8 {
    return switch (typ) {
        .u8 => "U8",
        .i8 => "I8",
        .u16 => "U16",
        .i16 => "I16",
        .u32 => "U32",
        .i32 => "I32",
        .u64 => "U64",
        .i64 => "I64",
        .u128 => "U128",
        .i128 => "I128",
        .dec => "Dec",
        .f32 => "F32",
        .f64 => "F64",
        else => unreachable,
    };
}

fn typeFromNumberOwner(owner: []const u8) ?Type {
    const typ = typeFromBuiltinName(owner) orelse return null;
    return if (typ.isNumber()) typ else null;
}

fn typeFromBuiltinName(name: []const u8) ?Type {
    if (std.mem.eql(u8, name, "U8")) return .u8;
    if (std.mem.eql(u8, name, "I8")) return .i8;
    if (std.mem.eql(u8, name, "U16")) return .u16;
    if (std.mem.eql(u8, name, "I16")) return .i16;
    if (std.mem.eql(u8, name, "U32")) return .u32;
    if (std.mem.eql(u8, name, "I32")) return .i32;
    if (std.mem.eql(u8, name, "U64")) return .u64;
    if (std.mem.eql(u8, name, "I64")) return .i64;
    if (std.mem.eql(u8, name, "U128")) return .u128;
    if (std.mem.eql(u8, name, "I128")) return .i128;
    if (std.mem.eql(u8, name, "Dec")) return .dec;
    if (std.mem.eql(u8, name, "F32")) return .f32;
    if (std.mem.eql(u8, name, "F64")) return .f64;
    return null;
}

fn trimLeft(bytes: []const u8) []const u8 {
    var index: usize = 0;
    while (index < bytes.len and (bytes[index] == ' ' or bytes[index] == '\t')) : (index += 1) {}
    return bytes[index..];
}

fn builtinIdentifierEnd(bytes: []const u8, start: usize) usize {
    var index = start;
    while (index < bytes.len and isBuiltinIdentifierChar(bytes[index])) : (index += 1) {}
    return index;
}

fn isBuiltinIdentifierChar(byte: u8) bool {
    return (byte >= 'a' and byte <= 'z') or
        (byte >= 'A' and byte <= 'Z') or
        std.ascii.isDigit(byte) or
        byte == '_';
}

fn absDiffReturnType(typ: Type) Type {
    return switch (typ) {
        .u8, .i8 => .u8,
        .u16, .i16 => .u16,
        .u32, .i32 => .u32,
        .u64, .i64 => .u64,
        .u128, .i128 => .u128,
        .dec => .dec,
        .f32 => .f32,
        .f64 => .f64,
        else => unreachable,
    };
}

fn writeType(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        switch (typ) {
            .bool => return self.write("AliasBool"),
            .record_str_u64 => return self.write("NameCount"),
            .list_u64 => return self.write("U64s"),
            .tuple_bool_u64, .tuple_str_u64, .tuple_u64_str, .tuple_record_bool_u64, .tuple_try_u64_str_bool, .tuple_list_u64_str => {
                try self.write("Pair(");
                try self.writeType(tupleFieldType(typ, true));
                try self.write(", ");
                try self.writeType(tupleFieldType(typ, false));
                return self.write(")");
            },
            .try_bool_str, .try_str_bool, .try_u64_str, .try_list_u64_str => {
                try self.write("Fallible(");
                try self.writeType(typ.tryOk());
                try self.write(", ");
                try self.writeType(typ.tryErr());
                return self.write(")");
            },
            else => {},
        }
    }

    try self.write(switch (typ) {
        .main => "Main",
        .support => "Support",
        .tree => "Tree",
        .my_num => "MyNum",
        .bool => "Bool",
        .str => "Str",
        .record_bool => "{ flag : Bool }",
        .record_str_u64 => "{ name : Str, count : U64 }",
        .holder_bool => "Holder(Bool)",
        .holder_str => "Holder(Str)",
        .holder_u64 => "Holder(U64)",
        .holder_record_bool => "Holder({ flag : Bool })",
        .holder_list_u64 => "Holder(List(U64))",
        .holder_try_u64_str => "Holder(Try(U64, Str))",
        .builder_bool => "Builder(Bool)",
        .builder_str => "Builder(Str)",
        .builder_u64 => "Builder(U64)",
        .builder_record_str_u64 => "Builder({ name : Str, count : U64 })",
        .builder_list_u64 => "Builder(List(U64))",
        .builder_try_u64_str => "Builder(Try(U64, Str))",
        .wrap_bool => "Wrap(Bool)",
        .wrap_str => "Wrap(Str)",
        .wrap_u64 => "Wrap(U64)",
        .wrap_record_str_u64 => "Wrap({ name : Str, count : U64 })",
        .wrap_list_u64 => "Wrap(List(U64))",
        .wrap_try_u64_str => "Wrap(Try(U64, Str))",
        .secret_bool => "Secret(Bool)",
        .secret_str => "Secret(Str)",
        .secret_u64 => "Secret(U64)",
        .secret_record_str_u64 => "Secret({ name : Str, count : U64 })",
        .secret_list_u64 => "Secret(List(U64))",
        .secret_try_u64_str => "Secret(Try(U64, Str))",
        .box_bool => "Box(Bool)",
        .box_str => "Box(Str)",
        .box_u64 => "Box(U64)",
        .box_record_str_u64 => "Box({ name : Str, count : U64 })",
        .box_list_u64 => "Box(List(U64))",
        .box_try_u64_str => "Box(Try(U64, Str))",
        .dict_str_u64 => "Dict(Str, U64)",
        .dict_u64_str => "Dict(U64, Str)",
        .set_str => "Set(Str)",
        .set_u64 => "Set(U64)",
        .fn_bool_bool => "(Bool -> Bool)",
        .fn_u64_u64 => "(U64 -> U64)",
        .fn_str_str => "(Str -> Str)",
        .tuple_bool_u64 => "(Bool, U64)",
        .tuple_str_u64 => "(Str, U64)",
        .tuple_u64_str => "(U64, Str)",
        .tuple_record_bool_u64 => "({ flag : Bool }, U64)",
        .tuple_try_u64_str_bool => "(Try(U64, Str), Bool)",
        .tuple_list_u64_str => "(List(U64), Str)",
        .try_bool_str => "Try(Bool, Str)",
        .try_str_bool => "Try(Str, Bool)",
        .try_u64_str => "Try(U64, Str)",
        .try_list_u64_str => "Try(List(U64), Str)",
        .list_bool => "List(Bool)",
        .list_str => "List(Str)",
        .list_u64 => "List(U64)",
        .list_record_bool => "List({ flag : Bool })",
        .list_list_u64 => "List(List(U64))",
        .list_tuple_bool_u64 => "List((Bool, U64))",
        .list_tuple_str_u64 => "List((Str, U64))",
        .list_tuple_u64_str => "List((U64, Str))",
        .list_try_u64_str => "List(Try(U64, Str))",
        .list_holder_u64 => "List(Holder(U64))",
        .u8 => "U8",
        .i8 => "I8",
        .u16 => "U16",
        .i16 => "I16",
        .u32 => "U32",
        .i32 => "I32",
        .u64 => "U64",
        .i64 => "I64",
        .u128 => "U128",
        .i128 => "I128",
        .dec => "Dec",
        .f32 => "F32",
        .f64 => "F64",
    });
}

fn writeLiteral(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .main => try self.writeMainLiteral(),
        .support => try self.writeSupportLiteral(),
        .tree => try self.writeTreeLiteral(),
        .my_num => try self.writeCustomNumberLiteral(),
        .bool => try self.writeBoolLiteral(),
        .str => try self.writeStringLiteral(),
        .record_bool => try self.writeRecordBoolLiteral(),
        .record_str_u64 => try self.writeRecordStrU64Literal(),
        .holder_bool => try self.writeHolderLiteral(.bool),
        .holder_str => try self.writeHolderLiteral(.str),
        .holder_u64 => try self.writeHolderLiteral(.u64),
        .holder_record_bool => try self.writeHolderLiteral(.record_bool),
        .holder_list_u64 => try self.writeHolderLiteral(.list_u64),
        .holder_try_u64_str => try self.writeHolderLiteral(.try_u64_str),
        .builder_bool => try self.writeBuilderLiteral(.bool),
        .builder_str => try self.writeBuilderLiteral(.str),
        .builder_u64 => try self.writeBuilderLiteral(.u64),
        .builder_record_str_u64 => try self.writeBuilderLiteral(.record_str_u64),
        .builder_list_u64 => try self.writeBuilderLiteral(.list_u64),
        .builder_try_u64_str => try self.writeBuilderLiteral(.try_u64_str),
        .wrap_bool => try self.writeWrapLiteral(.bool),
        .wrap_str => try self.writeWrapLiteral(.str),
        .wrap_u64 => try self.writeWrapLiteral(.u64),
        .wrap_record_str_u64 => try self.writeWrapLiteral(.record_str_u64),
        .wrap_list_u64 => try self.writeWrapLiteral(.list_u64),
        .wrap_try_u64_str => try self.writeWrapLiteral(.try_u64_str),
        .secret_bool => try self.writeSecretLiteral(.bool),
        .secret_str => try self.writeSecretLiteral(.str),
        .secret_u64 => try self.writeSecretLiteral(.u64),
        .secret_record_str_u64 => try self.writeSecretLiteral(.record_str_u64),
        .secret_list_u64 => try self.writeSecretLiteral(.list_u64),
        .secret_try_u64_str => try self.writeSecretLiteral(.try_u64_str),
        .box_bool => try self.writeBoxLiteral(.bool),
        .box_str => try self.writeBoxLiteral(.str),
        .box_u64 => try self.writeBoxLiteral(.u64),
        .box_record_str_u64 => try self.writeBoxLiteral(.record_str_u64),
        .box_list_u64 => try self.writeBoxLiteral(.list_u64),
        .box_try_u64_str => try self.writeBoxLiteral(.try_u64_str),
        .dict_str_u64 => try self.writeDictLiteral(.str, .u64),
        .dict_u64_str => try self.writeDictLiteral(.u64, .str),
        .set_str => try self.writeSetLiteral(.str),
        .set_u64 => try self.writeSetLiteral(.u64),
        .fn_bool_bool => try self.writeBoolFunctionLiteral(),
        .fn_u64_u64 => try self.writeU64FunctionLiteral(),
        .fn_str_str => try self.writeStrFunctionLiteral(),
        .tuple_bool_u64 => try self.writeTupleLiteral(.bool, .u64),
        .tuple_str_u64 => try self.writeTupleLiteral(.str, .u64),
        .tuple_u64_str => try self.writeTupleLiteral(.u64, .str),
        .tuple_record_bool_u64 => try self.writeTupleLiteral(.record_bool, .u64),
        .tuple_try_u64_str_bool => try self.writeTupleLiteral(.try_u64_str, .bool),
        .tuple_list_u64_str => try self.writeTupleLiteral(.list_u64, .str),
        .try_bool_str => try self.writeTryLiteral(.bool, .str),
        .try_str_bool => try self.writeTryLiteral(.str, .bool),
        .try_u64_str => try self.writeTryLiteral(.u64, .str),
        .try_list_u64_str => try self.writeTryLiteral(.list_u64, .str),
        .list_bool => try self.writeListLiteral(.bool),
        .list_str => try self.writeListLiteral(.str),
        .list_u64 => try self.writeListLiteral(.u64),
        .list_record_bool => try self.writeListLiteral(.record_bool),
        .list_list_u64 => try self.writeListLiteral(.list_u64),
        .list_tuple_bool_u64 => try self.writeListLiteral(.tuple_bool_u64),
        .list_tuple_str_u64 => try self.writeListLiteral(.tuple_str_u64),
        .list_tuple_u64_str => try self.writeListLiteral(.tuple_u64_str),
        .list_try_u64_str => try self.writeListLiteral(.try_u64_str),
        .list_holder_u64 => try self.writeListLiteral(.holder_u64),
        .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => {
            try self.writeIntegerLiteral();
        },
        .dec, .f32, .f64 => {
            const whole = self.reader.intRangeAtMost(u8, 0, 100);
            const frac = self.reader.intRangeAtMost(u8, 0, 99);
            try self.output.print(self.allocator, "{d}.{d}", .{ whole, frac });
        },
    }
}

fn writeBoolPayloadBranch(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .bool => try self.write("flag"),
        .record_bool => try self.write("{ flag: flag }"),
        else => {
            try self.write("if flag ");
            try self.writeLiteral(typ);
            try self.write(" else ");
            try self.writeLiteral(typ);
        },
    }
}

fn writeStrU64PayloadBranch(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .str => try self.write("name"),
        .u64 => try self.write("count"),
        .record_str_u64 => try self.write("{ name: name, count: count }"),
        .holder_str => try self.write("Holder.make(name)"),
        .holder_u64 => try self.write("Holder.make(count)"),
        .builder_str => try self.write("Builder.field(name)"),
        .builder_u64 => try self.write("Builder.field(count)"),
        .wrap_str => try self.write("Wrap.make(name)"),
        .wrap_u64 => try self.write("Wrap.make(count)"),
        .secret_str => try self.write("Secret.make(name)"),
        .secret_u64 => try self.write("Secret.make(count)"),
        .box_str => try self.write("Box.box(name)"),
        .box_u64 => try self.write("Box.box(count)"),
        else => try self.writeLiteral(typ),
    }
}

fn writeListMapBody(self: *Self, elem_type: Type) std.mem.Allocator.Error!void {
    switch (elem_type) {
        .bool => {
            if (self.reader.boolean()) {
                try self.write("value");
            } else {
                try self.write("if value ");
                try self.writeBoolLiteral();
                try self.write(" else ");
                try self.writeBoolLiteral();
            }
        },
        .str => {
            if (self.reader.boolean()) {
                try self.write("value");
            } else {
                try self.write("Str.concat(value, ");
                try self.writeStringLiteral();
                try self.write(")");
            }
        },
        .u64 => {
            if (self.reader.boolean()) {
                try self.write("value");
            } else {
                try self.write("value + ");
                try self.writeSmallNumberLiteral(.u64);
            }
        },
        .record_bool => {
            if (self.reader.boolean()) {
                try self.write("value");
            } else {
                try self.write("{ flag: value.flag }");
            }
        },
        else => {
            if (self.reader.boolean()) {
                try self.write("value");
            } else {
                try self.writeLiteral(elem_type);
            }
        },
    }
}

fn writeListMap2Body(self: *Self, elem_type: Type) std.mem.Allocator.Error!void {
    switch (elem_type) {
        .bool => {
            if (self.reader.boolean()) {
                try self.write("left == right");
            } else {
                try self.write("if left right else left");
            }
        },
        .str => {
            if (self.reader.boolean()) {
                try self.write("left");
            } else {
                try self.write("Str.concat(left, right)");
            }
        },
        .u64 => {
            if (self.reader.boolean()) {
                try self.write("left");
            } else {
                try self.write("left + right");
            }
        },
        .record_bool => {
            if (self.reader.boolean()) {
                try self.write("left");
            } else {
                try self.write("{ flag: left.flag == right.flag }");
            }
        },
        else => {
            if (self.reader.boolean()) {
                try self.write("left");
            } else {
                try self.writeLiteral(elem_type);
            }
        },
    }
}

fn writeListLiteral(self: *Self, elem_type: Type) std.mem.Allocator.Error!void {
    try self.writeListLiteralRange(elem_type, 0, 4);
}

fn writeNonEmptyListLiteral(self: *Self, elem_type: Type) std.mem.Allocator.Error!void {
    try self.writeListLiteralRange(elem_type, 1, 4);
}

fn writeListLiteralRange(self: *Self, elem_type: Type, min_len: u8, max_len: u8) std.mem.Allocator.Error!void {
    try self.write("[");
    const len = self.reader.intRangeAtMost(u8, min_len, max_len);
    for (0..len) |i| {
        if (i > 0) try self.write(", ");
        try self.writeLiteral(elem_type);
    }
    try self.write("]");
}

fn writeRecordBoolLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.writeRecordLiteral(&.{.{ .name = "flag", .value = .{ .bool_literal = {} } }});
}

fn writeMainLiteral(self: *Self) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => try self.write("Main"),
        1 => try self.writeCall("WithBool", &.{.{ .bool_literal = {} }}),
        2 => try self.writeCall("WithStrU64", &.{ .{ .string_literal = {} }, .{ .integer_literal = {} } }),
        else => unreachable,
    }
}

fn writeSupportLiteral(self: *Self) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 2)) {
        0 => try self.writeCall("Support.make", &.{.{ .integer_literal = {} }}),
        1 => try self.writeCall("Support.from_list", &.{.{ .literal = .list_u64 }}),
        2 => try self.writeCall("Support.from_pair", &.{.{ .literal = .tuple_str_u64 }}),
        else => unreachable,
    }
}

fn writeToolsLiteral(self: *Self) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 1)) {
        0 => try self.writeCall("Tools.make", &.{.{ .integer_literal = {} }}),
        1 => try self.writeCall("Tools.from_support", &.{.{ .literal = .support }}),
        else => unreachable,
    }
}

fn writeSupportListLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write("[");
    const len = self.reader.intRangeAtMost(u8, 1, 4);
    for (0..len) |i| {
        if (i > 0) try self.write(", ");
        try self.writeSupportLiteral();
    }
    try self.write("]");
}

fn writeOpenColorLiteral(self: *Self) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.write("Red"),
        1 => try self.write("Green"),
        2 => try self.write("Blue"),
        3 => try self.writeCall("Other", &.{.{ .string_literal = {} }}),
        else => unreachable,
    }
}

fn writeOpenColorMatch(self: *Self, expr: []const u8) std.mem.Allocator.Error!void {
    try self.write("match ");
    try self.write(expr);
    try self.write(" {\n");
    try self.write("            Red => \"red\"\n");
    try self.write("            Green => \"green\"\n");
    try self.write("            Blue => \"blue\"\n");
    try self.write("            Other(name) => name\n");
    try self.write("            _ => \"other\"\n");
    try self.write("        }");
}

fn writeTreeLiteral(self: *Self) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.writeCall("Leaf", &.{.{ .string_literal = {} }}),
        1 => try self.writeCall("Node", &.{.{ .raw = "[]" }}),
        2 => {
            try self.write("Node(");
            try self.write("[");
            try self.writeCall("Leaf", &.{.{ .string_literal = {} }});
            try self.write("])");
        },
        3 => try self.writeCall("Node", &.{.{ .raw = "[Node([])]" }}),
        else => unreachable,
    }
}

fn writeCustomNumberLiteral(self: *Self) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 1)) {
        0 => try self.writeTypedCustomNumberLiteral(),
        1 => try self.writeCall("MyNum.MyNum", &.{.{ .integer_literal = {} }}),
        else => unreachable,
    }
}

fn writeTypedCustomNumberLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.writeIntegerLiteral();
    try self.write(".MyNum");
}

fn writeRecordStrU64Literal(self: *Self) std.mem.Allocator.Error!void {
    try self.writeRecordLiteral(&.{
        .{ .name = "name", .value = .{ .string_literal = {} } },
        .{ .name = "count", .value = .{ .integer_literal = {} } },
    });
}

fn writeWideRecordLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.writeRecordLiteral(&.{
        .{ .name = "name", .value = .{ .string_literal = {} } },
        .{ .name = "count", .value = .{ .integer_literal = {} } },
        .{ .name = "flag", .value = .{ .bool_literal = {} } },
    });
}

fn writeHolderLiteral(self: *Self, inner: Type) std.mem.Allocator.Error!void {
    try self.writeCall("Holder.make", &.{.{ .literal = inner }});
}

fn writeBuilderLiteral(self: *Self, inner: Type) std.mem.Allocator.Error!void {
    try self.writeCall("Builder.field", &.{.{ .literal = inner }});
}

fn writeWrapLiteral(self: *Self, inner: Type) std.mem.Allocator.Error!void {
    try self.writeCall("Wrap.make", &.{.{ .literal = inner }});
}

fn writeSecretLiteral(self: *Self, inner: Type) std.mem.Allocator.Error!void {
    try self.writeCall("Secret.make", &.{.{ .literal = inner }});
}

fn writeBoxLiteral(self: *Self, inner: Type) std.mem.Allocator.Error!void {
    try self.writeCall("Box.box", &.{.{ .literal = inner }});
}

fn writeDictLiteral(self: *Self, key: Type, value: Type) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.writeCall("Dict.single", &.{ .{ .literal = key }, .{ .literal = value } }),
        1 => {
            try self.writeMethodCall(.{ .raw = "Dict.empty()" }, "insert", &.{ .{ .literal = key }, .{ .literal = value } });
        },
        2 => {
            try self.writeCallThenMethod("Dict.single", &.{ .{ .literal = key }, .{ .literal = value } }, "insert", &.{ .{ .literal = key }, .{ .literal = value } });
        },
        3 => {
            try self.writeCall("Dict.from_list", &.{.{ .literal = if (key == .str) .list_tuple_str_u64 else .list_tuple_u64_str }});
        },
        else => unreachable,
    }
}

fn writeSetLiteral(self: *Self, elem: Type) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.writeCall("Set.single", &.{.{ .literal = elem }}),
        1 => {
            try self.writeMethodCall(.{ .raw = "Set.empty()" }, "insert", &.{.{ .literal = elem }});
        },
        2 => {
            try self.writeCallThenMethod("Set.single", &.{.{ .literal = elem }}, "insert", &.{.{ .literal = elem }});
        },
        3 => {
            try self.writeCall("Set.from_list", &.{.{ .literal = if (elem == .str) .list_str else .list_u64 }});
        },
        else => unreachable,
    }
}

fn writeTupleLiteral(self: *Self, first: Type, second: Type) std.mem.Allocator.Error!void {
    try self.writeTuple(&.{ .{ .literal = first }, .{ .literal = second } });
}

fn writeTryLiteral(self: *Self, ok_type: Type, err_type: Type) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.writeCall("Ok", &.{.{ .literal = ok_type }});
    } else {
        try self.writeCall("Err", &.{.{ .literal = err_type }});
    }
}

fn writeBoolFunctionLiteral(self: *Self) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.write("|value| value");
    } else {
        try self.write("|value| if value ");
        try self.writeBoolLiteral();
        try self.write(" else ");
        try self.writeBoolLiteral();
    }
}

fn writeU64FunctionLiteral(self: *Self) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.write("|value| value");
    } else {
        try self.write("|value| if ");
        try self.writeBoolLiteral();
        try self.write(" ");
        try self.writeIntegerLiteral();
        try self.write(" else value");
    }
}

fn writeStrFunctionLiteral(self: *Self) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.write("|value| value");
    } else {
        try self.write("|value| if ");
        try self.writeBoolLiteral();
        try self.write(" ");
        try self.writeStringLiteral();
        try self.write(" else value");
    }
}

fn writeBoolLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write(if (self.reader.boolean()) "True" else "False");
}

fn writeIntegerLiteral(self: *Self) std.mem.Allocator.Error!void {
    const value = self.reader.intRangeAtMost(u8, 0, 100);
    try self.output.print(self.allocator, "{d}", .{value});
}

fn writeSmallNumberLiteral(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    if (typ.isFloat() or typ == .dec) {
        const whole = self.reader.intRangeAtMost(u8, 0, 10);
        const frac = self.reader.intRangeAtMost(u8, 0, 9);
        try self.output.print(self.allocator, "{d}.{d}", .{ whole, frac });
    } else {
        const value = self.reader.intRangeAtMost(u8, 0, 10);
        try self.output.print(self.allocator, "{d}", .{value});
    }
}

fn writeStringLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write("\"");
    const len = self.reader.intRangeAtMost(u8, 0, 8);
    for (0..len) |_| {
        const char = self.reader.intRangeAtMost(u8, 'a', 'z');
        try self.output.append(self.allocator, char);
    }
    try self.write("\"");
}

fn writeUtf8BytesLiteral(self: *Self) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.writeList(&.{}),
        1 => try self.writeList(&.{ .{ .raw = "82" }, .{ .raw = "111" }, .{ .raw = "99" } }),
        2 => try self.writeList(&.{ .{ .raw = "240" }, .{ .raw = "159" }, .{ .raw = "144" }, .{ .raw = "166" } }),
        3 => try self.writeList(&.{.{ .raw = "255" }}),
        else => unreachable,
    }
}

fn writeNumeralLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write("Literal(");
    try self.writeRecordLiteral(&.{
        .{ .name = "is_negative", .value = .{ .bool_literal = {} } },
        .{ .name = "digits_before_pt", .value = .{ .utf8_bytes_literal = {} } },
        .{ .name = "digits_after_pt", .value = .{ .utf8_bytes_literal = {} } },
        .{ .name = "digits_after_pt_count", .value = .{ .integer_literal = {} } },
    });
    try self.write(")");
}

fn writeMutableLocalDeclaration(self: *Self, name: []const u8, initializer: Expr) std.mem.Allocator.Error!void {
    try self.write("        var $");
    try self.write(name);
    try self.write(" = ");
    try self.writeExpr(initializer);
    try self.write("\n");
}

fn writeMutableLocalAssignmentStart(self: *Self, name: []const u8) std.mem.Allocator.Error!void {
    try self.write("            $");
    try self.write(name);
    try self.write(" = ");
}

fn writeMutableLocalReference(self: *Self, name: []const u8) std.mem.Allocator.Error!void {
    try self.write("$");
    try self.write(name);
}

fn writeForInPrefix(self: *Self, item_name: []const u8) std.mem.Allocator.Error!void {
    try self.write("        for ");
    try self.write(item_name);
    try self.write(" in ");
}

fn writeForInBodyStart(self: *Self) std.mem.Allocator.Error!void {
    try self.write(" {\n");
}

fn writeForInBodyEnd(self: *Self) std.mem.Allocator.Error!void {
    try self.write("        }\n");
}

fn writeExpr(self: *Self, expr: Expr) std.mem.Allocator.Error!void {
    switch (expr) {
        .raw => |text| try self.write(text),
        .literal => |typ| try self.writeLiteral(typ),
        .type_name => |typ| try self.writeType(typ),
        .bool_literal => try self.writeBoolLiteral(),
        .string_literal => try self.writeStringLiteral(),
        .utf8_bytes_literal => try self.writeUtf8BytesLiteral(),
        .numeral_literal => try self.writeNumeralLiteral(),
        .integer_literal => try self.writeIntegerLiteral(),
        .small_number_literal => |typ| try self.writeSmallNumberLiteral(typ),
    }
}

fn writeCall(self: *Self, callee: []const u8, args: []const Expr) std.mem.Allocator.Error!void {
    try self.write(callee);
    try self.write("(");
    try self.writeArgs(args);
    try self.write(")");
}

fn writeGeneratedFunctionCall(self: *Self, name_id: u32, args: []const Expr) std.mem.Allocator.Error!void {
    try self.write("Main.");
    try self.writeFunctionName(name_id);
    try self.write("(");
    try self.writeArgs(args);
    try self.write(")");
}

fn writeQualifiedCall(self: *Self, owner: []const u8, function: []const u8, args: []const Expr) std.mem.Allocator.Error!void {
    try self.write(owner);
    try self.write(".");
    try self.writeCall(function, args);
}

fn writeMethodCall(self: *Self, receiver: Expr, method: []const u8, args: []const Expr) std.mem.Allocator.Error!void {
    try self.writeExpr(receiver);
    try self.write(".");
    try self.write(method);
    try self.write("(");
    try self.writeArgs(args);
    try self.write(")");
}

fn writeAssociatedOrMethodCall(self: *Self, owner: []const u8, receiver: Expr, method: []const u8, args: []const Expr) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.write(owner);
        try self.write(".");
        try self.write(method);
        try self.write("(");
        try self.writeExpr(receiver);
        if (args.len > 0) {
            try self.write(", ");
            try self.writeArgs(args);
        }
        try self.write(")");
    } else {
        try self.writeMethodCall(receiver, method, args);
    }
}

fn writeCallThenMethod(
    self: *Self,
    callee: []const u8,
    call_args: []const Expr,
    method: []const u8,
    method_args: []const Expr,
) std.mem.Allocator.Error!void {
    try self.writeCall(callee, call_args);
    try self.write(".");
    try self.write(method);
    try self.write("(");
    try self.writeArgs(method_args);
    try self.write(")");
}

fn writeArgs(self: *Self, args: []const Expr) std.mem.Allocator.Error!void {
    for (args, 0..) |arg, index| {
        if (index > 0) try self.write(", ");
        try self.writeExpr(arg);
    }
}

fn writeList(self: *Self, items: []const Expr) std.mem.Allocator.Error!void {
    try self.write("[");
    try self.writeArgs(items);
    try self.write("]");
}

fn writeTuple(self: *Self, items: []const Expr) std.mem.Allocator.Error!void {
    try self.write("(");
    try self.writeArgs(items);
    try self.write(")");
}

fn writeRecordLiteral(self: *Self, fields: []const RecordField) std.mem.Allocator.Error!void {
    try self.write("{ ");
    for (fields, 0..) |field, index| {
        if (index > 0) try self.write(", ");
        try self.write(field.name);
        try self.write(": ");
        try self.writeExpr(field.value);
    }
    try self.write(" }");
}

fn writeTypedLocalFunctionStart(self: *Self, name_id: u32, local_name: []const u8, local_type: Type, return_type: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", return_type);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n        ");
    try self.write(local_name);
    try self.write(" : ");
    try self.writeType(local_type);
    try self.write("\n        ");
    try self.write(local_name);
    try self.write(" = ");
    try self.writeLiteral(local_type);
    try self.write("\n");
}

fn write(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.output.appendSlice(self.allocator, text);
}

fn writeSupport(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.support_output.appendSlice(self.allocator, text);
}

fn writeTools(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.tools_output.appendSlice(self.allocator, text);
}

fn tupleFieldType(typ: Type, first: bool) Type {
    return switch (typ) {
        .tuple_bool_u64 => if (first) .bool else .u64,
        .tuple_str_u64 => if (first) .str else .u64,
        .tuple_u64_str => if (first) .u64 else .str,
        .tuple_record_bool_u64 => if (first) .record_bool else .u64,
        .tuple_try_u64_str_bool => if (first) .try_u64_str else .bool,
        .tuple_list_u64_str => if (first) .list_u64 else .str,
        else => unreachable,
    };
}
