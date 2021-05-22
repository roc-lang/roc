const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;

// List Module
const list = @import("list.zig");

comptime {
    exportListFn(list.listMap, "map");
    exportListFn(list.listMap2, "map2");
    exportListFn(list.listMap3, "map3");
    exportListFn(list.listMapWithIndex, "map_with_index");
    exportListFn(list.listKeepIf, "keep_if");
    exportListFn(list.listWalk, "walk");
    exportListFn(list.listWalkUntil, "walkUntil");
    exportListFn(list.listWalkBackwards, "walk_backwards");
    exportListFn(list.listKeepOks, "keep_oks");
    exportListFn(list.listKeepErrs, "keep_errs");
    exportListFn(list.listContains, "contains");
    exportListFn(list.listRepeat, "repeat");
    exportListFn(list.listAppend, "append");
    exportListFn(list.listSingle, "single");
    exportListFn(list.listJoin, "join");
    exportListFn(list.listRange, "range");
    exportListFn(list.listReverse, "reverse");
    exportListFn(list.listSortWith, "sort_with");
    exportListFn(list.listConcat, "concat");
    exportListFn(list.listDrop, "drop");
}

// Dict Module
const dict = @import("dict.zig");
const hash = @import("hash.zig");

comptime {
    exportDictFn(dict.dictLen, "len");
    exportDictFn(dict.dictEmpty, "empty");
    exportDictFn(dict.dictInsert, "insert");
    exportDictFn(dict.dictRemove, "remove");
    exportDictFn(dict.dictContains, "contains");
    exportDictFn(dict.dictGet, "get");
    exportDictFn(dict.elementsRc, "elementsRc");
    exportDictFn(dict.dictKeys, "keys");
    exportDictFn(dict.dictValues, "values");
    exportDictFn(dict.dictUnion, "union");
    exportDictFn(dict.dictIntersection, "intersection");
    exportDictFn(dict.dictDifference, "difference");
    exportDictFn(dict.dictWalk, "walk");

    exportDictFn(dict.setFromList, "set_from_list");

    exportDictFn(hash.wyhash, "hash");
    exportDictFn(hash.wyhash_rocstr, "hash_str");
}

// Num Module
const num = @import("num.zig");
comptime {
    exportNumFn(num.atan, "atan");
    exportNumFn(num.isFinite, "is_finite");
    exportNumFn(num.powInt, "pow_int");
    exportNumFn(num.acos, "acos");
    exportNumFn(num.asin, "asin");
}

// Str Module
const str = @import("str.zig");
comptime {
    exportStrFn(str.init, "init");
    exportStrFn(str.strSplitInPlaceC, "str_split_in_place");
    exportStrFn(str.countSegments, "count_segments");
    exportStrFn(str.countGraphemeClusters, "count_grapheme_clusters");
    exportStrFn(str.startsWith, "starts_with");
    exportStrFn(str.startsWithCodePoint, "starts_with_code_point");
    exportStrFn(str.endsWith, "ends_with");
    exportStrFn(str.strConcatC, "concat");
    exportStrFn(str.strJoinWithC, "joinWith");
    exportStrFn(str.strNumberOfBytes, "number_of_bytes");
    exportStrFn(str.strFromIntC, "from_int");
    exportStrFn(str.strFromFloatC, "from_float");
    exportStrFn(str.strEqual, "equal");
    exportStrFn(str.strToBytesC, "to_bytes");
    exportStrFn(str.fromUtf8C, "from_utf8");
}

// Export helpers - Must be run inside a comptime
fn exportBuiltinFn(comptime func: anytype, comptime func_name: []const u8) void {
    @export(func, .{ .name = "roc_builtins." ++ func_name, .linkage = .Strong });
}
fn exportNumFn(comptime func: anytype, comptime func_name: []const u8) void {
    exportBuiltinFn(func, "num." ++ func_name);
}
fn exportStrFn(comptime func: anytype, comptime func_name: []const u8) void {
    exportBuiltinFn(func, "str." ++ func_name);
}
fn exportDictFn(comptime func: anytype, comptime func_name: []const u8) void {
    exportBuiltinFn(func, "dict." ++ func_name);
}

fn exportListFn(comptime func: anytype, comptime func_name: []const u8) void {
    exportBuiltinFn(func, "list." ++ func_name);
}

// Run all tests in imported modules
// https://github.com/ziglang/zig/blob/master/lib/std/std.zig#L94
test "" {
    testing.refAllDecls(@This());
}
