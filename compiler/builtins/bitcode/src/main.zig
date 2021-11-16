const std = @import("std");
const math = std.math;
const utils = @import("utils.zig");

const ROC_BUILTINS = "roc_builtins";
const NUM = "num";
const STR = "str";

// Dec Module
const dec = @import("dec.zig");

comptime {
    exportDecFn(dec.fromF64C, "from_f64");
    exportDecFn(dec.eqC, "eq");
    exportDecFn(dec.neqC, "neq");
    exportDecFn(dec.negateC, "negate");
    exportDecFn(dec.addC, "add_with_overflow");
    exportDecFn(dec.subC, "sub_with_overflow");
    exportDecFn(dec.mulC, "mul_with_overflow");
    exportDecFn(dec.divC, "div");
}

// List Module
const list = @import("list.zig");

comptime {
    exportListFn(list.listMap, "map");
    exportListFn(list.listMap2, "map2");
    exportListFn(list.listMap3, "map3");
    exportListFn(list.listMap4, "map4");
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
    exportListFn(list.listPrepend, "prepend");
    exportListFn(list.listSingle, "single");
    exportListFn(list.listJoin, "join");
    exportListFn(list.listRange, "range");
    exportListFn(list.listReverse, "reverse");
    exportListFn(list.listSortWith, "sort_with");
    exportListFn(list.listConcat, "concat");
    exportListFn(list.listSublist, "sublist");
    exportListFn(list.listDropAt, "drop_at");
    exportListFn(list.listSet, "set");
    exportListFn(list.listSetInPlace, "set_in_place");
    exportListFn(list.listSwap, "swap");
    exportListFn(list.listAny, "any");
    exportListFn(list.listFindUnsafe, "find_unsafe");
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

const INTEGERS = [_]type{ i8, i16, i32, i64, i128, u8, u16, u32, u64, u128 };
const FLOATS = [_]type{ f32, f64 };
const NUMBERS = INTEGERS ++ FLOATS;

comptime {
    exportNumFn(num.bytesToU16C, "bytes_to_u16");
    exportNumFn(num.bytesToU32C, "bytes_to_u32");

    inline for (INTEGERS) |T| {
        num.exportPow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".pow_int.");
        num.exportDivCeil(T, ROC_BUILTINS ++ "." ++ NUM ++ ".div_ceil.");
    }

    inline for (FLOATS) |T| {
        num.exportAsin(T, ROC_BUILTINS ++ "." ++ NUM ++ ".asin.");
        num.exportAcos(T, ROC_BUILTINS ++ "." ++ NUM ++ ".acos.");
        num.exportAtan(T, ROC_BUILTINS ++ "." ++ NUM ++ ".atan.");

        num.exportIsFinite(T, ROC_BUILTINS ++ "." ++ NUM ++ ".is_finite.");
        num.exportRound(T, ROC_BUILTINS ++ "." ++ NUM ++ ".round.");
    }
}

// Str Module
const str = @import("str.zig");
comptime {
    exportStrFn(str.init, "init");
    exportStrFn(str.strSplitInPlaceC, "str_split_in_place");
    exportStrFn(str.countSegments, "count_segments");
    exportStrFn(str.countGraphemeClusters, "count_grapheme_clusters");
    exportStrFn(str.startsWith, "starts_with");
    exportStrFn(str.startsWithCodePt, "starts_with_code_point");
    exportStrFn(str.endsWith, "ends_with");
    exportStrFn(str.strConcatC, "concat");
    exportStrFn(str.strJoinWithC, "joinWith");
    exportStrFn(str.strNumberOfBytes, "number_of_bytes");
    exportStrFn(str.strFromFloatC, "from_float");
    exportStrFn(str.strEqual, "equal");
    exportStrFn(str.strToUtf8C, "to_utf8");
    exportStrFn(str.fromUtf8C, "from_utf8");
    exportStrFn(str.fromUtf8RangeC, "from_utf8_range");
    exportStrFn(str.repeat, "repeat");
    exportStrFn(str.strTrim, "trim");
    exportStrFn(str.strTrimLeft, "trim_left");
    exportStrFn(str.strTrimRight, "trim_right");

    inline for (INTEGERS) |T| {
        str.exportFromInt(T, ROC_BUILTINS ++ "." ++ STR ++ ".from_int.");
    }
}

// Utils

comptime {
    exportUtilsFn(utils.test_panic, "test_panic");
    exportUtilsFn(utils.decrefC, "decref");
    exportUtilsFn(utils.decrefCheckNullC, "decref_check_null");

    @export(utils.panic, .{ .name = "roc_builtins.utils." ++ "panic", .linkage = .Weak });
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
fn exportDecFn(comptime func: anytype, comptime func_name: []const u8) void {
    exportBuiltinFn(func, "dec." ++ func_name);
}

fn exportUtilsFn(comptime func: anytype, comptime func_name: []const u8) void {
    exportBuiltinFn(func, "utils." ++ func_name);
}

// Custom panic function, as builtin Zig version errors during LLVM verification
pub fn panic(message: []const u8, stacktrace: ?*std.builtin.StackTrace) noreturn {
    const builtin = @import("builtin");
    if (builtin.is_test) {
        std.debug.print("{s}: {?}", .{ message, stacktrace });
    } else {
        _ = message;
        _ = stacktrace;
    }

    unreachable;
}

// Run all tests in imported modules
// https://github.com/ziglang/zig/blob/master/lib/std/std.zig#L94
test "" {
    const testing = std.testing;

    testing.refAllDecls(@This());
}

// For unclear reasons, sometimes this function is not linked in on some machines.
// Therefore we provide it as LLVM bitcode and mark it as externally linked during our LLVM codegen
//
// Taken from
// https://github.com/ziglang/zig/blob/85755c51d529e7d9b406c6bdf69ce0a0f33f3353/lib/std/special/compiler_rt/muloti4.zig
//
// Thank you Zig Contributors!

// Export it as weak incase it is already linked in by something else.
comptime {
    @export(__muloti4, .{ .name = "__muloti4", .linkage = .Weak });
}
fn __muloti4(a: i128, b: i128, overflow: *c_int) callconv(.C) i128 {
    // @setRuntimeSafety(std.builtin.is_test);

    const min = @bitCast(i128, @as(u128, 1 << (128 - 1)));
    const max = ~min;
    overflow.* = 0;

    const r = a *% b;
    if (a == min) {
        if (b != 0 and b != 1) {
            overflow.* = 1;
        }
        return r;
    }
    if (b == min) {
        if (a != 0 and a != 1) {
            overflow.* = 1;
        }
        return r;
    }

    const sa = a >> (128 - 1);
    const abs_a = (a ^ sa) -% sa;
    const sb = b >> (128 - 1);
    const abs_b = (b ^ sb) -% sb;

    if (abs_a < 2 or abs_b < 2) {
        return r;
    }

    if (sa == sb) {
        if (abs_a > @divTrunc(max, abs_b)) {
            overflow.* = 1;
        }
    } else {
        if (abs_a > @divTrunc(min, -abs_b)) {
            overflow.* = 1;
        }
    }

    return r;
}
