const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const utils = @import("utils.zig");
const expect = @import("expect.zig");
const panic_utils = @import("panic.zig");

comptime {
    _ = @import("compiler_rt.zig");
    _ = @import("libc.zig");
}

const ROC_BUILTINS = "roc_builtins";
const NUM = "num";
const STR = "str";

// Dec Module
const dec = @import("dec.zig");

comptime {
    exportDecFn(dec.absC, "abs");
    exportDecFn(dec.acosC, "acos");
    exportDecFn(dec.addC, "add_with_overflow");
    exportDecFn(dec.addOrPanicC, "add_or_panic");
    exportDecFn(dec.addSaturatedC, "add_saturated");
    exportDecFn(dec.asinC, "asin");
    exportDecFn(dec.atanC, "atan");
    exportDecFn(dec.cosC, "cos");
    exportDecFn(dec.divC, "div");
    exportDecFn(dec.eqC, "eq");
    exportDecFn(dec.fromF32C, "from_float.f32");
    exportDecFn(dec.fromF64C, "from_float.f64");
    exportDecFn(dec.fromStr, "from_str");
    exportDecFn(dec.fromU64C, "from_u64");
    exportDecFn(dec.mulC, "mul_with_overflow");
    exportDecFn(dec.mulOrPanicC, "mul_or_panic");
    exportDecFn(dec.mulSaturatedC, "mul_saturated");
    exportDecFn(dec.negateC, "negate");
    exportDecFn(dec.neqC, "neq");
    exportDecFn(dec.sinC, "sin");
    exportDecFn(dec.subC, "sub_with_overflow");
    exportDecFn(dec.subOrPanicC, "sub_or_panic");
    exportDecFn(dec.subSaturatedC, "sub_saturated");
    exportDecFn(dec.tanC, "tan");
    exportDecFn(dec.toF64, "to_f64");
    exportDecFn(dec.toI128, "to_i128");
    exportDecFn(dec.toStr, "to_str");

    inline for (INTEGERS) |T| {
        dec.exportFromInt(T, ROC_BUILTINS ++ ".dec.from_int.");
    }
}

// List Module
const list = @import("list.zig");

comptime {
    exportListFn(list.listMap, "map");
    exportListFn(list.listMap2, "map2");
    exportListFn(list.listMap3, "map3");
    exportListFn(list.listMap4, "map4");
    exportListFn(list.listAppendUnsafe, "append_unsafe");
    exportListFn(list.listReserve, "reserve");
    exportListFn(list.listPrepend, "prepend");
    exportListFn(list.listWithCapacity, "with_capacity");
    exportListFn(list.listSortWith, "sort_with");
    exportListFn(list.listConcat, "concat");
    exportListFn(list.listSublist, "sublist");
    exportListFn(list.listDropAt, "drop_at");
    exportListFn(list.listReplace, "replace");
    exportListFn(list.listReplaceInPlace, "replace_in_place");
    exportListFn(list.listSwap, "swap");
    exportListFn(list.listIsUnique, "is_unique");
    exportListFn(list.listCapacity, "capacity");
    exportListFn(list.listRefcountPtr, "refcount_ptr");
    exportListFn(list.listReleaseExcessCapacity, "release_excess_capacity");
}

// Num Module
const num = @import("num.zig");

const INTEGERS = [_]type{ i8, i16, i32, i64, i128, u8, u16, u32, u64, u128 };
const WIDEINTS = [_]type{ i16, i32, i64, i128, i256, u16, u32, u64, u128, u256 };
const FLOATS = [_]type{ f32, f64 };
const NUMBERS = INTEGERS ++ FLOATS;

comptime {
    exportNumFn(num.bytesToU16C, "bytes_to_u16");
    exportNumFn(num.bytesToU32C, "bytes_to_u32");
    exportNumFn(num.bytesToU64C, "bytes_to_u64");
    exportNumFn(num.bytesToU128C, "bytes_to_u128");

    exportNumFn(num.shiftRightZeroFillI128, "shift_right_zero_fill.i128");
    exportNumFn(num.shiftRightZeroFillU128, "shift_right_zero_fill.u128");

    exportNumFn(num.compareI128, "compare.i128");
    exportNumFn(num.compareU128, "compare.u128");

    exportNumFn(num.lessThanI128, "less_than.i128");
    exportNumFn(num.lessThanOrEqualI128, "less_than_or_equal.i128");
    exportNumFn(num.greaterThanI128, "greater_than.i128");
    exportNumFn(num.greaterThanOrEqualI128, "greater_than_or_equal.i128");

    exportNumFn(num.lessThanU128, "less_than.u128");
    exportNumFn(num.lessThanOrEqualU128, "less_than_or_equal.u128");
    exportNumFn(num.greaterThanU128, "greater_than.u128");
    exportNumFn(num.greaterThanOrEqualU128, "greater_than_or_equal.u128");

    exportNumFn(num.compareI128, "compare.i128");
    exportNumFn(num.compareU128, "compare.u128");

    exportNumFn(num.lessThanI128, "less_than.i128");
    exportNumFn(num.lessThanOrEqualI128, "less_than_or_equal.i128");
    exportNumFn(num.greaterThanI128, "greater_than.i128");
    exportNumFn(num.greaterThanOrEqualI128, "greater_than_or_equal.i128");

    exportNumFn(num.lessThanU128, "less_than.u128");
    exportNumFn(num.lessThanOrEqualU128, "less_than_or_equal.u128");
    exportNumFn(num.greaterThanU128, "greater_than.u128");
    exportNumFn(num.greaterThanOrEqualU128, "greater_than_or_equal.u128");

    inline for (INTEGERS, 0..) |T, i| {
        num.exportPow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".pow_int.");
        num.exportDivCeil(T, ROC_BUILTINS ++ "." ++ NUM ++ ".div_ceil.");

        num.exportRound(f32, T, ROC_BUILTINS ++ "." ++ NUM ++ ".round_f32.");
        num.exportRound(f64, T, ROC_BUILTINS ++ "." ++ NUM ++ ".round_f64.");
        num.exportFloor(f32, T, ROC_BUILTINS ++ "." ++ NUM ++ ".floor_f32.");
        num.exportFloor(f64, T, ROC_BUILTINS ++ "." ++ NUM ++ ".floor_f64.");
        num.exportCeiling(f32, T, ROC_BUILTINS ++ "." ++ NUM ++ ".ceiling_f32.");
        num.exportCeiling(f64, T, ROC_BUILTINS ++ "." ++ NUM ++ ".ceiling_f64.");

        num.exportAddWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_with_overflow.");
        num.exportAddOrPanic(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_or_panic.");
        num.exportAddSaturatedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_saturated.");

        num.exportSubWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_with_overflow.");
        num.exportSubOrPanic(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_or_panic.");
        num.exportSubSaturatedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_saturated.");

        num.exportMulWithOverflow(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_with_overflow.");
        num.exportMulOrPanic(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_or_panic.");
        num.exportMulSaturatedInt(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_saturated.");
        num.exportMulWrappedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".mul_wrapped.");

        num.exportIsMultipleOf(T, ROC_BUILTINS ++ "." ++ NUM ++ ".is_multiple_of.");

        num.exportCountLeadingZeroBits(T, ROC_BUILTINS ++ "." ++ NUM ++ ".count_leading_zero_bits.");
        num.exportCountTrailingZeroBits(T, ROC_BUILTINS ++ "." ++ NUM ++ ".count_trailing_zero_bits.");
        num.exportCountOneBits(T, ROC_BUILTINS ++ "." ++ NUM ++ ".count_one_bits.");
    }

    inline for (INTEGERS) |FROM| {
        inline for (INTEGERS) |TO| {
            // We're exporting more than we need here, but that's okay.
            num.exportToIntCheckingMax(FROM, TO, ROC_BUILTINS ++ "." ++ NUM ++ ".int_to_" ++ @typeName(TO) ++ "_checking_max.");
            num.exportToIntCheckingMaxAndMin(FROM, TO, ROC_BUILTINS ++ "." ++ NUM ++ ".int_to_" ++ @typeName(TO) ++ "_checking_max_and_min.");
        }
    }

    inline for (FLOATS) |T| {
        num.exportAsin(T, ROC_BUILTINS ++ "." ++ NUM ++ ".asin.");
        num.exportAcos(T, ROC_BUILTINS ++ "." ++ NUM ++ ".acos.");
        num.exportAtan(T, ROC_BUILTINS ++ "." ++ NUM ++ ".atan.");

        num.exportSin(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sin.");
        num.exportCos(T, ROC_BUILTINS ++ "." ++ NUM ++ ".cos.");
        num.exportTan(T, ROC_BUILTINS ++ "." ++ NUM ++ ".tan.");

        num.exportPow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".pow.");
        num.exportLog(T, ROC_BUILTINS ++ "." ++ NUM ++ ".log.");
        num.exportFAbs(T, ROC_BUILTINS ++ "." ++ NUM ++ ".fabs.");
        num.exportSqrt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sqrt.");

        num.exportAddWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_with_overflow.");
        num.exportSubWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_with_overflow.");
        num.exportMulWithOverflow(T, T, ROC_BUILTINS ++ "." ++ NUM ++ ".mul_with_overflow.");

        num.exportIsNan(T, ROC_BUILTINS ++ "." ++ NUM ++ ".is_nan.");
        num.exportIsInfinite(T, ROC_BUILTINS ++ "." ++ NUM ++ ".is_infinite.");
        num.exportIsFinite(T, ROC_BUILTINS ++ "." ++ NUM ++ ".is_finite.");
    }
}

// Str Module
const str = @import("str.zig");
comptime {
    exportStrFn(str.init, "init");
    exportStrFn(str.strToScalarsC, "to_scalars");
    exportStrFn(str.strSplit, "str_split");
    exportStrFn(str.countSegments, "count_segments");
    exportStrFn(str.countGraphemeClusters, "count_grapheme_clusters");
    exportStrFn(str.countUtf8Bytes, "count_utf8_bytes");
    exportStrFn(str.isEmpty, "is_empty");
    exportStrFn(str.getCapacity, "capacity");
    exportStrFn(str.startsWith, "starts_with");
    exportStrFn(str.startsWithScalar, "starts_with_scalar");
    exportStrFn(str.endsWith, "ends_with");
    exportStrFn(str.strConcatC, "concat");
    exportStrFn(str.strJoinWithC, "joinWith");
    exportStrFn(str.strNumberOfBytes, "number_of_bytes");
    exportStrFn(str.strEqual, "equal");
    exportStrFn(str.substringUnsafe, "substring_unsafe");
    exportStrFn(str.getUnsafe, "get_unsafe");
    exportStrFn(str.reserve, "reserve");
    exportStrFn(str.getScalarUnsafe, "get_scalar_unsafe");
    exportStrFn(str.appendScalar, "append_scalar");
    exportStrFn(str.strToUtf8C, "to_utf8");
    exportStrFn(str.fromUtf8RangeC, "from_utf8_range");
    exportStrFn(str.repeat, "repeat");
    exportStrFn(str.strTrim, "trim");
    exportStrFn(str.strTrimStart, "trim_start");
    exportStrFn(str.strTrimEnd, "trim_end");
    exportStrFn(str.strCloneTo, "clone_to");
    exportStrFn(str.withCapacity, "with_capacity");
    exportStrFn(str.strGraphemes, "graphemes");
    exportStrFn(str.strRefcountPtr, "refcount_ptr");
    exportStrFn(str.strReleaseExcessCapacity, "release_excess_capacity");

    inline for (INTEGERS) |T| {
        str.exportFromInt(T, ROC_BUILTINS ++ "." ++ STR ++ ".from_int.");
        num.exportParseInt(T, ROC_BUILTINS ++ "." ++ STR ++ ".to_int.");
    }

    inline for (FLOATS) |T| {
        str.exportFromFloat(T, ROC_BUILTINS ++ "." ++ STR ++ ".from_float.");
        num.exportParseFloat(T, ROC_BUILTINS ++ "." ++ STR ++ ".to_float.");
    }
}

// Utils
comptime {
    exportUtilsFn(utils.test_panic, "test_panic");
    exportUtilsFn(utils.increfRcPtrC, "incref_rc_ptr");
    exportUtilsFn(utils.decrefRcPtrC, "decref_rc_ptr");
    exportUtilsFn(utils.freeRcPtrC, "free_rc_ptr");
    exportUtilsFn(utils.increfDataPtrC, "incref_data_ptr");
    exportUtilsFn(utils.decrefDataPtrC, "decref_data_ptr");
    exportUtilsFn(utils.freeDataPtrC, "free_data_ptr");
    exportUtilsFn(utils.isUnique, "is_unique");
    exportUtilsFn(utils.decrefCheckNullC, "decref_check_null");
    exportUtilsFn(utils.allocateWithRefcountC, "allocate_with_refcount");
    exportUtilsFn(utils.dictPseudoSeed, "dict_pseudo_seed");

    @export(panic_utils.panic, .{ .name = "roc_builtins.utils." ++ "panic", .linkage = .Weak });

    if (builtin.target.cpu.arch != .wasm32) {
        exportUtilsFn(expect.expectFailedStartSharedBuffer, "expect_failed_start_shared_buffer");
        exportUtilsFn(expect.expectFailedStartSharedFile, "expect_failed_start_shared_file");
        exportUtilsFn(expect.notifyParentExpect, "notify_parent_expect");
        exportUtilsFn(expect.notifyParentDbg, "notify_parent_dbg");

        // sets the buffer used for expect failures
        @export(expect.setSharedBuffer, .{ .name = "set_shared_buffer", .linkage = .Weak });

        exportUtilsFn(expect.readSharedBufferEnv, "read_env_shared_buffer");
    }

    if (builtin.target.cpu.arch == .aarch64) {
        @export(__roc_force_setjmp, .{ .name = "__roc_force_setjmp", .linkage = .Weak });
        @export(__roc_force_longjmp, .{ .name = "__roc_force_longjmp", .linkage = .Weak });
    }
}

// Utils continued - SJLJ
// For tests (in particular test_gen), roc_panic is implemented in terms of
// setjmp/longjmp. LLVM is unable to generate code for longjmp on AArch64 (https://github.com/roc-lang/roc/issues/2965),
// so instead we ask Zig to please provide implementations for us, which is does
// (seemingly via musl).
pub extern fn setjmp([*c]c_int) c_int;
pub extern fn longjmp([*c]c_int, c_int) noreturn;
pub extern fn _setjmp([*c]c_int) c_int;
pub extern fn _longjmp([*c]c_int, c_int) noreturn;
pub extern fn sigsetjmp([*c]c_int, c_int) c_int;
pub extern fn siglongjmp([*c]c_int, c_int) noreturn;
pub extern fn longjmperror() void;
// Zig won't expose the externs (and hence link correctly) unless we force them to be used.
fn __roc_force_setjmp(it: [*c]c_int) callconv(.C) c_int {
    return setjmp(it);
}
fn __roc_force_longjmp(a0: [*c]c_int, a1: c_int) callconv(.C) noreturn {
    longjmp(a0, a1);
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
pub fn panic(message: []const u8, stacktrace: ?*std.builtin.StackTrace, _: ?usize) noreturn {
    if (builtin.is_test) {
        std.debug.print("{s}: {?}", .{ message, stacktrace });
    }

    unreachable;
}

// Run all tests in imported modules
// https://github.com/ziglang/zig/blob/master/lib/std/std.zig#L94
test {
    const testing = std.testing;

    testing.refAllDecls(@This());
}
