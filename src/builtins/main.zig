//! C Interface for Roc Builtins
//!
//! This module provides C-compatible functions for Roc's built-in functionality.
//! All functions are exported and follow C calling conventions.
//!
//! The exported functions are available in the static library `libroc_builtins.a`
//! and can be used by including the appropriate header file.
//!
//! ## Usage from C
//!
//! ```c
//! #include "roc_builtins.h"
//! // Link against libroc_builtins.a
//! ```
const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const utils = @import("utils.zig");
const panic_utils = @import("panic.zig");

const ROC_BUILTINS = "roc_builtins";
const NUM = "num";
const STR = "str";

// Dec Module
const dec = @import("dec.zig");

var FLTUSED: i32 = 0;
comptime {
    if (builtin.os.tag == .windows) {
        @export(&FLTUSED, .{ .name = "_fltused", .linkage = .weak });
    }
}

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
    exportDecFn(dec.logC, "log");
    exportDecFn(dec.powC, "pow");
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
    exportDecFn(dec.fromI128, "from_i128");
    exportDecFn(dec.to_str, "to_str");

    for (INTEGERS) |T| {
        dec.exportFromInt(T, ROC_BUILTINS ++ ".dec.from_int.");

        dec.exportRound(T, ROC_BUILTINS ++ ".dec.round.");
        dec.exportFloor(T, ROC_BUILTINS ++ ".dec.floor.");
        dec.exportCeiling(T, ROC_BUILTINS ++ ".dec.ceiling.");
    }
}

// List Module
const list = @import("list.zig");

comptime {
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
    exportListFn(list.listClone, "clone");
    exportListFn(list.listCapacity, "capacity");
    exportListFn(list.listAllocationPtr, "allocation_ptr");
    exportListFn(list.listReleaseExcessCapacity, "release_excess_capacity");
    exportListFn(list.listConcatUtf8, "concat_utf8");
    exportListFn(list.listIncref, "incref");
    exportListFn(list.listDecref, "decref");
}

// Num Module
const num = @import("num.zig");

const INTEGERS = [_]type{ i8, i16, i32, i64, i128, u8, u16, u32, u64, u128 };
const WIDEINTS = [_]type{ i16, i32, i64, i128, i256, u16, u32, u64, u128, u256 };
const FLOATS = [_]type{ f32, f64 };
const NUMBERS = INTEGERS ++ FLOATS;

comptime {
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
    exportNumFn(num.f32ToParts, "f32_to_parts");
    exportNumFn(num.f64ToParts, "f64_to_parts");
    exportNumFn(num.f32FromParts, "f32_from_parts");
    exportNumFn(num.f64FromParts, "f64_from_parts");
    exportNumFn(num.f32ToBits, "f32_to_bits");
    exportNumFn(num.f64ToBits, "f64_to_bits");
    exportNumFn(num.i128ToBits, "i128_to_bits");
    exportNumFn(num.f32FromBits, "f32_from_bits");
    exportNumFn(num.f64FromBits, "f64_from_bits");
    exportNumFn(num.i128FromBits, "i128_from_bits");

    for (INTEGERS, 0..) |T, i| {
        num.exportPow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".pow_int.");
        num.exportDivCeil(T, ROC_BUILTINS ++ "." ++ NUM ++ ".div_ceil.");

        num.exportRound(f32, T, ROC_BUILTINS ++ "." ++ NUM ++ ".round_f32.");
        num.exportRound(f64, T, ROC_BUILTINS ++ "." ++ NUM ++ ".round_f64.");
        num.exportFloor(f32, T, ROC_BUILTINS ++ "." ++ NUM ++ ".floor_f32.");
        num.exportFloor(f64, T, ROC_BUILTINS ++ "." ++ NUM ++ ".floor_f64.");
        num.exportCeiling(f32, T, ROC_BUILTINS ++ "." ++ NUM ++ ".ceiling_f32.");
        num.exportCeiling(f64, T, ROC_BUILTINS ++ "." ++ NUM ++ ".ceiling_f64.");

        num.exportNumToFloatCast(T, f32, ROC_BUILTINS ++ "." ++ NUM ++ ".num_to_float_cast_f32.");
        num.exportNumToFloatCast(T, f64, ROC_BUILTINS ++ "." ++ NUM ++ ".num_to_float_cast_f64.");

        num.exportAddWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_with_overflow.");
        num.exportAddOrPanic(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_or_panic.");
        num.exportAddSaturatedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_saturated.");
        num.exportAddWrappedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_wrapped.");

        num.exportSubWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_with_overflow.");
        num.exportSubOrPanic(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_or_panic.");
        num.exportSubSaturatedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_saturated.");
        num.exportSubWrappedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_wrapped.");

        num.exportMulWithOverflow(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_with_overflow.");
        num.exportMulOrPanic(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_or_panic.");
        num.exportMulSaturatedInt(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_saturated.");
        num.exportMulWrappedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".mul_wrapped.");

        num.exportIsMultipleOf(T, ROC_BUILTINS ++ "." ++ NUM ++ ".is_multiple_of.");

        num.exportCountLeadingZeroBits(T, ROC_BUILTINS ++ "." ++ NUM ++ ".count_leading_zero_bits.");
        num.exportCountTrailingZeroBits(T, ROC_BUILTINS ++ "." ++ NUM ++ ".count_trailing_zero_bits.");
        num.exportCountOneBits(T, ROC_BUILTINS ++ "." ++ NUM ++ ".count_one_bits.");
    }

    for (INTEGERS) |FROM| {
        for (INTEGERS) |TO| {
            // We're exporting more than we need here, but that's okay.
            num.exportToIntCheckingMax(FROM, TO, ROC_BUILTINS ++ "." ++ NUM ++ ".int_to_" ++ @typeName(TO) ++ "_checking_max.");
            num.exportToIntCheckingMaxAndMin(FROM, TO, ROC_BUILTINS ++ "." ++ NUM ++ ".int_to_" ++ @typeName(TO) ++ "_checking_max_and_min.");
        }
    }

    for (FLOATS) |T| {
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
    exportStrFn(str.strSplitOn, "str_split_on");
    exportStrFn(str.countSegments, "count_segments");
    exportStrFn(str.countUtf8Bytes, "count_utf8_bytes");
    exportStrFn(str.isEmpty, "is_empty");
    exportStrFn(str.getCapacity, "capacity");
    exportStrFn(str.startsWith, "starts_with");
    exportStrFn(str.endsWith, "ends_with");
    exportStrFn(str.strConcatC, "concat");
    exportStrFn(str.strJoinWithC, "joinWith");
    exportStrFn(str.strNumberOfBytes, "number_of_bytes");
    exportStrFn(str.strEqual, "equal");
    exportStrFn(str.substringUnsafeC, "substring_unsafe");
    exportStrFn(str.getUnsafeC, "get_unsafe");
    exportStrFn(str.reserveC, "reserve");
    exportStrFn(str.strToUtf8C, "to_utf8");
    exportStrFn(str.fromUtf8C, "from_utf8");
    exportStrFn(str.fromUtf8Lossy, "from_utf8_lossy");
    exportStrFn(str.repeatC, "repeat");
    exportStrFn(str.strTrim, "trim");
    exportStrFn(str.strTrimStart, "trim_start");
    exportStrFn(str.strTrimEnd, "trim_end");
    exportStrFn(str.strCloneTo, "clone_to");
    exportStrFn(str.withCapacityC, "with_capacity");
    exportStrFn(str.strAllocationPtr, "allocation_ptr");
    exportStrFn(str.strReleaseExcessCapacity, "release_excess_capacity");
    exportStrFn(str.strWithAsciiLowercased, "with_ascii_lowercased");
    exportStrFn(str.strWithAsciiUppercased, "with_ascii_uppercased");
    exportStrFn(str.strCaselessAsciiEquals, "caseless_ascii_equals");

    for (INTEGERS) |T| {
        str.exportFromInt(T, ROC_BUILTINS ++ "." ++ STR ++ ".from_int.");
        num.exportParseInt(T, ROC_BUILTINS ++ "." ++ STR ++ ".to_int.");
    }

    for (FLOATS) |T| {
        str.exportFromFloat(T, ROC_BUILTINS ++ "." ++ STR ++ ".from_float.");
        num.exportParseFloat(T, ROC_BUILTINS ++ "." ++ STR ++ ".to_float.");
    }
}

// Utils
comptime {
    exportUtilsFn(utils.test_dbg, "test_dbg");
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

    @export(&panic_utils.panic, .{ .name = "roc_builtins.utils." ++ "panic", .linkage = .weak });
}

// Export helpers - Must be run inside a comptime
// These functions handle the C function export process
fn exportBuiltinFn(comptime func: anytype, comptime func_name: []const u8) void {
    const func_ptr = &func;
    @export(func_ptr, .{ .name = "roc_builtins." ++ func_name, .linkage = .strong });
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
/// Panic function for the Roc builtins C interface.
/// This function handles runtime errors and panics in a way that's compatible
/// with the C ABI and doesn't interfere with LLVM verification.
pub fn panic(message: []const u8, stacktrace: ?*std.builtin.StackTrace, _: ?usize) noreturn {
    if (builtin.target.cpu.arch != .wasm32) {
        std.debug.print("\nSomehow in unreachable zig panic!\nThis is a roc standard library bug\n{s}: {?}", .{ message, stacktrace });
        std.process.abort();
    } else {
        // Can't call abort or print from wasm. Just leave it as unreachable.
        unreachable;
    }
}
