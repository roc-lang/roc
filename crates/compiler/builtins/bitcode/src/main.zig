const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const utils = @import("utils.zig");
const expect = @import("expect.zig");
const panic_utils = @import("panic.zig");

const ROC_BUILTINS = "roc_builtins";
const NUM = "num";
const STR = "str";

// Dec Module
const dec = @import("dec.zig");

comptime {
    exportDecFn(dec.fromStr, "from_str");
    exportDecFn(dec.toStr, "to_str");
    exportDecFn(dec.fromF64C, "from_f64");
    exportDecFn(dec.eqC, "eq");
    exportDecFn(dec.neqC, "neq");
    exportDecFn(dec.negateC, "negate");
    exportDecFn(dec.divC, "div");

    exportDecFn(dec.addC, "add_with_overflow");
    exportDecFn(dec.addOrPanicC, "add_or_panic");
    exportDecFn(dec.addSaturatedC, "add_saturated");

    exportDecFn(dec.subC, "sub_with_overflow");
    exportDecFn(dec.subOrPanicC, "sub_or_panic");
    exportDecFn(dec.subSaturatedC, "sub_saturated");

    exportDecFn(dec.mulC, "mul_with_overflow");
    exportDecFn(dec.mulOrPanicC, "mul_or_panic");
    exportDecFn(dec.mulSaturatedC, "mul_saturated");
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

    inline for (INTEGERS) |T, i| {
        num.exportPow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".pow_int.");
        num.exportDivCeil(T, ROC_BUILTINS ++ "." ++ NUM ++ ".div_ceil.");

        num.exportRoundF32(T, ROC_BUILTINS ++ "." ++ NUM ++ ".round_f32.");
        num.exportRoundF64(T, ROC_BUILTINS ++ "." ++ NUM ++ ".round_f64.");

        num.exportAddWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_with_overflow.");
        num.exportAddOrPanic(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_or_panic.");
        num.exportAddSaturatedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_saturated.");

        num.exportSubWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_with_overflow.");
        num.exportSubOrPanic(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_or_panic.");
        num.exportSubSaturatedInt(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_saturated.");

        num.exportMulWithOverflow(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_with_overflow.");
        num.exportMulOrPanic(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_or_panic.");
        num.exportMulSaturatedInt(T, WIDEINTS[i], ROC_BUILTINS ++ "." ++ NUM ++ ".mul_saturated.");
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

        num.exportPow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".pow.");
        num.exportLog(T, ROC_BUILTINS ++ "." ++ NUM ++ ".log.");

        num.exportAddWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".add_with_overflow.");
        num.exportSubWithOverflow(T, ROC_BUILTINS ++ "." ++ NUM ++ ".sub_with_overflow.");
        num.exportMulWithOverflow(T, T, ROC_BUILTINS ++ "." ++ NUM ++ ".mul_with_overflow.");

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
    exportStrFn(str.fromUtf8C, "from_utf8");
    exportStrFn(str.fromUtf8RangeC, "from_utf8_range");
    exportStrFn(str.repeat, "repeat");
    exportStrFn(str.strTrim, "trim");
    exportStrFn(str.strTrimLeft, "trim_left");
    exportStrFn(str.strTrimRight, "trim_right");
    exportStrFn(str.strCloneTo, "clone_to");
    exportStrFn(str.withCapacity, "with_capacity");
    exportStrFn(str.strGraphemes, "graphemes");

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
    exportUtilsFn(utils.increfC, "incref");
    exportUtilsFn(utils.decrefC, "decref");
    exportUtilsFn(utils.decrefCheckNullC, "decref_check_null");
    exportUtilsFn(utils.allocateWithRefcountC, "allocate_with_refcount");

    @export(panic_utils.panic, .{ .name = "roc_builtins.utils." ++ "panic", .linkage = .Weak });

    if (builtin.target.cpu.arch != .wasm32) {
        exportUtilsFn(expect.expectFailedStartSharedBuffer, "expect_failed_start_shared_buffer");
        exportUtilsFn(expect.expectFailedStartSharedFile, "expect_failed_start_shared_file");
        exportUtilsFn(expect.expectFailedFinalize, "expect_failed_finalize");
        exportUtilsFn(expect.sendDbg, "send_dbg");

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
pub fn panic(message: []const u8, stacktrace: ?*std.builtin.StackTrace) noreturn {
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
    if (builtin.target.os.tag != .windows) {
        @export(__muloti4, .{ .name = "__muloti4", .linkage = .Weak });
    }
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
