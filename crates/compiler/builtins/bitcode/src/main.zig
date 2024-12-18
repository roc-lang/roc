const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const utils = @import("utils.zig");
const expect = @import("expect.zig");
const panic_utils = @import("panic.zig");
const dbg_utils = @import("dbg.zig");

const ROC_BUILTINS = "roc_builtins";
const NUM = "num";
const STR = "str";

// Dec Module
const dec = @import("dec.zig");

var FLTUSED: i32 = 0;
comptime {
    if (builtin.os.tag == .windows) {
        @export(FLTUSED, .{ .name = "_fltused", .linkage = .weak });
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
    exportDecFn(dec.toStr, "to_str");

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
    exportStrFn(str.repeatC, "repeat");
    exportStrFn(str.strTrim, "trim");
    exportStrFn(str.strTrimStart, "trim_start");
    exportStrFn(str.strTrimEnd, "trim_end");
    exportStrFn(str.strCloneTo, "clone_to");
    exportStrFn(str.withCapacityC, "with_capacity");
    exportStrFn(str.strAllocationPtr, "allocation_ptr");
    exportStrFn(str.strReleaseExcessCapacity, "release_excess_capacity");

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

    @export(panic_utils.panic, .{ .name = "roc_builtins.utils." ++ "panic", .linkage = .weak });
    @export(dbg_utils.dbg_impl, .{ .name = "roc_builtins.utils." ++ "dbg_impl", .linkage = .weak });

    if (builtin.target.cpu.arch != .wasm32) {
        exportUtilsFn(expect.expectFailedStartSharedBuffer, "expect_failed_start_shared_buffer");
        exportUtilsFn(expect.expectFailedStartSharedFile, "expect_failed_start_shared_file");
        exportUtilsFn(expect.notifyParentExpect, "notify_parent_expect");

        // sets the buffer used for expect failures
        @export(expect.setSharedBuffer, .{ .name = "set_shared_buffer", .linkage = .weak });

        exportUtilsFn(expect.readSharedBufferEnv, "read_env_shared_buffer");
    }

    if (builtin.target.cpu.arch == .aarch64) {
        @export(__roc_force_setjmp, .{ .name = "__roc_force_setjmp", .linkage = .weak });
        @export(__roc_force_longjmp, .{ .name = "__roc_force_longjmp", .linkage = .weak });
    } else if (builtin.os.tag == .windows) {
        @export(__roc_force_setjmp_windows, .{ .name = "__roc_force_setjmp", .linkage = .weak });
        @export(__roc_force_longjmp_windows, .{ .name = "__roc_force_longjmp", .linkage = .weak });
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

pub extern fn windows_setjmp([*c]c_int) c_int;
pub extern fn windows_longjmp([*c]c_int, c_int) noreturn;

fn __roc_force_setjmp_windows(it: [*c]c_int) callconv(.C) c_int {
    return windows_setjmp(it);
}

fn __roc_force_longjmp_windows(a0: [*c]c_int, a1: c_int) callconv(.C) noreturn {
    windows_longjmp(a0, a1);
}

comptime {
    if (builtin.os.tag == .windows) {
        asm (
            \\.global windows_longjmp;
            \\windows_longjmp:
            \\  movq 0x00(%rcx), %rdx
            \\  movq 0x08(%rcx), %rbx
            \\  # note 0x10 is not used yet!
            \\  movq 0x18(%rcx), %rbp
            \\  movq 0x20(%rcx), %rsi
            \\  movq 0x28(%rcx), %rdi
            \\  movq 0x30(%rcx), %r12
            \\  movq 0x38(%rcx), %r13
            \\  movq 0x40(%rcx), %r14
            \\  movq 0x48(%rcx), %r15
            \\
            \\  # restore stack pointer
            \\  movq 0x10(%rcx), %rsp
            \\
            \\  # load jmp address
            \\  movq 0x50(%rcx), %r8
            \\
            \\  # set up return value
            \\  movq %rbx, %rax
            \\
            \\  movdqu 0x60(%rcx), %xmm6
            \\  movdqu 0x70(%rcx), %xmm7
            \\  movdqu 0x80(%rcx), %xmm8
            \\  movdqu 0x90(%rcx), %xmm9
            \\  movdqu 0xa0(%rcx), %xmm10
            \\  movdqu 0xb0(%rcx), %xmm11
            \\  movdqu 0xc0(%rcx), %xmm12
            \\  movdqu 0xd0(%rcx), %xmm13
            \\  movdqu 0xe0(%rcx), %xmm14
            \\  movdqu 0xf0(%rcx), %xmm15
            \\
            \\  jmp *%r8
            \\
            \\.global windows_setjmp;
            \\windows_setjmp:
            \\  movq %rdx, 0x00(%rcx)
            \\  movq %rbx, 0x08(%rcx)
            \\  # note 0x10 is not used yet!
            \\  movq %rbp, 0x18(%rcx)
            \\  movq %rsi, 0x20(%rcx)
            \\  movq %rdi, 0x28(%rcx)
            \\  movq %r12, 0x30(%rcx)
            \\  movq %r13, 0x38(%rcx)
            \\  movq %r14, 0x40(%rcx)
            \\  movq %r15, 0x48(%rcx)
            \\
            \\  # the stack location right after the windows_setjmp call
            \\  leaq 0x08(%rsp), %r8
            \\  movq %r8, 0x10(%rcx)
            \\
            \\  movq (%rsp), %r8
            \\  movq %r8, 0x50(%rcx)
            \\
            \\  movdqu %xmm6,  0x60(%rcx)
            \\  movdqu %xmm7,  0x70(%rcx)
            \\  movdqu %xmm8,  0x80(%rcx)
            \\  movdqu %xmm9,  0x90(%rcx)
            \\  movdqu %xmm10, 0xa0(%rcx)
            \\  movdqu %xmm11, 0xb0(%rcx)
            \\  movdqu %xmm12, 0xc0(%rcx)
            \\  movdqu %xmm13, 0xd0(%rcx)
            \\  movdqu %xmm14, 0xe0(%rcx)
            \\  movdqu %xmm15, 0xf0(%rcx)
            \\
            \\  xorl %eax, %eax
            \\  ret
            \\
        );
    }
}

// Export helpers - Must be run inside a comptime
fn exportBuiltinFn(comptime func: anytype, comptime func_name: []const u8) void {
    @export(func, .{ .name = "roc_builtins." ++ func_name, .linkage = .strong });
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
    if (builtin.target.cpu.arch != .wasm32) {
        std.debug.print("\nSomehow in unreachable zig panic!\nThis is a roc standard libarry bug\n{s}: {?}", .{ message, stacktrace });
        std.process.abort();
    } else {
        // Can't call abort or print from wasm. Just leave it as unreachable.
        unreachable;
    }
}

// Run all tests in imported modules
// https://github.com/ziglang/zig/blob/master/lib/std/std.zig#L94
test {
    const testing = std.testing;

    testing.refAllDecls(@This());
}
