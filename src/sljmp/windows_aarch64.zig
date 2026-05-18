//! Windows ARM64 implementation of setjmp/longjmp using naked functions.
//!
//! On Windows, the C library setjmp is a compiler intrinsic that cannot be
//! linked directly, so we provide our own implementation.

const std = @import("std");

/// Windows ARM64 jmp_buf: stores callee-saved registers per Windows ARM64 ABI.
/// Layout: X19-X28 (10 regs), X29 (FP), X30 (LR), SP, D8-D15 (8 SIMD regs)
/// Total: 13 * 8 + 8 * 8 = 168 bytes
pub const JmpBuf = [21]u64;

/// Naked function implementation for setjmp.
/// Saves callee-saved registers per Windows ARM64 ABI.
fn setjmpImpl() callconv(.naked) void {
    // Windows ARM64: first arg in X0, return in X0
    asm volatile (
    // Save general-purpose callee-saved registers X19-X28
        \\stp x19, x20, [x0, #0]
        \\stp x21, x22, [x0, #16]
        \\stp x23, x24, [x0, #32]
        \\stp x25, x26, [x0, #48]
        \\stp x27, x28, [x0, #64]
        // Save frame pointer (X29) and link register (X30)
        \\stp x29, x30, [x0, #80]
        // Save stack pointer
        \\mov x1, sp
        \\str x1, [x0, #96]
        // Save SIMD callee-saved registers D8-D15
        \\stp d8, d9, [x0, #104]
        \\stp d10, d11, [x0, #120]
        \\stp d12, d13, [x0, #136]
        \\stp d14, d15, [x0, #152]
        // Return 0 on first call (use w0 since c_int is 32 bits)
        \\mov w0, #0
        \\ret
    );
}

/// Naked function implementation for longjmp.
/// Restores callee-saved registers and returns to saved location.
fn longjmpImpl() callconv(.naked) void {
    // Windows ARM64: first arg (jmp_buf) in X0, second arg (val) in W1 (32-bit)
    asm volatile (
    // Set return value (if val == 0, return 1)
    // Use 32-bit registers (w) since c_int is 32 bits
        \\cmp w1, #0
        \\csinc w1, w1, wzr, ne
        // Restore SIMD callee-saved registers D8-D15
        \\ldp d8, d9, [x0, #104]
        \\ldp d10, d11, [x0, #120]
        \\ldp d12, d13, [x0, #136]
        \\ldp d14, d15, [x0, #152]
        // Restore stack pointer
        \\ldr x2, [x0, #96]
        \\mov sp, x2
        // Restore frame pointer (X29) and link register (X30)
        \\ldp x29, x30, [x0, #80]
        // Restore general-purpose callee-saved registers X19-X28
        \\ldp x19, x20, [x0, #0]
        \\ldp x21, x22, [x0, #16]
        \\ldp x23, x24, [x0, #32]
        \\ldp x25, x26, [x0, #48]
        \\ldp x27, x28, [x0, #64]
        // Move return value to W0 (32-bit) and return
        \\mov w0, w1
        \\ret
    );
}

/// Function pointer to the naked setjmp implementation.
/// Using a function pointer allows us to call the naked function.
const setjmpFn: *const fn (*JmpBuf) callconv(.c) c_int = @ptrCast(&setjmpImpl);

/// Function pointer to the naked longjmp implementation.
const longjmpFn: *const fn (*JmpBuf, c_int) callconv(.c) noreturn = @ptrCast(&longjmpImpl);

/// Save the current execution context for later restoration via longjmp.
/// Returns 0 on first call, non-zero when returning via longjmp.
pub inline fn setjmp(env: *JmpBuf) c_int {
    return setjmpFn(env);
}

/// Restore execution context saved by setjmp, causing setjmp to return `val`.
/// If val is 0, setjmp returns 1 instead.
pub inline fn longjmp(env: *JmpBuf, val: c_int) noreturn {
    longjmpFn(env, val);
}

test "setjmp returns 0 on first call" {
    var buf: JmpBuf = undefined;
    const result = setjmp(&buf);
    try std.testing.expectEqual(@as(c_int, 0), result);
}

test "setjmp/longjmp basic functionality" {
    var buf: JmpBuf = undefined;
    var longjmp_called = false;

    const result = setjmp(&buf);
    if (result == 0) {
        longjmp(&buf, 42);
        unreachable;
    } else {
        longjmp_called = true;
        try std.testing.expectEqual(@as(c_int, 42), result);
    }
    try std.testing.expect(longjmp_called);
}

test "longjmp with value 0 returns 1" {
    var buf: JmpBuf = undefined;

    const result = setjmp(&buf);
    if (result == 0) {
        longjmp(&buf, 0);
        unreachable;
    } else {
        try std.testing.expectEqual(@as(c_int, 1), result);
    }
}

test "crash protection pattern (nested callback)" {
    const Env = struct {
        jmp_buf: JmpBuf = undefined,
        crashed: bool = false,

        fn crashCallback(self: *@This()) noreturn {
            self.crashed = true;
            longjmp(&self.jmp_buf, 1);
        }

        fn riskyOperation(self: *@This(), should_crash: bool) i32 {
            if (should_crash) {
                self.crashCallback();
            }
            return 42;
        }
    };

    var env = Env{};

    // Test normal execution
    {
        env.crashed = false;
        const result = setjmp(&env.jmp_buf);
        if (result == 0) {
            const value = env.riskyOperation(false);
            try std.testing.expectEqual(@as(i32, 42), value);
            try std.testing.expect(!env.crashed);
        } else {
            return error.UnexpectedLongjmp;
        }
    }

    // Test crash recovery
    {
        env.crashed = false;
        const result = setjmp(&env.jmp_buf);
        if (result == 0) {
            _ = env.riskyOperation(true);
            unreachable;
        } else {
            try std.testing.expect(env.crashed);
            try std.testing.expectEqual(@as(c_int, 1), result);
        }
    }
}
