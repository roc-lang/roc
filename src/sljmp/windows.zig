//! Windows x64 implementation of setjmp/longjmp using naked functions.
//!
//! On Windows, the C library setjmp is a compiler intrinsic that cannot be
//! linked directly, so we provide our own implementation based on musl libc.

const std = @import("std");

/// Windows x64 jmp_buf: 10 * 8 = 80 bytes
/// Stores: RBX, RBP, RSI, RDI, R12-R15, RSP, RIP
pub const JmpBuf = [10]u64;

/// Naked function implementation for setjmp.
/// Saves callee-saved registers per Windows x64 ABI.
fn setjmpImpl() callconv(.naked) void {
    // Windows x64: first arg in RCX, return in EAX
    asm volatile (
        \\mov %%rbx, 0x00(%%rcx)
        \\mov %%rbp, 0x08(%%rcx)
        \\mov %%rsi, 0x10(%%rcx)
        \\mov %%rdi, 0x18(%%rcx)
        \\mov %%r12, 0x20(%%rcx)
        \\mov %%r13, 0x28(%%rcx)
        \\mov %%r14, 0x30(%%rcx)
        \\mov %%r15, 0x38(%%rcx)
        // Save stack pointer (after return address is popped)
        \\lea 0x08(%%rsp), %%rdx
        \\mov %%rdx, 0x40(%%rcx)
        // Save return address
        \\mov (%%rsp), %%rdx
        \\mov %%rdx, 0x48(%%rcx)
        // Return 0 on first call
        \\xor %%eax, %%eax
        \\ret
    );
}

/// Naked function implementation for longjmp.
/// Restores callee-saved registers and jumps to saved return address.
fn longjmpImpl() callconv(.naked) void {
    // Windows x64: first arg in RCX, second in EDX
    asm volatile (
        // Set return value (if val == 0, return 1)
        \\mov %%edx, %%eax
        \\test %%eax, %%eax
        \\jnz 1f
        \\mov $1, %%eax
        \\1:
        // Restore callee-saved registers
        \\mov 0x00(%%rcx), %%rbx
        \\mov 0x08(%%rcx), %%rbp
        \\mov 0x10(%%rcx), %%rsi
        \\mov 0x18(%%rcx), %%rdi
        \\mov 0x20(%%rcx), %%r12
        \\mov 0x28(%%rcx), %%r13
        \\mov 0x30(%%rcx), %%r14
        \\mov 0x38(%%rcx), %%r15
        // Restore stack pointer
        \\mov 0x40(%%rcx), %%rsp
        // Jump to saved return address
        \\jmp *0x48(%%rcx)
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
    // This test mimics the dev_evaluator crash protection pattern:
    // setjmp is called, then a nested function (like generated code calling roc_crashed)
    // triggers longjmp to unwind back to the setjmp call site.
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
