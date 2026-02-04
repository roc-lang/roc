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

test "setjmp followed by function pointer call" {
    // Allocate executable memory and call code after setjmp
    // This tests the same pattern as dev_evaluator.callWithCrashProtection
    const os = std.os;

    // Simple code: mov qword ptr [rcx], 42; ret
    // Windows: first arg in RCX
    const code = [_]u8{ 0x48, 0xC7, 0x01, 0x2A, 0x00, 0x00, 0x00, 0xC3 };

    // Allocate executable memory
    const mem = os.windows.VirtualAlloc(
        null,
        4096,
        os.windows.MEM_COMMIT | os.windows.MEM_RESERVE,
        os.windows.PAGE_READWRITE,
    ) catch return error.VirtualAllocFailed;
    defer _ = os.windows.VirtualFree(mem, 0, os.windows.MEM_RELEASE);

    // Copy code
    const ptr: [*]u8 = @ptrCast(mem);
    @memcpy(ptr[0..code.len], &code);

    // Make executable
    var old_protect: os.windows.DWORD = undefined;
    os.windows.VirtualProtect(
        mem,
        4096,
        os.windows.PAGE_EXECUTE_READ,
        &old_protect,
    ) catch return error.VirtualProtectFailed;

    // Call pattern: setjmp, then call the function
    var result: i64 = 0;

    var jmp_buf: JmpBuf = undefined;
    if (setjmp(&jmp_buf) == 0) {
        const func: *const fn (*i64) callconv(.c) void = @ptrCast(@alignCast(mem));
        func(&result);
    } else {
        return error.UnexpectedLongjmp;
    }

    try std.testing.expectEqual(@as(i64, 42), result);
}

test "setjmp with full prologue/epilogue code" {
    // Test with code that has full Windows x64 prologue/epilogue like MonoExprCodeGen generates
    const os = std.os;

    // This mimics MonoExprCodeGen's generated code structure:
    // - Prologue: push rbp; mov rbp,rsp; push rbx; push r12; sub rsp,1024
    // - Save args: mov rbx, rcx (result ptr); mov r12, rdx (roc_ops - ignored)
    // - Compute: mov rax, 42
    // - Store result: mov [rbx], rax
    // - Epilogue: add rsp,1024; pop r12; pop rbx; pop rbp; ret
    const code = [_]u8{
        // Prologue
        0x55, // push rbp
        0x48, 0x89, 0xE5, // mov rbp, rsp
        0x53, // push rbx
        0x41, 0x54, // push r12
        0x48, 0x81, 0xEC, 0x00, 0x04, 0x00, 0x00, // sub rsp, 1024
        // Save args
        0x48, 0x89, 0xCB, // mov rbx, rcx (result ptr)
        0x49, 0x89, 0xD4, // mov r12, rdx (roc_ops)
        // Compute result
        0x48, 0xC7, 0xC0, 0x2A, 0x00, 0x00, 0x00, // mov rax, 42
        // Store to result ptr
        0x48, 0x89, 0x03, // mov [rbx], rax
        // Epilogue
        0x48, 0x81, 0xC4, 0x00, 0x04, 0x00, 0x00, // add rsp, 1024
        0x41, 0x5C, // pop r12
        0x5B, // pop rbx
        0x5D, // pop rbp
        0xC3, // ret
    };

    // Allocate executable memory
    const mem = os.windows.VirtualAlloc(
        null,
        4096,
        os.windows.MEM_COMMIT | os.windows.MEM_RESERVE,
        os.windows.PAGE_READWRITE,
    ) catch return error.VirtualAllocFailed;
    defer _ = os.windows.VirtualFree(mem, 0, os.windows.MEM_RELEASE);

    // Copy code
    const ptr: [*]u8 = @ptrCast(mem);
    @memcpy(ptr[0..code.len], &code);

    // Make executable
    var old_protect: os.windows.DWORD = undefined;
    os.windows.VirtualProtect(
        mem,
        4096,
        os.windows.PAGE_EXECUTE_READ,
        &old_protect,
    ) catch return error.VirtualProtectFailed;

    // Call pattern: setjmp, then call the function with 2 args
    var result: i64 = 0;
    var dummy_roc_ops: u64 = 0xDEADBEEF;

    var jmp_buf: JmpBuf = undefined;
    if (setjmp(&jmp_buf) == 0) {
        const func: *const fn (*i64, *u64) callconv(.c) void = @ptrCast(@alignCast(mem));
        func(&result, &dummy_roc_ops);
    } else {
        return error.UnexpectedLongjmp;
    }

    try std.testing.expectEqual(@as(i64, 42), result);
}

test "setjmp with i128 result (Dec pattern)" {
    // Test the exact pattern used for Dec (i128) results
    const os = std.os;

    // Code that stores a 128-bit value (like Dec):
    // - mov rax, low_64_bits
    // - mov [rbx+0], rax
    // - mov rax, high_64_bits
    // - mov [rbx+8], rax
    const code = [_]u8{
        // Prologue
        0x55, // push rbp
        0x48, 0x89, 0xE5, // mov rbp, rsp
        0x53, // push rbx
        0x41, 0x54, // push r12
        0x48, 0x81, 0xEC, 0x00, 0x04, 0x00, 0x00, // sub rsp, 1024
        // Save args
        0x48, 0x89, 0xCB, // mov rbx, rcx (result ptr)
        0x49, 0x89, 0xD4, // mov r12, rdx (roc_ops)
        // Load and store low 64 bits (42 as Dec = 42 * 10^18)
        0x48, 0xB8, 0x00, 0x00, 0x64, 0xA7, 0xB3, 0xB6, 0xE0, 0x0D, // mov rax, 0x0DE0B6B3A7640000
        0x48, 0x89, 0x03, // mov [rbx], rax
        // Load and store high 64 bits (0 for positive Dec)
        0x48, 0xC7, 0xC0, 0x00, 0x00, 0x00, 0x00, // mov rax, 0
        0x48, 0x89, 0x43, 0x08, // mov [rbx+8], rax
        // Epilogue
        0x48, 0x81, 0xC4, 0x00, 0x04, 0x00, 0x00, // add rsp, 1024
        0x41, 0x5C, // pop r12
        0x5B, // pop rbx
        0x5D, // pop rbp
        0xC3, // ret
    };

    // Allocate executable memory
    const mem = os.windows.VirtualAlloc(
        null,
        4096,
        os.windows.MEM_COMMIT | os.windows.MEM_RESERVE,
        os.windows.PAGE_READWRITE,
    ) catch return error.VirtualAllocFailed;
    defer _ = os.windows.VirtualFree(mem, 0, os.windows.MEM_RELEASE);

    // Copy code
    const ptr: [*]u8 = @ptrCast(mem);
    @memcpy(ptr[0..code.len], &code);

    // Make executable
    var old_protect: os.windows.DWORD = undefined;
    os.windows.VirtualProtect(
        mem,
        4096,
        os.windows.PAGE_EXECUTE_READ,
        &old_protect,
    ) catch return error.VirtualProtectFailed;

    // Call pattern with i128 result (aligned like actual code)
    var result: i128 align(16) = 0;
    var dummy_roc_ops: u64 = 0xDEADBEEF;

    var jmp_buf: JmpBuf = undefined;
    if (setjmp(&jmp_buf) == 0) {
        const func: *const fn (*i128, *u64) callconv(.c) void = @ptrCast(@alignCast(mem));
        func(&result, &dummy_roc_ops);
    } else {
        return error.UnexpectedLongjmp;
    }

    // Dec value for 42 = 42 * 10^18 = 0x0DE0B6B3A7640000
    const expected: i128 = 0x0DE0B6B3A7640000;
    try std.testing.expectEqual(expected, result);
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
