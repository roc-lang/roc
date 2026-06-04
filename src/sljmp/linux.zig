//! Linux implementation of setjmp/longjmp using naked functions.

const std = @import("std");
const builtin = @import("builtin");

const Impl = switch (builtin.cpu.arch) {
    .x86_64 => X86_64,
    .aarch64 => AArch64,
    else => @compileError("Unsupported Linux architecture for setjmp/longjmp"),
};

/// Architecture-specific buffer that stores the machine state for setjmp/longjmp.
pub const JmpBuf = Impl.JmpBuf;

/// Saves the current execution context in `env` and returns 0 on the initial call.
pub inline fn setjmp(env: *JmpBuf) c_int {
    return Impl.setjmp(env);
}

/// Restores `env` and resumes as if `setjmp` returned `val`, or 1 when `val` is 0.
pub inline fn longjmp(env: *JmpBuf, val: c_int) noreturn {
    Impl.longjmp(env, val);
}

const X86_64 = struct {
    const Self = @This();

    /// Linux x86_64 SysV state: RBX, RBP, R12-R15, RSP, RIP.
    pub const JmpBuf = [8]u64;

    fn setjmpImpl() callconv(.naked) void {
        asm volatile (
            \\movq %%rbx, 0x00(%%rdi)
            \\movq %%rbp, 0x08(%%rdi)
            \\movq %%r12, 0x10(%%rdi)
            \\movq %%r13, 0x18(%%rdi)
            \\movq %%r14, 0x20(%%rdi)
            \\movq %%r15, 0x28(%%rdi)
            \\leaq 0x08(%%rsp), %%rdx
            \\movq %%rdx, 0x30(%%rdi)
            \\movq (%%rsp), %%rdx
            \\movq %%rdx, 0x38(%%rdi)
            \\xorl %%eax, %%eax
            \\ret
        );
    }

    fn longjmpImpl() callconv(.naked) void {
        asm volatile (
            \\movl %%esi, %%eax
            \\testl %%eax, %%eax
            \\jnz 1f
            \\movl $1, %%eax
            \\1:
            \\movq 0x00(%%rdi), %%rbx
            \\movq 0x08(%%rdi), %%rbp
            \\movq 0x10(%%rdi), %%r12
            \\movq 0x18(%%rdi), %%r13
            \\movq 0x20(%%rdi), %%r14
            \\movq 0x28(%%rdi), %%r15
            \\movq 0x30(%%rdi), %%rsp
            \\jmpq *0x38(%%rdi)
        );
    }

    const setjmpFn: *const fn (*Self.JmpBuf) callconv(.c) c_int = @ptrCast(&setjmpImpl);
    const longjmpFn: *const fn (*Self.JmpBuf, c_int) callconv(.c) noreturn = @ptrCast(&longjmpImpl);

    pub inline fn setjmp(env: *Self.JmpBuf) c_int {
        return setjmpFn(env);
    }

    pub inline fn longjmp(env: *Self.JmpBuf, val: c_int) noreturn {
        longjmpFn(env, val);
    }
};

const AArch64 = struct {
    const Self = @This();

    /// Linux AArch64 state: X19-X30, SP, and D8-D15.
    pub const JmpBuf = [22]u64;

    fn setjmpImpl() callconv(.naked) void {
        asm volatile (
            \\stp x19, x20, [x0, #0]
            \\stp x21, x22, [x0, #16]
            \\stp x23, x24, [x0, #32]
            \\stp x25, x26, [x0, #48]
            \\stp x27, x28, [x0, #64]
            \\stp x29, x30, [x0, #80]
            \\mov x2, sp
            \\str x2, [x0, #104]
            \\stp d8, d9, [x0, #112]
            \\stp d10, d11, [x0, #128]
            \\stp d12, d13, [x0, #144]
            \\stp d14, d15, [x0, #160]
            \\mov w0, #0
            \\ret
        );
    }

    fn longjmpImpl() callconv(.naked) void {
        asm volatile (
            \\cmp w1, #0
            \\csinc w1, w1, wzr, ne
            \\ldp x19, x20, [x0, #0]
            \\ldp x21, x22, [x0, #16]
            \\ldp x23, x24, [x0, #32]
            \\ldp x25, x26, [x0, #48]
            \\ldp x27, x28, [x0, #64]
            \\ldp x29, x30, [x0, #80]
            \\ldr x2, [x0, #104]
            \\mov sp, x2
            \\ldp d8, d9, [x0, #112]
            \\ldp d10, d11, [x0, #128]
            \\ldp d12, d13, [x0, #144]
            \\ldp d14, d15, [x0, #160]
            \\mov w0, w1
            \\ret
        );
    }

    const setjmpFn: *const fn (*Self.JmpBuf) callconv(.c) c_int = @ptrCast(&setjmpImpl);
    const longjmpFn: *const fn (*Self.JmpBuf, c_int) callconv(.c) noreturn = @ptrCast(&longjmpImpl);

    pub inline fn setjmp(env: *Self.JmpBuf) c_int {
        return setjmpFn(env);
    }

    pub inline fn longjmp(env: *Self.JmpBuf, val: c_int) noreturn {
        longjmpFn(env, val);
    }
};

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
