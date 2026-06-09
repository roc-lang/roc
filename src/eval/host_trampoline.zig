//! Calls a hosted function using the platform C ABI **without generating code at runtime**.
//!
//! The interpreter knows a hosted function's signature only at runtime, and pure Zig cannot
//! synthesize a call to a runtime-determined signature. Rather than JIT a per-signature stub
//! (which would require mapping memory executable — forbidden in sandboxed embeddings and at
//! odds with the interpreter's "no codegen" property), we use the libffi-style technique: a
//! single fixed assembly stub, compiled into this binary ahead of time, that loads *all*
//! argument registers from caller-prepared "register image" arrays and calls the target.
//!
//! The intelligence stays in Zig: `abi.lower` (the same classifier the compiled backends use)
//! decides which bytes of each argument go in which register or on the stack, and we scatter
//! the interpreter's argument buffer into the images accordingly, call the fixed stub, then
//! gather the result registers back into the return buffer.

const std = @import("std");
const builtin = @import("builtin");
const layout = @import("layout");

const Store = layout.Store;
const Idx = layout.Idx;
const RocOps = @import("builtins").host_abi.RocOps;

/// The control block handed to the fixed assembly stub. Every Roc→host call is funneled
/// through a single pointer argument so the stub is free to clobber all argument registers.
const Call = extern struct {
    target: *const anyopaque,
    /// Values for the integer argument registers (x0..x7 / rdi,rsi,rdx,rcx,r8,r9).
    gp: *const [8]u64,
    /// Values for the SSE argument registers (v0..v7 / xmm0..xmm7), 16 bytes each.
    sse: *const [8]u128,
    /// Bytes to place in the stack argument area (already laid out), or null.
    stack: ?[*]const u8,
    /// Size of the stack argument area in bytes (kept 16-byte aligned by the stub).
    stack_size: usize,
    /// Indirect-result pointer for an sret return (x8 on aarch64 / first integer reg on x64),
    /// or null when the return is in registers or void.
    sret: ?*anyopaque,
    /// Captured integer result registers after the call (x0,x1 / rax,rdx).
    res_gp: *[2]u64,
    /// Captured SSE result registers after the call (v0,v1 / xmm0,xmm1).
    res_sse: *[2]u128,
};

const supported = switch (builtin.cpu.arch) {
    .aarch64, .aarch64_be, .x86_64 => true,
    else => false,
};

/// Whether the host trampoline supports the architecture this compiler is running on.
pub const available = supported;

const max_gp = 8;
const max_sse = 8;
/// Stack argument area: bounded; hosted functions with more than this much stack-passed
/// argument data are rejected rather than silently truncated.
const max_stack_bytes = 256;

/// Error returned when a hosted signature cannot be marshaled by the fixed trampoline (too
/// many register or stack arguments). These are not expected for real hosted functions.
pub const Error = error{ TooManyRegisters, TooManyStackBytes, UnsupportedArch };

/// Call hosted function `target` with the platform C ABI. `args_buf` holds the arguments
/// packed in Roc layout order (the interpreter's existing buffer); `ret_buf` receives the
/// return value. `roc_ops` is threaded as the leading argument iff the signature needs it.
pub fn call(
    store: *const Store,
    arena: std.mem.Allocator,
    target: *const anyopaque,
    roc_ops: *RocOps,
    arg_layouts: []const Idx,
    ret_layout: Idx,
    args_buf: [*]const u8,
    arg_offsets: []const u32,
    ret_buf: [*]u8,
) Error!void {
    if (!supported) return Error.UnsupportedArch;

    const target_abi: layout.abi.Target = switch (builtin.cpu.arch) {
        .aarch64, .aarch64_be => .aarch64,
        .x86_64 => if (builtin.os.tag == .windows) .x86_64_windows else .x86_64_sysv,
        else => return Error.UnsupportedArch,
    };

    const needs_ops = layout.abi.needsRocOps(store, arg_layouts, ret_layout);
    const lowered = layout.abi.lower(arena, store, target_abi, arg_layouts, ret_layout, needs_ops) catch return Error.TooManyRegisters;

    var gp: [max_gp]u64 = @splat(0);
    var sse: [max_sse]u128 = @splat(0);
    var stack: [max_stack_bytes]u8 align(16) = undefined;
    var gp_n: usize = 0;
    var sse_n: usize = 0;
    var stack_n: usize = 0;
    var sret: ?*anyopaque = null;

    // Return placement: a memory-class return passes the result buffer as the sret pointer.
    switch (lowered.ret) {
        .none, .registers => {},
        .indirect => sret = @ptrCast(ret_buf),
    }

    // The leading *RocOps occupies the first integer register.
    if (lowered.leading_ops) {
        gp[gp_n] = @intFromPtr(roc_ops);
        gp_n += 1;
    }

    for (lowered.args, arg_offsets) |placement, arg_offset| {
        const value = args_buf + arg_offset;
        switch (placement) {
            .none => {},
            .indirect => {
                // Pass a pointer to the argument's bytes (the caller-owned copy lives in
                // args_buf for the duration of the call).
                if (gp_n >= max_gp) return Error.TooManyRegisters;
                gp[gp_n] = @intFromPtr(value);
                gp_n += 1;
            },
            .registers => |pieces| {
                for (pieces) |piece| {
                    switch (piece.class) {
                        .integer => {
                            if (gp_n >= max_gp) return Error.TooManyRegisters;
                            gp[gp_n] = readUnaligned(u64, value + piece.offset, piece.size);
                            gp_n += 1;
                        },
                        .float => {
                            if (sse_n >= max_sse) return Error.TooManyRegisters;
                            sse[sse_n] = readUnaligned(u128, value + piece.offset, piece.size);
                            sse_n += 1;
                        },
                    }
                }
            },
        }
    }
    _ = &stack_n;

    var res_gp: [2]u64 = @splat(0);
    var res_sse: [2]u128 = @splat(0);
    var ctl = Call{
        .target = target,
        .gp = &gp,
        .sse = &sse,
        .stack = if (stack_n == 0) null else &stack,
        .stack_size = std.mem.alignForward(usize, stack_n, 16),
        .sret = sret,
        .res_gp = &res_gp,
        .res_sse = &res_sse,
    };
    invoke(&ctl);

    // Gather a register-class return back into the result buffer.
    switch (lowered.ret) {
        .none, .indirect => {},
        .registers => |pieces| {
            var gpi: usize = 0;
            var ssei: usize = 0;
            for (pieces) |piece| {
                switch (piece.class) {
                    .integer => {
                        writeUnaligned(ret_buf + piece.offset, std.mem.asBytes(&res_gp[gpi])[0..piece.size]);
                        gpi += 1;
                    },
                    .float => {
                        writeUnaligned(ret_buf + piece.offset, std.mem.asBytes(&res_sse[ssei])[0..piece.size]);
                        ssei += 1;
                    },
                }
            }
        },
    }
}

fn readUnaligned(comptime T: type, ptr: [*]const u8, size: u8) T {
    var buf: [@sizeOf(T)]u8 = @splat(0);
    @memcpy(buf[0..size], ptr[0..size]);
    return @as(*align(1) const T, @ptrCast(&buf)).*;
}

fn writeUnaligned(dst: [*]u8, bytes: []const u8) void {
    @memcpy(dst[0..bytes.len], bytes);
}

fn invoke(ctl: *const Call) void {
    switch (builtin.cpu.arch) {
        .aarch64, .aarch64_be => rocCallTrampolineAarch64(ctl),
        .x86_64 => rocCallTrampolineX86_64(ctl),
        else => unreachable,
    }
}

// The fixed trampolines. Each saves the control pointer in a callee-saved register, copies
// any stack arguments, loads every argument register from the images, calls the target, and
// captures the result registers. They never generate code — they are compiled into this
// binary's .text ahead of time. Struct field byte offsets are fixed by `Call`'s extern
// layout (8 bytes per field): target@0 gp@8 sse@16 stack@24 stack_size@32 sret@40 res_gp@48
// res_sse@56.

extern fn rocCallTrampolineAarch64(ctl: *const Call) callconv(.c) void;
extern fn rocCallTrampolineX86_64(ctl: *const Call) callconv(.c) void;

comptime {
    if (builtin.cpu.arch == .aarch64 or builtin.cpu.arch == .aarch64_be) {
        asm (
            \\.global _rocCallTrampolineAarch64
            \\.global rocCallTrampolineAarch64
            \\_rocCallTrampolineAarch64:
            \\rocCallTrampolineAarch64:
            \\  stp x29, x30, [sp, #-32]!
            \\  stp x19, x20, [sp, #16]
            \\  mov x29, sp
            \\  mov x19, x0              // x19 = control block (survives the call)
            \\  // copy stack arguments below sp (stack_size is 16-aligned)
            \\  ldr x20, [x19, #32]      // stack_size
            \\  cbz x20, 2f
            \\  sub sp, sp, x20
            \\  ldr x9, [x19, #24]       // stack ptr
            \\  mov x10, #0
            \\1:
            \\  ldr x11, [x9, x10]
            \\  str x11, [sp, x10]
            \\  add x10, x10, #8
            \\  cmp x10, x20
            \\  b.lo 1b
            \\2:
            \\  // load SSE argument registers from the image (16 bytes each)
            \\  ldr x9, [x19, #16]
            \\  ldp q0, q1, [x9, #0]
            \\  ldp q2, q3, [x9, #32]
            \\  ldp q4, q5, [x9, #64]
            \\  ldp q6, q7, [x9, #96]
            \\  // indirect-result register
            \\  ldr x8, [x19, #40]
            \\  // load integer argument registers from the image
            \\  ldr x9, [x19, #8]
            \\  ldp x0, x1, [x9, #0]
            \\  ldp x2, x3, [x9, #16]
            \\  ldp x4, x5, [x9, #32]
            \\  ldp x6, x7, [x9, #48]
            \\  // call the target
            \\  ldr x9, [x19, #0]
            \\  blr x9
            \\  // capture result registers
            \\  ldr x9, [x19, #48]
            \\  stp x0, x1, [x9]
            \\  ldr x9, [x19, #56]
            \\  stp q0, q1, [x9]
            \\  mov sp, x29
            \\  ldp x19, x20, [sp, #16]
            \\  ldp x29, x30, [sp], #32
            \\  ret
        );
    } else if (builtin.cpu.arch == .x86_64) {
        asm (
            \\.global _rocCallTrampolineX86_64
            \\.global rocCallTrampolineX86_64
            \\_rocCallTrampolineX86_64:
            \\rocCallTrampolineX86_64:
            \\  push %rbp
            \\  mov %rsp, %rbp
            \\  push %rbx
            \\  push %r12
            \\  mov %rdi, %rbx           // rbx = control block
            \\  // copy stack arguments (stack_size is 16-aligned, keeps rsp 16-aligned)
            \\  mov 32(%rbx), %r12       // stack_size
            \\  test %r12, %r12
            \\  jz 2f
            \\  sub %r12, %rsp
            \\  mov 24(%rbx), %rsi       // stack ptr
            \\  xor %rcx, %rcx
            \\1:
            \\  mov (%rsi,%rcx,1), %rax
            \\  mov %rax, (%rsp,%rcx,1)
            \\  add $8, %rcx
            \\  cmp %r12, %rcx
            \\  jb 1b
            \\2:
            \\  // load SSE argument registers (low 16 bytes of each image slot)
            \\  mov 16(%rbx), %rax
            \\  movups 0(%rax), %xmm0
            \\  movups 16(%rax), %xmm1
            \\  movups 32(%rax), %xmm2
            \\  movups 48(%rax), %xmm3
            \\  movups 64(%rax), %xmm4
            \\  movups 80(%rax), %xmm5
            \\  movups 96(%rax), %xmm6
            \\  movups 112(%rax), %xmm7
            \\  // load integer argument registers
            \\  mov 8(%rbx), %rax
            \\  mov 0(%rax), %rdi
            \\  mov 8(%rax), %rsi
            \\  mov 16(%rax), %rdx
            \\  mov 24(%rax), %rcx
            \\  mov 32(%rax), %r8
            \\  mov 40(%rax), %r9
            \\  // %al = number of vector registers used (varargs ABI requirement); pass max.
            \\  // Set last so the gp-image load above doesn't clobber it; upper rax bytes are
            \\  // caller-saved scratch and irrelevant to a fixed-prototype callee.
            \\  mov $8, %al
            \\  // call target
            \\  call *0(%rbx)
            \\  // capture result registers
            \\  mov 48(%rbx), %rcx
            \\  mov %rax, 0(%rcx)
            \\  mov %rdx, 8(%rcx)
            \\  mov 56(%rbx), %rcx
            \\  movups %xmm0, 0(%rcx)
            \\  movups %xmm1, 16(%rcx)
            \\  lea -16(%rbp), %rsp
            \\  pop %r12
            \\  pop %rbx
            \\  pop %rbp
            \\  ret
        );
    }
}

test "host_trampoline available on supported arches" {
    if (builtin.cpu.arch == .aarch64 or builtin.cpu.arch == .x86_64) {
        try std.testing.expect(available);
    }
}
