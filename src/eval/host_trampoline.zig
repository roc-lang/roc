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
/// return value. Hosted functions take their natural C ABI: no RocOps is passed.
pub fn call(
    store: *const Store,
    arena: std.mem.Allocator,
    target: *const anyopaque,
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

    // Hosted functions take their natural C ABI under the symbol ABI: the host
    // reaches its own runtime operations directly, so no leading *RocOps.
    const lowered = layout.abi.lower(arena, store, target_abi, arg_layouts, ret_layout, false) catch return Error.TooManyRegisters;

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

    // x86_64 passes an indirect result pointer as the first integer argument.
    // AArch64 uses x8, which the assembly stub loads from the `sret` field.
    if (sret) |ret_ptr| {
        if (target_abi == .x86_64_sysv or target_abi == .x86_64_windows) {
            if (gp_n >= max_gp) return Error.TooManyRegisters;
            gp[gp_n] = @intFromPtr(ret_ptr);
            gp_n += 1;
        }
    }

    for (lowered.args, arg_offsets, arg_layouts) |placement, arg_offset, arg_layout| {
        const value = args_buf + arg_offset;
        switch (placement) {
            .none => {},
            .indirect => {
                if (target_abi == .x86_64_sysv) {
                    const size = store.layoutSize(store.getLayout(arg_layout));
                    try appendStackBytes(&stack, &stack_n, value, size);
                } else {
                    // AAPCS64 and Win64 pass memory-class arguments by pointer.
                    if (gp_n >= max_gp) return Error.TooManyRegisters;
                    gp[gp_n] = @intFromPtr(value);
                    gp_n += 1;
                }
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

fn appendStackBytes(stack: *[max_stack_bytes]u8, stack_n: *usize, value: [*]const u8, size: usize) Error!void {
    const aligned_size = std.mem.alignForward(usize, size, 8);
    if (stack_n.* + aligned_size > max_stack_bytes) return Error.TooManyStackBytes;
    @memcpy(stack[stack_n.* .. stack_n.* + size], value[0..size]);
    if (aligned_size > size) {
        @memset(stack[stack_n.* + size .. stack_n.* + aligned_size], 0);
    }
    stack_n.* += aligned_size;
}

fn invoke(ctl: *const Call) void {
    switch (builtin.cpu.arch) {
        .aarch64, .aarch64_be, .x86_64 => rocCallTrampoline(ctl),
        else => unreachable,
    }
}

// The fixed trampoline, defined in `host_trampoline.S`. It saves the control pointer in a
// callee-saved register, copies any stack arguments, loads every argument register from the
// images, calls the target, and captures the result registers. It never generates code — it
// is assembled into this binary's .text ahead of time.
extern fn rocCallTrampoline(ctl: *const Call) callconv(.c) void;

test "host_trampoline available on supported arches" {
    if (builtin.cpu.arch == .aarch64 or builtin.cpu.arch == .x86_64) {
        try std.testing.expect(available);
    }
}
