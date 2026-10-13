//! setjmp/longjmp boundary around Roc code that may crash.
//!
//! A host env owns a `jmp_buf` and an `active_jmp_buf` pointer; its
//! `roc_crashed` callback longjmps to the active buffer. A boundary installs
//! the env's buffer for the duration of one guarded run and restores the
//! previous one afterwards, so boundaries nest.

const std = @import("std");
const base = @import("base");
const sljmp = @import("sljmp");

const signal_handler = base.signal_handler;

pub const JmpBuf = sljmp.JmpBuf;
pub const setjmp = sljmp.setjmp;
pub const longjmp = sljmp.longjmp;

/// The message recorded when Roc code overflows its stack.
pub const STACK_OVERFLOW_MESSAGE = "This Roc program overflowed its stack memory. This usually means there is very deep or infinite recursion somewhere in the code.";

/// A boundary over `Env`, which must hold `jmp_buf: JmpBuf` and
/// `active_jmp_buf: ?*JmpBuf`. With `catches_stack_overflow`, the boundary
/// also registers itself as the thread's escape route for a native stack
/// overflow inside the guarded code: the fault handler longjmps back here and
/// `Env.noteStackOverflow` records the overflow as this run's crash instead of
/// the overflow taking the process down.
pub fn CrashBoundary(comptime Env: type, comptime catches_stack_overflow: bool) type {
    return struct {
        const Self = @This();

        env: *Env,
        prev_jmp_buf: ?*JmpBuf,
        recovery: signal_handler.StackOverflowRecovery = undefined,
        prev_recovery: ?*const signal_handler.StackOverflowRecovery = null,
        recovery_registered: bool = false,

        pub fn init(env: *Env) Self {
            const prev_jmp_buf = env.active_jmp_buf;
            if (sljmp.supported) env.active_jmp_buf = &env.jmp_buf;
            return .{ .env = env, .prev_jmp_buf = prev_jmp_buf };
        }

        pub fn deinit(self: *Self) void {
            if (self.recovery_registered) {
                _ = signal_handler.setStackOverflowRecovery(self.prev_recovery);
                self.recovery_registered = false;
            }
            if (sljmp.supported) self.env.active_jmp_buf = self.prev_jmp_buf;
        }

        pub inline fn set(self: *Self) c_int {
            if (!sljmp.supported) return 0;
            const sj = setjmp(&self.env.jmp_buf);
            if (catches_stack_overflow) {
                if (sj == 0) {
                    self.recovery = .{
                        .context = @ptrCast(self.env),
                        .escape = &escapeStackOverflow,
                    };
                    self.prev_recovery = signal_handler.setStackOverflowRecovery(&self.recovery);
                    self.recovery_registered = true;
                } else if (sj == signal_handler.stack_overflow_longjmp_value) {
                    signal_handler.restoreStackGuardAfterOverflow();
                    self.env.noteStackOverflow();
                }
            }
            return sj;
        }

        fn escapeStackOverflow(context: *anyopaque) noreturn {
            const env: *Env = @ptrCast(@alignCast(context));
            if (sljmp.supported) {
                if (env.active_jmp_buf) |active_jmp_buf| {
                    env.active_jmp_buf = null;
                    longjmp(active_jmp_buf, signal_handler.stack_overflow_longjmp_value);
                }
            }
            std.process.exit(134);
        }
    };
}
