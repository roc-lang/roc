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
pub const STACK_OVERFLOW_MESSAGE = base.stack_overflow.program_message;

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

        /// Run `call(context)` inside this boundary on the calling thread's
        /// evaluation stack (`base.eval_stack`) and return what `set` returned
        /// there: 0 when `call` returned, otherwise the value the boundary was
        /// escaped with. The boundary is set on the evaluation stack, so an
        /// escape from a crash or a stack overflow lands there and the run
        /// returns to the caller's stack through the switch it entered by.
        pub fn run(self: *Self, comptime Context: type, context: *Context, comptime call: fn (*Context) void) base.eval_stack.Error!c_int {
            const Guarded = struct {
                boundary: *Self,
                context: *Context,
                jumped: c_int = 0,

                fn body(guarded: *@This()) void {
                    const jumped = guarded.boundary.set();
                    guarded.jumped = jumped;
                    // This frame owns the recovery point. Tail-calling the body
                    // would discard it before code that can longjmp runs.
                    if (jumped == 0) @call(.never_tail, call, .{guarded.context});
                }
            };
            var guarded: Guarded = .{ .boundary = self, .context = context };
            try base.eval_stack.run(Guarded, &guarded, Guarded.body);
            return guarded.jumped;
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

test "crash boundary retains its recovery frame across a tail-position callback" {
    if (!sljmp.supported or !base.eval_stack.supported) return error.SkipZigTest;
    const Env = struct {
        jmp_buf: JmpBuf = undefined,
        active_jmp_buf: ?*JmpBuf = null,
        calls: usize = 0,
    };
    const Call = struct {
        env: *Env,
        entry: *const fn (*Env) callconv(.c) void,

        fn invoke(self: *@This()) void {
            self.entry(self.env);
        }

        noinline fn crash(env: *Env) callconv(.c) void {
            // Overwrite a discarded caller frame so a return into it cannot
            // accidentally succeed because the bytes happened to survive.
            // Volatile stores keep the overwrite even though nothing reads it.
            var scratch: [64]u64 = undefined;
            const volatile_scratch: *volatile [64]u64 = &scratch;
            for (0..scratch.len) |index| volatile_scratch[index] = 0xabababababababab;
            env.calls += 1;
            longjmp(env.active_jmp_buf.?, 7);
        }
    };
    var env: Env = .{};
    var context: Call = .{ .env = &env, .entry = &Call.crash };
    for (0..3) |_| {
        var boundary = CrashBoundary(Env, false).init(&env);
        defer boundary.deinit();
        try std.testing.expectEqual(@as(c_int, 7), try boundary.run(Call, &context, Call.invoke));
    }
    try std.testing.expectEqual(@as(usize, 3), env.calls);
    try std.testing.expect(env.active_jmp_buf == null);
}
