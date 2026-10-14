//! What the compiler is doing, recorded per thread so that an internal
//! compiler error (ICE) can say so.
//!
//! Each phase pushes a small `Frame` when it starts a unit of work (a module's
//! parse, canonicalization or type check, a post-check stage, one procedure's
//! code generation, one interpreter evaluation) and pops it when the unit
//! ends. The frames live in fixed thread-local storage: pushing and popping
//! copy a small struct and never allocate, lock, or touch another thread.
//!
//! When the compiler panics, or a fatal signal reaches the compiler's own
//! handler, the panicking thread's frames are printed ahead of the stack
//! trace together with the command that reproduces the crash. Only the
//! panicking thread's frames are printed; other threads' work is unrelated.
//!
//! Strings in a frame are borrowed. A frame is live only while the code that
//! pushed it is running, and that code keeps the strings alive for at least
//! that long.

const std = @import("std");
const builtin = @import("builtin");

/// The kind of work a frame describes.
pub const Phase = enum {
    parse,
    canonicalize,
    type_check,
    post_check,
    codegen,
    interpret,

    /// The phase's name as printed in a report.
    pub fn label(self: Phase) []const u8 {
        return switch (self) {
            .parse => "parse",
            .canonicalize => "canonicalize",
            .type_check => "type-check",
            .post_check => "post-check",
            .codegen => "codegen",
            .interpret => "interpret",
        };
    }
};

/// A procedure a frame is working on. `names` resolves the procedure's source
/// name only when a report is printed, so recording a procedure costs nothing
/// beyond storing its id.
pub const Proc = struct {
    id: u32,
    names: ?NameLookup = null,
};

/// Resolves a procedure id to its source name. Called only while printing a
/// panic report, never on a fatal signal, because a signal may have
/// interrupted the code that owns the names.
pub const NameLookup = struct {
    context: *const anyopaque,
    nameFn: *const fn (context: *const anyopaque, id: u32) ?[]const u8,
};

/// One unit of in-flight compiler work.
pub const Frame = struct {
    phase: Phase,
    /// The module the work belongs to, for per-module phases.
    module_name: ?[]const u8 = null,
    /// The filesystem path of `module_name`.
    module_path: ?[]const u8 = null,
    /// The named step within the phase, such as a post-check stage or a
    /// backend.
    stage: ?[]const u8 = null,
    /// The procedure being processed, for per-procedure work.
    proc: ?Proc = null,
};

/// How many frames each thread stores. Real nesting stays far below this; a
/// push beyond it is counted but not stored, and the report says how many
/// frames it could not show.
pub const max_depth = 16;

threadlocal var frames: [max_depth]Frame = undefined;
threadlocal var depth: usize = 0;
threadlocal var thread_role: ?ThreadRole = null;
threadlocal var reporting: bool = false;

/// The command line the process was started with. Recorded by the binary's
/// entry point before any other thread exists, cleared before the arguments
/// are freed, and read only while reporting a crash.
var invocation_args: []const []const u8 = &.{};
var invocation: std.atomic.Value(?*const []const []const u8) = .init(null);

/// What a thread is for, printed next to its id in a report.
pub const ThreadRole = struct {
    name: []const u8,
    index: ?usize = null,
};

/// Keeps `enter` and `leave` paired. `leave` must run exactly once, in the
/// reverse order of `enter`; callers write `defer guard.leave();`.
pub const Guard = struct {
    index: usize,

    /// Pop the frame `enter` pushed.
    pub fn leave(self: Guard) void {
        std.debug.assert(depth == self.index + 1);
        depth = self.index;
    }
};

/// Push `frame` for the duration of a unit of work.
pub fn enter(frame: Frame) Guard {
    const index = depth;
    if (index < max_depth) frames[index] = frame;
    depth = index + 1;
    if (comptime forced_ice_supported) {
        if (forced_ice) |forced| {
            if (forced.matches(frame)) {
                std.debug.panic("forced internal compiler error requested by {s}={s}", .{ forced_ice_env_var, forced.spec });
            }
        }
    }
    return .{ .index = index };
}

/// The innermost frame on this thread, if any. Work that one thread hands to
/// another pushes a copy of this on the receiving thread, so the receiving
/// thread's report names the work it was doing on the sender's behalf.
pub fn innermost() ?Frame {
    if (depth == 0) return null;
    return frames[@min(depth, max_depth) - 1];
}

/// Name the current thread in reports.
pub fn setThreadRole(role: ThreadRole) void {
    thread_role = role;
}

/// Record the command line that reproduces this process, before any thread
/// other than the caller exists.
pub fn setInvocation(args: []const []const u8) void {
    invocation_args = args;
    invocation.store(&invocation_args, .release);
}

/// Stop reporting the recorded command line, before its arguments are freed.
pub fn clearInvocation() void {
    invocation.store(null, .release);
}

/// The environment variable that makes a Debug build of the compiler panic on
/// purpose, so tests can observe the crash report of each phase. Its value is
/// a phase name (`parse`, `canonicalize`, `type_check`, `post_check`,
/// `codegen`, `interpret`), optionally followed by `:` and the module name (for
/// per-module phases) or stage name (for the others) the panic waits for.
pub const forced_ice_env_var = "ROC_DEBUG_FORCE_ICE";

/// Whether this build honors `forced_ice_env_var`.
pub const forced_ice_supported = builtin.mode == .debug;

const ForcedIce = struct {
    spec: []const u8,
    phase: Phase,
    name: ?[]const u8,

    fn matches(self: ForcedIce, frame: Frame) bool {
        if (frame.phase != self.phase) return false;
        const wanted = self.name orelse return true;
        const actual = frame.module_name orelse frame.stage orelse return false;
        return std.mem.eql(u8, wanted, actual);
    }
};

var forced_ice: if (forced_ice_supported) ?ForcedIce else void = if (forced_ice_supported) null else {};

/// Parse a `forced_ice_env_var` value and arm it. Returns false when `spec`
/// names no phase. `spec` must stay valid for the rest of the process and must
/// be armed before any thread other than the caller exists.
pub fn armForcedIce(spec: []const u8) bool {
    if (comptime !forced_ice_supported) return false;
    const parsed = parseForcedIce(spec) orelse return false;
    forced_ice = parsed;
    return true;
}

fn parseForcedIce(spec: []const u8) ?ForcedIce {
    const separator = std.mem.findScalar(u8, spec, ':');
    const phase_name = if (separator) |i| spec[0..i] else spec;
    const phase = std.meta.stringToEnum(Phase, phase_name) orelse return null;
    const name: ?[]const u8 = if (separator) |i| spec[i + 1 ..] else null;
    if (name) |n| if (n.len == 0) return null;
    return .{ .spec = spec, .phase = phase, .name = name };
}

/// Everything a report prints, gathered so rendering is a pure function.
pub const Report = struct {
    frames: []const Frame,
    /// Frames pushed beyond `max_depth`, which were not stored.
    dropped: usize = 0,
    thread_id: ?u64 = null,
    thread_role: ?ThreadRole = null,
    invocation: ?[]const []const u8 = null,
    cwd: ?[]const u8 = null,
    /// Whether procedure names may be resolved through `Proc.names`.
    resolve_names: bool = true,
};

/// Write the report block that precedes a crash's stack trace.
pub fn writeReport(w: *std.Io.Writer, report: Report) std.Io.Writer.Error!void {
    try w.writeAll(
        \\
        \\Roc hit an internal compiler error. This is a bug in the compiler, not in your code.
        \\
        \\
    );
    try writeContext(w, report);
    try w.writeAll("\nPlease report this at https://github.com/roc-lang/roc/issues and include\n");
    try w.writeAll(if (report.invocation != null)
        "the command above along with the panic message and stack trace below.\n\n"
    else
        "the panic message and stack trace below.\n\n");
}

/// Write the thread's work in progress and the reproducing command: the part
/// of a report that a fatal-signal message embeds in its own text.
pub fn writeContext(w: *std.Io.Writer, report: Report) std.Io.Writer.Error!void {
    if (report.frames.len == 0 and report.dropped == 0) {
        try w.writeAll("No compiler work was recorded on ");
        try writeThread(w, report);
        try w.writeAll(".\n");
    } else {
        try w.writeAll("Work in progress on ");
        try writeThread(w, report);
        try w.writeAll(", outermost first:\n");
        for (report.frames) |frame| try writeFrame(w, frame, report.resolve_names);
        if (report.dropped != 0) {
            try w.print("  ({d} deeper frames were not recorded)\n", .{report.dropped});
        }
    }

    if (report.invocation) |args| {
        try w.writeAll("\nReproduce with:\n  ");
        if (report.cwd) |cwd| {
            try w.writeAll("cd ");
            try writeShellWord(w, cwd);
            try w.writeAll(" && ");
        }
        for (args, 0..) |arg, i| {
            if (i != 0) try w.writeByte(' ');
            try writeShellWord(w, arg);
        }
        try w.writeByte('\n');
    }
}

fn writeThread(w: *std.Io.Writer, report: Report) std.Io.Writer.Error!void {
    try w.writeAll("thread");
    if (report.thread_id) |id| try w.print(" {d}", .{id});
    if (report.thread_role) |role| {
        try w.print(" ({s}", .{role.name});
        if (role.index) |index| try w.print(" {d}", .{index});
        try w.writeByte(')');
    }
}

fn writeFrame(w: *std.Io.Writer, frame: Frame, resolve_names: bool) std.Io.Writer.Error!void {
    const label = frame.phase.label();
    try w.print("  {s}", .{label});
    try w.splatByteAll(' ', max_label_len + 2 - label.len);
    var separate = false;
    if (frame.module_name) |name| {
        try w.print("module {s}", .{name});
        separate = true;
    }
    if (frame.module_path) |path| {
        if (separate) try w.writeByte(' ');
        try w.print("({s})", .{path});
        separate = true;
    }
    if (frame.stage) |stage| {
        if (separate) try w.writeAll(", ");
        try w.writeAll(stage);
        separate = true;
    }
    if (frame.proc) |proc| {
        if (separate) try w.writeAll(", ");
        try w.print("proc {d}", .{proc.id});
        if (resolve_names) {
            if (proc.names) |names| {
                if (names.nameFn(names.context, proc.id)) |name| try w.print(" ({s})", .{name});
            }
        }
    }
    try w.writeByte('\n');
}

const max_label_len = blk: {
    var longest: usize = 0;
    for (std.enums.values(Phase)) |phase| longest = @max(longest, phase.label().len);
    break :blk longest;
};

/// Write `word` so a POSIX shell reads it back unchanged.
fn writeShellWord(w: *std.Io.Writer, word: []const u8) std.Io.Writer.Error!void {
    const plain = word.len != 0 and for (word) |c| {
        switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9', '_', '-', '.', '/', '=', ':', ',', '+', '@', '%' => {},
            else => break false,
        }
    } else true;
    if (plain) return w.writeAll(word);
    try w.writeByte('\'');
    for (word) |c| {
        if (c == '\'') try w.writeAll("'\\''") else try w.writeByte(c);
    }
    try w.writeByte('\'');
}

/// The calling thread's report. `cwd_buf` holds the working directory the
/// report names.
pub fn currentThreadReport(cwd_buf: []u8, resolve_names: bool) Report {
    const cwd: ?[]const u8 = if (comptime builtin.os.tag == .freestanding or builtin.os.tag == .wasi)
        null
    else if (std.process.currentPath(std.Options.debug_io, cwd_buf)) |len|
        cwd_buf[0..len]
    else |_|
        null;
    return .{
        .frames = frames[0..@min(depth, max_depth)],
        .dropped = depth -| max_depth,
        .thread_id = if (comptime builtin.single_threaded or builtin.os.tag == .freestanding) null else std.Thread.getCurrentId(),
        .thread_role = thread_role,
        .invocation = if (invocation.load(.acquire)) |args| args.* else null,
        .cwd = cwd,
        .resolve_names = resolve_names,
    };
}

/// Print the calling thread's report to stderr, for a panic handler to call
/// before it prints the panic message and stack trace.
///
/// The report is written under the stderr lock, so a concurrent panic on
/// another thread cannot interleave its report with this one.
pub fn reportPanicToStderr() void {
    if (comptime builtin.os.tag == .freestanding) return;
    // A panic while reporting skips the report.
    if (reporting) return;
    reporting = true;
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const report = currentThreadReport(&cwd_buf, true);
    var buffer: [512]u8 = undefined;
    const stderr = std.debug.lockStderr(&buffer);
    // The panic handler takes the lock again to print the trace, and another
    // thread panicking at the same time reports through this lock too.
    defer std.debug.unlockStderr();
    const w = &stderr.file_writer.interface;
    writeReport(w, report) catch return;
    w.flush() catch return;
}

/// Render the calling thread's context (see `writeContext`) into `buffer` for
/// a fatal-signal handler, which writes the bytes with its own signal-safe
/// output. A context longer than `buffer` is truncated. Procedure names are
/// not resolved, because the signal may have interrupted the code that owns
/// them.
pub fn renderSignalContext(buffer: []u8) []const u8 {
    if (reporting) return buffer[0..0];
    reporting = true;
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const report = currentThreadReport(&cwd_buf, false);
    var w: std.Io.Writer = .fixed(buffer);
    writeContext(&w, report) catch {};
    return w.buffered();
}

/// The number of frames pushed on this thread, including dropped ones.
pub fn currentDepth() usize {
    return depth;
}

fn testNames(_: *const anyopaque, id: u32) ?[]const u8 {
    return if (id == 7) "main!" else null;
}

test "report names every frame, the thread, and the reproducing command" {
    const sentinel: u8 = 0;
    const report_frames = [_]Frame{
        .{ .phase = .type_check, .module_name = "Json.Decode", .module_path = "/src/Json/Decode.roc" },
        .{ .phase = .post_check, .stage = "lambda_solve" },
        .{ .phase = .codegen, .stage = "dev backend", .proc = .{ .id = 7, .names = .{ .context = &sentinel, .nameFn = testNames } } },
        .{ .phase = .interpret, .proc = .{ .id = 9, .names = .{ .context = &sentinel, .nameFn = testNames } } },
    };
    var buffer: [2048]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buffer);
    try writeReport(&w, .{
        .frames = &report_frames,
        .thread_id = 42,
        .thread_role = .{ .name = "compile worker", .index = 3 },
        .invocation = &.{ "roc", "check", "my app/main.roc", "--opt=dev" },
        .cwd = "/home/me",
    });
    try std.testing.expectEqualStrings(
        \\
        \\Roc hit an internal compiler error. This is a bug in the compiler, not in your code.
        \\
        \\Work in progress on thread 42 (compile worker 3), outermost first:
        \\  type-check    module Json.Decode (/src/Json/Decode.roc)
        \\  post-check    lambda_solve
        \\  codegen       dev backend, proc 7 (main!)
        \\  interpret     proc 9
        \\
        \\Reproduce with:
        \\  cd /home/me && roc check 'my app/main.roc' --opt=dev
        \\
        \\Please report this at https://github.com/roc-lang/roc/issues and include
        \\the command above along with the panic message and stack trace below.
        \\
        \\
    , w.buffered());
}

test "report without frames or invocation names only the thread" {
    var buffer: [1024]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buffer);
    try writeReport(&w, .{ .frames = &.{}, .thread_id = 5, .thread_role = .{ .name = "main" } });
    try std.testing.expectEqualStrings(
        \\
        \\Roc hit an internal compiler error. This is a bug in the compiler, not in your code.
        \\
        \\No compiler work was recorded on thread 5 (main).
        \\
        \\Please report this at https://github.com/roc-lang/roc/issues and include
        \\the panic message and stack trace below.
        \\
        \\
    , w.buffered());
}

test "shell words quote exactly the characters a shell would reinterpret" {
    var buffer: [256]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buffer);
    for ([_][]const u8{ "plain/path.roc", "", "it's", "a b", "$HOME" }) |word| {
        try writeShellWord(&w, word);
        try w.writeByte('|');
    }
    try std.testing.expectEqualStrings("plain/path.roc|''|'it'\\''s'|'a b'|'$HOME'|", w.buffered());
}

test "frames pushed beyond max_depth are counted and reported as dropped" {
    var guards: [max_depth + 3]Guard = undefined;
    for (&guards, 0..) |*guard, i| {
        guard.* = enter(.{ .phase = .codegen, .proc = .{ .id = @intCast(i) } });
    }
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const report = currentThreadReport(&cwd_buf, false);
    try std.testing.expectEqual(@as(usize, max_depth), report.frames.len);
    try std.testing.expectEqual(@as(usize, 3), report.dropped);
    try std.testing.expectEqual(@as(u32, max_depth - 1), report.frames[max_depth - 1].proc.?.id);

    var buffer: [4096]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buffer);
    try writeReport(&w, report);
    try std.testing.expect(std.mem.find(u8, w.buffered(), "  (3 deeper frames were not recorded)\n") != null);

    var i = guards.len;
    while (i > 0) {
        i -= 1;
        guards[i].leave();
    }
    try std.testing.expectEqual(@as(usize, 0), currentDepth());
}

fn enterThenReturnEarly(fail: bool) error{ Failed, TestExpectedEqual }!void {
    const guard = enter(.{ .phase = .parse, .module_name = "A" });
    defer guard.leave();
    {
        const inner = enter(.{ .phase = .post_check, .stage = "arc" });
        defer inner.leave();
        if (fail) return error.Failed;
        try std.testing.expectEqual(@as(usize, 2), currentDepth());
    }
}

test "guards keep the depth balanced across early and error returns" {
    try enterThenReturnEarly(false);
    try std.testing.expectEqual(@as(usize, 0), currentDepth());
    try std.testing.expectError(error.Failed, enterThenReturnEarly(true));
    try std.testing.expectEqual(@as(usize, 0), currentDepth());
    try std.testing.expect(innermost() == null);
}

test "innermost returns a copy of the newest frame" {
    const outer = enter(.{ .phase = .type_check, .module_name = "Main" });
    defer outer.leave();
    const inner = enter(.{ .phase = .post_check, .stage = "monotype" });
    defer inner.leave();
    const frame = innermost().?;
    try std.testing.expectEqual(Phase.post_check, frame.phase);
    try std.testing.expectEqualStrings("monotype", frame.stage.?);
}

test "forced ICE specs name a phase and optionally a module or stage" {
    const by_module = parseForcedIce("type_check:Helper").?;
    try std.testing.expect(by_module.matches(.{ .phase = .type_check, .module_name = "Helper" }));
    try std.testing.expect(!by_module.matches(.{ .phase = .type_check, .module_name = "Main" }));
    try std.testing.expect(!by_module.matches(.{ .phase = .parse, .module_name = "Helper" }));

    const by_stage = parseForcedIce("post_check:arc").?;
    try std.testing.expect(by_stage.matches(.{ .phase = .post_check, .stage = "arc" }));
    try std.testing.expect(!by_stage.matches(.{ .phase = .post_check, .stage = "monotype" }));

    const any = parseForcedIce("codegen").?;
    try std.testing.expect(any.matches(.{ .phase = .codegen, .proc = .{ .id = 1 } }));

    try std.testing.expect(parseForcedIce("typecheck") == null);
    try std.testing.expect(parseForcedIce("parse:") == null);
}
