//! Death tests: pin that a compiler invariant fires.
//!
//! A violated invariant panics (`base.invariant`), and a panic ends the whole
//! test process, so a test cannot observe one in-process. `expectPanic` re-runs
//! the calling test in a child copy of the test binary (the runner's
//! `--roc-death-test=<test name>` mode). In the child, it calls the trigger;
//! in the parent, it passes when the child panicked with the expected message.
//!
//! Invariants panic only in builds with runtime safety; elsewhere they are
//! `unreachable`, so death tests skip there.

const std = @import("std");
const builtin = @import("builtin");

/// The child's exit code when the trigger returns instead of panicking.
const trigger_returned_exit_code = 86;

/// Failures to start or observe a death-test child.
pub const ChildError = std.mem.Allocator.Error ||
    std.process.ExecutablePathAllocError ||
    std.process.Environ.CreateMapError ||
    std.process.RunError ||
    error{SkipZigTest};

/// A death test's failures: the child could not run, or it did not panic as
/// expected.
pub const Error = ChildError || error{ DeathTestDidNotPanic, DeathTestWrongPanic };

/// Runs `trigger` with `args` in a child copy of the calling test and expects
/// it to panic with a message containing `expected_message`. Call it at most
/// once per test: the child re-runs the whole test up to this call.
pub fn expectPanic(expected_message: []const u8, comptime trigger: anytype, args: anytype) Error!void {
    const gpa = std.testing.allocator;
    var outcome = try runInChild(gpa, trigger, args);
    defer outcome.deinit(gpa);
    switch (outcome) {
        .returned => |stderr| {
            std.debug.print("death test child did not panic; its stderr:\n{s}\n", .{stderr});
            return error.DeathTestDidNotPanic;
        },
        .panicked => |stderr| if (std.mem.find(u8, stderr, expected_message) == null) {
            std.debug.print("death test child stderr lacks \"{s}\":\n{s}\n", .{ expected_message, stderr });
            return error.DeathTestWrongPanic;
        },
    }
}

/// How a death-test child ended, with its stderr.
const Outcome = union(enum) {
    returned: []u8,
    panicked: []u8,

    fn deinit(self: *Outcome, gpa: std.mem.Allocator) void {
        switch (self.*) {
            .returned, .panicked => |stderr| gpa.free(stderr),
        }
    }
};

/// In the parent, runs the calling test in a child and reports how it ended.
/// In the child, calls `trigger` and exits if it returns.
fn runInChild(gpa: std.mem.Allocator, comptime trigger: anytype, args: anytype) ChildError!Outcome {
    if (comptime !builtin.is_test) @compileError("death tests run only in test binaries");
    if (comptime !std.debug.runtime_safety or !std.process.can_spawn) return error.SkipZigTest;
    const runner = @import("root");
    if (comptime !@hasDecl(runner, "isDeathTestChild")) {
        @compileError("death tests need Roc's unit test runner (src/build/unit_test_runner.zig)");
    }

    if (runner.isDeathTestChild()) {
        if (@call(.auto, trigger, args)) {
            std.debug.print("death test trigger returned without panicking\n", .{});
        } else |err| {
            std.debug.print("death test trigger returned error.{t} without panicking\n", .{err});
        }
        std.process.exit(trigger_returned_exit_code);
    }

    const io = std.testing.io;
    const exe_path = try std.process.executablePathAlloc(io, gpa);
    defer gpa.free(exe_path);
    const test_arg = try std.fmt.allocPrint(gpa, "--roc-death-test={s}", .{runner.currentTestName()});
    defer gpa.free(test_arg);

    // The child must not report into the build runner's progress pipe.
    var environ_map = try std.testing.environ.createMap(gpa);
    defer environ_map.deinit();
    _ = environ_map.swapRemove("ZIG_PROGRESS");

    const result = try std.process.run(gpa, io, .{
        .argv = &.{ exe_path, test_arg },
        .environ_map = &environ_map,
    });
    gpa.free(result.stdout);
    return switch (result.term) {
        .exited => |code| if (code == trigger_returned_exit_code or code == 0)
            .{ .returned = result.stderr }
        else
            .{ .panicked = result.stderr },
        .signal, .stopped, .unknown => .{ .panicked = result.stderr },
    };
}

fn violateInvariant(value: u32) error{}!void {
    if (value != 0) @import("invariant.zig").invariant("death test sample invariant saw {d}", .{value});
}

fn returnNormally(value: u32) error{}!void {
    _ = value;
}

test "expectPanic passes when the trigger violates an invariant" {
    try expectPanic("death test sample invariant saw 7", violateInvariant, .{7});
}

test "a death-test child whose trigger returns is reported as returned" {
    var outcome = try runInChild(std.testing.allocator, returnNormally, .{0});
    defer outcome.deinit(std.testing.allocator);
    try std.testing.expect(outcome == .returned);
}
