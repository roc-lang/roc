const std = @import("std");
const utils = @import("utils.zig");
const CSlice = utils.CSlice;
const always_inline = std.builtin.CallOptions.Modifier.always_inline;

const Failure = struct {
    start_line: u32,
    end_line: u32,
    start_col: u16,
    end_col: u16,
};

// BEGIN FAILURES GLOBALS ///////////////////
var failures_mutex = std.Thread.Mutex{};
var failures: [*]Failure = undefined;
var failure_length: usize = 0;
var failure_capacity: usize = 0;
// END FAILURES GLOBALS /////////////////////

pub fn expectFailed(
    start_line: u32,
    end_line: u32,
    start_col: u16,
    end_col: u16,
) void {
    const new_failure = Failure{ .start_line = start_line, .end_line = end_line, .start_col = start_col, .end_col = end_col };

    // Lock the failures mutex before reading from any of the failures globals,
    // and then release the lock once we're done modifying things.
    failures_mutex.lock();
    defer failures_mutex.unlock();

    // If we don't have enough capacity to add a failure, allocate a new failures pointer.
    if (failure_length >= failure_capacity) {
        if (failure_capacity > 0) {
            // We already had previous failures allocated, so try to realloc in order
            // to grow the size in-place without having to memcpy bytes over.
            const old_pointer = failures;
            const old_bytes = failure_capacity * @sizeOf(Failure);

            failure_capacity *= 2;

            const new_bytes = failure_capacity * @sizeOf(Failure);
            const failures_u8 = @ptrCast([*]u8, @alignCast(@alignOf(Failure), failures));
            const raw_pointer = utils.realloc(failures_u8, new_bytes, old_bytes, @alignOf(Failure));

            failures = @ptrCast([*]Failure, @alignCast(@alignOf(Failure), raw_pointer));

            // If realloc wasn't able to expand in-place (that is, it returned a different pointer),
            // then copy the data into the new pointer and dealloc the old one.
            if (failures != old_pointer) {
                const old_pointer_u8 = @ptrCast([*]u8, old_pointer);
                utils.memcpy(@ptrCast([*]u8, failures), old_pointer_u8, old_bytes);
                utils.dealloc(old_pointer_u8, @alignOf(Failure));
            }
        } else {
            // We've never had any failures before, so allocate the failures for the first time.
            failure_capacity = 10;

            const raw_pointer = utils.alloc(failure_capacity * @sizeOf(Failure), @alignOf(Failure));

            failures = @ptrCast([*]Failure, @alignCast(@alignOf(Failure), raw_pointer));
        }
    }

    failures[failure_length] = new_failure;
    failure_length += 1;
}

pub fn expectFailedC(
    start_line: u32,
    end_line: u32,
    start_col: u16,
    end_col: u16,
) callconv(.C) void {
    return @call(.{ .modifier = always_inline }, expectFailed, .{ start_line, end_line, start_col, end_col });
}

pub fn getExpectFailures() []Failure {
    failures_mutex.lock();
    defer failures_mutex.unlock();

    if (failure_length > 0) {
        // defensively clone failures, in case someone modifies the originals after the mutex has been released.
        const num_bytes = failure_length * @sizeOf(Failure);
        // TODO handle the possibility of alloc failing
        const raw_clones = utils.alloc(num_bytes, @alignOf(Failure)) orelse unreachable;

        utils.memcpy(raw_clones, @ptrCast([*]u8, failures), num_bytes);

        const clones = @ptrCast([*]Failure, @alignCast(@alignOf(Failure), raw_clones));

        return clones[0..failure_length];
    } else {
        return failures[0..0];
    }
}

pub fn getExpectFailuresC() callconv(.C) CSlice {
    var bytes = @ptrCast(*anyopaque, failures);

    return .{ .pointer = bytes, .len = failure_length };
}

pub fn deinitFailures() void {
    failures_mutex.lock();
    defer failures_mutex.unlock();

    utils.dealloc(@ptrCast([*]u8, failures), @alignOf(Failure));
    failure_length = 0;
}

pub fn deinitFailuresC() callconv(.C) void {
    return @call(.{ .modifier = always_inline }, deinitFailures, .{});
}

test "expectFailure does something" {
    defer deinitFailures();

    var fails = getExpectFailures();
    try std.testing.expectEqual(fails.len, 0);

    expectFailed(1, 2, 3, 4);

    fails = getExpectFailures();
    try std.testing.expectEqual(fails.len, 1);
    utils.dealloc(@ptrCast([*]u8, fails.ptr), @alignOf([*]Failure));

    const what_it_should_look_like = Failure{ .start_line = 1, .end_line = 2, .start_col = 3, .end_col = 4 };

    fails = getExpectFailures();
    try std.testing.expectEqual(fails[0], what_it_should_look_like);
    utils.dealloc(@ptrCast([*]u8, fails.ptr), @alignOf([*]Failure));
}
