//! The bounded tasks a patch link fans its output writes out over
//! (`patch_link.zig`, `macho/PatchLink.zig`): byte copies batched by size,
//! and per-image ranges of entries, run on the compilation's post-check
//! executor or inline without one.
const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;
const Executor = base.post_check_task_executor.Executor;

/// The most bytes one copy task moves.
pub const copy_chunk: usize = 256 * 1024;
/// The most sites or symbols one task handles.
pub const entry_chunk: u32 = 8192;

/// One run of bytes to copy into the output.
pub const Copy = struct { from: []const u8, to: u64 };

/// A range of one image's entries.
pub const Range = struct { image: u32, first: u32, count: u32 };

/// Add the copy of `from` to output offset `to`, in pieces of at most
/// `copy_chunk` bytes.
pub fn addCopies(gpa: Allocator, list: *std.ArrayList(Copy), from: []const u8, to: u64) Allocator.Error!void {
    var at: usize = 0;
    while (at < from.len) : (at += copy_chunk) {
        const len = @min(copy_chunk, from.len - at);
        try list.append(gpa, .{ .from = from[at..][0..len], .to = to + at });
    }
}

/// The ranges of `count` entries of `image`, each at most `entry_chunk`
/// long.
pub fn addRanges(gpa: Allocator, ranges: *std.ArrayList(Range), image: u32, count: usize) Allocator.Error!void {
    var first: u32 = 0;
    while (first < count) : (first += entry_chunk) {
        try ranges.append(gpa, .{ .image = image, .first = first, .count = @min(entry_chunk, @as(u32, @intCast(count)) - first) });
    }
}

/// Copy `copies` into `out`, returning the bytes written.
pub fn runCopies(out: []u8, copies: []const Copy) u64 {
    var bytes: u64 = 0;
    for (copies) |copy| {
        @memcpy(out[@intCast(copy.to)..][0..copy.from.len], copy.from);
        bytes += copy.from.len;
    }
    return bytes;
}

/// Group `copies` into batches of about `copy_chunk` bytes each.
pub fn batches(gpa: Allocator, copies: []const Copy) Allocator.Error![]const []const Copy {
    var out: std.ArrayList([]const Copy) = .empty;
    errdefer out.deinit(gpa);
    var batch_first: usize = 0;
    var batch_bytes: usize = 0;
    for (copies, 0..) |copy, at| {
        batch_bytes += copy.from.len;
        if (batch_bytes < copy_chunk and at + 1 < copies.len) continue;
        try out.append(gpa, copies[batch_first .. at + 1]);
        batch_first = at + 1;
        batch_bytes = 0;
    }
    return out.toOwnedSlice(gpa);
}

/// Run every task, on the executor when there is one. `Task` declares
/// `fn run(opaque_task: *anyopaque, worker: Worker) ?*anyopaque`.
pub fn runTasks(gpa: Allocator, executor: ?Executor, comptime Task: type, tasks: []Task) Allocator.Error!void {
    const pool = executor orelse {
        for (tasks) |*task| _ = Task.run(task, .{ .id = 0, .allocator = gpa, .scratch = gpa, .lane_state = undefined });
        return;
    };
    const work = try gpa.alloc(base.post_check_task_executor.Task, tasks.len);
    defer gpa.free(work);
    for (tasks, work, 0..) |*task, *item, index| item.* = .{ .id = index, .context = task, .run = Task.run };
    const completions = try gpa.alloc(base.post_check_task_executor.Completion, tasks.len);
    defer gpa.free(completions);
    try pool.run(work, completions);
}
