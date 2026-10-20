//! Bounded executor waves shared by the native and wasm code generators.
//!
//! A wave runs one callback per job, on the executor's workers or inline on a
//! sequential lane when there is no executor, through the same callback path
//! either way. Admission stops on the first observed OOM, but every accepted
//! callback finishes before the wave returns, so the caller may destroy job
//! owners only after `execute` returns.

const std = @import("std");
const base = @import("base");

const tasks = base.post_check_task_executor;
const Allocator = std.mem.Allocator;

/// Saturating admission counters, including those of a failed wave.
pub const Admission = struct {
    submitted: u64 = 0,
    /// The largest number of callbacks one wave accepted.
    peak: u64 = 0,
};

/// Run every job of `jobs` whose `skipped` is false. `Job` declares
/// `fn skipped(*const Job) bool`, an `oom: bool` field its callback sets on
/// allocation failure, and `fn execute(*anyopaque, tasks.Worker) ?*anyopaque`.
/// Without an executor, jobs run in order on `sequential_lane`, which outlives
/// the waves so lane state carries from one wave to the next as on a worker.
pub fn execute(
    comptime Job: type,
    allocator: Allocator,
    jobs: []Job,
    executor: ?tasks.Executor,
    sequential_lane: *tasks.LaneState,
    admission: *Admission,
) Allocator.Error!void {
    var failed = false;
    if (executor) |exec| {
        var session = exec.begin();
        var next: usize = 0;
        var accepted: usize = 0;
        while (next < jobs.len or session.outstanding != 0) {
            while (!failed and next < jobs.len and session.canSubmit()) {
                if (jobs[next].skipped()) {
                    next += 1;
                    continue;
                }
                session.submit(.{ .id = next, .context = &jobs[next], .run = Job.execute }) catch {
                    failed = true;
                    break;
                };
                next += 1;
                accepted += 1;
                admission.submitted +|= 1;
                admission.peak = @max(admission.peak, accepted);
            }
            if (session.outstanding != 0) {
                const completion = session.receive();
                if (jobs[completion.id].oom) failed = true;
            } else if (failed) break;
        }
        session.end();
    } else {
        var scratch = base.SingleThreadArena.init(allocator);
        defer scratch.deinit();
        var accepted: usize = 0;
        for (jobs) |*job| {
            if (job.skipped()) continue;
            _ = scratch.reset(.retain_capacity);
            admission.submitted +|= 1;
            accepted += 1;
            admission.peak = @max(admission.peak, accepted);
            _ = Job.execute(job, .{ .id = 0, .allocator = allocator, .scratch = scratch.allocator(), .lane_state = sequential_lane });
            if (job.oom) {
                failed = true;
                break;
            }
        }
    }
    if (failed) return error.OutOfMemory;
}
