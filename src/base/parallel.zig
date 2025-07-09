const std = @import("std");
const Allocator = std.mem.Allocator;
const Thread = std.Thread;

/// Maximum number of threads that can be spawned
const MAX_THREADS = 128;

/// Configuration for parallel execution
pub const ParallelConfig = struct {
    /// Maximum number of threads to spawn (0 = auto-detect from CPU count)
    max_threads: usize = 0,

    /// Force single-threaded execution
    single_threaded: bool = false,
};

/// Result of parallel execution
pub const ParallelResult = struct {
    success: usize,
    failed: usize,
};

/// Worker thread function signature
/// Takes: allocator, work_item -> bool (success/failure)
pub fn WorkerFn(comptime T: type) type {
    return *const fn (allocator: Allocator, work_item: T) bool;
}

/// Internal worker thread context
fn WorkerContext(comptime T: type) type {
    return struct {
        allocator: Allocator,
        work_items: []const T,
        index: *std.atomic.Value(usize),
        success_count: *std.atomic.Value(usize),
        failed_count: *std.atomic.Value(usize),
        worker_fn: WorkerFn(T),
        thread_id: usize,
    };
}

/// Worker thread implementation using work-stealing
fn workerThread(comptime T: type, ctx: WorkerContext(T)) void {
    while (true) {
        const i = ctx.index.fetchAdd(1, .seq_cst);
        if (i >= ctx.work_items.len) break;

        const work_item = ctx.work_items[i];
        const success = ctx.worker_fn(ctx.allocator, work_item);

        if (success) {
            _ = ctx.success_count.fetchAdd(1, .seq_cst);
        } else {
            _ = ctx.failed_count.fetchAdd(1, .seq_cst);
        }
    }
}

/// Process work items in parallel across multiple threads
///
/// Generic function that:
/// 1. Takes an array of work items of type T
/// 2. Calls worker_fn for each item on available threads
/// 3. Returns success/failure counts
///
/// Example usage:
/// ```
/// const MyWorkItem = struct { path: []const u8 };
///
/// fn processItem(allocator: Allocator, item: MyWorkItem) bool {
///     // Process the work item
///     std.log.info("processing {s}", .{item.path});
///     return true; // or false on failure
/// }
///
/// const result = try processParallel(MyWorkItem, allocator, work_items, processItem, .{});
/// ```
pub fn processParallel(
    comptime T: type,
    allocator: Allocator,
    work_items: []const T,
    worker_fn: WorkerFn(T),
    config: ParallelConfig,
) !ParallelResult {
    if (work_items.len == 0) {
        return ParallelResult{ .success = 0, .failed = 0 };
    }

    var success_count = std.atomic.Value(usize).init(0);
    var failed_count = std.atomic.Value(usize).init(0);

    if (config.single_threaded) {
        // Process everything in main thread
        var index = std.atomic.Value(usize).init(0);
        const ctx = WorkerContext(T){
            .allocator = allocator,
            .work_items = work_items,
            .index = &index,
            .success_count = &success_count,
            .failed_count = &failed_count,
            .worker_fn = worker_fn,
            .thread_id = 0,
        };
        workerThread(T, ctx);
    } else {
        // Process in parallel using work-stealing
        const cpu_count = std.Thread.getCpuCount() catch 1;
        const max_threads = if (config.max_threads == 0) cpu_count else config.max_threads;
        const thread_count = @min(@min(max_threads, MAX_THREADS), work_items.len);

        var index = std.atomic.Value(usize).init(0);
        var threads: [MAX_THREADS]Thread = undefined;

        // Start worker threads
        for (0..thread_count) |i| {
            const ctx = WorkerContext(T){
                .allocator = allocator,
                .work_items = work_items,
                .index = &index,
                .success_count = &success_count,
                .failed_count = &failed_count,
                .worker_fn = worker_fn,
                .thread_id = i,
            };
            threads[i] = try Thread.spawn(.{}, workerThread, .{ T, ctx });
        }

        // Wait for all threads to complete
        for (threads[0..thread_count]) |thread| {
            thread.join();
        }
    }

    return ParallelResult{
        .success = success_count.load(.seq_cst),
        .failed = failed_count.load(.seq_cst),
    };
}

/// Convenience function for simple parallel map operations
/// Maps each input item to an output item using the provided function
pub fn parallelMap(
    comptime T: type,
    comptime R: type,
    allocator: Allocator,
    inputs: []const T,
    outputs: []R,
    map_fn: fn (T) R,
    config: ParallelConfig,
) !void {
    if (inputs.len != outputs.len) {
        return error.LengthMismatch;
    }

    const MapItem = struct { input: T, output_ptr: *R };

    const MapWorker = struct {
        fn worker(alloc: Allocator, item: MapItem) bool {
            _ = alloc; // unused
            item.output_ptr.* = map_fn(item.input);
            return true;
        }
    };

    var work_items = try allocator.alloc(MapItem, inputs.len);
    defer allocator.free(work_items);

    for (inputs, outputs, 0..) |input, *output, i| {
        work_items[i] = .{ .input = input, .output_ptr = output };
    }

    const result = try processParallel(
        MapItem,
        allocator,
        work_items,
        MapWorker.worker,
        config,
    );

    if (result.failed > 0) {
        return error.SomeWorkItemsFailed;
    }
}

test "processParallel basic functionality" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const WorkItem = struct { value: i32 };

    const TestWorker = struct {
        fn worker(alloc: Allocator, item: WorkItem) bool {
            _ = alloc; // unused
            return item.value >= 0; // Success if non-negative
        }
    };

    const work_items = [_]WorkItem{
        .{ .value = 1 },
        .{ .value = 2 },
        .{ .value = -1 }, // This will fail
        .{ .value = 4 },
    };

    const result = try processParallel(
        WorkItem,
        allocator,
        &work_items,
        TestWorker.worker,
        .{ .single_threaded = true },
    );

    try testing.expectEqual(@as(usize, 3), result.success);
    try testing.expectEqual(@as(usize, 1), result.failed);
}

test "parallelMap basic functionality" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const square = struct {
        fn square(x: i32) i32 {
            return x * x;
        }
    }.square;

    const inputs = [_]i32{ 1, 2, 3, 4, 5 };
    var outputs: [5]i32 = undefined;

    try parallelMap(
        i32,
        i32,
        allocator,
        &inputs,
        &outputs,
        square,
        .{ .single_threaded = true },
    );

    const expected = [_]i32{ 1, 4, 9, 16, 25 };
    try testing.expectEqualSlices(i32, &expected, &outputs);
}
