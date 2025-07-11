//! TODO Doc Comment

const std = @import("std");
const Allocator = std.mem.Allocator;
const Thread = std.Thread;

/// Atomic type for thread-safe usize operations
pub const AtomicUsize = std.atomic.Value(usize);

/// Processing options for parallel execution
pub const ProcessOptions = struct {
    max_threads: usize,
    use_per_thread_arenas: bool,
};

/// Worker thread function signature
/// Takes: allocator, context, item_id -> void
pub fn WorkerFn(comptime T: type) type {
    return *const fn (allocator: Allocator, context: *T, item_id: usize) void;
}

/// Internal worker thread context
fn WorkerContext(comptime T: type) type {
    return struct {
        work_item_count: usize,
        index: *AtomicUsize,
        worker_fn: WorkerFn(T),
        context: *T,
        base_allocator: Allocator,
        options: ProcessOptions,
    };
}

/// Worker thread implementation using work-stealing
fn workerThread(comptime T: type, ctx: WorkerContext(T)) void {
    if (ctx.options.use_per_thread_arenas) {
        // Use per-thread arena allocator with page allocator that clears between work items
        // var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        var arena = std.heap.ArenaAllocator.init(ctx.base_allocator);
        defer arena.deinit();

        while (true) {
            const i = ctx.index.fetchAdd(1, .monotonic);
            if (i >= ctx.work_item_count) break;

            // Clear arena between work items
            _ = arena.reset(.retain_capacity);

            ctx.worker_fn(arena.allocator(), ctx.context, i);
        }
    } else {
        // Use the base allocator directly
        while (true) {
            const i = ctx.index.fetchAdd(1, .monotonic);
            if (i >= ctx.work_item_count) break;
            ctx.worker_fn(ctx.base_allocator, ctx.context, i);
        }
    }
}

/// Process work items in parallel across multiple threads
///
/// Generic function that:
/// 1. Takes a count of work items
/// 2. Spawns worker threads to process items
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
pub fn process(
    comptime T: type,
    context: *T,
    worker_fn: WorkerFn(T),
    allocator: Allocator,
    work_item_count: usize,
    options: ProcessOptions,
) !void {
    if (work_item_count == 0) {
        return;
    }

    if (options.max_threads == 1) {
        // Process everything in main thread
        var index = AtomicUsize.init(0);
        const ctx = WorkerContext(T){
            .work_item_count = work_item_count,
            .index = &index,
            .worker_fn = worker_fn,
            .context = context,
            .base_allocator = allocator,
            .options = options,
        };
        workerThread(T, ctx);
    } else {
        const thread_count = @min(
            if (options.max_threads == 0) std.Thread.getCpuCount() catch 1 else options.max_threads,
            work_item_count,
        );

        var index = AtomicUsize.init(0);
        const fixed_stack_thread_count: usize = 16;
        var threads: [fixed_stack_thread_count]Thread = undefined;
        var extra_threads: std.ArrayList(Thread) = undefined;

        if (thread_count > fixed_stack_thread_count) {
            extra_threads = std.ArrayList(Thread).init(allocator);
        }

        // Start worker threads
        for (0..thread_count) |i| {
            const ctx = WorkerContext(T){
                .work_item_count = work_item_count,
                .index = &index,
                .worker_fn = worker_fn,
                .context = context,
                .base_allocator = allocator,
                .options = options,
            };
            if (i < threads.len) {
                threads[i] = try Thread.spawn(.{}, workerThread, .{ T, ctx });
            } else {
                try extra_threads.append(try Thread.spawn(.{}, workerThread, .{ T, ctx }));
            }
        }

        // Wait for all threads to complete
        for (threads[0..@min(thread_count, fixed_stack_thread_count)]) |thread| {
            thread.join();
        }
        if (thread_count > fixed_stack_thread_count) {
            for (extra_threads.items) |thread| {
                thread.join();
            }
            extra_threads.deinit();
        }
    }
}

test "process basic functionality" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const MyContext = struct {
        items: []const i32,
        outputs: []i32,
    };

    const TestWorker = struct {
        fn worker(worker_allocator: Allocator, item: *MyContext, item_id: usize) void {
            _ = worker_allocator; // unused in this test
            const value = item.items[item_id];
            if (value < 0) {
                item.outputs[item_id] = -1;
            } else {
                item.outputs[item_id] = value * value;
            }
        }
    };

    var outputs: [5]i32 = undefined; // Preallocate output array

    var context = MyContext{
        .items = &[_]i32{ 1, 2, -3, 4, 5 },
        .outputs = &outputs,
    };

    try process(
        MyContext,
        &context,
        TestWorker.worker,
        allocator,
        outputs.len,
        .{ .max_threads = 1, .use_per_thread_arenas = false },
    );
    try testing.expectEqual(
        outputs,
        [_]i32{ 1, 4, -1, 16, 25 },
    );
}
