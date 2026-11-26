//! Shared native procedure compilation and owned, same-program excerpt reuse.
//!
//! A reuse handle is a capability supplied by the caller: its LIR and layout
//! domain must be the very same transferred program, never an independently
//! lowered program with matching identities. Excerpts outlive executable images.
//! Procedures and helpers compile in ordered runs (`tasks.runOrdered`): lanes
//! read only the frozen program and their own generators while the owner
//! appends each finished fragment in demand order.

const std = @import("std");
const base = @import("base");
const lir = @import("lir");
const layout = @import("layout");
const roc_target = @import("roc_target");
const Emitter = @import("LirCodeGen.zig");
const Excerpt = @import("CodeExcerpt.zig");
const StackPlan = lir.StackPlan;
const tasks = base.post_check_task_executor;
const Allocator = std.mem.Allocator;
const ProcId = lir.LIR.LirProcSpecId;

/// Saturating counters for one driver invocation, including failed runs.
pub const Metrics = struct {
    tasks_submitted: u64 = 0,
    tasks_committed: u64 = 0,
    procedures_emitted: u64 = 0,
    procedures_reused: u64 = 0,
    helpers_emitted: u64 = 0,
    helpers_reused: u64 = 0,
    code_bytes_emitted: u64 = 0,
    code_bytes_reused: u64 = 0,
    rejected_revision: u64 = 0,
    rejected_context: u64 = 0,
    rejected_target: u64 = 0,
    /// The most jobs the owner saw claimed by lanes and not yet committed.
    peak_inflight_fragments: u64 = 0,

    /// Combine work counters while preserving the largest fragment count held.
    pub fn add(self: *Metrics, other: Metrics) void {
        inline for (@typeInfo(Metrics).@"struct".field_names) |field_name| {
            if (comptime std.mem.eql(u8, field_name, "peak_inflight_fragments")) {
                @field(self, field_name) = @max(@field(self, field_name), @field(other, field_name));
            } else {
                @field(self, field_name) +|= @field(other, field_name);
            }
        }
    }
};

/// Step durations of one driver invocation, recorded only when a caller asks
/// for detailed timing. Coordinator steps are elapsed times; worker steps
/// are summed over lanes, each lane writing only its own slot.
pub const Timing = struct {
    std_io: std.Io,
    /// Indexing the program's data and procedures before the first run.
    setup_ns: u64 = 0,
    /// Elapsed time of the ordered runs that compile and append procedures
    /// and helpers.
    runs_ns: u64 = 0,
    /// The workers' capacity over the runs: each run's elapsed time times
    /// the executor's worker count, or its elapsed time when run inline.
    capacity_ns: u64 = 0,
    /// Appending fragments to the destination, in order, on the owner while
    /// lanes keep compiling.
    commit_ns: u64 = 0,
    /// Resolving the destination's symbolic references after the last run.
    resolve_ns: u64 = 0,
    /// Worker code generator setup, summed over lanes.
    worker_setup_ns: u64 = 0,
    /// Stack planning and instruction emission, summed over lanes.
    emit_ns: u64 = 0,
    /// Fragment extraction, summed over lanes.
    extract_ns: u64 = 0,
    histogram: Emitter.StmtHistogram = .{},
    lanes: []Lane = &.{},

    /// One lane's own measures; lanes never share a cache line.
    pub const Lane = struct {
        setup_ns: u64 = 0,
        emit_ns: u64 = 0,
        extract_ns: u64 = 0,
        histogram: Emitter.StmtHistogram = .{},
        _padding: [std.atomic.cache_line]u8 = undefined,
    };

    fn now(self: *const Timing) i96 {
        return std.Io.Timestamp.now(self.std_io, .awake).nanoseconds;
    }

    fn since(self: *const Timing, started: i96) u64 {
        return @intCast(@max(0, self.now() - started));
    }

    /// Fold every lane's measures into the totals and release the lanes.
    fn mergeLanes(self: *Timing, allocator: Allocator) void {
        for (self.lanes) |*lane| {
            self.worker_setup_ns +|= lane.setup_ns;
            self.emit_ns +|= lane.emit_ns;
            self.extract_ns +|= lane.extract_ns;
            self.histogram.add(&lane.histogram);
        }
        allocator.free(self.lanes);
        self.lanes = &.{};
    }
};

/// Target, scheduling, and same-program reuse policy for native emission.
pub const Options = struct {
    target: roc_target.RocTarget,
    executor: ?tasks.Executor = null,
    /// Proof of the same transferred LIR/layout domain belongs to the caller.
    reuse_same_program: ?*const Retained = null,
    metrics_out: ?*Metrics = null,
    /// Receives step durations and the statement histogram when detailed
    /// timing is enabled.
    timing: ?*Timing = null,
    static_helpers: []const layout.RcHelperKey = &.{},
    /// Object-file roots are immutable; execution callers leave mutable slots external.
    constant_exports: []const lir.Program.StaticDataExport = &.{},
    /// Whether the returned fragments copy the payload bytes of the
    /// destination's literal backings and of `constant_exports`, or borrow
    /// them from exports the caller keeps alive as long as the fragments.
    export_payload: Excerpt.PreparedData.ExportPayload = .copy,
};

/// An owned procedure fragment with the revision that produced it.
pub const Procedure = struct {
    id: ProcId,
    identity: lir.ProcIdentity,
    revision: u64,
    fragment: Excerpt.Fragment,
};

/// An owned explicit LIR reference-count helper fragment.
pub const Helper = struct {
    key: u64,
    fragment: Excerpt.Fragment,
};

/// Owns reusable fragments independently of workers and executable images.
pub const Retained = struct {
    allocator: Allocator,
    contract: Emitter.FragmentContract,
    procedures: std.ArrayList(Procedure) = .empty,
    helpers: std.ArrayList(Helper) = .empty,

    /// Final image/object linking consumes carried data separately from code.
    /// The returned slice is caller-owned; its fields borrow this retained owner.
    pub fn dataItems(self: *const Retained, allocator: Allocator) Allocator.Error![]const Excerpt.DataItem {
        var collector = Excerpt.DataCollector{ .allocator = allocator };
        defer collector.deinit();
        for (self.procedures.items) |proc| for (proc.fragment.set.excerpts) |excerpt| try collector.add(excerpt);
        for (self.helpers.items) |helper| for (helper.fragment.set.excerpts) |excerpt| try collector.add(excerpt);
        var items = std.ArrayList(Excerpt.DataItem).empty;
        errdefer items.deinit(allocator);
        var names = std.StringHashMap(void).init(allocator);
        defer names.deinit();
        for (collector.items.items) |item| {
            if (!(try names.getOrPut(item.name)).found_existing) try items.append(allocator, item);
        }
        return items.toOwnedSlice(allocator);
    }

    /// Release all fragment and collection storage.
    pub fn deinit(self: *Retained) void {
        for (self.procedures.items) |*proc| proc.fragment.deinit();
        for (self.helpers.items) |*helper| helper.fragment.deinit();
        self.procedures.deinit(self.allocator);
        self.helpers.deinit(self.allocator);
        self.* = undefined;
    }
};

fn invariant(err: (Excerpt.ExtractError || Excerpt.AssembleError)) Allocator.Error {
    return switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        else => base.invariant("native fragment compiler invariant: {s}", .{@errorName(err)}),
    };
}

fn reusable(source: Emitter.FragmentContract, target: Emitter.FragmentContract, deps: Emitter.FragmentContextDependencies, metrics: *Metrics) bool {
    if (source.target != target.target or source.cpu_level != target.cpu_level) {
        metrics.rejected_target +|= 1;
        return false;
    }
    // Static-data ordinals are rebound by runtime compaction. String symbols
    // are relocatable and deliberately do not count as static-data dependencies.
    if (source.hot_reload != target.hot_reload or
        source.default_platform_runtime != target.default_platform_runtime or
        (source.static_data_readonly != target.static_data_readonly and deps.static_data) or
        deps.static_data)
    {
        metrics.rejected_context +|= 1;
        return false;
    }
    return true;
}

/// What every job of one driver invocation reads, and each lane's generator.
fn Shared(comptime CG: type) type {
    return struct {
        source: *const CG,
        data: *const Excerpt.PreparedData,
        timing: ?*Timing,
        /// One slot per executor lane, written only by its lane: the lane's
        /// generator, built on its first job and reset before each later one
        /// (`resetFragmentLane`), so a lane keeps its generator's capacity
        /// from one procedure to the next. The driver destroys them once
        /// every job has finished.
        lanes: []LaneGenerator(CG),

        /// The lane's generator, ready for its next fragment.
        fn generator(self: *const @This(), worker: tasks.Worker) Allocator.Error!*CG {
            const slot = &self.lanes[worker.id];
            if (slot.cg) |*cg| {
                cg.resetFragmentLane();
                return cg;
            }
            const source = self.source;
            var cg = try CG.initWithCapturePrefix(
                worker.allocator,
                source.store,
                source.layout_store,
                source.static_strings,
                source.erased_capture_prefix,
                source.cpu_level,
            );
            errdefer cg.deinit();
            cg.borrowStackPlanIndexes(try laneStackPlanIndexes(worker));
            cg.generation_mode = source.generation_mode;
            cg.enable_default_platform_runtime = source.enable_default_platform_runtime;
            cg.relocatable_expect_sites = source.relocatable_expect_sites;
            cg.borrowStaticDataSymbolsFrom(source);
            if (self.timing) |timing| cg.stmt_histogram = &timing.lanes[worker.id].histogram;
            slot.cg = cg;
            return &slot.cg.?;
        }
    };
}

/// One lane's generator; lanes never share a cache line.
fn LaneGenerator(comptime CG: type) type {
    return struct {
        cg: ?CG = null,
        _padding: [std.atomic.cache_line]u8 = undefined,
    };
}

fn Job(comptime CG: type) type {
    return struct {
        proc: ?ProcId = null,
        helper: u64 = 0,
        fragment: ?Excerpt.Fragment = null,
        reused: bool = false,

        fn compile(self: *@This(), shared: *const Shared(CG), worker: tasks.Worker) Excerpt.ExtractError!void {
            const source = shared.source;
            if (shared.timing) |timing| return self.compileTimed(shared, worker, timing, &timing.lanes[worker.id]);
            const cg = try shared.generator(worker);
            self.fragment = if (self.proc) |id|
                try Excerpt.compileProcFragmentPrepared(CG, worker.allocator, cg, id, source.store.getProcSpecs(), source.layout_store, shared.data)
            else
                try Excerpt.compileRcHelperFragmentPrepared(CG, worker.allocator, cg, self.helper, source.store.getProcSpecs(), source.layout_store, shared.data);
        }

        /// `compile`, attributing its time to setup, emission, and
        /// extraction in `lane`, and its code to the lane's histogram.
        fn compileTimed(self: *@This(), shared: *const Shared(CG), worker: tasks.Worker, timing: *const Timing, lane: *Timing.Lane) Excerpt.ExtractError!void {
            const source = shared.source;
            var started = timing.now();
            const cg = try shared.generator(worker);
            lane.setup_ns +|= timing.since(started);
            started = timing.now();
            if (self.proc) |id| try cg.emitProcFragment(id) else try cg.emitRcHelperFragment(self.helper);
            lane.emit_ns +|= timing.since(started);
            started = timing.now();
            self.fragment = try Excerpt.captureFragmentPrepared(CG, worker.allocator, cg, source.store.getProcSpecs(), source.layout_store, shared.data);
            lane.extract_ns +|= timing.since(started);
        }
    };
}

const StackPlanIndexesKey = struct {
    var value: u8 = 0;
};

/// The stack-plan indexes this lane lends every procedure it generates.
fn laneStackPlanIndexes(worker: tasks.Worker) Allocator.Error!*StackPlan.Indexes {
    const key: *const anyopaque = @ptrCast(&StackPlanIndexesKey.value);
    if (worker.lane_state.get(key)) |opaque_indexes| return @ptrCast(@alignCast(opaque_indexes));
    const indexes = try worker.allocator.create(StackPlan.Indexes);
    errdefer worker.allocator.destroy(indexes);
    indexes.* = StackPlan.Indexes.init(worker.allocator);
    errdefer indexes.deinit();
    try worker.lane_state.put(key, indexes, destroyStackPlanIndexes);
    return indexes;
}

fn destroyStackPlanIndexes(opaque_indexes: *anyopaque) void {
    const indexes: *StackPlan.Indexes = @ptrCast(@alignCast(opaque_indexes));
    const allocator = indexes.allocator;
    indexes.deinit();
    allocator.destroy(indexes);
}

/// One ordered run over procedure or helper jobs: lanes compile them, and
/// the owner appends each to the destination in job order as soon as every
/// job before it is appended (`tasks.runOrdered`).
fn Batch(comptime CG: type) type {
    return struct {
        pub const Error = Allocator.Error;

        shared: *const Shared(CG),
        jobs: []Job(CG),
        committer: *Committer(CG),

        pub fn weight(self: *const @This(), index: usize) u64 {
            const job = &self.jobs[index];
            if (job.reused) return 1;
            const id = job.proc orelse return 1;
            return 1 + @as(u64, self.shared.source.store.getProcSpecImmediate(id).frame_locals.len);
        }

        pub fn runItem(self: *@This(), index: usize, worker: tasks.Worker) Error!void {
            const job = &self.jobs[index];
            if (job.reused) return;
            job.compile(self.shared, worker) catch |err| return invariant(err);
        }

        pub fn commitItem(self: *@This(), index: usize) Error!void {
            return self.committer.commit(&self.jobs[index]);
        }
    };
}

/// The destination and the owner-side tables each commit extends.
fn Committer(comptime CG: type) type {
    return struct {
        allocator: Allocator,
        destination: *CG,
        identities: *const std.AutoHashMap(lir.ProcIdentity, ProcId),
        helper_keys: *Excerpt.HelperKeys,
        helper_queue: *std.ArrayList(u64),
        helper_seen: *std.AutoHashMap(u64, void),
        retained: *Retained,
        metrics: *Metrics,

        fn commit(self: *@This(), job: *Job(CG)) Allocator.Error!void {
            const allocator = self.allocator;
            const fragment = &job.fragment.?;
            try requireHelpers(allocator, fragment, self.helper_queue, self.helper_seen);
            if (job.proc) |id| {
                Excerpt.appendPrepared(CG, allocator, self.destination, &fragment.set, self.identities, self.helper_keys) catch |err| return invariant(err);
                const spec = self.destination.store.getProcSpecs()[@backingInt(id)];
                try self.retained.procedures.append(allocator, .{ .id = id, .identity = spec.identity, .revision = spec.native_code_revision, .fragment = fragment.* });
                account(self.metrics, fragment, job.reused, false);
            } else {
                const name = try Emitter.compiledRcHelperSymbolName(allocator, self.destination.layout_store, job.helper);
                self.helper_keys.putNoClobber(name, job.helper) catch |err| {
                    allocator.free(name);
                    return err;
                };
                Excerpt.appendPrepared(CG, allocator, self.destination, &fragment.set, self.identities, self.helper_keys) catch |err| return invariant(err);
                try self.retained.helpers.append(allocator, .{ .key = job.helper, .fragment = fragment.* });
                account(self.metrics, fragment, job.reused, true);
            }
            job.fragment = null;
        }
    };
}

/// Compile and commit `jobs` in order through one ordered run.
fn runBatch(
    comptime CG: type,
    allocator: Allocator,
    shared: *const Shared(CG),
    jobs: []Job(CG),
    committer: *Committer(CG),
    executor: ?tasks.Executor,
    sequential_lane: *tasks.LaneState,
) Allocator.Error!void {
    for (jobs) |job| if (!job.reused) {
        committer.metrics.tasks_submitted +|= 1;
    };
    var batch: Batch(CG) = .{ .shared = shared, .jobs = jobs, .committer = committer };
    var ordered: ?tasks.OrderedTiming = if (shared.timing) |timing| .{ .std_io = timing.std_io } else null;
    var peak: usize = 0;
    defer {
        committer.metrics.peak_inflight_fragments = @max(committer.metrics.peak_inflight_fragments, peak);
        if (shared.timing) |timing| {
            timing.runs_ns +|= ordered.?.elapsed_ns;
            timing.capacity_ns +|= ordered.?.capacity_ns;
            timing.commit_ns +|= ordered.?.commit_ns;
        }
    }
    try tasks.runOrdered(Batch(CG), allocator, &batch, jobs.len, executor, sequential_lane, .{
        .min_chunk_weight = min_chunk_locals,
        .timing = if (ordered) |*active| active else null,
        .peak_claimed = &peak,
    });
}

/// The smallest summed frame size of the procedures one lane claims at a
/// time, so short wrappers share a claim.
const min_chunk_locals = 64;

fn requireHelpers(allocator: Allocator, fragment: *const Excerpt.Fragment, queue: *std.ArrayList(u64), seen: *std.AutoHashMap(u64, void)) Allocator.Error!void {
    for (fragment.required_helpers) |key| {
        const entry = try seen.getOrPut(key);
        if (!entry.found_existing) try queue.append(allocator, key);
    }
}

fn account(metrics: *Metrics, fragment: *const Excerpt.Fragment, reused: bool, helper: bool) void {
    var bytes: u64 = 0;
    for (fragment.set.excerpts) |excerpt| bytes +|= excerpt.code.len;
    if (reused) {
        metrics.code_bytes_reused +|= bytes;
        if (helper) metrics.helpers_reused +|= 1 else metrics.procedures_reused +|= 1;
    } else {
        metrics.tasks_committed +|= 1;
        metrics.code_bytes_emitted +|= bytes;
        if (helper) metrics.helpers_emitted +|= 1 else metrics.procedures_emitted +|= 1;
    }
}

/// Compile and append demanded procedures and their explicit helper closure.
/// On failure all accepted tasks drain; the partially appended destination must
/// be discarded. Returned fragments own their data independently of destination,
/// except payload bytes `options.export_payload` borrows.
pub fn run(comptime CG: type, allocator: Allocator, destination: *CG, demand: []const ProcId, options: Options) Allocator.Error!Retained {
    const timing = options.timing;
    if (timing) |active| {
        active.lanes = try allocator.alloc(Timing.Lane, if (options.executor) |executor| @max(executor.lane_count, 1) else 1);
        for (active.lanes) |*lane| lane.* = .{};
    }
    defer if (timing) |active| active.mergeLanes(allocator);
    var step_started: i96 = if (timing) |active| active.now() else 0;
    var metrics: Metrics = .{};
    defer {
        if (options.metrics_out) |out| out.* = metrics;
    }
    // Baseline targets share their platform's contract target. CPU-level reuse
    // checks use the generator's explicit instruction floor, not this tag.
    const contract = destination.getFragmentContract(options.target);
    var retained = Retained{ .allocator = allocator, .contract = contract };
    errdefer retained.deinit();
    var sequential_lane = tasks.LaneState.init(allocator);
    defer sequential_lane.deinit();
    // Index immutable backing once; workers capture only reached data. Execution
    // callers leave mutable static roots external, never copied placeholder values.
    std.debug.assert(contract.static_data_readonly or options.constant_exports.len == 0);
    var data = Excerpt.PreparedData.init(allocator, destination.static_strings.exports, options.constant_exports, options.export_payload, &.{}) catch |err| return invariant(err);
    defer data.deinit();
    const specs = destination.store.getProcSpecs();
    var demanded = try std.DynamicBitSetUnmanaged.initEmpty(allocator, specs.len);
    defer demanded.deinit(allocator);
    var identities = std.AutoHashMap(lir.ProcIdentity, ProcId).init(allocator);
    defer identities.deinit();
    for (specs, 0..) |spec, i| try identities.putNoClobber(spec.identity, @fromBackingInt(@intCast(i)));
    var cached_procs = std.AutoHashMap(lir.ProcIdentity, *const Procedure).init(allocator);
    defer cached_procs.deinit();
    var cached_helpers = std.AutoHashMap(u64, *const Helper).init(allocator);
    defer cached_helpers.deinit();
    if (options.reuse_same_program) |cache| {
        for (cache.procedures.items) |*proc| try cached_procs.putNoClobber(proc.identity, proc);
        for (cache.helpers.items) |*helper| try cached_helpers.putNoClobber(helper.key, helper);
    }
    var helper_keys = Excerpt.HelperKeys.init(allocator);
    defer {
        var names = helper_keys.keyIterator();
        while (names.next()) |name| allocator.free(name.*);
        helper_keys.deinit();
    }
    var helper_queue: std.ArrayList(u64) = .empty;
    defer helper_queue.deinit(allocator);
    var helper_seen = std.AutoHashMap(u64, void).init(allocator);
    defer helper_seen.deinit();
    for (options.static_helpers) |key| {
        const encoded = Emitter.staticDataRcHelperKey(key);
        const entry = try helper_seen.getOrPut(encoded);
        if (!entry.found_existing) try helper_queue.append(allocator, encoded);
    }

    const lanes = try allocator.alloc(LaneGenerator(CG), if (options.executor) |executor| @max(executor.lane_count, 1) else 1);
    for (lanes) |*lane| lane.* = .{};
    defer {
        for (lanes) |*lane| if (lane.cg) |*cg| cg.deinit();
        allocator.free(lanes);
    }
    const shared: Shared(CG) = .{ .source = destination, .data = &data, .timing = timing, .lanes = lanes };
    var committer: Committer(CG) = .{
        .allocator = allocator,
        .destination = destination,
        .identities = &identities,
        .helper_keys = &helper_keys,
        .helper_queue = &helper_queue,
        .helper_seen = &helper_seen,
        .retained = &retained,
        .metrics = &metrics,
    };
    if (timing) |active| active.setup_ns +|= active.since(step_started);

    {
        var jobs: std.ArrayList(Job(CG)) = .empty;
        defer {
            for (jobs.items) |*job| if (job.fragment) |*fragment| fragment.deinit();
            jobs.deinit(allocator);
        }
        for (demand) |id| {
            if (demanded.isSet(@backingInt(id))) continue;
            demanded.set(@backingInt(id));
            if (destination.compiledProcSymbol(id) != null) continue;
            const spec = specs[@backingInt(id)];
            const job = try jobs.addOne(allocator);
            job.* = .{ .proc = id };
            if (cached_procs.get(spec.identity)) |cached| {
                if (cached.revision != spec.native_code_revision) {
                    metrics.rejected_revision +|= 1;
                } else if (reusable(options.reuse_same_program.?.contract, contract, cached.fragment.context_dependencies, &metrics)) {
                    job.fragment = try cached.fragment.clone(allocator);
                    job.reused = true;
                }
            }
        }
        try runBatch(CG, allocator, &shared, jobs.items, &committer, options.executor, &sequential_lane);
    }
    // Each helper run takes only keys already queued when it starts, and
    // commits in queue order, so every helper's requirements land in the queue
    // exactly where a one-helper-at-a-time drain would put them.
    var cursor: usize = 0;
    while (cursor < helper_queue.items.len) {
        const frontier = helper_queue.items.len;
        var jobs: std.ArrayList(Job(CG)) = .empty;
        defer {
            for (jobs.items) |*job| if (job.fragment) |*fragment| fragment.deinit();
            jobs.deinit(allocator);
        }
        while (cursor < frontier) : (cursor += 1) {
            const key = helper_queue.items[cursor];
            if (destination.compiledRcHelperOffset(key) != null) continue;
            const job = try jobs.addOne(allocator);
            job.* = .{ .helper = key };
            if (cached_helpers.get(key)) |cached| {
                if (reusable(options.reuse_same_program.?.contract, contract, cached.fragment.context_dependencies, &metrics)) {
                    job.fragment = try cached.fragment.clone(allocator);
                    job.reused = true;
                }
            }
        }
        try runBatch(CG, allocator, &shared, jobs.items, &committer, options.executor, &sequential_lane);
    }
    if (timing) |active| step_started = active.now();
    destination.resolveAssembledSymbolicRefs() catch |err| return invariant(err);
    if (timing) |active| active.resolve_ns +|= active.since(step_started);
    return retained;
}

const TestExecutor = struct {
    allocator: Allocator,
    lane: tasks.LaneState,
    pending: [4]tasks.Task = undefined,
    len: usize = 0,
    submitted: usize = 0,
    received: usize = 0,
    /// Submissions accepted before every further one fails; `unlimited`
    /// accepts every submission.
    fail_after: usize = unlimited,
    fail_output: bool = false,
    fail_scratch: bool = false,
    lanes: usize,

    /// A `fail_after` above every submission count.
    const unlimited: usize = std.math.maxInt(usize);

    fn executor(self: *@This()) tasks.Executor {
        return .{ .context = self, .worker_count = self.lanes, .lane_count = self.lanes, .beginFn = begin, .submitFn = submit, .receiveFn = receive, .tryReceiveFn = tryReceive, .endFn = end };
    }
    fn begin(context: *anyopaque) std.mem.Allocator.Error!*anyopaque {
        return context;
    }
    fn submit(context: *anyopaque, _: *anyopaque, task: tasks.Task) Allocator.Error!void {
        const self: *@This() = @ptrCast(@alignCast(context));
        if (self.submitted >= self.fail_after) return error.OutOfMemory;
        std.debug.assert(self.len < self.lanes);
        self.pending[self.len] = task;
        self.len += 1;
        self.submitted += 1;
    }
    fn receive(context: *anyopaque, _: *anyopaque) tasks.Completion {
        const self: *@This() = @ptrCast(@alignCast(context));
        self.len -= 1;
        const task = self.pending[self.len];
        var scratch_bytes: [1024 * 1024]u8 = undefined;
        var scratch = std.heap.FixedBufferAllocator.init(&scratch_bytes);
        const value = task.run(task.context, .{
            .id = self.len,
            .allocator = if (self.fail_output) std.testing.failing_allocator else self.allocator,
            .scratch = if (self.fail_scratch) std.testing.failing_allocator else scratch.allocator(),
            .lane_state = &self.lane,
        });
        // Overwrite the callback's transient workspace before the driver
        // sees its completion. Retained code cannot borrow this workspace.
        @memset(&scratch_bytes, 0xa5);
        self.received += 1;
        return .{ .id = task.id, .worker_id = self.len, .value = value };
    }
    fn tryReceive(_: *anyopaque, _: *anyopaque) ?tasks.Completion {
        return null;
    }
    fn end(context: *anyopaque, _: *anyopaque) void {
        const self: *@This() = @ptrCast(@alignCast(context));
        std.debug.assert(self.len == 0 and self.received == self.submitted);
    }
};

fn testProc(store: *lir.LirStore, value: i64) Allocator.Error!ProcId {
    const local = try store.addLocal(.{ .layout_idx = .i64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_literal = .{
        .target = local,
        .value = .{ .i64_literal = .{ .value = value, .layout_idx = .i64 } },
        .next = ret,
    } }, .test_fixture);
    return store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = lir.ProcIdentity.forTest(@intCast(store.getProcSpecs().len + 1)),
        .args = lir.LIR.LocalSpan.empty(),
        .body = .some(body),
        .ret_layout = .i64,
    }, .none);
}

test "native driver serial reverse lanes own identical executable excerpts and reuse" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    const a = std.testing.allocator;
    var store = lir.LirStore.init(a);
    defer store.deinit();
    var layouts = try layout.Store.init(a, .u64);
    defer layouts.deinit();
    var demand: [35]ProcId = undefined;
    for (&demand, 0..) |*id, i| id.* = try testProc(&store, @intCast(i + 42));
    const CG = Emitter.HostLirCodeGen;
    var baseline: ?[]u8 = null;
    defer if (baseline) |code| a.free(code);
    var cache: ?Retained = null;
    defer if (cache) |*retained| retained.deinit();
    for ([_]usize{ 0, 1, 2, 4 }) |lanes| {
        var executor = TestExecutor{ .allocator = a, .lane = tasks.LaneState.init(a), .lanes = lanes };
        defer executor.lane.deinit();
        var cg = try CG.init(a, &store, &layouts, .{}, .default);
        defer cg.deinit();
        var metrics: Metrics = .{};
        var retained = try run(CG, a, &cg, &demand, .{
            .target = Emitter.host_lir_codegen_target,
            .executor = if (lanes == 0) null else executor.executor(),
            .metrics_out = &metrics,
        });
        errdefer retained.deinit();
        try std.testing.expectEqual(@as(u64, demand.len), metrics.tasks_submitted);
        try std.testing.expectEqual(metrics.tasks_submitted, metrics.tasks_committed);
        try std.testing.expect(metrics.peak_inflight_fragments >= 1);
        const result = try cg.generateCode(demand[0], .i64);
        defer a.free(result.code);
        if (baseline) |code| {
            try std.testing.expectEqualSlices(u8, code, result.code);
        } else baseline = try a.dupe(u8, result.code);
        var executable = try @import("ExecutableMemory.zig").ExecutableMemory.initWithEntryOffsetAndUnwindInfo(result.code, result.entry_offset, cg.getUnwindFunctions());
        defer executable.deinit();
        var out: i64 = 0;
        executable.callRocABI(@ptrCast(&out), null);
        try std.testing.expectEqual(@as(i64, 42), out);
        if (cache) |*old| old.deinit();
        cache = retained;
    }
    // Every source CG, executable and executor lane is dead before reuse.
    var cg = try CG.init(a, &store, &layouts, .{}, .default);
    defer cg.deinit();
    var metrics: Metrics = .{};
    var reused = try run(CG, a, &cg, &demand, .{ .target = Emitter.host_lir_codegen_target, .reuse_same_program = &cache.?, .metrics_out = &metrics });
    defer reused.deinit();
    cache.?.deinit();
    cache = null;
    try std.testing.expectEqual(@as(u64, demand.len), metrics.procedures_reused);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_submitted);
    const result = try cg.generateCode(demand[0], .i64);
    defer a.free(result.code);
    try std.testing.expectEqualSlices(u8, baseline.?, result.code);
}

test "native driver drains submission output and scratch OOM before cleanup" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    const a = std.testing.allocator;
    var store = lir.LirStore.init(a);
    defer store.deinit();
    var layouts = try layout.Store.init(a, .u64);
    defer layouts.deinit();
    var demand: [4]ProcId = undefined;
    for (&demand) |*id| id.* = try testProc(&store, 42);
    const CG = Emitter.HostLirCodeGen;
    for (0..3) |failure| {
        // Item scratch comes from the lane, so a failing lane allocator
        // fails it.
        var executor = TestExecutor{
            .allocator = a,
            .lane = tasks.LaneState.init(if (failure == 2) std.testing.failing_allocator else a),
            .lanes = 4,
            .fail_after = if (failure == 0) 0 else TestExecutor.unlimited,
            .fail_output = failure == 1,
            .fail_scratch = failure == 2,
        };
        defer executor.lane.deinit();
        var cg = try CG.init(a, &store, &layouts, .{}, .default);
        defer cg.deinit();
        var metrics: Metrics = .{};
        try std.testing.expectError(error.OutOfMemory, run(CG, a, &cg, &demand, .{
            .target = Emitter.host_lir_codegen_target,
            .executor = executor.executor(),
            .metrics_out = &metrics,
        }));
        try std.testing.expectEqual(executor.submitted, executor.received);
        try std.testing.expectEqual(@as(u64, 0), metrics.tasks_committed);
        try std.testing.expectEqual(@as(usize, 0), cg.codeRegions().len);
    }
}

test "native driver reuse contract is directional and dependency sensitive" {
    const source = Emitter.FragmentContract{
        .target = .x64linux,
        .cpu_level = .default,
        .hot_reload = false,
        .default_platform_runtime = false,
        .static_data_readonly = false,
    };
    var runtime = source;
    var metrics: Metrics = .{};
    try std.testing.expect(reusable(source, runtime, .{}, &metrics));
    runtime.hot_reload = true;
    try std.testing.expect(!reusable(source, runtime, .{}, &metrics));
    runtime.hot_reload = false;
    try std.testing.expect(!reusable(source, source, .{ .static_data = true }, &metrics));
    runtime.target = .arm64linux;
    try std.testing.expect(!reusable(source, runtime, .{}, &metrics));
    runtime.target = source.target;
    runtime.cpu_level = .v1;
    try std.testing.expect(!reusable(source, runtime, .{}, &metrics));
    try std.testing.expectEqual(@as(u64, 2), metrics.rejected_target);
    var totals = Metrics{ .tasks_submitted = std.math.maxInt(u64), .peak_inflight_fragments = 4 };
    totals.add(.{ .tasks_submitted = 1, .peak_inflight_fragments = 2 });
    try std.testing.expectEqual(std.math.maxInt(u64), totals.tasks_submitted);
    try std.testing.expectEqual(@as(u64, 4), totals.peak_inflight_fragments);
}

test "native driver retained libc fragments remain target specific" {
    const source = Emitter.FragmentContract{
        .target = .x64musl,
        .cpu_level = .default,
        .hot_reload = false,
        .default_platform_runtime = false,
        .static_data_readonly = false,
    };
    var target = source;
    target.target = .x64glibc;
    var metrics: Metrics = .{};
    try std.testing.expect(!reusable(source, target, .{}, &metrics));
    try std.testing.expectEqual(@as(u64, 1), metrics.rejected_target);
    try std.testing.expectEqual(@as(u64, 0), metrics.rejected_context);
}

test "native driver reuse survives procedure compaction and rejects changed revisions" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    const a = std.testing.allocator;
    var program = try lir.Program.Result.init(a, .u64);
    defer program.deinit();
    const store = &program.store;
    _ = try testProc(store, 11);
    const original = try testProc(store, 42);
    try program.root_procs.append(a, original);
    const CG = Emitter.HostLirCodeGen;
    var cache = blk: {
        var cg = try CG.init(a, store, &program.layouts, .{}, .default);
        defer cg.deinit();
        break :blk try run(CG, a, &cg, &.{original}, .{ .target = Emitter.host_lir_codegen_target });
    };
    defer cache.deinit();
    try lir.ReachableProcs.run(&program);
    const compacted = program.root_procs.items[0];
    try std.testing.expect(compacted != original);
    for (0..2) |revision| {
        store.getProcSpecPtr(compacted).native_code_revision = revision;
        var cg = try CG.init(a, store, &program.layouts, .{}, .default);
        defer cg.deinit();
        var metrics: Metrics = .{};
        var retained = try run(CG, a, &cg, &.{ compacted, compacted }, .{
            .target = Emitter.host_lir_codegen_target,
            .reuse_same_program = &cache,
            .metrics_out = &metrics,
        });
        defer retained.deinit();
        try std.testing.expectEqual(@as(u64, 1 - revision), metrics.procedures_reused);
        try std.testing.expectEqual(@as(u64, revision), metrics.rejected_revision);
        try std.testing.expectEqual(@as(u64, revision), metrics.procedures_emitted);
        const result = try cg.generateCode(compacted, .i64);
        defer a.free(result.code);
        var executable = try @import("ExecutableMemory.zig").ExecutableMemory.initWithEntryOffsetAndUnwindInfo(result.code, result.entry_offset, cg.getUnwindFunctions());
        defer executable.deinit();
        var out: i64 = 0;
        executable.callRocABI(@ptrCast(&out), null);
        try std.testing.expectEqual(@as(i64, 42), out);
    }
}

test "native driver owns and reuses complete static helper closure" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    const a = std.testing.allocator;
    var store = lir.LirStore.init(a);
    defer store.deinit();
    var layouts = try layout.Store.init(a, .u64);
    defer layouts.deinit();
    const list = try layouts.insertList(.str);
    const roots = [_]layout.RcHelperKey{.{ .op = .decref, .layout_idx = list }};
    const CG = Emitter.HostLirCodeGen;
    var cache: ?Retained = null;
    defer if (cache) |*owner| owner.deinit();
    for (0..2) |iteration| {
        var cg = try CG.init(a, &store, &layouts, .{}, .default);
        defer cg.deinit();
        var executor = TestExecutor{ .allocator = a, .lane = tasks.LaneState.init(a), .lanes = 4 };
        defer executor.lane.deinit();
        var metrics: Metrics = .{};
        var retained = try run(CG, a, &cg, &.{}, .{
            .target = Emitter.host_lir_codegen_target,
            .static_helpers = &roots,
            .executor = executor.executor(),
            .reuse_same_program = if (cache) |*owner| owner else null,
            .metrics_out = &metrics,
        });
        errdefer retained.deinit();
        try std.testing.expect(retained.helpers.items.len >= 2);
        if (iteration == 0) {
            try std.testing.expectEqual(retained.helpers.items.len, metrics.helpers_emitted);
        } else {
            try std.testing.expectEqual(cache.?.helpers.items.len, retained.helpers.items.len);
            try std.testing.expectEqual(retained.helpers.items.len, metrics.helpers_reused);
            try std.testing.expectEqual(@as(u64, 0), metrics.tasks_submitted);
            for (cache.?.helpers.items, retained.helpers.items) |old, new| {
                try std.testing.expectEqual(old.key, new.key);
                try std.testing.expectEqualSlices(u8, old.fragment.set.excerpts[0].code, new.fragment.set.excerpts[0].code);
            }
        }
        if (cache) |*owner| owner.deinit();
        cache = retained;
    }
}

test "native driver helper runs keep the serial queue order at every lane count" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    const a = std.testing.allocator;
    var store = lir.LirStore.init(a);
    defer store.deinit();
    var layouts = try layout.Store.init(a, .u64);
    defer layouts.deinit();
    const strs = try layouts.insertList(.str);
    const nested = try layouts.insertList(strs);
    const boxed = try layouts.insertBox(nested);
    const tuple = try layouts.putTuple(&.{ layouts.getLayout(strs), layouts.getLayout(boxed), layouts.getLayout(nested) });
    const roots = [_]layout.RcHelperKey{
        .{ .op = .decref, .layout_idx = tuple },
        .{ .op = .decref, .layout_idx = try layouts.insertList(tuple) },
        .{ .op = .free, .layout_idx = boxed },
    };
    const CG = Emitter.HostLirCodeGen;
    var baseline_keys: std.ArrayList(u64) = .empty;
    defer baseline_keys.deinit(a);
    var baseline_code: ?[]u8 = null;
    defer if (baseline_code) |code| a.free(code);
    for ([_]usize{ 0, 1, 2, 4 }) |lanes| {
        var executor = TestExecutor{ .allocator = a, .lane = tasks.LaneState.init(a), .lanes = lanes };
        defer executor.lane.deinit();
        var cg = try CG.init(a, &store, &layouts, .{}, .default);
        defer cg.deinit();
        var metrics: Metrics = .{};
        var retained = try run(CG, a, &cg, &.{}, .{
            .target = Emitter.host_lir_codegen_target,
            .static_helpers = &roots,
            .executor = if (lanes == 0) null else executor.executor(),
            .metrics_out = &metrics,
        });
        defer retained.deinit();
        try std.testing.expect(retained.helpers.items.len > roots.len);
        try std.testing.expectEqual(retained.helpers.items.len, metrics.helpers_emitted);
        try std.testing.expect(metrics.peak_inflight_fragments >= 1);
        // Commit order fixes each helper's region, so equal keys, code, and
        // region bounds mean an identical code buffer.
        var layout_record: std.ArrayList(u8) = .empty;
        defer layout_record.deinit(a);
        for (retained.helpers.items) |helper| try layout_record.appendSlice(a, helper.fragment.set.excerpts[0].code);
        for (cg.codeRegions()) |region| try layout_record.appendSlice(a, std.mem.asBytes(&[2]usize{ region.start, region.end }));
        if (baseline_code) |expected| {
            try std.testing.expectEqual(baseline_keys.items.len, retained.helpers.items.len);
            for (baseline_keys.items, retained.helpers.items) |key, helper| try std.testing.expectEqual(key, helper.key);
            try std.testing.expectEqualSlices(u8, expected, layout_record.items);
        } else {
            for (retained.helpers.items) |helper| try baseline_keys.append(a, helper.key);
            baseline_code = try layout_record.toOwnedSlice(a);
        }
    }
}

fn testCoordinatorAllocationFailure(a: Allocator) Allocator.Error!void {
    const backing = std.testing.allocator;
    var store = lir.LirStore.init(backing);
    defer store.deinit();
    var layouts = try layout.Store.init(backing, .u64);
    defer layouts.deinit();
    const id = try testProc(&store, 42);
    const CG = Emitter.HostLirCodeGen;
    var cg = try CG.init(a, &store, &layouts, .{}, .default);
    defer cg.deinit();
    // Workers succeed independently; enumerate coordinator/append allocations.
    var executor = TestExecutor{ .allocator = backing, .lane = tasks.LaneState.init(backing), .lanes = 2 };
    defer executor.lane.deinit();
    defer std.debug.assert(executor.submitted == executor.received);
    var retained = try run(CG, a, &cg, &.{id}, .{
        .target = Emitter.host_lir_codegen_target,
        .executor = executor.executor(),
    });
    defer retained.deinit();
}

test "native driver coordinator allocation failures drain and release owners" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testCoordinatorAllocationFailure, .{});
}

test "native driver shared literal data outlives source and reused fragment owners" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    const a = std.testing.allocator;
    var store = lir.LirStore.init(a);
    defer store.deinit();
    var layouts = try layout.Store.init(a, .u64);
    defer layouts.deinit();
    const text = "a readonly literal with more than twenty three bytes";
    const literal = try store.insertString(text);
    const local = try store.addLocal(.{ .layout_idx = .str });
    const end = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_literal = .{
        .target = local,
        .value = .{ .str_literal = .{ .backing = literal, .offset = 0, .len = text.len } },
        .next = end,
    } }, .test_fixture);
    var demand: [2]ProcId = undefined;
    for (&demand, 0..) |*id, i| id.* = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = lir.ProcIdentity.forTest(@intCast(i + 1)),
        .args = .empty(),
        .body = .some(body),
        .ret_layout = .str,
    }, .none);
    const CG = Emitter.HostLirCodeGen;
    var cache = blk: {
        var strings = try @import("StaticStringData.zig").build(a, &store, roc_target.RocTarget.detectNative());
        defer strings.deinit();
        var cg = try CG.init(a, &store, &layouts, strings.view(), .default);
        defer cg.deinit();
        var executor = TestExecutor{ .allocator = a, .lane = tasks.LaneState.init(a), .lanes = 2 };
        defer executor.lane.deinit();
        break :blk try run(CG, a, &cg, &demand, .{
            .target = Emitter.host_lir_codegen_target,
            .executor = executor.executor(),
        });
    };
    var cache_live = true;
    defer if (cache_live) cache.deinit();
    var cg = try CG.init(a, &store, &layouts, .{}, .default);
    defer cg.deinit();
    var metrics: Metrics = .{};
    var retained = try run(CG, a, &cg, &demand, .{
        .target = Emitter.host_lir_codegen_target,
        .reuse_same_program = &cache,
        .metrics_out = &metrics,
    });
    defer retained.deinit();
    cache.deinit();
    cache_live = false;
    try std.testing.expectEqual(@as(u64, 2), metrics.procedures_reused);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_submitted);
    const items = try retained.dataItems(a);
    defer a.free(items);
    try std.testing.expectEqual(@as(usize, 1), items.len);
    const offset = items[0].symbol_offset;
    try std.testing.expectEqualStrings(text, items[0].bytes[offset..][0..text.len]);
}

test "native driver lanes on real threads append identical code at every worker count" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    const a = std.testing.allocator;
    var store = lir.LirStore.init(a);
    defer store.deinit();
    var layouts = try layout.Store.init(a, .u64);
    defer layouts.deinit();
    var demand: [300]ProcId = undefined;
    for (&demand, 0..) |*id, i| id.* = try testProc(&store, @intCast(i * 7919));
    const CG = Emitter.HostLirCodeGen;
    var baseline: ?[]u8 = null;
    defer if (baseline) |code| a.free(code);
    for ([_]?usize{ null, 0, 1, 2, 4, 8 }) |maybe_threads| {
        var pool = tasks.ThreadedTestExecutor{};
        if (maybe_threads) |threads| try pool.start(threads);
        defer if (maybe_threads != null) pool.stop();
        var cg = try CG.init(a, &store, &layouts, .{}, .default);
        defer cg.deinit();
        var retained = try run(CG, a, &cg, &demand, .{
            .target = Emitter.host_lir_codegen_target,
            .executor = if (maybe_threads != null) pool.executor() else null,
        });
        defer retained.deinit();
        // Commit order fixes each procedure's region, so equal code and
        // region bounds mean an identical code buffer.
        var record: std.ArrayList(u8) = .empty;
        defer record.deinit(a);
        for (retained.procedures.items) |proc| for (proc.fragment.set.excerpts) |excerpt| try record.appendSlice(a, excerpt.code);
        for (cg.codeRegions()) |region| try record.appendSlice(a, std.mem.asBytes(&[2]usize{ region.start, region.end }));
        if (baseline) |expected| {
            try std.testing.expectEqualSlices(u8, expected, record.items);
        } else baseline = try record.toOwnedSlice(a);
    }
}

test "native driver lane generators emit what fresh generators emit" {
    if (comptime !Emitter.host_lir_codegen_available) return error.SkipZigTest;
    const a = std.testing.allocator;
    var store = lir.LirStore.init(a);
    defer store.deinit();
    var layouts = try layout.Store.init(a, .u64);
    defer layouts.deinit();
    const list = try layouts.insertList(.str);
    const roots = [_]layout.RcHelperKey{ .{ .op = .decref, .layout_idx = list }, .{ .op = .incref, .layout_idx = list } };
    var demand: [12]ProcId = undefined;
    for (&demand, 0..) |*id, i| id.* = try testProc(&store, @intCast(i));
    const CG = Emitter.HostLirCodeGen;
    // One lane reuses one generator for every procedure and helper; a run
    // per procedure gives each a fresh one.
    var reused_cg = try CG.init(a, &store, &layouts, .{}, .default);
    defer reused_cg.deinit();
    var reused = try run(CG, a, &reused_cg, &demand, .{ .target = Emitter.host_lir_codegen_target, .static_helpers = &roots });
    defer reused.deinit();
    for (demand, 0..) |id, index| {
        var fresh_cg = try CG.init(a, &store, &layouts, .{}, .default);
        defer fresh_cg.deinit();
        var fresh = try run(CG, a, &fresh_cg, &.{id}, .{ .target = Emitter.host_lir_codegen_target });
        defer fresh.deinit();
        try std.testing.expectEqual(@as(usize, 1), fresh.procedures.items.len);
        const expected = fresh.procedures.items[0].fragment.set.excerpts;
        const actual = reused.procedures.items[index].fragment.set.excerpts;
        try std.testing.expectEqual(expected.len, actual.len);
        for (expected, actual) |left, right| try std.testing.expectEqualSlices(u8, left.code, right.code);
    }
}
