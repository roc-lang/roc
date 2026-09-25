//! Shared native procedure compilation and owned, same-program artifact reuse.
//!
//! A reuse handle is a capability supplied by the caller: its LIR and layout
//! domain must be the very same transferred program, never an independently
//! lowered program with matching identities. Artifacts outlive executable images.
//! Each wave freezes borrowed inputs until every accepted callback has drained;
//! only then may the coordinator append code or destroy fragment owners.

const std = @import("std");
const base = @import("base");
const lir = @import("lir");
const layout = @import("layout");
const roc_target = @import("roc_target");
const Emitter = @import("LirCodeGen.zig");
const Artifact = @import("ProcArtifact.zig");
const tasks = base.post_check_task_executor;
const Allocator = std.mem.Allocator;
const ProcId = lir.LIR.LirProcSpecId;

/// Saturating counters for one driver invocation, including failed waves.
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
    peak_inflight_fragments: u64 = 0,

    /// Combine work counters while preserving the maximum retained wave size.
    pub fn add(self: *Metrics, other: Metrics) void {
        inline for (@typeInfo(Metrics).@"struct".fields) |field| {
            if (comptime std.mem.eql(u8, field.name, "peak_inflight_fragments")) {
                @field(self, field.name) = @max(@field(self, field.name), @field(other, field.name));
            } else {
                @field(self, field.name) +|= @field(other, field.name);
            }
        }
    }
};

/// Target, scheduling, and same-program reuse policy for native emission.
pub const Options = struct {
    target: roc_target.RocTarget,
    executor: ?tasks.Executor = null,
    /// Proof of the same transferred LIR/layout domain belongs to the caller.
    reuse_same_program: ?*const Retained = null,
    metrics_out: ?*Metrics = null,
    static_helpers: []const layout.RcHelperKey = &.{},
    /// Object-file roots are immutable; execution callers leave mutable slots external.
    constant_exports: []const lir.Program.StaticDataExport = &.{},
};

/// An owned procedure fragment with the revision that produced it.
pub const Procedure = struct {
    id: ProcId,
    identity: lir.ProcIdentity,
    revision: u64,
    fragment: Artifact.Fragment,
};

/// An owned explicit LIR reference-count helper fragment.
pub const Helper = struct {
    key: u64,
    fragment: Artifact.Fragment,
};

/// Owns reusable fragments independently of workers and executable images.
pub const Retained = struct {
    allocator: Allocator,
    contract: Emitter.FragmentContract,
    procedures: std.ArrayList(Procedure) = .empty,
    helpers: std.ArrayList(Helper) = .empty,

    /// Final image/object linking consumes carried data separately from code.
    /// The returned slice is caller-owned; its fields borrow this retained owner.
    pub fn dataItems(self: *const Retained, allocator: Allocator) Allocator.Error![]const Artifact.DataItem {
        var items: std.ArrayList(Artifact.DataItem) = .empty;
        errdefer items.deinit(allocator);
        var names = std.StringHashMap(void).init(allocator);
        defer names.deinit();
        for (self.procedures.items) |proc| try collectData(allocator, &proc.fragment, &items, &names);
        for (self.helpers.items) |helper| try collectData(allocator, &helper.fragment, &items, &names);
        return items.toOwnedSlice(allocator);
    }

    fn collectData(allocator: Allocator, fragment: *const Artifact.Fragment, items: *std.ArrayList(Artifact.DataItem), names: *std.StringHashMap(void)) Allocator.Error!void {
        for (fragment.set.artifacts) |artifact| {
            for (artifact.data) |item| {
                const entry = try names.getOrPut(item.name);
                if (!entry.found_existing) try items.append(allocator, item);
            }
        }
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

fn invariant(err: (Artifact.ExtractError || Artifact.AssembleError)) Allocator.Error {
    return switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        else => std.debug.panic("native fragment compiler invariant: {s}", .{@errorName(err)}),
    };
}

fn reusable(source: Emitter.FragmentContract, target: Emitter.FragmentContract, deps: Emitter.FragmentContextDependencies, metrics: *Metrics) bool {
    if (source.target != target.target or source.cpu_level != target.cpu_level) {
        metrics.rejected_target +|= 1;
        return false;
    }
    // Static-data ordinals are rebound by runtime compaction. String symbols
    // are relocatable and deliberately do not count as static-data dependencies.
    const hook_transition = source.hooks_enabled and !target.hooks_enabled and !deps.comptime_hooks;
    if (source.hot_reload != target.hot_reload or
        source.default_platform_runtime != target.default_platform_runtime or
        (source.static_data_readonly != target.static_data_readonly and deps.static_data) or
        (source.hooks_enabled != target.hooks_enabled and !hook_transition) or
        deps.comptime_hooks or deps.static_data or
        (source.dict_seed_mode != target.dict_seed_mode and deps.dict_seed) or
        (source.initialize_boxy_runtime != target.initialize_boxy_runtime and deps.boxy_runtime_entry))
    {
        metrics.rejected_context +|= 1;
        return false;
    }
    return true;
}

fn Job(comptime CG: type) type {
    return struct {
        source: *const CG,
        data: *const Artifact.PreparedData,
        proc: ?ProcId = null,
        helper: u64 = 0,
        fragment: ?Artifact.Fragment = null,
        oom: bool = false,
        reused: bool = false,

        fn execute(context: *anyopaque, worker: tasks.Worker) ?*anyopaque {
            const self: *@This() = @ptrCast(@alignCast(context));
            self.compile(worker) catch |err| {
                switch (invariant(err)) {
                    error.OutOfMemory => self.oom = true,
                }
            };
            return self;
        }

        fn compile(self: *@This(), worker: tasks.Worker) Artifact.ExtractError!void {
            const source = self.source;
            var cg = try CG.initWithBoxyMetadata(
                worker.scratch,
                source.store,
                source.layout_store,
                source.static_strings,
                source.erased_arg_desc_offsets,
                source.erased_arg_desc_params,
                source.boxy_worker_procs,
                source.cpu_level,
            );
            defer cg.deinit();
            cg.generation_mode = source.generation_mode;
            cg.dict_seed_mode = source.dict_seed_mode;
            cg.enable_hot_reload = source.enable_hot_reload;
            cg.enable_default_platform_runtime = source.enable_default_platform_runtime;
            cg.comptime_hooks = source.comptime_hooks;
            cg.borrowStaticDataSymbolsFrom(source);
            self.fragment = if (self.proc) |id|
                try Artifact.compileProcFragmentPrepared(CG, worker.allocator, &cg, id, source.store.getProcSpecs(), source.layout_store, self.data)
            else
                try Artifact.compileRcHelperFragmentPrepared(CG, worker.allocator, &cg, self.helper, source.store.getProcSpecs(), source.layout_store, self.data);
        }
    };
}

const wave_capacity = 32;

/// Runs the same callback path with or without an executor. Admission stops on
/// any observed OOM, but all accepted callbacks finish before owners are touched.
fn executeWave(comptime CG: type, allocator: Allocator, jobs: []Job(CG), executor: ?tasks.Executor, metrics: *Metrics) Allocator.Error!void {
    var failed = false;
    if (executor) |exec| {
        var session = exec.begin();
        var next: usize = 0;
        var accepted: usize = 0;
        while (next < jobs.len or session.outstanding != 0) {
            while (!failed and next < jobs.len and session.canSubmit()) {
                if (jobs[next].reused) {
                    next += 1;
                    continue;
                }
                session.submit(.{ .id = next, .context = &jobs[next], .run = Job(CG).execute }) catch {
                    failed = true;
                    break;
                };
                next += 1;
                accepted += 1;
                metrics.tasks_submitted +|= 1;
                metrics.peak_inflight_fragments = @max(metrics.peak_inflight_fragments, accepted);
            }
            if (session.outstanding != 0) {
                const completion = session.receive();
                if (jobs[completion.id].oom) failed = true;
            } else if (failed) break;
        }
        session.end();
    } else {
        var lane = tasks.LaneState.init(allocator);
        defer lane.deinit();
        var scratch = std.heap.ArenaAllocator.init(allocator);
        defer scratch.deinit();
        var accepted: usize = 0;
        for (jobs) |*job| {
            if (job.reused) continue;
            _ = scratch.reset(.retain_capacity);
            metrics.tasks_submitted +|= 1;
            accepted += 1;
            metrics.peak_inflight_fragments = @max(metrics.peak_inflight_fragments, accepted);
            _ = Job(CG).execute(job, .{ .id = 0, .allocator = allocator, .scratch = scratch.allocator(), .lane_state = &lane });
            if (job.oom) {
                failed = true;
                break;
            }
        }
    }
    if (failed) return error.OutOfMemory;
}

fn requireHelpers(allocator: Allocator, fragment: *const Artifact.Fragment, queue: *std.ArrayList(u64), seen: *std.AutoHashMap(u64, void)) Allocator.Error!void {
    for (fragment.required_helpers) |key| {
        const entry = try seen.getOrPut(key);
        if (!entry.found_existing) try queue.append(allocator, key);
    }
}

fn account(metrics: *Metrics, fragment: *const Artifact.Fragment, reused: bool, helper: bool) void {
    var bytes: u64 = 0;
    for (fragment.set.artifacts) |artifact| bytes +|= artifact.code.len;
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
/// be discarded. Returned fragments own their data independently of destination.
pub fn run(comptime CG: type, allocator: Allocator, destination: *CG, demand: []const ProcId, options: Options) Allocator.Error!Retained {
    var metrics: Metrics = .{};
    defer {
        if (options.metrics_out) |out| out.* = metrics;
    }
    const contract = destination.getFragmentContract();
    // Baseline targets share their platform's code generator. CPU-level reuse
    // checks use the generator's explicit instruction floor, not this dispatch tag.
    std.debug.assert(contract.target == options.target.defaultCpuTarget());
    var retained = Retained{ .allocator = allocator, .contract = contract };
    errdefer retained.deinit();
    // Index immutable backing once; workers capture only reached data. Execution
    // callers leave mutable static roots external, never copied placeholder values.
    std.debug.assert(contract.static_data_readonly or options.constant_exports.len == 0);
    var data = Artifact.PreparedData.init(allocator, destination.static_strings.exports, options.constant_exports, &.{}) catch |err| return invariant(err);
    defer data.deinit();
    const specs = destination.store.getProcSpecs();
    var demanded = try std.DynamicBitSetUnmanaged.initEmpty(allocator, specs.len);
    defer demanded.deinit(allocator);
    var identities = std.AutoHashMap(lir.ProcIdentity, ProcId).init(allocator);
    defer identities.deinit();
    for (specs, 0..) |spec, i| try identities.putNoClobber(spec.identity, @enumFromInt(i));
    var cached_procs = std.AutoHashMap(lir.ProcIdentity, *const Procedure).init(allocator);
    defer cached_procs.deinit();
    var cached_helpers = std.AutoHashMap(u64, *const Helper).init(allocator);
    defer cached_helpers.deinit();
    if (options.reuse_same_program) |cache| {
        for (cache.procedures.items) |*proc| try cached_procs.putNoClobber(proc.identity, proc);
        for (cache.helpers.items) |*helper| try cached_helpers.putNoClobber(helper.key, helper);
    }
    var helper_keys = Artifact.HelperKeys.init(allocator);
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

    var cursor: usize = 0;
    while (cursor < demand.len) {
        var storage: [wave_capacity]Job(CG) = undefined;
        var count: usize = 0;
        defer for (storage[0..count]) |*job| {
            if (job.fragment) |*fragment| fragment.deinit();
        };
        while (cursor < demand.len and count < wave_capacity) : (cursor += 1) {
            const id = demand[cursor];
            if (demanded.isSet(@intFromEnum(id))) continue;
            demanded.set(@intFromEnum(id));
            if (destination.compiledProcSymbol(id) != null) continue;
            const spec = specs[@intFromEnum(id)];
            storage[count] = .{ .source = destination, .data = &data, .proc = id };
            const job = &storage[count];
            count += 1;
            if (cached_procs.get(spec.identity)) |cached| {
                if (cached.revision != spec.native_code_revision) {
                    metrics.rejected_revision +|= 1;
                } else if (reusable(options.reuse_same_program.?.contract, contract, cached.fragment.context_dependencies, &metrics)) {
                    job.fragment = try cached.fragment.clone(allocator);
                    job.reused = true;
                }
            }
        }
        try executeWave(CG, allocator, storage[0..count], options.executor, &metrics);
        for (storage[0..count]) |*job| {
            const fragment = &job.fragment.?;
            try requireHelpers(allocator, fragment, &helper_queue, &helper_seen);
            destination.boxy_runtime_used = destination.boxy_runtime_used or fragment.context_dependencies.boxy_runtime;
            Artifact.appendPrepared(CG, allocator, destination, &fragment.set, &identities, &helper_keys) catch |err| return invariant(err);
            const id = job.proc.?;
            const spec = specs[@intFromEnum(id)];
            try retained.procedures.append(allocator, .{ .id = id, .identity = spec.identity, .revision = spec.native_code_revision, .fragment = fragment.* });
            account(&metrics, fragment, job.reused, false);
            job.fragment = null;
        }
    }
    cursor = 0;
    while (cursor < helper_queue.items.len) : (cursor += 1) {
        const key = helper_queue.items[cursor];
        if (destination.compiledRcHelperOffset(key) != null) continue;
        var job = Job(CG){ .source = destination, .data = &data, .helper = key };
        defer if (job.fragment) |*fragment| fragment.deinit();
        if (cached_helpers.get(key)) |cached| {
            if (reusable(options.reuse_same_program.?.contract, contract, cached.fragment.context_dependencies, &metrics)) {
                job.fragment = try cached.fragment.clone(allocator);
                job.reused = true;
            }
        }
        try executeWave(CG, allocator, @as(*[1]Job(CG), @ptrCast(&job)), options.executor, &metrics);
        const fragment = &job.fragment.?;
        try requireHelpers(allocator, fragment, &helper_queue, &helper_seen);
        const name = try Emitter.compiledRcHelperSymbolName(allocator, destination.layout_store, key);
        helper_keys.putNoClobber(name, key) catch |err| {
            allocator.free(name);
            return err;
        };
        destination.boxy_runtime_used = destination.boxy_runtime_used or fragment.context_dependencies.boxy_runtime;
        Artifact.appendPrepared(CG, allocator, destination, &fragment.set, &identities, &helper_keys) catch |err| return invariant(err);
        try retained.helpers.append(allocator, .{ .key = key, .fragment = fragment.* });
        account(&metrics, fragment, job.reused, true);
        job.fragment = null;
    }
    destination.generateBoxyDictProcThunks() catch |err| return invariant(err);
    destination.resolveAssembledSymbolicRefs() catch |err| return invariant(err);
    return retained;
}

const TestExecutor = struct {
    allocator: Allocator,
    lane: tasks.LaneState,
    pending: [4]tasks.Task = undefined,
    len: usize = 0,
    submitted: usize = 0,
    received: usize = 0,
    fail_after: usize = std.math.maxInt(usize),
    fail_output: bool = false,
    fail_scratch: bool = false,
    lanes: usize,

    fn executor(self: *@This()) tasks.Executor {
        return .{ .context = self, .worker_count = self.lanes, .beginFn = begin, .submitFn = submit, .receiveFn = receive, .endFn = end };
    }
    fn begin(_: *anyopaque) void {}
    fn submit(context: *anyopaque, task: tasks.Task) Allocator.Error!void {
        const self: *@This() = @ptrCast(@alignCast(context));
        if (self.submitted == self.fail_after) return error.OutOfMemory;
        std.debug.assert(self.len < self.lanes);
        self.pending[self.len] = task;
        self.len += 1;
        self.submitted += 1;
    }
    fn receive(context: *anyopaque) tasks.Completion {
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
    fn end(context: *anyopaque) void {
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
        .body = body,
        .ret_layout = .i64,
    }, .none);
}

test "native driver serial reverse lanes own identical executable artifacts and reuse" {
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
        var cg = try CG.init(a, &store, &layouts, .{}, &.{}, .default);
        defer cg.deinit();
        var metrics: Metrics = .{};
        var retained = try run(CG, a, &cg, &demand, .{
            .target = cg.getFragmentContract().target,
            .executor = if (lanes == 0) null else executor.executor(),
            .metrics_out = &metrics,
        });
        errdefer retained.deinit();
        try std.testing.expectEqual(@as(u64, demand.len), metrics.tasks_submitted);
        try std.testing.expectEqual(metrics.tasks_submitted, metrics.tasks_committed);
        try std.testing.expectEqual(@as(u64, wave_capacity), metrics.peak_inflight_fragments);
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
    var cg = try CG.init(a, &store, &layouts, .{}, &.{}, .default);
    defer cg.deinit();
    var metrics: Metrics = .{};
    var reused = try run(CG, a, &cg, &demand, .{ .target = cg.getFragmentContract().target, .reuse_same_program = &cache.?, .metrics_out = &metrics });
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
        var executor = TestExecutor{
            .allocator = a,
            .lane = tasks.LaneState.init(a),
            .lanes = 4,
            .fail_after = if (failure == 0) 2 else std.math.maxInt(usize),
            .fail_output = failure == 1,
            .fail_scratch = failure == 2,
        };
        defer executor.lane.deinit();
        var cg = try CG.init(a, &store, &layouts, .{}, &.{}, .default);
        defer cg.deinit();
        var metrics: Metrics = .{};
        try std.testing.expectError(error.OutOfMemory, run(CG, a, &cg, &demand, .{
            .target = cg.getFragmentContract().target,
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
        .dict_seed_mode = .comptime_zero,
        .hooks_enabled = true,
        .initialize_boxy_runtime = false,
        .static_data_readonly = false,
    };
    var runtime = source;
    runtime.hooks_enabled = false;
    runtime.dict_seed_mode = .runtime;
    var metrics: Metrics = .{};
    try std.testing.expect(reusable(source, runtime, .{}, &metrics));
    try std.testing.expect(!reusable(runtime, source, .{}, &metrics));
    try std.testing.expect(!reusable(source, runtime, .{ .comptime_hooks = true }, &metrics));
    try std.testing.expect(!reusable(source, runtime, .{ .dict_seed = true }, &metrics));
    try std.testing.expect(!reusable(source, source, .{ .static_data = true }, &metrics));
    try std.testing.expect(reusable(source, runtime, .{ .boxy_runtime = true }, &metrics));
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
        var cg = try CG.init(a, store, &program.layouts, .{}, &.{}, .default);
        defer cg.deinit();
        break :blk try run(CG, a, &cg, &.{original}, .{ .target = cg.getFragmentContract().target });
    };
    defer cache.deinit();
    try lir.ReachableProcs.run(&program);
    const compacted = program.root_procs.items[0];
    try std.testing.expect(compacted != original);
    for (0..2) |revision| {
        store.getProcSpecPtr(compacted).native_code_revision = revision;
        var cg = try CG.init(a, store, &program.layouts, .{}, &.{}, .default);
        defer cg.deinit();
        var metrics: Metrics = .{};
        var retained = try run(CG, a, &cg, &.{ compacted, compacted }, .{
            .target = cg.getFragmentContract().target,
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
        var cg = try CG.init(a, &store, &layouts, .{}, &.{}, .default);
        defer cg.deinit();
        var executor = TestExecutor{ .allocator = a, .lane = tasks.LaneState.init(a), .lanes = 4 };
        defer executor.lane.deinit();
        var metrics: Metrics = .{};
        var retained = try run(CG, a, &cg, &.{}, .{
            .target = cg.getFragmentContract().target,
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
                try std.testing.expectEqualSlices(u8, old.fragment.set.artifacts[0].code, new.fragment.set.artifacts[0].code);
            }
        }
        if (cache) |*owner| owner.deinit();
        cache = retained;
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
    var cg = try CG.init(a, &store, &layouts, .{}, &.{}, .default);
    defer cg.deinit();
    // Workers succeed independently; enumerate coordinator/append allocations.
    var executor = TestExecutor{ .allocator = backing, .lane = tasks.LaneState.init(backing), .lanes = 2 };
    defer executor.lane.deinit();
    defer std.debug.assert(executor.submitted == executor.received);
    var retained = try run(CG, a, &cg, &.{id}, .{
        .target = cg.getFragmentContract().target,
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
        .body = body,
        .ret_layout = .str,
    }, .none);
    const CG = Emitter.HostLirCodeGen;
    var cache = blk: {
        var strings = try @import("StaticStringData.zig").build(a, &store, roc_target.RocTarget.detectNative());
        defer strings.deinit();
        var cg = try CG.init(a, &store, &layouts, strings.view(), &.{}, .default);
        defer cg.deinit();
        var executor = TestExecutor{ .allocator = a, .lane = tasks.LaneState.init(a), .lanes = 2 };
        defer executor.lane.deinit();
        break :blk try run(CG, a, &cg, &demand, .{
            .target = cg.getFragmentContract().target,
            .executor = executor.executor(),
        });
    };
    var cache_live = true;
    defer if (cache_live) cache.deinit();
    var cg = try CG.init(a, &store, &layouts, .{}, &.{}, .default);
    defer cg.deinit();
    var metrics: Metrics = .{};
    var retained = try run(CG, a, &cg, &demand, .{
        .target = cg.getFragmentContract().target,
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
