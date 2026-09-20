//! Real compiler-worker coverage for deterministic native artifacts and their
//! ownership across code-generator, literal-table, and executable teardown.

const std = @import("std");
const base = @import("base");
const backend = @import("backend").dev;
const eval = @import("eval");
const lir = @import("lir");
const target = @import("roc_target").RocTarget;
const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;
const harness = @import("lower_to_lir_harness.zig");
const tasks = base.post_check_task_executor;
const Native = backend.NativeProcCompiler;
const CG = backend.HostLirCodeGen;
const allocator = std.testing.allocator;
const literal = "native artifacts retain this long source string after image teardown";

const AssertionError = error{ TestExpectedEqual, TestUnexpectedResult };
const EmissionError = std.mem.Allocator.Error || AssertionError || error{
    UnresolvedSymbol,
    InvalidRelocation,
    MappingFailed,
};
const InspectionError = EmissionError || backend.CompilationError ||
    eval.BuiltinModules.InitError || std.Thread.SpawnError ||
    @import("../coordinator.zig").CoordinatorError;

/// Delegate every callback to the real persistent compiler workers. The first
/// full batch rendezvous makes parallelism an assertion, not a timing guess.
const ObservedExecutor = struct {
    inner: tasks.Executor,
    reverse: bool,
    session: ?tasks.Session = null,
    slots: [4]?WrappedTask = @splat(null),
    completions: [4]tasks.Completion = undefined,
    buffered: usize = 0,
    outstanding: usize = 0,
    submitted: usize = 0,
    received: usize = 0,
    previous_completion: ?usize = null,
    out_of_order: bool = false,
    entered: std.atomic.Value(usize) = .init(0),
    lanes: std.atomic.Value(usize) = .init(0),

    const WrappedTask = struct {
        owner: *ObservedExecutor,
        task: tasks.Task,
        rendezvous: bool,

        fn run(context: *anyopaque, worker: tasks.Worker) ?*anyopaque {
            const self: *WrappedTask = @ptrCast(@alignCast(context));
            if (self.rendezvous) {
                _ = self.owner.lanes.fetchOr(@as(usize, 1) << @intCast(worker.id), .acq_rel);
                _ = self.owner.entered.fetchAdd(1, .acq_rel);
                while (self.owner.entered.load(.acquire) < self.owner.inner.worker_count) {
                    std.atomic.spinLoopHint();
                }
            }
            return self.task.run(self.task.context, worker);
        }
    };

    fn begin(context: *anyopaque) void {
        const self: *ObservedExecutor = @ptrCast(@alignCast(context));
        std.debug.assert(self.session == null);
        self.session = self.inner.begin();
    }

    fn submit(context: *anyopaque, task: tasks.Task) std.mem.Allocator.Error!void {
        const self: *ObservedExecutor = @ptrCast(@alignCast(context));
        for (&self.slots) |*slot| {
            if (slot.* != null) continue;
            slot.* = .{ .owner = self, .task = task, .rendezvous = self.submitted < self.inner.worker_count };
            errdefer slot.* = null;
            try self.session.?.submit(.{ .id = task.id, .context = &slot.*.?, .run = WrappedTask.run });
            self.submitted += 1;
            self.outstanding += 1;
            return;
        }
        unreachable;
    }

    fn receive(context: *anyopaque) tasks.Completion {
        const self: *ObservedExecutor = @ptrCast(@alignCast(context));
        const completion = if (self.reverse) blk: {
            if (self.buffered == 0) {
                while (self.outstanding > 0) {
                    self.completions[self.buffered] = self.session.?.receive();
                    self.buffered += 1;
                    self.outstanding -= 1;
                }
                // Arrival order is nondeterministic; descending task IDs ensure
                // the consumer actually sees a reversed logical batch.
                std.mem.sort(tasks.Completion, self.completions[0..self.buffered], {}, struct {
                    fn lessThan(_: void, lhs: tasks.Completion, rhs: tasks.Completion) bool {
                        return lhs.id < rhs.id;
                    }
                }.lessThan);
            }
            self.buffered -= 1;
            break :blk self.completions[self.buffered];
        } else blk: {
            self.outstanding -= 1;
            break :blk self.session.?.receive();
        };
        for (&self.slots) |*slot| {
            if (slot.*) |wrapped| {
                if (wrapped.task.id == completion.id) {
                    slot.* = null;
                    self.received += 1;
                    if (self.previous_completion) |previous| {
                        self.out_of_order = self.out_of_order or completion.id < previous;
                    }
                    self.previous_completion = completion.id;
                    return completion;
                }
            }
        }
        unreachable;
    }

    fn end(context: *anyopaque) void {
        const self: *ObservedExecutor = @ptrCast(@alignCast(context));
        std.debug.assert(self.outstanding == 0 and self.buffered == 0);
        self.session.?.end();
        self.session = null;
    }

    fn executor(self: *ObservedExecutor) tasks.Executor {
        return .{
            .context = self,
            .worker_count = self.inner.worker_count,
            .beginFn = begin,
            .submitFn = submit,
            .receiveFn = receive,
            .endFn = end,
        };
    }

    fn verify(self: *const ObservedExecutor) AssertionError!void {
        try std.testing.expectEqual(self.inner.worker_count, self.entered.load(.acquire));
        try std.testing.expectEqual(self.inner.worker_count, @as(usize, @popCount(self.lanes.load(.acquire))));
        try std.testing.expect(self.submitted > 0);
        try std.testing.expectEqual(self.submitted, self.received);
        if (self.reverse) try std.testing.expect(self.out_of_order);
    }
};

fn expectLiteral(retained: *const Native.Retained) (std.mem.Allocator.Error || AssertionError)!void {
    const data = try retained.dataItems(allocator);
    defer allocator.free(data);
    for (data) |item| {
        if (std.mem.find(u8, item.bytes, literal) != null) return;
    }
    return error.TestUnexpectedResult;
}

const Emission = struct {
    code: []const u8,
    retained: Native.Retained,
    metrics: Native.Metrics,

    fn deinit(self: *Emission) void {
        allocator.free(self.code);
        self.retained.deinit();
    }
};

fn emit(
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    demand: []const lir.LIR.LirProcSpecId,
    root: lir.LIR.LirProcSpecId,
    executor: ?tasks.Executor,
    reuse: ?*const Native.Retained,
) EmissionError!Emission {
    const program = &lowered.lir_result;
    var strings = try backend.StaticStringData.build(allocator, &program.store, target.detectNative());
    defer strings.deinit();
    var cg = try CG.initWithBoxyMetadata(
        allocator,
        &program.store,
        &program.layouts,
        strings.view(),
        program.boxy_erased_arg_desc_offsets.items,
        program.boxy_erased_arg_desc_params.items,
        program.boxy_worker_procs.items,
        .default,
    );
    defer cg.deinit();
    var metrics: Native.Metrics = .{};
    var retained = try Native.run(CG, allocator, &cg, demand, .{
        .target = target.detectNative(),
        .executor = executor,
        .reuse_same_program = reuse,
        .static_helpers = &.{.{ .op = .decref, .layout_idx = .str }},
        .metrics_out = &metrics,
    });
    errdefer retained.deinit();
    const result = try cg.generateCode(root, program.store.getProcSpec(root).ret_layout);
    errdefer allocator.free(result.code);
    var splice = backend.HostSplice.init(allocator);
    defer splice.deinit();
    const data = try retained.dataItems(allocator);
    defer allocator.free(data);
    try splice.addDataItems(data);
    const boxy_fns = eval.boxy_abi.nativeFnTable();
    var image = try splice.link(&cg, &boxy_fns);
    defer image.deinit();
    var answer: i64 = 0;
    image.callRocABIAt(result.entry_offset, @ptrCast(&answer), null);
    try std.testing.expectEqual(@as(i64, 42), answer);
    return .{ .code = result.code, .retained = retained, .metrics = metrics };
}

fn object(lowered: *const lir.CheckedPipeline.LoweredProgram, executor: ?tasks.Executor, metrics: *Native.Metrics) backend.CompilationError!backend.CompilationResult {
    const program = &lowered.lir_result;
    var compiler = backend.ObjectFileCompiler.initForPack(allocator);
    var timing = backend.ObjectFileCompiler.Timing.init(std.testing.io);
    compiler.timing = &timing;
    compiler.post_check_executor = executor;
    const result = try compiler.compileToObjectFile(
        &program.store,
        &program.layouts,
        &.{},
        &.{},
        program.store.getProcSpecs(),
        program.boxy_erased_arg_desc_offsets.items,
        program.boxy_erased_arg_desc_params.items,
        program.boxy_worker_procs.items,
        target.detectNative(),
    );
    metrics.* = timing.snapshot().native_emission;
    return result;
}

fn expectUseful(metrics: Native.Metrics) AssertionError!void {
    try std.testing.expect(metrics.procedures_emitted >= 4);
    try std.testing.expect(metrics.code_bytes_emitted > 0);
    try std.testing.expectEqual(metrics.tasks_submitted, metrics.tasks_committed);
    try std.testing.expectEqual(metrics.procedures_emitted + metrics.helpers_emitted, metrics.tasks_committed);
}

fn inspect(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    inspectFallible(lowered) catch |err| {
        std.log.err("native artifact parallel integration failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

fn inspectFallible(lowered: *const lir.CheckedPipeline.LoweredProgram) InspectionError!void {
    const program = &lowered.lir_result;
    const demand = try allocator.alloc(lir.LIR.LirProcSpecId, program.store.getProcSpecs().len);
    defer allocator.free(demand);
    try std.testing.expect(demand.len >= 4);
    for (demand, 0..) |*id, i| id.* = @enumFromInt(i);
    var root: ?lir.LIR.LirProcSpecId = null;
    for (program.root_procs.items) |id| {
        const proc = program.store.getProcSpec(id);
        if (proc.ret_layout == .i64 and program.store.getLocalSpan(proc.args).len == 0) root = id;
    }
    try std.testing.expect(root != null);
    var baseline = try emit(lowered, demand, root.?, null, null);
    defer baseline.deinit();
    try expectUseful(baseline.metrics);
    try std.testing.expect(baseline.metrics.helpers_emitted > 0);
    try expectLiteral(&baseline.retained);
    var object_metrics: Native.Metrics = .{};
    var baseline_object = try object(lowered, null, &object_metrics);
    defer baseline_object.deinit();
    try expectUseful(object_metrics);
    var builtins = try eval.BuiltinModules.init(allocator);
    defer builtins.deinit();
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    for ([_]usize{ 2, 4 }) |workers| {
        var coord = try Coordinator.init(allocator, .multi_threaded, workers, target.detectNative(), &builtins, @import("build_options").compiler_version, null, CoreCtx.default(allocator, arena.allocator(), std.testing.io));
        defer coord.deinit();
        try coord.start();
        try coord.coordinatorLoop();
        for ([_]bool{ false, true }) |reverse| {
            var observed = ObservedExecutor{ .inner = coord.postCheckExecutor(), .reverse = reverse };
            var emitted = try emit(lowered, demand, root.?, observed.executor(), null);
            defer emitted.deinit();
            try observed.verify();
            try expectUseful(emitted.metrics);
            try std.testing.expectEqualSlices(u8, baseline.code, emitted.code);
            // emit returned only after its image, generator and literal table died.
            try expectLiteral(&emitted.retained);
            var reused = try emit(lowered, demand, root.?, observed.executor(), &emitted.retained);
            defer reused.deinit();
            try std.testing.expectEqual(@as(u64, demand.len), reused.metrics.procedures_reused);
            try std.testing.expectEqual(@as(u64, 0), reused.metrics.tasks_submitted);
            try std.testing.expect(reused.metrics.code_bytes_reused > 0);
            try std.testing.expectEqual(emitted.metrics.helpers_emitted, reused.metrics.helpers_reused);
            try std.testing.expect(reused.metrics.helpers_reused > 0);
            try std.testing.expectEqualSlices(u8, baseline.code, reused.code);
            try expectLiteral(&reused.retained);
            var object_observed = ObservedExecutor{ .inner = coord.postCheckExecutor(), .reverse = reverse };
            var result = try object(lowered, object_observed.executor(), &object_metrics);
            defer result.deinit();
            try object_observed.verify();
            try expectUseful(object_metrics);
            try std.testing.expectEqualSlices(u8, baseline_object.object_bytes, result.object_bytes);
        }
    }
}

test "native artifacts real parallel workers deterministic code objects and retained data" {
    if (comptime !backend.host_lir_codegen_available) return error.SkipZigTest;
    var dir = std.testing.tmpDir(.{});
    defer dir.cleanup();
    try dir.dir.writeFile(std.testing.io, .{ .sub_path = "main.roc", .data =
        \\app [main!, text!] { pf: platform "./platform.roc" }
        \\a = |x| x + 1
        \\b = |x| x + 2
        \\c = |x| x + 3
        \\d = |x| x + 4
        \\main! : {} => I64
        \\main! = |_unit| a(8) + b(8) + c(8) + d(8)
        \\text! : {} => Str
        \\text! = |_unit| "native artifacts retain this long source string after image teardown"
    });
    try dir.dir.writeFile(std.testing.io, .{ .sub_path = "platform.roc", .data =
        \\platform ""
        \\    requires {} { main! : {} => I64, text! : {} => Str }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": run!, "roc_text": string!, "roc_add": add, "roc_subtract": subtract, "roc_multiply": multiply }
        \\    hosted {}
        \\run! : () => I64
        \\run! = || main!({})
        \\string! : () => Str
        \\string! = || text!({})
        \\add : I64 -> I64
        \\add = |x| x + 1
        \\subtract : I64 -> I64
        \\subtract = |x| x - 2
        \\multiply : I64 -> I64
        \\multiply = |x| x * 3
    });
    const path = try dir.dir.realPathFileAlloc(std.testing.io, "main.roc", allocator);
    defer allocator.free(path);
    try harness.runAppPathLoweredInspection(path, .{}, inspect);
}
