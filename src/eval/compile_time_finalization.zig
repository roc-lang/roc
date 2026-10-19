//! Compile-time evaluation finalization.
//!
//! This module is intentionally only the checking finalizer boundary.
//! Compile-time values must be stored through the checked `ConstStore` path
//! produced by the post-check pipeline.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const backend = @import("backend");
const builtins = @import("builtins");
const check = @import("check");
const collections = @import("collections");
const lir = @import("lir");
const roc_target = @import("roc_target");

const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const CompilerHost = @import("compiler_host.zig");
const ConstStoreWriter = @import("const_store_writer.zig");
const NativeRootExport = @import("native_root_export.zig");
const FrozenRootTranscode = @import("frozen_root_transcode.zig");
const CompileTimeHost = @import("compile_time_host.zig");
const RootGraph = @import("compile_time_root_graph.zig");
const boxy_abi = @import("boxy_abi.zig");
const interpreter_mod = @import("interpreter.zig");
const interpreter_static_data = @import("interpreter_static_data.zig");
const static_data_exports = @import("static_data");
const Interpreter = interpreter_mod.Interpreter;
const ExpectFailure = interpreter_mod.ExpectFailure;
const FinalizeError = checked.CompileTimeFinalizer.Error;
pub const RuntimeMaterializationError = lir.CheckedPipeline.LowerResourceError || FinalizeError;
const LirProgram = lir.Program;
const BoxyBuiltinFn = backend.LirCodeGenMod.BoxyBuiltinFn;
const BoxyNativeFnTable = backend.LirCodeGenMod.BoxyNativeFnTable;
const NativeProcCompiler = backend.dev.NativeProcCompiler;

/// Borrowed compile-time `dbg` observation delivered while finalized roots are
/// replayed in deterministic request order.
pub const EventView = union(enum) {
    dbg: []const u8,
};

/// Borrowed callback used to forward structured compile-time events to a
/// caller such as the REPL without routing them through stderr.
pub const EventCallback = struct {
    context: *anyopaque,
    notify: *const fn (*anyopaque, EventView) void,
};

/// Where the compile-time evaluator splices object-cache entries from.
pub const SpliceSource = backend.dev.SpliceSource;

/// The object cache compile-time evaluation reads, the same way in every
/// command: entries compiled for the host under the dev policy, which run
/// expects. The evaluator's program takes a hit for a procedure it runs only
/// when the entry names that procedure's own identity, and splices its cached
/// code in place of compiling it.
pub const CompileTimeObjectCache = struct {
    spec_cache: lir.CheckedPipeline.SpecCacheLookup,
    splice_source: SpliceSource,
};
/// Runtime options for compile-time finalization.
pub const Options = struct {
    pub const StderrWriter = struct {
        context: ?*anyopaque = null,
        write: *const fn (?*anyopaque, []const u8) void = noop,

        fn noop(_: ?*anyopaque, _: []const u8) void {}

        fn writeAll(self: StderrWriter, bytes: []const u8) void {
            self.write(self.context, bytes);
        }
    };

    max_threads: usize = 0,
    /// Borrow the coordinator's workers after all frontend tasks have finished.
    /// Standalone finalizers without a coordinator lower on their calling thread.
    post_check_executor: ?base.post_check_task_executor.Executor = null,
    stderr: ?StderrWriter = null,
    event_callback: ?EventCallback = null,
    debug_events: ?*DebugEvents = null,
    /// Artifact copies store results independently; shared slots publish once.
    publish_shared_slots: bool = true,
    /// A coordinator may persist an early dependency batch, then replay all
    /// stored and new observations together with the complete program.
    defer_debug_replay: bool = false,
    /// Completed checked artifacts contribute stored observations, without
    /// evaluating their roots again. Borrowed only for finalization.
    cached_debug_modules: []const *const checked.CheckedModuleArtifact = &.{},
    std_io: ?std.Io = null,
    slow_root_threshold_ns: u64 = 3 * std.time.ns_per_s,
    slow_root_period_ns: u64 = std.time.ns_per_s,
    timing: ?*Timing = null,
    object_cache: ?CompileTimeObjectCache = null,
    /// Keep each stage's code as object-cache artifacts
    /// (`CompletedStages.artifacts`): what an object cache publishes.
    keep_artifacts: bool = false,
    /// Where a compile-time failure is reported when the source it names
    /// belongs to a checked module this finalization does not complete: a
    /// literal in a module whose checking finished in an earlier compilation,
    /// reached through a specialization this program makes.
    unfinalized_reports: ?UnfinalizedReports = null,
    /// While a group of roots that read each other evaluates, a member whose
    /// evaluation failed by reading a member not yet evaluated is neither
    /// reported nor completed, so the group can run it again.
    retry: ?*const CycleRetry = null,
};

/// The members of a group of compile-time roots that read each other
/// (design.md "Dependency-Ordered Compile-Time Evaluation").
const CycleRetry = struct {
    pending: []const StaticSlotEnvironment.ProducerOriginKey,

    /// Whether a failure with `meaning` read a member not yet evaluated.
    fn holds(self: *const CycleRetry, meaning: ?lir.LIR.FailureMeaning) bool {
        const value = switch (meaning orelse return false) {
            .failed_value => |value| value,
            .checked_error, .literal_rejection, .exhaustiveness => return false,
        };
        for (self.pending) |member| {
            if (moduleKeyBytesEqual(member.module, value.module) and member.producer.eql(value.producer)) return true;
        }
        return false;
    }
};

/// The report destination of a checked module this finalization does not
/// complete: one whose finalization completed in an earlier compilation, or
/// the builtin module.
pub const UnfinalizedReports = struct {
    context: *anyopaque,
    module: *const fn (context: *anyopaque, key: checked.ModuleId) Allocator.Error!ReportDestination,
};

/// A checked module's source, and the store its reports go to.
pub const ReportDestination = struct {
    module: *const checked.CheckedModuleArtifact,
    problem_store: ?*check.problem.Store,
};

const DebugEvents = struct {
    const Event = struct {
        module: checked.ModuleId,
        root: checked.ComptimeRootId,
        sequence: usize,
        is_repl: bool,
        message: []const u8,
        /// Observed by this compilation's evaluation, rather than replayed
        /// from a root an earlier compilation evaluated.
        fresh: bool = true,
    };
    allocator: Allocator,
    events: std.ArrayList(Event) = .empty,

    fn deinit(self: *DebugEvents) void {
        for (self.events.items) |event| self.allocator.free(event.message);
        self.events.deinit(self.allocator);
    }

    fn append(self: *DebugEvents, module: checked.ModuleId, root: checked.CompileTimeRoot, message: []const u8) Allocator.Error!void {
        return self.appendEvent(module, root, message, true);
    }

    fn appendEvent(self: *DebugEvents, module: checked.ModuleId, root: checked.CompileTimeRoot, message: []const u8, fresh: bool) Allocator.Error!void {
        const owned = try self.allocator.dupe(u8, message);
        errdefer self.allocator.free(owned);
        try self.events.append(self.allocator, .{ .module = module, .root = root.id, .sequence = self.events.items.len, .is_repl = root.kind == .repl_expr, .message = owned, .fresh = fresh });
    }

    fn lessThan(_: void, a: Event, b: Event) bool {
        const order = std.mem.order(u8, &a.module.bytes, &b.module.bytes);
        if (order != .eq) return order == .lt;
        if (a.root != b.root) return @backingInt(a.root) < @backingInt(b.root);
        return a.sequence < b.sequence;
    }

    /// Replay the observations an earlier compilation stored for the roots
    /// this compilation demands: every root, or, for a demand-driven
    /// compilation, the stored roots its program demands.
    fn appendStored(self: *DebugEvents, module: *const checked.CheckedModuleArtifact, demanded: ?*const StoredRootSet) Allocator.Error!void {
        for (module.compile_time_debug.entries) |entry| {
            if (demanded) |set| if (!set.contains(.{ .module = module.key.bytes, .root = entry.root })) continue;
            try self.appendEvent(module.key, module.compile_time_roots.root(entry.root), module.compile_time_debug.message(entry), false);
        }
    }

    fn appendCached(self: *DebugEvents, modules: []const *const checked.CheckedModuleArtifact, demanded: ?*const StoredRootSet) Allocator.Error!void {
        for (modules) |module| try self.appendStored(module, demanded);
    }

    /// Store each finalized module's observations: those it already held
    /// for roots an earlier compilation evaluated, and this compilation's.
    fn persist(self: *DebugEvents, modules: []const ProgramModule) Allocator.Error!void {
        std.mem.sort(Event, self.events.items, {}, lessThan);
        var inputs = std.ArrayList(checked.CompileTimeDebugStore.Input).empty;
        defer inputs.deinit(self.allocator);
        for (modules) |entry| {
            inputs.clearRetainingCapacity();
            const previous = entry.module.compile_time_debug;
            for (previous.entries) |stored| try inputs.append(self.allocator, .{ .root = stored.root, .message = previous.message(stored) });
            for (self.events.items) |event| {
                if (event.fresh and std.meta.eql(entry.module.key, event.module)) try inputs.append(self.allocator, .{ .root = event.root, .message = event.message });
            }
            std.mem.sort(checked.CompileTimeDebugStore.Input, inputs.items, {}, struct {
                fn lessThan(_: void, a: checked.CompileTimeDebugStore.Input, b: checked.CompileTimeDebugStore.Input) bool {
                    return @backingInt(a.root) < @backingInt(b.root);
                }
            }.lessThan);
            // The artifact owns these bytes for cache publication and replay.
            const artifact_allocator = entry.module.evaluationAllocator();
            const stored = try checked.CompileTimeDebugStore.init(artifact_allocator, inputs.items);
            if (entry.module.freesReplacedEvaluationColumns()) entry.module.compile_time_debug.deinit(artifact_allocator);
            entry.module.compile_time_debug = stored;
        }
    }

    fn replay(self: *DebugEvents, options: Options) Allocator.Error!void {
        std.mem.sort(Event, self.events.items, {}, lessThan);
        for (self.events.items) |event| try emitDebugMessage(self.allocator, options, event.is_repl, event.message);
    }
};

/// A prepared module and its checking-owned diagnostic destination.
pub const ProgramModule = struct {
    module: *checked.CheckedModuleArtifact,
    problem_store: ?*check.problem.Store,
};

/// Resolution from one lowered program's dense checked-module ids to the
/// modules this finalization owns.
///
/// A lowered program contains procedures and compile-time sites from every
/// checked module of its lowering input, so a row's module-local checked ids
/// belong to whichever module the producer recorded on that row. Building this
/// once turns every later owner question into a direct array index.
const ModuleOwners = struct {
    modules: []const ProgramModule,
    /// Position in `modules` per `LIR.LoweringModuleId`, or null when the
    /// lowering carried a checked module this finalization does not complete.
    positions: []const ?u32,
    /// Independently owned artifacts with the same checked identity.
    next_alias: []const ?u32,
    /// Site observations apply once to every artifact of the declared owner.
    report_sites: bool = true,
    /// The lowering's module table, by `LIR.LoweringModuleId`.
    lowering_modules: []const checked.ModuleId,
    unfinalized: ?UnfinalizedReports = null,
    /// The failures of this program's literal roots, when it has any.
    literal_failures: ?*LiteralRootFailures = null,

    /// The report destinations of source owned by one lowering module.
    const ReportTargets = struct {
        owners: *const ModuleOwners,
        next: ?u32,
        unfinalized: ?ReportDestination,

        fn nextTarget(self: *ReportTargets) ?ReportDestination {
            if (self.unfinalized) |target| {
                self.unfinalized = null;
                return target;
            }
            const index = self.next orelse return null;
            self.next = self.owners.next_alias[index];
            const owner = self.owners.modules[index];
            return .{ .module = owner.module, .problem_store = owner.problem_store };
        }
    };

    fn init(
        allocator: Allocator,
        lowering_modules: []const checked.ModuleId,
        modules: []const ProgramModule,
    ) Allocator.Error!ModuleOwners {
        const positions = try allocator.alloc(?u32, lowering_modules.len);
        errdefer allocator.free(positions);
        const next_alias = try allocator.alloc(?u32, modules.len);
        errdefer allocator.free(next_alias);
        @memset(next_alias, null);
        for (lowering_modules, positions) |key, *slot| {
            slot.* = null;
            var tail = slot;
            for (modules, 0..) |entry, index| {
                if (!entry.module.key.eql(key)) continue;
                tail.* = @intCast(index);
                tail = &next_alias[index];
            }
        }
        return .{ .modules = modules, .positions = positions, .next_alias = next_alias, .lowering_modules = lowering_modules };
    }

    fn deinit(self: *const ModuleOwners, allocator: Allocator) void {
        allocator.free(self.next_alias);
        allocator.free(self.positions);
    }

    fn position(self: *const ModuleOwners, id: lir.LIR.LoweringModuleId) ?u32 {
        const raw = @backingInt(id);
        if (raw >= self.positions.len) {
            finalizationInvariant("lowered row named a checked module outside the lowering's module table");
        }
        return self.positions[raw];
    }

    fn get(self: *const ModuleOwners, id: lir.LIR.LoweringModuleId) ?ProgramModule {
        const index = self.position(id) orelse return null;
        return self.modules[index];
    }

    /// Every artifact this finalization completes for lowering module `id`,
    /// or, for a module whose finalization completed earlier, its report
    /// destination.
    fn reportTargets(self: *const ModuleOwners, id: lir.LIR.LoweringModuleId) Allocator.Error!ReportTargets {
        if (self.position(id)) |first| return .{ .owners = self, .next = first, .unfinalized = null };
        const reports = self.unfinalized orelse
            finalizationInvariant("compile-time failure named a checked module this finalization neither completes nor reports for");
        return .{ .owners = self, .next = null, .unfinalized = try reports.module(reports.context, self.lowering_modules[@backingInt(id)]) };
    }
};

/// Retains the compilation's compile-time results, and the specialized
/// program its runtime consumer continues, across checking completion.
/// Diagnostic destinations are consumed during finalizeProgram.
pub const ProgramSession = struct {
    /// Boxy materializes its own literal results after shared finalization.
    /// This program already owns those frozen values when runtime takes it.
    allocator: Allocator,
    modules: lir.CheckedPipeline.CheckedModuleSet,
    runtime_requests: []const checked.RootRequest,
    runtime_roots: lir.CheckedPipeline.RootRequestSet,
    runtime_target: ?lir.CheckedPipeline.TargetConfig,
    /// The programs compile-time evaluation ran in, holding every completed
    /// value the runtime consumer reads. Null when this compilation evaluated
    /// nothing.
    completed: ?CompletedStages,
    /// The Solved program the runtime consumer continues, when its Solved
    /// policy is compile-time evaluation's.
    runtime_prepared: ?lir.CheckedPipeline.PreparedSolved,
    /// The position of each runtime request in the specialized program's root
    /// plan.
    runtime_positions: []u32,

    pub fn deinit(self: *ProgramSession) void {
        if (self.completed) |*completed| completed.deinit();
        if (self.runtime_prepared) |*prepared| prepared.deinit();
        self.allocator.free(self.runtime_positions);
        self.allocator.free(self.modules.root.relation_modules);
        self.allocator.free(self.modules.imports);
        deinitRootRequests(self.allocator, self.runtime_roots);
        self.* = undefined;
    }

    /// The compile-time reads of the specializations compile-time
    /// evaluation lowered: its own, or, while a runtime consumer is still to
    /// continue the same Solved program, that program's.
    pub fn specComptimeReads(self: *const ProgramSession) ?*const lir.CheckedPipeline.SpecComptimeReads {
        if (self.completed) |*completed| if (completed.specComptimeReads()) |reads| return reads;
        const prepared = if (self.runtime_prepared) |*prepared| prepared else return null;
        return if (prepared.spec_comptime_reads) |*reads| reads else null;
    }

    pub fn takeRuntime(
        self: *ProgramSession,
        allocator: Allocator,
        roots: lir.CheckedPipeline.RootRequestSet,
        requested_target: lir.CheckedPipeline.TargetConfig,
    ) RuntimeMaterializationError!lir.CheckedPipeline.LoweredProgram {
        const configured = self.runtime_target orelse finalizationInvariant("check-only session has no runtime consumer");
        // Whether the program's specializations go to the object cache was
        // decided with the consumer, before the shared program was lowered.
        var target = requested_target;
        target.spec_comptime_reads = configured.spec_comptime_reads;
        inline for (comptime std.meta.tags(std.meta.FieldEnum(lir.CheckedPipeline.TargetConfig))) |field| {
            if (comptime field == .timing or
                field == .work_metrics or
                field == .post_check_executor or
                field == .solved_lir_parallel_metrics_out or
                field == .lifted_expr_count_out or
                field == .completed_scalar_values)
            {
                continue;
            }
            if (!std.meta.eql(@field(configured, @tagName(field)), @field(target, @tagName(field))))
                finalizationInvariant("runtime policy differs from the compilation's declared consumer");
        }
        inline for (@typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".field_names, @typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".field_types) |field_name, field_type| {
            const expected = @field(self.runtime_roots, field_name);
            const actual = @field(roots, field_name);
            if (@typeInfo(field_type) == .pointer) {
                if (expected.len != actual.len) finalizationInvariant("runtime request differs from the declared consumer");
                for (expected, actual) |a, b| {
                    if (!std.meta.eql(a, b)) finalizationInvariant("runtime root metadata differs from the declared consumer");
                }
            } else if (expected != actual) finalizationInvariant("runtime request policy differs from the declared consumer");
        }
        self.runtime_target = null;
        if (target.specialization_strategy == .boxy) {
            try lir.CheckedPipeline.requireHostedProceduresBound(self.modules, target);
            return materializeBoxyLiterals(allocator, self.modules, roots, target);
        }
        if (self.runtime_prepared) |prepared| {
            self.runtime_prepared = null;
            var owned = prepared;
            lir.CheckedPipeline.requireHostedProceduresBound(self.modules, target) catch |err| {
                owned.deinit();
                return err;
            };
            return self.continueRuntimeConsumer(owned, target);
        }
        // A Boxy runtime program has no Monotype stage to share, and a
        // compilation with no program roots specialized nothing: either
        // lowers the checked modules itself, reading every compile-time value
        // from the modules' constant stores.
        return lir.CheckedPipeline.lowerCheckedModulesToLir(allocator, self.modules, roots, target);
    }

    /// Lower the runtime consumer's own share of the specialized program,
    /// under its own target and LIR policy, reading every compile-time value
    /// from the completed evaluation. The specialized program has no consumer
    /// after this one, so lowering releases it.
    fn continueRuntimeConsumer(
        self: *ProgramSession,
        prepared: lir.CheckedPipeline.PreparedSolved,
        target: lir.CheckedPipeline.TargetConfig,
    ) lir.CheckedPipeline.LowerResourceError!lir.CheckedPipeline.LoweredProgram {
        var owned = prepared;
        var owned_live = true;
        errdefer if (owned_live) owned.deinit();
        const consumer_roots: lir.CheckedPipeline.ConsumerRoots = .{ .roots = self.runtime_positions, .literal_roots = .none };
        const source = if (self.completed) |*completed| completed else {
            // Nothing was evaluated, so the program reads no compile-time
            // value slot.
            owned_live = false;
            return lir.CheckedPipeline.lowerFinalConsumerToLir(owned, .{
                .roots = consumer_roots,
                .target_usize = target.target_usize,
                .erased_capture_prefix = target.erased_capture_prefix,
                .inline_expects = target.inline_expects,
                .observers = lir.CheckedPipeline.Observers.fromTarget(target),
                .lir_policy = lir.CheckedPipeline.LirPolicy.fromTarget(target),
            });
        };
        // Every compile-time producer has completed, so its scalar values
        // lower as literals here. The completed values are transcoded after
        // target LIR generation and before reachability compacts the target
        // tables.
        source.pending.clearRetainingCapacity();
        owned_live = false;
        var lowered = try lir.CheckedPipeline.lowerFinalConsumerToLir(owned, .{
            .roots = consumer_roots,
            .target_usize = target.target_usize,
            .erased_capture_prefix = target.erased_capture_prefix,
            .inline_expects = target.inline_expects,
            .completed_scalar_values = &source.scalar_values,
            .frozen_materializer = .{
                .context = source,
                .materialize = CompletedStages.materializeStage,
            },
            .observers = lir.CheckedPipeline.Observers.fromTarget(target),
            .lir_policy = lir.CheckedPipeline.LirPolicy.fromTarget(target),
        });
        errdefer lowered.deinit();
        if (lowered.lir_result.root_procs.items.len != self.runtime_positions.len)
            finalizationInvariant("runtime consumer lowering changed the requested root count");
        try lir.CheckedPipeline.adoptReachableCompletedComptimeValues(&lowered);
        // The stages' procedures have no reader left: the runtime program has
        // its own, and only their completed values and representation
        // metadata are still consulted.
        source.releaseCode();
        return lowered;
    }
};

/// The programs a staged evaluation ran its levels in, with the values they
/// completed: where each later level, and a runtime consumer continuing the
/// same Solved program, reads completed values from.
pub const CompletedStages = struct {
    allocator: Allocator,
    /// Every stage's program, in evaluation order. Each is boxed, so the
    /// pointers evaluation keeps stay valid as stages are added.
    stages: std.ArrayList(*lir.CheckedPipeline.LoweredProgram) = .empty,
    /// Per stage, the frozen export of each of its slots.
    stage_symbols: std.ArrayList([]?static_data_exports.StaticDataSymbolId) = .empty,
    /// The stage and slot holding each completed producer's value and
    /// failure record.
    sources: std.AutoHashMapUnmanaged(SlotKey, StageSlot) = .empty,
    /// The completed successful values that lower as constructions.
    scalar_values: lir.CheckedPipeline.CompletedScalarValues,
    /// The producers of the stage being lowered, whose slots it evaluates.
    pending: std.AutoHashMapUnmanaged(ProducerSlotKey, void) = .empty,
    /// The specializations' compile-time reads, which the object cache
    /// writes beside the program's procedures.
    spec_comptime_reads: ?lir.CheckedPipeline.SpecComptimeReads = null,
    /// Each stage's code as object-cache artifacts, when the evaluation
    /// writes to an object cache.
    artifacts: std.ArrayList(backend.dev.ProcArtifact.Set) = .empty,
    /// The discovery summaries of the evaluation's program (design.md
    /// "Discovery Summaries"); null when the evaluation produced a message a
    /// later compilation that skipped its bodies would not repeat.
    discovery_summaries: ?lir.CheckedPipeline.DiscoverySummary.Summaries = null,

    const ComptimeRootOwner = @typeInfo(@FieldType(LirProgram.StaticDataValue, "compile_time_root")).optional.child;
    const SlotKey = struct {
        module: checked.ModuleId,
        root: lir.LIR.ComptimeProducer,
        role: std.meta.Tag(@FieldType(ComptimeRootOwner, "role")),
    };
    const ProducerSlotKey = struct {
        module: checked.ModuleId,
        root: lir.LIR.ComptimeProducer,
    };
    const StageSlot = struct { stage: u32, slot: u32 };

    fn init(allocator: Allocator) CompletedStages {
        return .{ .allocator = allocator, .scalar_values = lir.CheckedPipeline.CompletedScalarValues.initEmpty(allocator) };
    }

    pub fn deinit(self: *CompletedStages) void {
        for (self.stages.items) |stage| {
            stage.deinit();
            self.allocator.destroy(stage);
        }
        self.stages.deinit(self.allocator);
        for (self.stage_symbols.items) |symbols| self.allocator.free(symbols);
        self.stage_symbols.deinit(self.allocator);
        self.sources.deinit(self.allocator);
        self.scalar_values.deinit(self.allocator);
        self.pending.deinit(self.allocator);
        if (self.spec_comptime_reads) |*reads| reads.deinit();
        for (self.artifacts.items) |*set| set.deinit();
        self.artifacts.deinit(self.allocator);
        if (self.discovery_summaries) |*summaries| summaries.deinit();
        self.* = undefined;
    }

    /// The stages' code as one artifact set, one copy of each procedure.
    pub fn combinedArtifacts(self: *const CompletedStages, allocator: Allocator) Allocator.Error!backend.dev.ProcArtifact.Set {
        const sets = try allocator.alloc(*const backend.dev.ProcArtifact.Set, self.artifacts.items.len);
        defer allocator.free(sets);
        for (self.artifacts.items, sets) |*set, *ptr| ptr.* = set;
        return backend.dev.ProcArtifact.combineDistinct(allocator, sets);
    }

    /// The stages' programs, in evaluation order.
    pub fn programs(self: *const CompletedStages) []const *const lir.CheckedPipeline.LoweredProgram {
        return self.stages.items;
    }

    /// The discovery summaries a compilation that reports no error publishes.
    pub fn discoverySummaries(self: *const CompletedStages) []const lir.CheckedPipeline.DiscoverySummary.Summary {
        return if (self.discovery_summaries) |summaries| summaries.items else &.{};
    }

    /// The specializations' compile-time reads, when this evaluation's
    /// Solved program has no later consumer to hand them to.
    pub fn specComptimeReads(self: *const CompletedStages) ?*const lir.CheckedPipeline.SpecComptimeReads {
        return if (self.spec_comptime_reads) |*reads| reads else null;
    }

    /// Release every stage's code: only completed values and their
    /// representation metadata are still consulted.
    fn releaseCode(self: *CompletedStages) void {
        for (self.stages.items) |stage| stage.releaseCode();
    }

    /// Lower the next stage: the producers `stage_producers` name, reading
    /// every value an earlier stage completed.
    fn lowerStage(
        self: *CompletedStages,
        prepared: *lir.CheckedPipeline.PreparedSolved,
        consumer: lir.CheckedPipeline.Consumer,
        stage_producers: []const usize,
        producers: Producers,
        lowering_modules: []const checked.ModuleId,
    ) FinalizeError!*lir.CheckedPipeline.LoweredProgram {
        self.pending.clearRetainingCapacity();
        for (stage_producers) |ordinal| try self.pending.put(self.allocator, producerSlotKey(producers, lowering_modules, ordinal), {});
        var stage_consumer = consumer;
        stage_consumer.completed_scalar_values = &self.scalar_values;
        stage_consumer.frozen_materializer = .{ .context = self, .materialize = materializeStage };
        const stage = try self.allocator.create(lir.CheckedPipeline.LoweredProgram);
        errdefer self.allocator.destroy(stage);
        stage.* = lir.CheckedPipeline.lowerConsumerToLir(prepared, stage_consumer) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.HostedFunctionNotBound => finalizationInvariant("compile-time evaluation required every hosted procedure to be bound"),
        };
        errdefer stage.deinit();
        try self.stages.append(self.allocator, stage);
        return stage;
    }

    /// Record the values the last stage completed: those of the producers
    /// it was lowered for.
    fn finishStage(self: *CompletedStages) Allocator.Error!void {
        const stage_index: u32 = @intCast(self.stages.items.len - 1);
        const stage = self.stages.items[stage_index];
        const frozen = if (stage.frozen_static_data) |*frozen| frozen else finalizationInvariant("compile-time stage froze no completed values");
        const symbols = try self.allocator.alloc(?static_data_exports.StaticDataSymbolId, stage.lir_result.static_data_values.items.len);
        errdefer self.allocator.free(symbols);
        @memset(symbols, null);
        for (frozen.exports, 0..) |item, symbol| {
            const slot = item.value_id orelse continue;
            symbols[@backingInt(slot)] = @fromBackingInt(@intCast(symbol));
        }
        try self.stage_symbols.append(self.allocator, symbols);
        for (stage.lir_result.static_data_values.items, 0..) |entry, ordinal| {
            const root = entry.compile_time_root orelse continue;
            if (!self.pending.contains(.{ .module = root.module, .root = root.root })) continue;
            const gop = try self.sources.getOrPut(self.allocator, .{ .module = root.module, .root = root.root, .role = std.meta.activeTag(root.role) });
            if (gop.found_existing) finalizationInvariant("checked root has ambiguous source value slots");
            gop.value_ptr.* = .{ .stage = stage_index, .slot = @intCast(ordinal) };
        }
        try self.scalar_values.add(self.allocator, &stage.lir_result, frozen, .{ .context = self, .admits = admitsPending });
        self.pending.clearRetainingCapacity();
    }

    fn admitsPending(context: *const anyopaque, module: checked.ModuleId, root: lir.LIR.ComptimeProducer) bool {
        const self: *const CompletedStages = @ptrCast(@alignCast(context));
        return self.pending.contains(.{ .module = module, .root = root });
    }

    fn producerSlotKey(producers: Producers, lowering_modules: []const checked.ModuleId, ordinal: usize) ProducerSlotKey {
        if (ordinal < producers.roots.len) {
            const root = producers.roots[ordinal];
            return .{ .module = lowering_modules[@backingInt(root.owner)], .root = .{ .checked = root.request.compile_time_root.? } };
        }
        const index = ordinal - producers.roots.len;
        return .{ .module = producers.literals[index].module, .root = .{ .literal = @fromBackingInt(@intCast(index)) } };
    }

    /// The source of a completed value's slot.
    fn find(self: *const CompletedStages, module: checked.ModuleId, root: lir.LIR.ComptimeProducer, role: std.meta.Tag(@FieldType(ComptimeRootOwner, "role"))) ?SlotSource {
        const found = self.sources.get(.{ .module = module, .root = root, .role = role }) orelse return null;
        return .{
            .program = self.stages.items[found.stage],
            .symbols = self.stage_symbols.items[found.stage],
            .slot = found.slot,
        };
    }

    /// Whether the stage being lowered evaluates `module`'s `root` itself.
    fn evaluatesHere(self: *const CompletedStages, module: checked.ModuleId, root: lir.LIR.ComptimeProducer) bool {
        return self.pending.contains(.{ .module = module, .root = root });
    }

    fn materializeStage(allocator: Allocator, context: *anyopaque, target: *LirProgram.Result) Allocator.Error!LirProgram.FrozenStaticData {
        const self: *CompletedStages = @ptrCast(@alignCast(context));
        return transcodeCompletedSlots(allocator, self, target);
    }
};

/// One completed value's slot in the program that froze it.
const SlotSource = struct {
    program: *const lir.CheckedPipeline.LoweredProgram,
    /// That program's frozen export of each of its slots.
    symbols: []const ?static_data_exports.StaticDataSymbolId,
    slot: u32,
};

/// The completed values of one evaluated program, every one of its slots a
/// source.
const ProgramSlots = struct {
    program: *const lir.CheckedPipeline.LoweredProgram,
    symbols: []?static_data_exports.StaticDataSymbolId,
    slots: std.AutoHashMapUnmanaged(CompletedStages.SlotKey, u32) = .empty,

    fn init(allocator: Allocator, program: *const lir.CheckedPipeline.LoweredProgram) Allocator.Error!ProgramSlots {
        const frozen = program.frozen_static_data orelse finalizationInvariant("host program omitted its completed frozen values");
        const symbols = try allocator.alloc(?static_data_exports.StaticDataSymbolId, program.lir_result.static_data_values.items.len);
        errdefer allocator.free(symbols);
        @memset(symbols, null);
        for (frozen.exports, 0..) |item, symbol| {
            const slot = item.value_id orelse continue;
            symbols[@backingInt(slot)] = @fromBackingInt(@intCast(symbol));
        }
        var result = ProgramSlots{ .program = program, .symbols = symbols };
        errdefer result.slots.deinit(allocator);
        for (program.lir_result.static_data_values.items, 0..) |entry, ordinal| {
            const root = entry.compile_time_root orelse continue;
            const gop = try result.slots.getOrPut(allocator, .{ .module = root.module, .root = root.root, .role = std.meta.activeTag(root.role) });
            if (gop.found_existing) finalizationInvariant("checked root has ambiguous source value slots");
            gop.value_ptr.* = @intCast(ordinal);
        }
        return result;
    }

    fn deinit(self: *ProgramSlots, allocator: Allocator) void {
        allocator.free(self.symbols);
        self.slots.deinit(allocator);
    }

    fn find(self: *const ProgramSlots, module: checked.ModuleId, root: lir.LIR.ComptimeProducer, role: std.meta.Tag(@FieldType(CompletedStages.ComptimeRootOwner, "role"))) ?SlotSource {
        const slot = self.slots.get(.{ .module = module, .root = root, .role = role }) orelse return null;
        return .{ .program = self.program, .symbols = self.symbols, .slot = slot };
    }

    fn evaluatesHere(_: *const ProgramSlots, _: checked.ModuleId, _: lir.LIR.ComptimeProducer) bool {
        return false;
    }
};

/// Freeze each of `target`'s compile-time value slots that `sources` holds
/// a completed value for, matched by owner module and producer identity.
/// Target representation comes exclusively from the paired slot plans. A
/// slot `sources` holds no value for is one the target evaluates itself.
fn transcodeCompletedSlots(
    allocator: Allocator,
    sources: anytype,
    target: *LirProgram.Result,
) Allocator.Error!LirProgram.FrozenStaticData {
    var exports = std.ArrayList(static_data_exports.StaticDataExport).empty;
    errdefer {
        for (exports.items) |item| {
            allocator.free(item.symbol_name);
            allocator.free(item.bytes);
            for (item.relocations) |relocation| if (relocation.owns_target_symbol_name) allocator.free(relocation.target_symbol_name);
            allocator.free(item.relocations);
        }
        exports.deinit(allocator);
    }
    for (target.static_data_values.items, 0..) |target_entry, index| {
        const target_root = target_entry.compile_time_root orelse continue;
        const target_slot: lir.LIR.StaticDataId = @fromBackingInt(@intCast(index));
        const source = sources.find(target_root.module, target_root.root, std.meta.activeTag(target_root.role)) orelse {
            if (sources.evaluatesHere(target_root.module, target_root.root)) continue;
            finalizationInvariant("target slot has no corresponding host root");
        };
        const source_result = &source.program.lir_result;
        const frozen = source.program.frozen_static_data.?;
        const source_entry = source_result.static_data_values.items[source.slot];
        const source_symbol = frozenSlotSymbol(source.symbols, @fromBackingInt(source.slot));
        const failed = if (target_root.role == .value) block: {
            const failure_slot = source_entry.compile_time_root.?.role.value.failure_slot;
            const failure_entry = source_result.static_data_values.items[@backingInt(failure_slot)];
            const failure_symbol = frozenSlotSymbol(source.symbols, failure_slot);
            const failure_export = frozen.exports[@backingInt(failure_symbol)];
            const offset = failure_entry.compile_time_root.?.role.failure_message.failed_offset;
            break :block failure_export.bytes[failure_export.symbol_offset + offset] != 0;
        } else false;
        const converted = if (target_root.role == .failure_message)
            try FrozenRootTranscode.transcodeFailure(allocator, source_result, source_entry, frozen.exports, source_symbol, target, target_slot)
        else if (failed)
            try uninitializedFailedSlot(allocator, target, target_slot)
        else
            try FrozenRootTranscode.transcodeValueSlot(allocator, source_result, source_entry, frozen.exports, source_symbol, target, target_slot);
        appendFrozenGraph(allocator, &exports, converted) catch |err| {
            static_data_exports.deinitStaticData(allocator, converted);
            return err;
        };
        allocator.free(converted);
    }
    // Roots evaluated separately freeze their equal backings separately.
    return .{ .allocator = allocator, .exports = try static_data_exports.mergeIdenticalNodes(allocator, try exports.toOwnedSlice(allocator)) };
}

fn frozenSlotSymbol(symbols: []const ?static_data_exports.StaticDataSymbolId, slot: lir.LIR.StaticDataId) static_data_exports.StaticDataSymbolId {
    return symbols[@backingInt(slot)] orelse finalizationInvariant("completed frozen graph omitted a declared slot");
}

/// Failed values have no representation to convert. Their explicit guard
/// owns the failure; reserve the destination slot without inspecting its bytes.
fn uninitializedFailedSlot(allocator: Allocator, program: *const LirProgram.Result, slot: lir.LIR.StaticDataId) Allocator.Error![]static_data_exports.StaticDataExport {
    const entry = program.static_data_values.items[@backingInt(slot)];
    const size_align = program.layouts.layoutSizeAlign(program.layouts.getLayout(entry.layout_idx));
    const name = try LirProgram.staticDataSymbolName(allocator, slot);
    errdefer allocator.free(name);
    const bytes = try allocator.alloc(u8, size_align.size);
    errdefer allocator.free(bytes);
    @memset(bytes, 0);
    const exports = try allocator.alloc(static_data_exports.StaticDataExport, 1);
    exports[0] = .{ .symbol_name = name, .value_id = slot, .bytes = bytes, .alignment = @intCast(size_align.alignment.toByteUnits()), .is_exported = false };
    return exports;
}

fn appendFrozenGraph(allocator: Allocator, destination: *std.ArrayList(static_data_exports.StaticDataExport), graph: []static_data_exports.StaticDataExport) Allocator.Error!void {
    try destination.ensureUnusedCapacity(allocator, graph.len);
    const start = destination.items.len;
    for (graph) |*item| for (@constCast(item.relocations)) |*relocation| {
        if (relocation.target == .data_symbol) relocation.target.data_symbol = @fromBackingInt(@intCast(start + @backingInt(relocation.target.data_symbol)));
    };
    destination.appendSliceAssumeCapacity(graph);
}

fn cloneRootRequests(allocator: Allocator, roots: lir.CheckedPipeline.RootRequestSet) Allocator.Error!lir.CheckedPipeline.RootRequestSet {
    var owned = lir.CheckedPipeline.RootRequestSet{
        .include_provided_data_exports = roots.include_provided_data_exports,
        .include_internal_static_data = roots.include_internal_static_data,
    };
    errdefer deinitRootRequests(allocator, owned);
    inline for (@typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".field_names, @typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".field_types) |field_name, field_type| {
        if (@typeInfo(field_type) == .pointer) {
            @field(owned, field_name) = try allocator.dupe(@typeInfo(field_type).pointer.child, @field(roots, field_name));
        }
    }
    return owned;
}

fn deinitRootRequests(allocator: Allocator, roots: lir.CheckedPipeline.RootRequestSet) void {
    inline for (@typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".field_names, @typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".field_types) |field_name, field_type| {
        if (@typeInfo(field_type) == .pointer) allocator.free(@field(roots, field_name));
    }
}

/// The Solved policy compile-time evaluation specializes under when no
/// runtime program shares its specialization, as in `roc check`: a dev
/// build's, so the code it compiles and caches is what a dev build of the
/// same program compiles.
const check_solved_policy = lir.CheckedPipeline.SolvedPolicy.fromTarget(.{
    .inline_mode = lir.CheckedPipeline.dev_inline_mode,
    .spec_constr_clone_inlining = lir.CheckedPipeline.dev_spec_constr_clone_inlining,
});

/// The target compile-time evaluation lowers under. Its Solved policy is
/// that of the program being built, so evaluation runs inside that program's
/// one specialization; inlining and SpecConstr preserve meaning, so every
/// command evaluates the same roots to the same values and reports. Every
/// other setting is fixed: evaluation runs on the host, runs expects, and
/// hoists every literal conversion.
fn compileTimeTarget(options: Options, solved_policy: lir.CheckedPipeline.SolvedPolicy) lir.CheckedPipeline.TargetConfig {
    var target: lir.CheckedPipeline.TargetConfig = .{
        .target_usize = base.target.TargetUsize.native,
        .specialization_strategy = .lss,
        .checked_module_state = .checking_finalization,
        .comptime_value_reads = true,
        .literal_roots = true,
        .inline_expects = .run,
        // Every LIR setting is a dev build's, so compile-time code is the
        // code a dev build compiles: the rewrites that only speed up the
        // produced program stay off, and tag-case fusion, which bounds list
        // copying, stays on.
        .consume_dead_boxes = true,
        .zero_dict_seed = true,
        // Native dev code always supports a hot-reload session, so its erased
        // callables reserve the hot-reload prefix; an interpreted evaluation
        // reserves none.
        .erased_capture_prefix = if (compilerHostMustUseInterpreterForCtfe()) 0 else builtins.erased_callable.hot_reload_capture_prefix_size,
        .scalarize_joins = false,
        .reuse_boxes = false,
        // Specialization records procedure names for every consumer of the
        // program; a runtime consumer's diagnostics read them.
        .proc_debug_names = true,
        .spec_cache = if (options.object_cache) |cache| cache.spec_cache else null,
        .code_provision = if (!compilerHostMustUseInterpreterForCtfe() and backend.host_lir_codegen_available and options.object_cache != null)
            .host_dev_objects
        else
            .source_bodies,
        .post_check_executor = options.post_check_executor,
        .timing = if (options.timing) |timing| &timing.lowering else null,
    };
    solved_policy.applyTo(&target);
    return target;
}

/// Name every reader before a shared Monotype producer can erase bodies.
/// Boxy runtime lowering has its own producer and does not read this one.
fn sharedProducerProvision(
    host: lir.CheckedPipeline.TargetConfig,
    runtime: ?lir.CheckedPipeline.TargetConfig,
) lir.CheckedPipeline.CodeProvision {
    const consumer = runtime orelse return host.code_provision;
    if (consumer.specialization_strategy != .lss) return host.code_provision;
    const host_provider = host.spec_cache orelse return .source_bodies;
    const runtime_provider = consumer.spec_cache orelse return .source_bodies;
    if (!host_provider.sameProvider(runtime_provider)) return .source_bodies;
    return host.code_provision.sharedDomain(consumer.code_provision);
}

test "shared producer requires the exact non-null provider capability" {
    const Lookup = lir.CheckedPipeline.SpecCacheLookup;
    const LookupResult = @typeInfo(@typeInfo(@FieldType(Lookup, "find")).pointer.child).@"fn".return_type.?;
    const Callbacks = struct {
        fn first(_: *anyopaque, _: [32]u8, _: bool) LookupResult {
            return null;
        }
        fn second(context: *anyopaque, _: [32]u8, _: bool) LookupResult {
            // Distinct behavior keeps optimized builds from merging callbacks.
            const count: *u8 = @ptrCast(context);
            count.* += 1;
            return null;
        }
    };
    var contexts = [_]u8{ 0, 0 };
    const provider: Lookup = .{ .context = &contexts[0], .find = Callbacks.first };
    const Provision = lir.CheckedPipeline.CodeProvision;
    for (std.enums.values(Provision)) |host_domain| {
        const host: lir.CheckedPipeline.TargetConfig = .{ .code_provision = host_domain, .spec_cache = provider };
        for (std.enums.values(Provision)) |runtime_domain| {
            for ([_]?Lookup{
                provider,
                .{ .context = &contexts[1], .find = Callbacks.first },
                .{ .context = &contexts[0], .find = Callbacks.second },
                null,
            }, 0..) |lookup, index| {
                const runtime: lir.CheckedPipeline.TargetConfig = .{
                    .specialization_strategy = .lss,
                    .code_provision = runtime_domain,
                    .spec_cache = lookup,
                };
                try std.testing.expectEqual(
                    if (index == 0) host_domain.sharedDomain(runtime_domain) else Provision.source_bodies,
                    sharedProducerProvision(host, runtime),
                );
                var boxy = runtime;
                boxy.specialization_strategy = .boxy;
                try std.testing.expectEqual(host_domain, sharedProducerProvision(host, boxy));
                var missing_host = host;
                missing_host.spec_cache = null;
                try std.testing.expectEqual(Provision.source_bodies, sharedProducerProvision(missing_host, runtime));
            }
        }
        try std.testing.expectEqual(host_domain, sharedProducerProvision(host, null));
    }
}

test "declared consumers choose shared producer provision before discovery" {
    const Provision = lir.CheckedPipeline.CodeProvision;
    for (std.enums.values(Provision)) |host| {
        try std.testing.expectEqual(host, sharedProducerProvision(.{ .code_provision = host }, null));
        for (std.enums.values(Provision)) |runtime| {
            try std.testing.expectEqual(Provision.source_bodies, sharedProducerProvision(.{ .code_provision = host }, .{
                .specialization_strategy = .lss,
                .code_provision = runtime,
            }));
            try std.testing.expectEqual(host, sharedProducerProvision(.{ .code_provision = host }, .{
                .specialization_strategy = .boxy,
                .code_provision = runtime,
            }));
        }
    }
}

/// Complete checked values in the caller's dependency order, using the
/// released frontend workers.
///
/// Compile-time evaluation specializes the modules' compile-time roots
/// together with `program_roots`, the runtime roots the checked program
/// itself declares, and evaluates the compile-time roots and every literal
/// root that specialization registers, under `compileTimeTarget`. Neither the
/// roots nor the policy depend on the command, so every command that checks
/// a program performs the same evaluation. A runtime consumer
/// (`runtime_roots`, which must be among `program_roots`) continues the
/// specialized program afterwards and reads the completed values.
pub fn finalizeProgram(
    allocator: Allocator,
    modules: []const ProgramModule,
    lowering_modules: lir.CheckedPipeline.CheckedModuleSet,
    program_roots: lir.CheckedPipeline.RootRequestSet,
    runtime_roots: lir.CheckedPipeline.RootRequestSet,
    runtime_target: ?lir.CheckedPipeline.TargetConfig,
    options: Options,
) FinalizeError!ProgramSession {
    const total_started_ns = if (options.timing) |timing| timing.start() else 0;
    defer if (options.timing) |timing| timing.finish(total_started_ns, .total);
    var debug_events = DebugEvents{ .allocator = allocator };
    defer debug_events.deinit();
    var requests = std.ArrayList(checked.RootRequest).empty;
    defer requests.deinit(allocator);
    var source_modules = std.ArrayList(checked.ModuleId).empty;
    defer source_modules.deinit(allocator);
    for (modules) |entry| {
        for (entry.module.root_requests.compile_time_requests) |request| {
            // A reopened artifact's roots that an earlier compilation
            // evaluated are read from its store like any other module's.
            if (entry.module.compile_time_roots.root(compileTimeRootForRequest(entry.module, request)).payload != .pending) continue;
            try requests.append(allocator, request);
            try source_modules.append(allocator, entry.module.key);
        }
    }
    const requested_root_count = requests.items.len;
    if (program_roots.source_modules.len != 0 and program_roots.source_modules.len != program_roots.requests.len)
        finalizationInvariant("program roots omitted their checked owners");

    const lss_runtime = if (runtime_target) |target| target.specialization_strategy == .lss else false;
    const demand_driven = demandDrivenEvaluation(modules, program_roots, lss_runtime);
    // Without an LSS runtime consumer to continue this specialization, the
    // program roots are discovery roots: compile-time evaluation specializes
    // a runtime body only where it can register a literal root, and lowers
    // none of them for execution (design.md "Demand-Driven Compile-Time
    // Specialization").
    var literal_demand: ?lir.CheckedPipeline.LiteralDemand.LiteralDemand = null;
    defer if (literal_demand) |*literal| literal.deinit();
    if (lss_runtime) {
        try requests.appendSlice(allocator, program_roots.requests);
        if (program_roots.source_modules.len == 0) {
            try source_modules.appendNTimes(allocator, lowering_modules.root.module.key, program_roots.requests.len);
        } else {
            try source_modules.appendSlice(allocator, program_roots.source_modules);
        }
    } else if (program_roots.requests.len != 0) {
        var declared = std.AutoHashMapUnmanaged(lir.CheckedPipeline.LiteralDemand.DeclaredRoot, void).empty;
        defer declared.deinit(allocator);
        for (modules) |entry| {
            for (entry.module.root_requests.compile_time_requests) |request| {
                try declared.put(allocator, .{ .module = entry.module.key, .root = compileTimeRootForRequest(entry.module, request) }, {});
            }
        }
        const checked_modules: lir.CheckedPipeline.LiteralDemand.CheckedModules = .{ .root = lowering_modules.root, .imports = lowering_modules.imports };
        literal_demand = try lir.CheckedPipeline.LiteralDemand.compute(allocator, checked_modules, &declared);
        var selected = std.ArrayList(usize).empty;
        defer selected.deinit(allocator);
        try lir.CheckedPipeline.LiteralDemand.selectDiscoveryRoots(allocator, &literal_demand.?, checked_modules, program_roots.requests, program_roots.source_modules, &selected);
        for (selected.items) |index| {
            try requests.append(allocator, program_roots.requests[index]);
            try source_modules.append(allocator, if (program_roots.source_modules.len == 0) lowering_modules.root.module.key else program_roots.source_modules[index]);
        }
        if (options.timing) |timing| {
            const counters = literal_demand.?.counters;
            timing.lowering.addDemand(.{
                .program_roots = program_roots.requests.len,
                .discovery_roots = selected.items.len,
                .modules = counters.modules,
                .templates = counters.templates,
                .source_templates = counters.source_templates,
                .reaching_templates = counters.reaching_templates,
                .custom_literal_types = counters.custom_literal_types,
            });
        }
    }
    const owned_imports = try allocator.dupe(checked.ImportedModuleView, lowering_modules.imports);
    errdefer allocator.free(owned_imports);
    const owned_relations = try allocator.dupe(checked.ImportedModuleView, lowering_modules.root.relation_modules);
    errdefer allocator.free(owned_relations);
    const owned_runtime_roots = try cloneRootRequests(allocator, runtime_roots);
    errdefer deinitRootRequests(allocator, owned_runtime_roots);

    var completed: ?CompletedStages = null;
    errdefer if (completed) |*stages| stages.deinit();
    var runtime_prepared: ?lir.CheckedPipeline.PreparedSolved = null;
    errdefer if (runtime_prepared) |*prepared| prepared.deinit();
    // The compile-time roots the specialized program publishes, ahead of its
    // program roots: every requested one, or, for a demand-driven program,
    // the ones it demands.
    var compile_time_root_count: usize = 0;
    var demand: ?lir.CheckedPipeline.ComptimeDemand = null;
    defer if (demand) |*owned| owned.deinit();
    var published = PublishedRequests{};
    defer published.deinit(allocator);
    var stored_demand = StoredRootSet{};
    defer stored_demand.deinit(allocator);

    if (requests.items.len != 0) {
        var union_roots = program_roots;
        union_roots.requests = requests.items;
        union_roots.source_modules = source_modules.items;
        // Test roots are addressed by their position in the request list, so
        // a program that runs tests is never demand-driven.
        const union_test_metadata = try allocator.dupe(@typeInfo(@TypeOf(program_roots.test_plan_metadata)).pointer.child, if (lss_runtime) program_roots.test_plan_metadata else &.{});
        defer allocator.free(union_test_metadata);
        for (union_test_metadata) |*metadata| metadata.request_index += @intCast(requested_root_count);
        union_roots.test_plan_metadata = union_test_metadata;
        if (!lss_runtime) {
            // Discovery roots materialize nothing for a runtime consumer.
            union_roots.include_provided_data_exports = false;
            union_roots.include_internal_static_data = false;
            union_roots.layout_requests = &.{};
            union_roots.static_data_requests = &.{};
            if (literal_demand) |*literal| {
                union_roots.discovery_start = requested_root_count;
                union_roots.literal_demand = literal;
            }
        }
        // An LSS runtime consumer continues this specialization, so it is
        // made under the runtime's Solved policy.
        const solved_policy = if (lss_runtime) lir.CheckedPipeline.SolvedPolicy.fromTarget(runtime_target.?) else check_solved_policy;
        var host_target = compileTimeTarget(options, solved_policy);
        host_target.demand_driven_comptime_roots = demand_driven;
        const host_lir_policy = lir.CheckedPipeline.LirPolicy.fromTarget(host_target);
        // All readers of this producer are declared before any cache hit can
        // erase a source body. CTFE still owns its native late-splice policy.
        host_target.code_provision = sharedProducerProvision(host_target, runtime_target);
        // Counting work observes the evaluation without shaping it. A program
        // whose specializations go into the object cache needs what each one
        // reads of compile-time evaluation: the runtime consumer's, when it
        // asks for them, and this program's own, which the compile-time pack
        // publishes whenever there is an object cache.
        if (runtime_target) |target| {
            host_target.work_metrics = target.work_metrics;
            host_target.spec_comptime_reads = target.spec_comptime_reads;
        }
        if (options.object_cache != null) host_target.spec_comptime_reads = true;
        var monotype = lir.CheckedPipeline.prepareCheckedModulesMonotype(allocator, lowering_modules, union_roots, host_target) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.HostedFunctionNotBound => finalizationInvariant("compile-time evaluation required every hosted procedure to be bound"),
        };
        var monotype_owned = true;
        errdefer if (monotype_owned) monotype.deinit();
        demand = monotype.takeComptimeDemand();
        compile_time_root_count = demand.?.root_reads.len;
        if (!demand_driven and compile_time_root_count != requested_root_count)
            finalizationInvariant("compile-time evaluation published a different count of compile-time roots than it requested");
        if (options.timing) |timing| timing.recordRoots(demand.?);
        try published.init(allocator, modules, monotype.program.rootsView()[0..compile_time_root_count], monotype.program.view().lowering_modules);
        if (demand_driven) try stored_demand.fill(allocator, demand.?.stored);
        // An LSS runtime consumer continues this Solved program.
        const shares_solved = lss_runtime;
        monotype_owned = false;
        var prepared = try lir.CheckedPipeline.prepareMonotypeToSolved(monotype);
        var prepared_owned = true;
        errdefer if (prepared_owned) prepared.deinit();

        if (compile_time_root_count != 0 or prepared.literalRootCount() != 0) {
            // The compile-time roots are published first, so compile-time
            // evaluation's own share of the root plan is the positions before
            // the program roots; its literal roots join it.
            var evaluation_options = options;
            evaluation_options.debug_events = &debug_events;
            completed = try evaluateStaged(allocator, modules, &published, lowering_modules, &prepared, .{
                .target_usize = host_target.target_usize,
                .erased_capture_prefix = host_target.erased_capture_prefix,
                .inline_expects = host_target.inline_expects,
                .observers = lir.CheckedPipeline.Observers.fromTarget(host_target),
                .lir_policy = host_lir_policy,
            }, compile_time_root_count, evaluation_options);
            // Every literal root the program registered is evaluated now;
            // its outcome is settled unless evaluation printed a message.
            const fresh_message = for (debug_events.events.items) |event| {
                if (event.fresh) break true;
            } else false;
            if (options.object_cache != null and !fresh_message) {
                completed.?.discovery_summaries = try lir.CheckedPipeline.DiscoverySummary.collect(allocator, &prepared.program.lifted);
            }
            if (!shares_solved) {
                // The specializations' compile-time reads outlive the
                // Solved program: the object cache writes them beside the
                // program's procedures.
                completed.?.spec_comptime_reads = prepared.spec_comptime_reads;
                prepared.spec_comptime_reads = null;
                prepared_owned = false;
                prepared.deinit();
            }
        } else {
            for (modules) |entry| {
                if (entry.problem_store) |store| try finishPendingExhaustiveness(entry.module, store);
                try entry.module.const_store.verifyComplete();
            }
            if (!shares_solved) {
                prepared_owned = false;
                prepared.deinit();
            }
        }
        if (shares_solved) {
            prepared_owned = false;
            runtime_prepared = prepared;
        }
        try recordEvaluatedRootReads(allocator, modules, &published, demand.?);
    } else {
        for (modules) |entry| {
            if (entry.problem_store) |store| try finishPendingExhaustiveness(entry.module, store);
            try entry.module.const_store.verifyComplete();
        }
    }
    sealUndemandedRoots(modules);
    const runtime_positions = try runtimeRootPositions(allocator, program_roots, runtime_roots, compile_time_root_count, lss_runtime);
    var positions_owned = true;
    errdefer if (positions_owned) allocator.free(runtime_positions);
    // Observations replay in sorted order once every root has evaluated:
    // those stored for the roots this program demands, and this
    // compilation's own.
    const replay_filter: ?*const StoredRootSet = if (demand_driven) &stored_demand else null;
    try debug_events.appendCached(options.cached_debug_modules, replay_filter);
    for (modules) |entry| try debug_events.appendStored(entry.module, replay_filter);
    try debug_events.persist(modules);
    if (!options.defer_debug_replay) try debug_events.replay(options);
    var retained_root = lowering_modules.root;
    retained_root.relation_modules = owned_relations;
    positions_owned = false;
    return .{
        .allocator = allocator,
        .modules = .{ .root = retained_root, .imports = owned_imports },
        .runtime_requests = owned_runtime_roots.requests,
        .runtime_roots = owned_runtime_roots,
        .runtime_target = runtime_target,
        .completed = completed,
        .runtime_prepared = runtime_prepared,
        .runtime_positions = runtime_positions,
    };
}

/// Whether this compilation evaluates only the compile-time roots its program
/// demands (design.md "Demand-Driven Compile-Time Evaluation"): it builds a
/// program whose runtime consumer continues the evaluation's specialization,
/// runs no tests, and leaves no static exhaustiveness diagnostic to a
/// compile-time evaluation, whose outcome only evaluating every root decides.
/// Every other compilation evaluates every root it requests.
fn demandDrivenEvaluation(
    modules: []const ProgramModule,
    program_roots: lir.CheckedPipeline.RootRequestSet,
    lss_runtime: bool,
) bool {
    if (!lss_runtime or program_roots.requests.len == 0 or program_roots.test_plan_metadata.len != 0) return false;
    for (modules) |entry| {
        const store = entry.problem_store orelse continue;
        for (store.pending_static_exhaustiveness.items) |pending| {
            if (pending.mode != .static or pending.reported) continue;
            const site = pending.site orelse continue;
            switch (entry.module.exhaustiveness_sites.get(site).policy) {
                .compile_time_only, .compile_time_replaced_by_root => return false,
                .runtime_reachable, .not_pending => {},
            }
        }
    }
    return true;
}

/// The compile-time requests each finalized module's roots publish, in
/// request order: all of them, or the ones a demand-driven program demands.
const PublishedRequests = struct {
    by_module: [][]checked.RootRequest = &.{},

    fn init(
        self: *PublishedRequests,
        allocator: Allocator,
        modules: []const ProgramModule,
        roots: []const lir.CheckedPipeline.MonotypeRoot,
        lowering_modules: []const checked.ModuleId,
    ) Allocator.Error!void {
        const Key = struct { module: [32]u8, root: checked.ComptimeRootId };
        var demanded = std.AutoHashMapUnmanaged(Key, void).empty;
        defer demanded.deinit(allocator);
        for (roots) |root| {
            const id = root.request.compile_time_root orelse finalizationInvariant("published compile-time root lacked its checked root identity");
            try demanded.put(allocator, .{ .module = lowering_modules[@backingInt(root.owner)].bytes, .root = id }, {});
        }
        self.by_module = try allocator.alloc([]checked.RootRequest, modules.len);
        @memset(self.by_module, &.{});
        for (modules, self.by_module) |entry, *slot| {
            var list = std.ArrayList(checked.RootRequest).empty;
            errdefer list.deinit(allocator);
            for (entry.module.root_requests.compile_time_requests) |request| {
                if (demanded.contains(.{ .module = entry.module.key.bytes, .root = compileTimeRootForRequest(entry.module, request) }))
                    try list.append(allocator, request);
            }
            slot.* = try list.toOwnedSlice(allocator);
        }
    }

    fn deinit(self: *PublishedRequests, allocator: Allocator) void {
        for (self.by_module) |requests| allocator.free(requests);
        allocator.free(self.by_module);
        self.* = .{};
    }
};

/// Stored roots a demand-driven program demands, by module identity bytes.
const StoredRootSet = struct {
    const Key = struct { module: [32]u8, root: checked.ComptimeRootId };
    keys: std.AutoHashMapUnmanaged(Key, void) = .empty,

    fn fill(self: *StoredRootSet, allocator: Allocator, roots: []const lir.CheckedPipeline.ComptimeDemandRoot) Allocator.Error!void {
        for (roots) |root| try self.keys.put(allocator, .{ .module = root.module.bytes, .root = root.root }, {});
    }

    fn contains(self: *const StoredRootSet, key: Key) bool {
        return self.keys.contains(key);
    }

    fn deinit(self: *StoredRootSet, allocator: Allocator) void {
        self.keys.deinit(allocator);
    }
};

/// Record, for every root this compilation evaluated, the checked roots its
/// body reads, beside the reads earlier compilations recorded for roots they
/// evaluated.
fn recordEvaluatedRootReads(
    allocator: Allocator,
    modules: []const ProgramModule,
    published: *const PublishedRequests,
    demand: lir.CheckedPipeline.ComptimeDemand,
) Allocator.Error!void {
    var refs = std.ArrayList(checked.CompileTimeRootReadTable.Ref).empty;
    defer refs.deinit(allocator);
    var inputs = std.ArrayList(checked.CompileTimeRootReadTable.Input).empty;
    defer inputs.deinit(allocator);
    var ranges = std.ArrayList(struct { start: usize, len: usize }).empty;
    defer ranges.deinit(allocator);
    var position: usize = 0;
    for (modules, published.by_module) |entry, requests| {
        refs.clearRetainingCapacity();
        inputs.clearRetainingCapacity();
        ranges.clearRetainingCapacity();
        for (requests) |_| {
            const start = refs.items.len;
            for (demand.rootReads(position)) |read| try refs.append(allocator, .{ .module = read.module.bytes, .root = read.root });
            try ranges.append(allocator, .{ .start = start, .len = refs.items.len - start });
            position += 1;
        }
        for (requests, ranges.items) |request, range| {
            try inputs.append(allocator, .{ .root = compileTimeRootForRequest(entry.module, request), .reads = refs.items[range.start..][0..range.len] });
        }
        if (inputs.items.len == 0) continue;
        const merged = try checked.CompileTimeRootReadTable.merge(
            entry.module.evaluationAllocator(),
            entry.module.compile_time_root_reads,
            entry.module.compile_time_roots.roots.len,
            inputs.items,
        );
        if (entry.module.freesReplacedEvaluationColumns()) entry.module.compile_time_root_reads.deinit(entry.module.evaluationAllocator());
        entry.module.compile_time_root_reads = merged;
    }
    if (position != demand.root_reads.len) finalizationInvariant("published compile-time roots did not match the finalized modules' demanded requests");
}

/// A requested root nothing demanded stays unevaluated: the explicit state a
/// later compilation that demands it evaluates it from.
fn sealUndemandedRoots(modules: []const ProgramModule) void {
    for (modules) |entry| {
        for (entry.module.root_requests.compile_time_requests) |request| {
            const root = compileTimeRootForRequest(entry.module, request);
            if (entry.module.compile_time_roots.root(root).payload == .pending) {
                entry.module.compile_time_roots.fillPayload(root, .unevaluated);
            }
        }
    }
}

/// Boxy alone pays for this second representation of literal results. Normal
/// finalization already owns all language observations, so there are no report
/// destinations or debug sinks here. Operational failures still propagate.
fn materializeBoxyLiterals(
    allocator: Allocator,
    modules: lir.CheckedPipeline.CheckedModuleSet,
    roots: lir.CheckedPipeline.RootRequestSet,
    target: lir.CheckedPipeline.TargetConfig,
) FinalizeError!lir.CheckedPipeline.LoweredProgram {
    // The host program runs in the interpreter, whose erased callables
    // reserve no capture prefix.
    var host_target = target;
    host_target.target_usize = .native;
    host_target.erased_capture_prefix = 0;
    host_target.checked_module_state = .checking_finalization;
    var prepared = lir.CheckedPipeline.prepareBoxyCheckedModules(allocator, modules, roots, host_target) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.HostedFunctionNotBound => finalizationInvariant("Boxy literal planning encountered an unbound hosted procedure"),
    };
    defer prepared.deinit();
    if (!prepared.hasLiteralRoots()) return prepared.lower(target) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.HostedFunctionNotBound => finalizationInvariant("prepared Boxy lowering changed hosted procedure bindings"),
    };
    var host = prepared.lower(host_target) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.HostedFunctionNotBound => finalizationInvariant("Boxy literal lowering encountered an unbound hosted procedure"),
    };
    errdefer host.deinit();
    if (host.lir_result.literal_roots.items.len != 0)
        try evaluateBoxyLiteralRoots(allocator, modules, &host);

    if (target.target_usize != host_target.target_usize or target.erased_capture_prefix != host_target.erased_capture_prefix) {
        var runtime_target = target;
        runtime_target.checked_module_state = .checking_finalization;
        var runtime = prepared.lower(runtime_target) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.HostedFunctionNotBound => finalizationInvariant("Boxy runtime lowering encountered an unbound hosted procedure"),
        };
        errdefer runtime.deinit();
        if (host.frozen_static_data != null) {
            var sources = try ProgramSlots.init(allocator, &host);
            defer sources.deinit(allocator);
            runtime.frozen_static_data = try transcodeCompletedSlots(allocator, &sources, &runtime.lir_result);
        }
        try retainBoxyRuntimeRoots(allocator, &runtime);
        host.deinit();
        return runtime;
    }
    try retainBoxyRuntimeRoots(allocator, &host);
    return host;
}

fn retainBoxyRuntimeRoots(allocator: Allocator, program: *lir.CheckedPipeline.LoweredProgram) Allocator.Error!void {
    if (program.lir_result.literal_roots.items.len == 0) return;
    const positions = try allocator.alloc(u32, program.lir_result.root_procs.items.len);
    defer allocator.free(positions);
    for (positions, 0..) |*position, index| position.* = @intCast(index);
    try lir.CheckedPipeline.retainRuntimeRoots(program, positions);
}

/// Where each runtime request sits in the specialized program's root plan.
/// A runtime consumer continues the program compile-time evaluation
/// specialized, so everything it lowers is among the program roots that
/// specialization started from.
fn runtimeRootPositions(
    allocator: Allocator,
    program_roots: lir.CheckedPipeline.RootRequestSet,
    runtime_roots: lir.CheckedPipeline.RootRequestSet,
    compile_time_root_count: usize,
    lss_runtime: bool,
) Allocator.Error![]u32 {
    if (!lss_runtime) return try allocator.alloc(u32, 0);
    if (runtime_roots.include_provided_data_exports and !program_roots.include_provided_data_exports)
        finalizationInvariant("runtime consumer materializes provided data exports the program roots omit");
    if (runtime_roots.include_internal_static_data and !program_roots.include_internal_static_data)
        finalizationInvariant("runtime consumer materializes internal static data the program roots omit");
    if (!std.meta.eql(runtime_roots.layout_requests, program_roots.layout_requests) and runtime_roots.layout_requests.len != 0)
        finalizationInvariant("runtime consumer requested layouts the program roots omit");
    if (!std.meta.eql(runtime_roots.static_data_requests, program_roots.static_data_requests) and runtime_roots.static_data_requests.len != 0)
        finalizationInvariant("runtime consumer requested static data the program roots omit");
    const positions = try allocator.alloc(u32, runtime_roots.requests.len);
    for (runtime_roots.requests, positions) |request, *position| {
        for (program_roots.requests, 0..) |program_request, index| {
            if (!std.meta.eql(program_request, request)) continue;
            position.* = @intCast(compile_time_root_count + index);
            break;
        } else finalizationInvariant("runtime consumer named a root compile-time evaluation did not specialize");
    }
    return positions;
}

/// Evaluate a Boxy program's literal roots in dependency order, and freeze
/// their completed values into it. A Boxy program has no Solved program to
/// order its producers before lowering, so the order comes from its own LIR.
fn evaluateBoxyLiteralRoots(
    allocator: Allocator,
    lowering_modules: lir.CheckedPipeline.CheckedModuleSet,
    host: *lir.CheckedPipeline.LoweredProgram,
) FinalizeError!void {
    const options: Options = .{ .stderr = .{} };
    const literals = try allocator.alloc(ProducerLiteral, host.lir_result.literal_roots.items.len);
    defer allocator.free(literals);
    for (host.lir_result.literal_roots.items, literals, 0..) |plan, *literal, index| {
        if (@backingInt(plan.id) != index) finalizationInvariant("lowered program published a literal root away from its id");
        literal.* = .{ .module = plan.module, .site = plan.site };
    }
    const producers: Producers = .{ .roots = &.{}, .literals = literals };
    const ProgramPtr = if (comptime compilerHostMustUseInterpreterForCtfe()) *InterpreterProgram else *DevProgram;
    var evaluation: Evaluation(ProgramPtr) = undefined;
    try evaluation.init(allocator, &.{}, &PublishedRequests{}, host.lir_result.lowering_modules.items, producers, options);
    defer evaluation.deinit();
    evaluation.owners.report_sites = false;
    var program_storage = try initEvaluationProgram(ProgramPtr, allocator, lowering_modules, host, .boxy_in_process, options);
    defer program_storage.deinit();
    const program = program_storage.ptr();
    try evaluation.enterStage(host, program, &.{});
    var graph = try evaluation.lirGraph();
    defer graph.deinit(allocator);
    var groups = try ProducerGroups.build(allocator, &evaluation, graph.deps);
    defer groups.deinit(allocator);
    for (0..groups.level_count) |level| try evaluation.evaluateLevel(&groups, level);
    try evaluation.finish();
    host.frozen_static_data = try program.freezeCompleted();
}

/// The program one stage evaluates in, native or interpreted.
fn EvaluationProgram(comptime ProgramPtr: type) type {
    return struct {
        storage: if (ProgramPtr == *DevProgram) DevProgram else *InterpreterProgram,

        fn ptr(self: *@This()) ProgramPtr {
            return if (ProgramPtr == *DevProgram) &self.storage else self.storage;
        }

        fn deinit(self: *@This()) void {
            if (ProgramPtr == *DevProgram) self.storage.deinit() else self.storage.deinit();
        }
    };
}

fn initEvaluationProgram(
    comptime ProgramPtr: type,
    allocator: Allocator,
    lowering_modules: lir.CheckedPipeline.CheckedModuleSet,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    code: DevProgram.Code,
    options: Options,
) FinalizeError!EvaluationProgram(ProgramPtr) {
    if (ProgramPtr == *InterpreterProgram) return .{ .storage = try InterpreterProgram.init(allocator, lowering_modules, lowered, options) };
    if (comptime !backend.host_lir_codegen_available) return error.UnsupportedPlatform;
    var native = try DevProgram.init(allocator, lowering_modules, lowered, code, options);
    native.codegen.static_strings = native.static_strings.view();
    return .{ .storage = native };
}

/// What a staged evaluation lowers each stage under: the consumer settings
/// every stage shares.
const StageConsumer = struct {
    target_usize: base.target.TargetUsize,
    erased_capture_prefix: u32,
    inline_expects: lir.CheckedPipeline.InlineExpectMode,
    observers: lir.CheckedPipeline.Observers,
    lir_policy: lir.CheckedPipeline.LirPolicy,
};

/// Evaluate the compile-time producers of a Solved program in dependency
/// order, lowering each level's code only once every value it reads exists
/// (design.md "Dependency-Ordered Compile-Time Evaluation"). Each level is
/// lowered as a consumer of the Solved program that reads the completed
/// values of the levels before it the way a build reads them: folded where
/// they fold, read from their slots otherwise, and guarded only where they
/// failed.
fn evaluateStaged(
    allocator: Allocator,
    modules: []const ProgramModule,
    published: *const PublishedRequests,
    lowering_modules: lir.CheckedPipeline.CheckedModuleSet,
    prepared: *lir.CheckedPipeline.PreparedSolved,
    consumer: StageConsumer,
    compile_time_root_count: usize,
    options: Options,
) FinalizeError!CompletedStages {
    const lifted = &prepared.program.lifted;
    const plan_roots = lifted.rootsView()[0..compile_time_root_count];
    const roots = try allocator.alloc(ProducerRoot, plan_roots.len);
    defer allocator.free(roots);
    for (plan_roots, roots) |root, *producer| producer.* = .{ .owner = root.owner, .request = root.request };
    const literal_roots = lifted.literalRootsView();
    const literals = try allocator.alloc(ProducerLiteral, literal_roots.len);
    defer allocator.free(literals);
    for (literal_roots, literals) |root, *literal| literal.* = .{ .module = root.module, .site = root.site };
    const producers: Producers = .{ .roots = roots, .literals = literals };

    var graph = try prepared.producerGraph(allocator, compile_time_root_count);
    defer graph.deinit(allocator);

    const ProgramPtr = if (comptime compilerHostMustUseInterpreterForCtfe()) *InterpreterProgram else *DevProgram;
    var evaluation: Evaluation(ProgramPtr) = undefined;
    try evaluation.init(allocator, modules, published, lifted.loweringModules(), producers, options);
    defer evaluation.deinit();
    var groups = try ProducerGroups.build(allocator, &evaluation, graph.deps);
    defer groups.deinit(allocator);

    // The roots whose completed values the program reads, so each one's
    // stage declares the slot its value is frozen into.
    const ReadKey = struct { module: [32]u8, root: checked.ComptimeRootId };
    var read_roots = std.AutoHashMap(ReadKey, @FieldType(lir.CheckedPipeline.CompletedValueRequest, "value")).init(allocator);
    defer read_roots.deinit();
    for (prepared.comptimeValueReads()) |read| switch (read.root) {
        .checked => |id| try read_roots.put(.{ .module = read.module.bytes, .root = id }, read),
        .literal => finalizationInvariant("producer recorded a read of a literal root as checked module demand"),
    };

    var completed = CompletedStages.init(allocator);
    errdefer completed.deinit();
    var stage_roots = std.ArrayList(u32).empty;
    defer stage_roots.deinit(allocator);
    var stage_literals = std.ArrayList(lir.LIR.LiteralRootId).empty;
    defer stage_literals.deinit(allocator);
    var completed_values = std.ArrayList(lir.CheckedPipeline.CompletedValueRequest).empty;
    defer completed_values.deinit(allocator);
    var stage_producers = std.ArrayList(usize).empty;
    defer stage_producers.deinit(allocator);
    // Where each failed producer failed. A later stage reading a failed
    // value reports the failure there, so every stage's program starts with
    // the origins the stages before it published.
    var failure_origins = std.AutoHashMapUnmanaged(StaticSlotEnvironment.ProducerOriginKey, lir.LIR.ComptimeFailureOrigin).empty;
    defer failure_origins.deinit(allocator);
    for (0..groups.level_count) |level| {
        stage_roots.clearRetainingCapacity();
        stage_literals.clearRetainingCapacity();
        completed_values.clearRetainingCapacity();
        stage_producers.clearRetainingCapacity();
        for (groups.groups.items) |group| {
            if (group.level != level) continue;
            try stage_producers.appendSlice(allocator, groups.membersOf(group));
        }
        std.sort.pdq(usize, stage_producers.items, {}, std.sort.asc(usize));
        for (stage_producers.items) |ordinal| {
            if (ordinal < roots.len) {
                try stage_roots.append(allocator, @intCast(ordinal));
                const root = roots[ordinal];
                const module = lifted.loweringModules()[@backingInt(root.owner)];
                if (read_roots.get(.{ .module = module.bytes, .root = root.request.compile_time_root.? })) |read| {
                    try completed_values.append(allocator, .{ .root = @intCast(ordinal), .value = read });
                }
            } else {
                try stage_literals.append(allocator, @fromBackingInt(@intCast(ordinal - roots.len)));
            }
        }
        const stage = try completed.lowerStage(prepared, .{
            .roots = .{
                .roots = stage_roots.items,
                .completed_values = completed_values.items,
                .layout_requests = false,
                .runtime_schema_requests = false,
                .literal_roots = .{ .only = stage_literals.items },
            },
            .target_usize = consumer.target_usize,
            .erased_capture_prefix = consumer.erased_capture_prefix,
            .inline_expects = consumer.inline_expects,
            .observers = consumer.observers,
            .lir_policy = consumer.lir_policy,
        }, stage_producers.items, producers, lifted.loweringModules());
        if (stage.lir_result.root_procs.items.len != stage_roots.items.len)
            finalizationInvariant("compile-time stage lowering changed its requested root count");
        var program_storage = try initEvaluationProgram(ProgramPtr, allocator, lowering_modules, stage, .dev_object, options);
        defer program_storage.deinit();
        const program = program_storage.ptr();
        const slots = if (ProgramPtr == *DevProgram) &program.slots else program.slotEnvironment();
        var inherited = failure_origins.iterator();
        while (inherited.next()) |entry| try slots.producer_origins.put(slots.allocator, entry.key_ptr.*, entry.value_ptr.*);
        try evaluation.enterStage(stage, program, stage_roots.items);
        try evaluation.evaluateLevel(&groups, level);
        var stage_origins = slots.producer_origins.iterator();
        while (stage_origins.next()) |entry| try failure_origins.put(allocator, entry.key_ptr.*, entry.value_ptr.*);
        // The stage's image holds the values it read and the ones it
        // completed.
        const frozen = try program.freezeCompleted();
        if (stage.frozen_static_data) |*read| read.deinit();
        stage.frozen_static_data = frozen;
        if (ProgramPtr == *DevProgram and options.keep_artifacts) {
            var set = try program.extractArtifacts(stage);
            errdefer set.deinit();
            try completed.artifacts.append(allocator, set);
        }
        try completed.finishStage();
    }
    try evaluation.finish();
    return completed;
}

/// A compile-time root a finalization evaluates, at its position in the
/// root plan.
const ProducerRoot = struct {
    owner: lir.LIR.LoweringModuleId,
    request: checked.RootRequest,
};

/// A literal root a finalization evaluates, at its id.
const ProducerLiteral = struct {
    module: checked.ModuleId,
    site: lir.LIR.LiteralRejectionSite,
};

/// The producers one finalization evaluates: its compile-time roots in root
/// plan order as producers `0..roots.len`, then its literal roots in id
/// order.
const Producers = struct {
    roots: []const ProducerRoot,
    literals: []const ProducerLiteral,

    fn count(self: Producers) usize {
        return self.roots.len + self.literals.len;
    }
};

/// Evaluates compile-time producers in dependency order (design.md
/// "Dependency-Ordered Compile-Time Evaluation"): every producer runs once
/// each producer its code reads has finished, and the producers of one level
/// of the dependency graph run together. Each level runs in a program
/// `enterStage` names; what evaluation learns about the producers, their
/// completion and their failures, outlives every one of those programs.
fn Evaluation(comptime ProgramPtr: type) type {
    return struct {
        const Self = @This();
        const State = struct {
            completion: RootCompletionState,
            /// The first producer naming each of this module's compile-time
            /// roots, by root id.
            ordinals: []?usize,
        };
        const Root = struct { module: usize, request: usize };

        allocator: Allocator,
        modules: []const ProgramModule,
        published: *const PublishedRequests,
        options: Options,
        owners: ModuleOwners,
        producers: Producers,
        states: []State,
        /// Every compile-time root producer's module and request.
        roots: []Root,
        /// Which literal roots have completed.
        literal_done: []bool,
        literal_failures: LiteralRootFailures,
        /// The program each literal root ran in, which its failure is
        /// reported against once every producer has run.
        literal_programs: []?*lir.CheckedPipeline.LoweredProgram,

        /// The program the current level runs in.
        lowered: *lir.CheckedPipeline.LoweredProgram = undefined,
        program: ProgramPtr = undefined,
        /// The current program's plan of each producer it evaluates.
        root_plans: []?u32,
        literal_plans: []?u32,

        fn init(
            self: *Self,
            allocator: Allocator,
            modules: []const ProgramModule,
            published: *const PublishedRequests,
            lowering_modules: []const checked.ModuleId,
            producers: Producers,
            options: Options,
        ) FinalizeError!void {
            var owners = try ModuleOwners.init(allocator, lowering_modules, modules);
            errdefer owners.deinit(allocator);
            owners.unfinalized = options.unfinalized_reports;
            const states = try allocator.alloc(State, modules.len);
            var initialized: usize = 0;
            errdefer {
                for (states[0..initialized]) |*state| {
                    state.completion.deinit();
                    allocator.free(state.ordinals);
                }
                allocator.free(states);
            }
            for (modules, states, published.by_module) |entry, *state, requests| {
                var completion = try RootCompletionState.init(allocator, entry.module, requests);
                errdefer completion.deinit();
                const ordinals = try allocator.alloc(?usize, entry.module.compile_time_roots.roots.len);
                @memset(ordinals, null);
                state.* = .{ .completion = completion, .ordinals = ordinals };
                initialized += 1;
            }
            const roots = try allocator.alloc(Root, producers.roots.len);
            errdefer allocator.free(roots);
            // The root plan records each root's owner and checked root id, so
            // the order is read out of the plan itself. Nothing here infers a
            // module from a root's position in the concatenated requests.
            for (producers.roots, roots, 0..) |producer, *root, ordinal| {
                const module_index = owners.position(producer.owner) orelse
                    finalizationInvariant("compile-time root named a checked module this finalization does not complete");
                const root_id = producer.request.compile_time_root orelse
                    finalizationInvariant("compile-time root request reached finalization without its checked root identity");
                const state = &states[module_index];
                root.* = .{ .module = module_index, .request = state.completion.requestIndexForRoot(root_id) };
                const slot = &state.ordinals[@backingInt(root_id)];
                if (slot.*) |existing| {
                    if (owners.next_alias[module_index] == null) {
                        finalizationInvariant("root plan named one compile-time root twice");
                    }
                    // Shared identity permits repeated requests, never
                    // conflicting requests for the same root.
                    if (!std.meta.eql(producer.request, producers.roots[existing].request)) {
                        finalizationInvariant("root plan named conflicting requests for one compile-time root");
                    }
                } else {
                    var alias: ?u32 = module_index;
                    while (alias) |index| : (alias = owners.next_alias[index]) {
                        _ = states[index].completion.requestIndexForRoot(root_id);
                        states[index].ordinals[@backingInt(root_id)] = ordinal;
                    }
                }
            }
            for (states) |*state| {
                for (state.completion.request_root_ids) |id| {
                    if (state.ordinals[@backingInt(id)] == null) {
                        finalizationInvariant("root plan omitted a requested compile-time root");
                    }
                }
            }
            const literal_done = try allocator.alloc(bool, producers.literals.len);
            errdefer allocator.free(literal_done);
            @memset(literal_done, false);
            var literal_failures = try LiteralRootFailures.init(allocator, producers.literals.len);
            errdefer literal_failures.deinit(allocator);
            const literal_programs = try allocator.alloc(?*lir.CheckedPipeline.LoweredProgram, producers.literals.len);
            errdefer allocator.free(literal_programs);
            @memset(literal_programs, null);
            const root_plans = try allocator.alloc(?u32, producers.roots.len);
            errdefer allocator.free(root_plans);
            const literal_plans = try allocator.alloc(?u32, producers.literals.len);
            self.* = .{
                .allocator = allocator,
                .modules = modules,
                .published = published,
                .options = options,
                .owners = owners,
                .producers = producers,
                .states = states,
                .roots = roots,
                .literal_done = literal_done,
                .literal_failures = literal_failures,
                .literal_programs = literal_programs,
                .root_plans = root_plans,
                .literal_plans = literal_plans,
            };
            self.owners.literal_failures = &self.literal_failures;
        }

        fn deinit(self: *Self) void {
            for (self.states) |*state| {
                state.completion.deinit();
                self.allocator.free(state.ordinals);
            }
            self.allocator.free(self.states);
            self.allocator.free(self.roots);
            self.allocator.free(self.literal_done);
            self.literal_failures.deinit(self.allocator);
            self.allocator.free(self.literal_programs);
            self.allocator.free(self.root_plans);
            self.allocator.free(self.literal_plans);
            self.owners.deinit(self.allocator);
        }

        /// Run the next levels in `lowered`, whose compile-time root plans
        /// are those of `root_ordinals`, in order, and whose literal root
        /// plans name their producers by id.
        fn enterStage(self: *Self, lowered: *lir.CheckedPipeline.LoweredProgram, program: ProgramPtr, root_ordinals: []const u32) FinalizeError!void {
            self.lowered = lowered;
            self.program = program;
            @memset(self.root_plans, null);
            @memset(self.literal_plans, null);
            const const_roots = lowered.lir_result.const_roots.items;
            if (const_roots.len != root_ordinals.len) finalizationInvariant("lowered program published a different count of compile-time roots than it was asked for");
            for (const_roots, root_ordinals, 0..) |plan, ordinal, index| {
                const producer = self.producers.roots[ordinal];
                if (plan.owner != producer.owner or !std.meta.eql(plan.request, producer.request))
                    finalizationInvariant("lowered program changed a compile-time root's plan");
                self.root_plans[ordinal] = @intCast(index);
            }
            for (lowered.lir_result.literal_roots.items, 0..) |plan, index| {
                const raw = @backingInt(plan.id);
                if (raw >= self.literal_plans.len or self.literal_plans[raw] != null)
                    finalizationInvariant("lowered program published a literal root outside its producers");
                self.literal_plans[raw] = @intCast(index);
                self.literal_programs[raw] = lowered;
            }
            // Every compile-time value slot is some producer's.
            for (lowered.lir_result.static_data_values.items) |slot| {
                const root = slot.compile_time_root orelse continue;
                _ = self.producerOfSlot(root.module, root.root);
            }
        }

        /// The producer of every value of `module`'s `producer`.
        fn producerOfSlot(self: *const Self, module: checked.ModuleId, producer: lir.LIR.ComptimeProducer) usize {
            switch (producer) {
                .literal => |id| {
                    if (@backingInt(id) >= self.producers.literals.len) finalizationInvariant("compile-time slot names a literal root no producer evaluates");
                    return self.producers.roots.len + @backingInt(id);
                },
                .checked => |id| {
                    for (self.modules, self.states) |entry, state| {
                        if (!entry.module.key.eql(module)) continue;
                        return state.ordinals[@backingInt(id)] orelse continue;
                    }
                    finalizationInvariant("compile-time slot has no declared producer");
                },
            }
        }

        /// The producers that read each producer, from the current
        /// program's LIR (`evaluateBoxyLiteralRoots`).
        fn lirGraph(self: *const Self) Allocator.Error!RootGraph.Graph {
            const allocator = self.allocator;
            const result = &self.lowered.lir_result;
            const producer_procs = try allocator.alloc(lir.LIR.LirProcSpecId, self.producers.count());
            defer allocator.free(producer_procs);
            for (self.root_plans, producer_procs[0..self.roots.len]) |plan, *proc| proc.* = result.const_roots.items[plan.?].proc;
            for (self.literal_plans, producer_procs[self.roots.len..]) |plan, *proc| proc.* = result.literal_roots.items[plan.?].proc;
            const slot_roots = try allocator.alloc(?usize, result.static_data_values.items.len);
            defer allocator.free(slot_roots);
            for (result.static_data_values.items, slot_roots) |slot, *owner| {
                const root = slot.compile_time_root orelse {
                    owner.* = null;
                    continue;
                };
                owner.* = self.producerOfSlot(root.module, root.root);
            }
            return RootGraph.build(allocator, &result.store, producer_procs, slot_roots, result.boxy_worker_procs.items);
        }

        fn rootId(self: *const Self, ordinal: usize) checked.ComptimeRootId {
            const root = self.roots[ordinal];
            return self.states[root.module].completion.request_root_ids[root.request];
        }

        /// The ordinal evaluation names a producer by: the first one for its
        /// checked identity.
        fn canonical(self: *const Self, ordinal: usize) usize {
            if (ordinal >= self.roots.len) return ordinal;
            return self.states[self.roots[ordinal].module].ordinals[@backingInt(self.rootId(ordinal))].?;
        }

        fn rootPlan(self: *const Self, ordinal: usize) LirProgram.ConstRootPlan {
            const index = self.root_plans[ordinal] orelse finalizationInvariant("a compile-time root ran in a program that does not lower it");
            return self.lowered.lir_result.const_roots.items[index];
        }

        fn literalPlan(self: *const Self, index: usize) LirProgram.LiteralRootPlan {
            const plan = self.literal_plans[index] orelse finalizationInvariant("a literal root ran in a program that does not lower it");
            return self.lowered.lir_result.literal_roots.items[plan];
        }

        /// Run every group of `level`: those that read no member of their
        /// own together, then each that does.
        fn evaluateLevel(self: *Self, groups: *const ProducerGroups, level: usize) FinalizeError!void {
            var batch = std.ArrayList(usize).empty;
            defer batch.deinit(self.allocator);
            for (groups.groups.items) |group| {
                if (group.level != level or group.cyclic) continue;
                try batch.appendSlice(self.allocator, groups.membersOf(group));
            }
            try self.evaluateBatch(batch.items);
            for (groups.groups.items) |group| {
                if (group.level != level or !group.cyclic) continue;
                try self.evaluateCycle(groups.membersOf(group));
            }
        }

        /// Evaluate independent producers together: each module's checked
        /// roots in one run, then each literal root.
        fn evaluateBatch(self: *Self, producers: []const usize) FinalizeError!void {
            const allocator = self.allocator;
            var requests = std.ArrayList(checked.RootRequest).empty;
            defer requests.deinit(allocator);
            var root_ids = std.ArrayList(checked.ComptimeRootId).empty;
            defer root_ids.deinit(allocator);
            var plans = std.ArrayList(LirProgram.ConstRootPlan).empty;
            defer plans.deinit(allocator);
            for (0..self.modules.len) |module_index| {
                requests.clearRetainingCapacity();
                root_ids.clearRetainingCapacity();
                plans.clearRetainingCapacity();
                for (producers) |ordinal| {
                    if (ordinal >= self.roots.len) continue;
                    if (self.roots[ordinal].module != module_index) continue;
                    try requests.append(allocator, self.published.by_module[module_index][self.roots[ordinal].request]);
                    try root_ids.append(allocator, self.rootId(ordinal));
                    try plans.append(allocator, self.rootPlan(ordinal));
                }
                if (requests.items.len == 0) continue;
                try self.evaluateModuleRoots(module_index, requests.items, root_ids.items, plans.items);
            }
            for (producers) |ordinal| {
                if (ordinal < self.roots.len) continue;
                const index = ordinal - self.roots.len;
                if (try evalLiteralRoot(allocator, &self.owners, self.options, self.lowered, self.literalPlan(index), self.program)) self.literal_done[index] = true;
            }
        }

        /// Run one module's roots, then repeat each root that completed for
        /// every other artifact sharing its checked identity.
        fn evaluateModuleRoots(
            self: *Self,
            module_index: usize,
            requests: []const checked.RootRequest,
            root_ids: []const checked.ComptimeRootId,
            plans: []const LirProgram.ConstRootPlan,
        ) FinalizeError!void {
            const entry = self.modules[module_index];
            const state = &self.states[module_index];
            {
                const previous_report_sites = self.owners.report_sites;
                defer self.owners.report_sites = previous_report_sites;
                _ = try evalProgramRoots(self.allocator, entry.module, requests, root_ids, &state.completion, entry.problem_store, &self.owners, self.options, self.lowered, plans, self.program);
            }
            var alias: ?u32 = self.owners.next_alias[module_index];
            while (alias) |index| : (alias = self.owners.next_alias[index]) {
                const alias_entry = self.modules[index];
                const alias_state = &self.states[index];
                const alias_requests = self.published.by_module[index];
                // Each artifact owns its ConstStore and diagnostics, but debug
                // observations persist and replay once per checked identity.
                var alias_events = DebugEvents{ .allocator = self.allocator };
                defer alias_events.deinit();
                var evaluation_options = self.options;
                evaluation_options.debug_events = &alias_events;
                evaluation_options.publish_shared_slots = false;
                evaluation_options.retry = null;
                const previous_report_sites = self.owners.report_sites;
                self.owners.report_sites = false;
                defer self.owners.report_sites = previous_report_sites;
                for (root_ids, plans) |id, plan| {
                    // A root its own module left for its group to run again
                    // is repeated once it completes.
                    if (!state.completion.isDone(id)) continue;
                    const request_index = alias_state.completion.requestIndexForRoot(id);
                    _ = try evalProgramRoots(self.allocator, alias_entry.module, alias_requests[request_index..][0..1], alias_state.completion.request_root_ids[request_index..][0..1], &alias_state.completion, alias_entry.problem_store, &self.owners, evaluation_options, self.lowered, &.{plan}, self.program);
                }
            }
        }

        /// Evaluate a group of roots that read each other. Only hoisted
        /// roots can form one, since checking rejects every cycle among
        /// named values. Reading a member while the group evaluates is a
        /// read of a value that needs itself: evaluating the hoisted
        /// expression in place would recurse forever, so every member
        /// starts out failed with the stack overflow that recursion ends
        /// in. Each pass runs the members not yet completed; a member that
        /// failed only by reading one of those is run again once another
        /// completes, and when a pass completes none, the rest fail as
        /// they read.
        fn evaluateCycle(self: *Self, members: []const usize) FinalizeError!void {
            const allocator = self.allocator;
            if (@import("builtin").mode == .debug) {
                for (members) |ordinal| {
                    if (ordinal >= self.roots.len) continue;
                    const entry = self.modules[self.roots[ordinal].module];
                    if (entry.module.compile_time_roots.root(self.rootId(ordinal)).kind != .hoisted_constant)
                        finalizationInvariant("a cycle among compile-time roots included a root checking did not hoist");
                }
            }
            var pending = std.ArrayList(usize).empty;
            defer pending.deinit(allocator);
            try pending.appendSlice(allocator, members);
            var keys = std.ArrayList(StaticSlotEnvironment.ProducerOriginKey).empty;
            defer keys.deinit(allocator);
            for (members) |ordinal| try self.publishPendingFailure(ordinal);
            while (pending.items.len != 0) {
                keys.clearRetainingCapacity();
                for (pending.items) |ordinal| try keys.append(allocator, self.producerKey(ordinal));
                const retry = CycleRetry{ .pending = keys.items };
                var options = self.options;
                options.retry = &retry;
                const saved = self.options;
                self.options = options;
                defer self.options = saved;
                try self.evaluateBatch(pending.items);
                var kept: usize = 0;
                for (pending.items) |ordinal| {
                    if (self.done(ordinal)) continue;
                    pending.items[kept] = ordinal;
                    kept += 1;
                }
                if (kept == pending.items.len) {
                    self.options = saved;
                    try self.evaluateBatch(pending.items);
                    return;
                }
                pending.shrinkRetainingCapacity(kept);
            }
        }

        fn done(self: *const Self, ordinal: usize) bool {
            if (ordinal >= self.roots.len) return self.literal_done[ordinal - self.roots.len];
            return self.states[self.roots[ordinal].module].completion.isDone(self.rootId(ordinal));
        }

        fn producerKey(self: *const Self, ordinal: usize) StaticSlotEnvironment.ProducerOriginKey {
            if (ordinal < self.roots.len) {
                const owner = self.producers.roots[ordinal].owner;
                return .{ .module = self.owners.lowering_modules[@backingInt(owner)].bytes, .producer = .{ .checked = self.rootId(ordinal) } };
            }
            const index = ordinal - self.roots.len;
            return .{ .module = self.producers.literals[index].module.bytes, .producer = .{ .literal = @fromBackingInt(@intCast(index)) } };
        }

        fn publishPendingFailure(self: *Self, ordinal: usize) FinalizeError!void {
            const owner, const producer: lir.LIR.ComptimeProducer = if (ordinal < self.roots.len)
                .{ self.producers.roots[ordinal].owner, .{ .checked = self.rootId(ordinal) } }
            else blk: {
                const index = ordinal - self.roots.len;
                break :blk .{ self.producers.literals[index].site.owner, .{ .literal = @fromBackingInt(@intCast(index)) } };
            };
            const failure: SlotFailure = .{ .message = @import("crash_boundary.zig").STACK_OVERFLOW_MESSAGE, .kind = .crash };
            if (ProgramPtr == *DevProgram) {
                try self.program.publishFailure(self.lowered, owner, producer, failure);
            } else {
                try self.program.slotEnvironment().publishFailure(self.lowered, owner, producer, failure, .{ .resolve = InterpreterProgram.resolveFunction });
                self.program.refreshCallableMetadata();
            }
        }

        /// Report what only every producer having run decides: each literal
        /// root failure no checked root embeds, and the exhaustiveness
        /// problems evaluation left pending.
        fn finish(self: *Self) FinalizeError!void {
            for (self.literal_failures.records, self.literal_failures.embedded, self.literal_programs, 0..) |maybe_record, embedded, program, index| {
                const entry = maybe_record orelse continue;
                if (embedded) continue;
                switch (entry) {
                    .own => |own| {
                        const lowered = program orelse finalizationInvariant("a literal root failed without running");
                        try reportLiteralRootFailure(&self.owners, lowered, lowered.lir_result.literal_roots.items[self.literalPlanIn(lowered, index)], own.borrow(self.literal_failures.messages.items));
                    },
                    .literal, .checked => {},
                }
            }
            for (self.modules) |entry| {
                if (entry.problem_store) |store| try finishPendingExhaustiveness(entry.module, store);
                try entry.module.const_store.verifyComplete();
            }
        }

        fn literalPlanIn(_: *const Self, lowered: *const lir.CheckedPipeline.LoweredProgram, index: usize) usize {
            for (lowered.lir_result.literal_roots.items, 0..) |plan, position| {
                if (@backingInt(plan.id) == index) return position;
            }
            finalizationInvariant("a literal root's program does not lower it");
        }
    };
}

/// Producers grouped into strongly connected groups of the dependency graph,
/// with each group's level.
const ProducerGroups = struct {
    const Group = struct {
        start: u32,
        len: u32,
        level: u32,
        /// The group reads itself: several members, or one reading itself.
        cyclic: bool,
    };

    groups: std.ArrayList(Group) = .empty,
    member_list: std.ArrayList(usize) = .empty,
    level_count: usize = 0,
    fn membersOf(self: *const ProducerGroups, group: Group) []const usize {
        return self.member_list.items[group.start..][0..group.len];
    }

    fn deinit(self: *ProducerGroups, allocator: Allocator) void {
        self.groups.deinit(allocator);
        self.member_list.deinit(allocator);
    }

    /// Tarjan's algorithm over canonical producers; groups come out after
    /// every group they read.
    fn build(allocator: Allocator, scheduler: anytype, deps_of: []const []const u32) Allocator.Error!ProducerGroups {
        const count = deps_of.len;
        var result = ProducerGroups{};
        errdefer result.deinit(allocator);
        const unvisited = std.math.maxInt(u32);
        const index = try allocator.alloc(u32, count);
        defer allocator.free(index);
        @memset(index, unvisited);
        const low = try allocator.alloc(u32, count);
        defer allocator.free(low);
        const group_of = try allocator.alloc(u32, count);
        defer allocator.free(group_of);
        @memset(group_of, unvisited);
        const on_stack = try allocator.alloc(bool, count);
        defer allocator.free(on_stack);
        @memset(on_stack, false);
        var stack = std.ArrayList(usize).empty;
        defer stack.deinit(allocator);
        const Frame = struct { node: usize, next: usize };
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(allocator);
        var next_index: u32 = 0;
        for (0..count) |start| {
            if (scheduler.canonical(start) != start) continue;
            if (index[start] != unvisited) continue;
            index[start] = next_index;
            low[start] = next_index;
            next_index += 1;
            try stack.append(allocator, start);
            on_stack[start] = true;
            try frames.append(allocator, .{ .node = start, .next = 0 });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                const deps = deps_of[node];
                if (frame.next < deps.len) {
                    const dep = scheduler.canonical(deps[frame.next]);
                    frame.next += 1;
                    if (index[dep] == unvisited) {
                        index[dep] = next_index;
                        low[dep] = next_index;
                        next_index += 1;
                        try stack.append(allocator, dep);
                        on_stack[dep] = true;
                        try frames.append(allocator, .{ .node = dep, .next = 0 });
                    } else if (on_stack[dep]) {
                        low[node] = @min(low[node], index[dep]);
                    }
                    continue;
                }
                _ = frames.pop();
                if (frames.items.len != 0) {
                    const parent = frames.items[frames.items.len - 1].node;
                    low[parent] = @min(low[parent], low[node]);
                }
                if (low[node] != index[node]) continue;
                const group_id: u32 = @intCast(result.groups.items.len);
                const member_start: u32 = @intCast(result.member_list.items.len);
                var level: u32 = 0;
                var reads_itself = false;
                while (true) {
                    const member = stack.pop().?;
                    on_stack[member] = false;
                    group_of[member] = group_id;
                    try result.member_list.append(allocator, member);
                    if (member == node) break;
                }
                const members = result.member_list.items[member_start..];
                // Members in ascending order keep each batch in request order.
                std.sort.pdq(usize, members, {}, std.sort.asc(usize));
                for (members) |member| {
                    for (deps_of[member]) |raw_dep| {
                        const dep = scheduler.canonical(raw_dep);
                        if (group_of[dep] == group_id) {
                            reads_itself = true;
                        } else {
                            level = @max(level, result.groups.items[group_of[dep]].level + 1);
                        }
                    }
                }
                try result.groups.append(allocator, .{
                    .start = member_start,
                    .len = @intCast(members.len),
                    .level = level,
                    .cyclic = reads_itself,
                });
                result.level_count = @max(result.level_count, level + 1);
            }
        }
        return result;
    }
};

/// Thread-safe timing totals accumulated across compile-time root batches.
pub const Timing = struct {
    std_io: std.Io,
    lowering: lir.CheckedPipeline.Timing,
    total_ns: TimingCounter = .{},
    static_data_ns: TimingCounter = .{},
    code_generation_ns: TimingCounter = .{},
    execution_ns: TimingCounter = .{},
    store_results_ns: TimingCounter = .{},
    /// Compile-time roots the shared program declared, and how many of them
    /// it published for evaluation.
    roots_declared: TimingCounter = .{},
    roots_evaluated: TimingCounter = .{},
    /// Process footprint range observed at burst boundaries. Compile-time
    /// evaluation runs as bursts interleaved with checking, so the progress
    /// reporter cannot window-sample it; the brackets that already time each
    /// burst fold a footprint reading at the same points.
    mem_min: MemMinCounter = MemMinCounter.init(std.math.maxInt(u64)),
    mem_max: MemMaxCounter = .{},
    native_emission_mutex: std.atomic.Mutex = .unlocked,
    native_emission: NativeProcCompiler.Metrics = .{},

    pub fn init(std_io: std.Io) Timing {
        return .{
            .std_io = std_io,
            .lowering = lir.CheckedPipeline.Timing.init(std_io),
        };
    }

    pub fn snapshot(self: *const Timing) TimingSnapshot {
        const mutable = @constCast(self);
        while (!mutable.native_emission_mutex.tryLock()) std.atomic.spinLoopHint();
        defer mutable.native_emission_mutex.unlock();
        return .{
            .total_ns = self.total_ns.load(),
            .lowering = self.lowering.snapshot(),
            .static_data_ns = self.static_data_ns.load(),
            .code_generation_ns = self.code_generation_ns.load(),
            .execution_ns = self.execution_ns.load(),
            .store_results_ns = self.store_results_ns.load(),
            .roots_declared = self.roots_declared.load(),
            .roots_evaluated = self.roots_evaluated.load(),
            .mem_min = self.mem_min.load(),
            .mem_max = self.mem_max.load(),
            .native_emission = self.native_emission,
        };
    }

    fn addNativeEmission(self: *Timing, metrics: NativeProcCompiler.Metrics) void {
        while (!self.native_emission_mutex.tryLock()) std.atomic.spinLoopHint();
        defer self.native_emission_mutex.unlock();
        self.native_emission.add(metrics);
    }

    pub fn addSnapshot(self: *Timing, snapshot_value: TimingSnapshot) void {
        self.addNativeEmission(snapshot_value.native_emission);
        self.lowering.addSnapshot(snapshot_value.lowering);
        self.total_ns.add(snapshot_value.total_ns);
        self.static_data_ns.add(snapshot_value.static_data_ns);
        self.code_generation_ns.add(snapshot_value.code_generation_ns);
        self.execution_ns.add(snapshot_value.execution_ns);
        self.store_results_ns.add(snapshot_value.store_results_ns);
        self.roots_declared.add(snapshot_value.roots_declared);
        self.roots_evaluated.add(snapshot_value.roots_evaluated);
        if (snapshot_value.mem_min != std.math.maxInt(u64)) self.mem_min.min(snapshot_value.mem_min);
        self.mem_max.max(snapshot_value.mem_max);
    }

    fn start(self: *Timing) i64 {
        self.sampleMemory();
        return nowNs(self.std_io);
    }

    fn recordRoots(self: *Timing, demand: lir.CheckedPipeline.ComptimeDemand) void {
        self.roots_declared.add(demand.declared_root_count);
        self.roots_evaluated.add(demand.root_reads.len);
    }

    fn sampleMemory(self: *Timing) void {
        const bytes = base.process_memory.currentBytes() orelse return;
        self.mem_min.min(bytes);
        self.mem_max.max(bytes);
    }

    fn finishExecution(self: *Timing, started_ns: i64) void {
        self.sampleMemory();
        const elapsed: u64 = @intCast(@max(0, nowNs(self.std_io) - started_ns));
        self.execution_ns.add(elapsed);
    }

    fn finish(self: *Timing, started_ns: i64, phase: TimingPhase) void {
        self.sampleMemory();
        const elapsed_ns: u64 = @intCast(@max(0, nowNs(self.std_io) - started_ns));
        switch (phase) {
            .total => self.total_ns.add(elapsed_ns),
            .static_data => self.static_data_ns.add(elapsed_ns),
            .code_generation => self.code_generation_ns.add(elapsed_ns),
            .execution => self.execution_ns.add(elapsed_ns),
            .store_results => self.store_results_ns.add(elapsed_ns),
        }
    }
};

const MemMinCounter = base.ConcurrentU64;
const MemMaxCounter = base.ConcurrentU64;
const TimingCounter = base.ConcurrentU64;

/// Immutable compile-time finalization timings for progress reporting.
pub const TimingSnapshot = struct {
    total_ns: u64 = 0,
    /// Preserve phase and worker identities when runtime reuses this program.
    lowering: lir.CheckedPipeline.TimingSnapshot = .{},
    static_data_ns: u64 = 0,
    code_generation_ns: u64 = 0,
    execution_ns: u64 = 0,
    store_results_ns: u64 = 0,
    roots_declared: u64 = 0,
    roots_evaluated: u64 = 0,
    mem_min: u64 = std.math.maxInt(u64),
    mem_max: u64 = 0,
    native_emission: NativeProcCompiler.Metrics = .{},
};

test "shared lowering timing preserves full snapshots through aggregation" {
    var lowering: lir.CheckedPipeline.TimingSnapshot = .{};
    inline for (@typeInfo(lir.CheckedPipeline.TimingSnapshot).@"struct".field_names, 0..) |field_name, i| {
        if (@FieldType(lir.CheckedPipeline.TimingSnapshot, field_name) == u64) @field(lowering, field_name) = i + 1;
    }
    lowering.monotype_parallel.specialization_tasks_submitted = 11;
    lowering.monotype_parallel.peak_worker_lanes_used = 4;
    lowering.solved_lir_parallel.tasks_committed = 13;
    lowering.lir_pass_parallel.tasks_committed = 17;
    lowering.arc_parallel.planning_tasks_committed = 19;
    lowering.arc_parallel.uniqueness.component_runs = 23;
    lowering.monotype_diagnostics.specialization.template_requests = 23;
    const input: TimingSnapshot = .{
        .total_ns = 101,
        .lowering = lowering,
        .static_data_ns = 103,
        .code_generation_ns = 107,
        .execution_ns = 109,
        .store_results_ns = 113,
        .mem_min = 127,
        .mem_max = 131,
        .native_emission = .{ .tasks_submitted = 3, .procedures_emitted = 5, .procedures_reused = 7, .peak_inflight_fragments = 2 },
    };
    var first = Timing.init(std.testing.io);
    first.addSnapshot(input);
    var second = Timing.init(std.testing.io);
    second.addSnapshot(first.snapshot());
    try std.testing.expectEqualDeep(input, second.snapshot());

    second.addSnapshot(input);
    var expected_lowering = lir.CheckedPipeline.Timing.init(std.testing.io);
    expected_lowering.addSnapshot(lowering);
    expected_lowering.addSnapshot(lowering);
    const result = second.snapshot();
    try std.testing.expectEqualDeep(expected_lowering.snapshot(), result.lowering);
    try std.testing.expectEqual(@as(u64, 202), result.total_ns);
    try std.testing.expectEqual(@as(u64, 127), result.mem_min);
    try std.testing.expectEqual(@as(u64, 131), result.mem_max);
    try std.testing.expectEqual(@as(u64, 6), result.native_emission.tasks_submitted);
    try std.testing.expectEqual(@as(u64, 10), result.native_emission.procedures_emitted);
    try std.testing.expectEqual(@as(u64, 14), result.native_emission.procedures_reused);
    try std.testing.expectEqual(@as(u64, 2), result.native_emission.peak_inflight_fragments);
    try std.testing.expectEqualDeep(TimingSnapshot{}, Timing.init(std.testing.io).snapshot());
}

const TimingPhase = enum {
    total,
    static_data,
    code_generation,
    execution,
    store_results,
};

/// Return the checking finalizer that evaluates compile-time roots.
pub fn finalizer() checked.CompileTimeFinalizer {
    return .{ .finalize = finalize };
}

/// Return a checking finalizer configured with caller-provided CTFE options.
pub fn finalizerWithOptions(options: *const Options) checked.CompileTimeFinalizer {
    return .{
        .context = @ptrCast(@constCast(options)),
        .finalize = finalize,
    };
}

fn finalize(
    context: ?*anyopaque,
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    imports: []const checked.PublishImportArtifact,
    available_modules: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    problem_store: ?*check.problem.Store,
) FinalizeError!void {
    const options: Options = if (context) |raw|
        @as(*const Options, @ptrCast(@alignCast(raw))).*
    else
        .{};
    const requests = module.root_requests.compile_time_requests;
    const total_started_ns = if (requests.len != 0) if (options.timing) |timing| timing.start() else 0 else 0;
    if (requests.len != 0) {
        const lowering_imports = try finalizationImports(allocator, checked.importedView(module), imports, available_modules);
        defer allocator.free(lowering_imports);

        var state = try RootCompletionState.init(allocator, module, requests);
        defer state.deinit();

        var batch_requests = std.ArrayList(checked.RootRequest).empty;
        defer batch_requests.deinit(allocator);

        var batch_root_ids = std.ArrayList(checked.ComptimeRootId).empty;
        defer batch_root_ids.deinit(allocator);

        for (requests, 0..) |request, request_index| {
            const root_id = state.rootIdForRequestIndex(request_index);
            if (!try state.dependenciesComplete(request)) {
                if (batch_requests.items.len == 0) {
                    finalizationInvariant("compile-time root request order placed a root before another root it depends on");
                }
                _ = try lowerEvalAndFinishRoots(
                    allocator,
                    module,
                    lowering_imports,
                    relation_modules,
                    batch_requests.items,
                    batch_root_ids.items,
                    &state,
                    problem_store,
                    options,
                );
                batch_requests.clearRetainingCapacity();
                batch_root_ids.clearRetainingCapacity();

                if (!try state.dependenciesComplete(request)) {
                    finalizationInvariant("compile-time root request order referenced a later or cyclic dependency");
                }
            }
            try batch_requests.append(allocator, request);
            try batch_root_ids.append(allocator, root_id);
        }

        if (batch_requests.items.len != 0) {
            _ = try lowerEvalAndFinishRoots(
                allocator,
                module,
                lowering_imports,
                relation_modules,
                batch_requests.items,
                batch_root_ids.items,
                &state,
                problem_store,
                options,
            );
        }
    }

    if (problem_store) |store| {
        try finishPendingExhaustiveness(module, store);
    }

    try module.const_store.verifyComplete();
    if (requests.len != 0) {
        if (options.timing) |timing| timing.finish(total_started_ns, .total);
    }
}

const RootStatus = enum {
    pending,
    done,
};

const RootCompletionState = struct {
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    statuses: []RootStatus,
    requested_roots: []bool,
    request_root_ids: []checked.ComptimeRootId,
    /// Position in `request_root_ids` per compile-time root id, or
    /// `no_request` when this module requests no evaluation of that root.
    request_index_by_root: []u32,
    visited_templates: []u32,
    visit: u32,
    /// Procedure templates whose dependencies are still to be checked.
    pending_templates: std.ArrayList(canonical.ProcedureTemplateRef) = .empty,
    current_root_id: ?checked.ComptimeRootId = null,

    fn init(
        allocator: Allocator,
        module: *checked.CheckedModuleArtifact,
        requests: []const checked.RootRequest,
    ) Allocator.Error!RootCompletionState {
        const statuses = try allocator.alloc(RootStatus, module.compile_time_roots.roots.len);
        errdefer allocator.free(statuses);
        @memset(statuses, .pending);

        const requested_roots = try allocator.alloc(bool, module.compile_time_roots.roots.len);
        errdefer allocator.free(requested_roots);
        @memset(requested_roots, false);

        const request_root_ids = try allocator.alloc(checked.ComptimeRootId, requests.len);
        errdefer allocator.free(request_root_ids);
        const request_index_by_root = try allocator.alloc(u32, module.compile_time_roots.roots.len);
        errdefer allocator.free(request_index_by_root);
        @memset(request_index_by_root, no_request);
        for (requests, 0..) |request, i| {
            const root_id = compileTimeRootForRequest(module, request);
            const raw = @backingInt(root_id);
            if (requested_roots[raw]) {
                finalizationInvariant("compile-time root was requested more than once");
            }
            requested_roots[raw] = true;
            request_root_ids[i] = root_id;
            request_index_by_root[raw] = @intCast(i);
        }

        const visited_templates = try allocator.alloc(u32, module.checked_procedure_templates.templates.items.len);
        errdefer allocator.free(visited_templates);
        @memset(visited_templates, 0);

        return .{
            .allocator = allocator,
            .module = module,
            .statuses = statuses,
            .requested_roots = requested_roots,
            .request_root_ids = request_root_ids,
            .request_index_by_root = request_index_by_root,
            .visited_templates = visited_templates,
            .visit = 0,
        };
    }

    /// No compile-time request of this module evaluates the root.
    const no_request: u32 = std.math.maxInt(u32);

    /// Position of a compile-time root in this module's request stream. The
    /// producer records the owner and root id on each lowered root plan, so
    /// finalization asks the owner for the position instead of assuming the
    /// plan appears at the same offset the request did.
    fn requestIndexForRoot(self: *const RootCompletionState, root_id: checked.ComptimeRootId) usize {
        const raw = @backingInt(root_id);
        if (raw >= self.request_index_by_root.len) {
            finalizationInvariant("compile-time root id is outside its declared owner's root table");
        }
        const index = self.request_index_by_root[raw];
        if (index == no_request) {
            finalizationInvariant("lowered compile-time root was not requested by its declared owner");
        }
        return index;
    }

    fn deinit(self: *RootCompletionState) void {
        const allocator = self.allocator;
        self.pending_templates.deinit(allocator);
        allocator.free(self.visited_templates);
        allocator.free(self.request_index_by_root);
        allocator.free(self.request_root_ids);
        allocator.free(self.requested_roots);
        allocator.free(self.statuses);
        self.* = undefined;
    }

    fn isDone(self: *const RootCompletionState, root_id: checked.ComptimeRootId) bool {
        return self.statuses[@backingInt(root_id)] == .done;
    }

    fn markDone(self: *RootCompletionState, root_id: checked.ComptimeRootId) void {
        self.statuses[@backingInt(root_id)] = .done;
    }

    fn rootIdForRequestIndex(self: *const RootCompletionState, request_index: usize) checked.ComptimeRootId {
        if (request_index >= self.request_root_ids.len) {
            finalizationInvariant("compile-time request index was out of range");
        }
        return self.request_root_ids[request_index];
    }

    fn dependenciesComplete(
        self: *RootCompletionState,
        request: checked.RootRequest,
    ) Allocator.Error!bool {
        const request_root_id = compileTimeRootForRequest(self.module, request);
        const saved_current_root_id = self.current_root_id;
        defer self.current_root_id = saved_current_root_id;
        self.current_root_id = request_root_id;

        self.visit +%= 1;
        if (self.visit == 0) {
            @memset(self.visited_templates, 0);
            self.visit = 1;
        }
        const template_ref = request.procedure_template orelse
            finalizationInvariant("compile-time root had no checked wrapper template");

        // Every template reachable through the checked references must be
        // complete, so templates are checked from a worklist in any order.
        self.pending_templates.clearRetainingCapacity();
        try self.pending_templates.append(self.allocator, template_ref);
        while (self.pending_templates.pop()) |next| {
            if (!try self.templateDependenciesComplete(next)) return false;
        }
        return true;
    }

    /// Whether the template's own references are complete, queueing the
    /// local templates they reach.
    fn templateDependenciesComplete(
        self: *RootCompletionState,
        template_ref: canonical.ProcedureTemplateRef,
    ) Allocator.Error!bool {
        if (!artifactMatches(template_ref.artifact, self.module.key)) return true;
        const index = @backingInt(template_ref.template);
        if (index >= self.visited_templates.len) {
            finalizationInvariant("compile-time dependency referenced an unknown local procedure template");
        }
        if (self.visited_templates[index] == self.visit) return true;
        self.visited_templates[index] = self.visit;

        const template = self.module.checked_procedure_templates.get(template_ref.template);
        const refs = template.resolved_value_refs;
        const start = refs.start;
        const end = refs.start + refs.len;
        if (end > self.module.resolved_value_refs.template_refs.len) {
            finalizationInvariant("compile-time dependency template-ref span was outside the checked table");
        }
        for (self.module.resolved_value_refs.template_refs[start..end]) |ref_id| {
            const raw = @backingInt(ref_id);
            if (raw >= self.module.resolved_value_refs.records.len) {
                finalizationInvariant("compile-time dependency ref id was outside the checked table");
            }
            switch (self.resolvedRefDependency(self.module.resolved_value_refs.records[raw].ref)) {
                .complete => |complete| if (!complete) return false,
                .template => |dependency| try self.pending_templates.append(self.allocator, dependency),
            }
        }
        return true;
    }

    /// What a reference depends on: an answer known at once, or a checked
    /// procedure template whose own references decide it.
    const Dependency = union(enum) {
        complete: bool,
        template: canonical.ProcedureTemplateRef,
    };

    fn resolvedRefDependency(
        self: *RootCompletionState,
        ref: checked.ResolvedValueRef,
    ) Dependency {
        return switch (ref) {
            .top_level_const => |const_use| .{ .complete = self.constUseComplete(const_use) },
            .selected_hoisted_const => |selected| .{ .complete = self.constUseComplete(selected.const_use) },
            .top_level_proc,
            .promoted_top_level_proc,
            => |proc_use| self.procedureUseDependency(proc_use),
            .platform_required_const => |required| .{ .complete = self.constUseComplete(required.const_use) },
            .platform_required_proc => |required| self.procedureUseDependency(required.procedure),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .local_proc,
            .imported_const,
            .imported_proc,
            .hosted_proc,
            .platform_required_declaration,
            .platform_required_checked_error,
            => .{ .complete = true },
        };
    }

    fn constUseComplete(
        self: *RootCompletionState,
        const_use: checked.ConstUseTemplate,
    ) bool {
        // A declaration with no implementation evaluates nothing, so it
        // publishes no compile-time root and there is nothing to wait on.
        if (self.constUseIsUnimplemented(const_use.const_ref)) return true;
        const root_id = self.rootForConstRef(const_use.const_ref) orelse return true;
        return self.rootDependencyComplete(root_id);
    }

    fn constUseIsUnimplemented(self: *RootCompletionState, const_ref: checked.ConstRef) bool {
        if (!artifactMatches(const_ref.artifact, self.module.key)) return false;
        return self.module.const_templates.get(const_ref).state == .unimplemented;
    }

    fn rootDependencyComplete(
        self: *const RootCompletionState,
        dependency_root_id: checked.ComptimeRootId,
    ) bool {
        const dependent_root_id = self.current_root_id orelse
            finalizationInvariant("compile-time dependency checked outside a root request");
        if (dependency_root_id == dependent_root_id) return true;

        const dependent = self.module.compile_time_roots.root(dependent_root_id);
        const dependency = self.module.compile_time_roots.root(dependency_root_id);
        const is_strict = switch (dependent.source) {
            .def => |dependent_def| switch (dependency.source) {
                .def => |dependency_def| self.module.moduleEnvConst().hasTopLevelDemandDependency(
                    dependent_def,
                    dependency_def,
                ),
                .expr, .statement, .required_binding, .hoisted => true,
            },
            .expr, .statement, .required_binding, .hoisted => true,
        };
        if (!is_strict) return true;

        return !self.requested_roots[@backingInt(dependency_root_id)] or self.isDone(dependency_root_id);
    }

    fn rootForConstRef(
        self: *RootCompletionState,
        const_ref: checked.ConstRef,
    ) ?checked.ComptimeRootId {
        if (!artifactMatches(const_ref.artifact, self.module.key)) return null;
        return switch (const_ref.owner) {
            .top_level_binding => |top_level| {
                return self.module.compile_time_roots.lookupIdByPattern(top_level.pattern) orelse
                    finalizationInvariant("local const dependency had no compile-time root");
            },
            .hoisted_expr => |hoisted| {
                if (hoisted.module_idx != self.module.module_identity.module_idx) {
                    finalizationInvariant("local hoisted const dependency had mismatched module index");
                }
                const entry = self.module.hoisted_constants.lookupByExpr(hoisted.expr) orelse
                    finalizationInvariant("local hoisted const dependency had no hoisted const entry");
                return entry.root;
            },
        };
    }

    fn procedureUseDependency(
        self: *RootCompletionState,
        proc_use: checked.ProcedureUseTemplate,
    ) Dependency {
        var binding = proc_use.binding;
        while (true) switch (binding) {
            .top_level => |top_level| {
                if (!artifactMatches(top_level.artifact, self.module.key)) return .{ .complete = true };
                return switch (self.module.top_level_procedure_bindings.get(top_level.binding).body) {
                    .direct_template => |direct| switch (direct.template) {
                        .checked => |checked_template| .{ .template = checked_template },
                        .lifted, .synthetic => finalizationInvariant("checked procedure dependency referenced a post-check template"),
                    },
                    .checked_error => .{ .complete = true },
                    .callable_eval_template => |template_id| .{
                        .complete = self.rootDependencyComplete(self.module.callable_eval_templates.get(template_id).root),
                    },
                };
            },
            .imported, .hosted => return .{ .complete = true },
            .platform_required => |required| {
                if (!artifactMatches(required.artifact, self.module.key)) return .{ .complete = true };
                const required_binding = self.module.platform_required_bindings.lookupByBindingId(@backingInt(required.procedure_binding)) orelse
                    finalizationInvariant("platform-required procedure dependency referenced a missing binding");
                switch (required_binding.value_use) {
                    .procedure_value => |procedure_use| binding = procedure_use.procedure.binding,
                    .const_value => |const_use| return .{ .complete = self.constUseComplete(const_use.const_use) },
                }
            },
        };
    }
};

fn lowerEvalAndFinishRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    lowering_imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    options: Options,
) FinalizeError!bool {
    if (requests.len != root_ids.len) {
        finalizationInvariant("compile-time finalization request/root-id batch length mismatch");
    }

    const use_interpreter = comptime compilerHostMustUseInterpreterForCtfe();
    if (comptime !use_interpreter and !backend.host_lir_codegen_available) return error.UnsupportedPlatform;

    const lowering_modules: lir.CheckedPipeline.CheckedModuleSet = .{
        .root = checked.loweringViewWithRelations(module, relation_modules),
        .imports = lowering_imports,
    };
    var lowered = try lowerFinalizationModulesToLir(
        allocator,
        lowering_modules,
        .{ .requests = requests },
        .{
            .target_usize = base.target.TargetUsize.native,
            .specialization_strategy = .lss,
            .checked_module_state = .checking_finalization,
            .post_check_executor = options.post_check_executor,
            .timing = if (options.timing) |timing| &timing.lowering else null,
        },
    );
    defer lowered.deinit();

    var owners = try ModuleOwners.init(allocator, lowered.lir_result.lowering_modules.items, &.{.{ .module = module, .problem_store = problem_store }});
    defer owners.deinit(allocator);
    const const_roots = lowered.lir_result.const_roots.items;
    if (comptime use_interpreter) {
        const program = try InterpreterProgram.init(allocator, lowering_modules, &lowered, options);
        defer program.deinit();
        return evalInterpreterProgramRoots(allocator, module, requests, root_ids, state, problem_store, &owners, options, &lowered, const_roots, program);
    }
    if (const_roots.len != requests.len) {
        finalizationInvariant("LIR lowering returned a different number of compile-time roots than requested");
    }
    var native = try DevProgram.init(allocator, lowering_modules, &lowered, .dev_object, options);
    defer native.deinit();
    native.codegen.static_strings = native.static_strings.view();
    return evalDevProgramRoots(allocator, module, requests, root_ids, state, problem_store, &owners, options, &lowered, const_roots, &native);
}

fn evalInterpreterProgramRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    owners: *const ModuleOwners,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    const_roots: []const LirProgram.ConstRootPlan,
    shared_program: *InterpreterProgram,
) FinalizeError!bool {
    const program = try shared_program.fork(lowered);
    defer program.deinit();
    const interpreter = &program.interpreter;
    const host = &program.host;
    program.timing_io = if (options.timing) |timing| timing.std_io else null;
    var writer = ConstStoreWriter.Writer.init(allocator, module, &lowered.lir_result);
    defer writer.deinit();
    writer.setErasedCallableResolver(.{ .context = program, .resolve = InterpreterProgram.resolveStoredCallable });

    const execution_started_ns = if (options.timing) |timing| timing.start() else 0;
    var had_problem = false;
    if (const_roots.len != requests.len) {
        finalizationInvariant("LIR lowering returned a different number of compile-time roots than requested");
    }
    for (const_roots, 0..) |root, i| {
        if (!std.meta.eql(root.request, requests[i])) {
            finalizationInvariant("LIR lowering changed compile-time root request order");
        }
        const root_id = root_ids[i];
        const compile_time_root = module.compile_time_roots.root(root_id);
        host.clearDebugMessages();
        var succeeded = false;
        var failed_message: ?[]const u8 = null;
        const payload: checked.CompileTimeRootPayload = blk: {
            if (root.request.kind == .compile_time_constant and problem_store == null) {
                const eval_result = interpreter.eval(.{
                    .proc_id = root.proc,
                    .ret_layout = root.ret_layout,
                }) catch |err| failure: {
                    break :failure switch (err) {
                        error.OutOfMemory => return error.OutOfMemory,
                        error.RuntimeError, error.DivisionByZero => {
                            const message = interpreter.getRuntimeErrorMessage() orelse "compile-time evaluation failed";
                            failed_message = message;
                            break :blk try unreportedFailedRootPayload(module, compile_time_root, message, .crash);
                        },
                        error.Crash => {
                            const message = interpreter.getCrashMessage();
                            const meaning = interpreterFailedAt(&lowered.lir_result, interpreter, program.slotEnvironment()).meaning;
                            if (options.retry) |retry| if (retry.holds(meaning)) continue;
                            failed_message = message;
                            break :blk try unreportedFailedRootPayload(module, compile_time_root, message, RootFailure.of(meaning));
                        },
                        error.UnsupportedHostedFunction => finalizationInvariant("compile-time constant reached an unsupported hosted function"),
                        error.InvalidHostedFunctionSignature => finalizationInvariant("compile-time constant reached an invalid hosted function signature"),
                        // expect_err statements only occur in top-level expect
                        // test roots, never in compile-time constant roots.
                        error.ExpectErr => unreachable,
                    };
                };
                defer interpreter.dropValue(eval_result.value, root.ret_layout);
                const stored = try storeRootValue(&writer, root, compile_time_root, eval_result.value);
                if (options.publish_shared_slots) try program.publishRoot(lowered, root.owner, .{ .checked = root_id }, root.shape(), eval_result.value);
                succeeded = true;
                break :blk stored;
            }

            const eval_result = try evalCompileTimeRoot(interpreter, problem_store, owners, root.owner, module, compile_time_root, &lowered.lir_result, program.slotEnvironment(), root.proc, root.ret_layout, options.retry);
            switch (eval_result) {
                .retry => continue,
                .value => |value| {
                    defer interpreter.dropValue(value.value, root.ret_layout);
                    const stored = try storeRootValue(&writer, root, compile_time_root, value.value);
                    if (options.publish_shared_slots) try program.publishRoot(lowered, root.owner, .{ .checked = root_id }, root.shape(), value.value);
                    succeeded = true;
                    break :blk stored;
                },
                .failed => |failed| {
                    failed_message = failed.message;
                    break :blk failed.payload;
                },
            }
        };

        if (!succeeded and options.publish_shared_slots) {
            const message = failed_message orelse finalizationInvariant("failed interpreter root omitted its explicit failure message");
            const failed_at = interpreterFailedAt(&lowered.lir_result, interpreter, program.slotEnvironment());
            try program.slotEnvironment().publishFailureOrigin(root.owner, .{ .checked = root_id }, .{ .loc = failed_at.loc, .region = failed_at.region }, &lowered.lir_result);
            try program.slotEnvironment().publishFailure(lowered, root.owner, .{ .checked = root_id }, .{ .message = message, .kind = RootFailure.ofPayload(module, payload) }, .{ .resolve = InterpreterProgram.resolveFunction });
            program.refreshCallableMetadata();
        }

        if (try reportCompileTimeExpectFailures(
            problem_store,
            module,
            compile_time_root,
            &lowered.lir_result.store,
            interpreter.getExpectFailures(),
        )) had_problem = true;
        try reportInterpreterDebugMessages(
            allocator,
            options,
            module.key,
            compile_time_root,
            host.debugMessages(),
        );

        try completeRoot(module, &writer, state, root, root_id, compile_time_root, payload);
    }
    if (options.timing) |timing| timing.finishExecution(execution_started_ns);

    return had_problem;
}

/// The failed literal roots of one lowered program, by id.
///
/// A literal root belongs to no checked module, so nothing retains its
/// failure between compilations except the checked results that embed it. A
/// checked root that fails by reading a failed literal root reports that
/// failure in its own module, which keeps that module from being cached
/// clean; a literal root no checked root embeds reports in its literal's
/// module once finalization is done.
const LiteralRootFailures = struct {
    records: []?Record,
    embedded: []bool,
    messages: std.ArrayList(u8) = .empty,

    const Record = union(enum) {
        own: OwnedFailure,
        literal: lir.LIR.LiteralRootId,
        checked,
    };

    /// Offsets survive message-buffer growth; no evaluator-owned slice escapes.
    const OwnedFailure = struct {
        kind: ?lir.LIR.LiteralRejectionKind,
        message_start: usize,
        message_len: usize,
        meaning: ?lir.LIR.FailureMeaning,
        region: ?base.Region,
        loc: ?base.SourceLoc,

        fn borrow(self: OwnedFailure, messages: []const u8) LiteralRootFailure {
            return .{
                .message = messages[self.message_start..][0..self.message_len],
                .meaning = self.meaning,
                .region = self.region,
                .loc = self.loc,
            };
        }
    };

    /// What the failure consists of.
    const Cause = union(enum) {
        rejection: lir.LIR.LiteralRejectionKind,
        crash,
        /// The conversion read another literal root's failed value.
        literal: lir.LIR.LiteralRootId,
        /// The conversion read a checked root's failed value, which that
        /// root reports.
        checked,
    };

    fn init(allocator: Allocator, count: usize) Allocator.Error!LiteralRootFailures {
        const records = try allocator.alloc(?Record, count);
        errdefer allocator.free(records);
        @memset(records, null);
        const embedded = try allocator.alloc(bool, count);
        @memset(embedded, false);
        return .{ .records = records, .embedded = embedded };
    }

    fn deinit(self: *LiteralRootFailures, allocator: Allocator) void {
        self.messages.deinit(allocator);
        allocator.free(self.records);
        allocator.free(self.embedded);
    }

    fn record(self: *LiteralRootFailures, allocator: Allocator, id: lir.LIR.LiteralRootId, cause: Cause, failure: LiteralRootFailure) Allocator.Error!void {
        const destination = &self.records[@backingInt(id)];
        std.debug.assert(destination.* == null);
        destination.* = switch (cause) {
            .literal => |read| .{ .literal = read },
            .checked => .checked,
            .rejection, .crash => blk: {
                const start = self.messages.items.len;
                try self.messages.appendSlice(allocator, failure.message);
                break :blk .{ .own = .{
                    .kind = if (cause == .rejection) cause.rejection else null,
                    .message_start = start,
                    .message_len = failure.message.len,
                    .meaning = failure.meaning,
                    .region = failure.region,
                    .loc = failure.loc,
                } };
            },
        };
    }

    /// The literal root whose own failure `id`'s failure is, or null when
    /// it is a checked root's.
    fn source(self: *const LiteralRootFailures, id: lir.LIR.LiteralRootId) ?lir.LIR.LiteralRootId {
        var current = id;
        while (true) {
            const entry = self.records[@backingInt(current)] orelse
                finalizationInvariant("a failed literal root value was read before its root failed");
            switch (entry) {
                .own => return current,
                .literal => |read| current = read,
                .checked => return null,
            }
        }
    }
};

test "literal root failures own messages across evaluator teardown and buffer growth" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testLiteralRootFailureOwnership, .{});
}

test "literal root failures leave no record when retaining a message runs out of memory" {
    const allocator = std.testing.allocator;
    var failures = try LiteralRootFailures.init(allocator, 1);
    defer failures.deinit(allocator);
    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    const failure = LiteralRootFailure{ .message = "rejected", .meaning = null, .region = null, .loc = null };
    for (failures.records, 0..) |_, index| {
        const id: lir.LIR.LiteralRootId = @fromBackingInt(@intCast(index));
        try std.testing.expectError(error.OutOfMemory, failures.record(failing.allocator(), id, .crash, failure));
        try std.testing.expect(failures.records[0] == null);
        try std.testing.expectEqual(@as(usize, 0), failures.messages.items.len);
        try failures.record(allocator, id, .crash, failure);
        try std.testing.expectEqualStrings("rejected", failures.records[0].?.own.borrow(failures.messages.items).message);
    }
}

fn testLiteralRootFailureOwnership(allocator: Allocator) (Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
    var failures = try LiteralRootFailures.init(allocator, 6);
    defer failures.deinit(allocator);
    var root_ids: [6]lir.LIR.LiteralRootId = undefined;
    for (failures.records, 0..) |_, index| root_ids[index] = @fromBackingInt(@intCast(index));
    try std.testing.expectEqual(@as(usize, 0), failures.messages.capacity);

    const expected = "this message is long enough to live on the heap";
    var temporary: [expected.len]u8 = undefined;
    @memcpy(&temporary, expected);
    const failure = LiteralRootFailure{
        .message = &temporary,
        .meaning = .checked_error,
        .region = base.Region.from_raw_offsets(3, 8),
        .loc = .{ .file = 2, .line = 4, .column = 5 },
    };
    try failures.record(allocator, root_ids[0], .{ .rejection = .quote }, failure);
    // Deterministically invalidate the evaluator's bytes without reading freed memory.
    @memset(&temporary, 'x');
    try std.testing.expectEqualStrings(expected, failures.records[0].?.own.borrow(failures.messages.items).message);

    const previous_capacity = failures.messages.capacity;
    const later_message = try allocator.alloc(u8, previous_capacity + 1);
    defer allocator.free(later_message);
    @memset(later_message, 'y');
    var later = failure;
    later.message = later_message;
    try failures.record(allocator, root_ids[1], .crash, later);
    @memset(later_message, 'z');
    try std.testing.expect(failures.messages.capacity > previous_capacity);
    const retained = failures.records[0].?.own.borrow(failures.messages.items);
    try std.testing.expectEqualStrings(expected, retained.message);
    try std.testing.expectEqualDeep(failure.meaning, retained.meaning);
    try std.testing.expectEqualDeep(failure.region, retained.region);
    try std.testing.expectEqualDeep(failure.loc, retained.loc);
    try std.testing.expectEqual(lir.LIR.LiteralRejectionKind.quote, failures.records[0].?.own.kind.?);
    try std.testing.expectEqual(@as(?lir.LIR.LiteralRejectionKind, null), failures.records[1].?.own.kind);
    for (failures.records[1].?.own.borrow(failures.messages.items).message) |byte| try std.testing.expectEqual(@as(u8, 'y'), byte);

    const retained_len = failures.messages.items.len;
    later.message = "";
    try failures.record(allocator, root_ids[2], .{ .rejection = .numeral }, later);
    try std.testing.expectEqualStrings("", failures.records[2].?.own.borrow(failures.messages.items).message);
    try std.testing.expectEqual(lir.LIR.LiteralRejectionKind.numeral, failures.records[2].?.own.kind.?);
    try failures.record(allocator, root_ids[3], .{ .literal = root_ids[0] }, failure);
    try failures.record(allocator, root_ids[4], .{ .literal = root_ids[3] }, failure);
    try failures.record(allocator, root_ids[5], .checked, failure);
    try std.testing.expectEqual(retained_len, failures.messages.items.len);
    try std.testing.expectEqual(@as(?lir.LIR.LiteralRootId, root_ids[0]), failures.source(root_ids[4]));
    try std.testing.expectEqual(@as(?lir.LIR.LiteralRootId, null), failures.source(root_ids[5]));
}

const GuardProducerSlot = struct {
    module: checked.ModuleId,
    root: lir.LIR.ComptimeProducer,
};

/// The producer whose failed value a failure read.
fn guardProducerSlot(lir_result: *const lir.Program.Result, meaning: ?lir.LIR.FailureMeaning) ?GuardProducerSlot {
    return switch (meaning orelse return null) {
        .failed_value => |value| .{ .module = loweredModuleOfKey(lir_result, value.module), .root = value.producer },
        .checked_error, .literal_rejection, .exhaustiveness => null,
    };
}

fn moduleKeyBytesEqual(a: [32]u8, b: [32]u8) bool {
    for (a, b) |left, right| {
        if (left != right) return false;
    }
    return true;
}

/// The checked module a program lowered whose artifact key is `key`.
fn loweredModuleOfKey(lir_result: *const lir.Program.Result, key: [32]u8) checked.ModuleId {
    for (lir_result.lowering_modules.items) |module| {
        if (moduleKeyBytesEqual(module.bytes, key)) return module;
    }
    finalizationInvariant("a failure named a module its program does not lower");
}

/// Whether a checked root's failure was reported by that root. A guarded
/// hoisted root reports none: its failure leaves the original expression to
/// runtime, so a root that reads its failed value reports the crash itself.
fn checkedProducerReportedFailure(
    owners: *const ModuleOwners,
    module: checked.ModuleId,
    root_id: checked.ComptimeRootId,
) bool {
    for (owners.modules) |owner| {
        if (!artifactMatches(module, owner.module.key)) continue;
        return !owner.module.compile_time_roots.root(root_id).guarded;
    }
    finalizationInvariant("value guard producer belonged to a module this finalization does not complete");
}

/// What a checked root's failure reports in its own module.
const EmbeddedReport = enum {
    /// The root reported the failure it embeds.
    reported,
    /// The failure is another checked root's, which reports it.
    reported_elsewhere,
    /// The root's failure is its own crash.
    none,
};

/// Report, in a checked root's own module, a failure its result embeds: a
/// literal its evaluation rejected, or the failure of a literal root it read.
/// The report stays with the result that embeds it, so the module cannot be
/// cached clean while its result holds the failure; source outside the
/// module is named as the failure's origin.
fn reportEmbeddedFailure(
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root_region: base.Region,
    owners: *const ModuleOwners,
    lir_result: *const lir.Program.Result,
    failed_at: FailedAt,
    message: []const u8,
) FinalizeError!EmbeddedReport {
    // Code checking rejected owns its diagnostic; reaching it discards the
    // root's result without reporting the problem a second time.
    if (failedAtCheckedError(failed_at.meaning)) return .reported_elsewhere;
    const Embedded = struct { kind: ?lir.LIR.LiteralRejectionKind, message: []const u8, region: ?base.Region, loc: ?base.SourceLoc };
    const embedded: Embedded = if (guardProducerSlot(lir_result, failed_at.meaning)) |producer| switch (producer.root) {
        .checked => |producer_root| {
            if (checkedProducerReportedFailure(owners, producer.module, producer_root)) return .reported_elsewhere;
            return .none;
        },
        .literal => |read| blk: {
            const failures = owners.literal_failures orelse
                finalizationInvariant("a literal root guard was read in a program without literal roots");
            const source = failures.source(read) orelse return .reported_elsewhere;
            failures.embedded[@backingInt(source)] = true;
            const own = failures.records[@backingInt(source)].?.own;
            const failure = own.borrow(failures.messages.items);
            break :blk .{
                .kind = own.kind,
                .message = failure.message,
                .region = failure.region,
                .loc = failure.loc,
            };
        },
    } else if (failedLiteralRejection(lir_result, failed_at.meaning)) |site|
        .{ .kind = site.kind, .message = message, .region = failed_at.region, .loc = failed_at.loc }
    else
        return .none;
    const store = maybe_problem_store orelse return .reported_elsewhere;
    const site = comptimeFailureSiteFromLoc(module, &lir_result.store, root_region, embedded.region, embedded.loc);
    if (embedded.kind) |kind| {
        if (literalRejectionReported(store, kind, site.region)) return .reported;
        const message_idx = try store.putExtraString(embedded.message);
        const origin = try comptimeFailureOrigin(store, site);
        switch (kind) {
            .numeral => _ = try store.appendProblem(.{ .comptime_invalid_numeral = .{ .message = message_idx, .region = site.region, .origin = origin } }),
            .quote => _ = try store.appendProblem(.{ .comptime_invalid_quote = .{ .message = message_idx, .region = site.region, .origin = origin } }),
            .interpolation => _ = try store.appendProblem(.{ .comptime_invalid_interpolation = .{ .message = message_idx, .region = site.region, .origin = origin } }),
        }
    } else {
        const message_idx = try store.putExtraString(embedded.message);
        _ = try store.appendProblem(.{ .comptime_crash = .{
            .message = message_idx,
            .region = site.region,
            .origin = try comptimeFailureOrigin(store, site),
        } });
    }
    return .reported;
}

/// How evaluating a literal root failed.
const LiteralRootFailure = struct {
    message: []const u8,
    /// What failing where the evaluation stopped means.
    meaning: ?lir.LIR.FailureMeaning,
    region: ?base.Region,
    loc: ?base.SourceLoc,
};

/// Where a compile-time evaluation failed: the statement it stopped at, as
/// its source position and what failing there means
/// (`LirStore.failureMeaning`).
const FailedAt = struct {
    meaning: ?lir.LIR.FailureMeaning = null,
    region: ?base.Region = null,
    loc: ?base.SourceLoc = null,
};

/// Where an interpreter's evaluation failed.
fn interpreterFailedAt(lir_result: *const lir.Program.Result, interpreter: *const Interpreter, slots: *const StaticSlotEnvironment) FailedAt {
    return slots.withProducerOrigin(.{
        .meaning = if (interpreter.getFailedCrashStmt()) |stmt| lir_result.store.failureMeaning(stmt) else null,
        .region = interpreter.getFailedCheckedRegion(),
        .loc = interpreter.getFailedSourceLoc(),
    });
}

fn evalLiteralRoot(
    allocator: Allocator,
    owners: *const ModuleOwners,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    plan: LirProgram.LiteralRootPlan,
    program: anytype,
) FinalizeError!bool {
    if (@TypeOf(program) == *DevProgram) return evalDevLiteralRoot(allocator, owners, options, lowered, plan, program);
    if (@TypeOf(program) == *InterpreterProgram) return evalInterpreterLiteralRoot(allocator, owners, options, lowered, plan, program);
    @compileError("compile-time evaluation owner must be explicit native or interpreter program");
}

fn evalInterpreterLiteralRoot(
    allocator: Allocator,
    owners: *const ModuleOwners,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    plan: LirProgram.LiteralRootPlan,
    shared_program: *InterpreterProgram,
) FinalizeError!bool {
    const program = try shared_program.fork(lowered);
    defer program.deinit();
    program.timing_io = if (options.timing) |timing| timing.std_io else null;
    const interpreter = &program.interpreter;
    const producer: lir.LIR.ComptimeProducer = .{ .literal = plan.id };
    const failure: ?LiteralRootFailure = evaluated: {
        const result = interpreter.eval(.{ .proc_id = plan.proc, .ret_layout = plan.ret_layout }) catch |err| {
            const message = switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.RuntimeError, error.DivisionByZero => interpreter.getRuntimeErrorMessage() orelse "compile-time evaluation failed",
                error.Crash => interpreter.getCrashMessage(),
                error.UnsupportedHostedFunction => finalizationInvariant("literal root reached an unsupported hosted function"),
                error.InvalidHostedFunctionSignature => finalizationInvariant("literal root reached an invalid hosted function signature"),
                // expect_err statements only occur in top-level expect test roots.
                error.ExpectErr => unreachable,
            };
            const failed_at = interpreterFailedAt(&lowered.lir_result, interpreter, program.slotEnvironment());
            break :evaluated .{
                .message = message,
                .meaning = failed_at.meaning,
                .region = failed_at.region,
                .loc = failed_at.loc,
            };
        };
        defer interpreter.dropValue(result.value, plan.ret_layout);
        if (options.publish_shared_slots) try program.publishRoot(lowered, plan.site.owner, producer, plan.shape(), result.value);
        break :evaluated null;
    };
    // A failure that read a member of its group not yet evaluated
    // (`Options.retry`) leaves the root for the group to run again.
    if (failure) |failed| if (options.retry) |retry| if (retry.holds(failed.meaning)) return false;
    for (program.host.debugMessages()) |message| try emitDebugMessage(allocator, options, false, message);
    for (interpreter.getExpectFailures()) |expect_failure| {
        try reportLiteralRootExpectFailure(owners, lowered, plan, expect_failure.message, expect_failure.region, expect_failure.loc);
    }
    const failed = failure orelse return true;
    try recordLiteralRootFailure(allocator, owners, lowered, plan, failed);
    if (options.publish_shared_slots) {
        try program.slotEnvironment().publishFailureOrigin(plan.site.owner, producer, .{ .loc = failed.loc, .region = failed.region }, &lowered.lir_result);
        try program.slotEnvironment().publishFailure(lowered, plan.site.owner, producer, .{ .message = failed.message, .kind = .crash }, .{ .resolve = InterpreterProgram.resolveFunction });
        program.refreshCallableMetadata();
    }
    return true;
}

fn evalDevLiteralRoot(
    allocator: Allocator,
    owners: *const ModuleOwners,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    plan: LirProgram.LiteralRootPlan,
    native: *DevProgram,
) FinalizeError!bool {
    var host_allocator_impl = ThreadSafeAllocator.init(allocator);
    const host_allocator = host_allocator_impl.allocator();
    const entry_offset = native.entry_offsets.get(plan.proc) orelse
        finalizationInvariant("native program omitted a literal root entry wrapper");
    const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(plan.ret_layout));
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(size_align.size, 1));
    defer allocator.free(ret_buf);
    @memset(ret_buf, 0);
    var host = CompileTimeHost.init(host_allocator);
    defer host.deinit();
    host.setGeneratedCode(native.executable.generatedCode());
    host.timing_io = if (options.timing) |timing| timing.std_io else null;

    const boxy_tables = Interpreter.BoxyTables.fromResult(&lowered.lir_result);
    const selected_runtime = if (boxy_tables.needsRuntimeForStore(&lowered.lir_result.store))
        try boxy_abi.createRuntimeFromStores(allocator, &lowered.lir_result.store, &lowered.lir_result.layouts, boxy_tables, host.ops())
    else
        null;
    defer if (selected_runtime) |runtime| boxy_abi.deinitRuntime(runtime);
    const previous_runtime = if (selected_runtime) |runtime| boxy_abi.swapActiveRuntime(runtime) else null;
    defer if (selected_runtime != null) {
        _ = boxy_abi.swapActiveRuntime(previous_runtime);
    };

    host.resetForRun();
    if (selected_runtime != null) boxy_abi.setGlobalRocOps(host.ops());
    host.entry_frame = @frameAddress();
    var crash_boundary = host.enterCrashBoundary();
    const entered = builtins.in_process_host.enter(host.ops(), null);
    if (crash_boundary.set() == 0) {
        native.executable.callRocABIAt(entry_offset, @ptrCast(ret_buf.ptr), null);
    }
    builtins.in_process_host.leave(entered);
    crash_boundary.deinit();

    const producer: lir.LIR.ComptimeProducer = .{ .literal = plan.id };
    const failure: ?LiteralRootFailure = switch (host.termination) {
        .returned => null,
        .crashed => crashed: {
            const failed_at = native.failedAt(host.failure);
            break :crashed .{ .message = host.crashMessage() orelse "Roc crashed", .meaning = failed_at.meaning, .region = failed_at.region, .loc = failed_at.loc };
        },
        .host_oom => return error.OutOfMemory,
    };
    // A failure that read a member of its group not yet evaluated
    // (`Options.retry`) leaves the root for the group to run again.
    if (failure) |failed| if (options.retry) |retry| if (retry.holds(failed.meaning)) return false;
    for (host.events.items) |event| switch (event) {
        .dbg => |message| try emitDebugMessage(allocator, options, false, message),
        .expect_failed => |expect_failure| {
            const failed_at = native.failedAt(expect_failure.point);
            try reportLiteralRootExpectFailure(owners, lowered, plan, expect_failure.message, failed_at.region, failed_at.loc);
        },
        .crashed => {},
    };
    const failed = failure orelse {
        if (options.publish_shared_slots) try native.publishRootWithRuntime(lowered, plan.site.owner, producer, plan.shape(), .{ .ptr = ret_buf.ptr }, if (selected_runtime) |runtime| &runtime.runtime else null);
        if (options.publish_shared_slots) try native.publishFailure(lowered, plan.site.owner, producer, null);
        return true;
    };
    try recordLiteralRootFailure(allocator, owners, lowered, plan, failed);
    if (options.publish_shared_slots) {
        try native.slots.publishFailureOrigin(plan.site.owner, producer, .{ .loc = failed.loc, .region = failed.region }, &lowered.lir_result);
        try native.publishFailure(lowered, plan.site.owner, producer, .{ .message = failed.message, .kind = .crash });
    }
    return true;
}

fn recordLiteralRootFailure(
    allocator: Allocator,
    owners: *const ModuleOwners,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    plan: LirProgram.LiteralRootPlan,
    failure: LiteralRootFailure,
) Allocator.Error!void {
    const failures = owners.literal_failures orelse
        finalizationInvariant("a literal root failed in a finalization that records no literal root failures");
    const cause: LiteralRootFailures.Cause = if (guardProducerSlot(&lowered.lir_result, failure.meaning)) |producer| switch (producer.root) {
        .literal => |read| .{ .literal = read },
        .checked => |producer_root| if (checkedProducerReportedFailure(owners, producer.module, producer_root)) .checked else .crash,
    } else if (failedLiteralRejection(&lowered.lir_result, failure.meaning)) |site|
        .{ .rejection = site.kind }
    else
        .crash;
    try failures.record(allocator, plan.id, cause, failure);
}

/// Report a failed literal root that no checked root embeds, in its literal's
/// module: a rejection reports the rejected literal, and any other failure is
/// a crash of the conversion, reported at the literal.
fn reportLiteralRootFailure(
    owners: *const ModuleOwners,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    plan: LirProgram.LiteralRootPlan,
    failure: LiteralRootFailure,
) FinalizeError!void {
    if (!owners.report_sites) return;
    if (failedLiteralRejection(&lowered.lir_result, failure.meaning)) |site| {
        return reportLiteralRejection(owners, site, failure.message);
    }
    var targets = try owners.reportTargets(plan.site.owner);
    while (targets.nextTarget()) |owner| {
        const store = owner.problem_store orelse continue;
        const literal_region = owner.module.checked_bodies.expr(@fromBackingInt(@intCast(plan.site.checked_expr))).source_region;
        const site = comptimeFailureSiteFromLoc(owner.module, &lowered.lir_result.store, literal_region, failure.region, failure.loc);
        const message_idx = try store.putExtraString(failure.message);
        _ = try store.appendProblem(.{ .comptime_crash = .{
            .message = message_idx,
            .region = site.region,
            .origin = try comptimeFailureOrigin(store, site),
        } });
    }
}

fn reportLiteralRootExpectFailure(
    owners: *const ModuleOwners,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    plan: LirProgram.LiteralRootPlan,
    message: []const u8,
    region: ?base.Region,
    loc: ?base.SourceLoc,
) FinalizeError!void {
    var targets = try owners.reportTargets(plan.site.owner);
    while (targets.nextTarget()) |owner| {
        const store = owner.problem_store orelse continue;
        const literal_region = owner.module.checked_bodies.expr(@fromBackingInt(@intCast(plan.site.checked_expr))).source_region;
        const site = comptimeFailureSiteFromLoc(owner.module, &lowered.lir_result.store, literal_region, region, loc);
        const message_idx = try store.putExtraString(message);
        _ = try store.appendProblem(.{ .comptime_expect_failed = .{
            .message = message_idx,
            .region = site.region,
            .origin = try comptimeFailureOrigin(store, site),
        } });
    }
}

fn compilerHostMustUseInterpreterForCtfe() bool {
    return builtin.target.os.tag == .freestanding or
        builtin.target.cpu.arch == .wasm32;
}

const DevRootProgressState = enum(u8) {
    pending,
    running,
    done,
};

const ProgressMillis = usize;

const DevRootResult = enum {
    pending,
    success,
    crashed,
    host_oom,
};

const ThreadSafeAllocator = struct {
    child: Allocator,
    mutex: std.atomic.Mutex = .unlocked,

    fn init(child: Allocator) ThreadSafeAllocator {
        return .{ .child = child };
    }

    fn allocator(self: *ThreadSafeAllocator) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn lock(self: *ThreadSafeAllocator) void {
        while (!self.mutex.tryLock()) {
            std.atomic.spinLoopHint();
        }
    }

    fn unlock(self: *ThreadSafeAllocator) void {
        self.mutex.unlock();
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *ThreadSafeAllocator = @ptrCast(@alignCast(ctx));
        self.lock();
        defer self.unlock();
        return self.child.rawAlloc(len, alignment, ret_addr);
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *ThreadSafeAllocator = @ptrCast(@alignCast(ctx));
        self.lock();
        defer self.unlock();
        return self.child.rawResize(memory, alignment, new_len, ret_addr);
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *ThreadSafeAllocator = @ptrCast(@alignCast(ctx));
        self.lock();
        defer self.unlock();
        return self.child.rawRemap(memory, alignment, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *ThreadSafeAllocator = @ptrCast(@alignCast(ctx));
        self.lock();
        defer self.unlock();
        self.child.rawFree(memory, alignment, ret_addr);
    }
};

fn boxyNativeFnTable() BoxyNativeFnTable {
    var table: BoxyNativeFnTable = undefined;
    inline for (@typeInfo(BoxyBuiltinFn).@"enum".field_values) |field_value| {
        const boxy_fn: BoxyBuiltinFn = @fromBackingInt(@intCast(field_value));
        const name = comptime boxy_fn.symbolName();
        table[field_value] = @intFromPtr(&@field(boxy_abi, name));
    }
    return table;
}

const DevRootLabel = struct {
    module_name: []const u8,
    snippet: []u8,
    line: u32,
    column: u32,

    fn deinit(self: *DevRootLabel, allocator: Allocator) void {
        allocator.free(self.snippet);
    }
};

const DevRootJob = struct {
    root: LirProgram.ConstRootPlan,
    root_id: checked.ComptimeRootId,
    compile_time_root: checked.CompileTimeRoot,
    entry_offset: usize,
    ret_buf: []align(collections.max_roc_alignment.toByteUnits()) u8,
    host: CompileTimeHost,
    result: DevRootResult = .pending,
    progress: std.atomic.Value(u8) = std.atomic.Value(u8).init(@backingInt(DevRootProgressState.pending)),
    start_ms: std.atomic.Value(ProgressMillis) = std.atomic.Value(ProgressMillis).init(0),
    last_progress_ms: std.atomic.Value(ProgressMillis) = std.atomic.Value(ProgressMillis).init(0),
    label: DevRootLabel,

    fn deinit(self: *DevRootJob, allocator: Allocator) void {
        self.host.deinit();
        allocator.free(self.ret_buf);
        self.label.deinit(allocator);
    }
};

const DevRunContext = struct {
    executable: *const backend.ExecutableMemory,
    jobs: []DevRootJob,
    std_io: ?std.Io,
    progress_reporter: ?*DevProgressReporter,
    boxy_global_installed: bool,
    had_oom: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
};

const DevProgressThread = if (base.parallel.is_freestanding) struct {} else std.Thread;

const DevProgressReporter = struct {
    options: Options,
    jobs: []DevRootJob,
    stop: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    wake_epoch: std.atomic.Value(u32) = std.atomic.Value(u32).init(0),
    thread: ?DevProgressThread = null,

    fn init(options: Options, jobs: []DevRootJob) DevProgressReporter {
        return .{
            .options = options,
            .jobs = jobs,
        };
    }

    fn deinit(self: *DevProgressReporter) void {
        self.finish();
        self.* = undefined;
    }

    fn start(self: *DevProgressReporter) std.Thread.SpawnError!void {
        if (comptime base.parallel.is_freestanding) return;
        if (self.options.stderr == null) return;
        if (self.options.std_io == null) return;
        if (self.jobs.len == 0) return;
        if (self.options.slow_root_threshold_ns == 0) return;
        if (self.options.slow_root_period_ns == 0) return;
        self.thread = try std.Thread.spawn(.{}, progressThreadMain, .{self});
    }

    fn finish(self: *DevProgressReporter) void {
        if (comptime base.parallel.is_freestanding) return;
        const thread = self.thread orelse return;
        self.stop.store(true, .release);
        self.wake();
        thread.join();
        self.thread = null;
    }

    fn rootStarted(self: *DevProgressReporter) void {
        if (comptime base.parallel.is_freestanding) return;
        if (self.thread == null) return;
        self.wake();
    }

    fn wake(self: *DevProgressReporter) void {
        const io = self.options.std_io orelse return;
        _ = self.wake_epoch.fetchAdd(1, .acq_rel);
        std.Io.futexWake(io, u32, &self.wake_epoch.raw, 1);
    }

    fn progressThreadMain(self: *DevProgressReporter) void {
        const io = self.options.std_io orelse return;
        while (!self.stop.load(.acquire)) {
            const epoch = self.wake_epoch.load(.acquire);
            const now = nowMs(io);
            self.reportDue(now);
            const next_wait_ms = self.nextWaitMs(now);
            if (self.stop.load(.acquire)) break;
            if (next_wait_ms) |wait_ms| {
                if (wait_ms == 0) continue;
                self.waitForWake(io, epoch, .{ .duration = .{
                    .raw = std.Io.Duration.fromMilliseconds(std.math.cast(i64, wait_ms) orelse std.math.maxInt(i64)),
                    .clock = .awake,
                } });
            } else {
                self.waitForWake(io, epoch, .none);
            }
        }
    }

    fn waitForWake(self: *DevProgressReporter, io: std.Io, epoch: u32, timeout: std.Io.Timeout) void {
        std.Io.futexWaitTimeout(io, u32, &self.wake_epoch.raw, epoch, timeout) catch {};
    }

    fn reportDue(self: *DevProgressReporter, now: ProgressMillis) void {
        const stderr = self.options.stderr orelse return;
        const threshold = progressDurationMs(self.options.slow_root_threshold_ns);
        const period = progressDurationMs(self.options.slow_root_period_ns);
        const spinner = spinnerByte(now / 250);

        for (self.jobs) |*job| {
            const progress: DevRootProgressState = @fromBackingInt(@intCast(job.progress.load(.acquire)));
            if (progress != .running) continue;
            const started_at = job.start_ms.load(.acquire);
            const elapsed_since_start = elapsedMs(now, started_at) orelse continue;
            if (elapsed_since_start < threshold) continue;
            const last = job.last_progress_ms.load(.acquire);
            if (last != 0) {
                const elapsed_since_last = elapsedMs(now, last) orelse continue;
                if (elapsed_since_last < period) continue;
            }
            if (job.last_progress_ms.cmpxchgStrong(last, now, .acq_rel, .acquire) != null) continue;
            const elapsed_s = elapsed_since_start / std.time.ms_per_s;
            var line_buf: [4096]u8 = undefined;
            const line = progressLine(&line_buf, spinner, job.label, @intCast(elapsed_s)) catch return;
            stderr.writeAll(line);
        }
    }

    fn nextWaitMs(self: *DevProgressReporter, now: ProgressMillis) ?ProgressMillis {
        const threshold = progressDurationMs(self.options.slow_root_threshold_ns);
        const period = progressDurationMs(self.options.slow_root_period_ns);
        var next: ?ProgressMillis = null;

        for (self.jobs) |*job| {
            const progress: DevRootProgressState = @fromBackingInt(@intCast(job.progress.load(.acquire)));
            if (progress != .running) continue;
            const started_at = job.start_ms.load(.acquire);
            if (started_at == 0) continue;
            const wait = blk: {
                const threshold_wait = msUntilElapsed(now, started_at, threshold);
                if (threshold_wait != 0) break :blk threshold_wait;
                const last = job.last_progress_ms.load(.acquire);
                if (last == 0) break :blk 0;
                break :blk msUntilElapsed(now, last, period);
            };
            if (next == null or wait < next.?) next = wait;
        }

        return next;
    }

    fn progressLine(
        buf: []u8,
        spinner: u8,
        label: DevRootLabel,
        elapsed_s: u64,
    ) std.fmt.BufPrintError![]u8 {
        return std.fmt.bufPrint(
            buf,
            "{c} Evaluating `{s}` at compile time in {s}:{d}:{d} ({d}s)\n",
            .{
                spinner,
                label.snippet,
                label.module_name,
                label.line,
                label.column,
                elapsed_s,
            },
        );
    }
};

const CompletedNativeRoot = struct {
    exports: []static_data_exports.StaticDataExport,
    image: backend.StaticDataImage,

    fn deinit(self: *CompletedNativeRoot, allocator: Allocator) void {
        self.image.deinit();
        static_data_exports.deinitStaticData(allocator, self.exports);
    }
};

fn resolveInterpreterCallable(data_ptr: [*]u8) NativeRootExport.CallableResolution {
    const callable = Interpreter.interpretedCallable(data_ptr) orelse finalizationInvariant("interpreter result omitted its explicit callable ABI");
    return .{ .proc = callable.proc, .capture_ptr = callable.capture_ptr };
}

/// Owns one native compilation shared by dependency-ordered root batches.
/// Stable owner: interpreter-created callable contexts retain its address.
const InterpreterProgram = struct {
    timing_io: ?std.Io = null,
    allocator: Allocator,
    slots: StaticSlotEnvironment,
    shared_slots: ?*StaticSlotEnvironment = null,
    host: CompilerHost,
    /// Literal backings owned by the root program and borrowed by its forks,
    /// so published values may reference them for the whole finalization.
    static_strings: Interpreter.StaticStrings.Table,
    interpreter: Interpreter,
    /// Owner named by the callable headers of every image this program's
    /// slot environment installs: its static callables run on `interpreter`.
    callable_owner: Interpreter.StaticCallableOwner = .unbound,
    /// Forks share the root program's slot environment, so the root writes
    /// every image's callable headers and owns their callables.
    shared_callable_owner: ?*InterpreterProgram = null,
    callable_images: usize = 0,
    base_callables_ready: bool = false,

    fn init(allocator: Allocator, modules: lir.CheckedPipeline.CheckedModuleSet, lowered: *lir.CheckedPipeline.LoweredProgram, options: Options) FinalizeError!*InterpreterProgram {
        const self = try allocator.create(InterpreterProgram);
        errdefer allocator.destroy(self);
        self.allocator = allocator;
        self.timing_io = null;
        self.shared_slots = null;
        self.callable_owner = .unbound;
        self.shared_callable_owner = null;
        self.callable_images = 0;
        self.base_callables_ready = false;
        const started = if (options.timing) |timing| timing.start() else 0;
        self.slots = try StaticSlotEnvironment.init(allocator, modules, lowered, roc_target.RocTarget.detectNative(), interpreter_static_data.image_options);
        errdefer self.slots.deinit();
        self.host = CompilerHost.init(allocator);
        errdefer self.host.deinit();
        self.static_strings = try Interpreter.buildStaticStrings(allocator, &lowered.lir_result.store);
        errdefer self.static_strings.deinit();
        self.interpreter = try Interpreter.initWithBoxyTables(allocator, &lowered.lir_result.store, &lowered.lir_result.layouts, Interpreter.BoxyTables.fromResult(&lowered.lir_result), self.static_strings.view(), self.host.ops());
        errdefer self.interpreter.deinit();
        self.host.bindInterpreter(&self.interpreter);
        self.callable_owner = .forInterpreter(&self.interpreter);
        self.slots.image.resolveFunctionRelocations(.{ .resolve = resolveFunction }) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => finalizationInvariant("interpreter image omitted a callable procedure"),
        };
        self.refreshCallableMetadata();
        if (options.timing) |timing| timing.finish(started, .static_data);
        return self;
    }

    fn slotEnvironment(self: *InterpreterProgram) *StaticSlotEnvironment {
        return self.shared_slots orelse &self.slots;
    }

    fn freezeCompleted(self: *InterpreterProgram) Allocator.Error!LirProgram.FrozenStaticData {
        return self.slotEnvironment().freezeCompleted();
    }

    /// A fork keeps its own frames, host events, and temporary values while
    /// sharing the root program's slots.
    fn fork(self: *InterpreterProgram, lowered: *lir.CheckedPipeline.LoweredProgram) FinalizeError!*InterpreterProgram {
        const allocator = self.allocator;
        const child = try allocator.create(InterpreterProgram);
        errdefer allocator.destroy(child);
        child.* = .{
            .allocator = allocator,
            .slots = undefined,
            .shared_slots = self.slotEnvironment(),
            .shared_callable_owner = self.shared_callable_owner orelse self,
            .host = CompilerHost.init(allocator),
            .static_strings = undefined,
            .interpreter = undefined,
        };
        errdefer child.host.deinit();
        child.interpreter = try Interpreter.initWithBoxyTables(allocator, &lowered.lir_result.store, &lowered.lir_result.layouts, Interpreter.BoxyTables.fromResult(&lowered.lir_result), self.interpreter.static_strings, child.host.ops());
        errdefer child.interpreter.deinit();
        child.host.bindInterpreter(&child.interpreter);
        child.refreshCallableMetadata();
        return child;
    }

    const resolveFunction = interpreter_static_data.resolveFunction;

    fn resolveCallable(_: ?*anyopaque, data_ptr: [*]u8) error{RuntimeError}!NativeRootExport.CallableResolution {
        return resolveInterpreterCallable(data_ptr);
    }

    fn resolveStoredCallable(raw: ?*anyopaque, data_ptr: [*]u8) error{RuntimeError}!ConstStoreWriter.ErasedCallableResolution {
        const callable = try resolveCallable(raw, data_ptr);
        return .{ .proc = callable.proc, .capture_ptr = callable.capture_ptr };
    }

    fn refreshCallableMetadata(self: *InterpreterProgram) void {
        const owner = self.shared_callable_owner orelse self;
        const slots = owner.slotEnvironment();
        if (!owner.base_callables_ready) {
            interpreter_static_data.writeCallableHeaders(slots.materialized, &slots.image, &owner.callable_owner);
            owner.base_callables_ready = true;
        }
        while (owner.callable_images < slots.completed_roots.items.len) : (owner.callable_images += 1) {
            const root = &slots.completed_roots.items[owner.callable_images];
            interpreter_static_data.writeCallableHeaders(root.exports, &root.image, &owner.callable_owner);
        }
        self.interpreter.setStaticData(slots.addresses);
    }

    fn publishRoot(self: *InterpreterProgram, lowered: *lir.CheckedPipeline.LoweredProgram, module: lir.LIR.LoweringModuleId, producer: lir.LIR.ComptimeProducer, shape: LirProgram.RootShape, value: @import("value.zig").Value) FinalizeError!void {
        try self.slotEnvironment().publishRoot(lowered, module, producer, shape, value, .{ .context = self, .resolve = resolveCallable }, .{ .resolve = resolveFunction });
        try self.slotEnvironment().publishFailure(lowered, module, producer, null, .{ .resolve = resolveFunction });
        self.refreshCallableMetadata();
    }

    fn deinit(self: *InterpreterProgram) void {
        const allocator = self.allocator;
        self.interpreter.deinit();
        self.host.deinit();
        if (self.shared_slots == null) {
            self.slots.deinit();
            self.static_strings.deinit();
        }
        allocator.destroy(self);
    }
};

/// Session-local reverse links from declared producers to all their slots.
/// Module and root ids are the existing lowering identities, including every
/// representation of a checked root. No source identities are reconstructed.
const PublicationSlots = struct {
    checked_heads: []std.ArrayList(?lir.LIR.StaticDataId),
    literal_heads: []?lir.LIR.StaticDataId,
    next: []?lir.LIR.StaticDataId,

    fn init(allocator: Allocator, result: *const LirProgram.Result) Allocator.Error!PublicationSlots {
        const checked_heads = try allocator.alloc(std.ArrayList(?lir.LIR.StaticDataId), result.lowering_modules.items.len);
        @memset(checked_heads, .empty);
        errdefer {
            for (checked_heads) |*heads| heads.deinit(allocator);
            allocator.free(checked_heads);
        }
        var literal_heads = std.ArrayList(?lir.LIR.StaticDataId).empty;
        errdefer literal_heads.deinit(allocator);
        const next = try allocator.alloc(?lir.LIR.StaticDataId, result.static_data_values.items.len);
        errdefer allocator.free(next);
        @memset(next, null);
        var last_key: ?checked.ModuleId = null;
        var last_owner: lir.LIR.LoweringModuleId = undefined;
        for (result.static_data_values.items, 0..) |entry, index| {
            const root = entry.compile_time_root orelse continue;
            const head = switch (root.root) {
                .literal => |id| blk: {
                    const raw = @backingInt(id);
                    if (raw >= literal_heads.items.len) try literal_heads.appendNTimes(allocator, null, raw + 1 - literal_heads.items.len);
                    break :blk &literal_heads.items[raw];
                },
                .checked => |id| blk: {
                    const reuse = if (last_key) |key| key.eql(root.module) else false;
                    if (!reuse) {
                        last_owner = result.loweringModuleId(root.module) orelse
                            finalizationInvariant("compile-time slot has no declared module");
                        last_key = root.module;
                    }
                    const heads = &checked_heads[@backingInt(last_owner)];
                    const raw = @backingInt(id);
                    if (raw >= heads.items.len) try heads.appendNTimes(allocator, null, raw + 1 - heads.items.len);
                    break :blk &heads.items[raw];
                },
            };
            next[index] = head.*;
            head.* = @fromBackingInt(@intCast(index));
        }
        return .{ .checked_heads = checked_heads, .literal_heads = try literal_heads.toOwnedSlice(allocator), .next = next };
    }

    fn first(self: *const PublicationSlots, owner: lir.LIR.LoweringModuleId, producer: lir.LIR.ComptimeProducer) ?lir.LIR.StaticDataId {
        return switch (producer) {
            .literal => |id| if (@backingInt(id) < self.literal_heads.len) self.literal_heads[@backingInt(id)] else null,
            .checked => |id| blk: {
                const heads = self.checked_heads[@backingInt(owner)].items;
                const raw = @backingInt(id);
                break :blk if (raw < heads.len) heads[raw] else null;
            },
        };
    }

    fn deinit(self: *PublicationSlots, allocator: Allocator) void {
        for (self.checked_heads) |*heads| heads.deinit(allocator);
        allocator.free(self.checked_heads);
        allocator.free(self.literal_heads);
        allocator.free(self.next);
    }
};

test "publication slots preserve module identity and every root representation" {
    try testPublicationSlots(std.testing.allocator);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testPublicationSlots, .{});
}

fn testPublicationSlots(allocator: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
    var result = try LirProgram.Result.init(allocator, .native);
    defer result.deinit();
    var keys = [_]checked.ModuleId{ .{}, .{} };
    keys[1].bytes[0] = 1;
    try result.setLoweringModules(&keys);
    const first_module = result.loweringModuleId(keys[0]).?;
    const second_module = result.loweringModuleId(keys[1]).?;
    const literal_id: lir.LIR.LiteralRootId = @fromBackingInt(@intCast(result.literal_roots.items.len));
    // Only the literal table's length is used to allocate its dense index.
    try result.literal_roots.append(allocator, undefined);
    const producers = [_]lir.LIR.ComptimeProducer{
        .{ .checked = @fromBackingInt(@intCast(7)) },
        .{ .checked = @fromBackingInt(@intCast(7)) },
        .{ .checked = @fromBackingInt(@intCast(7)) },
        .{ .literal = literal_id },
    };
    const modules = [_]usize{ 0, 1, 0, 1 };
    // The append loop fills every slot ID before the assertions read them.
    var slots: [producers.len]lir.LIR.StaticDataId = undefined;
    for (producers, modules, &slots) |producer, module, *slot| {
        slot.* = @fromBackingInt(@intCast(result.static_data_values.items.len));
        try result.static_data_values.append(allocator, .{
            .initializer = null,
            .layout_idx = .u8,
            .compile_time_root = .{
                .module = keys[module],
                .root = producer,
                .const_locator = null,
                // Reverse links consume only module and producer identity, never the role.
                .role = undefined,
            },
        });
    }
    var publication = try PublicationSlots.init(allocator, &result);
    defer publication.deinit(allocator);
    const slot = lir.LIR.StaticDataId;
    try std.testing.expectEqual(@as(?slot, slots[2]), publication.first(first_module, producers[0]));
    try std.testing.expectEqual(@as(?slot, slots[0]), publication.next[@backingInt(slots[2])]);
    try std.testing.expectEqual(@as(?slot, null), publication.next[@backingInt(slots[0])]);
    try std.testing.expectEqual(@as(?slot, slots[1]), publication.first(second_module, producers[1]));
    try std.testing.expectEqual(@as(?slot, null), publication.next[@backingInt(slots[1])]);
    try std.testing.expectEqual(@as(?slot, slots[3]), publication.first(second_module, producers[3]));
    try std.testing.expectEqual(@as(?slot, null), publication.first(first_module, .{ .checked = @fromBackingInt(@intCast(6)) }));
    try std.testing.expectEqual(@as(?slot, null), publication.first(first_module, .{ .checked = @fromBackingInt(@intCast(8)) }));
}

/// Shared immutable-slot publication owner for native and interpreter sessions.
const StaticSlotEnvironment = struct {
    allocator: Allocator,
    materialized: []static_data_exports.StaticDataExport,
    image: backend.StaticDataImage,
    addresses: []usize,
    completed_roots: std.ArrayList(CompletedNativeRoot) = .empty,
    /// Layout of every image this environment builds, chosen by its evaluator.
    image_options: backend.StaticDataImage.Options = .{},
    publication: PublicationSlots,
    export_positions: []?usize,
    /// Where each failed producer failed. A read of its failed value that
    /// reports the failure reports it there.
    producer_origins: std.AutoHashMapUnmanaged(ProducerOriginKey, lir.LIR.ComptimeFailureOrigin) = .empty,

    const ProducerOriginKey = struct {
        module: [32]u8,
        producer: lir.LIR.ComptimeProducer,
    };

    fn init(allocator: Allocator, modules: lir.CheckedPipeline.CheckedModuleSet, lowered: *lir.CheckedPipeline.LoweredProgram, target: roc_target.RocTarget, image_options: backend.StaticDataImage.Options) FinalizeError!StaticSlotEnvironment {
        const materialized_static_data = try materialize(allocator, modules, lowered, target);
        const static_data_image = backend.StaticDataImage.initWithOptions(allocator, materialized_static_data, image_options) catch |err| {
            static_data_exports.deinitStaticData(allocator, materialized_static_data);
            return staticImageError(err);
        };
        return initInImage(allocator, lowered, materialized_static_data, static_data_image, image_options);
    }

    /// The program's static data, with every compile-time value slot this
    /// program evaluates zeroed.
    fn materialize(allocator: Allocator, modules: lir.CheckedPipeline.CheckedModuleSet, lowered: *lir.CheckedPipeline.LoweredProgram, target: roc_target.RocTarget) FinalizeError![]static_data_exports.StaticDataExport {
        return static_data_exports.buildStaticData(
            allocator,
            .{
                .root = modules.root,
                .imports = modules.imports,
            },
            lowered,
            target,
            .{ .prepare_compile_time_slots = true },
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.UnsupportedTarget => return error.UnsupportedPlatform,
        };
    }

    fn staticImageError(err: backend.StaticDataImageError) Allocator.Error {
        return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            error.DuplicateStaticDataSymbol,
            error.InvalidStaticDataAlignment,
            error.InvalidStaticDataRelocation,
            error.MissingStaticDataSymbol,
            error.UnresolvedStaticFunction,
            => finalizationInvariant("invalid native static-data image during compile-time finalization"),
        };
    }

    /// The environment of `materialized_static_data`, built into
    /// `static_data_image`. Takes ownership of both, on success and failure.
    fn initInImage(
        allocator: Allocator,
        lowered: *lir.CheckedPipeline.LoweredProgram,
        materialized_static_data: []static_data_exports.StaticDataExport,
        image: backend.StaticDataImage,
        image_options: backend.StaticDataImage.Options,
    ) FinalizeError!StaticSlotEnvironment {
        errdefer static_data_exports.deinitStaticData(allocator, materialized_static_data);
        var static_data_image = image;
        errdefer static_data_image.deinit();

        const native_static_data = static_data_image.lirValueAddresses(
            allocator,
            lowered.lir_result.static_data_values.items.len,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DuplicateStaticDataSymbol,
            error.InvalidStaticDataAlignment,
            error.InvalidStaticDataRelocation,
            error.MissingStaticDataSymbol,
            error.UnresolvedStaticFunction,
            => finalizationInvariant("materialized compile-time static data omitted an LIR static-data symbol"),
        };
        errdefer allocator.free(native_static_data);
        var publication = try PublicationSlots.init(allocator, &lowered.lir_result);
        errdefer publication.deinit(allocator);
        const export_positions = try exportPositions(allocator, materialized_static_data, native_static_data.len);
        errdefer allocator.free(export_positions);
        return .{ .allocator = allocator, .materialized = materialized_static_data, .image = static_data_image, .addresses = native_static_data, .image_options = image_options, .publication = publication, .export_positions = export_positions };
    }

    fn exportPositions(allocator: Allocator, exports: []const static_data_exports.StaticDataExport, count: usize) Allocator.Error![]?usize {
        const positions = try allocator.alloc(?usize, count);
        @memset(positions, null);
        for (exports, 0..) |entry, index| if (entry.value_id) |slot| {
            positions[@backingInt(slot)] = index;
        };
        return positions;
    }

    fn publishRoot(
        self: *StaticSlotEnvironment,
        lowered: *lir.CheckedPipeline.LoweredProgram,
        module: lir.LIR.LoweringModuleId,
        producer: lir.LIR.ComptimeProducer,
        plan: LirProgram.RootShape,
        value: @import("value.zig").Value,
        callables: NativeRootExport.CallableResolver,
        functions: backend.StaticDataImageFunctionResolver,
    ) FinalizeError!void {
        var next = self.publication.first(module, producer);
        while (next) |slot| {
            const index = @backingInt(slot);
            next = self.publication.next[index];
            const entry = lowered.lir_result.static_data_values.items[index];
            const root = entry.compile_time_root.?;
            if (root.role != .value) continue;
            // The slot may hold a pointer to the value rather than the value:
            // a constant folded into a recursive tag's payload lands in a
            // pointer slot, while the root's own procedure returns the union
            // unboxed. Both describe the same value, and the freezer emits the
            // payload as its own node with a relocation into the slot.
            const destination_layout = entry.layout_idx;
            if (destination_layout != plan.ret_layout) {
                const destination = lowered.lir_result.layouts.getLayout(destination_layout);
                if (destination.tag != .box or destination.getIdx() != plan.ret_layout) {
                    finalizationInvariant("evaluated root requires its explicit instance conversion");
                }
            }
            const exports = try NativeRootExport.freezeRootIntoSlot(self.allocator, &lowered.lir_result, slot, plan, value, callables, destination_layout);
            try self.installExports(lowered, slot, exports, functions);
            try lir.ComptimeValueGuards.completeSuccessfulSlot(&lowered.lir_result, slot);
        }
    }

    /// Takes ownership of the frozen graph on success and failure.
    fn installExports(
        self: *StaticSlotEnvironment,
        lowered: *const lir.CheckedPipeline.LoweredProgram,
        slot: lir.LIR.StaticDataId,
        exports: []static_data_exports.StaticDataExport,
        functions: backend.StaticDataImageFunctionResolver,
    ) FinalizeError!void {
        errdefer static_data_exports.deinitStaticData(self.allocator, exports);
        var image = backend.StaticDataImage.initWithOptions(self.allocator, exports, self.image_options) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => finalizationInvariant("evaluated root export contains an invalid relocation"),
        };
        errdefer image.deinit();
        image.resolveFunctionRelocations(functions) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => finalizationInvariant("evaluated root export omitted an emitted function"),
        };
        const source = image.symbolAddress(exports[0].symbol_name) orelse finalizationInvariant("evaluated root export omitted its root symbol");
        const index = @backingInt(slot);
        const value_layout = lowered.lir_result.static_data_values.items[index].layout_idx;
        const size = lowered.lir_result.layouts.layoutSize(lowered.lir_result.layouts.getLayout(value_layout));
        const destination: [*]u8 = @ptrFromInt(self.addresses[index]);
        @memcpy(destination[0..size], @as([*]const u8, @ptrFromInt(source))[0..size]);
        try self.completed_roots.append(self.allocator, .{ .exports = exports, .image = image });
    }

    fn publishFailureOrigin(self: *StaticSlotEnvironment, module: lir.LIR.LoweringModuleId, producer: lir.LIR.ComptimeProducer, origin: lir.LIR.ComptimeFailureOrigin, lir_result: *const lir.Program.Result) Allocator.Error!void {
        try self.producer_origins.put(self.allocator, .{ .module = lir_result.lowering_modules.items[@backingInt(module)].bytes, .producer = producer }, origin);
    }

    /// `failed_at`, located where its producer failed when it is a read of a
    /// failed value whose producer published its origin.
    fn withProducerOrigin(self: *const StaticSlotEnvironment, failed_at: FailedAt) FailedAt {
        const value = switch (failed_at.meaning orelse return failed_at) {
            .failed_value => |value| value,
            .checked_error, .literal_rejection, .exhaustiveness => return failed_at,
        };
        const origin = self.producer_origins.get(.{ .module = value.module, .producer = value.producer }) orelse return failed_at;
        return .{ .meaning = failed_at.meaning, .region = origin.region, .loc = origin.loc };
    }

    fn publishFailure(
        self: *StaticSlotEnvironment,
        lowered: *const lir.CheckedPipeline.LoweredProgram,
        module: lir.LIR.LoweringModuleId,
        producer: lir.LIR.ComptimeProducer,
        failure: ?SlotFailure,
        functions: backend.StaticDataImageFunctionResolver,
    ) FinalizeError!void {
        const allocator = self.allocator;
        var next = self.publication.first(module, producer);
        while (next) |slot| {
            const index = @backingInt(slot);
            next = self.publication.next[index];
            const entry = lowered.lir_result.static_data_values.items[index];
            const root = entry.compile_time_root.?;
            if (root.role != .failure_message) continue;
            const metadata = root.role.failure_message;
            const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(entry.layout_idx));
            const text = if (failure) |failed| failed.message else "";
            const large = !builtins.str.RocStr.fitsInSmallStr(text.len);
            const exports = try allocator.alloc(static_data_exports.StaticDataExport, if (large) 2 else 1);
            var count: usize = 0;
            var owned = true;
            errdefer if (owned) {
                for (exports[0..count]) |item| {
                    allocator.free(item.symbol_name);
                    allocator.free(item.bytes);
                    allocator.free(item.relocations);
                }
                allocator.free(exports);
            };
            const name = try LirProgram.staticDataSymbolName(allocator, slot);
            const bytes = allocator.alloc(u8, size_align.size) catch |err| {
                allocator.free(name);
                return err;
            };
            @memset(bytes, 0);
            exports[0] = .{ .symbol_name = name, .value_id = slot, .bytes = bytes, .alignment = @intCast(size_align.alignment.toByteUnits()), .is_exported = false };
            count = 1;
            bytes[metadata.failed_offset] = @backingInt(if (failure) |failed| failed.kind.recordKind() else lir.Program.ComptimeFailureKind.none);
            if (large) {
                const backing_name = try LirProgram.staticDataNodeSymbolName(allocator, @backingInt(slot), 1);
                const backing = allocator.alloc(u8, @sizeOf(usize) + text.len) catch |err| {
                    allocator.free(backing_name);
                    return err;
                };
                @memset(backing[0..@sizeOf(usize)], 0);
                @memcpy(backing[@sizeOf(usize)..], text);
                exports[1] = .{ .symbol_name = backing_name, .bytes = backing, .symbol_offset = @sizeOf(usize), .alignment = @alignOf(usize), .is_exported = false };
                count = 2;
                const relocations = try allocator.alloc(static_data_exports.StaticDataRelocation, 1);
                relocations[0] = .{ .offset = metadata.message_offset, .target_symbol_name = backing_name, .target = .{ .data_symbol = @fromBackingInt(@intCast(1)) } };
                exports[0].relocations = relocations;
                std.mem.writeInt(usize, bytes[metadata.message_offset + @offsetOf(builtins.str.RocStr, "length") ..][0..@sizeOf(usize)], text.len, builtin.cpu.arch.endian());
                std.mem.writeInt(usize, bytes[metadata.message_offset + @offsetOf(builtins.str.RocStr, "capacity_or_alloc_ptr") ..][0..@sizeOf(usize)], builtins.str.RocStr.encodeCapacity(text.len), builtin.cpu.arch.endian());
            } else {
                var str = builtins.str.RocStr.fromSliceSmall(text);
                @memcpy(bytes[metadata.message_offset..][0..@sizeOf(builtins.str.RocStr)], std.mem.asBytes(&str));
            }
            // Ownership transfers before installExports, including its error path.
            owned = false;
            try self.installExports(lowered, slot, exports, functions);
        }
    }

    fn freezeCompleted(self: *StaticSlotEnvironment) Allocator.Error!LirProgram.FrozenStaticData {
        var all = std.ArrayList(static_data_exports.StaticDataExport).empty;
        errdefer {
            for (all.items) |item| {
                self.allocator.free(item.symbol_name);
                self.allocator.free(item.bytes);
                for (item.relocations) |relocation| if (relocation.owns_target_symbol_name) self.allocator.free(relocation.target_symbol_name);
                self.allocator.free(item.relocations);
            }
            all.deinit(self.allocator);
        }
        var total = self.materialized.len;
        for (self.completed_roots.items) |root| total += root.exports.len - 1;
        try all.ensureTotalCapacity(self.allocator, total);
        const initial = try static_data_exports.cloneStaticData(self.allocator, self.materialized);
        defer self.allocator.free(initial);
        all.appendSliceAssumeCapacity(initial);
        for (self.completed_roots.items) |completed| {
            const cloned = try static_data_exports.cloneStaticData(self.allocator, completed.exports);
            defer self.allocator.free(cloned);
            const slot = cloned[0].value_id.?;
            const index = self.export_positions[@backingInt(slot)] orelse finalizationInvariant("completed root had no allocated value slot");
            const nested_start = all.items.len;
            for (cloned) |*item| for (@constCast(item.relocations)) |*relocation| {
                if (relocation.target == .data_symbol) {
                    const local = @backingInt(relocation.target.data_symbol);
                    relocation.target.data_symbol = @fromBackingInt(@intCast(if (local == 0) index else nested_start + local - 1));
                }
            };
            const old = all.items[index];
            self.allocator.free(old.symbol_name);
            self.allocator.free(old.bytes);
            for (old.relocations) |relocation| if (relocation.owns_target_symbol_name) self.allocator.free(relocation.target_symbol_name);
            self.allocator.free(old.relocations);
            all.items[index] = cloned[0];
            all.appendSliceAssumeCapacity(cloned[1..]);
        }
        return .{ .allocator = self.allocator, .exports = try static_data_exports.mergeIdenticalNodes(self.allocator, try all.toOwnedSlice(self.allocator)) };
    }

    fn deinit(self: *StaticSlotEnvironment) void {
        for (self.completed_roots.items) |*root| root.deinit(self.allocator);
        self.completed_roots.deinit(self.allocator);
        self.producer_origins.deinit(self.allocator);
        self.publication.deinit(self.allocator);
        self.allocator.free(self.export_positions);
        self.allocator.free(self.addresses);
        self.image.deinit();
        static_data_exports.deinitStaticData(self.allocator, self.materialized);
        self.* = undefined;
    }
};

/// The procedures the compile-time evaluator compiles: the closures of the
/// roots it runs, and of every procedure of the program that the
/// object-cache code it splices calls, to a fixed point. A spliced entry's
/// call to a procedure the program has binds to the program's own code
/// (`ProcArtifact.splice`), so that procedure is compiled too, and the
/// object-cache entries its closure reaches are spliced in turn.
fn evaluationDemand(
    allocator: Allocator,
    result: *LirProgram.Result,
    roots: []const lir.LIR.LirProcSpecId,
    materialized: []LirProgram.StaticDataExport,
    splice_source: ?backend.dev.SpliceSource,
) Allocator.Error![]lir.LIR.LirProcSpecId {
    const source = splice_source orelse return lir.ReachableProcs.collectProcDemand(allocator, result, roots, materialized);
    const proc_specs = result.store.getProcSpecs();
    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        try procs_by_identity.put(proc.identity, @fromBackingInt(@intCast(@as(u32, @intCast(index)))));
    }
    var demand_roots = std.ArrayList(lir.LIR.LirProcSpecId).empty;
    defer demand_roots.deinit(allocator);
    try demand_roots.appendSlice(allocator, roots);
    var added = collections.DenseMap(lir.LIR.LirProcSpecId, void).init(allocator);
    defer added.deinit();
    for (roots) |root| try added.put(root, {});
    const Visit = struct { set: *const backend.dev.ProcArtifact.Set, index: u32 };
    var visited = std.AutoHashMapUnmanaged(Visit, void).empty;
    defer visited.deinit(allocator);
    var stack = std.ArrayList(Visit).empty;
    defer stack.deinit(allocator);
    while (true) {
        const demand = try lir.ReachableProcs.collectProcDemand(allocator, result, demand_roots.items, materialized);
        var grew = false;
        for (demand) |proc_id| {
            const proc = proc_specs[@backingInt(proc_id)];
            if (!proc.external or proc.c_abi_entry) continue;
            const key = proc.cache_key orelse continue;
            const located = source.find(source.context, key) orelse continue;
            try stack.append(allocator, .{ .set = located.set, .index = located.index });
        }
        // Every artifact the spliced closures place, walked once, as
        // `ProcArtifact.splice` places them: set-local references, and
        // symbolic references to code the program does not have.
        while (stack.pop()) |visit| {
            if ((try visited.getOrPut(allocator, visit)).found_existing) continue;
            const artifact = visit.set.artifacts[visit.index];
            for (artifact.refs) |ref| try stack.append(allocator, .{ .set = visit.set, .index = ref.target });
            for (artifact.symbolic_refs) |ref| {
                switch (ref.target) {
                    .proc => |identity| if (procs_by_identity.get(identity)) |proc_id| {
                        if ((try added.getOrPut(proc_id)).found_existing) continue;
                        try demand_roots.append(allocator, proc_id);
                        grew = true;
                        continue;
                    },
                    .boxy_thunk, .rc_helper => {},
                }
                if (visit.set.definitionOf(ref.target)) |definition| try stack.append(allocator, .{ .set = visit.set, .index = definition });
            }
        }
        if (!grew) return demand;
        allocator.free(demand);
    }
}

const DevProgram = struct {
    allocator: Allocator,
    static_strings: backend.StaticStringData.Table,
    slots: StaticSlotEnvironment,
    codegen: backend.HostLirCodeGen,
    /// The object-cache entries spliced into the image and the names the
    /// image binds for them.
    splice: backend.dev.HostSplice,
    executable: backend.ExecutableMemory,
    entry_offsets: collections.DenseMap(lir.LIR.LirProcSpecId, usize),
    artifacts: ?NativeProcCompiler.Retained,
    /// The bytes the program's erased callables reserve before their
    /// captures (`LirProgram.Result.erased_capture_prefix`).
    capture_prefix: u32,

    /// How a program's procedures are compiled: as a dev build's object
    /// code, linked into this process, or, for a Boxy program, whose code no
    /// build shares and whose runtime the evaluator installs itself, as
    /// in-process code.
    const Code = enum { dev_object, boxy_in_process };

    fn init(allocator: Allocator, modules: lir.CheckedPipeline.CheckedModuleSet, lowered: *lir.CheckedPipeline.LoweredProgram, code: Code, options: Options) FinalizeError!DevProgram {
        const static_data_started_ns = if (options.timing) |timing| timing.start() else 0;
        var static_strings = try backend.StaticStringData.build(allocator, &lowered.lir_result.store, backend.dev.LirCodeGenMod.host_lir_codegen_target);
        errdefer static_strings.deinit();

        // The program's static data is placed in the image, after its code,
        // where the code reaches it as object code reaches data.
        const materialized = try StaticSlotEnvironment.materialize(allocator, modules, lowered, backend.dev.LirCodeGenMod.host_lir_codegen_target);
        var materialized_owned = true;
        errdefer if (materialized_owned) static_data_exports.deinitStaticData(allocator, materialized);
        var static_layout = backend.StaticDataImage.Layout.plan(allocator, materialized, .{}) catch |err| return StaticSlotEnvironment.staticImageError(err);
        var layout_owned = true;
        errdefer if (layout_owned) static_layout.deinit(allocator);
        if (options.timing) |timing| timing.finish(static_data_started_ns, .static_data);

        const code_generation_started_ns = if (options.timing) |timing| timing.start() else 0;
        // Compile-time code runs in this process, so it is generated for the CPU
        // of the machine compiling, not for the CPU the program is compiled for.
        var codegen = try backend.HostLirCodeGen.initWithBoxyMetadata(
            allocator,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            static_strings.view(),
            lowered.lir_result.boxy_erased_arg_desc_offsets.items,
            lowered.lir_result.boxy_erased_arg_desc_params.items,
            lowered.lir_result.boxy_worker_procs.items,
            roc_target.host_cpu.level(),
        );
        errdefer codegen.deinit();
        // A dev object program's procedures compile exactly as a dev build's
        // object code does, so the code evaluation runs is the code a build
        // links; this image links it into the compiler's own process.
        codegen.generation_mode = switch (code) {
            .dev_object => .object_file,
            .boxy_in_process => .native_execution,
        };
        try codegen.setStaticDataSymbols(materialized);
        codegen.enable_hot_reload = lowered.lir_result.erased_capture_prefix != 0;
        var native_fns = boxyNativeFnTable();
        codegen.boxy_native_fns = &native_fns;
        const const_root_count = lowered.lir_result.const_roots.items.len;
        const evaluation_roots = try allocator.alloc(lir.LIR.LirProcSpecId, const_root_count + lowered.lir_result.literal_roots.items.len);
        defer allocator.free(evaluation_roots);
        for (lowered.lir_result.const_roots.items, evaluation_roots[0..const_root_count]) |root, *proc| proc.* = root.proc;
        for (lowered.lir_result.literal_roots.items, evaluation_roots[const_root_count..]) |root, *proc| proc.* = root.proc;
        const evaluation_demand = try evaluationDemand(allocator, &lowered.lir_result, evaluation_roots, materialized, if (options.object_cache) |cache| cache.splice_source else null);
        defer allocator.free(evaluation_demand);
        var splice = backend.dev.HostSplice.init(allocator);
        errdefer splice.deinit();
        splice.reserveStaticData(&static_layout);
        if (options.object_cache) |cache| try splice.spliceExternal(&codegen, evaluation_demand, cache.splice_source);
        const static_rc_helpers = try static_data_exports.collectRequiredRcHelpers(allocator, materialized);
        defer allocator.free(static_rc_helpers);
        var artifacts = try compileProcedures(allocator, &codegen, evaluation_demand, static_rc_helpers, options);
        errdefer artifacts.deinit();
        // Prepared fragments own their carried strings. The splice borrows
        // those bytes until image placement completes.
        const carried_data = try artifacts.dataItems(allocator);
        defer allocator.free(carried_data);
        try splice.addDataItems(carried_data);

        // The evaluator calls each root through its own in-process entry
        // wrapper.
        codegen.generation_mode = .native_execution;

        var entry_offsets = collections.DenseMap(lir.LIR.LirProcSpecId, usize).init(allocator);
        errdefer entry_offsets.deinit();
        for (lowered.lir_result.const_roots.items, 0..) |root, index| {
            if (entry_offsets.get(root.proc) != null) continue;
            var name_buf: [64]u8 = undefined;
            const symbol_name = std.fmt.bufPrint(&name_buf, "roc_ctfe_root_{d}", .{index}) catch unreachable;
            const entrypoint = try codegen.generateEntrypointWrapper(symbol_name, root.proc, &.{}, root.ret_layout);
            try entry_offsets.put(root.proc, entrypoint.offset);
        }
        for (lowered.lir_result.literal_roots.items, 0..) |root, index| {
            if (entry_offsets.get(root.proc) != null) continue;
            var name_buf: [64]u8 = undefined;
            const symbol_name = std.fmt.bufPrint(&name_buf, "roc_ctfe_literal_root_{d}", .{index}) catch unreachable;
            const entrypoint = try codegen.generateEntrypointWrapper(symbol_name, root.proc, &.{}, root.ret_layout);
            try entry_offsets.put(root.proc, entrypoint.offset);
        }
        codegen.boxy_native_fns = null;
        try splice.generateHostedStubs(&codegen, &native_fns);
        try codegen.finishImage();
        var executable = splice.link(&codegen, &native_fns) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.MappingFailed => return error.MmapFailed,
            error.UnresolvedSymbol => spliceInvariant(&splice, "compile-time code names a symbol the compile-time evaluator cannot bind"),
            error.InvalidRelocation => spliceInvariant(&splice, "compile-time code carries a relocation the compile-time evaluator cannot patch"),
        };
        errdefer executable.deinit();
        layout_owned = false;
        const static_image = static_layout.place(allocator, splice.staticDataMemory(&executable)) catch |err| return StaticSlotEnvironment.staticImageError(err);
        materialized_owned = false;
        var slots = try StaticSlotEnvironment.initInImage(allocator, lowered, materialized, static_image, .{});
        errdefer slots.deinit();

        const StaticFunctionResolver = struct {
            codegen: *const backend.HostLirCodeGen,
            executable: *const backend.ExecutableMemory,

            fn resolve(raw: ?*anyopaque, relocation: backend.StaticDataRelocation) ?usize {
                const self: *@This() = @ptrCast(@alignCast(raw.?));
                if (relocation.rc_helper) |helper| {
                    const offset = self.codegen.compiledStaticDataRcHelperOffset(helper) orelse return null;
                    return @intFromPtr(self.executable.codePtr() + offset);
                }
                if (relocation.callable_capture_offset == null) return null;
                const proc_id = relocation.procedure orelse return null;
                const compiled = self.codegen.compiledProcSymbol(proc_id) orelse return null;
                return @intFromPtr(self.executable.codePtr() + compiled.code_start);
            }
        };
        var static_function_resolver = StaticFunctionResolver{
            .codegen = &codegen,
            .executable = &executable,
        };
        slots.image.resolveFunctionRelocations(.{
            .context = @ptrCast(&static_function_resolver),
            .resolve = StaticFunctionResolver.resolve,
        }) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DuplicateStaticDataSymbol,
            error.InvalidStaticDataAlignment,
            error.InvalidStaticDataRelocation,
            error.MissingStaticDataSymbol,
            error.UnresolvedStaticFunction,
            => finalizationInvariant("compile-time static data referenced an unresolved generated function"),
        };
        if (options.timing) |timing| timing.finish(code_generation_started_ns, .code_generation);

        return .{
            .allocator = allocator,
            .static_strings = static_strings,
            .slots = slots,
            .codegen = codegen,
            .splice = splice,
            .executable = executable,
            .entry_offsets = entry_offsets,
            .artifacts = artifacts,
            .capture_prefix = lowered.lir_result.erased_capture_prefix,
        };
    }

    fn compileProcedures(allocator: Allocator, codegen: *backend.HostLirCodeGen, demand: []const lir.LIR.LirProcSpecId, static_helpers: []const @import("layout").RcHelperKey, options: Options) Allocator.Error!NativeProcCompiler.Retained {
        var metrics: NativeProcCompiler.Metrics = .{};
        const artifacts = try NativeProcCompiler.run(backend.HostLirCodeGen, allocator, codegen, demand, .{
            .target = backend.dev.LirCodeGenMod.host_lir_codegen_target,
            .executor = options.post_check_executor,
            .static_helpers = static_helpers,
            .metrics_out = &metrics,
        });
        if (options.timing) |timing| timing.addNativeEmission(metrics);
        return artifacts;
    }

    fn resolveCallable(raw: ?*anyopaque, data_ptr: [*]u8) error{RuntimeError}!NativeRootExport.CallableResolution {
        const self: *DevProgram = @ptrCast(@alignCast(raw.?));
        const payload = builtins.erased_callable.payloadPtr(data_ptr);
        const address = @intFromPtr(payload.callable_fn_ptr);
        const base_address = @intFromPtr(self.executable.codePtr());
        for (self.codegen.store.getProcSpecs(), 0..) |_, index| {
            const proc: lir.LIR.LirProcSpecId = @fromBackingInt(@intCast(index));
            const symbol = self.codegen.compiledProcSymbol(proc) orelse continue;
            // The callable's captures follow the program's capture prefix.
            if (base_address + symbol.code_start == address) return .{
                .proc = proc,
                .capture_ptr = builtins.erased_callable.capturePtr(data_ptr) + self.capture_prefix,
            };
        }
        finalizationInvariant("native callable omitted its emitted procedure identity");
    }

    fn resolveFrozenFunction(raw: ?*anyopaque, relocation: backend.StaticDataRelocation) ?usize {
        const self: *DevProgram = @ptrCast(@alignCast(raw.?));
        const base_address = @intFromPtr(self.executable.codePtr());
        if (relocation.rc_helper) |helper| return base_address + (self.codegen.compiledStaticDataRcHelperOffset(helper) orelse return null);
        const proc = relocation.procedure orelse return null;
        return base_address + (self.codegen.compiledProcSymbol(proc) orelse return null).code_start;
    }

    fn publishRoot(self: *DevProgram, lowered: *lir.CheckedPipeline.LoweredProgram, module: lir.LIR.LoweringModuleId, producer: lir.LIR.ComptimeProducer, shape: LirProgram.RootShape, value: @import("value.zig").Value) FinalizeError!void {
        try self.publishRootWithRuntime(lowered, module, producer, shape, value, null);
    }
    fn publishRootWithRuntime(self: *DevProgram, lowered: *lir.CheckedPipeline.LoweredProgram, module: lir.LIR.LoweringModuleId, producer: lir.LIR.ComptimeProducer, shape: LirProgram.RootShape, value: @import("value.zig").Value, runtime: ?*const @import("boxy_runtime.zig").BoxyRuntime) FinalizeError!void {
        try self.slots.publishRoot(lowered, module, producer, shape, value, .{ .context = self, .resolve = resolveCallable, .runtime = runtime }, .{ .context = self, .resolve = resolveFrozenFunction });
    }
    fn publishFailure(self: *DevProgram, lowered: *const lir.CheckedPipeline.LoweredProgram, module: lir.LIR.LoweringModuleId, producer: lir.LIR.ComptimeProducer, failure: ?SlotFailure) FinalizeError!void {
        try self.slots.publishFailure(lowered, module, producer, failure, .{ .context = self, .resolve = resolveFrozenFunction });
    }
    /// Where a failure the host located in this program's generated code
    /// happened. It is reported at the innermost statement with a source
    /// region among the faulting instruction's and those of the generated
    /// calls enclosing it, with the meaning of the call it came out of.
    fn failedAt(self: *const DevProgram, point: ?CompileTimeHost.FailurePoint) FailedAt {
        const known = point orelse return .{};
        const code_base = @intFromPtr(self.executable.codePtr());
        var failed_at = FailedAt{};
        if (known.fault == null and known.calls.len != 0) {
            failed_at.meaning = self.codegen.failureMeaningAt(known.calls[0] - code_base);
        }
        const located = if (known.fault) |pc| self.statementAt(pc - code_base) else null;
        const statement = located orelse for (known.calls) |address| {
            // A return address follows its call instruction.
            if (self.statementAt(address - code_base - 1)) |line| break line;
        } else null;
        if (statement) |line| {
            failed_at.region = line.region;
            failed_at.loc = line.loc;
        }
        return self.slots.withProducerOrigin(failed_at);
    }

    fn statementAt(self: *const DevProgram, offset: usize) ?backend.dev.LirCodeGenMod.LineEntry {
        const line = self.codegen.lineEntryAt(offset) orelse return null;
        if (line.region.isEmpty() or !line.loc.hasLocation()) return null;
        return line;
    }

    fn freezeCompleted(self: *DevProgram) Allocator.Error!LirProgram.FrozenStaticData {
        return self.slots.freezeCompleted();
    }

    /// The image's code as object-cache artifacts, carrying the completed
    /// values it reads. Its procedures are a dev build's object code, so a
    /// build splices them as its own.
    fn extractArtifacts(self: *DevProgram, lowered: *const lir.CheckedPipeline.LoweredProgram) Allocator.Error!backend.dev.ProcArtifact.Set {
        const frozen = lowered.frozen_static_data orelse finalizationInvariant("compile-time stage froze no completed values");
        return backend.dev.ProcArtifact.extract(
            backend.HostLirCodeGen,
            self.allocator,
            &self.codegen,
            lowered.lir_result.store.getProcSpecs(),
            &lowered.lir_result.layouts,
            self.static_strings.exports,
            frozen.exports,
            self.splice.data.items,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.NestedCodeRegion, error.UncoveredCode, error.DanglingReference, error.UnsupportedRelocation => finalizationInvariant("compile-time image could not be lifted into artifacts"),
        };
    }

    fn deinit(self: *DevProgram) void {
        self.entry_offsets.deinit();
        self.executable.deinit();
        self.splice.deinit();
        self.codegen.deinit();
        if (self.artifacts) |*artifacts| artifacts.deinit();
        self.slots.deinit();
        self.static_strings.deinit();
        self.* = undefined;
    }
};

/// Execute borrowed LIR; the compilation owner retains the lowered program.
fn evalProgramRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    owners: *const ModuleOwners,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    const_roots: []const LirProgram.ConstRootPlan,
    program: anytype,
) FinalizeError!bool {
    if (@TypeOf(program) == *DevProgram) return evalDevProgramRoots(allocator, module, requests, root_ids, state, problem_store, owners, options, lowered, const_roots, program);
    if (@TypeOf(program) == *InterpreterProgram) return evalInterpreterProgramRoots(allocator, module, requests, root_ids, state, problem_store, owners, options, lowered, const_roots, program);
    @compileError("compile-time evaluation owner must be explicit native or interpreter program");
}

fn evalDevProgramRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    owners: *const ModuleOwners,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    const_roots: []const LirProgram.ConstRootPlan,
    native: *DevProgram,
) FinalizeError!bool {
    var host_allocator_impl = ThreadSafeAllocator.init(allocator);
    const host_allocator = host_allocator_impl.allocator();

    var jobs = try allocator.alloc(DevRootJob, const_roots.len);
    var jobs_len: usize = 0;
    {
        errdefer {
            for (jobs[0..jobs_len]) |*job| job.deinit(allocator);
            allocator.free(jobs);
        }

        for (const_roots, 0..) |root, i| {
            if (!std.meta.eql(root.request, requests[i])) {
                finalizationInvariant("LIR lowering changed compile-time root request order");
            }

            const entry_offset = native.entry_offsets.get(root.proc) orelse
                finalizationInvariant("native program omitted a requested compile-time entry wrapper");

            const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(root.ret_layout));
            const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(size_align.size, 1));
            errdefer allocator.free(ret_buf);
            @memset(ret_buf, 0);

            const root_id = root_ids[i];
            const compile_time_root = module.compile_time_roots.root(root_id);
            const label = try devRootLabel(allocator, module, compile_time_root);
            errdefer {
                var owned = label;
                owned.deinit(allocator);
            }

            jobs[i] = .{
                .root = root,
                .root_id = root_id,
                .compile_time_root = compile_time_root,
                .entry_offset = entry_offset,
                .ret_buf = ret_buf,
                .host = CompileTimeHost.init(host_allocator),
                .label = label,
            };
            jobs[i].host.setGeneratedCode(native.executable.generatedCode());
            jobs[i].host.timing_io = if (options.timing) |timing| timing.std_io else null;
            jobs_len += 1;
        }
    }
    defer {
        for (jobs[0..jobs_len]) |*job| job.deinit(allocator);
        allocator.free(jobs);
    }

    const boxy_tables = Interpreter.BoxyTables.fromResult(&lowered.lir_result);
    const boxy_global_installed = jobs_len != 0 and boxy_tables.needsRuntimeForStore(&lowered.lir_result.store);
    const selected_runtime = if (boxy_global_installed)
        try boxy_abi.createRuntimeFromStores(allocator, &lowered.lir_result.store, &lowered.lir_result.layouts, boxy_tables, jobs[0].host.ops())
    else
        null;
    defer if (selected_runtime) |runtime| boxy_abi.deinitRuntime(runtime);
    const previous_runtime = if (selected_runtime) |runtime| boxy_abi.swapActiveRuntime(runtime) else null;
    defer if (selected_runtime != null) {
        _ = boxy_abi.swapActiveRuntime(previous_runtime);
    };

    const execution_started_ns = if (options.timing) |timing| timing.start() else 0;
    var progress = DevProgressReporter.init(options, jobs[0..jobs_len]);
    defer progress.deinit();
    try progress.start();
    var run_context = DevRunContext{
        .executable = &native.executable,
        .jobs = jobs[0..jobs_len],
        .std_io = options.std_io,
        .progress_reporter = if (progress.thread == null) null else &progress,
        .boxy_global_installed = boxy_global_installed,
    };
    const max_threads = if (boxy_global_installed)
        1
    else if (options.max_threads == 0)
        0
    else
        @max(options.max_threads, 1);
    if (max_threads == 1) {
        for (0..jobs_len) |index| devRootWorker(host_allocator, &run_context, index);
    } else if (options.post_check_executor) |executor| {
        // The coordinator's persistent lanes run the batch, so no threads are
        // spawned per batch.
        const RootTask = struct {
            context: *DevRunContext,
            host_allocator: Allocator,
            index: usize,
            fn run(ptr: *anyopaque, _: base.post_check_task_executor.Worker) ?*anyopaque {
                const self: *@This() = @ptrCast(@alignCast(ptr));
                devRootWorker(self.host_allocator, self.context, self.index);
                return null;
            }
        };
        const root_tasks = try allocator.alloc(RootTask, jobs_len);
        defer allocator.free(root_tasks);
        const tasks = try allocator.alloc(base.post_check_task_executor.Task, jobs_len);
        defer allocator.free(tasks);
        const completions = try allocator.alloc(base.post_check_task_executor.Completion, jobs_len);
        defer allocator.free(completions);
        for (root_tasks, tasks, 0..) |*root_task, *task, index| {
            root_task.* = .{ .context = &run_context, .host_allocator = host_allocator, .index = index };
            task.* = .{ .id = index, .context = root_task, .run = RootTask.run };
        }
        try executor.run(tasks, completions);
    } else {
        try base.parallel.process(
            DevRunContext,
            &run_context,
            devRootWorker,
            host_allocator,
            jobs_len,
            .{
                .max_threads = max_threads,
                .use_per_thread_arenas = false,
            },
        );
    }
    progress.finish();
    if (options.timing) |timing| {
        timing.finishExecution(execution_started_ns);
    }

    if (run_context.had_oom.load(.acquire)) return error.OutOfMemory;

    const DevResolver = struct {
        codegen: *const backend.HostLirCodeGen,
        store: *const lir.LirStore,
        executable: *const backend.ExecutableMemory,
        capture_prefix: u32,

        fn resolve(raw: ?*anyopaque, data_ptr: [*]u8) error{RuntimeError}!ConstStoreWriter.ErasedCallableResolution {
            const self: *@This() = @ptrCast(@alignCast(raw.?));
            const payload = builtins.erased_callable.payloadPtr(data_ptr);
            const runtime_addr = @intFromPtr(payload.callable_fn_ptr);
            for (self.store.getProcSpecs(), 0..) |_, proc_index| {
                const proc_id: lir.LIR.LirProcSpecId = @fromBackingInt(@intCast(proc_index));
                const symbol = self.codegen.compiledProcSymbol(proc_id) orelse continue;
                const compiled_addr = @intFromPtr(self.executable.codePtr() + symbol.code_start);
                if (compiled_addr == runtime_addr) {
                    // The callable's captures follow the program's capture
                    // prefix.
                    return .{
                        .proc = proc_id,
                        .capture_ptr = builtins.erased_callable.capturePtr(data_ptr) + self.capture_prefix,
                    };
                }
            }
            finalizationInvariant("dev backend erased callable result did not map to a compiled proc");
        }
    };

    var resolver = DevResolver{
        .codegen = &native.codegen,
        .store = &lowered.lir_result.store,
        .executable = &native.executable,
        .capture_prefix = lowered.lir_result.erased_capture_prefix,
    };

    const store_results_started_ns = if (options.timing) |timing| timing.start() else 0;
    var writer = ConstStoreWriter.Writer.init(allocator, module, &lowered.lir_result);
    defer writer.deinit();
    writer.setErasedCallableResolver(.{
        .context = @ptrCast(&resolver),
        .resolve = DevResolver.resolve,
    });

    var had_problem = false;
    for (jobs[0..jobs_len]) |*job| {
        if (options.retry) |retry| {
            if (job.result == .crashed and retry.holds(native.failedAt(job.host.failure).meaning)) continue;
        }
        const payload: checked.CompileTimeRootPayload = switch (job.result) {
            .pending => finalizationInvariant("dev backend compile-time root was not evaluated"),
            .host_oom => return error.OutOfMemory,
            .crashed => crashed: {
                const message = job.host.crashMessage() orelse "Roc crashed";
                const failed_at = native.failedAt(job.host.failure);
                if (exhaustivenessFailure(failed_at.meaning)) |failure| break :crashed try devComptimeExhaustivenessRootPayload(
                    problem_store,
                    owners,
                    job.root.owner,
                    module,
                    job.compile_time_root,
                    &lowered.lir_result,
                    job.root.proc,
                    failure,
                    message,
                    job.root.request,
                    &had_problem,
                );
                break :crashed try devCrashedRootPayload(
                    problem_store,
                    module,
                    job.compile_time_root,
                    job.root.request,
                    message,
                    failed_at,
                    owners,
                    &lowered.lir_result,
                    &had_problem,
                );
            },
            .success => try storeRootValue(&writer, job.root, job.compile_time_root, .{ .ptr = job.ret_buf.ptr }),
        };

        const failure_message: ?[]const u8 = switch (job.result) {
            .success => null,
            .crashed => job.host.crashMessage() orelse "Roc crashed",
            .pending, .host_oom => unreachable,
        };
        if (options.publish_shared_slots and failure_message == null) try native.publishRoot(lowered, job.root.owner, .{ .checked = job.root_id }, job.root.shape(), .{ .ptr = job.ret_buf.ptr });
        if (options.publish_shared_slots and failure_message != null) {
            const failed_at = native.failedAt(job.host.failure);
            try native.slots.publishFailureOrigin(job.root.owner, .{ .checked = job.root_id }, .{ .loc = failed_at.loc, .region = failed_at.region }, &lowered.lir_result);
        }
        if (options.publish_shared_slots) try native.publishFailure(lowered, job.root.owner, .{ .checked = job.root_id }, if (failure_message) |message| .{ .message = message, .kind = RootFailure.ofPayload(module, payload) } else null);

        if (try reportDevHostEvents(allocator, options, problem_store, module, job.compile_time_root, native, &lowered.lir_result.store, job.host.events.items)) {
            had_problem = true;
        }

        try completeRoot(module, &writer, state, job.root, job.root_id, job.compile_time_root, payload);
    }
    if (options.timing) |timing| timing.finish(store_results_started_ns, .store_results);

    return had_problem;
}

fn devRootWorker(_: Allocator, context: *DevRunContext, item_id: usize) void {
    const job = &context.jobs[item_id];
    job.host.resetForRun();
    if (context.boxy_global_installed) {
        boxy_abi.setGlobalRocOps(job.host.ops());
    }
    job.start_ms.store(if (context.std_io) |io| nowMs(io) else 0, .release);
    job.last_progress_ms.store(0, .release);
    job.progress.store(@backingInt(DevRootProgressState.running), .release);
    if (context.progress_reporter) |progress| progress.rootStarted();

    job.host.entry_frame = @frameAddress();
    var crash_boundary = job.host.enterCrashBoundary();
    const entered = builtins.in_process_host.enter(job.host.ops(), null);
    if (crash_boundary.set() == 0) {
        context.executable.callRocABIAt(
            job.entry_offset,
            @ptrCast(job.ret_buf.ptr),
            null,
        );
    }
    builtins.in_process_host.leave(entered);
    crash_boundary.deinit();

    job.result = switch (job.host.termination) {
        .returned => .success,
        .crashed => .crashed,
        .host_oom => blk: {
            context.had_oom.store(true, .release);
            break :blk .host_oom;
        },
    };
    job.progress.store(@backingInt(DevRootProgressState.done), .release);
}

fn nowNs(io: std.Io) i64 {
    return @intCast(@max(0, std.Io.Timestamp.now(io, .awake).nanoseconds));
}

fn nowMs(io: std.Io) ProgressMillis {
    const ns: u64 = @intCast(nowNs(io));
    return @truncate(ns / std.time.ns_per_ms);
}

fn elapsedMs(now: ProgressMillis, since: ProgressMillis) ?ProgressMillis {
    if (since == 0 or now < since) return null;
    return now - since;
}

fn msUntilElapsed(now: ProgressMillis, since: ProgressMillis, target: ProgressMillis) ProgressMillis {
    if (since == 0) return target;
    if (now < since) return saturatingAddMs(since - now, target);
    const elapsed = now - since;
    return if (elapsed < target) target - elapsed else 0;
}

fn saturatingAddMs(a: ProgressMillis, b: ProgressMillis) ProgressMillis {
    return std.math.add(ProgressMillis, a, b) catch std.math.maxInt(ProgressMillis);
}

fn progressDurationMs(ns: u64) ProgressMillis {
    const ms = ns / std.time.ns_per_ms;
    return @intCast(@min(ms, std.math.maxInt(ProgressMillis)));
}

fn spinnerByte(tick: usize) u8 {
    const frames = "|/-\\";
    return frames[tick % frames.len];
}

fn devRootLabel(
    allocator: Allocator,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
) Allocator.Error!DevRootLabel {
    const env = module.moduleEnvConst();
    const region = devRootSourceRegion(module, root);
    const info = env.calcRegionInfo(region);
    const source = env.getSourceAll();
    const line_starts = env.getLineStartsAll();
    const raw_line = base.RegionInfo.getLineText(source, line_starts, info.start_line_idx, info.start_line_idx);
    const snippet = try truncateSnippet(allocator, raw_line, 96);
    return .{
        .module_name = env.module_name,
        .snippet = snippet,
        .line = info.start_line_idx + 1,
        .column = info.start_col_idx + 1,
    };
}

fn devRootSourceRegion(
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
) base.Region {
    const env = module.moduleEnvConst();
    return switch (root.source) {
        .def => |def_idx| blk: {
            const def = env.store.getDef(def_idx);
            break :blk env.store.getPatternRegion(def.pattern);
        },
        .expr => |expr_idx| env.store.getExprRegion(expr_idx),
        .statement => |stmt_idx| env.store.getStatementRegion(stmt_idx),
        .hoisted => |hoisted| env.store.getExprRegion(hoisted.expr),
        .required_binding => module.checked_bodies.expr(root.expr).source_region,
    };
}

fn truncateSnippet(allocator: Allocator, bytes: []const u8, max_bytes: usize) Allocator.Error![]u8 {
    const ellipsis = "…";
    if (bytes.len <= max_bytes) return try allocator.dupe(u8, bytes);
    if (max_bytes <= ellipsis.len) return try allocator.dupe(u8, ellipsis);
    const prefix_len = validUtf8PrefixLen(bytes, max_bytes - ellipsis.len);
    return try std.fmt.allocPrint(allocator, "{s}{s}", .{ bytes[0..prefix_len], ellipsis });
}

fn validUtf8PrefixLen(bytes: []const u8, max_bytes: usize) usize {
    var end = @min(bytes.len, max_bytes);
    while (end > 0 and !std.unicode.utf8ValidateSlice(bytes[0..end])) {
        end -= 1;
    }
    return end;
}

fn devComptimeExhaustivenessRootPayload(
    problem_store: ?*check.problem.Store,
    owners: *const ModuleOwners,
    root_owner: lir.LIR.LoweringModuleId,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    failure: ExhaustivenessFailure,
    message: []const u8,
    request: checked.RootRequest,
    had_problem: *bool,
) FinalizeError!checked.CompileTimeRootPayload {
    if (request.kind == .compile_time_constant and problem_store == null) {
        return try unreportedFailedRootPayload(module, root, message, .crash);
    }

    _ = problem_store orelse {
        finalizationInvariant("compile-time root reached an empirical exhaustiveness failure without a checking problem store");
    };
    try appendCompileTimeExhaustivenessProblem(owners, root_owner, root, lir_result, root_proc, failure);
    had_problem.* = true;
    return try failedRootPayload(module, root, message, .crash);
}

fn devCrashedRootPayload(
    problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    request: checked.RootRequest,
    message: []const u8,
    failed_at: FailedAt,
    owners: *const ModuleOwners,
    lir_result: *const lir.Program.Result,
    had_problem: *bool,
) FinalizeError!checked.CompileTimeRootPayload {
    const lir_store = &lir_result.store;
    switch (try reportEmbeddedFailure(problem_store, module, devRootSourceRegion(module, root), owners, lir_result, failed_at, message)) {
        .reported => {
            had_problem.* = true;
            return try failedRootPayload(module, root, message, RootFailure.of(failed_at.meaning));
        },
        .reported_elsewhere => return try failedRootPayload(module, root, message, RootFailure.of(failed_at.meaning)),
        .none => {},
    }
    if (root.guarded) return try failedRootPayload(module, root, message, RootFailure.of(failed_at.meaning));
    if (request.kind == .compile_time_constant and problem_store == null) {
        return try unreportedFailedRootPayload(module, root, message, .crash);
    }
    const store = problem_store orelse {
        finalizationInvariant("compile-time root crashed without a checking problem store");
    };
    const message_idx = try store.putExtraString(message);
    const site = comptimeFailureSiteFromLoc(
        module,
        lir_store,
        devRootSourceRegion(module, root),
        failed_at.region,
        failed_at.loc,
    );
    _ = try store.appendProblem(.{ .comptime_crash = .{
        .message = message_idx,
        .region = site.region,
        .origin = try comptimeFailureOrigin(store, site),
    } });
    had_problem.* = true;
    return try failedRootPayload(module, root, message, RootFailure.of(failed_at.meaning));
}

fn reportDevHostEvents(
    allocator: Allocator,
    options: Options,
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    native: *const DevProgram,
    lir_store: *const lir.LirStore,
    events: []const CompileTimeHost.HostEvent,
) FinalizeError!bool {
    var had_problem = false;
    const root_region = module.checked_bodies.expr(root.expr).source_region;
    for (events) |event| {
        switch (event) {
            .dbg => |msg| try reportDebugMessage(allocator, options, module.key, root, msg),
            .expect_failed => |failure| if (maybe_problem_store) |store| {
                const message_idx = try store.putExtraString(failure.message);
                const failed_at = native.failedAt(failure.point);
                const site = comptimeFailureSiteFromLoc(
                    module,
                    lir_store,
                    root_region,
                    failed_at.region,
                    failed_at.loc,
                );
                _ = try store.appendProblem(.{ .comptime_expect_failed = .{
                    .message = message_idx,
                    .region = site.region,
                    .origin = try comptimeFailureOrigin(store, site),
                } });
                had_problem = true;
            },
            .crashed => {},
        }
    }
    return had_problem;
}

fn reportInterpreterDebugMessages(
    allocator: Allocator,
    options: Options,
    module: checked.ModuleId,
    root: checked.CompileTimeRoot,
    messages: []const []const u8,
) Allocator.Error!void {
    for (messages) |msg| try reportDebugMessage(allocator, options, module, root, msg);
}

fn reportDebugMessage(allocator: Allocator, options: Options, module: checked.ModuleId, root: checked.CompileTimeRoot, message: []const u8) Allocator.Error!void {
    if (options.debug_events) |events| return events.append(module, root, message);
    try emitDebugMessage(allocator, options, root.kind == .repl_expr, message);
}

fn emitDebugMessage(allocator: Allocator, options: Options, is_repl: bool, message: []const u8) Allocator.Error!void {
    if (is_repl) {
        if (options.event_callback) |callback| callback.notify(callback.context, .{ .dbg = message });
    }
    if (options.stderr) |writer| {
        const line = try std.fmt.allocPrint(allocator, "[dbg] {s}\n", .{message});
        defer allocator.free(line);
        writer.writeAll(line);
    }
}

/// Whether a root failed at code checking rejected and already reported.
pub fn failedAtCheckedError(meaning: ?lir.LIR.FailureMeaning) bool {
    return if (meaning) |known| known == .checked_error else false;
}

/// The literal a root's failing crash rejected, when that crash is a literal
/// conversion rejecting its literal.
fn failedLiteralRejection(lir_result: *const lir.Program.Result, meaning: ?lir.LIR.FailureMeaning) ?lir.LIR.LiteralRejectionSite {
    return switch (meaning orelse return null) {
        .literal_rejection => |literal| .{
            .owner = lir_result.loweringModuleId(loweredModuleOfKey(lir_result, literal.module)).?,
            .checked_expr = literal.checked_expr,
            .kind = literal.kind,
        },
        .checked_error, .failed_value, .exhaustiveness => null,
    };
}

/// Report a rejected literal at the literal itself, in its own module, once
/// per literal: the report of a literal root whose failure no checked root
/// embeds.
fn reportLiteralRejection(
    owners: *const ModuleOwners,
    site: lir.LIR.LiteralRejectionSite,
    message: []const u8,
) FinalizeError!void {
    if (!owners.report_sites) return;
    var targets = try owners.reportTargets(site.owner);
    while (targets.nextTarget()) |owner| {
        const store = owner.problem_store orelse continue;
        const region = owner.module.checked_bodies.expr(@fromBackingInt(@intCast(site.checked_expr))).source_region;
        if (literalRejectionReported(store, site.kind, region)) continue;
        const failure_site = comptimeFailureSiteFrom(owner.module, region, null, null, null);
        const message_idx = try store.putExtraString(message);
        switch (site.kind) {
            .numeral => _ = try store.appendProblem(.{ .comptime_invalid_numeral = .{
                .message = message_idx,
                .region = failure_site.region,
                .origin = try comptimeFailureOrigin(store, failure_site),
            } }),
            .quote => _ = try store.appendProblem(.{ .comptime_invalid_quote = .{
                .message = message_idx,
                .region = failure_site.region,
                .origin = try comptimeFailureOrigin(store, failure_site),
            } }),
            .interpolation => _ = try store.appendProblem(.{ .comptime_invalid_interpolation = .{
                .message = message_idx,
                .region = failure_site.region,
                .origin = try comptimeFailureOrigin(store, failure_site),
            } }),
        }
    }
}

/// Whether a store already reports this literal's rejection.
fn literalRejectionReported(store: *const check.problem.Store, kind: lir.LIR.LiteralRejectionKind, region: base.Region) bool {
    for (store.problems.items) |problem| {
        const reported_region = switch (kind) {
            .numeral => if (problem == .comptime_invalid_numeral) problem.comptime_invalid_numeral.region else continue,
            .quote => if (problem == .comptime_invalid_quote) problem.comptime_invalid_quote.region else continue,
            .interpolation => if (problem == .comptime_invalid_interpolation) problem.comptime_invalid_interpolation.region else continue,
        };
        if (regionsEqual(reported_region, region)) return true;
    }
    return false;
}

/// The payload of a failed root in an evaluation that collects no diagnostics.
fn unreportedFailedRootPayload(
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    message: []const u8,
    failure: RootFailure,
) Allocator.Error!checked.CompileTimeRootPayload {
    if (root.guarded) return .runtime;
    return .{ .const_node = try appendFailureConst(module, message, failure) };
}

fn appendCrashConst(
    module: *checked.CheckedModuleArtifact,
    message: []const u8,
) Allocator.Error!checked.ConstNodeId {
    const data = try module.const_store.addBlobData(message);
    return try module.const_store.append(.{ .crash = .{
        .data = data,
        .offset = 0,
        .len = @intCast(message.len),
    } });
}

fn appendCheckedErrorConst(
    module: *checked.CheckedModuleArtifact,
    message: []const u8,
) Allocator.Error!checked.ConstNodeId {
    const data = try module.const_store.addBlobData(message);
    return try module.const_store.append(.{ .checked_error = .{
        .data = data,
        .offset = 0,
        .len = @intCast(message.len),
    } });
}

fn reportCompileTimeExpectFailures(
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_store: *const lir.LirStore,
    failures: []const ExpectFailure,
) FinalizeError!bool {
    if (failures.len == 0) return false;
    const problem_store = maybe_problem_store orelse return false;
    const root_region = module.checked_bodies.expr(root.expr).source_region;
    for (failures) |failure| {
        const message_idx = try problem_store.putExtraString(failure.message);
        const site = comptimeFailureSiteFromLoc(
            module,
            lir_store,
            root_region,
            failure.region,
            failure.loc,
        );
        _ = try problem_store.appendProblem(.{ .comptime_expect_failed = .{
            .message = message_idx,
            .region = site.region,
            .origin = try comptimeFailureOrigin(problem_store, site),
        } });
    }
    return true;
}

const CompileTimeEvalResult = union(enum) {
    value: Interpreter.EvalResult,
    failed: struct { payload: checked.CompileTimeRootPayload, message: []const u8 },
    /// The evaluation failed by reading a member of its group not yet
    /// evaluated (`Options.retry`), so it reports and completes nothing.
    retry,
};

fn evalCompileTimeRoot(
    interpreter: *Interpreter,
    problem_store: ?*check.problem.Store,
    owners: *const ModuleOwners,
    root_owner: lir.LIR.LoweringModuleId,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    slots: *const StaticSlotEnvironment,
    proc: lir.LIR.LirProcSpecId,
    ret_layout: @import("layout").Idx,
    retry: ?*const CycleRetry,
) FinalizeError!CompileTimeEvalResult {
    const result = interpreter.eval(.{
        .proc_id = proc,
        .ret_layout = ret_layout,
    }) catch |err| failure: {
        break :failure switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.RuntimeError => {
                const message = interpreter.getRuntimeErrorMessage() orelse "compile-time evaluation failed";
                return .{ .failed = .{ .message = message, .payload = try reportCompileTimeCrash(problem_store, module, root, interpreterFailedAt(lir_result, interpreter, slots), owners, lir_result, message) } };
            },
            error.DivisionByZero => {
                const message = interpreter.getRuntimeErrorMessage() orelse "Division by zero";
                return .{ .failed = .{ .message = message, .payload = try reportCompileTimeCrash(problem_store, module, root, interpreterFailedAt(lir_result, interpreter, slots), owners, lir_result, message) } };
            },
            error.Crash => {
                const message = interpreter.getCrashMessage();
                const failed_at = interpreterFailedAt(lir_result, interpreter, slots);
                if (retry) |group| if (group.holds(failed_at.meaning)) return .retry;
                if (exhaustivenessFailure(failed_at.meaning)) |failure| {
                    return .{ .failed = .{ .message = message, .payload = try reportCompileTimeExhaustiveness(problem_store, owners, root_owner, module, root, lir_result, failure, proc) } };
                }
                return .{ .failed = .{ .message = message, .payload = try reportCompileTimeCrash(problem_store, module, root, failed_at, owners, lir_result, message) } };
            },
            error.ExpectErr => finalizationInvariant("compile-time root reached an expect_err statement"),
            error.UnsupportedHostedFunction => finalizationInvariant("compile-time root reached an unsupported hosted function"),
            error.InvalidHostedFunctionSignature => finalizationInvariant("compile-time root reached an invalid hosted function signature"),
        };
    };
    return .{ .value = result };
}

fn reportCompileTimeExhaustiveness(
    maybe_problem_store: ?*check.problem.Store,
    owners: *const ModuleOwners,
    root_owner: lir.LIR.LoweringModuleId,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    failure: ExhaustivenessFailure,
    root_proc: lir.LIR.LirProcSpecId,
) FinalizeError!checked.CompileTimeRootPayload {
    _ = maybe_problem_store orelse {
        finalizationInvariant("compile-time root reached an empirical exhaustiveness failure without a checking problem store");
    };
    try appendCompileTimeExhaustivenessProblem(owners, root_owner, root, lir_result, root_proc, failure);
    return try failedRootPayload(module, root, "compile-time exhaustiveness failure", .crash);
}

/// Whether evaluation may decide a site's pending static diagnostic. `root_id`
/// is the running compile-time root when that root belongs to `module`, and
/// null otherwise: a root of another module cannot name this module's root
/// ids, so it can never be the root a site's validation was explicitly
/// delegated to.
fn comptimeSiteMayResolvePending(
    module: *const checked.CheckedModuleArtifact,
    root_id: ?checked.ComptimeRootId,
    checked_site: checked.CheckedExhaustivenessSiteId,
) bool {
    const site = module.exhaustiveness_sites.get(checked_site);
    if (site.requires_pairing) return false;
    return switch (site.policy) {
        .compile_time_replaced_by_root => |owner_root| if (root_id) |id| owner_root == id else false,
        .compile_time_only => true,
        .runtime_reachable,
        .not_pending,
        => false,
    };
}

fn appendCompileTimeExhaustivenessProblem(
    owners: *const ModuleOwners,
    root_owner: lir.LIR.LoweringModuleId,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    failure: ExhaustivenessFailure,
) Allocator.Error!void {
    if (!owners.report_sites) return;
    const checked_site = failure.checked_site orelse {
        finalizationInvariant("empirical exhaustiveness failure had no checked site id");
    };
    const site_owner = lir_result.loweringModuleId(loweredModuleOfKey(lir_result, failure.module)).?;
    var alias: ?u32 = owners.position(site_owner) orelse
        finalizationInvariant("empirical exhaustiveness failure named a checked module this finalization does not complete");
    discardUnreachedRootComptimeSites(owners, lir_result, root_proc, site_owner, checked_site);
    while (alias) |index| : (alias = owners.next_alias[index]) {
        const owner = owners.modules[index];
        const problem_store = owner.problem_store orelse {
            finalizationInvariant("compile-time root reached an empirical exhaustiveness failure without a checking problem store");
        };
        const owning_root: ?checked.ComptimeRootId = if (site_owner == root_owner) root.id else null;
        if (!comptimeSiteMayResolvePending(owner.module, owning_root, checked_site)) {
            const site_record = owner.module.exhaustiveness_sites.get(checked_site);
            if (!site_record.requires_pairing) switch (site_record.policy) {
                .runtime_reachable => {},
                .not_pending,
                .compile_time_only,
                .compile_time_replaced_by_root,
                => finalizationInvariant("compile-time exhaustiveness failure had an impossible site policy"),
            };
        }
        const matched = try problem_store.appendEmpiricalExhaustivenessFailureRetaining(checked_site, owner.module.exhaustiveness_sites.get(checked_site).requires_pairing);
        if (!matched) {
            finalizationInvariant("empirical exhaustiveness failure had no pending static diagnostic");
        }
    }
}

fn discardUnreachedRootComptimeSites(
    owners: *const ModuleOwners,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    failed_owner: lir.LIR.LoweringModuleId,
    failed_checked_site: checked.CheckedExhaustivenessSiteId,
) void {
    for (lir_result.comptime_sites.items) |root_site| {
        if (root_site.proc != root_proc) continue;
        const checked_site = root_site.checked_site orelse continue;
        if (root_site.owner == failed_owner and checked_site == failed_checked_site) continue;
        var alias = owners.position(root_site.owner);
        while (alias) |index| : (alias = owners.next_alias[index]) {
            const owner = owners.modules[index];
            const problem_store = owner.problem_store orelse continue;
            const site = owner.module.exhaustiveness_sites.get(checked_site);
            if (site.requires_pairing) continue;
            switch (site.policy) {
                .compile_time_replaced_by_root,
                .compile_time_only,
                => problem_store.discardPendingStaticExhaustiveness(checked_site),
                .runtime_reachable,
                .not_pending,
                => {},
            }
        }
    }
}

/// Complete only diagnostics whose checked evaluation context is available.
/// A generic platform carries the remaining recipes into its cache entry.
///
/// A site a compile-time root replaced is decided by that root's outcome:
/// evaluation ran the site's code, so it reports the site exactly when it
/// reached a missing case, unless the root is a guarded root that failed and
/// left its original expression to runtime, which decides the site
/// statically again, exactly as if the root had not been selected. Every
/// other site is decided by its checking mode.
pub fn finishPendingExhaustiveness(module: *const checked.CheckedModuleArtifact, store: *check.problem.Store) Allocator.Error!void {
    var retained: usize = 0;
    for (store.pending_static_exhaustiveness.items) |pending| {
        if (pending.reported) continue;
        const site = if (pending.site) |id| module.exhaustiveness_sites.get(id) else null;
        if (site != null and site.?.requires_pairing) {
            store.pending_static_exhaustiveness.items[retained] = pending;
            retained += 1;
            continue;
        }
        const decided_statically = if (site) |checked_site| switch (checked_site.policy) {
            .compile_time_replaced_by_root => |root| switch (module.compile_time_roots.root(root).payload) {
                .runtime => true,
                .const_node, .fn_value, .discarded, .expect => false,
                // A root this compilation never evaluated ran none of the
                // site's code.
                .pending, .unevaluated => pending.mode == .static,
            },
            .compile_time_only, .runtime_reachable, .not_pending => pending.mode == .static,
        } else pending.mode == .static;
        if (decided_statically) _ = try store.appendProblem(pending.problem);
    }
    store.pending_static_exhaustiveness.items.len = retained;
}

fn reportCompileTimeCrash(
    maybe_problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    failed_at: FailedAt,
    owners: *const ModuleOwners,
    lir_result: *const lir.Program.Result,
    message: []const u8,
) FinalizeError!checked.CompileTimeRootPayload {
    const root_region = module.checked_bodies.expr(root.expr).source_region;
    switch (try reportEmbeddedFailure(maybe_problem_store, module, root_region, owners, lir_result, failed_at, message)) {
        .reported, .reported_elsewhere => return try failedRootPayload(module, root, message, RootFailure.of(failed_at.meaning)),
        .none => {},
    }
    if (root.guarded) return try failedRootPayload(module, root, message, RootFailure.of(failed_at.meaning));
    const problem_store = maybe_problem_store orelse {
        finalizationInvariant("compile-time root crashed without a checking problem store");
    };
    const message_idx = try problem_store.putExtraString(message);
    const site = comptimeFailureSiteFromLoc(module, &lir_result.store, root_region, failed_at.region, failed_at.loc);
    _ = try problem_store.appendProblem(.{ .comptime_crash = .{
        .message = message_idx,
        .region = site.region,
        .origin = try comptimeFailureOrigin(problem_store, site),
    } });
    return try failedRootPayload(module, root, message, RootFailure.of(failed_at.meaning));
}

/// The checked site whose exhaustiveness a failure is the empirical failure
/// of.
const ExhaustivenessFailure = @FieldType(lir.LIR.FailureMeaning, "exhaustiveness");

fn exhaustivenessFailure(meaning: ?lir.LIR.FailureMeaning) ?ExhaustivenessFailure {
    return switch (meaning orelse return null) {
        .exhaustiveness => |failure| failure,
        .checked_error, .literal_rejection, .failed_value => null,
    };
}

fn failedRootPayload(
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    message: []const u8,
    failure: RootFailure,
) Allocator.Error!checked.CompileTimeRootPayload {
    if (root.guarded) return .runtime;
    return switch (root.kind) {
        .expect => .expect,
        .hoisted_validation => .discarded,
        .constant,
        .hoisted_constant,
        .callable_binding,
        .numeral_conversion,
        .quote_conversion,
        .repl_expr,
        => .{ .const_node = try appendFailureConst(module, message, failure) },
    };
}

/// A compile-time root's failure as its failure record carries it.
const SlotFailure = struct {
    message: []const u8,
    kind: RootFailure,
};

/// Whether a root's evaluation failed at code checking rejected, which keeps
/// that fact in the value it stores for the root and in its failure record.
const RootFailure = enum {
    crash,
    checked_error,

    fn of(meaning: ?lir.LIR.FailureMeaning) RootFailure {
        return if (failedAtCheckedError(meaning)) .checked_error else .crash;
    }

    /// The failure a failed root's stored payload records.
    fn ofPayload(module: *const checked.CheckedModuleArtifact, payload: checked.CompileTimeRootPayload) RootFailure {
        return switch (payload) {
            .const_node => |node| switch (module.const_store.get(node)) {
                .checked_error => .checked_error,
                .pending, .zst, .scalar, .str, .list, .box, .tuple, .record, .crash, .tag, .nominal, .fn_value => .crash,
            },
            .pending, .unevaluated, .fn_value, .discarded, .expect, .runtime => .crash,
        };
    }

    fn recordKind(self: RootFailure) lir.Program.ComptimeFailureKind {
        return switch (self) {
            .crash => .crash,
            .checked_error => .checked_error,
        };
    }
};

fn appendFailureConst(
    module: *checked.CheckedModuleArtifact,
    message: []const u8,
    failure: RootFailure,
) Allocator.Error!checked.ConstNodeId {
    return switch (failure) {
        .crash => try appendCrashConst(module, message),
        .checked_error => try appendCheckedErrorConst(module, message),
    };
}

const ComptimeFailureSite = struct {
    /// A region in the finalized module's source: the failed statement when
    /// it belongs to this module, otherwise the consuming compile-time root.
    region: base.Region,
    /// The failed statement's declaring module when it is not the finalized
    /// module (source inlined across modules, e.g. a `??` field default
    /// materialized per specialization).
    foreign: ?struct {
        module_name: []const u8,
        line: u32,
        column: u32,
    },
};

/// Resolve a compile-time failure's report site from the failed LIR
/// statement's explicit source stamp. The lowerer records each statement's
/// declaring module in the LIR source-file table, so a failed region is only
/// rendered against this module's source when it actually belongs to this
/// module; a foreign region (source inlined across modules, e.g. a `??`
/// field default materialized per specialization) is reported by its
/// declaring module's name and resolved position, with the consuming root as
/// the local region.
///
/// Module identity is the module's content identity, never a name: two
/// packages may both contain a `Utils`, and matching by bare name would render
/// the foreign module's byte offsets against this module's source. The
/// rendered origin stays human-readable: the bare display name, or the
/// qualified name when the bare name coincides with the finalized module's and
/// would not identify the declaring module.
fn comptimeFailureSiteFrom(
    module: *const checked.CheckedModuleArtifact,
    root_region: base.Region,
    failed_region: ?base.Region,
    failed_loc: ?base.SourceLoc,
    failed_file: ?base.SourceFileEntry,
) ComptimeFailureSite {
    const loc = failed_loc orelse return .{ .region = root_region, .foreign = null };
    const file = failed_file orelse return .{ .region = root_region, .foreign = null };
    const env = module.moduleEnvConst();
    if (@as(u256, @bitCast(file.module_identity)) == @as(u256, @bitCast(module.module_identity.stable_hash))) {
        return .{ .region = failed_region orelse root_region, .foreign = null };
    }
    const bare_name_collides = if (env.common.idents.lookup(base.Ident.for_text(file.name))) |display_ident|
        display_ident.eql(env.display_module_name_idx)
    else
        false;
    return .{ .region = root_region, .foreign = .{
        .module_name = if (bare_name_collides) file.qualified_name else file.name,
        .line = loc.line,
        .column = loc.column,
    } };
}

/// `comptimeFailureSiteFrom` with the failed statement's file entry resolved
/// through the LIR store's source-file table.
fn comptimeFailureSiteFromLoc(
    module: *const checked.CheckedModuleArtifact,
    lir_store: *const lir.LirStore,
    root_region: base.Region,
    failed_region: ?base.Region,
    failed_loc: ?base.SourceLoc,
) ComptimeFailureSite {
    const failed_file: ?base.SourceFileEntry = if (failed_loc) |loc|
        (if (loc.hasLocation()) .{
            .name = lir_store.sourceFileName(loc.file),
            .qualified_name = lir_store.sourceFileQualifiedName(loc.file),
            .module_identity = lir_store.sourceFileModuleIdentity(loc.file),
        } else null)
    else
        null;
    return comptimeFailureSiteFrom(module, root_region, failed_region, failed_loc, failed_file);
}

fn comptimeFailureOrigin(
    problem_store: *check.problem.Store,
    site: ComptimeFailureSite,
) Allocator.Error!?check.problem.types.ComptimeOrigin {
    const foreign = site.foreign orelse return null;
    return .{
        .module_name = try problem_store.putExtraString(foreign.module_name),
        .line = foreign.line,
        .column = foreign.column,
    };
}

fn finalizationImports(
    allocator: Allocator,
    root: checked.ImportedModuleView,
    imports: []const checked.PublishImportArtifact,
    available_modules: []const checked.ImportedModuleView,
) Allocator.Error![]checked.ImportedModuleView {
    var out = std.ArrayList(checked.ImportedModuleView).empty;
    errdefer out.deinit(allocator);

    for (imports) |import| {
        try appendUniqueImport(allocator, root, &out, import.view);
    }
    for (available_modules) |module| {
        try appendUniqueImport(allocator, root, &out, module);
    }

    return try out.toOwnedSlice(allocator);
}

fn appendUniqueImport(
    allocator: Allocator,
    root: checked.ImportedModuleView,
    out: *std.ArrayList(checked.ImportedModuleView),
    module: checked.ImportedModuleView,
) Allocator.Error!void {
    if (sameModuleIdentity(root, module)) return;
    for (out.items) |existing| {
        if (sameModuleIdentity(existing, module)) return;
    }
    try out.append(allocator, module);
}

fn sameModuleIdentity(a: checked.ImportedModuleView, b: checked.ImportedModuleView) bool {
    return std.meta.eql(a.module_identity.stable_hash, b.module_identity.stable_hash);
}

fn regionsEqual(a: base.Region, b: base.Region) bool {
    return a.start.offset == b.start.offset and a.end.offset == b.end.offset;
}

fn compileTimeRootForRequest(
    module: *const checked.CheckedModuleArtifact,
    request: checked.RootRequest,
) checked.ComptimeRootId {
    const root_id = request.compile_time_root orelse {
        finalizationInvariant("compile-time request had no exact checked root identity");
    };
    const raw = @backingInt(root_id);
    if (raw >= module.compile_time_roots.roots.len) {
        finalizationInvariant("compile-time request root identity was outside the checked root table");
    }
    const root = module.compile_time_roots.roots[raw];
    const kind_matches = switch (request.kind) {
        .compile_time_constant => root.kind == .constant or root.kind == .hoisted_constant or root.kind == .hoisted_validation or root.kind == .numeral_conversion or root.kind == .quote_conversion,
        .compile_time_callable => root.kind == .callable_binding,
        .repl_expr => root.kind == .repl_expr,
        .runtime_entrypoint,
        .provided_export,
        .platform_required_binding,
        .hosted_export,
        .test_expect,
        .dev_expr,
        => finalizationInvariant("non compile-time request reached compile-time root lookup"),
    };
    if (root.id != root_id or !kind_matches or !rootSourceEql(root.source, request.source)) {
        finalizationInvariant("compile-time request identity did not match its checked root");
    }

    return root_id;
}

/// Store an evaluated root's value; a hoisted validation keeps no value.
fn storeRootValue(
    writer: *ConstStoreWriter.Writer,
    root: LirProgram.ConstRootPlan,
    compile_time_root: checked.CompileTimeRoot,
    value: @import("value.zig").Value,
) ConstStoreWriter.Error!checked.CompileTimeRootPayload {
    if (compile_time_root.kind == .hoisted_validation) return .discarded;
    return writer.storeRoot(root, value);
}

/// Record a root's final payload and stored type and mark it done.
fn completeRoot(
    module: *checked.CheckedModuleArtifact,
    writer: *ConstStoreWriter.Writer,
    state: *RootCompletionState,
    root: LirProgram.ConstRootPlan,
    root_id: checked.ComptimeRootId,
    compile_time_root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
) ConstStoreWriter.Error!void {
    module.compile_time_roots.fillPayload(root_id, payload);
    const stored_root_type = switch (compile_time_root.kind) {
        .constant, .hoisted_constant => try writer.storeRootType(root),
        .hoisted_validation,
        .callable_binding,
        .expect,
        .numeral_conversion,
        .quote_conversion,
        .repl_expr,
        => null,
    };
    finishConstRoot(module, compile_time_root, payload, stored_root_type);
    state.markDone(root_id);
}

fn finishConstRoot(
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
    root_type: ?check.ConstStore.ConstTypeId,
) void {
    if (root.kind == .repl_expr) {
        if (payload != .const_node) {
            finalizationInvariant("compile-time REPL root finalized with non-constant payload");
        }
        return;
    }
    if (root.kind != .constant and root.kind != .hoisted_constant) return;
    const node = switch (payload) {
        .const_node => |id| id,
        // Runtime lowering evaluates the original expression; no stored
        // constant exists for it.
        .runtime => return,
        .pending,
        .unevaluated,
        .fn_value,
        .discarded,
        .expect,
        => finalizationInvariant("constant root finalized with non-constant payload"),
    };
    const const_ref = switch (root.kind) {
        .constant => blk: {
            const def_idx = switch (root.source) {
                .def => |def| def,
                .expr, .statement, .required_binding, .hoisted => finalizationInvariant("constant root source was not a top-level definition"),
            };
            const top_level = module.top_level_values.lookupByDef(def_idx) orelse
                finalizationInvariant("constant root had no top-level value");
            break :blk switch (top_level.value) {
                .const_ref => |ref| ref,
                .procedure_binding => finalizationInvariant("constant root top-level value was not a constant"),
            };
        },
        .hoisted_constant => blk: {
            const hoisted = module.hoisted_constants.lookupByRoot(root.id) orelse
                finalizationInvariant("hoisted constant root had no hoisted const entry");
            break :blk hoisted.const_ref;
        },
        .hoisted_validation,
        .callable_binding,
        .expect,
        .numeral_conversion,
        .quote_conversion,
        .repl_expr,
        => unreachable,
    };
    const stored = checked.StoredConstTemplate{
        .node = node,
        .root = root.id,
        .root_type = root_type orelse finalizationInvariant("constant root finalized without exact Monotype representation evidence"),
    };
    module.const_templates.fillStoredConst(const_ref, stored);
    if (root.kind == .constant) {
        module.exported_const_templates.fillStoredConst(const_ref, stored);
    }
}

fn rootSourceEql(a: checked.RootSource, b: checked.RootSource) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .def => |left| left == b.def,
        .expr => |left| left == b.expr,
        .statement => |left| left == b.statement,
        .required_binding => |left| left == b.required_binding,
        .hoisted => |left| left.index == b.hoisted.index and left.expr == b.hoisted.expr,
    };
}

fn artifactMatches(a: anytype, b: checked.CheckedModuleArtifactKey) bool {
    return std.meta.eql(a.bytes, b.bytes);
}

/// Lower a module that is still being checked.
///
/// `lowerCheckedModulesToLir` binds hosted declarations against a platform
/// header's hosted section only once the module holding that section finished
/// checking, so the unbound-declaration rejection cannot reach compile-time
/// finalization.
fn lowerFinalizationModulesToLir(
    allocator: Allocator,
    modules: lir.CheckedPipeline.CheckedModuleSet,
    roots: lir.CheckedPipeline.RootRequestSet,
    target: lir.CheckedPipeline.TargetConfig,
) Allocator.Error!lir.CheckedPipeline.LoweredProgram {
    return lir.CheckedPipeline.lowerCheckedModulesToLir(allocator, modules, roots, target) catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.HostedFunctionNotBound => finalizationInvariant(
            "compile-time finalization lowering rejected a hosted declaration the platform header did not bind",
        ),
    };
}

/// A splice invariant failure names the symbol the link stopped at.
fn spliceInvariant(splice: *const backend.dev.HostSplice, comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .debug) {
        base.invariant("compile-time finalization invariant violated: {s}: {s}", .{ message, splice.unresolved orelse "" });
    }
    finalizationInvariant(message);
}

fn finalizationInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .debug) {
        base.invariant("compile-time finalization invariant violated: {s}", .{message});
    }
    unreachable;
}

test "module owners resolve a lowered program's module ids to their finalized modules" {
    const allocator = std.testing.allocator;
    var program = try LirProgram.Result.init(allocator, .u64);
    defer program.deinit();

    var keys: [3]checked.ModuleId = .{ .{}, .{}, .{} };
    keys[0].bytes[0] = 1;
    keys[1].bytes[0] = 2;
    keys[2].bytes[0] = 3;
    try program.setLoweringModules(&keys);

    // Finalization completes a subset of the lowering's modules, in an order
    // of its own. Only `key` is read, so the rest of each artifact stays out
    // of this test.
    var artifacts: [3]checked.CheckedModuleArtifact = undefined;
    artifacts[0].key = keys[2];
    artifacts[1].key = keys[0];
    artifacts[2].key = keys[2];
    const modules = [_]ProgramModule{
        .{ .module = &artifacts[0], .problem_store = null },
        .{ .module = &artifacts[1], .problem_store = null },
        .{ .module = &artifacts[2], .problem_store = null },
    };

    var owners = try ModuleOwners.init(allocator, program.lowering_modules.items, &modules);
    defer owners.deinit(allocator);
    try std.testing.expectEqual(@as(?u32, 1), owners.position(.first));
    try std.testing.expectEqual(@as(?u32, null), owners.position(@fromBackingInt(@intCast(1))));
    try std.testing.expectEqual(@as(?u32, 0), owners.position(@fromBackingInt(@intCast(2))));
    try std.testing.expect(owners.get(@fromBackingInt(@intCast(1))) == null);
    try std.testing.expectEqual(&artifacts[0], owners.get(@fromBackingInt(@intCast(2))).?.module);
    try std.testing.expectEqual(&artifacts[1], owners.get(.first).?.module);
    try std.testing.expectEqual(@as(?u32, 2), owners.next_alias[0]);
    try std.testing.expectEqual(@as(?u32, null), owners.next_alias[1]);
    try std.testing.expectEqual(@as(?u32, null), owners.next_alias[2]);
}

test "compile-time progress elapsed rejects unset and future timestamps" {
    try std.testing.expectEqual(@as(?ProgressMillis, null), elapsedMs(10, 0));
    try std.testing.expectEqual(@as(?ProgressMillis, null), elapsedMs(10, 11));
    try std.testing.expectEqual(@as(?ProgressMillis, 7), elapsedMs(18, 11));
}

test "compile-time progress wait avoids timestamp wraparound" {
    try std.testing.expectEqual(@as(ProgressMillis, 3007), msUntilElapsed(10, 17, 3000));
    try std.testing.expectEqual(@as(ProgressMillis, 2500), msUntilElapsed(500, 0, 2500));
    try std.testing.expectEqual(@as(ProgressMillis, 4), msUntilElapsed(16, 10, 10));
    try std.testing.expectEqual(@as(ProgressMillis, 0), msUntilElapsed(20, 10, 10));
    try std.testing.expectEqual(std.math.maxInt(ProgressMillis), msUntilElapsed(0, std.math.maxInt(ProgressMillis), 1));
}

test "compile-time finalization declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "CTFE native emission uses worker callbacks and retains reusable code without its image" {
    if (comptime !backend.host_lir_codegen_available) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const tasks = base.post_check_task_executor;
    const ExecutorFixture = struct {
        lane: tasks.LaneState,
        completions: std.ArrayList(tasks.Completion) = .empty,
        callbacks: usize = 0,

        fn begin(_: *anyopaque) void {}
        fn end(_: *anyopaque) void {}
        fn submit(raw: *anyopaque, task: tasks.Task) Allocator.Error!void {
            const self: *@This() = @ptrCast(@alignCast(raw));
            var scratch = base.SingleThreadArena.init(std.testing.allocator);
            defer scratch.deinit();
            self.callbacks += 1;
            const value = task.run(task.context, .{
                .id = 0,
                .allocator = std.testing.allocator,
                .scratch = scratch.allocator(),
                .lane_state = &self.lane,
            });
            try self.completions.append(std.testing.allocator, .{ .id = task.id, .worker_id = 0, .value = value });
        }
        fn receive(raw: *anyopaque) tasks.Completion {
            const self: *@This() = @ptrCast(@alignCast(raw));
            return self.completions.orderedRemove(0);
        }
        fn executor(self: *@This()) tasks.Executor {
            return .{ .context = self, .worker_count = 1, .beginFn = begin, .submitFn = submit, .receiveFn = receive, .endFn = end };
        }
    };
    var program = try LirProgram.Result.init(allocator, .native);
    defer program.deinit();
    const answer = try program.store.addLocal(.{ .layout_idx = .u64 });
    const ret = try program.store.addCFStmt(.{ .ret = .{ .value = answer } }, .test_fixture);
    const body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = answer, .value = .{ .i64_literal = .{ .value = 42, .layout_idx = .u64 } }, .next = ret } }, .test_fixture);
    const proc = try program.store.addProcSpec(.{
        .name = .fromRaw(0),
        .identity = lir.LIR.ProcIdentity.forTest(901),
        .args = .empty(),
        .frame_locals = try program.store.addLocalSpan(&.{answer}),
        .body = body,
        .ret_layout = .u64,
    }, .none);
    var timing = Timing.init(std.testing.io);
    var retained = block: {
        var executor = ExecutorFixture{ .lane = tasks.LaneState.init(allocator) };
        defer executor.lane.deinit();
        defer executor.completions.deinit(allocator);
        var strings = try backend.StaticStringData.build(allocator, &program.store, backend.dev.LirCodeGenMod.host_lir_codegen_target);
        defer strings.deinit();
        var codegen = try backend.HostLirCodeGen.initWithBoxyMetadata(allocator, &program.store, &program.layouts, strings.view(), &.{}, &.{}, &.{}, roc_target.host_cpu.level());
        defer codegen.deinit();
        var artifacts = try DevProgram.compileProcedures(allocator, &codegen, &.{proc}, &.{}, .{ .post_check_executor = executor.executor(), .timing = &timing });
        errdefer artifacts.deinit();
        try std.testing.expect(executor.callbacks > 0);
        try std.testing.expect(timing.snapshot().native_emission.procedures_emitted > 0);
        const entry = try codegen.generateEntrypointWrapper("ctfe_retained_leaf", proc, &.{}, .u64);
        try codegen.finishImage();
        var image = try backend.ExecutableMemory.initWithEntryOffset(codegen.getGeneratedCode(), 0);
        defer image.deinit();
        var result: u64 = 0;
        image.callRocABIAt(entry.offset, @ptrCast(&result), null);
        try std.testing.expectEqual(@as(u64, 42), result);
        break :block artifacts;
    };
    defer retained.deinit();
    // Every producer workspace, string table, and executable mapping is dead.
    // Only the same LIR domain and independently owned artifacts remain.
    for (0..2) |iteration| {
        if (iteration == 1) program.store.getProcSpecPtr(proc).native_code_revision += 1;
        var strings = try backend.StaticStringData.build(allocator, &program.store, backend.dev.LirCodeGenMod.host_lir_codegen_target);
        defer strings.deinit();
        var codegen = try backend.HostLirCodeGen.initWithBoxyMetadata(allocator, &program.store, &program.layouts, strings.view(), &.{}, &.{}, &.{}, roc_target.host_cpu.level());
        defer codegen.deinit();
        var metrics: NativeProcCompiler.Metrics = .{};
        var runtime_artifacts = try NativeProcCompiler.run(backend.HostLirCodeGen, allocator, &codegen, &.{proc}, .{
            .target = backend.dev.LirCodeGenMod.host_lir_codegen_target,
            .reuse_same_program = &retained,
            .metrics_out = &metrics,
        });
        defer runtime_artifacts.deinit();
        if (iteration == 0) {
            try std.testing.expectEqual(@as(u64, 1), metrics.procedures_reused);
            try std.testing.expectEqual(@as(u64, 0), metrics.procedures_emitted);
        } else {
            try std.testing.expectEqual(@as(u64, 0), metrics.procedures_reused);
            try std.testing.expectEqual(@as(u64, 1), metrics.procedures_emitted);
            try std.testing.expectEqual(@as(u64, 1), metrics.rejected_revision);
        }
        const entry = try codegen.generateEntrypointWrapper("runtime_retained_leaf", proc, &.{}, .u64);
        try codegen.finishImage();
        var image = try backend.ExecutableMemory.initWithEntryOffset(codegen.getGeneratedCode(), 0);
        defer image.deinit();
        var result: u64 = 0;
        image.callRocABIAt(entry.offset, @ptrCast(&result), null);
        try std.testing.expectEqual(@as(u64, 42), result);
    }
}

test "shared compile-time debug replay sorts module root and event identities" {
    const allocator = std.testing.allocator;
    var events = DebugEvents{ .allocator = allocator };
    defer events.deinit();
    const inputs = [_]struct { module: u8, root: u32, message: []const u8 }{
        .{ .module = 2, .root = 0, .message = "last" },
        .{ .module = 1, .root = 1, .message = "second" },
        .{ .module = 1, .root = 0, .message = "first" },
        .{ .module = 1, .root = 1, .message = "third" },
    };
    for (inputs, 0..) |input, index| {
        const message = try allocator.dupe(u8, input.message);
        errdefer allocator.free(message);
        try events.events.append(allocator, .{ .module = .{ .bytes = @as([32]u8, @splat(input.module)) }, .root = @fromBackingInt(@intCast(input.root)), .sequence = index, .is_repl = false, .message = message });
    }
    const Output = struct {
        bytes: [128]u8 = undefined,
        len: usize = 0,
        fn write(raw: ?*anyopaque, message: []const u8) void {
            const self: *@This() = @ptrCast(@alignCast(raw.?));
            @memcpy(self.bytes[self.len..][0..message.len], message);
            self.len += message.len;
        }
    };
    var output = Output{};
    try events.replay(.{ .stderr = .{ .context = &output, .write = Output.write } });
    try std.testing.expectEqualStrings("[dbg] first\n[dbg] second\n[dbg] third\n[dbg] last\n", output.bytes[0..output.len]);
}

test "completed frozen graphs rebase independent nested symbols" {
    const allocator = std.testing.allocator;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const temporary = arena.allocator();
    const first = try static_data_exports.cloneStaticData(temporary, &.{
        .{ .symbol_name = "root_a", .bytes = &.{ 0, 0, 0, 0 }, .alignment = 4, .relocations = &.{.{ .offset = 0, .target_symbol_name = "backing_a", .target = .{ .data_symbol = @fromBackingInt(@intCast(1)) } }} },
        .{ .symbol_name = "backing_a", .bytes = "a", .alignment = 1 },
    });
    const second = try static_data_exports.cloneStaticData(temporary, &.{
        .{ .symbol_name = "root_b", .bytes = &.{ 0, 0, 0, 0 }, .alignment = 4, .relocations = &.{.{ .offset = 0, .target_symbol_name = "backing_b", .target = .{ .data_symbol = @fromBackingInt(@intCast(1)) } }} },
        .{ .symbol_name = "backing_b", .bytes = "b", .alignment = 1 },
    });
    var destination = std.ArrayList(static_data_exports.StaticDataExport).empty;
    defer destination.deinit(allocator);
    try appendFrozenGraph(allocator, &destination, first);
    try appendFrozenGraph(allocator, &destination, second);
    const owned = try static_data_exports.cloneStaticData(allocator, destination.items);
    defer static_data_exports.deinitStaticData(allocator, owned);
    try std.testing.expectEqual(@as(u32, 1), @backingInt(owned[0].relocations[0].target.data_symbol));
    try std.testing.expectEqual(@as(u32, 3), @backingInt(owned[2].relocations[0].target.data_symbol));
    try std.testing.expectEqualStrings("backing_a", owned[0].relocations[0].target_symbol_name);
    try std.testing.expectEqualStrings("backing_b", owned[2].relocations[0].target_symbol_name);
}

test "shared interpreter slots publish dependent strings and distinguish empty failures" {
    for ([_]?[]const u8{ null, "", "a failure message longer than the inline string representation" }) |failure| {
        try testInterpreterSlot(failure);
    }
}

fn testInterpreterSlot(failure_message: ?[]const u8) (FinalizeError || Interpreter.Error || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedError, TestUnexpectedError, DuplicateStaticDataSymbol, InvalidStaticDataAlignment, InvalidStaticDataRelocation, MissingStaticDataSymbol, UnresolvedStaticFunction })!void {
    const allocator = std.testing.allocator;
    var lowered = lir.CheckedPipeline.LoweredProgram{
        .lir_result = try LirProgram.Result.init(allocator, .native),
        .main_proc = null,
        .target_usize = .native,
        .runtime_value_schemas = lir.CheckedPipeline.RuntimeValueSchemaStore.init(allocator),
    };
    defer lowered.deinit();
    const result = &lowered.lir_result;
    const module_key: checked.ModuleId = .{};
    try result.setLoweringModules(&.{module_key});
    const module_id = result.loweringModuleId(module_key).?;
    var checked_roots = std.ArrayList(checked.CompileTimeRoot).empty;
    defer checked_roots.deinit(allocator);
    const root_id: checked.ComptimeRootId = @fromBackingInt(@intCast(checked_roots.items.len));
    // This execution fixture consumes only root identity, never checked syntax.
    try checked_roots.append(allocator, .{ .id = root_id, .module_idx = 0, .kind = .constant, .source = undefined, .pattern = null, .expr = undefined, .checked_type = undefined, .request_eligibility = .eligible, .payload = .pending });
    const failure_layout = try result.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = result.layouts.getLayout(failure_layout).getStruct().idx;
    const failed_offset = result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const message_offset = result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
    const failure_slot: lir.LIR.StaticDataId = @fromBackingInt(@intCast(result.static_data_values.items.len));
    try result.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = failure_layout,
        .compile_time_root = .{ .module = module_key, .root = .{ .checked = root_id }, .const_locator = null, .role = .{ .failure_message = .{
            .failed_field = 0,
            .message_field = 1,
            .failed_offset = failed_offset,
            .message_offset = message_offset,
        } } },
    });
    const value_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(result.const_plans.items.len));
    try result.const_plans.append(allocator, .str);
    const value_slot: lir.LIR.StaticDataId = @fromBackingInt(@intCast(result.static_data_values.items.len));
    try result.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = .str,
        .compile_time_root = .{ .module = module_key, .root = .{ .checked = root_id }, .const_locator = null, .role = .{ .value = .{ .failure_slot = failure_slot, .plan = value_plan } } },
    });
    const text = "a dependent root borrows this frozen string after its producer is dropped";
    const source_local = try result.store.addLocal(.{ .layout_idx = .str });
    const source_ret = try result.store.addCFStmt(.{ .ret = .{ .value = source_local } }, .test_fixture);
    const source_body = try result.store.addCFStmt(.{ .assign_literal = .{
        .target = source_local,
        .value = .{ .str_literal = .{ .backing = try result.store.insertString(text), .offset = 0, .len = text.len } },
        .next = source_ret,
    } }, .test_fixture);
    const source_proc = try result.store.addProcSpec(.{ .name = .fromRaw(0), .identity = lir.LIR.ProcIdentity.forTest(3), .args = .empty(), .frame_locals = try result.store.addLocalSpan(&.{source_local}), .body = source_body, .ret_layout = .str }, .none);
    const consumer_local = try result.store.addLocal(.{ .layout_idx = .str });
    const consumer_ret = try result.store.addCFStmt(.{ .ret = .{ .value = consumer_local } }, .test_fixture);
    const live_int = try result.store.addLocal(.{ .layout_idx = .u64 });
    const live_float = try result.store.addLocal(.{ .layout_idx = .f64 });
    const expected_int = try result.store.addLocal(.{ .layout_idx = .u64 });
    const expected_float = try result.store.addLocal(.{ .layout_idx = .f64 });
    const equal_int = try result.store.addLocal(.{ .layout_idx = .bool });
    const equal_float = try result.store.addLocal(.{ .layout_idx = .bool });
    const crash_text = "suspended consumer locals changed";
    const corrupt = try result.store.addCFStmt(.{ .crash = .{ .msg = .{ .literal = try result.store.insertString(crash_text) } } }, .test_fixture);
    const float_branch = try result.store.addCFStmt(.{ .switch_stmt = .{ .cond = equal_float, .branches = try result.store.addCFSwitchBranches(&.{.{ .value = 1, .body = consumer_ret }}), .default_branch = corrupt } }, .test_fixture);
    const float_compare = try result.store.addCFStmt(.{ .assign_low_level = .{ .target = equal_float, .op = .num_is_eq, .rc_effect = try result.store.addRcEffect(.{}), .args = try result.store.addLocalSpan(&.{ live_float, expected_float }), .next = float_branch } }, .test_fixture);
    const int_branch = try result.store.addCFStmt(.{ .switch_stmt = .{ .cond = equal_int, .branches = try result.store.addCFSwitchBranches(&.{.{ .value = 1, .body = float_compare }}), .default_branch = corrupt } }, .test_fixture);
    const int_compare = try result.store.addCFStmt(.{ .assign_low_level = .{ .target = equal_int, .op = .num_is_eq, .rc_effect = try result.store.addRcEffect(.{}), .args = try result.store.addLocalSpan(&.{ live_int, expected_int }), .next = int_branch } }, .test_fixture);
    const expected_float_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = expected_float, .value = .{ .f64_literal = 13.25 }, .next = int_compare } }, .test_fixture);
    const expected_int_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = expected_int, .value = .{ .i64_literal = .{ .value = 12345, .layout_idx = .u64 } }, .next = expected_float_stmt } }, .test_fixture);
    const consumer_load = try result.store.addCFStmt(.{ .assign_literal = .{ .target = consumer_local, .value = .{ .static_data = .{ .id = value_slot } }, .next = expected_int_stmt } }, .test_fixture);
    const input_int = try result.store.addLocal(.{ .layout_idx = .u64 });
    const addend_int = try result.store.addLocal(.{ .layout_idx = .u64 });
    const input_float = try result.store.addLocal(.{ .layout_idx = .f64 });
    const addend_float = try result.store.addLocal(.{ .layout_idx = .f64 });
    const live_float_stmt = try result.store.addCFStmt(.{ .assign_low_level = .{ .target = live_float, .op = .num_float_add, .rc_effect = try result.store.addRcEffect(.{}), .args = try result.store.addLocalSpan(&.{ input_float, addend_float }), .next = consumer_load } }, .test_fixture);
    const live_int_stmt = try result.store.addCFStmt(.{ .assign_low_level = .{ .target = live_int, .op = .num_int_add_wrap, .rc_effect = try result.store.addRcEffect(.{}), .args = try result.store.addLocalSpan(&.{ input_int, addend_int }), .next = live_float_stmt } }, .test_fixture);
    const float_addend_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = addend_float, .value = .{ .f64_literal = 3.25 }, .next = live_int_stmt } }, .test_fixture);
    const float_input_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = input_float, .value = .{ .f64_literal = 10.0 }, .next = float_addend_stmt } }, .test_fixture);
    const int_addend_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = addend_int, .value = .{ .i64_literal = .{ .value = 345, .layout_idx = .u64 } }, .next = float_input_stmt } }, .test_fixture);
    const consumer_body = try result.store.addCFStmt(.{ .assign_literal = .{ .target = input_int, .value = .{ .i64_literal = .{ .value = 12000, .layout_idx = .u64 } }, .next = int_addend_stmt } }, .test_fixture);
    const consumer_proc = try result.store.addProcSpec(.{ .name = .fromRaw(1), .identity = lir.LIR.ProcIdentity.forTest(2), .args = .empty(), .frame_locals = try result.store.addLocalSpan(&.{ consumer_local, live_int, live_float, expected_int, expected_float, equal_int, equal_float, input_int, addend_int, input_float, addend_float }), .body = consumer_body, .ret_layout = .str }, .none);
    try lir.ComptimeValueGuards.insert(allocator, result, null);

    const failure_size = result.layouts.layoutSize(result.layouts.getLayout(failure_layout));
    const zeros = try allocator.alloc(u8, failure_size);
    defer allocator.free(zeros);
    @memset(zeros, 0);
    const failure_name = try LirProgram.staticDataSymbolName(allocator, failure_slot);
    defer allocator.free(failure_name);
    const value_name = try LirProgram.staticDataSymbolName(allocator, value_slot);
    defer allocator.free(value_name);
    const materialized = try static_data_exports.cloneStaticData(allocator, &.{
        .{ .symbol_name = failure_name, .value_id = failure_slot, .bytes = zeros, .alignment = @alignOf(usize) },
        .{ .symbol_name = value_name, .value_id = value_slot, .bytes = zeros[0..@sizeOf(builtins.str.RocStr)], .alignment = @alignOf(usize) },
    });
    var image = try backend.StaticDataImage.init(allocator, materialized);
    const addresses = try image.lirValueAddresses(allocator, 2);
    const owner = try allocator.create(InterpreterProgram);
    owner.allocator = allocator;
    owner.shared_slots = null;
    owner.timing_io = null;
    owner.slots = .{ .allocator = allocator, .materialized = materialized, .image = image, .addresses = addresses, .publication = try PublicationSlots.init(allocator, result), .export_positions = try StaticSlotEnvironment.exportPositions(allocator, materialized, addresses.len) };
    owner.host = CompilerHost.init(allocator);
    owner.shared_callable_owner = null;
    owner.callable_images = 0;
    owner.base_callables_ready = false;
    owner.static_strings = try Interpreter.buildStaticStrings(allocator, &result.store);
    owner.interpreter = try Interpreter.initWithBoxyTables(allocator, &result.store, &result.layouts, Interpreter.BoxyTables.fromResult(result), owner.static_strings.view(), owner.host.ops());
    owner.host.bindInterpreter(&owner.interpreter);
    owner.callable_owner = .forInterpreter(&owner.interpreter);
    defer owner.deinit();
    owner.refreshCallableMetadata();
    if (failure_message) |message| {
        try owner.slots.publishFailure(&lowered, module_id, .{ .checked = root_id }, .{ .message = message, .kind = .crash }, .{ .resolve = InterpreterProgram.resolveFunction });
        const bytes: [*]const u8 = @ptrFromInt(addresses[0]);
        try std.testing.expectEqual(@as(u8, 1), bytes[failed_offset]);
        const stored: *const builtins.str.RocStr = @ptrCast(@alignCast(bytes + message_offset));
        try std.testing.expectEqualStrings(message, stored.asSlice());
        try std.testing.expectError(error.Crash, owner.interpreter.eval(.{ .proc_id = consumer_proc, .ret_layout = .str }));
        try std.testing.expectEqualStrings(message, owner.interpreter.getCrashMessage());
    } else {
        const value = try owner.interpreter.eval(.{ .proc_id = source_proc, .ret_layout = .str });
        try owner.publishRoot(&lowered, module_id, .{ .checked = root_id }, .{
            .ret_layout = .str,
            .plan = value_plan,
        }, value.value);
        owner.interpreter.dropValue(value.value, .str);
        const dependent = try owner.interpreter.eval(.{ .proc_id = consumer_proc, .ret_layout = .str });
        defer owner.interpreter.dropValue(dependent.value, .str);
        const stored: *const builtins.str.RocStr = @ptrCast(@alignCast(dependent.value.ptr));
        try std.testing.expectEqualStrings(text, stored.asSlice());
        const bytes: [*]const u8 = @ptrFromInt(addresses[0]);
        try std.testing.expectEqual(@as(u8, 0), bytes[failed_offset]);
    }
}

// Runtime-only fixture: checked syntax/type/template fields are never consumed;
// their undefined values distinguish them from the explicit runtime IDs below.
test "shared frozen erased callables execute on interpreter dev and LLVM" {
    const allocator = std.testing.allocator;
    var program = try LirProgram.Result.init(allocator, .native);
    defer program.deinit();
    const erased_layout = try program.layouts.insertErasedCallable();
    const closure_slot: lir.LIR.StaticDataId = @fromBackingInt(@intCast(program.static_data_values.items.len));
    try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = erased_layout });
    const capture_arg = try program.store.addLocal(.{ .layout_idx = .opaque_ptr });
    const reuse_arg = try program.store.addLocal(.{ .layout_idx = erased_layout });
    const answer = try program.store.addLocal(.{ .layout_idx = .bool });
    const worker_ret = try program.store.addCFStmt(.{ .ret = .{ .value = answer } }, .test_fixture);
    const worker_body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = answer, .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .bool } }, .next = worker_ret } }, .test_fixture);
    const arg_plan = try program.store.internErasedCallArgsPlan(&program.layouts, &.{});
    const worker = try program.store.addProcSpec(.{
        .name = .fromRaw(55),
        .identity = lir.LIR.ProcIdentity.forTest(2),
        .abi = .erased_callable,
        .args = try program.store.addLocalSpan(&.{ capture_arg, reuse_arg }),
        .erased_capture_arg = capture_arg,
        .erased_reuse_arg = reuse_arg,
        .erased_call_args = arg_plan,
        .frame_locals = try program.store.addLocalSpan(&.{ capture_arg, reuse_arg, answer }),
        .body = worker_body,
        .ret_layout = .bool,
    }, .none);
    const closure = try program.store.addLocal(.{ .layout_idx = erased_layout });
    const call_result = try program.store.addLocal(.{ .layout_idx = .bool });
    const caller_ret = try program.store.addCFStmt(.{ .ret = .{ .value = call_result } }, .test_fixture);
    const call = try program.store.addCFStmt(.{ .assign_call_erased = .{
        .target = call_result,
        .data = try program.store.addStmtRow(lir.LIR.ErasedCallData, .{
            .closure = closure,
            .args = .empty(),
            .arg_plan = arg_plan,
        }),
        .next = caller_ret,
    } }, .test_fixture);
    const caller_body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = closure, .value = .{ .static_data = .{ .id = closure_slot } }, .next = call } }, .test_fixture);
    const caller = try program.store.addProcSpec(.{ .name = .fromRaw(56), .identity = lir.LIR.ProcIdentity.forTest(1), .args = .empty(), .frame_locals = try program.store.addLocalSpan(&.{ closure, call_result }), .body = caller_body, .ret_layout = .bool }, .none);
    const closure_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    const erased_set: LirProgram.ErasedFnsId = @fromBackingInt(@intCast(program.erased_fns.items.len));
    try program.const_plans.append(allocator, .{ .erased_fn = erased_set });
    const capture_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .str);
    const captures = try allocator.dupe(LirProgram.CaptureSlot, &.{.{ .id = undefined, .kind = .lexical, .slot = 0, .ty = undefined, .plan = capture_plan, .storage = .value }});
    const entries = try allocator.dupe(LirProgram.ErasedFn, &.{.{
        .entry = worker,
        .capture_layout = .str,
        .template = .{ .fn_def = undefined, .source_fn_ty = undefined, .source_fn_key = undefined },
        .captures = captures,
        .on_drop = .{ .rc_helper = .{ .op = .host_drop, .layout_idx = .str } },
    }});
    try program.erased_fns.append(allocator, .{ .layout = erased_layout, .entries = entries });
    const slot_name = try LirProgram.staticDataSymbolName(allocator, closure_slot);
    defer allocator.free(slot_name);
    const proc_name = try static_data_exports.procSymbolName(allocator, program.store.getProcSpecImmediate(worker).identity);
    defer allocator.free(proc_name);
    const word = @sizeOf(usize);
    var payload_bytes: [4 * word + @sizeOf(builtins.str.RocStr)]u8 = @splat(0);
    const capture_str = builtins.str.RocStr.fromSliceSmall("capture");
    @memcpy(payload_bytes[4 * word ..], std.mem.asBytes(&capture_str));
    const drop_name = try static_data_exports.atomicRcHelperSymbolName(allocator, &program.layouts, .{ .op = .host_drop, .layout_idx = .str });
    defer allocator.free(drop_name);
    const exports = try static_data_exports.cloneStaticData(allocator, &.{
        .{ .symbol_name = slot_name, .value_id = closure_slot, .bytes = &(@as([word]u8, @splat(0))), .alignment = @alignOf(usize), .relocations = &.{.{ .offset = 0, .target_symbol_name = "closure_payload", .target = .{ .data_symbol = @fromBackingInt(@intCast(1)) }, .addend = 2 * word }} },
        .{ .symbol_name = "closure_payload", .bytes = &payload_bytes, .alignment = builtins.erased_callable.payload_alignment, .relocations = &.{ .{ .offset = 2 * word, .target_symbol_name = proc_name, .kind = .function_pointer, .procedure = worker, .callable_capture_offset = builtins.erased_callable.capture_offset }, .{ .offset = 3 * word, .target_symbol_name = drop_name, .kind = .function_pointer, .rc_helper = .{ .op = .host_drop, .layout_idx = .str } } } },
    });
    defer static_data_exports.deinitStaticData(allocator, exports);
    const StaticInterpreterData = @import("interpreter_static_data.zig").InterpreterStaticData;
    var data = try StaticInterpreterData.init(allocator, exports, 1);
    defer data.deinit();
    var host = CompilerHost.init(allocator);
    defer host.deinit();
    var static_strings = try Interpreter.buildStaticStrings(allocator, &program.store);
    defer static_strings.deinit();
    var interpreter = try Interpreter.initWithBoxyTables(allocator, &program.store, &program.layouts, Interpreter.BoxyTables.fromResult(&program), static_strings.view(), host.ops());
    defer interpreter.deinit();
    host.bindInterpreter(&interpreter);
    data.install(&interpreter);
    data.ownByInterpreter(&interpreter);
    const first = try interpreter.eval(.{ .proc_id = caller, .ret_layout = .bool });
    try std.testing.expectEqual(@as(u8, 1), first.value.read(u8));
    const root_value = @import("value.zig").Value{ .ptr = @ptrFromInt(data.addresses[0]) };
    const payload_ptr: [*]u8 = @ptrFromInt(root_value.read(usize));
    // A caller of the erased-callable ABI passes its own RocOps, which tell the
    // callable nothing about the interpreter that owns it.
    var foreign_host = CompilerHost.init(allocator);
    defer foreign_host.deinit();
    foreign_host.bindInterpreter(&interpreter);
    const payload = builtins.erased_callable.payloadPtr(payload_ptr);
    var direct_answer: u8 = 0;
    var out_desc: ?*const anyopaque = null;
    payload.callable_fn_ptr(foreign_host.ops(), @ptrCast(&direct_answer), null, builtins.erased_callable.capturePtr(payload_ptr), null, &out_desc);
    try std.testing.expectEqual(@as(u8, 1), direct_answer);
    const Resolver = struct {
        fn resolve(_: ?*anyopaque, ptr: [*]u8) error{RuntimeError}!NativeRootExport.CallableResolution {
            const value = Interpreter.interpretedCallable(ptr).?;
            return .{ .proc = value.proc, .capture_ptr = value.capture_ptr };
        }
    };
    const copied = try NativeRootExport.freezeRoot(allocator, &program, closure_slot, .{
        .root_order = 0,
        .owner = .first,
        .request = .{ .order = 0, .module_idx = 0, .kind = .compile_time_constant, .source = undefined, .checked_type = undefined, .abi = .compile_time, .exposure = .private },
        .proc = caller,
        .ret_layout = erased_layout,
        .ret_type = undefined,
        .plan = closure_plan,
    }, root_value, .{ .context = &interpreter, .resolve = Resolver.resolve });
    defer static_data_exports.deinitStaticData(allocator, copied);
    var copied_data = try StaticInterpreterData.init(allocator, copied, 1);
    defer copied_data.deinit();
    copied_data.install(&interpreter);
    copied_data.ownByInterpreter(&interpreter);
    const second = try interpreter.eval(.{ .proc_id = caller, .ret_layout = .bool });
    try std.testing.expectEqual(@as(u8, 1), second.value.read(u8));
    // Execute only the mapped graph and its image-local procedure identities.
    // This is the same installation boundary used by interpreter shims.
    const image_bytes = try allocator.alignedAlloc(u8, collections.max_roc_alignment, 1024 * 1024);
    defer allocator.free(image_bytes);
    var fba = std.heap.FixedBufferAllocator.init(image_bytes);
    const header = try fba.allocator().create(lir.LirImage.Header);
    const image_program = try lir.LirImage.copyProgramWithStaticDataIntoBuffer(fba.allocator(), image_bytes.ptr, image_bytes.len, .mapped, &program, &.{.{ .ordinal = 0, .root_proc = caller }}, copied);
    try image_program.fillHeader(header, fba.end_index);
    var view = try lir.LirImage.viewMappedImageWithAllocator(header, image_bytes.ptr, fba.end_index, .native, allocator);
    defer view.deinit();
    var mapped_data = try StaticInterpreterData.init(allocator, view.static_data, view.static_data_value_count);
    defer mapped_data.deinit();
    var mapped_static_strings = try Interpreter.buildStaticStrings(allocator, &view.store);
    defer mapped_static_strings.deinit();
    var mapped_interpreter = try Interpreter.initWithBoxyTables(allocator, &view.store, &view.layouts, Interpreter.BoxyTables.fromImageView(&view), mapped_static_strings.view(), host.ops());
    defer mapped_interpreter.deinit();
    host.bindInterpreter(&mapped_interpreter);
    mapped_data.install(&mapped_interpreter);
    mapped_data.ownByInterpreter(&mapped_interpreter);
    var mapped_answer: u8 = 0;
    _ = try mapped_interpreter.runEntrypoint(&view, 0, null, @ptrCast(&mapped_answer));
    try std.testing.expectEqual(@as(u8, 1), mapped_answer);

    const Inspected = @import("inspected.zig");
    const roots = [_]Inspected.BoolRoot{.{ .symbol_name = "test_frozen_erased", .proc = caller, .arg_layouts = &.{}, .ret_layout = .bool }};
    const module = Inspected.BoolRootModule{ .store = &program.store, .layouts = &program.layouts, .tables = Interpreter.BoxyTables.fromResult(&program), .roots = &roots, .static_data = .{ .exports = copied, .value_count = 1 } };
    if (comptime backend.host_lir_codegen_available) {
        var dev_result = try Inspected.devEvalBoolRootModule(allocator, module, null, 1);
        defer dev_result.deinit(allocator);
        try std.testing.expect(dev_result.results[0].outcome.passed);
    }
    if (comptime builtin.os.tag != .freestanding) {
        const llvm_batch = try Inspected.llvmEvalBoolRootModules(allocator, &.{module}, .speed, .{});
        defer allocator.free(llvm_batch.expect_counts);
        defer Inspected.deinitBoolRootEvalResults(allocator, llvm_batch.results);
        try std.testing.expect(llvm_batch.results[0].outcome.passed);
    }
    // Exercise the inspected-run ownership path with procedure and drop-helper
    // relocations, then return a separate frozen string through its Str ABI.
    const result_slot: lir.LIR.StaticDataId = @fromBackingInt(@intCast(program.static_data_values.items.len));
    try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = .str });
    const inspected_closure = try program.store.addLocal(.{ .layout_idx = erased_layout });
    const inspected_bool = try program.store.addLocal(.{ .layout_idx = .bool });
    const inspected_str = try program.store.addLocal(.{ .layout_idx = .str });
    const inspected_ret = try program.store.addCFStmt(.{ .ret = .{ .value = inspected_str } }, .test_fixture);
    const inspected_load = try program.store.addCFStmt(.{ .assign_literal = .{ .target = inspected_str, .value = .{ .static_data = .{ .id = result_slot } }, .next = inspected_ret } }, .test_fixture);
    const inspected_call = try program.store.addCFStmt(.{ .assign_call_erased = .{
        .target = inspected_bool,
        .data = try program.store.addStmtRow(lir.LIR.ErasedCallData, .{
            .closure = inspected_closure,
            .args = .empty(),
            .arg_plan = arg_plan,
        }),
        .next = inspected_load,
    } }, .test_fixture);
    const inspected_body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = inspected_closure, .value = .{ .static_data = .{ .id = closure_slot } }, .next = inspected_call } }, .test_fixture);
    const inspected_root = try program.store.addProcSpec(.{ .name = .fromRaw(57), .identity = lir.LIR.ProcIdentity.forTest(1), .args = .empty(), .frame_locals = try program.store.addLocalSpan(&.{ inspected_closure, inspected_bool, inspected_str }), .body = inspected_body, .ret_layout = .str }, .none);
    const result_name = try LirProgram.staticDataSymbolName(allocator, result_slot);
    defer allocator.free(result_name);
    const result_str = builtins.str.RocStr.fromSliceSmall("relocated");
    const inspected_exports = try allocator.alloc(LirProgram.StaticDataExport, copied.len + 1);
    defer allocator.free(inspected_exports);
    @memcpy(inspected_exports[0..copied.len], copied);
    inspected_exports[copied.len] = .{ .symbol_name = result_name, .value_id = result_slot, .bytes = std.mem.asBytes(&result_str), .alignment = @alignOf(usize), .relocations = &.{} };
    const InspectedRun = @import("inspected_run.zig");
    inline for (.{ InspectedRun.Backend.interpreter, .dev, .llvm }) |kind| {
        if (comptime kind == .dev and !backend.host_lir_codegen_available) continue;
        if (comptime kind != .interpreter and builtin.os.tag == .freestanding) continue;
        const result = try InspectedRun.run(allocator, kind, .{ .store = &program.store, .layouts = &program.layouts, .main_proc = inspected_root, .static_data = inspected_exports, .static_data_value_count = 2 }, if (kind == .interpreter) .reject else {});
        defer result.deinit(allocator);
        try std.testing.expect(result.outcome == .returned);
        try std.testing.expectEqualStrings("relocated", result.outcome.returned);
    }
}

// Each backend receives bytes encoded for its explicit pointer width. The root
// has no initializer: execution must consume the supplied frozen slot.
test "inspected runners consume frozen string slots on every backend" {
    const InspectedRun = @import("inspected_run.zig");
    const allocator = std.testing.allocator;
    inline for (.{ InspectedRun.Backend.interpreter, .dev, .wasm, .llvm }) |kind| {
        if (comptime kind == .dev and !backend.host_lir_codegen_available) continue;
        if (comptime kind != .interpreter and builtin.os.tag == .freestanding) continue;
        const width: base.target.TargetUsize = if (kind == .wasm) .u32 else .native;
        var program = try LirProgram.Result.init(allocator, width);
        defer program.deinit();
        const slot: lir.LIR.StaticDataId = @fromBackingInt(@intCast(program.static_data_values.items.len));
        try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = .str });
        const value = try program.store.addLocal(.{ .layout_idx = .str });
        const ret = try program.store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture);
        const body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = value, .value = .{ .static_data = .{ .id = slot } }, .next = ret } }, .test_fixture);
        const root = try program.store.addProcSpec(.{ .name = .fromRaw(101), .identity = lir.LIR.ProcIdentity.forTest(1), .args = .empty(), .frame_locals = try program.store.addLocalSpan(&.{value}), .body = body, .ret_layout = .str }, .none);
        const name = try LirProgram.staticDataSymbolName(allocator, slot);
        defer allocator.free(name);
        const bytes = try allocator.alloc(u8, 3 * width.size());
        defer allocator.free(bytes);
        @memset(bytes, 0);
        @memcpy(bytes[0..6], "frozen");
        bytes[bytes.len - 1] = 0x80 | 6;
        const exports = [_]LirProgram.StaticDataExport{.{ .symbol_name = name, .value_id = slot, .bytes = bytes, .alignment = width.size(), .relocations = &.{} }};
        const result = try InspectedRun.run(allocator, kind, .{ .store = &program.store, .layouts = &program.layouts, .main_proc = root, .static_data = &exports, .static_data_value_count = 1 }, if (kind == .interpreter) .reject else {});
        defer result.deinit(allocator);
        try std.testing.expect(result.outcome == .returned);
        try std.testing.expectEqualStrings("frozen", result.outcome.returned);
    }
}

// No target has a float remainder instruction, so the object LLVM compiles
// calls the C math library for one, and the in-process loader has to bind that
// call. Each operand is derived from the root's argument, which only exists
// when the root runs, so the remainder cannot be folded away before then.
test "LLVM test roots run a float remainder in process" {
    if (comptime builtin.os.tag == .freestanding) return error.SkipZigTest;
    const Inspected = @import("inspected.zig");
    const allocator = std.testing.allocator;
    var program = try LirProgram.Result.init(allocator, .native);
    defer program.deinit();
    const store = &program.store;

    var roots: [2]Inspected.BoolRoot = undefined;
    inline for (.{ lir.LIR.LiteralValue{ .f64_literal = 7.5 }, lir.LIR.LiteralValue{ .f32_literal = 7.5 } }, 0..) |dividend_literal, index| {
        const wide = index == 0;
        const float_layout: @FieldType(Inspected.BoolRoot, "ret_layout") = if (wide) .f64 else .f32;
        const offset = try store.addLocal(.{ .layout_idx = float_layout });
        const dividend_base = try store.addLocal(.{ .layout_idx = float_layout });
        const dividend = try store.addLocal(.{ .layout_idx = float_layout });
        const divisor_base = try store.addLocal(.{ .layout_idx = float_layout });
        const divisor = try store.addLocal(.{ .layout_idx = float_layout });
        const remainder = try store.addLocal(.{ .layout_idx = float_layout });
        const expected = try store.addLocal(.{ .layout_idx = float_layout });
        const matches = try store.addLocal(.{ .layout_idx = .bool });

        const ret = try store.addCFStmt(.{ .ret = .{ .value = matches } }, .test_fixture);
        const compare = try store.addLowLevelStmt(matches, .num_is_eq, &.{ remainder, expected }, ret, .test_fixture);
        const expect_literal = try store.addCFStmt(.{ .assign_literal = .{
            .target = expected,
            .value = if (wide) .{ .f64_literal = 1.5 } else .{ .f32_literal = 1.5 },
            .next = compare,
        } }, .test_fixture);
        const rem = try store.addLowLevelStmt(remainder, .num_rem_by, &.{ dividend, divisor }, expect_literal, .test_fixture);
        const add_divisor = try store.addLowLevelStmt(divisor, .num_float_add, &.{ offset, divisor_base }, rem, .test_fixture);
        const divisor_literal = try store.addCFStmt(.{ .assign_literal = .{
            .target = divisor_base,
            .value = if (wide) .{ .f64_literal = 2.0 } else .{ .f32_literal = 2.0 },
            .next = add_divisor,
        } }, .test_fixture);
        const add_dividend = try store.addLowLevelStmt(dividend, .num_float_add, &.{ offset, dividend_base }, divisor_literal, .test_fixture);
        const body = try store.addCFStmt(.{ .assign_literal = .{
            .target = dividend_base,
            .value = dividend_literal,
            .next = add_dividend,
        } }, .test_fixture);
        const proc = try store.addProcSpec(.{
            .name = .fromRaw(201 + index),
            .identity = lir.LIR.ProcIdentity.forTest(@intCast(index + 1)),
            .args = try store.addLocalSpan(&.{offset}),
            .frame_locals = try store.addLocalSpan(&.{ offset, dividend_base, dividend, divisor_base, divisor, remainder, expected, matches }),
            .body = body,
            .ret_layout = .bool,
        }, .none);
        roots[index] = .{
            .symbol_name = if (wide) "test_float_rem_f64" else "test_float_rem_f32",
            .proc = proc,
            .arg_layouts = if (wide) &.{.f64} else &.{.f32},
            .ret_layout = .bool,
        };
    }

    const module = Inspected.BoolRootModule{ .store = &program.store, .layouts = &program.layouts, .tables = Interpreter.BoxyTables.fromResult(&program), .roots = &roots };
    inline for (.{ Inspected.LlvmTestOpt.size, Inspected.LlvmTestOpt.speed }) |opt| {
        const batch = try Inspected.llvmEvalBoolRootModules(allocator, &.{module}, opt, .{});
        defer allocator.free(batch.expect_counts);
        const results = batch.results;
        defer Inspected.deinitBoolRootEvalResults(allocator, results);
        try std.testing.expectEqual(roots.len, results.len);
        for (results) |result| {
            try std.testing.expect(result.outcome == .passed);
            try std.testing.expect(result.outcome.passed);
        }
    }
}
