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
const boxy_abi = @import("boxy_abi.zig");
const interpreter_mod = @import("interpreter.zig");
const static_data_exports = @import("static_data");
const Interpreter = interpreter_mod.Interpreter;
const ExpectFailure = interpreter_mod.ExpectFailure;
const FinalizeError = checked.CompileTimeFinalizer.Error;
const LirProgram = lir.Program;
const BoxyBuiltinFn = backend.LirCodeGenMod.BoxyBuiltinFn;
const BoxyNativeFnTable = backend.LirCodeGenMod.BoxyNativeFnTable;

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
    /// Completed checked artifacts contribute stored observations, without
    /// evaluating their roots again. Borrowed only for finalization.
    cached_debug_modules: []const *const checked.CheckedModuleArtifact = &.{},
    std_io: ?std.Io = null,
    slow_root_threshold_ns: u64 = 3 * std.time.ns_per_s,
    slow_root_period_ns: u64 = std.time.ns_per_s,
    timing: ?*Timing = null,
};

const DebugEvents = struct {
    const Event = struct { module: checked.ModuleId, root: checked.ComptimeRootId, sequence: usize, is_repl: bool, message: []const u8 };
    allocator: Allocator,
    events: std.ArrayList(Event) = .empty,

    fn deinit(self: *DebugEvents) void {
        for (self.events.items) |event| self.allocator.free(event.message);
        self.events.deinit(self.allocator);
    }

    fn append(self: *DebugEvents, module: checked.ModuleId, root: checked.CompileTimeRoot, message: []const u8) Allocator.Error!void {
        const owned = try self.allocator.dupe(u8, message);
        errdefer self.allocator.free(owned);
        try self.events.append(self.allocator, .{ .module = module, .root = root.id, .sequence = self.events.items.len, .is_repl = root.kind == .repl_expr, .message = owned });
    }

    fn lessThan(_: void, a: Event, b: Event) bool {
        const order = std.mem.order(u8, &a.module.bytes, &b.module.bytes);
        if (order != .eq) return order == .lt;
        if (a.root != b.root) return @intFromEnum(a.root) < @intFromEnum(b.root);
        return a.sequence < b.sequence;
    }

    fn appendCached(self: *DebugEvents, modules: []const *const checked.CheckedModuleArtifact) Allocator.Error!void {
        for (modules) |module| for (module.compile_time_debug.entries) |entry| {
            try self.append(module.key, module.compile_time_roots.root(entry.root), module.compile_time_debug.message(entry));
        };
    }

    fn persist(self: *DebugEvents, modules: []const ProgramModule) Allocator.Error!void {
        std.mem.sort(Event, self.events.items, {}, lessThan);
        var inputs = std.ArrayList(checked.CompileTimeDebugStore.Input).empty;
        defer inputs.deinit(self.allocator);
        for (modules) |entry| {
            inputs.clearRetainingCapacity();
            for (self.events.items) |event| {
                if (std.meta.eql(entry.module.key, event.module)) try inputs.append(self.allocator, .{ .root = event.root, .message = event.message });
            }
            // The artifact owns these bytes for cache publication and replay.
            const artifact_allocator = entry.module.canonical_names.allocator;
            const stored = try checked.CompileTimeDebugStore.init(artifact_allocator, inputs.items);
            entry.module.compile_time_debug.deinit(artifact_allocator);
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

/// Retains the compilation's specialization and host lowering across checking
/// completion. Diagnostic destinations are consumed during finalizeProgram.
pub const ProgramSession = struct {
    allocator: Allocator,
    modules: lir.CheckedPipeline.CheckedModuleSet,
    runtime_requests: []const checked.RootRequest,
    runtime_roots: lir.CheckedPipeline.RootRequestSet,
    runtime_target: ?lir.CheckedPipeline.TargetConfig,
    host: ?lir.CheckedPipeline.LoweredProgram,
    runtime_prepared: ?lir.CheckedPipeline.PreparedMonotype,
    compile_time_root_count: usize,

    pub fn deinit(self: *ProgramSession) void {
        if (self.host) |*host| host.deinit();
        if (self.runtime_prepared) |*prepared| prepared.deinit();
        self.allocator.free(self.modules.root.relation_modules);
        self.allocator.free(self.modules.imports);
        deinitRootRequests(self.allocator, self.runtime_roots);
        self.* = undefined;
    }

    pub fn takeRuntime(
        self: *ProgramSession,
        allocator: Allocator,
        roots: lir.CheckedPipeline.RootRequestSet,
        target: lir.CheckedPipeline.TargetConfig,
    ) lir.CheckedPipeline.LowerResourceError!lir.CheckedPipeline.LoweredProgram {
        const configured = self.runtime_target orelse finalizationInvariant("check-only session has no runtime consumer");
        inline for (@typeInfo(lir.CheckedPipeline.TargetConfig).@"struct".fields) |field| {
            if (comptime std.mem.eql(u8, field.name, "timing") or
                std.mem.eql(u8, field.name, "work_metrics") or
                std.mem.eql(u8, field.name, "post_check_executor") or
                std.mem.eql(u8, field.name, "debug_materialized_out") or
                std.mem.eql(u8, field.name, "solved_lir_parallel_metrics_out") or
                std.mem.eql(u8, field.name, "lifted_expr_count_out"))
            {
                // A completed host program has already published its outputs.
                // Reusing it cannot silently redirect those results or count
                // its producer work again in another metrics destination.
                if (comptime !std.mem.eql(u8, field.name, "timing") and
                    !std.mem.eql(u8, field.name, "post_check_executor"))
                {
                    const reuses_completed_host = target.specialization_strategy == .lss and
                        self.compile_time_root_count != 0 and self.runtime_prepared == null;
                    if (reuses_completed_host and !std.meta.eql(@field(configured, field.name), @field(target, field.name)))
                        finalizationInvariant("completed runtime program cannot redirect previously published lowering outputs");
                }
                continue;
            }
            if (!std.meta.eql(@field(configured, field.name), @field(target, field.name)))
                finalizationInvariant("runtime policy differs from the compilation's declared consumer");
        }
        inline for (@typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".fields) |field| {
            const expected = @field(self.runtime_roots, field.name);
            const actual = @field(roots, field.name);
            if (@typeInfo(field.type) == .pointer) {
                if (expected.len != actual.len) finalizationInvariant("runtime request differs from the declared consumer");
                for (expected, actual) |a, b| {
                    if (!std.meta.eql(a, b)) finalizationInvariant("runtime root metadata differs from the declared consumer");
                }
            } else if (expected != actual) finalizationInvariant("runtime request policy differs from the declared consumer");
        }
        if (target.specialization_strategy == .boxy or self.compile_time_root_count == 0) {
            self.runtime_target = null;
            return lir.CheckedPipeline.lowerCheckedModulesToLir(allocator, self.modules, roots, target);
        }
        const needs_conversion = self.runtime_prepared != null;
        var lowered = if (self.runtime_prepared) |owned_prepared| block: {
            self.runtime_prepared = null;
            var prepared = owned_prepared;
            // Consumer-local observers and workers may change between the
            // prepared phase and the continuation. Semantic options above are
            // fixed, while these live destinations belong to this invocation.
            inline for (.{ "timing", "work_metrics", "post_check_executor", "debug_materialized_out", "solved_lir_parallel_metrics_out", "lifted_expr_count_out" }) |field| {
                @field(prepared.target, field) = @field(target, field);
            }
            break :block try lir.CheckedPipeline.lowerPreparedMonotypeToLir(prepared);
        } else block: {
            const host = self.host orelse finalizationInvariant("runtime program was already consumed");
            self.host = null;
            break :block host;
        };
        errdefer lowered.deinit();
        if (needs_conversion) {
            const source = if (self.host) |*host| host else finalizationInvariant("target consumer omitted its completed host program");
            lowered.frozen_static_data = try transcodeCompletedSlots(allocator, source, &lowered);
        }
        const start = self.compile_time_root_count;
        const runtime_count = self.runtime_requests.len;
        if (lowered.lir_result.root_procs.items.len != start + runtime_count)
            finalizationInvariant("union root lowering changed the requested root count");
        const runtime_indices = try allocator.alloc(u32, runtime_count);
        defer allocator.free(runtime_indices);
        for (runtime_indices, 0..) |*index, ordinal| index.* = @intCast(start + ordinal);
        try lir.CheckedPipeline.retainRuntimeRoots(&lowered, runtime_indices);
        self.runtime_target = null;
        return lowered;
    }
};

/// Match completed values by their checked owner and root identity. Target
/// representation comes exclusively from the paired slot plans.
fn transcodeCompletedSlots(
    allocator: Allocator,
    source: *const lir.CheckedPipeline.LoweredProgram,
    target: *lir.CheckedPipeline.LoweredProgram,
) Allocator.Error!LirProgram.FrozenStaticData {
    const frozen = source.frozen_static_data orelse finalizationInvariant("host program omitted its completed frozen values");
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
    for (target.lir_result.static_data_values.items, 0..) |target_entry, index| {
        const target_root = target_entry.compile_time_root orelse continue;
        const target_slot: lir.LIR.StaticDataId = @enumFromInt(index);
        var source_index: ?usize = null;
        for (source.lir_result.static_data_values.items, 0..) |source_entry, ordinal| {
            const source_root = source_entry.compile_time_root orelse continue;
            if (!std.meta.eql(target_root.module, source_root.module) or target_root.root != source_root.root or
                std.meta.activeTag(target_root.role) != std.meta.activeTag(source_root.role)) continue;
            if (source_index != null) finalizationInvariant("checked root has ambiguous source value slots");
            source_index = ordinal;
        }
        const ordinal = source_index orelse finalizationInvariant("target slot has no corresponding host root");
        const source_entry = source.lir_result.static_data_values.items[ordinal];
        const source_symbol = frozenSlotSymbol(frozen.exports, @enumFromInt(ordinal));
        const failed = if (target_root.role == .value) block: {
            const failure_slot = source_entry.compile_time_root.?.role.value.failure_slot;
            const failure_entry = source.lir_result.static_data_values.items[@intFromEnum(failure_slot)];
            const failure_symbol = frozenSlotSymbol(frozen.exports, failure_slot);
            const failure_export = frozen.exports[@intFromEnum(failure_symbol)];
            const offset = failure_entry.compile_time_root.?.role.failure_message.failed_offset;
            break :block failure_export.bytes[failure_export.symbol_offset + offset] != 0;
        } else false;
        const converted = if (target_root.role == .failure_message)
            try FrozenRootTranscode.transcodeFailure(allocator, &source.lir_result, source_entry, frozen.exports, source_symbol, &target.lir_result, target_slot)
        else if (failed)
            try uninitializedFailedSlot(allocator, &target.lir_result, target_slot)
        else
            try FrozenRootTranscode.transcodeValueSlot(allocator, &source.lir_result, source_entry, frozen.exports, source_symbol, &target.lir_result, target_slot);
        appendFrozenGraph(allocator, &exports, converted) catch |err| {
            static_data_exports.deinitStaticData(allocator, converted);
            return err;
        };
        allocator.free(converted);
        if (target_root.role == .value and !failed) lir.ComptimeValueGuards.completeSuccessfulSlot(&target.lir_result, target_slot);
    }
    return .{ .allocator = allocator, .exports = try exports.toOwnedSlice(allocator) };
}

fn frozenSlotSymbol(exports: []const static_data_exports.StaticDataExport, slot: lir.LIR.StaticDataId) static_data_exports.StaticDataSymbolId {
    for (exports, 0..) |item, index| if (item.value_id == slot) return @enumFromInt(index);
    finalizationInvariant("completed frozen graph omitted a declared slot");
}

/// Failed values have no representation to convert. Their explicit guard
/// owns the failure; reserve the destination slot without inspecting its bytes.
fn uninitializedFailedSlot(allocator: Allocator, program: *const LirProgram.Result, slot: lir.LIR.StaticDataId) Allocator.Error![]static_data_exports.StaticDataExport {
    const entry = program.static_data_values.items[@intFromEnum(slot)];
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
        if (relocation.target == .data_symbol) relocation.target.data_symbol = @enumFromInt(start + @intFromEnum(relocation.target.data_symbol));
    };
    destination.appendSliceAssumeCapacity(graph);
}

fn cloneRootRequests(allocator: Allocator, roots: lir.CheckedPipeline.RootRequestSet) Allocator.Error!lir.CheckedPipeline.RootRequestSet {
    var owned = lir.CheckedPipeline.RootRequestSet{
        .include_provided_data_exports = roots.include_provided_data_exports,
        .include_internal_static_data = roots.include_internal_static_data,
    };
    errdefer deinitRootRequests(allocator, owned);
    inline for (@typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".fields) |field| {
        if (@typeInfo(field.type) == .pointer) {
            @field(owned, field.name) = try allocator.dupe(@typeInfo(field.type).pointer.child, @field(roots, field.name));
        }
    }
    return owned;
}

fn deinitRootRequests(allocator: Allocator, roots: lir.CheckedPipeline.RootRequestSet) void {
    inline for (@typeInfo(lir.CheckedPipeline.RootRequestSet).@"struct".fields) |field| {
        if (@typeInfo(field.type) == .pointer) allocator.free(@field(roots, field.name));
    }
}

/// Lower the union of checking and runtime roots once, using the released
/// frontend workers, then complete checked values in the caller's dependency order.
pub fn finalizeProgram(
    allocator: Allocator,
    modules: []const ProgramModule,
    lowering_modules: lir.CheckedPipeline.CheckedModuleSet,
    runtime_roots: lir.CheckedPipeline.RootRequestSet,
    runtime_target: ?lir.CheckedPipeline.TargetConfig,
    options: Options,
) FinalizeError!ProgramSession {
    const total_started_ns = if (options.timing) |timing| timing.start() else 0;
    defer if (options.timing) |timing| timing.finish(total_started_ns, .total);
    var debug_events = DebugEvents{ .allocator = allocator };
    defer debug_events.deinit();
    try debug_events.appendCached(options.cached_debug_modules);
    var requests = std.ArrayList(checked.RootRequest).empty;
    defer requests.deinit(allocator);
    var source_modules = std.ArrayList(checked.ModuleId).empty;
    defer source_modules.deinit(allocator);
    for (modules) |entry| {
        try requests.appendSlice(allocator, entry.module.root_requests.compile_time_requests);
        try source_modules.appendNTimes(allocator, entry.module.key, entry.module.root_requests.compile_time_requests.len);
    }
    const compile_time_root_count = requests.items.len;
    const share_runtime = if (runtime_target) |target| target.specialization_strategy == .lss else false;
    const lowering_runtime_roots = if (share_runtime) runtime_roots else lir.CheckedPipeline.RootRequestSet{};
    try requests.appendSlice(allocator, lowering_runtime_roots.requests);
    if (lowering_runtime_roots.source_modules.len == 0) {
        try source_modules.appendNTimes(allocator, lowering_modules.root.module.key, lowering_runtime_roots.requests.len);
    } else {
        if (lowering_runtime_roots.source_modules.len != lowering_runtime_roots.requests.len) finalizationInvariant("runtime roots omitted their checked owners");
        try source_modules.appendSlice(allocator, lowering_runtime_roots.source_modules);
    }
    const owned_imports = try allocator.dupe(checked.ImportedModuleView, lowering_modules.imports);
    errdefer allocator.free(owned_imports);
    const owned_relations = try allocator.dupe(checked.ImportedModuleView, lowering_modules.root.relation_modules);
    errdefer allocator.free(owned_relations);
    const owned_runtime_roots = try cloneRootRequests(allocator, runtime_roots);
    errdefer deinitRootRequests(allocator, owned_runtime_roots);
    const owned_runtime_requests = owned_runtime_roots.requests;
    if (compile_time_root_count == 0) {
        for (modules) |entry| {
            if (entry.problem_store) |store| _ = try store.flushPendingStaticExhaustiveness(allocator);
            try entry.module.const_store.verifyComplete();
        }
        try debug_events.persist(modules);
        try debug_events.replay(options);
        var retained_root = lowering_modules.root;
        retained_root.relation_modules = owned_relations;
        const retained_roots = owned_runtime_roots;
        return .{
            .allocator = allocator,
            .modules = .{ .root = retained_root, .imports = owned_imports },
            .runtime_requests = owned_runtime_requests,
            .runtime_roots = retained_roots,
            .runtime_target = runtime_target,
            .host = null,
            .runtime_prepared = null,
            .compile_time_root_count = 0,
        };
    }
    var union_roots = lowering_runtime_roots;
    union_roots.requests = requests.items;
    union_roots.source_modules = source_modules.items;
    const union_test_metadata = try allocator.dupe(@typeInfo(@TypeOf(lowering_runtime_roots.test_plan_metadata)).pointer.child, lowering_runtime_roots.test_plan_metadata);
    defer allocator.free(union_test_metadata);
    for (union_test_metadata) |*metadata| metadata.request_index += @intCast(compile_time_root_count);
    union_roots.test_plan_metadata = union_test_metadata;
    var host_target = runtime_target orelse lir.CheckedPipeline.TargetConfig{};
    host_target.target_usize = base.target.TargetUsize.native;
    host_target.specialization_strategy = .lss;
    host_target.checked_module_state = .checking_finalization;
    host_target.comptime_value_reads = true;
    host_target.inline_expects = .run;
    host_target.post_check_executor = options.post_check_executor;
    host_target.timing = if (options.timing) |timing| &timing.lowering else null;
    var prepared = lir.CheckedPipeline.prepareCheckedModulesMonotype(allocator, lowering_modules, union_roots, host_target) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.HostedFunctionNotBound => finalizationInvariant("prepared program contains an unbound hosted declaration"),
    };
    var prepared_owned = true;
    errdefer if (prepared_owned) prepared.deinit();
    var runtime_prepared: ?lir.CheckedPipeline.PreparedMonotype = null;
    errdefer if (runtime_prepared) |*owned| owned.deinit();
    if (runtime_target) |target| {
        if (target.specialization_strategy == .lss and
            (target.target_usize != host_target.target_usize or target.inline_expects != .run))
            runtime_prepared = try prepared.forkForConsumer(target.target_usize, target.inline_expects);
    }
    prepared_owned = false;
    var host = lir.CheckedPipeline.lowerPreparedMonotypeToLir(prepared) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.HostedFunctionNotBound => unreachable,
    };
    errdefer host.deinit();
    var evaluation_options = options;
    evaluation_options.debug_events = &debug_events;
    if (compile_time_root_count != 0) {
        if (comptime compilerHostMustUseInterpreterForCtfe()) {
            const interpreted = try InterpreterProgram.init(allocator, lowering_modules, &host, evaluation_options);
            defer interpreted.deinit();
            try finalizeLoweredProgram(allocator, modules, &host, compile_time_root_count, interpreted, evaluation_options);
            host.frozen_static_data = try interpreted.slots.freezeCompleted();
        } else {
            if (comptime !backend.host_lir_codegen_available) return error.UnsupportedPlatform;
            var native = try DevProgram.init(allocator, lowering_modules, &host, options);
            defer native.deinit();
            native.codegen.static_strings = native.static_strings.view();
            try finalizeLoweredProgram(allocator, modules, &host, compile_time_root_count, &native, evaluation_options);
            host.frozen_static_data = try native.freezeCompleted();
        }
    } else {
        for (modules) |entry| {
            if (entry.problem_store) |store| _ = try store.flushPendingStaticExhaustiveness(allocator);
            try entry.module.const_store.verifyComplete();
        }
    }
    try debug_events.persist(modules);
    try debug_events.replay(options);
    var retained_root = lowering_modules.root;
    retained_root.relation_modules = owned_relations;
    const retained_roots = owned_runtime_roots;
    return .{
        .allocator = allocator,
        .modules = .{ .root = retained_root, .imports = owned_imports },
        .runtime_requests = owned_runtime_requests,
        .runtime_roots = retained_roots,
        .runtime_target = runtime_target,
        .host = host,
        .runtime_prepared = runtime_prepared,
        .compile_time_root_count = compile_time_root_count,
    };
}

/// A slot read demands its declared producer before observing its storage.
const SlotDemand = CompileTimeHost.SlotDemand;

fn finalizeLoweredProgram(
    allocator: Allocator,
    modules: []const ProgramModule,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    root_count: usize,
    program: anytype,
    options: Options,
) FinalizeError!void {
    const Driver = struct {
        const Self = @This();
        const State = struct {
            completion: RootCompletionState,
            coverage: ComptimeCoverage,
            offset: usize,
        };
        const Root = struct { module: usize, request: usize };
        allocator: Allocator,
        modules: []const ProgramModule,
        lowered: *lir.CheckedPipeline.LoweredProgram,
        program: @TypeOf(program),
        options: Options,
        states: []State,
        roots: []Root,
        active: []bool,
        slot_roots: []?usize,

        fn ensure(context: *anyopaque, slot: lir.LIR.StaticDataId) (FinalizeError || error{CompileTimeDependencyCycle})!void {
            const self: *Self = @ptrCast(@alignCast(context));
            const ordinal = self.slot_roots[@intFromEnum(slot)] orelse return;
            try self.evaluate(ordinal);
        }

        fn evaluate(self: *Self, ordinal: usize) (FinalizeError || error{CompileTimeDependencyCycle})!void {
            const root = self.roots[ordinal];
            const entry = self.modules[root.module];
            const state = &self.states[root.module];
            const id = state.completion.request_root_ids[root.request];
            if (state.completion.isDone(id)) return;
            if (self.active[ordinal]) return error.CompileTimeDependencyCycle;
            self.active[ordinal] = true;
            defer self.active[ordinal] = false;
            const requests = entry.module.root_requests.compile_time_requests;
            _ = try evalProgramRoots(self.allocator, entry.module, requests[root.request..][0..1], state.completion.request_root_ids[root.request..][0..1], &state.completion, entry.problem_store, &state.coverage, self.options, self.lowered, self.lowered.lir_result.const_roots.items[ordinal..][0..1], self.program);
            if (!state.completion.isDone(id)) finalizationInvariant("demanded compile-time producer did not complete");
        }
    };
    const states = try allocator.alloc(Driver.State, modules.len);
    var initialized: usize = 0;
    defer {
        for (states[0..initialized]) |*state| {
            state.completion.deinit();
            state.coverage.deinit();
        }
        allocator.free(states);
    }
    const roots = try allocator.alloc(Driver.Root, root_count);
    defer allocator.free(roots);
    const active = try allocator.alloc(bool, root_count);
    defer allocator.free(active);
    @memset(active, false);
    const slot_roots = try allocator.alloc(?usize, lowered.lir_result.static_data_values.items.len);
    defer allocator.free(slot_roots);
    @memset(slot_roots, null);
    const Key = struct { module: checked.ModuleId, root: checked.ComptimeRootId };
    var root_indices = std.AutoHashMap(Key, usize).init(allocator);
    defer root_indices.deinit();
    var offset: usize = 0;
    for (modules, states, 0..) |entry, *state, module_index| {
        state.* = .{ .completion = try RootCompletionState.init(allocator, entry.module), .coverage = ComptimeCoverage.init(allocator), .offset = offset };
        initialized += 1;
        for (state.completion.request_root_ids, 0..) |id, request_index| {
            roots[offset] = .{ .module = module_index, .request = request_index };
            try root_indices.put(.{ .module = entry.module.key, .root = id }, offset);
            offset += 1;
        }
    }
    if (offset != root_count) finalizationInvariant("demand root partitions differ from lowering requests");
    for (lowered.lir_result.static_data_values.items, slot_roots) |slot, *owner| {
        const root = slot.compile_time_root orelse continue;
        owner.* = root_indices.get(.{ .module = root.module, .root = root.root }) orelse
            finalizationInvariant("compile-time slot has no declared producer");
    }
    var driver: Driver = .{ .allocator = allocator, .modules = modules, .lowered = lowered, .program = program, .options = options, .states = states, .roots = roots, .active = active, .slot_roots = slot_roots };
    program.slot_demand = .{ .context = &driver, .ensure = Driver.ensure };
    defer program.slot_demand = null;
    for (0..root_count) |ordinal| {
        driver.evaluate(ordinal) catch |err| switch (err) {
            error.CompileTimeDependencyCycle => finalizationInvariant("slot demand cycle escaped its execution host"),
            else => |operational| return operational,
        };
    }
    for (modules, states) |entry, *state| {
        try state.coverage.reportUnusedBranches(allocator, entry.problem_store);
        if (entry.problem_store) |store| _ = try store.flushPendingStaticExhaustiveness(allocator);
        try entry.module.const_store.verifyComplete();
    }
}

/// Thread-safe timing totals accumulated across compile-time root batches.
pub const Timing = struct {
    std_io: std.Io,
    lowering: lir.CheckedPipeline.Timing,
    total_ns: TimingCounter = .{},
    static_data_ns: TimingCounter = .{},
    code_generation_ns: TimingCounter = .{},
    execution_ns: TimingCounter = .{},
    store_results_ns: TimingCounter = .{},
    /// Process footprint range observed at burst boundaries. Compile-time
    /// evaluation runs as bursts interleaved with checking, so the progress
    /// reporter cannot window-sample it; the brackets that already time each
    /// burst fold a footprint reading at the same points.
    mem_min: MemMinCounter = MemMinCounter.init(std.math.maxInt(u64)),
    mem_max: MemMaxCounter = .{},

    pub fn init(std_io: std.Io) Timing {
        return .{
            .std_io = std_io,
            .lowering = lir.CheckedPipeline.Timing.init(std_io),
        };
    }

    pub fn snapshot(self: *const Timing) TimingSnapshot {
        const lowering = self.lowering.snapshot();
        return .{
            .total_ns = self.total_ns.load(),
            .monotype_ns = lowering.monotype_ns,
            .postcheck_to_lir_ns = lowering.lift_ns + lowering.spec_constr_ns + lowering.lambda_solve_ns + lowering.inline_plan_ns + lowering.lir_gen_ns,
            .lir_passes_ns = lowering.lir_passes_ns,
            .arc_ns = lowering.arc_ns,
            .static_data_ns = self.static_data_ns.load(),
            .code_generation_ns = self.code_generation_ns.load(),
            .execution_ns = self.execution_ns.load(),
            .store_results_ns = self.store_results_ns.load(),
            .mem_min = self.mem_min.load(),
            .mem_max = self.mem_max.load(),
        };
    }

    pub fn addSnapshot(self: *Timing, snapshot_value: TimingSnapshot) void {
        self.lowering.addSnapshot(.{
            .monotype_ns = snapshot_value.monotype_ns,
            // The compile-time evaluation report shows lowering as one
            // category; re-attribute the merged span to its first stage.
            .lift_ns = snapshot_value.postcheck_to_lir_ns,
            .lir_passes_ns = snapshot_value.lir_passes_ns,
            .arc_ns = snapshot_value.arc_ns,
        });
        self.total_ns.add(snapshot_value.total_ns);
        self.static_data_ns.add(snapshot_value.static_data_ns);
        self.code_generation_ns.add(snapshot_value.code_generation_ns);
        self.execution_ns.add(snapshot_value.execution_ns);
        self.store_results_ns.add(snapshot_value.store_results_ns);
        if (snapshot_value.mem_min != std.math.maxInt(u64)) self.mem_min.min(snapshot_value.mem_min);
        self.mem_max.max(snapshot_value.mem_max);
    }

    fn start(self: *Timing) i64 {
        self.sampleMemory();
        return nowNs(self.std_io);
    }

    fn sampleMemory(self: *Timing) void {
        const bytes = base.process_memory.currentBytes() orelse return;
        self.mem_min.min(bytes);
        self.mem_max.max(bytes);
    }

    fn finishExecution(self: *Timing, started_ns: i64, suspended_ns: u64) void {
        self.sampleMemory();
        const elapsed: u64 = @intCast(@max(0, nowNs(self.std_io) - started_ns));
        self.execution_ns.add(elapsed -| suspended_ns);
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
    monotype_ns: u64 = 0,
    postcheck_to_lir_ns: u64 = 0,
    lir_passes_ns: u64 = 0,
    arc_ns: u64 = 0,
    static_data_ns: u64 = 0,
    code_generation_ns: u64 = 0,
    execution_ns: u64 = 0,
    store_results_ns: u64 = 0,
    mem_min: u64 = std.math.maxInt(u64),
    mem_max: u64 = 0,
};

const TimingPhase = enum {
    total,
    static_data,
    code_generation,
    execution,
    store_results,
};

const ComptimeCoverage = struct {
    allocator: Allocator,
    entries: std.ArrayList(Entry),

    const Entry = struct {
        kind: lir.LIR.ComptimeSiteKind,
        region: base.Region,
        branch_regions: []base.Region,
        hits: []bool,
    };

    fn init(allocator: Allocator) ComptimeCoverage {
        return .{
            .allocator = allocator,
            .entries = .empty,
        };
    }

    fn deinit(self: *ComptimeCoverage) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry.branch_regions);
            self.allocator.free(entry.hits);
        }
        self.entries.deinit(self.allocator);
    }

    fn record(self: *ComptimeCoverage, site: lir.LIR.ComptimeSite, branch_index: u32) Allocator.Error!void {
        if (site.kind != .match and site.kind != .if_) return;
        if (site.branch_regions.len == 0) return;
        if (branch_index >= site.branch_regions.len) {
            finalizationInvariant("compile-time branch hit referenced a branch outside its site");
        }

        const entry = try self.entryFor(site);
        entry.hits[branch_index] = true;
    }

    fn entryFor(self: *ComptimeCoverage, site: lir.LIR.ComptimeSite) Allocator.Error!*Entry {
        for (self.entries.items) |*entry| {
            if (entry.kind == site.kind and regionsEqual(entry.region, site.region)) {
                if (entry.branch_regions.len != site.branch_regions.len) {
                    finalizationInvariant("compile-time site branch count changed for one source region");
                }
                return entry;
            }
        }

        const branch_regions = try self.allocator.dupe(base.Region, site.branch_regions);
        errdefer self.allocator.free(branch_regions);
        const hits = try self.allocator.alloc(bool, site.branch_regions.len);
        errdefer self.allocator.free(hits);
        @memset(hits, false);

        try self.entries.append(self.allocator, .{
            .kind = site.kind,
            .region = site.region,
            .branch_regions = branch_regions,
            .hits = hits,
        });
        return &self.entries.items[self.entries.items.len - 1];
    }

    fn reportUnusedBranches(self: *const ComptimeCoverage, allocator: Allocator, problem_store: ?*check.problem.Store) Allocator.Error!void {
        const store = problem_store orelse return;
        for (self.entries.items) |entry| {
            for (entry.hits, 0..) |hit, index| {
                if (hit) continue;
                _ = try store.appendProblem(allocator, .{ .comptime_unused_branch = .{
                    .kind = switch (entry.kind) {
                        .match => .match,
                        .if_ => .if_,
                        .destructure => unreachable,
                    },
                    .site_region = entry.region,
                    .branch_region = entry.branch_regions[index],
                } });
            }
        }
    }
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
        var coverage = ComptimeCoverage.init(allocator);
        defer coverage.deinit();

        const lowering_imports = try finalizationImports(allocator, checked.importedView(module), imports, available_modules);
        defer allocator.free(lowering_imports);

        var state = try RootCompletionState.init(allocator, module);
        defer state.deinit();

        var batch_requests = std.ArrayList(checked.RootRequest).empty;
        defer batch_requests.deinit(allocator);

        var batch_root_ids = std.ArrayList(checked.ComptimeRootId).empty;
        defer batch_root_ids.deinit(allocator);

        for (requests, 0..) |request, request_index| {
            const root_id = state.rootIdForRequestIndex(request_index);
            if (!state.dependenciesComplete(request)) {
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
                    &coverage,
                    options,
                );
                batch_requests.clearRetainingCapacity();
                batch_root_ids.clearRetainingCapacity();

                if (!state.dependenciesComplete(request)) {
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
                &coverage,
                options,
            );
        }

        try coverage.reportUnusedBranches(allocator, problem_store);
    }

    if (problem_store) |store| {
        _ = try store.flushPendingStaticExhaustiveness(allocator);
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
    visited_templates: []u32,
    visit: u32,
    current_root_id: ?checked.ComptimeRootId = null,

    fn init(
        allocator: Allocator,
        module: *checked.CheckedModuleArtifact,
    ) Allocator.Error!RootCompletionState {
        const statuses = try allocator.alloc(RootStatus, module.compile_time_roots.roots.len);
        errdefer allocator.free(statuses);
        @memset(statuses, .pending);

        const requested_roots = try allocator.alloc(bool, module.compile_time_roots.roots.len);
        errdefer allocator.free(requested_roots);
        @memset(requested_roots, false);

        const request_root_ids = try allocator.alloc(checked.ComptimeRootId, module.root_requests.compile_time_requests.len);
        errdefer allocator.free(request_root_ids);
        for (module.root_requests.compile_time_requests, 0..) |request, i| {
            const root_id = compileTimeRootForRequest(module, request);
            const raw = @intFromEnum(root_id);
            if (requested_roots[raw]) {
                finalizationInvariant("compile-time root was requested more than once");
            }
            requested_roots[raw] = true;
            request_root_ids[i] = root_id;
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
            .visited_templates = visited_templates,
            .visit = 0,
        };
    }

    fn deinit(self: *RootCompletionState) void {
        const allocator = self.allocator;
        allocator.free(self.visited_templates);
        allocator.free(self.request_root_ids);
        allocator.free(self.requested_roots);
        allocator.free(self.statuses);
        self.* = undefined;
    }

    fn isDone(self: *const RootCompletionState, root_id: checked.ComptimeRootId) bool {
        return self.statuses[@intFromEnum(root_id)] == .done;
    }

    fn markDone(self: *RootCompletionState, root_id: checked.ComptimeRootId) void {
        self.statuses[@intFromEnum(root_id)] = .done;
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
    ) bool {
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
        return self.templateDependenciesComplete(template_ref);
    }

    fn templateDependenciesComplete(
        self: *RootCompletionState,
        template_ref: canonical.ProcedureTemplateRef,
    ) bool {
        if (!artifactMatches(template_ref.artifact, self.module.key)) return true;
        const index = @intFromEnum(template_ref.template);
        if (index >= self.visited_templates.len) {
            finalizationInvariant("compile-time dependency referenced an unknown local procedure template");
        }
        if (self.visited_templates[index] == self.visit) return true;
        self.visited_templates[index] = self.visit;

        const template = self.module.checked_procedure_templates.get(template_ref.template);
        return self.resolvedRefsDependenciesComplete(template.resolved_value_refs);
    }

    fn resolvedRefsDependenciesComplete(
        self: *RootCompletionState,
        refs: checked.ResolvedValueRefTableRef,
    ) bool {
        const start = refs.start;
        const end = refs.start + refs.len;
        if (end > self.module.resolved_value_refs.template_refs.len) {
            finalizationInvariant("compile-time dependency template-ref span was outside the checked table");
        }
        for (self.module.resolved_value_refs.template_refs[start..end]) |ref_id| {
            const raw = @intFromEnum(ref_id);
            if (raw >= self.module.resolved_value_refs.records.len) {
                finalizationInvariant("compile-time dependency ref id was outside the checked table");
            }
            if (!self.resolvedRefDependenciesComplete(self.module.resolved_value_refs.records[raw].ref)) {
                return false;
            }
        }
        return true;
    }

    fn resolvedRefDependenciesComplete(
        self: *RootCompletionState,
        ref: checked.ResolvedValueRef,
    ) bool {
        return switch (ref) {
            .top_level_const => |const_use| self.constUseComplete(const_use),
            .selected_hoisted_const => |selected| self.constUseComplete(selected.const_use),
            .top_level_proc,
            .promoted_top_level_proc,
            => |proc_use| self.procedureUseDependenciesComplete(proc_use),
            .platform_required_const => |required| self.constUseComplete(required.const_use),
            .platform_required_proc => |required| self.procedureUseDependenciesComplete(required.procedure),
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
            => true,
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

        return !self.requested_roots[@intFromEnum(dependency_root_id)] or self.isDone(dependency_root_id);
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

    fn procedureUseDependenciesComplete(
        self: *RootCompletionState,
        proc_use: checked.ProcedureUseTemplate,
    ) bool {
        return switch (proc_use.binding) {
            .top_level => |top_level| self.topLevelProcedureDependenciesComplete(top_level),
            .imported, .hosted => true,
            .platform_required => |required| self.platformRequiredProcedureDependenciesComplete(required),
        };
    }

    fn topLevelProcedureDependenciesComplete(
        self: *RootCompletionState,
        top_level: checked.ArtifactTopLevelProcedureBindingRef,
    ) bool {
        if (!artifactMatches(top_level.artifact, self.module.key)) return true;
        const binding = self.module.top_level_procedure_bindings.get(top_level.binding);
        return self.procedureBindingDependenciesComplete(binding.body);
    }

    fn procedureBindingDependenciesComplete(
        self: *RootCompletionState,
        body: checked.ProcedureBindingBody,
    ) bool {
        return switch (body) {
            .direct_template => |direct| self.callableTemplateDependenciesComplete(direct.template),
            .checked_error => true,
            .callable_eval_template => |template_id| blk: {
                const template = self.module.callable_eval_templates.get(template_id);
                break :blk self.rootDependencyComplete(template.root);
            },
        };
    }

    fn callableTemplateDependenciesComplete(
        self: *RootCompletionState,
        template: canonical.CallableProcedureTemplateRef,
    ) bool {
        return switch (template) {
            .checked => |checked_template| self.templateDependenciesComplete(checked_template),
            .lifted, .synthetic => finalizationInvariant("checked procedure dependency referenced a post-check template"),
        };
    }

    fn platformRequiredProcedureDependenciesComplete(
        self: *RootCompletionState,
        required: checked.RequiredAppProcedureRef,
    ) bool {
        if (!artifactMatches(required.artifact, self.module.key)) return true;
        const binding = self.module.platform_required_bindings.lookupByBindingId(@intFromEnum(required.procedure_binding)) orelse
            finalizationInvariant("platform-required procedure dependency referenced a missing binding");
        return switch (binding.value_use) {
            .procedure_value => |procedure_use| self.procedureUseDependenciesComplete(procedure_use.procedure),
            .const_value => |const_use| self.constUseComplete(const_use.const_use),
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
    coverage: *ComptimeCoverage,
    options: Options,
) FinalizeError!bool {
    if (requests.len != root_ids.len) {
        finalizationInvariant("compile-time finalization request/root-id batch length mismatch");
    }

    if (comptime !compilerHostMustUseInterpreterForCtfe()) {
        if (comptime !backend.host_lir_codegen_available) return error.UnsupportedPlatform;
        return lowerDevEvalAndFinishRoots(
            allocator,
            module,
            lowering_imports,
            relation_modules,
            requests,
            root_ids,
            state,
            problem_store,
            coverage,
            options,
        );
    }

    var lowered = try lowerFinalizationModulesToLir(
        allocator,
        .{
            .root = checked.loweringViewWithRelations(module, relation_modules),
            .imports = lowering_imports,
        },
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

    return evalInterpreterLoweredRoots(allocator, module, lowering_imports, relation_modules, requests, root_ids, state, problem_store, coverage, options, &lowered, lowered.lir_result.const_roots.items);
}

/// Execute borrowed LIR on compiler hosts that cannot run native generated code.
fn evalInterpreterLoweredRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    lowering_imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    const_roots: []const LirProgram.ConstRootPlan,
) FinalizeError!bool {
    const program = try InterpreterProgram.init(allocator, .{ .root = checked.loweringViewWithRelations(module, relation_modules), .imports = lowering_imports }, lowered, options);
    defer program.deinit();
    return evalInterpreterProgramRoots(allocator, module, requests, root_ids, state, problem_store, coverage, options, lowered, const_roots, program);
}

fn evalInterpreterProgramRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
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
        var payload: checked.CompileTimeRootPayload = blk: {
            if (root.request.kind == .compile_time_constant and problem_store == null) {
                const eval_result = interpreter.eval(.{
                    .proc_id = root.proc,
                    .ret_layout = root.ret_layout,
                }) catch |err| failure: {
                    if (program.demand_error) |cause| return cause;
                    break :failure switch (err) {
                        error.OutOfMemory => return error.OutOfMemory,
                        error.RuntimeError, error.DivisionByZero => {
                            const message = interpreter.getRuntimeErrorMessage() orelse host.crash_message orelse "compile-time evaluation failed";
                            failed_message = message;
                            break :blk .{ .const_node = try appendCrashConst(module, message) };
                        },
                        error.ComptimeExhaustiveness => {
                            failed_message = "compile-time exhaustiveness failure";
                            break :blk .{ .const_node = try appendCrashConst(module, "compile-time exhaustiveness failure") };
                        },
                        error.Crash => {
                            const message = interpreter.getCrashMessage() orelse host.crash_message orelse "Roc crashed";
                            failed_message = message;
                            break :blk .{ .const_node = try appendCrashConst(module, message) };
                        },
                        error.UnsupportedHostedFunction => finalizationInvariant("compile-time constant reached an unsupported hosted function"),
                        error.InvalidHostedFunctionSignature => finalizationInvariant("compile-time constant reached an invalid hosted function signature"),
                        // expect_err statements only occur in top-level expect
                        // test roots, never in compile-time constant roots.
                        error.ExpectErr => unreachable,
                    };
                };
                defer interpreter.dropValue(eval_result.value, root.ret_layout);
                try program.publishRoot(lowered, module.key, root_id, root, eval_result.value);
                succeeded = true;
                break :blk if (compile_time_root.kind == .hoisted_validation)
                    .discarded
                else
                    try writer.storeRoot(root, eval_result.value);
            }

            const eval_result = try evalCompileTimeRoot(allocator, interpreter, problem_store, module, compile_time_root, &lowered.lir_result, root.proc, root.ret_layout, &program.demand_error);
            try recordComptimeSiteHits(problem_store, coverage, module, compile_time_root, &lowered.lir_result, interpreter.getComptimeBranchHits(), root.proc);
            switch (eval_result) {
                .value => |value| {
                    defer interpreter.dropValue(value.value, root.ret_layout);
                    try program.publishRoot(lowered, module.key, root_id, root, value.value);
                    succeeded = true;
                    break :blk if (compile_time_root.kind == .hoisted_validation)
                        .discarded
                    else
                        try writer.storeRoot(root, value.value);
                },
                .failed => |failed| {
                    failed_message = failed.message;
                    break :blk failed.payload;
                },
            }
        };

        if (!succeeded) {
            const message = failed_message orelse finalizationInvariant("failed interpreter root omitted its explicit failure message");
            program.slotEnvironment().publishFailureOrigin(lowered, module.key, root_id, .{ .loc = interpreter.getFailedSourceLoc(), .region = interpreter.getFailedCheckedRegion() });
            try program.slotEnvironment().publishFailure(lowered, module.key, root_id, message, .{ .resolve = InterpreterProgram.resolveFunction });
            try program.refreshCallableMetadata();
        }

        if (try reportCompileTimeExpectFailures(
            allocator,
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

        if (compile_time_root.literalConversionKind() != null) {
            payload = try finishLiteralConversionRoot(allocator, module, problem_store, compile_time_root, payload);
        }

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
    if (options.timing) |timing| timing.finishExecution(execution_started_ns, program.suspended_ns);

    return had_problem;
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
    comptime_exhaustiveness,
    host_oom,
    host_error,
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
    inline for (@typeInfo(BoxyBuiltinFn).@"enum".fields) |field| {
        const boxy_fn: BoxyBuiltinFn = @enumFromInt(field.value);
        const name = comptime boxy_fn.symbolName();
        table[field.value] = @intFromPtr(&@field(boxy_abi, name));
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
    progress: std.atomic.Value(u8) = std.atomic.Value(u8).init(@intFromEnum(DevRootProgressState.pending)),
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
            const progress: DevRootProgressState = @enumFromInt(job.progress.load(.acquire));
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
            const progress: DevRootProgressState = @enumFromInt(job.progress.load(.acquire));
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

fn lowerDevEvalAndFinishRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    lowering_imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
    options: Options,
) FinalizeError!bool {
    var lowered = try lowerFinalizationModulesToLir(
        allocator,
        .{
            .root = checked.loweringViewWithRelations(module, relation_modules),
            .imports = lowering_imports,
        },
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

    return evalDevLoweredRoots(allocator, module, lowering_imports, relation_modules, requests, root_ids, state, problem_store, coverage, options, &lowered, lowered.lir_result.const_roots.items);
}

const CompletedNativeRoot = struct {
    exports: []static_data_exports.StaticDataExport,
    image: backend.StaticDataImage,

    fn deinit(self: *CompletedNativeRoot, allocator: Allocator) void {
        self.image.deinit();
        static_data_exports.deinitStaticData(allocator, self.exports);
    }
};

/// Owns one native compilation shared by dependency-ordered root batches.
/// Stable owner: interpreter-created callable contexts retain its address.
const InterpreterProgram = struct {
    timing_io: ?std.Io = null,
    suspended_ns: u64 = 0,
    allocator: Allocator,
    slots: StaticSlotEnvironment,
    shared_slots: ?*StaticSlotEnvironment = null,
    slot_demand: ?SlotDemand = null,
    demand_error: ?FinalizeError = null,
    host: CompilerHost,
    interpreter: Interpreter,
    static_callables: std.ArrayList(Interpreter.StaticErasedCallable) = .empty,

    fn init(allocator: Allocator, modules: lir.CheckedPipeline.CheckedModuleSet, lowered: *lir.CheckedPipeline.LoweredProgram, options: Options) FinalizeError!*InterpreterProgram {
        const self = try allocator.create(InterpreterProgram);
        errdefer allocator.destroy(self);
        self.allocator = allocator;
        self.timing_io = null;
        self.suspended_ns = 0;
        self.shared_slots = null;
        self.slot_demand = null;
        self.demand_error = null;
        self.static_callables = .empty;
        errdefer self.static_callables.deinit(allocator);
        const started = if (options.timing) |timing| timing.start() else 0;
        self.slots = try StaticSlotEnvironment.init(allocator, modules, lowered, roc_target.RocTarget.detectNative());
        errdefer self.slots.deinit();
        self.host = CompilerHost.init(allocator);
        errdefer self.host.deinit();
        self.interpreter = try Interpreter.initWithBoxyTables(allocator, &lowered.lir_result.store, &lowered.lir_result.layouts, Interpreter.BoxyTables.fromResult(&lowered.lir_result), self.host.ops(), .normalize);
        errdefer self.interpreter.deinit();
        self.interpreter.dict_seed_mode = .comptime_zero;
        self.interpreter.failure_origins = self.slots.failure_origins;
        self.slots.image.resolveFunctionRelocations(.{ .resolve = resolveFunction }) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => finalizationInvariant("interpreter image omitted a callable procedure"),
        };
        try self.refreshCallableMetadata();
        if (options.timing) |timing| timing.finish(started, .static_data);
        return self;
    }

    fn slotEnvironment(self: *InterpreterProgram) *StaticSlotEnvironment {
        return self.shared_slots orelse &self.slots;
    }

    /// A suspended evaluator keeps its own frames, host events, and temporary
    /// values while another root publishes into the shared immutable slots.
    fn fork(self: *InterpreterProgram, lowered: *lir.CheckedPipeline.LoweredProgram) FinalizeError!*InterpreterProgram {
        const allocator = self.allocator;
        const child = try allocator.create(InterpreterProgram);
        errdefer allocator.destroy(child);
        child.* = .{
            .allocator = allocator,
            .slots = undefined,
            .shared_slots = self.slotEnvironment(),
            .slot_demand = self.slot_demand,
            .host = CompilerHost.init(allocator),
            .interpreter = undefined,
        };
        errdefer child.host.deinit();
        errdefer child.static_callables.deinit(allocator);
        child.interpreter = try Interpreter.initWithBoxyTables(allocator, &lowered.lir_result.store, &lowered.lir_result.layouts, Interpreter.BoxyTables.fromResult(&lowered.lir_result), child.host.ops(), .normalize);
        errdefer child.interpreter.deinit();
        child.interpreter.dict_seed_mode = .comptime_zero;
        child.interpreter.failure_origins = child.slotEnvironment().failure_origins;
        child.interpreter.static_data_demand = .{ .context = child, .ensure = ensureStaticData };
        try child.refreshCallableMetadata();
        return child;
    }

    fn ensureStaticData(raw: *anyopaque, slot: lir.LIR.StaticDataId) Interpreter.Error!void {
        const self: *InterpreterProgram = @ptrCast(@alignCast(raw));
        if (self.slot_demand) |demand| {
            const started = if (self.timing_io) |io| nowNs(io) else 0;
            const result = demand.ensure(demand.context, slot);
            if (self.timing_io) |io| self.suspended_ns += @intCast(@max(0, nowNs(io) - started));
            result catch |err| switch (err) {
                error.CompileTimeDependencyCycle => return self.interpreter.failStaticDataDemand("cyclic compile-time value dependency"),
                else => |operational| {
                    // Preserve the actual operational cause outside the
                    // interpreter's language-error channel while unwinding.
                    self.demand_error = operational;
                    return error.RuntimeError;
                },
            };
            // Nested evaluation can append frozen callable images. Refresh the
            // suspended consumer's registry before it reads the published slot.
            try self.refreshCallableMetadata();
        }
    }

    const resolveFunction = @import("interpreter_static_data.zig").resolveFunction;

    fn resolveCallable(raw: ?*anyopaque, data_ptr: [*]u8) NativeRootExport.CallableResolution {
        const self: *InterpreterProgram = @ptrCast(@alignCast(raw.?));
        const callable = self.interpreter.interpretedCallable(data_ptr) orelse finalizationInvariant("interpreter result omitted its explicit callable ABI");
        return .{ .proc = callable.proc, .capture_ptr = callable.capture_ptr };
    }

    fn resolveStoredCallable(raw: ?*anyopaque, data_ptr: [*]u8) ConstStoreWriter.ErasedCallableResolution {
        const callable = resolveCallable(raw, data_ptr);
        return .{ .proc = callable.proc, .capture_ptr = callable.capture_ptr };
    }

    fn refreshCallableMetadata(self: *InterpreterProgram) Allocator.Error!void {
        self.static_callables.clearRetainingCapacity();
        try @import("interpreter_static_data.zig").appendCallableMetadata(self.allocator, self.slotEnvironment().materialized, &self.slotEnvironment().image, &self.static_callables);
        for (self.slotEnvironment().completed_roots.items) |*root| try @import("interpreter_static_data.zig").appendCallableMetadata(self.allocator, root.exports, &root.image, &self.static_callables);
        self.interpreter.setStaticData(self.slotEnvironment().addresses, self.static_callables.items);
    }

    fn publishRoot(self: *InterpreterProgram, lowered: *lir.CheckedPipeline.LoweredProgram, module: checked.ModuleId, root_id: checked.ComptimeRootId, root: LirProgram.ConstRootPlan, value: @import("value.zig").Value) FinalizeError!void {
        try self.slotEnvironment().publishRoot(lowered, module, root_id, root, value, .{ .context = self, .resolve = resolveCallable }, .{ .resolve = resolveFunction });
        try self.slotEnvironment().publishFailure(lowered, module, root_id, null, .{ .resolve = resolveFunction });
        try self.refreshCallableMetadata();
    }

    fn deinit(self: *InterpreterProgram) void {
        const allocator = self.allocator;
        self.interpreter.deinit();
        self.host.deinit();
        self.static_callables.deinit(allocator);
        if (self.shared_slots == null) self.slots.deinit();
        allocator.destroy(self);
    }
};

/// Shared immutable-slot publication owner for native and interpreter sessions.
const StaticSlotEnvironment = struct {
    allocator: Allocator,
    materialized: []static_data_exports.StaticDataExport,
    image: backend.StaticDataImage,
    addresses: []usize,
    completed_roots: std.ArrayList(CompletedNativeRoot) = .empty,
    failure_origins: []?lir.LIR.ComptimeFailureOrigin,

    fn init(allocator: Allocator, modules: lir.CheckedPipeline.CheckedModuleSet, lowered: *lir.CheckedPipeline.LoweredProgram, target: roc_target.RocTarget) FinalizeError!StaticSlotEnvironment {
        const materialized_static_data = static_data_exports.buildStaticData(
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
        errdefer static_data_exports.deinitStaticData(allocator, materialized_static_data);

        var static_data_image = backend.StaticDataImage.init(allocator, materialized_static_data) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DuplicateStaticDataSymbol,
            error.InvalidStaticDataAlignment,
            error.InvalidStaticDataRelocation,
            error.MissingStaticDataSymbol,
            error.UnresolvedStaticFunction,
            => finalizationInvariant("invalid native static-data image during compile-time finalization"),
        };
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
        const failure_origins = try allocator.alloc(?lir.LIR.ComptimeFailureOrigin, lowered.lir_result.store.getCFStmts().len);
        @memset(failure_origins, null);
        return .{ .allocator = allocator, .materialized = materialized_static_data, .image = static_data_image, .addresses = native_static_data, .failure_origins = failure_origins };
    }

    fn publishRoot(
        self: *StaticSlotEnvironment,
        lowered: *lir.CheckedPipeline.LoweredProgram,
        module: checked.ModuleId,
        root_id: checked.ComptimeRootId,
        plan: LirProgram.ConstRootPlan,
        value: @import("value.zig").Value,
        callables: NativeRootExport.CallableResolver,
        functions: backend.StaticDataImageFunctionResolver,
    ) FinalizeError!void {
        for (lowered.lir_result.static_data_values.items, 0..) |entry, index| {
            const root = entry.compile_time_root orelse continue;
            if (!std.meta.eql(root.module, module) or root.root != root_id or root.role != .value) continue;
            const destination_layout = entry.layout_idx;
            if (destination_layout != plan.ret_layout) finalizationInvariant("evaluated root requires its explicit instance conversion");
            const slot: lir.LIR.StaticDataId = @enumFromInt(index);
            const exports = try NativeRootExport.freezeRoot(self.allocator, &lowered.lir_result, slot, plan, value, callables);
            try self.installExports(lowered, slot, exports, functions);
            lir.ComptimeValueGuards.completeSuccessfulSlot(&lowered.lir_result, slot);
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
        var image = backend.StaticDataImage.init(self.allocator, exports) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => finalizationInvariant("evaluated root export contains an invalid relocation"),
        };
        errdefer image.deinit();
        image.resolveFunctionRelocations(functions) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => finalizationInvariant("evaluated root export omitted an emitted function"),
        };
        const source = image.symbolAddress(exports[0].symbol_name) orelse finalizationInvariant("evaluated root export omitted its root symbol");
        const index = @intFromEnum(slot);
        const value_layout = lowered.lir_result.static_data_values.items[index].layout_idx;
        const size = lowered.lir_result.layouts.layoutSize(lowered.lir_result.layouts.getLayout(value_layout));
        const destination: [*]u8 = @ptrFromInt(self.addresses[index]);
        @memcpy(destination[0..size], @as([*]const u8, @ptrFromInt(source))[0..size]);
        try self.completed_roots.append(self.allocator, .{ .exports = exports, .image = image });
    }

    fn publishFailureOrigin(self: *StaticSlotEnvironment, lowered: *const lir.CheckedPipeline.LoweredProgram, module: checked.ModuleId, root_id: checked.ComptimeRootId, origin: lir.LIR.ComptimeFailureOrigin) void {
        for (lowered.lir_result.comptime_value_guards.items) |guard| {
            const root = lowered.lir_result.static_data_values.items[@intFromEnum(guard.value_slot)].compile_time_root.?;
            if (std.meta.eql(root.module, module) and root.root == root_id) {
                self.failure_origins[@intFromEnum(guard.crash)] = origin;
            }
        }
    }

    fn publishFailure(
        self: *StaticSlotEnvironment,
        lowered: *const lir.CheckedPipeline.LoweredProgram,
        module: checked.ModuleId,
        root_id: checked.ComptimeRootId,
        message: ?[]const u8,
        functions: backend.StaticDataImageFunctionResolver,
    ) FinalizeError!void {
        const allocator = self.allocator;
        for (lowered.lir_result.static_data_values.items, 0..) |entry, index| {
            const root = entry.compile_time_root orelse continue;
            if (!std.meta.eql(root.module, module) or root.root != root_id or root.role != .failure_message) continue;
            const metadata = root.role.failure_message;
            const slot: lir.LIR.StaticDataId = @enumFromInt(index);
            const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(entry.layout_idx));
            const text = message orelse "";
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
            bytes[metadata.failed_offset] = @intFromBool(message != null);
            if (large) {
                const backing_name = try std.fmt.allocPrint(allocator, "{s}_message", .{name});
                const backing = allocator.alloc(u8, @sizeOf(usize) + text.len) catch |err| {
                    allocator.free(backing_name);
                    return err;
                };
                @memset(backing[0..@sizeOf(usize)], 0);
                @memcpy(backing[@sizeOf(usize)..], text);
                exports[1] = .{ .symbol_name = backing_name, .bytes = backing, .symbol_offset = @sizeOf(usize), .alignment = @alignOf(usize), .is_exported = false };
                count = 2;
                const relocations = try allocator.alloc(static_data_exports.StaticDataRelocation, 1);
                relocations[0] = .{ .offset = metadata.message_offset, .target_symbol_name = backing_name, .target = .{ .data_symbol = @enumFromInt(1) } };
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
            var root_index: ?usize = null;
            for (all.items, 0..) |item, index| if (item.value_id == slot) {
                root_index = index;
                break;
            };
            const index = root_index orelse finalizationInvariant("completed root had no allocated value slot");
            const nested_start = all.items.len;
            for (cloned) |*item| for (@constCast(item.relocations)) |*relocation| {
                if (relocation.target == .data_symbol) {
                    const local = @intFromEnum(relocation.target.data_symbol);
                    relocation.target.data_symbol = @enumFromInt(if (local == 0) index else nested_start + local - 1);
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
        return .{ .allocator = self.allocator, .exports = try all.toOwnedSlice(self.allocator) };
    }

    fn deinit(self: *StaticSlotEnvironment) void {
        for (self.completed_roots.items) |*root| root.deinit(self.allocator);
        self.completed_roots.deinit(self.allocator);
        self.allocator.free(self.failure_origins);
        self.allocator.free(self.addresses);
        self.image.deinit();
        static_data_exports.deinitStaticData(self.allocator, self.materialized);
        self.* = undefined;
    }
};

const DevProgram = struct {
    slot_demand: ?SlotDemand = null,
    allocator: Allocator,
    static_strings: backend.StaticStringData.Table,
    slots: StaticSlotEnvironment,
    codegen: backend.HostLirCodeGen,
    executable: backend.ExecutableMemory,
    entry_offsets: collections.DenseMap(lir.LIR.LirProcSpecId, usize),

    fn init(allocator: Allocator, modules: lir.CheckedPipeline.CheckedModuleSet, lowered: *lir.CheckedPipeline.LoweredProgram, options: Options) FinalizeError!DevProgram {
        const static_data_started_ns = if (options.timing) |timing| timing.start() else 0;
        var static_strings = try backend.StaticStringData.build(allocator, &lowered.lir_result.store, backend.dev.LirCodeGenMod.host_lir_codegen_target);
        errdefer static_strings.deinit();

        var slots = try StaticSlotEnvironment.init(allocator, modules, lowered, backend.dev.LirCodeGenMod.host_lir_codegen_target);
        errdefer slots.deinit();
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
            .normalize,
            roc_target.host_cpu.level(),
        );
        errdefer codegen.deinit();
        codegen.dict_seed_mode = .comptime_zero;
        codegen.setNativeStaticData(slots.addresses);
        codegen.setComptimeHooks(.{
            .branch_taken = CompileTimeHost.rocComptimeBranchTaken,
            .exhaustiveness_failed = CompileTimeHost.rocComptimeExhaustivenessFailed,
            .failure_region = CompileTimeHost.rocComptimeFailureRegion,
            .ensure_static_value = CompileTimeHost.rocComptimeEnsureStaticValue,
            .call_enter = CompileTimeHost.rocComptimeCallEnter,
            .call_exit = CompileTimeHost.rocComptimeCallExit,
        });
        var native_fns = boxyNativeFnTable();
        codegen.boxy_native_fns = &native_fns;
        const evaluation_roots = try allocator.alloc(lir.LIR.LirProcSpecId, lowered.lir_result.const_roots.items.len);
        defer allocator.free(evaluation_roots);
        for (lowered.lir_result.const_roots.items, evaluation_roots) |root, *proc| proc.* = root.proc;
        const evaluation_demand = try lir.ReachableProcs.collectProcDemand(allocator, &lowered.lir_result, evaluation_roots, slots.materialized);
        defer allocator.free(evaluation_demand);
        try codegen.compileSelectedProcSpecs(evaluation_demand);
        const static_rc_helpers = try static_data_exports.collectRequiredRcHelpers(allocator, slots.materialized);
        defer allocator.free(static_rc_helpers);
        try codegen.compileStaticDataRcHelpers(static_rc_helpers);

        var entry_offsets = collections.DenseMap(lir.LIR.LirProcSpecId, usize).init(allocator);
        errdefer entry_offsets.deinit();
        for (lowered.lir_result.const_roots.items, 0..) |root, index| {
            if (entry_offsets.get(root.proc) != null) continue;
            var name_buf: [64]u8 = undefined;
            const symbol_name = std.fmt.bufPrint(&name_buf, "roc_ctfe_root_{d}", .{index}) catch unreachable;
            const entrypoint = try codegen.generateEntrypointWrapper(symbol_name, root.proc, &.{}, root.ret_layout);
            try entry_offsets.put(root.proc, entrypoint.offset);
        }
        codegen.boxy_native_fns = null;
        var executable = try backend.ExecutableMemory.initWithEntryOffset(codegen.getGeneratedCode(), 0);
        errdefer executable.deinit();

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
            .executable = executable,
            .entry_offsets = entry_offsets,
        };
    }

    fn resolveCallable(raw: ?*anyopaque, data_ptr: [*]u8) NativeRootExport.CallableResolution {
        const self: *DevProgram = @ptrCast(@alignCast(raw.?));
        const payload = builtins.erased_callable.payloadPtr(data_ptr);
        const address = @intFromPtr(payload.callable_fn_ptr);
        const base_address = @intFromPtr(self.executable.codePtr());
        for (self.codegen.store.getProcSpecs(), 0..) |_, index| {
            const proc: lir.LIR.LirProcSpecId = @enumFromInt(index);
            const symbol = self.codegen.compiledProcSymbol(proc) orelse continue;
            if (base_address + symbol.code_start == address) return .{
                .proc = proc,
                .capture_ptr = builtins.erased_callable.capturePtr(data_ptr),
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

    fn publishRoot(self: *DevProgram, lowered: *lir.CheckedPipeline.LoweredProgram, module: checked.ModuleId, root_id: checked.ComptimeRootId, plan: LirProgram.ConstRootPlan, value: @import("value.zig").Value) FinalizeError!void {
        try self.slots.publishRoot(lowered, module, root_id, plan, value, .{ .context = self, .resolve = resolveCallable }, .{ .context = self, .resolve = resolveFrozenFunction });
    }
    fn publishFailure(self: *DevProgram, lowered: *const lir.CheckedPipeline.LoweredProgram, module: checked.ModuleId, root_id: checked.ComptimeRootId, message: ?[]const u8) FinalizeError!void {
        try self.slots.publishFailure(lowered, module, root_id, message, .{ .context = self, .resolve = resolveFrozenFunction });
    }
    fn freezeCompleted(self: *DevProgram) Allocator.Error!LirProgram.FrozenStaticData {
        return self.slots.freezeCompleted();
    }

    fn deinit(self: *DevProgram) void {
        self.entry_offsets.deinit();
        self.executable.deinit();
        self.codegen.deinit();
        self.slots.deinit();
        self.static_strings.deinit();
        self.* = undefined;
    }
};

/// Execute borrowed LIR; the compilation owner retains the lowered program.
fn evalDevLoweredRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    lowering_imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    const_roots: []const LirProgram.ConstRootPlan,
) FinalizeError!bool {
    if (const_roots.len != requests.len) {
        finalizationInvariant("LIR lowering returned a different number of compile-time roots than requested");
    }
    var native = try DevProgram.init(allocator, .{
        .root = checked.loweringViewWithRelations(module, relation_modules),
        .imports = lowering_imports,
    }, lowered, options);
    defer native.deinit();
    native.codegen.static_strings = native.static_strings.view();
    return evalDevProgramRoots(allocator, module, requests, root_ids, state, problem_store, coverage, options, lowered, const_roots, &native);
}

fn evalProgramRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
    options: Options,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    const_roots: []const LirProgram.ConstRootPlan,
    program: anytype,
) FinalizeError!bool {
    if (@TypeOf(program) == *DevProgram) return evalDevProgramRoots(allocator, module, requests, root_ids, state, problem_store, coverage, options, lowered, const_roots, program);
    if (@TypeOf(program) == *InterpreterProgram) return evalInterpreterProgramRoots(allocator, module, requests, root_ids, state, problem_store, coverage, options, lowered, const_roots, program);
    @compileError("compile-time evaluation owner must be explicit native or interpreter program");
}

fn evalDevProgramRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
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
            jobs[i].host.failure_origins = native.slots.failure_origins;
            jobs[i].host.slot_demand = native.slot_demand;
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
    if (native.slot_demand != null) {
        // Demands nest on this thread; each invocation owns its host and return storage.
        for (0..jobs_len) |index| devRootWorker(host_allocator, &run_context, index);
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
        var suspended_ns: u64 = 0;
        for (jobs) |job| suspended_ns += job.host.suspended_ns;
        timing.finishExecution(execution_started_ns, suspended_ns);
    }

    if (run_context.had_oom.load(.acquire)) return error.OutOfMemory;

    const DevResolver = struct {
        codegen: *const backend.HostLirCodeGen,
        store: *const lir.LirStore,
        executable: *const backend.ExecutableMemory,

        fn resolve(raw: ?*anyopaque, data_ptr: [*]u8) ConstStoreWriter.ErasedCallableResolution {
            const self: *@This() = @ptrCast(@alignCast(raw.?));
            const payload = builtins.erased_callable.payloadPtr(data_ptr);
            const runtime_addr = @intFromPtr(payload.callable_fn_ptr);
            for (self.store.getProcSpecs(), 0..) |_, proc_index| {
                const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(proc_index);
                const symbol = self.codegen.compiledProcSymbol(proc_id) orelse continue;
                const compiled_addr = @intFromPtr(self.executable.codePtr() + symbol.code_start);
                if (compiled_addr == runtime_addr) {
                    return .{
                        .proc = proc_id,
                        .capture_ptr = builtins.erased_callable.capturePtr(data_ptr),
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
        var payload: checked.CompileTimeRootPayload = switch (job.result) {
            .pending => finalizationInvariant("dev backend compile-time root was not evaluated"),
            .host_oom => return error.OutOfMemory,
            .host_error => return job.host.operational_error orelse finalizationInvariant("native host error omitted its operational cause"),
            .comptime_exhaustiveness => try devComptimeExhaustivenessRootPayload(
                allocator,
                problem_store,
                module,
                job.compile_time_root,
                &lowered.lir_result,
                job.root.proc,
                job.host.comptime_failed_site orelse finalizationInvariant("dev backend CTFE exhaustiveness failure had no site"),
                job.root.request,
                &had_problem,
            ),
            .crashed => try devCrashedRootPayload(
                allocator,
                problem_store,
                module,
                job.compile_time_root,
                job.root.request,
                job.host.crashMessage() orelse "Roc crashed",
                job.host.failed_region,
                job.host.failed_loc,
                &lowered.lir_result.store,
                &had_problem,
            ),
            .success => if (job.compile_time_root.kind == .hoisted_validation)
                .discarded
            else
                try writer.storeRoot(job.root, .{ .ptr = job.ret_buf.ptr }),
        };

        if (job.result == .success) try native.publishRoot(lowered, module.key, job.root_id, job.root, .{ .ptr = job.ret_buf.ptr });
        const failure_message: ?[]const u8 = switch (job.result) {
            .success => null,
            .crashed => job.host.crashMessage() orelse "Roc crashed",
            .comptime_exhaustiveness => "compile-time exhaustiveness failure",
            .pending, .host_oom, .host_error => unreachable,
        };
        if (failure_message != null) native.slots.publishFailureOrigin(lowered, module.key, job.root_id, .{ .loc = job.host.failed_loc, .region = job.host.failed_region });
        try native.publishFailure(lowered, module.key, job.root_id, failure_message);

        try recordComptimeSiteHits(problem_store, coverage, module, job.compile_time_root, &lowered.lir_result, job.host.comptime_branch_hits.items, job.root.proc);

        if (try reportDevHostEvents(allocator, options, problem_store, module, job.compile_time_root, &lowered.lir_result.store, job.host.events.items)) {
            had_problem = true;
        }

        if (job.compile_time_root.literalConversionKind() != null) {
            const conversion = try finishLiteralConversionRootDetailed(allocator, module, problem_store, job.compile_time_root, payload);
            payload = conversion.payload;
            if (conversion.had_problem) had_problem = true;
        }

        module.compile_time_roots.fillPayload(job.root_id, payload);
        const stored_root_type = switch (job.compile_time_root.kind) {
            .constant, .hoisted_constant => try writer.storeRootType(job.root),
            .hoisted_validation,
            .callable_binding,
            .expect,
            .numeral_conversion,
            .quote_conversion,
            .repl_expr,
            => null,
        };
        finishConstRoot(module, job.compile_time_root, payload, stored_root_type);
        state.markDone(job.root_id);
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
    job.progress.store(@intFromEnum(DevRootProgressState.running), .release);
    if (context.progress_reporter) |progress| progress.rootStarted();

    var crash_boundary = job.host.enterCrashBoundary();
    const sj = crash_boundary.set();
    if (sj == 0) {
        context.executable.callRocABIAt(
            job.entry_offset,
            @ptrCast(job.host.ops()),
            @ptrCast(job.ret_buf.ptr),
            null,
        );
    }
    crash_boundary.deinit();

    job.result = switch (job.host.termination) {
        .returned => .success,
        .crashed => .crashed,
        .comptime_exhaustiveness => .comptime_exhaustiveness,
        .host_error => .host_error,
        .host_oom => blk: {
            context.had_oom.store(true, .release);
            break :blk .host_oom;
        },
    };
    job.progress.store(@intFromEnum(DevRootProgressState.done), .release);
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
    allocator: Allocator,
    problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    site_id: lir.LIR.ComptimeSiteId,
    request: checked.RootRequest,
    had_problem: *bool,
) FinalizeError!checked.CompileTimeRootPayload {
    if (request.kind == .compile_time_constant and problem_store == null) {
        return .{ .const_node = try appendCrashConst(module, "compile-time exhaustiveness failure") };
    }

    const store = problem_store orelse {
        finalizationInvariant("compile-time root reached an empirical exhaustiveness failure without a checking problem store");
    };
    try appendCompileTimeExhaustivenessProblem(allocator, store, module, root, lir_result, root_proc, site_id);
    had_problem.* = true;
    return try failedRootPayload(module, root, "compile-time exhaustiveness failure");
}

fn devCrashedRootPayload(
    allocator: Allocator,
    problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    request: checked.RootRequest,
    message: []const u8,
    failed_region: ?base.Region,
    failed_loc: ?base.SourceLoc,
    lir_store: *const lir.LirStore,
    had_problem: *bool,
) FinalizeError!checked.CompileTimeRootPayload {
    if (request.kind == .compile_time_constant and problem_store == null) {
        return .{ .const_node = try appendCrashConst(module, message) };
    }
    const store = problem_store orelse {
        finalizationInvariant("compile-time root crashed without a checking problem store");
    };
    const message_idx = try store.putExtraString(message);
    const site = comptimeFailureSiteFromLoc(
        module,
        lir_store,
        devRootSourceRegion(module, root),
        failed_region,
        failed_loc,
    );
    _ = try store.appendProblem(allocator, .{ .comptime_crash = .{
        .message = message_idx,
        .region = site.region,
        .origin = try comptimeFailureOrigin(store, site),
    } });
    had_problem.* = true;
    return try failedRootPayload(module, root, message);
}

fn reportDevHostEvents(
    allocator: Allocator,
    options: Options,
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
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
                const site = comptimeFailureSiteFromLoc(
                    module,
                    lir_store,
                    root_region,
                    failure.region,
                    failure.loc,
                );
                _ = try store.appendProblem(allocator, .{ .comptime_expect_failed = .{
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

/// Unwrap the `Try` value a literal-conversion root evaluated to. `Ok` payloads
/// become the stored constant; `Err(InvalidNumeral(msg))` / `Err(BadQuotedBytes(msg))`
/// becomes a checking problem carrying the implementation's message.
const LiteralConversionFinish = struct {
    payload: checked.CompileTimeRootPayload,
    had_problem: bool,
};

fn finishLiteralConversionRoot(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    problem_store: ?*check.problem.Store,
    root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
) FinalizeError!checked.CompileTimeRootPayload {
    const result = try finishLiteralConversionRootDetailed(allocator, module, problem_store, root, payload);
    return result.payload;
}

fn finishLiteralConversionRootDetailed(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    problem_store: ?*check.problem.Store,
    root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
) FinalizeError!LiteralConversionFinish {
    const try_node = switch (payload) {
        .const_node => |node| node,
        .pending, .fn_value, .discarded, .expect => finalizationInvariant("numeral conversion root did not store a constant"),
    };
    switch (module.const_store.get(try_node)) {
        // The from_numeral implementation itself crashed; that crash was
        // already stored (and reported when a problem store exists).
        .crash => return .{ .payload = payload, .had_problem = false },
        .pending,
        .zst,
        .scalar,
        .str,
        .list,
        .box,
        .tuple,
        .record,
        .tag,
        .nominal,
        .fn_value,
        => {},
    }
    const try_tag = constTagValue(module, try_node);
    if (constTagNameIs(try_tag.tag_name, "Ok")) {
        if (try_tag.payloads.len != 1) finalizationInvariant("numeral conversion Ok did not carry one payload");
        return .{ .payload = .{ .const_node = try_tag.payloads[0] }, .had_problem = false };
    }
    if (!constTagNameIs(try_tag.tag_name, "Err")) {
        finalizationInvariant("numeral conversion result was neither Ok nor Err");
    }
    if (try_tag.payloads.len != 1) finalizationInvariant("numeral conversion Err did not carry one payload");
    const err_tag = constTagValue(module, try_tag.payloads[0]);
    if (err_tag.payloads.len != 1) finalizationInvariant("numeral conversion error tag did not carry one payload");
    const message_str = switch (module.const_store.get(err_tag.payloads[0])) {
        .str => |str| str,
        .pending,
        .zst,
        .scalar,
        .list,
        .box,
        .tuple,
        .record,
        .crash,
        .tag,
        .nominal,
        .fn_value,
        => finalizationInvariant("numeral conversion error payload was not a string"),
    };
    const message = module.const_store.strBytes(message_str);
    if (problem_store) |store| {
        const message_idx = try store.putExtraString(message);
        // A rejected literal conversion is not a failed statement: the root
        // evaluated successfully to `Err(...)`, so there is no failed LIR
        // source stamp to resolve. The conversion root lives in the module
        // that declares the literal (conversion roots are created while
        // checking that module's own CIR, and each module finalizes its own
        // roots), so the site resolution always degrades to the local case
        // here: the literal's region in this module's source, no origin.
        const site = comptimeFailureSiteFrom(
            module,
            module.checked_bodies.expr(root.expr).source_region,
            null,
            null,
            null,
        );
        switch (root.literalConversionKind() orelse finalizationInvariant("non literal-conversion root reported a conversion problem")) {
            .numeral => _ = try store.appendProblem(allocator, .{ .comptime_invalid_numeral = .{
                .message = message_idx,
                .region = site.region,
                .origin = try comptimeFailureOrigin(store, site),
            } }),
            .quote => _ = try store.appendProblem(allocator, .{ .comptime_invalid_quote = .{
                .message = message_idx,
                .region = site.region,
                .origin = try comptimeFailureOrigin(store, site),
            } }),
        }
        return .{
            .payload = .{ .const_node = try appendCrashConst(module, message) },
            .had_problem = true,
        };
    }
    return .{ .payload = .{ .const_node = try appendCrashConst(module, message) }, .had_problem = false };
}

fn constTagValue(
    module: *const checked.CheckedModuleArtifact,
    node: checked.ConstNodeId,
) @FieldType(checked.ConstValue, "tag") {
    var current = node;
    while (true) {
        switch (module.const_store.get(current)) {
            .nominal => |nominal| current = nominal.backing,
            .tag => |tag| return tag,
            .pending,
            .zst,
            .scalar,
            .str,
            .list,
            .box,
            .tuple,
            .record,
            .crash,
            .fn_value,
            => finalizationInvariant("numeral conversion constant was not a tag value"),
        }
    }
}

fn constTagNameIs(name: []const u8, expected: []const u8) bool {
    if (name.len != expected.len) return false;
    for (name, expected) |actual, wanted| {
        if (actual != wanted) return false;
    }
    return true;
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

fn reportCompileTimeExpectFailures(
    allocator: Allocator,
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
        _ = try problem_store.appendProblem(allocator, .{ .comptime_expect_failed = .{
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
};

fn evalCompileTimeRoot(
    allocator: Allocator,
    interpreter: *Interpreter,
    problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    proc: lir.LIR.LirProcSpecId,
    ret_layout: @import("layout").Idx,
    demand_error: *const ?FinalizeError,
) FinalizeError!CompileTimeEvalResult {
    const result = interpreter.eval(.{
        .proc_id = proc,
        .ret_layout = ret_layout,
    }) catch |err| failure: {
        if (demand_error.*) |cause| return cause;
        break :failure switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.RuntimeError => {
                const message = interpreter.getRuntimeErrorMessage() orelse "compile-time evaluation failed";
                return .{ .failed = .{ .message = message, .payload = try reportCompileTimeCrash(allocator, problem_store, module, root, interpreter, message) } };
            },
            error.ComptimeExhaustiveness => return .{ .failed = .{ .message = "compile-time exhaustiveness failure", .payload = try reportCompileTimeExhaustiveness(allocator, problem_store, module, root, lir_result, interpreter, proc) } },
            error.DivisionByZero => {
                const message = interpreter.getRuntimeErrorMessage() orelse "Division by zero";
                return .{ .failed = .{ .message = message, .payload = try reportCompileTimeCrash(allocator, problem_store, module, root, interpreter, message) } };
            },
            error.Crash => {
                const message = interpreter.getCrashMessage() orelse "Roc crashed";
                return .{ .failed = .{ .message = message, .payload = try reportCompileTimeCrash(allocator, problem_store, module, root, interpreter, message) } };
            },
            error.ExpectErr => finalizationInvariant("compile-time root reached an expect_err statement"),
            error.UnsupportedHostedFunction => finalizationInvariant("compile-time root reached an unsupported hosted function"),
            error.InvalidHostedFunctionSignature => finalizationInvariant("compile-time root reached an invalid hosted function signature"),
        };
    };
    return .{ .value = result };
}

fn recordComptimeSiteHits(
    maybe_problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
    module: *checked.CheckedModuleArtifact,
    compile_time_root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    hits: anytype,
    root_proc: lir.LIR.LirProcSpecId,
) Allocator.Error!void {
    const problem_store = maybe_problem_store orelse return;
    for (hits) |hit| {
        const site = lir_result.comptime_sites.items[@intFromEnum(hit.site)];
        if (comptimeSiteEmpiricalKind(site.kind) != null) {
            if (site.checked_site) |checked_site| {
                if (comptimeSiteMayResolvePending(module, compile_time_root.id, checked_site)) {
                    problem_store.resolvePendingStaticExhaustiveness(checked_site);
                }
            }
        }
        if (reportsUnusedBranches(compile_time_root.kind) and site.proc == root_proc) {
            try coverage.record(site, hit.branch_index);
        }
    }
}

fn reportsUnusedBranches(kind: checked.CompileTimeRootKind) bool {
    return switch (kind) {
        .constant,
        .callable_binding,
        .expect,
        .numeral_conversion,
        .quote_conversion,
        .repl_expr,
        => true,
        .hoisted_constant,
        .hoisted_validation,
        => false,
    };
}

fn reportCompileTimeExhaustiveness(
    allocator: Allocator,
    maybe_problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    interpreter: *const Interpreter,
    root_proc: lir.LIR.LirProcSpecId,
) FinalizeError!checked.CompileTimeRootPayload {
    const problem_store = maybe_problem_store orelse {
        finalizationInvariant("compile-time root reached an empirical exhaustiveness failure without a checking problem store");
    };
    const site_id = interpreter.getComptimeFailedSite() orelse {
        finalizationInvariant("compile-time root reported empirical exhaustiveness failure without a site");
    };
    try appendCompileTimeExhaustivenessProblem(allocator, problem_store, module, root, lir_result, root_proc, site_id);
    return try failedRootPayload(module, root, "compile-time exhaustiveness failure");
}

fn appendCompileTimeExhaustivenessProblem(
    allocator: Allocator,
    problem_store: *check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    site_id: lir.LIR.ComptimeSiteId,
) Allocator.Error!void {
    const site = lir_result.comptime_sites.items[@intFromEnum(site_id)];
    _ = comptimeSiteEmpiricalKind(site.kind) orelse switch (site.kind) {
        .if_ => finalizationInvariant("if expression reached empirical exhaustiveness failure"),
        .match, .destructure => finalizationInvariant("compile-time root had no empirical exhaustiveness kind"),
    };
    const checked_site = site.checked_site orelse {
        finalizationInvariant("empirical exhaustiveness failure had no checked site id");
    };
    discardUnreachedRootComptimeSites(problem_store, lir_result, root_proc, checked_site, module, site_id);
    if (!comptimeSiteMayResolvePending(module, root.id, checked_site)) {
        const site_record = module.exhaustiveness_sites.get(checked_site);
        switch (site_record.policy) {
            .runtime_reachable => {},
            .not_pending,
            .compile_time_only,
            .compile_time_replaced_by_root,
            => finalizationInvariant("compile-time exhaustiveness failure had an impossible site policy"),
        }
    }
    const matched = try problem_store.appendEmpiricalExhaustivenessFailure(allocator, checked_site);
    if (!matched) {
        finalizationInvariant("empirical exhaustiveness failure had no pending static diagnostic");
    }
}

fn discardUnreachedRootComptimeSites(
    problem_store: *check.problem.Store,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    failed_checked_site: checked.CheckedExhaustivenessSiteId,
    module: *const checked.CheckedModuleArtifact,
    failed_site_id: lir.LIR.ComptimeSiteId,
) void {
    for (lir_result.comptime_sites.items, 0..) |root_site, raw_site_id| {
        if (root_site.proc != root_proc) continue;
        if (raw_site_id == @intFromEnum(failed_site_id)) continue;
        if (comptimeSiteEmpiricalKind(root_site.kind) == null) continue;
        const checked_site = root_site.checked_site orelse continue;
        if (checked_site == failed_checked_site) continue;
        const site = module.exhaustiveness_sites.get(checked_site);
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

fn comptimeSiteMayResolvePending(
    module: *const checked.CheckedModuleArtifact,
    root_id: checked.ComptimeRootId,
    checked_site: checked.CheckedExhaustivenessSiteId,
) bool {
    const site = module.exhaustiveness_sites.get(checked_site);
    return switch (site.policy) {
        .compile_time_replaced_by_root => |owner_root| owner_root == root_id,
        .compile_time_only => true,
        .runtime_reachable,
        .not_pending,
        => false,
    };
}

fn comptimeSiteEmpiricalKind(
    site_kind: lir.LIR.ComptimeSiteKind,
) ?check.problem.Store.EmpiricalSiteKind {
    return switch (site_kind) {
        .match => .match,
        .destructure => .destructure,
        .if_ => null,
    };
}

fn reportCompileTimeCrash(
    allocator: Allocator,
    maybe_problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    interpreter: *const Interpreter,
    message: []const u8,
) FinalizeError!checked.CompileTimeRootPayload {
    const problem_store = maybe_problem_store orelse {
        finalizationInvariant("compile-time root crashed without a checking problem store");
    };
    const message_idx = try problem_store.putExtraString(message);
    const site = compileTimeCrashSite(module, root, interpreter);
    _ = try problem_store.appendProblem(allocator, .{ .comptime_crash = .{
        .message = message_idx,
        .region = site.region,
        .origin = try comptimeFailureOrigin(problem_store, site),
    } });
    return try failedRootPayload(module, root, message);
}

fn failedRootPayload(
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    message: []const u8,
) Allocator.Error!checked.CompileTimeRootPayload {
    return switch (root.kind) {
        .expect => .expect,
        .hoisted_validation => .discarded,
        .constant,
        .hoisted_constant,
        .callable_binding,
        .numeral_conversion,
        .quote_conversion,
        .repl_expr,
        => .{ .const_node = try appendCrashConst(module, message) },
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
/// Module identity is the package-qualified name (`pf.Utils`), never the bare
/// display name: two packages may both contain a `Utils`, and matching by
/// bare name would render the foreign module's byte offsets against this
/// module's source. The rendered origin stays human-readable: the bare
/// display name, or the qualified name when the bare name coincides with the
/// finalized module's and would not identify the declaring module.
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
    if (std.mem.eql(u8, file.qualified_name, env.qualifiedModuleName())) {
        return .{ .region = failed_region orelse root_region, .foreign = null };
    }
    const bare_name_collides = std.mem.eql(u8, file.name, env.module_name);
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
        } else null)
    else
        null;
    return comptimeFailureSiteFrom(module, root_region, failed_region, failed_loc, failed_file);
}

fn compileTimeCrashSite(
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    interpreter: *const Interpreter,
) ComptimeFailureSite {
    return comptimeFailureSiteFrom(
        module,
        module.checked_bodies.expr(root.expr).source_region,
        interpreter.getFailedCheckedRegion(),
        interpreter.getFailedSourceLoc(),
        interpreter.getFailedSourceFile(),
    );
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
    const raw = @intFromEnum(root_id);
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
        .pending,
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

fn finalizationInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("compile-time finalization invariant violated: {s}", .{message});
    }
    unreachable;
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
        try events.events.append(allocator, .{ .module = .{ .bytes = [_]u8{input.module} ** 32 }, .root = @enumFromInt(input.root), .sequence = index, .is_repl = false, .message = message });
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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const temporary = arena.allocator();
    const first = try static_data_exports.cloneStaticData(temporary, &.{
        .{ .symbol_name = "root_a", .bytes = &.{ 0, 0, 0, 0 }, .alignment = 4, .relocations = &.{.{ .offset = 0, .target_symbol_name = "backing_a", .target = .{ .data_symbol = @enumFromInt(1) } }} },
        .{ .symbol_name = "backing_a", .bytes = "a", .alignment = 1 },
    });
    const second = try static_data_exports.cloneStaticData(temporary, &.{
        .{ .symbol_name = "root_b", .bytes = &.{ 0, 0, 0, 0 }, .alignment = 4, .relocations = &.{.{ .offset = 0, .target_symbol_name = "backing_b", .target = .{ .data_symbol = @enumFromInt(1) } }} },
        .{ .symbol_name = "backing_b", .bytes = "b", .alignment = 1 },
    });
    var destination = std.ArrayList(static_data_exports.StaticDataExport).empty;
    defer destination.deinit(allocator);
    try appendFrozenGraph(allocator, &destination, first);
    try appendFrozenGraph(allocator, &destination, second);
    const owned = try static_data_exports.cloneStaticData(allocator, destination.items);
    defer static_data_exports.deinitStaticData(allocator, owned);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(owned[0].relocations[0].target.data_symbol));
    try std.testing.expectEqual(@as(u32, 3), @intFromEnum(owned[2].relocations[0].target.data_symbol));
    try std.testing.expectEqualStrings("backing_a", owned[0].relocations[0].target_symbol_name);
    try std.testing.expectEqualStrings("backing_b", owned[2].relocations[0].target_symbol_name);
}

test "shared interpreter slots publish dependent strings and distinguish empty failures" {
    for ([_]?[]const u8{ null, "", "a failure message longer than the inline string representation" }) |failure| {
        try testInterpreterSlot(failure, false, false);
    }
}

test "shared interpreter slots suspend consumers while a producer evaluates once" {
    for ([_]?[]const u8{ null, "", "nested producer failure" }) |failure| try testInterpreterSlot(failure, true, false);
}

test "shared interpreter slots report an active demand cycle as a normal crash" {
    try testInterpreterSlot("cyclic compile-time value dependency", true, true);
}

fn testInterpreterSlot(failure_message: ?[]const u8, nested: bool, cycle: bool) (FinalizeError || Interpreter.Error || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedError, TestUnexpectedError, DuplicateStaticDataSymbol, InvalidStaticDataAlignment, InvalidStaticDataRelocation, MissingStaticDataSymbol, UnresolvedStaticFunction })!void {
    const allocator = std.testing.allocator;
    var lowered = lir.CheckedPipeline.LoweredProgram{
        .lir_result = try LirProgram.Result.init(allocator, .native),
        .main_proc = null,
        .target_usize = .native,
        .runtime_value_schemas = lir.CheckedPipeline.RuntimeValueSchemaStore.init(allocator),
    };
    defer lowered.deinit();
    const result = &lowered.lir_result;
    const failure_layout = try result.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = result.layouts.getLayout(failure_layout).getStruct().idx;
    const failed_offset = result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const message_offset = result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
    try result.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = failure_layout,
        .compile_time_root = .{ .module = .{}, .root = @enumFromInt(0), .const_locator = null, .role = .{ .failure_message = .{
            .failed_field = 0,
            .message_field = 1,
            .failed_offset = failed_offset,
            .message_offset = message_offset,
        } } },
    });
    try result.const_plans.append(allocator, .str);
    try result.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = .str,
        .compile_time_root = .{ .module = .{}, .root = @enumFromInt(0), .const_locator = null, .role = .{ .value = .{ .failure_slot = @enumFromInt(0), .plan = @enumFromInt(0) } } },
    });
    const text = "a dependent root borrows this frozen string after its producer is dropped";
    const source_local = try result.store.addLocal(.{ .layout_idx = .str });
    const source_ret = try result.store.addCFStmt(.{ .ret = .{ .value = source_local } });
    const source_body = try result.store.addCFStmt(.{ .assign_literal = .{
        .target = source_local,
        .value = .{ .str_literal = .{ .backing = try result.store.insertString(text), .offset = 0, .len = text.len } },
        .next = source_ret,
    } });
    const source_proc = try result.store.addProcSpec(.{ .name = .fromRaw(0), .args = .empty(), .frame_locals = try result.store.addLocalSpan(&.{source_local}), .body = source_body, .ret_layout = .str });
    const consumer_local = try result.store.addLocal(.{ .layout_idx = .str });
    const consumer_ret = try result.store.addCFStmt(.{ .ret = .{ .value = consumer_local } });
    const live_int = try result.store.addLocal(.{ .layout_idx = .u64 });
    const live_float = try result.store.addLocal(.{ .layout_idx = .f64 });
    const expected_int = try result.store.addLocal(.{ .layout_idx = .u64 });
    const expected_float = try result.store.addLocal(.{ .layout_idx = .f64 });
    const equal_int = try result.store.addLocal(.{ .layout_idx = .bool });
    const equal_float = try result.store.addLocal(.{ .layout_idx = .bool });
    const crash_text = "suspended consumer locals changed";
    const corrupt = try result.store.addCFStmt(.{ .crash = .{ .msg = .{ .literal = try result.store.insertString(crash_text) } } });
    const float_branch = try result.store.addCFStmt(.{ .switch_stmt = .{ .cond = equal_float, .branches = try result.store.addCFSwitchBranches(&.{.{ .value = 1, .body = consumer_ret }}), .default_branch = corrupt } });
    const float_compare = try result.store.addCFStmt(.{ .assign_low_level = .{ .target = equal_float, .op = .num_is_eq, .rc_effect = .{}, .args = try result.store.addLocalSpan(&.{ live_float, expected_float }), .next = float_branch } });
    const int_branch = try result.store.addCFStmt(.{ .switch_stmt = .{ .cond = equal_int, .branches = try result.store.addCFSwitchBranches(&.{.{ .value = 1, .body = float_compare }}), .default_branch = corrupt } });
    const int_compare = try result.store.addCFStmt(.{ .assign_low_level = .{ .target = equal_int, .op = .num_is_eq, .rc_effect = .{}, .args = try result.store.addLocalSpan(&.{ live_int, expected_int }), .next = int_branch } });
    const expected_float_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = expected_float, .value = .{ .f64_literal = 13.25 }, .next = int_compare } });
    const expected_int_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = expected_int, .value = .{ .i64_literal = .{ .value = 12345, .layout_idx = .u64 } }, .next = expected_float_stmt } });
    const consumer_load = try result.store.addCFStmt(.{ .assign_literal = .{ .target = consumer_local, .value = .{ .static_data = @enumFromInt(1) }, .next = expected_int_stmt } });
    const input_int = try result.store.addLocal(.{ .layout_idx = .u64 });
    const addend_int = try result.store.addLocal(.{ .layout_idx = .u64 });
    const input_float = try result.store.addLocal(.{ .layout_idx = .f64 });
    const addend_float = try result.store.addLocal(.{ .layout_idx = .f64 });
    const live_float_stmt = try result.store.addCFStmt(.{ .assign_low_level = .{ .target = live_float, .op = .num_float_add, .rc_effect = .{}, .args = try result.store.addLocalSpan(&.{ input_float, addend_float }), .next = consumer_load } });
    const live_int_stmt = try result.store.addCFStmt(.{ .assign_low_level = .{ .target = live_int, .op = .num_int_add_wrap, .rc_effect = .{}, .args = try result.store.addLocalSpan(&.{ input_int, addend_int }), .next = live_float_stmt } });
    const float_addend_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = addend_float, .value = .{ .f64_literal = 3.25 }, .next = live_int_stmt } });
    const float_input_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = input_float, .value = .{ .f64_literal = 10.0 }, .next = float_addend_stmt } });
    const int_addend_stmt = try result.store.addCFStmt(.{ .assign_literal = .{ .target = addend_int, .value = .{ .i64_literal = .{ .value = 345, .layout_idx = .u64 } }, .next = float_input_stmt } });
    const consumer_body = try result.store.addCFStmt(.{ .assign_literal = .{ .target = input_int, .value = .{ .i64_literal = .{ .value = 12000, .layout_idx = .u64 } }, .next = int_addend_stmt } });
    const consumer_proc = try result.store.addProcSpec(.{ .name = .fromRaw(1), .args = .empty(), .frame_locals = try result.store.addLocalSpan(&.{ consumer_local, live_int, live_float, expected_int, expected_float, equal_int, equal_float, input_int, addend_int, input_float, addend_float }), .body = consumer_body, .ret_layout = .str });
    try lir.ComptimeValueGuards.insert(allocator, result);

    const failure_size = result.layouts.layoutSize(result.layouts.getLayout(failure_layout));
    const zeros = try allocator.alloc(u8, failure_size);
    defer allocator.free(zeros);
    @memset(zeros, 0);
    const failure_name = try LirProgram.staticDataSymbolName(allocator, @enumFromInt(0));
    defer allocator.free(failure_name);
    const value_name = try LirProgram.staticDataSymbolName(allocator, @enumFromInt(1));
    defer allocator.free(value_name);
    const materialized = try static_data_exports.cloneStaticData(allocator, &.{
        .{ .symbol_name = failure_name, .value_id = @enumFromInt(0), .bytes = zeros, .alignment = @alignOf(usize) },
        .{ .symbol_name = value_name, .value_id = @enumFromInt(1), .bytes = zeros[0..@sizeOf(builtins.str.RocStr)], .alignment = @alignOf(usize) },
    });
    var image = try backend.StaticDataImage.init(allocator, materialized);
    const addresses = try image.lirValueAddresses(allocator, 2);
    const owner = try allocator.create(InterpreterProgram);
    owner.allocator = allocator;
    owner.shared_slots = null;
    owner.slot_demand = null;
    owner.timing_io = null;
    owner.suspended_ns = 0;
    const failure_origins = try allocator.alloc(?lir.LIR.ComptimeFailureOrigin, result.store.getCFStmts().len);
    @memset(failure_origins, null);
    owner.slots = .{ .allocator = allocator, .materialized = materialized, .image = image, .addresses = addresses, .failure_origins = failure_origins };
    owner.host = CompilerHost.init(allocator);
    owner.static_callables = .empty;
    owner.interpreter = try Interpreter.initWithBoxyTables(allocator, &result.store, &result.layouts, Interpreter.BoxyTables.fromResult(result), owner.host.ops(), .normalize);
    defer owner.deinit();
    try owner.refreshCallableMetadata();
    if (nested) {
        const Demand = struct {
            owner: *InterpreterProgram,
            lowered: *lir.CheckedPipeline.LoweredProgram,
            proc: lir.LIR.LirProcSpecId,
            failure: ?[]const u8,
            cycle: bool,
            evaluations: usize = 0,

            fn ensure(raw: *anyopaque, _: lir.LIR.StaticDataId) (FinalizeError || error{CompileTimeDependencyCycle})!void {
                const self: *@This() = @ptrCast(@alignCast(raw));
                if (self.evaluations != 0) return;
                self.evaluations += 1;
                if (self.cycle) return error.CompileTimeDependencyCycle;
                const child = try self.owner.fork(self.lowered);
                defer child.deinit();
                if (self.failure) |message| {
                    child.slotEnvironment().publishFailureOrigin(self.lowered, .{}, @enumFromInt(0), .{
                        .loc = .{ .file = 0, .line = 7, .column = 3 },
                        .region = base.Region.from_raw_offsets(40, 51),
                    });
                    try child.slotEnvironment().publishFailure(self.lowered, .{}, @enumFromInt(0), message, .{ .resolve = InterpreterProgram.resolveFunction });
                } else {
                    const value = child.interpreter.eval(.{ .proc_id = self.proc, .ret_layout = .str }) catch |err| switch (err) {
                        error.OutOfMemory => return error.OutOfMemory,
                        else => unreachable,
                    };
                    defer child.interpreter.dropValue(value.value, .str);
                    try child.publishRoot(self.lowered, .{}, @enumFromInt(0), .{
                        .root_order = 0,
                        .request = .{ .order = 0, .module_idx = 0, .kind = .compile_time_constant, .source = undefined, .checked_type = undefined, .abi = .compile_time, .exposure = .private },
                        .proc = self.proc,
                        .ret_layout = .str,
                        .ret_type = undefined,
                        .plan = @enumFromInt(0),
                    }, value.value);
                }
            }
        };
        var demand = Demand{ .owner = owner, .lowered = &lowered, .proc = source_proc, .failure = failure_message, .cycle = cycle };
        owner.slot_demand = .{ .context = &demand, .ensure = Demand.ensure };
        const consumer = try owner.fork(&lowered);
        defer consumer.deinit();
        for (0..@as(usize, if (cycle) 1 else 2)) |_| {
            if (failure_message) |message| {
                try std.testing.expectError(error.Crash, consumer.interpreter.eval(.{ .proc_id = consumer_proc, .ret_layout = .str }));
                try std.testing.expectEqualStrings(message, consumer.interpreter.getCrashMessage().?);
                if (!cycle) {
                    try std.testing.expectEqual(base.SourceLoc{ .file = 0, .line = 7, .column = 3 }, consumer.interpreter.getFailedSourceLoc().?);
                    try std.testing.expectEqual(base.Region.from_raw_offsets(40, 51), consumer.interpreter.getFailedCheckedRegion().?);
                }
            } else {
                const value = try consumer.interpreter.eval(.{ .proc_id = consumer_proc, .ret_layout = .str });
                defer consumer.interpreter.dropValue(value.value, .str);
                const str: *const builtins.str.RocStr = @ptrCast(@alignCast(value.value.ptr));
                try std.testing.expectEqualStrings(text, str.asSlice());
            }
        }
        try std.testing.expectEqual(@as(usize, 1), demand.evaluations);
        if (failure_message == null and comptime backend.host_lir_codegen_available)
            try testNativeSlotDemand(&lowered, &owner.slots, source_proc, consumer_proc, text);
        return;
    }
    if (failure_message) |message| {
        try owner.slots.publishFailure(&lowered, .{}, @enumFromInt(0), message, .{ .resolve = InterpreterProgram.resolveFunction });
        const bytes: [*]const u8 = @ptrFromInt(addresses[0]);
        try std.testing.expectEqual(@as(u8, 1), bytes[failed_offset]);
        const stored: *const builtins.str.RocStr = @ptrCast(@alignCast(bytes + message_offset));
        try std.testing.expectEqualStrings(message, stored.asSlice());
        try std.testing.expectError(error.Crash, owner.interpreter.eval(.{ .proc_id = consumer_proc, .ret_layout = .str }));
        try std.testing.expectEqualStrings(message, owner.interpreter.getCrashMessage().?);
    } else {
        const value = try owner.interpreter.eval(.{ .proc_id = source_proc, .ret_layout = .str });
        try owner.publishRoot(&lowered, .{}, @enumFromInt(0), .{
            .root_order = 0,
            .request = .{ .order = 0, .module_idx = 0, .kind = .compile_time_constant, .source = undefined, .checked_type = undefined, .abi = .compile_time, .exposure = .private },
            .proc = source_proc,
            .ret_layout = .str,
            .ret_type = undefined,
            .plan = @enumFromInt(0),
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

fn testNativeSlotDemand(lowered: *lir.CheckedPipeline.LoweredProgram, slots: *StaticSlotEnvironment, producer: lir.LIR.LirProcSpecId, consumer: lir.LIR.LirProcSpecId, text: []const u8) (FinalizeError || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    if (comptime !backend.host_lir_codegen_available) return;
    const allocator = std.testing.allocator;
    var strings = try backend.StaticStringData.build(allocator, &lowered.lir_result.store, backend.dev.LirCodeGenMod.host_lir_codegen_target);
    defer strings.deinit();
    var codegen = try backend.HostLirCodeGen.initWithBoxyMetadata(allocator, &lowered.lir_result.store, &lowered.lir_result.layouts, strings.view(), &.{}, &.{}, &.{}, .normalize, roc_target.host_cpu.level());
    defer codegen.deinit();
    codegen.setNativeStaticData(slots.addresses);
    codegen.setComptimeHooks(.{
        .branch_taken = CompileTimeHost.rocComptimeBranchTaken,
        .exhaustiveness_failed = CompileTimeHost.rocComptimeExhaustivenessFailed,
        .failure_region = CompileTimeHost.rocComptimeFailureRegion,
        .ensure_static_value = CompileTimeHost.rocComptimeEnsureStaticValue,
        .call_enter = CompileTimeHost.rocComptimeCallEnter,
        .call_exit = CompileTimeHost.rocComptimeCallExit,
    });
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());
    const source_entry = try codegen.generateEntrypointWrapper("native_slot_source", producer, &.{}, .str);
    const consumer_entry = try codegen.generateEntrypointWrapper("native_slot_consumer", consumer, &.{}, .str);
    var executable = try backend.ExecutableMemory.initWithEntryOffset(codegen.getGeneratedCode(), 0);
    defer executable.deinit();
    const Demand = struct {
        executable: *backend.ExecutableMemory,
        lowered: *lir.CheckedPipeline.LoweredProgram,
        slots: *StaticSlotEnvironment,
        source_offset: usize,
        producer: lir.LIR.LirProcSpecId,
        evaluations: usize = 0,

        fn ensure(raw: *anyopaque, _: lir.LIR.StaticDataId) SlotDemand.Error!void {
            const self: *@This() = @ptrCast(@alignCast(raw));
            if (self.evaluations != 0) return;
            self.evaluations += 1;
            var child = CompileTimeHost.init(std.testing.allocator);
            defer child.deinit();
            var bytes: [@sizeOf(builtins.str.RocStr)]u8 align(16) = @splat(0);
            var boundary = child.enterCrashBoundary();
            if (boundary.set() == 0) self.executable.callRocABIAt(self.source_offset, @ptrCast(child.ops()), @ptrCast(&bytes), null);
            boundary.deinit();
            if (child.termination != .returned) return error.Unexpected;
            try self.slots.publishRoot(self.lowered, .{}, @enumFromInt(0), .{
                .root_order = 0,
                .request = .{ .order = 0, .module_idx = 0, .kind = .compile_time_constant, .source = undefined, .checked_type = undefined, .abi = .compile_time, .exposure = .private },
                .proc = self.producer,
                .ret_layout = .str,
                .ret_type = undefined,
                .plan = @enumFromInt(0),
            }, .{ .ptr = &bytes }, .{}, .{ .resolve = InterpreterProgram.resolveFunction });
        }
    };
    var demand = Demand{ .executable = &executable, .lowered = lowered, .slots = slots, .source_offset = source_entry.offset, .producer = producer };
    var host = CompileTimeHost.init(allocator);
    defer host.deinit();
    host.slot_demand = .{ .context = &demand, .ensure = Demand.ensure };
    for (0..2) |_| {
        host.resetForRun();
        var bytes: [@sizeOf(builtins.str.RocStr)]u8 align(16) = @splat(0);
        var boundary = host.enterCrashBoundary();
        if (boundary.set() == 0) executable.callRocABIAt(consumer_entry.offset, @ptrCast(host.ops()), @ptrCast(&bytes), null);
        boundary.deinit();
        try std.testing.expectEqual(CompileTimeHost.Termination.returned, host.termination);
        const str: *const builtins.str.RocStr = @ptrCast(&bytes);
        try std.testing.expectEqualStrings(text, str.asSlice());
    }
    try std.testing.expectEqual(@as(usize, 1), demand.evaluations);
}

test "shared frozen erased callables execute on interpreter dev and LLVM" {
    const allocator = std.testing.allocator;
    var program = try LirProgram.Result.init(allocator, .native);
    defer program.deinit();
    const erased_layout = try program.layouts.insertErasedCallable();
    try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = erased_layout });
    const capture_arg = try program.store.addLocal(.{ .layout_idx = .opaque_ptr });
    const reuse_arg = try program.store.addLocal(.{ .layout_idx = erased_layout });
    const answer = try program.store.addLocal(.{ .layout_idx = .bool });
    const worker_ret = try program.store.addCFStmt(.{ .ret = .{ .value = answer } });
    const worker_body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = answer, .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .bool } }, .next = worker_ret } });
    const arg_plan = try program.store.internErasedCallArgsPlan(&program.layouts, &.{});
    const worker = try program.store.addProcSpec(.{
        .name = .fromRaw(55),
        .abi = .erased_callable,
        .args = try program.store.addLocalSpan(&.{ capture_arg, reuse_arg }),
        .erased_capture_arg = capture_arg,
        .erased_reuse_arg = reuse_arg,
        .erased_call_args = arg_plan,
        .frame_locals = try program.store.addLocalSpan(&.{ capture_arg, reuse_arg, answer }),
        .body = worker_body,
        .ret_layout = .bool,
    });
    const closure = try program.store.addLocal(.{ .layout_idx = erased_layout });
    const call_result = try program.store.addLocal(.{ .layout_idx = .bool });
    const caller_ret = try program.store.addCFStmt(.{ .ret = .{ .value = call_result } });
    const call = try program.store.addCFStmt(.{ .assign_call_erased = .{ .target = call_result, .closure = closure, .args = .empty(), .arg_plan = arg_plan, .next = caller_ret } });
    const caller_body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = closure, .value = .{ .static_data = @enumFromInt(0) }, .next = call } });
    const caller = try program.store.addProcSpec(.{ .name = .fromRaw(56), .args = .empty(), .frame_locals = try program.store.addLocalSpan(&.{ closure, call_result }), .body = caller_body, .ret_layout = .bool });
    try program.const_plans.append(allocator, .{ .erased_fn = @enumFromInt(0) });
    try program.const_plans.append(allocator, .str);
    const captures = try allocator.dupe(LirProgram.CaptureSlot, &.{.{ .id = undefined, .slot = 0, .ty = undefined, .plan = @enumFromInt(1), .storage = .value }});
    const entries = try allocator.dupe(LirProgram.ErasedFn, &.{.{
        .entry = worker,
        .capture_layout = .str,
        .template = .{ .fn_def = undefined, .source_fn_ty = undefined, .source_fn_key = undefined },
        .captures = captures,
        .on_drop = .{ .rc_helper = .{ .op = .decref, .layout_idx = .str } },
    }});
    try program.erased_fns.append(allocator, .{ .layout = erased_layout, .entries = entries });
    const slot_name = try LirProgram.staticDataSymbolName(allocator, @enumFromInt(0));
    defer allocator.free(slot_name);
    const proc_name = try static_data_exports.procSymbolName(allocator, program.store.getProcSpec(worker).name);
    defer allocator.free(proc_name);
    const word = @sizeOf(usize);
    var payload_bytes: [4 * word + @sizeOf(builtins.str.RocStr)]u8 = @splat(0);
    const capture_str = builtins.str.RocStr.fromSliceSmall("capture");
    @memcpy(payload_bytes[4 * word ..], std.mem.asBytes(&capture_str));
    const drop_name = try static_data_exports.atomicRcHelperSymbolName(allocator, .{ .op = .decref, .layout_idx = .str });
    defer allocator.free(drop_name);
    const exports = try static_data_exports.cloneStaticData(allocator, &.{
        .{ .symbol_name = slot_name, .value_id = @enumFromInt(0), .bytes = &(@as([word]u8, @splat(0))), .alignment = @alignOf(usize), .relocations = &.{.{ .offset = 0, .target_symbol_name = "closure_payload", .target = .{ .data_symbol = @enumFromInt(1) }, .addend = 2 * word }} },
        .{ .symbol_name = "closure_payload", .bytes = &payload_bytes, .alignment = builtins.erased_callable.payload_alignment, .relocations = &.{ .{ .offset = 2 * word, .target_symbol_name = proc_name, .kind = .function_pointer, .procedure = worker, .callable_capture_offset = builtins.erased_callable.capture_offset }, .{ .offset = 3 * word, .target_symbol_name = drop_name, .kind = .function_pointer, .rc_helper = .{ .op = .decref, .layout_idx = .str } } } },
    });
    defer static_data_exports.deinitStaticData(allocator, exports);
    const StaticInterpreterData = @import("interpreter_static_data.zig").InterpreterStaticData;
    var data = try StaticInterpreterData.init(allocator, exports, 1);
    defer data.deinit();
    var host = CompilerHost.init(allocator);
    defer host.deinit();
    var interpreter = try Interpreter.initWithBoxyTables(allocator, &program.store, &program.layouts, Interpreter.BoxyTables.fromResult(&program), host.ops(), .normalize);
    defer interpreter.deinit();
    data.install(&interpreter);
    const first = try interpreter.eval(.{ .proc_id = caller, .ret_layout = .bool });
    try std.testing.expectEqual(@as(u8, 1), first.value.read(u8));
    const root_value = @import("value.zig").Value{ .ptr = @ptrFromInt(data.addresses[0]) };
    const payload_ptr: [*]u8 = @ptrFromInt(root_value.read(usize));
    const payload = builtins.erased_callable.payloadPtr(payload_ptr);
    var direct_answer: u8 = 0;
    var out_desc: ?*const anyopaque = null;
    payload.callable_fn_ptr(&interpreter.roc_ops, @ptrCast(&direct_answer), null, builtins.erased_callable.capturePtr(payload_ptr), null, &out_desc);
    try std.testing.expectEqual(@as(u8, 1), direct_answer);
    const Resolver = struct {
        fn resolve(raw: ?*anyopaque, ptr: [*]u8) NativeRootExport.CallableResolution {
            const interp: *Interpreter = @ptrCast(@alignCast(raw.?));
            const value = interp.interpretedCallable(ptr).?;
            return .{ .proc = value.proc, .capture_ptr = value.capture_ptr };
        }
    };
    const copied = try NativeRootExport.freezeRoot(allocator, &program, @enumFromInt(0), .{
        .root_order = 0,
        .request = .{ .order = 0, .module_idx = 0, .kind = .compile_time_constant, .source = undefined, .checked_type = undefined, .abi = .compile_time, .exposure = .private },
        .proc = caller,
        .ret_layout = erased_layout,
        .ret_type = undefined,
        .plan = @enumFromInt(0),
    }, root_value, .{ .context = &interpreter, .resolve = Resolver.resolve });
    defer static_data_exports.deinitStaticData(allocator, copied);
    var copied_data = try StaticInterpreterData.init(allocator, copied, 1);
    defer copied_data.deinit();
    copied_data.install(&interpreter);
    const second = try interpreter.eval(.{ .proc_id = caller, .ret_layout = .bool });
    try std.testing.expectEqual(@as(u8, 1), second.value.read(u8));
    // Execute only the mapped graph and its image-local procedure identities.
    // This is the same installation boundary used by interpreter shims.
    const image_bytes = try allocator.alignedAlloc(u8, collections.max_roc_alignment, 1024 * 1024);
    defer allocator.free(image_bytes);
    var fba = std.heap.FixedBufferAllocator.init(image_bytes);
    const header = try fba.allocator().create(lir.LirImage.Header);
    const image_program = try lir.LirImage.copyProgramWithStaticDataIntoBuffer(fba.allocator(), image_bytes.ptr, image_bytes.len, &program, &.{.{ .ordinal = 0, .root_proc = caller }}, copied);
    try image_program.fillHeader(header, fba.end_index);
    var view = try lir.LirImage.viewMappedImageWithAllocator(header, image_bytes.ptr, fba.end_index, .native, allocator);
    defer view.deinit();
    var mapped_data = try StaticInterpreterData.init(allocator, view.static_data, view.static_data_value_count);
    defer mapped_data.deinit();
    var mapped_interpreter = try Interpreter.initWithBoxyTables(allocator, &view.store, &view.layouts, Interpreter.BoxyTables.fromImageView(&view), host.ops(), .normalize);
    defer mapped_interpreter.deinit();
    mapped_data.install(&mapped_interpreter);
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
        const llvm_result = try Inspected.llvmEvalBoolRootModules(allocator, &.{module}, .speed);
        defer Inspected.deinitBoolRootEvalResults(allocator, llvm_result);
        try std.testing.expect(llvm_result[0].outcome.passed);
    }
    // Exercise the inspected-run ownership path with procedure and drop-helper
    // relocations, then return a separate frozen string through its Str ABI.
    try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = .str });
    const inspected_closure = try program.store.addLocal(.{ .layout_idx = erased_layout });
    const inspected_bool = try program.store.addLocal(.{ .layout_idx = .bool });
    const inspected_str = try program.store.addLocal(.{ .layout_idx = .str });
    const inspected_ret = try program.store.addCFStmt(.{ .ret = .{ .value = inspected_str } });
    const inspected_load = try program.store.addCFStmt(.{ .assign_literal = .{ .target = inspected_str, .value = .{ .static_data = @enumFromInt(1) }, .next = inspected_ret } });
    const inspected_call = try program.store.addCFStmt(.{ .assign_call_erased = .{ .target = inspected_bool, .closure = inspected_closure, .args = .empty(), .arg_plan = arg_plan, .next = inspected_load } });
    const inspected_body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = inspected_closure, .value = .{ .static_data = @enumFromInt(0) }, .next = inspected_call } });
    const inspected_root = try program.store.addProcSpec(.{ .name = .fromRaw(57), .args = .empty(), .frame_locals = try program.store.addLocalSpan(&.{ inspected_closure, inspected_bool, inspected_str }), .body = inspected_body, .ret_layout = .str });
    const result_name = try LirProgram.staticDataSymbolName(allocator, @enumFromInt(1));
    defer allocator.free(result_name);
    const result_str = builtins.str.RocStr.fromSliceSmall("relocated");
    const inspected_exports = try allocator.alloc(LirProgram.StaticDataExport, copied.len + 1);
    defer allocator.free(inspected_exports);
    @memcpy(inspected_exports[0..copied.len], copied);
    inspected_exports[copied.len] = .{ .symbol_name = result_name, .value_id = @enumFromInt(1), .bytes = std.mem.asBytes(&result_str), .alignment = @alignOf(usize), .relocations = &.{} };
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
        try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = .str });
        const value = try program.store.addLocal(.{ .layout_idx = .str });
        const ret = try program.store.addCFStmt(.{ .ret = .{ .value = value } });
        const body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = value, .value = .{ .static_data = @enumFromInt(0) }, .next = ret } });
        const root = try program.store.addProcSpec(.{ .name = .fromRaw(101), .args = .empty(), .frame_locals = try program.store.addLocalSpan(&.{value}), .body = body, .ret_layout = .str });
        const name = try LirProgram.staticDataSymbolName(allocator, @enumFromInt(0));
        defer allocator.free(name);
        const bytes = try allocator.alloc(u8, 3 * width.size());
        defer allocator.free(bytes);
        @memset(bytes, 0);
        @memcpy(bytes[0..6], "frozen");
        bytes[bytes.len - 1] = 0x80 | 6;
        const exports = [_]LirProgram.StaticDataExport{.{ .symbol_name = name, .value_id = @enumFromInt(0), .bytes = bytes, .alignment = width.size(), .relocations = &.{} }};
        const result = try InspectedRun.run(allocator, kind, .{ .store = &program.store, .layouts = &program.layouts, .main_proc = root, .static_data = &exports, .static_data_value_count = 1 }, if (kind == .interpreter) .reject else {});
        defer result.deinit(allocator);
        try std.testing.expect(result.outcome == .returned);
        try std.testing.expectEqualStrings("frozen", result.outcome.returned);
    }
}
