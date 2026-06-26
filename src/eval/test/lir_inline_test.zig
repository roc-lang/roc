//! Structural LIR tests for post-check wrapper inlining.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const eval = @import("eval");
const lir = @import("lir");
const postcheck = @import("postcheck");
const helpers = eval.test_helpers;

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;
const layout_mod = @import("layout");
const LayoutIdx = layout_mod.Idx;

var shared_test_builtins: ?eval.BuiltinModules = null;
var shared_test_builtins_mutex: std.Io.Mutex = .init;

const LoweredSource = struct {
    resources: helpers.ParsedResources,
    lowered: lir.CheckedPipeline.LoweredProgram,

    fn deinit(self: *LoweredSource, allocator: Allocator) void {
        self.lowered.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

const LiftedSource = struct {
    resources: helpers.ParsedResources,
    lifted: postcheck.MonotypeLifted.Ast.Program,

    fn deinit(self: *LiftedSource, allocator: Allocator) void {
        self.lifted.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

fn sharedPrePublishedBuiltin() anyerror!helpers.PrePublishedBuiltin {
    shared_test_builtins_mutex.lockUncancelable(std.testing.io);
    defer shared_test_builtins_mutex.unlock(std.testing.io);

    if (shared_test_builtins == null) {
        shared_test_builtins = try eval.BuiltinModules.init(std.heap.page_allocator);
    }

    return .{
        .env = shared_test_builtins.?.builtin_module.env,
        .indices = shared_test_builtins.?.builtin_indices,
        .artifact = &shared_test_builtins.?.checked_artifact,
    };
}

fn lowerModule(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
) anyerror!LoweredSource {
    return lowerModuleWithOptions(allocator, source, inline_mode, .{});
}

const LowerModuleOptions = struct {
    debug_effects: lir.CheckedPipeline.DebugEffectMode = .run,
    proc_debug_names: bool = false,
    tag_reachability: bool = false,
    imports: []const helpers.ModuleSource = &.{},
};

fn lowerModuleWithOptions(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    options: LowerModuleOptions,
) anyerror!LoweredSource {
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, source, options.imports, try sharedPrePublishedBuiltin());
    errdefer helpers.cleanupParseAndCanonical(allocator, resources);

    const import_count = resources.import_artifacts.len + if (resources.borrowed_builtin_artifact == null) @as(usize, 0) else 1;
    const import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, import_count);
    defer allocator.free(import_views);

    var view_index: usize = 0;
    if (resources.borrowed_builtin_artifact) |builtin_artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(builtin_artifact);
        view_index += 1;
    }
    for (resources.import_artifacts) |*artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(artifact);
        view_index += 1;
    }

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{ .requests = resources.checked_artifact.root_requests.requests },
        .{
            .target_usize = base.target.TargetUsize.native,
            .inline_mode = inline_mode,
            .debug_effects = options.debug_effects,
            .proc_debug_names = options.proc_debug_names,
            .tag_reachability = options.tag_reachability,
        },
    );
    errdefer lowered.deinit();

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

fn lowerModuleWithDebugEffects(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    debug_effects: lir.CheckedPipeline.DebugEffectMode,
) anyerror!LoweredSource {
    return lowerModuleWithOptions(allocator, source, inline_mode, .{ .debug_effects = debug_effects });
}

fn lowerModuleWithProcDebugNames(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    proc_debug_names: bool,
) anyerror!LoweredSource {
    return lowerModuleWithOptions(allocator, source, inline_mode, .{ .proc_debug_names = proc_debug_names });
}

fn mainProcArgLayouts(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) anyerror![]LayoutIdx {
    const proc = lowered.lir_result.store.getProcSpec(try rootProc(lowered));
    const arg_locals = lowered.lir_result.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(LayoutIdx, arg_locals.len);
    errdefer allocator.free(arg_layouts);

    for (arg_locals, 0..) |local_id, index| {
        arg_layouts[index] = lowered.lir_result.store.getLocal(local_id).layout_idx;
    }

    return arg_layouts;
}

fn runLoweredWithHostEvents(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) anyerror!eval.RuntimeHostEnv.RecordedRun {
    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interpreter = try eval.Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interpreter.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const result = interpreter.eval(.{
        .proc_id = try rootProc(lowered),
        .arg_layouts = arg_layouts,
    }) catch |err| switch (err) {
        error.Crash => return runtime_env.snapshot(allocator),
        else => return err,
    };
    switch (result) {
        .value => {},
    }

    return runtime_env.snapshot(allocator);
}

fn expectOptimizedDbgEvents(source: []const u8, expected: []const []const u8) anyerror!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var run = try runLoweredWithHostEvents(allocator, &optimized.lowered);
    defer run.deinit(allocator);

    try std.testing.expectEqual(eval.RuntimeHostEnv.Termination.returned, run.termination);
    try std.testing.expectEqual(expected.len, run.events.len);
    for (expected, run.events) |expected_event, actual_event| {
        switch (actual_event) {
            .dbg => |msg| try std.testing.expectEqualStrings(expected_event, msg),
            else => return error.TestUnexpectedResult,
        }
    }
}

const ExpectedHostEvent = union(enum) {
    dbg: []const u8,
    expect_failed,
    crashed: []const u8,
};

fn expectOptimizedHostEvents(
    source: []const u8,
    expected_termination: eval.RuntimeHostEnv.Termination,
    expected: []const ExpectedHostEvent,
) anyerror!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var run = try runLoweredWithHostEvents(allocator, &optimized.lowered);
    defer run.deinit(allocator);

    try std.testing.expectEqual(expected_termination, run.termination);
    try std.testing.expectEqual(expected.len, run.events.len);
    for (expected, run.events) |expected_event, actual_event| {
        switch (expected_event) {
            .dbg => |expected_msg| switch (actual_event) {
                .dbg => |actual_msg| try std.testing.expectEqualStrings(expected_msg, actual_msg),
                else => return error.TestUnexpectedResult,
            },
            .expect_failed => switch (actual_event) {
                .expect_failed => {},
                else => return error.TestUnexpectedResult,
            },
            .crashed => |expected_msg| switch (actual_event) {
                .crashed => |actual_msg| try std.testing.expectEqualStrings(expected_msg, actual_msg),
                else => return error.TestUnexpectedResult,
            },
        }
    }
}

const DebugEffectCounts = struct {
    debug: usize = 0,
    expect: usize = 0,
};

fn countDebugEffectStmts(lowered: *const lir.CheckedPipeline.LoweredProgram) DebugEffectCounts {
    var counts = DebugEffectCounts{};
    for (lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .debug => counts.debug += 1,
            .expect => counts.expect += 1,
            else => {},
        }
    }
    return counts;
}

test "optimized debug effect lowering erases inline dbg and expect" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\main : I64
        \\main = {
        \\    dbg 1
        \\    expect False
        \\    expect 1 == 1
        \\    2
        \\}
    ;

    var run_effects = try lowerModuleWithDebugEffects(allocator, source, .wrappers, .run);
    defer run_effects.deinit(allocator);

    const run_counts = countDebugEffectStmts(&run_effects.lowered);
    try std.testing.expect(run_counts.debug > 0);
    try std.testing.expect(run_counts.expect > 0);

    var erased_effects = try lowerModuleWithDebugEffects(allocator, source, .wrappers, .erase);
    defer erased_effects.deinit(allocator);

    const erased_counts = countDebugEffectStmts(&erased_effects.lowered);
    try std.testing.expectEqual(@as(usize, 0), erased_counts.debug);
    try std.testing.expectEqual(@as(usize, 0), erased_counts.expect);
}

test "nominal record lays out fields in declared order" {
    const allocator = std.testing.allocator;
    // Declared order { z: U16, y: U16, x: U32 } is padding-free, so it is kept
    // verbatim. It differs from both alphabetical order ({ x, y, z }) and the
    // descending-alignment sort, which would both hoist the U32 to offset 0.
    const source =
        \\module [main]
        \\
        \\Account := { z : U16, y : U16, x : U32 }
        \\
        \\main : Account -> Account
        \\main = |account| account
    ;

    var lowered_source = try lowerModule(allocator, source, .wrappers);
    defer lowered_source.deinit(allocator);
    const lowered = &lowered_source.lowered;

    const proc = lowered.lir_result.store.getProcSpec(try rootProc(lowered));
    const layout_val = lowered.lir_result.layouts.getLayout(proc.ret_layout);
    try std.testing.expectEqual(layout_mod.LayoutTag.struct_, layout_val.tag);

    const struct_idx = layout_val.getStruct().idx;
    // Field at memory position 0 is the first declared field z (U16); an
    // alphabetical or alignment layout would put the U32 (x) there instead.
    try std.testing.expectEqual(LayoutIdx.u16, lowered.lir_result.layouts.getStructFieldLayout(struct_idx, 0));
    // z (original/lexicographic field index 2) at offset 0, x (index 0) at 4.
    try std.testing.expectEqual(@as(u32, 0), lowered.lir_result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 2));
    try std.testing.expectEqual(@as(u32, 4), lowered.lir_result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try std.testing.expectEqual(@as(u32, 8), lowered.lir_result.layouts.getStructData(struct_idx).size);
}

test "imported nominal record lays out fields in declared order" {
    const allocator = std.testing.allocator;
    const acct_module =
        \\module [Account]
        \\
        \\Account := { z : U16, y : U16, x : U32 }
    ;
    // An imported nominal record must lay out identically to a local one, or
    // values would be read with the wrong offsets across module boundaries.
    const source =
        \\module [main]
        \\
        \\import Acct exposing [Account]
        \\
        \\main : Account -> Account
        \\main = |account| account
    ;

    var lowered_source = try lowerModuleWithOptions(allocator, source, .wrappers, .{
        .imports = &.{.{ .name = "Acct", .source = acct_module }},
    });
    defer lowered_source.deinit(allocator);
    const lowered = &lowered_source.lowered;

    const proc = lowered.lir_result.store.getProcSpec(try rootProc(lowered));
    const layout_val = lowered.lir_result.layouts.getLayout(proc.ret_layout);
    try std.testing.expectEqual(layout_mod.LayoutTag.struct_, layout_val.tag);

    const struct_idx = layout_val.getStruct().idx;
    try std.testing.expectEqual(LayoutIdx.u16, lowered.lir_result.layouts.getStructFieldLayout(struct_idx, 0));
    try std.testing.expectEqual(@as(u32, 8), lowered.lir_result.layouts.getStructData(struct_idx).size);
}

test "nominal record reserves unnamed padding fields without inflating alignment" {
    const allocator = std.testing.allocator;
    // Mirrors a C `struct { uint8_t a; char pad[3]; uint32_t b; }`: the three
    // unnamed bytes hold the explicit padding so `b` lands at offset 4 without
    // the compiler inserting alignment padding of its own.
    const source =
        \\module [main]
        \\
        \\Padded := { a : U8, _ : U8, _ : U8, _ : U8, b : U32 }
        \\
        \\main : Padded -> Padded
        \\main = |padded| padded
    ;

    var lowered_source = try lowerModule(allocator, source, .wrappers);
    defer lowered_source.deinit(allocator);
    const lowered = &lowered_source.lowered;

    const proc = lowered.lir_result.store.getProcSpec(try rootProc(lowered));
    const layout_val = lowered.lir_result.layouts.getLayout(proc.ret_layout);
    try std.testing.expectEqual(layout_mod.LayoutTag.struct_, layout_val.tag);

    const struct_idx = layout_val.getStruct().idx;
    // The committed struct keeps the named fields plus three padding spacers.
    try std.testing.expectEqual(@as(u16, 5), lowered.lir_result.layouts.getStructData(struct_idx).fields.count);
    // Named field a (lexicographic index 0) at offset 0, b (index 1) at offset 4.
    try std.testing.expectEqual(@as(u32, 0), lowered.lir_result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try std.testing.expectEqual(@as(u32, 4), lowered.lir_result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1));
    // Total size 8 and alignment 4 (padding bytes are alignment 1, so they do
    // not raise the struct's alignment above the U32's).
    try std.testing.expectEqual(@as(u32, 8), lowered.lir_result.layouts.getStructData(struct_idx).size);
    try std.testing.expectEqual(@as(u64, 4), layout_val.alignment(.u64).toByteUnits());
}

test "generic nominal record instantiates unnamed padding to the argument's size" {
    const allocator = std.testing.allocator;
    // A type-parameterized unnamed field (`_ : a`) must reserve the *instantiated*
    // size, exactly like a named field of the same type: `Foo(U64)` is 16 bytes
    // (x:U64 @0 plus 8 bytes of padding), just as `{ x : a, y : a }(U64)` would be.
    const source =
        \\module [main]
        \\
        \\Foo(a) := { x : a, _ : a }
        \\
        \\main : Foo(U64) -> Foo(U64)
        \\main = |foo| foo
    ;

    var lowered_source = try lowerModule(allocator, source, .wrappers);
    defer lowered_source.deinit(allocator);
    const lowered = &lowered_source.lowered;

    const proc = lowered.lir_result.store.getProcSpec(try rootProc(lowered));
    const layout_val = lowered.lir_result.layouts.getLayout(proc.ret_layout);
    try std.testing.expectEqual(layout_mod.LayoutTag.struct_, layout_val.tag);

    const struct_idx = layout_val.getStruct().idx;
    // x (the only named field) at offset 0; padding reserves the instantiated
    // sizeof(U64) = 8 bytes, so the whole struct is 16 bytes (not 8).
    try std.testing.expectEqual(@as(u16, 2), lowered.lir_result.layouts.getStructData(struct_idx).fields.count);
    try std.testing.expectEqual(@as(u32, 0), lowered.lir_result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try std.testing.expectEqual(@as(u32, 16), lowered.lir_result.layouts.getStructData(struct_idx).size);
}

test "nominal record with a parenthesized backing still honors declared order and padding" {
    const allocator = std.testing.allocator;
    // The backing record is wrapped in parentheses. Parens are transparent here:
    // the unnamed field must still be accepted and the layout must match the
    // unparenthesized form (a@0, b@4, size 8, with three padding spacers).
    const source =
        \\module [main]
        \\
        \\Padded := ({ a : U8, _ : U8, _ : U8, _ : U8, b : U32 })
        \\
        \\main : Padded -> Padded
        \\main = |padded| padded
    ;

    var lowered_source = try lowerModule(allocator, source, .wrappers);
    defer lowered_source.deinit(allocator);
    const lowered = &lowered_source.lowered;

    const proc = lowered.lir_result.store.getProcSpec(try rootProc(lowered));
    const layout_val = lowered.lir_result.layouts.getLayout(proc.ret_layout);
    try std.testing.expectEqual(layout_mod.LayoutTag.struct_, layout_val.tag);

    const struct_idx = layout_val.getStruct().idx;
    try std.testing.expectEqual(@as(u16, 5), lowered.lir_result.layouts.getStructData(struct_idx).fields.count);
    try std.testing.expectEqual(@as(u32, 0), lowered.lir_result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try std.testing.expectEqual(@as(u32, 4), lowered.lir_result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1));
    try std.testing.expectEqual(@as(u32, 8), lowered.lir_result.layouts.getStructData(struct_idx).size);
}

fn liftModuleAfterSpecConstr(
    allocator: Allocator,
    source: []const u8,
) anyerror!LiftedSource {
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, source, &.{}, try sharedPrePublishedBuiltin());
    errdefer helpers.cleanupParseAndCanonical(allocator, resources);

    const import_count = resources.import_artifacts.len + if (resources.borrowed_builtin_artifact == null) @as(usize, 0) else 1;
    const import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, import_count);
    defer allocator.free(import_views);

    var view_index: usize = 0;
    if (resources.borrowed_builtin_artifact) |builtin_artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(builtin_artifact);
        view_index += 1;
    }
    for (resources.import_artifacts) |*artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(artifact);
        view_index += 1;
    }

    var mono = try postcheck.Monotype.Lower.run(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{ .requests = resources.checked_artifact.root_requests.requests },
        .{},
    );
    var mono_owned = true;
    errdefer if (mono_owned) mono.deinit();

    var lifted = try postcheck.MonotypeLifted.Lift.run(allocator, mono);
    mono_owned = false;
    mono = undefined;
    errdefer lifted.deinit();

    try postcheck.MonotypeLifted.SpecConstr.run(allocator, &lifted);

    return .{
        .resources = resources,
        .lifted = lifted,
    };
}

fn expectInlinePlanDecision(
    source: []const u8,
    fn_name: []const u8,
    expected: bool,
) anyerror!void {
    const allocator = std.testing.allocator;
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, source, &.{}, try sharedPrePublishedBuiltin());
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    const import_count = resources.import_artifacts.len + if (resources.borrowed_builtin_artifact == null) @as(usize, 0) else 1;
    const import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, import_count);
    defer allocator.free(import_views);

    var view_index: usize = 0;
    if (resources.borrowed_builtin_artifact) |builtin_artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(builtin_artifact);
        view_index += 1;
    }
    for (resources.import_artifacts) |*artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(artifact);
        view_index += 1;
    }

    var mono = try postcheck.Monotype.Lower.run(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{ .requests = resources.checked_artifact.root_requests.requests },
        .{ .proc_debug_names = true },
    );
    var mono_owned = true;
    errdefer if (mono_owned) mono.deinit();

    var lifted = try postcheck.MonotypeLifted.Lift.run(allocator, mono);
    mono_owned = false;
    mono = undefined;
    var lifted_owned = true;
    errdefer if (lifted_owned) lifted.deinit();

    var solved = try postcheck.LambdaSolved.Solve.run(allocator, lifted);
    lifted_owned = false;
    lifted = undefined;
    defer solved.deinit();

    var inline_plan = try postcheck.SolvedInline.analyze(allocator, .wrappers, &solved);
    defer inline_plan.deinit();
    const plan = inline_plan.view();

    var found = false;
    for (solved.lifted.fns.items, 0..) |fn_, index| {
        const name_id = solved.lifted.procDebugName(fn_.symbol) orelse continue;
        const actual_name = solved.lifted.names.exportNameText(name_id);
        if (!std.mem.eql(u8, actual_name, fn_name)) continue;

        found = true;
        const fn_id: postcheck.MonotypeLifted.Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
        try std.testing.expectEqual(expected, plan.bodyForFn(fn_id) != null);
    }

    try std.testing.expect(found);
}

fn rootProc(lowered: *const lir.CheckedPipeline.LoweredProgram) anyerror!LIR.LirProcSpecId {
    try std.testing.expectEqual(@as(usize, 1), lowered.lir_result.root_procs.items.len);
    return lowered.lir_result.root_procs.items[0];
}

fn collectAssignCallProcs(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
) anyerror![]LIR.LirProcSpecId {
    const proc = lowered.lir_result.store.getProcSpec(proc_id);
    const body = proc.body orelse return allocator.alloc(LIR.LirProcSpecId, 0);

    var calls = std.ArrayList(LIR.LirProcSpecId).empty;
    errdefer calls.deinit(allocator);

    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, body);

    var visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |stmt_id| {
        const visited_entry = try visited.getOrPut(stmt_id);
        if (visited_entry.found_existing) continue;

        switch (lowered.lir_result.store.getCFStmt(stmt_id)) {
            .assign_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_literal => |stmt| try work.append(allocator, stmt.next),
            .init_uninitialized => |stmt| try work.append(allocator, stmt.next),
            .assign_call => |stmt| {
                try calls.append(allocator, stmt.proc);
                try work.append(allocator, stmt.next);
            },
            .assign_call_erased => |stmt| try work.append(allocator, stmt.next),
            .assign_packed_erased_fn => |stmt| try work.append(allocator, stmt.next),
            .assign_low_level => |stmt| try work.append(allocator, stmt.next),
            .assign_list => |stmt| try work.append(allocator, stmt.next),
            .assign_struct => |stmt| try work.append(allocator, stmt.next),
            .assign_tag => |stmt| try work.append(allocator, stmt.next),
            .store_struct => |stmt| try work.append(allocator, stmt.next),
            .store_tag => |stmt| try work.append(allocator, stmt.next),
            .set_local => |stmt| try work.append(allocator, stmt.next),
            .debug => |stmt| try work.append(allocator, stmt.next),
            .expect => |stmt| try work.append(allocator, stmt.next),
            .comptime_branch_taken => |stmt| try work.append(allocator, stmt.next),
            .incref => |stmt| try work.append(allocator, stmt.next),
            .decref => |stmt| try work.append(allocator, stmt.next),
            .decref_if_initialized => |stmt| try work.append(allocator, stmt.next),
            .free => |stmt| try work.append(allocator, stmt.next),
            .switch_stmt => |stmt| {
                if (stmt.continuation) |continuation| try work.append(allocator, continuation);
                try work.append(allocator, stmt.default_branch);
                for (lowered.lir_result.store.getCFSwitchBranches(stmt.branches)) |branch| {
                    try work.append(allocator, branch.body);
                }
            },
            .switch_initialized_payload => |stmt| {
                try work.append(allocator, stmt.initialized_branch);
                try work.append(allocator, stmt.uninitialized_branch);
            },
            .str_match => |stmt| {
                try work.append(allocator, stmt.on_match);
                try work.append(allocator, stmt.on_miss);
            },
            .str_match_set => |stmt| {
                for (lowered.lir_result.store.getStrMatchArms(stmt.arms)) |arm| {
                    try work.append(allocator, arm.on_match);
                }
                try work.append(allocator, stmt.on_miss);
            },
            .join => |stmt| {
                try work.append(allocator, stmt.body);
                try work.append(allocator, stmt.remainder);
            },
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            .expect_err,
            => {},
        }
    }

    return try calls.toOwnedSlice(allocator);
}

const ProcShape = struct {
    arg_count: usize,
    direct_call_count: usize = 0,
    erased_call_count: usize = 0,
    packed_erased_fn_count: usize = 0,
    low_level_count: usize = 0,
    list_len_count: usize = 0,
    list_get_unsafe_count: usize = 0,
    list_with_capacity_count: usize = 0,
    list_append_unsafe_count: usize = 0,
    list_reserve_count: usize = 0,
    str_count_utf8_bytes_count: usize = 0,
    str_concat_count: usize = 0,
    box_box_count: usize = 0,
    box_unbox_count: usize = 0,
    box_prepare_update_count: usize = 0,
    ptr_cast_count: usize = 0,
    ptr_load_count: usize = 0,
    ptr_store_count: usize = 0,
    self_call_count: usize = 0,
    switch_count: usize = 0,
    str_match_set_count: usize = 0,
    join_count: usize = 0,
    max_join_param_count: usize = 0,
    jump_count: usize = 0,
    struct_assign_count: usize = 0,
    tag_assign_count: usize = 0,
    store_struct_count: usize = 0,
    store_tag_count: usize = 0,
};

fn collectProcShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
) anyerror!ProcShape {
    const proc = lowered.lir_result.store.getProcSpec(proc_id);
    var shape = ProcShape{
        .arg_count = lowered.lir_result.store.getLocalSpan(proc.args).len,
    };

    const body = proc.body orelse return shape;

    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, body);

    var visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |stmt_id| {
        const visited_entry = try visited.getOrPut(stmt_id);
        if (visited_entry.found_existing) continue;

        switch (lowered.lir_result.store.getCFStmt(stmt_id)) {
            .assign_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_literal => |stmt| try work.append(allocator, stmt.next),
            .init_uninitialized => |stmt| try work.append(allocator, stmt.next),
            .assign_call => |stmt| {
                shape.direct_call_count += 1;
                if (stmt.proc == proc_id) shape.self_call_count += 1;
                try work.append(allocator, stmt.next);
            },
            .assign_call_erased => |stmt| {
                shape.erased_call_count += 1;
                try work.append(allocator, stmt.next);
            },
            .assign_packed_erased_fn => |stmt| {
                shape.packed_erased_fn_count += 1;
                try work.append(allocator, stmt.next);
            },
            .assign_low_level => |stmt| {
                shape.low_level_count += 1;
                switch (stmt.op) {
                    .list_len => shape.list_len_count += 1,
                    .list_get_unsafe => shape.list_get_unsafe_count += 1,
                    .list_with_capacity => shape.list_with_capacity_count += 1,
                    .list_append_unsafe => shape.list_append_unsafe_count += 1,
                    .list_reserve => shape.list_reserve_count += 1,
                    .str_count_utf8_bytes => shape.str_count_utf8_bytes_count += 1,
                    .str_concat => shape.str_concat_count += 1,
                    .box_box => shape.box_box_count += 1,
                    .box_unbox => shape.box_unbox_count += 1,
                    .box_prepare_update => shape.box_prepare_update_count += 1,
                    .ptr_cast => shape.ptr_cast_count += 1,
                    .ptr_load => shape.ptr_load_count += 1,
                    .ptr_store => shape.ptr_store_count += 1,
                    else => {},
                }
                try work.append(allocator, stmt.next);
            },
            .assign_list => |stmt| try work.append(allocator, stmt.next),
            .assign_struct => |stmt| {
                shape.struct_assign_count += 1;
                try work.append(allocator, stmt.next);
            },
            .assign_tag => |stmt| {
                shape.tag_assign_count += 1;
                try work.append(allocator, stmt.next);
            },
            .store_struct => |stmt| {
                shape.store_struct_count += 1;
                try work.append(allocator, stmt.next);
            },
            .store_tag => |stmt| {
                shape.store_tag_count += 1;
                try work.append(allocator, stmt.next);
            },
            .set_local => |stmt| try work.append(allocator, stmt.next),
            .debug => |stmt| try work.append(allocator, stmt.next),
            .expect => |stmt| try work.append(allocator, stmt.next),
            .comptime_branch_taken => |stmt| try work.append(allocator, stmt.next),
            .incref => |stmt| try work.append(allocator, stmt.next),
            .decref => |stmt| try work.append(allocator, stmt.next),
            .decref_if_initialized => |stmt| try work.append(allocator, stmt.next),
            .free => |stmt| try work.append(allocator, stmt.next),
            .switch_stmt => |stmt| {
                shape.switch_count += 1;
                if (stmt.continuation) |continuation| try work.append(allocator, continuation);
                try work.append(allocator, stmt.default_branch);
                for (lowered.lir_result.store.getCFSwitchBranches(stmt.branches)) |branch| {
                    try work.append(allocator, branch.body);
                }
            },
            .switch_initialized_payload => |stmt| {
                shape.switch_count += 1;
                try work.append(allocator, stmt.initialized_branch);
                try work.append(allocator, stmt.uninitialized_branch);
            },
            .str_match => |stmt| {
                try work.append(allocator, stmt.on_match);
                try work.append(allocator, stmt.on_miss);
            },
            .str_match_set => |stmt| {
                shape.str_match_set_count += 1;
                for (lowered.lir_result.store.getStrMatchArms(stmt.arms)) |arm| {
                    try work.append(allocator, arm.on_match);
                }
                try work.append(allocator, stmt.on_miss);
            },
            .join => |stmt| {
                shape.join_count += 1;
                shape.max_join_param_count = @max(
                    shape.max_join_param_count,
                    lowered.lir_result.store.getLocalSpan(stmt.params).len,
                );
                try work.append(allocator, stmt.body);
                try work.append(allocator, stmt.remainder);
            },
            .jump => {
                shape.jump_count += 1;
            },
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .loop_continue,
            .loop_break,
            .ret,
            .crash,
            .expect_err,
            => {},
        }
    }

    return shape;
}

const IterCollectShape = enum {
    specialized,
    generic,
};

fn procShapeMatchesIterCollect(shape: ProcShape, wanted: IterCollectShape) bool {
    // Fingerprints of the `Iter.collect` -> `List.from_iter` worker over a range.
    // `from_iter` branches on the iterator's length: a Known length reserves the
    // whole allocation up front and writes each item with the unchecked append,
    // while an Unknown length grows with the reserving append. That per-element
    // branch (the inner `match length`) is the extra switch/join/jump over the
    // earlier single-append loop. Spec constr specializes the worker for the
    // concrete element type, and because ranges carry a Known length (via each
    // numeric type's `steps_between`) the specialized worker threads that count
    // as a third arg (`with_capacity` preallocation).
    return switch (wanted) {
        .specialized => shape.arg_count == 3 and
            shape.direct_call_count >= 5 and
            shape.switch_count >= 10 and
            shape.join_count >= 16 and
            shape.jump_count >= 20,
        .generic => shape.arg_count == 1 and
            shape.direct_call_count == 4 and
            shape.switch_count == 8 and
            shape.join_count == 12 and
            shape.jump_count == 15 and
            shape.struct_assign_count >= 8,
    };
}

fn reachableIterCollectShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    wanted: IterCollectShape,
) anyerror!bool {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = std.AutoHashMap(LIR.LirProcSpecId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |proc_id| {
        const visited_entry = try visited.getOrPut(proc_id);
        if (visited_entry.found_existing) continue;

        const shape = try collectProcShape(allocator, lowered, proc_id);
        if (procShapeMatchesIterCollect(shape, wanted)) return true;

        const calls = try collectAssignCallProcs(allocator, lowered, proc_id);
        defer allocator.free(calls);
        for (calls) |call| try work.append(allocator, call);
    }
    return false;
}

fn reachableProcShapeCount(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime matches: fn (ProcShape) bool,
) anyerror!usize {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = std.AutoHashMap(LIR.LirProcSpecId, void).init(allocator);
    defer visited.deinit();

    var count: usize = 0;
    while (work.pop()) |proc_id| {
        const visited_entry = try visited.getOrPut(proc_id);
        if (visited_entry.found_existing) continue;

        const shape = try collectProcShape(allocator, lowered, proc_id);
        if (matches(shape)) count += 1;

        const calls = try collectAssignCallProcs(allocator, lowered, proc_id);
        defer allocator.free(calls);
        for (calls) |call| try work.append(allocator, call);
    }
    return count;
}

fn reachableProcDebugName(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    expected_name: []const u8,
) anyerror!bool {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = std.AutoHashMap(LIR.LirProcSpecId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |proc_id| {
        const visited_entry = try visited.getOrPut(proc_id);
        if (visited_entry.found_existing) continue;

        if (lowered.lir_result.store.procDebugName(proc_id)) |name| {
            if (std.mem.eql(u8, name, expected_name)) return true;
        }

        const calls = try collectAssignCallProcs(allocator, lowered, proc_id);
        defer allocator.free(calls);
        for (calls) |call| try work.append(allocator, call);
    }
    return false;
}

fn reachableProcShapeFieldTotal(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime field_name: []const u8,
) anyerror!usize {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = std.AutoHashMap(LIR.LirProcSpecId, void).init(allocator);
    defer visited.deinit();

    var total: usize = 0;
    while (work.pop()) |proc_id| {
        const visited_entry = try visited.getOrPut(proc_id);
        if (visited_entry.found_existing) continue;

        const shape = try collectProcShape(allocator, lowered, proc_id);
        total += @field(shape, field_name);

        const calls = try collectAssignCallProcs(allocator, lowered, proc_id);
        defer allocator.free(calls);
        for (calls) |call| try work.append(allocator, call);
    }
    return total;
}

fn expectReachableProcShapeFieldNoGreater(
    allocator: Allocator,
    iter_lowered: *const lir.CheckedPipeline.LoweredProgram,
    list_lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime field_name: []const u8,
) anyerror!void {
    const iter_total = try reachableProcShapeFieldTotal(allocator, iter_lowered, field_name);
    const list_total = try reachableProcShapeFieldTotal(allocator, list_lowered, field_name);
    if (iter_total > list_total) {
        std.debug.print(
            "{s}: iter form has {d}, direct-list form has {d}\n",
            .{ field_name, iter_total, list_total },
        );
    }
    try std.testing.expect(iter_total <= list_total);
}

fn expectStaticListIterAppendLoopNoBulkierThanDirectList(
    iter_source: []const u8,
    list_source: []const u8,
) anyerror!void {
    const allocator = std.testing.allocator;
    var iter_optimized = try lowerModuleWithOptions(allocator, iter_source, .wrappers, .{ .tag_reachability = true });
    defer iter_optimized.deinit(allocator);
    var list_optimized = try lowerModuleWithOptions(allocator, list_source, .wrappers, .{ .tag_reachability = true });
    defer list_optimized.deinit(allocator);

    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "direct_call_count");
    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "switch_count");
    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "struct_assign_count");
    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "tag_assign_count");
    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "join_count");
}

fn reachableProcShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime matches: fn (ProcShape) bool,
) anyerror!bool {
    return (try reachableProcShapeCount(allocator, lowered, matches)) > 0;
}

fn expectNoReachableErasedCallableLowering(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) anyerror!void {
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, lowered, "erased_call_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, lowered, "packed_erased_fn_count"));
}

fn reachableReturnSlotProcCount(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) anyerror!usize {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = std.AutoHashMap(LIR.LirProcSpecId, void).init(allocator);
    defer visited.deinit();

    var count: usize = 0;
    while (work.pop()) |proc_id| {
        const visited_entry = try visited.getOrPut(proc_id);
        if (visited_entry.found_existing) continue;

        const proc = lowered.lir_result.store.getProcSpec(proc_id);
        const args = lowered.lir_result.store.getLocalSpan(proc.args);
        if (proc.ret_layout == .zst and args.len != 0) candidate: {
            const first_arg_layout = lowered.lir_result.layouts.getLayout(
                lowered.lir_result.store.getLocal(args[0]).layout_idx,
            );
            if (first_arg_layout.tag != .ptr) break :candidate;
            const result_layout = lowered.lir_result.layouts.getLayout(first_arg_layout.getIdx());
            switch (result_layout.tag) {
                .struct_, .tag_union => {},
                else => break :candidate,
            }
            const shape = try collectProcShape(allocator, lowered, proc_id);
            if (shape.ptr_store_count != 0 or shape.store_struct_count != 0 or shape.store_tag_count != 0) count += 1;
        }

        const calls = try collectAssignCallProcs(allocator, lowered, proc_id);
        defer allocator.free(calls);
        for (calls) |call| try work.append(allocator, call);
    }
    return count;
}

fn countHostedLiftedFns(program: *const postcheck.MonotypeLifted.Ast.Program) usize {
    var count: usize = 0;
    for (program.fns.items) |fn_| {
        switch (fn_.body) {
            .roc => {},
            .hosted => count += 1,
        }
    }
    return count;
}

fn directRecordWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0;
}

fn directRecordWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count >= 1;
}

fn whileRecordStateWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 2 and
        shape.jump_count >= 2;
}

fn whileRecordStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 1 and
        shape.jump_count >= 2;
}

fn localLoopStateIsSplitToTwoLeaves(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 2 and
        shape.jump_count >= 2;
}

fn whileRecordStateWithCallableCapturesIsSpecialized(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 3 and
        shape.jump_count >= 2;
}

fn whileRecordStateWithZeroCaptureCallableIsSpecialized(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 1 and
        shape.jump_count >= 2 and
        shape.direct_call_count == 0;
}

fn whileRecordStateWithOpaqueCallableIsSpecialized(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 2 and
        shape.jump_count >= 2;
}

fn branchJoinedRecordStateWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 2 and
        shape.jump_count >= 2;
}

fn branchJoinedRecordStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 1 and
        shape.jump_count >= 2;
}

fn directTupleWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0;
}

fn directTupleWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count >= 1;
}

fn unusedStateWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0;
}

fn unusedStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count >= 1;
}

fn taggedStepWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.tag_assign_count == 0;
}

fn taggedStepWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.tag_assign_count >= 1;
}

fn multiTupleWorkerIsFullySpecialized(shape: ProcShape) bool {
    return shape.arg_count == 5 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0;
}

fn multiTupleWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 3 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count >= 2;
}

fn opaqueLetCallWorkerDoesNotDuplicateCall(shape: ProcShape) bool {
    return shape.direct_call_count == 0 and
        shape.low_level_count == 2 and
        shape.struct_assign_count == 0;
}

fn opaqueLetCallWorkerDuplicatesCall(shape: ProcShape) bool {
    return shape.low_level_count > 2 and
        shape.struct_assign_count == 0;
}

fn hasGroupedStrMatchSet(shape: ProcShape) bool {
    return shape.str_match_set_count == 1;
}

fn expectRangeMapCollectUsesDirectListLoop(source: []const u8, expected_append_unsafe_count: usize) anyerror!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    try std.testing.expect(!try reachableIterCollectShape(allocator, &optimized.lowered, .specialized));
    try std.testing.expect(!try reachableIterCollectShape(allocator, &optimized.lowered, .generic));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_len_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_get_unsafe_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_with_capacity_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_reserve_count"));
    try std.testing.expectEqual(expected_append_unsafe_count, try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_append_unsafe_count"));
}

fn rootDirectCallTarget(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) anyerror!LIR.LirProcSpecId {
    const root = try rootProc(lowered);
    const root_calls = try collectAssignCallProcs(allocator, lowered, root);
    defer allocator.free(root_calls);

    try std.testing.expectEqual(@as(usize, 1), root_calls.len);
    return root_calls[0];
}

fn expectRootDirectCallCount(
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    expected: usize,
) anyerror!void {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator, source, inline_mode);
    defer lowered_source.deinit(allocator);

    const root_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, try rootProc(&lowered_source.lowered));
    defer allocator.free(root_calls);

    try std.testing.expectEqual(expected, root_calls.len);
}

fn expectRootTargetHasCalls(
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
) anyerror!void {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator, source, inline_mode);
    defer lowered_source.deinit(allocator);

    const target = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    const target_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, target);
    defer allocator.free(target_calls);

    try std.testing.expect(target_calls.len > 0);
}

test "direct call wrapper is inlined when inline mode is enabled" {
    try expectRootDirectCallCount(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| callee(x)
        \\
        \\main : U64
        \\main = wrapper(41)
    , .wrappers, 0);
}

test "direct call wrapper is not inlined when inline mode is none" {
    try expectRootTargetHasCalls(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| callee(x)
        \\
        \\main : U64
        \\main = wrapper(41)
    , .none);
}

test "zero statement block wrapper is inlined" {
    try expectRootDirectCallCount(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| {
        \\    callee(x)
        \\}
        \\
        \\main : U64
        \\main = wrapper(41)
    , .wrappers, 0);
}

test "low level wrapper is inlined when inline mode is enabled" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\main : Str -> U64
        \\main = |str| Str.count_utf8_bytes(str)
    , .wrappers);
    defer lowered_source.deinit(allocator);

    const shape = try collectProcShape(allocator, &lowered_source.lowered, try rootProc(&lowered_source.lowered));
    try std.testing.expectEqual(@as(usize, 0), shape.direct_call_count);
    try std.testing.expectEqual(@as(usize, 1), shape.str_count_utf8_bytes_count);
}

test "user single method is not recognized as builtin single iterator" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\Boxed := [Boxed].{
        \\    single : I64 -> Iter(I64)
        \\    single = |item| Iter.single(item)
        \\}
        \\
        \\main : I64
        \\main = {
        \\    var $sum = 0.I64
        \\    for item in Boxed.single(42.I64) {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
    , .wrappers);
    defer lowered_source.deinit(allocator);

    const shape = try collectProcShape(allocator, &lowered_source.lowered, try rootProc(&lowered_source.lowered));
    try std.testing.expect(shape.direct_call_count > 0 or shape.tag_assign_count > 0 or shape.store_tag_count > 0);
}

test "user iter method is not recognized as builtin list cursor" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\Bag := [Bag].{
        \\    iter : Bag -> Iter(I64)
        \\    iter = |_| Iter.single(1.I64)
        \\}
        \\
        \\main : I64
        \\main = {
        \\    var $sum = 0.I64
        \\    for item in Bag.Bag {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
    , .wrappers);
    defer lowered_source.deinit(allocator);

    const shape = try collectProcShape(allocator, &lowered_source.lowered, try rootProc(&lowered_source.lowered));
    try std.testing.expectEqual(@as(usize, 0), shape.list_len_count);
    try std.testing.expectEqual(@as(usize, 0), shape.list_get_unsafe_count);
}

test "destination baseline: boxed record update reboxes a list and string payload" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\Plant : {
        \\    x : I32,
        \\    label : Str,
        \\}
        \\
        \\Model : {
        \\    tick : U64,
        \\    label : Str,
        \\    plants : List(Plant),
        \\}
        \\
        \\State : [Running(Model), Done(Str)]
        \\
        \\step : Box(State) -> Box(State)
        \\step = |boxed| {
        \\    state = Box.unbox(boxed)
        \\
        \\    next =
        \\        match state {
        \\            Running(model) => {
        \\                plants = List.append(model.plants, { x: 160, label: model.label })
        \\                Running({ ..model, tick: model.tick + 1, plants })
        \\            }
        \\
        \\            Done(msg) => Done(Str.concat(msg, "!"))
        \\        }
        \\
        \\    Box.box(next)
        \\}
        \\
        \\main : Box(State) -> Box(State)
        \\main = |boxed| step(boxed)
    , .wrappers);
    defer lowered_source.deinit(allocator);

    const step_proc = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    const shape = try collectProcShape(allocator, &lowered_source.lowered, step_proc);

    try std.testing.expectEqual(@as(usize, 1), shape.box_unbox_count);
    try std.testing.expectEqual(@as(usize, 1), shape.box_box_count);
    try std.testing.expect(shape.struct_assign_count >= 2);
    try std.testing.expect(shape.tag_assign_count >= 2);
}

test "destination phase 3: direct boxed update wrapper calls a return-slot variant" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\Model : {
        \\    tick : U64,
        \\    label : Str,
        \\}
        \\
        \\update : Model -> Model
        \\update = |model| {
        \\    tick = model.tick + 1
        \\    { ..model, tick }
        \\}
        \\
        \\step : Box(Model) -> Box(Model)
        \\step = |boxed| Box.box(update(Box.unbox(boxed)))
        \\
        \\main : Box(Model) -> Box(Model)
        \\main = |boxed| step(boxed)
    , .wrappers);
    defer lowered_source.deinit(allocator);

    const root_shape = try collectProcShape(allocator, &lowered_source.lowered, try rootProc(&lowered_source.lowered));

    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "box_unbox_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "box_box_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "box_prepare_update_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "ptr_cast_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "ptr_load_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "ptr_store_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "store_struct_count"));
    try std.testing.expectEqual(@as(usize, 0), root_shape.ptr_store_count);
    try std.testing.expectEqual(@as(usize, 1), try reachableReturnSlotProcCount(allocator, &lowered_source.lowered));
}

test "destination baseline: boxed lambda is packed then boxed" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\Formatter : U64 -> Str
        \\
        \\make : Str -> Box(Formatter)
        \\make = |prefix| Box.box(|n| Str.concat(prefix, U64.to_str(n)))
        \\
        \\main : Str -> Box(Formatter)
        \\main = |prefix| make(prefix)
    , .none);
    defer lowered_source.deinit(allocator);

    const make_proc = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    const shape = try collectProcShape(allocator, &lowered_source.lowered, make_proc);

    try std.testing.expectEqual(@as(usize, 1), shape.packed_erased_fn_count);
}

test "destination baseline: large record return feeds a record update" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\Big : {
        \\    label : Str,
        \\    items : List(U64),
        \\    a : U64,
        \\    b : U64,
        \\    c : U64,
        \\    d : U64,
        \\    e : U64,
        \\}
        \\
        \\make_big : Str, U64 -> Big
        \\make_big = |label, n| {
        \\    label,
        \\    items: [n, n + 1],
        \\    a: n,
        \\    b: n + 1,
        \\    c: n + 2,
        \\    d: n + 3,
        \\    e: n + 4,
        \\}
        \\
        \\change_big : Str, U64 -> Big
        \\change_big = |label, n| { ..make_big(label, n), e: n + 5 }
        \\
        \\main : Str, U64 -> Big
        \\main = |label, n| change_big(label, n)
    , .none);
    defer lowered_source.deinit(allocator);

    const change_proc = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    const shape = try collectProcShape(allocator, &lowered_source.lowered, change_proc);

    try std.testing.expect(shape.direct_call_count >= 1);
    try std.testing.expect(shape.struct_assign_count >= 1);
}

test "destination phase 6: string concat caller uses append variant" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\suffix : Str -> Str
        \\suffix = |input| {
        \\    middle = if input == "" { input } else { input }
        \\    Str.concat(middle, "!")
        \\}
        \\
        \\build : Str -> Str
        \\build = |input| {
        \\    prefix = if input == "" { "pre" } else { "pre" }
        \\    result = suffix(input)
        \\    Str.concat(prefix, result)
        \\}
        \\
        \\main : Str -> Str
        \\main = |input| {
        \\    held = input
        \\    build(held)
        \\}
    , .wrappers);
    defer lowered_source.deinit(allocator);

    const build_proc = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    const build_shape = try collectProcShape(allocator, &lowered_source.lowered, build_proc);

    try std.testing.expectEqual(@as(usize, 1), build_shape.direct_call_count);
    try std.testing.expectEqual(@as(usize, 0), build_shape.str_concat_count);

    const build_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, build_proc);
    defer allocator.free(build_calls);

    try std.testing.expectEqual(@as(usize, 1), build_calls.len);

    const append_shape = try collectProcShape(allocator, &lowered_source.lowered, build_calls[0]);

    try std.testing.expectEqual(@as(usize, 2), append_shape.arg_count);
    try std.testing.expectEqual(@as(usize, 0), append_shape.direct_call_count);
    try std.testing.expectEqual(@as(usize, 2), append_shape.str_concat_count);
    try std.testing.expectEqual(@as(usize, 2), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "str_concat_count"));
}

test "block wrapper with statements is not inlined" {
    try expectInlinePlanDecision(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| {
        \\    y = x
        \\    callee(y)
        \\}
        \\
        \\main : U64
        \\main = wrapper(41)
    , "wrapper", false);
}

test "call value wrapper is not inlined" {
    try expectInlinePlanDecision(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\apply : (U64 -> U64), U64 -> U64
        \\apply = |fn, x| fn(x)
        \\
        \\main : U64
        \\main = apply(callee, 41)
    , "apply", false);
}

test "self-recursive direct wrapper is not inlined" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| wrapper(x)
        \\
        \\main : U64 -> U64
        \\main = |x| wrapper(x)
    , .wrappers);
    defer lowered_source.deinit(allocator);

    // The root still calls the wrapper as a separate proc (not inlined). The
    // wrapper's own self-call is gone: the TRMC pass rewrote it into a tail
    // jump, recorded as a TCE transform.
    const target = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    try std.testing.expectEqual(
        LIR.TailTransform.tce,
        lowered_source.lowered.lir_result.store.getProcSpec(target).tail_transform,
    );
    const target_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, target);
    defer allocator.free(target_calls);
    try std.testing.expectEqual(@as(usize, 0), target_calls.len);
}

test "mutually recursive direct wrappers are not inlined" {
    try expectRootTargetHasCalls(
        \\module [main]
        \\
        \\a : U64 -> U64
        \\a = |x| b(x)
        \\
        \\b : U64 -> U64
        \\b = |x| a(x)
        \\
        \\main : U64 -> U64
        \\main = |x| a(x)
    , .wrappers);
}

test "capturing direct wrapper is not inlined" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\main : U64 -> U64
        \\main = |offset| {
        \\    wrapper = |x| callee(x + offset)
        \\    wrapper(41)
        \\}
    , .wrappers);
    defer lowered_source.deinit(allocator);

    const root_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, try rootProc(&lowered_source.lowered));
    defer allocator.free(root_calls);

    try std.testing.expectEqual(@as(usize, 1), root_calls.len);
    const target_shape = try collectProcShape(allocator, &lowered_source.lowered, root_calls[0]);
    try std.testing.expectEqual(@as(usize, 2), target_shape.arg_count);
}
// ─── TRMC pass outcomes through the full pipeline ───

fn expectRootTargetTailTransform(
    source: []const u8,
    expected: LIR.TailTransform,
) anyerror!void {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator, source, .none);
    defer lowered_source.deinit(allocator);

    const target = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    try std.testing.expectEqual(
        expected,
        lowered_source.lowered.lir_result.store.getProcSpec(target).tail_transform,
    );
}

test "trmc: recursive list builder is TRMC-transformed through the pipeline" {
    try expectRootTargetTailTransform(
        \\module [main]
        \\
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\repeat : I64, I64 -> LinkedList
        \\repeat = |value, n| if n <= 0.I64 LinkedList.Nil else LinkedList.Cons(value, repeat(value, n - 1))
        \\
        \\main = repeat(7.I64, 3.I64)
    , .trmc);
}

test "trmc: accumulator recursion is TCE-transformed through the pipeline" {
    try expectRootTargetTailTransform(
        \\module [main]
        \\
        \\sum_to : I64, I64 -> I64
        \\sum_to = |n, acc| if n == 0.I64 acc else sum_to(n - 1, acc + n)
        \\
        \\main = sum_to(10.I64, 0.I64)
    , .tce);
}

test "trmc: result used before the constructor is not transformed" {
    try expectRootTargetTailTransform(
        \\module [main]
        \\
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\length_acc : LinkedList, I64 -> I64
        \\length_acc = |list, acc| match list {
        \\    Nil => acc
        \\    Cons(_, rest) => length_acc(rest, acc + 1)
        \\}
        \\
        \\with_lengths : I64 -> LinkedList
        \\with_lengths = |n| if n <= 0.I64 LinkedList.Nil else {
        \\    rest = with_lengths(n - 1)
        \\    LinkedList.Cons(length_acc(rest, 0), rest)
        \\}
        \\
        \\main = with_lengths(4.I64)
    , .none);
}

test "plant iter pipeline collect uses direct range map list loop" {
    try expectRangeMapCollectUsesDirectListLoop(
        \\module [main]
        \\
        \\Plant : { seed : I64 }
        \\
        \\random_plant : I64 -> Plant
        \\random_plant = |seed| { seed: seed }
        \\
        \\starting_plants : () -> List(Plant)
        \\starting_plants = || {
        \\    (0.I64..=15)
        \\        .map(|i| random_plant(i * 12))
        \\        .collect()
        \\}
        \\
        \\main : () -> List(Plant)
        \\main = || starting_plants()
    , 2);
}

test "known-length List.iter collect specializes without unbound locals" {
    // Regression: collecting a Known-length iterator (List.iter) under
    // optimization specializes a recursive capturing worker (List.iter's `make`
    // step). The specializer must reuse the source capture local ids; otherwise
    // a leftover direct call to the un-specialized worker references an unbound
    // capture local, which the ARC borrow certifier rejects. (Also exercises the
    // ARC use-after-realloc fix, since main's rewrite emits an owned variant.)
    const allocator = std.testing.allocator;
    var optimized = try lowerModule(allocator,
        \\module [main]
        \\
        \\main : List(I64)
        \\main =
        \\    Iter.collect(
        \\        Iter.map(List.iter([1.I64, 2, 3]), |i| i * 12),
        \\    )
    , .wrappers);
    defer optimized.deinit(allocator);
}

test "direct range map collect uses direct list loop" {
    try expectRangeMapCollectUsesDirectListLoop(
        \\module [main]
        \\
        \\Plant : { seed : I64 }
        \\
        \\random_plant : I64 -> Plant
        \\random_plant = |seed| { seed: seed }
        \\
        \\main : () -> List(Plant)
        \\main = ||
        \\    Iter.collect(
        \\        Iter.map(0.I64..=15, |i| random_plant(i * 12)),
        \\    )
    , 2);
}

test "imported iterator producer keeps finite step callables" {
    const allocator = std.testing.allocator;
    const producer_module =
        \\module [points]
        \\
        \\Point : { x : I64 }
        \\
        \\points : () -> Iter(Point)
        \\points = || [{ x: 1.I64 }, { x: 2 }].iter().append({ x: 3 })
    ;
    const source =
        \\module [main]
        \\
        \\import Points
        \\
        \\main : I64
        \\main = {
        \\    iter = Points.points()
        \\    var $sum = 0.I64
        \\    for point in iter {
        \\        $sum = $sum + point.x
        \\    }
        \\    $sum
        \\}
    ;

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{
        .imports = &.{.{ .name = "Points", .source = producer_module }},
    });
    defer optimized.deinit(allocator);

    try expectNoReachableErasedCallableLowering(allocator, &optimized.lowered);
}

test "static list iter append loop eliminates public iter adapters" {
    const allocator = std.testing.allocator;
    const iter_source =
        \\module [main]
        \\
        \\Point : { x : I64, y : I64 }
        \\
        \\sum_points : U64 -> I64
        \\sum_points = |anim_index| {
        \\    base_points = [
        \\        { x: 11, y: 2 },
        \\        { x: 13, y: 3 },
        \\        { x: 3, y: 5 },
        \\        { x: 11, y: 6 },
        \\    ].iter()
        \\
        \\    collision_points =
        \\        if anim_index == 2 {
        \\            base_points.append({ x: 2, y: 1 }).append({ x: 7, y: 1 })
        \\        } else if anim_index == 1 {
        \\            base_points.append({ x: 2, y: 2 })
        \\        } else {
        \\            base_points
        \\        }
        \\
        \\    var $sum = 0
        \\    for { x, y } in collision_points {
        \\        $sum = $sum + x + y
        \\    }
        \\    $sum
        \\}
        \\
        \\main : I64
        \\main = sum_points(2)
    ;
    const list_source =
        \\module [main]
        \\
        \\Point : { x : I64, y : I64 }
        \\
        \\sum_points : U64 -> I64
        \\sum_points = |anim_index| {
        \\    base_points = [
        \\        { x: 11, y: 2 },
        \\        { x: 13, y: 3 },
        \\        { x: 3, y: 5 },
        \\        { x: 11, y: 6 },
        \\    ]
        \\
        \\    collision_points =
        \\        if anim_index == 2 {
        \\            base_points.append({ x: 2, y: 1 }).append({ x: 7, y: 1 })
        \\        } else if anim_index == 1 {
        \\            base_points.append({ x: 2, y: 2 })
        \\        } else {
        \\            base_points
        \\        }
        \\
        \\    var $sum = 0
        \\    for { x, y } in collision_points {
        \\        $sum = $sum + x + y
        \\    }
        \\    $sum
        \\}
        \\
        \\main : I64
        \\main = sum_points(2)
    ;

    var iter_optimized = try lowerModuleWithProcDebugNames(allocator, iter_source, .wrappers, true);
    defer iter_optimized.deinit(allocator);
    var list_optimized = try lowerModuleWithProcDebugNames(allocator, list_source, .wrappers, true);
    defer list_optimized.deinit(allocator);

    try std.testing.expect(!try reachableProcDebugName(allocator, &iter_optimized.lowered, "Builtin.List.iter"));
    try std.testing.expect(!try reachableProcDebugName(allocator, &iter_optimized.lowered, "Builtin.Iter.append"));
    try std.testing.expect(!try reachableProcDebugName(allocator, &iter_optimized.lowered, "iter_from_step"));
    try std.testing.expect(!try reachableProcDebugName(allocator, &list_optimized.lowered, "Builtin.Iter.append"));
}

test "static record list iter append loop lowers no bulkier than direct list loop" {
    const record_iter_source =
        \\module [main]
        \\
        \\Point : { x : I64, y : I64 }
        \\
        \\sum_points : U64 -> I64
        \\sum_points = |anim_index| {
        \\    base_points = [
        \\        { x: 11, y: 2 },
        \\        { x: 13, y: 3 },
        \\        { x: 3, y: 5 },
        \\        { x: 11, y: 6 },
        \\    ].iter()
        \\
        \\    collision_points =
        \\        if anim_index == 2 {
        \\            base_points.append({ x: 2, y: 1 }).append({ x: 7, y: 1 })
        \\        } else if anim_index == 1 {
        \\            base_points.append({ x: 2, y: 2 })
        \\        } else {
        \\            base_points
        \\        }
        \\
        \\    var $sum = 0
        \\    for { x, y } in collision_points {
        \\        $sum = $sum + x + y
        \\    }
        \\    $sum
        \\}
        \\
        \\main : I64
        \\main = sum_points(2)
    ;
    const record_list_source =
        \\module [main]
        \\
        \\Point : { x : I64, y : I64 }
        \\
        \\sum_points : U64 -> I64
        \\sum_points = |anim_index| {
        \\    base_points = [
        \\        { x: 11, y: 2 },
        \\        { x: 13, y: 3 },
        \\        { x: 3, y: 5 },
        \\        { x: 11, y: 6 },
        \\    ]
        \\
        \\    collision_points =
        \\        if anim_index == 2 {
        \\            base_points.append({ x: 2, y: 1 }).append({ x: 7, y: 1 })
        \\        } else if anim_index == 1 {
        \\            base_points.append({ x: 2, y: 2 })
        \\        } else {
        \\            base_points
        \\        }
        \\
        \\    var $sum = 0
        \\    for { x, y } in collision_points {
        \\        $sum = $sum + x + y
        \\    }
        \\    $sum
        \\}
        \\
        \\main : I64
        \\main = sum_points(2)
    ;

    try expectStaticListIterAppendLoopNoBulkierThanDirectList(record_iter_source, record_list_source);
}

test "static primitive list iter append loop lowers no bulkier than direct list loop" {
    const primitive_iter_source =
        \\module [main]
        \\
        \\sum_points : U64 -> I64
        \\sum_points = |anim_index| {
        \\    base_points = [11.I64, 13, 3, 11].iter()
        \\
        \\    collision_points =
        \\        if anim_index == 2 {
        \\            base_points.append(2).append(7)
        \\        } else if anim_index == 1 {
        \\            base_points.append(2)
        \\        } else {
        \\            base_points
        \\        }
        \\
        \\    var $sum = 0
        \\    for point in collision_points {
        \\        $sum = $sum + point
        \\    }
        \\    $sum
        \\}
        \\
        \\main : I64
        \\main = sum_points(2)
    ;
    const primitive_list_source =
        \\module [main]
        \\
        \\sum_points : U64 -> I64
        \\sum_points = |anim_index| {
        \\    base_points = [11.I64, 13, 3, 11]
        \\
        \\    collision_points =
        \\        if anim_index == 2 {
        \\            base_points.append(2).append(7)
        \\        } else if anim_index == 1 {
        \\            base_points.append(2)
        \\        } else {
        \\            base_points
        \\        }
        \\
        \\    var $sum = 0
        \\    for point in collision_points {
        \\        $sum = $sum + point
        \\    }
        \\    $sum
        \\}
        \\
        \\main : I64
        \\main = sum_points(2)
    ;

    try expectStaticListIterAppendLoopNoBulkierThanDirectList(primitive_iter_source, primitive_list_source);
}

test "stream from iterator collect keeps finite step callables" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\main : () => List(I64)
        \\main = || {
        \\    stream =
        \\        [1.I64, 2]
        \\            .iter()
        \\            .append(3)
        \\            .stream()
        \\            .map!(|n| n + 1)
        \\
        \\    Stream.collect!(stream)
        \\}
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    try expectNoReachableErasedCallableLowering(allocator, &optimized.lowered);
}

test "spec constr list filter-map loop does not produce unbound ARC locals" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\main : List(I32)
        \\main = {
        \\    var $out = []
        \\    for item in [] {
        \\        $out = $out.append(item)
        \\    }
        \\    $out
        \\}
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);
}

test "spec constr does not duplicate opaque let-bound direct calls" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tick : I64 -> I64
        \\tick = |n| n + 1
        \\
        \\read_twice : State -> I64
        \\read_twice = |state| {
        \\    x = tick(state.n)
        \\    x + x
        \\}
        \\
        \\main : I64
        \\main = read_twice({ n: 1 })
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, opaqueLetCallWorkerDoesNotDuplicateCall));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, opaqueLetCallWorkerDuplicatesCall));
}

test "spec constr does not duplicate opaque known-match payloads" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64 }
        \\Step : [One(I64)]
        \\
        \\tick : I64 -> I64
        \\tick = |n| n + 1
        \\
        \\read_twice : State -> I64
        \\read_twice = |state|
        \\    match One(tick(state.n)) {
        \\        One(x) => x + x
        \\    }
        \\
        \\main : I64
        \\main = read_twice({ n: 1 })
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, opaqueLetCallWorkerDoesNotDuplicateCall));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, opaqueLetCallWorkerDuplicatesCall));
}

test "spec constr preserves direct call argument effect order" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "arg"
        \\    n
        \\}
        \\
        \\use_after : State, I64 -> I64
        \\use_after = |state, x| {
        \\    dbg "callee-before"
        \\    state.n + x
        \\}
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    use_after({ n: state.n }, tap(state.n))
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"arg\"", "\"callee-before\"" });
}

test "spec constr preserves left-to-right order for multiple unsafe call args" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tap_one : I64 -> I64
        \\tap_one = |n| {
        \\    dbg "arg-one"
        \\    n
        \\}
        \\
        \\tap_two : I64 -> I64
        \\tap_two = |n| {
        \\    dbg "arg-two"
        \\    n + 1
        \\}
        \\
        \\combine_after : State, I64, I64 -> I64
        \\combine_after = |state, x, y| {
        \\    dbg "callee-before"
        \\    state.n + x + y
        \\}
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    combine_after({ n: state.n }, tap_one(state.n), tap_two(state.n))
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"arg-one\"", "\"arg-two\"", "\"callee-before\"" });
}

test "spec constr preserves substituted capture order before direct call args" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tap_capture : I64 -> I64
        \\tap_capture = |n| {
        \\    dbg "capture"
        \\    n
        \\}
        \\
        \\tap_arg : I64 -> I64
        \\tap_arg = |n| {
        \\    dbg "arg"
        \\    n
        \\}
        \\
        \\outer : State, I64 -> I64
        \\outer = |state, seed| {
        \\    inner = |next, arg| {
        \\        dbg "callee-before"
        \\        seed + next.n + arg
        \\    }
        \\    inner({ n: seed }, tap_arg(state.n))
        \\}
        \\
        \\main : I64
        \\main = outer({ n: 1 }, tap_capture(2))
    , &.{ "\"capture\"", "\"arg\"", "\"callee-before\"" });
}

test "spec constr preserves callable argument effect order" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "arg"
        \\    n
        \\}
        \\
        \\call_it : State, (I64 -> I64) -> I64
        \\call_it = |state, f|
        \\    f(tap(state.n))
        \\
        \\outer : State -> I64
        \\outer = |state| {
        \\    f = |x| {
        \\        dbg "fn-before"
        \\        x
        \\    }
        \\    call_it({ n: state.n }, f)
        \\}
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"arg\"", "\"fn-before\"" });
}

test "spec constr preserves known-match single-use payload effect order" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\Step : [One(I64)]
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "payload"
        \\    n
        \\}
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    match One(tap(state.n)) {
        \\        One(x) => {
        \\            dbg "branch-before"
        \\            x
        \\        }
        \\    }
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"payload\"", "\"branch-before\"" });
}

test "spec constr preserves nested known-match payload effect order" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\Step : [One({ item : I64 })]
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "payload"
        \\    n
        \\}
        \\
        \\consume : State, Step -> I64
        \\consume = |state, step|
        \\    match step {
        \\        One({ item }) => {
        \\            dbg "branch-before"
        \\            state.n + item
        \\        }
        \\    }
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    consume({ n: state.n }, One({ item: tap(state.n) }))
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"payload\"", "\"branch-before\"" });
}

test "spec constr preserves known-match expect failure order" {
    try expectOptimizedHostEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\Step : [One({ item : I64 })]
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "payload"
        \\    n
        \\}
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    match One({ item: tap(state.n) }) {
        \\        One({ item }) => {
        \\            dbg "branch-before"
        \\            expect False
        \\            item
        \\        }
        \\    }
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , .returned, &.{
        .{ .dbg = "\"payload\"" },
        .{ .dbg = "\"branch-before\"" },
        .expect_failed,
    });
}

test "spec constr preserves known-match crash order" {
    try expectOptimizedHostEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\Step : [One({ item : I64 })]
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "payload"
        \\    n
        \\}
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    match One({ item: tap(state.n) }) {
        \\        One({ item: _ }) => {
        \\            dbg "branch-before"
        \\            crash "boom"
        \\        }
        \\    }
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , .crashed, &.{
        .{ .dbg = "\"payload\"" },
        .{ .dbg = "\"branch-before\"" },
        .{ .crashed = "boom" },
    });
}

test "spec constr writes dynamically discovered workers once" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Step : [Start(I64), Loop(I64)]
        \\
        \\go : Step -> I64
        \\go = |step|
        \\    match step {
        \\        Start(n) => {
        \\            next = Loop(n)
        \\            go(next)
        \\        }
        \\        Loop(n) => tick(n)
        \\    }
        \\
        \\tick : I64 -> I64
        \\tick = |n| n + 1
        \\
        \\main : I64
        \\main = go(Start(1))
    ;

    var lifted = try liftModuleAfterSpecConstr(allocator, source);
    defer lifted.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), countHostedLiftedFns(&lifted.lifted));
}

test "spec constr specializes recursive record state" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_record : State -> I64
        \\sum_record = |state|
        \\    if state.n == 0 {
        \\        state.acc
        \\    } else {
        \\        sum_record({ n: state.n - 1, acc: state.acc + state.n })
        \\    }
        \\
        \\main : I64
        \\main = sum_record({ n: 4, acc: 0 })
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's SpecConstr examples for inspected loop state.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, directRecordWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, directRecordWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, directRecordWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, directRecordWorkerIsGeneric));
}

test "spec constr specializes record state carried by while loop" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Start : { n : I64 }
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_from : Start -> I64
        \\sum_from = |start| {
        \\    var $state = { n: start.n, acc: 0 }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
        \\    }
        \\
        \\    $state.acc
        \\}
        \\
        \\main : I64
        \\main = sum_from({ n: 4 })
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
}

test "spec constr specializes primitive-start record state carried by while loop" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_from : I64 -> I64
        \\sum_from = |start| {
        \\    var $state = { n: start, acc: 0 }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
        \\    }
        \\
        \\    $state.acc
        \\}
        \\
        \\main : I64
        \\main = sum_from(4)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
}

test "spec constr does not require single-field record wrapper for local loop splitting" {
    const allocator = std.testing.allocator;
    const wrapped_source =
        \\module [main]
        \\
        \\Start : { n : I64 }
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_from : Start -> I64
        \\sum_from = |start| {
        \\    var $state = { n: start.n, acc: 0 }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
        \\    }
        \\
        \\    $state.acc
        \\}
        \\
        \\main : I64
        \\main = sum_from({ n: 4 })
    ;
    const primitive_source =
        \\module [main]
        \\
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_from : I64 -> I64
        \\sum_from = |start| {
        \\    var $state = { n: start, acc: 0 }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
        \\    }
        \\
        \\    $state.acc
        \\}
        \\
        \\main : I64
        \\main = sum_from(4)
    ;

    var wrapped_optimized = try lowerModule(allocator, wrapped_source, .wrappers);
    defer wrapped_optimized.deinit(allocator);
    var primitive_optimized = try lowerModule(allocator, primitive_source, .wrappers);
    defer primitive_optimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &wrapped_optimized.lowered, localLoopStateIsSplitToTwoLeaves));
    try std.testing.expect(try reachableProcShape(allocator, &primitive_optimized.lowered, localLoopStateIsSplitToTwoLeaves));
}

test "spec constr splits loop record state with opaque callable field" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, f : I64 -> I64 }
        \\
        \\inc : I64 -> I64
        \\inc = |n| n + 1
        \\
        \\sum_from : I64 -> I64
        \\sum_from = |start| {
        \\    var $state = { n: start, f: inc }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, f: $state.f }
        \\    }
        \\
        \\    f = $state.f
        \\    f($state.n)
        \\}
        \\
        \\main : I64
        \\main = sum_from(4)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWithZeroCaptureCallableIsSpecialized));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
}

test "spec constr splits loop record state with direct callable captures" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, f : I64 -> I64 }
        \\
        \\sum_from : I64, I64, I64 -> I64
        \\sum_from = |start, scale, offset| {
        \\    f = |n| n * scale + offset
        \\    var $state = { n: start, f }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, f: $state.f }
        \\    }
        \\
        \\    f = $state.f
        \\    f($state.n)
        \\}
        \\
        \\main : I64
        \\main = sum_from(4, 10, 3)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWithCallableCapturesIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWithOpaqueCallableIsSpecialized));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWithCallableCapturesIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
}

test "spec constr splits loop record state with returned callable captures" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, f : I64 -> I64 }
        \\
        \\make_affine = |scale, offset| |n| n * scale + offset
        \\
        \\sum_from : I64, I64, I64 -> I64
        \\sum_from = |start, scale, offset| {
        \\    var $state = { n: start, f: make_affine(scale, offset) }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, f: $state.f }
        \\    }
        \\
        \\    f = $state.f
        \\    f($state.n)
        \\}
        \\
        \\main : I64
        \\main = sum_from(4, 10, 3)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWithCallableCapturesIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWithOpaqueCallableIsSpecialized));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWithCallableCapturesIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
}

test "spec constr splits loop record state with annotated returned callable captures" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, f : I64 -> I64 }
        \\
        \\make_affine : I64, I64 -> (I64 -> I64)
        \\make_affine = |scale, offset| |n| n * scale + offset
        \\
        \\sum_from : I64, I64, I64 -> I64
        \\sum_from = |start, scale, offset| {
        \\    var $state = { n: start, f: make_affine(scale, offset) }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, f: $state.f }
        \\    }
        \\
        \\    f = $state.f
        \\    f($state.n)
        \\}
        \\
        \\main : I64
        \\main = sum_from(4, 10, 3)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWithCallableCapturesIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWithOpaqueCallableIsSpecialized));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWithCallableCapturesIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
}

test "spec constr exposes direct call record result for field access" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Start : { n : I64 }
        \\State : { n : I64, acc : I64 }
        \\
        \\make_state : I64 -> State
        \\make_state = |n| { n: n, acc: n + 1 }
        \\
        \\read_acc : Start -> I64
        \\read_acc = |start| make_state(start.n).acc
        \\
        \\main : I64
        \\main = read_acc({ n: 4 })
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "direct_call_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "struct_assign_count"));

    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, &unoptimized.lowered, "direct_call_count") > 0);
    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, &unoptimized.lowered, "struct_assign_count") > 0);
}

test "spec constr exposes block-wrapped direct call record result for field access" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, acc : I64 }
        \\
        \\make_state : I64 -> State
        \\make_state = |n| { n: n, acc: n + 1 }
        \\
        \\main : I64
        \\main = { make_state(4) }.acc
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "direct_call_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "struct_assign_count"));

    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, &unoptimized.lowered, "direct_call_count") > 0);
    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, &unoptimized.lowered, "struct_assign_count") > 0);
}

test "spec constr exposes demanded direct call argument facts" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, acc : I64 }
        \\
        \\make_state : I64 -> State
        \\make_state = |n| { n: n, acc: n + 1 }
        \\
        \\copy_state : State -> State
        \\copy_state = |state| { n: state.n, acc: state.acc }
        \\
        \\main : I64
        \\main = copy_state(make_state(4)).acc
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "direct_call_count"));

    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, &unoptimized.lowered, "direct_call_count") > 0);
}

test "spec constr specializes if-joined record state carried by while loop" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Start : { n : I64 }
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_from : Start, Bool -> I64
        \\sum_from = |seed, flag| {
        \\    start =
        \\        if flag {
        \\            { n: seed.n, acc: 0 }
        \\        } else {
        \\            { n: seed.n - 1, acc: 1 }
        \\        }
        \\
        \\    var $state = start
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
        \\    }
        \\
        \\    $state.acc
        \\}
        \\
        \\main : I64
        \\main = sum_from({ n: 4 }, True)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, branchJoinedRecordStateWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, branchJoinedRecordStateWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, branchJoinedRecordStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, branchJoinedRecordStateWorkerIsGeneric));
}

test "spec constr specializes match-joined record state carried by while loop" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Start : { n : I64 }
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_from : Start, Bool -> I64
        \\sum_from = |seed, flag| {
        \\    start =
        \\        match flag {
        \\            True => { n: seed.n, acc: 0 }
        \\            False => { n: seed.n - 1, acc: 1 }
        \\        }
        \\
        \\    var $state = start
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
        \\    }
        \\
        \\    $state.acc
        \\}
        \\
        \\main : I64
        \\main = sum_from({ n: 4 }, True)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, branchJoinedRecordStateWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, branchJoinedRecordStateWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, branchJoinedRecordStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, branchJoinedRecordStateWorkerIsGeneric));
}

test "spec constr specializes recursive tuple state" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\sum_tuple : (I64, I64) -> I64
        \\sum_tuple = |state|
        \\    match state {
        \\        (n, acc) =>
        \\            if n == 0 {
        \\                acc
        \\            } else {
        \\                sum_tuple((n - 1, acc + n))
        \\            }
        \\    }
        \\
        \\main : I64
        \\main = sum_tuple((4, 0))
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's SpecConstr strict-tuple examples.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, directTupleWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, directTupleWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, directTupleWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, directTupleWorkerIsGeneric));
}

test "spec constr leaves uninspected constructor arguments generic" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\unused_state : { n : I64 }, I64 -> I64
        \\unused_state = |state, n|
        \\    if n == 0 {
        \\        0
        \\    } else {
        \\        unused_state({ n: n }, n - 1)
        \\    }
        \\
        \\main : I64
        \\main = unused_state({ n: 0 }, 3)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's Note [Good arguments].
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, unusedStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, unusedStateWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, unusedStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, unusedStateWorkerIsGeneric));
}

test "spec constr specializes tagged recursive state" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Step : [Done, More(I64)]
        \\
        \\count_down : Step, I64 -> I64
        \\count_down = |step, acc|
        \\    match step {
        \\        Done => acc
        \\        More(n) =>
        \\            if n == 0 {
        \\                count_down(Done, acc)
        \\            } else {
        \\                count_down(More(n - 1), acc + n)
        \\            }
        \\    }
        \\
        \\main : I64
        \\main = count_down(More(4), 0)
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's SpecConstr constructor-call examples.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, taggedStepWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, taggedStepWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, taggedStepWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, taggedStepWorkerIsGeneric));
}

test "spec constr uses fully known entry shape for multiple tuple states" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\roman : I64, (I64, I64), (I64, I64) -> I64
        \\roman = |n, p, q|
        \\    if n == 0 {
        \\        p.0 + q.0
        \\    } else if n > 2 {
        \\        roman(n - 1, (p.1, p.0), q)
        \\    } else {
        \\        roman(n - 1, p, (q.1, q.0))
        \\    }
        \\
        \\main : I64
        \\main = roman(4, (1, 2), (3, 4))
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's testsuite/tests/eyeball/spec-constr1.hs.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, multiTupleWorkerIsFullySpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, multiTupleWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, multiTupleWorkerIsFullySpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, multiTupleWorkerIsGeneric));
}

test "LIR statements and procs carry resolved source locations" {
    const allocator = std.testing.allocator;

    const source =
        \\module [main]
        \\
        \\add2 : U64 -> U64
        \\add2 = |n| n + 2
        \\
        \\mul3 : U64 -> U64
        \\mul3 = |n| n * 3
        \\
        \\main : U64
        \\main = {
        \\    x = 40
        \\    mul3(add2(x))
        \\}
    ;

    var lowered_source = try lowerModuleWithProcDebugNames(allocator, source, .none, true);
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    try std.testing.expectEqual(store.cf_stmts.items.len, store.cf_stmt_locs.items.len);
    try std.testing.expectEqual(store.cf_stmts.items.len, store.cf_stmt_regions.items.len);
    try std.testing.expectEqual(store.proc_specs.items.len, store.proc_locs.items.len);
    try std.testing.expect(store.proc_debug_names.items.len > 0);
    for (store.proc_debug_names.items) |entry| {
        try std.testing.expect(entry.proc < store.proc_specs.items.len);
    }
    try std.testing.expect(store.sourceFileCount() >= 1);

    var located: usize = 0;
    for (store.cf_stmt_locs.items, store.cf_stmt_regions.items, store.cf_stmts.items) |loc, region, stmt| {
        const has_source = switch (stmt) {
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            => false,

            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            => true,
        };
        if (!has_source) {
            try std.testing.expect(!loc.hasLocation());
            try std.testing.expect(region.isEmpty());
        }
        if (loc.hasLocation()) {
            located += 1;
            try std.testing.expect(!region.isEmpty());
            try std.testing.expect(loc.file < store.sourceFileCount());
            try std.testing.expect(loc.line >= 1);
            try std.testing.expect(loc.column >= 1);
        }
    }
    try std.testing.expect(located > 0);

    var located_procs: usize = 0;
    for (store.proc_locs.items) |loc| {
        if (loc.hasLocation()) {
            located_procs += 1;
            try std.testing.expect(loc.file < store.sourceFileCount());
        }
    }
    try std.testing.expect(located_procs > 0);

    var found_add2 = false;
    var found_mul3 = false;
    for (0..store.proc_specs.items.len) |i| {
        const name = store.procDebugName(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, name, "add2")) found_add2 = true;
        if (std.mem.eql(u8, name, "mul3")) found_mul3 = true;
    }
    try std.testing.expect(found_add2);
    try std.testing.expect(found_mul3);
}

test "referenced but uncalled function does not materialize a proc" {
    const allocator = std.testing.allocator;

    const source =
        \\module [main]
        \\
        \\unused : U64 -> U64
        \\unused = |n| n + 1
        \\
        \\main : U64
        \\main = {
        \\    _fn = unused
        \\    0
        \\}
    ;

    var lowered_source = try lowerModuleWithProcDebugNames(allocator, source, .none, true);
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    var found_unused = false;
    for (0..store.proc_specs.items.len) |i| {
        const name = store.procDebugName(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, name, "unused")) found_unused = true;
    }
    try std.testing.expect(!found_unused);
}

test "LIR statements carry source locations under optimizing inline mode" {
    const allocator = std.testing.allocator;

    const source =
        \\module [main]
        \\
        \\add2 : U64 -> U64
        \\add2 = |n| n + 2
        \\
        \\main : U64
        \\main = {
        \\    x = 40
        \\    add2(x)
        \\}
    ;

    var lowered_source = try lowerModule(allocator, source, .wrappers);
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    var located: usize = 0;
    for (store.cf_stmt_locs.items, store.cf_stmt_regions.items) |loc, region| {
        if (loc.hasLocation()) located += 1;
        if (loc.hasLocation()) try std.testing.expect(!region.isEmpty());
    }
    try std.testing.expect(located > 0);
}

test "adjacent string interpolation patterns lower to grouped LIR match set" {
    const allocator = std.testing.allocator;

    const source =
        \\module [main]
        \\
        \\classify : Str -> Str
        \\classify = |s| match s {
        \\    "a${x}z" => x
        \\    "b${y}z" => y
        \\    "${_}.txt" => "file"
        \\    _ => "miss"
        \\}
        \\
        \\main : Str
        \\main = classify("bOKz")
    ;

    var lowered_source = try lowerModule(allocator, source, .none);
    defer lowered_source.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &lowered_source.lowered, hasGroupedStrMatchSet));
}

test "LIR locals carry source-level names" {
    const allocator = std.testing.allocator;

    const source =
        \\module [main]
        \\
        \\compute : U64 -> U64
        \\compute = |n| {
        \\    first_part = n * 2
        \\    second_part = first_part + 1
        \\    second_part
        \\}
        \\
        \\main : U64
        \\main = compute(20)
    ;

    var lowered_source = try lowerModule(allocator, source, .none);
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    try std.testing.expectEqual(store.locals.items.len, store.local_names.items.len);

    var found_first = false;
    var found_second = false;
    for (0..store.locals.items.len) |i| {
        const name = store.localName(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, name, "first_part")) found_first = true;
        if (std.mem.eql(u8, name, "second_part")) found_second = true;
    }
    try std.testing.expect(found_first);
    try std.testing.expect(found_second);
}

test "shared callees are lifted once and never gain spurious captures" {
    // A small diamond call graph: every function calls the one below it twice.
    // Capture collection reuses each callee's solved free set instead of
    // re-walking shared callee bodies, so the closed chain lifts cleanly and no
    // function gains a closure capture. The depth here keeps the surrounding
    // monomorphization cheap while still exercising shared-callee reuse.
    const allocator = std.testing.allocator;
    const depth = 6;

    var source = std.ArrayList(u8).empty;
    defer source.deinit(allocator);
    try source.appendSlice(allocator, "module [main]\n\nf0 : U64 -> U64\nf0 = |n| n + 1\n\n");
    var level: usize = 1;
    while (level <= depth) : (level += 1) {
        const chunk = try std.fmt.allocPrint(
            allocator,
            "f{d} : U64 -> U64\nf{d} = |n| {{\n    a = f{d}(n)\n    b = f{d}(n)\n    a + b\n}}\n\n",
            .{ level, level, level - 1, level - 1 },
        );
        defer allocator.free(chunk);
        try source.appendSlice(allocator, chunk);
    }
    const tail = try std.fmt.allocPrint(allocator, "main : U64\nmain = f{d}(0)\n", .{depth});
    defer allocator.free(tail);
    try source.appendSlice(allocator, tail);

    var lifted = try liftModuleAfterSpecConstr(allocator, source.items);
    defer lifted.deinit(allocator);

    // The whole chain survives lifting as distinct closed functions: the diamond
    // is not collapsed, and no function gains spurious closure captures.
    try std.testing.expect(lifted.lifted.fns.items.len >= depth);
    for (lifted.lifted.fns.items) |func| {
        try std.testing.expectEqual(@as(u32, 0), func.captures.len);
    }
}
