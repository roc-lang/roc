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
const MonoAst = postcheck.Monotype.Ast;
const MonoLower = postcheck.Monotype.Lower;
const MonoType = postcheck.Monotype.Type;

const TestError = helpers.TestHelperError || eval.BuiltinModules.InitError || error{
    TestExpectedEqual,
    TestUnexpectedResult,
    MissingRootProcedure,
    MissingProcSpec,
    MissingCallable,
    MissingDbgRoot,
    MissingIterCollectWorker,
    MissingSpecializedWorker,
};

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

const MonotypeSource = struct {
    resources: helpers.ParsedResources,
    mono: postcheck.Monotype.Ast.Program,

    fn deinit(self: *MonotypeSource, allocator: Allocator) void {
        self.mono.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

fn sharedPrePublishedBuiltin() TestError!helpers.PrePublishedBuiltin {
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
) TestError!LoweredSource {
    return lowerModuleWithOptions(allocator, source, inline_mode, .{});
}

const LowerModuleOptions = struct {
    inline_expects: lir.CheckedPipeline.InlineExpectMode = .run,
    proc_debug_names: bool = false,
    imports: []const helpers.ModuleSource = &.{},
};

fn lowerModuleWithOptions(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    options: LowerModuleOptions,
) TestError!LoweredSource {
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
            .inline_expects = options.inline_expects,
            .proc_debug_names = options.proc_debug_names,
        },
    );
    errdefer lowered.deinit();

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

fn monotypeCountersForModule(
    allocator: Allocator,
    source: []const u8,
) TestError!postcheck.Monotype.Lower.SpecializationCounters {
    return monotypeCountersForModuleWithImports(allocator, source, &.{});
}

fn lowerMonotypeModule(
    allocator: Allocator,
    source: []const u8,
) TestError!MonotypeSource {
    return lowerMonotypeModuleWithOptions(allocator, source, .{});
}

const LowerMonotypeOptions = struct {
    specialization_cache: MonoLower.SpecializationCacheControl = .{},
    loaded_specialization_shards: []const MonoLower.LoadedSpecializationShard = &.{},
    specialization_counters: ?*MonoLower.SpecializationCounters = null,
};

fn lowerMonotypeModuleWithOptions(
    allocator: Allocator,
    source: []const u8,
    options: LowerMonotypeOptions,
) TestError!MonotypeSource {
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
        .{
            .specialization_cache = options.specialization_cache,
            .loaded_specialization_shards = options.loaded_specialization_shards,
            .specialization_counters = options.specialization_counters,
        },
    );
    errdefer mono.deinit();

    return .{
        .resources = resources,
        .mono = mono,
    };
}

fn monotypeCountersForModuleWithImports(
    allocator: Allocator,
    source: []const u8,
    imports: []const helpers.ModuleSource,
) TestError!postcheck.Monotype.Lower.SpecializationCounters {
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, source, imports, try sharedPrePublishedBuiltin());
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

    var counters: postcheck.Monotype.Lower.SpecializationCounters = .{};
    var mono = try postcheck.Monotype.Lower.run(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{ .requests = resources.checked_artifact.root_requests.requests },
        .{ .specialization_counters = &counters },
    );
    defer mono.deinit();

    return counters;
}

fn expectEquivalentMonotypeProgramViews(lhs: postcheck.Monotype.Ast.ProgramView, rhs: postcheck.Monotype.Ast.ProgramView) error{TestExpectedEqual}!void {
    try std.testing.expectEqual(lhs.next_symbol, rhs.next_symbol);

    try std.testing.expectEqualSlices(postcheck.Monotype.Type.Content, lhs.types.types, rhs.types.types);
    try std.testing.expectEqualSlices(?check.CheckedNames.TypeDigest, lhs.types.type_digests, rhs.types.type_digests);
    try std.testing.expectEqualSlices(postcheck.Monotype.Type.TypeId, lhs.types.spans, rhs.types.spans);
    try std.testing.expectEqualSlices(postcheck.Monotype.Type.Field, lhs.types.fields, rhs.types.fields);
    try std.testing.expectEqualSlices(postcheck.Monotype.Type.Tag, lhs.types.tags, rhs.types.tags);
    try std.testing.expectEqualSlices(postcheck.Monotype.Type.DeclaredField, lhs.types.declared_fields, rhs.types.declared_fields);

    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.SpecRecord, lhs.specs, rhs.specs);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.ImportedFn, lhs.imported_fns, rhs.imported_fns);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.Fn, lhs.fns, rhs.fns);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.Def, lhs.defs, rhs.defs);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.NestedDef, lhs.nested_defs, rhs.nested_defs);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.Expr, lhs.exprs, rhs.exprs);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.Pat, lhs.pats, rhs.pats);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.Stmt, lhs.stmts, rhs.stmts);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.Local, lhs.locals, rhs.locals);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.ExprId, lhs.expr_ids, rhs.expr_ids);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.PatId, lhs.pat_ids, rhs.pat_ids);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.TypedLocal, lhs.typed_locals, rhs.typed_locals);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.StmtId, lhs.stmt_ids, rhs.stmt_ids);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.FieldExpr, lhs.field_exprs, rhs.field_exprs);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.RecordDestruct, lhs.record_destructs, rhs.record_destructs);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.StrPatternStep, lhs.str_pattern_steps, rhs.str_pattern_steps);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.Branch, lhs.branches, rhs.branches);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.IfBranch, lhs.if_branches, rhs.if_branches);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.Root, lhs.roots, rhs.roots);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.LayoutRequest, lhs.layout_requests, rhs.layout_requests);
    try std.testing.expectEqualSlices(postcheck.Monotype.Ast.RuntimeSchemaRequest, lhs.runtime_schema_requests, rhs.runtime_schema_requests);
    try std.testing.expectEqualSlices(base.SourceLoc, lhs.expr_locs, rhs.expr_locs);
    try std.testing.expectEqualSlices(base.Region, lhs.expr_regions, rhs.expr_regions);
    try std.testing.expectEqualSlices(base.SourceLoc, lhs.stmt_locs, rhs.stmt_locs);
    try std.testing.expectEqualSlices(base.Region, lhs.stmt_regions, rhs.stmt_regions);
}

const DurableTypeSnapshot = struct {
    view: MonoType.DurableView,
    type_digests: []check.CheckedNames.TypeDigest,

    fn deinit(self: DurableTypeSnapshot, allocator: Allocator) void {
        allocator.free(self.type_digests);
    }
};

fn durableTypeSnapshot(allocator: Allocator, program: *const MonoAst.Program) Allocator.Error!DurableTypeSnapshot {
    const store_view = program.types.view();
    const type_digests = try allocator.alloc(check.CheckedNames.TypeDigest, store_view.types.len);
    errdefer allocator.free(type_digests);

    for (type_digests, 0..) |*digest, index| {
        digest.* = store_view.type_digests[index] orelse
            program.types.typeDigest(&program.names, @enumFromInt(@as(u32, @intCast(index))));
    }

    return .{
        .view = .{
            .types = store_view.types,
            .type_digests = type_digests,
            .spans = store_view.spans,
            .fields = store_view.fields,
            .tags = store_view.tags,
            .declared_fields = store_view.declared_fields,
        },
        .type_digests = type_digests,
    };
}

fn digestBytesEqual(lhs: check.CheckedNames.TypeDigest, rhs: check.CheckedNames.TypeDigest) bool {
    return std.mem.eql(u8, lhs.bytes[0..], rhs.bytes[0..]);
}

fn specRecordMatches(
    allocator: Allocator,
    name_store: *const check.CheckedNames.NameStore,
    candidate_types: anytype,
    candidate: MonoAst.SpecRecord,
    expected_types: anytype,
    expected: MonoAst.SpecRecord,
) Allocator.Error!bool {
    if (!std.meta.eql(candidate.identity.callable, expected.identity.callable)) return false;
    if (!digestBytesEqual(candidate.identity.source_fn_ty_digest, expected.identity.source_fn_ty_digest)) return false;
    if (!digestBytesEqual(candidate.identity.mono_fn_ty_digest, expected.identity.mono_fn_ty_digest)) return false;
    return try MonoType.typeEqlAcrossStores(
        allocator,
        name_store,
        candidate_types,
        candidate.identity.mono_fn_ty,
        expected_types,
        expected.identity.mono_fn_ty,
    );
}

fn specCoveredByLocalOrLoaded(
    allocator: Allocator,
    cached: MonoAst.ProgramView,
    loaded: MonoLower.LoadedSpecializationShard,
    expected_types: anytype,
    expected: MonoAst.SpecRecord,
) Allocator.Error!bool {
    for (cached.specs) |candidate| {
        if (try specRecordMatches(allocator, cached.names, cached.types, candidate, expected_types, expected)) return true;
    }

    for (loaded.specs) |candidate| {
        if (try specRecordMatches(allocator, cached.names, loaded.types, candidate, expected_types, expected)) return true;
    }

    return false;
}

fn expectSpecsCoveredByCachedOrLoaded(
    allocator: Allocator,
    no_cache: MonoAst.ProgramView,
    cached: MonoAst.ProgramView,
    loaded: MonoLower.LoadedSpecializationShard,
) TestError!void {
    for (no_cache.specs) |expected| {
        if (!try specCoveredByLocalOrLoaded(allocator, cached, loaded, no_cache.types, expected)) {
            return error.MissingProcSpec;
        }
    }
}

fn isUnaryPrimitiveFnSpec(view: MonoAst.ProgramView, record: MonoAst.SpecRecord, primitive: MonoType.Primitive) bool {
    const func = switch (view.types.get(record.identity.mono_fn_ty)) {
        .func => |func| func,
        else => return false,
    };
    const args = view.types.span(func.args);
    if (args.len != 1) return false;
    const arg_matches = switch (view.types.get(args[0])) {
        .primitive => |arg| arg == primitive,
        else => false,
    };
    const ret_matches = switch (view.types.get(func.ret)) {
        .primitive => |ret| ret == primitive,
        else => false,
    };
    return arg_matches and ret_matches;
}

fn lowerModuleWithInlineExpects(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    inline_expects: lir.CheckedPipeline.InlineExpectMode,
) TestError!LoweredSource {
    return lowerModuleWithOptions(allocator, source, inline_mode, .{ .inline_expects = inline_expects });
}

fn lowerModuleWithProcDebugNames(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    proc_debug_names: bool,
) TestError!LoweredSource {
    return lowerModuleWithOptions(allocator, source, inline_mode, .{ .proc_debug_names = proc_debug_names });
}

fn mainProcArgLayouts(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) TestError![]LayoutIdx {
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
) TestError!eval.RuntimeHostEnv.RecordedRun {
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

fn expectOptimizedDbgEvents(source: []const u8, expected: []const []const u8) TestError!void {
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

test "optimized inline expect lowering omits expects and keeps dbg" {
    const allocator = std.testing.allocator;
    const source =
        \\main : I64
        \\main = {
        \\    dbg 1
        \\    expect False
        \\    expect 1 == 1
        \\    2
        \\}
    ;

    var run_effects = try lowerModuleWithInlineExpects(allocator, source, .wrappers, .run);
    defer run_effects.deinit(allocator);

    const run_counts = countDebugEffectStmts(&run_effects.lowered);
    try std.testing.expect(run_counts.debug > 0);
    try std.testing.expect(run_counts.expect > 0);

    var omitted_effects = try lowerModuleWithInlineExpects(allocator, source, .wrappers, .omit);
    defer omitted_effects.deinit(allocator);

    const omitted_counts = countDebugEffectStmts(&omitted_effects.lowered);
    try std.testing.expect(omitted_counts.debug > 0);
    try std.testing.expectEqual(@as(usize, 0), omitted_counts.expect);
}

test "nominal record lays out fields in declared order" {
    const allocator = std.testing.allocator;
    // The unnamed `_ : {}` field opts this nominal record into declared-order
    // layout, so { z: U16, y: U16, x: U32 } is kept verbatim. Without the marker
    // it would sort structurally and hoist the U32 to offset 0.
    const source =
        \\Account := { z : U16, y : U16, x : U32, _ : {} }
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
    try std.testing.expectEqual(@as(u32, 8), lowered.lir_result.layouts.getStructSize(struct_idx));
}

test "imported nominal record lays out fields in declared order" {
    const allocator = std.testing.allocator;
    const acct_module =
        \\Account := { z : U16, y : U16, x : U32, _ : {} }
    ;
    // An imported nominal record must lay out identically to a local one, or
    // values would be read with the wrong offsets across module boundaries.
    const source =
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
    try std.testing.expectEqual(@as(u32, 8), lowered.lir_result.layouts.getStructSize(struct_idx));
}

test "nominal record reserves unnamed padding fields without inflating alignment" {
    const allocator = std.testing.allocator;
    // Mirrors a C `struct { uint8_t a; char pad[3]; uint32_t b; }`: the three
    // unnamed bytes hold the explicit padding so `b` lands at offset 4 without
    // the compiler inserting alignment padding of its own.
    const source =
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
    try std.testing.expectEqual(@as(u32, 8), lowered.lir_result.layouts.getStructSize(struct_idx));
    try std.testing.expectEqual(@as(u64, 4), layout_val.alignment(.u64).toByteUnits());
}

test "generic nominal record instantiates unnamed padding to the argument's size" {
    const allocator = std.testing.allocator;
    // A type-parameterized unnamed field (`_ : a`) must reserve the *instantiated*
    // size, exactly like a named field of the same type: `Foo(U64)` is 16 bytes
    // (x:U64 @0 plus 8 bytes of padding), just as `{ x : a, y : a }(U64)` would be.
    const source =
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
    try std.testing.expectEqual(@as(u32, 16), lowered.lir_result.layouts.getStructSize(struct_idx));
}

test "nominal record with a parenthesized backing still honors declared order and padding" {
    const allocator = std.testing.allocator;
    // The backing record is wrapped in parentheses. Parens are transparent here:
    // the unnamed field must still be accepted and the layout must match the
    // unparenthesized form (a@0, b@4, size 8, with three padding spacers).
    const source =
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
    try std.testing.expectEqual(@as(u32, 8), lowered.lir_result.layouts.getStructSize(struct_idx));
}

fn liftModuleAfterSpecConstr(
    allocator: Allocator,
    source: []const u8,
) TestError!LiftedSource {
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
) TestError!void {
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

fn rootProc(lowered: *const lir.CheckedPipeline.LoweredProgram) TestError!LIR.LirProcSpecId {
    try std.testing.expectEqual(@as(usize, 1), lowered.lir_result.root_procs.items.len);
    return lowered.lir_result.root_procs.items[0];
}

fn collectAssignCallProcs(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
) TestError![]LIR.LirProcSpecId {
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
    low_level_count: usize = 0,
    str_count_utf8_bytes_count: usize = 0,
    self_call_count: usize = 0,
    switch_count: usize = 0,
    str_match_set_count: usize = 0,
    join_count: usize = 0,
    max_join_param_count: usize = 0,
    jump_count: usize = 0,
    struct_assign_count: usize = 0,
    tag_assign_count: usize = 0,
};

fn collectProcShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
) TestError!ProcShape {
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
            .assign_packed_erased_fn => |stmt| try work.append(allocator, stmt.next),
            .assign_low_level => |stmt| {
                shape.low_level_count += 1;
                if (stmt.op == .str_count_utf8_bytes) shape.str_count_utf8_bytes_count += 1;
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
            shape.join_count == 11 and
            shape.jump_count == 15 and
            shape.struct_assign_count >= 2,
    };
}

fn reachableIterCollectShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    wanted: IterCollectShape,
) TestError!bool {
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
) TestError!usize {
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

fn reachableProcShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime matches: fn (ProcShape) bool,
) TestError!bool {
    return (try reachableProcShapeCount(allocator, lowered, matches)) > 0;
}

fn markReachableLiftedExpr(
    program: *const postcheck.MonotypeLifted.Ast.Program,
    expr_id: postcheck.MonotypeLifted.Ast.ExprId,
    reachable: []bool,
) void {
    const index = @intFromEnum(expr_id);
    if (reachable[index]) return;
    reachable[index] = true;

    switch (program.exprs.items[index].data) {
        .local,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .crash,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        => {},
        .fn_ref => |fn_ref| {
            for (program.captureOperandSpan(fn_ref.captures)) |operand| {
                markReachableLiftedExpr(program, operand.value, reachable);
            }
        },
        .list,
        .tuple,
        => |items| for (program.exprSpan(items)) |child| markReachableLiftedExpr(program, child, reachable),
        .record => |fields| for (program.fieldExprSpan(fields)) |field| markReachableLiftedExpr(program, field.value, reachable),
        .tag => |tag| for (program.exprSpan(tag.payloads)) |payload| markReachableLiftedExpr(program, payload, reachable),
        .nominal,
        .dbg,
        .expect,
        => |child| markReachableLiftedExpr(program, child, reachable),
        .return_ => |ret| markReachableLiftedExpr(program, ret.value, reachable),
        .expect_err => |expect_err| markReachableLiftedExpr(program, expect_err.msg, reachable),
        .comptime_branch_taken => |taken| markReachableLiftedExpr(program, taken.body, reachable),
        .if_initialized_payload => |switch_| {
            markReachableLiftedExpr(program, switch_.cond, reachable);
            markReachableLiftedExpr(program, switch_.initialized, reachable);
            markReachableLiftedExpr(program, switch_.uninitialized, reachable);
        },
        .try_sequence => |sequence| {
            markReachableLiftedExpr(program, sequence.try_expr, reachable);
            markReachableLiftedExpr(program, sequence.ok_body, reachable);
        },
        .try_record_sequence => |sequence| {
            markReachableLiftedExpr(program, sequence.try_expr, reachable);
            markReachableLiftedExpr(program, sequence.ok_body, reachable);
        },
        .let_ => |let_| {
            markReachableLiftedExpr(program, let_.value, reachable);
            markReachableLiftedExpr(program, let_.rest, reachable);
        },
        .lambda,
        .def_ref,
        .fn_def,
        => {},
        .call_value => |call| {
            markReachableLiftedExpr(program, call.callee, reachable);
            for (program.exprSpan(call.args)) |arg| markReachableLiftedExpr(program, arg, reachable);
        },
        .call_proc => |call| {
            for (program.exprSpan(call.args)) |arg| markReachableLiftedExpr(program, arg, reachable);
            for (program.captureOperandSpan(call.captures)) |operand| markReachableLiftedExpr(program, operand.value, reachable);
        },
        .low_level => |call| for (program.exprSpan(call.args)) |arg| markReachableLiftedExpr(program, arg, reachable),
        .field_access => |field| markReachableLiftedExpr(program, field.receiver, reachable),
        .tuple_access => |access| markReachableLiftedExpr(program, access.tuple, reachable),
        .structural_eq => |eq| {
            markReachableLiftedExpr(program, eq.lhs, reachable);
            markReachableLiftedExpr(program, eq.rhs, reachable);
        },
        .structural_hash => |h| {
            markReachableLiftedExpr(program, h.value, reachable);
            markReachableLiftedExpr(program, h.hasher, reachable);
        },
        .match_ => |match| {
            markReachableLiftedExpr(program, match.scrutinee, reachable);
            for (program.branchSpan(match.branches)) |branch| {
                if (branch.guard) |guard| markReachableLiftedExpr(program, guard, reachable);
                markReachableLiftedExpr(program, branch.body, reachable);
            }
        },
        .if_ => |if_| {
            for (program.ifBranchSpan(if_.branches)) |branch| {
                markReachableLiftedExpr(program, branch.cond, reachable);
                markReachableLiftedExpr(program, branch.body, reachable);
            }
            markReachableLiftedExpr(program, if_.final_else, reachable);
        },
        .block => |block| {
            for (program.stmtSpan(block.statements)) |stmt| markReachableLiftedStmt(program, stmt, reachable);
            markReachableLiftedExpr(program, block.final_expr, reachable);
        },
        .loop_ => |loop| {
            for (program.exprSpan(loop.initial_values)) |initial| markReachableLiftedExpr(program, initial, reachable);
            markReachableLiftedExpr(program, loop.body, reachable);
        },
        .break_ => |maybe| if (maybe) |value| markReachableLiftedExpr(program, value, reachable),
        .continue_ => |continue_| for (program.exprSpan(continue_.values)) |value| markReachableLiftedExpr(program, value, reachable),
    }
}

fn markReachableLiftedStmt(
    program: *const postcheck.MonotypeLifted.Ast.Program,
    stmt_id: postcheck.MonotypeLifted.Ast.StmtId,
    reachable: []bool,
) void {
    switch (program.stmts.items[@intFromEnum(stmt_id)]) {
        .let_ => |let_| markReachableLiftedExpr(program, let_.value, reachable),
        .expr,
        .expect,
        .dbg,
        => |expr| markReachableLiftedExpr(program, expr, reachable),
        .return_ => |ret| markReachableLiftedExpr(program, ret.value, reachable),
        .crash => {},
        .uninitialized => {},
    }
}

fn countUnreachableLiftedDirectCalls(
    allocator: Allocator,
    program: *const postcheck.MonotypeLifted.Ast.Program,
) TestError!usize {
    const reachable = try allocator.alloc(bool, program.exprs.items.len);
    defer allocator.free(reachable);
    @memset(reachable, false);

    for (program.fns.items) |fn_| {
        switch (fn_.body) {
            .roc => |body| markReachableLiftedExpr(program, body, reachable),
            .hosted => {},
        }
    }

    var count: usize = 0;
    for (program.exprs.items, reachable) |expr, is_reachable| {
        if (!is_reachable and expr.data == .call_proc) count += 1;
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
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 2 and
        shape.jump_count >= 2;
}

fn whileRecordStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
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
    return shape.arg_count == 1 and
        shape.direct_call_count == 0 and
        shape.low_level_count == 2 and
        shape.struct_assign_count == 0;
}

fn opaqueLetCallWorkerDuplicatesCall(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.low_level_count > 2 and
        shape.struct_assign_count == 0;
}

fn hasGroupedStrMatchSet(shape: ProcShape) bool {
    return shape.str_match_set_count == 1;
}

fn expectIterCollectWorkerSpecialized(source: []const u8) TestError!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableIterCollectShape(allocator, &optimized.lowered, .specialized));
    try std.testing.expect(!try reachableIterCollectShape(allocator, &optimized.lowered, .generic));

    try std.testing.expect(!try reachableIterCollectShape(allocator, &unoptimized.lowered, .specialized));
    try std.testing.expect(try reachableIterCollectShape(allocator, &unoptimized.lowered, .generic));
}

fn rootDirectCallTarget(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) TestError!LIR.LirProcSpecId {
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
) TestError!void {
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
) TestError!void {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator, source, inline_mode);
    defer lowered_source.deinit(allocator);

    const target = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    const target_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, target);
    defer allocator.free(target_calls);

    try std.testing.expect(target_calls.len > 0);
}

fn nestedSite(def: postcheck.Monotype.Ast.NestedDef) ?postcheck.Monotype.Ast.NestedFn {
    return switch (def.fn_def.fn_def) {
        .nested => |site| site,
        else => null,
    };
}

fn sameNestedSourceSite(
    lhs: postcheck.Monotype.Ast.NestedFn,
    rhs: postcheck.Monotype.Ast.NestedFn,
) bool {
    return std.mem.eql(u8, lhs.owner.artifact.bytes[0..], rhs.owner.artifact.bytes[0..]) and
        lhs.owner.proc_base == rhs.owner.proc_base and
        lhs.owner.template == rhs.owner.template and
        lhs.site == rhs.site;
}

test "issue 9802 same-type map2 specialization counters are bounded" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Boxed(a) := [Boxed(a)]
        \\
        \\const : a -> Boxed(a)
        \\const = |value| Boxed(value)
        \\
        \\map2 : Boxed(a), Boxed(b), (a, b -> c) -> Boxed(c)
        \\map2 = |Boxed(left), Boxed(right), f| Boxed(f(left, right))
        \\
        \\unwrap : Boxed(a) -> a
        \\unwrap = |Boxed(value)| value
        \\
        \\main : I64
        \\main = {
        \\    v0 = const(0)
        \\    v1 = map2(v0, const(1), |a, b| a + b)
        \\    v2 = map2(v1, const(2), |a, b| a + b)
        \\    v3 = map2(v2, const(3), |a, b| a + b)
        \\    v4 = map2(v3, const(4), |a, b| a + b)
        \\    v5 = map2(v4, const(5), |a, b| a + b)
        \\    v6 = map2(v5, const(6), |a, b| a + b)
        \\    v7 = map2(v6, const(7), |a, b| a + b)
        \\    v8 = map2(v7, const(8), |a, b| a + b)
        \\    unwrap(v8)
        \\}
    ;

    const counters = try monotypeCountersForModule(allocator, source);

    try std.testing.expectEqual(postcheck.Monotype.Lower.SpecializationCounters{
        .template_requests = 53,
        .template_hits = 22,
        .template_misses = 5,
        .nested_requests = 8,
        .nested_hits = 0,
        .nested_misses = 8,
        .template_lookup_candidates = 22,
        .nested_lookup_candidates = 0,
        .specialization_type_digest_requests = 75,
        .specialization_type_digest_cache_hits = 140,
        .specialization_type_digest_cache_misses = 128,
        .specialization_type_digest_nodes_visited = 128,
        .exact_type_checks = 22,
    }, counters);
}

test "issue 9802 growing-structural map2 specialization counters are bounded" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Boxed(a) := [Boxed(a)]
        \\
        \\const : a -> Boxed(a)
        \\const = |value| Boxed(value)
        \\
        \\map2 : Boxed(a), Boxed(b), (a, b -> c) -> Boxed(c)
        \\map2 = |Boxed(left), Boxed(right), f| Boxed(f(left, right))
        \\
        \\unwrap : Boxed(a) -> a
        \\unwrap = |Boxed(value)| value
        \\
        \\main : I64
        \\main = {
        \\    v0 = const(0)
        \\    v1 = map2(v0, const(1), |acc, n| { acc, n1: n })
        \\    v2 = map2(v1, const(2), |acc, n| { acc, n2: n })
        \\    v3 = map2(v2, const(3), |acc, n| { acc, n3: n })
        \\    v4 = map2(v3, const(4), |acc, n| { acc, n4: n })
        \\    v5 = map2(v4, const(5), |acc, n| { acc, n5: n })
        \\    v6 = map2(v5, const(6), |acc, n| { acc, n6: n })
        \\    unwrap(v6).n6
        \\}
    ;

    const counters = try monotypeCountersForModule(allocator, source);

    try std.testing.expectEqual(postcheck.Monotype.Lower.SpecializationCounters{
        .template_requests = 29,
        .template_hits = 5,
        .template_misses = 10,
        .nested_requests = 6,
        .nested_hits = 0,
        .nested_misses = 6,
        .template_lookup_candidates = 5,
        .nested_lookup_candidates = 0,
        .specialization_type_digest_requests = 52,
        .specialization_type_digest_cache_hits = 245,
        .specialization_type_digest_cache_misses = 290,
        .specialization_type_digest_nodes_visited = 290,
        .exact_type_checks = 5,
    }, counters);
}

test "imported and local generic specialization counters reuse closed types" {
    const allocator = std.testing.allocator;
    const util_module =
        \\module [identity]
        \\
        \\identity : a -> a
        \\identity = |value| value
    ;
    const source =
        \\module [main]
        \\
        \\import Util exposing [identity]
        \\
        \\Boxed(a) := [Boxed(a)]
        \\
        \\local_identity : a -> a
        \\local_identity = |value| value
        \\
        \\main : { imported_a : Boxed(U64), imported_b : Boxed(U64), local_a : Boxed(U64), local_b : Boxed(U64) }
        \\main = {
        \\    value = Boxed(1)
        \\    {
        \\        imported_a: identity(value),
        \\        imported_b: identity(value),
        \\        local_a: local_identity(value),
        \\        local_b: local_identity(value),
        \\    }
        \\}
    ;

    const counters = try monotypeCountersForModuleWithImports(allocator, source, &.{
        .{ .name = "Util", .source = util_module },
    });

    try std.testing.expect(counters.template_requests >= 4);
    try std.testing.expect(counters.template_misses >= 2);
    try std.testing.expect(counters.template_hits >= 2);
    try std.testing.expect(counters.template_lookup_candidates <= counters.template_requests);
}

test "disabling monotype specialization cache does not change monotype output" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\identity : a -> a
        \\identity = |value| value
        \\
        \\main : { n : U64, flag : Bool }
        \\main = {
        \\    { n: identity(1), flag: identity(Bool.True) }
        \\}
    ;

    var default = try lowerMonotypeModule(allocator, source);
    defer default.deinit(allocator);

    var disabled = try lowerMonotypeModuleWithOptions(allocator, source, .{
        .specialization_cache = .disabled,
    });
    defer disabled.deinit(allocator);

    try expectEquivalentMonotypeProgramViews(default.mono.view(), disabled.mono.view());
}

test "monotype specialization cache read reuses loaded hits and lowers fresh misses" {
    const allocator = std.testing.allocator;
    const mixed_source =
        \\module [main]
        \\
        \\identity : a -> a
        \\identity = |value| value
        \\
        \\main : { n : U64, flag : Bool }
        \\main = {
        \\    { n: identity(1), flag: identity(Bool.True) }
        \\}
    ;

    var loaded_program = try lowerMonotypeModule(allocator, mixed_source);
    defer loaded_program.deinit(allocator);
    const loaded_program_view = loaded_program.mono.view();

    const selected_loaded_spec = for (loaded_program_view.specs) |record| {
        if (isUnaryPrimitiveFnSpec(loaded_program_view, record, .u64)) break record;
    } else return error.MissingProcSpec;
    const loaded_specs = [_]MonoAst.SpecRecord{selected_loaded_spec};

    const loaded_types = try durableTypeSnapshot(allocator, &loaded_program.mono);
    defer loaded_types.deinit(allocator);
    const loaded_shards = [_]MonoLower.LoadedSpecializationShard{.{
        .shard_id = @enumFromInt(1),
        .types = loaded_types.view,
        .specs = &loaded_specs,
    }};

    var no_cache = try lowerMonotypeModuleWithOptions(allocator, mixed_source, .{
        .specialization_cache = .disabled,
    });
    defer no_cache.deinit(allocator);

    var counters: MonoLower.SpecializationCounters = .{};
    var cached = try lowerMonotypeModuleWithOptions(allocator, mixed_source, .{
        .specialization_cache = .{},
        .loaded_specialization_shards = &loaded_shards,
        .specialization_counters = &counters,
    });
    defer cached.deinit(allocator);

    try std.testing.expect(cached.mono.view().imported_fns.len > 0);
    try std.testing.expect(cached.mono.view().specs.len < no_cache.mono.view().specs.len);
    try std.testing.expect(counters.template_hits > 0);
    try std.testing.expect(counters.template_misses > 0);
    try expectSpecsCoveredByCachedOrLoaded(allocator, no_cache.mono.view(), cached.mono.view(), loaded_shards[0]);
}

test "nested function specializations keep equal types at different sites distinct" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\first : U64 -> U64
        \\first = |n| {
        \\    id = |x| x
        \\    id(n)
        \\}
        \\
        \\second : U64 -> U64
        \\second = |n| {
        \\    id = |x| x
        \\    id(n)
        \\}
        \\
        \\main : { first : U64, second : U64 }
        \\main = { first: first(1), second: second(2) }
    ;

    var lowered = try lowerMonotypeModule(allocator, source);
    defer lowered.deinit(allocator);

    var found_distinct_sites = false;
    for (lowered.mono.nested_defs.items, 0..) |lhs, lhs_index| {
        const lhs_site = nestedSite(lhs) orelse continue;
        for (lowered.mono.nested_defs.items[lhs_index + 1 ..]) |rhs| {
            const rhs_site = nestedSite(rhs) orelse continue;
            if (!sameNestedSourceSite(lhs_site, rhs_site) and
                try lowered.mono.types.typeEql(&lowered.mono.names, lhs.fn_def.mono_fn_ty, rhs.fn_def.mono_fn_ty))
            {
                found_distinct_sites = true;
            }
        }
    }

    try std.testing.expect(found_distinct_sites);
}

test "one nested function site specializes at multiple closed function types" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\choose : a -> a
        \\choose = |value| {
        \\    id = |x| x
        \\    id(value)
        \\}
        \\
        \\main : { n : U64, s : Str }
        \\main = { n: choose(1), s: choose("hi") }
    ;

    var lowered = try lowerMonotypeModule(allocator, source);
    defer lowered.deinit(allocator);

    var found_same_site_distinct_types = false;
    for (lowered.mono.nested_defs.items, 0..) |lhs, lhs_index| {
        const lhs_site = nestedSite(lhs) orelse continue;
        for (lowered.mono.nested_defs.items[lhs_index + 1 ..]) |rhs| {
            const rhs_site = nestedSite(rhs) orelse continue;
            if (!sameNestedSourceSite(lhs_site, rhs_site)) continue;
            if (lhs.fn_def.mono_fn_ty != rhs.fn_def.mono_fn_ty) {
                found_same_site_distinct_types = true;
            }
        }
    }

    try std.testing.expect(found_same_site_distinct_types);
}

test "differently ordered source record rows produce normalized monotype rows" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\choose : Bool -> { a : U64, b : U64 }
        \\choose = |flag| if flag { b: 2, a: 1 } else { a: 3, b: 4 }
        \\
        \\main : { a : U64, b : U64 }
        \\main = choose(Bool.True)
    ;

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
        .{},
    );
    defer mono.deinit();

    try std.testing.expect(mono.specs.items.len > 0);
    for (mono.specs.items) |spec| {
        try std.testing.expectEqual(postcheck.Monotype.Ast.SpecStatus.ready, spec.status);
    }

    const a_name = try mono.names.internRecordFieldLabel("a");
    const b_name = try mono.names.internRecordFieldLabel("b");
    var normalized_rows: usize = 0;
    for (mono.types.types.items) |content| {
        const span = switch (content) {
            .record => |fields| fields,
            else => continue,
        };
        const fields = mono.types.fieldSpan(span);
        if (fields.len != 2) continue;
        if (fields[0].name == a_name and fields[1].name == b_name) {
            normalized_rows += 1;
        } else if (fields[0].name == b_name and fields[1].name == a_name) {
            return error.TestUnexpectedResult;
        }
    }

    try std.testing.expect(normalized_rows > 0);
}

test "direct call wrapper is inlined when inline mode is enabled" {
    try expectRootDirectCallCount(
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
        \\main : Str -> U64
        \\main = |str| Str.count_utf8_bytes(str)
    , .wrappers);
    defer lowered_source.deinit(allocator);

    const shape = try collectProcShape(allocator, &lowered_source.lowered, try rootProc(&lowered_source.lowered));
    try std.testing.expectEqual(@as(usize, 0), shape.direct_call_count);
    try std.testing.expectEqual(@as(usize, 1), shape.str_count_utf8_bytes_count);
}

test "block wrapper with statements is not inlined" {
    try expectInlinePlanDecision(
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
) TestError!void {
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
        \\sum_to : I64, I64 -> I64
        \\sum_to = |n, acc| if n == 0.I64 acc else sum_to(n - 1, acc + n)
        \\
        \\main = sum_to(10.I64, 0.I64)
    , .tce);
}

test "trmc: result used before the constructor is not transformed" {
    try expectRootTargetTailTransform(
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

test "plant iter pipeline specializes collect worker after inlining" {
    try expectIterCollectWorkerSpecialized(
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
    );
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
        \\main : List(I64)
        \\main =
        \\    Iter.collect(
        \\        Iter.map(List.iter([1.I64, 2, 3]), |i| i * 12),
        \\    )
    , .wrappers);
    defer optimized.deinit(allocator);
}

test "direct iter collect worker specializes constructor recursive call" {
    try expectIterCollectWorkerSpecialized(
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
    );
}

test "spec constr does not duplicate opaque let-bound direct calls" {
    const allocator = std.testing.allocator;
    const source =
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

test "spec constr writes dynamically discovered workers once" {
    const allocator = std.testing.allocator;
    const source =
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

    try std.testing.expectEqual(@as(usize, 0), try countUnreachableLiftedDirectCalls(allocator, &lifted.lifted));
}

test "spec constr specializes recursive record state" {
    const allocator = std.testing.allocator;
    const source =
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

test "spec constr specializes recursive tuple state" {
    const allocator = std.testing.allocator;
    const source =
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
    try source.appendSlice(allocator, "f0 : U64 -> U64\nf0 = |n| n + 1\n\n");
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
