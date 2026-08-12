//! Structural LIR tests for post-check wrapper inlining.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const check = @import("check");
const eval = @import("eval");
const lir = @import("lir");
const postcheck = @import("postcheck");
const helpers = eval.test_helpers;

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;
const GuardedList = lir.LirStore.GuardedList;
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
    specialization_strategy: base.SpecializationStrategy = .lss,
    checked_module_state: lir.CheckedPipeline.CheckedModuleState = .complete,
    inline_expects: lir.CheckedPipeline.InlineExpectMode = .run,
    proc_debug_names: bool = false,
    tag_reachability: bool = false,
    promote_loop_appends: bool = true,
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

    const main_def = resources.can.explicitRootDefByName("main") orelse return error.MissingRootProcedure;
    const main_request = for (resources.checked_artifact.root_requests.requests) |request| {
        switch (request.source) {
            .def => |def| if (def == main_def) break request,
            .expr, .statement, .required_binding, .hoisted => {},
        }
    } else return error.MissingRootProcedure;
    const published_roots = [_]check.CheckedArtifact.RootRequest{main_request};

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{ .requests = &published_roots },
        .{
            .target_usize = base.target.TargetUsize.native,
            .specialization_strategy = options.specialization_strategy,
            .checked_module_state = options.checked_module_state,
            .inline_mode = inline_mode,
            .inline_expects = options.inline_expects,
            .proc_debug_names = options.proc_debug_names,
            .promote_loop_appends = options.promote_loop_appends,
            .tag_reachability = options.tag_reachability,
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
    diagnostics: ?*MonoLower.Diagnostics = null,
    root_selection: enum { all, test_expects } = .all,
    procedure_template_root_grouping: postcheck.Common.ProcedureTemplateRootGrouping = .isolated,
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

    var selected_test_roots = std.ArrayList(check.CheckedArtifact.RootRequest).empty;
    defer selected_test_roots.deinit(allocator);
    const root_requests = switch (options.root_selection) {
        .all => resources.checked_artifact.root_requests.requests,
        .test_expects => blk: {
            for (resources.checked_artifact.root_requests.requests) |request| {
                if (request.kind == .test_expect) try selected_test_roots.append(allocator, request);
            }
            break :blk selected_test_roots.items;
        },
    };

    var mono = try postcheck.Monotype.Lower.run(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{
            .requests = root_requests,
            .procedure_template_root_grouping = options.procedure_template_root_grouping,
        },
        .{
            .specialization_cache = options.specialization_cache,
            .loaded_specialization_shards = options.loaded_specialization_shards,
            .specialization_counters = options.specialization_counters,
            .diagnostics = options.diagnostics,
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

const ExpectedMonotypeSpecializationCounters = struct {
    template_requests: u64,
    template_hits: u64,
    template_misses: u64,
    nested_requests: u64,
    nested_hits: u64,
    nested_misses: u64,
    template_lookup_candidates: u64 = 0,
    nested_lookup_candidates: u64 = 0,
    specialization_type_digest_requests: u64,
    max_specialization_type_digest_cache_hits: u64,
    max_specialization_type_digest_cache_misses: u64,
    max_specialization_type_digest_nodes_visited: u64,
    exact_type_checks: u64 = 0,
    nominal_backing_reuses: u64,
    nominal_backing_instantiations: u64,
    evidence_missing: u64 = 0,
};

fn expectMonotypeSpecializationCountersWithin(
    counters: MonoLower.SpecializationCounters,
    expected: ExpectedMonotypeSpecializationCounters,
) TestError!void {
    try std.testing.expectEqual(expected.template_requests, counters.template_requests);
    try std.testing.expectEqual(expected.template_hits, counters.template_hits);
    try std.testing.expectEqual(expected.template_misses, counters.template_misses);
    try std.testing.expectEqual(expected.nested_requests, counters.nested_requests);
    try std.testing.expectEqual(expected.nested_hits, counters.nested_hits);
    try std.testing.expectEqual(expected.nested_misses, counters.nested_misses);
    try std.testing.expectEqual(expected.template_lookup_candidates, counters.template_lookup_candidates);
    try std.testing.expectEqual(expected.nested_lookup_candidates, counters.nested_lookup_candidates);
    try std.testing.expectEqual(expected.specialization_type_digest_requests, counters.specialization_type_digest_requests);
    try std.testing.expect(counters.specialization_type_digest_cache_hits <= expected.max_specialization_type_digest_cache_hits);
    try std.testing.expect(counters.specialization_type_digest_cache_misses <= expected.max_specialization_type_digest_cache_misses);
    try std.testing.expect(counters.specialization_type_digest_nodes_visited <= expected.max_specialization_type_digest_nodes_visited);
    try std.testing.expectEqual(expected.exact_type_checks, counters.exact_type_checks);
    try std.testing.expectEqual(expected.nominal_backing_reuses, counters.nominal_backing_reuses);
    try std.testing.expectEqual(expected.nominal_backing_instantiations, counters.nominal_backing_instantiations);
    try std.testing.expectEqual(expected.evidence_missing, counters.evidence_missing);
}

const StructuralJsonMonotypeStats = struct {
    functions: usize,
    definitions: usize,
    expressions: usize,
    locals: usize,
    template_misses: u64,
    nested_misses: u64,
};

const StructuralJsonOperation = enum { parse, encode };

fn structuralJsonSource(
    allocator: Allocator,
    field_count: usize,
    field_ty: []const u8,
    operation: StructuralJsonOperation,
) Allocator.Error![]u8 {
    var source = std.ArrayList(u8).empty;
    errdefer source.deinit(allocator);

    try source.appendSlice(allocator,
        \\Shape : {
        \\
    );
    for (0..field_count) |field_index| {
        const field = try std.fmt.allocPrint(
            allocator,
            "    f{d} : {s},\n",
            .{ field_index, field_ty },
        );
        defer allocator.free(field);
        try source.appendSlice(allocator, field);
    }
    try source.appendSlice(allocator, "}\n\n");
    switch (operation) {
        .parse => try source.appendSlice(allocator,
            \\main : Str -> Try(Shape, [InvalidJson(Str), MissingRequiredField(Str)])
            \\main = |json| Json.parse(json)
            \\
        ),
        .encode => try source.appendSlice(allocator,
            \\main : Shape -> Str
            \\main = |value| Json.to_str(value)
            \\
        ),
    }

    return try source.toOwnedSlice(allocator);
}

fn structuralJsonMonotypeStats(
    allocator: Allocator,
    field_count: usize,
    field_ty: []const u8,
    operation: StructuralJsonOperation,
) TestError!StructuralJsonMonotypeStats {
    const source = try structuralJsonSource(allocator, field_count, field_ty, operation);
    defer allocator.free(source);

    var counters: MonoLower.SpecializationCounters = .{};
    var lowered = try lowerMonotypeModuleWithOptions(allocator, source, .{
        .specialization_counters = &counters,
    });
    defer lowered.deinit(allocator);

    return .{
        .functions = lowered.mono.view().fns.len,
        .definitions = lowered.mono.view().defs.len,
        .expressions = lowered.mono.view().exprs.len,
        .locals = lowered.mono.view().locals.len,
        .template_misses = counters.template_misses,
        .nested_misses = counters.nested_misses,
    };
}

const StructuralJsonLirStats = struct {
    procedures: usize,
    statements: usize,
    locals: usize,
};

fn structuralJsonLirStats(
    allocator: Allocator,
    field_count: usize,
    field_ty: []const u8,
    operation: StructuralJsonOperation,
) TestError!StructuralJsonLirStats {
    const source = try structuralJsonSource(allocator, field_count, field_ty, operation);
    defer allocator.free(source);

    var lowered = try lowerModule(allocator, source, .wrappers);
    defer lowered.deinit(allocator);
    const store = &lowered.lowered.lir_result.store;
    return .{
        .procedures = store.getProcSpecs().len,
        .statements = store.getCFStmts().len,
        .locals = store.getLocals().len,
    };
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
    if (!digestBytesEqual(candidate.identity.request_fn_ty_digest, expected.identity.request_fn_ty_digest)) return false;
    if (!digestBytesEqual(candidate.solved_fn_ty_digest, expected.solved_fn_ty_digest)) return false;
    return try MonoType.typeEqlAcrossStores(
        allocator,
        name_store,
        candidate_types,
        candidate.solved_fn_ty,
        expected_types,
        expected.solved_fn_ty,
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
    const func = switch (view.types.get(record.solved_fn_ty)) {
        .func => |func| func,
        .primitive, .named, .record, .tuple, .tag_union, .list, .box, .erased, .zst => return false,
    };
    const args = view.types.span(func.args);
    if (args.len != 1) return false;
    const arg_matches = switch (view.types.get(args[0])) {
        .primitive => |arg| arg == primitive,
        .named, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => false,
    };
    const ret_matches = switch (view.types.get(func.ret)) {
        .primitive => |ret| ret == primitive,
        .named, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => false,
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

    for (0..arg_locals.len) |index| {
        const local_id = GuardedList.at(arg_locals, index);
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
        .preserve,
    );
    defer interpreter.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const result = interpreter.eval(.{
        .proc_id = try rootProc(lowered),
        .arg_layouts = arg_layouts,
    }) catch |err| switch (err) {
        error.Crash => return runtime_env.snapshot(allocator),
        error.ComptimeExhaustiveness,
        error.DivisionByZero,
        error.ExpectErr,
        error.InvalidHostedFunctionSignature,
        error.OutOfMemory,
        error.RuntimeError,
        error.UnsupportedHostedFunction,
        => return err,
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
            .expect_failed, .crashed, .effect => return error.TestUnexpectedResult,
        }
    }
}

const DebugEffectCounts = struct {
    debug: usize = 0,
    expect: usize = 0,
};

fn countDebugEffectStmts(lowered: *const lir.CheckedPipeline.LoweredProgram) DebugEffectCounts {
    var counts = DebugEffectCounts{};
    for (lowered.lir_result.store.getCFStmts()) |stmt| {
        switch (stmt) {
            .debug => counts.debug += 1,
            .expect => counts.expect += 1,
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
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
            => {},
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

// Repro for https://github.com/roc-lang/roc/issues/10461: specializing a
// constructor argument must preserve the distinct back edges of nested loops.
test "issue 10461 SpecConstr preserves nested loop back-edge arities" {
    const allocator = std.testing.allocator;
    const source =
        \\run : { inner_limit : U64 }, U64 -> U64
        \\run = |config, outer_limit| {
        \\    var $outer = 0.U64
        \\    var $total = 0.U64
        \\    while $outer < outer_limit {
        \\        var $inner = 0.U64
        \\        var $discard_a = 0.U64
        \\        var $discard_b = 0.U64
        \\        while $inner < config.inner_limit {
        \\            $inner = $inner + 1
        \\            $discard_a = $discard_a + 1
        \\            $discard_b = $discard_b + 1
        \\        }
        \\        $outer = $outer + 1
        \\        $total = $total + $inner + $discard_a
        \\    }
        \\    $total
        \\}
        \\
        \\main : Bool -> U64
        \\main = |flag| {
        \\    outer_limit = match flag {
        \\        Bool.True => 2
        \\        Bool.False => 3
        \\    }
        \\    run({ inner_limit: 1 }, outer_limit)
        \\}
    ;

    var lifted = try liftModuleAfterSpecConstr(allocator, source);
    defer lifted.deinit(allocator);
}

test "issue 10153 nested loops do not multiply SpecConstr callable functions" {
    const allocator = std.testing.allocator;
    // Repro for https://github.com/roc-lang/roc/issues/10153. Adding one fixed
    // nested loop may add its own worker family, but must not duplicate the
    // already-specialized callable graph produced by the generic Json wrapper.
    const single_loop_source =
        \\outer : List(U8) -> List(U8)
        \\outer = |xs| {
        \\    var $out = []
        \\    for x in xs {
        \\        $out = List.concat($out, inner(x))
        \\    }
        \\    $out
        \\}
        \\
        \\inner : U8 -> List(U8)
        \\inner = |x| [x]
        \\
        \\parse_response = |body|
        \\    match Json.parse(body) {
        \\        Ok(v) => Ok(v)
        \\        Err(_) => Err(Bad)
        \\    }
        \\
        \\main : {}
        \\main = {
        \\    _xs = outer([1, 2])
        \\    decoded : Try({ id : I32 }, _)
        \\    decoded = parse_response("{\"id\": 1}")
        \\    match decoded {
        \\        Ok(_) => {}
        \\        Err(_) => {}
        \\    }
        \\}
    ;
    const nested_loop_source =
        \\outer : List(U8) -> List(U8)
        \\outer = |xs| {
        \\    var $out = []
        \\    for x in xs {
        \\        $out = List.concat($out, inner(x))
        \\    }
        \\    $out
        \\}
        \\
        \\inner : U8 -> List(U8)
        \\inner = |x| {
        \\    var $out = []
        \\    for _y in 0..<1 {
        \\        $out = List.append($out, x)
        \\    }
        \\    $out
        \\}
        \\
        \\parse_response = |body|
        \\    match Json.parse(body) {
        \\        Ok(v) => Ok(v)
        \\        Err(_) => Err(Bad)
        \\    }
        \\
        \\main : {}
        \\main = {
        \\    _xs = outer([1, 2])
        \\    decoded : Try({ id : I32 }, _)
        \\    decoded = parse_response("{\"id\": 1}")
        \\    match decoded {
        \\        Ok(_) => {}
        \\        Err(_) => {}
        \\    }
        \\}
    ;

    var single_loop = try liftModuleAfterSpecConstr(allocator, single_loop_source);
    defer single_loop.deinit(allocator);
    var nested_loop = try liftModuleAfterSpecConstr(allocator, nested_loop_source);
    defer nested_loop.deinit(allocator);

    try std.testing.expect(nested_loop.lifted.fnCount() <= single_loop.lifted.fnCount() * 2);
    try std.testing.expect(nested_loop.lifted.exprCount() <= single_loop.lifted.exprCount() * 2);
}

test "issue 10165 higher-order decoder widens propagated error row" {
    const allocator = std.testing.allocator;
    // Repro for https://github.com/roc-lang/roc/issues/10165. A decoder that
    // propagates a leaf error with `?` may add its own error tag, and the
    // resulting higher-order callable must lower with that wider error row.
    const source =
        \\Stmt : {}
        \\
        \\str_dec : Str -> (List(Str) -> (Stmt -> Try(Str, [NoSuchField(Str), ..])))
        \\str_dec = |_name| |_cols| |_stmt| Ok("todo")
        \\
        \\main : {} -> Try({}, _)
        \\main = |_args| {
        \\    dec = decode_row(["status"])
        \\    row = dec({})?
        \\    _ = row
        \\    Ok({})
        \\}
        \\
        \\decode_row = |cols|
        \\    |stmt| {
        \\        status_str = str_dec("status")(cols)(stmt)?
        \\        match status_str {
        \\            "todo" => Ok(Todo)
        \\            _ => Err(ParseError("unknown status"))
        \\        }
        \\    }
    ;

    var lowered = try lowerModule(allocator, source, .none);
    defer lowered.deinit(allocator);

    var run = try runLoweredWithHostEvents(allocator, &lowered.lowered);
    defer run.deinit(allocator);
    try std.testing.expectEqual(eval.RuntimeHostEnv.Termination.returned, run.termination);
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
    for (solved.lifted.fnsView(), 0..) |fn_, index| {
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
    return lowered.main_proc orelse error.MissingRootProcedure;
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

    var visited = collections.DenseMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |stmt_id| {
        const visited_entry = try visited.getOrPut(stmt_id);
        if (visited_entry.found_existing) continue;

        switch (lowered.lir_result.store.getCFStmt(stmt_id)) {
            .assign_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_literal => |stmt| try work.append(allocator, stmt.next),
            .init_uninitialized => |stmt| try work.append(allocator, stmt.next),
            .store_struct => |stmt| try work.append(allocator, stmt.next),
            .store_tag => |stmt| try work.append(allocator, stmt.next),
            .assign_call => |stmt| {
                try calls.append(allocator, stmt.proc);
                try work.append(allocator, stmt.next);
            },
            .assign_call_erased => |stmt| try work.append(allocator, stmt.next),
            .assign_packed_erased_fn => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_desc_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_dict_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_box => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_reuse_box => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_unbox => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_adapt => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_inspect => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_eq => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_tag => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_tag_payload => |stmt| try work.append(allocator, stmt.next),
            .assign_call_dict => |stmt| try work.append(allocator, stmt.next),
            .boxy_tag_match => |stmt| {
                try work.append(allocator, stmt.on_match);
                try work.append(allocator, stmt.on_miss);
            },
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
                const branches = lowered.lir_result.store.getCFSwitchBranches(stmt.branches);
                for (0..branches.len) |i| {
                    const branch = GuardedList.at(branches, i);
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
                const arms = lowered.lir_result.store.getStrMatchArms(stmt.arms);
                for (0..arms.len) |i| {
                    const arm = GuardedList.at(arms, i);
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
    incref_count: usize = 0,
    decref_count: usize = 0,
    decref_if_initialized_count: usize = 0,
    free_count: usize = 0,
};

fn collectProcShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
) TestError!ProcShape {
    return collectLirResultProcShape(allocator, &lowered.lir_result, proc_id);
}

const IterCollectShape = enum {
    specialized,
    generic,
};

fn procShapeMatchesIterCollect(shape: ProcShape, wanted: IterCollectShape) bool {
    // Fingerprints of the `Iter.collect` -> `List.from_iter` worker over a range
    // (ranges are built on `Iter.custom`, so the worker's iterator argument is a
    // custom-step record). `from_iter` branches on the iterator's length: a Known
    // length reserves the whole allocation up front and writes each item with the
    // unchecked append, while an Unknown length grows with the reserving append.
    // That per-element branch (the inner `match length`) accounts for the
    // worker's switch/join/jump counts, which are identical in both builds. The
    // builds differ in call structure: the generic worker leaves the range
    // step's iterator-rebuild helpers as four outgoing direct calls, while spec
    // constr inlines the step closure for the concrete range state, leaving at
    // most two.
    return switch (wanted) {
        .specialized => shape.arg_count == 1 and
            shape.direct_call_count <= 2 and
            shape.switch_count == 8 and
            shape.join_count == 11 and
            shape.jump_count == 15 and
            shape.struct_assign_count >= 2,
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

    var visited = collections.DenseMap(LIR.LirProcSpecId, void).init(allocator);
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

    var visited = collections.DenseMap(LIR.LirProcSpecId, void).init(allocator);
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

    switch (program.getExprAt(index).data) {
        .@"unreachable",
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
            const operands = program.captureOperandSpan(fn_ref.captures);
            for (0..operands.len) |i| {
                const operand = GuardedList.at(operands, i);
                markReachableLiftedExpr(program, operand.value, reachable);
            }
        },
        .list,
        .tuple,
        => |items| {
            const children = program.exprSpan(items);
            for (0..children.len) |i| markReachableLiftedExpr(program, GuardedList.at(children, i), reachable);
        },
        .record => |fields| {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |i| {
                const field = GuardedList.at(field_exprs, i);
                markReachableLiftedExpr(program, field.value, reachable);
            }
        },
        .tag => |tag| {
            const payloads = program.exprSpan(tag.payloads);
            for (0..payloads.len) |i| markReachableLiftedExpr(program, GuardedList.at(payloads, i), reachable);
        },
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
            const args = program.exprSpan(call.args);
            for (0..args.len) |i| markReachableLiftedExpr(program, GuardedList.at(args, i), reachable);
        },
        .call_proc => |call| {
            const args = program.exprSpan(call.args);
            for (0..args.len) |i| markReachableLiftedExpr(program, GuardedList.at(args, i), reachable);
            const operands = program.captureOperandSpan(call.captures);
            for (0..operands.len) |i| {
                const operand = GuardedList.at(operands, i);
                markReachableLiftedExpr(program, operand.value, reachable);
            }
        },
        .low_level => |call| {
            const args = program.exprSpan(call.args);
            for (0..args.len) |i| markReachableLiftedExpr(program, GuardedList.at(args, i), reachable);
        },
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
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |i| {
                const branch = GuardedList.at(branches, i);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    markReachableLiftedStmt(program, GuardedList.at(bindings, binding_index), reachable);
                }
                if (branch.guard) |guard| markReachableLiftedExpr(program, guard, reachable);
                markReachableLiftedExpr(program, branch.body, reachable);
            }
        },
        .if_ => |if_| {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |i| {
                const branch = GuardedList.at(branches, i);
                markReachableLiftedExpr(program, branch.cond, reachable);
                markReachableLiftedExpr(program, branch.body, reachable);
            }
            markReachableLiftedExpr(program, if_.final_else, reachable);
        },
        .block => |block| {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |i| markReachableLiftedStmt(program, GuardedList.at(statements, i), reachable);
            markReachableLiftedExpr(program, block.final_expr, reachable);
        },
        .loop_ => |loop| {
            const initial_values = program.exprSpan(loop.initial_values);
            for (0..initial_values.len) |i| markReachableLiftedExpr(program, GuardedList.at(initial_values, i), reachable);
            markReachableLiftedExpr(program, loop.body, reachable);
        },
        .break_ => |maybe| if (maybe) |value| markReachableLiftedExpr(program, value, reachable),
        .continue_ => |continue_| {
            const values = program.exprSpan(continue_.values);
            for (0..values.len) |i| markReachableLiftedExpr(program, GuardedList.at(values, i), reachable);
        },
    }
}

fn markReachableLiftedStmt(
    program: *const postcheck.MonotypeLifted.Ast.Program,
    stmt_id: postcheck.MonotypeLifted.Ast.StmtId,
    reachable: []bool,
) void {
    switch (program.getStmt(stmt_id)) {
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
        shape.jump_count >= 2 and
        shape.struct_assign_count == 0;
}

fn whileRecordStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 1 and
        shape.jump_count >= 2;
}

/// The shape aggregate loop state takes without call specialization now that
/// join scalarization sees through lowered aliases: the loop carries the
/// fields as separate join parameters, while the seeded initializer builds
/// remain live outside the loop.
fn whileRecordStateWorkerIsScalarizedUnspecialized(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count >= 2 and
        shape.jump_count >= 2 and
        shape.struct_assign_count >= 1;
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

/// The shape a tail-recursive aggregate-state worker takes without call
/// specialization: the proc still receives the aggregate, but join
/// scalarization has dissolved the loop-carried wrapper, so the loop rebuilds
/// no struct and carries the fields as separate join parameters.
fn unspecializedWorkerLoopIsScalarized(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0 and
        shape.max_join_param_count >= 2;
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
        .local_template,
        .imported_template,
        .local_hosted,
        .imported_hosted,
        .checked_generated,
        .parser_runtime,
        .encoder_for_runtime,
        => null,
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

test "issue 10121 optional JSON record encoder has linear Monotype function growth" {
    // Repro for https://github.com/roc-lang/roc/issues/10121: generated
    // functions should grow at most linearly with the encoded record shape.
    const allocator = std.testing.allocator;
    const four_fields = try structuralJsonMonotypeStats(allocator, 4, "Try(Str, [Missing])", .encode);
    const eight_fields = try structuralJsonMonotypeStats(allocator, 8, "Try(Str, [Missing])", .encode);

    const linear_bound = four_fields.functions * 2;
    if (eight_fields.functions > linear_bound) {
        std.debug.print(
            "optional JSON encoder generated {d} functions for 4 fields but {d} for 8 fields " ++
                "(template misses {d}->{d}, nested misses {d}->{d})\n",
            .{
                four_fields.functions,
                eight_fields.functions,
                four_fields.template_misses,
                eight_fields.template_misses,
                four_fields.nested_misses,
                eight_fields.nested_misses,
            },
        );
    }
    try std.testing.expect(eight_fields.functions <= linear_bound);
}

test "issue 10121 repeated nested JSON record fields share encoder helpers" {
    const allocator = std.testing.allocator;
    const four_fields = try structuralJsonMonotypeStats(allocator, 4, "{ bar : Str, count : U64 }", .encode);
    const eight_fields = try structuralJsonMonotypeStats(allocator, 8, "{ bar : Str, count : U64 }", .encode);

    if (eight_fields.functions > four_fields.functions + 4) {
        std.debug.print("nested JSON encoder functions 4={d} 8={d}\n", .{ four_fields.functions, eight_fields.functions });
    }
    try std.testing.expect(eight_fields.functions <= four_fields.functions + 4);
}

test "issue 10121 repeated nested JSON record fields share parser helpers" {
    const allocator = std.testing.allocator;
    const four_fields = try structuralJsonMonotypeStats(allocator, 4, "{ bar : Str, count : U64 }", .parse);
    const eight_fields = try structuralJsonMonotypeStats(allocator, 8, "{ bar : Str, count : U64 }", .parse);

    const linear_expression_bound = four_fields.expressions * 2;
    if (eight_fields.expressions > linear_expression_bound) {
        std.debug.print(
            "nested JSON parser Monotype size grew nonlinearly: " ++
                "defs {d}->{d}, exprs {d}->{d}, locals {d}->{d}\n",
            .{
                four_fields.definitions,
                eight_fields.definitions,
                four_fields.expressions,
                eight_fields.expressions,
                four_fields.locals,
                eight_fields.locals,
            },
        );
    }
    try std.testing.expect(eight_fields.expressions <= linear_expression_bound);
}

test "issue 10121 structural JSON helper sharing survives LIR lowering" {
    const allocator = std.testing.allocator;
    const encoder_one = try structuralJsonLirStats(allocator, 1, "Try(Str, [Missing])", .encode);
    const encoder_two = try structuralJsonLirStats(allocator, 2, "Try(Str, [Missing])", .encode);
    const parser_one = try structuralJsonLirStats(allocator, 1, "{ bar : Str, count : U64 }", .parse);

    const encoder_is_linear = encoder_two.procedures <= encoder_one.procedures * 2 and
        encoder_two.statements <= encoder_one.statements * 2 and
        encoder_two.locals <= encoder_one.locals * 2;
    const parser_within_budget = parser_one.procedures < 4096 and
        parser_one.statements < 65536 and
        parser_one.locals < 65536;
    if (!encoder_is_linear or !parser_within_budget) {
        std.debug.print(
            "structural JSON LIR exceeded smoke-test bounds: encoder " ++
                "procs {d}->{d}, stmts {d}->{d}, locals {d}->{d}; parser " ++
                "procs {d}, stmts {d}, locals {d}\n",
            .{
                encoder_one.procedures,
                encoder_two.procedures,
                encoder_one.statements,
                encoder_two.statements,
                encoder_one.locals,
                encoder_two.locals,
                parser_one.procedures,
                parser_one.statements,
                parser_one.locals,
            },
        );
    }
    try std.testing.expect(encoder_is_linear);
    try std.testing.expect(parser_within_budget);
}

test "issue 10121 shared JSON helpers preserve optional nested round trips" {
    const allocator = std.testing.allocator;
    const source =
        \\Shape : {
        \\    item : Try({ bar : Str, count : U64 }, [Missing]),
        \\}
        \\
        \\main : Bool
        \\main = {
        \\    ok_original : Shape
        \\    ok_original = {
        \\        item: Ok({ bar: "one", count: 1 }),
        \\    }
        \\    missing_original : Shape
        \\    missing_original = {
        \\        item: Err(Missing),
        \\    }
        \\
        \\    ok_encoded = Json.to_str(ok_original)
        \\    missing_encoded = Json.to_str(missing_original)
        \\    ok_parsed : Try(Shape, [InvalidJson(Str), MissingRequiredField(Str)])
        \\    ok_parsed = Json.parse(ok_encoded)
        \\    missing_parsed : Try(Shape, [InvalidJson(Str), MissingRequiredField(Str)])
        \\    missing_parsed = Json.parse(missing_encoded)
        \\
        \\    ok_round_trips =
        \\        match ok_parsed {
        \\            Ok(value) => Json.to_str(value) == ok_encoded
        \\            Err(_) => False
        \\        }
        \\    missing_round_trips =
        \\        match missing_parsed {
        \\            Ok(value) => Json.to_str(value) == missing_encoded
        \\            Err(_) => False
        \\        }
        \\    ok_round_trips and missing_round_trips
        \\}
    ;

    var compiled = try helpers.compileInspectedProgramForTargetWithBuiltin(
        allocator,
        std.testing.io,
        .module,
        source,
        &.{},
        .native,
        try sharedPrePublishedBuiltin(),
        null,
        .lss,
    );
    defer compiled.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), compiled.resources.checker.problems.problems.items.len);
    const output = try helpers.lirInterpreterInspectedStr(allocator, &compiled.lowered);
    defer allocator.free(output);
    try std.testing.expectEqualStrings("True", output);
}

test "issue 9802 same-type map2 specialization counters are bounded" {
    const allocator = std.testing.allocator;
    const source =
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

    try expectMonotypeSpecializationCountersWithin(try monotypeCountersForModule(allocator, source), .{
        // The eight scalar `plus` method calls are producer-authored low-level
        // operations, so direct publication emits them without procedure
        // specialization requests.
        .template_requests = 19,
        .template_hits = 15,
        .template_misses = 4,
        .nested_requests = 16,
        .nested_hits = 8,
        .nested_misses = 8,
        .template_lookup_candidates = 0,
        .nested_lookup_candidates = 0,
        .specialization_type_digest_requests = 74,
        .max_specialization_type_digest_cache_hits = 160,
        .max_specialization_type_digest_cache_misses = 160,
        .max_specialization_type_digest_nodes_visited = 160,
        .exact_type_checks = 0,
        .nominal_backing_reuses = 1,
        .nominal_backing_instantiations = 86,
    });
}

test "test roots share template work only when explicitly grouped" {
    const allocator = std.testing.allocator;
    const source =
        \\identity : a -> a
        \\identity = |value| value
        \\
        \\expect identity(1) == 1
        \\expect identity(2) == 2
        \\
        \\main = 0
    ;

    var isolated_diagnostics = MonoLower.Diagnostics{};
    var isolated = try lowerMonotypeModuleWithOptions(allocator, source, .{
        .diagnostics = &isolated_diagnostics,
        .root_selection = .test_expects,
    });
    defer isolated.deinit(allocator);

    var shared_diagnostics = MonoLower.Diagnostics{};
    var shared = try lowerMonotypeModuleWithOptions(allocator, source, .{
        .diagnostics = &shared_diagnostics,
        .root_selection = .test_expects,
        .procedure_template_root_grouping = .shared_adjacent,
    });
    defer shared.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 0), isolated_diagnostics.body.cross_root_template_reuses);
    try std.testing.expect(shared_diagnostics.body.cross_root_template_reuses > 0);
}

test "issue 10529 open Try chain with named local callback stays bounded" {
    const allocator = std.testing.allocator;
    const source =
        \\take0 = |b| {
        \\    to_end = |_| End
        \\    Ok({ val: b.get(0).map_err(to_end)?, rest: b.drop_first(1) })
        \\}
        \\take1 = |b| Ok({ val: take0(b)?.val, rest: take0(b)?.rest })
        \\take2 = |b| Ok({ val: take1(b)?.val, rest: take1(b)?.rest })
        \\take3 = |b| Ok({ val: take2(b)?.val, rest: take2(b)?.rest })
        \\take4 = |b| Ok({ val: take3(b)?.val, rest: take3(b)?.rest })
        \\take5 = |b| Ok({ val: take4(b)?.val, rest: take4(b)?.rest })
        \\take6 = |b| Ok({ val: take5(b)?.val, rest: take5(b)?.rest })
        \\
        \\main : {} -> Try({ val : U8, rest : List(U8) }, [End, ..])
        \\main = |_| take6([1, 2, 3])
    ;

    const counters = try monotypeCountersForModule(allocator, source);
    try std.testing.expect(counters.template_misses <= 20);
    // Generalized record fields retain distinct source-value/runtime-slot
    // cells until specialization freeze. Keep that fixed linear bookkeeping
    // bounded while guarding against the former exponential Try-chain growth.
    try std.testing.expect(counters.nominal_backing_instantiations <= 325);
}

test "specialization interface replay follows returned local functions through wrappers" {
    const allocator = std.testing.allocator;
    const source =
        \\mk = |f| {
        \\    show = || f({}).map_err(|_| ShowFailed)
        \\    show
        \\}
        \\
        \\wrap = |f| mk(f)
        \\
        \\main : {} -> Try({}, [ShowFailed])
        \\main = |_| {
        \\    f : {} -> Try({}, [Empty])
        \\    f = |_| Ok({})
        \\    wrap(f)()
        \\}
    ;

    _ = try monotypeCountersForModule(allocator, source);
}

test "specialization interface replay keeps unequal generic requests through local dependencies distinct" {
    const allocator = std.testing.allocator;
    const source =
        \\id = |value| value
        \\
        \\make = |value| {
        \\    get = || id(value)
        \\    get
        \\}
        \\
        \\pair = |left, right| {
        \\    left: make(left)(),
        \\    right: make(right)(),
        \\}
        \\
        \\main : {} -> { left : U64, right : Str }
        \\main = |_| pair(1, "one")
    ;

    _ = try monotypeCountersForModule(allocator, source);
}

test "issue 9802 growing-structural map2 specialization counters are bounded" {
    const allocator = std.testing.allocator;
    const source =
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

    try expectMonotypeSpecializationCountersWithin(try monotypeCountersForModule(allocator, source), .{
        .template_requests = 15,
        .template_hits = 5,
        .template_misses = 10,
        .nested_requests = 12,
        .nested_hits = 6,
        .nested_misses = 6,
        .template_lookup_candidates = 0,
        .nested_lookup_candidates = 0,
        .specialization_type_digest_requests = 70,
        .max_specialization_type_digest_cache_hits = 320,
        .max_specialization_type_digest_cache_misses = 360,
        .max_specialization_type_digest_nodes_visited = 360,
        .exact_type_checks = 0,
        .nominal_backing_reuses = 8,
        .nominal_backing_instantiations = 149,
    });
}

test "imported and local generic specialization counters reuse closed types" {
    const allocator = std.testing.allocator;
    const util_module =
        \\Util := [].{
        \\    identity : a -> a
        \\    identity = |value| value
        \\}
    ;
    const source =
        \\import Util
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
        \\        imported_a: Util.identity(value),
        \\        imported_b: Util.identity(value),
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

test "closed direct method calls reuse specialization before durable key construction" {
    const allocator = std.testing.allocator;
    const one_call =
        \\Thing := [Val(U64)].{
        \\    next : Thing -> Thing
        \\    next = |Thing.Val(n)| Thing.Val(n.plus_wrap(1))
        \\}
        \\
        \\main : Thing
        \\main = Thing.Val(0).next()
    ;
    const repeated_calls =
        \\Thing := [Val(U64)].{
        \\    next : Thing -> Thing
        \\    next = |Thing.Val(n)| Thing.Val(n.plus_wrap(1))
        \\}
        \\
        \\main : Thing
        \\main = {
        \\    v0 = Thing.Val(0)
        \\    v1 = v0.next()
        \\    v2 = v1.next()
        \\    v3 = v2.next()
        \\    v4 = v3.next()
        \\    v5 = v4.next()
        \\    v6 = v5.next()
        \\    v7 = v6.next()
        \\    v7.next()
        \\}
    ;

    const one = try monotypeCountersForModule(allocator, one_call);
    const repeated = try monotypeCountersForModule(allocator, repeated_calls);
    try std.testing.expectEqual(one.template_requests, repeated.template_requests);
    try std.testing.expectEqual(one.template_misses, repeated.template_misses);
    try std.testing.expectEqual(one.specialization_type_digest_requests, repeated.specialization_type_digest_requests);
}

test "alias-heavy generic specialization count does not exceed backing types" {
    const allocator = std.testing.allocator;
    const backing_source =
        \\id : a -> a
        \\id = |value| value
        \\
        \\main : { a : U64, b : U64, c : U64, d : U64, e : U64 }
        \\main = {
        \\    x0 : U64
        \\    x0 = 1
        \\    x1 : U64
        \\    x1 = 2
        \\    x2 : U64
        \\    x2 = 3
        \\    x3 : U64
        \\    x3 = 4
        \\    {
        \\        a: id(0),
        \\        b: id(x0),
        \\        c: id(x1),
        \\        d: id(x2),
        \\        e: id(x3),
        \\    }
        \\}
    ;
    const alias_source =
        \\Alias0 : U64
        \\Alias1 : Alias0
        \\Alias2 : Alias1
        \\Alias3 : Alias2
        \\
        \\id : a -> a
        \\id = |value| value
        \\
        \\main : { a : U64, b : U64, c : U64, d : U64, e : U64 }
        \\main = {
        \\    x0 : Alias0
        \\    x0 = 1
        \\    x1 : Alias1
        \\    x1 = 2
        \\    x2 : Alias2
        \\    x2 = 3
        \\    x3 : Alias3
        \\    x3 = 4
        \\    {
        \\        a: id(0),
        \\        b: id(x0),
        \\        c: id(x1),
        \\        d: id(x2),
        \\        e: id(x3),
        \\    }
        \\}
    ;

    var backing = try lowerMonotypeModule(allocator, backing_source);
    defer backing.deinit(allocator);
    var alias = try lowerMonotypeModule(allocator, alias_source);
    defer alias.deinit(allocator);

    try std.testing.expect(alias.mono.view().specs.len <= backing.mono.view().specs.len);
}

test "disabling monotype specialization cache does not change monotype output" {
    const allocator = std.testing.allocator;
    const source =
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
        .fns = loaded_program_view.fns,
        .const_fn_evidence = loaded_program_view.const_fn_evidence,
        .const_fn_evidence_frames = loaded_program_view.const_fn_evidence_frames,
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
    try std.testing.expect(counters.template_misses > 0);
    try expectSpecsCoveredByCachedOrLoaded(allocator, no_cache.mono.view(), cached.mono.view(), loaded_shards[0]);
}

test "nested function specializations keep equal types at different sites distinct" {
    const allocator = std.testing.allocator;
    const source =
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
    const nested_defs = lowered.mono.nestedDefsView();
    for (nested_defs, 0..) |lhs, lhs_index| {
        const lhs_site = nestedSite(lhs) orelse continue;
        for (nested_defs[lhs_index + 1 ..]) |rhs| {
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
    const nested_defs = lowered.mono.nestedDefsView();
    for (nested_defs, 0..) |lhs, lhs_index| {
        const lhs_site = nestedSite(lhs) orelse continue;
        for (nested_defs[lhs_index + 1 ..]) |rhs| {
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

    const specs = mono.specsView();
    try std.testing.expect(specs.len > 0);
    for (specs) |spec| {
        try std.testing.expectEqual(postcheck.Monotype.Ast.SpecStatus.ready, spec.status);
    }

    const a_name = try mono.names.internRecordFieldLabel("a");
    const b_name = try mono.names.internRecordFieldLabel("b");
    var normalized_rows: usize = 0;
    const type_view = mono.types.view();
    for (type_view.types) |content| {
        const span = switch (content) {
            .record => |fields| fields,
            .primitive, .named, .tuple, .tag_union, .list, .box, .func, .erased, .zst => continue,
        };
        const fields = type_view.fieldSpan(span);
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

test "spec constr retains an exact virtual source frame for an inlined procedure" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModuleWithOptions(allocator,
        \\State : { n : U64 }
        \\
        \\read : State -> U64
        \\read = |state| state.n
        \\
        \\main : U64
        \\main = Iter.fold([{ n: 1.U64 }, { n: 2 }].iter().map(read), 0, |acc, n| acc + n)
    , .wrappers, .{ .proc_debug_names = true });
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    try std.testing.expect(store.inlineScopeCount() > 0);

    var found_source_scope = false;
    for (0..store.cf_stmts.len()) |stmt_index| {
        const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
        const scope_id = store.stmtInlineScope(stmt_id);
        if (scope_id == LIR.InlineScopeId.none) continue;
        const scope = store.inlineScope(scope_id);
        if (scope.source_name.isNone()) continue;
        if (!std.mem.eql(u8, store.getString(scope.source_name), "read")) continue;
        if (!scope.call_site.hasLocation()) continue;

        found_source_scope = true;
        try std.testing.expect(!scope.source_symbol.isNone());
        try std.testing.expect(scope.source_loc.hasLocation());
        try std.testing.expect(store.stmtLoc(stmt_id).hasLocation());
    }
    try std.testing.expect(found_source_scope);
}

test "interpreter captures the virtual source frame of an inlined crash" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModuleWithOptions(allocator,
        \\State : { n : U64 }
        \\
        \\read : State -> U64
        \\read = |_state| {
        \\    crash "inline boom"
        \\}
        \\
        \\main : U64
        \\main = Iter.fold([{ n: 1.U64 }].iter().map(read), 0, |acc, n| acc + n)
    , .wrappers, .{ .proc_debug_names = true });
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    var interpreter = try eval.Interpreter.init(
        allocator,
        store,
        &lowered_source.lowered.lir_result.layouts,
        runtime_env.get_ops(),
        .preserve,
    );
    defer interpreter.deinit();

    _ = interpreter.eval(.{ .proc_id = try rootProc(&lowered_source.lowered) }) catch |err| {
        try std.testing.expectEqual(error.Crash, err);
        const scope_id = interpreter.getFailedInlineScope() orelse return error.TestUnexpectedResult;
        const scope = store.inlineScope(scope_id);
        try std.testing.expect(!scope.source_name.isNone());
        try std.testing.expectEqualStrings("read", store.getString(scope.source_name));
        try std.testing.expect(scope.source_loc.hasLocation());
        try std.testing.expect(scope.call_site.hasLocation());
        return;
    };
    return error.TestUnexpectedResult;
}

test "boxy lowering preserves a runtime-built crash message" {
    const allocator = std.testing.allocator;
    const expected_message = "runtime-built crash message long enough for heap storage: 42";
    var lowered_source = try lowerModuleWithOptions(allocator,
        \\main : I64
        \\main = {
        \\    n : I64
        \\    n = 42
        \\    crash "runtime-built crash message long enough for heap storage: ${n.to_str()}"
        \\}
    , .none, .{ .specialization_strategy = .boxy });
    defer lowered_source.deinit(allocator);

    const result = &lowered_source.lowered.lir_result;
    var found_local_crash_message = false;
    for (0..result.store.cf_stmts.len()) |stmt_index| {
        const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
        const stmt = result.store.getCFStmt(stmt_id);
        if (std.meta.activeTag(stmt) != .crash) continue;
        switch (stmt.crash.msg) {
            .literal => {},
            .local => found_local_crash_message = true,
        }
    }
    try std.testing.expect(found_local_crash_message);

    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    var interpreter = try eval.Interpreter.initWithBoxyTables(
        allocator,
        &result.store,
        &result.layouts,
        eval.boxy_runtime.BoxyTables.fromResult(result),
        runtime_env.get_ops(),
        .preserve,
    );
    defer interpreter.deinit();

    _ = interpreter.eval(.{ .proc_id = try rootProc(&lowered_source.lowered) }) catch |err| {
        try std.testing.expectEqual(error.Crash, err);
        try std.testing.expectEqualStrings(expected_message, interpreter.getCrashMessage() orelse return error.TestUnexpectedResult);
        return;
    };
    return error.TestUnexpectedResult;
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

    for (lifted.lifted.fnsView()) |fn_| {
        try std.testing.expect(fn_.body == .roc);
    }
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

    // Without call specialization the proc keeps its aggregate argument, but
    // join scalarization still dissolves the loop-carried record.
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, directRecordWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, directRecordWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, unspecializedWorkerLoopIsScalarized));
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
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsScalarizedUnspecialized));
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

    // As with record state: no call specialization, but the loop-carried
    // tuple still scalarizes.
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, directTupleWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, directTupleWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, unspecializedWorkerLoopIsScalarized));
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
    try std.testing.expectEqual(store.getCFStmts().len, store.getCFStmtLocs().len);
    try std.testing.expectEqual(store.getCFStmts().len, store.getCFStmtRegions().len);
    try std.testing.expectEqual(store.getProcSpecs().len, store.getProcLocs().len);
    try std.testing.expect(store.getProcDebugNames().len > 0);
    for (store.getProcDebugNames()) |entry| {
        try std.testing.expect(entry.proc < store.getProcSpecs().len);
    }
    try std.testing.expect(store.sourceFileCount() >= 1);

    var located: usize = 0;
    for (store.getCFStmtLocs(), store.getCFStmtRegions(), store.getCFStmts()) |loc, region, stmt| {
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
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
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
    for (store.getProcLocs()) |loc| {
        if (loc.hasLocation()) {
            located_procs += 1;
            try std.testing.expect(loc.file < store.sourceFileCount());
        }
    }
    try std.testing.expect(located_procs > 0);

    var found_add2 = false;
    var found_mul3 = false;
    for (0..store.getProcSpecs().len) |i| {
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
    for (0..store.getProcSpecs().len) |i| {
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
    for (store.getCFStmtLocs(), store.getCFStmtRegions()) |loc, region| {
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
    try std.testing.expectEqual(store.getLocals().len, store.getLocalNamesRaw().len);

    var found_first = false;
    var found_second = false;
    for (0..store.getLocals().len) |i| {
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
    const lifted_fns = lifted.lifted.fnsView();
    try std.testing.expect(lifted_fns.len >= depth);
    for (lifted_fns) |func| {
        try std.testing.expectEqual(@as(u32, 0), func.captures.len);
    }
}

const LirProgram = lir.Program;

const ExpectedHostEvent = union(enum) {
    dbg: []const u8,
    expect_failed,
    crashed: []const u8,
};

fn expectOptimizedHostEvents(
    source: []const u8,
    expected_termination: eval.RuntimeHostEnv.Termination,
    expected: []const ExpectedHostEvent,
) TestError!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .proc_debug_names = true });
    defer optimized.deinit(allocator);

    var run = try runLoweredWithHostEvents(allocator, &optimized.lowered);
    defer run.deinit(allocator);

    try std.testing.expectEqual(expected_termination, run.termination);
    try std.testing.expectEqual(expected.len, run.events.len);
    for (expected, run.events) |expected_event, actual_event| {
        switch (expected_event) {
            .dbg => |expected_msg| switch (actual_event) {
                .dbg => |actual_msg| try std.testing.expectEqualStrings(expected_msg, actual_msg),
                .expect_failed, .crashed, .effect => return error.TestUnexpectedResult,
            },
            .expect_failed => switch (actual_event) {
                .expect_failed => {},
                .dbg, .crashed, .effect => return error.TestUnexpectedResult,
            },
            .crashed => |expected_msg| switch (actual_event) {
                .crashed => |actual_msg| try std.testing.expectEqualStrings(expected_msg, actual_msg),
                .dbg, .expect_failed, .effect => return error.TestUnexpectedResult,
            },
        }
    }
}

fn collectLirResultProcShape(
    allocator: Allocator,
    result: *const LirProgram.Result,
    proc_id: LIR.LirProcSpecId,
) TestError!ProcShape {
    const proc = result.store.getProcSpec(proc_id);
    var shape = ProcShape{
        .arg_count = result.store.getLocalSpan(proc.args).len,
    };

    const body = proc.body orelse return shape;

    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, body);

    var visited = collections.DenseMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |stmt_id| {
        const visited_entry = try visited.getOrPut(stmt_id);
        if (visited_entry.found_existing) continue;

        switch (result.store.getCFStmt(stmt_id)) {
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
            .assign_boxy_desc_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_dict_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_box => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_reuse_box => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_unbox => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_adapt => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_inspect => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_eq => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_tag => |stmt| try work.append(allocator, stmt.next),
            .assign_boxy_tag_payload => |stmt| try work.append(allocator, stmt.next),
            .assign_call_dict => |stmt| try work.append(allocator, stmt.next),
            .boxy_tag_match => |stmt| {
                try work.append(allocator, stmt.on_match);
                try work.append(allocator, stmt.on_miss);
            },
            .assign_low_level => |stmt| {
                shape.low_level_count += 1;
                if (stmt.op == .list_len) shape.list_len_count += 1;
                if (stmt.op == .list_get_unsafe) shape.list_get_unsafe_count += 1;
                if (stmt.op == .list_with_capacity) shape.list_with_capacity_count += 1;
                if (stmt.op == .list_append_unsafe) shape.list_append_unsafe_count += 1;
                if (stmt.op == .list_reserve) shape.list_reserve_count += 1;
                if (stmt.op == .str_count_utf8_bytes) shape.str_count_utf8_bytes_count += 1;
                if (stmt.op == .str_concat) shape.str_concat_count += 1;
                if (stmt.op == .box_box) shape.box_box_count += 1;
                if (stmt.op == .box_unbox) shape.box_unbox_count += 1;
                if (stmt.op == .box_prepare_update) shape.box_prepare_update_count += 1;
                if (stmt.op == .ptr_cast) shape.ptr_cast_count += 1;
                if (stmt.op == .ptr_load) shape.ptr_load_count += 1;
                if (stmt.op == .ptr_store) shape.ptr_store_count += 1;
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
            .incref => |stmt| {
                shape.incref_count += 1;
                try work.append(allocator, stmt.next);
            },
            .decref => |stmt| {
                shape.decref_count += 1;
                try work.append(allocator, stmt.next);
            },
            .decref_if_initialized => |stmt| {
                shape.decref_if_initialized_count += 1;
                try work.append(allocator, stmt.next);
            },
            .free => |stmt| {
                shape.free_count += 1;
                try work.append(allocator, stmt.next);
            },
            .switch_stmt => |stmt| {
                shape.switch_count += 1;
                if (stmt.continuation) |continuation| try work.append(allocator, continuation);
                try work.append(allocator, stmt.default_branch);
                const branches = result.store.getCFSwitchBranches(stmt.branches);
                for (0..branches.len) |index| {
                    try work.append(allocator, GuardedList.at(branches, index).body);
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
                const arms = result.store.getStrMatchArms(stmt.arms);
                for (0..arms.len) |index| {
                    try work.append(allocator, GuardedList.at(arms, index).on_match);
                }
                try work.append(allocator, stmt.on_miss);
            },
            .join => |stmt| {
                shape.join_count += 1;
                shape.max_join_param_count = @max(
                    shape.max_join_param_count,
                    result.store.getLocalSpan(stmt.params).len,
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

fn reachableProcDebugName(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    expected_name: []const u8,
) TestError!bool {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = collections.DenseMap(LIR.LirProcSpecId, void).init(allocator);
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
) TestError!usize {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = collections.DenseMap(LIR.LirProcSpecId, void).init(allocator);
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
) TestError!void {
    try expectReachableProcShapeFieldNoGreaterBy(allocator, iter_lowered, list_lowered, field_name, 0);
}

fn expectReachableProcShapeFieldNoGreaterBy(
    allocator: Allocator,
    iter_lowered: *const lir.CheckedPipeline.LoweredProgram,
    list_lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime field_name: []const u8,
    allowed_extra: usize,
) TestError!void {
    const iter_total = try reachableProcShapeFieldTotal(allocator, iter_lowered, field_name);
    const list_total = try reachableProcShapeFieldTotal(allocator, list_lowered, field_name);
    try std.testing.expect(iter_total <= list_total + allowed_extra);
}

fn expectReachableProcShapeFieldEqual(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime field_name: []const u8,
    expected: usize,
) TestError!void {
    const actual = try reachableProcShapeFieldTotal(allocator, lowered, field_name);
    try std.testing.expectEqual(expected, actual);
}

fn expectStaticListIterAppendLoopAvoidsListAppendAllocation(
    iter_source: []const u8,
    list_source: []const u8,
) TestError!void {
    const allocator = std.testing.allocator;
    var iter_optimized = try lowerModuleWithOptions(allocator, iter_source, .wrappers, .{ .tag_reachability = true });
    defer iter_optimized.deinit(allocator);
    var list_optimized = try lowerModuleWithOptions(allocator, list_source, .wrappers, .{ .tag_reachability = true });
    defer list_optimized.deinit(allocator);

    try expectReachableProcShapeFieldEqual(allocator, &iter_optimized.lowered, "erased_call_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &iter_optimized.lowered, "packed_erased_fn_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &iter_optimized.lowered, "list_with_capacity_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &iter_optimized.lowered, "list_reserve_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &iter_optimized.lowered, "list_append_unsafe_count", 0);
    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "list_with_capacity_count");
    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "list_reserve_count");
    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "list_append_unsafe_count");
    try expectReachableProcShapeFieldNoGreater(allocator, &iter_optimized.lowered, &list_optimized.lowered, "box_box_count");
    try expectReachableProcShapeFieldNoGreaterBy(allocator, &iter_optimized.lowered, &list_optimized.lowered, "switch_count", 1);
}

fn expectNoReachableErasedCallableLowering(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) TestError!void {
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, lowered, "erased_call_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, lowered, "packed_erased_fn_count"));
}

fn expectLoweredIterChainAllocatesNothing(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) TestError!void {
    try expectReachableProcShapeFieldEqual(allocator, lowered, "box_box_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, lowered, "erased_call_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, lowered, "packed_erased_fn_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, lowered, "list_with_capacity_count", 0);
}

fn expectLoweredIterStateHasNoBoxesOrErasedCallables(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) TestError!void {
    try expectReachableProcShapeFieldEqual(allocator, lowered, "box_box_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, lowered, "erased_call_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, lowered, "packed_erased_fn_count", 0);
}

// Repro for https://github.com/roc-lang/roc/issues/10429: numeric `until` and
// `range_exclusive` iterators consumed directly by `for` have scalar state,
// with no heap or ARC operations.
test "issue 10429 numeric until and range_exclusive loops have no heap or RC operations" {
    const allocator = std.testing.allocator;
    const numeric_types = [_][]const u8{
        "U8",  "I8",  "U16",  "I16",  "U32", "I32",
        "U64", "I64", "U128", "I128", "Dec",
    };

    for (numeric_types) |numeric_type| {
        const source = try std.fmt.allocPrint(allocator,
            \\until_last : {s} -> {s}
            \\until_last = |n| {{
            \\    var $last = 0.{s}
            \\    for i in {s}.until(0, n) {{
            \\        $last = i
            \\    }}
            \\    $last
            \\}}
            \\
            \\range_last : {s} -> {s}
            \\range_last = |n| {{
            \\    var $last = 0.{s}
            \\    for i in {s}.range_exclusive(0, n) {{
            \\        $last = i
            \\    }}
            \\    $last
            \\}}
            \\
            \\main : {s} -> ({s}, {s})
            \\main = |n| (until_last(n), range_last(n))
        , .{
            numeric_type, numeric_type, numeric_type, numeric_type,
            numeric_type, numeric_type, numeric_type, numeric_type,
            numeric_type, numeric_type, numeric_type,
        });
        defer allocator.free(source);

        var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .tag_reachability = true });
        defer optimized.deinit(allocator);

        try expectLoweredIterChainAllocatesNothing(allocator, &optimized.lowered);
        try expectReachableProcShapeFieldEqual(allocator, &optimized.lowered, "incref_count", 0);
        try expectReachableProcShapeFieldEqual(allocator, &optimized.lowered, "decref_count", 0);
        try expectReachableProcShapeFieldEqual(allocator, &optimized.lowered, "decref_if_initialized_count", 0);
        try expectReachableProcShapeFieldEqual(allocator, &optimized.lowered, "free_count", 0);
    }
}

// Zero-allocation gate for iterator chains that escape their construction site
// (returned from a function, passed to a non-inlined function, chosen by a
// branch). Range sources carry no list, so a statically-known chain must lower
// to no heap allocation at all: no boxed iterator state, no erased callable
// dispatch, no list allocation. This is the static companion to the runtime
// allocations_at_most=0 gate in eval_iter_alloc_tests.zig, which cannot express
// module-level function definitions. It checks both `.none` and `.wrappers` so
// the gate proves representation-level minting, not opt-only wrapper
// specialization. RED on the recursive-nominal representation (an escaping
// iterator boxes its state in its constructor).
fn expectEscapingIterChainAllocatesNothing(source: []const u8) TestError!void {
    const allocator = std.testing.allocator;

    var ordinary = try lowerModuleWithOptions(allocator, source, .none, .{ .tag_reachability = true });
    defer ordinary.deinit(allocator);
    try expectLoweredIterChainAllocatesNothing(allocator, &ordinary.lowered);

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .tag_reachability = true });
    defer optimized.deinit(allocator);
    try expectLoweredIterChainAllocatesNothing(allocator, &optimized.lowered);
}

test "issue 10348 nominal declaration with unbound annotation does not panic" {
    const allocator = std.testing.allocator;
    const source =
        \\main : {}
        \\main = {}
        \\M := { f : I }
    ;

    var lowered = try lowerModule(allocator, source, .wrappers);
    defer lowered.deinit(allocator);
}

test "iter alloc static: iterator returned from a function is zero-alloc" {
    try expectEscapingIterChainAllocatesNothing(
        \\consume : Iter(U64) -> U64
        \\consume = |it| {
        \\    var $sum = 0.U64
        \\    for x in it {
        \\        $sum = $sum + x
        \\    }
        \\    $sum
        \\}
        \\
        \\make : U64 -> Iter(U64)
        \\make = |n| Iter.map(0.U64..<n, |x| x + 1)
        \\
        \\main : U64
        \\main = consume(make(5))
    );
}

test "iter alloc static: iterator passed to a non-inlined function is zero-alloc" {
    try expectEscapingIterChainAllocatesNothing(
        \\consume : Iter(U64) -> U64
        \\consume = |it| {
        \\    var $sum = 0.U64
        \\    for x in it {
        \\        $sum = $sum + x
        \\    }
        \\    $sum
        \\}
        \\
        \\main : U64
        \\main = consume(Iter.map(0.U64..<5, |x| x + 1))
    );
}

test "iter alloc static: branch-chosen iterator is zero-alloc" {
    try expectEscapingIterChainAllocatesNothing(
        \\consume : Iter(U64) -> U64
        \\consume = |it| {
        \\    var $sum = 0.U64
        \\    for x in it {
        \\        $sum = $sum + x
        \\    }
        \\    $sum
        \\}
        \\
        \\choose : Bool -> Iter(U64)
        \\choose = |flag|
        \\    if flag {
        \\        Iter.map(0.U64..<5, |x| x + 1)
        \\    } else {
        \\        Iter.keep_if(0.U64..<5, |x| x > 2)
        \\    }
        \\
        \\main : U64
        \\main = consume(choose(5.U64 > 0))
    );
}

test "iter alloc static: same adapter with different capture layouts is zero-alloc" {
    try expectEscapingIterChainAllocatesNothing(
        \\Config : { big : U64, small : U64 }
        \\
        \\consume : Iter(U64) -> U64
        \\consume = |it| {
        \\    var $sum = 0.U64
        \\    for x in it {
        \\        $sum = $sum + x
        \\    }
        \\    $sum
        \\}
        \\
        \\choose : Bool -> Iter(U64)
        \\choose = |flag| {
        \\    offset = 1.U64
        \\    config : Config
        \\    config = { big: 10, small: 3 }
        \\    if flag {
        \\        Iter.map(0.U64..<5, |x| x + offset)
        \\    } else {
        \\        Iter.map(0.U64..<5, |x| x + config.big + config.small)
        \\    }
        \\}
        \\
        \\main : Bool -> U64
        \\main = |flag| consume(choose(flag))
    );
}

test "iter alloc static: runtime-count map wrapping terminates at dynamic boundary" {
    const allocator = std.testing.allocator;
    const source =
        \\consume : Iter(U64) -> U64
        \\consume = |it| {
        \\    var $sum = 0.U64
        \\    for x in it {
        \\        $sum = $sum + x
        \\    }
        \\    $sum
        \\}
        \\
        \\wrap : U64, Iter(U64) -> Iter(U64)
        \\wrap = |count, iterator| {
        \\    var $i = 0.U64
        \\    var $current = iterator
        \\    while $i < count {
        \\        offset = $i
        \\        $current = Iter.map($current, |x| x + offset)
        \\        $i = $i + 1
        \\    }
        \\    $current
        \\}
        \\
        \\main : U64 -> U64
        \\main = |count| consume(wrap(count, 0.U64..<5))
    ;

    var ordinary = try lowerModuleWithOptions(allocator, source, .none, .{ .tag_reachability = true });
    defer ordinary.deinit(allocator);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "box_box_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "erased_call_count", 1);
    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, &ordinary.lowered, "packed_erased_fn_count") > 0);

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .tag_reachability = true });
    defer optimized.deinit(allocator);
    try expectReachableProcShapeFieldEqual(allocator, &optimized.lowered, "box_box_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &optimized.lowered, "erased_call_count", 1);
    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "packed_erased_fn_count") > 0);
}

test "iter alloc static: recursive map wrapping terminates at dynamic boundary" {
    const allocator = std.testing.allocator;
    const source =
        \\consume : Iter(U64) -> U64
        \\consume = |it| {
        \\    var $sum = 0.U64
        \\    for x in it {
        \\        $sum = $sum + x
        \\    }
        \\    $sum
        \\}
        \\
        \\wrap : U64, Iter(U64) -> Iter(U64)
        \\wrap = |count, iterator|
        \\    if count == 0 {
        \\        iterator
        \\    } else {
        \\        offset = count
        \\        wrap(count - 1, Iter.map(iterator, |x| x + offset))
        \\    }
        \\
        \\main : U64 -> U64
        \\main = |count| consume(wrap(count, 0.U64..<5))
    ;

    var ordinary = try lowerModuleWithOptions(allocator, source, .none, .{ .tag_reachability = true });
    defer ordinary.deinit(allocator);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "box_box_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "erased_call_count", 1);
    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, &ordinary.lowered, "packed_erased_fn_count") > 0);

    // The `.wrappers` half of this case is blocked on a capture-identity bug
    // that the termination fixes above unmasked: the recursively-wrapped
    // iterator captures the same binder at two recursion depths, and both
    // capture slots receive the same CaptureId, tripping the lift.zig
    // "lifted capture set contained two slots with the same CaptureId"
    // invariant. Disambiguating recursion-level captures in the
    // BinderIdentity/CaptureId system is tracked as its own fix; when it
    // lands, this test must also lower the source at `.wrappers` and make
    // the same three assertions.
}

// Both sides of the depth backstop on statically bounded chains: a 10-adapter
// chain (depth 11) stays under the cap and lowers flat, while a 20-adapter
// chain (depth 21) trips it and takes the explicit forced-dynamic callable
// representation. Sources are generated so the two tests differ only in
// adapter count.
fn deepStaticChainSource(comptime map_count: usize) []const u8 {
    comptime {
        var source: []const u8 =
            \\main : U64 -> U64
            \\main = |n| {
            \\    i0 = 0.U64..<n
            \\
        ;
        for (0..map_count) |index| {
            source = source ++ std.fmt.comptimePrint("    i{d} = Iter.map(i{d}, |x| x + 1)\n", .{ index + 1, index });
        }
        source = source ++ std.fmt.comptimePrint("    Iter.fold(i{d}, 0.U64, |acc, x| acc + x)\n}}\n", .{map_count});
        return source;
    }
}

test "iter alloc static: deep static chain under the depth cap stays flat" {
    const allocator = std.testing.allocator;
    const source = comptime deepStaticChainSource(10);
    var ordinary = try lowerModuleWithOptions(allocator, source, .none, .{ .tag_reachability = true });
    defer ordinary.deinit(allocator);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "box_box_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "erased_call_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "packed_erased_fn_count", 0);
}

test "iter alloc static: static chain past the depth cap uses forced dynamic representation" {
    const allocator = std.testing.allocator;
    const source = comptime deepStaticChainSource(20);
    var ordinary = try lowerModuleWithOptions(allocator, source, .none, .{ .tag_reachability = true });
    defer ordinary.deinit(allocator);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "box_box_count", 0);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "erased_call_count", 1);
    try expectReachableProcShapeFieldEqual(allocator, &ordinary.lowered, "packed_erased_fn_count", 2);
}

// The base `[list].iter().fold` must lower with no boxed iterator state and no
// erased callable dispatch: the list literal may allocate its backing store, but
// the iterator itself must carry its step closure inline by value. This asserts
// only the iterator-attributable counts (box_box / erased_call / packed_erased);
// the list's own `list_with_capacity` is expected and not asserted here.
test "iter alloc static: base list fold is zero-alloc" {
    const allocator = std.testing.allocator;
    const source =
        \\main : I64
        \\main = {
        \\    xs = [1.I64, 2, 3, 4, 5]
        \\    Iter.fold(xs.iter(), 0, |a, b| a + b)
        \\}
    ;

    var ordinary = try lowerModuleWithOptions(allocator, source, .none, .{ .tag_reachability = true });
    defer ordinary.deinit(allocator);
    try expectLoweredIterStateHasNoBoxesOrErasedCallables(allocator, &ordinary.lowered);

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .tag_reachability = true });
    defer optimized.deinit(allocator);
    try expectLoweredIterStateHasNoBoxesOrErasedCallables(allocator, &optimized.lowered);
}

fn reachableReturnSlotProcCount(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) TestError!usize {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = collections.DenseMap(LIR.LirProcSpecId, void).init(allocator);
    defer visited.deinit();

    var count: usize = 0;
    while (work.pop()) |proc_id| {
        const visited_entry = try visited.getOrPut(proc_id);
        if (visited_entry.found_existing) continue;

        const proc = lowered.lir_result.store.getProcSpec(proc_id);
        const args = lowered.lir_result.store.getLocalSpan(proc.args);
        if (proc.ret_layout == .zst and args.len != 0) candidate: {
            const first_arg_layout = lowered.lir_result.layouts.getLayout(
                lowered.lir_result.store.getLocal(GuardedList.at(args, 0)).layout_idx,
            );
            if (first_arg_layout.tag != .ptr) break :candidate;
            const result_layout = lowered.lir_result.layouts.getLayout(first_arg_layout.getIdx());
            if (result_layout.tag != .struct_ and result_layout.tag != .tag_union) break :candidate;
            const shape = try collectProcShape(allocator, lowered, proc_id);
            if (shape.ptr_store_count != 0 or shape.store_struct_count != 0 or shape.store_tag_count != 0) count += 1;
        }

        const calls = try collectAssignCallProcs(allocator, lowered, proc_id);
        defer allocator.free(calls);
        for (calls) |call| try work.append(allocator, call);
    }
    return count;
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
        shape.jump_count >= 2 and
        shape.struct_assign_count == 0;
}

fn branchJoinedRecordStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 1 and
        shape.jump_count >= 2;
}

fn expectRangeMapCollectUsesDirectListLoop(source: []const u8, expected_append_unsafe_count: usize) TestError!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .proc_debug_names = true });
    defer optimized.deinit(allocator);

    try std.testing.expect(!try reachableIterCollectShape(allocator, &optimized.lowered, .specialized));
    try std.testing.expect(!try reachableIterCollectShape(allocator, &optimized.lowered, .generic));
    // Promoted appends compare the length against the carried fill limit at
    // each site, and the limit seeds on entry and regrowth read it once each.
    try std.testing.expectEqual(@as(usize, 4), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_len_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_get_unsafe_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_with_capacity_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_reserve_count"));
    try std.testing.expectEqual(expected_append_unsafe_count, try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_append_unsafe_count"));
}

test "user iter method is not recognized as builtin list cursor" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
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

test "destination phase 3: effectful boxed update wrapper prepares box update" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\Model : {
        \\    tick : U64,
        \\    label : Str,
        \\}
        \\
        \\update! : Model => Model
        \\update! = |model| {
        \\    tick = model.tick + 1
        \\    { ..model, tick }
        \\}
        \\
        \\main : Box(Model) => Box(Model)
        \\main = |boxed| Box.box(update!(Box.unbox(boxed)))
    , .wrappers);
    defer lowered_source.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "box_unbox_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "box_box_count"));
    try std.testing.expectEqual(@as(usize, 1), try reachableProcShapeFieldTotal(allocator, &lowered_source.lowered, "box_prepare_update_count"));
}

test "destination baseline: boxed lambda is packed then boxed" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
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

// Ported pending iterator redesign: StrAppend remains out of the production
// pipeline until variant generation has a size cost model.

// Ported pending iterator redesign: the materialize-inline plan decision this test asserts is not part of the current inline plan.
// test "call value wrapper is optimized-inline eligible but not materialize-inline eligible" {
//     try expectInlinePlanDecisions(
//         \\callee : U64 -> U64
//         \\callee = |x| x + 1
//         \\
//         \\apply : (U64 -> U64), U64 -> U64
//         \\apply = |fn, x| fn(x)
//         \\
//         \\main : U64
//         \\main = apply(callee, 41)
//     , "apply", true, false);
// }

// Ported pending iterator redesign: the materialize-inline plan decision this test asserts is not part of the current inline plan.
// test "simple direct low-level wrapper is materialize-inline eligible" {
//     try expectInlinePlanDecisions(
//         \\callee : U64 -> U64
//         \\callee = |x| x + 1
//         \\
//         \\main : U64 -> U64
//         \\main = |x| callee(x)
//     , "callee", true, true);
// }

// Ported pending iterator redesign: the current inline plan deliberately
// excludes functions with captures, even when every capture is an inline input.
// Re-enable this when the inline plan represents capture substitution.
// ─── TRMC pass outcomes through the full pipeline ───

test "plant iter pipeline collect uses direct range map list loop" {
    try expectRangeMapCollectUsesDirectListLoop(
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

test "direct range map collect uses direct list loop" {
    try expectRangeMapCollectUsesDirectListLoop(
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

test "non-inlined call list argument keeps let-bound leaves available" {
    // A boundary call cannot be inlined, so its arguments must materialize as
    // ordinary public values. A list argument whose elements are let-bound
    // locals must keep those bindings available (or substituted) when the
    // boundary materializes inside nested inlining.
    const allocator = std.testing.allocator;
    var optimized = try lowerModule(allocator,
        \\len_rec : List(U64), U64 -> U64
        \\len_rec = |bytes, acc| {
        \\    match bytes {
        \\        [] => acc
        \\        [_, .. as rest] => len_rec(rest, acc + 1)
        \\    }
        \\}
        \\
        \\countdown : U64 -> U64
        \\countdown = |x| if x == 0 1 else countdown(x - 1)
        \\
        \\save : U64 -> U64
        \\save = |frame| {
        \\    data = U64.bitwise_and(frame, 255)
        \\    other = countdown(3)
        \\    len_rec([data, other], 0)
        \\}
        \\
        \\init : { frame : U64 } -> U64
        \\init = |state| {
        \\    frame_count = state.frame
        \\    save(frame_count)
        \\}
        \\
        \\step : { frame : U64 }, U64 -> U64
        \\step = |state, mode| {
        \\    if mode == 1 {
        \\        init(state)
        \\    } else {
        \\        0
        \\    }
        \\}
        \\
        \\main : U64
        \\main = step({ frame: 9 }, 1)
    , .wrappers);
    defer optimized.deinit(allocator);
}

test "multi-use match binding emits branch bodies once" {
    // A control-flow value re-emits its branch bodies wherever it
    // materializes, so a let-bound match consumed by more than one
    // materializing use must be emitted once at its binding statement and
    // referenced; otherwise every use duplicates every branch body.
    const allocator = std.testing.allocator;
    var optimized = try lowerModule(allocator,
        \\route : U64 -> U64
        \\route = |x| {
        \\    if x > 3 {
        \\        return 0
        \\    }
        \\    x + 1
        \\}
        \\
        \\label : U64 -> Str
        \\label = |n| {
        \\    state = match route(n) {
        \\        0 => Str.concat("a", "0")
        \\        1 => Str.concat("b", "1")
        \\        2 => Str.concat("c", "2")
        \\        _ => Str.concat("d", "?")
        \\    }
        \\    Str.concat(state, state)
        \\}
        \\
        \\main : Str
        \\main = label(9)
    , .wrappers);
    defer optimized.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 5), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "str_concat_count"));
}

test "boundary field access projects private leaf branch" {
    // A record consumed only through demanded field accesses splits into a
    // sparse private product, and an if branch whose value is an opaque call
    // result is carried whole as a private leaf. A boundary argument that
    // projects a field from such an if value must project through every
    // branch—including the leaf branch, whose field is an ordinary field
    // access on the carried public value—rather than materialize the
    // sparse receiver whole.
    const allocator = std.testing.allocator;
    var optimized = try lowerModule(allocator,
        \\countdown : U64 -> U64
        \\countdown = |x| {
        \\    if x > 3 {
        \\        return 0
        \\    }
        \\    x + 1
        \\}
        \\
        \\load : U64 -> { score : U64, hi : U64, pad : U64 }
        \\load = |seed| {
        \\    if seed == 0 {
        \\        { score: 0, hi: 1, pad: 2 }
        \\    } else {
        \\        load(seed - 1)
        \\    }
        \\}
        \\
        \\use : { score : U64, hi : U64, pad : U64 }, U64 -> U64
        \\use = |state, mode| {
        \\    match countdown(state.score) {
        \\        1 => state.hi + mode
        \\        other => other
        \\    }
        \\}
        \\
        \\main : U64
        \\main = {
        \\    state = if countdown(3) == 1 {
        \\        { score: 10, hi: 20, pad: 30 }
        \\    } else {
        \\        load(7)
        \\    }
        \\    use(state, 1)
        \\}
    , .wrappers);
    defer optimized.deinit(allocator);
}

test "local iterator append loop demands step captures across states" {
    // The append step callable's appended-item capture is demanded only
    // through the step-result `item` demand observed inside the loop body.
    // That observation must reach the owning loop demand node so the state
    // key carries the capture; otherwise the state callable is reconstructed
    // without a capture its body demands.
    const allocator = std.testing.allocator;
    var optimized = try lowerModule(allocator,
        \\Point : { x : I64 }
        \\
        \\points : () -> Iter(Point)
        \\points = || [{ x: 1.I64 }, { x: 2 }].iter().append({ x: 3 })
        \\
        \\main : I64
        \\main = {
        \\    iter = points()
        \\    var $sum = 0.I64
        \\    for point in iter {
        \\        $sum = $sum + point.x
        \\    }
        \\    $sum
        \\}
    , .wrappers);
    defer optimized.deinit(allocator);

    try expectNoReachableErasedCallableLowering(allocator, &optimized.lowered);
}

test "imported iterator producer keeps finite step callables" {
    const allocator = std.testing.allocator;
    const producer_module =
        \\Points := [].{
        \\    Point : { x : I64 }
        \\
        \\    points : () -> Iter(Point)
        \\    points = || [{ x: 1.I64 }, { x: 2 }].iter().append({ x: 3 })
        \\}
    ;
    const source =
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

// Ported pending iterator redesign: post_check_stats.optimized_contexts instrumentation is not part of the current pipeline.
// test "post-check lowering mode constructs optimized context only in optimized mode" {
//     const allocator = std.testing.allocator;
//     const source =
//         \\main : U64
//         \\main = 0
//     ;
//
//     var optimized = try lowerModule(allocator, source, .wrappers);
//     defer optimized.deinit(allocator);
//     var ordinary = try lowerModule(allocator, source, .none);
//     defer ordinary.deinit(allocator);
//
//     try std.testing.expectEqual(@as(u32, 1), optimized.lowered.post_check_stats.optimized_contexts);
//     try std.testing.expectEqual(@as(u32, 0), ordinary.lowered.post_check_stats.optimized_contexts);
// }

// Ported pending iterator redesign: post_check_stats.optimized_contexts instrumentation is not part of the current pipeline.
// test "checking finalization lowering constructs no optimized context" {
//     const allocator = std.testing.allocator;
//     const source =
//         \\main : U64
//         \\main = 0
//     ;
//
//     var lowered = try lowerModuleWithOptions(allocator, source, .none, .{
//         .checked_module_state = .checking_finalization,
//     });
//     defer lowered.deinit(allocator);
//
//     try std.testing.expectEqual(@as(u32, 0), lowered.lowered.post_check_stats.optimized_contexts);
// }

test "post-check lowering mode gates public iter adapter elimination" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_points : U64 -> U64
        \\sum_points = |extra| {
        \\    base_points = [1, 2, 3].iter()
        \\
        \\    collision_points =
        \\        if extra == 0 {
        \\            base_points
        \\        } else {
        \\            base_points.append(extra)
        \\        }
        \\
        \\    var $sum = 0
        \\    for point in collision_points {
        \\        $sum = $sum + point
        \\    }
        \\    $sum
        \\}
        \\
        \\main : U64
        \\main = sum_points(4)
    ;

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .proc_debug_names = true });
    defer optimized.deinit(allocator);
    var ordinary = try lowerModuleWithOptions(allocator, source, .none, .{ .proc_debug_names = true });
    defer ordinary.deinit(allocator);

    try std.testing.expect(!try reachableProcDebugName(allocator, &optimized.lowered, "Builtin.Iter.append"));
    try std.testing.expect(try reachableProcDebugName(allocator, &ordinary.lowered, "Builtin.Iter.append"));
}

// Ported pending iterator redesign: this test constructs state_loop/state_continue lifted IR that the current lifted AST does not define.
// test "state loop lowers to ordinary lir joins" {
//     const allocator = std.testing.allocator;
//     const source =
//         \\main : U64
//         \\main = 0
//     ;
//
//     var lifted_source = try liftModuleAfterSpecConstr(allocator, source);
//     defer helpers.cleanupParseAndCanonical(allocator, lifted_source.resources);
//
//     const Lifted = postcheck.MonotypeLifted.Ast;
//     var lifted = lifted_source.lifted;
//     var lifted_owned = true;
//     defer if (lifted_owned) lifted.deinit();
//     lifted_source.lifted = undefined;
//
//     try std.testing.expectEqual(@as(usize, 1), lifted.roots.items.len);
//     const root_fn_id = lifted.roots.items[0].fn_id;
//     const root_fn_index = @intFromEnum(root_fn_id);
//     const ret_ty = lifted.fns.items[root_fn_index].ret;
//     const original_body = switch (lifted.fns.items[root_fn_index].body) {
//         .roc => |body| body,
//         .hosted => return error.TestUnexpectedResult,
//     };
//
//     const empty_params = try lifted.addTypedLocalSpan(&.{});
//     const empty_values = try lifted.addExprSpan(&.{});
//     const state_start: u32 = @intCast(lifted.state_loop_states.items.len);
//     const state0_id: Lifted.StateLoopStateId = @enumFromInt(state_start);
//     const state1_id: Lifted.StateLoopStateId = @enumFromInt(state_start + 1);
//
//     const break_expr = try lifted.addExpr(.{
//         .ty = ret_ty,
//         .data = .{ .break_ = original_body },
//     });
//     const continue_expr = try lifted.addExpr(.{
//         .ty = ret_ty,
//         .data = .{ .state_continue = .{
//             .target_state = state1_id,
//             .values = empty_values,
//         } },
//     });
//     const states = [_]Lifted.StateLoopState{
//         .{
//             .params = empty_params,
//             .body = continue_expr,
//         },
//         .{
//             .params = empty_params,
//             .body = break_expr,
//         },
//     };
//     const state_span = try lifted.addStateLoopStateSpan(&states);
//     const state_loop_expr = try lifted.addExpr(.{
//         .ty = ret_ty,
//         .data = .{ .state_loop = .{
//             .entry_state = state0_id,
//             .entry_values = empty_values,
//             .states = state_span,
//         } },
//     });
//     lifted.fns.items[root_fn_index].body = .{ .roc = state_loop_expr };
//
//     var solved = try postcheck.LambdaSolved.Solve.run(allocator, lifted);
//     lifted_owned = false;
//     lifted = undefined;
//     var solved_owned = true;
//     errdefer if (solved_owned) solved.deinit();
//
//     var output = try postcheck.SolvedLirLower.run(allocator, base.target.TargetUsize.native, solved, .{});
//     solved_owned = false;
//     solved = undefined;
//     defer output.deinit();
//
//     try std.testing.expectEqual(@as(usize, 1), output.lir_result.root_procs.items.len);
//     const root_proc = output.lir_result.root_procs.items[0];
//     const shape = try collectLirResultProcShape(allocator, &output.lir_result, root_proc);
//
//     try std.testing.expectEqual(@as(usize, 2), shape.join_count);
//     try std.testing.expectEqual(@as(usize, 0), shape.max_join_param_count);
//     try std.testing.expectEqual(@as(usize, 2), shape.jump_count);
// }

test "dynamic static list iter append loop splits nested callable captures" {
    const allocator = std.testing.allocator;
    const source =
        \\Point : { x : I64, y : I64 }
        \\
        \\main : U64 -> I64
        \\main = |anim_index| {
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
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);
}

test "static record list iter append loop avoids direct-list append allocation" {
    const record_iter_source =
        \\Point : { x : I64, y : I64 }
        \\
        \\main : Bool -> I64
        \\main = |use_extra| {
        \\    base_points = [
        \\        { x: 11, y: 2 },
        \\    ].iter()
        \\
        \\    collision_points =
        \\        if use_extra {
        \\            base_points.append({ x: 2, y: 1 })
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
    ;
    const record_list_source =
        \\Point : { x : I64, y : I64 }
        \\
        \\main : Bool -> I64
        \\main = |use_extra| {
        \\    base_points = [
        \\        { x: 11, y: 2 },
        \\    ]
        \\
        \\    collision_points =
        \\        if use_extra {
        \\            base_points.append({ x: 2, y: 1 })
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
    ;

    try expectStaticListIterAppendLoopAvoidsListAppendAllocation(record_iter_source, record_list_source);
}

test "static primitive list iter append loop avoids direct-list append allocation" {
    const primitive_iter_source =
        \\main : Bool -> I64
        \\main = |use_extra| {
        \\    base_points = [11.I64].iter()
        \\
        \\    collision_points =
        \\        if use_extra {
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
    ;
    const primitive_list_source =
        \\main : Bool -> I64
        \\main = |use_extra| {
        \\    base_points = [11.I64]
        \\
        \\    collision_points =
        \\        if use_extra {
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
    ;

    try expectStaticListIterAppendLoopAvoidsListAppendAllocation(primitive_iter_source, primitive_list_source);
}

test "stream from iterator collect keeps finite step callables" {
    const allocator = std.testing.allocator;
    const source =
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

test "optimized infinite custom iterator consumes finite prefix" {
    const source =
        \\main : U64
        \\main = {
        \\    adv : ((U64, U64) -> Try((U64, (U64, U64)), [NoMore]))
        \\    adv = |(a, b)| Try.Ok((a, (b, a + b)))
        \\
        \\    fib_iter = Iter.custom((0.U64, 1.U64), Unknown, adv)
        \\
        \\    var $sum = 0.U64
        \\    for f in fib_iter.take_first(5) {
        \\        $sum = $sum + f
        \\    }
        \\    dbg $sum
        \\    $sum
        \\}
    ;

    try expectOptimizedDbgEvents(source, &.{"7"});
}

test "spec constr list filter-map loop does not produce unbound ARC locals" {
    const allocator = std.testing.allocator;
    const source =
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

test "spec constr preserves known-match expect failure order" {
    try expectOptimizedHostEvents(
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

test "spec constr specializes primitive-start record state carried by while loop" {
    const allocator = std.testing.allocator;
    const source =
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
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsScalarizedUnspecialized));
}

test "spec constr does not require single-field record wrapper for local loop splitting" {
    const allocator = std.testing.allocator;
    const wrapped_source =
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
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsScalarizedUnspecialized));
}

test "spec constr splits loop record state with direct callable captures" {
    const allocator = std.testing.allocator;
    const source =
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
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsScalarizedUnspecialized));
}

test "spec constr splits loop record state with returned callable captures" {
    const allocator = std.testing.allocator;
    const source =
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
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsScalarizedUnspecialized));
}

test "spec constr splits loop record state with annotated returned callable captures" {
    const allocator = std.testing.allocator;
    const source =
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
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsScalarizedUnspecialized));
}

test "spec constr exposes direct call record result for field access" {
    const allocator = std.testing.allocator;
    const source =
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
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, branchJoinedRecordStateWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsScalarizedUnspecialized));
}

test "spec constr specializes match-joined record state carried by while loop" {
    const allocator = std.testing.allocator;
    const source =
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
    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, branchJoinedRecordStateWorkerIsGeneric));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsScalarizedUnspecialized));
}

// Iterator lowering differential harness.
//
// Each `iterdiff:` test lowers ONE Roc source under two inline modes and runs
// both through the interpreter against `RuntimeHostEnv`, then asserts the two
// runs are observationally identical:
//
//   * `.wrappers` is the optimized/inlined lowering (the closest proxy the tree
//     has for the lower-all-known-wrappers path).
//   * `.none` is the naive, un-inlined lowering ("unfused").
//
// The two runs must agree on:
//   * crash-versus-no-crash (`RecordedRun.termination`), and
//   * the full ordered host-effect trace (`RecordedRun.events`): every `dbg`,
//     `expect` failure, and crash message, in order.
//
// Result VALUES are observed through the effect trace: each pipeline `dbg`s its
// result (and, where useful, each element as it is produced). `dbg` renders a
// value structurally and pointer-independently (e.g. `[6, 8, 10, 12]`), so a
// `dbg` of the collected List/Set output is a complete, allocation-independent
// value assertion that lives inside the compared trace. Ordered per-element
// `dbg`s additionally pin element order and effect ordering (design invariants
// 4 and 5). Allocation counts are intentionally NOT compared: fusing away
// adapter objects legitimately changes how much a run allocates.
//
// A test that fails or crashes here on the current tree is a genuine
// pre-existing divergence between the optimized and naive lowerings, not a test
// bug; such cases are committed commented-out with a `// Pre-existing
// divergence:` marker rather than weakened to pass.

fn expectRecordedRunsEqual(
    expected: eval.RuntimeHostEnv.RecordedRun,
    actual: eval.RuntimeHostEnv.RecordedRun,
) TestError!void {
    // crash-versus-no-crash
    try std.testing.expectEqual(expected.termination, actual.termination);

    // full ordered effect trace (dbg values, expect failures, crash messages)
    try std.testing.expectEqual(expected.events.len, actual.events.len);
    for (expected.events, actual.events) |expected_event, actual_event| {
        try std.testing.expectEqual(
            std.meta.activeTag(expected_event),
            std.meta.activeTag(actual_event),
        );
        switch (expected_event) {
            .effect => |expected_effect| switch (actual_event) {
                .effect => |actual_effect| try std.testing.expectEqualStrings(expected_effect.name, actual_effect.name),
                // The activeTag equality above already proved both are effects.
                .dbg, .expect_failed, .crashed => unreachable,
            },
            .dbg, .expect_failed, .crashed => {},
        }
        try std.testing.expectEqualStrings(expected_event.bytes(), actual_event.bytes());
    }
}

fn expectSameObservationsAcrossInlineModes(source: []const u8) TestError!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var naive = try lowerModule(allocator, source, .none);
    defer naive.deinit(allocator);

    var naive_run = try runLoweredWithHostEvents(allocator, &naive.lowered);
    defer naive_run.deinit(allocator);

    var optimized_run = try runLoweredWithHostEvents(allocator, &optimized.lowered);
    defer optimized_run.deinit(allocator);

    try expectRecordedRunsEqual(naive_run, optimized_run);
}

test "iterdiff: recursively-constructed iterator chain agrees across inline modes" {
    // The e026f6e678 fixpoint shape: `wrap` maps its iterator argument and
    // recurses on itself a runtime number of times, so the known iterator value
    // for a use site references the recursive construction and its measured
    // size saturates the work budget. Constructor specialization must not
    // deep-materialize that value; it rebinds it through a plain clone of the
    // source expression instead. This case proves that path lowers and produces
    // the same result as the naive lowering (`wrap(depth, 0..<5)` at depth 3
    // adds 3 to each element, summing to 25). `depth` is a runtime function
    // argument, so the recursion is not statically unrolled.
    try expectSameObservationsAcrossInlineModes(
        \\wrap : U64, Iter(U64) -> Iter(U64)
        \\wrap = |n, it|
        \\    if n == 0 {
        \\        it
        \\    } else {
        \\        wrap(n - 1, Iter.map(it, |x| x + 1))
        \\    }
        \\
        \\build : U64 -> U64
        \\build = |depth| {
        \\    var $sum = 0.U64
        \\    for x in wrap(depth, 0.U64..<5) {
        \\        $sum = $sum + x
        \\    }
        \\    dbg $sum
        \\    $sum
        \\}
        \\
        \\main : U64
        \\main = build(3)
    );
}

test "iterdiff: bounded list map collect agrees across inline modes" {
    // Map over a statically-known list, collected into a List, then reduced to a
    // scalar. The `dbg` of the collected list is the structural (allocation-
    // independent) value assertion; `dbg` of the scalar pins the fold result.
    try expectSameObservationsAcrossInlineModes(
        \\main : I64
        \\main = {
        \\    doubled : List(I64)
        \\    doubled =
        \\        [1.I64, 2, 3, 4, 5, 6]
        \\            .iter()
        \\            .map(|n| n * 2)
        \\            .collect()
        \\    total = List.sum(doubled)
        \\    dbg doubled
        \\    dbg total
        \\    total
        \\}
    );
}

// A filter-like adapter (`keep_if`) drives a collect loop whose loop-carried
// source iterator advances through a runtime step result. The step callable's
// successor iterator must carry the advanced inner iterator produced by the
// step, so the inner index advances every iteration and the loop terminates.
// Both lowering modes observe the same filtered list. Minimal repro:
// `[1.I64, 2, 3].iter().keep_if(|n| n > 1).collect()` returns `[2, 3]`.
test "iterdiff: bounded list map keep_if collect agrees across inline modes" {
    try expectSameObservationsAcrossInlineModes(
        \\main : I64
        \\main = {
        \\    doubled : List(I64)
        \\    doubled =
        \\        [1.I64, 2, 3, 4, 5, 6]
        \\            .iter()
        \\            .map(|n| n * 2)
        \\            .keep_if(|n| n > 5)
        \\            .collect()
        \\    total = List.sum(doubled)
        \\    dbg doubled
        \\    dbg total
        \\    total
        \\}
    );
}

test "iterdiff: if-chosen iterator chains consumed by one loop agree across inline modes" {
    try expectSameObservationsAcrossInlineModes(
        \\main : I64
        \\main = {
        \\    threshold = 4.I64
        \\    chosen : Iter(I64)
        \\    chosen =
        \\        if threshold > 3 {
        \\            [1.I64, 2, 3].iter().map(|n| n * 10)
        \\        } else {
        \\            [4.I64, 5, 6].iter().keep_if(|n| n > 4)
        \\        }
        \\    var $sum = 0.I64
        \\    for x in chosen {
        \\        dbg x
        \\        $sum = $sum + x
        \\    }
        \\    dbg $sum
        \\    $sum
        \\}
    );
}

test "iterdiff: branch-chosen append search with early return agrees across inline modes" {
    // Rocci's `on_screen_collided!` shape exactly: a zero-accumulator `for` over
    // a branch-chosen append chain of record elements that returns early on the
    // first match. The branch-append peel factors the shared base iteration out
    // and replays the per-element check over each arm's appended items (binding
    // each appended record's fields directly); the returned first-match value
    // pins the exact pull order (base elements, then appended items in append
    // order, with the early return short-circuiting). Both lowerings must return
    // the same value for every `(selector, target)` probe.
    try expectSameObservationsAcrossInlineModes(
        \\Point : { x : I64, y : I64 }
        \\
        \\find : U64, I64 -> I64
        \\find = |selector, target| {
        \\    base = [{ x: 10, y: 1 }, { x: 20, y: 2 }, { x: 30, y: 3 }].iter()
        \\    chosen =
        \\        if selector == 2 {
        \\            base.append({ x: 40, y: 4 }).append({ x: 50, y: 5 })
        \\        } else if selector == 1 {
        \\            base.append({ x: 60, y: 6 })
        \\        } else {
        \\            base
        \\        }
        \\    for { x, y } in chosen {
        \\        if x >= target {
        \\            return x + y
        \\        }
        \\    }
        \\    -1
        \\}
        \\
        \\main : I64
        \\main = {
        \\    a = find(2, 35)
        \\    b = find(2, 45)
        \\    c = find(2, 100)
        \\    d = find(1, 55)
        \\    e = find(0, 5)
        \\    f = find(0, 100)
        \\    dbg a
        \\    dbg b
        \\    dbg c
        \\    dbg d
        \\    dbg e
        \\    dbg f
        \\    a + b + c + d + e + f
        \\}
    );
}

test "iterdiff: branch-chosen append evaluates selection and items before base-loop early return" {
    // Constructing the chosen iterator is strict: its condition and selected
    // arm's appended item run before the consuming loop. Even when the loop
    // returns from the shared base and never pulls the appended item, optimized
    // lowering must retain that exact ordered trace.
    try expectSameObservationsAcrossInlineModes(
        \\Point : { x : I64, y : I64 }
        \\
        \\trace : I64 -> I64
        \\trace = |n| {
        \\    dbg n
        \\    n
        \\}
        \\
        \\trace_point : I64, I64 -> Point
        \\trace_point = |x, y| {
        \\    dbg x
        \\    { x, y }
        \\}
        \\
        \\find : I64, I64 -> I64
        \\find = |selector, target| {
        \\    base = [{ x: 10, y: 1 }, { x: 20, y: 2 }, { x: 30, y: 3 }].iter()
        \\    chosen =
        \\        if trace(selector) == 1 {
        \\            base.append(trace_point(40, 4))
        \\        } else {
        \\            base.append(trace_point(50, 5))
        \\        }
        \\    for { x, y } in chosen {
        \\        if x >= target {
        \\            return x + y
        \\        }
        \\    }
        \\    -1
        \\}
        \\
        \\main : I64
        \\main = find(1, 5)
    );
}

test "iterdiff: set materialized mid-pipeline then iterated agrees across inline modes" {
    // Design invariant 4: constructing a Set from the elements really runs, so
    // its deduplication happens exactly where written; the pipeline then keeps
    // iterating over the materialized result. Both lowerings must observe the
    // same deduplicated element sequence and the same collected output.
    try expectSameObservationsAcrossInlineModes(
        \\main : I64
        \\main = {
        \\    deduped : Set(I64)
        \\    deduped = Set.from_list([3.I64, 1, 2, 2, 3, 1, 4, 3])
        \\    doubled : List(I64)
        \\    doubled =
        \\        deduped
        \\            .to_list()
        \\            .iter()
        \\            .map(|n| n * 2)
        \\            .collect()
        \\    dbg deduped.to_list()
        \\    dbg doubled
        \\    List.sum(doubled)
        \\}
    );
}

test "iterdiff: coarse custom is_eq set dedup keeps same representative across inline modes" {
    // Design invariant 6: the optimizer must never use a user `is_eq` result to
    // substitute one value for another. `Bucket.is_eq` compares only `key`, so
    // deduplication is a coarse quotient; `tag` is the representative-
    // distinguishing observer. Both lowerings must keep the SAME surviving
    // representative (identical ordered `tag` trace), never a different one the
    // quotient happens to call equal.
    try expectSameObservationsAcrossInlineModes(
        \\Bucket := { key : I64, tag : I64 }.{
        \\    is_eq : Bucket, Bucket -> Bool
        \\    is_eq = |a, b| a.key == b.key
        \\}
        \\
        \\main : I64
        \\main = {
        \\    buckets : List(Bucket)
        \\    buckets = [
        \\        { key: 1, tag: 100 },
        \\        { key: 2, tag: 200 },
        \\        { key: 1, tag: 999 },
        \\        { key: 2, tag: 888 },
        \\        { key: 3, tag: 300 },
        \\    ]
        \\    deduped : Set(Bucket)
        \\    deduped = Set.from_list(buckets)
        \\    var $tag_sum = 0.I64
        \\    for b in deduped.to_list().iter() {
        \\        dbg b.tag
        \\        $tag_sum = $tag_sum + b.tag
        \\    }
        \\    dbg $tag_sum
        \\    $tag_sum
        \\}
    );
}

test "iterdiff: stream per-element effects agree across inline modes" {
    // Design invariant 5: a Stream pipeline's observable effect trace is the
    // per-element, innermost-first pull order, and every lowering must
    // reproduce it exactly. The effectful `map!` step `dbg`s each element as it
    // is pulled, so the ordered trace pins effect order across inline modes.
    try expectSameObservationsAcrossInlineModes(
        \\main : () => List(I64)
        \\main = || {
        \\    stream =
        \\        [1.I64, 2, 3]
        \\            .iter()
        \\            .stream()
        \\            .map!(|n| {
        \\                dbg n
        \\                n * 2
        \\            })
        \\    result = Stream.collect!(stream)
        \\    dbg result
        \\    result
        \\}
    );
}

// Pre-existing divergence: a bounded prefix (`take_first`) of an infinite custom
// iterator (`Iter.custom`, the Fibonacci unfold below) diverges between the two
// lowerings, and the seed+step representation does NOT fix it: the divergence is
// an optimizer (spec_constr) miscompile, not a representation issue. The naive
// (`.none`) run yields the correct sequence 0,1,1,2,3,5,8,13; the optimized
// (`.wrappers`) run yields 0,0,0,0,0,0,0,0 (sum 0). Root cause, confirmed from
// the lowered LIR: the custom step correctly computes the advanced `next_seed`,
// but spec_constr rebuilds the successor iterator re-reading the ORIGINAL
// captured seed instead of `next_seed` (the seed's initial value is entry-known,
// so spec_constr treats a runtime-varying loop-carried field as loop-invariant
// and freezes it). The `keep_if` hang above is the same bug on a loop-carried
// iterator box. Repro kept commented out per the iterdiff convention above; on
// the current tree, executing it sends Debug lowering down a multi-minute
// spec-constr path before it can report the disagreement.
//
// test "iterdiff: infinite custom iterator bounded prefix agrees across inline modes" {
//     try expectSameObservationsAcrossInlineModes(
//         \\main : U64
//         \\main = {
//         \\    adv : ((U64, U64) -> Try((U64, (U64, U64)), [NoMore]))
//         \\    adv = |(a, b)| Try.Ok((a, (b, a + b)))
//         \\    fib_iter = Iter.custom((0.U64, 1.U64), Unknown, adv)
//         \\    var $sum = 0.U64
//         \\    for f in fib_iter.take_first(8) {
//         \\        dbg f
//         \\        $sum = $sum + f
//         \\    }
//         \\    dbg $sum
//         \\    $sum
//         \\}
//     );
// }

// Tier-one LIR identity. A bounded
// `list.iter().map(f).collect()` whose construction is statically known at its
// consuming loop fuses to the same generated-code loop as a hand-written `for`
// loop: no adapter dispatch, no per-element indirect call, one scalar loop that
// indexes the source list directly.
//
// The comparison is asserted per the principled relation rather than raw
// per-field equality across every field, because two field families cannot
// reach equality for reasons that are inherent to the compared programs, not
// missed fusion:
//
//   * Consumer allocation strategy. `.collect()` on a bounded iterator knows
//     the length up front, so it pre-sizes with `list_with_capacity` and writes
//     each element with the unchecked append. A hand-written `for` + `.append`
//     is `List.append`, which reserves incrementally (`list_reserve`) and stays
//     a per-element call. This is a consumer difference, not an iterator one, so
//     `list_with_capacity`/`list_reserve`/`list_append_unsafe`/`direct_call`
//     differ by design; the relation (collect pre-sizes, manual grows) is
//     asserted instead.
//   * Adapter carried box. `map` over a list carries a nested recursive-nominal
//     iterator (map wraps the list iterator), whose loop-exit re-materialization
//     needs a box (amplified to a nested pair here). The plain list-iterator
//     `for` loop carries no such box, so its exit re-materializes nothing.
test "iterdiff: tier-one map collect matches hand-written loop shape" {
    const allocator = std.testing.allocator;
    const iter_source =
        \\main : List(I64)
        \\main =
        \\    [1.I64, 2, 3, 4, 5, 6]
        \\        .iter()
        \\        .map(|n| n * 2)
        \\        .collect()
    ;
    const loop_source =
        \\main : List(I64)
        \\main = {
        \\    var $out = []
        \\    for n in [1.I64, 2, 3, 4, 5, 6] {
        \\        $out = $out.append(n * 2)
        \\    }
        \\    $out
        \\}
    ;

    // Append promotion rewrites qualifying checked appends into slack
    // diamonds, and qualification legitimately differs between these two
    // lowerings; this test compares the fused loop skeletons themselves, so
    // it runs with promotion off.
    var iter_lowered = try lowerModuleWithOptions(allocator, iter_source, .wrappers, .{ .promote_loop_appends = false });
    defer iter_lowered.deinit(allocator);
    var loop_lowered = try lowerModuleWithOptions(allocator, loop_source, .wrappers, .{ .promote_loop_appends = false });
    defer loop_lowered.deinit(allocator);

    const iter = &iter_lowered.lowered;
    const loop = &loop_lowered.lowered;

    // Tier-one guarantee: neither side dispatches through an erased adapter
    // callable. Both the fused pipeline and the fused hand-written loop drive a
    // first-order loop with no `Iter.next` indirection.
    inline for (.{ "erased_call_count", "packed_erased_fn_count" }) |field_name| {
        try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, iter, field_name));
        try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, loop, field_name));
    }

    // Same fused loop skeleton: one loop join, the same set of back/exit edges,
    // and one direct source-list index per element on each side.
    inline for (.{ "join_count", "jump_count", "list_get_unsafe_count" }) |field_name| {
        const iter_total = try reachableProcShapeFieldTotal(allocator, iter, field_name);
        const loop_total = try reachableProcShapeFieldTotal(allocator, loop, field_name);
        try std.testing.expectEqual(loop_total, iter_total);
    }

    // Consumer allocation strategy differs by design (see header): collect
    // pre-sizes, the manual loop grows.
    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, iter, "list_with_capacity_count") >= 1);
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, loop, "list_with_capacity_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, iter, "list_reserve_count"));
    try std.testing.expect(try reachableProcShapeFieldTotal(allocator, loop, "list_reserve_count") >= 1);

    // The adapter and list loop both carry their state as scalar values, so no
    // boxed iterator state remains reachable.
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, loop, "box_box_count"));
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, iter, "box_box_count"));
}

test "iter alloc static: list append append for-loop has no boxed iterator state" {
    const allocator = std.testing.allocator;
    const source =
        \\main : U64 -> Str
        \\main = |_seed| {
        \\    base_points = [
        \\        { x: 11.I64, y: 2.I64 }, { x: 13, y: 3 },
        \\        { x: 3, y: 5 }, { x: 11, y: 6 },
        \\        { x: 9, y: 8 }, { x: 5, y: 9 },
        \\        { x: 7, y: 10 }, { x: 5, y: 12 },
        \\    ].iter()
        \\    collision_points = base_points.append({ x: 2, y: 1 }).append({ x: 7, y: 1 })
        \\    var $sum = 0.I64
        \\    for { x, y } in collision_points {
        \\        $sum = $sum + x + y
        \\    }
        \\    if $sum == 130 { "ok" } else { "bad" }
        \\}
    ;

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .tag_reachability = true });
    defer optimized.deinit(allocator);

    try expectReachableProcShapeFieldEqual(allocator, &optimized.lowered, "box_box_count", 0);
    try expectNoReachableErasedCallableLowering(allocator, &optimized.lowered);
}

test "iter alloc static: list append append fold has no boxed iterator state" {
    const allocator = std.testing.allocator;
    const source =
        \\main : U64 -> Str
        \\main = |_seed| {
        \\    base_points = [
        \\        { x: 11.I64, y: 2.I64 }, { x: 13, y: 3 },
        \\        { x: 3, y: 5 }, { x: 11, y: 6 },
        \\        { x: 9, y: 8 }, { x: 5, y: 9 },
        \\        { x: 7, y: 10 }, { x: 5, y: 12 },
        \\    ].iter()
        \\    collision_points = base_points.append({ x: 2, y: 1 }).append({ x: 7, y: 1 })
        \\    sum = Iter.fold(collision_points, 0.I64, |acc, p| acc + p.x + p.y)
        \\    if sum == 130 { "ok" } else { "bad" }
        \\}
    ;

    var optimized = try lowerModuleWithOptions(allocator, source, .wrappers, .{ .tag_reachability = true });
    defer optimized.deinit(allocator);

    try expectReachableProcShapeFieldEqual(allocator, &optimized.lowered, "box_box_count", 0);
    try expectNoReachableErasedCallableLowering(allocator, &optimized.lowered);
}

// Slice H aliasing guard (refcount-exactness for opportunistic mutation).
//
// Slice H turns per-element reads of a loop-carried list into borrows anchored
// on the loop join parameter, dropping the retain/release pair those reads used
// to carry. Roc's in-place mutation is refcount-exact: `List.append` mutates
// its argument in place only when the list is uniquely owned. If the elision
// ever undercounted a shared list, an append would wrongly see it as unique and
// mutate shared data. These tests alias one list into two live consumers (one
// of which would mutate it in place if it looked unique) and assert the naive
// and optimized lowerings observe identical, unmutated values.
test "iterdiff: list aliased into an append and a loop stays unmutated across inline modes" {
    // `base` feeds both an append (a would-be in-place mutation) and a loop that
    // reads it per element (the Slice H borrow pattern). Because both consumers
    // are live, `base` is shared, so the append must copy it. The per-element
    // `dbg x`, the final `dbg base`, and `dbg grown` diverge between modes if the
    // shared list is ever mutated in place.
    try expectSameObservationsAcrossInlineModes(
        \\main : I64
        \\main = {
        \\    base : List(I64)
        \\    base = [10.I64, 20, 30]
        \\    grown : List(I64)
        \\    grown = base.append(40)
        \\    var $sum = 0.I64
        \\    for x in base.iter() {
        \\        dbg x
        \\        $sum = $sum + x
        \\    }
        \\    dbg base
        \\    dbg grown
        \\    dbg $sum
        \\    $sum
        \\}
    );
}

test "iterdiff: loop-carried list appended inside its own loop stays unmutated across inline modes" {
    // The list is the loop source (carried across the join and read per element
    // as a Slice H borrow) AND is appended inside the body. It is shared for the
    // whole loop, so each append must copy it; an in-place mutation of the
    // carried source would change later iterations and the final `dbg base`.
    try expectSameObservationsAcrossInlineModes(
        \\main : U64
        \\main = {
        \\    base : List(I64)
        \\    base = [1.I64, 2, 3]
        \\    var $out = []
        \\    for x in base.iter() {
        \\        with_x : List(I64)
        \\        with_x = base.append(x)
        \\        dbg with_x
        \\        $out = $out.append(List.len(with_x))
        \\    }
        \\    dbg base
        \\    dbg $out
        \\    List.len($out)
        \\}
    );
}

test "spec constr keeps a same-binder scalar distinct from a substituted aggregate" {
    // A source pattern binder is reused across every monomorphization of its
    // binding. Here `pair` (a tuple parameter the caller passes a known tuple to,
    // so call-pattern specialization substitutes it) and `scalar` (a runtime
    // `let` local left un-inlined by a non-substitutable value) deliberately
    // share one binder at two monomorphic types. Keying binder-scoped
    // substitutions by the binder alone resolves the scalar reference to the
    // substituted tuple, materializing a tuple directly inside the result tuple.
    // The layout-carrying identity must keep them distinct.
    const allocator = std.testing.allocator;
    var mono = MonoAst.Program.init(allocator);
    var mono_consumed = false;
    errdefer if (!mono_consumed) mono.deinit();

    const shared_binder: check.CheckedModule.PatternBinderId = @enumFromInt(7);

    const u32_ty = try mono.types.add(.{ .primitive = .u32 });
    const pair_span = try mono.types.addSpan(&.{ u32_ty, u32_ty });
    const pair_ty = try mono.types.add(.{ .tuple = pair_span });
    const worker_fn_ty = try mono.types.add(.{ .func = .{
        .args = try mono.types.addSpan(&.{pair_ty}),
        .ret = pair_ty,
    } });
    const worker_fn_id = try mono.addFn(.{
        .fn_def = undefined,
        .source_fn_ty = undefined,
        .source_fn_key = .{},
        .mono_fn_ty = worker_fn_ty,
    });

    const opaque_scalar = try mono.addImportedFn(.{ .shard = @enumFromInt(1), .fn_id = @enumFromInt(1) });

    const pair_local = try mono.addLocalWithBinder(@enumFromInt(1), pair_ty, shared_binder);
    const scalar_local = try mono.addLocalWithBinder(@enumFromInt(2), u32_ty, shared_binder);

    const scalar_value = try mono.addExpr(.{ .ty = u32_ty, .data = .{ .call_proc = .{
        .callee = MonoAst.importedProcCallee(opaque_scalar),
        .args = MonoAst.Span(MonoAst.ExprId).empty(),
    } } });
    const scalar_pat = try mono.addPat(.{ .ty = u32_ty, .data = .{ .bind = scalar_local } });

    const pair_ref = try mono.addExpr(.{ .ty = pair_ty, .data = .{ .local = pair_local } });
    const pair_first = try mono.addExpr(.{ .ty = u32_ty, .data = .{ .tuple_access = .{ .tuple = pair_ref, .elem_index = 0 } } });
    const scalar_ref = try mono.addExpr(.{ .ty = u32_ty, .data = .{ .local = scalar_local } });
    const result_pair = try mono.addExpr(.{ .ty = pair_ty, .data = .{ .tuple = try mono.addExprSpan(&.{ pair_first, scalar_ref }) } });
    const worker_body = try mono.addExpr(.{ .ty = pair_ty, .data = .{ .let_ = .{
        .bind = scalar_pat,
        .value = scalar_value,
        .rest = result_pair,
    } } });

    try mono.defs.append(allocator, .{
        .symbol = @enumFromInt(10),
        .fn_id = worker_fn_id,
        .args = try mono.addTypedLocalSpan(&.{.{ .local = pair_local, .ty = pair_ty }}),
        .body = .{ .roc = worker_body },
        .ret = pair_ty,
    });

    const lit_a = try mono.addExpr(.{ .ty = u32_ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(u128, 3)), .kind = .u128 } } });
    const lit_b = try mono.addExpr(.{ .ty = u32_ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(u128, 4)), .kind = .u128 } } });
    const call_arg = try mono.addExpr(.{ .ty = pair_ty, .data = .{ .tuple = try mono.addExprSpan(&.{ lit_a, lit_b }) } });
    const caller_body = try mono.addExpr(.{ .ty = pair_ty, .data = .{ .call_proc = .{
        .callee = MonoAst.localProcCallee(worker_fn_id),
        .args = try mono.addExprSpan(&.{call_arg}),
    } } });
    try mono.defs.append(allocator, .{
        .symbol = @enumFromInt(11),
        .args = MonoAst.Span(MonoAst.TypedLocal).empty(),
        .body = .{ .roc = caller_body },
        .ret = pair_ty,
    });

    var lifted = try postcheck.MonotypeLifted.Lift.run(allocator, mono);
    mono_consumed = true;
    defer lifted.deinit();

    try postcheck.MonotypeLifted.SpecConstr.run(allocator, &lifted);

    // The input program has no tuple nested directly inside another tuple, so a
    // nested tuple after specialization means the substituted aggregate leaked
    // into the scalar slot.
    for (lifted.exprsView()) |expr| {
        if (std.meta.activeTag(expr.data) != .tuple) continue;
        const items = expr.data.tuple;
        const tuple_items = lifted.exprSpan(items);
        for (0..tuple_items.len) |index| {
            const item = GuardedList.at(tuple_items, index);
            if (std.meta.activeTag(lifted.getExpr(item).data) == .tuple)
                return error.SubstitutedAggregateLeakedIntoScalar;
        }
    }
}

fn bareListIterCollectLoopIsScalar(shape: ProcShape) bool {
    return shape.join_count >= 1 and
        shape.max_join_param_count >= 5 and
        shape.list_get_unsafe_count >= 1 and
        shape.list_append_unsafe_count >= 1 and
        shape.erased_call_count == 0 and
        shape.direct_call_count == 0;
}

test "bare list iter collect carries scalar list state in the loop" {
    const allocator = std.testing.allocator;
    const source =
        \\main : () -> List(I64)
        \\main = || [1.I64, 2, 3].iter().collect()
    ;
    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    // The consumer loop carries the list-iter state as scalar loop variables
    // (length payload, list, index) plus the output list, and indexes the
    // list directly per element. No reachable proc dispatches through the
    // erased step callable and no per-element call remains.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, bareListIterCollectLoopIsScalar));
    try std.testing.expect(!try reachableIterCollectShape(allocator, &optimized.lowered, .generic));
    try std.testing.expect(!try reachableIterCollectShape(allocator, &optimized.lowered, .specialized));
    try expectNoReachableErasedCallableLowering(allocator, &optimized.lowered);
    // The list-iter carries its step closure inline by value, so the loop state
    // needs no boxed iterator state at all; the only allocation is the output
    // list itself.
    try std.testing.expectEqual(@as(usize, 0), try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "box_box_count"));
}

test "issue 10181 explicit Str interpolation suffix checks cleanly" {
    // Repro for https://github.com/roc-lang/roc/issues/10181
    const allocator = std.testing.allocator;
    const source =
        \\main! = |_| {
        \\    x = "world"
        \\    y = "hello ${x}".Str
        \\    Ok({})
        \\}
    ;

    const resources = try helpers.parseAndCanonicalizeProgramPublishedRootsWithBuiltin(
        allocator,
        .module,
        source,
        &.{},
        try sharedPrePublishedBuiltin(),
        null,
    );
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    try std.testing.expectEqual(@as(usize, 0), resources.checker.problems.problems.items.len);
}

const dispatch_boundary_source =
    \\Thing := [Val(Str)].{
    \\    to_str : Thing -> Str
    \\    to_str = |Thing.Val(s)| s
    \\}
    \\
    \\main : Str
    \\main = Thing.Val("hi").to_str()
;

test "dispatch evidence boundary validator accepts a published artifact" {
    const allocator = std.testing.allocator;
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, dispatch_boundary_source, &.{}, try sharedPrePublishedBuiltin());
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    try std.testing.expect(resources.checked_artifact.validateDispatchEvidence() == null);
}

test "custom literal field default owns its conversion root" {
    const allocator = std.testing.allocator;
    const source =
        \\MyNum := [Value(U64)].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Value(numeral.digits_before_pt().len()))
        \\}
        \\
        \\Label := [Label(Str)].{
        \\    from_quote : Str -> Try(Label, [BadQuotedBytes(Str)])
        \\    from_quote = |str| Ok(Label(str))
        \\}
        \\
        \\Config : { size : MyNum ?? 5, label : Label ?? "hi" }
        \\
        \\config : Config
        \\config = {}
        \\
        \\main = config.size
    ;

    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(
        allocator,
        .module,
        source,
        &.{},
        try sharedPrePublishedBuiltin(),
    );
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    var default_count: usize = 0;
    for (resources.checked_artifact.compile_time_roots.roots) |root| {
        if (root.kind != .field_default) continue;
        default_count += 1;
        try std.testing.expect(root.literalConversionKind() != null);
        const conversion = resources.checked_artifact.compile_time_roots.lookupNumeralRootByExpr(root.expr) orelse
            return error.TestUnexpectedResult;
        try std.testing.expectEqual(root.id, conversion.id);
    }
    try std.testing.expectEqual(@as(usize, 2), default_count);
}

test "dispatch evidence boundary validator rejects malformed specialization interface metadata" {
    const allocator = std.testing.allocator;
    const source =
        \\mk = |f| {
        \\    show = || f({}).map_err(|_| ShowFailed)
        \\    show
        \\}
        \\
        \\wrap = |f| mk(f)
        \\
        \\main : {} -> Try({}, [ShowFailed])
        \\main = |_| {
        \\    f : {} -> Try({}, [Empty])
        \\    f = |_| Ok({})
        \\    wrap(f)()
        \\}
    ;
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, source, &.{}, try sharedPrePublishedBuiltin());
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    const artifact = &resources.checked_artifact;
    const templates = &artifact.checked_procedure_templates;
    try std.testing.expect(artifact.validateDispatchEvidence() == null);
    try std.testing.expect(templates.dispatch_scopes.len > 0);
    try std.testing.expect(templates.specialization_interface_relations.len > 0);

    var template_index: ?usize = null;
    for (templates.templates, 0..) |template, i| {
        if (template.specialization_interface_relations.len > 0) {
            template_index = i;
            break;
        }
    }
    const raw_template = template_index orelse return error.TestUnexpectedResult;
    const saved_template_span = templates.templates[raw_template].specialization_interface_relations;
    templates.templates[raw_template].specialization_interface_relations.start = @intCast(templates.specialization_interface_relations.len);
    templates.templates[raw_template].specialization_interface_relations.len = 1;
    var failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.template_specialization_relations_out_of_bounds, failure.kind);
    templates.templates[raw_template].specialization_interface_relations = saved_template_span;

    const saved_parent = templates.dispatch_scopes[0].parent;
    templates.dispatch_scopes[0].parent = @enumFromInt(templates.dispatch_scopes.len);
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_scope_parent_invalid, failure.kind);
    templates.dispatch_scopes[0].parent = saved_parent;

    const saved_scheme_root = templates.dispatch_scopes[0].scheme_root;
    templates.dispatch_scopes[0].scheme_root = @enumFromInt(artifact.checked_types.payloadCount());
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_scope_scheme_root_out_of_bounds, failure.kind);
    templates.dispatch_scopes[0].scheme_root = saved_scheme_root;

    const saved_scope = templates.specialization_interface_relations[0].scope;
    templates.specialization_interface_relations[0].scope = .{ .generalized = @enumFromInt(templates.dispatch_scopes.len) };
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_relation_scope_out_of_bounds, failure.kind);
    templates.specialization_interface_relations[0].scope = saved_scope;

    const saved_relation_data = templates.specialization_interface_relations[0].data;
    templates.specialization_interface_relations[0].data = .{ .type_equality = .{
        .left = @enumFromInt(artifact.checked_types.payloadCount()),
        .right = templates.dispatch_scopes[0].scheme_root,
    } };
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_relation_type_out_of_bounds, failure.kind);
    templates.specialization_interface_relations[0].data = saved_relation_data;

    var call_index: ?usize = null;
    var direct_call_index: ?usize = null;
    var local_use_index: ?usize = null;
    for (templates.specialization_interface_relations, 0..) |relation, i| switch (relation.data) {
        .call => |call| {
            if (call_index == null) call_index = i;
            if (call.direct_target != null and direct_call_index == null) direct_call_index = i;
        },
        .local_proc_use => if (local_use_index == null) {
            local_use_index = i;
        },
        .type_equality, .procedure => {},
    };

    const raw_call = call_index orelse return error.TestUnexpectedResult;
    const saved_args = templates.specialization_interface_relations[raw_call].data.call.args;
    templates.specialization_interface_relations[raw_call].data.call.args = .{
        .start = @intCast(templates.specialization_interface_types.len),
        .len = 1,
    };
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_relation_call_args_out_of_bounds, failure.kind);
    templates.specialization_interface_relations[raw_call].data.call.args = saved_args;

    const raw_direct_call = direct_call_index orelse return error.TestUnexpectedResult;
    const saved_direct_target = templates.specialization_interface_relations[raw_direct_call].data.call.direct_target;
    templates.specialization_interface_relations[raw_direct_call].data.call.direct_target = @enumFromInt(artifact.resolved_value_refs.records.len);
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_relation_value_ref_out_of_bounds, failure.kind);
    templates.specialization_interface_relations[raw_direct_call].data.call.direct_target = saved_direct_target;

    var non_procedure_ref: ?check.CheckedArtifact.ResolvedValueRefId = null;
    for (artifact.resolved_value_refs.records, 0..) |record, i| {
        const ref_tag = std.meta.activeTag(record.ref);
        if (ref_tag != .local_proc and
            ref_tag != .top_level_proc and
            ref_tag != .imported_proc and
            ref_tag != .hosted_proc and
            ref_tag != .platform_required_proc and
            ref_tag != .promoted_top_level_proc)
        {
            non_procedure_ref = @enumFromInt(i);
            break;
        }
    }
    const invalid_procedure_ref = non_procedure_ref orelse return error.TestUnexpectedResult;
    templates.specialization_interface_relations[raw_direct_call].data.call.direct_target = invalid_procedure_ref;
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_relation_direct_target_invalid, failure.kind);
    templates.specialization_interface_relations[raw_direct_call].data.call.direct_target = saved_direct_target;

    const raw_local_use = local_use_index orelse return error.TestUnexpectedResult;
    const saved_local_ref = templates.specialization_interface_relations[raw_local_use].data.local_proc_use;
    templates.specialization_interface_relations[raw_local_use].data.local_proc_use = invalid_procedure_ref;
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_relation_local_proc_use_invalid, failure.kind);
    templates.specialization_interface_relations[raw_local_use].data.local_proc_use = saved_local_ref;

    const local_record = artifact.resolved_value_refs.records[@intFromEnum(saved_local_ref)].ref.local_proc;
    const local_scope = local_record.dispatch_scope orelse return error.TestUnexpectedResult;
    const raw_local_scope = @intFromEnum(local_scope);
    const saved_scope_expr = templates.dispatch_scopes[raw_local_scope].checked_expr;
    const next_expr = (@intFromEnum(saved_scope_expr) + 1) % artifact.checked_bodies.exprCount();
    templates.dispatch_scopes[raw_local_scope].checked_expr = @enumFromInt(next_expr);
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.specialization_relation_local_proc_use_invalid, failure.kind);
    templates.dispatch_scopes[raw_local_scope].checked_expr = saved_scope_expr;

    var path_param_span: ?@TypeOf(templates.templates[0].evidence_params) = null;
    for (templates.templates) |template| {
        const params = templates.evidenceParams(&template);
        for (params) |param| {
            if (param.path.len > 0) {
                path_param_span = template.evidence_params;
                break;
            }
        }
        if (path_param_span != null) break;
    }
    const saved_scope_params = templates.dispatch_scopes[raw_local_scope].evidence_params;
    templates.dispatch_scopes[raw_local_scope].evidence_params = path_param_span orelse return error.TestUnexpectedResult;
    failure = artifact.validateDispatchEvidence() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.evidence_param_path_diverges_from_checked_type, failure.kind);
    templates.dispatch_scopes[raw_local_scope].evidence_params = saved_scope_params;

    try std.testing.expect(artifact.validateDispatchEvidence() == null);
}

test "dispatch evidence boundary validator rejects non-normalized and malformed paths" {
    const allocator = std.testing.allocator;
    const source =
        \\helper : a -> Str where [a.to_str : a -> Str]
        \\helper = |_x| "ok"
        \\
        \\main : Str
        \\main = helper("hi")
    ;
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, source, &.{}, try sharedPrePublishedBuiltin());
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    const paths = resources.checked_artifact.checked_procedure_templates.evidence_param_paths;
    try std.testing.expect(paths.len > 0);

    // Raw discriminant 8 is the retired checked-store `record_ext` step.
    paths[0].kind = 8;
    var failure = resources.checked_artifact.validateDispatchEvidence() orelse
        return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.evidence_param_path_invalid_kind, failure.kind);

    // A tag label must be immediately paired with a payload-index step.
    paths[0].kind = 9;
    failure = resources.checked_artifact.validateDispatchEvidence() orelse
        return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.evidence_param_path_invalid_shape, failure.kind);

    // A well-formed selector must still resolve over the checked callable.
    paths[0].kind = 0;
    paths[0].data = std.math.maxInt(u32);
    failure = resources.checked_artifact.validateDispatchEvidence() orelse
        return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.evidence_param_path_diverges_from_checked_type, failure.kind);
}

test "dispatch evidence boundary validator reports a removed dispatch plan by expression" {
    const allocator = std.testing.allocator;
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, dispatch_boundary_source, &.{}, try sharedPrePublishedBuiltin());
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    var removed: ?check.CheckedArtifact.CheckedExprId = null;
    for (resources.checked_artifact.checked_bodies.stored_exprs.items) |*expr| {
        if (std.meta.activeTag(expr.data) == .dispatch_call and expr.data.dispatch_call != null) {
            expr.data = .{ .dispatch_call = null };
            removed = expr.id;
            break;
        }
    }
    try std.testing.expect(removed != null);

    const failure = resources.checked_artifact.validateDispatchEvidence() orelse
        return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.dispatch_expr_missing_plan, failure.kind);
    try std.testing.expectEqual(removed.?, failure.expr.?);
}

test "dispatch evidence boundary validator names the method of a dangling evidence node" {
    const allocator = std.testing.allocator;
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, dispatch_boundary_source, &.{}, try sharedPrePublishedBuiltin());
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    const table = &resources.checked_artifact.static_dispatch_plans;
    var corrupted_method: ?[]const u8 = null;
    for (table.plans) |*plan| {
        switch (plan.resolution) {
            .direct_closed => {
                plan.resolution = .{ .direct_closed = .{ .evidence = @enumFromInt(table.evidence_nodes.len) } };
                corrupted_method = resources.checked_artifact.canonical_names.methodNameText(plan.method);
                break;
            },
            .direct_parametric => {
                plan.resolution = .{ .direct_parametric = .{ .evidence = @enumFromInt(table.evidence_nodes.len) } };
                corrupted_method = resources.checked_artifact.canonical_names.methodNameText(plan.method);
                break;
            },
            .direct_pending,
            .evidence_dependent,
            .structural,
            .checked_error,
            .@"unreachable",
            => {},
        }
    }
    try std.testing.expect(corrupted_method != null);

    const failure = resources.checked_artifact.validateDispatchEvidence() orelse
        return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.plan_evidence_node_out_of_bounds, failure.kind);
    const named_method = resources.checked_artifact.canonical_names.methodNameText(failure.method orelse return error.TestUnexpectedResult);
    try std.testing.expectEqualStrings(corrupted_method.?, named_method);
}

test "dispatch evidence boundary validator reports a site-evidence key outside the body store" {
    const allocator = std.testing.allocator;
    // A where-constrained helper instantiated at a concrete type gives the
    // instantiation site a site-evidence entry to corrupt.
    const source =
        \\Thing := [Val(Str)].{
        \\    to_str : Thing -> Str
        \\    to_str = |Thing.Val(s)| s
        \\}
        \\
        \\helper : a -> Str where [a.to_str : a -> Str]
        \\helper = |x| x.to_str()
        \\
        \\main : Str
        \\main = helper(Thing.Val("hi"))
    ;
    var resources = try helpers.parseAndCanonicalizeProgramWithBuiltin(allocator, .module, source, &.{}, try sharedPrePublishedBuiltin());
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    const table = &resources.checked_artifact.static_dispatch_plans;
    try std.testing.expect(table.site_evidence.len > 0);
    table.site_evidence[0].key = @intCast(resources.checked_artifact.checked_bodies.exprCount());

    const failure = resources.checked_artifact.validateDispatchEvidence() orelse
        return error.TestUnexpectedResult;
    try std.testing.expectEqual(check.CheckedArtifact.DispatchEvidenceFailure.Kind.site_evidence_key_out_of_bounds, failure.kind);
    try std.testing.expectEqual(@as(?u32, 0), failure.index);
}

test "compiler-generated dispatch classes lower via checked evidence" {
    const allocator = std.testing.allocator;
    // One program exercising every compiler-generated dispatch class served
    // by the component-lookup seam: iterator `for` dispatch, structural
    // record equality dispatching a nominal component's own `is_eq`,
    // `Str.inspect` through a custom `to_inspect`, and parser-format
    // synthesis with builtin Set helpers (JSON parse of a Set field).
    // Debug-mode lowering asserts dispatch-evidence totality by invariant
    // throughout, and the evaluated output asserts every class resolved to
    // the RIGHT target, not merely some lowerable one.
    const source =
        \\Speed := [Mph(U64)].{
        \\    is_eq : Speed, Speed -> Bool
        \\    is_eq = |Speed.Mph(a), Speed.Mph(b)| a == b
        \\    to_inspect : Speed -> Str
        \\    to_inspect = |Speed.Mph(mph)| Str.inspect(mph)
        \\}
        \\
        \\main : Bool
        \\main = {
        \\    var $sum = 0.U64
        \\    for item in [1.U64, 2.U64, 3.U64] {
        \\        $sum = $sum + item
        \\    }
        \\    lhs = { speed: Speed.Mph($sum), label: "total" }
        \\    rhs = { speed: Speed.Mph(6), label: "total" }
        \\    other = { speed: Speed.Mph(7), label: "total" }
        \\    parsed : Try({ names : Set(Str) }, [InvalidJson(Str), MissingRequiredField(Str)])
        \\    parsed = Json.parse("{ \"names\": [\"a\", \"b\"] }")
        \\    parsed_count = match parsed {
        \\        Ok(rec) => rec.names.len()
        \\        Err(_) => 0
        \\    }
        \\    lhs == rhs and lhs != other and parsed_count == 2 and Str.inspect(lhs.speed) == "6"
        \\}
    ;

    var compiled = try helpers.compileInspectedProgramForTargetWithBuiltin(allocator, std.testing.io, .module, source, &.{}, .native, try sharedPrePublishedBuiltin(), null, .lss);
    defer compiled.deinit(allocator);

    // The program must check cleanly: a reported problem would resolve the
    // dispatch plans as checked errors and crash-lower the very classes this
    // test exists to exercise.
    try std.testing.expectEqual(@as(usize, 0), compiled.resources.checker.problems.problems.items.len);

    const output = try helpers.lirInterpreterInspectedStr(allocator, &compiled.lowered);
    defer allocator.free(output);
    try std.testing.expectEqualStrings("True", output);
}

// Repro for https://github.com/roc-lang/roc/issues/10301: a list produced by an
// opaque effectful expression and iterated by `for` must scalarize into a raw
// indexed loop in the root proc, leaving no per-element iterator-step calls in
// any reachable proc.
test "issue 10301 for-loop over effect-produced list scalarizes" {
    const allocator = std.testing.allocator;
    const source =
        \\produce : U64 -> List(U64)
        \\produce = |n| {
        \\    dbg "produce"
        \\    [n, 2, 3]
        \\}
        \\
        \\main : U64
        \\main = {
        \\    var $sum = 0
        \\    for byte in produce(1) {
        \\        $sum = $sum * 31 + byte
        \\    }
        \\    $sum
        \\}
    ;
    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    const root_shape = try collectProcShape(allocator, &optimized.lowered, try rootProc(&optimized.lowered));
    const reachable_total = try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_get_unsafe_count");
    // Every raw list access lives in the root: the iterator scalarized into a
    // direct indexed loop, and no per-element step proc remains reachable.
    try std.testing.expect(root_shape.list_get_unsafe_count >= 1);
    try std.testing.expectEqual(root_shape.list_get_unsafe_count, reachable_total);
}

test "iter alloc static: runtime list for-loop has no boxed iterator state" {
    const allocator = std.testing.allocator;
    const source =
        \\main : List(U8) -> U64
        \\main = |bytes| {
        \\    var $sum = 0.U64
        \\    for byte in bytes {
        \\        $sum = $sum + byte.to_u64()
        \\    }
        \\    $sum
        \\}
    ;
    var lowered = try lowerModuleWithOptions(allocator, source, .none, .{ .tag_reachability = true });
    defer lowered.deinit(allocator);

    try expectLoweredIterStateHasNoBoxesOrErasedCallables(allocator, &lowered.lowered);
}

// Repro for https://github.com/roc-lang/roc/issues/10340: the fold must
// scalarize into one self-contained raw-indexed loop in the root proc, without
// peeling the first step and calling a separate fused worker for the rest.
test "issue 10340 fold over effect-produced list scalarizes in root" {
    const allocator = std.testing.allocator;
    const source =
        \\produce : U64 -> List(U64)
        \\produce = |n| {
        \\    dbg 7.U64
        \\    [n, 2, 3]
        \\}
        \\
        \\main : U64
        \\main = Iter.fold(produce(1).iter(), 0, |acc, byte| acc * 31 + byte)
    ;
    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    const root_shape = try collectProcShape(allocator, &optimized.lowered, try rootProc(&optimized.lowered));
    const reachable_total = try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_get_unsafe_count");
    try std.testing.expect(root_shape.list_get_unsafe_count >= 1);
    try std.testing.expect(root_shape.join_count >= 1);
    try std.testing.expectEqual(@as(usize, 0), root_shape.direct_call_count);
    try std.testing.expectEqual(root_shape.list_get_unsafe_count, reachable_total);

    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    var interpreter = try eval.Interpreter.init(
        allocator,
        &optimized.lowered.lir_result.store,
        &optimized.lowered.lir_result.layouts,
        runtime_env.get_ops(),
        .preserve,
    );
    defer interpreter.deinit();
    const result = try interpreter.eval(.{ .proc_id = try rootProc(&optimized.lowered) });
    switch (result) {
        .value => |value| try std.testing.expectEqual(@as(u64, 1026), value.read(u64)),
    }
}

// Repro for https://github.com/roc-lang/roc/issues/10317: a loop-carried
// variable reassigned under a branch must keep resolving through its binder
// identity after the carried slot is rebound to a fresh param; a dangling
// reference would surface as a phantom root argument.
test "issue 10317 loop-carried reassignment keeps root arg count" {
    const allocator = std.testing.allocator;
    const source =
        \\main : I64
        \\main = {
        \\    var $x = 0
        \\    var $y = 0
        \\    for flag in [Bool.False] {
        \\        $y = if flag {
        \\            $x = 1
        \\            0
        \\        } else {
        \\            0
        \\        }
        \\    }
        \\    $x + $y
        \\}
    ;
    var dev = try lowerModule(allocator, source, .none);
    defer dev.deinit(allocator);
    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    // Comparing full argument layouts (not just counts) rules out a phantom
    // argument replacing a legitimate one at the same arity.
    const dev_layouts = try mainProcArgLayouts(allocator, &dev.lowered);
    defer allocator.free(dev_layouts);
    const opt_layouts = try mainProcArgLayouts(allocator, &optimized.lowered);
    defer allocator.free(opt_layouts);
    try std.testing.expectEqualSlices(LayoutIdx, dev_layouts, opt_layouts);
}

test "iterdiff: issue 10317 branch-reassigned carry agrees across inline modes" {
    try expectSameObservationsAcrossInlineModes(
        \\main : I64
        \\main = {
        \\    var $x = 0
        \\    var $y = 0
        \\    for flag in [Bool.False, Bool.True, Bool.False] {
        \\        $y = if flag {
        \\            $x = $x + 1
        \\            $x
        \\        } else {
        \\            $y
        \\        }
        \\    }
        \\    dbg $x
        \\    dbg $y
        \\    $x * 10 + $y
        \\}
    );
}

test "iterdiff: match-reassigned carry agrees across inline modes" {
    try expectSameObservationsAcrossInlineModes(
        \\main : I64
        \\main = {
        \\    var $x = 0
        \\    var $y = 0
        \\    for v in [1.I64, 0, 2, 0] {
        \\        $y = match v {
        \\            0 => $y
        \\            n => {
        \\                $x = $x + n
        \\                $x
        \\            }
        \\        }
        \\    }
        \\    dbg $x
        \\    dbg $y
        \\    $x * 10 + $y
        \\}
    );
}

test "sequential effect-produced for-loops both scalarize" {
    const allocator = std.testing.allocator;
    const source =
        \\produce : U64 -> List(U64)
        \\produce = |n| {
        \\    dbg 7
        \\    [n, 2, 3]
        \\}
        \\
        \\main : U64
        \\main = {
        \\    var $a = 0.U64
        \\    for x in produce(1) {
        \\        $a = $a + x
        \\    }
        \\    var $b = 0.U64
        \\    for y in produce(4) {
        \\        $b = $b * 2 + y
        \\    }
        \\    $a + $b
        \\}
    ;
    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    const root_shape = try collectProcShape(allocator, &optimized.lowered, try rootProc(&optimized.lowered));
    const reachable_total = try reachableProcShapeFieldTotal(allocator, &optimized.lowered, "list_get_unsafe_count");
    // Both loops fuse: each contributes its raw indexed access in the root and
    // no step proc remains reachable for either.
    try std.testing.expect(root_shape.list_get_unsafe_count >= 2);
    try std.testing.expectEqual(root_shape.list_get_unsafe_count, reachable_total);
}

test "issue 10301 producer effect runs exactly once when the loop fuses" {
    try expectOptimizedDbgEvents(
        \\produce : U64 -> List(U64)
        \\produce = |n| {
        \\    dbg 7.U64
        \\    [n, 2, 3]
        \\}
        \\
        \\main : {}
        \\main = {
        \\    var $sum = 0.U64
        \\    for byte in produce(1) {
        \\        $sum = $sum * 31 + byte
        \\    }
        \\    dbg $sum
        \\    {}
        \\}
    ,
        &.{ "7", "1026" },
    );
}

test "iterdiff: effect-produced fold agrees across inline modes" {
    try expectSameObservationsAcrossInlineModes(
        \\produce : U64 -> List(U64)
        \\produce = |n| {
        \\    dbg 7.U64
        \\    [n, 2, 3]
        \\}
        \\
        \\main : U64
        \\main = {
        \\    total = Iter.fold(produce(1).iter(), 0, |acc, byte| acc * 31 + byte)
        \\    dbg total
        \\    total
        \\}
    );
}

test "iterdiff: effect-produced for-loop agrees across inline modes" {
    try expectSameObservationsAcrossInlineModes(
        \\produce : U64 -> List(U64)
        \\produce = |n| {
        \\    dbg 7
        \\    [n, 2, 3]
        \\}
        \\
        \\main : U64
        \\main = {
        \\    var $sum = 0.U64
        \\    for byte in produce(1) {
        \\        dbg byte
        \\        $sum = $sum * 31 + byte
        \\    }
        \\    dbg $sum
        \\    $sum
        \\}
    );
}

test "iterdiff: two effect producers keep source order across inline modes" {
    try expectSameObservationsAcrossInlineModes(
        \\produce : U64, U64 -> List(U64)
        \\produce = |label, n| {
        \\    dbg label
        \\    [n, 2, 3]
        \\}
        \\
        \\combine : List(U64), List(U64) -> U64
        \\combine = |xs, ys| {
        \\    var $sum = 0.U64
        \\    for x in xs {
        \\        $sum = $sum + x
        \\    }
        \\    for y in ys {
        \\        $sum = $sum * 2 + y
        \\    }
        \\    $sum
        \\}
        \\
        \\main : U64
        \\main = combine(produce(1, 10), produce(2, 20))
    );
}

test "iterdiff: conditional effect producer stays conditional across inline modes" {
    try expectSameObservationsAcrossInlineModes(
        \\produce : U64 -> List(U64)
        \\produce = |n| {
        \\    dbg n
        \\    [n, 2, 3]
        \\}
        \\
        \\pick : Bool -> U64
        \\pick = |flag| {
        \\    xs = if flag { produce(1) } else { [] }
        \\    var $sum = 0.U64
        \\    for x in xs {
        \\        $sum = $sum + x
        \\    }
        \\    $sum
        \\}
        \\
        \\main : U64
        \\main = pick(Bool.True) + pick(Bool.False)
    );
}

// Repro for https://github.com/roc-lang/roc/issues/10253: the recursive call
// must carry the current position, 1, into the next iteration's `prev_len`.
test "issue 10253 optimized tail recursion preserves the previous scalar argument" {
    try expectOptimizedDbgEvents(
        \\go : U64, List(U64), U64, Bool -> U64
        \\go = |pos, heads, prev_len, _pending| {
        \\    heads2 = heads.set(0, 7) ?? []
        \\    cur = if pos != 0 { pos } else { 0 }
        \\    if prev_len != 0 {
        \\        prev_len
        \\    } else {
        \\        go(pos + 1, heads2, cur, Bool.False)
        \\    }
        \\}
        \\
        \\main : U64 -> {}
        \\main = |zero| {
        \\    dbg go(zero + 1, [], 0, Bool.False)
        \\    {}
        \\}
    ,
        &.{"1"},
    );
}

/// How many distinct fields of `record` are read before the first call in the
/// proc, and how many are read overall. A record update evaluates its own
/// field expressions as calls; spread-carried fields are read out of the base
/// before those run, so an update that carries any spread field reads more
/// than one field ahead of its first call. Leaving a spread read after a call
/// keeps the base live across whatever that call does to a collection read out
/// of it, which is what forces the copy path in issue 10426.
fn recordFieldReadCounts(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
    record: LIR.LocalId,
) TestError!struct { before_first_call: usize, total: usize } {
    const store = &lowered.lir_result.store;
    const proc = store.getProcSpec(proc_id);
    const body = proc.body orelse return .{ .before_first_call = 0, .total = 0 };

    var aliases = collections.DenseMap(LIR.LocalId, void).init(allocator);
    defer aliases.deinit();
    try aliases.put(record, {});

    var before = std.AutoHashMap(u32, void).init(allocator);
    defer before.deinit();
    var total = std.AutoHashMap(u32, void).init(allocator);
    defer total.deinit();

    // Straight-line walk from the entry: the reads in question are all in the
    // proc's entry chain, and stopping at the first branch keeps the count
    // unambiguous.
    var cursor = body;
    var seen_call = false;
    var steps: usize = 0;
    while (steps < 4096) : (steps += 1) {
        switch (store.getCFStmt(cursor)) {
            .assign_ref => |stmt| {
                switch (stmt.op) {
                    .local => |src| if (aliases.contains(src)) try aliases.put(stmt.target, {}),
                    .field => |ref| if (aliases.contains(ref.source)) {
                        try total.put(ref.field_idx, {});
                        if (!seen_call) try before.put(ref.field_idx, {});
                    },
                    .discriminant,
                    .tag_payload,
                    .tag_payload_struct,
                    .list_reinterpret,
                    .nominal,
                    => {},
                }
                cursor = stmt.next;
            },
            inline .assign_call, .assign_call_dict => |stmt| {
                seen_call = true;
                cursor = stmt.next;
            },
            .assign_low_level => |stmt| {
                seen_call = true;
                cursor = stmt.next;
            },
            inline .assign_literal, .init_uninitialized, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| {
                cursor = stmt.next;
            },
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .boxy_tag_match,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            => break,
        }
    }
    return .{ .before_first_call = before.count(), .total = total.count() };
}

// Issue 10426: with several refcounted fields, a record update wrote in place
// only for the field whose read happened to come last -- canonical field
// order, so whichever sorted last, and nothing at all when a non-refcounted
// field sorted after them. Every other field copied its whole collection.
// Spread-carried reads now bind before the update's own field expressions, so
// the base's last use precedes the mutation for every field.
test "issue 10426 record update reads spread fields before the mutation" {
    const allocator = std.testing.allocator;
    const source =
        \\Model : { count : I64, other : List(I64), rows : List(I64) }
        \\
        \\bump_other : Model -> Model
        \\bump_other = |m| { ..m, count: m.count + 1, other: List.set(m.other, 0, 7) ?? [] }
        \\
        \\bump_rows : Model -> Model
        \\bump_rows = |m| { ..m, count: m.count + 1, rows: List.set(m.rows, 0, 7) ?? [] }
        \\
        \\main : I64
        \\main = {
        \\    m0 = { count: 0, other: List.repeat(1, 8), rows: List.repeat(2, 8) }
        \\    a = bump_rows(bump_other(m0))
        \\    a.count + (List.get(a.other, 0) ?? 0) + (List.get(a.rows, 0) ?? 0)
        \\}
    ;

    var lowered = try lowerModule(allocator, source, .none);
    defer lowered.deinit(allocator);

    const store = &lowered.lowered.lir_result.store;
    var checked_any = false;
    for (0..store.procSpecCount()) |index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const proc = store.getProcSpec(proc_id);
        const args = store.getLocalSpan(proc.args);
        if (GuardedList.borrowLen(args) != 1) continue;
        const counts = try recordFieldReadCounts(allocator, &lowered.lowered, proc_id, GuardedList.at(args, 0));
        // The update procs read the spread field and `count` in their entry
        // chain, before the branch the mutated field's expression builds;
        // anything reading fewer than two fields is unrelated.
        if (counts.total < 2) continue;
        checked_any = true;
        // Both the spread field and the explicit `count` read precede the
        // first call; only the mutated field's read may follow it.
        try std.testing.expect(counts.before_first_call >= 2);
    }
    try std.testing.expect(checked_any);
}

// Counts incref statements whose value is the target of a `ref.field` read
// (or a pure alias of one) anywhere in the proc. Field takes hand such reads
// the container's stored unit, so a take-covered read pays no retain.
fn fieldReadRetainCount(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
) TestError!usize {
    const store = &lowered.lir_result.store;
    const proc = store.getProcSpec(proc_id);
    const body = proc.body orelse return 0;

    var read_targets = collections.DenseMap(LIR.LocalId, void).init(allocator);
    defer read_targets.deinit();
    var retained = collections.DenseMap(LIR.LocalId, void).init(allocator);
    defer retained.deinit();
    var visited = std.AutoHashMap(u32, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(allocator);

    // Two sweeps so alias edges and increfs seen before their read resolve:
    // first collect read targets and their alias closure, then count.
    for (0..2) |sweep| {
        visited.clearRetainingCapacity();
        stack.clearRetainingCapacity();
        try stack.append(allocator, body);
        while (stack.pop()) |cursor| {
            const seen = try visited.getOrPut(@intFromEnum(cursor));
            if (seen.found_existing) continue;
            switch (store.getCFStmt(cursor)) {
                .assign_ref => |stmt| {
                    switch (stmt.op) {
                        .field => try read_targets.put(stmt.target, {}),
                        .local => |src| if (read_targets.contains(src)) {
                            try read_targets.put(stmt.target, {});
                        },
                        .discriminant,
                        .tag_payload,
                        .tag_payload_struct,
                        .list_reinterpret,
                        .nominal,
                        => {},
                    }
                    try stack.append(allocator, stmt.next);
                },
                .incref => |stmt| {
                    if (sweep == 1 and read_targets.contains(stmt.value)) {
                        try retained.put(stmt.value, {});
                    }
                    try stack.append(allocator, stmt.next);
                },
                inline .init_uninitialized, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .decref, .decref_if_initialized, .free => |stmt| {
                    try stack.append(allocator, stmt.next);
                },
                .switch_stmt => |stmt| {
                    const branches = store.getCFSwitchBranches(stmt.branches);
                    for (0..GuardedList.borrowLen(branches)) |i| {
                        try stack.append(allocator, GuardedList.at(branches, i).body);
                    }
                    try stack.append(allocator, stmt.default_branch);
                    if (stmt.continuation) |continuation| try stack.append(allocator, continuation);
                },
                .switch_initialized_payload => |stmt| {
                    try stack.append(allocator, stmt.initialized_branch);
                    try stack.append(allocator, stmt.uninitialized_branch);
                },
                .str_match => |stmt| {
                    try stack.append(allocator, stmt.on_match);
                    try stack.append(allocator, stmt.on_miss);
                },
                .str_match_set => |stmt| {
                    const arms = store.getStrMatchArms(stmt.arms);
                    for (0..GuardedList.borrowLen(arms)) |i| {
                        try stack.append(allocator, GuardedList.at(arms, i).on_match);
                    }
                    try stack.append(allocator, stmt.on_miss);
                },
                .boxy_tag_match => |stmt| {
                    try stack.append(allocator, stmt.on_match);
                    try stack.append(allocator, stmt.on_miss);
                },
                .join => |stmt| {
                    try stack.append(allocator, stmt.body);
                    try stack.append(allocator, stmt.remainder);
                },
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                .jump,
                .ret,
                .crash,
                => {},
            }
        }
    }
    return retained.count();
}

// A locally built record whose fields are read once each and then dies is
// dismantled by field takes: every read on the record's spine keeps the
// record's stored unit instead of paying a retain, so the update's mutation
// sees a unique collection and writes in place. Both records here qualify,
// leaving no retained field read anywhere in the program.
test "field takes drop the field-read retains of dying local records" {
    const allocator = std.testing.allocator;
    const source =
        \\main : I64
        \\main = {
        \\    m = { count: 0.I64, other: List.repeat(1.I64, 8), rows: List.repeat(2.I64, 8) }
        \\    a = { ..m, count: m.count + 1, rows: List.set(m.rows, 0, 7) ?? [] }
        \\    a.count + (List.get(a.other, 0) ?? 0) + (List.get(a.rows, 0) ?? 0)
        \\}
    ;

    var lowered = try lowerModule(allocator, source, .none);
    defer lowered.deinit(allocator);

    const store = &lowered.lowered.lir_result.store;
    var root_retained: ?usize = null;
    for (0..store.procSpecCount()) |index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const proc = store.getProcSpec(proc_id);
        const args = store.getLocalSpan(proc.args);
        if (GuardedList.borrowLen(args) != 0) continue;
        if (proc.body == null) continue;
        const retained = try fieldReadRetainCount(allocator, &lowered.lowered, proc_id);
        root_retained = (root_retained orelse 0) + retained;
    }
    // m's `count`/`other`/`rows` reads and a's `count`/`other` reads are all
    // takes; only a's `rows` read may retain.
    try std.testing.expect(root_retained != null);
    try std.testing.expectEqual(@as(usize, 0), root_retained.?);
}

// A field read placed after an if-diamond still takes: every branch of the
// lowered switch falls straight through to its shared continuation, so the
// read past the rejoin runs exactly once on every path and may consume the
// dying record's stored unit for its field.
test "field takes cross a fall-through branch diamond" {
    const allocator = std.testing.allocator;
    const source =
        \\main : I64
        \\main = {
        \\    m = { flag: 3.I64, rows: List.repeat(2.I64, 8) }
        \\    bump = if m.flag > 0 { 1.I64 } else { 2 }
        \\    r = List.set(m.rows, 0, bump) ?? []
        \\    (List.get(r, 0) ?? 0) + bump
        \\}
    ;

    var lowered = try lowerModule(allocator, source, .none);
    defer lowered.deinit(allocator);

    const store = &lowered.lowered.lir_result.store;
    var root_retained: ?usize = null;
    for (0..store.procSpecCount()) |index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const proc = store.getProcSpec(proc_id);
        const args = store.getLocalSpan(proc.args);
        if (GuardedList.borrowLen(args) != 0) continue;
        if (proc.body == null) continue;
        const retained = try fieldReadRetainCount(allocator, &lowered.lowered, proc_id);
        root_retained = (root_retained orelse 0) + retained;
    }
    try std.testing.expect(root_retained != null);
    try std.testing.expectEqual(@as(usize, 0), root_retained.?);
}

// A field consumed on every arm of a branch—here List.set's success arm and
// the `??` fallback arm—takes on each path: the paths are exclusive, each
// takes exactly once, and the residual is the same wherever the record dies.
test "field takes split across the arms of a branch" {
    const allocator = std.testing.allocator;
    const source =
        \\main : I64
        \\main = {
        \\    m = { rows: List.repeat(2.I64, 8), n: 3.I64 }
        \\    r = List.set(m.rows, 9, 7) ?? m.rows
        \\    (List.get(r, 0) ?? 0) + m.n
        \\}
    ;

    var lowered = try lowerModule(allocator, source, .wrappers);
    defer lowered.deinit(allocator);

    const store = &lowered.lowered.lir_result.store;
    var root_retained: ?usize = null;
    for (0..store.procSpecCount()) |index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const proc = store.getProcSpec(proc_id);
        const args = store.getLocalSpan(proc.args);
        if (GuardedList.borrowLen(args) != 0) continue;
        if (proc.body == null) continue;
        const retained = try fieldReadRetainCount(allocator, &lowered.lowered, proc_id);
        root_retained = (root_retained orelse 0) + retained;
    }
    try std.testing.expect(root_retained != null);
    try std.testing.expectEqual(@as(usize, 0), root_retained.?);
}

// A record of lists updated through a helper function stays in place: the
// call site demands a mode-specialized variant whose owned parameter lets the
// field reads take, so no emitted variant both mutates a list and retains a
// field read. This is the record-state-through-a-call shape hash-table
// updates lower to.
test "owned variants take a helper parameter's fields at the call" {
    const allocator = std.testing.allocator;
    const source =
        \\step : { a : List(I64), b : List(I64) }, U64, I64 -> { s : { a : List(I64), b : List(I64) }, prev : I64 }
        \\step = |st, i, v| {
        \\    prev = List.get(st.a, i) ?? 0
        \\    s1 = { a: List.set(st.a, i, v) ?? st.a, b: st.b }
        \\    s2 = { ..s1, b: List.set(s1.b, i, prev) ?? s1.b }
        \\    { s: s2, prev }
        \\}
        \\
        \\main : I64
        \\main = {
        \\    var $st = { a: List.repeat(0.I64, 8), b: List.repeat(0.I64, 8) }
        \\    var $i = 0.U64
        \\    var $v = 5.I64
        \\    var $acc = 0.I64
        \\    while $i < 8 {
        \\        r = step($st, $i, $v)
        \\        $st = r.s
        \\        $acc = $acc + r.prev
        \\        $v = $v + 1
        \\        $i = $i + 1
        \\    }
        \\    $acc + (List.get($st.a, 3) ?? 0) + (List.get($st.b, 2) ?? 0)
        \\}
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    const store = &optimized.lowered.lir_result.store;
    var mutating_retain_free: usize = 0;
    for (0..store.procSpecCount()) |index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const proc = store.getProcSpec(proc_id);
        if (proc.body == null) continue;
        if (!procContainsListSet(store, proc_id)) continue;
        const retained = try fieldReadRetainCount(allocator, &optimized.lowered, proc_id);
        if (retained == 0) mutating_retain_free += 1;
    }
    // At least the specialized variant of `step` mutates without retaining.
    try std.testing.expect(mutating_retain_free >= 1);

    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    var interpreter = try eval.Interpreter.init(
        allocator,
        &optimized.lowered.lir_result.store,
        &optimized.lowered.lir_result.layouts,
        runtime_env.get_ops(),
        .preserve,
    );
    defer interpreter.deinit();

    const result = try interpreter.eval(.{ .proc_id = try rootProc(&optimized.lowered) });
    switch (result) {
        .value => |value| try std.testing.expectEqual(@as(i64, 8), value.read(i64)),
    }
}

fn procContainsListSet(store: *const lir.LirStore, proc_id: LIR.LirProcSpecId) bool {
    const proc = store.getProcSpec(proc_id);
    const body = proc.body orelse return false;
    var cursor_stack: [256]LIR.CFStmtId = undefined;
    var top: usize = 0;
    cursor_stack[top] = body;
    top += 1;
    var seen = std.bit_set.ArrayBitSet(usize, 1 << 20).initEmpty();
    while (top > 0) {
        top -= 1;
        const cursor = cursor_stack[top];
        if (seen.isSet(@intFromEnum(cursor))) continue;
        seen.set(@intFromEnum(cursor));
        switch (store.getCFStmt(cursor)) {
            .assign_low_level => |stmt| {
                if (stmt.op == .list_set) return true;
                if (top < cursor_stack.len) {
                    cursor_stack[top] = stmt.next;
                    top += 1;
                }
            },
            inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| {
                if (top < cursor_stack.len) {
                    cursor_stack[top] = stmt.next;
                    top += 1;
                }
            },
            .join => |stmt| {
                if (top + 1 < cursor_stack.len) {
                    cursor_stack[top] = stmt.body;
                    cursor_stack[top + 1] = stmt.remainder;
                    top += 2;
                }
            },
            .switch_stmt => |stmt| {
                const heads = store.getCFSwitchBranches(stmt.branches);
                for (0..GuardedList.borrowLen(heads)) |i| {
                    if (top < cursor_stack.len) {
                        cursor_stack[top] = GuardedList.at(heads, i).body;
                        top += 1;
                    }
                }
                if (top < cursor_stack.len) {
                    cursor_stack[top] = stmt.default_branch;
                    top += 1;
                }
                if (stmt.continuation) |continuation| {
                    if (top < cursor_stack.len) {
                        cursor_stack[top] = continuation;
                        top += 1;
                    }
                }
            },
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .boxy_tag_match,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            => {},
        }
    }
    return false;
}

// Repro for https://github.com/roc-lang/roc/issues/10435: SpecConstr must
// preserve the two observed loop results without mutating frozen Monotype type
// data while removing the unused third result.
test "issue 10435 SpecConstr preserves frozen types for partially used while state" {
    const allocator = std.testing.allocator;
    const source =
        \\main : U64
        \\main = {
        \\    var $x = 0.U64
        \\    var $y = 0.U64
        \\    var $unused = 0.U64
        \\    while $x < 3 {
        \\        $x = $x + 1
        \\        $y = $y + 2
        \\        $unused = $unused + 3
        \\    }
        \\    $x + $y
        \\}
    ;

    var lifted = try liftModuleAfterSpecConstr(allocator, source);
    defer lifted.deinit(allocator);
    try std.testing.expect(lifted.lifted.types.isFrozen());

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    var interpreter = try eval.Interpreter.init(
        allocator,
        &optimized.lowered.lir_result.store,
        &optimized.lowered.lir_result.layouts,
        runtime_env.get_ops(),
        .preserve,
    );
    defer interpreter.deinit();

    const result = try interpreter.eval(.{ .proc_id = try rootProc(&optimized.lowered) });
    switch (result) {
        .value => |value| try std.testing.expectEqual(@as(u64, 9), value.read(u64)),
    }
}

// Repro for the ARC certifier failure behind
// https://github.com/roc-lang/roc/issues/10461: ScalarizeJoins treated a
// neighboring join's parameter as a splattable wrapper temporary because its
// only initialization was a struct literal and its only use was a
// `set_local initialize_join_param` copy. Splatting deleted the literal—
// that join's edge initialization—leaving the parameter uninitialized, so
// ARC's release of the (unused, refcounted) parameter had nothing to
// release. The parameter must instead be seeded by field reads and dissolve
// on a later fixpoint round.
test "issue 10461 ScalarizeJoins keeps neighboring join parameter initialization" {
    const allocator = std.testing.allocator;
    const source =
        \\main : U64
        \\main = {
        \\    data_len = 300.U64
        \\    var $bytes = List.with_capacity(8.U64)
        \\    var $bitcount = 0.U8
        \\    var $pos = 0.U64
        \\
        \\    while $pos < data_len {
        \\        var $seqs = List.with_capacity(8.U64)
        \\        var $litrun = 0.U64
        \\        var $in_block = True
        \\
        \\        while $in_block {
        \\            if data_len - $pos < 5 {
        \\                var $k = 0.U64
        \\                while $k < data_len - $pos {
        \\                    $litrun = $litrun + 1
        \\                    $pos = $pos + 1
        \\                    $k = $k + 1
        \\                }
        \\                $in_block = False
        \\            } else {
        \\                if $pos % 2 == 1 {
        \\                    $seqs = $seqs.append($litrun)
        \\                    $litrun = 0
        \\                    $pos = $pos + 4
        \\                } else {
        \\                    $litrun = $litrun + 1
        \\                    $pos = $pos + 1
        \\                }
        \\
        \\                if $pos >= data_len {
        \\                    $in_block = False
        \\                } else {}
        \\            }
        \\        }
        \\
        \\        seqs = if $litrun > 0 {
        \\            $seqs.append($litrun)
        \\        } else {
        \\            $seqs
        \\        }
        \\        var $s = 0.U64
        \\        while $s < seqs.len() {
        \\            seq = match seqs.get($s) {
        \\                Ok(v) => v
        \\                Err(_) => 0
        \\            }
        \\            c = $bitcount + seq.to_u8_wrap()
        \\            if c >= 8 {
        \\                $bytes = $bytes.append(c)
        \\                $bitcount = c - 8
        \\            } else {
        \\                $bitcount = c
        \\            }
        \\            $s = $s + 1
        \\        }
        \\    }
        \\
        \\    if $bitcount > 0 {
        \\        $bytes = $bytes.append($bitcount)
        \\    } else {}
        \\    $bytes.len()
        \\}
    ;

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    var interpreter = try eval.Interpreter.init(
        allocator,
        &optimized.lowered.lir_result.store,
        &optimized.lowered.lir_result.layouts,
        runtime_env.get_ops(),
        .preserve,
    );
    defer interpreter.deinit();

    const result = try interpreter.eval(.{ .proc_id = try rootProc(&optimized.lowered) });
    switch (result) {
        .value => |value| try std.testing.expectEqual(@as(u64, 1), value.read(u64)),
    }
}

// Repro for https://github.com/roc-lang/roc/issues/10461: when SpecConstr
// narrows an inner loop's partially demanded results and the continuation
// after that loop ends in the enclosing loop's back edge, inlining the
// continuation at the inner loop's exit site would rebind that `continue` to
// the inner loop. The continuation must stay outside as a join body.
test "issue 10461 SpecConstr keeps outer loop back edge out of inner loop body" {
    const allocator = std.testing.allocator;
    const source =
        \\main : U64
        \\main = {
        \\    var $a = 0.U64
        \\    var $b = 0.U64
        \\    var $c = 0.U64
        \\    var $d = 0.U64
        \\    while $a < 3 {
        \\        var $x = 0.U64
        \\        var $y = 0.U64
        \\        var $z = 0.U64
        \\        while $x < 2 {
        \\            $x = $x + 1
        \\            $y = $y + $a
        \\            $z = $z + 2
        \\        }
        \\        $a = $a + $x
        \\        $b = $b + $y
        \\        $c = $c + 1
        \\        $d = $d + 2
        \\    }
        \\    $a + $b
        \\}
    ;

    var lifted = try liftModuleAfterSpecConstr(allocator, source);
    defer lifted.deinit(allocator);

    var optimized = try lowerModule(allocator, source, .wrappers);
    defer optimized.deinit(allocator);

    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    var interpreter = try eval.Interpreter.init(
        allocator,
        &optimized.lowered.lir_result.store,
        &optimized.lowered.lir_result.layouts,
        runtime_env.get_ops(),
        .preserve,
    );
    defer interpreter.deinit();

    const result = try interpreter.eval(.{ .proc_id = try rootProc(&optimized.lowered) });
    switch (result) {
        .value => |value| try std.testing.expectEqual(@as(u64, 8), value.read(u64)),
    }
}

// A selected loop-result ABI applies to every break owned by that loop,
// including breaks nested inside match arms. Rewriting only the terminating
// spine leaves those tuple-valued breaks stamped with the selected scalar type
// and Lambda Solved rejects the inconsistent expression.
test "SpecConstr rewrites nested match breaks with the selected loop exit ABI" {
    const allocator = std.testing.allocator;
    const source =
        \\main : List(U8), U64, U64 -> U64
        \\main = |bytes, a_start, b_start| {
        \\    lo = a_start.min(b_start)
        \\    delta = a_start.max(b_start).minus_saturated(lo)
        \\    var $acc = 0.U64
        \\    var $a = lo
        \\
        \\    while True {
        \\        x = match U64.from_le_bytes(bytes, $a) {
        \\            Ok(v) => v
        \\            Err(_) => break
        \\        }
        \\        y = match U64.from_le_bytes(bytes, $a.plus_wrap(delta)) {
        \\            Ok(v) => v
        \\            Err(_) => break
        \\        }
        \\        if x != y {
        \\            return $acc.plus_wrap(U64.count_trailing_zero_bits(x.bitwise_xor(y)).to_u64() // 8)
        \\        }
        \\        $acc = $acc.plus_wrap(8)
        \\        $a = $a.plus_wrap(8)
        \\    }
        \\
        \\    while True {
        \\        p = match bytes.get($a) {
        \\            Ok(v) => v
        \\            Err(_) => break
        \\        }
        \\        q = match bytes.get($a.plus_wrap(delta)) {
        \\            Ok(v) => v
        \\            Err(_) => break
        \\        }
        \\        if p != q { break }
        \\        $acc = $acc.plus_wrap(1)
        \\        $a = $a.plus_wrap(1)
        \\    }
        \\
        \\    $acc
        \\}
    ;

    var lowered = try lowerModule(allocator, source, .wrappers);
    defer lowered.deinit(allocator);
}

test "issue 10354 undefined identifier in expression does not panic monotype lowering" {
    const allocator = std.testing.allocator;
    const source =
        \\undefined
    ;

    _ = lowerModule(allocator, source, .wrappers) catch |err| switch (err) {
        error.TypeCheckError, error.ParseError => {},
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.BitcodeParseError,
        error.BrokenPipe,
        error.BuiltinArtifactVersionMismatch,
        error.Canceled,
        error.CompilationFailed,
        error.ComptimeExhaustiveness,
        error.ConnectionResetByPeer,
        error.CorruptArtifact,
        error.CorruptBuiltinArtifact,
        error.CorruptEmbeddedBuiltins,
        error.Crash,
        error.CreateFileMappingFailed,
        error.DevBackendUnavailable,
        error.DeviceBusy,
        error.DiskQuota,
        error.DivisionByZero,
        error.ElfHashTableNotFound,
        error.ElfStringSectionNotFound,
        error.ElfSymSectionNotFound,
        error.EmptyCode,
        error.EntrypointNotFound,
        error.EvaluationFailed,
        error.ExpectErr,
        error.FileBusy,
        error.FileLocksUnsupported,
        error.FileNotFound,
        error.FileTooBig,
        error.FtruncateFailed,
        error.InputOutput,
        error.Internal,
        error.InvalidHandle,
        error.InvalidHostedFunctionSignature,
        error.InvalidLirImage,
        error.InvalidUtf8,
        error.IsDir,
        error.LinkFailed,
        error.LlvmBackendUnavailable,
        error.LlvmModuleVerificationFailed,
        error.LlvmObjectEmitFailed,
        error.LockViolation,
        error.LockedMemoryLimitExceeded,
        error.MapViewOfFileFailed,
        error.MappingAlreadyExists,
        error.MemfdCreateFailed,
        error.MemoryMappingNotSupported,
        error.MissingBuiltinBitcode,
        error.MissingCallable,
        error.MissingDbgRoot,
        error.MissingDynamicLinkingInformation,
        error.MissingIterCollectWorker,
        error.MissingProcSpec,
        error.MissingRootProcedure,
        error.MissingSpecializedWorker,
        error.MmapFailed,
        error.ModuleLinkFailed,
        error.MprotectFailed,
        error.NameTooLong,
        error.NetworkNotFound,
        error.NoBitcodeModules,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.NotDynamicLibrary,
        error.NotElfFile,
        error.NotOpenForReading,
        error.NotOpenForWriting,
        error.OpenFileMappingFailed,
        error.OutOfMemory,
        error.PageSizeQueryFailed,
        error.PathAlreadyExists,
        error.PermissionDenied,
        error.PipeBusy,
        error.ProcessFdQuotaExceeded,
        error.ReadOnlyFileSystem,
        error.RuntimeError,
        error.ShmOpenFailed,
        error.ShmUnlinkFailed,
        error.SocketUnconnected,
        error.StaleEmbeddedBuiltins,
        error.Streaming,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.TempFileError,
        error.TempFileOpenFailed,
        error.TempFileUnlinkFailed,
        error.TestExpectedEqual,
        error.TestUnexpectedResult,
        error.ThreadQuotaExceeded,
        error.Unexpected,
        error.Unseekable,
        error.UnsupportedHostedFunction,
        error.UnsupportedLirImageVersion,
        error.UnsupportedLlvmTriple,
        error.UnsupportedLowLevel,
        error.UnsupportedPlatform,
        error.UnsupportedTarget,
        error.UnwindRegistrationFailed,
        error.VirtualAllocFailed,
        error.VirtualProtectFailed,
        error.WasmExecFailed,
        error.WindowsSDKNotFound,
        error.WouldBlock,
        error.WriteFailed,
        => return err,
    };
}

test "issue 10409 duplicate top-level value defs do not panic constant root lookup" {
    const allocator = std.testing.allocator;
    const source =
        \\main : {}
        \\main = {}
        \\x = ()
        \\x = 0
    ;

    var lowered = try lowerModule(allocator, source, .wrappers);
    defer lowered.deinit(allocator);
}
