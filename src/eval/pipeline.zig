//! Shared parsing, checking, and lowering pipeline for eval and REPL.

const builtin = @import("builtin");
const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const can = @import("can");
const check = @import("check");
const parse = @import("parse");
const compiled_builtins = @import("compiled_builtins");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const lambdasolved = @import("lambdasolved");
const lambdamono = @import("lambdamono");
const ir = @import("ir");
const lir = @import("lir");
const layout_mod = @import("layout");
const symbol_mod = @import("symbol");
const comptime_value = @import("comptime_value.zig");
const builtin_loading = @import("builtin_loading.zig");

const Can = can.Can;
const HostedCompiler = can.HostedCompiler;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Allocators = base.Allocators;
const FromIr = lir.FromIr;
const is_freestanding = builtin.os.tag == .freestanding;

/// Comptime-gated tracing for the shared lowering pipeline.
const trace = if ((if (@hasDecl(build_options, "trace_eval")) build_options.trace_eval else false) and !is_freestanding)
    struct {
        fn log(comptime fmt: []const u8, args: anytype) void {
            std.debug.print("[lower] " ++ fmt ++ "\n", args);
        }
    }
else
    struct {
        fn log(comptime _: []const u8, _: anytype) void {}
    };

/// Public enum `SourceKind`.
pub const SourceKind = enum {
    expr,
    module,
};

/// Public struct `ModuleSource`.
pub const ModuleSource = struct {
    name: []const u8,
    source: []const u8,
};

const AvailableImport = struct {
    name: []const u8,
    env: *const ModuleEnv,
    statement_idx: ?CIR.Statement.Idx,
};

fn availableImportStatementIdx(_: *const ModuleEnv) ?CIR.Statement.Idx {
    return null;
}

/// Public struct `CheckedModule`.
pub const CheckedModule = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    imported_envs: []*const ModuleEnv,
    owned_source: ?[]u8 = null,
    published_owns_module_env: bool = false,
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,
};

/// Public struct `ParsedResources`.
pub const ParsedResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    typed_cir_modules: check.TypedCIR.Modules,
    expr_idx: CIR.Expr.Idx,
    builtin_module: builtin_loading.LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    imported_envs: []*const ModuleEnv,
    extra_modules: []CheckedModule,
    owned_source: ?[]u8 = null,
    published_owns_module_env: bool = false,
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,

    pub fn deinit(self: *ParsedResources, allocator: std.mem.Allocator) void {
        for (self.extra_modules) |module| cleanupCheckedModule(allocator, module);
        allocator.free(self.extra_modules);
        self.checker.deinit();
        self.can.deinit();
        self.parse_ast.deinit();
        self.typed_cir_modules.deinit();
        self.builtin_module.deinit();
        allocator.free(self.imported_envs);
        if (!self.published_owns_module_env) {
            self.module_env.deinit();
            if (self.owned_source) |source| allocator.free(source);
            allocator.destroy(self.module_env);
        }
        allocator.destroy(self.checker);
        allocator.destroy(self.can);
    }
};

/// LIR program plus root proc selected for interpreter execution.
pub const LoweredProgram = struct {
    lir_result: FromIr.Result,
    main_proc: lir.LIR.LirProcSpecId,
    target_usize: base.target.TargetUsize,

    pub fn deinit(self: *LoweredProgram) void {
        self.lir_result.deinit();
    }
};

/// Compile-time top-level binding lowered to a proc and reification schema.
pub const SemanticEvalTopLevelRoot = struct {
    def_idx: CIR.Def.Idx,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    proc_id: lir.LIR.LirProcSpecId,
    binding_schema_id: ?comptime_value.SchemaId,
    init_tuple_index: ?u32 = null,
    init_eval_index: ?u32 = null,
};

/// Source-level expect lowered to an executable proc root.
pub const SemanticEvalExpectRoot = struct {
    statement_idx: CIR.Statement.Idx,
    expr_idx: CIR.Expr.Idx,
    proc_id: lir.LIR.LirProcSpecId,
};

/// Shared LIR program plus schema metadata for semantic evaluation.
pub const SemanticEvalProgram = struct {
    lowered: LoweredProgram,
    schemas: comptime_value.SchemaStore,
    comptime_init_proc: ?lir.LIR.LirProcSpecId,
    comptime_init_schema_id: ?comptime_value.SchemaId,
    top_level_roots: []SemanticEvalTopLevelRoot,
    expect_roots: []SemanticEvalExpectRoot,

    pub fn deinit(self: *SemanticEvalProgram) void {
        const allocator = self.lowered.lir_result.store.allocator;
        allocator.free(self.top_level_roots);
        allocator.free(self.expect_roots);
        self.schemas.deinit();
        self.lowered.deinit();
    }
};

/// Public function `parseAndCanonicalizeProgramWrapped`.
pub fn parseAndCanonicalizeProgramWrapped(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    inspect_wrap: bool,
) !ParsedResources {
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
    var builtin_module = try builtin_loading.loadCompiledModule(
        allocator,
        compiled_builtins.builtin_bin,
        "Builtin",
        compiled_builtins.builtin_source,
    );
    errdefer builtin_module.deinit();

    var extra_modules = std.ArrayList(CheckedModule).empty;
    errdefer {
        for (extra_modules.items) |extra| cleanupCheckedModule(allocator, extra);
        extra_modules.deinit(allocator);
    }

    for (imports) |import_module| {
        const available_imports = try allocator.alloc(AvailableImport, extra_modules.items.len);
        defer allocator.free(available_imports);
        for (extra_modules.items, 0..) |extra, i| {
            available_imports[i] = .{
                .name = extra.module_env.module_name,
                .env = extra.module_env,
                .statement_idx = availableImportStatementIdx(extra.module_env),
            };
        }

        const checked = try parseCheckModule(
            allocator,
            import_module.name,
            .module,
            import_module.source,
            false,
            true,
            builtin_module.env,
            builtin_indices,
            available_imports,
        );
        try extra_modules.append(allocator, checked);
    }

    const main_imports = try allocator.alloc(AvailableImport, extra_modules.items.len);
    defer allocator.free(main_imports);
    for (extra_modules.items, 0..) |extra, i| {
        main_imports[i] = .{
            .name = extra.module_env.module_name,
            .env = extra.module_env,
            .statement_idx = availableImportStatementIdx(extra.module_env),
        };
    }

    var main_checked = try parseCheckModule(
        allocator,
        "Test",
        source_kind,
        source,
        inspect_wrap,
        false,
        builtin_module.env,
        builtin_indices,
        main_imports,
    );
    errdefer cleanupCheckedModule(allocator, main_checked);

    const defs = main_checked.module_env.store.sliceDefs(main_checked.module_env.all_defs);
    if (defs.len == 0) return error.NoRootDefinition;
    const expr_idx = main_checked.module_env.store.getDef(defs[defs.len - 1]).expr;

    var all_module_envs = try allocator.alloc(*ModuleEnv, extra_modules.items.len + 2);
    defer allocator.free(all_module_envs);
    all_module_envs[0] = main_checked.module_env;
    all_module_envs[1] = builtin_module.env;
    for (extra_modules.items, 0..) |extra, i| {
        all_module_envs[i + 2] = extra.module_env;
    }
    for (all_module_envs) |module_env| {
        module_env.imports.clearResolvedModules();
        for (module_env.imports.imports.items.items, 0..) |str_idx, i| {
            const import_name = module_env.getString(str_idx);
            for (all_module_envs, 0..) |candidate_env, module_idx| {
                if (std.mem.eql(u8, candidate_env.module_name, import_name)) {
                    module_env.imports.setResolvedModule(@enumFromInt(i), @intCast(module_idx));
                    break;
                }
            }
        }
    }

    var typed_cir_source_modules = try allocator.alloc(check.TypedCIR.Modules.SourceModule, extra_modules.items.len + 2);
    defer allocator.free(typed_cir_source_modules);
    typed_cir_source_modules[0] = .{ .owned_checked = .{
        .env = main_checked.module_env,
        .owned_source = main_checked.owned_source,
    } };
    typed_cir_source_modules[1] = .{ .precompiled = builtin_module.env };
    for (extra_modules.items, 0..) |extra, i| {
        typed_cir_source_modules[i + 2] = .{ .owned_checked = .{
            .env = extra.module_env,
            .owned_source = extra.owned_source,
        } };
    }

    const typed_cir_modules = try check.TypedCIR.Modules.init(allocator, typed_cir_source_modules);
    errdefer {
        var typed_cir_modules_mut = typed_cir_modules;
        typed_cir_modules_mut.deinit();
    }
    main_checked.published_owns_module_env = true;
    main_checked.owned_source = null;
    for (extra_modules.items) |*extra| {
        extra.published_owns_module_env = true;
        extra.owned_source = null;
    }

    return .{
        .module_env = main_checked.module_env,
        .parse_ast = main_checked.parse_ast,
        .can = main_checked.can,
        .checker = main_checked.checker,
        .typed_cir_modules = typed_cir_modules,
        .expr_idx = expr_idx,
        .builtin_module = builtin_module,
        .builtin_indices = builtin_indices,
        .imported_envs = main_checked.imported_envs,
        .extra_modules = try extra_modules.toOwnedSlice(allocator),
        .owned_source = null,
        .published_owns_module_env = true,
        .parse_ns = main_checked.parse_ns,
        .canonicalize_ns = main_checked.canonicalize_ns,
        .typecheck_ns = main_checked.typecheck_ns,
    };
}

/// Public function `parseCheckModule`.
pub fn parseCheckModule(
    allocator: std.mem.Allocator,
    module_name: []const u8,
    source_kind: SourceKind,
    source: []const u8,
    inspect_wrap: bool,
    hosted_transform: bool,
    builtin_module_env: *const ModuleEnv,
    builtin_indices: CIR.BuiltinIndices,
    available_imports: []const AvailableImport,
) !CheckedModule {
    const owned_source = try makeModuleSource(allocator, source_kind, source, inspect_wrap);
    errdefer allocator.free(owned_source);

    const module_env = try allocator.create(ModuleEnv);
    errdefer allocator.destroy(module_env);
    module_env.* = try ModuleEnv.init(allocator, owned_source);
    errdefer module_env.deinit();
    module_env.common.source = owned_source;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(module_env.gpa);

    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    errdefer allocators.deinit();

    var parse_elapsed: u64 = 0;
    var parse_ast: *parse.AST = undefined;
    if (comptime !is_freestanding) {
        var parse_timer = try std.time.Timer.start();
        parse_ast = try parse.parse(&allocators, &module_env.common);
        parse_elapsed = parse_timer.read();
    } else {
        parse_ast = try parse.parse(&allocators, &module_env.common);
    }
    errdefer {
        parse_ast.deinit();
        allocators.deinit();
    }
    parse_ast.store.emptyScratch();
    if (parse_ast.tokenize_diagnostics.items.len > 0 or parse_ast.parse_diagnostics.items.len > 0) {
        return error.ParseError;
    }

    try module_env.initCIRFields(module_name);
    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module_env,
        .builtin_indices = builtin_indices,
    };

    var imported_modules = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer imported_modules.deinit();
    for (available_imports) |available| {
        const import_ident = try module_env.insertIdent(base.Ident.for_text(available.name));
        const qualified_ident = try module_env.insertIdent(base.Ident.for_text(available.name));
        try imported_modules.put(import_ident, .{
            .env = available.env,
            .statement_idx = available.statement_idx,
            .qualified_type_ident = qualified_ident,
        });
    }

    const czer = try allocator.create(Can);
    errdefer allocator.destroy(czer);
    czer.* = try Can.initModule(&allocators, module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module_env,
            .builtin_indices = builtin_indices,
        },
        .imported_modules = if (available_imports.len == 0) null else &imported_modules,
    });
    errdefer czer.deinit();
    var can_elapsed: u64 = 0;
    if (comptime !is_freestanding) {
        var can_timer = try std.time.Timer.start();
        try czer.canonicalizeFile();
        if (hosted_transform) {
            var modified_defs = try HostedCompiler.replaceAnnoOnlyWithHosted(module_env);
            defer modified_defs.deinit(module_env.gpa);
            var hosted_fns = try HostedCompiler.collectAndSortHostedFunctions(module_env);
            defer {
                for (hosted_fns.items) |hosted_fn| allocator.free(hosted_fn.name_text);
                hosted_fns.deinit(module_env.gpa);
            }
            try HostedCompiler.assignHostedIndices(module_env, hosted_fns.items);
        }
        can_elapsed = can_timer.read();
    } else {
        try czer.canonicalizeFile();
        if (hosted_transform) {
            var modified_defs = try HostedCompiler.replaceAnnoOnlyWithHosted(module_env);
            defer modified_defs.deinit(module_env.gpa);
            var hosted_fns = try HostedCompiler.collectAndSortHostedFunctions(module_env);
            defer {
                for (hosted_fns.items) |hosted_fn| allocator.free(hosted_fn.name_text);
                hosted_fns.deinit(module_env.gpa);
            }
            try HostedCompiler.assignHostedIndices(module_env, hosted_fns.items);
        }
    }

    const imported_envs_len: usize = if (available_imports.len == 0 and source_kind == .expr) 1 else available_imports.len + 2;
    const imported_envs = try allocator.alloc(*const ModuleEnv, imported_envs_len);
    errdefer allocator.free(imported_envs);

    if (available_imports.len == 0 and source_kind == .expr) {
        imported_envs[0] = builtin_module_env;
    } else {
        imported_envs[0] = module_env;
        imported_envs[1] = builtin_module_env;
        for (available_imports, 0..) |available, i| {
            imported_envs[i + 2] = available.env;
        }
    }
    module_env.imports.clearResolvedModules();
    for (module_env.imports.imports.items.items, 0..) |str_idx, i| {
        const import_name = module_env.getString(str_idx);
        for (imported_envs, 0..) |candidate_env, module_idx| {
            if (std.mem.eql(u8, candidate_env.module_name, import_name)) {
                module_env.imports.setResolvedModule(@enumFromInt(i), @intCast(module_idx));
                break;
            }
        }
    }

    const checker = try allocator.create(Check);
    errdefer allocator.destroy(checker);
    checker.* = try Check.init(
        allocator,
        &module_env.types,
        module_env,
        imported_envs,
        null,
        &module_env.store.regions,
        builtin_ctx,
    );
    errdefer checker.deinit();
    var check_elapsed: u64 = 0;
    if (comptime !is_freestanding) {
        var check_timer = try std.time.Timer.start();
        try checker.checkFile();
        check_elapsed = check_timer.read();
    } else {
        try checker.checkFile();
    }
    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = czer,
        .checker = checker,
        .imported_envs = imported_envs,
        .owned_source = owned_source,
        .parse_ns = parse_elapsed,
        .canonicalize_ns = can_elapsed,
        .typecheck_ns = check_elapsed,
    };
}

/// Public function `exprSourcePrefixLen`.
pub fn exprSourcePrefixLen(inspect_wrap: bool) u32 {
    return @intCast(if (inspect_wrap) "main = Str.inspect((".len else "main = ".len);
}

/// Public function `lowerParsedExprToLir`.
pub fn lowerParsedExprToLir(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
) !LoweredProgram {
    return lowerToLirForTarget(allocator, resources, base.target.TargetUsize.native);
}

/// Public function `lowerParsedExprToLirForTarget`.
pub fn lowerParsedExprToLirForTarget(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
    target_usize: base.target.TargetUsize,
) !LoweredProgram {
    return lowerToLirForTarget(allocator, resources, target_usize);
}

/// Public function `lowerTypedCIRToLir`.
pub fn lowerTypedCIRToLir(
    allocator: std.mem.Allocator,
    typed_cir_modules: *check.TypedCIR.Modules,
    module_envs: []const *const ModuleEnv,
) !LoweredProgram {
    return lowerTypedCIRToLirForTarget(
        allocator,
        typed_cir_modules,
        module_envs,
        base.target.TargetUsize.native,
    );
}

/// Public function `lowerTypedCIRToLirForTarget`.
pub fn lowerTypedCIRToLirForTarget(
    allocator: std.mem.Allocator,
    typed_cir_modules: *check.TypedCIR.Modules,
    module_envs: []const *const ModuleEnv,
    target_usize: base.target.TargetUsize,
) !LoweredProgram {
    trace.log("typed-cir -> monotype", .{});
    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();
    var mono = try mono_lowerer.run(0);
    defer mono.deinit();
    debugValidateMonotypeTypes(&mono.types);
    trace.log("monotype -> monotype_lifted", .{});
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    defer lifted.deinit();
    trace.log("monotype_lifted -> lambdasolved", .{});
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    defer solved.deinit();
    trace.log("lambdasolved -> lambdamono", .{});
    var executable = try lambdamono.Lower.run(allocator, &solved);
    defer executable.deinit();
    trace.log("lambdamono -> ir", .{});
    var lowered_ir = try ir.Lower.run(allocator, &executable);
    defer lowered_ir.deinit();

    trace.log("ir -> lir", .{});
    var lowered_lir = try FromIr.run(
        allocator,
        module_envs,
        null,
        target_usize,
        &lowered_ir,
    );
    errdefer lowered_lir.deinit();
    try lir.Ownership.inferProcResultContracts(allocator, &lowered_lir.store, &lowered_lir.layouts);
    try lir.RcInsert.run(allocator, &lowered_lir.store, &lowered_lir.layouts);
    try lir.SharedSwitchTail.run(allocator, &lowered_lir.store);
    if (lowered_lir.root_procs.items.len == 0) return error.NoRootProc;
    return .{
        .lir_result = lowered_lir,
        .main_proc = lowered_lir.root_procs.items[lowered_lir.root_procs.items.len - 1],
        .target_usize = target_usize,
    };
}

/// Lower typed CIR to the default-target semantic-eval LIR program.
pub fn lowerTypedCIRToSemanticEvalProgram(
    allocator: std.mem.Allocator,
    typed_cir_modules: *check.TypedCIR.Modules,
    module_envs: []const *const ModuleEnv,
) !SemanticEvalProgram {
    return lowerTypedCIRToSemanticEvalProgramForTarget(
        allocator,
        typed_cir_modules,
        module_envs,
        base.target.TargetUsize.native,
    );
}

const SyntheticComptimeInit = struct {
    proc_id: lir.LIR.LirProcSpecId,
    schema_id: comptime_value.SchemaId,
};

fn topLevelExprNeedsBindingSchema(expr: CIR.Expr) bool {
    return switch (expr.data) {
        .e_lambda, .e_closure, .e_anno_only, .e_lookup_required, .e_runtime_error => false,
        else => true,
    };
}

fn topLevelExprNeedsEvaluation(expr: CIR.Expr) bool {
    return switch (expr.data) {
        .e_lambda, .e_closure, .e_anno_only, .e_lookup_required => false,
        else => true,
    };
}

fn synthesizeSemanticEvalExpectWrapper(
    allocator: std.mem.Allocator,
    lowered_lir: *FromIr.Result,
    body_proc_id: lir.LIR.LirProcSpecId,
) !lir.LIR.LirProcSpecId {
    const unit_layout = try lowered_lir.layouts.ensureZstLayout();
    const cond_local = try lowered_lir.store.addLocal(.{
        .layout_idx = lowered_lir.store.getProcSpec(body_proc_id).ret_layout,
    });
    const unit_local = try lowered_lir.store.addLocal(.{ .layout_idx = unit_layout });
    const ret_stmt = try lowered_lir.store.addCFStmt(.{ .ret = .{ .value = unit_local } });
    const unit_stmt = try lowered_lir.store.addCFStmt(.{ .assign_struct = .{
        .target = unit_local,
        .result = .fresh,
        .ownership = .{},
        .fields = lir.LIR.LocalSpan.empty(),
        .next = ret_stmt,
    } });
    const expect_stmt = try lowered_lir.store.addCFStmt(.{ .expect = .{
        .condition = cond_local,
        .next = unit_stmt,
    } });
    const call_stmt = try lowered_lir.store.addCFStmt(.{ .assign_call = .{
        .target = cond_local,
        .result = .fresh,
        .proc = body_proc_id,
        .args = lir.LIR.LocalSpan.empty(),
        .next = expect_stmt,
    } });
    const symbol = lowered_lir.store.freshSyntheticSymbol();
    const proc_id = try lowered_lir.store.addProcSpec(.{
        .name = symbol,
        .args = lir.LIR.LocalSpan.empty(),
        .body = call_stmt,
        .ret_layout = unit_layout,
        .result_contract = .fresh,
    });
    try lowered_lir.proc_ids_by_symbol.put(symbol.raw(), proc_id);
    try lowered_lir.root_procs.append(allocator, proc_id);
    return proc_id;
}

fn synthesizeSemanticEvalComptimeInitProc(
    allocator: std.mem.Allocator,
    root_env: *const ModuleEnv,
    lowered_lir: *FromIr.Result,
    schema_store_owner: *comptime_value.Store,
    top_level_roots: []SemanticEvalTopLevelRoot,
) !?SyntheticComptimeInit {
    if (top_level_roots.len == 0) return null;

    var roots_by_def = std.AutoHashMap(CIR.Def.Idx, usize).init(allocator);
    defer roots_by_def.deinit();
    for (top_level_roots, 0..) |root, i| {
        try roots_by_def.put(root.def_idx, i);
    }

    var ordered_root_indices = std.ArrayList(usize).empty;
    defer ordered_root_indices.deinit(allocator);
    const seen = try allocator.alloc(bool, top_level_roots.len);
    defer allocator.free(seen);
    @memset(seen, false);

    if (root_env.evaluation_order) |eval_order| {
        for (eval_order.sccs) |scc| {
            for (scc.defs) |def_idx| {
                const root_index = roots_by_def.get(def_idx) orelse continue;
                if (!seen[root_index]) {
                    seen[root_index] = true;
                    try ordered_root_indices.append(allocator, root_index);
                }
            }
        }
    }
    for (seen, 0..) |was_seen, i| {
        if (!was_seen) try ordered_root_indices.append(allocator, i);
    }

    const call_result_locals = try allocator.alloc(lir.LIR.LocalId, ordered_root_indices.items.len);
    defer allocator.free(call_result_locals);
    var tuple_schema_ids = std.ArrayList(comptime_value.SchemaId).empty;
    defer tuple_schema_ids.deinit(allocator);
    var tuple_result_locals = std.ArrayList(lir.LIR.LocalId).empty;
    defer tuple_result_locals.deinit(allocator);
    var runnable_root_indices = std.ArrayList(usize).empty;
    defer runnable_root_indices.deinit(allocator);

    for (top_level_roots) |*root| {
        root.init_tuple_index = null;
        root.init_eval_index = null;
    }

    for (ordered_root_indices.items) |root_index| {
        const root = &top_level_roots[root_index];
        if (!topLevelExprNeedsEvaluation(root_env.store.getExpr(root.expr_idx))) continue;

        root.init_eval_index = @intCast(runnable_root_indices.items.len);
        try runnable_root_indices.append(allocator, root_index);
    }

    if (runnable_root_indices.items.len == 0) return null;

    for (runnable_root_indices.items, 0..) |root_index, i| {
        const root = &top_level_roots[root_index];
        const proc_spec = lowered_lir.store.getProcSpec(root.proc_id);
        const result_local = try lowered_lir.store.addLocal(.{ .layout_idx = proc_spec.ret_layout });
        call_result_locals[i] = result_local;
        if (root.binding_schema_id) |schema_id| {
            root.init_tuple_index = @intCast(tuple_result_locals.items.len);
            try tuple_schema_ids.append(allocator, schema_id);
            try tuple_result_locals.append(allocator, result_local);
        }
    }

    const init_schema_id, const init_ret_layout, const aggregate_local = blk: {
        if (tuple_result_locals.items.len == 0) {
            const unit_layout = try lowered_lir.layouts.ensureZstLayout();
            break :blk .{
                try schema_store_owner.schemas.add(.zst),
                unit_layout,
                try lowered_lir.store.addLocal(.{ .layout_idx = unit_layout }),
            };
        }

        const schema_ids = try schema_store_owner.schemas.allocator.dupe(
            comptime_value.SchemaId,
            tuple_schema_ids.items,
        );
        const tuple_layouts = try allocator.alloc(layout_mod.Layout, tuple_result_locals.items.len);
        defer allocator.free(tuple_layouts);
        for (tuple_result_locals.items, 0..) |local_id, i| {
            tuple_layouts[i] = lowered_lir.layouts.getLayout(lowered_lir.store.getLocal(local_id).layout_idx);
        }
        const tuple_layout = try lowered_lir.layouts.putTuple(tuple_layouts);
        break :blk .{
            try schema_store_owner.schemas.add(.{ .tuple = schema_ids }),
            tuple_layout,
            try lowered_lir.store.addLocal(.{ .layout_idx = tuple_layout }),
        };
    };

    const ret_stmt = try lowered_lir.store.addCFStmt(.{ .ret = .{ .value = aggregate_local } });
    var cursor = if (tuple_result_locals.items.len == 0)
        try lowered_lir.store.addCFStmt(.{ .assign_struct = .{
            .target = aggregate_local,
            .result = .fresh,
            .ownership = .{},
            .fields = lir.LIR.LocalSpan.empty(),
            .next = ret_stmt,
        } })
    else
        try lowered_lir.store.addCFStmt(.{ .assign_struct = .{
            .target = aggregate_local,
            .result = .fresh,
            .ownership = .{},
            .fields = try lowered_lir.store.addLocalSpan(tuple_result_locals.items),
            .next = ret_stmt,
        } });

    var idx = runnable_root_indices.items.len;
    while (idx > 0) {
        idx -= 1;
        const root_index = runnable_root_indices.items[idx];
        cursor = try lowered_lir.store.addCFStmt(.{ .assign_call = .{
            .target = call_result_locals[idx],
            .result = .fresh,
            .proc = top_level_roots[root_index].proc_id,
            .args = lir.LIR.LocalSpan.empty(),
            .next = cursor,
        } });
    }

    const symbol = lowered_lir.store.freshSyntheticSymbol();
    const proc_id = try lowered_lir.store.addProcSpec(.{
        .name = symbol,
        .args = lir.LIR.LocalSpan.empty(),
        .body = cursor,
        .ret_layout = init_ret_layout,
        .result_contract = .fresh,
    });
    try lowered_lir.proc_ids_by_symbol.put(symbol.raw(), proc_id);
    try lowered_lir.root_procs.append(allocator, proc_id);

    return .{
        .proc_id = proc_id,
        .schema_id = init_schema_id,
    };
}

/// Lower typed CIR to a target-specific semantic-eval LIR program.
pub fn lowerTypedCIRToSemanticEvalProgramForTarget(
    allocator: std.mem.Allocator,
    typed_cir_modules: *check.TypedCIR.Modules,
    module_envs: []const *const ModuleEnv,
    target_usize: base.target.TargetUsize,
) !SemanticEvalProgram {
    trace.log("typed-cir -> monotype (semantic eval)", .{});
    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();

    const root_module = typed_cir_modules.module(0);
    var top_level_symbols = std.ArrayList(struct {
        def_idx: CIR.Def.Idx,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
        symbol: symbol_mod.Symbol,
    }).empty;
    defer top_level_symbols.deinit(allocator);

    for (root_module.allDefs()) |def_idx| {
        const def = root_module.def(def_idx);
        const symbol = try mono_lowerer.specializeTopLevelDef(0, def_idx);
        try top_level_symbols.append(allocator, .{
            .def_idx = def_idx,
            .pattern_idx = def.pattern.idx,
            .expr_idx = def.expr.idx,
            .symbol = symbol,
        });
    }

    var expect_symbols = std.ArrayList(struct {
        statement_idx: CIR.Statement.Idx,
        expr_idx: CIR.Expr.Idx,
        symbol: symbol_mod.Symbol,
    }).empty;
    defer expect_symbols.deinit(allocator);

    const root_env = module_envs[0];
    for (root_env.store.sliceStatements(root_env.all_statements)) |statement_idx| {
        const stmt = root_env.store.getStatement(statement_idx);
        if (stmt != .s_expect) continue;
        const symbol = try mono_lowerer.addRootExprEntrypoint(0, stmt.s_expect.body);
        try expect_symbols.append(allocator, .{
            .statement_idx = statement_idx,
            .expr_idx = stmt.s_expect.body,
            .symbol = symbol,
        });
    }

    var mono = try mono_lowerer.run(0);
    defer mono.deinit();
    debugValidateMonotypeTypes(&mono.types);
    trace.log("monotype -> monotype_lifted (semantic eval)", .{});
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    defer lifted.deinit();
    trace.log("monotype_lifted -> lambdasolved (semantic eval)", .{});
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    defer solved.deinit();
    trace.log("lambdasolved -> lambdamono (semantic eval)", .{});
    var executable = try lambdamono.Lower.run(allocator, &solved);
    defer executable.deinit();
    trace.log("lambdamono -> ir (semantic eval)", .{});
    var lowered_ir = try ir.Lower.run(allocator, &executable);
    defer lowered_ir.deinit();

    trace.log("ir -> lir (semantic eval)", .{});
    var lowered_lir = try FromIr.run(
        allocator,
        module_envs,
        null,
        target_usize,
        &lowered_ir,
    );
    errdefer lowered_lir.deinit();

    var schema_store_owner = comptime_value.Store.init(allocator);
    errdefer schema_store_owner.deinit();

    const top_level_roots = try allocator.alloc(SemanticEvalTopLevelRoot, top_level_symbols.items.len);
    errdefer allocator.free(top_level_roots);
    for (top_level_symbols.items, 0..) |entry, i| {
        const proc_id = lowered_lir.proc_ids_by_symbol.get(entry.symbol.raw()) orelse return error.NoRootProc;
        const expr = root_env.store.getExpr(entry.expr_idx);
        top_level_roots[i] = .{
            .def_idx = entry.def_idx,
            .pattern_idx = entry.pattern_idx,
            .expr_idx = entry.expr_idx,
            .proc_id = proc_id,
            .binding_schema_id = if (topLevelExprNeedsBindingSchema(expr))
                try schema_store_owner.buildSchema(
                    root_module.typeStoreConst(),
                    root_module.identStoreConst(),
                    root_module.def(entry.def_idx).expr.ty(),
                    &lowered_lir.layouts,
                    lowered_lir.store.getProcSpec(proc_id).ret_layout,
                )
            else
                null,
        };
    }

    const expect_roots = try allocator.alloc(SemanticEvalExpectRoot, expect_symbols.items.len);
    errdefer allocator.free(expect_roots);
    for (expect_symbols.items, 0..) |entry, i| {
        const body_proc_id = lowered_lir.proc_ids_by_symbol.get(entry.symbol.raw()) orelse return error.NoRootProc;
        expect_roots[i] = .{
            .statement_idx = entry.statement_idx,
            .expr_idx = entry.expr_idx,
            .proc_id = try synthesizeSemanticEvalExpectWrapper(
                allocator,
                &lowered_lir,
                body_proc_id,
            ),
        };
    }

    const comptime_init = try synthesizeSemanticEvalComptimeInitProc(
        allocator,
        root_env,
        &lowered_lir,
        &schema_store_owner,
        top_level_roots,
    );

    try lir.Ownership.inferProcResultContracts(allocator, &lowered_lir.store, &lowered_lir.layouts);
    try lir.RcInsert.run(allocator, &lowered_lir.store, &lowered_lir.layouts);
    try lir.SharedSwitchTail.run(allocator, &lowered_lir.store);

    const schemas = schema_store_owner.schemas;
    schema_store_owner.schemas = comptime_value.SchemaStore.init(allocator);
    schema_store_owner.deinit();

    return .{
        .lowered = .{
            .lir_result = lowered_lir,
            .main_proc = if (comptime_init) |init|
                init.proc_id
            else if (expect_roots.len != 0)
                expect_roots[expect_roots.len - 1].proc_id
            else if (top_level_roots.len != 0)
                top_level_roots[top_level_roots.len - 1].proc_id
            else
                return error.NoRootProc,
            .target_usize = target_usize,
        },
        .schemas = schemas,
        .comptime_init_proc = if (comptime_init) |init| init.proc_id else null,
        .comptime_init_schema_id = if (comptime_init) |init| init.schema_id else null,
        .top_level_roots = top_level_roots,
        .expect_roots = expect_roots,
    };
}

fn lowerToLirForTarget(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
    target_usize: base.target.TargetUsize,
) !LoweredProgram {
    const module_envs = try allocator.alloc(*const ModuleEnv, resources.extra_modules.len + 2);
    defer allocator.free(module_envs);
    module_envs[0] = resources.module_env;
    module_envs[1] = resources.builtin_module.env;
    for (resources.extra_modules, 0..) |module, i| {
        module_envs[i + 2] = module.module_env;
    }

    const defs = resources.module_env.store.sliceDefs(resources.module_env.all_defs);
    var entry_def: ?CIR.Def.Idx = null;
    for (defs) |def_idx| {
        const def = resources.module_env.store.getDef(def_idx);
        if (def.expr == resources.expr_idx) {
            entry_def = def_idx;
            break;
        }
    }
    const entry_def_idx = entry_def orelse return error.NoRootDefinition;

    trace.log("typed-cir -> monotype", .{});
    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, &resources.typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();
    const entry_symbol = try mono_lowerer.specializeTopLevelDef(0, entry_def_idx);
    var mono = try mono_lowerer.run(0);
    defer mono.deinit();
    debugValidateMonotypeTypes(&mono.types);

    trace.log("monotype -> monotype_lifted", .{});
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    defer lifted.deinit();
    trace.log("monotype_lifted -> lambdasolved", .{});
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    defer solved.deinit();
    trace.log("lambdasolved -> lambdamono", .{});
    var executable = try lambdamono.Lower.runWithEntrypoints(allocator, &solved, &.{entry_symbol});
    defer executable.deinit();
    const runtime_entry_symbol = if (executable.entrypoint_wrappers.len != 0 and !executable.entrypoint_wrappers[0].isNone())
        executable.entrypoint_wrappers[0]
    else
        entry_symbol;
    trace.log("lambdamono -> ir", .{});
    var lowered_ir = try ir.Lower.run(allocator, &executable);
    defer lowered_ir.deinit();

    trace.log("ir -> lir", .{});
    var lowered_lir = try FromIr.run(
        allocator,
        module_envs,
        null,
        target_usize,
        &lowered_ir,
    );
    errdefer lowered_lir.deinit();
    try lir.Ownership.inferProcResultContracts(allocator, &lowered_lir.store, &lowered_lir.layouts);
    try lir.RcInsert.run(allocator, &lowered_lir.store, &lowered_lir.layouts);
    try lir.SharedSwitchTail.run(allocator, &lowered_lir.store);

    const proc_id = lowered_lir.proc_ids_by_symbol.get(runtime_entry_symbol.raw()) orelse return error.NoRootProc;
    return .{
        .lir_result = lowered_lir,
        .main_proc = proc_id,
        .target_usize = target_usize,
    };
}

/// Public function `cleanupCheckedModule`.
pub fn cleanupCheckedModule(allocator: std.mem.Allocator, module: CheckedModule) void {
    module.checker.deinit();
    module.can.deinit();
    module.parse_ast.deinit();
    allocator.free(module.imported_envs);
    if (!module.published_owns_module_env) {
        module.module_env.deinit();
        if (module.owned_source) |owned_source| allocator.free(owned_source);
        allocator.destroy(module.module_env);
    }
    allocator.destroy(module.checker);
    allocator.destroy(module.can);
}

fn makeModuleSource(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    inspect_wrap: bool,
) ![]u8 {
    return switch (source_kind) {
        .expr => if (inspect_wrap)
            std.fmt.allocPrint(allocator, "main = Str.inspect(({s}))", .{source})
        else
            std.fmt.allocPrint(allocator, "main = {s}", .{source}),
        .module => if (inspect_wrap)
            std.fmt.allocPrint(
                allocator,
                "{s}\n\ncodex_test_inspect_main = Str.inspect(main)\n",
                .{source},
            )
        else
            allocator.dupe(u8, source),
    };
}

fn debugValidateMonotypeTypes(types_store: *const monotype.Type.Store) void {
    if (builtin.mode != .Debug) return;
    if (comptime builtin.target.os.tag == .freestanding) return;
    const type_len = types_store.types.items.len;
    for (types_store.types.items, 0..) |ty, i| {
        switch (ty) {
            .placeholder => {},
            .unbd => {},
            .link => |target| {
                debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "link.target", target, type_len);
            },
            .nominal => |nominal| {
                for (nominal.args) |arg| {
                    debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "nominal.arg", arg, type_len);
                }
                debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "nominal.backing", nominal.backing, type_len);
            },
            .primitive => {},
            .func => |func| {
                for (func.args) |arg| {
                    debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "func.arg", arg, type_len);
                }
                debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "func.ret", func.ret, type_len);
            },
            .list => |elem| {
                debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "list.elem", elem, type_len);
            },
            .box => |elem| {
                debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "box.elem", elem, type_len);
            },
            .tuple => |tuple| for (tuple) |elem| {
                debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "tuple.elem", elem, type_len);
            },
            .record => |record| for (record.fields) |field| {
                debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "record.field", field.ty, type_len);
            },
            .tag_union => |tag_union| for (tag_union.tags) |tag| {
                for (tag.args) |arg| {
                    debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "tag.arg", arg, type_len);
                }
            },
        }
    }
}

fn debugAssertValidMonoTypeRef(
    owner: monotype.Type.TypeId,
    comptime label: []const u8,
    child: monotype.Type.TypeId,
    type_len: usize,
) void {
    if (comptime builtin.target.os.tag == .freestanding) return;
    if (@intFromEnum(child) >= type_len) {
        std.debug.print(
            "HELPER_MONO_BAD owner={d} label={s} child={d} len={d}\n",
            .{ @intFromEnum(owner), label, @intFromEnum(child), type_len },
        );
        @panic("monotype produced invalid type reference");
    }
}
