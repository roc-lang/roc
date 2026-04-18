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

/// Public struct `LoweredProgram`.
pub const LoweredProgram = struct {
    lir_result: FromIr.Result,
    main_proc: lir.LIR.LirProcSpecId,
    target_usize: base.target.TargetUsize,

    pub fn deinit(self: *LoweredProgram) void {
        self.lir_result.deinit();
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
        module_env.imports.resolveImports(module_env, all_module_envs);
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
    module_env.imports.resolveImports(module_env, imported_envs);

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
    debugValidateMonotypeTypes(&mono.types);
    trace.log("monotype -> monotype_lifted", .{});
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    trace.log("monotype_lifted -> lambdasolved", .{});
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    trace.log("lambdasolved -> lambdamono", .{});
    var executable = try lambdamono.Lower.run(allocator, &solved);
    trace.log("lambdamono -> ir", .{});
    var lowered_ir = try ir.Lower.run(allocator, &executable);

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
    debugValidateMonotypeTypes(&mono.types);

    trace.log("monotype -> monotype_lifted", .{});
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    trace.log("monotype_lifted -> lambdasolved", .{});
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    trace.log("lambdasolved -> lambdamono", .{});
    var executable = try lambdamono.Lower.runWithEntrypoints(allocator, &solved, &.{entry_symbol});
    const runtime_entry_symbol = if (executable.entrypoint_wrappers.len != 0 and !executable.entrypoint_wrappers[0].isNone())
        executable.entrypoint_wrappers[0]
    else
        entry_symbol;
    trace.log("lambdamono -> ir", .{});
    var lowered_ir = try ir.Lower.run(allocator, &executable);

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
                debugAssertValidMonoTypeRef(@enumFromInt(@as(u32, @intCast(i))), "func.arg", func.arg, type_len);
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
