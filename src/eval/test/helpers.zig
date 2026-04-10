//! Shared eval test helpers built on the cor-style lowering pipeline.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const can = @import("can");
const check = @import("check");
const parse = @import("parse");
const builtins = @import("builtins");
const compiled_builtins = @import("compiled_builtins");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const lambdasolved = @import("lambdasolved");
const lambdamono = @import("lambdamono");
const ir = @import("ir");
const lir = @import("lir");
const backend = @import("backend");
const collections = @import("collections");
const builtin_loading = @import("../builtin_loading.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("RuntimeHostEnv.zig");

const Can = can.Can;
const HostedCompiler = can.HostedCompiler;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Allocators = base.Allocators;
const RocStr = builtins.str.RocStr;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
const FromIr = lir.FromIr;
const LayoutStore = @import("layout").Store;
const LayoutIdx = @import("layout").Idx;

pub const interpreter_allocator = std.heap.page_allocator;

pub const SourceKind = enum {
    expr,
    module,
};

pub const ModuleSource = struct {
    name: []const u8,
    source: []const u8,
};

const AvailableImport = struct {
    name: []const u8,
    env: *const ModuleEnv,
};

pub const CheckedModule = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    imported_envs: []*const ModuleEnv,
    owned_source: ?[]u8 = null,
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,
};

pub const ParsedResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    mir_modules: ?check.MIR.Modules = null,
    expr_idx: CIR.Expr.Idx,
    builtin_module: builtin_loading.LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    imported_envs: []*const ModuleEnv,
    extra_modules: []CheckedModule = &.{},
    owned_source: ?[]u8 = null,
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,
};

pub const LoweredProgram = struct {
    lir_result: FromIr.Result,
    main_proc: lir.LIR.LirProcSpecId,

    pub fn deinit(self: *LoweredProgram) void {
        self.lir_result.deinit();
    }
};

pub const CompiledProgram = struct {
    resources: ParsedResources,
    lowered: LoweredProgram,

    pub fn deinit(self: *CompiledProgram, allocator: std.mem.Allocator) void {
        self.lowered.deinit();
        cleanupParseAndCanonical(allocator, self.resources);
    }
};

pub const CompiledInspectedExpr = CompiledProgram;

pub fn allocInspectedExprSource(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "Str.inspect(({s}))", .{source});
}

pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    return parseAndCanonicalizeProgram(allocator, .expr, source, &.{});
}

pub fn parseAndCanonicalizeInspectedExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, .expr, source, &.{}, true);
}

pub fn compileInspectedExpr(allocator: std.mem.Allocator, source: []const u8) !CompiledInspectedExpr {
    return compileInspectedProgram(allocator, .expr, source, &.{});
}

pub fn compileProgram(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !CompiledProgram {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedExprToLir(allocator, &resources);
    errdefer {
        var lowered_mut = lowered;
        lowered_mut.deinit();
    }

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

pub fn parseAndCanonicalizeProgram(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
}

pub fn compileInspectedProgram(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !CompiledInspectedExpr {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, true);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedExprToLir(allocator, &resources);
    errdefer {
        var lowered_mut = lowered;
        lowered_mut.deinit();
    }

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

pub fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: ParsedResources) void {
    var resources_mut = resources;
    if (resources_mut.mir_modules) |*mir_modules| mir_modules.deinit();
    for (resources.extra_modules) |extra| {
        cleanupCheckedModule(allocator, extra);
    }
    allocator.free(resources.extra_modules);
    resources.checker.deinit();
    resources.can.deinit();
    resources.parse_ast.deinit();
    resources.module_env.deinit();
    allocator.free(resources.imported_envs);
    var builtin_module = resources.builtin_module;
    builtin_module.deinit();
    if (resources.owned_source) |source| allocator.free(source);
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
    allocator.destroy(resources.module_env);
}

pub fn lowerParsedExprToLir(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
) !LoweredProgram {
    return lowerToLir(allocator, resources);
}

pub fn lirInterpreterInspectedStr(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
) ![]u8 {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const result = interp.eval(.{ .proc_id = lowered.main_proc }) catch |err| switch (err) {
        error.RuntimeError => return error.Crash,
        else => return err,
    };
    const ret_layout = lowered.lir_result.store.getProcSpec(lowered.main_proc).ret_layout;
    return copyReturnedRocStr(
        allocator,
        &lowered.lir_result.layouts,
        ret_layout,
        result.value.ptr,
        null,
    );
}

pub fn devEvaluatorInspectedStr(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
) ![]u8 {
    var codegen = try HostLirCodeGen.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        null,
    );
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());

    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const entrypoint = try codegen.generateEntrypointWrapper(
        "roc_eval_test_main",
        lowered.main_proc,
        &.{},
        proc.ret_layout,
    );
    var exec_mem = try ExecutableMemory.initWithEntryOffset(
        codegen.getGeneratedCode(),
        entrypoint.offset,
    );
    defer exec_mem.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const ret_layout = proc.ret_layout;
    const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(ret_layout));
    const alloc_len = @max(size_align.size, 1);
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, alloc_len);
    defer allocator.free(ret_buf);
    @memset(ret_buf, 0);

    var crash_boundary = runtime_env.enterCrashBoundary();
    defer crash_boundary.deinit();
    const sj = crash_boundary.set();
    if (sj != 0) return error.Crash;

    exec_mem.callRocABI(@ptrCast(runtime_env.get_ops()), @ptrCast(ret_buf.ptr), null);

    switch (runtime_env.crashState()) {
        .did_not_crash => {},
        .crashed => |_| return error.Crash,
    }

    return copyReturnedRocStr(
        allocator,
        &lowered.lir_result.layouts,
        ret_layout,
        ret_buf.ptr,
        runtime_env.get_ops(),
    );
}

pub fn wasmEvaluatorInspectedStr(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
) ![]u8 {
    _ = allocator;
    _ = lowered;
    @panic("TODO: wasm eval backend not implemented for the cor-style inspect-only pipeline");
}

fn lowerToLir(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
) !LoweredProgram {
    const module_env = resources.module_env;
    const builtin_module_env = resources.builtin_module.env;
    const extra_modules = resources.extra_modules;

    var all_module_envs = try allocator.alloc(*const ModuleEnv, extra_modules.len + 2);
    defer allocator.free(all_module_envs);
    all_module_envs[0] = module_env;
    all_module_envs[1] = builtin_module_env;
    for (extra_modules, 0..) |extra, i| {
        all_module_envs[i + 2] = extra.module_env;
    }

    var mir_modules = resources.mir_modules orelse return error.MirNotPublished;
    resources.mir_modules = null;
    errdefer mir_modules.deinit();
    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, mir_modules, 1);
    defer mono_lowerer.deinit();
    const mono = try mono_lowerer.run(0);
    debugValidateMonotypeTypes(&mono.types);
    const lifted = try monotype_lifted.Lower.run(allocator, mono);
    const solved = try lambdasolved.Lower.run(allocator, lifted);
    const executable = try lambdamono.Lower.run(allocator, solved);
    const lowered_ir = try ir.Lower.run(allocator, executable);

    var lowered_lir = try FromIr.run(
        allocator,
        all_module_envs,
        null,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    errdefer lowered_lir.deinit();
    if (lowered_lir.root_procs.items.len == 0) return error.NoRootProc;
    return .{
        .lir_result = lowered_lir,
        .main_proc = lowered_lir.root_procs.items[lowered_lir.root_procs.items.len - 1],
    };
}

fn parseAndCanonicalizeProgramWrapped(
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
        };
    }

    const main_checked = try parseCheckModule(
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

    var mir_source_modules = try allocator.alloc(check.MIR.Modules.SourceModule, extra_modules.items.len + 2);
    defer allocator.free(mir_source_modules);
    mir_source_modules[0] = .{ .checked = main_checked.checker };
    mir_source_modules[1] = .{ .precompiled = builtin_module.env };
    for (extra_modules.items, 0..) |extra, i| {
        mir_source_modules[i + 2] = .{ .checked = extra.checker };
    }

    const mir_modules = try check.MIR.Modules.publish(allocator, mir_source_modules);
    errdefer {
        var mir_modules_mut = mir_modules;
        mir_modules_mut.deinit();
    }

    return .{
        .module_env = main_checked.module_env,
        .parse_ast = main_checked.parse_ast,
        .can = main_checked.can,
        .checker = main_checked.checker,
        .mir_modules = mir_modules,
        .expr_idx = expr_idx,
        .builtin_module = builtin_module,
        .builtin_indices = builtin_indices,
        .imported_envs = main_checked.imported_envs,
        .extra_modules = try extra_modules.toOwnedSlice(allocator),
        .owned_source = main_checked.owned_source,
        .parse_ns = main_checked.parse_ns,
        .canonicalize_ns = main_checked.canonicalize_ns,
        .typecheck_ns = main_checked.typecheck_ns,
    };
}

fn parseCheckModule(
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
    var parse_timer = try std.time.Timer.start();
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

    const parse_ast = try parse.parse(&allocators, &module_env.common);
    errdefer {
        parse_ast.deinit();
        allocators.deinit();
    }
    parse_ast.store.emptyScratch();
    if (parse_ast.tokenize_diagnostics.items.len > 0 or parse_ast.parse_diagnostics.items.len > 0) {
        return error.ParseError;
    }
    const parse_elapsed = parse_timer.read();

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
            .qualified_type_ident = qualified_ident,
        });
    }

    var can_timer = try std.time.Timer.start();
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
    const can_elapsed = can_timer.read();

    var check_timer = try std.time.Timer.start();
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
    try checker.checkFile();
    const check_elapsed = check_timer.read();

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

fn cleanupCheckedModule(allocator: std.mem.Allocator, module: CheckedModule) void {
    module.checker.deinit();
    module.can.deinit();
    module.parse_ast.deinit();
    module.module_env.deinit();
    allocator.free(module.imported_envs);
    if (module.owned_source) |owned_source| allocator.free(owned_source);
    allocator.destroy(module.checker);
    allocator.destroy(module.can);
    allocator.destroy(module.module_env);
}

fn debugValidateMonotypeTypes(types_store: *const monotype.Type.Store) void {
    const type_len = types_store.types.items.len;
    for (types_store.types.items, 0..) |ty, i| {
        switch (ty) {
            .placeholder => {},
            .unbd => {},
            .link => |target| {
                debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "link.target", target, type_len);
            },
            .nominal => |backing| {
                debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "nominal.backing", backing, type_len);
            },
            .primitive => {},
            .func => |func| {
                debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "func.arg", func.arg, type_len);
                debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "func.ret", func.ret, type_len);
            },
            .list => |elem| {
                debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "list.elem", elem, type_len);
            },
            .box => |elem| {
                debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "box.elem", elem, type_len);
            },
            .tuple => |tuple| for (types_store.sliceTypeSpan(tuple)) |elem| {
                debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "tuple.elem", elem, type_len);
            },
            .record => |record| for (types_store.sliceFields(record.fields)) |field| {
                debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "record.field", field.ty, type_len);
            },
            .tag_union => |tag_union| for (types_store.sliceTags(tag_union.tags)) |tag| {
                for (types_store.sliceTypeSpan(tag.args)) |arg| {
                    debugAssertValidMonoTypeRef(types_store, @enumFromInt(@as(u32, @intCast(i))), "tag.arg", arg, type_len);
                }
            },
        }
    }
}

fn debugAssertValidMonoTypeRef(
    types_store: *const monotype.Type.Store,
    owner: monotype.Type.TypeId,
    comptime label: []const u8,
    child: monotype.Type.TypeId,
    type_len: usize,
) void {
    _ = types_store;
    if (@intFromEnum(child) >= type_len) {
        std.debug.print(
            "HELPER_MONO_BAD owner={d} label={s} child={d} len={d}\n",
            .{ @intFromEnum(owner), label, @intFromEnum(child), type_len },
        );
        @panic("monotype produced invalid type reference");
    }
}

fn copyReturnedRocStr(
    allocator: std.mem.Allocator,
    layout_store: *const LayoutStore,
    ret_layout: LayoutIdx,
    value_ptr: [*]u8,
    roc_ops: ?*builtins.host_abi.RocOps,
) ![]u8 {
    const layout_val = layout_store.getLayout(ret_layout);
    const is_str =
        ret_layout == .str or
        (layout_val.tag == .scalar and layout_val.data.scalar.tag == .str);

    if (!is_str) {
        std.debug.panic(
            "eval inspect invariant violated: expected Str return layout, found {s}",
            .{@tagName(layout_val.tag)},
        );
    }

    const roc_str = @as(*align(1) const RocStr, @ptrCast(value_ptr)).*;
    const copied = try allocator.dupe(u8, roc_str.asSlice());
    if (roc_ops) |ops| roc_str.decref(ops);
    return copied;
}
