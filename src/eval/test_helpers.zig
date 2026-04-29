//! Shared eval test helpers routed through the checked-artifact lowering API.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const parse = @import("parse");
const builtins = @import("builtins");
const backend = @import("backend");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");
const lir = @import("lir");

const builtin_loading = @import("builtin_loading.zig");
const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("test/RuntimeHostEnv.zig");

const Allocator = std.mem.Allocator;
const Allocators = base.Allocators;
const Can = can.Can;
const Check = check.Check;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const ModuleSourceFile = check.TypedCIR.Modules.SourceModule;
const RocStr = builtins.str.RocStr;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
const LayoutStore = @import("layout").Store;
const LayoutIdx = @import("layout").Idx;

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
    statement_idx: ?CIR.Statement.Idx,
};

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

pub const ParsedResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    typed_cir_modules: check.TypedCIR.Modules,
    checked_artifact: check.CheckedArtifact.CheckedModuleArtifact,
    expr_idx: CIR.Expr.Idx,
    entry_def_idx: CIR.Def.Idx,
    builtin_module: builtin_loading.LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    imported_envs: []*const ModuleEnv,
    extra_modules: []CheckedModule,
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,

    pub fn deinit(self: *ParsedResources, allocator: Allocator) void {
        for (self.extra_modules) |module| cleanupCheckedModule(allocator, module);
        allocator.free(self.extra_modules);
        self.checker.deinit();
        self.can.deinit();
        self.parse_ast.deinit();
        self.checked_artifact.deinit(allocator);
        self.typed_cir_modules.deinit();
        self.builtin_module.deinit();
        allocator.free(self.imported_envs);
        allocator.destroy(self.checker);
        allocator.destroy(self.can);
    }
};

pub const LoweredProgram = lir.CheckedPipeline.LoweredProgram;

pub const CompiledProgram = struct {
    resources: ParsedResources,
    lowered: LoweredProgram,
    wasm_lowered: LoweredProgram,

    pub fn deinit(self: *CompiledProgram, allocator: Allocator) void {
        self.wasm_lowered.deinit();
        self.lowered.deinit();
        cleanupParseAndCanonical(allocator, self.resources);
    }
};

pub const CompiledInspectedExpr = CompiledProgram;

pub fn parseAndCanonicalizeProgram(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
}

pub fn parseAndCanonicalizeExpr(allocator: Allocator, source: []const u8) !ParsedResources {
    return parseAndCanonicalizeProgram(allocator, .expr, source, &.{});
}

pub fn compileProgram(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !CompiledProgram {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLir(allocator, &resources, .native);
    errdefer {
        var owned = lowered;
        owned.deinit();
    }

    const wasm_lowered = try lowerParsedProgramToLir(allocator, &resources, .u32);
    errdefer {
        var owned = wasm_lowered;
        owned.deinit();
    }

    return .{
        .resources = resources,
        .lowered = lowered,
        .wasm_lowered = wasm_lowered,
    };
}

pub fn compileInspectedProgram(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !CompiledProgram {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, true);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLir(allocator, &resources, .native);
    errdefer {
        var owned = lowered;
        owned.deinit();
    }

    const wasm_lowered = try lowerParsedProgramToLir(allocator, &resources, .u32);
    errdefer {
        var owned = wasm_lowered;
        owned.deinit();
    }

    return .{
        .resources = resources,
        .lowered = lowered,
        .wasm_lowered = wasm_lowered,
    };
}

pub fn compileInspectedExpr(allocator: Allocator, source: []const u8) !CompiledInspectedExpr {
    return compileInspectedProgram(allocator, .expr, source, &.{});
}

pub fn cleanupParseAndCanonical(allocator: Allocator, resources: ParsedResources) void {
    var owned = resources;
    owned.deinit(allocator);
}

pub fn parseAndCanonicalizeProgramWrapped(
    allocator: Allocator,
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
                .statement_idx = null,
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
            .statement_idx = null,
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
    if (defs.len == 0) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("eval helper invariant violated: temporary module has no entry definition", .{});
        }
        unreachable;
    }
    const entry_def_idx = defs[defs.len - 1];
    const expr_idx = main_checked.module_env.store.getDef(entry_def_idx).expr;

    var all_module_envs = try allocator.alloc(*ModuleEnv, extra_modules.items.len + 2);
    defer allocator.free(all_module_envs);
    all_module_envs[0] = main_checked.module_env;
    all_module_envs[1] = builtin_module.env;
    for (extra_modules.items, 0..) |extra, i| {
        all_module_envs[i + 2] = extra.module_env;
    }
    resolveImportsByModuleIndex(all_module_envs);

    var source_modules = try allocator.alloc(ModuleSourceFile, extra_modules.items.len + 2);
    defer allocator.free(source_modules);
    source_modules[0] = .{ .owned_checked = .{
        .env = main_checked.module_env,
        .owned_source = main_checked.owned_source,
    } };
    source_modules[1] = .{ .precompiled = builtin_module.env };
    for (extra_modules.items, 0..) |extra, i| {
        source_modules[i + 2] = .{ .owned_checked = .{
            .env = extra.module_env,
            .owned_source = extra.owned_source,
        } };
    }

    const typed_cir_modules = try check.TypedCIR.Modules.init(allocator, source_modules);
    errdefer {
        var owned_modules = typed_cir_modules;
        owned_modules.deinit();
    }
    main_checked.published_owns_module_env = true;
    main_checked.owned_source = null;
    for (extra_modules.items) |*extra| {
        extra.published_owns_module_env = true;
        extra.owned_source = null;
    }

    var checked_artifact = try check.CheckedArtifact.publishFromTypedModule(allocator, &typed_cir_modules, 0);
    errdefer checked_artifact.deinit(allocator);

    return .{
        .module_env = main_checked.module_env,
        .parse_ast = main_checked.parse_ast,
        .can = main_checked.can,
        .checker = main_checked.checker,
        .typed_cir_modules = typed_cir_modules,
        .checked_artifact = checked_artifact,
        .expr_idx = expr_idx,
        .entry_def_idx = entry_def_idx,
        .builtin_module = builtin_module,
        .builtin_indices = builtin_indices,
        .imported_envs = main_checked.imported_envs,
        .extra_modules = try extra_modules.toOwnedSlice(allocator),
        .parse_ns = main_checked.parse_ns,
        .canonicalize_ns = main_checked.canonicalize_ns,
        .typecheck_ns = main_checked.typecheck_ns,
    };
}

pub fn parseCheckModule(
    allocator: Allocator,
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
    var parse_timer = try std.time.Timer.start();
    const parse_ast = try parse.parse(&allocators, &module_env.common);
    parse_elapsed = parse_timer.read();
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

    var can_timer = try std.time.Timer.start();
    try czer.canonicalizeFile();
    if (hosted_transform) {
        var modified_defs = try can.HostedCompiler.replaceAnnoOnlyWithHosted(module_env);
        defer modified_defs.deinit(module_env.gpa);
        var hosted_fns = try can.HostedCompiler.collectAndSortHostedFunctions(module_env);
        defer {
            for (hosted_fns.items) |hosted_fn| allocator.free(hosted_fn.name_text);
            hosted_fns.deinit(module_env.gpa);
        }
        try can.HostedCompiler.assignHostedIndices(module_env, hosted_fns.items);
    }
    const can_elapsed = can_timer.read();

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
    resolveImportsConst(module_env, imported_envs);

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
    var check_timer = try std.time.Timer.start();
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

fn lowerParsedProgramToLir(
    allocator: Allocator,
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

    var root_requests = [_]check.CheckedArtifact.RootRequest{.{
        .order = 0,
        .module_idx = 0,
        .kind = .dev_expr,
        .source = .{ .def = resources.entry_def_idx },
        .checked_type = resources.typed_cir_modules.module(0).defType(resources.entry_def_idx),
        .abi = .roc,
        .exposure = .private,
    }};

    return lir.CheckedPipeline.lowerArtifactsToLir(
        allocator,
        .{ .root = check.CheckedArtifact.loweringView(&resources.checked_artifact) },
        .{ .requests = &root_requests },
        .{
            .module_envs = module_envs,
            .target_usize = target_usize,
        },
    );
}

fn cleanupCheckedModule(allocator: Allocator, module: CheckedModule) void {
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
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    inspect_wrap: bool,
) ![]u8 {
    return switch (source_kind) {
        .expr => if (inspect_wrap)
            std.fmt.allocPrint(allocator, "main = || Str.inspect(({s}))", .{source})
        else
            std.fmt.allocPrint(allocator, "main = || ({s})", .{source}),
        .module => if (inspect_wrap)
            std.fmt.allocPrint(allocator, "{s}\n\ncodex_test_inspect_main = || Str.inspect(main)\n", .{source})
        else
            allocator.dupe(u8, source),
    };
}

fn resolveImportsByModuleIndex(module_envs: []const *ModuleEnv) void {
    for (module_envs) |module_env| {
        module_env.imports.clearResolvedModules();
        for (module_env.imports.imports.items.items, 0..) |str_idx, i| {
            const import_name = module_env.getString(str_idx);
            for (module_envs, 0..) |candidate_env, module_idx| {
                if (std.mem.eql(u8, candidate_env.module_name, import_name)) {
                    module_env.imports.setResolvedModule(@enumFromInt(i), @intCast(module_idx));
                    break;
                }
            }
        }
    }
}

fn resolveImportsConst(module_env: *ModuleEnv, imported_envs: []const *const ModuleEnv) void {
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
}

pub fn mainProcArgLayouts(allocator: Allocator, lowered: *const LoweredProgram) ![]LayoutIdx {
    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const arg_locals = lowered.lir_result.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(LayoutIdx, arg_locals.len);
    for (arg_locals, 0..) |local_id, i| {
        arg_layouts[i] = lowered.lir_result.store.getLocal(local_id).layout_idx;
    }
    return arg_layouts;
}

pub fn entrypointParamSlotSize(lowered: *const LoweredProgram, layout_idx: LayoutIdx) u32 {
    const layouts = &lowered.lir_result.layouts;
    const runtime_layout_idx = layouts.runtimeRepresentationLayoutIdx(layout_idx);
    if (runtime_layout_idx == .str) return 24;
    if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) return 16;

    if (@intFromEnum(runtime_layout_idx) < layouts.layouts.len()) {
        const layout_val = layouts.getLayout(runtime_layout_idx);
        const size = layouts.layoutSizeAlign(layout_val).size;
        if (layout_val.tag == .zst or size == 0) return 0;
        if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 24;
        if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
            if (size > 8) return @intCast(std.mem.alignForward(u32, size, 8));
        }
    }

    const size = layouts.layoutSizeAlign(layouts.getLayout(layout_idx)).size;
    return if (size == 0) 0 else 8;
}

pub fn zeroedEntrypointArgBuffer(
    allocator: Allocator,
    lowered: *const LoweredProgram,
    arg_layouts: []const LayoutIdx,
) !?[]align(collections.max_roc_alignment.toByteUnits()) u8 {
    const EntrypointArgOrder = struct {
        index: usize,
        alignment: u32,
        size: u32,
    };

    const arg_offsets = try allocator.alloc(u32, arg_layouts.len);
    defer allocator.free(arg_offsets);
    if (arg_layouts.len != 0) {
        const ordered = try allocator.alloc(EntrypointArgOrder, arg_layouts.len);
        defer allocator.free(ordered);

        for (arg_layouts, 0..) |arg_layout, i| {
            const size_align = lowered.lir_result.layouts.layoutSizeAlign(
                lowered.lir_result.layouts.getLayout(arg_layout),
            );
            const slot_size = entrypointParamSlotSize(lowered, arg_layout);
            ordered[i] = .{
                .index = i,
                .alignment = @intCast(size_align.alignment.toByteUnits()),
                .size = slot_size,
            };
        }

        const SortCtx = struct {
            fn lessThan(_: void, lhs: EntrypointArgOrder, rhs: EntrypointArgOrder) bool {
                if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
                return lhs.index < rhs.index;
            }
        };

        std.mem.sort(EntrypointArgOrder, ordered, {}, SortCtx.lessThan);

        var current_offset: u32 = 0;
        for (ordered) |arg| {
            current_offset = std.mem.alignForward(u32, current_offset, arg.alignment);
            arg_offsets[arg.index] = current_offset;
            current_offset += arg.size;
        }
    }

    var total_size: usize = 0;
    for (arg_layouts, 0..) |arg_layout, i| {
        total_size = @max(total_size, @as(usize, arg_offsets[i]) + entrypointParamSlotSize(lowered, arg_layout));
    }

    if (total_size == 0) return null;

    const buffer = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(total_size, 1));
    @memset(buffer, 0);
    return buffer;
}

pub fn lirInterpreterInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) ![]u8 {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const result = interp.eval(.{
        .proc_id = lowered.main_proc,
        .arg_layouts = arg_layouts,
    }) catch |err| switch (err) {
        error.RuntimeError => return error.Crash,
        error.Crash => return error.Crash,
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

pub fn devEvaluatorInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) ![]u8 {
    var codegen = try HostLirCodeGen.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        null,
    );
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());

    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);
    const entrypoint = try codegen.generateEntrypointWrapper(
        "roc_eval_test_main",
        lowered.main_proc,
        arg_layouts,
        proc.ret_layout,
    );
    var exec_mem = try ExecutableMemory.initWithEntryOffset(
        codegen.getGeneratedCode(),
        entrypoint.offset,
    );
    defer exec_mem.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const arg_buffer = try zeroedEntrypointArgBuffer(allocator, lowered, arg_layouts);
    defer if (arg_buffer) |buf| allocator.free(buf);

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

    exec_mem.callRocABI(
        @ptrCast(runtime_env.get_ops()),
        @ptrCast(ret_buf.ptr),
        if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
    );
    switch (runtime_env.crashState()) {
        .did_not_crash => {},
        .crashed => return error.Crash,
    }

    return copyReturnedRocStr(
        allocator,
        &lowered.lir_result.layouts,
        ret_layout,
        ret_buf.ptr,
        runtime_env.get_ops(),
    );
}

pub fn wasmEvaluatorInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) ![]u8 {
    if (@import("builtin").target.os.tag == .freestanding) return error.WasmExecFailed;
    var codegen = backend.wasm.WasmCodeGen.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
    );
    defer codegen.deinit();

    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const wasm_result = codegen.generateModule(lowered.main_proc, proc.ret_layout) catch return error.OutOfMemory;
    defer allocator.free(wasm_result.wasm_bytes);

    return @import("wasm_runner.zig").runWasmStr(allocator, wasm_result.wasm_bytes, wasm_result.has_imports);
}

fn copyReturnedRocStr(
    allocator: Allocator,
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
