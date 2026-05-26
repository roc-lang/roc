//! Shared eval test helpers routed through the checked-artifact lowering API.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const builtin = @import("builtin");
const build_options = @import("build_options");
const parse = @import("parse");
const builtins = @import("builtins");
const backend = @import("backend");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");
const lir = @import("lir");

const builtin_loading = @import("builtin_loading.zig");
const CompileTimeFinalization = @import("compile_time_finalization.zig");
const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("test/RuntimeHostEnv.zig");

const Allocator = std.mem.Allocator;
const Allocators = base.Allocators;
const Can = can.Can;
const Check = check.Check;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const RocStr = builtins.str.RocStr;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
const LayoutStore = @import("layout").Store;
const LayoutIdx = @import("layout").Idx;
const LirProcSpecId = lir.LirProcSpecId;
const LirImage = lir.LirImage;

/// Captures an eval backend's string output and host allocation count.
pub const EvalRunResult = struct {
    output: []u8,
    allocation_count: u32,
};
const SharedMemoryAllocator = if (builtin.target.os.tag == .freestanding) struct {
    base_ptr: [*]align(1) u8,
    buffer: []align(collections.max_roc_alignment.toByteUnits()) u8,
    fixed_buffer: std.heap.FixedBufferAllocator,
    page_size: usize,

    fn getSystemPageSize() !usize {
        return 64 * 1024;
    }

    fn create(size: usize, page_size: usize) !@This() {
        const aligned_size = std.mem.alignForward(usize, size, page_size);
        const buffer = try std.heap.wasm_allocator.alignedAlloc(
            u8,
            collections.max_roc_alignment,
            aligned_size,
        );
        errdefer std.heap.wasm_allocator.free(buffer);

        return .{
            .base_ptr = @ptrCast(buffer.ptr),
            .buffer = buffer,
            .fixed_buffer = std.heap.FixedBufferAllocator.init(buffer),
            .page_size = page_size,
        };
    }

    fn deinit(self: *@This(), _: Allocator) void {
        std.heap.wasm_allocator.free(self.buffer);
    }

    fn allocator(self: *@This()) Allocator {
        return self.fixed_buffer.allocator();
    }

    fn getUsedSize(self: *const @This()) usize {
        return self.fixed_buffer.end_index;
    }

    fn updateHeader(_: *@This()) void {}
} else @import("ipc").SharedMemoryAllocator;

const StageTimer = if (builtin.target.os.tag == .freestanding) struct {
    fn start() !@This() {
        return .{};
    }

    fn read(_: *@This()) u64 {
        return 0;
    }
} else std.time.Timer;

/// Public `SourceKind` declaration.
pub const SourceKind = enum {
    expr,
    module,
};

/// Public `ModuleSource` declaration.
pub const ModuleSource = struct {
    name: []const u8,
    source: []const u8,
};

const AvailableImport = struct {
    name: []const u8,
    env: *const ModuleEnv,
    statement_idx: ?CIR.Statement.Idx,
};

const ModuleValidation = enum {
    roc_check,
    checked_artifact,
};

/// Public `CheckedModule` declaration.
pub const CheckedModule = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    imported_envs: []*const ModuleEnv,
    auto_imported_types: *std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
    owned_source: ?[]u8 = null,
    published_owns_module_env: bool = false,
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,
};

/// Public `ProblemResources` declaration.
pub const ProblemResources = struct {
    main: CheckedModule,
    builtin_module: builtin_loading.LoadedModule,
    extra_modules: []CheckedModule,

    pub fn deinit(self: *ProblemResources, allocator: Allocator) void {
        cleanupCheckedModule(allocator, self.main);
        for (self.extra_modules) |module| cleanupCheckedModule(allocator, module);
        allocator.free(self.extra_modules);
        self.builtin_module.deinit();
    }
};

/// Public `ParsedResources` declaration.
pub const ParsedResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    checked_artifact: check.CheckedArtifact.CheckedModuleArtifact,
    import_artifacts: []check.CheckedArtifact.CheckedModuleArtifact,
    builtin_module: builtin_loading.LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    imported_envs: []*const ModuleEnv,
    auto_imported_types: *std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
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
        for (self.import_artifacts) |*artifact| artifact.deinit(allocator);
        allocator.free(self.import_artifacts);
        allocator.free(self.imported_envs);
        self.auto_imported_types.deinit();
        allocator.destroy(self.auto_imported_types);
        allocator.destroy(self.checker);
        allocator.destroy(self.can);
    }
};

// Per-test shared-memory reservation. Eval tests are small — most need a few
// MB at most. The huge values that follow are mostly to ensure the runtime
// image can grow if a test happens to construct large data; they are
// reservations, not commitments. On Windows the reservation cost matters for
// throughput because every parallel worker reserves its own region: keeping
// it modest (1 GB) lets MapViewOfFile complete quickly and lets us scale to
// many workers without tripping system address-space accounting.
const EVAL_SHARED_MEMORY_SIZE: usize = if (builtin.target.os.tag == .freestanding)
    8 * 1024 * 1024
else if (build_options.has_shared_memory_size)
    configuredSharedMemorySize()
else if (@sizeOf(usize) < 8)
    256 * 1024 * 1024
else if (builtin.os.tag == .macos)
    8 * 1024 * 1024 * 1024
else if (builtin.os.tag == .windows)
    256 * 1024 * 1024 // 256 MB on Windows — reservation cost matters for parallel workers
else
    2 * 1024 * 1024 * 1024 * 1024;

fn configuredSharedMemorySize() usize {
    if (comptime build_options.shared_memory_size > std.math.maxInt(usize)) {
        @compileError("-Dshared-memory-size does not fit in usize for this target");
    }

    return @intCast(build_options.shared_memory_size);
}

/// Public `LirImageProgram` declaration.
pub const LirImageProgram = struct {
    shm: SharedMemoryAllocator,
    image_header: *LirImage.Header,
    view: LirImage.ProgramView,

    /// First explicit LIR root for eval helpers. The root set was selected by
    /// checked-artifact publication and lowering; runtime evaluators must not
    /// rediscover roots from compiler data.
    pub fn mainProc(self: *const LirImageProgram) LirProcSpecId {
        if (self.view.root_procs.len == 0) {
            if (builtin.mode == .Debug) {
                std.debug.panic("eval LIR image invariant violated: no root procedures", .{});
            }
            unreachable;
        }
        return self.view.root_procs[0];
    }

    pub fn deinit(self: *LirImageProgram, allocator: Allocator) void {
        self.shm.deinit(allocator);
    }
};

/// Public `LoweredProgram` declaration.
pub const LoweredProgram = LirImageProgram;

/// Public `CompiledProgram` declaration.
pub const CompiledProgram = struct {
    resources: ParsedResources,
    lowered: LoweredProgram,
    wasm_lowered: LoweredProgram,

    pub fn deinit(self: *CompiledProgram, allocator: Allocator) void {
        self.wasm_lowered.deinit(allocator);
        self.lowered.deinit(allocator);
        cleanupParseAndCanonical(allocator, self.resources);
    }
};

/// Public `CompiledTargetProgram` declaration.
pub const CompiledTargetProgram = struct {
    resources: ParsedResources,
    lowered: LoweredProgram,

    pub fn deinit(self: *CompiledTargetProgram, allocator: Allocator) void {
        self.lowered.deinit(allocator);
        cleanupParseAndCanonical(allocator, self.resources);
    }
};

/// Public `CompiledInspectedExpr` declaration.
pub const CompiledInspectedExpr = CompiledProgram;

/// Public `parseAndCanonicalizeProgram` function.
pub fn parseAndCanonicalizeProgram(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
}

/// Public `parseAndCanonicalizeExpr` function.
pub fn parseAndCanonicalizeExpr(allocator: Allocator, source: []const u8) !ParsedResources {
    return parseAndCanonicalizeProgram(allocator, .expr, source, &.{});
}

/// Public `parseAndCheckProgramForProblems` function.
pub fn parseAndCheckProgramForProblems(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !ProblemResources {
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
            .checked_artifact,
            &.{},
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

    const main_checked = try parseCheckModule(
        allocator,
        "Test",
        source_kind,
        source,
        false,
        false,
        .checked_artifact,
        &.{},
        builtin_module.env,
        builtin_indices,
        main_imports,
    );
    errdefer cleanupCheckedModule(allocator, main_checked);

    var all_module_envs = try allocator.alloc(*ModuleEnv, extra_modules.items.len + 2);
    defer allocator.free(all_module_envs);
    all_module_envs[0] = main_checked.module_env;
    all_module_envs[1] = builtin_module.env;
    for (extra_modules.items, 0..) |extra, i| {
        all_module_envs[i + 2] = extra.module_env;
    }
    resolveImportsByModuleIndex(all_module_envs);

    return .{
        .main = main_checked,
        .builtin_module = builtin_module,
        .extra_modules = try extra_modules.toOwnedSlice(allocator),
    };
}

/// Public `compileProgram` function.
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
        owned.deinit(allocator);
    }

    const wasm_lowered = try lowerParsedProgramToLir(allocator, &resources, .u32);
    errdefer {
        var owned = wasm_lowered;
        owned.deinit(allocator);
    }

    return .{
        .resources = resources,
        .lowered = lowered,
        .wasm_lowered = wasm_lowered,
    };
}

/// Public `compileProgramForTarget` function.
pub fn compileProgramForTarget(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    target_usize: base.target.TargetUsize,
) !CompiledTargetProgram {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLir(allocator, &resources, target_usize);
    errdefer {
        var owned = lowered;
        owned.deinit(allocator);
    }

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

/// Public `compileInspectedProgram` function.
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
        owned.deinit(allocator);
    }

    const wasm_lowered = try lowerParsedProgramToLir(allocator, &resources, .u32);
    errdefer {
        var owned = wasm_lowered;
        owned.deinit(allocator);
    }

    return .{
        .resources = resources,
        .lowered = lowered,
        .wasm_lowered = wasm_lowered,
    };
}

/// Public `compileInspectedProgramForTarget` function.
pub fn compileInspectedProgramForTarget(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    target_usize: base.target.TargetUsize,
) !CompiledTargetProgram {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, true);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLir(allocator, &resources, target_usize);
    errdefer {
        var owned = lowered;
        owned.deinit(allocator);
    }

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

/// Public `compileInspectedExpr` function.
pub fn compileInspectedExpr(allocator: Allocator, source: []const u8) !CompiledInspectedExpr {
    return compileInspectedProgram(allocator, .expr, source, &.{});
}

/// Public `cleanupParseAndCanonical` function.
pub fn cleanupParseAndCanonical(allocator: Allocator, resources: ParsedResources) void {
    var owned = resources;
    owned.deinit(allocator);
}

/// Public `parseAndCanonicalizeProgramWrapped` function.
pub fn parseAndCanonicalizeProgramWrapped(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    inspect_wrap: bool,
) !ParsedResources {
    return parseAndCanonicalizeProgramWithRootMode(allocator, source_kind, source, imports, inspect_wrap, .{ .eval_root = inspect_wrap });
}

/// Public `parseAndCanonicalizeProgramPublishedRoots` function.
pub fn parseAndCanonicalizeProgramPublishedRoots(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !ParsedResources {
    return parseAndCanonicalizeProgramWithRootMode(allocator, source_kind, source, imports, false, .published_roots_only);
}

const PublishedRootMode = union(enum) {
    eval_root: bool,
    published_roots_only,
};

fn problemBlocksCheckedArtifact(problem: check.problem.Problem) bool {
    return switch (problem) {
        .redundant_pattern, .unmatchable_pattern => false,
        else => true,
    };
}

fn checkedModuleHasArtifactBlockingProblems(module: *const CheckedModule) bool {
    for (module.checker.problems.problems.items) |problem| {
        if (problemBlocksCheckedArtifact(problem)) return true;
    }
    return module.module_env.types.containsErrContent();
}

fn parseAndCanonicalizeProgramWithRootMode(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    inspect_wrap: bool,
    root_mode: PublishedRootMode,
) !ParsedResources {
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
    var builtin_module = try builtin_loading.loadCompiledModule(
        allocator,
        compiled_builtins.builtin_bin,
        "Builtin",
        compiled_builtins.builtin_source,
    );
    var builtin_module_owned_by_artifact = false;
    errdefer if (!builtin_module_owned_by_artifact) builtin_module.deinit();

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
            .checked_artifact,
            &.{},
            builtin_module.env,
            builtin_indices,
            available_imports,
        );
        if (checkedModuleHasArtifactBlockingProblems(&checked)) {
            cleanupCheckedModule(allocator, checked);
            return error.TypeCheckError;
        }
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

    var explicit_eval_root_names_storage: [1][]const u8 = undefined;
    var explicit_eval_root_names: []const []const u8 = &.{};
    switch (root_mode) {
        .eval_root => |root_inspect_wrap| {
            explicit_eval_root_names_storage[0] = evalRootName(source_kind, root_inspect_wrap);
            explicit_eval_root_names = explicit_eval_root_names_storage[0..];
        },
        .published_roots_only => {},
    }

    var main_checked = try parseCheckModule(
        allocator,
        "Test",
        source_kind,
        source,
        inspect_wrap,
        false,
        .checked_artifact,
        explicit_eval_root_names,
        builtin_module.env,
        builtin_indices,
        main_imports,
    );
    errdefer cleanupCheckedModule(allocator, main_checked);
    if (checkedModuleHasArtifactBlockingProblems(&main_checked)) {
        return error.TypeCheckError;
    }

    var all_module_envs = try allocator.alloc(*ModuleEnv, extra_modules.items.len + 2);
    defer allocator.free(all_module_envs);
    all_module_envs[0] = main_checked.module_env;
    all_module_envs[1] = builtin_module.env;
    for (extra_modules.items, 0..) |extra, i| {
        all_module_envs[i + 2] = extra.module_env;
    }
    resolveImportsByModuleIndex(all_module_envs);

    var source_modules = try allocator.alloc(check.TypedCIR.Modules.SourceModule, extra_modules.items.len + 2);
    defer allocator.free(source_modules);
    source_modules[0] = .{ .precompiled = main_checked.module_env };
    source_modules[1] = .{ .precompiled = builtin_module.env };
    for (extra_modules.items, 0..) |extra, i| {
        source_modules[i + 2] = .{ .precompiled = extra.module_env };
    }

    var typed_cir_modules = try check.TypedCIR.Modules.init(allocator, source_modules);
    defer typed_cir_modules.deinit();
    const import_artifacts = try publishImportArtifacts(
        allocator,
        &typed_cir_modules,
        &builtin_module,
        extra_modules.items,
        &builtin_module_owned_by_artifact,
    );
    errdefer {
        for (import_artifacts) |*artifact| artifact.deinit(allocator);
        allocator.free(import_artifacts);
    }

    const publish_imports = try publishImportKeys(allocator, import_artifacts);
    defer allocator.free(publish_imports);

    var explicit_root_storage: [1]check.CheckedArtifact.ExplicitRootRequestInput = undefined;
    var explicit_roots: []const check.CheckedArtifact.ExplicitRootRequestInput = &.{};
    switch (root_mode) {
        .eval_root => |root_inspect_wrap| {
            const root_name = evalRootName(source_kind, root_inspect_wrap);
            const root_def_idx = main_checked.can.explicitRootDefByName(root_name) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic("eval helper invariant violated: explicit eval root `{s}` was not found", .{root_name});
                }
                unreachable;
            };
            explicit_root_storage[0] = .{
                .kind = .dev_expr,
                .source = .{ .def = root_def_idx },
                .abi = .roc,
                .exposure = .private,
            };
            explicit_roots = explicit_root_storage[0..];
        },
        .published_roots_only => {},
    }

    var checked_artifact = try check.CheckedArtifact.publishFromTypedModule(
        allocator,
        &typed_cir_modules,
        0,
        .{
            .module_env_storage = .{ .checked_source = main_checked.module_env },
            .imports = publish_imports,
            .explicit_roots = explicit_roots,
            .compile_time_finalizer = CompileTimeFinalization.finalizer(),
        },
    );
    errdefer checked_artifact.deinit(allocator);
    main_checked.published_owns_module_env = true;
    main_checked.owned_source = null;

    return .{
        .module_env = main_checked.module_env,
        .parse_ast = main_checked.parse_ast,
        .can = main_checked.can,
        .checker = main_checked.checker,
        .checked_artifact = checked_artifact,
        .import_artifacts = import_artifacts,
        .builtin_module = builtin_module,
        .builtin_indices = builtin_indices,
        .imported_envs = main_checked.imported_envs,
        .auto_imported_types = main_checked.auto_imported_types,
        .extra_modules = try extra_modules.toOwnedSlice(allocator),
        .parse_ns = main_checked.parse_ns,
        .canonicalize_ns = main_checked.canonicalize_ns,
        .typecheck_ns = main_checked.typecheck_ns,
    };
}

/// Public `parseCheckModule` function.
pub fn parseCheckModule(
    allocator: Allocator,
    module_name: []const u8,
    source_kind: SourceKind,
    source: []const u8,
    inspect_wrap: bool,
    hosted_transform: bool,
    validation: ModuleValidation,
    explicit_root_names: []const []const u8,
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
    var parse_timer = try StageTimer.start();
    const parse_ast = try parse.parse(&allocators, &module_env.common);
    parse_elapsed = parse_timer.read();
    errdefer {
        parse_ast.deinit();
        allocators.deinit();
    }
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
        .explicit_root_names = explicit_root_names,
    });
    errdefer czer.deinit();

    var can_timer = try StageTimer.start();
    try czer.canonicalizeFile();
    switch (validation) {
        .roc_check => try czer.validateForChecking(),
        .checked_artifact => try czer.validateForExplicitRoots(),
    }
    if (hosted_transform) {
        var modified_defs = try can.HostedCompiler.replaceAnnoOnlyWithHosted(module_env);
        defer modified_defs.deinit(module_env.gpa);
    }
    const can_elapsed = can_timer.read();

    const auto_imported_types = try allocator.create(std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType));
    errdefer allocator.destroy(auto_imported_types);
    auto_imported_types.* = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    errdefer auto_imported_types.deinit();
    try Can.populateModuleEnvs(auto_imported_types, module_env, builtin_module_env, builtin_indices);

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
        auto_imported_types,
        &module_env.store.regions,
        builtin_ctx,
    );
    checker.fixupTypeWriter();
    errdefer checker.deinit();
    var check_timer = try StageTimer.start();
    try checker.checkFile();
    const check_elapsed = check_timer.read();

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = czer,
        .checker = checker,
        .imported_envs = imported_envs,
        .auto_imported_types = auto_imported_types,
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
    return lowerCheckedModuleSetToLir(allocator, &resources.checked_artifact, resources.import_artifacts, target_usize);
}

/// Lower already-published checked modules to a LIR image.
pub fn lowerCheckedModuleSetToLir(
    allocator: Allocator,
    root_module: *check.CheckedArtifact.CheckedModuleArtifact,
    import_modules: []check.CheckedArtifact.CheckedModuleArtifact,
    target_usize: base.target.TargetUsize,
) !LoweredProgram {
    const import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, import_modules.len);
    defer allocator.free(import_views);
    for (import_modules, 0..) |*module, i| {
        import_views[i] = check.CheckedArtifact.importedView(module);
    }

    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(EVAL_SHARED_MEMORY_SIZE, page_size);
    errdefer shm.deinit(allocator);

    const shm_allocator = shm.allocator();
    const image_header = try shm_allocator.create(LirImage.Header);

    const lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        shm_allocator,
        .{
            .root = check.CheckedArtifact.loweringView(root_module),
            .imports = import_views,
        },
        .{ .requests = root_module.root_requests.runtime_requests },
        .{
            .target_usize = target_usize,
        },
    );

    try LirImage.fillHeaderInSharedMemory(
        image_header,
        shm.base_ptr,
        shm.getUsedSize(),
        &lowered.lir_result,
        lowered.target_usize,
        &.{},
    );
    shm.updateHeader();

    const view = try LirImage.viewMappedImage(image_header, shm.base_ptr, shm.getUsedSize());
    return .{
        .shm = shm,
        .image_header = image_header,
        .view = view,
    };
}

fn evalRootName(source_kind: SourceKind, inspect_wrap: bool) []const u8 {
    return switch (source_kind) {
        .expr => "main",
        .module => if (inspect_wrap) "codex_test_inspect_main" else "main",
    };
}

fn publishImportArtifacts(
    allocator: Allocator,
    typed_cir_modules: *const check.TypedCIR.Modules,
    builtin_module: *builtin_loading.LoadedModule,
    extra_modules: []CheckedModule,
    builtin_module_owned_by_artifact: *bool,
) ![]check.CheckedArtifact.CheckedModuleArtifact {
    const extra_module_count = extra_modules.len;
    var artifacts = std.ArrayList(check.CheckedArtifact.CheckedModuleArtifact).empty;
    errdefer {
        for (artifacts.items) |*artifact| artifact.deinit(allocator);
        artifacts.deinit(allocator);
    }

    var published_keys = std.ArrayList(check.CheckedArtifact.PublishImportArtifact).empty;
    defer published_keys.deinit(allocator);

    var builtin_artifact = try check.CheckedArtifact.publishFromTypedModule(
        allocator,
        typed_cir_modules,
        1,
        .{
            .module_env_storage = .{ .compiled_buffer = .{
                .env = builtin_module.env,
                .buffer = builtin_module.buffer,
            } },
            .compile_time_finalizer = CompileTimeFinalization.finalizer(),
        },
    );
    builtin_module_owned_by_artifact.* = true;
    published_keys.append(allocator, .{
        .module_idx = 1,
        .key = builtin_artifact.key,
        .view = check.CheckedArtifact.importedView(&builtin_artifact),
    }) catch |err| {
        builtin_artifact.deinit(allocator);
        return err;
    };
    artifacts.append(allocator, builtin_artifact) catch |err| {
        _ = published_keys.pop();
        builtin_artifact.deinit(allocator);
        return err;
    };

    if (extra_module_count == 0) return try artifacts.toOwnedSlice(allocator);

    const published_extra = try allocator.alloc(bool, extra_module_count);
    defer allocator.free(published_extra);
    @memset(published_extra, false);

    var remaining = extra_module_count;
    while (remaining != 0) {
        var made_progress = false;

        for (0..extra_module_count) |extra_i| {
            if (published_extra[extra_i]) continue;

            const module_idx: u32 = @intCast(extra_i + 2);
            if (!directImportsArePublished(typed_cir_modules.module(module_idx), published_keys.items)) continue;

            var artifact = try check.CheckedArtifact.publishFromTypedModule(
                allocator,
                typed_cir_modules,
                module_idx,
                .{
                    .module_env_storage = .{ .checked_source = extra_modules[extra_i].module_env },
                    .imports = published_keys.items,
                    .compile_time_finalizer = CompileTimeFinalization.finalizer(),
                },
            );
            extra_modules[extra_i].published_owns_module_env = true;
            extra_modules[extra_i].owned_source = null;

            published_keys.append(allocator, .{
                .module_idx = module_idx,
                .key = artifact.key,
                .view = check.CheckedArtifact.importedView(&artifact),
            }) catch |err| {
                artifact.deinit(allocator);
                return err;
            };
            artifacts.append(allocator, artifact) catch |err| {
                _ = published_keys.pop();
                artifact.deinit(allocator);
                return err;
            };

            published_extra[extra_i] = true;
            remaining -= 1;
            made_progress = true;
        }

        if (!made_progress) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("eval helper invariant violated: import artifact publication graph is cyclic or incomplete", .{});
            }
            unreachable;
        }
    }

    return try artifacts.toOwnedSlice(allocator);
}

fn directImportsArePublished(
    module: check.TypedCIR.Module,
    published: []const check.CheckedArtifact.PublishImportArtifact,
) bool {
    const module_env = module.moduleEnvConst();
    for (module_env.imports.imports.items.items, 0..) |_, i| {
        const import_idx: CIR.Import.Idx = @enumFromInt(@as(u32, @intCast(i)));
        const resolved_module_idx = module.resolvedImportModule(import_idx) orelse continue;
        var found = false;
        for (published) |artifact| {
            if (artifact.module_idx == resolved_module_idx) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }
    return true;
}

fn publishImportKeys(
    allocator: Allocator,
    artifacts: []const check.CheckedArtifact.CheckedModuleArtifact,
) ![]check.CheckedArtifact.PublishImportArtifact {
    const imports = try allocator.alloc(check.CheckedArtifact.PublishImportArtifact, artifacts.len);
    for (artifacts, 0..) |artifact, i| {
        imports[i] = .{
            .module_idx = artifact.module_identity.module_idx,
            .key = artifact.key,
            .view = check.CheckedArtifact.importedView(&artifacts[i]),
        };
    }
    return imports;
}

fn cleanupCheckedModule(allocator: Allocator, module: CheckedModule) void {
    module.checker.deinit();
    module.can.deinit();
    module.parse_ast.deinit();
    allocator.free(module.imported_envs);
    module.auto_imported_types.deinit();
    allocator.destroy(module.auto_imported_types);
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
                if (base.Ident.textEql(candidate_env.module_name, import_name)) {
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
            if (base.Ident.textEql(candidate_env.module_name, import_name)) {
                module_env.imports.setResolvedModule(@enumFromInt(i), @intCast(module_idx));
                break;
            }
        }
    }
}

/// Public `mainProcArgLayouts` function.
pub fn mainProcArgLayouts(allocator: Allocator, lowered: *const LoweredProgram) ![]LayoutIdx {
    const proc = lowered.view.store.getProcSpec(lowered.mainProc());
    const arg_locals = lowered.view.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(LayoutIdx, arg_locals.len);
    for (arg_locals, 0..) |local_id, i| {
        arg_layouts[i] = lowered.view.store.getLocal(local_id).layout_idx;
    }
    return arg_layouts;
}

/// Public `entrypointParamSlotSize` function.
pub fn entrypointParamSlotSize(lowered: *const LoweredProgram, layout_idx: LayoutIdx) u32 {
    const layouts = &lowered.view.layouts;
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

/// Public `zeroedEntrypointArgBuffer` function.
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
            const size_align = lowered.view.layouts.layoutSizeAlign(
                lowered.view.layouts.getLayout(arg_layout),
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

/// Public `lirInterpreterInspectedStr` function.
pub fn lirInterpreterInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) ![]u8 {
    const result = try lirInterpreterStrWithStats(allocator, lowered);
    return result.output;
}

/// Public `lirInterpreterStrWithStats` function.
pub fn lirInterpreterStrWithStats(allocator: Allocator, lowered: *const LoweredProgram) !EvalRunResult {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.view.store,
        &lowered.view.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const result = interp.eval(.{
        .proc_id = lowered.mainProc(),
        .arg_layouts = arg_layouts,
    }) catch |err| switch (err) {
        error.RuntimeError => return error.Crash,
        error.Crash => return error.Crash,
        else => return err,
    };
    const ret_layout = lowered.view.store.getProcSpec(lowered.mainProc()).ret_layout;
    const output = try copyReturnedRocStr(
        allocator,
        &lowered.view.layouts,
        ret_layout,
        result.value.ptr,
        null,
    );
    return .{
        .output = output,
        .allocation_count = runtime_env.allocationCallCount(),
    };
}

/// Public `devEvaluatorInspectedStr` function.
pub fn devEvaluatorInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) ![]u8 {
    const result = try devEvaluatorStrWithStats(allocator, lowered);
    return result.output;
}

/// Public `devEvaluatorStrWithStats` function.
pub fn devEvaluatorStrWithStats(allocator: Allocator, lowered: *const LoweredProgram) !EvalRunResult {
    if (comptime !backend.host_lir_codegen_available) {
        return error.DevBackendUnavailable;
    } else {
        var codegen = try HostLirCodeGen.init(
            allocator,
            &lowered.view.store,
            &lowered.view.layouts,
            &.{},
        );
        defer codegen.deinit();
        try codegen.compileAllProcSpecs(lowered.view.store.getProcSpecs());

        const proc = lowered.view.store.getProcSpec(lowered.mainProc());
        const arg_layouts = try mainProcArgLayouts(allocator, lowered);
        defer allocator.free(arg_layouts);
        const entrypoint = try codegen.generateEntrypointWrapper(
            "roc_eval_test_main",
            lowered.mainProc(),
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
        const size_align = lowered.view.layouts.layoutSizeAlign(lowered.view.layouts.getLayout(ret_layout));
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

        const output = try copyReturnedRocStr(
            allocator,
            &lowered.view.layouts,
            ret_layout,
            ret_buf.ptr,
            runtime_env.get_ops(),
        );
        return .{
            .output = output,
            .allocation_count = runtime_env.allocationCallCount(),
        };
    }
}

/// Public `wasmEvaluatorInspectedStr` function.
pub fn wasmEvaluatorInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) ![]u8 {
    const result = try wasmEvaluatorStrWithStats(allocator, lowered);
    return result.output;
}

/// Public `wasmEvaluatorStrWithStats` function.
pub fn wasmEvaluatorStrWithStats(allocator: Allocator, lowered: *const LoweredProgram) !EvalRunResult {
    if (@import("builtin").target.os.tag == .freestanding) return error.WasmExecFailed;
    var codegen = backend.wasm.WasmCodeGen.init(
        allocator,
        &lowered.view.store,
        &lowered.view.layouts,
    );
    defer codegen.deinit();

    const proc = lowered.view.store.getProcSpec(lowered.mainProc());
    const wasm_result = codegen.generateModule(lowered.mainProc(), proc.ret_layout) catch return error.OutOfMemory;
    defer allocator.free(wasm_result.wasm_bytes);

    const result = try @import("wasm_runner.zig").runWasmStrWithStats(allocator, wasm_result.wasm_bytes, wasm_result.has_imports);
    return .{
        .output = result.output,
        .allocation_count = result.allocation_count,
    };
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
