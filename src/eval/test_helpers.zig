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
const reporting = @import("reporting");

const builtin_static = can.BuiltinStatic;
const CompileTimeFinalization = @import("compile_time_finalization.zig");
const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("test/RuntimeHostEnv.zig");
const EvalDynLib = @import("dynlib.zig").DynLib;

const Allocator = std.mem.Allocator;
const CoreCtx = @import("ctx").CoreCtx;
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
const GuardedList = lir.LirStore.GuardedList;

/// Errors surfaced by shared eval test helpers.
pub const TestHelperError = Allocator.Error || std.Thread.SpawnError || std.DynLib.Error || std.Io.File.OpenError || std.Io.File.Reader.Error || std.Io.File.Writer.Error || std.Io.File.StatError || std.Io.File.ReadPositionalError || std.Io.Writer.Error || check.CheckedArtifact.CompileTimeFinalizer.Error || error{
    InvalidUtf8,
    LlvmBackendUnavailable,
    DevBackendUnavailable,
    WasmExecFailed,
    TypeCheckError,
    ParseError,
    CorruptEmbeddedBuiltins,
    Crash,
    RuntimeError,
    DivisionByZero,
    ComptimeExhaustiveness,
    ExpectErr,
    EvaluationFailed,
    EntrypointNotFound,
    InvalidLirImage,
    UnsupportedLirImageVersion,
    Internal,
    UnsupportedTarget,
    UnsupportedPlatform,
    UnwindRegistrationFailed,
    SysctlFailed,
    CreateFileMappingFailed,
    OpenFileMappingFailed,
    MapViewOfFileFailed,
    TempFileOpenFailed,
    TempFileUnlinkFailed,
    ShmOpenFailed,
    ShmUnlinkFailed,
    MemfdCreateFailed,
    FtruncateFailed,
    MmapFailed,
    EmptyCode,
    VirtualAllocFailed,
    MprotectFailed,
    VirtualProtectFailed,
    InvalidHandle,
    WindowsSDKNotFound,
    CompilationFailed,
    BitcodeParseError,
    ModuleLinkFailed,
    TempFileError,
    LinkFailed,
    UnsupportedLowLevel,
    TestExpectedEqual,
    TestUnexpectedResult,
};

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

    fn getSystemPageSize() TestHelperError!usize {
        return 64 * 1024;
    }

    fn create(_: anytype, size: usize, page_size: usize) TestHelperError!@This() {
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

    fn createWithMinSize(_: std.Io, preferred_size: usize, _: usize, page_size: usize) TestHelperError!@This() {
        return create({}, preferred_size, page_size);
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

/// Monotonic stage timer (std.time.Timer was removed in Zig 0.16).
const StageTimer = if (builtin.target.os.tag == .freestanding) struct {
    fn start() TestHelperError!@This() {
        return .{};
    }

    fn read(_: *@This()) u64 {
        return 0;
    }
} else struct {
    start_ns: u64,

    fn start() error{}!@This() {
        return .{ .start_ns = readNs() };
    }

    fn read(self: *@This()) u64 {
        return readNs() - self.start_ns;
    }

    fn readNs() u64 {
        if (builtin.os.tag == .windows) {
            const k32 = struct {
                extern "kernel32" fn QueryPerformanceCounter(*i64) callconv(.winapi) std.os.windows.BOOL;
                extern "kernel32" fn QueryPerformanceFrequency(*i64) callconv(.winapi) std.os.windows.BOOL;
            };
            var counter: i64 = undefined;
            var freq: i64 = undefined;
            _ = k32.QueryPerformanceCounter(&counter);
            _ = k32.QueryPerformanceFrequency(&freq);
            // Use i128 to avoid overflow on the multiplication; QPC counter * 1e9
            // exceeds i64 within ~30 minutes of uptime on a typical 10MHz QPF.
            return @intCast(@divTrunc(@as(i128, counter) * 1_000_000_000, @as(i128, freq)));
        }
        if (builtin.os.tag == .linux) {
            var ts: std.os.linux.timespec = undefined;
            _ = std.os.linux.clock_gettime(.MONOTONIC, &ts);
            return @as(u64, @intCast(ts.sec)) * 1_000_000_000 + @as(u64, @intCast(ts.nsec));
        }
        var ts: std.c.timespec = undefined;
        _ = std.c.clock_gettime(.MONOTONIC, &ts);
        return @as(u64, @intCast(ts.sec)) * 1_000_000_000 + @as(u64, @intCast(ts.nsec));
    }
};

/// Whether the source is a standalone expression or a full module.
pub const SourceKind = enum {
    expr,
    module,
};

/// A named module with its source text, used to supply additional imports.
pub const ModuleSource = struct {
    name: []const u8,
    source: []const u8,
};

const AvailableImport = struct {
    name: []const u8,
    env: *const ModuleEnv,
    statement_idx: ?CIR.Statement.Idx,
};

/// Statement index of an imported type module's main type declaration, mirroring
/// the package driver's `computeSiblingStatementIdx`. Qualified member lookups
/// (`Mod.member(...)`) into a type module resolve through the type declaration's
/// exposed node; regular modules store members under plain names and need no
/// statement index. Without this, the canonicalizer falls back to the unqualified
/// lookup path and a type module's exposed functions cannot be called by import
/// qualification.
fn importStatementIdx(env: *const ModuleEnv, module_name: []const u8) ?CIR.Statement.Idx {
    switch (env.module_kind) {
        .type_module => {},
        else => return null,
    }
    const type_ident = env.common.findIdent(module_name) orelse return null;
    const type_node_idx = env.getExposedTypeNodeIndexById(type_ident) orelse return null;
    return @enumFromInt(type_node_idx);
}

const ModuleValidation = enum {
    roc_check,
    checked_artifact,
};

/// Compiler stage outputs (parse, canonicalize, typecheck) for a single module.
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

/// Groups a checked module with its builtin and extra modules for problem reporting.
pub const ProblemResources = struct {
    main: CheckedModule,
    /// Locally-loaded Builtin; null when the caller supplied a pre-published
    /// Builtin via `parseAndCheckProgramForProblemsWithBuiltin` and retains
    /// ownership of the borrowed env.
    builtin_module: ?builtin_static.BuiltinModuleView,
    extra_modules: []CheckedModule,

    pub fn deinit(self: *ProblemResources, allocator: Allocator) void {
        cleanupCheckedModule(allocator, self.main);
        for (self.extra_modules) |module| cleanupCheckedModule(allocator, module);
        allocator.free(self.extra_modules);
        if (self.builtin_module) |*lm| lm.deinit();
    }
};

/// Reference to a pre-published Builtin module artifact. When passed into
/// `parseAndCanonicalize…WithBuiltin` / `compileInspected…WithBuiltin`, the
/// callee will not re-publish the Builtin and will not deinit `artifact` —
/// the caller retains ownership.
pub const PrePublishedBuiltin = struct {
    env: *const ModuleEnv,
    indices: CIR.BuiltinIndices,
    artifact: *check.CheckedArtifact.CheckedModuleArtifact,
};

/// Fully parsed, canonicalized, and type-checked module ready for LIR lowering.
pub const ParsedResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    checked_artifact: check.CheckedArtifact.CheckedModuleArtifact,
    import_artifacts: []check.CheckedArtifact.CheckedModuleArtifact,
    /// Locally-loaded Builtin; null when a pre-published Builtin was supplied
    /// and ownership stays with the caller.
    builtin_module: ?builtin_static.BuiltinModuleView,
    /// Borrowed Builtin artifact when the caller pre-published it. Used during
    /// lowering to build import views; never deinit'd here.
    borrowed_builtin_artifact: ?*check.CheckedArtifact.CheckedModuleArtifact = null,
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
//
// If the OS rejects the preferred reservation (e.g. aarch64 Linux with
// CONFIG_ARM64_VA_BITS=39 — default on 64-bit Raspberry Pi OS — caps user
// VA at ~256 GiB), the allocator halves down to `EVAL_SHARED_MEMORY_MIN_SIZE`.
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

// Floor for the retry loop. Eval tests need very little arena, so 256 MB is
// plenty; any 64-bit Linux kernel can fit this even with reduced VA bits. The
// allocator clamps this down to `EVAL_SHARED_MEMORY_SIZE` for targets whose
// preferred size is smaller.
const EVAL_SHARED_MEMORY_MIN_SIZE: usize = 256 * 1024 * 1024;

fn configuredSharedMemorySize() usize {
    if (comptime build_options.shared_memory_size > std.math.maxInt(usize)) {
        @compileError("-Dshared-memory-size does not fit in usize for this target");
    }

    return @intCast(build_options.shared_memory_size);
}

/// LIR image stored in shared memory, ready for an eval backend to execute.
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

/// Type alias for LirImageProgram.
pub const LoweredProgram = LirImageProgram;

/// Describes a single boolean-returning proc used as a test root.
pub const BoolRoot = struct {
    symbol_name: [:0]const u8,
    proc: LirProcSpecId,
    arg_layouts: []const LayoutIdx,
    ret_layout: LayoutIdx,
};

/// A group of bool-returning test roots that share one lowered LIR module.
pub const BoolRootModule = struct {
    store: *const lir.LirStore,
    layouts: *const LayoutStore,
    roots: []const BoolRoot,
};

/// Per-call mutable observation state passed to optimized test entrypoints.
pub const TestInvocationContext = extern struct {
    expect_err_set: u32 = 0,
    expect_err_start: u32 = 0,
    expect_err_end: u32 = 0,
};

/// A host event observed while evaluating a bool-returning test root.
pub const BoolRootEvent = union(enum) {
    dbg: []const u8,
    expect_failed: []const u8,
    crashed: []const u8,
};

/// Outcome of evaluating a bool-returning test root: passed (bool), crashed
/// (message), or failed because a `?` operator evaluated an Err inside the
/// expect (message plus the source region of the `?` expression).
pub const BoolRootEvalOutcome = union(enum) {
    passed: bool,
    crashed: []const u8,
    expect_err: ExpectErrFailure,
};

/// Complete result for one bool-returning test root. `events` is a structured,
/// pre-render transcript captured from the root-local RocOps environment.
pub const BoolRootEvalResult = struct {
    outcome: BoolRootEvalOutcome,
    events: []BoolRootEvent,
};

/// Callback invoked when a bool-root worker has produced its final result.
pub const BoolRootCompletionCallback = struct {
    context: *anyopaque,
    complete: *const fn (*anyopaque, usize, *const BoolRootEvalResult) void,
};

/// Borrowed host event payload forwarded from a bool-root worker.
pub const BoolRootEventView = RuntimeHostEnv.HostEventView;

/// Callback invoked when a bool-root worker records a host transcript event.
pub const BoolRootEventCallback = struct {
    context: *anyopaque,
    notify: *const fn (*anyopaque, usize, BoolRootEventView) void,
};

const RuntimeHostEventForwarder = struct {
    callback: BoolRootEventCallback,
    call_index: usize,
};

fn forwardRuntimeHostEvent(context: *anyopaque, event: RuntimeHostEnv.HostEventView) void {
    const forwarder: *RuntimeHostEventForwarder = @ptrCast(@alignCast(context));
    forwarder.callback.notify(forwarder.callback.context, forwarder.call_index, event);
}

/// Failure detail for a `?` operator that evaluated an Err inside a
/// top-level expect: the runtime-built message and the byte offsets of the
/// `?` expression in the failing module's source.
pub const ExpectErrFailure = struct {
    message: []const u8,
    region_start: u32,
    region_end: u32,
};

/// LLVM optimization level for test compilation.
pub const LlvmTestOpt = enum {
    size,
    speed,
};

fn deinitBoolRootEvent(allocator: Allocator, event: BoolRootEvent) void {
    switch (event) {
        .dbg => |message| allocator.free(message),
        .expect_failed => |message| allocator.free(message),
        .crashed => |message| allocator.free(message),
    }
}

fn deinitBoolRootEvents(allocator: Allocator, events: []BoolRootEvent) void {
    for (events) |event| deinitBoolRootEvent(allocator, event);
    if (events.len > 0) allocator.free(events);
}

fn deinitBoolRootEvalOutcome(allocator: Allocator, outcome: BoolRootEvalOutcome) void {
    switch (outcome) {
        .passed => {},
        .crashed => |message| allocator.free(message),
        .expect_err => |failure| allocator.free(failure.message),
    }
}

fn deinitBoolRootEvalResult(allocator: Allocator, result: BoolRootEvalResult) void {
    deinitBoolRootEvalOutcome(allocator, result.outcome);
    deinitBoolRootEvents(allocator, result.events);
}

/// Free all crash messages, transcript events, and the results slice.
pub fn deinitBoolRootEvalResults(allocator: Allocator, results: []BoolRootEvalResult) void {
    for (results) |result| deinitBoolRootEvalResult(allocator, result);
    allocator.free(results);
}

/// Parsed resources plus native and wasm LIR lowerings.
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

/// Parsed resources plus a single-target LIR lowering.
pub const CompiledTargetProgram = struct {
    resources: ParsedResources,
    lowered: LoweredProgram,

    pub fn deinit(self: *CompiledTargetProgram, allocator: Allocator) void {
        self.lowered.deinit(allocator);
        cleanupParseAndCanonical(allocator, self.resources);
    }
};

/// Type alias for CompiledProgram used for inspect-wrapped expressions.
pub const CompiledInspectedExpr = CompiledProgram;

/// Parse, canonicalize, and type-check a program without inspect wrapping.
pub fn parseAndCanonicalizeProgram(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) TestHelperError!ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
}

/// Same as `parseAndCanonicalizeProgram` but reuses a Builtin artifact the
/// caller has already published.
pub fn parseAndCanonicalizeProgramWithBuiltin(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: PrePublishedBuiltin,
) TestHelperError!ParsedResources {
    return parseAndCanonicalizeProgramWithRootMode(
        allocator,
        source_kind,
        source,
        imports,
        false,
        .{ .eval_root = false },
        pre_published_builtin,
        null,
    );
}

/// Same as `parseAndCanonicalizeProgramPublishedRoots` but reuses a Builtin
/// artifact the caller has already published.
///
/// `roc_ctx` supplies filesystem access for `import "path" as x : Str`/`:
/// List(U8)` statements; the REPL passes its real `CoreCtx` so file imports can
/// be read.
pub fn parseAndCanonicalizeProgramPublishedRootsWithBuiltin(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: PrePublishedBuiltin,
    roc_ctx: ?CoreCtx,
) TestHelperError!ParsedResources {
    return parseAndCanonicalizeProgramWithRootMode(
        allocator,
        source_kind,
        source,
        imports,
        false,
        .published_roots_only,
        pre_published_builtin,
        roc_ctx,
    );
}

/// Parse and canonicalize a single expression (no imports).
pub fn parseAndCanonicalizeExpr(allocator: Allocator, source: []const u8) TestHelperError!ParsedResources {
    return parseAndCanonicalizeProgram(allocator, .expr, source, &.{});
}

/// Parse and type-check a program, returning resources for problem reporting.
pub fn parseAndCheckProgramForProblems(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) TestHelperError!ProblemResources {
    return parseAndCheckProgramForProblemsImpl(allocator, source_kind, source, imports, null, null);
}

/// Same as `parseAndCheckProgramForProblems` but reuses a Builtin module the
/// caller has already loaded. The returned `ProblemResources` borrows the
/// builtin env (its `builtin_module` is null) and never deinits it.
pub fn parseAndCheckProgramForProblemsWithBuiltin(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: PrePublishedBuiltin,
) TestHelperError!ProblemResources {
    return parseAndCheckProgramForProblemsImpl(allocator, source_kind, source, imports, pre_published_builtin, null);
}

fn parseAndCheckProgramForProblemsImpl(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: ?PrePublishedBuiltin,
    roc_ctx: ?CoreCtx,
) TestHelperError!ProblemResources {
    const builtin_indices: CIR.BuiltinIndices = if (pre_published_builtin) |ppb|
        ppb.indices
    else
        compiled_builtins.builtinIndices(CIR);

    var loaded_builtin: ?builtin_static.BuiltinModuleView = if (pre_published_builtin == null)
        try builtin_static.moduleView(
            allocator,
            compiled_builtins.builtin_bin[0..],
            "Builtin",
            compiled_builtins.builtin_source,
        )
    else
        null;
    errdefer if (loaded_builtin) |*lm| lm.deinit();

    const builtin_env: *const ModuleEnv = if (pre_published_builtin) |ppb|
        ppb.env
    else
        loaded_builtin.?.env;

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
                .statement_idx = importStatementIdx(extra.module_env, extra.module_env.module_name),
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
            builtin_env,
            builtin_indices,
            available_imports,
            roc_ctx,
        );
        try extra_modules.append(allocator, checked);
    }

    const main_imports = try allocator.alloc(AvailableImport, extra_modules.items.len);
    defer allocator.free(main_imports);
    for (extra_modules.items, 0..) |extra, i| {
        main_imports[i] = .{
            .name = extra.module_env.module_name,
            .env = extra.module_env,
            .statement_idx = importStatementIdx(extra.module_env, extra.module_env.module_name),
        };
    }

    var explicit_problem_root_names_storage: [1][]const u8 = undefined;
    var explicit_problem_root_names: []const []const u8 = &.{};
    switch (source_kind) {
        .expr => {
            explicit_problem_root_names_storage[0] = evalRootName(source_kind, false);
            explicit_problem_root_names = explicit_problem_root_names_storage[0..];
        },
        .module => {},
    }

    const main_checked = try parseCheckModule(
        allocator,
        "Test",
        source_kind,
        source,
        false,
        false,
        .checked_artifact,
        explicit_problem_root_names,
        builtin_env,
        builtin_indices,
        main_imports,
        roc_ctx,
    );
    errdefer cleanupCheckedModule(allocator, main_checked);

    var all_module_envs = try allocator.alloc(*ModuleEnv, extra_modules.items.len + 2);
    defer allocator.free(all_module_envs);
    all_module_envs[0] = main_checked.module_env;
    all_module_envs[1] = @constCast(builtin_env);
    for (extra_modules.items, 0..) |extra, i| {
        all_module_envs[i + 2] = extra.module_env;
    }
    resolveImportsByModuleIndex(all_module_envs);

    return .{
        .main = main_checked,
        .builtin_module = loaded_builtin,
        .extra_modules = try extra_modules.toOwnedSlice(allocator),
    };
}

/// Parse, canonicalize, type-check, and lower to native and wasm LIR.
pub fn compileProgram(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) TestHelperError!CompiledProgram {
    return compileProgramWithOptions(allocator, io, source_kind, source, imports, .{});
}

/// Parse, canonicalize, type-check, and lower with allocation-test options.
pub fn compileAllocationProgram(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) TestHelperError!CompiledProgram {
    return compileProgramWithOptions(allocator, io, source_kind, source, imports, .{
        .inline_mode = .wrappers,
        .tag_reachability = true,
    });
}

fn compileProgramWithOptions(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    options: LowerToLirOptions,
) TestHelperError!CompiledProgram {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLirWithOptions(allocator, io, &resources, .native, options);
    errdefer {
        var owned = lowered;
        owned.deinit(allocator);
    }

    const wasm_lowered = try lowerParsedProgramToLirWithOptions(allocator, io, &resources, .u32, options);
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

/// Parse, canonicalize, type-check, and lower to LIR for a specific target.
pub fn compileProgramForTarget(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    target_usize: base.target.TargetUsize,
) TestHelperError!CompiledTargetProgram {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLir(allocator, io, &resources, target_usize);
    errdefer {
        var owned = lowered;
        owned.deinit(allocator);
    }

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

/// Same as `compileProgramForTarget` but reuses a pre-published Builtin
/// artifact owned by the caller instead of loading it from the embedded
/// builtin blob on every call.
pub fn compileProgramForTargetWithBuiltin(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    target_usize: base.target.TargetUsize,
    pre_published_builtin: PrePublishedBuiltin,
) TestHelperError!CompiledTargetProgram {
    var resources = try parseAndCanonicalizeProgramWithRootMode(
        allocator,
        source_kind,
        source,
        imports,
        false,
        .{ .eval_root = false },
        pre_published_builtin,
        // No file-reading CoreCtx: this helper is not used for file imports.
        null,
    );
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLir(allocator, io, &resources, target_usize);
    errdefer {
        var owned = lowered;
        owned.deinit(allocator);
    }

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

/// Compile a program with inspect wrapping so the main proc returns a Str.
pub fn compileInspectedProgram(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) TestHelperError!CompiledProgram {
    return compileInspectedProgramImpl(allocator, io, source_kind, source, imports, null, null);
}

/// Same as `compileInspectedProgram` but reuses a pre-published Builtin
/// artifact owned by the caller. `roc_ctx` supplies filesystem access for file
/// imports (the REPL passes its real `CoreCtx`); pass `null` otherwise.
pub fn compileInspectedProgramWithBuiltin(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: PrePublishedBuiltin,
    roc_ctx: ?CoreCtx,
) TestHelperError!CompiledProgram {
    return compileInspectedProgramImpl(allocator, io, source_kind, source, imports, pre_published_builtin, roc_ctx);
}

fn compileInspectedProgramImpl(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: ?PrePublishedBuiltin,
    roc_ctx: ?CoreCtx,
) TestHelperError!CompiledProgram {
    var resources = try parseAndCanonicalizeProgramWithRootMode(
        allocator,
        source_kind,
        source,
        imports,
        true,
        .{ .eval_root = true },
        pre_published_builtin,
        roc_ctx,
    );
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLir(allocator, io, &resources, .native);
    errdefer {
        var owned = lowered;
        owned.deinit(allocator);
    }

    const wasm_lowered = try lowerParsedProgramToLir(allocator, io, &resources, .u32);
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

/// Compile an inspect-wrapped program for a specific target pointer size.
pub fn compileInspectedProgramForTarget(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    target_usize: base.target.TargetUsize,
) TestHelperError!CompiledTargetProgram {
    return compileInspectedProgramForTargetImpl(allocator, io, source_kind, source, imports, target_usize, null, null);
}

/// Same as `compileInspectedProgramForTarget` but reuses a pre-published
/// Builtin artifact owned by the caller. `roc_ctx` supplies filesystem access
/// for file imports (the REPL passes its real `CoreCtx`); pass `null` otherwise.
pub fn compileInspectedProgramForTargetWithBuiltin(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    target_usize: base.target.TargetUsize,
    pre_published_builtin: PrePublishedBuiltin,
    roc_ctx: ?CoreCtx,
) TestHelperError!CompiledTargetProgram {
    return compileInspectedProgramForTargetImpl(allocator, io, source_kind, source, imports, target_usize, pre_published_builtin, roc_ctx);
}

fn compileInspectedProgramForTargetImpl(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    target_usize: base.target.TargetUsize,
    pre_published_builtin: ?PrePublishedBuiltin,
    roc_ctx: ?CoreCtx,
) TestHelperError!CompiledTargetProgram {
    var resources = try parseAndCanonicalizeProgramWithRootMode(
        allocator,
        source_kind,
        source,
        imports,
        true,
        .{ .eval_root = true },
        pre_published_builtin,
        roc_ctx,
    );
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLir(allocator, io, &resources, target_usize);
    errdefer {
        var owned = lowered;
        owned.deinit(allocator);
    }

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

/// Compile a single expression with inspect wrapping, returning a Str result.
pub fn compileInspectedExpr(allocator: Allocator, io: std.Io, source: []const u8) TestHelperError!CompiledInspectedExpr {
    return compileInspectedProgram(allocator, io, .expr, source, &.{});
}

/// Debug-only: compile an inspect-wrapped program for the native target while
/// capturing the Debug verifier's materialized Lambda Mono program in
/// `materialized_out`. Compiles with the specialization cache disabled (so
/// every function body materializes locally rather than loading as an
/// imported shard) and the in-place List.map path off (so the materialized
/// tree stays on the copy path a tree evaluator executes).
pub fn compileInspectedProgramWithLambdaMono(
    allocator: Allocator,
    io: std.Io,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: ?PrePublishedBuiltin,
    materialized_out: *?lir.CheckedPipeline.LambdaMonoProgram,
) TestHelperError!CompiledTargetProgram {
    var resources = try parseAndCanonicalizeProgramWithRootMode(
        allocator,
        source_kind,
        source,
        imports,
        true,
        .{ .eval_root = true },
        pre_published_builtin,
        null,
    );
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedProgramToLirWithOptions(allocator, io, &resources, .native, .{
        .list_in_place_map = false,
        .monotype_cache = lir.CheckedPipeline.MonotypeCacheControl.disabled,
        .debug_materialized_out = materialized_out,
    });
    errdefer {
        var owned = lowered;
        owned.deinit(allocator);
    }

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

/// Free all resources held by a ParsedResources value.
pub fn cleanupParseAndCanonical(allocator: Allocator, resources: ParsedResources) void {
    var owned = resources;
    owned.deinit(allocator);
}

/// Parse and canonicalize a program, optionally wrapping it for inspect output.
pub fn parseAndCanonicalizeProgramWrapped(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    inspect_wrap: bool,
) TestHelperError!ParsedResources {
    return parseAndCanonicalizeProgramWithRootMode(allocator, source_kind, source, imports, inspect_wrap, .{ .eval_root = inspect_wrap }, null, null);
}

/// Parse and canonicalize a program using published-roots-only root selection.
pub fn parseAndCanonicalizeProgramPublishedRoots(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) TestHelperError!ParsedResources {
    return parseAndCanonicalizeProgramWithRootMode(allocator, source_kind, source, imports, false, .published_roots_only, null, null);
}

/// Whether publishing a program for compile-time evaluation reported problems.
pub const ComptimePublishOutcome = enum { no_problems, comptime_problems };

/// Publish a program with compile-time evaluation problems routed into the
/// checker's problem store, reporting whether any were found. The runtime eval
/// pipeline intentionally publishes without a problem store so that crashes
/// reachable from compile-time roots still compile and crash at runtime; this
/// entry point exists for tests that assert on the compile-time diagnostics
/// instead.
pub fn publishProgramForComptimeProblems(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) TestHelperError!ComptimePublishOutcome {
    return publishProgramForComptimeProblemsImpl(allocator, source_kind, source, imports, null);
}

/// Same as `publishProgramForComptimeProblems` but reuses a Builtin artifact
/// the caller has already published.
pub fn publishProgramForComptimeProblemsWithBuiltin(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: PrePublishedBuiltin,
) TestHelperError!ComptimePublishOutcome {
    return publishProgramForComptimeProblemsImpl(allocator, source_kind, source, imports, pre_published_builtin);
}

fn publishProgramForComptimeProblemsImpl(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    pre_published_builtin: ?PrePublishedBuiltin,
) TestHelperError!ComptimePublishOutcome {
    const resources = parseAndCanonicalizeProgramWithRootModeReporting(
        allocator,
        source_kind,
        source,
        imports,
        false,
        .published_roots_only,
        pre_published_builtin,
        .report_comptime_problems,
        null,
    ) catch |err| switch (err) {
        error.CompileTimeProblem => return .comptime_problems,
        else => return err,
    };
    defer cleanupParseAndCanonical(allocator, resources);

    return if (resources.checker.problems.problems.items.len == 0)
        .no_problems
    else
        .comptime_problems;
}

/// Publish a program with compile-time evaluation problems routed into each
/// module's checker problem store and return the full resources for tests that
/// need to inspect which module received which diagnostic. Unlike
/// `publishProgramForComptimeProblems`, this only returns resources when
/// publishing completes without a blocking compile-time problem; crashing roots
/// and failed expects still return `error.CompileTimeProblem`.
pub fn publishProgramKeepingReportedComptimeProblems(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) TestHelperError!ParsedResources {
    return parseAndCanonicalizeProgramWithRootModeReporting(
        allocator,
        source_kind,
        source,
        imports,
        false,
        .published_roots_only,
        null,
        .report_comptime_problems,
        null,
    );
}

const PublishedRootMode = union(enum) {
    eval_root: bool,
    published_roots_only,
};

const ComptimeProblemReporting = enum {
    ignore_comptime_problems,
    report_comptime_problems,
};

fn problemBlocksCheckedArtifact(problem: check.problem.Problem) bool {
    return switch (problem) {
        .effectful_function_name, .redundant_pattern, .unmatchable_pattern, .comptime_unused_branch, .comptime_condition, .literal_defaulted => false,
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
    pre_published_builtin: ?PrePublishedBuiltin,
    roc_ctx: ?CoreCtx,
) TestHelperError!ParsedResources {
    return parseAndCanonicalizeProgramWithRootModeReporting(
        allocator,
        source_kind,
        source,
        imports,
        inspect_wrap,
        root_mode,
        pre_published_builtin,
        .ignore_comptime_problems,
        roc_ctx,
    );
}

fn parseAndCanonicalizeProgramWithRootModeReporting(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    inspect_wrap: bool,
    root_mode: PublishedRootMode,
    pre_published_builtin: ?PrePublishedBuiltin,
    problem_reporting: ComptimeProblemReporting,
    roc_ctx: ?CoreCtx,
) TestHelperError!ParsedResources {
    const builtin_indices: CIR.BuiltinIndices = if (pre_published_builtin) |ppb|
        ppb.indices
    else
        compiled_builtins.builtinIndices(CIR);

    var loaded_builtin: ?builtin_static.BuiltinModuleView = if (pre_published_builtin == null)
        try builtin_static.moduleView(
            allocator,
            compiled_builtins.builtin_bin[0..],
            "Builtin",
            compiled_builtins.builtin_source,
        )
    else
        null;
    // Tracks whether `loaded_builtin`'s env-wrapper ownership has transferred to
    // an import artifact (`publishImportArtifacts`). Once transferred, the
    // errdefer must not deinit the view.
    var builtin_module_owned_by_artifact = false;
    errdefer if (loaded_builtin) |*lm| {
        if (!builtin_module_owned_by_artifact) lm.deinit();
    };

    const builtin_env: *const ModuleEnv = if (pre_published_builtin) |ppb|
        ppb.env
    else
        loaded_builtin.?.env;

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
                .statement_idx = importStatementIdx(extra.module_env, extra.module_env.module_name),
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
            builtin_env,
            builtin_indices,
            available_imports,
            roc_ctx,
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
            .statement_idx = importStatementIdx(extra.module_env, extra.module_env.module_name),
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
        builtin_env,
        builtin_indices,
        main_imports,
        roc_ctx,
    );
    errdefer cleanupCheckedModule(allocator, main_checked);
    if (checkedModuleHasArtifactBlockingProblems(&main_checked)) {
        return error.TypeCheckError;
    }

    var all_module_envs = try allocator.alloc(*ModuleEnv, extra_modules.items.len + 2);
    defer allocator.free(all_module_envs);
    all_module_envs[0] = main_checked.module_env;
    all_module_envs[1] = @constCast(builtin_env);
    for (extra_modules.items, 0..) |extra, i| {
        all_module_envs[i + 2] = extra.module_env;
    }
    resolveImportsByModuleIndex(all_module_envs);

    var source_modules = try allocator.alloc(check.TypedCIR.Modules.SourceModule, extra_modules.items.len + 2);
    defer allocator.free(source_modules);
    source_modules[0] = .{ .precompiled = main_checked.module_env };
    source_modules[1] = .{ .precompiled = @constCast(builtin_env) };
    for (extra_modules.items, 0..) |extra, i| {
        source_modules[i + 2] = .{ .precompiled = extra.module_env };
    }

    var typed_cir_modules = try check.TypedCIR.Modules.init(allocator, source_modules);
    defer typed_cir_modules.deinit();
    const import_artifacts = try publishImportArtifacts(
        allocator,
        &typed_cir_modules,
        if (loaded_builtin) |*lm| lm else null,
        extra_modules.items,
        &builtin_module_owned_by_artifact,
        pre_published_builtin,
        problem_reporting,
    );
    errdefer {
        for (import_artifacts) |*artifact| artifact.deinit(allocator);
        allocator.free(import_artifacts);
    }

    const publish_imports = try publishImportKeysWithBuiltin(allocator, import_artifacts, pre_published_builtin);
    defer allocator.free(publish_imports);
    const available_artifacts = try importedViewsFromPublishImports(allocator, publish_imports);
    defer allocator.free(available_artifacts);

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
            .available_artifacts = available_artifacts,
            .explicit_roots = explicit_roots,
            .compile_time_finalizer = CompileTimeFinalization.finalizer(),
            .problem_store = switch (problem_reporting) {
                .ignore_comptime_problems => null,
                .report_comptime_problems => &main_checked.checker.problems,
            },
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
        .builtin_module = loaded_builtin,
        .borrowed_builtin_artifact = if (pre_published_builtin) |ppb| ppb.artifact else null,
        .builtin_indices = builtin_indices,
        .imported_envs = main_checked.imported_envs,
        .auto_imported_types = main_checked.auto_imported_types,
        .extra_modules = try extra_modules.toOwnedSlice(allocator),
        .parse_ns = main_checked.parse_ns,
        .canonicalize_ns = main_checked.canonicalize_ns,
        .typecheck_ns = main_checked.typecheck_ns,
    };
}

/// Run parse, canonicalize, and typecheck for a single named module.
///
/// `roc_ctx` supplies the filesystem access canonicalization needs for `import
/// "path" as x : Str`/`: List(U8)` statements. Pass a real `CoreCtx` (as the
/// REPL does, built at the CLI entrypoint) to read imported files; pass `null`
/// for tests that never import a file, in which case any file read panics via
/// the testing ctx as a guardrail.
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
    roc_ctx: ?CoreCtx,
) TestHelperError!CheckedModule {
    const owned_source = try makeModuleSource(allocator, source_kind, source, inspect_wrap);
    errdefer allocator.free(owned_source);

    const module_env = try allocator.create(ModuleEnv);
    errdefer allocator.destroy(module_env);
    module_env.* = try ModuleEnv.init(allocator, owned_source);
    errdefer module_env.deinit();
    module_env.common.source = owned_source;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(module_env.gpa);

    var parse_elapsed: u64 = 0;
    var parse_timer = try StageTimer.start();
    const parse_ast = try parse.file(allocator, &module_env.common);
    parse_elapsed = parse_timer.read();
    errdefer {
        parse_ast.deinit();
    }
    if (parse_ast.tokenize_diagnostics.items.len > 0 or parse_ast.parse_diagnostics.items.len > 0) {
        return error.ParseError;
    }

    try module_env.initCIRFields(module_name);
    const builtin_ctx: Check.BuiltinContext = .{
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
    const canon_ctx = roc_ctx orelse CoreCtx.testing(allocator, allocator);
    czer.* = try Can.initModule(canon_ctx, module_env, parse_ast, .{
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
    for (explicit_root_names) |root_name| {
        const root_def_idx = czer.explicitRootDefByName(root_name) orelse {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("eval helper invariant violated: explicit executable root `{s}` was not found", .{root_name});
            }
            unreachable;
        };
        try checker.addExecutableRootDef(root_def_idx);
    }
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
    io: std.Io,
    resources: *ParsedResources,
    target_usize: base.target.TargetUsize,
) TestHelperError!LoweredProgram {
    return lowerParsedProgramToLirWithOptions(allocator, io, resources, target_usize, .{});
}

const LowerToLirOptions = struct {
    inline_mode: lir.CheckedPipeline.InlineMode = .none,
    tag_reachability: bool = false,
    /// Match optimized builds so every backend exercises the in-place
    /// List.map path; the copy path is still covered by shared-list,
    /// slice, and layout-mismatch cases. The Lambda Mono differential
    /// harness disables this so its tree stays on the copy path.
    list_in_place_map: bool = true,
    /// Specialization cache control; the differential harness disables the
    /// cache so every function body materializes locally.
    monotype_cache: lir.CheckedPipeline.MonotypeCacheControl = .{},
    /// Debug-only capture slot for the verifier's materialized Lambda Mono
    /// program.
    debug_materialized_out: ?*?lir.CheckedPipeline.LambdaMonoProgram = null,
};

fn lowerParsedProgramToLirWithOptions(
    allocator: Allocator,
    io: std.Io,
    resources: *ParsedResources,
    target_usize: base.target.TargetUsize,
    options: LowerToLirOptions,
) TestHelperError!LoweredProgram {
    if (resources.borrowed_builtin_artifact == null) {
        return lowerCheckedModuleSetToLirWithOptions(allocator, io, &resources.checked_artifact, resources.import_artifacts, target_usize, options);
    }

    const borrowed = resources.borrowed_builtin_artifact.?;
    const total = resources.import_artifacts.len + 1;
    const import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, total);
    defer allocator.free(import_views);
    import_views[0] = check.CheckedArtifact.importedView(borrowed);
    for (resources.import_artifacts, 0..) |*module, i| {
        import_views[i + 1] = check.CheckedArtifact.importedView(module);
    }
    return lowerCheckedRootWithViews(allocator, io, &resources.checked_artifact, import_views, target_usize, options);
}

/// Lower already-published checked modules to a LIR image.
pub fn lowerCheckedModuleSetToLir(
    allocator: Allocator,
    io: std.Io,
    root_module: *check.CheckedArtifact.CheckedModuleArtifact,
    import_modules: []check.CheckedArtifact.CheckedModuleArtifact,
    target_usize: base.target.TargetUsize,
) TestHelperError!LoweredProgram {
    return lowerCheckedModuleSetToLirWithOptions(allocator, io, root_module, import_modules, target_usize, .{});
}

fn lowerCheckedModuleSetToLirWithOptions(
    allocator: Allocator,
    io: std.Io,
    root_module: *check.CheckedArtifact.CheckedModuleArtifact,
    import_modules: []check.CheckedArtifact.CheckedModuleArtifact,
    target_usize: base.target.TargetUsize,
    options: LowerToLirOptions,
) TestHelperError!LoweredProgram {
    const import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, import_modules.len);
    defer allocator.free(import_views);
    for (import_modules, 0..) |*module, i| {
        import_views[i] = check.CheckedArtifact.importedView(module);
    }
    return lowerCheckedRootWithViews(allocator, io, root_module, import_views, target_usize, options);
}

fn lowerCheckedRootWithViews(
    allocator: Allocator,
    io: std.Io,
    root_module: *check.CheckedArtifact.CheckedModuleArtifact,
    import_views: []const check.CheckedArtifact.ImportedModuleView,
    target_usize: base.target.TargetUsize,
    options: LowerToLirOptions,
) TestHelperError!LoweredProgram {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.createWithMinSize(io, EVAL_SHARED_MEMORY_SIZE, EVAL_SHARED_MEMORY_MIN_SIZE, page_size);
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
            .inline_mode = options.inline_mode,
            .list_in_place_map = options.list_in_place_map,
            .monotype_cache = options.monotype_cache,
            .tag_reachability = options.tag_reachability,
            .debug_materialized_out = options.debug_materialized_out,
        },
    );

    try LirImage.fillHeaderInSharedMemory(
        image_header,
        shm.base_ptr,
        shm.getUsedSize(),
        &lowered.lir_result,
        &.{},
    );
    shm.updateHeader();

    const view = try LirImage.viewMappedImage(image_header, shm.base_ptr, shm.getUsedSize(), lowered.target_usize);
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
    builtin_module: ?*builtin_static.BuiltinModuleView,
    extra_modules: []CheckedModule,
    builtin_module_owned_by_artifact: *bool,
    pre_published_builtin: ?PrePublishedBuiltin,
    problem_reporting: ComptimeProblemReporting,
) TestHelperError![]check.CheckedArtifact.CheckedModuleArtifact {
    const extra_module_count = extra_modules.len;
    var artifacts = std.ArrayList(check.CheckedArtifact.CheckedModuleArtifact).empty;
    errdefer {
        for (artifacts.items) |*artifact| artifact.deinit(allocator);
        artifacts.deinit(allocator);
    }
    // Reserve the final size up front so `artifacts` never reallocates while we
    // publish. The `view` we store for each artifact in `published_keys` aliases
    // the artifact's in-list storage; if the backing array moved, those views
    // would dangle and later modules would read another module's identity.
    const builtin_in_artifacts: usize = if (pre_published_builtin == null) 1 else 0;
    try artifacts.ensureTotalCapacityPrecise(allocator, extra_module_count + builtin_in_artifacts);

    var published_keys = std.ArrayList(check.CheckedArtifact.PublishImportArtifact).empty;
    defer published_keys.deinit(allocator);

    if (pre_published_builtin) |ppb| {
        try published_keys.append(allocator, .{
            .module_idx = 1,
            .key = ppb.artifact.key,
            .view = check.CheckedArtifact.importedView(ppb.artifact),
        });
    } else {
        const builtin_artifact = try check.CheckedArtifact.publishFromTypedModule(
            allocator,
            typed_cir_modules,
            1,
            .{
                .module_env_storage = .{ .static_builtin = builtin_module.?.env },
                .compile_time_finalizer = CompileTimeFinalization.finalizer(),
            },
        );
        builtin_module_owned_by_artifact.* = true;
        // Move into stable storage first, then build the view from that pointer.
        // On failure the artifact is owned by `artifacts` and freed by errdefer.
        artifacts.appendAssumeCapacity(builtin_artifact);
        const builtin_ptr = &artifacts.items[artifacts.items.len - 1];
        try published_keys.append(allocator, .{
            .module_idx = 1,
            .key = builtin_ptr.key,
            .view = check.CheckedArtifact.importedView(builtin_ptr),
        });
    }

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

            const available_artifacts = try importedViewsFromPublishImports(allocator, published_keys.items);
            defer allocator.free(available_artifacts);

            const artifact = try check.CheckedArtifact.publishFromTypedModule(
                allocator,
                typed_cir_modules,
                module_idx,
                .{
                    .module_env_storage = .{ .checked_source = extra_modules[extra_i].module_env },
                    .imports = published_keys.items,
                    .available_artifacts = available_artifacts,
                    .compile_time_finalizer = CompileTimeFinalization.finalizer(),
                    .problem_store = switch (problem_reporting) {
                        .ignore_comptime_problems => null,
                        .report_comptime_problems => &extra_modules[extra_i].checker.problems,
                    },
                },
            );
            extra_modules[extra_i].published_owns_module_env = true;
            extra_modules[extra_i].owned_source = null;

            // Move into stable storage first, then build the view from that
            // pointer so it cannot dangle when later modules are published.
            artifacts.appendAssumeCapacity(artifact);
            const artifact_ptr = &artifacts.items[artifacts.items.len - 1];
            try published_keys.append(allocator, .{
                .module_idx = module_idx,
                .key = artifact_ptr.key,
                .view = check.CheckedArtifact.importedView(artifact_ptr),
            });

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

fn importedViewsFromPublishImports(
    allocator: Allocator,
    imports: []const check.CheckedArtifact.PublishImportArtifact,
) TestHelperError![]check.CheckedArtifact.ImportedModuleView {
    const views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, imports.len);
    for (imports, 0..) |import, i| {
        views[i] = import.view;
    }
    return views;
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

fn publishImportKeysWithBuiltin(
    allocator: Allocator,
    artifacts: []const check.CheckedArtifact.CheckedModuleArtifact,
    pre_published_builtin: ?PrePublishedBuiltin,
) TestHelperError![]check.CheckedArtifact.PublishImportArtifact {
    const borrowed_builtin_count: usize = if (pre_published_builtin == null) 0 else 1;
    const imports = try allocator.alloc(check.CheckedArtifact.PublishImportArtifact, artifacts.len + borrowed_builtin_count);
    if (pre_published_builtin) |ppb| {
        imports[0] = .{
            .module_idx = 1,
            .key = ppb.artifact.key,
            .view = check.CheckedArtifact.importedView(ppb.artifact),
        };
    }
    for (artifacts, 0..) |artifact, i| {
        imports[i + borrowed_builtin_count] = .{
            .module_idx = artifact.module_identity.module_idx,
            .key = artifact.key,
            .view = check.CheckedArtifact.importedView(&artifacts[i]),
        };
    }
    return imports;
}

/// Render diagnostics (tokenize, parse, canonicalize, type-check) for a source as a
/// terminal-formatted string. Use this on `error.TypeCheckError` to produce the same
/// nice messages the file-based path prints.
pub fn renderProblems(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
) TestHelperError![]u8 {
    return try renderProblemsWithConfig(allocator, source_kind, source, reporting.ReportingConfig.initColorTerminal());
}

/// Renders diagnostics for the given source using the provided reporting configuration.
pub fn renderProblemsWithConfig(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    config: reporting.ReportingConfig,
) TestHelperError![]u8 {
    return try renderProblemsWithConfigAndImports(allocator, source_kind, source, &.{}, config, null);
}

/// Like `renderProblemsWithConfig` but type-checks against the supplied imported
/// modules so a source containing `import` statements does not hit unresolved
/// imports while rendering problems.
///
/// `roc_ctx` supplies filesystem access for `import "path" as x : Str`/`:
/// List(U8)` statements so re-canonicalizing to render diagnostics can read the
/// file again; the REPL passes its real `CoreCtx`. Pass `null` when no file
/// imports are involved.
pub fn renderProblemsWithConfigAndImports(
    allocator: Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    config: reporting.ReportingConfig,
    roc_ctx: ?CoreCtx,
) TestHelperError![]u8 {
    var resources = try parseAndCheckProgramForProblemsImpl(allocator, source_kind, source, imports, null, roc_ctx);
    defer resources.deinit(allocator);

    return try renderCheckedModuleProblemsWithConfig(allocator, &resources.main, "repl", config);
}

fn renderCheckedModuleProblemsWithConfig(
    allocator: Allocator,
    main: *const CheckedModule,
    filename: []const u8,
    config: reporting.ReportingConfig,
) TestHelperError![]u8 {
    var reports = std.array_list.Managed(reporting.Report).init(allocator);
    defer {
        for (reports.items) |*r| r.deinit();
        reports.deinit();
    }

    for (main.parse_ast.tokenize_diagnostics.items) |diagnostic| {
        const report = try main.parse_ast.tokenizeDiagnosticToReport(diagnostic, allocator, filename);
        try reports.append(report);
    }

    for (main.parse_ast.parse_diagnostics.items) |diagnostic| {
        const report = try main.parse_ast.parseDiagnosticToReport(&main.module_env.common, diagnostic, allocator, filename);
        try reports.append(report);
    }

    const diagnostics = try main.module_env.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diagnostic| {
        const report = try main.module_env.diagnosticToReport(diagnostic, allocator, filename);
        try reports.append(report);
    }

    for (main.checker.problems.problems.items) |problem| {
        var report_builder = try check.ReportBuilder.init(
            allocator,
            main.module_env,
            main.module_env,
            &main.checker.snapshots,
            &main.checker.problems,
            filename,
            &.{},
            &main.checker.import_mapping,
            &main.checker.regions,
            null,
        );
        defer report_builder.deinit();

        const report = try report_builder.build(problem);
        try reports.append(report);
    }

    var out: std.Io.Writer.Allocating = .init(allocator);
    errdefer out.deinit();
    for (reports.items) |report| {
        try reporting.renderReportWithConfig(&report, &out.writer, config);
    }
    const raw = try out.toOwnedSlice();
    const trimmed = std.mem.trimEnd(u8, raw, "\r\n");
    if (trimmed.len == raw.len) return raw;
    const result = try allocator.dupe(u8, trimmed);
    allocator.free(raw);
    return result;
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
) TestHelperError![]u8 {
    return switch (source_kind) {
        .expr => if (inspect_wrap)
            std.fmt.allocPrint(allocator, "main = || Str.inspect(({s}))", .{source})
        else
            std.fmt.allocPrint(allocator, "main = || ({s})", .{source}),
        .module => if (inspect_wrap)
            std.fmt.allocPrint(allocator, "{s}\n\ncodex_test_inspect_main = || Str.inspect(({{ roc_eval_main: main }}).roc_eval_main)\n", .{source})
        else
            allocator.dupe(u8, source),
    };
}

fn resolveImportsByModuleIndex(module_envs: []const *ModuleEnv) void {
    for (module_envs) |module_env| {
        module_env.imports.clearResolvedModules();
        for (module_env.imports.imports.items.items, 0..) |str_idx, i| {
            const import_name = module_env.getString(str_idx);
            const import_idx: CIR.Import.Idx = @enumFromInt(i);
            if (CIR.Import.isCompilerBuiltinImportName(import_name)) {
                for (module_envs, 0..) |candidate_env, module_idx| {
                    if (candidate_env.module_role == .builtin) {
                        module_env.imports.setResolvedModule(import_idx, @intCast(module_idx));
                        break;
                    }
                }
                continue;
            }
            for (module_envs, 0..) |candidate_env, module_idx| {
                if (candidate_env.module_role == .builtin) continue;
                if (base.Ident.textEql(candidate_env.module_name, import_name)) {
                    module_env.imports.setResolvedModule(import_idx, @intCast(module_idx));
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
        const import_idx: CIR.Import.Idx = @enumFromInt(i);
        if (CIR.Import.isCompilerBuiltinImportName(import_name)) {
            for (imported_envs, 0..) |candidate_env, module_idx| {
                if (candidate_env.module_role == .builtin) {
                    module_env.imports.setResolvedModule(import_idx, @intCast(module_idx));
                    break;
                }
            }
            continue;
        }
        for (imported_envs, 0..) |candidate_env, module_idx| {
            if (candidate_env.module_role == .builtin) continue;
            if (base.Ident.textEql(candidate_env.module_name, import_name)) {
                module_env.imports.setResolvedModule(import_idx, @intCast(module_idx));
                break;
            }
        }
    }
}

/// Return the layout indices for the main proc's arguments.
pub fn mainProcArgLayouts(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError![]LayoutIdx {
    const proc = lowered.view.store.getProcSpec(lowered.mainProc());
    const arg_locals = lowered.view.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(LayoutIdx, arg_locals.len);
    for (0..arg_locals.len) |i| {
        const local_id = GuardedList.at(arg_locals, i);
        arg_layouts[i] = lowered.view.store.getLocal(local_id).layout_idx;
    }
    return arg_layouts;
}

/// Compute the entrypoint calling-convention slot size for a layout.
pub fn entrypointParamSlotSizeForLayouts(layouts: *const LayoutStore, layout_idx: LayoutIdx) u32 {
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

/// Entrypoint slot size for a layout, looked up from a lowered program.
pub fn entrypointParamSlotSize(lowered: *const LoweredProgram, layout_idx: LayoutIdx) u32 {
    return entrypointParamSlotSizeForLayouts(&lowered.view.layouts, layout_idx);
}

/// Allocate a zeroed, alignment-sorted entrypoint argument buffer, or null if no args.
pub fn zeroedEntrypointArgBufferForLayouts(
    allocator: Allocator,
    layouts: *const LayoutStore,
    arg_layouts: []const LayoutIdx,
) TestHelperError!?[]align(collections.max_roc_alignment.toByteUnits()) u8 {
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
            const size_align = layouts.layoutSizeAlign(layouts.getLayout(arg_layout));
            const slot_size = entrypointParamSlotSizeForLayouts(layouts, arg_layout);
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
        total_size = @max(total_size, @as(usize, arg_offsets[i]) + entrypointParamSlotSizeForLayouts(layouts, arg_layout));
    }

    if (total_size == 0) return null;

    const buffer = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(total_size, 1));
    @memset(buffer, 0);
    return buffer;
}

/// Zeroed entrypoint arg buffer using layout info from a lowered program.
pub fn zeroedEntrypointArgBuffer(
    allocator: Allocator,
    lowered: *const LoweredProgram,
    arg_layouts: []const LayoutIdx,
) TestHelperError!?[]align(collections.max_roc_alignment.toByteUnits()) u8 {
    return zeroedEntrypointArgBufferForLayouts(allocator, &lowered.view.layouts, arg_layouts);
}

fn boolRootRetBuffer(
    allocator: Allocator,
    layouts: *const LayoutStore,
    ret_layout: LayoutIdx,
) TestHelperError![]align(collections.max_roc_alignment.toByteUnits()) u8 {
    const size_align = layouts.layoutSizeAlign(layouts.getLayout(ret_layout));
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(size_align.size, 1));
    @memset(ret_buf, 0);
    return ret_buf;
}

fn copyRuntimeCrashMessage(allocator: Allocator, runtime_env: *const RuntimeHostEnv) TestHelperError![]const u8 {
    return switch (runtime_env.crashState()) {
        .did_not_crash => try allocator.dupe(u8, "Roc crashed"),
        .crashed => |msg| try allocator.dupe(u8, msg),
    };
}

fn deinitPartialBoolRootEvalResults(allocator: Allocator, results: []BoolRootEvalResult, len: usize) void {
    for (results[0..len]) |result| deinitBoolRootEvalResult(allocator, result);
    allocator.free(results);
}

fn copyRuntimeHostEvents(allocator: Allocator, runtime_env: *const RuntimeHostEnv) TestHelperError![]BoolRootEvent {
    var snapshot = try runtime_env.snapshot(allocator);
    defer snapshot.deinit(allocator);

    if (snapshot.events.len == 0) return &.{};

    const events = try allocator.alloc(BoolRootEvent, snapshot.events.len);
    var events_len: usize = 0;
    errdefer {
        for (events[0..events_len]) |event| deinitBoolRootEvent(allocator, event);
        allocator.free(events);
    }

    for (snapshot.events, 0..) |event, index| {
        events[index] = switch (event) {
            .dbg => |message| .{ .dbg = try allocator.dupe(u8, message) },
            .expect_failed => |message| .{ .expect_failed = try allocator.dupe(u8, message) },
            .crashed => |message| .{ .crashed = try allocator.dupe(u8, message) },
        };
        events_len += 1;
    }

    return events;
}

fn runExecutableBoolRoot(
    allocator: Allocator,
    layouts: *const LayoutStore,
    executable: *const ExecutableMemory,
    root: BoolRoot,
    runtime_env: *RuntimeHostEnv,
) TestHelperError!BoolRootEvalResult {
    runtime_env.resetObservation();
    runtime_env.resetAllocationTracker();
    // Dev-JIT code calls the host's own expect_err wrapper, which records the
    // `?` region in this thread-local slot; clear any stale value first.
    _ = builtins.dev_wrappers.takeExpectErrRegion();

    const arg_buffer = try zeroedEntrypointArgBufferForLayouts(allocator, layouts, root.arg_layouts);
    defer if (arg_buffer) |buf| allocator.free(buf);

    const ret_buf = try boolRootRetBuffer(allocator, layouts, root.ret_layout);
    defer allocator.free(ret_buf);

    var crash_boundary = runtime_env.enterCrashBoundary();
    defer crash_boundary.deinit();
    const sj = crash_boundary.set();
    if (sj == 0) {
        executable.callRocABI(
            @ptrCast(runtime_env.get_ops()),
            @ptrCast(ret_buf.ptr),
            if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
        );
    }

    const outcome: BoolRootEvalOutcome = switch (runtime_env.crashState()) {
        .did_not_crash => .{ .passed = ret_buf[0] != 0 },
        .crashed => if (builtins.dev_wrappers.takeExpectErrRegion()) |region| .{ .expect_err = .{
            .message = try copyRuntimeCrashMessage(allocator, runtime_env),
            .region_start = region.start,
            .region_end = region.end,
        } } else .{ .crashed = try copyRuntimeCrashMessage(allocator, runtime_env) },
    };
    errdefer deinitBoolRootEvalOutcome(allocator, outcome);
    const events = try copyRuntimeHostEvents(allocator, runtime_env);
    runtime_env.resetAllocationTracker();
    return .{
        .outcome = outcome,
        .events = events,
    };
}

/// JIT-compile and run bool-returning test roots via the dev backend.
pub fn devEvalBoolRoots(
    allocator: Allocator,
    store: *const lir.LirStore,
    layouts: *const LayoutStore,
    roots: []const BoolRoot,
) TestHelperError![]BoolRootEvalResult {
    if (comptime !backend.host_lir_codegen_available) {
        return error.DevBackendUnavailable;
    } else {
        var static_strings = try backend.StaticStringData.build(
            allocator,
            store,
            backend.dev.LirCodeGenMod.host_lir_codegen_target,
        );
        defer static_strings.deinit();

        var codegen = try HostLirCodeGen.init(allocator, store, layouts, static_strings.entries, .preserve);
        defer codegen.deinit();
        try codegen.compileAllProcSpecs(store.getProcSpecs());

        var runtime_env = RuntimeHostEnv.init(allocator);
        defer runtime_env.deinit();

        const results = try allocator.alloc(BoolRootEvalResult, roots.len);
        var result_len: usize = 0;
        errdefer deinitPartialBoolRootEvalResults(allocator, results, result_len);

        for (roots, 0..) |root, i| {
            const entrypoint = try codegen.generateEntrypointWrapper(
                root.symbol_name,
                root.proc,
                root.arg_layouts,
                root.ret_layout,
            );
            var executable = try ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
                codegen.getGeneratedCode(),
                entrypoint.offset,
                codegen.getUnwindFunctions(),
            );
            defer executable.deinit();

            results[i] = try runExecutableBoolRoot(allocator, layouts, &executable, root, &runtime_env);
            result_len += 1;
        }

        return results;
    }
}

fn targetPtrWidthBits(target_usize: base.target.TargetUsize) u8 {
    return @intCast(target_usize.size() * 8);
}

fn llvmCompileOptions(target_usize: base.target.TargetUsize, opt: LlvmTestOpt) @import("llvm_compile").CompileOptions {
    const llvm_compile = @import("llvm_compile");
    return switch (opt) {
        .size => .{
            .function_sections = false,
            .use_module_target_triple = true,
            .optimization = llvm_compile.bindings.IrOptimizationLevel.Oz,
            .target_ptr_width_bits = targetPtrWidthBits(target_usize),
        },
        .speed => .{
            .function_sections = false,
            .use_module_target_triple = true,
            .optimization = llvm_compile.bindings.IrOptimizationLevel.O3,
            .target_ptr_width_bits = targetPtrWidthBits(target_usize),
        },
    };
}

fn callLlvmBoolRoot(
    allocator: Allocator,
    layouts: *const LayoutStore,
    entry: *const fn (*builtins.host_abi.RocOps, *TestInvocationContext, [*]u8, ?*anyopaque) callconv(.c) void,
    root: BoolRoot,
    longjmp_on_crash: bool,
    call_index: usize,
    event_callback: ?BoolRootEventCallback,
) TestHelperError!BoolRootEvalResult {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    runtime_env.setLongjmpOnCrash(longjmp_on_crash);
    var event_forwarder: RuntimeHostEventForwarder = undefined;
    if (event_callback) |callback| {
        event_forwarder = .{
            .callback = callback,
            .call_index = call_index,
        };
        runtime_env.setEventCallback(.{
            .context = &event_forwarder,
            .notify = &forwardRuntimeHostEvent,
        });
    }
    runtime_env.resetObservation();
    runtime_env.resetAllocationTracker();
    var test_context: TestInvocationContext = .{};

    const arg_buffer = try zeroedEntrypointArgBufferForLayouts(allocator, layouts, root.arg_layouts);
    defer if (arg_buffer) |buf| allocator.free(buf);

    const ret_buf = try boolRootRetBuffer(allocator, layouts, root.ret_layout);
    defer allocator.free(ret_buf);

    var crash_boundary = runtime_env.enterCrashBoundary();
    defer crash_boundary.deinit();
    const sj = crash_boundary.set();
    if (sj == 0) {
        entry(
            runtime_env.get_ops(),
            &test_context,
            ret_buf.ptr,
            if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
        );
    }

    const outcome: BoolRootEvalOutcome = switch (runtime_env.crashState()) {
        .did_not_crash => .{ .passed = ret_buf[0] != 0 },
        .crashed => blk: {
            if (test_context.expect_err_set != 0) {
                break :blk .{ .expect_err = .{
                    .message = try copyRuntimeCrashMessage(allocator, &runtime_env),
                    .region_start = test_context.expect_err_start,
                    .region_end = test_context.expect_err_end,
                } };
            }
            break :blk .{ .crashed = try copyRuntimeCrashMessage(allocator, &runtime_env) };
        },
    };
    errdefer deinitBoolRootEvalOutcome(allocator, outcome);
    const events = try copyRuntimeHostEvents(allocator, &runtime_env);
    return .{
        .outcome = outcome,
        .events = events,
    };
}

const LlvmBoolRootEntryFn = *const fn (*builtins.host_abi.RocOps, *TestInvocationContext, [*]u8, ?*anyopaque) callconv(.c) void;

const LlvmBoolRootCall = struct {
    layouts: *const LayoutStore,
    entry: LlvmBoolRootEntryFn,
    root: BoolRoot,
};

const LlvmBoolRootWorkerState = struct {
    allocator: Allocator,
    calls: []const LlvmBoolRootCall,
    longjmp_on_crash: bool,
    next_call: std.atomic.Value(usize),
    results: []?BoolRootEvalResult,
    errors: []?TestHelperError,
    completion_callback: ?BoolRootCompletionCallback,
    event_callback: ?BoolRootEventCallback,
};

fn llvmBoolRootWorker(state: *LlvmBoolRootWorkerState) void {
    while (true) {
        const index = state.next_call.fetchAdd(1, .monotonic);
        if (index >= state.calls.len) break;

        const call = state.calls[index];
        state.results[index] = callLlvmBoolRoot(
            state.allocator,
            call.layouts,
            call.entry,
            call.root,
            state.longjmp_on_crash,
            index,
            state.event_callback,
        ) catch |err| {
            state.errors[index] = err;
            return;
        };
        if (state.completion_callback) |callback| {
            callback.complete(callback.context, index, &state.results[index].?);
        }
    }
}

fn deinitBoolRootEvalSlots(allocator: Allocator, slots: []?BoolRootEvalResult) void {
    for (slots) |*slot| {
        if (slot.*) |result| {
            deinitBoolRootEvalResult(allocator, result);
            slot.* = null;
        }
    }
}

fn optimizedTestWorkerCount(root_count: usize, max_workers: ?usize) usize {
    if (root_count <= 1 or builtin.single_threaded) return 1;
    const requested = max_workers orelse (std.Thread.getCpuCount() catch 1);
    return @min(@max(requested, 1), root_count);
}

fn runLlvmBoolRootCalls(
    allocator: Allocator,
    calls: []const LlvmBoolRootCall,
    longjmp_on_crash: bool,
    max_workers: ?usize,
    completion_callback: ?BoolRootCompletionCallback,
    event_callback: ?BoolRootEventCallback,
) TestHelperError![]BoolRootEvalResult {
    const slots = try allocator.alloc(?BoolRootEvalResult, calls.len);
    defer allocator.free(slots);
    for (slots) |*slot| slot.* = null;
    errdefer deinitBoolRootEvalSlots(allocator, slots);

    const errors = try allocator.alloc(?TestHelperError, calls.len);
    defer allocator.free(errors);
    for (errors) |*slot| slot.* = null;

    var state = LlvmBoolRootWorkerState{
        .allocator = allocator,
        .calls = calls,
        .longjmp_on_crash = longjmp_on_crash,
        .next_call = std.atomic.Value(usize).init(0),
        .results = slots,
        .errors = errors,
        .completion_callback = completion_callback,
        .event_callback = event_callback,
    };

    const worker_count = optimizedTestWorkerCount(calls.len, max_workers);
    if (worker_count == 1) {
        llvmBoolRootWorker(&state);
    } else {
        const threads = try allocator.alloc(std.Thread, worker_count);
        defer allocator.free(threads);

        var spawned: usize = 0;
        var spawn_error: ?std.Thread.SpawnError = null;
        while (spawned < worker_count) : (spawned += 1) {
            threads[spawned] = std.Thread.spawn(.{}, llvmBoolRootWorker, .{&state}) catch |err| {
                spawn_error = err;
                break;
            };
        }
        for (threads[0..spawned]) |thread| {
            thread.join();
        }
        if (spawn_error) |err| return err;
    }

    const results = try allocator.alloc(BoolRootEvalResult, calls.len);
    var result_len: usize = 0;
    errdefer deinitPartialBoolRootEvalResults(allocator, results, result_len);

    for (slots, errors) |*slot, maybe_error| {
        if (maybe_error) |err| return err;
        const result = slot.* orelse return error.Internal;
        results[result_len] = result;
        slot.* = null;
        result_len += 1;
    }

    return results;
}

/// Compile and run bool-returning test roots via the LLVM backend.
pub fn llvmEvalBoolRoots(
    allocator: Allocator,
    store: *const lir.LirStore,
    layouts: *const LayoutStore,
    roots: []const BoolRoot,
    opt: LlvmTestOpt,
) TestHelperError![]BoolRootEvalResult {
    const modules = [_]BoolRootModule{.{
        .store = store,
        .layouts = layouts,
        .roots = roots,
    }};
    return llvmEvalBoolRootModules(allocator, modules[0..], opt);
}

/// Compile bool-returning test roots from multiple lowered LIR modules via the
/// LLVM backend, link them into one shared library, and run roots in parallel.
pub fn llvmEvalBoolRootModules(
    allocator: Allocator,
    modules: []const BoolRootModule,
    opt: LlvmTestOpt,
) TestHelperError![]BoolRootEvalResult {
    return llvmEvalBoolRootModulesWithMaxWorkers(allocator, modules, opt, null);
}

/// Compile bool-returning test roots from multiple lowered LIR modules via the
/// LLVM backend, link them into one shared library, and run roots in parallel.
pub fn llvmEvalBoolRootModulesWithMaxWorkers(
    allocator: Allocator,
    modules: []const BoolRootModule,
    opt: LlvmTestOpt,
    max_workers: ?usize,
) TestHelperError![]BoolRootEvalResult {
    return llvmEvalBoolRootModulesWithMaxWorkersAndCallback(allocator, modules, opt, max_workers, null);
}

/// Compile bool-returning test roots from multiple lowered LIR modules via the
/// LLVM backend, link them into one shared library, run roots in parallel, and
/// publish each successful root result as soon as its worker finishes.
pub fn llvmEvalBoolRootModulesWithMaxWorkersAndCallback(
    allocator: Allocator,
    modules: []const BoolRootModule,
    opt: LlvmTestOpt,
    max_workers: ?usize,
    completion_callback: ?BoolRootCompletionCallback,
) TestHelperError![]BoolRootEvalResult {
    return llvmEvalBoolRootModulesWithMaxWorkersAndCallbacks(allocator, modules, opt, max_workers, completion_callback, null);
}

/// Compile bool-returning test roots from multiple lowered LIR modules via the
/// LLVM backend, link them into one shared library, run roots in parallel, and
/// publish root-local host events and successful root results while workers run.
pub fn llvmEvalBoolRootModulesWithMaxWorkersAndCallbacks(
    allocator: Allocator,
    modules: []const BoolRootModule,
    opt: LlvmTestOpt,
    max_workers: ?usize,
    completion_callback: ?BoolRootCompletionCallback,
    event_callback: ?BoolRootEventCallback,
) TestHelperError![]BoolRootEvalResult {
    if (@import("builtin").target.os.tag == .freestanding) return error.LlvmBackendUnavailable;
    if (modules.len == 0) return error.LlvmBackendUnavailable;

    const llvm_compile = @import("llvm_compile");

    var bitcodes = try allocator.alloc(llvm_compile.MonoLlvmCodeGen.GenerateResult, modules.len);
    var bitcode_len: usize = 0;
    defer {
        for (bitcodes[0..bitcode_len]) |*bitcode| {
            bitcode.deinit();
        }
        allocator.free(bitcodes);
    }

    var bitcode_slices = try allocator.alloc([]const u32, modules.len);
    defer allocator.free(bitcode_slices);

    var total_roots: usize = 0;
    for (modules) |module| {
        total_roots += module.roots.len;
    }

    for (modules, 0..) |module, module_index| {
        var codegen = llvm_compile.MonoLlvmCodeGen.init(allocator, module.store);
        codegen.layout_store = module.layouts;
        defer codegen.deinit();

        const entrypoints = try allocator.alloc(llvm_compile.MonoLlvmCodeGen.Entrypoint, module.roots.len);
        defer allocator.free(entrypoints);
        for (module.roots, 0..) |root, i| {
            entrypoints[i] = .{
                .symbol_name = root.symbol_name,
                .proc = root.proc,
                .arg_layouts = root.arg_layouts,
                .ret_layout = root.ret_layout,
            };
        }

        const module_name = try std.fmt.allocPrint(allocator, "roc_test_module_{d}", .{module_index});
        defer allocator.free(module_name);
        bitcodes[bitcode_len] = try codegen.generateEntrypointModule(module_name, entrypoints);
        bitcode_slices[bitcode_len] = bitcodes[bitcode_len].bitcode;
        bitcode_len += 1;
    }

    const dylib_path = try llvm_compile.compileBitcodeModulesToSharedLibrary(
        allocator,
        std.Options.debug_io,
        bitcode_slices,
        llvmCompileOptions(modules[0].layouts.targetUsize(), opt),
    );
    defer {
        std.Io.Dir.deleteFileAbsolute(std.Options.debug_io, std.mem.sliceTo(dylib_path, 0)) catch {};
        allocator.free(dylib_path);
    }

    var lib = try EvalDynLib.open(allocator, std.mem.sliceTo(dylib_path, 0));
    defer lib.close();

    var longjmp_on_crash = true;
    if (builtin.target.cpu.arch == .aarch64 and builtin.target.os.tag == .linux) {
        longjmp_on_crash = false;
    }

    const calls = try allocator.alloc(LlvmBoolRootCall, total_roots);
    defer allocator.free(calls);

    var call_index: usize = 0;
    for (modules) |module| {
        for (module.roots) |root| {
            calls[call_index] = .{
                .layouts = module.layouts,
                .entry = lib.lookup(LlvmBoolRootEntryFn, root.symbol_name) orelse return error.LlvmBackendUnavailable,
                .root = root,
            };
            call_index += 1;
        }
    }

    return runLlvmBoolRootCalls(allocator, calls, longjmp_on_crash, max_workers, completion_callback, event_callback);
}

/// Evaluate a lowered program via the LIR interpreter and return the output string.
pub fn lirInterpreterInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError![]u8 {
    const result = try lirInterpreterStrWithStats(allocator, lowered);
    return result.output;
}

/// Evaluate via the LIR interpreter, returning output string and allocation count.
pub fn lirInterpreterStrWithStats(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError!EvalRunResult {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.view.store,
        &lowered.view.layouts,
        runtime_env.get_ops(),
        .preserve,
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

/// Abort classification for a differential interpreter run.
pub const InterpreterAbortKind = enum { crash, runtime_error, comptime_exhaustiveness, expect_err };

/// Abort record for a differential interpreter run.
pub const InterpreterAbort = struct {
    kind: InterpreterAbortKind,
    message: ?[]u8,
};

/// Final outcome of a differential interpreter run.
pub const InterpreterOutcome = union(enum) {
    output: []u8,
    aborted: InterpreterAbort,
};

/// Interpreter run transcript for differential harnesses: final outcome plus
/// host-observable events in execution order. All slices are owned by the
/// caller's allocator.
pub const InterpreterTranscript = struct {
    outcome: InterpreterOutcome,
    dbg_events: [][]u8,
    expect_failures: [][]u8,

    pub fn deinit(self: *InterpreterTranscript, allocator: Allocator) void {
        switch (self.outcome) {
            .output => |bytes| allocator.free(bytes),
            .aborted => |aborted| if (aborted.message) |msg| allocator.free(msg),
        }
        for (self.dbg_events) |bytes| allocator.free(bytes);
        allocator.free(self.dbg_events);
        for (self.expect_failures) |bytes| allocator.free(bytes);
        allocator.free(self.expect_failures);
    }
};

/// Evaluate via the LIR interpreter, returning the full transcript a
/// differential harness compares against an independent execution.
pub fn lirInterpreterTranscript(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError!InterpreterTranscript {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.view.store,
        &lowered.view.layouts,
        runtime_env.get_ops(),
        .preserve,
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    var outcome: InterpreterOutcome = undefined;
    var outcome_owned = true;
    errdefer if (outcome_owned) switch (outcome) {
        .output => |bytes| allocator.free(bytes),
        .aborted => |aborted| if (aborted.message) |msg| allocator.free(msg),
    };
    if (interp.eval(.{ .proc_id = lowered.mainProc(), .arg_layouts = arg_layouts })) |result| {
        const ret_layout = lowered.view.store.getProcSpec(lowered.mainProc()).ret_layout;
        outcome = .{ .output = try copyReturnedRocStr(
            allocator,
            &lowered.view.layouts,
            ret_layout,
            result.value.ptr,
            null,
        ) };
    } else |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.Crash, error.DivisionByZero => {
            const message: ?[]u8 = switch (runtime_env.crashState()) {
                .crashed => |msg| try allocator.dupe(u8, msg),
                .did_not_crash => null,
            };
            outcome = .{ .aborted = .{ .kind = .crash, .message = message } };
        },
        error.RuntimeError => outcome = .{ .aborted = .{ .kind = .runtime_error, .message = null } },
        error.ComptimeExhaustiveness => outcome = .{ .aborted = .{ .kind = .comptime_exhaustiveness, .message = null } },
        error.ExpectErr => {
            const message: ?[]u8 = if (interp.getExpectErrMessage()) |msg|
                try allocator.dupe(u8, msg)
            else
                null;
            outcome = .{ .aborted = .{ .kind = .expect_err, .message = message } };
        },
    }

    var dbg_list = std.ArrayList([]u8).empty;
    errdefer {
        for (dbg_list.items) |bytes| allocator.free(bytes);
        dbg_list.deinit(allocator);
    }
    var expect_list = std.ArrayList([]u8).empty;
    errdefer {
        for (expect_list.items) |bytes| allocator.free(bytes);
        expect_list.deinit(allocator);
    }
    for (runtime_env.events.items) |event| switch (event) {
        .dbg => |bytes| try dbg_list.append(allocator, try allocator.dupe(u8, bytes)),
        .expect_failed => |bytes| try expect_list.append(allocator, try allocator.dupe(u8, bytes)),
        .crashed => {},
    };

    const dbg_events = try dbg_list.toOwnedSlice(allocator);
    errdefer {
        for (dbg_events) |bytes| allocator.free(bytes);
        allocator.free(dbg_events);
    }
    const expect_failures = try expect_list.toOwnedSlice(allocator);
    outcome_owned = false;
    return .{
        .outcome = outcome,
        .dbg_events = dbg_events,
        .expect_failures = expect_failures,
    };
}

/// Evaluate a lowered program via the dev JIT backend and return the output string.
pub fn devEvaluatorInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError![]u8 {
    const result = try devEvaluatorStrWithStats(allocator, lowered);
    return result.output;
}

/// Evaluate via the dev JIT backend, returning output string and allocation count.
pub fn devEvaluatorStrWithStats(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError!EvalRunResult {
    if (comptime !backend.host_lir_codegen_available) {
        return error.DevBackendUnavailable;
    } else {
        var static_strings = try backend.StaticStringData.build(
            allocator,
            &lowered.view.store,
            backend.dev.LirCodeGenMod.host_lir_codegen_target,
        );
        defer static_strings.deinit();

        var codegen = try HostLirCodeGen.init(
            allocator,
            &lowered.view.store,
            &lowered.view.layouts,
            static_strings.entries,
            .preserve,
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
        var exec_mem = try ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
            codegen.getGeneratedCode(),
            entrypoint.offset,
            codegen.getUnwindFunctions(),
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

/// Evaluate a lowered program via the LLVM backend and return the output string.
pub fn llvmEvaluatorInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError![]u8 {
    if (@import("builtin").target.os.tag == .freestanding) return error.LlvmBackendUnavailable;

    const llvm_compile = @import("llvm_compile");
    var codegen = llvm_compile.MonoLlvmCodeGen.init(allocator, &lowered.view.store);
    codegen.layout_store = &lowered.view.layouts;
    defer codegen.deinit();

    const proc = lowered.view.store.getProcSpec(lowered.mainProc());
    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const llvm_entrypoints = [_]llvm_compile.MonoLlvmCodeGen.Entrypoint{.{
        .symbol_name = "roc_eval_test_main",
        .proc = lowered.mainProc(),
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
    }};
    const bitcode = try codegen.generateEntrypointModule("roc_eval_test_module", llvm_entrypoints[0..]);
    defer {
        var owned = bitcode;
        owned.deinit();
    }

    const dylib_path = try llvm_compile.compileToSharedLibrary(allocator, std.Options.debug_io, bitcode.bitcode, .{
        .function_sections = false,
        .use_module_target_triple = true,
        .target_ptr_width_bits = targetPtrWidthBits(lowered.view.layouts.targetUsize()),
    });
    defer {
        std.Io.Dir.deleteFileAbsolute(std.Options.debug_io, std.mem.sliceTo(dylib_path, 0)) catch {};
        allocator.free(dylib_path);
    }

    var lib = try EvalDynLib.open(allocator, std.mem.sliceTo(dylib_path, 0));
    defer lib.close();

    const EntryFn = *const fn (*builtins.host_abi.RocOps, *TestInvocationContext, [*]u8, ?*anyopaque) callconv(.c) void;
    const entry = lib.lookup(EntryFn, "roc_eval_test_main") orelse return error.LlvmBackendUnavailable;

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    if (builtin.target.cpu.arch == .aarch64 and builtin.target.os.tag == .linux) {
        runtime_env.setLongjmpOnCrash(false);
    }

    const arg_buffer = try zeroedEntrypointArgBuffer(allocator, lowered, arg_layouts);
    defer if (arg_buffer) |buf| allocator.free(buf);

    const ret_layout = proc.ret_layout;
    const size_align = lowered.view.layouts.layoutSizeAlign(lowered.view.layouts.getLayout(ret_layout));
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(size_align.size, 1));
    defer allocator.free(ret_buf);
    @memset(ret_buf, 0);

    var crash_boundary = runtime_env.enterCrashBoundary();
    defer crash_boundary.deinit();
    const sj = crash_boundary.set();
    if (sj != 0) return error.Crash;

    var test_context: TestInvocationContext = .{};
    entry(
        runtime_env.get_ops(),
        &test_context,
        ret_buf.ptr,
        if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
    );
    switch (runtime_env.crashState()) {
        .did_not_crash => {},
        .crashed => return error.Crash,
    }

    return copyReturnedRocStr(
        allocator,
        &lowered.view.layouts,
        ret_layout,
        ret_buf.ptr,
        runtime_env.get_ops(),
    );
}

/// Evaluate a lowered program via the wasm backend and return the output string.
pub fn wasmEvaluatorInspectedStr(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError![]u8 {
    const result = try wasmEvaluatorStrWithStats(allocator, lowered);
    return result.output;
}

/// Evaluate via the wasm backend, returning output string and allocation count.
pub fn wasmEvaluatorStrWithStats(allocator: Allocator, lowered: *const LoweredProgram) TestHelperError!EvalRunResult {
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
) TestHelperError![]u8 {
    const layout_val = layout_store.getLayout(ret_layout);
    const is_str =
        ret_layout == .str or
        (layout_val.tag == .scalar and layout_val.getScalar().tag == .str);

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
