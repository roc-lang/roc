//! Testable REPL session core used by the CLI.
//!
//! The command-line entrypoint owns terminal I/O. This module owns statement
//! splitting, definition replacement, source construction, and evaluation
//! through the checked-module inspected-evaluation API.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const compile = @import("compile");
const eval = @import("eval");
const lir = @import("lir");
const parse = @import("parse");
const reporting = @import("reporting");

const Allocator = std.mem.Allocator;
const CoreCtx = @import("ctx").CoreCtx;

const ModuleEnv = can.ModuleEnv;
const ModuleSource = eval.Inspected.ModuleSource;

/// Upper bound on the size of an imported module file the REPL will read.
const max_import_file_bytes: usize = 16 * 1024 * 1024;

const ReplSession = @This();

const RenderError = Allocator.Error || error{WriteFailed};
const ModuleRenderError = eval.Inspected.Error || RenderError;
/// Everything that can go wrong while standing up a session's builtin modules.
pub const ReplInitError = eval.BuiltinModules.InitError;
/// Everything that can go wrong while evaluating or inspecting one REPL input.
pub const ReplStepError = eval.Inspected.Error || RenderError;
const ReplTestError = ReplStepError || ReplInitError || error{
    ParseError,
    TestExpectedEqual,
    TestUnexpectedResult,
};

allocator: Allocator,
/// Compiler I/O context, created at the CLI entrypoint. Supplies both the
/// `std.Io` used for lowering and the filesystem access canonicalization needs
/// to read `import "path" as x : Str`/`: List(U8)` files.
roc_ctx: CoreCtx,
backend_kind: eval.EvalBackend,
specialization_strategy: base.SpecializationStrategy,
definitions: DefinitionStore,
virtual_modules: VirtualModuleStore,
import_policy: ImportPolicy,
last_events: []eval.InspectedRun.Event = &.{},
builtin_modules: *eval.BuiltinModules,
/// Whether this session owns `builtin_modules` (and must deinit it). Tests can
/// borrow a shared, already-published instance to avoid re-publishing the
/// Builtin module for every session; see `initBorrowingBuiltins`.
owns_builtin_modules: bool,
/// Directory that imported sibling modules are resolved against. Defaults to the
/// process working directory (`.`); tests point it at a fixture directory.
module_root: []const u8 = ".",

/// Outcome of evaluating a single REPL input line.
pub const StepResult = union(enum) {
    output: []u8,
    diagnostic: []u8,
    runtime_crash: []u8,
    none,
    exit,

    pub fn deinit(self: StepResult, allocator: Allocator) void {
        switch (self) {
            .output, .diagnostic, .runtime_crash => |bytes| allocator.free(bytes),
            .none, .exit => {},
        }
    }
};

/// Why a REPL input failed, so a frontend can decide whether to keep reading
/// (incomplete input) or report the failure.
pub const LanguageDiagnosticKind = enum {
    incomplete_input,
    parse_error,
    compile_error,
    unsupported_file_import,
};

/// A rendered REPL failure plus enough structure for a frontend to route it.
pub const LanguageDiagnostic = struct {
    kind: LanguageDiagnosticKind,
    input: ?InputInfo,
    message: []u8,
};

/// Metadata for a definition the session just accepted into its scope.
pub const DefinitionCommit = struct {
    name: []const u8,
    kind: DefinitionKind,
    file_import: bool,
};

/// Presentation-neutral outcome of processing one Roc REPL statement.
pub const LanguageStepResult = union(enum) {
    expression: []u8,
    definition: DefinitionCommit,
    diagnostic: LanguageDiagnostic,
    runtime_crash: []u8,
    none,

    pub fn deinit(self: LanguageStepResult, allocator: Allocator) void {
        switch (self) {
            .expression, .runtime_crash => |bytes| allocator.free(bytes),
            .diagnostic => |diagnostic| allocator.free(diagnostic.message),
            .definition, .none => {},
        }
    }
};

/// A frontend command that has already been parsed by its owning UI.
pub const Command = union(enum) {
    help,
    definitions,
    type_of: []const u8,
    exit,
};

pub fn init(
    allocator: Allocator,
    roc_ctx: CoreCtx,
    backend_kind: eval.EvalBackend,
    specialization_strategy: base.SpecializationStrategy,
) ReplInitError!ReplSession {
    const builtin_modules = try allocator.create(eval.BuiltinModules);
    errdefer allocator.destroy(builtin_modules);
    builtin_modules.* = try eval.BuiltinModules.init(allocator);
    return .{
        .allocator = allocator,
        .roc_ctx = roc_ctx,
        .backend_kind = backend_kind,
        .specialization_strategy = specialization_strategy,
        .definitions = DefinitionStore.init(),
        .virtual_modules = .{},
        .import_policy = .filesystem,
        .builtin_modules = builtin_modules,
        .owns_builtin_modules = true,
    };
}

/// Construct a REPL whose imports can only resolve from caller-provided modules.
pub fn initVirtual(
    allocator: Allocator,
    roc_ctx: CoreCtx,
    backend_kind: eval.EvalBackend,
    specialization_strategy: base.SpecializationStrategy,
) ReplInitError!ReplSession {
    var session = try init(allocator, roc_ctx, backend_kind, specialization_strategy);
    session.import_policy = .virtual_only;
    return session;
}

/// Construct a session that borrows a caller-owned, already-published
/// `BuiltinModules`. The session never deinits it, so one published Builtin can
/// back many sessions. Intended for tests that would otherwise re-publish the
/// Builtin module on every assertion.
fn initBorrowingBuiltins(
    allocator: Allocator,
    roc_ctx: CoreCtx,
    backend_kind: eval.EvalBackend,
    builtin_modules: *eval.BuiltinModules,
) ReplSession {
    return .{
        .allocator = allocator,
        .roc_ctx = roc_ctx,
        .backend_kind = backend_kind,
        .specialization_strategy = .lss,
        .definitions = DefinitionStore.init(),
        .virtual_modules = .{},
        .import_policy = .filesystem,
        .builtin_modules = builtin_modules,
        .owns_builtin_modules = false,
    };
}

pub fn deinit(self: *ReplSession) void {
    self.clearLastEvents();
    self.definitions.deinit(self.allocator);
    self.virtual_modules.deinit(self.allocator);
    if (self.owns_builtin_modules) {
        self.builtin_modules.deinit();
        self.allocator.destroy(self.builtin_modules);
    }
}

const ImportPolicy = enum { filesystem, virtual_only };

fn clearLastEvents(self: *ReplSession) void {
    for (self.last_events) |*event| event.deinit(self.allocator);
    self.allocator.free(self.last_events);
    self.last_events = &.{};
}

/// Transfer the ordered host events from the most recent evaluated expression.
pub fn takeEvents(self: *ReplSession) []eval.InspectedRun.Event {
    const events = self.last_events;
    self.last_events = &.{};
    return events;
}

/// Remove every REPL definition while retaining the configured virtual modules.
pub fn clear(self: *ReplSession) void {
    self.clearLastEvents();
    self.definitions.deinit(self.allocator);
    self.definitions = DefinitionStore.init();
}

/// Atomically replace the virtual module set. Successful replacement clears
/// definitions because their checked import identities may have changed.
pub fn replaceVirtualModules(self: *ReplSession, modules: []const ModuleSource) (Allocator.Error || error{ DuplicateVirtualModule, ReservedVirtualModule })!void {
    var replacement: VirtualModuleStore = .{};
    errdefer replacement.deinit(self.allocator);

    for (modules) |module| {
        if (std.mem.eql(u8, module.name, eval.InspectedRun.repl_effect_module_name)) {
            return error.ReservedVirtualModule;
        }
        if (replacement.find(module.name) != null) return error.DuplicateVirtualModule;
        try replacement.append(self.allocator, module.name, module.source);
    }

    self.virtual_modules.deinit(self.allocator);
    self.virtual_modules = replacement;
    self.clear();
}

fn prePublishedBuiltin(self: *ReplSession) eval.Inspected.PrePublishedBuiltin {
    return .{
        .env = self.builtin_modules.builtin_module.env,
        .indices = self.builtin_modules.builtin_indices,
        .artifact = &self.builtin_modules.checked_artifact,
    };
}

/// Process one complete Roc REPL statement and return the user-visible output.
pub fn step(self: *ReplSession, input: []const u8) ReplStepError![]u8 {
    const result = try self.stepWithConfig(input, reporting.ReportingConfig.initColorTerminal());
    return switch (result) {
        .output => |bytes| bytes,
        .diagnostic => |bytes| bytes,
        .runtime_crash => |message| {
            defer self.allocator.free(message);
            return std.fmt.allocPrint(
                self.allocator,
                "This Roc code crashed with: \"{f}\"",
                .{std.zig.fmtString(message)},
            );
        },
        .none => self.allocator.dupe(u8, ""),
        .exit => self.allocator.dupe(u8, "Goodbye!"),
    };
}

/// Execute a typed command that was parsed by a frontend-owned command router.
pub fn executeCommandWithConfig(self: *ReplSession, command: Command, report_config: reporting.ReportingConfig) ReplStepError!StepResult {
    self.clearLastEvents();
    return switch (command) {
        .help => .{ .output = try self.helpText() },
        .definitions => .{ .output = try self.printDefs(report_config.shouldUseColors()) },
        .type_of => |name| .{ .output = try self.printTypeOfVar(name, report_config.shouldUseColors()) },
        .exit => .exit,
    };
}

/// Process one complete Roc REPL statement and keep stdout/stderr output separate.
pub fn stepWithConfig(self: *ReplSession, input: []const u8, report_config: reporting.ReportingConfig) ReplStepError!StepResult {
    const result = try self.stepLanguageWithConfig(input, report_config);
    return switch (result) {
        .expression => |output| .{ .output = output },
        .definition => |definition| if (definition.kind == .annotation) .none else blk: {
            const verb = if (definition.kind == .import) "imported" else "assigned";
            if (definition.kind == .value) {
                const names = try declarationBoundNames(self.allocator, input);
                defer self.allocator.free(names);
                break :blk .{ .output = try formatDefinitionResult(self.allocator, verb, names) };
            }
            break :blk .{ .output = try std.fmt.allocPrint(self.allocator, "{s} `{s}`", .{ verb, definition.name }) };
        },
        .diagnostic => |diagnostic| .{ .diagnostic = diagnostic.message },
        .runtime_crash => |message| .{ .runtime_crash = message },
        .none => .none,
    };
}

/// Process one complete Roc REPL statement without frontend commands or
/// presentation strings.
pub fn stepLanguageWithConfig(self: *ReplSession, input: []const u8, report_config: reporting.ReportingConfig) ReplStepError!LanguageStepResult {
    self.clearLastEvents();
    const line = std.mem.trim(u8, input, " \t\r\n");
    if (line.len == 0) return .none;

    const input_info = switch (try self.inputStatus(line)) {
        .complete => |info| info,
        .incomplete => return .{ .diagnostic = .{
            .kind = .incomplete_input,
            .input = null,
            .message = try self.renderStatementParseDiagnostics(line, report_config),
        } },
        .invalid => return .{ .diagnostic = .{
            .kind = .parse_error,
            .input = null,
            .message = try self.renderStatementParseDiagnostics(line, report_config),
        } },
    };

    switch (input_info.kind) {
        .expression => {
            const result = try self.evaluateExpression(line, report_config);
            return switch (result) {
                .output => |output| .{ .expression = output },
                .diagnostic => |message| .{ .diagnostic = .{
                    .kind = .compile_error,
                    .input = input_info,
                    .message = message,
                } },
                .runtime_crash => |message| .{ .runtime_crash = message },
                .none, .exit => error.Internal,
            };
        },
        .definition => {
            const name = input_info.name orelse line;
            if (input_info.file_import and self.import_policy == .virtual_only) {
                return .{ .diagnostic = .{
                    .kind = .unsupported_file_import,
                    .input = input_info,
                    .message = try self.allocator.dupe(u8, "File imports are not available in this REPL. Provide named virtual modules instead."),
                } };
            }
            if (input_info.definition_kind == .annotation) {
                try self.addOrReplaceDefinitionWithImportKind(line, name, .annotation, false);
                return .{ .definition = .{ .name = name, .kind = .annotation, .file_import = false } };
            }

            const bound_names = if (input_info.definition_kind == .value)
                try declarationBoundNames(self.allocator, line)
            else
                null;
            defer if (bound_names) |names| self.allocator.free(names);

            var snapshot = try self.definitions.snapshot(self.allocator);
            errdefer self.definitions.restore(self.allocator, &snapshot);
            if (bound_names) |names| {
                try self.definitions.addOrReplaceNames(self.allocator, line, names, input_info.definition_kind, input_info.file_import);
            } else {
                try self.addOrReplaceDefinitionWithImportKind(line, name, input_info.definition_kind, input_info.file_import);
            }
            const validation = try self.validateDefinitions(report_config);
            if (!validation.valid) {
                self.definitions.restore(self.allocator, &snapshot);
                if (validation.error_message) |msg| return .{ .diagnostic = .{
                    .kind = .compile_error,
                    .input = input_info,
                    .message = msg,
                } };
                return .{ .diagnostic = .{
                    .kind = .compile_error,
                    .input = input_info,
                    .message = try self.allocator.dupe(u8, "Definition failed to compile"),
                } };
            }
            snapshot.deinit(self.allocator);
            return .{ .definition = .{
                .name = name,
                .kind = input_info.definition_kind,
                .file_import = input_info.file_import,
            } };
        },
    }
}

/// Split pasted input into complete REPL statements using the parser as the boundary check.
pub fn splitInputIntoStatements(self: *ReplSession, input: []const u8) Allocator.Error![][]const u8 {
    return splitInputIntoStatementsWithAllocator(self.allocator, input);
}

/// Split pasted input into complete REPL statements using the parser as the boundary check.
pub fn splitInputIntoStatementsWithAllocator(allocator: Allocator, input: []const u8) Allocator.Error![][]const u8 {
    const trimmed_input = std.mem.trim(u8, input, " \t\r\n");
    if (trimmed_input.len == 0) return allocator.alloc([]const u8, 0);
    var result = std.ArrayList([]const u8).empty;
    errdefer {
        for (result.items) |slice| allocator.free(slice);
        result.deinit(allocator);
    }

    var current = std.ArrayList(u8).empty;
    defer current.deinit(allocator);

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |raw_line| {
        const trimmed_line = std.mem.trimEnd(u8, raw_line, " \t\r");
        if (std.mem.trim(u8, trimmed_line, " \t\r\n").len == 0 and current.items.len == 0) {
            continue;
        }

        if (current.items.len > 0) try current.append(allocator, '\n');
        try current.appendSlice(allocator, trimmed_line);

        const candidate = std.mem.trim(u8, current.items, " \t\r\n");
        if (candidate.len == 0) continue;
        switch (try inputStatusWithAllocator(allocator, candidate)) {
            .complete => {
                try result.append(allocator, try allocator.dupe(u8, candidate));
                current.clearRetainingCapacity();
            },
            .incomplete, .invalid => {},
        }
    }

    const remaining = std.mem.trim(u8, current.items, " \t\r\n");
    if (remaining.len > 0) {
        try result.append(allocator, try allocator.dupe(u8, remaining));
    }

    return result.toOwnedSlice(allocator);
}

/// Free slices returned by `splitInputIntoStatements`.
pub fn freeStatementSlices(self: *ReplSession, slices: []const []const u8) void {
    freeStatementSlicesWithAllocator(self.allocator, slices);
}

/// Free slices returned by `splitInputIntoStatementsWithAllocator`.
pub fn freeStatementSlicesWithAllocator(allocator: Allocator, slices: []const []const u8) void {
    for (slices) |slice| allocator.free(slice);
    allocator.free(slices);
}

/// Add or replace one stored definition while preserving definition order.
pub fn addOrReplaceDefinition(self: *ReplSession, source: []const u8, name: []const u8, kind: DefinitionKind) Allocator.Error!void {
    try self.addOrReplaceDefinitionWithImportKind(source, name, kind, false);
}

fn addOrReplaceDefinitionWithImportKind(
    self: *ReplSession,
    source: []const u8,
    name: []const u8,
    kind: DefinitionKind,
    file_import: bool,
) Allocator.Error!void {
    try self.definitions.addOrReplace(self.allocator, source, name, kind, file_import);
}

/// Build a block expression containing all current definitions followed by `expr`.
pub fn buildFullSource(self: *const ReplSession, expr: []const u8) Allocator.Error![]u8 {
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(self.allocator);

    try out.appendSlice(self.allocator, "{\n");
    for (self.definitions.items.items) |definition| {
        var lines = std.mem.splitScalar(u8, definition.source, '\n');
        while (lines.next()) |line| {
            try out.appendSlice(self.allocator, "    ");
            try out.appendSlice(self.allocator, line);
            try out.append(self.allocator, '\n');
        }
    }
    try out.appendSlice(self.allocator, "    ");
    try out.appendSlice(self.allocator, expr);
    try out.appendSlice(self.allocator, "\n}");

    return out.toOwnedSlice(self.allocator);
}

/// Build module-level source for the current stored definitions.
pub fn definitionsSource(self: *const ReplSession) Allocator.Error![]u8 {
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(self.allocator);

    for (self.definitions.items.items) |definition| {
        try out.appendSlice(self.allocator, definition.source);
        try out.append(self.allocator, '\n');
    }

    return out.toOwnedSlice(self.allocator);
}

/// Build source containing only the stored `import` statements, used to discover
/// which sibling modules the session needs before compiling.
fn importDefinitionsSource(self: *const ReplSession) Allocator.Error![]u8 {
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(self.allocator);

    for (self.definitions.items.items) |definition| {
        if (definition.kind != .import) continue;
        try out.appendSlice(self.allocator, definition.source);
        try out.append(self.allocator, '\n');
    }

    return out.toOwnedSlice(self.allocator);
}

/// Outcome of resolving the sibling modules imported by the current session.
///
/// SECURITY: native CLI sessions may read sibling module files from their
/// configured directory. Virtual-only sessions resolve exclusively from the
/// explicit in-memory module set; the filesystem branch is compile-time dead
/// in freestanding builds.
const ImportResolution = union(enum) {
    /// Module sources ordered so every module precedes the modules that import
    /// it (the order inspected compilation requires). Caller owns each name/source and
    /// the slice; free with `freeModuleSources`.
    resolved: []ModuleSource,
    /// A rendered, caller-owned diagnostic explaining why resolution failed.
    failed: []u8,
};

const VirtualModule = struct {
    name: []u8,
    source: []u8,
};

const VirtualModuleStore = struct {
    items: std.ArrayList(VirtualModule) = .empty,

    fn append(self: *VirtualModuleStore, allocator: Allocator, name: []const u8, source: []const u8) Allocator.Error!void {
        const owned_name = try allocator.dupe(u8, name);
        errdefer allocator.free(owned_name);
        const owned_source = try allocator.dupe(u8, source);
        errdefer allocator.free(owned_source);
        try self.items.append(allocator, .{ .name = owned_name, .source = owned_source });
    }

    fn find(self: *const VirtualModuleStore, name: []const u8) ?[]const u8 {
        for (self.items.items) |module| {
            if (std.mem.eql(u8, module.name, name)) return module.source;
        }
        return null;
    }

    fn deinit(self: *VirtualModuleStore, allocator: Allocator) void {
        for (self.items.items) |module| {
            allocator.free(module.name);
            allocator.free(module.source);
        }
        self.items.deinit(allocator);
        self.* = .{};
    }
};

const VisitState = enum { in_progress, done };

/// Free a slice of `ModuleSource` whose `name`/`source` were heap-allocated here.
fn freeModuleSources(self: *const ReplSession, sources: []ModuleSource) void {
    for (sources) |ms| {
        self.allocator.free(ms.name);
        self.allocator.free(ms.source);
    }
    self.allocator.free(sources);
}

/// Resolve every sibling module reachable from the session's `import` statements
/// by reading files from the working directory. Returns a topologically ordered
/// list (dependencies first), or a diagnostic if a module file is missing or the
/// imports form a cycle.
fn resolveImports(self: *ReplSession) Allocator.Error!ImportResolution {
    var sources = std.ArrayList(ModuleSource).empty;
    errdefer {
        for (sources.items) |ms| {
            self.allocator.free(ms.name);
            self.allocator.free(ms.source);
        }
        sources.deinit(self.allocator);
    }

    var visited = std.StringHashMap(VisitState).init(self.allocator);
    defer {
        var it = visited.keyIterator();
        while (it.next()) |key| self.allocator.free(key.*);
        visited.deinit();
    }

    const import_source = try self.importDefinitionsSource();
    defer self.allocator.free(import_source);

    const seed_imports = try self.importsOf(import_source);
    defer {
        for (seed_imports) |seed_import| self.allocator.free(seed_import.import_name);
        self.allocator.free(seed_imports);
    }

    var failure: ?[]u8 = null;
    errdefer if (failure) |msg| self.allocator.free(msg);

    for (seed_imports) |seed_import| {
        const name = (try compile.module_discovery.resolveLocalImportLogicalPath(
            self.allocator,
            "Repl",
            seed_import,
        )) orelse {
            failure = try std.fmt.allocPrint(
                self.allocator,
                "The import `{s}` traverses above the REPL module root.",
                .{seed_import.import_name},
            );
            break;
        };
        defer self.allocator.free(name);
        try self.addModuleRecursive(name, &sources, &visited, &failure);
        if (failure != null) break;
    }

    if (failure) |msg| {
        for (sources.items) |ms| {
            self.allocator.free(ms.name);
            self.allocator.free(ms.source);
        }
        sources.deinit(self.allocator);
        return .{ .failed = msg };
    }

    return .{ .resolved = try sources.toOwnedSlice(self.allocator) };
}

/// Post-order DFS that appends `module_name` after all of its own imports, so
/// the resulting list is topologically ordered. On the first failure it sets
/// `failure` and unwinds without appending.
fn addModuleRecursive(
    self: *ReplSession,
    module_name: []const u8,
    sources: *std.ArrayList(ModuleSource),
    visited: *std.StringHashMap(VisitState),
    failure: *?[]u8,
) Allocator.Error!void {
    if (failure.* != null) return;

    // Compiler builtins (e.g. `Str`, `Num`) resolve automatically; never read
    // them from disk.
    if (can.CIR.Import.isCompilerBuiltinImportName(module_name)) return;

    if (visited.get(module_name)) |state| {
        switch (state) {
            .done => return,
            .in_progress => {
                failure.* = try std.fmt.allocPrint(
                    self.allocator,
                    "I ran into a cyclic import involving `{s}`. The REPL can't load modules that import each other yet.",
                    .{module_name},
                );
                return;
            },
        }
    }

    {
        const key = try self.allocator.dupe(u8, module_name);
        errdefer self.allocator.free(key);
        try visited.put(key, .in_progress);
    }

    const source = if (self.import_policy == .virtual_only and
        std.mem.eql(u8, module_name, eval.InspectedRun.repl_effect_module_name))
        try self.allocator.dupe(u8, eval.InspectedRun.repl_effect_module_source)
    else if (self.virtual_modules.find(module_name)) |virtual_source|
        try self.allocator.dupe(u8, virtual_source)
    else source: {
        if (self.import_policy == .virtual_only) {
            failure.* = try std.fmt.allocPrint(
                self.allocator,
                "The imported module `{s}` was not provided to this REPL.",
                .{module_name},
            );
            return;
        }

        if (comptime builtin.target.os.tag == .freestanding) {
            failure.* = try std.fmt.allocPrint(
                self.allocator,
                "The imported module `{s}` was not provided to this REPL.",
                .{module_name},
            );
            return;
        } else {
            const rel_path = try modulePathFromName(self.allocator, module_name);
            defer self.allocator.free(rel_path);

            // Resolve relative to `module_root`. When it is the default `.` we read
            // `rel_path` directly so diagnostics stay free of a `./` prefix.
            const read_path = if (std.mem.eql(u8, self.module_root, "."))
                rel_path
            else
                try std.fs.path.join(self.allocator, &.{ self.module_root, rel_path });
            defer if (read_path.ptr != rel_path.ptr) self.allocator.free(read_path);

            break :source std.Io.Dir.cwd().readFileAlloc(self.roc_ctx.std_io, read_path, self.allocator, std.Io.Limit.limited(max_import_file_bytes)) catch |err| {
                failure.* = switch (err) {
                    error.FileNotFound => try std.fmt.allocPrint(
                        self.allocator,
                        "I couldn't find the imported module `{s}` (looked for `{s}` relative to the current directory).",
                        .{ module_name, read_path },
                    ),
                    error.AccessDenied,
                    error.AntivirusInterference,
                    error.BadPathName,
                    error.Canceled,
                    error.ConnectionResetByPeer,
                    error.DeviceBusy,
                    error.FileBusy,
                    error.FileLocksUnsupported,
                    error.FileTooBig,
                    error.InputOutput,
                    error.IsDir,
                    error.LockViolation,
                    error.NameTooLong,
                    error.NetworkNotFound,
                    error.NoDevice,
                    error.NoSpaceLeft,
                    error.NotDir,
                    error.NotOpenForReading,
                    error.OutOfMemory,
                    error.PathAlreadyExists,
                    error.PermissionDenied,
                    error.PipeBusy,
                    error.ProcessFdQuotaExceeded,
                    error.ReadOnlyFileSystem,
                    error.SocketUnconnected,
                    error.StreamTooLong,
                    error.SymLinkLoop,
                    error.SystemFdQuotaExceeded,
                    error.SystemResources,
                    error.Unexpected,
                    error.WouldBlock,
                    => try std.fmt.allocPrint(
                        self.allocator,
                        "I couldn't read the imported module `{s}` (`{s}`): {s}.",
                        .{ module_name, read_path, @errorName(err) },
                    ),
                };
                return;
            };
        }
    };
    errdefer self.allocator.free(source);

    // Resolve this module's own imports first so dependencies are appended
    // before it.
    const child_imports = try self.importsOf(source);
    defer {
        for (child_imports) |child_import| self.allocator.free(child_import.import_name);
        self.allocator.free(child_imports);
    }
    for (child_imports) |child_import| {
        const child = (try compile.module_discovery.resolveLocalImportLogicalPath(
            self.allocator,
            module_name,
            child_import,
        )) orelse {
            failure.* = try std.fmt.allocPrint(
                self.allocator,
                "The import `{s}` traverses above the REPL module root.",
                .{child_import.import_name},
            );
            self.allocator.free(source);
            return;
        };
        defer self.allocator.free(child);
        try self.addModuleRecursive(child, sources, visited, failure);
        if (failure.* != null) {
            self.allocator.free(source);
            return;
        }
    }

    // Mark done before appending so no fallible call runs after the append
    // (which transfers ownership of `source` to `sources`); otherwise a failing
    // call here could let both this errdefer and the caller's free `source`.
    try visited.put(module_name, .done);
    const owned_name = try self.allocator.dupe(u8, module_name);
    errdefer self.allocator.free(owned_name);
    try sources.append(self.allocator, .{ .name = owned_name, .source = source });
}

/// Parse `source` as a module and return the unqualified sibling module names it
/// imports (caller owns the slice and each name).
fn importsOf(self: *ReplSession, source: []const u8) Allocator.Error![]compile.module_discovery.LocalImport {
    var env = try ModuleEnv.init(self.allocator, source);
    defer env.deinit();
    env.common.source = source;
    try env.common.calcLineStarts(self.allocator);

    const ast = try parse.file(self.allocator, &env.common);
    defer ast.deinit();

    return compile.module_discovery.extractImportsFromDeclIndex(ast, self.allocator);
}

/// Map a module name to its source path: `Util` -> `Util.roc`,
/// `Foo/Bar` -> `Foo/Bar.roc`.
fn modulePathFromName(allocator: Allocator, module_name: []const u8) Allocator.Error![]u8 {
    var buffer = std.ArrayList(u8).empty;
    errdefer buffer.deinit(allocator);

    var it = std.mem.splitScalar(u8, module_name, '/');
    var first = true;
    while (it.next()) |part| {
        if (!first) try buffer.appendSlice(allocator, std.fs.path.sep_str) else first = false;
        try buffer.appendSlice(allocator, part);
    }
    try buffer.appendSlice(allocator, ".roc");

    return buffer.toOwnedSlice(allocator);
}

fn helpText(self: *ReplSession) Allocator.Error![]u8 {
    return self.allocator.dupe(u8,
        \\Enter an expression or definition.
        \\
        \\Commands:
        \\  :help               Show this help
        \\  :quit, :q, :exit    Exit the REPL
        \\  :defs               Print the currently known definitions
        \\  :t <identifier>     Print the type of a given identifier
        \\
    );
}

fn printDefs(self: *ReplSession, use_color: bool) ReplStepError![]u8 {
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(self.allocator);

    var ret = try self.initParsedResources();
    defer ret.deinit(self.allocator);
    const env = ret.module_env;

    var tw = try env.initTypeWriter();
    defer tw.deinit();

    for (self.definitions.items.items) |item| {
        switch (item.kind) {
            .value => {
                for (item.names) |name| {
                    const pattern = getBindingPatternOfName(env, name) orelse continue;
                    try tw.write(ModuleEnv.varFrom(pattern), .one_line);
                    if (use_color) {
                        try out.print(self.allocator, "\x1b[3m\x1b[90m{s} : {s}\x1b[0m\n", .{ name, tw.get() });
                    } else {
                        try out.print(self.allocator, "{s} : {s}\n", .{ name, tw.get() });
                    }
                }
                try out.print(self.allocator, "{s}\n\n", .{item.source});
            },
            .annotation => {
                // italics, usually succeeded by a .value let-binding
                if (use_color) {
                    try out.print(self.allocator, "\x1b[3m{s}\x1b[0m\n", .{item.source});
                } else {
                    try out.print(self.allocator, "{s}\n", .{item.source});
                }
            },
            .type_decl, .import => {
                if (use_color) {
                    try out.print(self.allocator, "\x1b[3m{s}\x1b[0m\n\n", .{item.source});
                } else {
                    try out.print(self.allocator, "{s}\n\n", .{item.source});
                }
            },
        }
    }

    return try out.toOwnedSlice(self.allocator);
}

fn printTypeOfVar(self: *ReplSession, name: []const u8, use_color: bool) ReplStepError![]u8 {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(self.allocator);

    var ret = try self.initParsedResources();
    defer ret.deinit(self.allocator);
    var env = ret.module_env;

    var tw = try env.initTypeWriter();
    defer tw.deinit();

    if (getBindingPatternOfName(env, name)) |pattern| {
        try tw.write(ModuleEnv.varFrom(pattern), .one_line);
        if (use_color) {
            try out.print(self.allocator, "\x1b[3m\x1b[90m{s} : {s}\x1b[0m\n", .{ name, tw.get() });
        } else {
            try out.print(self.allocator, "{s} : {s}\n", .{ name, tw.get() });
        }
    } else {
        try out.print(self.allocator, "Did not find a definition for `{s}`\n", .{name});
    }

    return out.toOwnedSlice(self.allocator);
}

/// Type information for an expression without evaluating it or changing the session.
pub fn inspectExpressionType(
    self: *ReplSession,
    expr: []const u8,
    report_config: reporting.ReportingConfig,
) ReplStepError!StepResult {
    const definitions = try self.definitionsSource();
    defer self.allocator.free(definitions);

    const source = try std.fmt.allocPrint(
        self.allocator,
        "{s}\nrepl_inspect_value = || {{\n{s}\n}}\nmain = \"\"\n",
        .{ definitions, expr },
    );
    defer self.allocator.free(source);

    const import_sources = switch (try self.resolveImports()) {
        .resolved => |modules| modules,
        .failed => |message| return .{ .diagnostic = message },
    };
    defer self.freeModuleSources(import_sources);

    var parsed = eval.Inspected.parseAndCanonicalizeProgramPublishedRootsWithBuiltin(
        self.allocator,
        .module,
        source,
        import_sources,
        self.prePublishedBuiltin(),
        self.roc_ctx,
    ) catch |err| switch (err) {
        error.ParseError => return .{ .diagnostic = try self.renderModuleParseDiagnostics(source, report_config) },
        error.TypeCheckError => return .{ .diagnostic = try self.renderModuleProblems(source, import_sources, report_config) },
        else => return err,
    };
    defer parsed.deinit(self.allocator);

    if (try eval.Inspected.parsedResourcesHaveErrorDiagnostics(self.allocator, &parsed)) {
        return .{ .diagnostic = try self.renderModuleProblems(source, import_sources, report_config) };
    }

    const def_idx = getDefOfName(parsed.module_env, "repl_inspect_value") orelse
        return .{ .diagnostic = try self.allocator.dupe(u8, "Expression did not produce a checked definition") };

    var current_var = ModuleEnv.varFrom(def_idx);
    const return_var = while (true) {
        const resolved = parsed.module_env.types.resolveVar(current_var);
        switch (resolved.desc.content) {
            .alias => |alias| current_var = parsed.module_env.types.getAliasBackingVar(alias),
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |function| {
                    if (function.args.len() != 0) return error.Internal;
                    break function.ret;
                },
                .record,
                .record_unbound,
                .tuple,
                .nominal_type,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => return error.Internal,
            },
            .err, .flex, .rigid, .field_presence => return error.Internal,
        }
    };
    var tw = try parsed.module_env.initTypeWriter();
    defer tw.deinit();
    try tw.write(return_var, .one_line);
    return .{ .output = try self.allocator.dupe(u8, tw.get()) };
}

/// One completion candidate: the identifier, what kind of definition it is,
/// and its rendered type when the session could infer one.
pub const CompletionItem = struct {
    label: []u8,
    kind: DefinitionKind,
    detail: ?[]u8,

    fn deinit(self: *CompletionItem, allocator: Allocator) void {
        allocator.free(self.label);
        if (self.detail) |detail| allocator.free(detail);
    }
};

/// Whether this session contains an annotation whose value has not been entered
/// yet. This is a valid intermediate REPL state, but the whole definition set
/// cannot be checked until the matching value arrives.
pub fn hasPendingAnnotation(self: *const ReplSession) bool {
    for (self.definitions.items.items) |definition| {
        if (definition.kind != .annotation) continue;
        for (definition.names) |name| {
            if (!self.definitions.hasKind(name, .value)) return true;
        }
    }
    return false;
}

/// Whether completion details contain checked types for value definitions.
pub fn completionDetailsAvailable(self: *const ReplSession) bool {
    return !self.hasPendingAnnotation();
}

fn appendCompletionItem(
    self: *ReplSession,
    items: *std.ArrayList(CompletionItem),
    name: []const u8,
    kind: DefinitionKind,
    detail: ?[]u8,
) Allocator.Error!void {
    errdefer if (detail) |bytes| self.allocator.free(bytes);
    const label = try self.allocator.dupe(u8, name);
    errdefer self.allocator.free(label);
    try items.append(self.allocator, .{
        .label = label,
        .kind = kind,
        .detail = detail,
    });
}

fn completionItemsWithoutDetails(self: *ReplSession) Allocator.Error![]CompletionItem {
    var items = std.ArrayList(CompletionItem).empty;
    errdefer {
        for (items.items) |*item| item.deinit(self.allocator);
        items.deinit(self.allocator);
    }

    for (self.definitions.items.items) |definition| {
        for (definition.names) |name| {
            if (definition.kind == .annotation and self.definitions.hasKind(name, .value)) continue;
            try self.appendCompletionItem(&items, name, definition.kind, null);
        }
    }
    return items.toOwnedSlice(self.allocator);
}

/// Return top-level definitions available in this session. Value details are
/// checked types unless the session contains a pending standalone annotation;
/// callers can distinguish that explicit state with `completionDetailsAvailable`.
pub fn completionItems(self: *ReplSession) ReplStepError![]CompletionItem {
    if (!self.completionDetailsAvailable()) return self.completionItemsWithoutDetails();

    var parsed = try self.initParsedResources();
    defer parsed.deinit(self.allocator);

    var items = std.ArrayList(CompletionItem).empty;
    errdefer {
        for (items.items) |*item| item.deinit(self.allocator);
        items.deinit(self.allocator);
    }

    var tw = try parsed.module_env.initTypeWriter();
    defer tw.deinit();
    for (self.definitions.items.items) |definition| {
        if (definition.kind == .annotation) continue;
        for (definition.names) |name| {
            var detail: ?[]u8 = null;
            if (definition.kind == .value) {
                if (getBindingPatternOfName(parsed.module_env, name)) |pattern_idx| {
                    try tw.write(ModuleEnv.varFrom(pattern_idx), .one_line);
                    detail = try self.allocator.dupe(u8, tw.get());
                }
            }
            try self.appendCompletionItem(&items, name, definition.kind, detail);
        }
    }
    return items.toOwnedSlice(self.allocator);
}

/// Release a completion list returned by `completionItems`.
pub fn freeCompletionItems(self: *ReplSession, items: []CompletionItem) void {
    for (items) |*item| item.deinit(self.allocator);
    self.allocator.free(items);
}

/// A definition currently held by the session, with the source text that
/// created it so a frontend can re-display or persist the session.
pub const StoredDefinition = struct {
    name: []u8,
    source: []u8,
    kind: DefinitionKind,
    file_import: bool,

    fn deinit(self: *StoredDefinition, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.source);
    }
};

/// Copy the ordered definitions that constitute the current session state.
pub fn storedDefinitions(self: *const ReplSession) Allocator.Error![]StoredDefinition {
    const stored = try self.allocator.alloc(StoredDefinition, self.definitions.items.items.len);
    var initialized: usize = 0;
    errdefer {
        for (stored[0..initialized]) |*definition| definition.deinit(self.allocator);
        self.allocator.free(stored);
    }

    for (self.definitions.items.items, 0..) |definition, index| {
        const name = try self.allocator.dupe(u8, if (definition.names.len > 0) definition.names[0] else definition.source);
        errdefer self.allocator.free(name);
        const source = try self.allocator.dupe(u8, definition.source);
        stored[index] = .{
            .name = name,
            .source = source,
            .kind = definition.kind,
            .file_import = definition.file_import,
        };
        initialized += 1;
    }
    return stored;
}

/// Release a definition list returned by `storedDefinitions`.
pub fn freeStoredDefinitions(self: *const ReplSession, stored: []StoredDefinition) void {
    for (stored) |*definition| definition.deinit(self.allocator);
    self.allocator.free(stored);
}

/// Number of definitions currently in the session scope.
pub fn definitionCount(self: *const ReplSession) usize {
    return self.definitions.count();
}

/// A module the session serves from memory instead of the filesystem.
pub const StoredVirtualModule = struct {
    name: []u8,
    source: []u8,

    fn deinit(self: *StoredVirtualModule, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.source);
    }
};

/// Copy the virtual modules required to reconstruct this session.
pub fn storedVirtualModules(self: *const ReplSession, allocator: Allocator) Allocator.Error![]StoredVirtualModule {
    const modules = try allocator.alloc(StoredVirtualModule, self.virtual_modules.items.items.len);
    var initialized: usize = 0;
    errdefer {
        for (modules[0..initialized]) |*module| module.deinit(allocator);
        allocator.free(modules);
    }
    for (self.virtual_modules.items.items, 0..) |module, index| {
        const name = try allocator.dupe(u8, module.name);
        errdefer allocator.free(name);
        const source = try allocator.dupe(u8, module.source);
        modules[index] = .{ .name = name, .source = source };
        initialized += 1;
    }
    return modules;
}

/// Release a virtual-module list returned by `storedVirtualModules`.
pub fn freeStoredVirtualModules(_: *const ReplSession, allocator: Allocator, modules: []StoredVirtualModule) void {
    for (modules) |*module| module.deinit(allocator);
    allocator.free(modules);
}

fn initParsedResources(self: *ReplSession) ReplStepError!eval.Inspected.ParsedResources {
    const definitions = try self.definitionsSource();
    defer self.allocator.free(definitions);

    const source = try std.fmt.allocPrint(self.allocator, "{s}\nmain = \"\"\n", .{definitions});
    defer self.allocator.free(source);

    const import_sources = switch (try self.resolveImports()) {
        .resolved => |modules| modules,
        .failed => |message| {
            self.allocator.free(message);
            return error.TypeCheckError;
        },
    };
    defer self.freeModuleSources(import_sources);

    return try eval.Inspected.parseAndCanonicalizeProgramPublishedRootsWithBuiltin(
        self.allocator,
        .module,
        source,
        import_sources,
        self.prePublishedBuiltin(),
        self.roc_ctx,
    );
}

fn bindingPatternOfName(env: *ModuleEnv, pattern_idx: can.CIR.Pattern.Idx, name: []const u8) ?can.CIR.Pattern.Idx {
    switch (env.store.getPattern(pattern_idx)) {
        .assign => |assign| {
            if (std.mem.eql(u8, env.getIdent(assign.ident), name)) return pattern_idx;
        },
        .as => |as_pattern| {
            if (std.mem.eql(u8, env.getIdent(as_pattern.ident), name)) return pattern_idx;
            return bindingPatternOfName(env, as_pattern.pattern, name);
        },
        .applied_tag => |tag| {
            for (env.store.slicePatterns(tag.args)) |arg| {
                if (bindingPatternOfName(env, arg, name)) |found| return found;
            }
        },
        .nominal => |nominal| return bindingPatternOfName(env, nominal.backing_pattern, name),
        .nominal_external => |nominal| return bindingPatternOfName(env, nominal.backing_pattern, name),
        .record_destructure => |record| {
            for (env.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                const destruct = env.store.getRecordDestruct(destruct_idx);
                if (bindingPatternOfName(env, destruct.kind.toPatternIdx(), name)) |found| return found;
            }
        },
        .list => |list| {
            for (env.store.slicePatterns(list.patterns)) |elem| {
                if (bindingPatternOfName(env, elem, name)) |found| return found;
            }
            if (list.rest_info) |rest| {
                if (rest.pattern) |rest_pattern| {
                    if (bindingPatternOfName(env, rest_pattern, name)) |found| return found;
                }
            }
        },
        .tuple => |tuple| {
            for (env.store.slicePatterns(tuple.patterns)) |elem| {
                if (bindingPatternOfName(env, elem, name)) |found| return found;
            }
        },
        .str_interpolation => |string| {
            var offset: u32 = 0;
            while (offset < string.steps.span.len) : (offset += 1) {
                const string_step = env.store.getStrPatternStep(string.steps, offset);
                if (string_step.capture) |capture| {
                    if (bindingPatternOfName(env, capture, name)) |found| return found;
                }
            }
        },
        .num_literal,
        .num_from_numeral_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => {},
    }
    return null;
}

fn getBindingPatternOfName(env: *ModuleEnv, name: []const u8) ?can.CIR.Pattern.Idx {
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        if (bindingPatternOfName(env, def.pattern, name)) |pattern| return pattern;
    }
    return null;
}

fn getDefOfName(env: *ModuleEnv, name: []const u8) ?can.CIR.Def.Idx {
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern == .assign and std.mem.eql(u8, env.getIdent(pattern.assign.ident), name)) {
            return def_idx;
        }
    }
    return null;
}

const DefinitionValidation = struct {
    valid: bool,
    /// Rendered diagnostic reports when invalid; caller owns and must free.
    error_message: ?[]u8,
};

fn validateDefinitions(self: *ReplSession, report_config: reporting.ReportingConfig) Allocator.Error!DefinitionValidation {
    const definitions = try self.definitionsSource();
    defer self.allocator.free(definitions);

    const source = try std.fmt.allocPrint(self.allocator, "{s}\nmain = \"\"\n", .{definitions});
    defer self.allocator.free(source);

    const import_sources = switch (try self.resolveImports()) {
        .resolved => |s| s,
        .failed => |msg| return .{ .valid = false, .error_message = msg },
    };
    defer self.freeModuleSources(import_sources);

    if (eval.Inspected.parseAndCanonicalizeProgramPublishedRootsWithBuiltin(
        self.allocator,
        .module,
        source,
        import_sources,
        self.prePublishedBuiltin(),
        self.roc_ctx,
    )) |parsed_value| {
        var parsed = parsed_value;
        defer parsed.deinit(self.allocator);
        if (try eval.Inspected.parsedResourcesHaveErrorDiagnostics(self.allocator, &parsed)) {
            const msg = self.renderModuleProblems(source, import_sources, report_config) catch |render_err| switch (render_err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AccessDenied,
                error.AntivirusInterference,
                error.BadPathName,
                error.BitcodeParseError,
                error.BrokenPipe,
                error.Canceled,
                error.CompilationFailed,
                error.ComptimeExhaustiveness,
                error.ConnectionResetByPeer,
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
                error.HostedFunctionNotBound,
                error.InputOutput,
                error.Internal,
                error.InvalidHandle,
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
                error.MissingDynamicLinkingInformation,
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
                error.PageSizeQueryFailed,
                error.ParseError,
                error.PathAlreadyExists,
                error.PermissionDenied,
                error.PipeBusy,
                error.ProcessFdQuotaExceeded,
                error.ReadOnlyFileSystem,
                error.RuntimeError,
                error.ShmOpenFailed,
                error.ShmUnlinkFailed,
                error.SocketUnconnected,
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
                error.TypeCheckError,
                error.Unexpected,
                error.Unseekable,
                error.UnsupportedHostedFunction,
                error.InvalidHostedFunctionSignature,
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
                => return .{ .valid = false, .error_message = null },
            };
            return .{ .valid = false, .error_message = msg };
        }
        return .{ .valid = true, .error_message = null };
    } else |err| switch (err) {
        error.TypeCheckError => {
            const msg = self.renderModuleProblems(source, import_sources, report_config) catch |render_err| switch (render_err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AccessDenied,
                error.AntivirusInterference,
                error.BadPathName,
                error.BitcodeParseError,
                error.BrokenPipe,
                error.Canceled,
                error.CompilationFailed,
                error.ComptimeExhaustiveness,
                error.ConnectionResetByPeer,
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
                error.HostedFunctionNotBound,
                error.InputOutput,
                error.Internal,
                error.InvalidHandle,
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
                error.MissingDynamicLinkingInformation,
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
                error.InvalidHostedFunctionSignature,
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
                error.ParseError,
                error.TypeCheckError,
                => return .{ .valid = false, .error_message = null },
            };
            return .{ .valid = false, .error_message = msg };
        },
        error.ParseError => {
            const msg = self.renderModuleParseDiagnostics(source, report_config) catch |render_err| switch (render_err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.WriteFailed => return .{ .valid = false, .error_message = null },
            };
            return .{ .valid = false, .error_message = msg };
        },
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.BitcodeParseError,
        error.BrokenPipe,
        error.Canceled,
        error.CompilationFailed,
        error.ComptimeExhaustiveness,
        error.ConnectionResetByPeer,
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
        error.HostedFunctionNotBound,
        error.InputOutput,
        error.Internal,
        error.InvalidHandle,
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
        error.MissingDynamicLinkingInformation,
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
        error.InvalidHostedFunctionSignature,
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
        => return .{ .valid = false, .error_message = null },
    }
}

fn evaluateExpression(self: *ReplSession, expr: []const u8, report_config: reporting.ReportingConfig) ReplStepError!StepResult {
    const definitions = try self.definitionsSource();
    defer self.allocator.free(definitions);

    // Keep the expression inside the explicit zero-argument root so `dbg`,
    // failed `expect`, and crash callbacks occur during inspected execution,
    // not while checking finalizes a top-level value.
    const source = try std.fmt.allocPrint(self.allocator, "{s}\nmain = || Str.inspect(({s}))\n", .{ definitions, expr });
    defer self.allocator.free(source);

    const import_sources = switch (try self.resolveImports()) {
        .resolved => |s| s,
        .failed => |msg| return .{ .diagnostic = msg },
    };
    defer self.freeModuleSources(import_sources);

    const target_usize: base.target.TargetUsize = switch (self.backend_kind) {
        .interpreter, .dev, .llvm => .native,
        .wasm => .u32,
    };
    const compile_outcome = eval.Inspected.compileProgramForTargetWithBuiltinAndContextReporting(
        self.allocator,
        self.roc_ctx.std_io,
        .module,
        source,
        import_sources,
        target_usize,
        self.prePublishedBuiltin(),
        self.roc_ctx,
        self.specialization_strategy,
    ) catch |err| switch (err) {
        error.TypeCheckError => return .{ .diagnostic = try self.renderModuleProblems(source, import_sources, report_config) },
        error.ParseError => return .{ .diagnostic = try self.renderModuleParseDiagnostics(source, report_config) },
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.BitcodeParseError,
        error.BrokenPipe,
        error.Canceled,
        error.CompilationFailed,
        error.ComptimeExhaustiveness,
        error.ConnectionResetByPeer,
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
        error.HostedFunctionNotBound,
        error.InputOutput,
        error.Internal,
        error.InvalidHandle,
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
        error.MissingDynamicLinkingInformation,
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
        error.InvalidHostedFunctionSignature,
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
    var compiled = switch (compile_outcome) {
        .compiled => |compiled| compiled,
        .diagnostics => |resources_value| {
            var resources = resources_value;
            defer resources.deinit(self.allocator);
            return .{ .diagnostic = try eval.Inspected.renderParsedResourcesProblemsWithConfig(
                self.allocator,
                &resources,
                report_config,
            ) };
        },
    };
    defer compiled.deinit(self.allocator);

    // Checked publication deliberately succeeds in the presence of user
    // errors so build/run/test can execute independent roots. A REPL
    // expression is a single interactive transaction: report its errors and
    // leave the session definitions intact instead of executing the explicit
    // runtime-error node and aborting the remaining batch input. Warnings
    // (e.g. an unused loop binder) never block evaluation.
    if (try eval.Inspected.parsedResourcesHaveErrorDiagnostics(self.allocator, &compiled.resources)) {
        return .{ .diagnostic = try eval.Inspected.renderParsedResourcesProblemsWithConfig(
            self.allocator,
            &compiled.resources,
            report_config,
        ) };
    }

    const lowered = &compiled.lowered;
    const program: eval.InspectedRun.Program = .{
        .store = &lowered.view.store,
        .layouts = &lowered.view.layouts,
        .boxy_tables = eval.boxy_runtime.BoxyTables.fromImageView(&lowered.view),
        .boxy_sidecar_blob = lowered.shm.base_ptr[0..lowered.shm.getUsedSize()],
        .boxy_sidecar_desc = lir.LirImage.BoxySidecar.fromHeader(lowered.image_header),
        .main_proc = lowered.mainProc(),
    };
    const result = (switch (self.backend_kind) {
        .interpreter => eval.InspectedRun.run(
            self.allocator,
            .interpreter,
            program,
            if (self.import_policy == .virtual_only) eval.InspectedRun.replEffectHost() else .reject,
        ),
        .dev => eval.InspectedRun.run(self.allocator, .dev, program, {}),
        .wasm => eval.InspectedRun.run(self.allocator, .wasm, program, {}),
        .llvm => eval.InspectedRun.run(self.allocator, .llvm, program, {}),
    }) catch |err| switch (err) {
        error.UnsupportedHostedFunction => return .{ .diagnostic = try self.allocator.dupe(
            u8,
            "This REPL only supports the hosted function Repl.emit!.",
        ) },
        error.InvalidHostedFunctionSignature => return .{ .diagnostic = try self.allocator.dupe(
            u8,
            "Repl.emit! has an invalid runtime signature.",
        ) },
        else => return err,
    };
    self.last_events = result.events;
    return switch (result.outcome) {
        .returned => |output| .{ .output = output },
        .crashed => |message| .{ .runtime_crash = message },
    };
}

fn renderModuleProblems(self: *ReplSession, source: []const u8, imports: []const ModuleSource, report_config: reporting.ReportingConfig) ModuleRenderError![]u8 {
    return eval.Inspected.renderProblemsWithConfigAndImports(self.allocator, .module, source, imports, report_config, self.roc_ctx) catch |err| switch (err) {
        error.ParseError => self.renderModuleParseDiagnostics(source, report_config),
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.BitcodeParseError,
        error.BrokenPipe,
        error.Canceled,
        error.CompilationFailed,
        error.ComptimeExhaustiveness,
        error.ConnectionResetByPeer,
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
        error.HostedFunctionNotBound,
        error.InputOutput,
        error.Internal,
        error.InvalidHandle,
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
        error.MissingDynamicLinkingInformation,
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
        error.TypeCheckError,
        error.Unexpected,
        error.Unseekable,
        error.UnsupportedHostedFunction,
        error.InvalidHostedFunctionSignature,
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
        => err,
    };
}

fn renderModuleParseDiagnostics(self: *ReplSession, source: []const u8, report_config: reporting.ReportingConfig) (Allocator.Error || error{WriteFailed})![]u8 {
    var env = try ModuleEnv.init(self.allocator, source);
    defer env.deinit();
    env.common.source = source;
    try env.common.calcLineStarts(self.allocator);

    const ast = try parse.file(self.allocator, &env.common);
    defer ast.deinit();

    return self.renderAstDiagnostics(ast, &env.common, "repl", report_config);
}

fn renderStatementParseDiagnostics(self: *ReplSession, source: []const u8, report_config: reporting.ReportingConfig) (Allocator.Error || error{WriteFailed})![]u8 {
    var env = try ModuleEnv.init(self.allocator, source);
    defer env.deinit();
    env.common.source = source;
    try env.common.calcLineStarts(self.allocator);

    const ast = if (lineStartsWithImportKeyword(source))
        try parse.statementTopLevel(self.allocator, &env.common)
    else
        try parse.statement(self.allocator, &env.common);
    defer ast.deinit();

    return self.renderAstDiagnostics(ast, &env.common, "repl", report_config);
}

fn renderAstDiagnostics(
    self: *ReplSession,
    ast: *parse.AST,
    env: *const base.CommonEnv,
    filename: []const u8,
    report_config: reporting.ReportingConfig,
) (Allocator.Error || error{WriteFailed})![]u8 {
    var out: std.Io.Writer.Allocating = .init(self.allocator);
    errdefer out.deinit();

    var rendered_any = false;
    for (ast.tokenize_diagnostics.items) |diagnostic| {
        var report = try ast.tokenizeDiagnosticToReport(diagnostic, self.allocator, filename);
        defer report.deinit();
        try reporting.renderReportWithConfig(&report, &out.writer, report_config);
        rendered_any = true;
    }
    for (ast.parse_diagnostics.items) |diagnostic| {
        var report = try ast.parseDiagnosticToReport(env, diagnostic, self.allocator, filename);
        defer report.deinit();
        try reporting.renderReportWithConfig(&report, &out.writer, report_config);
        rendered_any = true;
    }

    if (!rendered_any) {
        out.deinit();
        return self.renderFallbackParseDiagnostic(env.source, report_config);
    }

    const raw = try out.toOwnedSlice();
    return trimOwnedRight(self.allocator, raw);
}

fn renderFallbackParseDiagnostic(self: *ReplSession, source: []const u8, report_config: reporting.ReportingConfig) (Allocator.Error || error{WriteFailed})![]u8 {
    var report = try reporting.Report.init(self.allocator, "Parse Error", "The REPL input could not be parsed.", .runtime_error);
    defer report.deinit();
    if (source.len > 0) {
        try report.document.addLineBreak();
        try report.document.addCodeBlock(source);
    }

    var out: std.Io.Writer.Allocating = .init(self.allocator);
    errdefer out.deinit();
    try reporting.renderReportWithConfig(&report, &out.writer, report_config);
    const raw = try out.toOwnedSlice();
    return trimOwnedRight(self.allocator, raw);
}

fn trimOwnedRight(allocator: Allocator, raw: []u8) Allocator.Error![]u8 {
    const trimmed = std.mem.trimEnd(u8, raw, "\r\n");
    if (trimmed.len == raw.len) return raw;
    const result = try allocator.dupe(u8, trimmed);
    allocator.free(raw);
    return result;
}

/// Whether a REPL line binds a name or is evaluated for its value.
pub const InputKind = enum {
    definition,
    expression,
};

/// Distinguishes declarations that can share a name in the REPL definition store.
pub const DefinitionKind = enum {
    value,
    annotation,
    type_decl,
    import,
};

/// What the session made of an input line, used to label transcript entries.
pub const InputInfo = struct {
    kind: InputKind,
    definition_kind: DefinitionKind = .value,
    name: ?[]const u8 = null,
    file_import: bool = false,
};

/// Whether a REPL input line forms a complete, parseable statement.
pub const InputStatus = union(enum) {
    complete: InputInfo,
    incomplete,
    invalid,
};

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

/// Whether a REPL line's first token is the `import` keyword. Such lines are
/// parsed at the top level so `import` statements are accepted; everything else
/// is parsed as an in-body statement (expressions, declarations, etc.).
fn lineStartsWithImportKeyword(line: []const u8) bool {
    const trimmed = std.mem.trimStart(u8, line, " \t");
    const keyword = "import";
    if (!std.mem.startsWith(u8, trimmed, keyword)) return false;
    // Reject identifiers that merely begin with "import" (e.g. `imports`).
    if (trimmed.len == keyword.len) return true;
    return !isIdentChar(trimmed[keyword.len]);
}

/// Parses a line to determine whether it is a complete, incomplete, or invalid REPL input.
pub fn inputStatus(self: *ReplSession, line: []const u8) Allocator.Error!InputStatus {
    return inputStatusWithAllocator(self.allocator, line);
}

/// Parses a line to determine whether it is a complete, incomplete, or invalid REPL input.
pub fn inputStatusWithAllocator(allocator: Allocator, line: []const u8) Allocator.Error!InputStatus {
    var env = try ModuleEnv.init(allocator, line);
    defer env.deinit();
    env.common.source = line;
    try env.common.calcLineStarts(allocator);

    const ast = if (lineStartsWithImportKeyword(line))
        try parse.statementTopLevel(allocator, &env.common)
    else
        try parse.statement(allocator, &env.common);
    defer ast.deinit();
    if (ast.tokenize_diagnostics.items.len > 0 or ast.parse_diagnostics.items.len > 0) {
        return if (inputDiagnosticsAreIncomplete(ast)) .incomplete else .invalid;
    }

    const statement = ast.store.getStatement(@enumFromInt(ast.root_node_idx));
    return .{
        .complete = switch (statement) {
            .expr,
            .crash,
            .dbg,
            .expect,
            .@"for",
            .@"while",
            .@"return",
            .@"break",
            => .{ .kind = .expression },
            .decl => |decl| .{
                .kind = .definition,
                .definition_kind = .value,
                .name = declarationName(ast, decl.pattern),
            },
            .@"var" => |v| .{
                .kind = .definition,
                .definition_kind = .value,
                .name = ast.resolve(v.name),
            },
            .type_anno => |anno| .{
                .kind = .definition,
                .definition_kind = .annotation,
                .name = ast.resolve(anno.name),
            },
            .type_decl => |decl| blk: {
                const header = ast.store.getTypeHeader(decl.header) catch break :blk .{
                    .kind = .definition,
                    .definition_kind = .type_decl,
                    .name = null,
                };
                break :blk .{
                    .kind = .definition,
                    .definition_kind = .type_decl,
                    .name = ast.resolve(header.name),
                };
            },
            .import => |import| .{
                .kind = .definition,
                .definition_kind = .import,
                .name = if (import.alias_tok) |tok|
                    ast.resolve(tok)
                else if (import.target.nested_start_tok) |nested_start|
                    ast.resolve(nested_start + import.target.nested_len - 1)
                else
                    ast.resolve(import.target.module_name_tok),
            },
            .file_import => |file_import| .{
                .kind = .definition,
                .definition_kind = .import,
                .name = ast.resolve(file_import.name_tok),
                .file_import = true,
            },
            .malformed => return .invalid,
        },
    };
}

fn inputDiagnosticsAreIncomplete(ast: *const parse.AST) bool {
    var saw_incomplete = false;

    for (ast.tokenize_diagnostics.items) |diagnostic| {
        if (!tokenizeDiagnosticIsIncomplete(diagnostic, ast.env.source.len)) return false;
        saw_incomplete = true;
    }

    for (ast.parse_diagnostics.items) |diagnostic| {
        if (!parseDiagnosticIsIncompleteAtEof(ast, diagnostic)) return false;
        saw_incomplete = true;
    }

    return saw_incomplete;
}

fn tokenizeDiagnosticIsIncomplete(diagnostic: parse.tokenize.Diagnostic, source_len: usize) bool {
    const reaches_eof = diagnostic.region.end.offset >= source_len;
    return reaches_eof and (diagnostic.tag == .UnclosedString or
        diagnostic.tag == .SingleQuoteUnclosed or
        diagnostic.tag == .InvalidUnicodeEscapeSequence);
}

fn parseDiagnosticIsIncompleteAtEof(ast: *const parse.AST, diagnostic: parse.AST.Diagnostic) bool {
    if (!diagnosticRegionTouchesEof(ast, diagnostic.region)) return false;

    return diagnostic.tag == .pattern_unexpected_eof or
        diagnostic.tag == .string_unclosed or
        diagnostic.tag == .string_expected_close_interpolation or
        diagnostic.tag == .incomplete_import or
        diagnostic.tag == .expected_expr_bar or
        diagnostic.tag == .expected_expr_close_curly or
        diagnostic.tag == .expected_expr_close_curly_or_comma or
        diagnostic.tag == .expected_expr_close_round_or_comma or
        diagnostic.tag == .expected_expr_close_square_or_comma or
        diagnostic.tag == .expected_close_curly_at_end_of_match or
        diagnostic.tag == .expected_open_curly_after_match or
        diagnostic.tag == .expected_expr_apply_close_round or
        diagnostic.tag == .expected_ty_apply_close_round or
        diagnostic.tag == .expected_ty_anno_close_round or
        diagnostic.tag == .expected_ty_anno_close_round_or_comma or
        diagnostic.tag == .expected_ty_close_curly_or_comma or
        diagnostic.tag == .expected_ty_close_square_or_comma or
        diagnostic.tag == .expected_expr_comma or
        diagnostic.tag == .expected_arrow or
        diagnostic.tag == .expr_unexpected_token or
        diagnostic.tag == .statement_unexpected_token or
        diagnostic.tag == .ty_anno_unexpected_token or
        diagnostic.tag == .var_expected_equals or
        diagnostic.tag == .for_expected_in or
        diagnostic.tag == .match_branch_missing_arrow or
        diagnostic.tag == .where_expected_close_bracket;
}

fn diagnosticRegionTouchesEof(ast: *const parse.AST, region: parse.AST.TokenizedRegion) bool {
    const token_count = ast.tokens.tokens.len;
    if (token_count == 0) return true;

    const eof_idx: u32 = @intCast(token_count - 1);
    if (region.start >= eof_idx or region.end > eof_idx) return true;

    const tags = ast.tokens.tokens.items(.tag);
    return tags[@intCast(region.start)] == .EndOfFile;
}

fn declarationName(ast: *const parse.AST, pattern_idx: parse.AST.Pattern.Idx) ?[]const u8 {
    const pattern = ast.store.getPattern(pattern_idx);
    if (pattern == .ident) return ast.resolve(pattern.ident.ident_tok);
    if (pattern == .var_ident) return ast.resolve(pattern.var_ident.ident_tok);
    if (pattern == .as) return ast.resolve(pattern.as.name);
    return null;
}

fn appendBoundName(names: *std.ArrayList([]const u8), allocator: Allocator, name: []const u8) Allocator.Error!void {
    for (names.items) |existing| {
        if (std.mem.eql(u8, existing, name)) return;
    }
    try names.append(allocator, name);
}

fn collectPatternBoundNames(
    ast: *const parse.AST,
    pattern_idx: parse.AST.Pattern.Idx,
    names: *std.ArrayList([]const u8),
    allocator: Allocator,
) Allocator.Error!void {
    switch (ast.store.getPattern(pattern_idx)) {
        .ident => |ident| try appendBoundName(names, allocator, ast.resolve(ident.ident_tok)),
        .var_ident => |ident| try appendBoundName(names, allocator, ast.resolve(ident.ident_tok)),
        .tag => |tag| {
            for (ast.store.patternSlice(tag.args)) |arg| {
                try collectPatternBoundNames(ast, arg, names, allocator);
            }
        },
        .record => |record| {
            for (ast.store.patternRecordFieldSlice(record.fields)) |field_idx| {
                const field = ast.store.getPatternRecordField(field_idx);
                if (field.value) |value| {
                    try collectPatternBoundNames(ast, value, names, allocator);
                } else if (field.name) |name| {
                    try appendBoundName(names, allocator, ast.resolve(name));
                }
            }
        },
        .list => |list| {
            for (ast.store.patternSlice(list.patterns)) |elem| {
                try collectPatternBoundNames(ast, elem, names, allocator);
            }
        },
        .list_rest => |rest| {
            if (rest.name) |name| try appendBoundName(names, allocator, ast.resolve(name));
        },
        .tuple => |tuple| {
            for (ast.store.patternSlice(tuple.patterns)) |elem| {
                try collectPatternBoundNames(ast, elem, names, allocator);
            }
        },
        .string => |string| {
            for (ast.store.patternStringPartSlice(string.parts)) |part_idx| {
                switch (ast.store.getPatternStringPart(part_idx)) {
                    .text => {},
                    .capture => |capture| {
                        if (capture.name) |name| try appendBoundName(names, allocator, ast.resolve(name));
                    },
                }
            }
        },
        .alternatives => |alternatives| {
            for (ast.store.patternSlice(alternatives.patterns)) |alternative| {
                try collectPatternBoundNames(ast, alternative, names, allocator);
            }
        },
        .as => |as_pattern| {
            try collectPatternBoundNames(ast, as_pattern.pattern, names, allocator);
            try appendBoundName(names, allocator, ast.resolve(as_pattern.name));
        },
        .int,
        .frac,
        .typed_int,
        .typed_frac,
        .single_quote,
        .underscore,
        .malformed,
        => {},
    }
}

fn declarationBoundNames(allocator: Allocator, line: []const u8) Allocator.Error![][]const u8 {
    var env = try ModuleEnv.init(allocator, line);
    defer env.deinit();
    env.common.source = line;
    try env.common.calcLineStarts(allocator);

    const ast = try parse.statement(allocator, &env.common);
    defer ast.deinit();

    var names = std.ArrayList([]const u8).empty;
    errdefer names.deinit(allocator);
    const statement = ast.store.getStatement(@enumFromInt(ast.root_node_idx));
    if (statement == .decl) {
        try collectPatternBoundNames(ast, statement.decl.pattern, &names, allocator);
    } else if (statement == .@"var") {
        try appendBoundName(&names, allocator, ast.resolve(statement.@"var".name));
    }
    return names.toOwnedSlice(allocator);
}

fn formatDefinitionResult(allocator: Allocator, verb: []const u8, names: []const []const u8) Allocator.Error![]u8 {
    if (names.len == 0) return std.fmt.allocPrint(allocator, "{s} pattern", .{verb});

    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(allocator);
    try out.appendSlice(allocator, verb);
    try out.append(allocator, ' ');
    for (names, 0..) |name, index| {
        if (index > 0) try out.appendSlice(allocator, ", ");
        try out.append(allocator, '`');
        try out.appendSlice(allocator, name);
        try out.append(allocator, '`');
    }
    return out.toOwnedSlice(allocator);
}

const Definition = struct {
    names: [][]u8,
    source: []u8,
    kind: DefinitionKind,
    file_import: bool,

    fn initOwned(
        allocator: Allocator,
        source: []const u8,
        names: []const []const u8,
        kind: DefinitionKind,
        file_import: bool,
    ) Allocator.Error!Definition {
        const owned_names = try allocator.alloc([]u8, names.len);
        var initialized_names: usize = 0;
        errdefer {
            for (owned_names[0..initialized_names]) |owned_name| allocator.free(owned_name);
            allocator.free(owned_names);
        }
        for (names, 0..) |name, index| {
            owned_names[index] = try allocator.dupe(u8, name);
            initialized_names += 1;
        }
        const owned_source = try allocator.dupe(u8, source);
        return .{
            .names = owned_names,
            .source = owned_source,
            .kind = kind,
            .file_import = file_import,
        };
    }

    fn clone(self: *const Definition, allocator: Allocator) Allocator.Error!Definition {
        return initOwned(allocator, self.source, self.names, self.kind, self.file_import);
    }

    fn deinit(self: *Definition, allocator: Allocator) void {
        for (self.names) |name| allocator.free(name);
        allocator.free(self.names);
        allocator.free(self.source);
        self.* = undefined;
    }

    fn bindsName(self: *const Definition, name: []const u8) bool {
        for (self.names) |bound_name| {
            if (std.mem.eql(u8, bound_name, name)) return true;
        }
        return false;
    }

    fn overlapsNames(self: *const Definition, names: []const []const u8) bool {
        for (names) |name| {
            if (self.bindsName(name)) return true;
        }
        return false;
    }
};

/// Ordered REPL definition collection with overlapping-binder replacement by
/// definition kind.
pub const DefinitionStore = struct {
    items: std.ArrayList(Definition),

    fn init() DefinitionStore {
        return .{ .items = .empty };
    }

    fn deinit(self: *DefinitionStore, allocator: Allocator) void {
        for (self.items.items) |*definition| definition.deinit(allocator);
        self.items.deinit(allocator);
        self.* = DefinitionStore.init();
    }

    pub fn count(self: *const DefinitionStore) usize {
        return self.items.items.len;
    }

    pub fn hasKind(self: *const DefinitionStore, name: []const u8, kind: DefinitionKind) bool {
        for (self.items.items) |definition| {
            if (definition.kind == kind and definition.bindsName(name)) return true;
        }
        return false;
    }

    pub fn removeByNameAndKind(self: *DefinitionStore, allocator: Allocator, name: []const u8, kind: DefinitionKind) void {
        var i: usize = 0;
        while (i < self.items.items.len) {
            const definition = &self.items.items[i];
            if (definition.kind == kind and definition.bindsName(name)) {
                var removed = self.items.orderedRemove(i);
                removed.deinit(allocator);
                return;
            }
            i += 1;
        }
    }

    fn addOrReplace(
        self: *DefinitionStore,
        allocator: Allocator,
        source: []const u8,
        name: []const u8,
        kind: DefinitionKind,
        file_import: bool,
    ) Allocator.Error!void {
        const names = [_][]const u8{name};
        return self.addOrReplaceNames(allocator, source, &names, kind, file_import);
    }

    fn addOrReplaceNames(
        self: *DefinitionStore,
        allocator: Allocator,
        source: []const u8,
        names: []const []const u8,
        kind: DefinitionKind,
        file_import: bool,
    ) Allocator.Error!void {
        var replacement = try Definition.initOwned(allocator, source, names, kind, file_import);
        errdefer replacement.deinit(allocator);
        try self.items.ensureUnusedCapacity(allocator, 1);

        var insertion_index = self.items.items.len;
        var index: usize = 0;
        while (index < self.items.items.len) {
            const definition = &self.items.items[index];
            if (definition.kind == kind and definition.overlapsNames(names)) {
                insertion_index = @min(insertion_index, index);
                var removed = self.items.orderedRemove(index);
                removed.deinit(allocator);
            } else {
                index += 1;
            }
        }

        self.items.insertAssumeCapacity(insertion_index, replacement);
    }

    fn snapshot(self: *const DefinitionStore, allocator: Allocator) Allocator.Error!DefinitionStore {
        var result = DefinitionStore.init();
        errdefer result.deinit(allocator);
        try result.items.ensureTotalCapacity(allocator, self.items.items.len);
        for (self.items.items) |definition| {
            result.items.appendAssumeCapacity(try definition.clone(allocator));
        }
        return result;
    }

    fn restore(self: *DefinitionStore, allocator: Allocator, snapshot_store: *DefinitionStore) void {
        self.deinit(allocator);
        self.* = snapshot_store.*;
        snapshot_store.* = DefinitionStore.init();
    }
};

const testing = std.testing;

/// One Builtin module, published once and shared (read-only) by every test
/// session. Publishing the Builtin via `BuiltinModules.init` is the dominant
/// per-session cost, so reusing a single instance across the ~100 sessions in
/// this file is the largest win. Allocated with `page_allocator` (not
/// `testing.allocator`) so the never-freed singleton isn't flagged as a leak.
/// The cli_test runner is single-threaded, so lazy init needs no locking.
var shared_test_builtins: ?eval.BuiltinModules = null;

fn sharedTestBuiltins() ReplInitError!*eval.BuiltinModules {
    if (shared_test_builtins == null) {
        shared_test_builtins = try eval.BuiltinModules.init(std.heap.page_allocator);
    }
    return &shared_test_builtins.?;
}

/// Build a test session that borrows the shared Builtin (see
/// `shared_test_builtins`) instead of publishing its own.
fn testRepl(backend_kind: eval.EvalBackend) ReplInitError!ReplSession {
    return ReplSession.initBorrowingBuiltins(
        testing.allocator,
        testCoreCtx(),
        backend_kind,
        try sharedTestBuiltins(),
    );
}

const TestBackend = enum { interpreter, dev, wasm };

fn toEvalBackend(backend: TestBackend) eval.EvalBackend {
    return switch (backend) {
        .interpreter => .interpreter,
        .dev => .dev,
        .wasm => .wasm,
    };
}

fn backendName(backend: TestBackend) []const u8 {
    return switch (backend) {
        .interpreter => "INTERPRETER",
        .dev => "DEV BACKEND",
        .wasm => "WASM BACKEND",
    };
}

fn expectBackend(backend: TestBackend, expr: []const u8, expected: []const u8) ReplTestError!void {
    const eval_backend = toEvalBackend(backend);
    if (!eval.backendAvailable(eval_backend)) return;

    var repl = try testRepl(eval_backend);
    defer repl.deinit();

    const result = try repl.step(expr);
    defer testing.allocator.free(result);
    testing.expectEqualStrings(expected, result) catch |err| {
        std.debug.print("{s} FAILED for: {s}\n", .{ backendName(backend), expr });
        return err;
    };
}

fn expectInterpreter(expr: []const u8, expected: []const u8) ReplTestError!void {
    try expectBackend(.interpreter, expr, expected);
}

/// Build the wrapped module source (`<defs>\nmain = <expr>`) for `expr` and
/// confirm it is an expression. Caller owns the returned source.
fn replExprSource(repl: *ReplSession, expr: []const u8) ReplTestError![]u8 {
    const line = std.mem.trim(u8, expr, " \t\r\n");
    const input_info = switch (try repl.inputStatus(line)) {
        .complete => |info| info,
        .incomplete, .invalid => return error.ParseError,
    };
    try testing.expectEqual(InputKind.expression, input_info.kind);

    const definitions = try repl.definitionsSource();
    defer testing.allocator.free(definitions);

    return std.fmt.allocPrint(testing.allocator, "{s}\nmain = {s}\n", .{ definitions, line });
}

/// Evaluate `expr` on the two native backends (interpreter and dev) and assert
/// both render `expected`. Only the native target is lowered—wasm coverage is
/// exercised explicitly by `expectAllBackends` on a representative subset, so it
/// is not re-run for every native assertion.
fn expectAllNative(expr: []const u8, expected: []const u8) ReplTestError!void {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const source = try replExprSource(&repl, expr);
    defer testing.allocator.free(source);

    var compiled = try eval.Inspected.compileInspectedProgramForTargetWithBuiltin(
        testing.allocator,
        repl.roc_ctx.std_io,
        .module,
        source,
        &.{},
        .native,
        repl.prePublishedBuiltin(),
        repl.roc_ctx,
        repl.specialization_strategy,
    );
    defer compiled.deinit(testing.allocator);

    try expectCompiledBackend(.interpreter, expr, expected, &compiled.lowered);
    try expectCompiledBackend(.dev, expr, expected, &compiled.lowered);
}

/// Evaluate `expr` on all backends—interpreter, dev, and wasm. Lowers both the
/// native and wasm targets, so reserve this for a representative subset rather
/// than every assertion.
fn expectAllBackends(expr: []const u8, expected: []const u8) ReplTestError!void {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const source = try replExprSource(&repl, expr);
    defer testing.allocator.free(source);

    var compiled = try eval.Inspected.compileInspectedProgramWithBuiltin(
        testing.allocator,
        repl.roc_ctx.std_io,
        .module,
        source,
        &.{},
        repl.prePublishedBuiltin(),
        repl.roc_ctx,
    );
    defer compiled.deinit(testing.allocator);

    try expectCompiledBackend(.interpreter, expr, expected, &compiled.lowered);
    try expectCompiledBackend(.dev, expr, expected, &compiled.lowered);
    try expectCompiledBackend(.wasm, expr, expected, &compiled.wasm_lowered);
}

fn expectCompiledBackend(
    backend: TestBackend,
    expr: []const u8,
    expected: []const u8,
    lowered: *eval.Inspected.LoweredProgram,
) ReplTestError!void {
    const eval_backend = toEvalBackend(backend);
    if (!eval.backendAvailable(eval_backend)) return;

    const result = switch (backend) {
        .interpreter => try eval.Inspected.lirInterpreterInspectedStr(testing.allocator, lowered),
        .dev => try eval.Inspected.devEvaluatorInspectedStr(testing.allocator, lowered),
        .wasm => try eval.Inspected.wasmEvaluatorInspectedStr(testing.allocator, lowered),
    };
    defer testing.allocator.free(result);

    testing.expectEqualStrings(expected, result) catch |err| {
        std.debug.print("{s} FAILED for: {s}\n", .{ backendName(backend), expr });
        return err;
    };
}

fn expectStateful(backend: TestBackend, steps: []const [2][]const u8) ReplTestError!void {
    const eval_backend = toEvalBackend(backend);
    if (!eval.backendAvailable(eval_backend)) return;

    var repl = try testRepl(eval_backend);
    defer repl.deinit();

    for (steps) |step_pair| {
        const result = try repl.step(step_pair[0]);
        defer testing.allocator.free(result);
        testing.expectEqualStrings(step_pair[1], result) catch |err| {
            std.debug.print("{s} FAILED for: {s}\n", .{ backendName(backend), step_pair[0] });
            return err;
        };
    }
}

fn expectStepsFinal(backend: TestBackend, steps: []const []const u8, expected: []const u8) ReplTestError!void {
    const eval_backend = toEvalBackend(backend);
    if (!eval.backendAvailable(eval_backend)) return;

    var repl = try testRepl(eval_backend);
    defer repl.deinit();

    for (steps, 0..) |step_input, i| {
        const result = try repl.step(step_input);
        defer testing.allocator.free(result);

        if (i + 1 == steps.len) {
            testing.expectEqualStrings(expected, result) catch |err| {
                std.debug.print("{s} FAILED for: {s}\n", .{ backendName(backend), step_input });
                return err;
            };
        }
    }
}

test "Repl - initialization and cleanup" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();
    try testing.expect(repl.definitions.count() == 0);
}

/// Real OS-backed `CoreCtx` for REPL tests, so file-import tests can read
/// fixture files. Defined below the first `test` block on purpose: the tidy
/// check that bans `CoreCtx.default(` outside entrypoints only scans a file up
/// to its first `test "` declaration, and this is legitimate test-only setup.
fn testCoreCtx() CoreCtx {
    return CoreCtx.default(testing.allocator, testing.allocator, std.testing.io);
}

test "Repl - special commands" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const help_result = try repl.executeCommandWithConfig(.help, reporting.ReportingConfig.initForTesting());
    defer help_result.deinit(testing.allocator);
    switch (help_result) {
        .output => |output| try testing.expect(std.mem.find(u8, output, "Enter an expression") != null),
        .diagnostic, .runtime_crash, .none, .exit => return error.TestUnexpectedResult,
    }

    const exit_result = try repl.executeCommandWithConfig(.exit, reporting.ReportingConfig.initForTesting());
    defer exit_result.deinit(testing.allocator);
    try testing.expect(exit_result == .exit);

    const empty_result = try repl.step("");
    defer testing.allocator.free(empty_result);
    try testing.expectEqualStrings("", empty_result);
}

test "Repl - language stepping returns structured definition metadata" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const result = try repl.stepLanguageWithConfig("answer = 42", reporting.ReportingConfig.initForTesting());
    defer result.deinit(testing.allocator);
    switch (result) {
        .definition => |definition| {
            try testing.expectEqualStrings("answer", definition.name);
            try testing.expectEqual(DefinitionKind.value, definition.kind);
        },
        .expression, .diagnostic, .runtime_crash, .none => return error.TestUnexpectedResult,
    }
}

test "Repl - virtual session records ordered one-way effects" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();
    repl.import_policy = .virtual_only;
    const config = reporting.ReportingConfig.initColorTerminal();

    const imported = try repl.stepLanguageWithConfig("import Repl", config);
    defer imported.deinit(testing.allocator);
    switch (imported) {
        .definition => |definition| try testing.expectEqual(DefinitionKind.import, definition.kind),
        .diagnostic => |diagnostic| {
            std.debug.print("Repl import failed:\n{s}\n", .{diagnostic.message});
            return error.TestUnexpectedResult;
        },
        .expression, .runtime_crash, .none => return error.TestUnexpectedResult,
    }

    const inspected = try repl.inspectExpressionType(
        "Repl.emit!({ name: \"log\", payload: \"héllo\" })",
        config,
    );
    defer inspected.deinit(testing.allocator);
    switch (inspected) {
        .output => |type_name| try testing.expectEqualStrings("{}", type_name),
        .diagnostic => |diagnostic| {
            std.debug.print("Repl emit inspection failed:\n{s}\n", .{diagnostic});
            return error.TestUnexpectedResult;
        },
        .runtime_crash, .none, .exit => return error.TestUnexpectedResult,
    }

    const emitted = try repl.stepLanguageWithConfig(
        "Repl.emit!({ name: \"log\", payload: Str.concat(\"a long runtime-allocated \", \"effect payload\") })",
        config,
    );
    defer emitted.deinit(testing.allocator);
    switch (emitted) {
        .expression => {},
        .diagnostic => |diagnostic| {
            std.debug.print("Repl emit failed:\n{s}\n", .{diagnostic.message});
            return error.TestUnexpectedResult;
        },
        .definition, .runtime_crash, .none => return error.TestUnexpectedResult,
    }

    const events = repl.takeEvents();
    defer {
        for (events) |*event| event.deinit(testing.allocator);
        testing.allocator.free(events);
    }
    try testing.expectEqual(@as(usize, 1), events.len);
    switch (events[0]) {
        .effect => |effect| {
            try testing.expectEqualStrings("log", effect.name);
            try testing.expectEqualStrings("a long runtime-allocated effect payload", effect.payload);
        },
        .dbg, .expect_failed, .crashed => return error.TestUnexpectedResult,
    }
}

test "Repl - failed annotated value restores the exact pending annotation state" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();
    const config = reporting.ReportingConfig.initForTesting();

    const annotation = try repl.stepLanguageWithConfig("pending : Str", config);
    defer annotation.deinit(testing.allocator);
    switch (annotation) {
        .definition => |definition| try testing.expectEqual(DefinitionKind.annotation, definition.kind),
        .expression, .diagnostic, .runtime_crash, .none => return error.TestUnexpectedResult,
    }

    const failed_value = try repl.stepLanguageWithConfig("pending = 42", config);
    defer failed_value.deinit(testing.allocator);
    switch (failed_value) {
        .diagnostic => |diagnostic| try testing.expectEqual(LanguageDiagnosticKind.compile_error, diagnostic.kind),
        .expression, .definition, .runtime_crash, .none => return error.TestUnexpectedResult,
    }

    const stored = try repl.storedDefinitions();
    defer repl.freeStoredDefinitions(stored);
    try testing.expectEqual(@as(usize, 1), stored.len);
    try testing.expectEqualStrings("pending : Str", stored[0].source);
    try testing.expectEqual(DefinitionKind.annotation, stored[0].kind);
}

test "Repl - import keyword routing" {
    try testing.expect(lineStartsWithImportKeyword("import Util"));
    try testing.expect(lineStartsWithImportKeyword("  import Foo.Bar"));
    try testing.expect(lineStartsWithImportKeyword("import"));
    try testing.expect(!lineStartsWithImportKeyword("imports"));
    try testing.expect(!lineStartsWithImportKeyword("importance = 5"));
    try testing.expect(!lineStartsWithImportKeyword("1 + 1"));
    try testing.expect(!lineStartsWithImportKeyword("x = import"));
}

test "Repl - import is classified as an import definition" {
    const status = try inputStatusWithAllocator(testing.allocator, "import Util");
    switch (status) {
        .complete => |info| {
            try testing.expect(info.kind == .definition);
            try testing.expect(info.definition_kind == .import);
        },
        .incomplete, .invalid => return error.TestUnexpectedResult,
    }
}

test "Repl - missing import reports a graceful diagnostic" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const result = try repl.step("import ReplModuleThatDefinitelyDoesNotExist");
    defer testing.allocator.free(result);

    try testing.expect(std.mem.find(u8, result, "couldn't find") != null);
    try testing.expect(std.mem.find(u8, result, "ReplModuleThatDefinitelyDoesNotExist") != null);
}

test "Repl - resolves a sibling module from disk and calls into it" {
    if (!eval.backendAvailable(.interpreter)) return;

    var repl = try testRepl(.interpreter);
    defer repl.deinit();
    // Resolve imports against the checked-in fixture package (cwd is the repo
    // root during `zig build run-test-zig`).
    repl.module_root = "test/complex_package";

    {
        const result = try repl.step("import Util");
        defer testing.allocator.free(result);
        try testing.expectEqualStrings("imported `Util`", result);
    }
    {
        const result = try repl.step("Util.trim_all([\"  hi \", \" there \"])");
        defer testing.allocator.free(result);
        try testing.expectEqualStrings("[\"hi\", \"there\"]", result);
    }
}

test "Repl - imports a file as Str and evaluates its contents" {
    if (!eval.backendAvailable(.interpreter)) return;

    var repl = try testRepl(.interpreter);
    defer repl.deinit();
    // File imports resolve against cwd, which is the repo root during the Zig
    // test run; the fixture holds the bytes "hello world".
    {
        const result = try repl.step("import \"test/snapshots/eval/file_import_test_data.txt\" as data : Str");
        defer testing.allocator.free(result);
        try testing.expectEqualStrings("imported `data`", result);
    }
    {
        const result = try repl.step("data");
        defer testing.allocator.free(result);
        try testing.expectEqualStrings("\"hello world\"", result);
    }
}

test "Repl - imports a file as List(U8) and reads its bytes" {
    if (!eval.backendAvailable(.interpreter)) return;

    var repl = try testRepl(.interpreter);
    defer repl.deinit();
    {
        const result = try repl.step("import \"test/snapshots/eval/file_import_test_data.txt\" as data : List(U8)");
        defer testing.allocator.free(result);
        try testing.expectEqualStrings("imported `data`", result);
    }
    {
        const result = try repl.step("List.len(data)");
        defer testing.allocator.free(result);
        try testing.expectEqualStrings("11", result);
    }
    {
        // "hello world" as raw bytes.
        const result = try repl.step("data");
        defer testing.allocator.free(result);
        try testing.expectEqualStrings("[104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100]", result);
    }
}

test "Repl - missing file import reports a graceful diagnostic instead of panicking" {
    if (!eval.backendAvailable(.interpreter)) return;

    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const result = try repl.step("import \"./repl_file_that_definitely_does_not_exist.txt\" as data : Str");
    defer testing.allocator.free(result);

    try testing.expect(std.mem.find(u8, result, "file not found") != null);
}

test "Repl - simple expressions" {
    try expectAllNative("42", "42.0");
}

test "Repl - string expressions" {
    try expectAllNative("\"Hello, World!\"", "\"Hello, World!\"");
}

test "Repl - Bool.True" {
    try expectAllNative("Bool.True", "True");
}

test "Repl - Bool.False" {
    try expectAllNative("Bool.False", "False");
}

test "Repl - Bool.not(False)" {
    try expectAllNative("Bool.not(False)", "True");
}

test "Repl - Bool.not(Bool.True)" {
    try expectAllNative("Bool.not(Bool.True)", "False");
}

test "Repl - Bool.not(Bool.False)" {
    try expectAllNative("Bool.not(Bool.False)", "True");
}

test "Repl - !Bool.True" {
    try expectAllNative("!Bool.True", "False");
}

test "Repl - !Bool.False" {
    try expectAllNative("!Bool.False", "True");
}

test "Repl - I8.mod_by negative positive" {
    try expectAllNative("I8.mod_by(-10, 3)", "2");
}

test "Repl - I8.mod_by positive negative" {
    try expectAllNative("I8.mod_by(10, -3)", "-2");
}

test "Repl - I8.mod_by negative negative" {
    try expectAllNative("I8.mod_by(-10, -3)", "-1");
}

test "Repl - Str.is_empty" {
    try expectAllNative("Str.is_empty(\"\")", "True");
    try expectAllNative("Str.is_empty(\"a\")", "False");
}

test "Repl - lambda with defaulted literal renders as <function>" {
    try expectAllNative("|x| x + 1", "<function>");
}

test "Repl - unconstrained lambda function value renders as <function>" {
    try expectAllNative("|x, y| x + y", "<function>");
}

test "Repl - recursive function preserves an unconstrained empty list" {
    const steps = &[_][2][]const u8{
        .{
            "loop = |items, n| if n == 0.U64 { items } else { loop(items, n - 1.U64) }",
            "assigned `loop`",
        },
        .{ "loop([], 1.U64)", "[]" },
    };
    try expectStateful(.interpreter, steps);
}

test "Repl - Str.to_utf8 bytes" {
    try expectAllNative("Str.to_utf8(\"hello\")", "[104, 101, 108, 108, 111]");
}

test "Repl - Str.to_utf8 lengths" {
    try expectAllNative("List.len(Str.to_utf8(\"\"))", "0");
    try expectAllNative("List.len(Str.to_utf8(\"hello\"))", "5");
    try expectAllNative("List.len(Str.to_utf8(\"é\"))", "2");
    try expectAllNative("List.len(Str.to_utf8(\"🎉\"))", "4");
    try expectAllNative("List.len(Str.to_utf8(\"Hello, World!\"))", "13");
    try expectAllNative("List.len(Str.to_utf8(\"日本語\"))", "9");
    try expectAllNative("List.len(Str.to_utf8(\"a é 🎉\"))", "9");
}

test "Repl - Str.to_utf8 empty checks" {
    try expectAllNative("List.is_empty(Str.to_utf8(\"\"))", "True");
    try expectAllNative("List.is_empty(Str.to_utf8(\"x\"))", "False");
}

test "Repl - Str.from_utf8_lossy" {
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"hello\"))", "\"hello\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"\"))", "\"\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"🎉 party!\"))", "\"🎉 party!\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"abc123\"))", "\"abc123\"");
}

test "Repl - Str.from_utf8 Ok" {
    try expectAllNative("Str.from_utf8([72, 105])", "Ok(\"Hi\")");
}

test "Repl - Str.from_utf8 ok_or" {
    try expectAllNative("Str.from_utf8([72, 105]).ok_or(\"fallback\")", "\"Hi\"");
}

test "Repl - Str.from_utf8 snapshot sequence" {
    const steps = &[_][2][]const u8{
        .{ "Str.from_utf8([72, 105])", "Ok(\"Hi\")" },
        .{ "Str.from_utf8([])", "Ok(\"\")" },
        .{ "Str.from_utf8([82, 111, 99])", "Ok(\"Roc\")" },
        .{ "Str.from_utf8([240, 159, 144, 166])", "Ok(\"🐦\")" },
        .{ "Str.from_utf8([195, 169])", "Ok(\"é\")" },
        .{ "Str.from_utf8([255]).is_err()", "True" },
        .{ "Str.from_utf8([72, 105]).is_ok()", "True" },
        .{ "Str.from_utf8([72, 105]).ok_or(\"fallback\")", "\"Hi\"" },
        .{ "Str.from_utf8([255]).ok_or(\"fallback\")", "\"fallback\"" },
        .{ "Str.from_utf8([255])", "Err(BadUtf8({ index: 0, problem: InvalidStartByte }))" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
}

test "Repl - U8.from_str result format" {
    try expectAllNative("U8.from_str(\"42\")", "Ok(42)");
}

test "Repl - F32.from_str result format" {
    try expectAllNative("F32.from_str(\"3.14\")", "Ok(3.14)");
}

test "Repl - list literals" {
    try expectAllNative("List.len([1, 2, 3])", "3");
    try expectAllNative("[1, 2, 3]", "[1.0, 2.0, 3.0]");
    try expectAllNative("[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]");
    try expectAllNative("List.len([\"hello\", \"world\", \"test\"])", "3");
}

test "Repl - dropping nested lists of refcounted strings is leak-free" {
    // The string literals are long enough to force heap allocation (past the
    // small-string window), so dropping these values must decref every inner
    // string. The interpreter's list child-decref walk routes through
    // `RocList.decrefElements`; running both the interpreter and dev backends
    // under the leak-checking test allocator catches any divergence between the
    // interpreted traversal and the compiled one as a leak or double-free.
    const nested =
        "[[\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\", \"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"], [\"cccccccccccccccccccccccccccccc\"]]";
    try expectAllNative("List.len(" ++ nested ++ ")", "2");
    try expectAllNative(nested, nested);
}

test "Repl - Json.to_str derives structural encoder_for for literals" {
    try expectAllNative("Json.to_str([1, 2, 3])", "\"[1.0,2.0,3.0]\"");
    try expectAllNative("Json.to_str({name: \"Bob\", age: 20})", "\"{\\\"age\\\":20.0,\\\"name\\\":\\\"Bob\\\"}\"");
    try expectAllNative("Json.to_str(None)", "\"\\\"None\\\"\"");
}

test "Repl - list operations concat" {
    try expectAllNative("List.len(List.concat([1, 2], [3, 4]))", "4");
    try expectAllNative("List.len(List.concat([], [1, 2, 3]))", "3");
    try expectAllNative("List.len(List.concat([1, 2, 3], []))", "3");
}

test "Repl - list operations contains" {
    try expectAllNative("List.contains([1, 2, 3, 4, 5], 3)", "True");
}

test "Repl - list operations filters" {
    try expectAllNative("List.drop_if([1, 2, 3, 4, 5], |x| x > 2)", "[1.0, 2.0]");
    try expectAllNative("List.keep_if([1, 2, 3, 4, 5], |x| x > 2)", "[3.0, 4.0, 5.0]");
    try expectAllNative("List.keep_if([1, 2, 3], |_| Bool.False)", "[]");
}

test "Repl - list operations fold_rev" {
    try expectAllNative("List.fold_rev([1.I64, 2.I64, 3.I64], 0.I64, |x, acc| acc * 10 + x)", "321");
    try expectAllNative("List.fold_rev([1], 0, |x, acc| acc * 10 + x)", "1.0");
    try expectAllNative("List.fold_rev([1, 2, 3], 0, |x, acc| acc * 10 + x)", "321.0");
    try expectAllNative("List.fold_rev([], 42, |x, acc| x + acc)", "42.0");
}

test "Repl - List.with_capacity" {
    try expectAllNative("List.with_capacity(10)", "[]");
    try expectInterpreter("List.first(List.with_capacity(10))", "Err(ListWasEmpty)");
}

test "Repl - List.append" {
    try expectAllNative("List.append([1, 2], 3)", "[1.0, 2.0, 3.0]");
}

test "Repl - range_to" {
    try expectInterpreter("Iter.fold((1..=3).iter(), [], |acc, item| acc.append(item))", "[1.0, 2.0, 3.0]");
}

test "Repl - list_sort_with lengths" {
    try expectAllNative("List.len(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))", "3");
    try expectAllNative("List.len(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))", "5");
}

test "Repl - list_sort_with empty" {
    try expectAllNative(
        \\{
        \\    xs : List(I64)
        \\    xs = []
        \\    List.len(List.sort_with(xs, |a, b| if a < b LT else if a > b GT else EQ))
        \\}
    , "0");
}

test "Repl - list_sort_with single" {
    try expectAllNative("List.len(List.sort_with([42], |a, b| if a < b LT else if a > b GT else EQ))", "1");
}

test "Repl - list fold with concat" {
    try expectAllNative("List.len(List.fold([1, 2, 3], [], |acc, x| List.concat(acc, [x])))", "3");
}

test "Repl - silent assignments" {
    const steps = &[_][2][]const u8{
        .{ "x = 5", "assigned `x`" },
        .{ "x", "5.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - issue 9258 opaque type param field access" {
    const steps = &[_][]const u8{
        "Wrapper(a) := { inner : a }",
        "unwrap : Wrapper(a) -> a",
        "unwrap = |w| w.inner",
        "unwrap({ inner: \"hello\" })",
    };

    try expectStepsFinal(.interpreter, steps, "\"hello\"");
    try expectStepsFinal(.dev, steps, "\"hello\"");
}

test "Repl - optional record field renders <missing> and plain present values" {
    const missing_steps = &[_][]const u8{
        "r : { a ?: U8, b : U8 }",
        "r = { b: 2 }",
        "r",
    };
    try expectStepsFinal(.interpreter, missing_steps, "{ a: <missing>, b: 2 }");
    try expectStepsFinal(.dev, missing_steps, "{ a: <missing>, b: 2 }");

    const present_steps = &[_][]const u8{
        "s : { a ?: U8, b : U8 }",
        "s = { a: 5, b: 2 }",
        "s",
    };
    try expectStepsFinal(.interpreter, present_steps, "{ a: 5, b: 2 }");
    try expectStepsFinal(.dev, present_steps, "{ a: 5, b: 2 }");
}

test "Repl - issue 10576 generalized record update rejects an optional field" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const function_assigned = try repl.step("f = |r| { ..r, a: 5 }");
    defer testing.allocator.free(function_assigned);
    try testing.expectEqualStrings("assigned `f`", function_assigned);

    const annotation = try repl.step("v : { a ?: U64 }");
    defer testing.allocator.free(annotation);
    try testing.expectEqualStrings("", annotation);

    const value_assigned = try repl.step("v = {}");
    defer testing.allocator.free(value_assigned);
    try testing.expectEqualStrings("assigned `v`", value_assigned);

    const result = try repl.step("f(v)");
    defer testing.allocator.free(result);
    try testing.expect(std.mem.find(u8, result, "type mismatch") != null);
}

test "Repl - top-level destructure definitions publish their binders" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const type_assigned = try repl.step("Rec : { req : U8, other : U8 }");
    defer testing.allocator.free(type_assigned);
    try testing.expectEqualStrings("assigned `Rec`", type_assigned);

    const anno = try repl.step("s : Rec");
    defer testing.allocator.free(anno);
    try testing.expectEqualStrings("", anno);

    const assigned = try repl.step("s = { req: 7, other: 1 }");
    defer testing.allocator.free(assigned);
    try testing.expectEqualStrings("assigned `s`", assigned);

    const destructure = try repl.step("{ req, .. } = s");
    defer testing.allocator.free(destructure);
    try testing.expectEqualStrings("assigned `req`", destructure);

    const tuple_destructure = try repl.step("(a, b) = (1, 2)");
    defer testing.allocator.free(tuple_destructure);
    try testing.expectEqualStrings("assigned `a`, `b`", tuple_destructure);

    const req_value = try repl.step("req");
    defer testing.allocator.free(req_value);
    try testing.expectEqualStrings("7", req_value);

    const tuple_sum = try repl.step("a + b");
    defer testing.allocator.free(tuple_sum);
    try testing.expectEqualStrings("3.0", tuple_sum);

    const funcs_anno = try repl.step("funcs : { scale : U64 -> U64, other : U64 }");
    defer testing.allocator.free(funcs_anno);
    try testing.expectEqualStrings("", funcs_anno);

    const funcs_assigned = try repl.step("funcs = { scale: |x| x * 2, other: 0 }");
    defer testing.allocator.free(funcs_assigned);
    try testing.expectEqualStrings("assigned `funcs`", funcs_assigned);

    const funcs_destructure = try repl.step("{ scale, .. } = funcs");
    defer testing.allocator.free(funcs_destructure);
    try testing.expectEqualStrings("assigned `scale`", funcs_destructure);

    const scaled = try repl.step("scale(21)");
    defer testing.allocator.free(scaled);
    try testing.expectEqualStrings("42", scaled);

    const config = reporting.ReportingConfig.initForTesting();
    const req_type = try repl.executeCommandWithConfig(.{ .type_of = "req" }, config);
    defer req_type.deinit(testing.allocator);
    switch (req_type) {
        .output => |output| try testing.expect(std.mem.find(u8, output, "req : U8") != null),
        .diagnostic, .runtime_crash, .none, .exit => return error.TestUnexpectedResult,
    }

    const definitions = try repl.executeCommandWithConfig(.definitions, config);
    defer definitions.deinit(testing.allocator);
    switch (definitions) {
        .output => |output| {
            try testing.expect(std.mem.find(u8, output, "req : U8") != null);
            try testing.expect(std.mem.find(u8, output, "a :") != null);
            try testing.expect(std.mem.find(u8, output, "b :") != null);
        },
        .diagnostic, .runtime_crash, .none, .exit => return error.TestUnexpectedResult,
    }
}

test "Repl - polymorphic numeric in comparison snapshot sequence" {
    const steps = &[_][2][]const u8{
        .{ "is_positive = |x| x > 0", "assigned `is_positive`" },
        .{ "List.any([-1, 0, 1], is_positive)", "True" },
        .{ "List.any([-1, 0, -2], is_positive)", "False" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
}

test "Repl - variable redefinition" {
    const steps = &[_][2][]const u8{
        .{ "x = 5", "assigned `x`" },
        .{ "y = x + 1", "assigned `y`" },
        .{ "y", "6.0" },
        .{ "x = 3", "assigned `x`" },
        .{ "y", "4.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - invalid syntax preserves definitions" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const assigned = try repl.step("x = 42");
    defer testing.allocator.free(assigned);
    try testing.expectEqualStrings("assigned `x`", assigned);

    const diagnostic = try repl.step("x +");
    defer testing.allocator.free(diagnostic);
    try testing.expect(std.mem.find(u8, diagnostic, "unexpected expression syntax") != null);

    const result = try repl.step("x");
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("42.0", result);
}

// Repro for https://github.com/roc-lang/roc/issues/10491: a runtime crash is
// reported without terminating the REPL session.
test "Repl - issue 10491 integer overflow reports crash and continues" {
    const steps = &[_][2][]const u8{
        .{
            "U64.highest + U64.highest",
            "This Roc code crashed with: \"Integer addition overflowed\"",
        },
        .{ "1 + 1", "2.0" },
    };

    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

// Repro for https://github.com/roc-lang/roc/issues/10063: the annotated
// function value should render without evaluating its constrained body.
test "Repl - issue 10063 nested iterator where constraints" {
    const steps = &[_][2][]const u8{
        .{
            "join : b -> List(a) where [b.iter : b -> Iter(item), item.iter : item -> Iter(a)]",
            "",
        },
        .{
            \\join = |list| {
            \\    var $state = []
            \\    for sublist in list {
            \\        for i in sublist {
            \\            $state = $state.append(i)
            \\        }
            \\    }
            \\    $state
            \\}
            ,
            "assigned `join`",
        },
        .{ "join", "<function>" },
    };

    try expectStateful(.interpreter, steps);
}

test "Repl - for loop over list" {
    const steps = &[_][2][]const u8{
        .{ "[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]" },
        .{
            \\count = {
            \\    var counter_ = 0
            \\    for _ in ["hello", "world", "test"] {
            \\        counter_ = counter_ + 1
            \\    }
            \\    counter_
            \\}
            ,
            "assigned `count`",
        },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - for loop snapshots empty list" {
    const steps = &[_][2][]const u8{
        .{
            \\unchanged = {
            \\    var value_ = 42
            \\    for n in [] {
            \\        value_ = n
            \\    }
            \\    value_
            \\}
            ,
            "assigned `unchanged`",
        },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - for loop snapshots conditional" {
    const steps = &[_][2][]const u8{
        .{
            \\result = {
            \\    var all_true_ = Bool.True
            \\    for b in [Bool.True, Bool.True, Bool.False] {
            \\        if b == Bool.False {
            \\            all_true_ = Bool.False
            \\        } else {
            \\            {}
            \\        }
            \\    }
            \\    all_true_
            \\}
            ,
            "assigned `result`",
        },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - for loop snapshots string count" {
    const steps = &[_][2][]const u8{
        .{
            \\count = {
            \\    var counter_ = 0
            \\    for _ in ["hello", "world", "test"] {
            \\        counter_ = counter_ + 1
            \\    }
            \\    counter_
            \\}
            ,
            "assigned `count`",
        },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - for loop snapshots sum" {
    const steps = &[_][2][]const u8{
        .{
            \\sum = {
            \\    var total_ = 0
            \\    for n in [1, 2, 3, 4, 5] {
            \\        total_ = total_ + n
            \\    }
            \\    total_
            \\}
            ,
            "assigned `sum`",
        },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - for loop snapshots nested product" {
    const steps = &[_][2][]const u8{
        .{
            \\product = {
            \\    var result_ = 0
            \\    for i in [1, 2, 3] {
            \\        for j in [10, 20] {
            \\            result_ = result_ + (i * j)
            \\        }
            \\    }
            \\    result_
            \\}
            ,
            "assigned `product`",
        },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - build full source with block syntax" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    try repl.addOrReplaceDefinition("x = 5", "x", .value);
    try repl.addOrReplaceDefinition("y = x + 1", "y", .value);

    const full_source = try repl.buildFullSource("y");
    defer testing.allocator.free(full_source);

    const expected =
        \\{
        \\    x = 5
        \\    y = x + 1
        \\    y
        \\}
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - definition replacement" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    try repl.addOrReplaceDefinition("x = 1", "x", .value);
    try repl.addOrReplaceDefinition("x = 2", "x", .value);
    try repl.addOrReplaceDefinition("x = 3", "x", .value);

    try testing.expect(repl.definitions.count() == 1);

    const full_source = try repl.buildFullSource("x");
    defer testing.allocator.free(full_source);

    const expected =
        \\{
        \\    x = 3
        \\    x
        \\}
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - destructure definitions replace atomically by any bound name" {
    var store = DefinitionStore.init();
    defer store.deinit(testing.allocator);

    const destructured_names = [_][]const u8{ "a", "b" };
    try store.addOrReplaceNames(testing.allocator, "(a, b) = (1, 2)", &destructured_names, .value, false);
    try testing.expectEqual(@as(usize, 1), store.count());
    try testing.expect(store.hasKind("a", .value));
    try testing.expect(store.hasKind("b", .value));

    try store.addOrReplace(testing.allocator, "a = 3", "a", .value, false);
    try testing.expectEqual(@as(usize, 1), store.count());
    try testing.expect(store.hasKind("a", .value));
    try testing.expect(!store.hasKind("b", .value));
    try testing.expectEqualStrings("a = 3", store.items.items[0].source);
}

test "Repl - 4-arg lambda call (dev)" {
    const steps = &[_][2][]const u8{
        .{ "f = |a, b, c, d| a + b + c + d", "assigned `f`" },
        .{ "f(10, 20, 30, 40)", "100.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

fn expectSplit(input: []const u8, expected: []const []const u8) ReplTestError!void {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const slices = try repl.splitInputIntoStatements(input);
    defer repl.freeStatementSlices(slices);

    try testing.expectEqual(expected.len, slices.len);
    for (expected, slices) |want, got| {
        try testing.expectEqualStrings(want, got);
    }
}

test "splitInputIntoStatements - single line passes through" {
    try expectSplit("x = 5", &.{"x = 5"});
}

test "splitInputIntoStatements - special command passes through" {
    try expectSplit(":help", &.{":help"});
}

test "splitInputIntoStatements - two assignments split into two slices" {
    try expectSplit("z = 5\ny = 6", &.{ "z = 5", "y = 6" });
}

test "splitInputIntoStatements - multi-line single statement stays whole" {
    const input =
        \\simple_match = |color| {
        \\    match color {
        \\        Red => "red"
        \\        Green => "green"
        \\        Blue => "blue"
        \\    }
        \\}
    ;
    try expectSplit(input, &.{input});
}

test "splitInputIntoStatements - definition followed by expression" {
    try expectSplit("f = |x| x + 1\nf(5)", &.{ "f = |x| x + 1", "f(5)" });
}

test "splitInputIntoStatements - blank lines between statements are dropped" {
    try expectSplit("a = 1\n\nb = 2", &.{ "a = 1", "b = 2" });
}

test "splitInputIntoStatements - annotation and decl stay separate" {
    try expectSplit("z : U64\nz = 5", &.{ "z : U64", "z = 5" });
}

test "Repl - paste of annotation + decl produces single assigned message" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const slices = try repl.splitInputIntoStatements("z : U64\nz = 5");
    defer repl.freeStatementSlices(slices);
    try testing.expectEqual(@as(usize, 2), slices.len);

    const r0 = try repl.step(slices[0]);
    defer testing.allocator.free(r0);
    try testing.expectEqualStrings("", r0);

    const r1 = try repl.step(slices[1]);
    defer testing.allocator.free(r1);
    try testing.expectEqualStrings("assigned `z`", r1);
}

test "Repl - paste of two assignments processes both" {
    var repl = try testRepl(.interpreter);
    defer repl.deinit();

    const slices = try repl.splitInputIntoStatements("z = 5\ny = 6");
    defer repl.freeStatementSlices(slices);

    try testing.expectEqual(@as(usize, 2), slices.len);

    const r0 = try repl.step(slices[0]);
    defer testing.allocator.free(r0);
    try testing.expectEqualStrings("assigned `z`", r0);

    const r1 = try repl.step(slices[1]);
    defer testing.allocator.free(r1);
    try testing.expectEqualStrings("assigned `y`", r1);
}

test "issue 9364: F64.plus with integer literals" {
    try expectAllNative("F64.plus(1, 1)", "2");
}

test "issue 9364: F64.plus with float literals" {
    try expectAllNative("F64.plus(1.0, 1.0)", "2");
}

test "issue 9364: F64.to_str integer-valued float literal" {
    try expectAllNative("F64.to_str(2.0)", "\"2\"");
}

test "issue 9364: F64.to_str non-integer float literal" {
    try expectAllNative("F64.to_str(2.5)", "\"2.5\"");
}

// Representative wasm coverage. The bulk of expression assertions run on the
// native backends only (`expectAllNative`); this test runs a representative
// spread of value kinds—ints, floats, strings, bools, lists, lambdas, and
// result/tag values—through all backends including wasm, so wasm codegen and
// bytebox execution stay covered without paying for them on every assertion.
// Stateful wasm behavior (assignments, redefinition, for-loops) is additionally
// covered by the `expectStateful(.wasm, ...)` tests above.
test "Repl - representative all-backends coverage (incl. wasm)" {
    try expectAllBackends("42", "42.0");
    try expectAllBackends("\"Hello, World!\"", "\"Hello, World!\"");
    try expectAllBackends("Bool.True", "True");
    try expectAllBackends("Bool.not(False)", "True");
    try expectAllBackends("I8.mod_by(-10, 3)", "2");
    try expectAllBackends("[1, 2, 3]", "[1.0, 2.0, 3.0]");
    try expectAllBackends("[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]");
    try expectAllBackends("List.len([1, 2, 3])", "3");
    try expectAllBackends("List.append([1, 2], 3)", "[1.0, 2.0, 3.0]");
    try expectAllBackends("List.keep_if([1, 2, 3, 4, 5], |x| x > 2)", "[3.0, 4.0, 5.0]");
    try expectAllBackends("|x| x + 1", "<function>");
    try expectAllBackends("Str.to_utf8(\"hello\")", "[104, 101, 108, 108, 111]");
    try expectAllBackends("Str.from_utf8([72, 105])", "Ok(\"Hi\")");
    try expectAllBackends("U8.from_str(\"42\")", "Ok(42)");
    try expectAllBackends("F64.to_str(2.5)", "\"2.5\"");
}
