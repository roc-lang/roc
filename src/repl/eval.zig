//! The evaluation part of the Read-Eval-Print-Loop (REPL)

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const eval_mod = @import("eval");
const compile = @import("compile");
const backend = @import("backend");
const collections = @import("collections");

const single_module = compile.single_module;
const eval_pipeline = eval_mod.pipeline;
const builtin_loading = eval_mod.builtin_loading;
const Interpreter = eval_mod.Interpreter;
const CrashContext = eval_mod.CrashContext;

const AST = parse.AST;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const Check = check.Check;
const CIR = can.CIR;
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
const LayoutStore = @import("layout").Store;
const LayoutIdx = @import("layout").Idx;

const Backend = eval_mod.EvalBackend;

/// Render a parse diagnostic for REPL output (without source context for cleaner display).
/// The REPL already shows the input, so we don't need to repeat it in error messages.
fn renderParseDiagnosticForRepl(
    ast: *AST,
    env: *const base.CommonEnv,
    diagnostic: AST.Diagnostic,
    allocator: Allocator,
) ![]const u8 {
    var report = try ast.parseDiagnosticToReport(env, diagnostic, allocator, "repl");
    defer report.deinit();

    var output = std.array_list.Managed(u8).init(allocator);
    var unmanaged = output.moveToUnmanaged();
    var writer_alloc = std.Io.Writer.Allocating.fromArrayList(allocator, &unmanaged);
    report.render(&writer_alloc.writer, .markdown) catch |err| switch (err) {
        error.WriteFailed => return error.OutOfMemory,
        else => return err,
    };
    unmanaged = writer_alloc.toArrayList();
    output = unmanaged.toManaged(allocator);
    const full_result = try output.toOwnedSlice();
    defer allocator.free(full_result);

    var end_pos: usize = full_result.len;
    if (std.mem.lastIndexOf(u8, full_result, "\n\n**")) |pos| {
        end_pos = pos;
    }

    const trimmed = std.mem.trimRight(u8, full_result[0..end_pos], "\n");
    return try allocator.dupe(u8, trimmed);
}

const ParsedResources = eval_pipeline.ParsedResources;
const LoweredProgram = eval_pipeline.LoweredProgram;

const ParseOutcome = union(enum) {
    ok: ParsedResources,
    parse_error: []const u8,
    canonicalize_error: []const u8,
    type_error: []const u8,
};

/// REPL state that tracks past definitions and evaluates expressions
pub const Repl = struct {
    allocator: Allocator,
    /// Map from variable name to index in definitions_order
    definitions: std.StringHashMap(usize),
    /// Definitions in stable insertion order
    definitions_order: std.array_list.Managed(Definition),
    /// Operations for the Roc runtime
    roc_ops: *RocOps,
    /// Shared crash context managed by the host (optional)
    crash_ctx: ?*CrashContext,
    /// Backend for code evaluation
    backend: Backend,
    /// ModuleEnv from last successful evaluation (for snapshot generation)
    last_module_env: ?*ModuleEnv,
    /// Debug flag to store rendered HTML for snapshot generation
    debug_store_snapshots: bool,
    /// Storage for rendered CAN HTML at each step (only when debug_store_snapshots is true)
    debug_can_html: std.array_list.Managed([]const u8),
    /// Storage for rendered TYPES HTML at each step (only when debug_store_snapshots is true)
    debug_types_html: std.array_list.Managed([]const u8),
    /// Builtin type declaration indices (loaded once at startup from builtin_indices.bin)
    builtin_indices: can.CIR.BuiltinIndices,
    /// Loaded Builtin module (loaded once at startup)
    builtin_module: builtin_loading.LoadedModule,
    /// Resources from the last successful parse/check (owned for debug use)
    last_resources: ?ParsedResources,

    const Definition = struct {
        name: []const u8,
        source: []const u8,
    };

    pub fn init(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext) !Repl {
        return initInternal(allocator, roc_ops, crash_ctx, .interpreter);
    }

    pub fn initWithBackend(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext, backend_kind: Backend) !Repl {
        if (!Repl.backendAvailable(backend_kind)) return error.BackendUnavailable;
        return initInternal(allocator, roc_ops, crash_ctx, backend_kind);
    }

    pub fn initWithWasmBackend(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext) !Repl {
        if (!Repl.backendAvailable(.wasm)) return error.BackendUnavailable;
        return initInternal(allocator, roc_ops, crash_ctx, .wasm);
    }

    pub fn backendAvailable(backend_kind: Backend) bool {
        return switch (backend_kind) {
            .interpreter => true,
            .dev => true,
            .wasm => true,
            .llvm => false,
        };
    }

    fn initInternal(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext, backend_kind: Backend) !Repl {
        const compiled_builtins = @import("compiled_builtins");

        const builtin_indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
        var builtin_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
        errdefer builtin_module.deinit();

        return Repl{
            .allocator = allocator,
            .definitions = std.StringHashMap(usize).init(allocator),
            .definitions_order = std.array_list.Managed(Definition).init(allocator),
            .roc_ops = roc_ops,
            .crash_ctx = crash_ctx,
            .backend = backend_kind,
            .last_module_env = null,
            .debug_store_snapshots = false,
            .debug_can_html = std.array_list.Managed([]const u8).init(allocator),
            .debug_types_html = std.array_list.Managed([]const u8).init(allocator),
            .builtin_indices = builtin_indices,
            .builtin_module = builtin_module,
            .last_resources = null,
        };
    }

    /// Enable debug mode to store snapshot HTML for each REPL step
    pub fn enableDebugSnapshots(self: *Repl) void {
        self.debug_store_snapshots = true;
    }

    /// Get pointer to last ModuleEnv for snapshot generation
    pub fn getLastModuleEnv(self: *Repl) ?*ModuleEnv {
        return self.last_module_env;
    }

    /// Get debug CAN HTML for all steps (only available when debug_store_snapshots is enabled)
    pub fn getDebugCanHtml(self: *Repl) []const []const u8 {
        return self.debug_can_html.items;
    }

    /// Get debug TYPES HTML for all steps (only available when debug_store_snapshots is enabled)
    pub fn getDebugTypesHtml(self: *Repl) []const []const u8 {
        return self.debug_types_html.items;
    }

    /// Add or replace a definition in the REPL context
    pub fn addOrReplaceDefinition(self: *Repl, source: []const u8, var_name: []const u8) !void {
        if (self.definitions.getEntry(var_name)) |entry| {
            const index = entry.value_ptr.*;
            var definition = &self.definitions_order.items[index];
            self.allocator.free(definition.source);
            definition.source = try self.allocator.dupe(u8, source);
            return;
        }

        const owned_name = try self.allocator.dupe(u8, var_name);
        const owned_source = try self.allocator.dupe(u8, source);
        try self.definitions_order.append(.{
            .name = owned_name,
            .source = owned_source,
        });
        try self.definitions.put(owned_name, self.definitions_order.items.len - 1);
    }

    pub fn deinit(self: *Repl) void {
        self.clearLastResources();

        self.definitions.deinit();
        for (self.definitions_order.items) |definition| {
            self.allocator.free(definition.name);
            self.allocator.free(definition.source);
        }
        self.definitions_order.deinit();

        for (self.debug_can_html.items) |html| {
            self.allocator.free(html);
        }
        self.debug_can_html.deinit();

        for (self.debug_types_html.items) |html| {
            self.allocator.free(html);
        }
        self.debug_types_html.deinit();

        if (self.last_module_env) |module_env| {
            module_env.deinit();
            self.allocator.destroy(module_env);
        }

        self.builtin_module.deinit();
    }

    /// Process a line of input and return structured result data.
    pub fn stepStructured(self: *Repl, line: []const u8) !StepResult {
        const trimmed = std.mem.trim(u8, line, " \t\n\r");

        if (trimmed.len == 0) {
            return .empty;
        }

        if (std.mem.eql(u8, trimmed, ":help")) {
            return .{ .help = try self.allocator.dupe(u8,
                \\Enter an expression to evaluate, or a definition (like x = 1) to use later.
                \\
                \\  - :q quits
                \\  - :help shows this text again
            ) };
        }

        if (std.mem.eql(u8, trimmed, ":exit") or
            std.mem.eql(u8, trimmed, ":quit") or
            std.mem.eql(u8, trimmed, ":q") or
            std.mem.eql(u8, trimmed, "exit") or
            std.mem.eql(u8, trimmed, "quit") or
            std.mem.eql(u8, trimmed, "exit()") or
            std.mem.eql(u8, trimmed, "quit()"))
        {
            return .quit;
        }

        return try self.processInputStructured(trimmed);
    }

    /// Process a line of input and return the result as a string.
    pub fn step(self: *Repl, line: []const u8) ![]const u8 {
        const result = try self.stepStructured(line);
        return switch (result) {
            .expression => |s| s,
            .definition => |s| s,
            .help => |s| s,
            .parse_error => |s| s,
            .canonicalize_error => |s| s,
            .type_error => |s| s,
            .eval_error => |s| s,
            .quit => try self.allocator.dupe(u8, "Goodbye!"),
            .empty => try self.allocator.dupe(u8, ""),
        };
    }

    /// Process regular input (not special commands) - returns structured result
    fn processInputStructured(self: *Repl, input: []const u8) !StepResult {
        const cleaned_input = stripTrailingComment(input);
        const parse_result = try self.tryParseStatement(cleaned_input);

        switch (parse_result) {
            .assignment => |info| {
                try self.addOrReplaceDefinition(info.source, info.var_name);
                return .{ .definition = try std.fmt.allocPrint(self.allocator, "assigned `{s}`", .{info.var_name}) };
            },
            .import => {
                return .{ .parse_error = try self.allocator.dupe(u8, "Imports not yet supported") };
            },
            .expression => {
                const full_source = try self.buildFullSource(cleaned_input);
                defer self.allocator.free(full_source);
                return try self.evaluateSourceStructured(full_source);
            },
            .statement => {
                const full_source = try self.buildFullSourceStatement(cleaned_input);
                defer self.allocator.free(full_source);
                return try self.evaluateSourceStructured(full_source);
            },
            .type_decl => return .empty,
            .parse_error => |msg| {
                defer self.allocator.free(msg);
                return .{ .parse_error = try std.fmt.allocPrint(self.allocator, "Parse error: {s}", .{msg}) };
            },
        }
    }

    fn stripTrailingComment(input: []const u8) []const u8 {
        var in_string = false;
        var escape = false;
        for (input, 0..) |c, i| {
            if (in_string) {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (c == '\\') {
                    escape = true;
                    continue;
                }
                if (c == '"') {
                    in_string = false;
                }
                continue;
            }

            if (c == '"') {
                in_string = true;
                continue;
            }
            if (c == '#') {
                return std.mem.trimRight(u8, input[0..i], " \t");
            }
        }
        return std.mem.trimRight(u8, input, " \t");
    }

    const ParseResult = union(enum) {
        assignment: struct {
            source: []const u8,
            var_name: []const u8,
        },
        import,
        expression,
        statement,
        type_decl,
        parse_error: []const u8,
    };

    /// The result of a REPL step - structured data that callers can use directly
    pub const StepResult = union(enum) {
        expression: []const u8,
        definition: []const u8,
        help: []const u8,
        quit,
        empty,
        parse_error: []const u8,
        canonicalize_error: []const u8,
        type_error: []const u8,
        eval_error: []const u8,

        pub fn deinit(self: StepResult, allocator: Allocator) void {
            switch (self) {
                .expression => |s| allocator.free(s),
                .definition => |s| allocator.free(s),
                .help => |s| allocator.free(s),
                .parse_error => |s| allocator.free(s),
                .canonicalize_error => |s| allocator.free(s),
                .type_error => |s| allocator.free(s),
                .eval_error => |s| allocator.free(s),
                .quit, .empty => {},
            }
        }

        pub fn isError(self: StepResult) bool {
            return switch (self) {
                .parse_error, .canonicalize_error, .type_error, .eval_error => true,
                else => false,
            };
        }

        pub fn getMessage(self: StepResult) ?[]const u8 {
            return switch (self) {
                .expression => |s| s,
                .definition => |s| s,
                .help => |s| s,
                .parse_error => |s| s,
                .canonicalize_error => |s| s,
                .type_error => |s| s,
                .eval_error => |s| s,
                .quit, .empty => null,
            };
        }
    };

    /// Try to parse input as a statement
    fn tryParseStatement(self: *Repl, input: []const u8) !ParseResult {
        var module_env = try ModuleEnv.init(self.allocator, input);
        defer module_env.deinit();

        var allocators: single_module.Allocators = undefined;
        allocators.initInPlace(self.allocator);
        defer allocators.deinit();

        const stmt_ast = single_module.parseSingleModule(
            &allocators,
            &module_env,
            .statement,
            .{ .module_name = "REPL", .init_cir_fields = false },
        ) catch {
            return self.tryParseExpressionOnly(input);
        };
        defer stmt_ast.deinit();

        if (stmt_ast.root_node_idx != 0) {
            const stmt_idx: AST.Statement.Idx = @enumFromInt(stmt_ast.root_node_idx);
            const stmt = stmt_ast.store.getStatement(stmt_idx);

            switch (stmt) {
                .decl => |decl| {
                    const pattern = stmt_ast.store.getPattern(decl.pattern);
                    if (pattern == .ident) {
                        const ident_tok = pattern.ident.ident_tok;
                        const token_region = stmt_ast.tokens.resolve(ident_tok);
                        const ident_name = module_env.common.source[token_region.start.offset..token_region.end.offset];

                        return ParseResult{ .assignment = .{
                            .source = input,
                            .var_name = ident_name,
                        } };
                    }
                    return ParseResult.statement;
                },
                .type_anno => {
                    return ParseResult{ .parse_error = try self.allocator.dupe(u8, "Type annotations are not supported in the REPL yet") };
                },
                .expr => return ParseResult.expression,
                .import => return ParseResult.import,
                .type_decl => return ParseResult.type_decl,
                else => return ParseResult.statement,
            }
        }

        return self.tryParseExpressionOnly(input);
    }

    fn tryParseExpressionOnly(self: *Repl, input: []const u8) !ParseResult {
        var module_env = try ModuleEnv.init(self.allocator, input);
        defer module_env.deinit();

        var allocators: single_module.Allocators = undefined;
        allocators.initInPlace(self.allocator);
        defer allocators.deinit();

        const expr_ast = single_module.parseSingleModule(
            &allocators,
            &module_env,
            .expr,
            .{ .module_name = "REPL", .init_cir_fields = false },
        ) catch {
            return ParseResult{ .parse_error = try self.allocator.dupe(u8, "Failed to parse input") };
        };
        defer expr_ast.deinit();

        if (expr_ast.root_node_idx != 0) {
            return ParseResult.expression;
        }

        return ParseResult{ .parse_error = try self.allocator.dupe(u8, "Failed to parse input") };
    }

    /// Build full source including all definitions wrapped in block syntax
    pub fn buildFullSource(self: *Repl, current_expr: []const u8) ![]const u8 {
        if (self.definitions_order.items.len == 0) {
            return try self.allocator.dupe(u8, current_expr);
        }

        var buffer = std.ArrayList(u8).empty;
        errdefer buffer.deinit(self.allocator);

        try buffer.appendSlice(self.allocator, "{\n");
        for (self.definitions_order.items) |definition| {
            try buffer.appendSlice(self.allocator, "    ");
            try buffer.appendSlice(self.allocator, definition.source);
            try buffer.append(self.allocator, '\n');
        }

        try buffer.appendSlice(self.allocator, "    ");
        try buffer.appendSlice(self.allocator, current_expr);
        try buffer.append(self.allocator, '\n');
        try buffer.append(self.allocator, '}');

        return try buffer.toOwnedSlice(self.allocator);
    }

    pub fn buildFullSourceStatement(self: *Repl, current_stmt: []const u8) ![]const u8 {
        var buffer = std.ArrayList(u8).empty;
        errdefer buffer.deinit(self.allocator);

        try buffer.appendSlice(self.allocator, "{\n");
        for (self.definitions_order.items) |definition| {
            try buffer.appendSlice(self.allocator, "    ");
            try buffer.appendSlice(self.allocator, definition.source);
            try buffer.append(self.allocator, '\n');
        }

        try buffer.appendSlice(self.allocator, "    ");
        try buffer.appendSlice(self.allocator, current_stmt);
        try buffer.append(self.allocator, '\n');
        try buffer.append(self.allocator, '}');

        return try buffer.toOwnedSlice(self.allocator);
    }

    fn evaluateSourceStructured(self: *Repl, source: []const u8) !StepResult {
        return try self.evaluatePureExpressionStructured(source);
    }

    fn evaluatePureExpressionStructured(self: *Repl, source: []const u8) !StepResult {
        var pre = try self.parseAndCheck(source, false);
        switch (pre) {
            .parse_error => |msg| return .{ .parse_error = msg },
            .canonicalize_error => |msg| return .{ .canonicalize_error = msg },
            .type_error => |msg| return .{ .type_error = msg },
            .ok => |*resources| {
                defer resources.deinit(self.allocator);
                if (isTopLevelLambda(resources.module_env, resources.expr_idx)) {
                    return .{ .expression = try self.allocator.dupe(u8, "<function>") };
                }
            },
        }

        self.clearLastResources();

        const parsed = try self.parseAndCheck(source, true);
        switch (parsed) {
            .parse_error => |msg| return .{ .parse_error = msg },
            .canonicalize_error => |msg| return .{ .canonicalize_error = msg },
            .type_error => |msg| return .{ .type_error = msg },
            .ok => |resources| {
                self.last_module_env = resources.module_env;
                self.last_resources = resources;
            },
        }

        const resources_ptr = &self.last_resources.?;
        var lowered = lowerTypedCIRToLir(self.allocator, resources_ptr) catch |err| {
            return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Lowering error: {s}", .{@errorName(err)}) };
        };
        defer lowered.deinit();

        if (self.debug_store_snapshots) {
            try self.generateAndStoreDebugHtml(resources_ptr.module_env, resources_ptr.expr_idx);
        }

        return try self.evaluateLowered(&lowered);
    }

    fn evaluateLowered(self: *Repl, lowered: *const LoweredProgram) !StepResult {
        if (!Repl.backendAvailable(self.backend)) {
            return .{ .eval_error = try self.allocator.dupe(u8, "Backend unavailable") };
        }

        return switch (self.backend) {
            .interpreter => self.evaluateWithInterpreter(lowered),
            .dev => self.evaluateWithDev(lowered, "Dev"),
            .wasm => self.evaluateWithWasm(lowered),
            .llvm => .{ .eval_error = try self.allocator.dupe(u8, "LLVM backend not implemented for REPL") },
        };
    }

    fn evaluateWithInterpreter(self: *Repl, lowered: *const LoweredProgram) !StepResult {
        var interp = try Interpreter.init(
            self.allocator,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            self.roc_ops,
        );
        defer interp.deinit();

        if (self.crash_ctx) |ctx| {
            ctx.reset();
        }

        const result = interp.eval(.{ .proc_id = lowered.main_proc }) catch |err| {
            if (interp.getRuntimeErrorMessage()) |msg| {
                return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Runtime error: {s}", .{msg}) };
            }
            if (interp.getExpectMessage()) |msg| {
                return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Expect failed: {s}", .{msg}) };
            }
            if (interp.getCrashMessage()) |msg| {
                return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Crash: {s}", .{msg}) };
            }
            return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Interpreter error: {s}", .{@errorName(err)}) };
        };

        const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
        const output = try copyReturnedRocStr(
            self.allocator,
            &lowered.lir_result.layouts,
            proc.ret_layout,
            result.value.ptr,
            null,
        );
        interp.dropValue(result.value, proc.ret_layout);
        return .{ .expression = output };
    }

    fn evaluateWithDev(self: *Repl, lowered: *const LoweredProgram, backend_name: []const u8) !StepResult {
        var codegen = try HostLirCodeGen.init(
            self.allocator,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            null,
        );
        defer codegen.deinit();
        try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());

        const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
        const entrypoint = try codegen.generateEntrypointWrapper(
            "roc_repl_main",
            lowered.main_proc,
            &.{},
            proc.ret_layout,
        );

        var exec_mem = try ExecutableMemory.initWithEntryOffset(
            codegen.getGeneratedCode(),
            entrypoint.offset,
        );
        defer exec_mem.deinit();

        const ret_layout = proc.ret_layout;
        const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(ret_layout));
        const alloc_len = @max(size_align.size, 1);
        const ret_buf = try self.allocator.alignedAlloc(u8, collections.max_roc_alignment, alloc_len);
        defer self.allocator.free(ret_buf);
        @memset(ret_buf, 0);

        exec_mem.callRocABI(@ptrCast(self.roc_ops), @ptrCast(ret_buf.ptr), null);

        if (self.crash_ctx) |ctx| {
            if (ctx.crashMessage()) |msg| {
                return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "{s} backend crash: {s}", .{ backend_name, msg }) };
            }
        }

        const output = try copyReturnedRocStr(
            self.allocator,
            &lowered.lir_result.layouts,
            ret_layout,
            ret_buf.ptr,
            self.roc_ops,
        );
        return .{ .expression = output };
    }

    fn evaluateWithWasm(self: *Repl, lowered: *const LoweredProgram) !StepResult {
        const output = eval_mod.wasm_evaluator.evalLoweredToStr(self.allocator, lowered) catch |err| {
            return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Wasm backend error: {s}", .{@errorName(err)}) };
        };
        return .{ .expression = output };
    }

    fn clearLastResources(self: *Repl) void {
        if (self.last_resources) |*resources| {
            resources.deinit(self.allocator);
            self.last_resources = null;
            self.last_module_env = null;
        }
    }

    fn parseAndCheck(self: *Repl, source: []const u8, inspect_wrap: bool) !ParseOutcome {
        var resources = eval_pipeline.parseAndCanonicalizeProgramWrapped(
            self.allocator,
            .expr,
            source,
            &.{},
            inspect_wrap,
        ) catch |err| switch (err) {
            error.ParseError => return .{ .parse_error = try self.allocator.dupe(u8, "Parse error") },
            error.NoRootDefinition => return .{ .parse_error = try self.allocator.dupe(u8, "No expressions found") },
            else => return err,
        };
        errdefer resources.deinit(self.allocator);

        if (resources.checker.problems.problems.items.len > 0) {
            const problem = resources.checker.problems.problems.items[0];
            const empty_modules: []const *const ModuleEnv = &.{};
            const line_starts = resources.module_env.common.line_starts.items.items;
            const saved_line_start0 = if (line_starts.len > 0) line_starts[0] else 0;
            if (line_starts.len > 0) {
                line_starts[0] = eval_pipeline.exprSourcePrefixLen(inspect_wrap);
            }
            defer if (line_starts.len > 0) {
                line_starts[0] = saved_line_start0;
            };
            var report_builder = check.ReportBuilder.init(
                self.allocator,
                resources.module_env,
                resources.module_env,
                &resources.checker.snapshots,
                &resources.checker.problems,
                "repl",
                empty_modules,
                &resources.checker.import_mapping,
                &resources.checker.regions,
            ) catch {
                resources.deinit(self.allocator);
                return .{ .type_error = try self.allocator.dupe(u8, "TYPE MISMATCH") };
            };
            defer report_builder.deinit();

            var report = report_builder.build(problem) catch {
                resources.deinit(self.allocator);
                return .{ .type_error = try self.allocator.dupe(u8, "TYPE MISMATCH") };
            };
            defer report.deinit();

            var output = std.array_list.Managed(u8).init(self.allocator);
            var unmanaged = output.moveToUnmanaged();
            var writer_alloc = std.Io.Writer.Allocating.fromArrayList(self.allocator, &unmanaged);
            report.render(&writer_alloc.writer, .markdown) catch |err| switch (err) {
                error.WriteFailed => return error.OutOfMemory,
                else => return err,
            };
            unmanaged = writer_alloc.toArrayList();
            output = unmanaged.toManaged(self.allocator);
            const rendered = output.items;
            const trimmed = std.mem.trimRight(u8, rendered, " \t\r\n");
            var msg: []const u8 = try self.allocator.dupe(u8, trimmed);
            output.deinit();

            resources.deinit(self.allocator);
            msg = try stripReplWrapperFromReport(self.allocator, msg, inspect_wrap);
            return .{ .type_error = msg };
        }

        const diagnostics = try resources.module_env.getDiagnostics();
        if (diagnostics.len > 0) {
            const line_starts = resources.module_env.common.line_starts.items.items;
            const saved_line_start0 = if (line_starts.len > 0) line_starts[0] else 0;
            if (line_starts.len > 0) {
                line_starts[0] = eval_pipeline.exprSourcePrefixLen(inspect_wrap);
            }
            defer if (line_starts.len > 0) {
                line_starts[0] = saved_line_start0;
            };
            var error_diag_index: ?usize = null;
            for (diagnostics, 0..) |diagnostic, i| {
                var report = try resources.module_env.diagnosticToReport(diagnostic, self.allocator, "repl");
                if (report.severity != .warning) {
                    report.deinit();
                    error_diag_index = i;
                    break;
                }
                report.deinit();
            }
            if (error_diag_index == null) {
                return .{ .ok = resources };
            }
            var report = try resources.module_env.diagnosticToReport(diagnostics[error_diag_index.?], self.allocator, "repl");
            defer report.deinit();

            var output = std.array_list.Managed(u8).init(self.allocator);
            var unmanaged = output.moveToUnmanaged();
            var writer_alloc = std.Io.Writer.Allocating.fromArrayList(self.allocator, &unmanaged);
            report.render(&writer_alloc.writer, .markdown) catch |err| switch (err) {
                error.WriteFailed => return error.OutOfMemory,
                else => return err,
            };
            unmanaged = writer_alloc.toArrayList();
            output = unmanaged.toManaged(self.allocator);
            const rendered = output.items;
            const trimmed = std.mem.trimRight(u8, rendered, " \t\r\n");
            var msg: []const u8 = try self.allocator.dupe(u8, trimmed);
            output.deinit();
            resources.deinit(self.allocator);
            msg = try stripReplWrapperFromReport(self.allocator, msg, inspect_wrap);
            return .{ .canonicalize_error = msg };
        }

        return .{ .ok = resources };
    }

    fn generateAndStoreDebugHtml(self: *Repl, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) !void {
        const SExprTree = @import("base").SExprTree;

        {
            var tree = SExprTree.init(self.allocator);
            defer tree.deinit();
            try module_env.pushToSExprTree(expr_idx, &tree);

            var can_buffer = std.ArrayList(u8).empty;
            defer can_buffer.deinit(self.allocator);
            try tree.toStringPretty(can_buffer.writer(self.allocator).any(), .include_linecol);

            const can_html = try self.allocator.dupe(u8, can_buffer.items);
            try self.debug_can_html.append(can_html);
        }

        {
            var tree = SExprTree.init(self.allocator);
            defer tree.deinit();
            try module_env.pushTypesToSExprTree(expr_idx, &tree);

            var types_buffer = std.ArrayList(u8).empty;
            defer types_buffer.deinit(self.allocator);
            try tree.toStringPretty(types_buffer.writer(self.allocator).any(), .include_linecol);

            const types_html = try self.allocator.dupe(u8, types_buffer.items);
            try self.debug_types_html.append(types_html);
        }
    }

    fn stripReplWrapperFromReport(allocator: Allocator, msg: []const u8, inspect_wrap: bool) ![]const u8 {
        const prefix = if (inspect_wrap) "main = Str.inspect((" else "main = ";
        const suffix = if (inspect_wrap) "))" else "";
        const needle = if (inspect_wrap) "```roc\nmain = Str.inspect((" else "```roc\nmain = ";

        if (std.mem.indexOf(u8, msg, needle) == null) {
            return msg;
        }

        var output = std.ArrayList(u8).empty;
        errdefer output.deinit(allocator);

        var cursor: usize = 0;
        while (true) {
            const fence_start = std.mem.indexOfPos(u8, msg, cursor, "```roc\n") orelse break;
            try output.appendSlice(allocator, msg[cursor..fence_start]);
            try output.appendSlice(allocator, "```roc\n");

            const line_start = fence_start + "```roc\n".len;
            const line_end = std.mem.indexOfPos(u8, msg, line_start, "\n") orelse msg.len;
            const line = msg[line_start..line_end];
            if (std.mem.startsWith(u8, line, prefix)) {
                var trimmed_line = line[prefix.len..];
                if (inspect_wrap and std.mem.endsWith(u8, trimmed_line, suffix)) {
                    trimmed_line = trimmed_line[0 .. trimmed_line.len - suffix.len];
                }
                try output.appendSlice(allocator, trimmed_line);
            } else {
                try output.appendSlice(allocator, line);
            }

            cursor = line_end;
        }

        try output.appendSlice(allocator, msg[cursor..]);
        allocator.free(msg);
        return try output.toOwnedSlice(allocator);
    }
};

fn isTopLevelLambda(module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) bool {
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_lambda, .e_closure, .e_hosted_lambda => true,
        else => false,
    };
}

fn lowerTypedCIRToLir(allocator: Allocator, resources: *ParsedResources) !LoweredProgram {
    const module_envs = try allocator.alloc(*const ModuleEnv, resources.extra_modules.len + 2);
    defer allocator.free(module_envs);
    module_envs[0] = resources.module_env;
    module_envs[1] = resources.builtin_module.env;
    for (resources.extra_modules, 0..) |extra, i| {
        module_envs[i + 2] = extra.module_env;
    }

    return eval_pipeline.lowerTypedCIRToLir(allocator, &resources.typed_cir_modules, module_envs);
}

fn copyReturnedRocStr(
    allocator: Allocator,
    layout_store: *const LayoutStore,
    ret_layout: LayoutIdx,
    value_ptr: [*]u8,
    roc_ops: ?*RocOps,
) ![]u8 {
    const layout_val = layout_store.getLayout(ret_layout);
    const is_str =
        ret_layout == .str or
        (layout_val.tag == .scalar and layout_val.data.scalar.tag == .str);

    if (!is_str) {
        std.debug.panic(
            "repl inspect invariant violated: expected Str return layout, found {s}",
            .{@tagName(layout_val.tag)},
        );
    }

    const roc_str = @as(*align(1) const RocStr, @ptrCast(value_ptr)).*;
    const copied = try allocator.dupe(u8, roc_str.asSlice());
    if (roc_ops) |ops| roc_str.decref(ops);
    return copied;
}
