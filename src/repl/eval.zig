//! The evaluation part of the Read-Eval-Print-Loop (REPL)

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const parse = @import("parse");
const types = @import("types");
const can = @import("can");
const Can = can.Can;
const check = @import("check");
const Check = check.Check;
const builtins = @import("builtins");
const eval_mod = @import("eval");
const roc_target = @import("roc_target");
const compile = @import("compile");
const single_module = compile.single_module;
const CrashContext = eval_mod.CrashContext;
const BuiltinTypes = eval_mod.BuiltinTypes;
const builtin_loading = eval_mod.builtin_loading;

const AST = parse.AST;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;
const LoadedModule = builtin_loading.LoadedModule;
const DevEvaluator = eval_mod.DevEvaluator;

pub const Backend = @import("backend").EvalBackend;
const CommonEnv = base.CommonEnv;

/// Render a parse diagnostic for REPL output (without source context for cleaner display).
/// The REPL already shows the input, so we don't need to repeat it in error messages.
fn renderParseDiagnosticForRepl(
    ast: *AST,
    env: *const CommonEnv,
    diagnostic: AST.Diagnostic,
    allocator: Allocator,
) ![]const u8 {
    // Create the report (this includes source context, but we'll only render the message part)
    var report = try ast.parseDiagnosticToReport(env, diagnostic, allocator, "repl");
    defer report.deinit();

    // Render to markdown
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

    // Strip trailing source context (everything after the last blank line before code block)
    // The format is: **TITLE**\nmessage\n\n**location:**\n```roc\ncode\n```\n^^^^^^
    // We want just: **TITLE**\nmessage
    var end_pos: usize = full_result.len;

    // Find the last occurrence of "\n\n**" which marks the start of the source location section
    if (std.mem.lastIndexOf(u8, full_result, "\n\n**")) |pos| {
        end_pos = pos;
    }

    const trimmed = std.mem.trimRight(u8, full_result[0..end_pos], "\n");
    return try allocator.dupe(u8, trimmed);
}

/// REPL state that tracks past definitions and evaluates expressions
pub const Repl = struct {
    allocator: Allocator,
    /// Map from variable name to source string for definitions
    definitions: std.StringHashMap([]const u8),
    /// Operations for the Roc runtime
    roc_ops: *RocOps,
    /// Shared crash context managed by the host (optional)
    crash_ctx: ?*CrashContext,
    /// Backend for code evaluation
    backend: Backend,
    /// DevEvaluator instance (only initialized when backend is .dev)
    dev_evaluator: ?DevEvaluator,
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
    builtin_module: LoadedModule,

    pub fn init(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext) !Repl {
        return initWithBackend(allocator, roc_ops, crash_ctx, .interpreter);
    }

    pub fn initWithBackend(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext, backend: Backend) !Repl {
        const compiled_builtins = @import("compiled_builtins");

        // Load builtin indices once at startup (generated at build time)
        const builtin_indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Load Builtin module once at startup
        const builtin_source = compiled_builtins.builtin_source;
        var builtin_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", builtin_source);
        errdefer builtin_module.deinit();

        // Initialize DevEvaluator if using dev backend
        var dev_evaluator: ?DevEvaluator = null;
        if (backend == .dev) {
            dev_evaluator = DevEvaluator.init(allocator) catch null;
        }

        return Repl{
            .allocator = allocator,
            .definitions = std.StringHashMap([]const u8).init(allocator),
            .roc_ops = roc_ops,
            .crash_ctx = crash_ctx,
            .backend = backend,
            .dev_evaluator = dev_evaluator,
            .last_module_env = null,
            .debug_store_snapshots = false,
            .debug_can_html = std.array_list.Managed([]const u8).init(allocator),
            .debug_types_html = std.array_list.Managed([]const u8).init(allocator),
            .builtin_indices = builtin_indices,
            .builtin_module = builtin_module,
        };
    }

    // pub fn setTraceWriter(self: *Repl, trace_writer: std.io.AnyWriter) void {
    //     self.trace_writer = trace_writer;
    // }

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

    /// Allocate a new ModuleEnv and save it
    fn allocateModuleEnv(self: *Repl, source: []const u8) !*ModuleEnv {
        // Clean up previous ModuleEnv if it exists
        if (self.last_module_env) |old_env| {
            old_env.deinit();
            self.allocator.destroy(old_env);
        }

        // Allocate new ModuleEnv on heap
        const new_env = try self.allocator.create(ModuleEnv);
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        new_env.* = try ModuleEnv.init(self.allocator, source);
        self.last_module_env = new_env;
        return new_env;
    }

    /// Generate and store CAN and TYPES HTML for debugging
    fn generateAndStoreDebugHtml(self: *Repl, module_env: *ModuleEnv, expr_idx: can.CIR.Expr.Idx) !void {
        const SExprTree = @import("base").SExprTree;

        // Generate CAN HTML
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

        // Generate TYPES HTML
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

    /// Add or replace a definition in the REPL context
    pub fn addOrReplaceDefinition(self: *Repl, source: []const u8, var_name: []const u8) !void {
        // Check if we're replacing an existing definition
        if (self.definitions.fetchRemove(var_name)) |kv| {
            // Free both the old key and value
            self.allocator.free(kv.key);
            self.allocator.free(kv.value);
        }

        // Duplicate both key and value since they're borrowed from input
        const owned_key = try self.allocator.dupe(u8, var_name);
        const owned_source = try self.allocator.dupe(u8, source);
        try self.definitions.put(owned_key, owned_source);
    }

    pub fn deinit(self: *Repl) void {
        // Clean up definition strings and keys
        var iterator = self.definitions.iterator();
        while (iterator.next()) |kv| {
            self.allocator.free(kv.key_ptr.*); // Free the variable name
            self.allocator.free(kv.value_ptr.*); // Free the source string
        }
        self.definitions.deinit();

        // Clean up DevEvaluator if it exists
        if (self.dev_evaluator) |*dev_eval| {
            dev_eval.deinit();
        }

        // Clean up debug HTML storage
        for (self.debug_can_html.items) |html| {
            self.allocator.free(html);
        }
        self.debug_can_html.deinit();

        for (self.debug_types_html.items) |html| {
            self.allocator.free(html);
        }
        self.debug_types_html.deinit();

        // Clean up last ModuleEnv if it exists
        if (self.last_module_env) |module_env| {
            module_env.deinit();
            self.allocator.destroy(module_env);
        }

        // Clean up loaded builtin module
        self.builtin_module.deinit();
    }

    /// Process a line of input and return structured result data.
    /// This is the preferred API for programmatic use (e.g., playground, tests).
    pub fn stepStructured(self: *Repl, line: []const u8) !StepResult {
        const trimmed = std.mem.trim(u8, line, " \t\n\r");

        // Handle special commands
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

        // Process the input
        return try self.processInputStructured(trimmed);
    }

    /// Process a line of input and return the result as a string.
    /// This is a convenience wrapper for CLI REPL use.
    /// For programmatic use, prefer stepStructured() which returns typed data.
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
        // Try to parse as a statement first
        const parse_result = try self.tryParseStatement(input);

        switch (parse_result) {
            .assignment => |info| {
                // Add or replace definition (duplicates the strings for ownership)
                try self.addOrReplaceDefinition(info.source, info.var_name);

                // Return descriptive output for assignments
                return .{ .definition = try std.fmt.allocPrint(self.allocator, "assigned `{s}`", .{info.var_name}) };
            },
            .import => {
                // Imports are not supported in this implementation
                return .{ .parse_error = try self.allocator.dupe(u8, "Imports not yet supported") };
            },
            .expression => {
                // Evaluate expression with all past definitions
                const full_source = try self.buildFullSource(input);
                defer self.allocator.free(full_source);

                return try self.evaluateSourceStructured(full_source);
            },
            .type_decl => {
                // Type declarations can't be evaluated
                return .empty;
            },
            .parse_error => |msg| {
                defer self.allocator.free(msg);
                return .{ .parse_error = try std.fmt.allocPrint(self.allocator, "Parse error: {s}", .{msg}) };
            },
        }
    }

    const ParseResult = union(enum) {
        assignment: struct {
            source: []const u8, // Borrowed from input
            var_name: []const u8, // Borrowed from input
        },
        import,
        expression,
        type_decl,
        parse_error: []const u8, // Must be allocator.dupe'd
    };

    /// The result of a REPL step - structured data that callers can use directly
    /// without parsing human-readable strings.
    pub const StepResult = union(enum) {
        /// Successfully evaluated an expression, contains the rendered value
        expression: []const u8,
        /// Successfully defined a variable
        definition: []const u8,
        /// Help text requested
        help: []const u8,
        /// User requested quit
        quit,
        /// Empty input
        empty,
        /// Parse error with rendered message
        parse_error: []const u8,
        /// Canonicalization error with rendered message
        canonicalize_error: []const u8,
        /// Type checking error with rendered message
        type_error: []const u8,
        /// Evaluation/runtime error with rendered message
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

        /// Returns true if this result represents an error
        pub fn isError(self: StepResult) bool {
            return switch (self) {
                .parse_error, .canonicalize_error, .type_error, .eval_error => true,
                else => false,
            };
        }

        /// Get the message/output for display, if any
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

        // Try statement parsing using the unified compile_module interface
        const stmt_ast = single_module.parseSingleModule(
            &allocators,
            &module_env,
            .statement,
            .{ .module_name = "REPL", .init_cir_fields = false },
        ) catch {
            // Statement parse failed, continue to try expression parsing
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
                        // Extract the identifier name from the pattern
                        const ident_tok = pattern.ident.ident_tok;
                        const token_region = stmt_ast.tokens.resolve(ident_tok);
                        const ident_name = module_env.common.source[token_region.start.offset..token_region.end.offset];

                        // Return borrowed strings (no duplication needed)
                        return ParseResult{ .assignment = .{
                            .source = input,
                            .var_name = ident_name,
                        } };
                    }
                    return ParseResult.expression;
                },
                .import => return ParseResult.import,
                .type_decl => return ParseResult.type_decl,
                else => return ParseResult.expression,
            }
        }

        // No valid statement root, try expression
        return self.tryParseExpressionOnly(input);
    }

    /// Helper to try parsing as expression only
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
        // If no definitions exist, just return the expression as-is
        if (self.definitions.count() == 0) {
            return try self.allocator.dupe(u8, current_expr);
        }

        var buffer = std.ArrayList(u8).empty;
        errdefer buffer.deinit(self.allocator);

        // Start block
        try buffer.appendSlice(self.allocator, "{\n");

        // Add all definitions in order
        var iterator = self.definitions.iterator();
        while (iterator.next()) |kv| {
            try buffer.appendSlice(self.allocator, "    ");
            try buffer.appendSlice(self.allocator, kv.value_ptr.*);
            try buffer.append(self.allocator, '\n');
        }

        // Add current expression
        try buffer.appendSlice(self.allocator, "    ");
        try buffer.appendSlice(self.allocator, current_expr);
        try buffer.append(self.allocator, '\n');

        // End block
        try buffer.append(self.allocator, '}');

        return try buffer.toOwnedSlice(self.allocator);
    }

    /// Evaluate source code - returns structured result
    fn evaluateSourceStructured(self: *Repl, source: []const u8) !StepResult {
        const module_env = try self.allocateModuleEnv(source);
        return try self.evaluatePureExpressionStructured(module_env);
    }

    /// Evaluate a program (which may contain definitions) - returns structured result
    fn evaluatePureExpressionStructured(self: *Repl, module_env: *ModuleEnv) !StepResult {
        var allocators: single_module.Allocators = undefined;
        allocators.initInPlace(self.allocator);
        defer allocators.deinit();

        // Parse using the unified compile_module interface
        // Note: init_cir_fields=false because we call initCIRFields after parsing
        const parse_ast = single_module.parseSingleModule(
            &allocators,
            module_env,
            .expr,
            .{ .module_name = "repl", .init_cir_fields = false },
        ) catch |err| {
            return .{ .parse_error = try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err}) };
        };
        defer parse_ast.deinit();

        // Check for parse errors and render them
        if (parse_ast.hasErrors()) {
            if (parse_ast.tokenize_diagnostics.items.len > 0) {
                var report = try parse_ast.tokenizeDiagnosticToReport(
                    parse_ast.tokenize_diagnostics.items[0],
                    self.allocator,
                    null,
                );
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
                return .{ .parse_error = try output.toOwnedSlice() };
            } else if (parse_ast.parse_diagnostics.items.len > 0) {
                // Render parse diagnostic without source context for cleaner REPL output
                const diagnostic = parse_ast.parse_diagnostics.items[0];
                const error_text = try renderParseDiagnosticForRepl(parse_ast, &module_env.common, diagnostic, self.allocator);
                return .{ .parse_error = error_text };
            }
        }

        // Create CIR
        const cir = module_env;
        try cir.initCIRFields("repl");

        // Populate all auto-imported builtin types using the shared helper to keep behavior consistent
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, can.Can.AutoImportedType).init(self.allocator);
        defer module_envs_map.deinit();

        try Can.populateModuleEnvs(
            &module_envs_map,
            module_env,
            self.builtin_module.env,
            self.builtin_indices,
        );

        var czer = Can.init(&allocators, cir, parse_ast, &module_envs_map) catch |err| {
            return .{ .canonicalize_error = try std.fmt.allocPrint(self.allocator, "Canonicalize init error: {}", .{err}) };
        };
        defer czer.deinit();

        const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);

        const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse {
            const diagnostics = try module_env.getDiagnostics();
            if (diagnostics.len > 0) {
                const diagnostic = diagnostics[0];
                var report = try module_env.diagnosticToReport(diagnostic, self.allocator, "repl");
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
                return .{ .canonicalize_error = try output.toOwnedSlice() };
            }
            return .{ .canonicalize_error = try self.allocator.dupe(u8, "Canonicalize expr error: expression returned null") };
        };
        const final_expr_idx = canonical_expr.get_idx();

        // Build imported_modules with REPL module at index 0, Builtin at index 1.
        // This order must match the all_module_envs passed to the dev evaluator's lowerer
        // and the layout store. The lowerer uses resolved import indices to look up modules
        // in all_module_envs, so the arrays must agree. This matches the test helpers
        // (helpers.zig:1341-1347) which document: "index 0 = user module, index 1 = builtin".
        const imported_modules = [_]*const ModuleEnv{ module_env, self.builtin_module.env };

        // Resolve imports - map each import to its index in imported_modules
        module_env.imports.resolveImports(module_env, &imported_modules);

        const builtin_ctx: Check.BuiltinContext = .{
            .module_name = try module_env.insertIdent(base.Ident.for_text("repl")),
            .bool_stmt = self.builtin_indices.bool_type,
            .try_stmt = self.builtin_indices.try_type,
            .str_stmt = self.builtin_indices.str_type,
            .builtin_module = self.builtin_module.env,
            .builtin_indices = self.builtin_indices,
        };

        var checker = Check.init(
            self.allocator,
            &module_env.types,
            cir,
            &imported_modules,
            &module_envs_map,
            &cir.store.regions,
            builtin_ctx,
        ) catch |err| {
            return .{ .type_error = try std.fmt.allocPrint(self.allocator, "Type check init error: {}", .{err}) };
        };
        defer checker.deinit();

        _ = checker.checkExprRepl(final_expr_idx) catch |err| {
            return .{ .type_error = try std.fmt.allocPrint(self.allocator, "Type check expr error: {}", .{err}) };
        };

        // Check for type problems (e.g., type mismatches)
        if (checker.problems.problems.items.len > 0) {
            // Return a generic type error message
            // TODO: Use ReportBuilder to produce a more detailed error message
            return .{ .type_error = try self.allocator.dupe(u8, "TYPE MISMATCH") };
        }

        // Use DevEvaluator if backend is .dev and we have a DevEvaluator instance
        // ExecutableMemory is not available on freestanding targets (wasm32)
        if (comptime builtin.os.tag != .freestanding) {
            if (self.backend == .dev) {
                if (self.dev_evaluator) |*dev_eval| {
                    // Build module envs array matching imported_modules order:
                    // index 0 = REPL module, index 1 = Builtin module.
                    // This must match the resolveImports call above so that resolved
                    // import indices correctly map to the right module in the lowerer.
                    const all_module_envs: []const *ModuleEnv = &.{ module_env, self.builtin_module.env };
                    var code_result = dev_eval.generateCode(module_env, final_expr_idx, all_module_envs) catch {
                        // Fall back to interpreter on unsupported expressions
                        return self.evaluateWithInterpreter(module_env, final_expr_idx, &imported_modules, &checker);
                    };
                    defer code_result.deinit();

                    // Execute the compiled code (with entry_offset for compiled procedures)
                    var executable = eval_mod.ExecutableMemory.initWithEntryOffset(code_result.code, code_result.entry_offset) catch {
                        return self.evaluateWithInterpreter(module_env, final_expr_idx, &imported_modules, &checker);
                    };
                    defer executable.deinit();

                    // Execute and write result into a stack buffer
                    var result_buf: [512]u8 align(16) = undefined;
                    dev_eval.callWithCrashProtection(&executable, @ptrCast(&result_buf)) catch {
                        return self.evaluateWithInterpreter(module_env, final_expr_idx, &imported_modules, &checker);
                    };

                    // Format using type-driven rendering (walks type var for tag names)
                    const ls = code_result.layout_store orelse
                        return self.evaluateWithInterpreter(module_env, final_expr_idx, &imported_modules, &checker);
                    const result_layout = ls.getLayout(code_result.result_layout);
                    const expr_type_var = ModuleEnv.varFrom(final_expr_idx);

                    const output = formatWithTypes(
                        self.allocator,
                        &result_buf,
                        result_layout,
                        expr_type_var,
                        module_env,
                        ls,
                    ) catch {
                        return self.evaluateWithInterpreter(module_env, final_expr_idx, &imported_modules, &checker);
                    };
                    return .{ .expression = output };
                }
            }
        }

        return self.evaluateWithInterpreter(module_env, final_expr_idx, &imported_modules, &checker);
    }

    /// Evaluate using the interpreter (fallback path)
    fn evaluateWithInterpreter(self: *Repl, module_env: *ModuleEnv, final_expr_idx: can.CIR.Expr.Idx, imported_modules: []const *const ModuleEnv, checker: *Check) !StepResult {
        const builtin_types_for_eval = BuiltinTypes.init(self.builtin_indices, self.builtin_module.env, self.builtin_module.env, self.builtin_module.env);
        var interpreter = eval_mod.Interpreter.init(self.allocator, module_env, builtin_types_for_eval, self.builtin_module.env, imported_modules, &checker.import_mapping, null, null, roc_target.RocTarget.detectNative()) catch |err| {
            return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Interpreter init error: {}", .{err}) };
        };
        defer interpreter.deinitAndFreeOtherEnvs();

        if (self.crash_ctx) |ctx| {
            ctx.reset();
        }

        const result = interpreter.eval(final_expr_idx, self.roc_ops) catch |err| switch (err) {
            error.Crash => {
                if (self.crash_ctx) |ctx| {
                    if (ctx.crashMessage()) |msg| {
                        return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Crash: {s}", .{msg}) };
                    }
                }
                return .{ .eval_error = try self.allocator.dupe(u8, "Evaluation error: error.Crash") };
            },
            else => return .{ .eval_error = try std.fmt.allocPrint(self.allocator, "Evaluation error: {}", .{err}) },
        };

        if (self.debug_store_snapshots) {
            try self.generateAndStoreDebugHtml(module_env, final_expr_idx);
        }

        const output = try interpreter.renderValueRocForRepl(result, result.rt_var, self.roc_ops);

        result.decref(&interpreter.runtime_layout_store, self.roc_ops);
        interpreter.cleanupBindings(self.roc_ops);
        return .{ .expression = output };
    }
};

const layout_mod = @import("layout");
const Layout = layout_mod.Layout;
const RocValue = @import("values").RocValue;

const FormatError = error{OutOfMemory};

/// Format a dev backend result using the type variable for semantic info (tag names,
/// list element types, etc.) and the layout for memory structure. This is the dev
/// backend analog of the interpreter's `renderValueRocWithType`.
fn formatWithTypes(
    allocator: Allocator,
    ptr: ?[*]const u8,
    lay: Layout,
    type_var: types.Var,
    module_env: *const ModuleEnv,
    layout_store: *const layout_mod.Store,
) FormatError![]u8 {
    const types_store = &module_env.types;
    const ident_store = module_env.getIdentStoreConst();
    var resolved = types_store.resolveVar(type_var);

    // Unwrap aliases and nominal types to get the underlying structural type
    var guard: u32 = 0;
    while (guard < 100) : (guard += 1) {
        switch (resolved.desc.content) {
            .alias => |al| {
                const backing = types_store.getAliasBackingVar(al);
                resolved = types_store.resolveVar(backing);
            },
            .structure => |st| switch (st) {
                .nominal_type => |nt| {
                    // Check for List and Box before unwrapping — they need special handling
                    const ident_text = ident_store.getText(nt.ident.ident_idx);
                    if (std.mem.eql(u8, ident_text, "List")) {
                        return formatList(allocator, ptr, lay, nt, module_env, layout_store);
                    }
                    const backing = types_store.getNominalBackingVar(nt);
                    resolved = types_store.resolveVar(backing);
                },
                else => break,
            },
            else => break,
        }
    }

    // Now dispatch based on the resolved structural content + layout
    if (resolved.desc.content == .structure) switch (resolved.desc.content.structure) {
        .tag_union => |tu| {
            return formatTagUnion(allocator, ptr, lay, tu, module_env, layout_store);
        },
        .record => |rec| {
            if (lay.tag == .record) {
                return formatRecord(allocator, ptr, lay, rec, module_env, layout_store);
            }
        },
        .tuple => |tup| {
            if (lay.tag == .tuple) {
                return formatTuple(allocator, ptr, lay, tup, module_env, layout_store);
            }
        },
        .empty_record => return try allocator.dupe(u8, "{}"),
        .empty_tag_union => return try allocator.dupe(u8, "{}"),
        else => {},
    };

    // Use layout-only formatting for scalars and other types
    const roc_val = RocValue{ .ptr = ptr, .lay = lay };
    const fmt_ctx = RocValue.FormatContext{
        .layout_store = layout_store,
        .ident_store = ident_store,
        .strip_whole_number_decimal = true,
    };
    return roc_val.format(allocator, fmt_ctx);
}

/// Format a tag union using type info for tag names and recursive payload formatting.
fn formatTagUnion(
    allocator: Allocator,
    ptr: ?[*]const u8,
    lay: Layout,
    tu: types.TagUnion,
    module_env: *const ModuleEnv,
    layout_store: *const layout_mod.Store,
) FormatError![]u8 {
    const types_store = &module_env.types;
    const ident_store = module_env.getIdentStoreConst();

    // Get the sorted tag at a given discriminant index
    const sorted_tag = getSortedTag(allocator, types_store, ident_store, tu);

    if (lay.tag == .zst) {
        // Single-variant ZST tag union — just output the tag name
        if (sorted_tag) |tags| {
            defer allocator.free(tags);
            if (tags.len > 0) {
                return try allocator.dupe(u8, ident_store.getText(tags[0].name));
            }
        }
        return try allocator.dupe(u8, "{}");
    }

    if (lay.tag == .scalar) {
        // Tag union optimized to scalar — discriminant only, no multi-variant payload
        if (sorted_tag) |tags| {
            defer allocator.free(tags);
            if (lay.data.scalar.tag == .int) {
                const raw = ptr orelse return try allocator.dupe(u8, "<null>");
                const disc: usize = switch (lay.data.scalar.data.int) {
                    .u8 => raw[0],
                    .u16 => @as(u16, raw[0]) | (@as(u16, raw[1]) << 8),
                    .u32 => @intCast(@as(u32, raw[0]) | (@as(u32, raw[1]) << 8) | (@as(u32, raw[2]) << 16) | (@as(u32, raw[3]) << 24)),
                    else => 0,
                };
                if (disc < tags.len) {
                    const tag = tags[disc];
                    const tag_name = ident_store.getText(tag.name);
                    // Check if this tag has payload args
                    const args = types_store.sliceVars(toVarRange(tag.args));
                    if (args.len == 0) {
                        return try allocator.dupe(u8, tag_name);
                    }
                }
            }
        }
        // Fall through to default scalar formatting
        const roc_val = RocValue{ .ptr = ptr, .lay = lay };
        const fmt_ctx = RocValue.FormatContext{
            .layout_store = layout_store,
            .ident_store = ident_store,
            .strip_whole_number_decimal = true,
        };
        return roc_val.format(allocator, fmt_ctx);
    }

    if (lay.tag == .tag_union) {
        const tu_idx = lay.data.tag_union.idx;
        const tu_data = layout_store.getTagUnionData(tu_idx);
        const disc_offset = layout_store.getTagUnionDiscriminantOffset(tu_idx);

        const raw = ptr orelse return try allocator.dupe(u8, "<tag_union>");
        const discriminant = tu_data.readDiscriminantFromPtr(raw + disc_offset);

        if (sorted_tag) |tags| {
            defer allocator.free(tags);
            if (discriminant < tags.len) {
                const tag = tags[discriminant];
                const tag_name = ident_store.getText(tag.name);
                const arg_vars = types_store.sliceVars(toVarRange(tag.args));

                var out = std.array_list.AlignedManaged(u8, null).init(allocator);
                errdefer out.deinit();
                try out.appendSlice(tag_name);

                if (arg_vars.len > 0) {
                    try out.append('(');
                    // Get the payload layout from the variant
                    const variants = layout_store.getTagUnionVariants(tu_data);
                    const payload_layout = layout_store.getLayout(variants.get(discriminant).payload_layout);

                    if (arg_vars.len == 1) {
                        const rendered = try formatWithTypes(allocator, raw, payload_layout, arg_vars[0], module_env, layout_store);
                        defer allocator.free(rendered);
                        try out.appendSlice(rendered);
                    } else {
                        // Multi-arg: payload is a tuple
                        for (arg_vars, 0..) |arg_var, i| {
                            const rendered = try formatWithTypes(allocator, raw, payload_layout, arg_var, module_env, layout_store);
                            defer allocator.free(rendered);
                            try out.appendSlice(rendered);
                            if (i + 1 < arg_vars.len) try out.appendSlice(", ");
                        }
                    }
                    try out.append(')');
                }
                return out.toOwnedSlice();
            }
        }

        // Fallback: no type info
        return try std.fmt.allocPrint(allocator, "<tag_union variant={d}>", .{discriminant});
    }

    // Unknown layout for tag union type — use layout-only formatting
    const roc_val = RocValue{ .ptr = ptr, .lay = lay };
    const fmt_ctx = RocValue.FormatContext{
        .layout_store = layout_store,
        .ident_store = ident_store,
        .strip_whole_number_decimal = true,
    };
    return roc_val.format(allocator, fmt_ctx);
}

/// Format a list using element type info from the nominal List type.
fn formatList(
    allocator: Allocator,
    ptr: ?[*]const u8,
    lay: Layout,
    nt: types.NominalType,
    module_env: *const ModuleEnv,
    layout_store: *const layout_mod.Store,
) FormatError![]u8 {
    const types_store = &module_env.types;
    const arg_vars = types_store.sliceNominalArgs(nt);
    const elem_type_var = if (arg_vars.len == 1) arg_vars[0] else return try allocator.dupe(u8, "<list>");

    var out = std.array_list.AlignedManaged(u8, null).init(allocator);
    errdefer out.deinit();
    try out.append('[');

    if (lay.tag == .list) {
        const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(ptr.?));
        const len = roc_list.len();
        if (len > 0) {
            const elem_layout_idx = lay.data.list;
            const elem_layout = layout_store.getLayout(elem_layout_idx);
            const elem_size = layout_store.layoutSize(elem_layout);
            var i: usize = 0;
            while (i < len) : (i += 1) {
                if (roc_list.bytes) |bytes| {
                    const elem_ptr: [*]const u8 = bytes + i * elem_size;
                    const rendered = try formatWithTypes(allocator, elem_ptr, elem_layout, elem_type_var, module_env, layout_store);
                    defer allocator.free(rendered);
                    try out.appendSlice(rendered);
                    if (i + 1 < len) try out.appendSlice(", ");
                }
            }
        }
    } else if (lay.tag == .list_of_zst) {
        const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(ptr.?));
        const len = roc_list.len();
        const zst_layout = Layout{ .tag = .zst, .data = .{ .zst = {} } };
        var i: usize = 0;
        while (i < len) : (i += 1) {
            const rendered = try formatWithTypes(allocator, null, zst_layout, elem_type_var, module_env, layout_store);
            defer allocator.free(rendered);
            try out.appendSlice(rendered);
            if (i + 1 < len) try out.appendSlice(", ");
        }
    }

    try out.append(']');
    return out.toOwnedSlice();
}

/// Format a record using type info for field names and recursive field formatting.
fn formatRecord(
    allocator: Allocator,
    ptr: ?[*]const u8,
    lay: Layout,
    rec: types.Record,
    module_env: *const ModuleEnv,
    layout_store: *const layout_mod.Store,
) FormatError![]u8 {
    const types_store = &module_env.types;
    const rec_data = layout_store.getRecordData(lay.data.record.idx);

    if (rec_data.fields.count == 0) {
        return try allocator.dupe(u8, "{}");
    }

    var out = std.array_list.AlignedManaged(u8, null).init(allocator);
    errdefer out.deinit();
    try out.appendSlice("{ ");

    const layout_fields = layout_store.record_fields.sliceRange(rec_data.getFields());
    const type_fields = types_store.getRecordFieldsSlice(rec.fields);
    const count = @min(layout_fields.len, type_fields.len);

    for (0..count) |i| {
        const l_fld = layout_fields.get(i);
        const t_fld = type_fields.get(i);
        const name_text = layout_store.getFieldName(l_fld.name);
        try out.appendSlice(name_text);
        try out.appendSlice(": ");

        const offset = layout_store.getRecordFieldOffset(lay.data.record.idx, @intCast(i));
        const field_layout = layout_store.getLayout(l_fld.layout);
        const base_ptr = ptr.?;
        const field_ptr = base_ptr + offset;
        const rendered = try formatWithTypes(allocator, field_ptr, field_layout, t_fld.var_, module_env, layout_store);
        defer allocator.free(rendered);
        try out.appendSlice(rendered);
        if (i + 1 < count) try out.appendSlice(", ");
    }

    try out.appendSlice(" }");
    return out.toOwnedSlice();
}

/// Format a tuple using type info for element types.
fn formatTuple(
    allocator: Allocator,
    ptr: ?[*]const u8,
    lay: Layout,
    tup: types.Tuple,
    module_env: *const ModuleEnv,
    layout_store: *const layout_mod.Store,
) FormatError![]u8 {
    const types_store = &module_env.types;
    const tuple_data = layout_store.getTupleData(lay.data.tuple.idx);
    const layout_fields = layout_store.tuple_fields.sliceRange(tuple_data.getFields());
    const elem_vars = types_store.sliceVars(tup.elems);
    const count = @min(layout_fields.len, elem_vars.len);

    var out = std.array_list.AlignedManaged(u8, null).init(allocator);
    errdefer out.deinit();
    try out.append('(');

    // Iterate by original source index
    var original_idx: usize = 0;
    while (original_idx < count) : (original_idx += 1) {
        const sorted_idx = blk: {
            for (0..count) |si| {
                if (layout_fields.get(si).index == original_idx) break :blk si;
            }
            unreachable;
        };
        const fld = layout_fields.get(sorted_idx);
        const field_layout = layout_store.getLayout(fld.layout);
        const elem_offset = layout_store.getTupleElementOffset(lay.data.tuple.idx, @intCast(sorted_idx));
        const base_ptr = ptr.?;
        const elem_ptr = base_ptr + elem_offset;
        const rendered = try formatWithTypes(allocator, elem_ptr, field_layout, elem_vars[original_idx], module_env, layout_store);
        defer allocator.free(rendered);
        try out.appendSlice(rendered);
        if (original_idx + 1 < count) try out.appendSlice(", ");
    }

    try out.append(')');
    return out.toOwnedSlice();
}

/// Collect all tags from a tag union (following extension chains), sort alphabetically,
/// and return as an owned slice. Caller must free the returned slice.
fn getSortedTag(
    allocator: Allocator,
    types_store: *const types.Store,
    ident_store: *const base.Ident.Store,
    tu: types.TagUnion,
) ?[]types.Tag {
    var all_tags = std.ArrayList(types.Tag).empty;

    // Collect from initial row
    const tags_slice = types_store.getTagsSlice(tu.tags);
    const names = tags_slice.items(.name);
    const args = tags_slice.items(.args);
    for (names, args) |name, arg| {
        all_tags.append(allocator, .{ .name = name, .args = arg }) catch return null;
    }

    // Follow extension variable chain
    var current_ext = tu.ext;
    var guard: u32 = 0;
    while (guard < 100) : (guard += 1) {
        const ext_resolved = types_store.resolveVar(current_ext);
        switch (ext_resolved.desc.content) {
            .structure => |ext_flat| switch (ext_flat) {
                .tag_union => |ext_tu| {
                    const ext_tags = types_store.getTagsSlice(ext_tu.tags);
                    const ext_names = ext_tags.items(.name);
                    const ext_args = ext_tags.items(.args);
                    for (ext_names, ext_args) |name, arg| {
                        all_tags.append(allocator, .{ .name = name, .args = arg }) catch return null;
                    }
                    current_ext = ext_tu.ext;
                },
                .empty_tag_union => break,
                .nominal_type => |nominal| {
                    const backing_var = types_store.getNominalBackingVar(nominal);
                    current_ext = backing_var;
                },
                else => break,
            },
            .alias => |alias| {
                current_ext = types_store.getAliasBackingVar(alias);
            },
            .flex, .rigid => break,
            else => break,
        }
    }

    if (all_tags.items.len == 0) {
        all_tags.deinit(allocator);
        return null;
    }

    // Sort alphabetically — matches discriminant assignment order
    std.mem.sort(types.Tag, all_tags.items, ident_store, types.Tag.sortByNameAsc);
    return all_tags.toOwnedSlice(allocator) catch null;
}

fn toVarRange(range: anytype) types.Var.SafeList.Range {
    const RangeType = types.Var.SafeList.Range;
    if (comptime @hasField(@TypeOf(range), "nonempty")) {
        return @field(range, "nonempty");
    }
    return @as(RangeType, range);
}
