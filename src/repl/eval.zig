//! The evaluation part of the Read-Eval-Print-Loop (REPL)

const std = @import("std");
const base = @import("base");
const compile = @import("compile");
const parse = @import("parse");
const types = @import("types");
const can = @import("can");
const Can = can.Can;
const check = @import("check");
const Check = check.Check;
const builtins = @import("builtins");
const eval_mod = @import("eval");
const CrashContext = eval_mod.CrashContext;
const BuiltinTypes = eval_mod.BuiltinTypes;
const collections = @import("collections");

const AST = parse.AST;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;

/// Wrapper for a loaded compiled builtin module that tracks the buffer
const LoadedModule = struct {
    env: *ModuleEnv,
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    gpa: std.mem.Allocator,

    fn deinit(self: *LoadedModule) void {
        // IMPORTANT: When a module is deserialized from a buffer, all its internal structures
        // (common, types, external_decls, imports, store) contain pointers INTO the buffer,
        // not separately allocated memory. Therefore we should NOT call deinit() on any of them.
        // The only memory we need to free is:
        // 1. The buffer itself (which contains all the deserialized data)
        // 2. The env struct itself (which was allocated with create())

        // Free the buffer (all data structures point into this buffer)
        self.gpa.free(self.buffer);

        // Free the env struct itself
        self.gpa.destroy(self.env);
    }
};

/// Deserialize BuiltinIndices from the binary data generated at build time
fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) !can.CIR.BuiltinIndices {
    // Copy embedded data to properly aligned memory
    const alignment = @alignOf(can.CIR.BuiltinIndices);
    const buffer = try gpa.alignedAlloc(u8, @enumFromInt(alignment), bin_data.len);
    defer gpa.free(buffer);

    @memcpy(buffer, bin_data);

    // Cast to the structure
    const indices_ptr = @as(*const can.CIR.BuiltinIndices, @ptrCast(buffer.ptr));
    return indices_ptr.*;
}

/// Load a compiled ModuleEnv from embedded binary data
fn loadCompiledModule(gpa: std.mem.Allocator, bin_data: []const u8, module_name: []const u8, source: []const u8) !LoadedModule {
    // Copy the embedded data to properly aligned memory
    // CompactWriter requires specific alignment for serialization
    const CompactWriter = collections.CompactWriter;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bin_data.len);
    @memcpy(buffer, bin_data);

    // Cast to the serialized structure
    const serialized_ptr = @as(
        *ModuleEnv.Serialized,
        @ptrCast(@alignCast(buffer.ptr)),
    );

    const env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(env);

    // Deserialize
    const base_ptr = @intFromPtr(buffer.ptr);

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = serialized_ptr.common.deserialize(@as(i64, @intCast(base_ptr)), source).*,
        .types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr)), gpa).*, // Pass gpa to types deserialize
        .module_kind = serialized_ptr.module_kind,
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserialize(@as(i64, @intCast(base_ptr))).*,
        .imports = (try serialized_ptr.imports.deserialize(@as(i64, @intCast(base_ptr)), gpa)).*,
        .module_name = module_name,
        .module_name_idx = undefined, // Not used for deserialized modules (only needed during fresh canonicalization)
        .diagnostics = serialized_ptr.diagnostics,
        .store = serialized_ptr.store.deserialize(@as(i64, @intCast(base_ptr)), gpa).*,
        .evaluation_order = null,
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
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
    /// Optional trace writer for debugging evaluation
    //trace_writer: ?std.io.AnyWriter,
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
    /// Loaded Bool module (loaded once at startup)
    bool_module: LoadedModule,
    /// Loaded Result module (loaded once at startup)
    result_module: LoadedModule,

    pub fn init(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext) !Repl {
        const compiled_builtins = @import("compiled_builtins");

        // Load builtin indices once at startup (generated at build time)
        const builtin_indices = try deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Load Bool module once at startup
        const bool_source = "Bool := [True, False].{}\n";
        var bool_module = try loadCompiledModule(allocator, compiled_builtins.bool_bin, "Bool", bool_source);
        errdefer bool_module.deinit();

        // Load Result module once at startup
        const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
        var result_module = try loadCompiledModule(allocator, compiled_builtins.result_bin, "Result", result_source);
        errdefer result_module.deinit();

        return Repl{
            .allocator = allocator,
            .definitions = std.StringHashMap([]const u8).init(allocator),
            .roc_ops = roc_ops,
            .crash_ctx = crash_ctx,
            //.trace_writer = null,
            .last_module_env = null,
            .debug_store_snapshots = false,
            .debug_can_html = std.array_list.Managed([]const u8).init(allocator),
            .debug_types_html = std.array_list.Managed([]const u8).init(allocator),
            .builtin_indices = builtin_indices,
            .bool_module = bool_module,
            .result_module = result_module,
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

        // Clean up loaded builtin modules
        self.bool_module.deinit();
        self.result_module.deinit();
    }

    /// Process a line of input and return the result
    pub fn step(self: *Repl, line: []const u8) ![]const u8 {
        const trimmed = std.mem.trim(u8, line, " \t\n\r");

        // Handle special commands
        if (trimmed.len == 0) {
            return try self.allocator.dupe(u8, "");
        }

        if (std.mem.eql(u8, trimmed, ":help")) {
            return try self.allocator.dupe(u8,
                \\Enter an expression to evaluate, or a definition (like x = 1) to use later.
                \\
                \\  - :q quits
                \\  - :help shows this text again
            );
        }

        if (std.mem.eql(u8, trimmed, ":exit") or
            std.mem.eql(u8, trimmed, ":quit") or
            std.mem.eql(u8, trimmed, ":q") or
            std.mem.eql(u8, trimmed, "exit") or
            std.mem.eql(u8, trimmed, "quit") or
            std.mem.eql(u8, trimmed, "exit()") or
            std.mem.eql(u8, trimmed, "quit()"))
        {
            return try self.allocator.dupe(u8, "Goodbye!");
        }

        // Process the input
        return try self.processInput(trimmed);
    }

    /// Process regular input (not special commands)
    fn processInput(self: *Repl, input: []const u8) ![]const u8 {
        // Try to parse as a statement first
        const parse_result = try self.tryParseStatement(input);

        switch (parse_result) {
            .assignment => |info| {
                // Add or replace definition (duplicates the strings for ownership)
                try self.addOrReplaceDefinition(info.source, info.var_name);

                // Return descriptive output for assignments
                return try std.fmt.allocPrint(self.allocator, "assigned `{s}`", .{info.var_name});
            },
            .import => {
                // Imports are not supported in this implementation
                return try self.allocator.dupe(u8, "Imports not yet supported");
            },
            .expression => {
                // Evaluate expression with all past definitions
                const full_source = try self.buildFullSource(input);
                defer self.allocator.free(full_source);

                return try self.evaluateSource(full_source);
            },
            .type_decl => {
                // Type declarations can't be evaluated
                return try self.allocator.dupe(u8, "");
            },
            .parse_error => |msg| {
                defer self.allocator.free(msg);
                return try std.fmt.allocPrint(self.allocator, "Parse error: {s}", .{msg});
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

    /// Try to parse input as a statement
    fn tryParseStatement(self: *Repl, input: []const u8) !ParseResult {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        var module_env = try ModuleEnv.init(self.allocator, input);
        defer module_env.deinit();

        // Try statement parsing
        if (parse.parseStatement(&module_env.common, self.allocator)) |ast_const| {
            var ast = ast_const;
            defer ast.deinit(self.allocator);

            if (ast.root_node_idx != 0) {
                const stmt_idx: AST.Statement.Idx = @enumFromInt(ast.root_node_idx);
                const stmt = ast.store.getStatement(stmt_idx);

                switch (stmt) {
                    .decl => |decl| {
                        const pattern = ast.store.getPattern(decl.pattern);
                        if (pattern == .ident) {
                            // Extract the identifier name from the pattern
                            const ident_tok = pattern.ident.ident_tok;
                            const token_region = ast.tokens.resolve(ident_tok);
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
        } else |_| {
            // Statement parse failed, continue to try expression parsing
        }

        // Try expression parsing
        if (parse.parseExpr(&module_env.common, self.allocator)) |ast_const| {
            var ast = ast_const;
            defer ast.deinit(self.allocator);
            if (ast.root_node_idx != 0) {
                return ParseResult.expression;
            }
        } else |_| {
            // Expression parse failed too
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

    /// Evaluate source code
    fn evaluateSource(self: *Repl, source: []const u8) ![]const u8 {
        const module_env = try self.allocateModuleEnv(source);
        return try self.evaluatePureExpression(module_env);
    }

    /// Evaluate a program (which may contain definitions)
    fn evaluatePureExpression(self: *Repl, module_env: *ModuleEnv) ![]const u8 {

        // Determine if we have definitions (which means we built a block expression)
        const has_definitions = self.definitions.count() > 0;

        // Parse appropriately based on whether we have definitions
        var parse_ast = if (has_definitions)
            // Has definitions - we built a block expression, parse as expression
            parse.parseExpr(&module_env.common, self.allocator) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
            }
        else
            // No definitions - simple expression, parse as expression
            parse.parseExpr(&module_env.common, self.allocator) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
            };
        defer parse_ast.deinit(self.allocator);

        // Empty scratch space
        parse_ast.store.emptyScratch();

        // Create CIR
        const cir = module_env; // CIR is now just ModuleEnv
        try cir.initCIRFields(self.allocator, "repl");

        // Get Bool and Result statement indices from the IMPORTED modules (not copied!)
        // These refer to the actual statements in the Bool/Result modules
        const bool_stmt_in_bool_module = self.builtin_indices.bool_type;
        const result_stmt_in_result_module = self.builtin_indices.result_type;

        const module_common_idents: Check.CommonIdents = .{
            .module_name = try module_env.insertIdent(base.Ident.for_text("repl")),
            .list = try module_env.insertIdent(base.Ident.for_text("List")),
            .box = try module_env.insertIdent(base.Ident.for_text("Box")),
            .bool_stmt = bool_stmt_in_bool_module,
            .result_stmt = result_stmt_in_result_module,
        };

        // Create canonicalizer with Bool and Result modules available for qualified name resolution
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, can.Can.AutoImportedType).init(self.allocator);
        defer module_envs_map.deinit();
        const bool_ident = try cir.common.idents.insert(self.allocator, base.Ident.for_text("Bool"));
        const result_ident = try cir.common.idents.insert(self.allocator, base.Ident.for_text("Result"));
        try module_envs_map.put(bool_ident, .{
            .env = self.bool_module.env,
        });
        try module_envs_map.put(result_ident, .{
            .env = self.result_module.env,
        });

        var czer = Can.init(cir, &parse_ast, &module_envs_map) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize init error: {}", .{err});
        };
        defer czer.deinit();

        // NOTE: True/False/Ok/Err are now just anonymous tags that unify with Bool/Result automatically!
        // No need to register unqualified_nominal_tags - the type system handles it.

        // Since we're always parsing as expressions now, handle them the same way
        const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);

        const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse {
            return try self.allocator.dupe(u8, "Canonicalize expr error: expression returned null");
        };
        const final_expr_idx = canonical_expr.get_idx();

        // Type check - Pass Bool and Result as imported modules
        const imported_modules = [_]*const ModuleEnv{ self.bool_module.env, self.result_module.env };
        var checker = Check.init(
            self.allocator,
            &module_env.types,
            cir,
            &imported_modules,
            &module_envs_map,
            &cir.store.regions,
            module_common_idents,
        ) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check init error: {}", .{err});
        };
        defer checker.deinit();

        // Check the expression (no need to check defs since we're parsing as expressions)
        _ = checker.checkExprRepl(final_expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check expr error: {}", .{err});
        };

        // Create interpreter instance with BuiltinTypes containing real Bool and Result modules
        const builtin_types_for_eval = BuiltinTypes.init(self.builtin_indices, self.bool_module.env, self.result_module.env);
        var interpreter = eval_mod.Interpreter.init(self.allocator, module_env, builtin_types_for_eval, &module_envs_map) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Interpreter init error: {}", .{err});
        };
        defer interpreter.deinitAndFreeOtherEnvs();

        if (self.crash_ctx) |ctx| {
            ctx.reset();
        }

        const result = interpreter.evalMinimal(final_expr_idx, self.roc_ops) catch |err| switch (err) {
            error.Crash => {
                if (self.crash_ctx) |ctx| {
                    if (ctx.crashMessage()) |msg| {
                        return try std.fmt.allocPrint(self.allocator, "Crash: {s}", .{msg});
                    }
                }
                return try self.allocator.dupe(u8, "Evaluation error: error.Crash");
            },
            else => return try std.fmt.allocPrint(self.allocator, "Evaluation error: {}", .{err}),
        };

        // Generate debug HTML if enabled
        if (self.debug_store_snapshots) {
            try self.generateAndStoreDebugHtml(module_env, final_expr_idx);
        }

        const expr_ct_var = can.ModuleEnv.varFrom(final_expr_idx);
        const output = blk: {
            const expr_rt_var = interpreter.translateTypeVar(module_env, expr_ct_var) catch {
                break :blk try interpreter.renderValueRoc(result);
            };
            break :blk try interpreter.renderValueRocWithType(result, expr_rt_var);
        };

        result.decref(&interpreter.runtime_layout_store, self.roc_ops);
        if (result.layout.tag == .record) {
            self.allocator.free(output);
            return try self.allocator.dupe(u8, "<record>");
        }
        return output;
    }
};
