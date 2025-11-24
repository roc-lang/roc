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
const builtin_loading = eval_mod.builtin_loading;
const collections = @import("collections");

const AST = parse.AST;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;
const LoadedModule = builtin_loading.LoadedModule;

/// REPL state that tracks past definitions and evaluates expressions
pub const Repl = struct {
    allocator: Allocator,
    /// Map from variable name to source string for definitions
    definitions: std.StringHashMap([]const u8),
    /// Map from type name to source string for type declarations
    type_declarations: std.StringHashMap([]const u8),
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
    /// Loaded Builtin module (loaded once at startup)
    builtin_module: LoadedModule,

    pub fn init(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext) !Repl {
        const compiled_builtins = @import("compiled_builtins");

        // Load builtin indices once at startup (generated at build time)
        const builtin_indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Load Builtin module once at startup
        const builtin_source = compiled_builtins.builtin_source;
        var builtin_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", builtin_source);
        errdefer builtin_module.deinit();

        return Repl{
            .allocator = allocator,
            .definitions = std.StringHashMap([]const u8).init(allocator),
            .type_declarations = std.StringHashMap([]const u8).init(allocator),
            .roc_ops = roc_ops,
            .crash_ctx = crash_ctx,
            //.trace_writer = null,
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

    /// Add or replace a type declaration in the REPL context
    pub fn addOrReplaceTypeDeclaration(self: *Repl, source: []const u8, type_name: []const u8) !void {
        // Check if we're replacing an existing type declaration
        if (self.type_declarations.fetchRemove(type_name)) |kv| {
            // Free both the old key and value
            self.allocator.free(kv.key);
            self.allocator.free(kv.value);
        }

        // Duplicate both key and value since they're borrowed from input
        const owned_key = try self.allocator.dupe(u8, type_name);
        const owned_source = try self.allocator.dupe(u8, source);
        try self.type_declarations.put(owned_key, owned_source);
    }

    pub fn deinit(self: *Repl) void {
        // Clean up definition strings and keys
        var iterator = self.definitions.iterator();
        while (iterator.next()) |kv| {
            self.allocator.free(kv.key_ptr.*); // Free the variable name
            self.allocator.free(kv.value_ptr.*); // Free the source string
        }
        self.definitions.deinit();

        // Clean up type declaration strings and keys
        var type_iterator = self.type_declarations.iterator();
        while (type_iterator.next()) |kv| {
            self.allocator.free(kv.key_ptr.*); // Free the type name
            self.allocator.free(kv.value_ptr.*); // Free the source string
        }
        self.type_declarations.deinit();

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
            .type_decl => |info| {
                // Store type declaration for future use
                try self.addOrReplaceTypeDeclaration(info.source, info.type_name);

                // Return the type name (similar to how Roc shows the type name after defining)
                return try self.allocator.dupe(u8, info.type_name);
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
        type_decl: struct {
            source: []const u8, // Borrowed from input
            type_name: []const u8, // Borrowed from input
        },
        parse_error: []const u8, // Must be allocator.dupe'd
    };

    /// Try to parse input as a statement
    fn tryParseStatement(self: *Repl, input: []const u8) !ParseResult {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        var module_env = try ModuleEnv.init(self.allocator, input);
        defer module_env.deinit();

        // Try top-level statement parsing first (allows type declarations)
        if (parse.parseTopLevelStatement(&module_env.common, self.allocator)) |ast_const| {
            var ast = ast_const;
            defer ast.deinit(self.allocator);

            if (ast.root_node_idx != 0) {
                const stmt_idx: AST.Statement.Idx = @enumFromInt(ast.root_node_idx);
                const stmt = ast.store.getStatement(stmt_idx);

                // If we got a valid statement (not malformed), process it
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
                    .type_decl => |type_decl| {
                        // Extract the type name from the type header
                        const ty_header = ast.store.getTypeHeader(type_decl.header) catch {
                            return ParseResult.expression;
                        };
                        const name_tok = ty_header.name;
                        const name_region = ast.tokens.resolve(name_tok);
                        const type_name = module_env.common.source[name_region.start.offset..name_region.end.offset];

                        return ParseResult{ .type_decl = .{
                            .source = input,
                            .type_name = type_name,
                        } };
                    },
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

    /// Check if there are type declarations that require file-level parsing
    pub fn hasTypeDeclarations(self: *Repl) bool {
        return self.type_declarations.count() > 0;
    }

    /// Build full source including all type declarations and definitions
    /// When type declarations exist, generates file-level syntax (type declarations can't be in blocks)
    /// When only definitions exist, wraps in block syntax
    pub fn buildFullSource(self: *Repl, current_expr: []const u8) ![]const u8 {
        // If no definitions or type declarations exist, just return the expression as-is
        if (self.definitions.count() == 0 and self.type_declarations.count() == 0) {
            return try self.allocator.dupe(u8, current_expr);
        }

        var buffer = std.ArrayList(u8).empty;
        errdefer buffer.deinit(self.allocator);

        // If we have type declarations, we need file-level syntax (type declarations can't be in blocks)
        if (self.type_declarations.count() > 0) {
            // Add all type declarations at file level
            var type_iter = self.type_declarations.iterator();
            while (type_iter.next()) |kv| {
                try buffer.appendSlice(self.allocator, kv.value_ptr.*);
                try buffer.append(self.allocator, '\n');
            }

            // Add all variable definitions at file level
            var def_iterator = self.definitions.iterator();
            while (def_iterator.next()) |kv| {
                try buffer.appendSlice(self.allocator, kv.value_ptr.*);
                try buffer.append(self.allocator, '\n');
            }

            // Wrap the final expression in a special definition so it's valid at file level
            // (bare expressions are not allowed at file level)
            // Use a name that won't conflict and doesn't start with underscore
            try buffer.appendSlice(self.allocator, "replResult0 = ");
            try buffer.appendSlice(self.allocator, current_expr);
        } else {
            // No type declarations - use block syntax for definitions
            try buffer.appendSlice(self.allocator, "{\n");

            // Add all variable definitions
            var def_iterator = self.definitions.iterator();
            while (def_iterator.next()) |kv| {
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
        }

        return try buffer.toOwnedSlice(self.allocator);
    }

    /// Evaluate source code
    fn evaluateSource(self: *Repl, source: []const u8) ![]const u8 {
        const module_env = try self.allocateModuleEnv(source);
        return try self.evaluatePureExpression(module_env);
    }

    /// Evaluate a program (which may contain definitions)
    fn evaluatePureExpression(self: *Repl, module_env: *ModuleEnv) ![]const u8 {
        // Check if we have type declarations (requires file-level parsing)
        const has_type_decls = self.type_declarations.count() > 0;
        const has_definitions = self.definitions.count() > 0;

        // Parse appropriately based on what we have
        var parse_ast = if (has_type_decls)
            // Type declarations present - parse as file
            parse.parse(&module_env.common, self.allocator) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
            }
        else if (has_definitions)
            // Only definitions - parse as block expression
            parse.parseExpr(&module_env.common, self.allocator) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
            }
        else
            // No definitions - simple expression
            parse.parseExpr(&module_env.common, self.allocator) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
            };
        defer parse_ast.deinit(self.allocator);

        // Empty scratch space
        parse_ast.store.emptyScratch();

        // Create CIR
        const cir = module_env; // CIR is now just ModuleEnv
        try cir.initCIRFields(self.allocator, "repl");

        // Get Bool, Try, and Str statement indices from the IMPORTED modules (not copied!)
        // These refer to the actual statements in the Builtin module
        const bool_stmt_in_bool_module = self.builtin_indices.bool_type;
        const try_stmt_in_try_module = self.builtin_indices.try_type;
        const str_stmt_in_builtin_module = self.builtin_indices.str_type;

        const module_common_idents: Check.CommonIdents = .{
            .module_name = try module_env.insertIdent(base.Ident.for_text("repl")),
            .list = try module_env.insertIdent(base.Ident.for_text("List")),
            .box = try module_env.insertIdent(base.Ident.for_text("Box")),
            .@"try" = try module_env.insertIdent(base.Ident.for_text("Try")),
            .bool_stmt = bool_stmt_in_bool_module,
            .try_stmt = try_stmt_in_try_module,
            .str_stmt = str_stmt_in_builtin_module,
            .builtin_module = self.builtin_module.env,
        };

        // Create canonicalizer with nested types available for qualified name resolution
        // Register Bool, Try, Str, Dict, and Set individually so qualified access works (e.g., Bool.True)
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, can.Can.AutoImportedType).init(self.allocator);
        defer module_envs_map.deinit();

        const bool_ident = try cir.common.idents.insert(self.allocator, base.Ident.for_text("Bool"));
        const try_ident = try cir.common.idents.insert(self.allocator, base.Ident.for_text("Try"));
        const str_ident = try cir.common.idents.insert(self.allocator, base.Ident.for_text("Str"));
        const dict_ident = try cir.common.idents.insert(self.allocator, base.Ident.for_text("Dict"));
        const set_ident = try cir.common.idents.insert(self.allocator, base.Ident.for_text("Set"));

        try module_envs_map.put(bool_ident, .{
            .env = self.builtin_module.env,
            .statement_idx = self.builtin_indices.bool_type,
        });
        try module_envs_map.put(try_ident, .{
            .env = self.builtin_module.env,
            .statement_idx = self.builtin_indices.try_type,
        });
        // Str is added without statement_idx because it's a primitive builtin type
        try module_envs_map.put(str_ident, .{
            .env = self.builtin_module.env,
        });
        try module_envs_map.put(dict_ident, .{
            .env = self.builtin_module.env,
            .statement_idx = self.builtin_indices.dict_type,
        });
        try module_envs_map.put(set_ident, .{
            .env = self.builtin_module.env,
            .statement_idx = self.builtin_indices.set_type,
        });

        var czer = Can.init(cir, &parse_ast, &module_envs_map) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize init error: {}", .{err});
        };
        defer czer.deinit();

        // NOTE: True/False/Ok/Err are now just anonymous tags that unify with Bool/Try automatically!
        // No need to register unqualified_nominal_tags - the type system handles it.

        var final_expr_idx: can.CIR.Expr.Idx = undefined;

        if (has_type_decls) {
            // File-level canonicalization for type declarations
            czer.canonicalizeFile() catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Canonicalize file error: {}", .{err});
            };

            // Find the replResult0 definition and get its expression
            const repl_result_text = "replResult0";
            const all_defs = cir.store.sliceDefs(cir.all_defs);

            var found_result = false;
            for (all_defs) |def_idx| {
                const def = cir.store.getDef(def_idx);
                const pat = cir.store.getPattern(def.pattern);
                if (pat == .assign) {
                    const def_ident_text = cir.getIdent(pat.assign.ident);
                    if (std.mem.eql(u8, def_ident_text, repl_result_text)) {
                        final_expr_idx = def.expr;
                        found_result = true;
                        break;
                    }
                }
            }

            if (!found_result) {
                return try self.allocator.dupe(u8, "Error: could not find replResult0 definition");
            }
        } else {
            // Expression-level canonicalization (existing behavior)
            const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);

            const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse {
                return try self.allocator.dupe(u8, "Canonicalize expr error: expression returned null");
            };
            final_expr_idx = canonical_expr.get_idx();
        }

        // Type check - Pass Builtin as imported module
        const imported_modules = [_]*const ModuleEnv{self.builtin_module.env};
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

        if (has_type_decls) {
            // Type check the whole file when we have type declarations
            checker.checkFile() catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Type check file error: {}", .{err});
            };
        } else {
            // Just check the expression (existing behavior)
            _ = checker.checkExprRepl(final_expr_idx) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Type check expr error: {}", .{err});
            };
        }

        // Create interpreter instance with BuiltinTypes containing real Builtin module
        const builtin_types_for_eval = BuiltinTypes.init(self.builtin_indices, self.builtin_module.env, self.builtin_module.env, self.builtin_module.env);
        var interpreter = eval_mod.Interpreter.init(self.allocator, module_env, builtin_types_for_eval, self.builtin_module.env, &imported_modules) catch |err| {
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
            // First try to use the runtime type variable attached to the result value
            // This is set by operations like makeBoolValue and is more reliable
            // for values created through method dispatch (like != falling back to is_eq)
            if (result.rt_var) |rt_var| {
                break :blk try interpreter.renderValueRocWithType(result, rt_var);
            }
            // Fall back to translating the expression's compile-time type variable
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
