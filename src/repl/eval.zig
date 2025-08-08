//! The evaluation part of the Read-Eval-Print-Loop (REPL)

const std = @import("std");
const base = @import("base");
const compile = @import("compile");
const parse = @import("parse");
const types = @import("types");
const Can = @import("can");
const Check = @import("check");
const builtins = @import("builtins");

const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");
const eval = @import("../eval/interpreter.zig");
const stack = @import("../eval/stack.zig");
const TestEnv = @import("repl_test_env.zig").TestEnv;

const AST = parse.AST;
const Allocator = std.mem.Allocator;
const ModuleEnv = compile.ModuleEnv;
const RocDec = builtins.dec.RocDec;
const RocOps = builtins.host_abi.RocOps;
const types_store = types.store;
const writers = types.writers;
const target = base.target;

/// REPL state that tracks past definitions and evaluates expressions
pub const Repl = struct {
    allocator: Allocator,
    /// Map from variable name to source string for definitions  
    definitions: std.StringHashMap([]const u8),
    /// Stack for evaluation
    eval_stack: stack.Stack,
    /// Operations for the Roc runtime
    roc_ops: *RocOps,
    /// Optional trace writer for debugging evaluation
    trace_writer: ?std.io.AnyWriter,
    /// ModuleEnv from last successful evaluation (for snapshot generation)
    last_module_env: ?*ModuleEnv,
    /// Debug flag to store rendered HTML for snapshot generation
    debug_store_snapshots: bool,
    /// Storage for rendered CAN HTML at each step (only when debug_store_snapshots is true)
    debug_can_html: std.ArrayList([]const u8),
    /// Storage for rendered TYPES HTML at each step (only when debug_store_snapshots is true)
    debug_types_html: std.ArrayList([]const u8),

    pub fn init(allocator: Allocator, roc_ops: *RocOps) !Repl {
        const eval_stack = try stack.Stack.initCapacity(allocator, 8192);

        return Repl{
            .allocator = allocator,
            .definitions = std.StringHashMap([]const u8).init(allocator),
            .eval_stack = eval_stack,
            .roc_ops = roc_ops,
            .trace_writer = null,
            .last_module_env = null,
            .debug_store_snapshots = false,
            .debug_can_html = std.ArrayList([]const u8).init(allocator),
            .debug_types_html = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn setTraceWriter(self: *Repl, trace_writer: std.io.AnyWriter) void {
        self.trace_writer = trace_writer;
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

    /// Save ModuleEnv 
    fn saveModuleEnv(self: *Repl, module_env: *ModuleEnv) !void {
        // Clean up previous ModuleEnv if it exists
        if (self.last_module_env) |old_env| {
            old_env.deinit();
            self.allocator.destroy(old_env);
        }

        // Move the ModuleEnv to heap and save pointer (don't clone, just transfer ownership)
        const new_env = try self.allocator.create(ModuleEnv);
        new_env.* = module_env.*; // Transfer ownership
        self.last_module_env = new_env;
    }

    /// Generate and store CAN and TYPES HTML for debugging
    fn generateAndStoreDebugHtml(self: *Repl, module_env: *ModuleEnv, expr_idx: ModuleEnv.Expr.Idx) !void {
        const SExprTree = @import("base").SExprTree;
        
        // Generate CAN HTML
        {
            var tree = SExprTree.init(self.allocator);
            defer tree.deinit();
            try module_env.pushToSExprTree(expr_idx, &tree);
            
            var can_buffer = std.ArrayList(u8).init(self.allocator);
            defer can_buffer.deinit();
            try tree.toStringPretty(can_buffer.writer().any());
            
            const can_html = try self.allocator.dupe(u8, can_buffer.items);
            try self.debug_can_html.append(can_html);
        }

        // Generate TYPES HTML
        {
            var tree = SExprTree.init(self.allocator);
            defer tree.deinit();
            try module_env.pushTypesToSExprTree(expr_idx, &tree);
            
            var types_buffer = std.ArrayList(u8).init(self.allocator);
            defer types_buffer.deinit();
            try tree.toStringPretty(types_buffer.writer().any());
            
            const types_html = try self.allocator.dupe(u8, types_buffer.items);
            try self.debug_types_html.append(types_html);
        }
    }

    /// Add or replace a definition in the REPL context
    fn addOrReplaceDefinition(self: *Repl, source: []const u8, var_name: []const u8) !void {
        if (self.definitions.get(var_name)) |existing_source| {
            // Replace existing definition
            self.allocator.free(existing_source);
        }
        
        // Store the new definition (no need to dupe var_name, it's already duped)
        try self.definitions.put(var_name, try self.allocator.dupe(u8, source));
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
        
        self.eval_stack.deinit();
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
                defer self.allocator.free(info.source);
                defer self.allocator.free(info.var_name);

                // Add or replace definition
                try self.addOrReplaceDefinition(info.source, info.var_name);

                // Return empty string for silent assignments (per design document)
                return try self.allocator.dupe(u8, "");
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
            source: []const u8,      // Must be allocator.dupe'd
            var_name: []const u8,    // Must be allocator.dupe'd - the variable name
        },
        import,
        expression,
        type_decl,
        parse_error: []const u8, // Must be allocator.dupe'd
    };

    /// Try to parse input as a statement
    fn tryParseStatement(self: *Repl, input: []const u8) !ParseResult {
        var module_env = try ModuleEnv.init(self.allocator, input);
        defer module_env.deinit();

        // Try statement parsing
        if (parse.parseStatement(&module_env)) |ast_const| {
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
                            const ident_name = module_env.source[token_region.start.offset..token_region.end.offset];
                            
                            // Make copies since ast will be freed
                            const source_copy = try self.allocator.dupe(u8, input);
                            const var_name_copy = try self.allocator.dupe(u8, ident_name);
                            return ParseResult{ .assignment = .{ 
                                .source = source_copy,
                                .var_name = var_name_copy,
                            }};
                        }
                        return ParseResult.expression;
                    },
                    .import => return ParseResult.import,
                    .type_decl => return ParseResult.type_decl,
                    else => return ParseResult.expression,
                }
            }
        } else |err| {
            // Debug: let's see what the statement parse error is
            std.debug.print("Statement parse error: {}\n", .{err});
        }

        // Try expression parsing
        if (parse.parseExpr(&module_env)) |ast_const| {
            var ast = ast_const;
            defer ast.deinit(self.allocator);
            if (ast.root_node_idx != 0) {
                return ParseResult.expression;
            }
        } else |err| {
            // Debug: let's see what the parse error is
            std.debug.print("Expression parse error: {}\n", .{err});
        }

        return ParseResult{ .parse_error = try self.allocator.dupe(u8, "Failed to parse input") };
    }

    /// Build full source including all definitions wrapped in block syntax
    fn buildFullSource(self: *Repl, current_expr: []const u8) ![]const u8 {
        // If no definitions exist, just return the expression as-is
        if (self.definitions.count() == 0) {
            return try self.allocator.dupe(u8, current_expr);
        }

        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        // Start block
        try buffer.appendSlice("{\n");

        // Add all definitions in order
        var iterator = self.definitions.iterator();
        while (iterator.next()) |kv| {
            try buffer.appendSlice("    ");
            try buffer.appendSlice(kv.value_ptr.*);
            try buffer.append('\n');
        }

        // Add current expression
        try buffer.appendSlice("    ");
        try buffer.appendSlice(current_expr);
        try buffer.append('\n');

        // End block
        try buffer.append('}');

        return try buffer.toOwnedSlice();
    }

    /// Evaluate source code
    fn evaluateSource(self: *Repl, source: []const u8) ![]const u8 {
        return try self.evaluatePureExpression(source);
    }

    /// Evaluate a program (which may contain definitions)
    fn evaluatePureExpression(self: *Repl, program_source: []const u8) ![]const u8 {

        // Create module environment for the program
        var module_env = try ModuleEnv.init(self.allocator, program_source);
        errdefer module_env.deinit();

        // Determine if we have definitions (which means we built a block expression)
        const has_definitions = self.definitions.count() > 0;
        
        // Parse appropriately based on whether we have definitions
        var parse_ast = if (has_definitions) 
            // Has definitions - we built a block expression, parse as expression
            parse.parseExpr(&module_env) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
            }
        else
            // No definitions - simple expression, parse as expression
            parse.parseExpr(&module_env) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
            };
        defer parse_ast.deinit(self.allocator);

        // Empty scratch space
        parse_ast.store.emptyScratch();

        // Create CIR
        const cir = &module_env; // CIR is now just ModuleEnv
        try cir.initCIRFields(self.allocator, "repl");

        // Create canonicalizer
        var can = Can.init(cir, &parse_ast, null) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize init error: {}", .{err});
        };
        defer can.deinit();

        // Since we're always parsing as expressions now, handle them the same way
        const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        
        const canonical_expr = try can.canonicalizeExpr(expr_idx) orelse {
            return try self.allocator.dupe(u8, "Canonicalize expr error: expression returned null");
        };
        const final_expr_idx = canonical_expr.get_idx();

        // Type check
        var checker = Check.init(self.allocator, &module_env.types, cir, &.{}, &cir.store.regions) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check init error: {}", .{err});
        };
        defer checker.deinit();

        // Check the expression (no need to check defs since we're parsing as expressions)
        _ = checker.checkExpr(final_expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check expr error: {}", .{err});
        };

        // Create layout cache
        var layout_cache = layout_store.Store.init(&module_env, &module_env.types) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Layout cache error: {}", .{err});
        };
        defer layout_cache.deinit();

        // Clear eval stack to prevent memory corruption from previous evaluations
        self.eval_stack.used = 0;

        // Create interpreter
        var interpreter = eval.Interpreter.init(self.allocator, cir, &self.eval_stack, &layout_cache, &module_env.types) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Interpreter init error: {}", .{err});
        };
        defer interpreter.deinit(self.roc_ops);

        // Evaluate the expression
        if (self.trace_writer) |trace_writer| {
            interpreter.startTrace(trace_writer);
        }

        const result = interpreter.eval(final_expr_idx, self.roc_ops) catch |err| {
            if (self.trace_writer) |_| {
                interpreter.endTrace();
            }
            return try std.fmt.allocPrint(self.allocator, "Evaluation error: {}", .{err});
        };

        if (self.trace_writer) |_| {
            interpreter.endTrace();
        }

        // Save ModuleEnv after successful evaluation (transfer ownership)
        try self.saveModuleEnv(&module_env);

        // Format the result immediately while memory is still valid
        if (result.layout.tag == .scalar) {
            switch (result.layout.data.scalar.tag) {
                .bool => {
                    // Boolean values are stored as u8 (1 for True, 0 for False)
                    const bool_value: *u8 = @ptrCast(result.ptr.?);
                    return try self.allocator.dupe(u8, if (bool_value.* == 1) "True" else "False");
                },
                .int => {
                    const value = result.asI128();
                    return try std.fmt.allocPrint(self.allocator, "{d}", .{value});
                },
                .frac => {
                    const precision = result.layout.data.scalar.data.frac;
                    switch (precision) {
                        .f32 => {
                            const float_ptr: *f32 = @ptrCast(@alignCast(result.ptr.?));
                            return try std.fmt.allocPrint(self.allocator, "{}", .{float_ptr.*});
                        },
                        .f64 => {
                            const float_ptr: *f64 = @ptrCast(@alignCast(result.ptr.?));
                            return try std.fmt.allocPrint(self.allocator, "{}", .{float_ptr.*});
                        },
                        .dec => {
                            const dec_ptr: *RocDec = @ptrCast(@alignCast(result.ptr.?));
                            // Simple conversion to f64 for now to avoid allocator issues
                            const raw_value = dec_ptr.num;
                            const scale_factor = std.math.pow(f64, 10, RocDec.decimal_places);
                            const decimal_value = @as(f64, @floatFromInt(raw_value)) / scale_factor;
                            return try std.fmt.allocPrint(self.allocator, "{}", .{decimal_value});
                        },
                    }
                },
                .str => {
                    // Handle RocStr values
                    const roc_str: *builtins.str.RocStr = @ptrCast(@alignCast(result.ptr.?));
                    const str_slice = roc_str.asSlice();
                    const formatted = try std.fmt.allocPrint(self.allocator, "\"{s}\"", .{str_slice});
                    // Clean up the RocStr to prevent memory leak
                    roc_str.decref(self.roc_ops);
                    return formatted;
                },
                else => {},
            }
        }

        // Handle empty list specially
        if (result.layout.tag == .list_of_zst) {
            return try self.allocator.dupe(u8, "<list_of_zst>");
        }

        return try std.fmt.allocPrint(self.allocator, "<{s}>", .{@tagName(result.layout.tag)});
    }
};

// Tests
const testing = std.testing;

test "Repl - initialization and cleanup" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops());
    defer repl.deinit();

    try testing.expect(repl.definitions.count() == 0);
}

test "Repl - special commands" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops());
    defer repl.deinit();

    const help_result = try repl.step(":help");
    defer std.testing.allocator.free(help_result);
    try testing.expect(std.mem.indexOf(u8, help_result, "Enter an expression") != null);

    const exit_result = try repl.step(":exit");
    defer std.testing.allocator.free(exit_result);
    try testing.expectEqualStrings("Goodbye!", exit_result);

    const empty_result = try repl.step("");
    defer std.testing.allocator.free(empty_result);
    try testing.expectEqualStrings("", empty_result);
}

test "Repl - simple expressions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops());
    defer repl.deinit();

    const result = try repl.step("42");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("42", result);
}

test "Repl - string expressions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops());
    defer repl.deinit();

    const result = try repl.step("\"Hello, World!\"");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("\"Hello, World!\"", result);
}

test "Repl - silent assignments" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops());
    defer repl.deinit();

    // Assignment should be silent (return empty string)
    const result1 = try repl.step("x = 5");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("", result1);

    // Expression should evaluate with context
    const result2 = try repl.step("x");
    defer std.testing.allocator.free(result2);
    try testing.expectEqualStrings("5", result2);
}

test "Repl - variable redefinition" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops());
    defer repl.deinit();

    // First definition
    const result1 = try repl.step("x = 5");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("", result1);

    // Define y in terms of x
    const result2 = try repl.step("y = x + 1");
    defer std.testing.allocator.free(result2);
    try testing.expectEqualStrings("", result2);

    // Evaluate y
    const result3 = try repl.step("y");
    defer std.testing.allocator.free(result3);
    try testing.expectEqualStrings("6", result3);

    // Redefine x
    const result4 = try repl.step("x = 3");
    defer std.testing.allocator.free(result4);
    try testing.expectEqualStrings("", result4);

    // Evaluate y again (should reflect new x value)
    const result5 = try repl.step("y");
    defer std.testing.allocator.free(result5);
    try testing.expectEqualStrings("4", result5);
}

test "Repl - build full source with block syntax" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops());
    defer repl.deinit();

    // Add definitions manually to test source building
    try repl.addOrReplaceDefinition("x = 5", "x");
    try repl.addOrReplaceDefinition("y = x + 1", "y");

    // Build full source for evaluating y
    const full_source = try repl.buildFullSource("y");
    defer std.testing.allocator.free(full_source);

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
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops());
    defer repl.deinit();

    // Manually add definitions to test replacement behavior
    try repl.addOrReplaceDefinition("x = 1", "x");
    try repl.addOrReplaceDefinition("x = 2", "x");
    try repl.addOrReplaceDefinition("x = 3", "x");

    // Verify only the latest definition is kept (replacement, not accumulation)
    try testing.expect(repl.definitions.count() == 1);

    // Build source shows the latest definition
    const full_source = try repl.buildFullSource("x");
    defer std.testing.allocator.free(full_source);

    const expected =
        \\{
        \\    x = 3
        \\    x
        \\}
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - minimal interpreter integration" {
    const gpa = std.testing.allocator;

    var test_env = TestEnv.init(gpa);
    defer test_env.deinit();

    // Step 1: Create module environment
    const source = "42";
    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    // Step 2: Parse as expression
    var parse_ast = try parse.parseExpr(&module_env);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Step 3: Create CIR
    const cir = &module_env; // CIR is now just ModuleEnv
    try cir.initCIRFields(gpa, "test");

    // Step 4: Canonicalize
    var can = try Can.init(cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Step 5: Type check
    var checker = try Check.init(gpa, &module_env.types, cir, &.{}, &cir.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(canonical_expr_idx.get_idx());

    // Step 6: Create evaluation stack
    var eval_stack = try stack.Stack.initCapacity(gpa, 1024);
    defer eval_stack.deinit();

    // Step 7: Create layout cache
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Step 8: Create interpreter
    var interpreter = try eval.Interpreter.init(gpa, cir, &eval_stack, &layout_cache, &module_env.types);
    defer interpreter.deinit(test_env.get_ops());

    // Step 9: Evaluate
    const result = try interpreter.eval(canonical_expr_idx.get_idx(), test_env.get_ops());

    // Step 10: Verify result
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);

    // Read the value back
    const value = result.asI128();

    try testing.expectEqual(@as(i128, 42), value);
}

