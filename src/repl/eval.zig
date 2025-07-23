//! The evaluation part of the Read-Eval-Print-Loop (REPL)

const std = @import("std");
const base = @import("base");
const compile = @import("compile");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const types = @import("types");
const types_store = types.store;
const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");
const eval = @import("../eval/interpreter.zig");
const stack = @import("../eval/stack.zig");

const writers = types.writers;
const Allocator = std.mem.Allocator;
const target = base.target;
const CIR = canonicalize.CIR;
const AST = parse.AST;

/// Type of definition stored in the REPL history
const DefKind = union(enum) {
    /// An assignment with an identifier
    assignment: []const u8,
    /// An import statement
    import,
};

/// Represents a past definition in the REPL session
const PastDef = struct {
    /// The source code of the definition
    source: []const u8,
    /// The kind of definition
    kind: DefKind,

    pub fn deinit(self: *PastDef, allocator: Allocator) void {
        allocator.free(self.source);
        switch (self.kind) {
            .assignment => |ident| allocator.free(ident),
            .import => {},
        }
    }
};

/// REPL state that tracks past definitions and evaluates expressions
pub const Repl = struct {
    allocator: Allocator,
    /// All past definitions in order (allows redefinition/shadowing)
    past_defs: std.ArrayList(PastDef),
    /// Stack for evaluation
    eval_stack: stack.Stack,

    pub fn init(allocator: Allocator) !Repl {
        const eval_stack = try stack.Stack.initCapacity(allocator, 8192);

        return Repl{
            .allocator = allocator,
            .past_defs = std.ArrayList(PastDef).init(allocator),
            .eval_stack = eval_stack,
        };
    }

    pub fn deinit(self: *Repl) void {
        for (self.past_defs.items) |*def| {
            def.deinit(self.allocator);
        }
        self.past_defs.deinit();
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
                defer self.allocator.free(info.ident);

                // Add to past definitions (allows redefinition)
                try self.past_defs.append(.{
                    .source = try self.allocator.dupe(u8, input),
                    .kind = .{ .assignment = try self.allocator.dupe(u8, info.ident) },
                });

                // For assignments, evaluate the RHS directly
                // Extract the RHS from the assignment
                if (std.mem.indexOf(u8, input, "=")) |eq_pos| {
                    const rhs = std.mem.trim(u8, input[eq_pos + 1 ..], " \t\n");

                    // If the RHS is a simple literal, evaluate it directly
                    if (std.fmt.parseInt(i64, rhs, 10)) |num| {
                        return try std.fmt.allocPrint(self.allocator, "{d}", .{num});
                    } else |_| {}

                    // Otherwise, evaluate with context
                    const full_source = try self.buildFullSource(rhs);
                    defer self.allocator.free(full_source);
                    return try self.evaluateSource(full_source);
                }

                return try self.allocator.dupe(u8, "");
            },
            .import => {
                // Add import to past definitions
                try self.past_defs.append(.{
                    .source = try self.allocator.dupe(u8, input),
                    .kind = .import,
                });

                return try self.allocator.dupe(u8, "");
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
        assignment: struct { ident: []const u8 }, // This ident must be allocator.dupe'd
        import,
        expression,
        type_decl,
        parse_error: []const u8, // This must be allocator.dupe'd
    };

    /// Try to parse input as a statement
    fn tryParseStatement(self: *Repl, input: []const u8) !ParseResult {
        var module_env = try compile.ModuleEnv.init(self.allocator, input);
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
                            const ident_tok = pattern.ident.ident_tok;
                            const token_region = ast.tokens.resolve(@intCast(ident_tok));
                            const ident = ast.env.source[token_region.start.offset..token_region.end.offset];
                            // Make a copy of the identifier since ast will be freed
                            const ident_copy = try self.allocator.dupe(u8, ident);
                            return ParseResult{ .assignment = .{ .ident = ident_copy } };
                        }
                        return ParseResult.expression;
                    },
                    .import => return ParseResult.import,
                    .type_decl => return ParseResult.type_decl,
                    else => return ParseResult.expression,
                }
            }
        } else |_| {}

        // Try expression parsing
        if (parse.parseExpr(&module_env)) |ast_const| {
            var ast = ast_const;
            defer ast.deinit(self.allocator);
            if (ast.root_node_idx != 0) {
                return ParseResult.expression;
            }
        } else |_| {}

        return ParseResult{ .parse_error = try self.allocator.dupe(u8, "Failed to parse input") };
    }

    /// Build full source including all past definitions
    fn buildFullSource(self: *Repl, current_expr: []const u8) ![]const u8 {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        // Add all past definitions in order (later ones shadow earlier ones)
        for (self.past_defs.items) |def| {
            try buffer.appendSlice(def.source);
            try buffer.append('\n');
        }

        // Add current expression
        try buffer.appendSlice(current_expr);

        return try buffer.toOwnedSlice();
    }

    /// Evaluate source code
    fn evaluateSource(self: *Repl, source: []const u8) ![]const u8 {
        return try self.evaluatePureExpression(source);
    }

    /// Evaluate a pure expression
    fn evaluatePureExpression(self: *Repl, expr_source: []const u8) ![]const u8 {
        // If we have past definitions and the expression might reference them,
        // we need context-aware evaluation (not yet implemented)
        if (self.past_defs.items.len > 0) {
            // Check if it's a simple literal that doesn't need context
            if (std.fmt.parseInt(i64, std.mem.trim(u8, expr_source, " \t\n"), 10)) |num| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{num});
            } else |_| {}

            // Context-aware evaluation not yet implemented
            return try std.fmt.allocPrint(self.allocator, "<needs context>", .{});
        }

        // Create module environment for the expression
        var module_env = try compile.ModuleEnv.init(self.allocator, expr_source);
        defer module_env.deinit();

        // Parse as expression
        var parse_ast = parse.parseExpr(&module_env) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
        };
        defer parse_ast.deinit(self.allocator);

        // Empty scratch space
        parse_ast.store.emptyScratch();

        try module_env.initCIRFields(self.allocator, "repl");

        // Create canonicalizer
        var can = canonicalize.init(&module_env, &parse_ast, null) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize init error: {}", .{err});
        };
        defer can.deinit();

        // Canonicalize the expression
        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        const canonical_expr_idx = can.canonicalizeExpr(expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize expr error: {}", .{err});
        } orelse {
            return try self.allocator.dupe(u8, "Failed to canonicalize expression");
        };

        // Type check
        var checker = check_types.init(self.allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check init error: {}", .{err});
        };
        defer checker.deinit();

        _ = checker.checkExpr(canonical_expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check error: {}", .{err});
        };

        // Create layout cache
        var layout_cache = layout_store.Store.init(&module_env, &module_env.types) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Layout cache error: {}", .{err});
        };
        defer layout_cache.deinit();

        // Create interpreter
        var interpreter = eval.Interpreter.init(self.allocator, &module_env, &self.eval_stack, &layout_cache, &module_env.types) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Interpreter init error: {}", .{err});
        };
        defer interpreter.deinit();

        // Evaluate the expression
        const result = interpreter.eval(canonical_expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Evaluation error: {}", .{err});
        };

        // Format the result
        if (result.layout.tag == .scalar) {
            switch (result.layout.data.scalar.tag) {
                .bool => {
                    // Boolean values are stored as u8 (1 for True, 0 for False)
                    const bool_value = @as(*u8, @ptrCast(@alignCast(result.ptr))).*;
                    return try self.allocator.dupe(u8, if (bool_value == 1) "True" else "False");
                },
                .int => {
                    const value: i128 = switch (result.layout.data.scalar.data.int) {
                        .u8 => @as(*u8, @ptrCast(@alignCast(result.ptr))).*,
                        .i8 => @as(*i8, @ptrCast(@alignCast(result.ptr))).*,
                        .u16 => @as(*u16, @ptrCast(@alignCast(result.ptr))).*,
                        .i16 => @as(*i16, @ptrCast(@alignCast(result.ptr))).*,
                        .u32 => @as(*u32, @ptrCast(@alignCast(result.ptr))).*,
                        .i32 => @as(*i32, @ptrCast(@alignCast(result.ptr))).*,
                        .u64 => @as(*u64, @ptrCast(@alignCast(result.ptr))).*,
                        .i64 => @as(*i64, @ptrCast(@alignCast(result.ptr))).*,
                        .u128 => @intCast(@as(*u128, @ptrCast(@alignCast(result.ptr))).*),
                        .i128 => @as(*i128, @ptrCast(@alignCast(result.ptr))).*,
                    };
                    return try std.fmt.allocPrint(self.allocator, "{d}", .{value});
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
    var repl = try Repl.init(testing.allocator);
    defer repl.deinit();

    try testing.expect(repl.past_defs.items.len == 0);
}

test "Repl - special commands" {
    var repl = try Repl.init(testing.allocator);
    defer repl.deinit();

    const help_result = try repl.step(":help");
    defer testing.allocator.free(help_result);
    try testing.expect(std.mem.indexOf(u8, help_result, "Enter an expression") != null);

    const exit_result = try repl.step(":exit");
    defer testing.allocator.free(exit_result);
    try testing.expectEqualStrings("Goodbye!", exit_result);

    const empty_result = try repl.step("");
    defer testing.allocator.free(empty_result);
    try testing.expectEqualStrings("", empty_result);
}

test "Repl - simple expressions" {
    var repl = try Repl.init(testing.allocator);
    defer repl.deinit();

    const result = try repl.step("42");
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("42", result);
}

test "Repl - redefinition with evaluation" {
    var repl = try Repl.init(testing.allocator);
    defer repl.deinit();

    // First definition of x
    const result1 = try repl.step("x = 5");
    defer testing.allocator.free(result1);
    try testing.expectEqualStrings("5", result1);

    // Define y in terms of x (returns <needs context> as context-aware evaluation is not yet implemented)
    const result2 = try repl.step("y = x + 1");
    defer testing.allocator.free(result2);
    try testing.expectEqualStrings("<needs context>", result2);

    // Redefine x
    const result3 = try repl.step("x = 6");
    defer testing.allocator.free(result3);
    try testing.expectEqualStrings("6", result3);

    // Evaluate x (returns <needs context> as context-aware evaluation is not yet implemented)
    const result4 = try repl.step("x");
    defer testing.allocator.free(result4);
    try testing.expectEqualStrings("<needs context>", result4);

    // Evaluate y (returns <needs context> as context-aware evaluation is not yet implemented)
    const result5 = try repl.step("y");
    defer testing.allocator.free(result5);
    try testing.expectEqualStrings("<needs context>", result5);
}

test "Repl - build full source with redefinitions" {
    var repl = try Repl.init(testing.allocator);
    defer repl.deinit();

    // Add definitions manually to test source building
    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 5"),
        .kind = .{ .assignment = try testing.allocator.dupe(u8, "x") },
    });

    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "y = x + 1"),
        .kind = .{ .assignment = try testing.allocator.dupe(u8, "y") },
    });

    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 6"),
        .kind = .{ .assignment = try testing.allocator.dupe(u8, "x") },
    });

    // Build full source for evaluating y
    const full_source = try repl.buildFullSource("y");
    defer testing.allocator.free(full_source);

    const expected =
        \\x = 5
        \\y = x + 1
        \\x = 6
        \\y
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - past def ordering" {
    var repl = try Repl.init(testing.allocator);
    defer repl.deinit();

    // Manually add definitions to test ordering
    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 1"),
        .kind = .{ .assignment = try testing.allocator.dupe(u8, "x") },
    });

    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 2"),
        .kind = .{ .assignment = try testing.allocator.dupe(u8, "x") },
    });

    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 3"),
        .kind = .{ .assignment = try testing.allocator.dupe(u8, "x") },
    });

    // Verify all definitions are kept in order
    try testing.expect(repl.past_defs.items.len == 3);
    try testing.expectEqualStrings("x = 1", repl.past_defs.items[0].source);
    try testing.expectEqualStrings("x = 2", repl.past_defs.items[1].source);
    try testing.expectEqualStrings("x = 3", repl.past_defs.items[2].source);

    // Build source shows all definitions
    const full_source = try repl.buildFullSource("x");
    defer testing.allocator.free(full_source);

    const expected =
        \\x = 1
        \\x = 2
        \\x = 3
        \\x
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - minimal interpreter integration" {
    const allocator = testing.allocator;

    // Step 1: Create module environment
    const source = "42";
    var module_env = try compile.ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    // Step 2: Parse as expression
    var parse_ast = try parse.parseExpr(&module_env);
    defer parse_ast.deinit(allocator);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Step 3: Initialize CIR fields in the existing module_env
    try module_env.initCIRFields(allocator, "test");

    // Step 4: Canonicalize
    var can = try canonicalize.init(&module_env, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Step 5: Type check
    var checker = try check_types.init(allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(canonical_expr_idx);

    // Step 6: Create evaluation stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Step 7: Create layout cache
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Step 8: Create interpreter
    var interpreter = try eval.Interpreter.init(allocator, &module_env, &eval_stack, &layout_cache, &module_env.types);
    defer interpreter.deinit();

    // Step 9: Evaluate
    const result = try interpreter.eval(canonical_expr_idx);

    // Step 10: Verify result
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);

    // Read the value back
    const value: i128 = switch (result.layout.data.scalar.data.int) {
        .u8 => @as(*u8, @ptrCast(@alignCast(result.ptr))).*,
        .i8 => @as(*i8, @ptrCast(@alignCast(result.ptr))).*,
        .u16 => @as(*u16, @ptrCast(@alignCast(result.ptr))).*,
        .i16 => @as(*i16, @ptrCast(@alignCast(result.ptr))).*,
        .u32 => @as(*u32, @ptrCast(@alignCast(result.ptr))).*,
        .i32 => @as(*i32, @ptrCast(@alignCast(result.ptr))).*,
        .u64 => @as(*u64, @ptrCast(@alignCast(result.ptr))).*,
        .i64 => @as(*i64, @ptrCast(@alignCast(result.ptr))).*,
        .u128 => @intCast(@as(*u128, @ptrCast(@alignCast(result.ptr))).*),
        .i128 => @as(*i128, @ptrCast(@alignCast(result.ptr))).*,
    };

    try testing.expectEqual(@as(i128, 42), value);
}
