//! REPL implementation that uses real parsing and evaluation

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const types = @import("../types.zig");
const types_store = @import("../types/store.zig");
const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");
const eval = @import("../eval/interpreter.zig");
const stack = @import("../eval/stack.zig");
const CIR = canonicalize.CIR;
const target = @import("../base/target.zig");
const writers = @import("../types/writers.zig");
const AST = parse.AST;

/// Represents a past definition in the REPL session
const PastDef = struct {
    /// The source code of the definition
    source: []const u8,
    /// The identifier name if it's an assignment
    ident: ?[]const u8,
    /// Whether this is an import statement
    is_import: bool,

    pub fn deinit(self: *PastDef, allocator: Allocator) void {
        allocator.free(self.source);
        if (self.ident) |ident| {
            allocator.free(ident);
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
                    .ident = try self.allocator.dupe(u8, info.ident),
                    .is_import = false,
                });

                // Evaluate the identifier to show its value
                const full_source = try self.buildFullSource(info.ident);
                defer self.allocator.free(full_source);

                return try self.evaluateSource(full_source);
            },
            .import => {
                // Add import to past definitions
                try self.past_defs.append(.{
                    .source = try self.allocator.dupe(u8, input),
                    .ident = null,
                    .is_import = true,
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
        const source = try self.allocator.dupe(u8, input);

        var module_env = try base.ModuleEnv.init(self.allocator, source);
        defer module_env.deinit();

        // Try statement parsing
        if (parse.parseStatement(&module_env, source)) |ast_const| {
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
        if (parse.parseExpr(&module_env, source)) |ast_const| {
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
        // Extract the last line as the expression to evaluate
        var lines = std.mem.tokenizeScalar(u8, source, '\n');
        var last_line: ?[]const u8 = null;
        while (lines.next()) |line| {
            last_line = line;
        }

        if (last_line == null or last_line.?.len == 0) {
            return try self.allocator.dupe(u8, "");
        }

        // Check if the last line is an assignment
        var expr_source = last_line.?;
        if (std.mem.indexOf(u8, last_line.?, "=")) |eq_pos| {
            // Make sure it's not == or other operators
            if (eq_pos > 0 and last_line.?[eq_pos - 1] != '=' and
                eq_pos + 1 < last_line.?.len and last_line.?[eq_pos + 1] != '=')
            {
                // Extract just the value part after =
                expr_source = std.mem.trim(u8, last_line.?[eq_pos + 1 ..], " \t");
            }
        }

        // For simple numeric literals, we can evaluate directly
        if (std.fmt.parseInt(i64, std.mem.trim(u8, expr_source, " \t"), 10)) |num| {
            return try std.fmt.allocPrint(self.allocator, "{d}", .{num});
        } else |_| {}

        // For now, if we can't evaluate it directly, just return the expression
        // TODO: Implement full interpreter integration with context from past definitions
        return try self.allocator.dupe(u8, expr_source);
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

    // Define y in terms of x
    const result2 = try repl.step("y = x + 1");
    defer testing.allocator.free(result2);
    try testing.expectEqualStrings("6", result2);

    // Redefine x
    const result3 = try repl.step("x = 6");
    defer testing.allocator.free(result3);
    try testing.expectEqualStrings("6", result3);

    // Evaluate x - should get new value
    const result4 = try repl.step("x");
    defer testing.allocator.free(result4);
    try testing.expectEqualStrings("6", result4);

    // Evaluate y - should be 7 (uses current x which is 6)
    const result5 = try repl.step("y");
    defer testing.allocator.free(result5);
    try testing.expectEqualStrings("7", result5);
}

test "Repl - build full source with redefinitions" {
    var repl = try Repl.init(testing.allocator);
    defer repl.deinit();

    // Add definitions manually to test source building
    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 5"),
        .ident = try testing.allocator.dupe(u8, "x"),
        .is_import = false,
    });

    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "y = x + 1"),
        .ident = try testing.allocator.dupe(u8, "y"),
        .is_import = false,
    });

    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 6"),
        .ident = try testing.allocator.dupe(u8, "x"),
        .is_import = false,
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
        .ident = try testing.allocator.dupe(u8, "x"),
        .is_import = false,
    });

    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 2"),
        .ident = try testing.allocator.dupe(u8, "x"),
        .is_import = false,
    });

    try repl.past_defs.append(.{
        .source = try testing.allocator.dupe(u8, "x = 3"),
        .ident = try testing.allocator.dupe(u8, "x"),
        .is_import = false,
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
    var module_env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer module_env.deinit();

    // Step 2: Parse as expression
    var parse_ast = try parse.parseExpr(&module_env, source);
    defer parse_ast.deinit(allocator);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Step 3: Create CIR
    var cir = try CIR.init(&module_env, "test");
    defer cir.deinit();

    // Step 4: Canonicalize
    var can = try canonicalize.init(&cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Step 5: Type check
    var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(canonical_expr_idx);

    // Step 6: Create evaluation stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Step 7: Create layout cache
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Step 8: Create interpreter
    var interpreter = try eval.Interpreter.init(allocator, &cir, &eval_stack, &layout_cache, &module_env.types);
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
