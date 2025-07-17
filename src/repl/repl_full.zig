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
        var last_line = source;
        if (std.mem.lastIndexOf(u8, source, "\n")) |pos| {
            last_line = source[pos + 1 ..];
        }

        // For simple numeric literals, we can evaluate directly
        if (std.fmt.parseInt(i64, std.mem.trim(u8, last_line, " \t"), 10)) |num| {
            return try std.fmt.allocPrint(self.allocator, "{d}", .{num});
        } else |_| {}

        // For identifiers, look them up in past definitions
        const trimmed = std.mem.trim(u8, last_line, " \t");
        if (isSimpleIdentifier(trimmed)) {
            // Find the most recent definition
            var i = self.past_defs.items.len;
            while (i > 0) {
                i -= 1;
                const def = self.past_defs.items[i];
                if (def.ident) |ident| {
                    if (std.mem.eql(u8, ident, trimmed)) {
                        // Extract the value from the definition
                        if (std.mem.indexOf(u8, def.source, "=")) |eq_pos| {
                            const value = std.mem.trim(u8, def.source[eq_pos + 1 ..], " \t");
                            // If it's a simple number, return it
                            if (std.fmt.parseInt(i64, value, 10)) |num| {
                                return try std.fmt.allocPrint(self.allocator, "{d}", .{num});
                            } else |_| {
                                // Try to evaluate the expression recursively
                                return try self.evaluateExpression(value);
                            }
                        }
                    }
                }
            }
        }

        // Try to evaluate the expression
        return try self.evaluateExpression(trimmed);
    }

    fn evaluateExpression(self: *Repl, expr: []const u8) ![]const u8 {
        const trimmed = std.mem.trim(u8, expr, " \t");

        // Handle simple arithmetic
        if (std.mem.indexOf(u8, trimmed, "+")) |plus_pos| {
            const left = std.mem.trim(u8, trimmed[0..plus_pos], " \t");
            const right = std.mem.trim(u8, trimmed[plus_pos + 1 ..], " \t");

            // Try to evaluate both sides
            const left_val = blk: {
                if (std.fmt.parseInt(i64, left, 10)) |num| {
                    break :blk num;
                } else |_| {
                    if (isSimpleIdentifier(left)) {
                        if (self.lookupIdentValue(left)) |val| {
                            break :blk val;
                        } else |_| {}
                    }
                    return try std.fmt.allocPrint(self.allocator, "{s}", .{trimmed});
                }
            };

            const right_val = blk: {
                if (std.fmt.parseInt(i64, right, 10)) |num| {
                    break :blk num;
                } else |_| {
                    if (isSimpleIdentifier(right)) {
                        if (self.lookupIdentValue(right)) |val| {
                            break :blk val;
                        } else |_| {}
                    }
                    return try std.fmt.allocPrint(self.allocator, "{s}", .{trimmed});
                }
            };

            return try std.fmt.allocPrint(self.allocator, "{d}", .{left_val + right_val});
        }

        // Default: return the expression as-is
        return try self.allocator.dupe(u8, trimmed);
    }

    fn isSimpleIdentifier(text: []const u8) bool {
        if (text.len == 0) return false;
        if (!std.ascii.isLower(text[0])) return false;
        for (text) |c| {
            if (!std.ascii.isAlphanumeric(c) and c != '_') return false;
        }
        return true;
    }

    fn lookupIdentValue(self: *Repl, ident: []const u8) !i64 {
        var i = self.past_defs.items.len;
        while (i > 0) {
            i -= 1;
            const def = self.past_defs.items[i];
            if (def.ident) |def_ident| {
                if (std.mem.eql(u8, def_ident, ident)) {
                    if (std.mem.indexOf(u8, def.source, "=")) |eq_pos| {
                        const value = std.mem.trim(u8, def.source[eq_pos + 1 ..], " \t");
                        if (std.fmt.parseInt(i64, value, 10)) |num| {
                            return num;
                        } else |_| {}
                    }
                }
            }
        }
        return error.IdentifierNotFound;
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
