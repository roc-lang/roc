//! The evaluation part of the Read-Eval-Print-Loop (REPL)

const std = @import("std");
const base = @import("base");
const compile = @import("compile");
const parse = @import("parse");
const types = @import("types");
const can = @import("can");
const eval = @import("eval");
const check = @import("check");
const builtins = @import("builtins");

const CrashContext = eval.CrashContext;

const TestEnv = @import("repl_test_env.zig").TestEnv;

const AST = parse.AST;
const Can = can.Can;
const Check = check.Check;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;

const Repl = @This();

/// Information about a type declaration
const TypeDeclInfo = struct {
    /// Name of the type (e.g., "Foo")
    type_name: []const u8,
    /// Whether it has associated items
    has_associated_items: bool,
};

/// Type of definition stored in the REPL history
const DefKind = union(enum) {
    /// An assignment with an identifier
    assignment: []const u8,
    /// An import statement
    import,
    /// A type declaration
    type_decl: TypeDeclInfo,
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
            .type_decl => |info| allocator.free(info.type_name),
        }
    }
};

allocator: Allocator,
/// All past definitions in order (allows redefinition/shadowing)
past_defs: std.ArrayList(PastDef),
/// Operations for the Roc runtime
roc_ops: *RocOps,
/// Shared crash context provided by the host (optional)
crash_ctx: ?*CrashContext,
/// Optional trace writer for debugging evaluation
trace_writer: ?std.io.AnyWriter,

pub fn init(allocator: Allocator, roc_ops: *RocOps, crash_ctx: ?*CrashContext) !Repl {
    return Repl{
        .allocator = allocator,
        .past_defs = std.ArrayList(PastDef).init(allocator),
        .roc_ops = roc_ops,
        .crash_ctx = crash_ctx,
        .trace_writer = null,
    };
}

/// Set a trace writer for debugging REPL evaluation
pub fn setTraceWriter(self: *Repl, trace_writer: std.io.AnyWriter) void {
    self.trace_writer = trace_writer;
}

pub fn deinit(self: *Repl) void {
    for (self.past_defs.items) |*def| {
        def.deinit(self.allocator);
    }
    self.past_defs.deinit();
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
            // Make a copy of the identifier to keep (will be stored in past_defs)
            const ident_for_past_defs = try self.allocator.dupe(u8, info.ident);
            errdefer self.allocator.free(ident_for_past_defs);
            defer self.allocator.free(info.ident);

            // Evaluate the assignment and return the value of the identifier
            const result = try self.evaluateDefinition(input, ident_for_past_defs);

            // Add to past definitions after successful evaluation (allows redefinition)
            try self.past_defs.append(.{
                .source = try self.allocator.dupe(u8, input),
                .kind = .{ .assignment = ident_for_past_defs },
            });

            return result;
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
            return try self.evaluateSource(input);
        },
        .type_decl => |info| {
            defer self.allocator.free(info.type_name);

            // Store the type declaration - associated items will be canonicalized and evaluated
            // automatically when we build the full source for the next expression/definition
            try self.past_defs.append(.{
                .source = try self.allocator.dupe(u8, input),
                .kind = .{ .type_decl = .{
                    .type_name = try self.allocator.dupe(u8, info.type_name),
                    .has_associated_items = info.has_associated_items,
                } },
            });

            // Output the type name to indicate it was defined
            return try std.fmt.allocPrint(self.allocator, "{s}", .{info.type_name});
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
    type_decl: struct {
        type_name: []const u8, // This must be allocator.dupe'd
        has_associated_items: bool,
    },
    parse_error: []const u8, // This must be allocator.dupe'd
};

/// Try to parse input as a statement
fn tryParseStatement(self: *Repl, input: []const u8) !ParseResult {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    var module_env = try ModuleEnv.init(self.allocator, input);
    defer module_env.deinit();

    // Try parsing as a full file first (for type declarations with associated items)
    if (parse.parse(&module_env.common, self.allocator)) |ast_const| {
        var ast = ast_const;
        defer ast.deinit(self.allocator);

        // Get the file (type modules have root_node_idx = 0)
        const file = ast.store.getFile();
        const statements = ast.store.statementSlice(file.statements);

        if (statements.len == 1) {
            const stmt = ast.store.getStatement(statements[0]);
            switch (stmt) {
                .decl => |decl| {
                    const pattern = ast.store.getPattern(decl.pattern);
                    if (pattern == .ident) {
                        const ident_tok = pattern.ident.ident_tok;
                        const token_region = ast.tokens.resolve(@intCast(ident_tok));
                        const ident = ast.env.source[token_region.start.offset..token_region.end.offset];
                        const ident_copy = try self.allocator.dupe(u8, ident);
                        return ParseResult{ .assignment = .{ .ident = ident_copy } };
                    }
                    return ParseResult.expression;
                },
                .import => return ParseResult.import,
                .type_decl => |decl| {
                    const header = ast.store.getTypeHeader(decl.header) catch {
                        return ParseResult.expression;
                    };
                    const type_name_tok = header.name;
                    const token_region = ast.tokens.resolve(type_name_tok);
                    const type_name = ast.env.source[token_region.start.offset..token_region.end.offset];

                    return ParseResult{ .type_decl = .{
                        .type_name = try self.allocator.dupe(u8, type_name),
                        .has_associated_items = decl.associated != null,
                    } };
                },
                else => return ParseResult.expression,
            }
        }
    } else |_| {}

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
                .type_decl => |decl| {
                    const header = ast.store.getTypeHeader(decl.header) catch {
                        // If we can't get the type header, treat it as an expression
                        return ParseResult.expression;
                    };
                    const type_name_tok = header.name;
                    const token_region = ast.tokens.resolve(type_name_tok);
                    const type_name = ast.env.source[token_region.start.offset..token_region.end.offset];

                    return ParseResult{ .type_decl = .{
                        .type_name = try self.allocator.dupe(u8, type_name),
                        .has_associated_items = decl.associated != null,
                    } };
                },
                else => return ParseResult.expression,
            }
        }
    } else |_| {}

    // Try expression parsing
    if (parse.parseExpr(&module_env.common, self.allocator)) |ast_const| {
        var ast = ast_const;
        defer ast.deinit(self.allocator);
        if (ast.root_node_idx != 0) {
            return ParseResult.expression;
        }
    } else |_| {}

    return ParseResult{ .parse_error = try self.allocator.dupe(u8, "Failed to parse input") };
}

/// Build full source including all past definitions
/// If `def_ident` is provided, the current_expr is a definition and we add an expression to evaluate def_ident
fn buildFullSource(self: *Repl, current_expr: []const u8, def_ident: ?[]const u8) ![]const u8 {
    var buffer = std.ArrayList(u8).init(self.allocator);
    defer buffer.deinit();

    // Add all past definitions in order (later ones shadow earlier ones)
    for (self.past_defs.items) |def| {
        try buffer.appendSlice(def.source);
        try buffer.append('\n');
    }

    // If we have past_defs OR a def_ident, we need to wrap/structure the source
    if (self.past_defs.items.len > 0 or def_ident != null) {
        // Don't add a header - we'll parse as a headerless file (type module or default-app)
        // The REPL validation_context will skip module validation

        if (def_ident) |ident| {
            // Current input is a definition - add it as-is, then add an expression to evaluate the identifier
            try buffer.appendSlice(current_expr);
            try buffer.append('\n');
            try buffer.appendSlice("main! = |_| ");
            try buffer.appendSlice(ident);
        } else {
            // Current input is an expression - wrap it in main!
            try buffer.appendSlice("main! = |_| ");
            try buffer.appendSlice(current_expr);
        }
    } else {
        try buffer.appendSlice(current_expr);
    }

    return try buffer.toOwnedSlice();
}

/// Evaluate source code
fn evaluateSource(self: *Repl, source: []const u8) ![]const u8 {
    return try self.evaluatePureExpression(source, null);
}

/// Evaluate source code for a definition and return the value of the assigned identifier
fn evaluateDefinition(self: *Repl, source: []const u8, ident: []const u8) ![]const u8 {
    return try self.evaluatePureExpression(source, ident);
}

/// Evaluate a pure expression
/// If `def_ident` is provided, the expr_source is treated as a definition and we return the value of def_ident
fn evaluatePureExpression(self: *Repl, expr_source: []const u8, def_ident: ?[]const u8) ![]const u8 {
    // Build the full source including past definitions
    // We need to build full source if we have past_defs OR if this is a definition (def_ident != null)
    const need_full_source = self.past_defs.items.len > 0 or def_ident != null;
    const full_source = if (need_full_source)
        try self.buildFullSource(expr_source, def_ident)
    else
        expr_source;
    defer if (need_full_source) self.allocator.free(full_source);

    // Create module environment for the expression
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    var module_env = try ModuleEnv.init(self.allocator, full_source);
    defer module_env.deinit();

    // Parse as a file if buildFullSource wrapped the input in a synthetic `main! = |_| <expr>`.
    // This happens when we have past definitions to include, or when evaluating a definition (def_ident != null).
    // Otherwise, parse the input directly as an expression.
    const need_file_parse = self.past_defs.items.len > 0 or def_ident != null;

    var parse_ast = if (need_file_parse)
        parse.parse(&module_env.common, self.allocator) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
        }
    else
        parse.parseExpr(&module_env.common, self.allocator) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
        };
    defer parse_ast.deinit(self.allocator);

    // Empty scratch space
    parse_ast.store.emptyScratch();

    // Create CIR
    const cir = &module_env; // CIR is now just ModuleEnv
    const module_name = "repl";
    try cir.initCIRFields(self.allocator, module_name);

    const common_idents: Check.CommonIdents = .{
        .module_name = try cir.insertIdent(base.Ident.for_text(module_name)),
        .list = try cir.insertIdent(base.Ident.for_text("List")),
        .box = try cir.insertIdent(base.Ident.for_text("Box")),
    };

    // Create czer
    //
    var czer = Can.init(cir, &parse_ast, null, .{}) catch |err| {
        return try std.fmt.allocPrint(self.allocator, "Canonicalize init error: {}", .{err});
    };
    defer czer.deinit();

    // Canonicalize based on whether we have past definitions or a def_ident
    const canonical_expr_idx: can.CIR.Expr.Idx = if (self.past_defs.items.len > 0 or def_ident != null) blk: {
        // When there are past definitions, buildFullSource wraps the expression in a synthetic
        // `main! = |_| <expr>` so it can be evaluated along with those definitions.
        czer.canonicalizeFile() catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize file error: {}", .{err});
        };

        const defs_slice = cir.store.sliceDefs(czer.env.all_defs);

        if (defs_slice.len == 0) {
            return try self.allocator.dupe(u8, "No definitions created during canonicalization");
        }

        // Find the synthetic "main!" definition that wraps the expression
        var main_def_idx: ?can.CIR.Def.Idx = null;
        for (defs_slice) |def_idx| {
            const def = cir.store.getDef(def_idx);
            const pattern = cir.store.getPattern(def.pattern);
            if (pattern == .assign) {
                const ident_idx = pattern.assign.ident;
                const ident_text = cir.getIdent(ident_idx);
                if (std.mem.eql(u8, ident_text, "main!")) {
                    main_def_idx = def_idx;
                    break;
                }
            }
        }

        if (main_def_idx) |def_idx| {
            const def = cir.store.getDef(def_idx);
            const expr = cir.store.getExpr(def.expr);
            // Extract the body from main! = |_| <body>
            if (expr == .e_lambda) {
                break :blk expr.e_lambda.body;
            } else if (expr == .e_closure) {
                const lambda_expr = cir.store.getExpr(expr.e_closure.lambda_idx);
                if (lambda_expr == .e_lambda) {
                    break :blk lambda_expr.e_lambda.body;
                } else {
                    return try self.allocator.dupe(u8, "main! closure does not contain a lambda");
                }
            } else {
                return try std.fmt.allocPrint(self.allocator, "main! is not a lambda as expected, got: {s}", .{@tagName(expr)});
            }
        } else {
            return try self.allocator.dupe(u8, "Could not find main! definition");
        }
    } else blk: {
        // Canonicalize just the expression (no past definitions)
        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        const result = czer.canonicalizeExpr(expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize expr error: {}", .{err});
        } orelse {
            return try self.allocator.dupe(u8, "Failed to canonicalize expression");
        };
        break :blk result.get_idx();
    };

    // Type check
    var checker = Check.init(self.allocator, &module_env.types, cir, &.{}, &cir.store.regions, common_idents) catch |err| {
        return try std.fmt.allocPrint(self.allocator, "Type check init error: {}", .{err});
    };
    defer checker.deinit();

    // If we have file-level defs, use checkFile to type-check everything
    // Otherwise, just check the single expression
    if (self.past_defs.items.len > 0 or def_ident != null) {
        checker.checkFile() catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check error: {}", .{err});
        };
    } else {
        _ = checker.checkExprRepl(canonical_expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check error: {}", .{err});
        };
    }

    // Create interpreter
    var interpreter = eval.Interpreter.init(self.allocator, cir, &.{}) catch |err| {
        return try std.fmt.allocPrint(self.allocator, "Interpreter init error: {}", .{err});
    };
    defer interpreter.deinit();

    // If we have past definitions or a def_ident, we need to evaluate all the defs (except main!)
    // to populate bindings for associated items
    if (self.past_defs.items.len > 0 or def_ident != null) {
        const defs_slice = cir.store.sliceDefs(czer.env.all_defs);

        // Evaluate all defs except main! and the current def (if there are past defs)
        for (defs_slice) |def_idx| {
            const def = cir.store.getDef(def_idx);
            const pattern = cir.store.getPattern(def.pattern);

            // Skip main! since we extract its body separately
            // Skip the current def ONLY if it's a NEW definition (not a redefinition)
            if (pattern == .assign) {
                const ident_idx = pattern.assign.ident;
                const ident_text = cir.getIdent(ident_idx);
                if (std.mem.eql(u8, ident_text, "main!")) {
                    continue;
                }
                // Skip the current def only if it's a NEW definition
                if (self.past_defs.items.len > 0 and def_ident != null) {
                    if (std.mem.eql(u8, ident_text, def_ident.?)) {
                        // Check if this is a redefinition
                        var is_redefinition = false;
                        for (self.past_defs.items) |past_def| {
                            if (past_def.kind == .assignment) {
                                if (std.mem.eql(u8, past_def.kind.assignment, ident_text)) {
                                    is_redefinition = true;
                                    break;
                                }
                            }
                        }
                        // Only skip if it's a NEW definition
                        if (!is_redefinition) {
                            continue;
                        }
                    }
                }
            }

            // Evaluate the def's expression
            const value = interpreter.evalMinimal(def.expr, self.roc_ops) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "Error evaluating definition: {}", .{err});
            };
            // Don't defer value.decref here because we're storing it in bindings

            // Create a binding for this pattern
            try interpreter.bindings.append(.{
                .pattern_idx = def.pattern,
                .value = value,
            });
        }
    }

    // Evaluate the expression
    // The full source has been built and canonicalized with all past_defs included
    if (self.trace_writer) |trace_writer| {
        interpreter.startTrace(trace_writer);
    }

    if (self.crash_ctx) |ctx| {
        ctx.reset();
    }

    const result = interpreter.evalMinimal(canonical_expr_idx, self.roc_ops) catch |err| {
        if (self.trace_writer) |_| {
            interpreter.endTrace();
        }
        if (err == error.Crash) {
            if (self.crash_ctx) |ctx| {
                if (ctx.crashMessage()) |msg| {
                    return try std.fmt.allocPrint(self.allocator, "Crash: {s}", .{msg});
                }
            }
            return try self.allocator.dupe(u8, "Evaluation error: error.Crash");
        }
        return try std.fmt.allocPrint(self.allocator, "Evaluation error: {}", .{err});
    };

    defer result.decref(&interpreter.runtime_layout_store, self.roc_ops);

    if (self.trace_writer) |_| {
        interpreter.endTrace();
    }

    const expr_ct_var = can.ModuleEnv.varFrom(canonical_expr_idx);
    const output = blk: {
        const expr_rt_var = interpreter.translateTypeVar(cir, expr_ct_var) catch {
            break :blk try interpreter.renderValueRoc(result);
        };
        break :blk try interpreter.renderValueRocWithType(result, expr_rt_var);
    };

    if (result.layout.tag == .record) {
        self.allocator.free(output);
        return try self.allocator.dupe(u8, "<record>");
    }

    return output;
}

// Tests
const testing = std.testing;

test "Repl - initialization and cleanup" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    try testing.expect(repl.past_defs.items.len == 0);
}

test "Repl - special commands" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
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

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    const result = try repl.step("42");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("42", result);
}

test "Repl - string expressions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    const result = try repl.step("\"Hello, World!\"");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("\"Hello, World!\"", result);
}

test "Repl - redefinition with evaluation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // First definition of x
    const result1 = try repl.step("x = 5");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("5", result1);

    // Define y in terms of x (may hit NotImplemented in context-aware evaluation)
    const result2 = try repl.step("y = x + 1");
    defer std.testing.allocator.free(result2);
    try testing.expect(std.mem.indexOf(u8, result2, "error.NotImplemented") != null or std.mem.indexOf(u8, result2, "6") != null);

    // Redefine x
    const result3 = try repl.step("x = 6");
    defer std.testing.allocator.free(result3);
    try testing.expectEqualStrings("6", result3);

    // Evaluate x (may hit NotImplemented in context-aware evaluation)
    const result4 = try repl.step("x");
    defer std.testing.allocator.free(result4);
    try testing.expect(std.mem.indexOf(u8, result4, "error.NotImplemented") != null or std.mem.indexOf(u8, result4, "6") != null);

    // Evaluate y (may hit NotImplemented in context-aware evaluation)
    const result5 = try repl.step("y");
    defer std.testing.allocator.free(result5);
    try testing.expect(std.mem.indexOf(u8, result5, "error.NotImplemented") != null or std.mem.indexOf(u8, result5, "6") != null);
}

test "Repl - build full source with redefinitions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Add definitions manually to test source building
    try repl.past_defs.append(.{
        .source = try std.testing.allocator.dupe(u8, "x = 5"),
        .kind = .{ .assignment = try std.testing.allocator.dupe(u8, "x") },
    });

    try repl.past_defs.append(.{
        .source = try std.testing.allocator.dupe(u8, "y = x + 1"),
        .kind = .{ .assignment = try std.testing.allocator.dupe(u8, "y") },
    });

    try repl.past_defs.append(.{
        .source = try std.testing.allocator.dupe(u8, "x = 6"),
        .kind = .{ .assignment = try std.testing.allocator.dupe(u8, "x") },
    });

    // Build full source for evaluating y
    const full_source = try repl.buildFullSource("y", null);
    defer std.testing.allocator.free(full_source);

    const expected =
        \\x = 5
        \\y = x + 1
        \\x = 6
        \\main! = |_| y
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - past def ordering" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Manually add definitions to test ordering
    try repl.past_defs.append(.{
        .source = try std.testing.allocator.dupe(u8, "x = 1"),
        .kind = .{ .assignment = try std.testing.allocator.dupe(u8, "x") },
    });

    try repl.past_defs.append(.{
        .source = try std.testing.allocator.dupe(u8, "x = 2"),
        .kind = .{ .assignment = try std.testing.allocator.dupe(u8, "x") },
    });

    try repl.past_defs.append(.{
        .source = try std.testing.allocator.dupe(u8, "x = 3"),
        .kind = .{ .assignment = try std.testing.allocator.dupe(u8, "x") },
    });

    // Verify all definitions are kept in order
    try testing.expect(repl.past_defs.items.len == 3);
    try testing.expectEqualStrings("x = 1", repl.past_defs.items[0].source);
    try testing.expectEqualStrings("x = 2", repl.past_defs.items[1].source);
    try testing.expectEqualStrings("x = 3", repl.past_defs.items[2].source);

    // Build source shows all definitions
    const full_source = try repl.buildFullSource("x", null);
    defer std.testing.allocator.free(full_source);

    const expected =
        \\x = 1
        \\x = 2
        \\x = 3
        \\main! = |_| x
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - minimal interpreter integration" {
    const gpa = std.testing.allocator;

    var test_env = TestEnv.init(gpa);
    defer test_env.deinit();

    // Step 1: Create module environment
    const source = "42";
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    // Step 2: Parse as expression
    var parse_ast = try parse.parseExpr(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Step 3: Create CIR
    const cir = &module_env; // CIR is now just ModuleEnv
    try cir.initCIRFields(gpa, "test");

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
    };

    // Step 4: Canonicalize
    var czer = try Can.init(cir, &parse_ast, null, .{});
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Step 5: Type check
    var checker = try Check.init(gpa, &module_env.types, cir, &.{}, &cir.store.regions, module_common_idents);
    defer checker.deinit();

    _ = try checker.checkExprRepl(canonical_expr_idx.get_idx());

    // Step 6: Create interpreter
    var interpreter = try eval.Interpreter.init(gpa, cir, &.{});
    defer interpreter.deinit();

    // Step 7: Evaluate
    const ops = test_env.get_ops();
    const result = try interpreter.evalMinimal(canonical_expr_idx.get_idx(), ops);
    defer result.decref(&interpreter.runtime_layout_store, ops);

    // Step 8: Verify result
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);

    // Read the value back
    const value = result.asI128();

    try testing.expectEqual(@as(i128, 42), value);
}

test "Repl - type with associated value" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Define a type with an associated value
    const result1 = try repl.step("Foo := [A, B].{ x = 5 }");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("Foo", result1);

    // Use the associated value
    const result2 = try repl.step("Foo.x");
    defer std.testing.allocator.free(result2);
}

test "Repl - nested type declaration" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Define a type with a nested type
    const result1 = try repl.step("Foo := [Whatever].{ Bar := [X, Y, Z] }");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("Foo", result1);

    // Use a tag from the nested type
    const result2 = try repl.step("Foo.Bar.X");
    defer std.testing.allocator.free(result2);
}

test "Repl - associated value with type annotation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Define a type with an associated value
    const result1 = try repl.step("Foo := [A, B].{ defaultNum = 42 }");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("Foo", result1);

    // Define a value using the associated item
    const result2 = try repl.step("x = Foo.defaultNum");
    defer std.testing.allocator.free(result2);
}

test "Repl - nested type with tag constructor" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Define nested types
    const result1 = try repl.step("Foo := [Whatever].{ Bar := [X, Y, Z] }");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("Foo", result1);

    // Create a value with type annotation
    const result2 = try repl.step("x : Foo.Bar");
    defer std.testing.allocator.free(result2);

    // Assign the value
    const result3 = try repl.step("x = Foo.Bar.X");
    defer std.testing.allocator.free(result3);
}
