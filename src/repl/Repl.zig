//! The evaluation part of the Read-Eval-Print-Loop (REPL)

const std = @import("std");
const base = @import("base");
const compile = @import("compile");
const parse = @import("parse");
const types = @import("types");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const layout_mod = @import("layout");
const eval_mod = @import("eval");

const Can = can.Can;
const Check = check.Check;
const AST = parse.AST;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const RocDec = builtins.dec.RocDec;
const RocOps = builtins.host_abi.RocOps;
const types_store = types.store;
const writers = types.writers;
const target = base.target;

/// REPL state that tracks past definitions and evaluates expressions
const Repl = @This();

allocator: Allocator,
/// Map from variable name to source string for definitions
definitions: std.StringHashMap([]const u8),
/// Stack for evaluation
eval_stack: eval_mod.Stack,
/// Operations for the Roc runtime
roc_ops: *RocOps,
/// Optional trace writer for debugging evaluation
trace_writer: ?std.io.AnyWriter,
/// ModuleEnv from last successful evaluation (for snapshot generation)
last_module_env: ?*ModuleEnv,
/// Debug flag to store rendered HTML for snapshot generation
debug_store_snapshots: bool,
/// Storage for rendered CAN HTML at each step (only when debug_store_snapshots is true)
debug_can_html: std.array_list.Managed([]const u8),
/// Storage for rendered TYPES HTML at each step (only when debug_store_snapshots is true)
debug_types_html: std.array_list.Managed([]const u8),

pub fn init(allocator: Allocator, roc_ops: *RocOps) !Repl {
    const eval_stack = try eval_mod.Stack.initCapacity(allocator, 8192);

    return Repl{
        .allocator = allocator,
        .definitions = std.StringHashMap([]const u8).init(allocator),
        .eval_stack = eval_stack,
        .roc_ops = roc_ops,
        .trace_writer = null,
        .last_module_env = null,
        .debug_store_snapshots = false,
        .debug_can_html = std.array_list.Managed([]const u8).init(allocator),
        .debug_types_html = std.array_list.Managed([]const u8).init(allocator),
    };
}

/// Set the trace writer for the REPL.
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

/// Allocate a new ModuleEnv and save it
fn allocateModuleEnv(self: *Repl, source: []const u8) !*ModuleEnv {
    // Clean up previous ModuleEnv if it exists
    if (self.last_module_env) |old_env| {
        old_env.deinit();
        self.allocator.destroy(old_env);
    }

    // Allocate new ModuleEnv on heap
    const new_env = try self.allocator.create(ModuleEnv);
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

        var can_buffer = std.array_list.Managed(u8).init(self.allocator);
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

        var types_buffer = std.array_list.Managed(u8).init(self.allocator);
        defer types_buffer.deinit();
        try tree.toStringPretty(types_buffer.writer().any());

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

    var buffer = std.array_list.Managed(u8).init(self.allocator);
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

    // Create canonicalizer
    var czer = Can.init(cir, &parse_ast, null) catch |err| {
        return try std.fmt.allocPrint(self.allocator, "Canonicalize init error: {}", .{err});
    };
    defer czer.deinit();

    // Since we're always parsing as expressions now, handle them the same way
    const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);

    const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse {
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
    var layout_cache = layout_mod.Store.init(module_env, &module_env.types) catch |err| {
        return try std.fmt.allocPrint(self.allocator, "Layout cache error: {}", .{err});
    };
    defer layout_cache.deinit();

    // Clear eval stack to prevent memory corruption from previous evaluations
    self.eval_stack.used = 0;

    // Create interpreter
    var interpreter = eval_mod.Interpreter.init(self.allocator, cir, &self.eval_stack, &layout_cache, &module_env.types) catch |err| {
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

    // Generate debug HTML if enabled
    if (self.debug_store_snapshots) {
        try self.generateAndStoreDebugHtml(module_env, final_expr_idx);
    }

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
                        return try std.fmt.allocPrint(self.allocator, "{d}", .{float_ptr.*});
                    },
                    .f64 => {
                        const float_ptr: *f64 = @ptrCast(@alignCast(result.ptr.?));
                        return try std.fmt.allocPrint(self.allocator, "{d}", .{float_ptr.*});
                    },
                    .dec => {
                        const dec_ptr: *RocDec = @ptrCast(@alignCast(result.ptr.?));
                        // Simple conversion to f64 for now to avoid allocator issues
                        const raw_value = dec_ptr.num;
                        const scale_factor = std.math.pow(f64, 10, RocDec.decimal_places);
                        const decimal_value = @as(f64, @floatFromInt(raw_value)) / scale_factor;
                        return try std.fmt.allocPrint(self.allocator, "{d}", .{decimal_value});
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
