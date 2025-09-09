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
debug_can_html: std.ArrayList([]const u8),
/// Storage for rendered TYPES HTML at each step (only when debug_store_snapshots is true)
debug_types_html: std.ArrayList([]const u8),

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
        .debug_can_html = std.ArrayList([]const u8).init(allocator),
        .debug_types_html = std.ArrayList([]const u8).init(allocator),
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
fn allocateModuleEnv(self: *Repl, src: base.SrcBytes) !*ModuleEnv {
    // Clean up previous ModuleEnv if it exists
    if (self.last_module_env) |old_env| {
        old_env.deinit();
        self.allocator.destroy(old_env);
    }

    // Allocate new ModuleEnv on heap
    const new_env = try self.allocator.create(ModuleEnv);
    new_env.* = try ModuleEnv.init(self.allocator, src);
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

    // Create SrcBytes from input with single allocation
    const total_size = trimmed.len + base.SrcBytes.suffix.len;
    
    if (total_size > std.math.maxInt(u31)) {
        return error.InputTooBig;
    }
    
    // Single allocation with proper alignment and space for suffix
    const allocation = try self.allocator.allocWithOptions(u8, total_size, base.SrcBytes.alignment, null);
    defer self.allocator.free(allocation);
    
    // Copy trimmed input
    @memcpy(allocation[0..trimmed.len], trimmed);
    
    // Add the suffix
    @memcpy(allocation[trimmed.len..], &base.SrcBytes.suffix);
    
    // Create SrcBytes
    const src = base.SrcBytes{ .ptr = allocation.ptr, .len = @intCast(total_size) };

    // Process the input
    return try self.processInput(src);
}

/// Process regular input (not special commands)
fn processInput(self: *Repl, src: base.SrcBytes) ![]const u8 {
    // Try to parse as a statement first
    const parse_result = try self.tryParseStatement(src);
    const input = src.bytes();

    switch (parse_result) {
        .assignment => |info| {
            // Add or replace definition (duplicates the strings for ownership)
            try self.addOrReplaceDefinition(info.source, info.var_name);

            // Free the allocated var_name after using it
            defer self.allocator.free(info.var_name);

            // Return descriptive output for assignments
            return try std.fmt.allocPrint(self.allocator, "assigned `{s}`", .{info.var_name});
        },
        .import => {
            // Imports are not supported in this implementation
            return try self.allocator.dupe(u8, "Imports not yet supported");
        },
        .expression => {
            // Evaluate expression with all past definitions  
            // Remove suffix from input to get the original expression
            const expr_without_suffix = input[0..input.len - base.SrcBytes.suffix.len];
            const full_source = try self.buildFullSource(expr_without_suffix);
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
        var_name: []const u8, // Must be allocator.dupe'd
    },
    import,
    expression,
    type_decl,
    parse_error: []const u8, // Must be allocator.dupe'd
};

/// Try to parse input as a statement
fn tryParseStatement(self: *Repl, src: base.SrcBytes) !ParseResult {
    var module_env = try ModuleEnv.init(self.allocator, src);
    defer module_env.deinit();

    // Try parsing as an expression (since parseStatement uses parseFile which is wrong)
    if (parse.parseExpr(&module_env.common, self.allocator)) |ast_const| {
        var ast = ast_const;
        defer ast.deinit(self.allocator);

        // Check if we have a valid root node
        if (ast.root_node_idx != 0) {
            const root_node_idx = @as(AST.Node.Idx, @enumFromInt(ast.root_node_idx));
            const first_node = ast.nodes.get(@enumFromInt(@intFromEnum(root_node_idx)));

            // Check the node tag to determine what was parsed
            switch (first_node.tag) {
                .binop_equals => {
                    // This is an assignment like `x = 42`
                    const binop = ast.node_slices.binOp(first_node.payload.binop);
                    const lhs_node = ast.nodes.get(@enumFromInt(@intFromEnum(binop.lhs)));

                    // Check if LHS is an identifier
                    if (lhs_node.tag == .lc or lhs_node.tag == .var_lc) {
                        const ident = lhs_node.payload.ident;
                        const ident_name = module_env.common.getIdent(ident);

                        // Duplicate the identifier name since module_env will be deinitialized
                        const owned_name = try self.allocator.dupe(u8, ident_name);
                        
                        // Get the source without suffix for the assignment
                        const bytes = src.bytes();
                        const source_without_suffix = bytes[0..bytes.len - base.SrcBytes.suffix.len];

                        return ParseResult{ .assignment = .{
                            .source = source_without_suffix,
                            .var_name = owned_name,
                        } };
                    }
                    return ParseResult.expression;
                },
                .import => return ParseResult.import,
                .binop_colon => return ParseResult.type_decl,
                .binop_colon_equals => return ParseResult.type_decl,
                else => return ParseResult.expression,
            }
        }
        // If we didn't match any special forms, treat it as an expression
        return ParseResult.expression;
    } else |_| {
        // Parse failed
        return ParseResult{ .parse_error = try self.allocator.dupe(u8, "Failed to parse input") };
    }
}

/// Build full source including all definitions wrapped in block syntax
pub fn buildFullSource(self: *Repl, current_expr: []const u8) ![]const u8 {
    // If no definitions exist, just return the expression as-is
    if (self.definitions.count() == 0) {
        return try self.allocator.dupe(u8, current_expr);
    }

    var buffer = std.ArrayList(u8).init(self.allocator);
    errdefer buffer.deinit();

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

    return buffer.toOwnedSlice();
}

/// Evaluate source code
fn evaluateSource(self: *Repl, source: []const u8) ![]const u8 {
    // Create SrcBytes from source with single allocation
    const total_size = source.len + base.SrcBytes.suffix.len;
    
    if (total_size > std.math.maxInt(u31)) {
        return error.InputTooBig;
    }
    
    // Single allocation with proper alignment and space for suffix
    const allocation = try self.allocator.allocWithOptions(u8, total_size, base.SrcBytes.alignment, null);
    // Note: allocation will be freed when module_env is deinitialized
    
    // Copy source
    @memcpy(allocation[0..source.len], source);
    
    // Add the suffix
    @memcpy(allocation[source.len..], &base.SrcBytes.suffix);
    
    // Create SrcBytes
    const src = base.SrcBytes{ .ptr = allocation.ptr, .len = @intCast(total_size) };
    
    const module_env = try self.allocateModuleEnv(src);
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

    // The new AST doesn't have a store that needs emptying

    // Create CIR/canonicalizer (which mutates AST in place)
    var cir = can.CIR.init(&parse_ast, &module_env.types);
    defer cir.deinit(self.allocator);

    // Set the CIR and AST on module_env so the interpreter can access them
    module_env.cir = &cir;
    module_env.ast = &parse_ast;

    // In AST, we need to find the expression differently
    // For now, use the first node as a placeholder
    const expr_idx: AST.Node.Idx = @enumFromInt(parse_ast.root_node_idx);

    const canonical_expr = try cir.canonicalizeExpr(self.allocator, expr_idx, module_env.common.source.bytes(), &module_env.common.idents);

    // Type check
    // Check.initForCIR needs different parameters for new architecture
    var regions = base.Region.List{};
    var checker = Check.initForCIR(self.allocator, &module_env.types, &regions) catch |err| {
        return try std.fmt.allocPrint(self.allocator, "Type check init error: {}", .{err});
    };
    defer checker.deinit();

    // Check the expression (no need to check defs since we're parsing as expressions)
    _ = checker.checkCIRExpr(can.CIR, &cir, canonical_expr) catch |err| {
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
    var interpreter = eval_mod.Interpreter.init(self.allocator, module_env, &self.eval_stack, &layout_cache, &module_env.types) catch |err| {
        return try std.fmt.allocPrint(self.allocator, "Interpreter init error: {}", .{err});
    };
    defer interpreter.deinit(self.roc_ops);

    // Evaluate the expression
    if (self.trace_writer) |trace_writer| {
        interpreter.startTrace(trace_writer);
    }

    const result = interpreter.eval(canonical_expr, self.roc_ops) catch |err| {
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
        try self.generateAndStoreDebugHtml(module_env, canonical_expr);
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
