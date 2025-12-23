//! Closure Transformer
//!
//! Transforms closures with captures into tagged values with explicit capture records.
//! This is the first step of lambda set specialization following the Cor approach.
//!
//! ## Transformation Example
//!
//! Input:
//! ```roc
//! {
//!     x = 42
//!     addX = |y| x + y
//!     addX(10)
//! }
//! ```
//!
//! Output:
//! ```roc
//! {
//!     x = 42
//!     addX = #addX({ x: x })
//!     match addX {
//!         #addX({ x }) => {
//!             y = 10
//!             (x + y)
//!         },
//!     }
//! }
//! ```
//!
//! ## Implementation Notes
//!
//! - Closures become tags with capture records (using `#` prefix to avoid clashing with userspace tags)
//! - Call sites become inline match expressions that dispatch based on the lambda set
//! - Pure lambdas (no captures) become tags with empty records

const std = @import("std");
const base = @import("base");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const Expr = CIR.Expr;
const Pattern = @import("Pattern.zig").Pattern;
const RecordField = CIR.RecordField;

const Self = @This();

/// Information about a transformed closure
pub const ClosureInfo = struct {
    /// The tag name for this closure (e.g., `addX`)
    tag_name: base.Ident.Idx,
    /// The lambda body expression
    lambda_body: Expr.Idx,
    /// The lambda arguments
    lambda_args: CIR.Pattern.Span,
    /// The capture names (for generating dispatch function patterns)
    capture_names: std.ArrayList(base.Ident.Idx),
};

/// A lambda set - a collection of closures that could reach a given variable
pub const LambdaSet = struct {
    /// All closures in this lambda set
    closures: std.ArrayList(ClosureInfo),

    pub fn init() LambdaSet {
        return .{ .closures = std.ArrayList(ClosureInfo).empty };
    }

    /// Note: This does NOT free capture_names since they are shared with the closures map.
    /// The capture_names are owned by the original closures and freed when the transformer is deinitialized.
    pub fn deinit(self: *LambdaSet, allocator: std.mem.Allocator) void {
        self.closures.deinit(allocator);
    }

    pub fn addClosure(self: *LambdaSet, allocator: std.mem.Allocator, info: ClosureInfo) !void {
        try self.closures.append(allocator, info);
    }

    pub fn merge(self: *LambdaSet, allocator: std.mem.Allocator, other: *const LambdaSet) !void {
        for (other.closures.items) |info| {
            try self.closures.append(allocator, info);
        }
    }
};

/// Information for generating a dispatch function
pub const DispatchFunction = struct {
    /// Name of the dispatch function (e.g., `call_addX`)
    name: base.Ident.Idx,
    /// The closures that can reach this call site
    closures: std.ArrayList(ClosureInfo),
};

/// The allocator for intermediate allocations
allocator: std.mem.Allocator,

/// The module environment containing the CIR (mutable for adding new expressions)
module_env: *ModuleEnv,

/// Counter for generating unique closure names
closure_counter: u32,

/// Map from original closure expression to its transformation info
closures: std.AutoHashMap(Expr.Idx, ClosureInfo),

/// Map from pattern index to lambda set (for tracking which closures can reach a variable)
pattern_lambda_sets: std.AutoHashMap(CIR.Pattern.Idx, LambdaSet),

/// List of dispatch functions to generate
dispatch_functions: std.ArrayList(DispatchFunction),

/// Initialize the transformer
pub fn init(allocator: std.mem.Allocator, module_env: *ModuleEnv) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .closure_counter = 0,
        .closures = std.AutoHashMap(Expr.Idx, ClosureInfo).init(allocator),
        .pattern_lambda_sets = std.AutoHashMap(CIR.Pattern.Idx, LambdaSet).init(allocator),
        .dispatch_functions = std.ArrayList(DispatchFunction).empty,
    };
}

/// Free resources
pub fn deinit(self: *Self) void {
    // Free capture name lists
    var closure_iter = self.closures.valueIterator();
    while (closure_iter.next()) |info| {
        info.capture_names.deinit(self.allocator);
    }
    self.closures.deinit();

    // Free lambda sets (they own their ClosureInfo copies)
    var lambda_set_iter = self.pattern_lambda_sets.valueIterator();
    while (lambda_set_iter.next()) |lambda_set| {
        // Don't free individual capture_names here since they share data with closures map
        lambda_set.closures.deinit(self.allocator);
    }
    self.pattern_lambda_sets.deinit();

    // Free dispatch function closure lists
    for (self.dispatch_functions.items) |*df| {
        df.closures.deinit(self.allocator);
    }
    self.dispatch_functions.deinit(self.allocator);
}

/// Generate a unique tag name for a closure
pub fn generateClosureTagName(self: *Self, hint: ?base.Ident.Idx) !base.Ident.Idx {
    self.closure_counter += 1;

    // If we have a hint (e.g., from the variable name), use it with counter for uniqueness
    if (hint) |h| {
        const hint_name = self.module_env.getIdent(h);
        // Use # prefix since it's Roc's comment syntax and can't clash with userspace tags
        // Include counter to ensure uniqueness, e.g., "myFunc" becomes "#myFunc_1", "#myFunc_2", etc.
        const tag_name = try std.fmt.allocPrint(
            self.allocator,
            "#{s}_{d}",
            .{ hint_name, self.closure_counter },
        );
        defer self.allocator.free(tag_name);
        return try self.module_env.insertIdent(base.Ident.for_text(tag_name));
    }

    // Otherwise generate a numeric name
    const tag_name = try std.fmt.allocPrint(
        self.allocator,
        "#{d}",
        .{self.closure_counter},
    );
    defer self.allocator.free(tag_name);
    return try self.module_env.insertIdent(base.Ident.for_text(tag_name));
}

/// Generate a dispatch match expression for a closure call.
///
/// Transforms a call like `f(10)` where `f` is a closure into:
/// ```roc
/// match f {
///     #f({ x }) => {
///         y = 10      # Bind call arguments to lambda parameters
///         x + y       # Original lambda body
///     }
/// }
/// ```
fn generateDispatchMatch(
    self: *Self,
    closure_var_expr: Expr.Idx,
    closure_info: ClosureInfo,
    call_args: []const Expr.Idx,
) !Expr.Idx {
    // Step 1: Create the capture record destructure pattern
    // For `{ x, y }` we need a record_destructure with each field

    const record_destruct_start = self.module_env.store.scratchRecordDestructTop();

    for (closure_info.capture_names.items) |capture_name| {
        // Create an assign pattern for this capture binding
        const assign_pattern = try self.module_env.store.addPattern(
            Pattern{ .assign = .{ .ident = capture_name } },
            base.Region.zero(),
        );

        // Create the record destruct for this field
        const destruct = Pattern.RecordDestruct{
            .label = capture_name,
            .ident = capture_name,
            .kind = .{ .Required = assign_pattern },
        };
        const destruct_idx = try self.module_env.store.addRecordDestruct(destruct, base.Region.zero());
        try self.module_env.store.addScratchRecordDestruct(destruct_idx);
    }

    const destructs_span = try self.module_env.store.recordDestructSpanFrom(record_destruct_start);

    // Create the record destructure pattern
    const record_pattern = try self.module_env.store.addPattern(
        Pattern{ .record_destructure = .{ .destructs = destructs_span } },
        base.Region.zero(),
    );

    // Step 2: Create the applied_tag pattern: `f({ x, y })
    // The tag pattern takes the record pattern as its single argument
    const pattern_args_start = self.module_env.store.scratchPatternTop();
    try self.module_env.store.addScratchPattern(record_pattern);
    const pattern_args_span = try self.module_env.store.patternSpanFrom(pattern_args_start);

    const tag_pattern = try self.module_env.store.addPattern(
        Pattern{ .applied_tag = .{
            .name = closure_info.tag_name,
            .args = pattern_args_span,
        } },
        base.Region.zero(),
    );

    // Step 3: Create the body - a block that binds arguments then executes lambda body
    // We need to bind each call argument to the corresponding lambda parameter

    const lambda_params = self.module_env.store.slicePatterns(closure_info.lambda_args);

    // If we have arguments to bind, create a block with let bindings
    const body_expr = if (call_args.len > 0 and lambda_params.len > 0) blk: {
        const stmt_start = self.module_env.store.scratch.?.statements.top();

        // Bind each argument to its parameter
        const num_args = @min(call_args.len, lambda_params.len);
        for (0..num_args) |i| {
            const param_pattern = lambda_params[i];
            const arg_expr = call_args[i];

            const stmt = CIR.Statement{ .s_decl = .{
                .pattern = param_pattern,
                .expr = arg_expr,
                .anno = null,
            } };
            const stmt_idx = try self.module_env.store.addStatement(stmt, base.Region.zero());
            try self.module_env.store.scratch.?.statements.append(stmt_idx);
        }

        const stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);

        // Create block with bindings and lambda body as final expression
        break :blk try self.module_env.store.addExpr(Expr{
            .e_block = .{
                .stmts = stmts_span,
                .final_expr = closure_info.lambda_body,
            },
        }, base.Region.zero());
    } else blk: {
        // No arguments, just use the lambda body directly
        break :blk closure_info.lambda_body;
    };

    // Step 4: Create the match branch
    const branch_pattern_start = self.module_env.store.scratchMatchBranchPatternTop();
    const branch_pattern = try self.module_env.store.addMatchBranchPattern(
        Expr.Match.BranchPattern{
            .pattern = tag_pattern,
            .degenerate = false,
        },
        base.Region.zero(),
    );
    try self.module_env.store.addScratchMatchBranchPattern(branch_pattern);
    const branch_patterns_span = try self.module_env.store.matchBranchPatternSpanFrom(branch_pattern_start);

    // Create a fresh type variable for the redundant field
    const redundant_var = try self.module_env.types.fresh();

    const branch = Expr.Match.Branch{
        .patterns = branch_patterns_span,
        .value = body_expr,
        .guard = null,
        .redundant = redundant_var,
    };
    const branch_idx = try self.module_env.store.addMatchBranch(branch, base.Region.zero());

    // Step 5: Create the match expression
    const branch_start = self.module_env.store.scratchMatchBranchTop();
    try self.module_env.store.addScratchMatchBranch(branch_idx);
    const branches_span = try self.module_env.store.matchBranchSpanFrom(branch_start);

    // Create a fresh type variable for exhaustiveness
    const exhaustive_var = try self.module_env.types.fresh();

    return try self.module_env.store.addExpr(Expr{
        .e_match = .{
            .cond = closure_var_expr,
            .branches = branches_span,
            .exhaustive = exhaustive_var,
        },
    }, base.Region.zero());
}

/// Generate a dispatch match expression for a call to a variable with multiple possible closures.
///
/// Transforms a call like `f(10)` where `f` could be one of several closures into:
/// ```roc
/// match f {
///     Closure_add1({}) => 10 + 1,
///     Closure_mul2({}) => 10 * 2,
/// }
/// ```
fn generateLambdaSetDispatchMatch(
    self: *Self,
    closure_var_expr: Expr.Idx,
    lambda_set: *const LambdaSet,
    call_args: []const Expr.Idx,
) !Expr.Idx {
    // If there's only one closure, use the simpler single-closure dispatch
    if (lambda_set.closures.items.len == 1) {
        return try self.generateDispatchMatch(closure_var_expr, lambda_set.closures.items[0], call_args);
    }

    // Start collecting match branches
    const branch_start = self.module_env.store.scratchMatchBranchTop();

    // Generate a branch for each closure in the lambda set
    for (lambda_set.closures.items) |closure_info| {
        // Create the capture record destructure pattern
        const record_destruct_start = self.module_env.store.scratchRecordDestructTop();

        for (closure_info.capture_names.items) |capture_name| {
            const assign_pattern = try self.module_env.store.addPattern(
                Pattern{ .assign = .{ .ident = capture_name } },
                base.Region.zero(),
            );
            const destruct = Pattern.RecordDestruct{
                .label = capture_name,
                .ident = capture_name,
                .kind = .{ .Required = assign_pattern },
            };
            const destruct_idx = try self.module_env.store.addRecordDestruct(destruct, base.Region.zero());
            try self.module_env.store.addScratchRecordDestruct(destruct_idx);
        }

        const destructs_span = try self.module_env.store.recordDestructSpanFrom(record_destruct_start);

        // Create record destructure pattern
        const record_pattern = try self.module_env.store.addPattern(
            Pattern{ .record_destructure = .{ .destructs = destructs_span } },
            base.Region.zero(),
        );

        // Create applied_tag pattern
        const pattern_args_start = self.module_env.store.scratchPatternTop();
        try self.module_env.store.addScratchPattern(record_pattern);
        const pattern_args_span = try self.module_env.store.patternSpanFrom(pattern_args_start);

        const tag_pattern = try self.module_env.store.addPattern(
            Pattern{ .applied_tag = .{
                .name = closure_info.tag_name,
                .args = pattern_args_span,
            } },
            base.Region.zero(),
        );

        // Create the body - bind arguments and execute lambda body
        const lambda_params = self.module_env.store.slicePatterns(closure_info.lambda_args);

        const body_expr = if (call_args.len > 0 and lambda_params.len > 0) blk: {
            const stmt_start = self.module_env.store.scratch.?.statements.top();

            const num_args = @min(call_args.len, lambda_params.len);
            for (0..num_args) |i| {
                const param_pattern = lambda_params[i];
                const arg_expr = call_args[i];

                const stmt = CIR.Statement{ .s_decl = .{
                    .pattern = param_pattern,
                    .expr = arg_expr,
                    .anno = null,
                } };
                const stmt_idx = try self.module_env.store.addStatement(stmt, base.Region.zero());
                try self.module_env.store.scratch.?.statements.append(stmt_idx);
            }

            const stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);

            break :blk try self.module_env.store.addExpr(Expr{
                .e_block = .{
                    .stmts = stmts_span,
                    .final_expr = closure_info.lambda_body,
                },
            }, base.Region.zero());
        } else blk: {
            break :blk closure_info.lambda_body;
        };

        // Create match branch pattern
        const branch_pattern_start = self.module_env.store.scratchMatchBranchPatternTop();
        const branch_pattern = try self.module_env.store.addMatchBranchPattern(
            Expr.Match.BranchPattern{
                .pattern = tag_pattern,
                .degenerate = false,
            },
            base.Region.zero(),
        );
        try self.module_env.store.addScratchMatchBranchPattern(branch_pattern);
        const branch_patterns_span = try self.module_env.store.matchBranchPatternSpanFrom(branch_pattern_start);

        const redundant_var = try self.module_env.types.fresh();

        const branch = Expr.Match.Branch{
            .patterns = branch_patterns_span,
            .value = body_expr,
            .guard = null,
            .redundant = redundant_var,
        };
        const branch_idx = try self.module_env.store.addMatchBranch(branch, base.Region.zero());
        try self.module_env.store.addScratchMatchBranch(branch_idx);
    }

    const branches_span = try self.module_env.store.matchBranchSpanFrom(branch_start);

    const exhaustive_var = try self.module_env.types.fresh();

    return try self.module_env.store.addExpr(Expr{
        .e_match = .{
            .cond = closure_var_expr,
            .branches = branches_span,
            .exhaustive = exhaustive_var,
        },
    }, base.Region.zero());
}

/// Result of transforming an expression that may contain closures
pub const TransformResult = struct {
    expr: Expr.Idx,
    lambda_set: ?LambdaSet,
};

/// Transform an expression and collect any closures into a lambda set.
/// This handles direct closures, if expressions with closures, etc.
pub fn transformExprWithLambdaSet(
    self: *Self,
    expr_idx: Expr.Idx,
    name_hint: ?base.Ident.Idx,
) std.mem.Allocator.Error!TransformResult {
    const expr = self.module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_closure, .e_lambda => {
            // Direct closure/lambda - transform and create single-element lambda set
            const transformed = try self.transformClosure(expr_idx, name_hint);
            if (self.closures.get(expr_idx)) |closure_info| {
                var lambda_set = LambdaSet.init();
                try lambda_set.addClosure(self.allocator, closure_info);
                return .{ .expr = transformed, .lambda_set = lambda_set };
            }
            return .{ .expr = transformed, .lambda_set = null };
        },
        .e_if => |if_expr| {
            // If expression - collect closures from all branches
            var lambda_set = LambdaSet.init();

            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
            const branch_start = self.module_env.store.scratch.?.if_branches.top();

            for (branches) |branch_idx| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                const new_cond = try self.transformExpr(branch.cond);

                // Transform branch body and collect its lambda set
                var body_result = try self.transformExprWithLambdaSet(branch.body, name_hint);
                if (body_result.lambda_set) |*branch_lambda_set| {
                    try lambda_set.merge(self.allocator, branch_lambda_set);
                    // Free the branch lambda set's ArrayList after merging (not capture_names, those are shared)
                    branch_lambda_set.deinit(self.allocator);
                }

                const new_branch_idx = try self.module_env.store.addIfBranch(
                    Expr.IfBranch{ .cond = new_cond, .body = body_result.expr },
                    base.Region.zero(),
                );
                try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
            }

            // Transform else branch and collect its lambda set
            var else_result = try self.transformExprWithLambdaSet(if_expr.final_else, name_hint);
            if (else_result.lambda_set) |*else_lambda_set| {
                try lambda_set.merge(self.allocator, else_lambda_set);
                // Free the else lambda set's ArrayList after merging
                else_lambda_set.deinit(self.allocator);
            }

            const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);

            const new_if = try self.module_env.store.addExpr(Expr{
                .e_if = .{
                    .branches = new_branches_span,
                    .final_else = else_result.expr,
                },
            }, base.Region.zero());

            const result_lambda_set = if (lambda_set.closures.items.len > 0)
                lambda_set
            else
                null;

            return .{ .expr = new_if, .lambda_set = result_lambda_set };
        },
        else => {
            // Other expressions - just transform without lambda set
            const transformed = try self.transformExpr(expr_idx);
            return .{ .expr = transformed, .lambda_set = null };
        },
    }
}

/// Transform a closure expression into a tag with capture record.
/// Returns the new expression index.
pub fn transformClosure(
    self: *Self,
    closure_expr_idx: Expr.Idx,
    binding_name_hint: ?base.Ident.Idx,
) !Expr.Idx {
    const expr = self.module_env.store.getExpr(closure_expr_idx);

    switch (expr) {
        .e_closure => |closure| {
            // Get the lambda body and args
            const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
            const lambda = switch (lambda_expr) {
                .e_lambda => |l| l,
                else => return closure_expr_idx, // Not a lambda, return as-is
            };

            // Generate tag name
            const tag_name = try self.generateClosureTagName(binding_name_hint);

            // Get captures
            const captures = self.module_env.store.sliceCaptures(closure.captures);

            // Build capture record fields
            const scratch_top = self.module_env.store.scratch.?.record_fields.top();

            var capture_names = std.ArrayList(base.Ident.Idx).empty;

            for (captures) |capture_idx| {
                const capture = self.module_env.store.getCapture(capture_idx);

                // Create a lookup expression for the captured variable
                // Use store.addExpr directly to avoid region sync checks during transformation
                const lookup_expr = try self.module_env.store.addExpr(Expr{
                    .e_lookup_local = .{ .pattern_idx = capture.pattern_idx },
                }, base.Region.zero());

                // Create record field: { capture_name: capture_value }
                const field = RecordField{
                    .name = capture.name,
                    .value = lookup_expr,
                };
                const field_idx = try self.module_env.store.addRecordField(field, base.Region.zero());
                try self.module_env.store.scratch.?.record_fields.append(field_idx);
                try capture_names.append(self.allocator, capture.name);
            }

            // Create the record expression
            const fields_span = try self.module_env.store.recordFieldSpanFrom(scratch_top);

            const record_expr = if (captures.len > 0)
                try self.module_env.store.addExpr(Expr{
                    .e_record = .{ .fields = fields_span, .ext = null },
                }, base.Region.zero())
            else
                try self.module_env.store.addExpr(Expr{
                    .e_empty_record = .{},
                }, base.Region.zero());

            // Create the tag expression: `tagName(captureRecord)
            // First, add the record as an argument
            const args_start = self.module_env.store.scratch.?.exprs.top();
            try self.module_env.store.scratch.?.exprs.append(record_expr);
            const args_span = try self.module_env.store.exprSpanFrom(args_start);

            const tag_expr = try self.module_env.store.addExpr(Expr{
                .e_tag = .{
                    .name = tag_name,
                    .args = args_span,
                },
            }, base.Region.zero());

            // Store closure info for dispatch function generation
            try self.closures.put(closure_expr_idx, ClosureInfo{
                .tag_name = tag_name,
                .lambda_body = lambda.body,
                .lambda_args = lambda.args,
                .capture_names = capture_names,
            });

            return tag_expr;
        },
        .e_lambda => |lambda| {
            // Pure lambda (no captures) - still wrap in a tag with empty record
            const tag_name = try self.generateClosureTagName(binding_name_hint);

            const empty_record = try self.module_env.store.addExpr(Expr{
                .e_empty_record = .{},
            }, base.Region.zero());

            const args_start = self.module_env.store.scratch.?.exprs.top();
            try self.module_env.store.scratch.?.exprs.append(empty_record);
            const args_span = try self.module_env.store.exprSpanFrom(args_start);

            const tag_expr = try self.module_env.store.addExpr(Expr{
                .e_tag = .{
                    .name = tag_name,
                    .args = args_span,
                },
            }, base.Region.zero());

            // Store info for dispatch
            try self.closures.put(closure_expr_idx, ClosureInfo{
                .tag_name = tag_name,
                .lambda_body = lambda.body,
                .lambda_args = lambda.args,
                .capture_names = std.ArrayList(base.Ident.Idx).empty,
            });

            return tag_expr;
        },
        else => return closure_expr_idx, // Not a closure, return as-is
    }
}

/// Transform an entire expression tree, handling closures and their call sites.
/// This is the main entry point for the transformation.
pub fn transformExpr(self: *Self, expr_idx: Expr.Idx) std.mem.Allocator.Error!Expr.Idx {
    const expr = self.module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_closure => {
            // Transform closure to tag
            return try self.transformClosure(expr_idx, null);
        },
        .e_lambda => {
            // Transform pure lambda to tag
            return try self.transformClosure(expr_idx, null);
        },
        .e_block => |block| {
            // Transform block: handle statements and final expression
            const stmts = self.module_env.store.sliceStatements(block.stmts);

            // Create new statements with transformed expressions
            const stmt_start = self.module_env.store.scratch.?.statements.top();

            for (stmts) |stmt_idx| {
                const stmt = self.module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        // Get binding name hint from pattern
                        const pattern = self.module_env.store.getPattern(decl.pattern);
                        const name_hint: ?base.Ident.Idx = switch (pattern) {
                            .assign => |a| a.ident,
                            else => null,
                        };

                        // Transform expression and collect lambda set
                        const result = try self.transformExprWithLambdaSet(decl.expr, name_hint);

                        // Track this pattern's lambda set if it has closures
                        if (result.lambda_set) |lambda_set| {
                            try self.pattern_lambda_sets.put(decl.pattern, lambda_set);
                        }

                        // Create new statement with transformed expression
                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl = .{
                                .pattern = decl.pattern,
                                .expr = result.expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    .s_decl_gen => |decl| {
                        const pattern = self.module_env.store.getPattern(decl.pattern);
                        const name_hint: ?base.Ident.Idx = switch (pattern) {
                            .assign => |a| a.ident,
                            else => null,
                        };

                        // Transform expression and collect lambda set
                        const result = try self.transformExprWithLambdaSet(decl.expr, name_hint);

                        // Track this pattern's lambda set if it has closures
                        if (result.lambda_set) |lambda_set| {
                            try self.pattern_lambda_sets.put(decl.pattern, lambda_set);
                        }

                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl_gen = .{
                                .pattern = decl.pattern,
                                .expr = result.expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    else => {
                        // Copy statement as-is
                        try self.module_env.store.scratch.?.statements.append(stmt_idx);
                    },
                }
            }

            const new_stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);

            // Transform final expression
            const new_final = try self.transformExpr(block.final_expr);

            // Create new block
            return try self.module_env.store.addExpr(Expr{
                .e_block = .{
                    .stmts = new_stmts_span,
                    .final_expr = new_final,
                },
            }, base.Region.zero());
        },
        .e_call => |call| {
            // First transform arguments recursively
            const args = self.module_env.store.sliceExpr(call.args);
            const args_start = self.module_env.store.scratch.?.exprs.top();

            for (args) |arg_idx| {
                const new_arg = try self.transformExpr(arg_idx);
                try self.module_env.store.scratch.?.exprs.append(new_arg);
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);
            const transformed_args = self.module_env.store.sliceExpr(new_args_span);

            // Check if the function is a local variable that holds a closure
            const func_expr = self.module_env.store.getExpr(call.func);
            switch (func_expr) {
                .e_lookup_local => |lookup| {
                    // Check if this pattern has a lambda set (one or more closures)
                    if (self.pattern_lambda_sets.getPtr(lookup.pattern_idx)) |lambda_set| {
                        // Generate a dispatch match expression for all possible closures
                        return try self.generateLambdaSetDispatchMatch(
                            call.func,
                            lambda_set,
                            transformed_args,
                        );
                    }
                },
                else => {},
            }

            // Not a closure call, transform normally
            const new_func = try self.transformExpr(call.func);

            return try self.module_env.store.addExpr(Expr{
                .e_call = .{
                    .func = new_func,
                    .args = new_args_span,
                    .called_via = call.called_via,
                },
            }, base.Region.zero());
        },
        .e_if => |if_expr| {
            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
            const branch_start = self.module_env.store.scratch.?.if_branches.top();

            for (branches) |branch_idx| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                const new_cond = try self.transformExpr(branch.cond);
                const new_body = try self.transformExpr(branch.body);

                const new_branch_idx = try self.module_env.store.addIfBranch(
                    Expr.IfBranch{ .cond = new_cond, .body = new_body },
                    base.Region.zero(),
                );
                try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
            }

            const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);
            const new_else = try self.transformExpr(if_expr.final_else);

            return try self.module_env.store.addExpr(Expr{
                .e_if = .{
                    .branches = new_branches_span,
                    .final_else = new_else,
                },
            }, base.Region.zero());
        },
        .e_binop => |binop| {
            const new_lhs = try self.transformExpr(binop.lhs);
            const new_rhs = try self.transformExpr(binop.rhs);

            return try self.module_env.store.addExpr(Expr{
                .e_binop = .{
                    .op = binop.op,
                    .lhs = new_lhs,
                    .rhs = new_rhs,
                },
            }, base.Region.zero());
        },
        // Pass through simple expressions unchanged
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_str_segment,
        .e_str,
        .e_lookup_local,
        .e_lookup_external,
        .e_empty_list,
        .e_empty_record,
        .e_zero_argument_tag,
        .e_runtime_error,
        .e_ellipsis,
        .e_anno_only,
        .e_lookup_required,
        .e_type_var_dispatch,
        .e_hosted_lambda,
        .e_low_level_lambda,
        => return expr_idx,

        .e_list => |list| {
            const elems = self.module_env.store.sliceExpr(list.elems);
            const elems_start = self.module_env.store.scratch.?.exprs.top();

            for (elems) |elem_idx| {
                const new_elem = try self.transformExpr(elem_idx);
                try self.module_env.store.scratch.?.exprs.append(new_elem);
            }

            const new_elems_span = try self.module_env.store.exprSpanFrom(elems_start);

            return try self.module_env.store.addExpr(Expr{
                .e_list = .{ .elems = new_elems_span },
            }, base.Region.zero());
        },
        .e_tuple => |tuple| {
            const elems = self.module_env.store.sliceExpr(tuple.elems);
            const elems_start = self.module_env.store.scratch.?.exprs.top();

            for (elems) |elem_idx| {
                const new_elem = try self.transformExpr(elem_idx);
                try self.module_env.store.scratch.?.exprs.append(new_elem);
            }

            const new_elems_span = try self.module_env.store.exprSpanFrom(elems_start);

            return try self.module_env.store.addExpr(Expr{
                .e_tuple = .{ .elems = new_elems_span },
            }, base.Region.zero());
        },
        .e_record => |record| {
            const field_indices = self.module_env.store.sliceRecordFields(record.fields);
            const fields_start = self.module_env.store.scratch.?.record_fields.top();

            for (field_indices) |field_idx| {
                const field = self.module_env.store.getRecordField(field_idx);
                const new_value = try self.transformExpr(field.value);

                const new_field = RecordField{
                    .name = field.name,
                    .value = new_value,
                };
                const new_field_idx = try self.module_env.store.addRecordField(new_field, base.Region.zero());
                try self.module_env.store.scratch.?.record_fields.append(new_field_idx);
            }

            const new_fields_span = try self.module_env.store.recordFieldSpanFrom(fields_start);

            const new_ext = if (record.ext) |ext| try self.transformExpr(ext) else null;

            return try self.module_env.store.addExpr(Expr{
                .e_record = .{
                    .fields = new_fields_span,
                    .ext = new_ext,
                },
            }, base.Region.zero());
        },
        .e_tag => |tag| {
            const args = self.module_env.store.sliceExpr(tag.args);
            const args_start = self.module_env.store.scratch.?.exprs.top();

            for (args) |arg_idx| {
                const new_arg = try self.transformExpr(arg_idx);
                try self.module_env.store.scratch.?.exprs.append(new_arg);
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);

            return try self.module_env.store.addExpr(Expr{
                .e_tag = .{
                    .name = tag.name,
                    .args = new_args_span,
                },
            }, base.Region.zero());
        },
        .e_unary_minus => |unary| {
            const new_expr = try self.transformExpr(unary.expr);
            return try self.module_env.store.addExpr(Expr{
                .e_unary_minus = .{ .expr = new_expr },
            }, base.Region.zero());
        },
        .e_unary_not => |unary| {
            const new_expr = try self.transformExpr(unary.expr);
            return try self.module_env.store.addExpr(Expr{
                .e_unary_not = .{ .expr = new_expr },
            }, base.Region.zero());
        },
        .e_dot_access => |dot| {
            const new_receiver = try self.transformExpr(dot.receiver);
            const new_args = if (dot.args) |args_span| blk: {
                const args = self.module_env.store.sliceExpr(args_span);
                const args_start = self.module_env.store.scratch.?.exprs.top();

                for (args) |arg_idx| {
                    const new_arg = try self.transformExpr(arg_idx);
                    try self.module_env.store.scratch.?.exprs.append(new_arg);
                }

                break :blk try self.module_env.store.exprSpanFrom(args_start);
            } else null;

            return try self.module_env.store.addExpr(Expr{
                .e_dot_access = .{
                    .receiver = new_receiver,
                    .field_name = dot.field_name,
                    .field_name_region = dot.field_name_region,
                    .args = new_args,
                },
            }, base.Region.zero());
        },
        .e_crash => return expr_idx,
        .e_dbg => |dbg| {
            const new_expr = try self.transformExpr(dbg.expr);
            return try self.module_env.store.addExpr(Expr{
                .e_dbg = .{
                    .expr = new_expr,
                },
            }, base.Region.zero());
        },
        .e_expect => |expect| {
            const new_body = try self.transformExpr(expect.body);
            return try self.module_env.store.addExpr(Expr{
                .e_expect = .{
                    .body = new_body,
                },
            }, base.Region.zero());
        },
        .e_return => |ret| {
            const new_expr = try self.transformExpr(ret.expr);
            return try self.module_env.store.addExpr(Expr{
                .e_return = .{ .expr = new_expr },
            }, base.Region.zero());
        },
        .e_match => |match| {
            const new_cond = try self.transformExpr(match.cond);
            // Note: match branches would need deeper transformation for closures in branches
            // For now, pass through as-is
            return try self.module_env.store.addExpr(Expr{
                .e_match = .{
                    .cond = new_cond,
                    .branches = match.branches,
                    .exhaustive = match.exhaustive,
                },
            }, base.Region.zero());
        },
        .e_nominal => |nominal| {
            const new_backing = try self.transformExpr(nominal.backing_expr);
            return try self.module_env.store.addExpr(Expr{
                .e_nominal = .{
                    .nominal_type_decl = nominal.nominal_type_decl,
                    .backing_expr = new_backing,
                    .backing_type = nominal.backing_type,
                },
            }, base.Region.zero());
        },
        .e_nominal_external => |nominal| {
            const new_backing = try self.transformExpr(nominal.backing_expr);
            return try self.module_env.store.addExpr(Expr{
                .e_nominal_external = .{
                    .module_idx = nominal.module_idx,
                    .target_node_idx = nominal.target_node_idx,
                    .backing_expr = new_backing,
                    .backing_type = nominal.backing_type,
                },
            }, base.Region.zero());
        },
        .e_for => |for_expr| {
            const new_expr = try self.transformExpr(for_expr.expr);
            const new_body = try self.transformExpr(for_expr.body);
            return try self.module_env.store.addExpr(Expr{
                .e_for = .{
                    .patt = for_expr.patt,
                    .expr = new_expr,
                    .body = new_body,
                },
            }, base.Region.zero());
        },
    }
}

// Tests

const testing = std.testing;

test "ClosureTransformer: init and deinit" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    try testing.expectEqual(@as(u32, 0), transformer.closure_counter);
}

test "ClosureTransformer: generateClosureTagName with hint" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    // Create a hint identifier
    const hint = try module_env.insertIdent(base.Ident.for_text("addX"));

    const tag_name = try transformer.generateClosureTagName(hint);
    const tag_str = module_env.getIdent(tag_name);

    try testing.expectEqualStrings("#addX_1", tag_str);
}

test "ClosureTransformer: generateClosureTagName without hint" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    const tag_name = try transformer.generateClosureTagName(null);
    const tag_str = module_env.getIdent(tag_name);

    try testing.expectEqualStrings("#1", tag_str);
}
