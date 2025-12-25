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
//!     addX = Closure_addX_1({ x: x })
//!     match addX {
//!         Closure_addX_1({ x }) => {
//!             y = 10
//!             (x + y)
//!         },
//!     }
//! }
//! ```
//!
//! ## Implementation Notes
//!
//! - Closures become tags with capture records (using `Closure_` prefix to avoid clashing with userspace tags)
//! - Call sites to closures become inline match expressions that dispatch based on the lambda set
//! - Pure lambdas (no captures) are left unchanged - they don't need transformation

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
    /// The tag name for this closure (e.g., `Closure_addX_1`)
    tag_name: base.Ident.Idx,
    /// The lambda body expression
    lambda_body: Expr.Idx,
    /// The lambda arguments
    lambda_args: CIR.Pattern.Span,
    /// The capture names (for generating dispatch function patterns)
    capture_names: std.ArrayList(base.Ident.Idx),
    /// The pattern for the lifted function definition (for calling in dispatch)
    /// This is the pattern that binds the lifted function name (e.g., `closure_addX_1`)
    lifted_fn_pattern: ?CIR.Pattern.Idx,
    /// The captures pattern used in the lifted function (for passing to calls)
    lifted_captures_pattern: ?CIR.Pattern.Idx,
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

    /// Create a deep copy of this LambdaSet with its own ArrayList
    pub fn clone(self: *const LambdaSet, allocator: std.mem.Allocator) !LambdaSet {
        var new_set = LambdaSet.init();
        for (self.closures.items) |info| {
            try new_set.closures.append(allocator, info);
        }
        return new_set;
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

/// Map from lambda expression to its return lambda set (what closures it returns when called)
lambda_return_sets: std.AutoHashMap(Expr.Idx, LambdaSet),

/// Map from pattern to lambda return set (for looking up what a variable's lambda returns when called)
pattern_lambda_return_sets: std.AutoHashMap(CIR.Pattern.Idx, LambdaSet),

/// Set of top-level pattern indices (these don't need to be captured since they're always in scope)
top_level_patterns: std.AutoHashMap(CIR.Pattern.Idx, void),

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
        .lambda_return_sets = std.AutoHashMap(Expr.Idx, LambdaSet).init(allocator),
        .pattern_lambda_return_sets = std.AutoHashMap(CIR.Pattern.Idx, LambdaSet).init(allocator),
        .top_level_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(allocator),
        .dispatch_functions = std.ArrayList(DispatchFunction).empty,
    };
}

/// Mark a pattern as a top-level definition (doesn't need to be captured)
pub fn markTopLevel(self: *Self, pattern_idx: CIR.Pattern.Idx) !void {
    try self.top_level_patterns.put(pattern_idx, {});
}

/// Check if a pattern is a top-level definition
pub fn isTopLevel(self: *const Self, pattern_idx: CIR.Pattern.Idx) bool {
    return self.top_level_patterns.contains(pattern_idx);
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

    // Free lambda return sets
    var return_set_iter = self.lambda_return_sets.valueIterator();
    while (return_set_iter.next()) |lambda_set| {
        lambda_set.closures.deinit(self.allocator);
    }
    self.lambda_return_sets.deinit();

    // Free pattern lambda return sets
    var pattern_return_iter = self.pattern_lambda_return_sets.valueIterator();
    while (pattern_return_iter.next()) |lambda_set| {
        lambda_set.closures.deinit(self.allocator);
    }
    self.pattern_lambda_return_sets.deinit();

    // Free top-level patterns set
    self.top_level_patterns.deinit();

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
        // Use Closure_ prefix (capitalized) to create valid Roc tag names that won't clash with userspace tags
        // Include counter to ensure uniqueness, e.g., "myFunc" becomes "Closure_myFunc_1", "Closure_myFunc_2", etc.
        const tag_name = try std.fmt.allocPrint(
            self.allocator,
            "Closure_{s}_{d}",
            .{ hint_name, self.closure_counter },
        );
        defer self.allocator.free(tag_name);
        return try self.module_env.insertIdent(base.Ident.for_text(tag_name));
    }

    // Otherwise generate a numeric name
    const tag_name = try std.fmt.allocPrint(
        self.allocator,
        "Closure_{d}",
        .{self.closure_counter},
    );
    defer self.allocator.free(tag_name);
    return try self.module_env.insertIdent(base.Ident.for_text(tag_name));
}

/// Generate the lowercase function name from a closure tag name.
/// E.g., "Closure_addX_1" -> "closure_addX_1"
fn generateLiftedFunctionName(self: *Self, tag_name: base.Ident.Idx) !base.Ident.Idx {
    const tag_str = self.module_env.getIdent(tag_name);

    // Allocate a copy with first char lowercased
    var fn_name = try self.allocator.alloc(u8, tag_str.len);
    defer self.allocator.free(fn_name);
    @memcpy(fn_name, tag_str);

    if (fn_name.len > 0 and fn_name[0] >= 'A' and fn_name[0] <= 'Z') {
        fn_name[0] = fn_name[0] + ('a' - 'A');
    }

    return try self.module_env.insertIdent(base.Ident.for_text(fn_name));
}

/// Create patterns for calling a lifted function in dispatch.
/// Returns the pattern for the lifted function name and the captures pattern.
/// The actual lifted function body is created by LambdaLifter.
fn createLiftedFunctionPatterns(
    self: *Self,
    tag_name: base.Ident.Idx,
    has_captures: bool,
) !struct { fn_pattern: CIR.Pattern.Idx, captures_pattern: ?CIR.Pattern.Idx } {
    // Generate the lowercase function name
    const fn_name = try self.generateLiftedFunctionName(tag_name);

    // Create the pattern for the function (used in dispatch lookups)
    const fn_pattern = try self.module_env.store.addPattern(
        Pattern{ .assign = .{ .ident = fn_name } },
        base.Region.zero(),
    );

    // Build the captures parameter pattern if there are captures
    var captures_pattern: ?CIR.Pattern.Idx = null;
    if (has_captures) {
        // Create a "captures" identifier pattern (used in dispatch match branches)
        const captures_ident = try self.module_env.insertIdent(base.Ident.for_text("captures"));
        captures_pattern = try self.module_env.store.addPattern(
            Pattern{ .assign = .{ .ident = captures_ident } },
            base.Region.zero(),
        );
    }

    return .{ .fn_pattern = fn_pattern, .captures_pattern = captures_pattern };
}

/// Generate a dispatch match expression for a closure call.
///
/// Phase 5: Generates calls to lifted functions instead of inlining bodies.
/// Transforms a call like `f(10)` where `f` is a closure into:
/// ```roc
/// match f {
///     Closure_f_1(captures) => closure_f_1(10, captures)
/// }
/// ```
fn generateDispatchMatch(
    self: *Self,
    closure_var_expr: Expr.Idx,
    closure_info: ClosureInfo,
    call_args: []const Expr.Idx,
) !Expr.Idx {
    // Step 1: Create the captures pattern for matching
    // Use the lifted_captures_pattern if there are captures, otherwise use empty record
    const captures_pattern = if (closure_info.lifted_captures_pattern) |cap_pat|
        cap_pat
    else blk: {
        // No captures - create an empty record destructure pattern
        const destructs_span = try self.module_env.store.recordDestructSpanFrom(
            self.module_env.store.scratchRecordDestructTop(),
        );
        break :blk try self.module_env.store.addPattern(
            Pattern{ .record_destructure = .{ .destructs = destructs_span } },
            base.Region.zero(),
        );
    };

    // Step 2: Create the applied_tag pattern: `Closure_f_1(captures)`
    const pattern_args_start = self.module_env.store.scratchPatternTop();
    try self.module_env.store.addScratchPattern(captures_pattern);
    const pattern_args_span = try self.module_env.store.patternSpanFrom(pattern_args_start);

    const tag_pattern = try self.module_env.store.addPattern(
        Pattern{ .applied_tag = .{
            .name = closure_info.tag_name,
            .args = pattern_args_span,
        } },
        base.Region.zero(),
    );

    // Step 3: Create the body - a call to the lifted function
    // Generate: closure_f_1(arg1, arg2, ..., captures)
    const body_expr = if (closure_info.lifted_fn_pattern) |fn_pattern| blk: {
        // Create a lookup to the lifted function
        const fn_lookup = try self.module_env.store.addExpr(Expr{
            .e_lookup_local = .{ .pattern_idx = fn_pattern },
        }, base.Region.zero());

        // Build the argument list: (original call args..., captures)
        const args_start = self.module_env.store.scratch.?.exprs.top();

        // Add original call arguments
        for (call_args) |arg| {
            try self.module_env.store.scratch.?.exprs.append(arg);
        }

        // Add captures lookup if there are captures
        if (closure_info.lifted_captures_pattern) |cap_pat| {
            const captures_lookup = try self.module_env.store.addExpr(Expr{
                .e_lookup_local = .{ .pattern_idx = cap_pat },
            }, base.Region.zero());
            try self.module_env.store.scratch.?.exprs.append(captures_lookup);
        }

        const args_span = try self.module_env.store.exprSpanFrom(args_start);

        // Create the call expression
        break :blk try self.module_env.store.addExpr(Expr{
            .e_call = .{
                .func = fn_lookup,
                .args = args_span,
                .called_via = .apply,
            },
        }, base.Region.zero());
    } else blk: {
        // Fallback: no lifted function pattern (shouldn't happen)
        // Transform the lambda body to handle nested closures (old behavior)
        break :blk try self.transformExpr(closure_info.lambda_body);
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
/// Phase 5: Generates calls to lifted functions instead of inlining bodies.
/// Transforms a call like `f(10)` where `f` could be one of several closures into:
/// ```roc
/// match f {
///     Closure_add1_1(captures) => closure_add1_1(10, captures),
///     Closure_mul2_2({}) => closure_mul2_2(10),
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
        // Step 1: Create the captures pattern for matching
        const captures_pattern = if (closure_info.lifted_captures_pattern) |cap_pat|
            cap_pat
        else blk: {
            // No captures - create an empty record destructure pattern
            const destructs_span = try self.module_env.store.recordDestructSpanFrom(
                self.module_env.store.scratchRecordDestructTop(),
            );
            break :blk try self.module_env.store.addPattern(
                Pattern{ .record_destructure = .{ .destructs = destructs_span } },
                base.Region.zero(),
            );
        };

        // Step 2: Create applied_tag pattern
        const pattern_args_start = self.module_env.store.scratchPatternTop();
        try self.module_env.store.addScratchPattern(captures_pattern);
        const pattern_args_span = try self.module_env.store.patternSpanFrom(pattern_args_start);

        const tag_pattern = try self.module_env.store.addPattern(
            Pattern{ .applied_tag = .{
                .name = closure_info.tag_name,
                .args = pattern_args_span,
            } },
            base.Region.zero(),
        );

        // Step 3: Create the body - call to lifted function
        const body_expr = if (closure_info.lifted_fn_pattern) |fn_pattern| blk: {
            // Create a lookup to the lifted function
            const fn_lookup = try self.module_env.store.addExpr(Expr{
                .e_lookup_local = .{ .pattern_idx = fn_pattern },
            }, base.Region.zero());

            // Build the argument list: (original call args..., captures)
            const args_start = self.module_env.store.scratch.?.exprs.top();

            // Add original call arguments
            for (call_args) |arg| {
                try self.module_env.store.scratch.?.exprs.append(arg);
            }

            // Add captures lookup if there are captures
            if (closure_info.lifted_captures_pattern) |cap_pat| {
                const captures_lookup = try self.module_env.store.addExpr(Expr{
                    .e_lookup_local = .{ .pattern_idx = cap_pat },
                }, base.Region.zero());
                try self.module_env.store.scratch.?.exprs.append(captures_lookup);
            }

            const args_span = try self.module_env.store.exprSpanFrom(args_start);

            // Create the call expression
            break :blk try self.module_env.store.addExpr(Expr{
                .e_call = .{
                    .func = fn_lookup,
                    .args = args_span,
                    .called_via = .apply,
                },
            }, base.Region.zero());
        } else blk: {
            // Fallback: no lifted function pattern (shouldn't happen)
            break :blk try self.transformExpr(closure_info.lambda_body);
        };

        // Step 4: Create match branch pattern
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
        .e_closure => {
            // Closure with captures - transform to tag and create lambda set
            const transformed = try self.transformClosure(expr_idx, name_hint);
            if (self.closures.get(expr_idx)) |closure_info| {
                var lambda_set = LambdaSet.init();
                try lambda_set.addClosure(self.allocator, closure_info);
                return .{ .expr = transformed, .lambda_set = lambda_set };
            }
            // If no closure_info, it was converted to a pure lambda (all captures were top-level)
            return .{ .expr = transformed, .lambda_set = null };
        },
        .e_lambda => |lambda| {
            // Pure lambda (no captures) - transform body and track what it returns
            // Use transformExprWithLambdaSet to collect the body's lambda set
            const body_result = try self.transformExprWithLambdaSet(lambda.body, name_hint);

            // If body is unchanged, return original lambda
            if (body_result.expr == lambda.body) {
                return .{ .expr = expr_idx, .lambda_set = null };
            }

            // Create new lambda with transformed body
            const new_lambda = try self.module_env.store.addExpr(Expr{
                .e_lambda = .{
                    .args = lambda.args,
                    .body = body_result.expr,
                },
            }, base.Region.zero());

            // If the body returns closures, track this so calls to this lambda return those closures
            if (body_result.lambda_set) |body_lambda_set| {
                // Clone the lambda_set for storage since we're consuming the original
                const cloned = try body_lambda_set.clone(self.allocator);
                try self.lambda_return_sets.put(new_lambda, cloned);
                // Free the original since we're returning null (not passing ownership to caller)
                var to_free = body_lambda_set;
                to_free.deinit(self.allocator);
            }

            return .{ .expr = new_lambda, .lambda_set = null };
        },
        .e_if => |if_expr| {
            // If expression - collect closures from all branches
            // We need to handle the case where some branches are closures and others are pure lambdas
            var lambda_set = LambdaSet.init();

            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);

            // First pass: transform all branches and collect lambda sets
            // Store transformed branches temporarily
            const BranchInfo = struct { cond: Expr.Idx, body: Expr.Idx, has_lambda_set: bool };
            var transformed_branches = std.ArrayList(BranchInfo).empty;
            defer transformed_branches.deinit(self.allocator);

            var any_branch_has_lambda_set = false;

            for (branches) |branch_idx| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                const new_cond = try self.transformExpr(branch.cond);

                // Transform branch body and collect its lambda set
                var body_result = try self.transformExprWithLambdaSet(branch.body, name_hint);
                if (body_result.lambda_set) |*branch_lambda_set| {
                    try lambda_set.merge(self.allocator, branch_lambda_set);
                    branch_lambda_set.deinit(self.allocator);
                    any_branch_has_lambda_set = true;
                }

                try transformed_branches.append(self.allocator, .{
                    .cond = new_cond,
                    .body = body_result.expr,
                    .has_lambda_set = body_result.lambda_set != null,
                });
            }

            // Transform else branch and collect its lambda set
            var else_result = try self.transformExprWithLambdaSet(if_expr.final_else, name_hint);
            const else_has_lambda_set = else_result.lambda_set != null;
            if (else_result.lambda_set) |*else_lambda_set| {
                try lambda_set.merge(self.allocator, else_lambda_set);
                else_lambda_set.deinit(self.allocator);
                any_branch_has_lambda_set = true;
            }

            // Second pass: if any branch has a lambda set, convert pure lambdas to closure tags
            const branch_start = self.module_env.store.scratch.?.if_branches.top();

            if (any_branch_has_lambda_set) {
                for (transformed_branches.items) |*tb| {
                    var final_body = tb.body;
                    if (!tb.has_lambda_set) {
                        // Check if this is a pure lambda that needs to be converted to a closure tag
                        const body_expr = self.module_env.store.getExpr(tb.body);
                        if (body_expr == .e_lambda) {
                            // Convert pure lambda to closure tag with empty captures
                            const lambda = body_expr.e_lambda;
                            const tag_name = try self.generateClosureTagName(name_hint);

                            // Create empty record for captures
                            const empty_record = try self.module_env.store.addExpr(Expr.e_empty_record, base.Region.zero());

                            // Create tag expression with empty record
                            const args_start = self.module_env.store.scratch.?.exprs.top();
                            try self.module_env.store.scratch.?.exprs.append(empty_record);
                            const args_span = try self.module_env.store.exprSpanFrom(args_start);

                            final_body = try self.module_env.store.addExpr(Expr{
                                .e_tag = .{
                                    .name = tag_name,
                                    .args = args_span,
                                },
                            }, base.Region.zero());

                            // Create patterns for calling the lifted function in dispatch
                            const lifted_patterns = try self.createLiftedFunctionPatterns(tag_name, false);

                            // Add to lambda set with the lambda's info
                            try lambda_set.addClosure(self.allocator, ClosureInfo{
                                .tag_name = tag_name,
                                .lambda_body = lambda.body,
                                .lambda_args = lambda.args,
                                .capture_names = std.ArrayList(base.Ident.Idx).empty,
                                .lifted_fn_pattern = lifted_patterns.fn_pattern,
                                .lifted_captures_pattern = lifted_patterns.captures_pattern,
                            });
                        }
                    }

                    const new_branch_idx = try self.module_env.store.addIfBranch(
                        Expr.IfBranch{ .cond = tb.cond, .body = final_body },
                        base.Region.zero(),
                    );
                    try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
                }

                // Handle else branch - convert pure lambda if needed
                var final_else = else_result.expr;
                if (!else_has_lambda_set) {
                    const else_expr = self.module_env.store.getExpr(else_result.expr);
                    if (else_expr == .e_lambda) {
                        const lambda = else_expr.e_lambda;
                        const tag_name = try self.generateClosureTagName(name_hint);

                        // Create empty record for captures
                        const empty_record = try self.module_env.store.addExpr(Expr.e_empty_record, base.Region.zero());

                        // Create tag expression with empty record
                        const args_start = self.module_env.store.scratch.?.exprs.top();
                        try self.module_env.store.scratch.?.exprs.append(empty_record);
                        const args_span = try self.module_env.store.exprSpanFrom(args_start);

                        final_else = try self.module_env.store.addExpr(Expr{
                            .e_tag = .{
                                .name = tag_name,
                                .args = args_span,
                            },
                        }, base.Region.zero());

                        // Create patterns for calling the lifted function in dispatch
                        const lifted_patterns = try self.createLiftedFunctionPatterns(tag_name, false);

                        // Add to lambda set with the lambda's info
                        try lambda_set.addClosure(self.allocator, ClosureInfo{
                            .tag_name = tag_name,
                            .lambda_body = lambda.body,
                            .lambda_args = lambda.args,
                            .capture_names = std.ArrayList(base.Ident.Idx).empty,
                            .lifted_fn_pattern = lifted_patterns.fn_pattern,
                            .lifted_captures_pattern = lifted_patterns.captures_pattern,
                        });
                    }
                }

                const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);

                const new_if = try self.module_env.store.addExpr(Expr{
                    .e_if = .{
                        .branches = new_branches_span,
                        .final_else = final_else,
                    },
                }, base.Region.zero());

                return .{ .expr = new_if, .lambda_set = lambda_set };
            } else {
                // No closures in any branch, just create the if expression normally
                for (transformed_branches.items) |tb| {
                    const new_branch_idx = try self.module_env.store.addIfBranch(
                        Expr.IfBranch{ .cond = tb.cond, .body = tb.body },
                        base.Region.zero(),
                    );
                    try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
                }

                const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);

                const new_if = try self.module_env.store.addExpr(Expr{
                    .e_if = .{
                        .branches = new_branches_span,
                        .final_else = else_result.expr,
                    },
                }, base.Region.zero());

                return .{ .expr = new_if, .lambda_set = null };
            }
        },
        .e_call => |call| {
            // Call expression - transform and check if calling a function that returns closures
            const transformed = try self.transformExpr(expr_idx);

            // Check if the function is a local variable that returns closures when called
            const func_expr = self.module_env.store.getExpr(call.func);
            switch (func_expr) {
                .e_lookup_local => |lookup| {
                    // Check if this pattern has a lambda return set (calling a lambda that returns closures)
                    if (self.pattern_lambda_return_sets.get(lookup.pattern_idx)) |return_set| {
                        // Clone the return set for this call's result
                        const cloned = try return_set.clone(self.allocator);
                        return .{ .expr = transformed, .lambda_set = cloned };
                    }
                },
                else => {},
            }

            return .{ .expr = transformed, .lambda_set = null };
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

            // Get captures
            const captures = self.module_env.store.sliceCaptures(closure.captures);

            // Build capture record fields, filtering out top-level patterns
            // (top-level constants don't need to be captured since they're always in scope)
            const scratch_top = self.module_env.store.scratch.?.record_fields.top();

            var capture_names = std.ArrayList(base.Ident.Idx).empty;
            var non_toplevel_capture_count: usize = 0;

            for (captures) |capture_idx| {
                const capture = self.module_env.store.getCapture(capture_idx);

                // Skip top-level patterns - they don't need to be captured
                if (self.isTopLevel(capture.pattern_idx)) {
                    continue;
                }

                non_toplevel_capture_count += 1;

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

            // If all captures were top-level, this is effectively a pure lambda
            // But we still need to transform its body (it might contain nested closures)
            if (non_toplevel_capture_count == 0) {
                capture_names.deinit(self.allocator);

                // Transform the lambda's body using transformExprWithLambdaSet to track
                // what closures the body returns (important for nested closures)
                const body_result = try self.transformExprWithLambdaSet(lambda.body, binding_name_hint);
                if (body_result.expr == lambda.body) {
                    // Body unchanged, return original lambda
                    return closure.lambda_idx;
                }

                // Create new lambda with transformed body
                const new_lambda = try self.module_env.store.addExpr(Expr{
                    .e_lambda = .{
                        .args = lambda.args,
                        .body = body_result.expr,
                    },
                }, base.Region.zero());

                // If the body returns closures, track this so calls to this lambda return those closures
                if (body_result.lambda_set) |body_lambda_set| {
                    try self.lambda_return_sets.put(new_lambda, body_lambda_set);
                }

                return new_lambda;
            }

            // Generate tag name (only if we have real captures)
            const tag_name = try self.generateClosureTagName(binding_name_hint);

            // Create the record expression
            const fields_span = try self.module_env.store.recordFieldSpanFrom(scratch_top);

            const record_expr = try self.module_env.store.addExpr(Expr{
                .e_record = .{ .fields = fields_span, .ext = null },
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

            // Create patterns for calling the lifted function in dispatch
            const lifted_patterns = try self.createLiftedFunctionPatterns(
                tag_name,
                capture_names.items.len > 0,
            );

            // Store closure info for dispatch function generation
            try self.closures.put(closure_expr_idx, ClosureInfo{
                .tag_name = tag_name,
                .lambda_body = lambda.body,
                .lambda_args = lambda.args,
                .capture_names = capture_names,
                .lifted_fn_pattern = lifted_patterns.fn_pattern,
                .lifted_captures_pattern = lifted_patterns.captures_pattern,
            });

            return tag_expr;
        },
        .e_lambda => {
            // Pure lambda (no captures) - leave unchanged, no transformation needed
            return closure_expr_idx;
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
        .e_lambda => |lambda| {
            // Pure lambda (no captures) - transform the body to handle nested closures
            const transformed_body = try self.transformExpr(lambda.body);

            // If body is unchanged, return original lambda
            if (transformed_body == lambda.body) {
                return expr_idx;
            }

            // Create new lambda with transformed body
            return try self.module_env.store.addExpr(Expr{
                .e_lambda = .{
                    .args = lambda.args,
                    .body = transformed_body,
                },
            }, base.Region.zero());
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

            // Return original if unchanged
            if (new_lhs == binop.lhs and new_rhs == binop.rhs) {
                return expr_idx;
            }

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

    try testing.expectEqualStrings("Closure_addX_1", tag_str);
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

    try testing.expectEqualStrings("Closure_1", tag_str);
}
