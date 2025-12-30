//! Lambda Lifter
//!
//! Extracts closure bodies to top-level function definitions.
//! Works in conjunction with `ClosureTransformer` to implement Cor-style defunctionalization.
//!
//! ## Pipeline Overview
//!
//! 1. `ClosureTransformer` transforms closures to tags with capture records
//! 2. `LambdaLifter` (this module) extracts the lambda bodies to top-level functions
//! 3. Dispatch at call sites invokes these lifted functions
//!
//! ## Example
//!
//! Before transformation:
//! ```roc
//! make_adder = |x| |y| x + y
//! add5 = make_adder(5)
//! result = add5(10)
//! ```
//!
//! After transformation (internal representation):
//! ```roc
//! c1_make_adder = |y, captures| captures.x + y
//!
//! make_adder = |x| #1_make_adder({ x: x })
//! add5 = make_adder(5)
//! result = match add5 {
//!     #1_make_adder(captures) => c1_make_adder(10, captures)
//! }
//! ```
//!
//! When emitted by RocEmitter, `#` becomes `C`, so `#1_make_adder` prints as `C1_make_adder`.
//!
//! ## Implementation Notes
//!
//! - Each closure's lambda body becomes a top-level function with an extra `captures` parameter
//! - Captured variable references are replaced with `captures.field_name` accesses
//! - Pure lambdas (no captures) can also be lifted when they appear in mixed closure contexts

const std = @import("std");
const base = @import("base");
const types = @import("types");
const can = @import("can");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Expr = CIR.Expr;
const Pattern = can.CIR.Pattern;

const Self = @This();

/// A lifted function definition.
/// Represents a closure that has been extracted to a top-level function.
pub const LiftedFunction = struct {
    /// The function name (lowercase version of closure tag, e.g., "#1_addX" -> "c1_addX")
    name: base.Ident.Idx,
    /// The original lambda arguments
    args: CIR.Pattern.Span,
    /// The captures parameter pattern (record destructure), or null if no captures
    captures_pattern: ?CIR.Pattern.Idx,
    /// The transformed function body (with captures replaced by record field accesses)
    body: Expr.Idx,
    /// The function's return type variable
    ret_var: types.Var,
    /// The original closure expression index (for reference), or null for pure lambdas
    original_closure: ?Expr.Idx,
};

/// The allocator for intermediate allocations
allocator: std.mem.Allocator,

/// The module environment containing the CIR (mutable for adding new expressions)
module_env: *ModuleEnv,

/// Lifted functions collected during the lifting pass
lifted_functions: std.ArrayList(LiftedFunction),

/// Map from original closure expression index to its lifted function name
closure_to_function: std.AutoHashMap(Expr.Idx, base.Ident.Idx),

/// Map from captured pattern index to the corresponding captures record field access.
/// Used during body transformation to replace captures lookups.
capture_replacements: std.AutoHashMap(CIR.Pattern.Idx, Expr.Idx),

/// Set of top-level pattern indices (these don't need to be captured since they're always in scope)
/// This is a reference to the ClosureTransformer's top_level_patterns set.
top_level_patterns: *const std.AutoHashMap(CIR.Pattern.Idx, void),

/// Initialize the lambda lifter
pub fn init(allocator: std.mem.Allocator, module_env: *ModuleEnv, top_level_patterns: *const std.AutoHashMap(CIR.Pattern.Idx, void)) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .lifted_functions = std.ArrayList(LiftedFunction).empty,
        .closure_to_function = std.AutoHashMap(Expr.Idx, base.Ident.Idx).init(allocator),
        .capture_replacements = std.AutoHashMap(CIR.Pattern.Idx, Expr.Idx).init(allocator),
        .top_level_patterns = top_level_patterns,
    };
}

/// Check if a pattern is a top-level definition
fn isTopLevel(self: *const Self, pattern_idx: CIR.Pattern.Idx) bool {
    return self.top_level_patterns.contains(pattern_idx);
}

/// Free resources
pub fn deinit(self: *Self) void {
    self.lifted_functions.deinit(self.allocator);
    self.closure_to_function.deinit();
    self.capture_replacements.deinit();
}

/// Lift a closure to a top-level function.
///
/// This extracts the closure's lambda body and creates a new top-level function
/// that takes the closure's captures as an additional parameter (as a record).
///
/// The original closure expression is transformed to just construct the closure tag
/// with its captures record.
pub fn liftClosure(
    self: *Self,
    closure_idx: Expr.Idx,
    tag_name: base.Ident.Idx,
) !void {
    const expr = self.module_env.store.getExpr(closure_idx);

    switch (expr) {
        .e_closure => |closure| {
            const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
            const lambda = switch (lambda_expr) {
                .e_lambda => |l| l,
                else => return, // Not a lambda, skip
            };

            // Get captures
            const captures = self.module_env.store.sliceCaptures(closure.captures);

            // Build a simple "captures" identifier pattern for the captures parameter
            // This gives us `|arg, captures| captures.x + captures.y` instead of
            // `|arg, { x, y }| { x, y }.x + { x, y }.y`
            const captures_pattern = if (captures.len > 0)
                try self.buildSimpleCapturesPattern()
            else
                null;

            // Build capture replacements map for body transformation
            self.capture_replacements.clearRetainingCapacity();
            if (captures_pattern) |cap_pat| {
                try self.buildCaptureReplacements(captures, cap_pat);
            }

            // Transform the lambda body to use captures record field accesses
            const transformed_body = try self.transformBodyWithCaptures(lambda.body);

            // Create a fresh type variable for the return type
            const ret_var = try self.module_env.types.fresh();

            // Create the lifted function
            const lifted = LiftedFunction{
                .name = tag_name,
                .args = lambda.args,
                .captures_pattern = captures_pattern,
                .body = transformed_body,
                .ret_var = ret_var,
                .original_closure = closure_idx,
            };

            try self.lifted_functions.append(self.allocator, lifted);
            try self.closure_to_function.put(closure_idx, tag_name);
        },
        else => {}, // Not a closure, skip
    }
}

/// Import ClosureInfo from ClosureTransformer
const ClosureTransformer = can.ClosureTransformer;
const ClosureInfo = ClosureTransformer.ClosureInfo;

/// Lift a closure from ClosureInfo directly.
/// This is used for pure lambdas that were converted to closure tags
/// but don't have an e_closure expression.
pub fn liftFromInfo(
    self: *Self,
    info: ClosureInfo,
) !void {
    // Pure lambdas have no captures, so no captures pattern needed
    const has_captures = info.capture_names.items.len > 0;
    const captures_pattern = if (has_captures)
        try self.buildSimpleCapturesPattern()
    else
        null;

    // For pure lambdas, we don't need to transform the body (no captures to replace)
    // For closures with captures, the body transformation is already done
    const body = info.lambda_body;

    // Create a fresh type variable for the return type
    const ret_var = try self.module_env.types.fresh();

    // Create the lifted function
    const lifted = LiftedFunction{
        .name = info.tag_name,
        .args = info.lambda_args,
        .captures_pattern = captures_pattern,
        .body = body,
        .ret_var = ret_var,
        .original_closure = null, // Pure lambdas don't have an original closure
    };

    try self.lifted_functions.append(self.allocator, lifted);
}

/// Build a simple "captures" identifier pattern for the captures parameter.
/// This is cleaner than a record destructure pattern for the lifted function output.
fn buildSimpleCapturesPattern(self: *Self) !CIR.Pattern.Idx {
    // Create the "captures" identifier
    const captures_ident = try self.module_env.insertIdent(base.Ident.for_text("captures"));

    // Create an assign pattern for "captures"
    return try self.module_env.store.addPattern(
        Pattern{ .assign = .{ .ident = captures_ident } },
        base.Region.zero(),
    );
}

/// Build the capture replacements map.
///
/// For each captured variable, create a record field access expression
/// that will replace lookups to that variable in the transformed body.
/// Top-level patterns are skipped since they're always in scope and not in the captures record.
fn buildCaptureReplacements(
    self: *Self,
    captures: []const CIR.Expr.Capture.Idx,
    captures_pattern: CIR.Pattern.Idx,
) !void {
    // Create a lookup to the captures parameter
    const captures_lookup = try self.module_env.store.addExpr(Expr{
        .e_lookup_local = .{ .pattern_idx = captures_pattern },
    }, base.Region.zero());

    for (captures) |capture_idx| {
        const capture = self.module_env.store.getCapture(capture_idx);

        // Skip top-level patterns - they don't need to be captured and are not in the captures record
        if (self.isTopLevel(capture.pattern_idx)) {
            continue;
        }

        // Create a field access: captures.field_name
        const field_access = try self.module_env.store.addExpr(Expr{
            .e_dot_access = .{
                .receiver = captures_lookup,
                .field_name = capture.name,
                .field_name_region = base.Region.zero(),
                .args = null,
            },
        }, base.Region.zero());

        try self.capture_replacements.put(capture.pattern_idx, field_access);
    }
}

/// Transform the lambda body to replace captured variable lookups
/// with accesses to the captures record.
fn transformBodyWithCaptures(
    self: *Self,
    body_idx: Expr.Idx,
) !Expr.Idx {
    const expr = self.module_env.store.getExpr(body_idx);

    switch (expr) {
        .e_lookup_local => |lookup| {
            // Check if this lookup is for a captured variable
            if (self.capture_replacements.get(lookup.pattern_idx)) |replacement| {
                return replacement;
            }
            // Not a captured variable, return as-is
            return body_idx;
        },

        .e_block => |block| {
            // Transform block: handle statements and final expression
            const stmts = self.module_env.store.sliceStatements(block.stmts);
            const stmt_start = self.module_env.store.scratch.?.statements.top();

            for (stmts) |stmt_idx| {
                const stmt = self.module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        const new_expr = try self.transformBodyWithCaptures(decl.expr);
                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl = .{
                                .pattern = decl.pattern,
                                .expr = new_expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    .s_decl_gen => |decl| {
                        const new_expr = try self.transformBodyWithCaptures(decl.expr);
                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl_gen = .{
                                .pattern = decl.pattern,
                                .expr = new_expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    else => {
                        try self.module_env.store.scratch.?.statements.append(stmt_idx);
                    },
                }
            }

            const new_stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);
            const new_final = try self.transformBodyWithCaptures(block.final_expr);

            return try self.module_env.store.addExpr(Expr{
                .e_block = .{
                    .stmts = new_stmts_span,
                    .final_expr = new_final,
                },
            }, base.Region.zero());
        },

        .e_binop => |binop| {
            const new_lhs = try self.transformBodyWithCaptures(binop.lhs);
            const new_rhs = try self.transformBodyWithCaptures(binop.rhs);

            if (new_lhs == binop.lhs and new_rhs == binop.rhs) {
                return body_idx;
            }

            return try self.module_env.store.addExpr(Expr{
                .e_binop = .{
                    .op = binop.op,
                    .lhs = new_lhs,
                    .rhs = new_rhs,
                },
            }, base.Region.zero());
        },

        .e_call => |call| {
            const new_func = try self.transformBodyWithCaptures(call.func);

            const args = self.module_env.store.sliceExpr(call.args);
            const args_start = self.module_env.store.scratch.?.exprs.top();

            for (args) |arg_idx| {
                const new_arg = try self.transformBodyWithCaptures(arg_idx);
                try self.module_env.store.scratch.?.exprs.append(new_arg);
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);

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
                const new_cond = try self.transformBodyWithCaptures(branch.cond);
                const new_body = try self.transformBodyWithCaptures(branch.body);

                const new_branch_idx = try self.module_env.store.addIfBranch(
                    Expr.IfBranch{ .cond = new_cond, .body = new_body },
                    base.Region.zero(),
                );
                try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
            }

            const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);
            const new_else = try self.transformBodyWithCaptures(if_expr.final_else);

            return try self.module_env.store.addExpr(Expr{
                .e_if = .{
                    .branches = new_branches_span,
                    .final_else = new_else,
                },
            }, base.Region.zero());
        },

        .e_lambda => |lambda| {
            // Transform nested lambda body
            const new_body = try self.transformBodyWithCaptures(lambda.body);

            if (new_body == lambda.body) {
                return body_idx;
            }

            return try self.module_env.store.addExpr(Expr{
                .e_lambda = .{
                    .args = lambda.args,
                    .body = new_body,
                },
            }, base.Region.zero());
        },

        .e_closure => |closure| {
            // Transform nested closure's lambda
            const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
            switch (lambda_expr) {
                .e_lambda => |lambda| {
                    const new_body = try self.transformBodyWithCaptures(lambda.body);

                    if (new_body == lambda.body) {
                        return body_idx;
                    }

                    const new_lambda = try self.module_env.store.addExpr(Expr{
                        .e_lambda = .{
                            .args = lambda.args,
                            .body = new_body,
                        },
                    }, base.Region.zero());

                    return try self.module_env.store.addExpr(Expr{
                        .e_closure = .{
                            .lambda_idx = new_lambda,
                            .captures = closure.captures,
                            .tag_name = closure.tag_name,
                        },
                    }, base.Region.zero());
                },
                else => return body_idx,
            }
        },

        .e_unary_minus => |unary| {
            const new_expr = try self.transformBodyWithCaptures(unary.expr);
            if (new_expr == unary.expr) return body_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_unary_minus = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_unary_not => |unary| {
            const new_expr = try self.transformBodyWithCaptures(unary.expr);
            if (new_expr == unary.expr) return body_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_unary_not = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_list => |list| {
            const elems = self.module_env.store.sliceExpr(list.elems);
            const elems_start = self.module_env.store.scratch.?.exprs.top();

            for (elems) |elem_idx| {
                const new_elem = try self.transformBodyWithCaptures(elem_idx);
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
                const new_elem = try self.transformBodyWithCaptures(elem_idx);
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
                const new_value = try self.transformBodyWithCaptures(field.value);

                const new_field = CIR.RecordField{
                    .name = field.name,
                    .value = new_value,
                };
                const new_field_idx = try self.module_env.store.addRecordField(new_field, base.Region.zero());
                try self.module_env.store.scratch.?.record_fields.append(new_field_idx);
            }

            const new_fields_span = try self.module_env.store.recordFieldSpanFrom(fields_start);
            const new_ext = if (record.ext) |ext| try self.transformBodyWithCaptures(ext) else null;

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
                const new_arg = try self.transformBodyWithCaptures(arg_idx);
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

        .e_dot_access => |dot| {
            const new_receiver = try self.transformBodyWithCaptures(dot.receiver);
            const new_args = if (dot.args) |args_span| blk: {
                const args = self.module_env.store.sliceExpr(args_span);
                const args_start = self.module_env.store.scratch.?.exprs.top();

                for (args) |arg_idx| {
                    const new_arg = try self.transformBodyWithCaptures(arg_idx);
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

        .e_return => |ret| {
            const new_expr = try self.transformBodyWithCaptures(ret.expr);
            if (new_expr == ret.expr) return body_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_return = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_dbg => |dbg| {
            const new_expr = try self.transformBodyWithCaptures(dbg.expr);
            if (new_expr == dbg.expr) return body_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_dbg = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_expect => |expect| {
            const new_body = try self.transformBodyWithCaptures(expect.body);
            if (new_body == expect.body) return body_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_expect = .{ .body = new_body },
            }, base.Region.zero());
        },

        // Pass through expressions that don't contain sub-expressions needing transformation
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_str_segment,
        .e_str,
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
        .e_crash,
        => return body_idx,

        .e_match => |match| {
            const new_cond = try self.transformBodyWithCaptures(match.cond);
            // Note: match branches would need deeper transformation
            // For now, pass through as-is (matching ClosureTransformer behavior)
            return try self.module_env.store.addExpr(Expr{
                .e_match = .{
                    .cond = new_cond,
                    .branches = match.branches,
                    .exhaustive = match.exhaustive,
                },
            }, base.Region.zero());
        },

        .e_nominal => |nominal| {
            const new_backing = try self.transformBodyWithCaptures(nominal.backing_expr);
            if (new_backing == nominal.backing_expr) return body_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_nominal = .{
                    .nominal_type_decl = nominal.nominal_type_decl,
                    .backing_expr = new_backing,
                    .backing_type = nominal.backing_type,
                },
            }, base.Region.zero());
        },

        .e_nominal_external => |nominal| {
            const new_backing = try self.transformBodyWithCaptures(nominal.backing_expr);
            if (new_backing == nominal.backing_expr) return body_idx;
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
            const new_expr = try self.transformBodyWithCaptures(for_expr.expr);
            const new_body = try self.transformBodyWithCaptures(for_expr.body);
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

/// Get all lifted functions
pub fn getLiftedFunctions(self: *const Self) []const LiftedFunction {
    return self.lifted_functions.items;
}

/// Check if a closure was lifted
pub fn wasLifted(self: *const Self, closure_idx: Expr.Idx) bool {
    return self.closure_to_function.contains(closure_idx);
}

/// Get the lifted function name for a closure
pub fn getLiftedFunctionName(self: *const Self, closure_idx: Expr.Idx) ?base.Ident.Idx {
    return self.closure_to_function.get(closure_idx);
}

// Tests

const testing = std.testing;

test "LambdaLifter: init and deinit" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    // Create an empty top-level patterns set for testing
    var top_level_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(allocator);
    defer top_level_patterns.deinit();

    var lifter = Self.init(allocator, module_env, &top_level_patterns);
    defer lifter.deinit();

    try testing.expectEqual(@as(usize, 0), lifter.lifted_functions.items.len);
}

test "LambdaLifter: getLiftedFunctions returns empty on fresh lifter" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    // Create an empty top-level patterns set for testing
    var top_level_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(allocator);
    defer top_level_patterns.deinit();

    var lifter = Self.init(allocator, module_env, &top_level_patterns);
    defer lifter.deinit();

    const functions = lifter.getLiftedFunctions();
    try testing.expectEqual(@as(usize, 0), functions.len);
}
