//! Roc Code Emitter
//!
//! Converts the Canonical IR (CIR) to valid Roc source code.
//! This is primarily used for testing the monomorphization pipeline -
//! we can emit monomorphic Roc code and verify it produces the same results
//! as the original polymorphic code.
//!
//! The emitter walks the CIR expression tree and writes corresponding Roc syntax.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");

const i128h = builtins.compiler_rt_128;

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const PatternMod = @import("Pattern.zig");
const Expr = CIR.Expr;
const Pattern = PatternMod.Pattern;

const Self = @This();

/// The allocator used for intermediate allocations
allocator: std.mem.Allocator,

/// The module environment containing the CIR
module_env: *const ModuleEnv,

/// The output buffer where Roc code is written
output: std.ArrayList(u8),

/// Current indentation level
indent_level: u32,

/// Names currently in scope (to detect shadowing)
names_in_scope: std.StringHashMap(void),

/// Renames for captures that would shadow (pattern_idx -> renamed string)
capture_renames: std.AutoHashMap(PatternMod.Pattern.Idx, []const u8),

/// Counter for generating unique names
rename_counter: u32,

/// Initialize a new Emitter
pub fn init(allocator: std.mem.Allocator, module_env: *const ModuleEnv) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .output = std.ArrayList(u8).empty,
        .indent_level = 0,
        .names_in_scope = std.StringHashMap(void).init(allocator),
        .capture_renames = std.AutoHashMap(PatternMod.Pattern.Idx, []const u8).init(allocator),
        .rename_counter = 0,
    };
}

/// Free resources used by the emitter
pub fn deinit(self: *Self) void {
    self.output.deinit(self.allocator);
    self.names_in_scope.deinit();
    // Free the allocated rename strings
    var iter = self.capture_renames.valueIterator();
    while (iter.next()) |rename| {
        self.allocator.free(rename.*);
    }
    self.capture_renames.deinit();
}

/// Get the emitted Roc source code
pub fn getOutput(self: *const Self) []const u8 {
    return self.output.items;
}

/// Reset the emitter for reuse
pub fn reset(self: *Self) void {
    self.output.clearRetainingCapacity();
    self.indent_level = 0;
    self.names_in_scope.clearRetainingCapacity();
    // Free the allocated rename strings
    var iter = self.capture_renames.valueIterator();
    while (iter.next()) |rename| {
        self.allocator.free(rename.*);
    }
    self.capture_renames.clearRetainingCapacity();
    self.rename_counter = 0;
}

/// Emit an expression as Roc source code
pub fn emitExpr(self: *Self, expr_idx: Expr.Idx) !void {
    const expr = self.module_env.store.getExpr(expr_idx);
    try self.emitExprValue(expr);
}

/// Emit a pattern as Roc source code
pub fn emitPattern(self: *Self, pattern_idx: CIR.Pattern.Idx) !void {
    const pattern = self.module_env.store.getPattern(pattern_idx);
    try self.emitPatternValue(pattern);
}

/// Emit a pattern, checking for shadowing and generating unique names
fn emitPatternWithShadowCheck(self: *Self, pattern_idx: CIR.Pattern.Idx) !void {
    const pattern = self.module_env.store.getPattern(pattern_idx);

    // Only handle assign patterns for shadowing (other patterns don't introduce names)
    if (pattern == .assign) {
        const name = self.module_env.getIdent(pattern.assign.ident);

        if (self.names_in_scope.contains(name)) {
            // Generate a unique name
            const unique_name = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ name, self.rename_counter });
            self.rename_counter += 1;

            // Store the rename mapping
            try self.capture_renames.put(pattern_idx, unique_name);
            try self.names_in_scope.put(unique_name, {});
            try self.emitIdent(unique_name);
        } else {
            try self.names_in_scope.put(name, {});
            try self.emitIdent(name);
        }
    } else {
        // For other pattern types, just emit normally
        try self.emitPatternValue(pattern);
    }
}

/// Emit a binop operand, wrapping in parens only if needed for precedence
fn emitBinopOperand(self: *Self, expr_idx: Expr.Idx, outer_op: Expr.Binop.Op) !void {
    const expr = self.module_env.store.getExpr(expr_idx);
    if (expr == .e_binop) {
        const inner_op = expr.e_binop.op;
        // Only add parens if inner op has lower precedence than outer op
        if (binopPrecedence(inner_op) < binopPrecedence(outer_op)) {
            try self.write("(");
            try self.emitExprValue(expr);
            try self.write(")");
        } else {
            try self.emitExprValue(expr);
        }
    } else {
        try self.emitExprValue(expr);
    }
}

/// Returns precedence level for a binary operator (higher = binds tighter)
fn binopPrecedence(op: Expr.Binop.Op) u8 {
    return switch (op) {
        .@"or" => 1,
        .@"and" => 2,
        .eq, .ne => 3,
        .lt, .gt, .le, .ge => 4,
        .add, .sub => 5,
        .mul, .div, .div_trunc, .rem => 6,
    };
}

const EmitError = std.mem.Allocator.Error || std.fmt.BufPrintError;

fn emitExprValue(self: *Self, expr: Expr) EmitError!void {
    switch (expr) {
        .e_num => |num| {
            try self.emitIntValue(num.value);
        },
        .e_frac_f32 => |frac| {
            try self.writer().print("{d}f32", .{frac.value});
        },
        .e_frac_f64 => |frac| {
            try self.writer().print("{d}f64", .{frac.value});
        },
        .e_dec => |dec| {
            // Dec is stored scaled by 10^18, need to emit as decimal
            const value = dec.value.num;
            const scale: i128 = 1_000_000_000_000_000_000;
            const whole = i128h.divTrunc_i128(value, scale);
            const frac_part = i128h.rem_u128(@abs(value), @as(u128, @intCast(scale)));
            if (frac_part == 0) {
                var str_buf: [40]u8 = undefined;
                try self.writer().writeAll(i128h.i128_to_str(&str_buf, whole).str);
            } else {
                var str_buf: [40]u8 = undefined;
                try self.writer().writeAll(i128h.i128_to_str(&str_buf, whole).str);
                try self.writer().writeAll(".");
                // Format frac_part with leading zeros (18 digits)
                var frac_buf: [40]u8 = undefined;
                const frac_str = i128h.u128_to_str(&frac_buf, frac_part).str;
                // Pad with leading zeros to 18 digits
                var pad: usize = 18 - frac_str.len;
                while (pad > 0) : (pad -= 1) {
                    try self.writer().writeAll("0");
                }
                try self.writer().writeAll(frac_str);
            }
        },
        .e_dec_small => |small| {
            const numerator = small.value.numerator;
            const power = small.value.denominator_power_of_ten;
            if (power == 0) {
                try self.writer().print("{}", .{numerator});
            } else {
                // Convert to decimal string
                var divisor: i32 = 1;
                for (0..power) |_| {
                    divisor *= 10;
                }
                const whole = @divTrunc(numerator, @as(i16, @intCast(divisor)));
                const frac_part = @mod(@abs(numerator), @as(u16, @intCast(divisor)));
                try self.writer().print("{}.{}", .{ whole, frac_part });
            }
        },
        .e_typed_int => |typed| {
            try self.emitIntValue(typed.value);
            const type_name = self.module_env.getIdent(typed.type_name);
            try self.writer().print(".{s}", .{type_name});
        },
        .e_typed_frac => |typed| {
            // Emit as decimal and add type suffix
            const value = typed.value.toI128();
            const scale: i128 = 1_000_000_000_000_000_000;
            const whole = i128h.divTrunc_i128(value, scale);
            const frac_part = i128h.rem_u128(@abs(value), @as(u128, @intCast(scale)));
            if (frac_part == 0) {
                try self.writer().print("{d}.0", .{whole});
            } else {
                try self.writer().print("{d}.{d:0>18}", .{ whole, frac_part });
            }
            const type_name = self.module_env.getIdent(typed.type_name);
            try self.writer().print(".{s}", .{type_name});
        },
        .e_str_segment => |seg| {
            const text = self.module_env.common.getString(seg.literal);
            try self.writer().print("\"{s}\"", .{text});
        },
        .e_str => |str| {
            // Multi-segment string
            const segments = self.module_env.store.sliceExpr(str.span);
            for (segments) |seg_idx| {
                try self.emitExpr(seg_idx);
            }
        },
        .e_lookup_local => |lookup| {
            // Check if this lookup refers to a renamed capture
            if (self.capture_renames.get(lookup.pattern_idx)) |renamed| {
                try self.emitIdent(renamed);
            } else {
                const pattern = self.module_env.store.getPattern(lookup.pattern_idx);
                try self.emitPatternValue(pattern);
            }
        },
        .e_lookup_external => |ext| {
            // Get the identifier name from the ident_idx
            const ident_text = self.module_env.getIdent(ext.ident_idx);
            try self.emitIdent(ident_text);
        },
        .e_lookup_pending => {
            // Pending lookups must be resolved before emission
            unreachable;
        },
        .e_list => |list| {
            try self.write("[");
            const elems = self.module_env.store.sliceExpr(list.elems);
            for (elems, 0..) |elem_idx, i| {
                if (i > 0) try self.write(", ");
                try self.emitExpr(elem_idx);
            }
            try self.write("]");
        },
        .e_empty_list => {
            try self.write("[]");
        },
        .e_tuple => |tuple| {
            try self.write("(");
            const elems = self.module_env.store.sliceExpr(tuple.elems);
            for (elems, 0..) |elem_idx, i| {
                if (i > 0) try self.write(", ");
                try self.emitExpr(elem_idx);
            }
            try self.write(")");
        },
        .e_tuple_access => |tuple_access| {
            try self.emitExpr(tuple_access.tuple);
            try self.writer().print(".{d}", .{tuple_access.elem_index});
        },
        .e_if => |if_expr| {
            const branch_indices = self.module_env.store.sliceIfBranches(if_expr.branches);
            for (branch_indices, 0..) |branch_idx, i| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                if (i > 0) {
                    try self.write(" else if (");
                } else {
                    try self.write("if (");
                }
                try self.emitExpr(branch.cond);
                try self.write(") ");
                try self.emitExpr(branch.body);
            }
            try self.write(" else ");
            try self.emitExpr(if_expr.final_else);
        },
        .e_call => |call| {
            try self.emitExpr(call.func);
            try self.write("(");
            const args = self.module_env.store.sliceExpr(call.args);
            for (args, 0..) |arg_idx, i| {
                if (i > 0) try self.write(", ");
                try self.emitExpr(arg_idx);
            }
            try self.write(")");
        },
        .e_record => |record| {
            try self.write("{ ");
            const field_indices = self.module_env.store.sliceRecordFields(record.fields);
            for (field_indices, 0..) |field_idx, i| {
                const field = self.module_env.store.getRecordField(field_idx);
                if (i > 0) try self.write(", ");
                const name = self.module_env.getIdent(field.name);

                // Check if we can use shorthand syntax { x, y } instead of { x: x, y: y }
                // NOTE: Only use shorthand for records with multiple fields!
                // Single-field { x } would be parsed as a block, not a record shorthand.
                const field_value = self.module_env.store.getExpr(field.value);
                const use_shorthand = if (field_indices.len > 1 and field_value == .e_lookup_local) blk: {
                    const lookup_pattern = self.module_env.store.getPattern(field_value.e_lookup_local.pattern_idx);
                    if (lookup_pattern == .assign) {
                        const lookup_name = self.module_env.getIdent(lookup_pattern.assign.ident);
                        break :blk std.mem.eql(u8, name, lookup_name);
                    }
                    break :blk false;
                } else false;

                if (use_shorthand) {
                    try self.write(name);
                } else {
                    try self.writer().print("{s}: ", .{name});
                    try self.emitExpr(field.value);
                }
            }
            if (record.ext) |ext_idx| {
                if (field_indices.len > 0) try self.write(", ");
                try self.write("..");
                try self.emitExpr(ext_idx);
            }
            try self.write(" }");
        },
        .e_empty_record => {
            try self.write("{}");
        },
        .e_block => |block| {
            try self.write("{\n");
            self.indent_level += 1;

            // Emit statements
            const stmts = self.module_env.store.sliceStatements(block.stmts);
            for (stmts) |stmt_idx| {
                try self.emitIndent();
                try self.emitStatement(stmt_idx);
                try self.write("\n");
            }

            // Emit final expression
            try self.emitIndent();
            try self.emitExpr(block.final_expr);
            try self.write("\n");

            self.indent_level -= 1;
            try self.emitIndent();
            try self.write("}");
        },
        .e_tag => |tag| {
            const name = self.module_env.getIdent(tag.name);
            try self.emitTagName(name);
            const args = self.module_env.store.sliceExpr(tag.args);
            if (args.len > 0) {
                try self.write("(");
                for (args, 0..) |arg_idx, i| {
                    if (i > 0) try self.write(", ");
                    try self.emitExpr(arg_idx);
                }
                try self.write(")");
            }
        },
        .e_zero_argument_tag => |tag| {
            const name = self.module_env.getIdent(tag.name);
            try self.emitTagName(name);
        },
        .e_closure => |closure| {
            // Emit closure as a lambda with non-top-level captures as leading arguments
            // e.g., |y| x + y with capture x becomes |x, y| x + y (if x is local)
            // but top-level captures are not lifted (they're always in scope)
            // Handle shadowing by generating unique names for captures
            const lambda = self.module_env.store.getExpr(closure.lambda_idx);
            std.debug.assert(lambda == .e_lambda);

            try self.write("|");

            // First emit non-top-level captures as arguments, renaming if they would shadow
            const captures = self.module_env.store.sliceCaptures(closure.captures);
            var emitted_captures: u32 = 0;
            for (captures) |capture_idx| {
                const capture = self.module_env.store.getCapture(capture_idx);

                // Skip top-level captures - they're always in scope
                if (self.isTopLevelPattern(capture.pattern_idx)) continue;

                if (emitted_captures > 0) try self.write(", ");
                emitted_captures += 1;

                const capture_name = self.module_env.getIdent(capture.name);

                // Check if this name would shadow an existing name
                if (self.names_in_scope.contains(capture_name)) {
                    // Generate a unique name
                    const unique_name = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ capture_name, self.rename_counter });
                    self.rename_counter += 1;

                    // Store the rename mapping
                    try self.capture_renames.put(capture.pattern_idx, unique_name);
                    try self.names_in_scope.put(unique_name, {});
                    try self.write(unique_name);
                } else {
                    // No shadowing, use original name
                    try self.names_in_scope.put(capture_name, {});
                    try self.write(capture_name);
                }
            }

            // Then emit the lambda's own arguments (also check for shadowing)
            const args = self.module_env.store.slicePatterns(lambda.e_lambda.args);
            for (args, 0..) |arg_idx, i| {
                if (emitted_captures > 0 or i > 0) try self.write(", ");
                try self.emitPatternWithShadowCheck(arg_idx);
            }

            try self.write("| ");
            try self.emitExpr(lambda.e_lambda.body);
        },
        .e_lambda => |lambda| {
            try self.write("|");
            const args = self.module_env.store.slicePatterns(lambda.args);
            for (args, 0..) |arg_idx, i| {
                if (i > 0) try self.write(", ");
                try self.emitPattern(arg_idx);
            }
            try self.write("| ");
            try self.emitExpr(lambda.body);
        },
        .e_binop => |binop| {
            // Wrap nested binops in parens only when precedence requires it
            try self.emitBinopOperand(binop.lhs, binop.op);
            try self.write(" ");
            try self.write(binopToStr(binop.op));
            try self.write(" ");
            try self.emitBinopOperand(binop.rhs, binop.op);
        },
        .e_unary_minus => |unary| {
            try self.write("-");
            try self.emitExpr(unary.expr);
        },
        .e_unary_not => |unary| {
            try self.write("!");
            try self.emitExpr(unary.expr);
        },
        .e_dot_access => |dot| {
            try self.emitExpr(dot.receiver);
            try self.write(".");
            const field_name = self.module_env.getIdent(dot.field_name);
            try self.write(field_name);
            if (dot.args) |args_span| {
                try self.write("(");
                const args = self.module_env.store.sliceExpr(args_span);
                for (args, 0..) |arg_idx, i| {
                    if (i > 0) try self.write(", ");
                    try self.emitExpr(arg_idx);
                }
                try self.write(")");
            }
        },
        .e_runtime_error => {
            try self.write("<runtime_error>");
        },
        .e_crash => |crash| {
            const msg = self.module_env.common.getString(crash.msg);
            try self.writer().print("crash \"{s}\"", .{msg});
        },
        .e_dbg => |dbg| {
            try self.write("dbg ");
            try self.emitExpr(dbg.expr);
        },
        .e_expect => |expect| {
            try self.write("expect ");
            try self.emitExpr(expect.body);
        },
        .e_ellipsis => {
            try self.write("...");
        },
        .e_anno_only => {
            try self.write("<anno_only>");
        },
        .e_return => |ret| {
            try self.write("return ");
            try self.emitExpr(ret.expr);
        },
        .e_match => |match| {
            try self.write("match ");
            try self.emitExpr(match.cond);
            try self.write(" {\n");
            self.indent_level += 1;
            const branch_indices = self.module_env.store.sliceMatchBranches(match.branches);
            for (branch_indices) |branch_idx| {
                const branch = self.module_env.store.getMatchBranch(branch_idx);
                try self.emitIndent();
                // Emit patterns
                const pattern_indices = self.module_env.store.sliceMatchBranchPatterns(branch.patterns);
                for (pattern_indices, 0..) |pat_entry_idx, i| {
                    const pat_entry = self.module_env.store.getMatchBranchPattern(pat_entry_idx);
                    if (i > 0) try self.write(" | ");
                    try self.emitPattern(pat_entry.pattern);
                }
                // Emit guard if present
                if (branch.guard) |guard_idx| {
                    try self.write(" if ");
                    try self.emitExpr(guard_idx);
                }
                try self.write(" => ");
                try self.emitExpr(branch.value);
                try self.write("\n");
            }
            self.indent_level -= 1;
            try self.emitIndent();
            try self.write("}");
        },
        .e_nominal => |nominal| {
            // Emit the backing expression for now
            try self.emitExpr(nominal.backing_expr);
        },
        .e_nominal_external => |nominal| {
            try self.emitExpr(nominal.backing_expr);
        },
        .e_lookup_required => {
            try self.write("<required>");
        },
        .e_type_var_dispatch => {
            try self.write("<type_var_dispatch>");
        },
        .e_for => |for_expr| {
            try self.write("for ");
            try self.emitPattern(for_expr.patt);
            try self.write(" in ");
            try self.emitExpr(for_expr.expr);
            try self.write(" ");
            try self.emitExpr(for_expr.body);
        },
        .e_hosted_lambda => {
            try self.write("<hosted_lambda>");
        },
        .e_run_low_level => |run_ll| {
            try self.write("<run_low_level: ");
            try self.write(@tagName(run_ll.op));
            try self.write(">");
        },
    }
}

fn emitPatternValue(self: *Self, pattern: Pattern) EmitError!void {
    switch (pattern) {
        .assign => |ident| {
            const name = self.module_env.getIdent(ident.ident);
            try self.emitIdent(name);
        },
        .underscore => {
            try self.write("_");
        },
        .num_literal => |num| {
            try self.emitIntValue(num.value);
        },
        .str_literal => |str| {
            const text = self.module_env.common.getString(str.literal);
            try self.writer().print("\"{s}\"", .{text});
        },
        .applied_tag => |tag| {
            const name = self.module_env.getIdent(tag.name);
            try self.emitTagName(name);
            const args = self.module_env.store.slicePatterns(tag.args);
            if (args.len > 0) {
                try self.write("(");
                for (args, 0..) |arg_idx, i| {
                    if (i > 0) try self.write(", ");
                    try self.emitPattern(arg_idx);
                }
                try self.write(")");
            }
        },
        .record_destructure => |record| {
            const destruct_indices = self.module_env.store.sliceRecordDestructs(record.destructs);
            // Empty record destructure should be {}
            if (destruct_indices.len == 0) {
                try self.write("{}");
            } else {
                try self.write("{ ");
                for (destruct_indices, 0..) |destruct_idx, i| {
                    const destruct = self.module_env.store.getRecordDestruct(destruct_idx);
                    if (i > 0) try self.write(", ");
                    const name = self.module_env.getIdent(destruct.label);
                    try self.write(name);
                    switch (destruct.kind) {
                        .Required => |pat_idx| {
                            // Check if the pattern is just an assign with same name
                            const inner_pat = self.module_env.store.getPattern(pat_idx);
                            switch (inner_pat) {
                                .assign => |inner_assign| {
                                    const inner_name = self.module_env.getIdent(inner_assign.ident);
                                    if (!std.mem.eql(u8, name, inner_name)) {
                                        try self.write(": ");
                                        try self.emitPattern(pat_idx);
                                    }
                                },
                                else => {
                                    try self.write(": ");
                                    try self.emitPattern(pat_idx);
                                },
                            }
                        },
                        .SubPattern => |pat_idx| {
                            try self.write(": ");
                            try self.emitPattern(pat_idx);
                        },
                        .Rest => |pat_idx| {
                            try self.write("..");
                            try self.emitPattern(pat_idx);
                        },
                    }
                }
                try self.write(" }");
            }
        },
        .tuple => |t| {
            try self.write("(");
            const elems = self.module_env.store.slicePatterns(t.patterns);
            for (elems, 0..) |elem_idx, i| {
                if (i > 0) try self.write(", ");
                try self.emitPattern(elem_idx);
            }
            try self.write(")");
        },
        .list => |l| {
            try self.write("[");
            const elems = self.module_env.store.slicePatterns(l.patterns);
            for (elems, 0..) |elem_idx, i| {
                if (i > 0) try self.write(", ");
                try self.emitPattern(elem_idx);
            }
            if (l.rest_info) |rest| {
                if (elems.len > 0) try self.write(", ");
                try self.write("..");
                if (rest.pattern) |rest_pat| {
                    try self.emitPattern(rest_pat);
                }
            }
            try self.write("]");
        },
        .as => |as_pat| {
            try self.emitPattern(as_pat.pattern);
            try self.write(" as ");
            const name = self.module_env.getIdent(as_pat.ident);
            try self.write(name);
        },
        .runtime_error => {
            try self.write("<pattern_error>");
        },
        .nominal => |nom| {
            try self.emitPattern(nom.backing_pattern);
        },
        .nominal_external => |nom| {
            try self.emitPattern(nom.backing_pattern);
        },
        .small_dec_literal => |dec| {
            const numerator = dec.value.numerator;
            const power = dec.value.denominator_power_of_ten;
            if (power == 0) {
                try self.writer().print("{}", .{numerator});
            } else {
                var divisor: i32 = 1;
                for (0..power) |_| {
                    divisor *= 10;
                }
                const whole = @divTrunc(numerator, @as(i16, @intCast(divisor)));
                const frac_part = @mod(@abs(numerator), @as(u16, @intCast(divisor)));
                try self.writer().print("{}.{}", .{ whole, frac_part });
            }
        },
        .dec_literal => |dec| {
            const value = dec.value.num;
            const scale: i128 = 1_000_000_000_000_000_000;
            const whole = i128h.divTrunc_i128(value, scale);
            const frac_part = i128h.rem_u128(@abs(value), @as(u128, @intCast(scale)));
            if (frac_part == 0) {
                var str_buf: [40]u8 = undefined;
                try self.writer().writeAll(i128h.i128_to_str(&str_buf, whole).str);
            } else {
                var str_buf: [40]u8 = undefined;
                try self.writer().writeAll(i128h.i128_to_str(&str_buf, whole).str);
                try self.writer().writeAll(".");
                var frac_buf: [40]u8 = undefined;
                const frac_str = i128h.u128_to_str(&frac_buf, frac_part).str;
                var pad: usize = 18 - frac_str.len;
                while (pad > 0) : (pad -= 1) {
                    try self.writer().writeAll("0");
                }
                try self.writer().writeAll(frac_str);
            }
        },
        .frac_f32_literal => |frac| {
            var float_buf: [400]u8 = undefined;
            try self.writer().writeAll(i128h.f32_to_str(&float_buf, frac.value));
            try self.writer().writeAll("f32");
        },
        .frac_f64_literal => |frac| {
            var float_buf: [400]u8 = undefined;
            try self.writer().writeAll(i128h.f64_to_str(&float_buf, frac.value));
            try self.writer().writeAll("f64");
        },
    }
}

fn emitStatement(self: *Self, stmt_idx: CIR.Statement.Idx) EmitError!void {
    const stmt = self.module_env.store.getStatement(stmt_idx);
    switch (stmt) {
        .s_decl => |decl| {
            // Add the declared name to scope
            try self.addPatternToScope(decl.pattern);
            try self.emitPattern(decl.pattern);
            try self.write(" = ");
            try self.emitExpr(decl.expr);
        },
        .s_type_anno, .s_type_var_alias, .s_alias_decl, .s_nominal_decl => {
            // Type declarations are not emitted for now
        },
        else => {},
    }
}

fn addPatternToScope(self: *Self, pattern_idx: CIR.Pattern.Idx) !void {
    const pattern = self.module_env.store.getPattern(pattern_idx);
    if (pattern == .assign) {
        const name = self.module_env.getIdent(pattern.assign.ident);
        try self.names_in_scope.put(name, {});
    }
    // For other pattern types (destructuring, etc.), we could recursively add names
    // but for now just handling simple assigns
}

/// Check if a pattern belongs to a top-level definition
fn isTopLevelPattern(self: *Self, pattern_idx: CIR.Pattern.Idx) bool {
    const defs = self.module_env.store.sliceDefs(self.module_env.all_defs);
    for (defs) |def_idx| {
        const def = self.module_env.store.getDef(def_idx);
        if (def.pattern == pattern_idx) {
            return true;
        }
    }
    return false;
}

fn emitIntValue(self: *Self, value: CIR.IntValue) !void {
    var buf: [64]u8 = undefined;
    const str = try value.bufPrint(&buf);
    try self.write(str);
}

fn emitIndent(self: *Self) !void {
    for (0..self.indent_level) |_| {
        try self.write("\t");
    }
}

fn write(self: *Self, str: []const u8) !void {
    try self.output.appendSlice(self.allocator, str);
}

/// Emit a tag name, transforming compiler-generated `#` prefix to `C`.
/// This handles closure tag names like "#1_foo" which become "C1_foo" in output.
/// The `#` prefix is used internally because it's reserved for comments in Roc
/// source code, ensuring no collision with user-defined tag names.
fn emitTagName(self: *Self, name: []const u8) !void {
    if (name.len > 0 and name[0] == '#') {
        // Compiler-generated tag: replace # with C (uppercase for tags)
        try self.output.append(self.allocator, 'C');
        try self.output.appendSlice(self.allocator, name[1..]);
    } else {
        // Regular user tag: emit as-is
        try self.output.appendSlice(self.allocator, name);
    }
}

/// Emit an identifier, transforming compiler-generated `#` prefix to `c`.
/// This handles lifted function names like "#1_foo" which become "c1_foo" in output.
/// The `#` prefix is used internally because it's reserved for comments in Roc
/// source code, ensuring no collision with user-defined identifiers.
fn emitIdent(self: *Self, name: []const u8) !void {
    if (name.len > 0 and name[0] == '#') {
        // Compiler-generated ident: replace # with c (lowercase for functions)
        try self.output.append(self.allocator, 'c');
        try self.output.appendSlice(self.allocator, name[1..]);
    } else {
        // Regular user identifier: emit as-is
        try self.output.appendSlice(self.allocator, name);
    }
}

fn writer(self: *Self) std.ArrayList(u8).Writer {
    return self.output.writer(self.allocator);
}

fn binopToStr(op: Expr.Binop.Op) []const u8 {
    return switch (op) {
        .add => "+",
        .sub => "-",
        .mul => "*",
        .div => "/",
        .div_trunc => "//",
        .rem => "%",
        .lt => "<",
        .gt => ">",
        .le => "<=",
        .ge => ">=",
        .eq => "==",
        .ne => "!=",
        .@"and" => "and",
        .@"or" => "or",
    };
}

// Tests
test "emit simple integer" {
    const allocator = std.testing.allocator;

    // Create a minimal test environment
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "42");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var emitter = Self.init(allocator, module_env);
    defer emitter.deinit();

    // Create a simple integer expression
    const int_value = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 42)),
        .kind = .i128,
    };
    const expr_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value, .kind = .i64 },
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try std.testing.expectEqualStrings("42", emitter.getOutput());
}

test "emit lambda expression" {
    const allocator = std.testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "|x| x");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var emitter = Self.init(allocator, module_env);
    defer emitter.deinit();

    // Create pattern for 'x'
    const x_ident = try module_env.insertIdent(base.Ident.for_text("x"));
    const x_pattern_idx = try module_env.store.addPattern(.{
        .assign = .{ .ident = x_ident },
    }, base.Region.zero());

    // Create lookup expression for body
    const body_idx = try module_env.store.addExpr(.{
        .e_lookup_local = .{ .pattern_idx = x_pattern_idx },
    }, base.Region.zero());

    // Create lambda expression using scratch system
    const start = module_env.store.scratchPatternTop();
    try module_env.store.addScratchPattern(x_pattern_idx);
    const args_span = try module_env.store.patternSpanFrom(start);

    const lambda_idx = try module_env.store.addExpr(.{
        .e_lambda = .{ .args = args_span, .body = body_idx },
    }, base.Region.zero());

    try emitter.emitExpr(lambda_idx);
    try std.testing.expectEqualStrings("|x| x", emitter.getOutput());
}
