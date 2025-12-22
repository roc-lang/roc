//! Roc Code Emitter
//!
//! Converts the Canonical IR (CIR) to valid Roc source code.
//! This is primarily used for testing the monomorphization pipeline -
//! we can emit monomorphic Roc code and verify it produces the same results
//! as the original polymorphic code.
//!
//! The emitter walks the CIR expression tree and writes corresponding Roc syntax.

const std = @import("std");
const types = @import("types");
const base = @import("base");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const PatternMod = @import("Pattern.zig");
const Expr = CIR.Expr;
const Pattern = PatternMod.Pattern;
const TypeVar = types.Var;

const Self = @This();

/// The allocator used for intermediate allocations
allocator: std.mem.Allocator,

/// The module environment containing the CIR
module_env: *const ModuleEnv,

/// The output buffer where Roc code is written
output: std.ArrayList(u8),

/// Current indentation level
indent_level: u32,

/// Initialize a new Emitter
pub fn init(allocator: std.mem.Allocator, module_env: *const ModuleEnv) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .output = std.ArrayList(u8).empty,
        .indent_level = 0,
    };
}

/// Free resources used by the emitter
pub fn deinit(self: *Self) void {
    self.output.deinit(self.allocator);
}

/// Get the emitted Roc source code
pub fn getOutput(self: *const Self) []const u8 {
    return self.output.items;
}

/// Reset the emitter for reuse
pub fn reset(self: *Self) void {
    self.output.clearRetainingCapacity();
    self.indent_level = 0;
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
            const whole = @divTrunc(value, scale);
            const frac_part = @mod(@abs(value), @as(u128, @intCast(scale)));
            if (frac_part == 0) {
                try self.writer().print("{d}", .{whole});
            } else {
                try self.writer().print("{d}.{d:0>18}", .{ whole, frac_part });
            }
        },
        .e_dec_small => |small| {
            const numerator = small.value.numerator;
            const power = small.value.denominator_power_of_ten;
            if (power == 0) {
                try self.writer().print("{d}", .{numerator});
            } else {
                // Convert to decimal string
                var divisor: i32 = 1;
                for (0..power) |_| {
                    divisor *= 10;
                }
                const whole = @divTrunc(numerator, @as(i16, @intCast(divisor)));
                const frac_part = @mod(@abs(numerator), @as(u16, @intCast(divisor)));
                try self.writer().print("{d}.{d}", .{ whole, frac_part });
            }
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
            const pattern = self.module_env.store.getPattern(lookup.pattern_idx);
            try self.emitPatternValue(pattern);
        },
        .e_lookup_external => |ext| {
            // Get the identifier name from the ident_idx
            const ident_text = self.module_env.getIdent(ext.ident_idx);
            try self.write(ident_text);
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
        .e_if => |if_expr| {
            const branch_indices = self.module_env.store.sliceIfBranches(if_expr.branches);
            for (branch_indices, 0..) |branch_idx, i| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                if (i > 0) {
                    try self.write(" else if ");
                } else {
                    try self.write("if ");
                }
                try self.emitExpr(branch.cond);
                try self.write(" ");
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
            try self.write("{");
            const field_indices = self.module_env.store.sliceRecordFields(record.fields);
            for (field_indices, 0..) |field_idx, i| {
                const field = self.module_env.store.getRecordField(field_idx);
                if (i > 0) try self.write(", ");
                const name = self.module_env.getIdent(field.name);
                try self.writer().print("{s}: ", .{name});
                try self.emitExpr(field.value);
            }
            if (record.ext) |ext_idx| {
                if (field_indices.len > 0) try self.write(", ");
                try self.write("..");
                try self.emitExpr(ext_idx);
            }
            try self.write("}");
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
            try self.write(name);
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
            try self.write(name);
        },
        .e_closure => |closure| {
            // Emit the underlying lambda
            try self.emitExpr(closure.lambda_idx);
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
            try self.write("(");
            try self.emitExpr(binop.lhs);
            try self.write(" ");
            try self.write(binopToStr(binop.op));
            try self.write(" ");
            try self.emitExpr(binop.rhs);
            try self.write(")");
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
                try self.write(",\n");
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
        .e_low_level_lambda => {
            try self.write("<low_level>");
        },
    }
}

fn emitPatternValue(self: *Self, pattern: Pattern) EmitError!void {
    switch (pattern) {
        .assign => |ident| {
            const name = self.module_env.getIdent(ident.ident);
            try self.write(name);
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
            try self.write(name);
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
            try self.write("{");
            const destruct_indices = self.module_env.store.sliceRecordDestructs(record.destructs);
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
                }
            }
            try self.write("}");
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
                try self.writer().print("{d}", .{numerator});
            } else {
                var divisor: i32 = 1;
                for (0..power) |_| {
                    divisor *= 10;
                }
                const whole = @divTrunc(numerator, @as(i16, @intCast(divisor)));
                const frac_part = @mod(@abs(numerator), @as(u16, @intCast(divisor)));
                try self.writer().print("{d}.{d}", .{ whole, frac_part });
            }
        },
        .dec_literal => |dec| {
            const value = dec.value.num;
            const scale: i128 = 1_000_000_000_000_000_000;
            const whole = @divTrunc(value, scale);
            const frac_part = @mod(@abs(value), @as(u128, @intCast(scale)));
            if (frac_part == 0) {
                try self.writer().print("{d}", .{whole});
            } else {
                try self.writer().print("{d}.{d:0>18}", .{ whole, frac_part });
            }
        },
        .frac_f32_literal => |frac| {
            try self.writer().print("{d}f32", .{frac.value});
        },
        .frac_f64_literal => |frac| {
            try self.writer().print("{d}f64", .{frac.value});
        },
    }
}

fn emitStatement(self: *Self, stmt_idx: CIR.Statement.Idx) EmitError!void {
    const stmt = self.module_env.store.getStatement(stmt_idx);
    switch (stmt) {
        .s_decl => |decl| {
            try self.emitPattern(decl.pattern);
            try self.write(" = ");
            try self.emitExpr(decl.expr);
        },
        .s_decl_gen => |decl| {
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

fn emitIntValue(self: *Self, value: CIR.IntValue) !void {
    var buf: [64]u8 = undefined;
    const str = try value.bufPrint(&buf);
    try self.write(str);
}

fn emitIndent(self: *Self) !void {
    for (0..self.indent_level) |_| {
        try self.write("    ");
    }
}

fn write(self: *Self, str: []const u8) !void {
    try self.output.appendSlice(self.allocator, str);
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
