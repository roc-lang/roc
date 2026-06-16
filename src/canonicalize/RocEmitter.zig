//! Roc Code Emitter
//!
//! Converts the Canonical IR (CIR) to valid Roc source code.
//! This is primarily used for testing the monomorphization pipeline -
//! we can emit monomorphic Roc code and verify it produces the same results
//! as the original polymorphic code.
//!
//! The emitter walks the CIR expression tree and writes corresponding Roc syntax.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");
const builtins = @import("builtins");
const parse = @import("parse");

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

/// Scratch buffer for short-lived formatting.
scratch: std.ArrayList(u8),

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
        .scratch = std.ArrayList(u8).empty,
        .indent_level = 0,
        .names_in_scope = std.StringHashMap(void).init(allocator),
        .capture_renames = std.AutoHashMap(PatternMod.Pattern.Idx, []const u8).init(allocator),
        .rename_counter = 0,
    };
}

/// Free resources used by the emitter
pub fn deinit(self: *Self) void {
    self.output.deinit(self.allocator);
    self.scratch.deinit(self.allocator);
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
    self.scratch.clearRetainingCapacity();
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
pub fn emitExpr(self: *Self, expr_idx: Expr.Idx) EmitError!void {
    try self.emitFromFrame(.{ .expr = expr_idx });
}

/// Emit a pattern as Roc source code
pub fn emitPattern(self: *Self, pattern_idx: CIR.Pattern.Idx) EmitError!void {
    try self.emitFromFrame(.{ .pattern = pattern_idx });
}

const EmitFrame = union(enum) {
    expr: Expr.Idx,
    pattern: CIR.Pattern.Idx,
    statement: CIR.Statement.Idx,
    binop_operand: struct { expr_idx: Expr.Idx, outer_op: Expr.Binop.Op, side: BinopSide },
    write: []const u8,
    indent,
    inc_indent,
    dec_indent,
    pattern_record_field: struct { destruct_idx: CIR.Pattern.RecordDestruct.Idx, index: usize },
};

fn emitFromFrame(self: *Self, first: EmitFrame) EmitError!void {
    var stack_allocator_state = std.heap.stackFallback(8192, self.allocator);
    const stack_allocator = stack_allocator_state.get();
    var frames: std.ArrayList(EmitFrame) = .empty;
    defer frames.deinit(stack_allocator);

    try frames.append(stack_allocator, first);
    while (frames.pop()) |frame| {
        switch (frame) {
            .write => |text| try self.write(text),
            .indent => try self.emitIndent(),
            .inc_indent => self.indent_level += 1,
            .dec_indent => self.indent_level -= 1,
            .expr => |idx| try self.emitExprFrame(idx, &frames, stack_allocator),
            .pattern => |idx| try self.emitPatternFrame(idx, &frames, stack_allocator),
            .statement => |idx| try self.emitStatementFrame(idx, &frames, stack_allocator),
            .binop_operand => |operand| try self.emitBinopOperandFrame(operand.expr_idx, operand.outer_op, operand.side, &frames, stack_allocator),
            .pattern_record_field => |field| try self.emitPatternRecordFieldFrame(field.destruct_idx, field.index, &frames, stack_allocator),
        }
    }
}

fn pushExprList(
    frames: *std.ArrayList(EmitFrame),
    allocator: std.mem.Allocator,
    exprs: []const Expr.Idx,
    close: []const u8,
    separator: []const u8,
) std.mem.Allocator.Error!void {
    try frames.append(allocator, .{ .write = close });
    var i = exprs.len;
    while (i > 0) {
        i -= 1;
        try frames.append(allocator, .{ .expr = exprs[i] });
        if (i > 0) try frames.append(allocator, .{ .write = separator });
    }
}

fn pushPatternList(
    frames: *std.ArrayList(EmitFrame),
    allocator: std.mem.Allocator,
    patterns: []const CIR.Pattern.Idx,
    close: []const u8,
    separator: []const u8,
) std.mem.Allocator.Error!void {
    try frames.append(allocator, .{ .write = close });
    var i = patterns.len;
    while (i > 0) {
        i -= 1;
        try frames.append(allocator, .{ .pattern = patterns[i] });
        if (i > 0) try frames.append(allocator, .{ .write = separator });
    }
}

/// The binary operator this expression prints as, if any: a still-sugared
/// `e_binop`, a checked `e_dispatch_call` desugared from one, or a checked
/// equality node (printed as `==`/`!=`).
fn surfaceBinopOp(expr: Expr) ?Expr.Binop.Op {
    return switch (expr) {
        .e_binop => |binop| binop.op,
        .e_dispatch_call => |method_call| switch (method_call.surface_origin) {
            .binop => |op| op,
            else => null,
        },
        .e_structural_eq => |eq| if (eq.negated) .ne else .eq,
        .e_method_eq => |eq| if (eq.negated) .ne else .eq,
        else => null,
    };
}

/// Which side of a binary operator an operand sits on. The two sides have
/// different parenthesization rules: e.g. left-associative operators need
/// parens around an equal-precedence RIGHT operand (`1 - (2 - 3)`) but not
/// around an equal-precedence left one (`(1 - 2) - 3` reparses unchanged).
const BinopSide = enum { left, right };

fn emitBinopOperandFrame(
    self: *Self,
    expr_idx: Expr.Idx,
    outer_op: Expr.Binop.Op,
    side: BinopSide,
    frames: *std.ArrayList(EmitFrame),
    allocator: std.mem.Allocator,
) EmitError!void {
    const expr = self.module_env.store.getExpr(expr_idx);
    const needs_parens = if (surfaceBinopOp(expr)) |inner_op| switch (side) {
        // The left operand's text is parsed first; the inner expression then
        // keeps its operands iff the outer operator does NOT bind into the
        // inner operator's right operand, i.e. the outer operator's left
        // binding power is below the inner operator's right binding power.
        .left => binopBp(outer_op).left >= binopBp(inner_op).right,
        // The right operand's text is parsed with the outer operator's right
        // binding power as the minimum; the inner operator only binds its
        // operands together if its left binding power reaches that minimum.
        .right => binopBp(inner_op).left < binopBp(outer_op).right,
    } else false;
    if (needs_parens) {
        try frames.append(allocator, .{ .write = ")" });
        try frames.append(allocator, .{ .expr = expr_idx });
        try frames.append(allocator, .{ .write = "(" });
    } else {
        try frames.append(allocator, .{ .expr = expr_idx });
    }
}

/// Whether a unary operator's receiver must be parenthesized to reparse as
/// written: operator expressions (unary binds tighter than any binop), other
/// unary applications, and negative literals. The latter two print a leading
/// `-` or `!`, which would fuse with the unary operator into different
/// tokens: `--x` tokenizes as binary minus, not as two negations.
fn unaryReceiverNeedsParens(self: *Self, receiver_idx: Expr.Idx) bool {
    const receiver = self.module_env.store.getExpr(receiver_idx);
    if (surfaceBinopOp(receiver) != null) return true;
    return switch (receiver) {
        .e_unary_minus, .e_unary_not => true,
        .e_dispatch_call => |method_call| switch (method_call.surface_origin) {
            .unary_minus, .unary_not => true,
            else => false,
        },
        .e_num => |num| num.value.kind == .i128 and num.value.toI128() < 0,
        .e_frac_f32 => |frac| std.math.signbit(frac.value),
        .e_frac_f64 => |frac| std.math.signbit(frac.value),
        .e_dec => |dec| dec.value.num < 0,
        .e_dec_small => |small| small.value.numerator < 0,
        .e_num_from_numeral, .e_typed_num_from_numeral => blk: {
            const literal = self.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(receiver_idx)) orelse {
                std.debug.panic("missing recorded numeral for expression {}", .{@intFromEnum(receiver_idx)});
            };
            break :blk literal.isNegative();
        },
        .e_typed_int => |typed| typed.value.kind == .i128 and typed.value.toI128() < 0,
        .e_typed_frac => |typed| typed.value.toI128() < 0,
        else => false,
    };
}

/// Push frames emitting a unary operator's receiver, parenthesized when
/// required to reparse as the same expression.
fn pushUnaryReceiverFrames(
    self: *Self,
    receiver_idx: Expr.Idx,
    frames: *std.ArrayList(EmitFrame),
    allocator: std.mem.Allocator,
) EmitError!void {
    if (self.unaryReceiverNeedsParens(receiver_idx)) {
        try frames.append(allocator, .{ .write = ")" });
        try frames.append(allocator, .{ .expr = receiver_idx });
        try frames.append(allocator, .{ .write = "(" });
    } else {
        try frames.append(allocator, .{ .expr = receiver_idx });
    }
}

fn emitExprFrame(
    self: *Self,
    expr_idx: Expr.Idx,
    frames: *std.ArrayList(EmitFrame),
    allocator: std.mem.Allocator,
) EmitError!void {
    const expr = self.module_env.store.getExpr(expr_idx);
    switch (expr) {
        .e_num => |num| try self.emitIntValue(num.value),
        .e_frac_f32 => |frac| try self.output.print(self.allocator, "{d}f32", .{frac.value}),
        .e_frac_f64 => |frac| try self.output.print(self.allocator, "{d}f64", .{frac.value}),
        .e_dec => |dec| {
            const value = dec.value.num;
            const scale: i128 = 1_000_000_000_000_000_000;
            const whole = i128h.divTrunc_i128(value, scale);
            const frac_part = i128h.rem_u128(@abs(value), @as(u128, @intCast(scale)));
            try self.write(try self.formatI128(whole));
            if (frac_part != 0) {
                try self.write(".");
                const frac_str = try self.formatU128(frac_part);
                var pad: usize = 18 - frac_str.len;
                while (pad > 0) : (pad -= 1) try self.write("0");
                try self.write(frac_str);
            }
        },
        .e_dec_small => |small| {
            const numerator = small.value.numerator;
            const power = small.value.denominator_power_of_ten;
            if (power == 0) {
                try self.output.print(self.allocator, "{}", .{numerator});
            } else {
                var divisor: i32 = 1;
                for (0..power) |_| divisor *= 10;
                const whole = @divTrunc(numerator, @as(i16, @intCast(divisor)));
                const frac_part = @mod(@abs(numerator), @as(u16, @intCast(divisor)));
                try self.output.print(self.allocator, "{}.{}", .{ whole, frac_part });
            }
        },
        .e_num_from_numeral => try self.emitRecordedNumeral(expr_idx, null),
        .e_typed_int => |typed| {
            try self.emitIntValue(typed.value);
            try self.output.print(self.allocator, ".{s}", .{self.module_env.getIdent(typed.type_name)});
        },
        .e_typed_frac => |typed| {
            const value = typed.value.toI128();
            const scale: i128 = 1_000_000_000_000_000_000;
            const whole = i128h.divTrunc_i128(value, scale);
            const frac_part = i128h.rem_u128(@abs(value), @as(u128, @intCast(scale)));
            if (frac_part == 0) {
                try self.output.print(self.allocator, "{d}.0", .{whole});
            } else {
                try self.output.print(self.allocator, "{d}.{d:0>18}", .{ whole, frac_part });
            }
            try self.output.print(self.allocator, ".{s}", .{self.module_env.getIdent(typed.type_name)});
        },
        .e_typed_num_from_numeral => |typed| try self.emitRecordedNumeral(expr_idx, typed.type_name),
        .e_str_segment => |seg| try self.output.print(self.allocator, "\"{s}\"", .{self.module_env.common.getString(seg.literal)}),
        .e_bytes_literal => |bytes| try self.output.print(self.allocator, "<bytes:{d}>", .{self.module_env.common.getString(bytes.literal).len}),
        .e_str => |str| {
            const segments = self.module_env.store.sliceExpr(str.span);
            var i = segments.len;
            while (i > 0) {
                i -= 1;
                try frames.append(allocator, .{ .expr = segments[i] });
            }
        },
        .e_lookup_local => |lookup| {
            if (self.capture_renames.get(lookup.pattern_idx)) |renamed| {
                try self.emitIdent(renamed);
            } else {
                try frames.append(allocator, .{ .pattern = lookup.pattern_idx });
            }
        },
        .e_lookup_external => |ext| try self.emitIdent(self.module_env.getIdent(ext.ident_idx)),
        .e_list => |list| {
            try self.write("[");
            try pushExprList(frames, allocator, self.module_env.store.sliceExpr(list.elems), "]", ", ");
        },
        .e_empty_list => try self.write("[]"),
        .e_tuple => |tuple| {
            try self.write("(");
            try pushExprList(frames, allocator, self.module_env.store.sliceExpr(tuple.elems), ")", ", ");
        },
        .e_tuple_access => |tuple_access| {
            try frames.append(allocator, .{ .write = try std.fmt.allocPrint(allocator, ".{d}", .{tuple_access.elem_index}) });
            try frames.append(allocator, .{ .expr = tuple_access.tuple });
        },
        .e_if => |if_expr| {
            try frames.append(allocator, .{ .expr = if_expr.final_else });
            try frames.append(allocator, .{ .write = " else " });
            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
            var i = branches.len;
            while (i > 0) {
                i -= 1;
                const branch = self.module_env.store.getIfBranch(branches[i]);
                try frames.append(allocator, .{ .expr = branch.body });
                try frames.append(allocator, .{ .write = ") " });
                try frames.append(allocator, .{ .expr = branch.cond });
                try frames.append(allocator, .{ .write = if (i > 0) " else if (" else "if (" });
            }
        },
        .e_call => |call| {
            try self.write("");
            try pushExprList(frames, allocator, self.module_env.store.sliceExpr(call.args), ")", ", ");
            try frames.append(allocator, .{ .write = "(" });
            try frames.append(allocator, .{ .expr = call.func });
        },
        .e_record => |record| {
            try self.write("{ ");
            try frames.append(allocator, .{ .write = " }" });
            if (record.ext) |ext_idx| {
                try frames.append(allocator, .{ .expr = ext_idx });
                try frames.append(allocator, .{ .write = ".." });
                if (self.module_env.store.sliceRecordFields(record.fields).len > 0) try frames.append(allocator, .{ .write = ", " });
            }
            const fields = self.module_env.store.sliceRecordFields(record.fields);
            var i = fields.len;
            while (i > 0) {
                i -= 1;
                const field = self.module_env.store.getRecordField(fields[i]);
                try frames.append(allocator, .{ .expr = field.value });
                try frames.append(allocator, .{ .write = ": " });
                try frames.append(allocator, .{ .write = self.module_env.getIdent(field.name) });
                if (i > 0) try frames.append(allocator, .{ .write = ", " });
            }
        },
        .e_empty_record => try self.write("{}"),
        .e_block => |block| {
            try self.write("{\n");
            self.indent_level += 1;
            try frames.append(allocator, .{ .write = "}" });
            try frames.append(allocator, .indent);
            try frames.append(allocator, .dec_indent);
            try frames.append(allocator, .{ .write = "\n" });
            try frames.append(allocator, .{ .expr = block.final_expr });
            try frames.append(allocator, .indent);
            const stmts = self.module_env.store.sliceStatements(block.stmts);
            var i = stmts.len;
            while (i > 0) {
                i -= 1;
                try frames.append(allocator, .{ .write = "\n" });
                try frames.append(allocator, .{ .statement = stmts[i] });
                try frames.append(allocator, .indent);
            }
        },
        .e_tag => |tag| {
            try self.emitTagName(self.module_env.getIdent(tag.name));
            const args = self.module_env.store.sliceExpr(tag.args);
            if (args.len > 0) {
                try self.write("(");
                try pushExprList(frames, allocator, args, ")", ", ");
            }
        },
        .e_zero_argument_tag => |tag| try self.emitTagName(self.module_env.getIdent(tag.name)),
        .e_closure => |closure| try frames.append(allocator, .{ .expr = closure.lambda_idx }),
        .e_lambda => |lambda| {
            try self.write("|");
            const args = self.module_env.store.slicePatterns(lambda.args);
            try frames.append(allocator, .{ .expr = lambda.body });
            try frames.append(allocator, .{ .write = "| " });
            var i = args.len;
            while (i > 0) {
                i -= 1;
                try frames.append(allocator, .{ .pattern = args[i] });
                if (i > 0) try frames.append(allocator, .{ .write = ", " });
            }
        },
        .e_binop => |binop| {
            try frames.append(allocator, .{ .binop_operand = .{ .expr_idx = binop.rhs, .outer_op = binop.op, .side = .right } });
            try frames.append(allocator, .{ .write = " " });
            try frames.append(allocator, .{ .write = binopToStr(binop.op) });
            try frames.append(allocator, .{ .write = " " });
            try frames.append(allocator, .{ .binop_operand = .{ .expr_idx = binop.lhs, .outer_op = binop.op, .side = .left } });
        },
        .e_unary_minus => |unary| {
            try self.pushUnaryReceiverFrames(unary.expr, frames, allocator);
            try frames.append(allocator, .{ .write = "-" });
        },
        .e_unary_not => |unary| {
            try self.pushUnaryReceiverFrames(unary.expr, frames, allocator);
            try frames.append(allocator, .{ .write = "!" });
        },
        .e_field_access => |field_access| {
            try frames.append(allocator, .{ .write = self.module_env.getIdent(field_access.field_name) });
            try frames.append(allocator, .{ .write = "." });
            try frames.append(allocator, .{ .expr = field_access.receiver });
        },
        .e_method_call => |method_call| {
            try pushExprList(frames, allocator, self.module_env.store.sliceExpr(method_call.args), ")", ", ");
            try frames.append(allocator, .{ .write = "(" });
            try frames.append(allocator, .{ .write = self.module_env.getIdent(method_call.method_name) });
            try frames.append(allocator, .{ .write = "." });
            try frames.append(allocator, .{ .expr = method_call.receiver });
        },
        .e_dispatch_call => |method_call| {
            // Re-emit in the surface form the dispatch was desugared from
            // (see `SurfaceOrigin`): printing a desugared `+` back as
            // `.plus()` would weaken the re-checked program.
            switch (method_call.surface_origin) {
                .binop => |op| {
                    const args = self.module_env.store.sliceExpr(method_call.args);
                    std.debug.assert(args.len == 1);
                    try frames.append(allocator, .{ .binop_operand = .{ .expr_idx = args[0], .outer_op = op, .side = .right } });
                    try frames.append(allocator, .{ .write = " " });
                    try frames.append(allocator, .{ .write = binopToStr(op) });
                    try frames.append(allocator, .{ .write = " " });
                    try frames.append(allocator, .{ .binop_operand = .{ .expr_idx = method_call.receiver, .outer_op = op, .side = .left } });
                },
                .unary_minus, .unary_not => {
                    try self.pushUnaryReceiverFrames(method_call.receiver, frames, allocator);
                    try frames.append(allocator, .{ .write = if (method_call.surface_origin == .unary_minus) "-" else "!" });
                },
                .method_call => {
                    try pushExprList(frames, allocator, self.module_env.store.sliceExpr(method_call.args), ")", ", ");
                    try frames.append(allocator, .{ .write = "(" });
                    try frames.append(allocator, .{ .write = self.module_env.getIdent(method_call.method_name) });
                    try frames.append(allocator, .{ .write = "." });
                    try frames.append(allocator, .{ .expr = method_call.receiver });
                },
            }
        },
        .e_interpolation => |interpolation| {
            try frames.append(allocator, .{ .write = ")" });
            try pushExprList(frames, allocator, self.module_env.store.sliceExpr(interpolation.parts), "]", ", ");
            try frames.append(allocator, .{ .write = ", [" });
            try frames.append(allocator, .{ .expr = interpolation.first });
            try frames.append(allocator, .{ .write = "<interpolation>(" });
        },
        .e_structural_eq => |eq| {
            const op: Expr.Binop.Op = if (eq.negated) .ne else .eq;
            try frames.append(allocator, .{ .binop_operand = .{ .expr_idx = eq.rhs, .outer_op = op, .side = .right } });
            try frames.append(allocator, .{ .write = if (eq.negated) " != " else " == " });
            try frames.append(allocator, .{ .binop_operand = .{ .expr_idx = eq.lhs, .outer_op = op, .side = .left } });
        },
        .e_structural_hash => |h| {
            try frames.append(allocator, .{ .write = ")" });
            try frames.append(allocator, .{ .expr = h.hasher });
            try frames.append(allocator, .{ .write = ".to_hash(" });
            try frames.append(allocator, .{ .expr = h.value });
        },
        .e_method_eq => |eq| {
            const op: Expr.Binop.Op = if (eq.negated) .ne else .eq;
            try frames.append(allocator, .{ .binop_operand = .{ .expr_idx = eq.rhs, .outer_op = op, .side = .right } });
            try frames.append(allocator, .{ .write = if (eq.negated) " != " else " == " });
            try frames.append(allocator, .{ .binop_operand = .{ .expr_idx = eq.lhs, .outer_op = op, .side = .left } });
        },
        .e_type_method_call => |method_call| {
            try pushExprList(frames, allocator, self.module_env.store.sliceExpr(method_call.args), ")", ", ");
            try frames.append(allocator, .{ .write = "(" });
            try frames.append(allocator, .{ .write = self.module_env.getIdent(method_call.method_name) });
            try frames.append(allocator, .{ .write = "." });
            const alias_str = try std.fmt.allocPrint(allocator, "__type_dispatch_{d}__", .{@intFromEnum(method_call.type_dispatch_stmt)});
            try frames.append(allocator, .{ .write = alias_str });
        },
        .e_type_dispatch_call => |method_call| {
            try pushExprList(frames, allocator, self.module_env.store.sliceExpr(method_call.args), ")", ", ");
            try frames.append(allocator, .{ .write = "(" });
            try frames.append(allocator, .{ .write = self.module_env.getIdent(method_call.method_name) });
            try frames.append(allocator, .{ .write = "." });
            const alias_str = try std.fmt.allocPrint(allocator, "__type_dispatch_{d}__", .{@intFromEnum(method_call.type_dispatch_stmt)});
            try frames.append(allocator, .{ .write = alias_str });
        },
        .e_runtime_error => try self.write("<runtime_error>"),
        .e_crash => |crash| try self.output.print(self.allocator, "crash \"{s}\"", .{self.module_env.common.getString(crash.msg)}),
        .e_dbg => |dbg| {
            try frames.append(allocator, .{ .expr = dbg.expr });
            try frames.append(allocator, .{ .write = "dbg " });
        },
        .e_expect_err => try self.write("<expect_err>"),
        .e_expect => |expect| {
            try frames.append(allocator, .{ .expr = expect.body });
            try frames.append(allocator, .{ .write = "expect " });
        },
        .e_ellipsis => try self.write("..."),
        .e_anno_only => try self.write("<anno_only>"),
        .e_return => |ret| {
            try frames.append(allocator, .{ .expr = ret.expr });
            try frames.append(allocator, .{ .write = "return " });
        },
        .e_break => try self.write("break"),
        .e_match => |match| {
            try self.write("match ");
            try frames.append(allocator, .{ .write = "}" });
            try frames.append(allocator, .indent);
            try frames.append(allocator, .dec_indent);
            const branches = self.module_env.store.sliceMatchBranches(match.branches);
            var i = branches.len;
            while (i > 0) {
                i -= 1;
                const branch = self.module_env.store.getMatchBranch(branches[i]);
                try frames.append(allocator, .{ .write = "\n" });
                try frames.append(allocator, .{ .expr = branch.value });
                try frames.append(allocator, .{ .write = " => " });
                if (branch.guard) |guard_idx| {
                    try frames.append(allocator, .{ .expr = guard_idx });
                    try frames.append(allocator, .{ .write = " if " });
                }
                const patterns = self.module_env.store.sliceMatchBranchPatterns(branch.patterns);
                var j = patterns.len;
                while (j > 0) {
                    j -= 1;
                    try frames.append(allocator, .{ .pattern = self.module_env.store.getMatchBranchPattern(patterns[j]).pattern });
                    if (j > 0) try frames.append(allocator, .{ .write = " | " });
                }
                try frames.append(allocator, .indent);
            }
            try frames.append(allocator, .inc_indent);
            try frames.append(allocator, .{ .write = " {\n" });
            try frames.append(allocator, .{ .expr = match.cond });
        },
        .e_nominal => |nominal| try frames.append(allocator, .{ .expr = nominal.backing_expr }),
        .e_nominal_external => |nominal| try frames.append(allocator, .{ .expr = nominal.backing_expr }),
        .e_lookup_required => try self.write("<required>"),
        .e_for => |for_expr| {
            try frames.append(allocator, .{ .expr = for_expr.body });
            try frames.append(allocator, .{ .write = " " });
            try frames.append(allocator, .{ .expr = for_expr.expr });
            try frames.append(allocator, .{ .write = " in " });
            try frames.append(allocator, .{ .pattern = for_expr.patt });
            try frames.append(allocator, .{ .write = "for " });
        },
        .e_hosted_lambda => try self.write("<hosted_lambda>"),
        .e_run_low_level => |run_ll| try self.output.print(self.allocator, "<run_low_level: {s}>", .{@tagName(run_ll.op)}),
    }
}

fn emitPatternFrame(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    frames: *std.ArrayList(EmitFrame),
    allocator: std.mem.Allocator,
) EmitError!void {
    const pattern = self.module_env.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign => |ident| try self.emitIdent(self.module_env.getIdent(ident.ident)),
        .underscore => try self.write("_"),
        .num_literal => |num| try self.emitIntValue(num.value),
        .str_literal => |str| try self.output.print(self.allocator, "\"{s}\"", .{self.module_env.common.getString(str.literal)}),
        .str_interpolation => |str| {
            try self.write("\"");
            try self.write(self.module_env.common.getString(str.prefix));
            var step_offset: u32 = 0;
            while (step_offset < str.steps.span.len) : (step_offset += 1) {
                const step = self.module_env.store.getStrPatternStep(str.steps, step_offset);
                try self.write("${");
                if (step.capture) |capture_idx| {
                    switch (self.module_env.store.getPattern(capture_idx)) {
                        .assign => |assign| try self.emitIdent(self.module_env.getIdent(assign.ident)),
                        else => try self.write("<capture>"),
                    }
                } else {
                    try self.write("_");
                }
                try self.write("}");
                try self.write(self.module_env.common.getString(step.delimiter));
            }
            try self.write("\"");
        },
        .applied_tag => |tag| {
            try self.emitTagName(self.module_env.getIdent(tag.name));
            const args = self.module_env.store.slicePatterns(tag.args);
            if (args.len > 0) {
                try self.write("(");
                try pushPatternList(frames, allocator, args, ")", ", ");
            }
        },
        .record_destructure => |record| {
            const destructs = self.module_env.store.sliceRecordDestructs(record.destructs);
            if (destructs.len == 0) {
                try self.write("{}");
            } else {
                try self.write("{ ");
                try frames.append(allocator, .{ .write = " }" });
                var i = destructs.len;
                while (i > 0) {
                    i -= 1;
                    try frames.append(allocator, .{ .pattern_record_field = .{ .destruct_idx = destructs[i], .index = i } });
                    if (i > 0) try frames.append(allocator, .{ .write = ", " });
                }
            }
        },
        .tuple => |t| {
            try self.write("(");
            try pushPatternList(frames, allocator, self.module_env.store.slicePatterns(t.patterns), ")", ", ");
        },
        .list => |l| {
            try self.write("[");
            try frames.append(allocator, .{ .write = "]" });
            if (l.rest_info) |rest| {
                if (rest.pattern) |rest_pat| {
                    try frames.append(allocator, .{ .pattern = rest_pat });
                }
                try frames.append(allocator, .{ .write = ".." });
                if (self.module_env.store.slicePatterns(l.patterns).len > 0) try frames.append(allocator, .{ .write = ", " });
            }
            const elems = self.module_env.store.slicePatterns(l.patterns);
            var i = elems.len;
            while (i > 0) {
                i -= 1;
                try frames.append(allocator, .{ .pattern = elems[i] });
                if (i > 0) try frames.append(allocator, .{ .write = ", " });
            }
        },
        .as => |as_pat| {
            try frames.append(allocator, .{ .write = self.module_env.getIdent(as_pat.ident) });
            try frames.append(allocator, .{ .write = " as " });
            try frames.append(allocator, .{ .pattern = as_pat.pattern });
        },
        .runtime_error => try self.write("<pattern_error>"),
        .nominal => |nom| try frames.append(allocator, .{ .pattern = nom.backing_pattern }),
        .nominal_external => |nom| try frames.append(allocator, .{ .pattern = nom.backing_pattern }),
        .small_dec_literal => |dec| {
            const numerator = dec.value.numerator;
            const power = dec.value.denominator_power_of_ten;
            if (power == 0) {
                try self.output.print(self.allocator, "{}", .{numerator});
            } else {
                var divisor: i32 = 1;
                for (0..power) |_| divisor *= 10;
                const whole = @divTrunc(numerator, @as(i16, @intCast(divisor)));
                const frac_part = @mod(@abs(numerator), @as(u16, @intCast(divisor)));
                try self.output.print(self.allocator, "{}.{}", .{ whole, frac_part });
            }
        },
        .dec_literal => |dec| {
            const value = dec.value.num;
            const scale: i128 = 1_000_000_000_000_000_000;
            const whole = i128h.divTrunc_i128(value, scale);
            const frac_part = i128h.rem_u128(@abs(value), @as(u128, @intCast(scale)));
            try self.write(try self.formatI128(whole));
            if (frac_part != 0) {
                try self.write(".");
                const frac_str = try self.formatU128(frac_part);
                var pad: usize = 18 - frac_str.len;
                while (pad > 0) : (pad -= 1) try self.write("0");
                try self.write(frac_str);
            }
        },
        .frac_f32_literal => |frac| {
            try self.write(try self.formatF32(frac.value));
            try self.write("f32");
        },
        .frac_f64_literal => |frac| {
            try self.write(try self.formatF64(frac.value));
            try self.write("f64");
        },
    }
}

fn emitPatternRecordFieldFrame(
    self: *Self,
    destruct_idx: CIR.Pattern.RecordDestruct.Idx,
    _: usize,
    frames: *std.ArrayList(EmitFrame),
    allocator: std.mem.Allocator,
) EmitError!void {
    const destruct = self.module_env.store.getRecordDestruct(destruct_idx);
    const name = self.module_env.getIdent(destruct.label);
    try self.write(name);
    switch (destruct.kind) {
        .Required => |pat_idx| {
            const inner_pat = self.module_env.store.getPattern(pat_idx);
            const shorthand = if (inner_pat == .assign)
                std.mem.eql(u8, name, self.module_env.getIdent(inner_pat.assign.ident))
            else
                false;
            if (!shorthand) {
                try frames.append(allocator, .{ .pattern = pat_idx });
                try frames.append(allocator, .{ .write = ": " });
            }
        },
        .SubPattern => |pat_idx| {
            try frames.append(allocator, .{ .pattern = pat_idx });
            try frames.append(allocator, .{ .write = ": " });
        },
        .Rest => |pat_idx| {
            try frames.append(allocator, .{ .pattern = pat_idx });
            try frames.append(allocator, .{ .write = ".." });
        },
    }
}

fn emitStatementFrame(
    self: *Self,
    stmt_idx: CIR.Statement.Idx,
    frames: *std.ArrayList(EmitFrame),
    allocator: std.mem.Allocator,
) EmitError!void {
    const stmt = self.module_env.store.getStatement(stmt_idx);
    switch (stmt) {
        .s_decl => |decl| {
            try self.addPatternToScope(decl.pattern);
            try frames.append(allocator, .{ .expr = decl.expr });
            try frames.append(allocator, .{ .write = " = " });
            try frames.append(allocator, .{ .pattern = decl.pattern });
        },
        else => {},
    }
}

fn binopOpToToken(op: Expr.Binop.Op) parse.tokenize.Token.Tag {
    return switch (op) {
        .add => .OpPlus,
        .sub => .OpBinaryMinus,
        .mul => .OpStar,
        .div => .OpSlash,
        .rem => .OpPercent,
        .lt => .OpLessThan,
        .gt => .OpGreaterThan,
        .le => .OpLessThanOrEq,
        .ge => .OpGreaterThanOrEq,
        .eq => .OpEquals,
        .ne => .OpNotEquals,
        .div_trunc => .OpDoubleSlash,
        .@"and" => .OpAnd,
        .@"or" => .OpOr,
    };
}

/// Binding powers for re-emission parenthesization come straight from the
/// parser's single binding-power table, so a re-emitted operator expression
/// always reparses to the same tree. (Every `Binop.Op` maps to an in-range
/// operator token with non-zero binding power, so the lookup never returns null.)
fn binopBp(op: Expr.Binop.Op) parse.Parser.BinOpBp {
    return parse.Parser.getTokenBP(binopOpToToken(op)).?;
}

comptime {
    // Enforce the invariant `binopBp`'s doc comment states: every `Binop.Op` maps
    // to an operator token with a defined binding power, so the `.?` above can
    // never panic. A future `binopOpToToken` mapping onto a non-operator (or
    // zero-binding-power) token becomes a compile error here instead of a runtime
    // crash during re-emission.
    for (@typeInfo(Expr.Binop.Op).@"enum".fields) |field| {
        const op: Expr.Binop.Op = @enumFromInt(field.value);
        std.debug.assert(parse.Parser.getTokenBP(binopOpToToken(op)) != null);
    }
}

const EmitError = std.mem.Allocator.Error || std.fmt.BufPrintError;

fn emitRecordedNumeral(self: *Self, expr_idx: Expr.Idx, maybe_type_name: ?base.Ident.Idx) EmitError!void {
    const literal = self.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse {
        std.debug.panic("missing recorded numeral for expression {}", .{@intFromEnum(expr_idx)});
    };

    if (literal.isNegative()) {
        try self.write("-");
    }

    const before = self.module_env.numeralDigitsBefore(literal);
    const before_digits = try self.base256ToDecimalDigits(before);
    defer self.allocator.free(before_digits);
    try self.write(before_digits);

    if (literal.isFractional()) {
        try self.write(".");
        const after = self.module_env.numeralDigitsAfter(literal);
        const after_digits = try self.base256ToDecimalDigits(after);
        defer self.allocator.free(after_digits);

        const after_count: usize = @intCast(literal.after_decimal_digit_count);
        if (after_count <= after_digits.len) {
            try self.write(after_digits);
        } else {
            var pad = after_count - after_digits.len;
            while (pad > 0) : (pad -= 1) {
                try self.write("0");
            }
            try self.write(after_digits);
        }
    }

    if (maybe_type_name) |type_name| {
        try self.output.print(self.allocator, ".{s}", .{self.module_env.getIdent(type_name)});
    }
}

fn base256ToDecimalDigits(self: *Self, bytes_be: []const u8) std.mem.Allocator.Error![]u8 {
    var start: usize = 0;
    while (start < bytes_be.len and bytes_be[start] == 0) : (start += 1) {}
    if (start == bytes_be.len) {
        return try self.allocator.dupe(u8, "0");
    }

    const value = try self.allocator.dupe(u8, bytes_be[start..]);
    defer self.allocator.free(value);

    var digits = std.ArrayList(u8).empty;
    defer digits.deinit(self.allocator);

    var active = value;
    while (active.len > 0) {
        var remainder: u16 = 0;
        for (active) |*byte| {
            const next = remainder * 256 + byte.*;
            byte.* = @intCast(next / 10);
            remainder = next % 10;
        }
        try digits.append(self.allocator, @as(u8, @intCast(remainder)) + '0');

        var trim: usize = 0;
        while (trim < active.len and active[trim] == 0) : (trim += 1) {}
        active = active[trim..];
    }

    const out = try self.allocator.alloc(u8, digits.items.len);
    for (out, 0..) |*digit, i| {
        digit.* = digits.items[digits.items.len - 1 - i];
    }
    return out;
}

fn addPatternToScope(self: *Self, pattern_idx: CIR.Pattern.Idx) Allocator.Error!void {
    const pattern = self.module_env.store.getPattern(pattern_idx);
    if (pattern == .assign) {
        const name = self.module_env.getIdent(pattern.assign.ident);
        try self.names_in_scope.put(name, {});
    }
    // For other pattern types (destructuring, etc.), we could recursively add names
    // but for now just handling simple assigns
}

fn emitIntValue(self: *Self, value: CIR.IntValue) Allocator.Error!void {
    const str = try value.bufPrint(try self.scratchBuffer(40));
    try self.write(str);
}

fn emitIndent(self: *Self) Allocator.Error!void {
    for (0..self.indent_level) |_| {
        try self.write("\t");
    }
}

fn write(self: *Self, str: []const u8) Allocator.Error!void {
    try self.output.appendSlice(self.allocator, str);
}

fn scratchBuffer(self: *Self, len: usize) std.mem.Allocator.Error![]u8 {
    try self.scratch.resize(self.allocator, len);
    return self.scratch.items;
}

fn formatI128(self: *Self, value: i128) std.mem.Allocator.Error![]const u8 {
    const buf = try self.scratchBuffer(40);
    return i128h.i128_to_str(buf, value).str;
}

fn formatU128(self: *Self, value: u128) std.mem.Allocator.Error![]const u8 {
    const buf = try self.scratchBuffer(40);
    return i128h.u128_to_str(buf, value).str;
}

fn formatF32(self: *Self, value: f32) std.mem.Allocator.Error![]const u8 {
    const buf = try self.scratchBuffer(400);
    return i128h.f32_to_str(buf, value);
}

fn formatF64(self: *Self, value: f64) std.mem.Allocator.Error![]const u8 {
    const buf = try self.scratchBuffer(400);
    return i128h.f64_to_str(buf, value);
}

/// Emit a tag name, transforming compiler-generated `#` prefix to `C`.
/// This handles closure tag names like "#1_foo" which become "C1_foo" in output.
/// The `#` prefix is used internally because it's reserved for comments in Roc
/// source code, ensuring no collision with user-defined tag names.
fn emitTagName(self: *Self, name: []const u8) Allocator.Error!void {
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
fn emitIdent(self: *Self, name: []const u8) Allocator.Error!void {
    if (name.len > 0 and name[0] == '#') {
        // Compiler-generated ident: replace # with c (lowercase for functions)
        try self.output.append(self.allocator, 'c');
        try self.output.appendSlice(self.allocator, name[1..]);
    } else {
        // Regular user identifier: emit as-is
        try self.output.appendSlice(self.allocator, name);
    }
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
