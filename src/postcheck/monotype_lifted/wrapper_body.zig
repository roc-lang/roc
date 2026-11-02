//! Wrapper bodies: a function whose body does less work than a call to it
//! costs. Such a body is one operation, a constant, or a single guard whose
//! arms are each of those or a literal crash, and it reads nothing but its
//! arguments. Inlining it at a call never grows the program, so every stage
//! that inlines direct calls admits it.

const std = @import("std");
const collections = @import("collections");
const Ast = @import("ast.zig");

const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

/// The body of `fn_id` when it is a wrapper with no lifted captures.
pub fn wrapperBody(allocator: Allocator, program: *const Ast.Program, fn_id: Ast.FnId) Allocator.Error!?Ast.ExprId {
    const source_fn = program.getFn(fn_id);
    if (program.typedLocalSpan(source_fn.captures).len != 0) return null;
    const body = switch (source_fn.body) {
        .roc => |body_expr| body_expr,
        .hosted => return null,
    };
    if (!try isInlineableWrapperBody(allocator, program, body)) return null;
    if (!try bodyReadsOnlyArgs(allocator, program, fn_id, body)) return null;
    return body;
}

fn bodyReadsOnlyArgs(allocator: Allocator, program: *const Ast.Program, fn_id: Ast.FnId, body: Ast.ExprId) Allocator.Error!bool {
    const source_fn = program.getFn(fn_id);
    return exprReadsOnlyArgs(allocator, program, body, program.typedLocalSpan(source_fn.args));
}

/// Whether `root` reads no local other than `args`. Every subexpression
/// must, so they are checked in any order on a work stack.
fn exprReadsOnlyArgs(allocator: Allocator, program: *const Ast.Program, root: Ast.ExprId, args: anytype) Allocator.Error!bool {
    const lifted = program;
    var stack: std.ArrayList(Ast.ExprChild) = .empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, .{ .expr = root });
    while (stack.pop()) |child| {
        const expr_id = child.expr;
        switch (lifted.getExpr(expr_id).data) {
            .local => |local| if (!localIsArg(local, args)) return false,
            .@"unreachable",
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .crash,
            .checked_error,
            .bytes_lit,
            .def_ref,
            => {},
            .fn_ref,
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
            .static_data_candidate,
            .comptime_value,
            .typed_boundary,
            .row_widen,
            .nominal,
            .dbg,
            .expect,
            .return_,
            .expect_err,
            .literal_rejected,
            .call_value,
            .low_level,
            .field_access,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            => try Ast.appendChildren(allocator, lifted, expr_id, &stack),
            .call_proc => |call| {
                if (call.is_cold) return false;
                try Ast.appendChildren(allocator, lifted, expr_id, &stack);
            },
            .block => |block| if (!isLiteralCrash(program, expr_id)) {
                if (lifted.stmtSpan(block.statements).len != 0) return false;
                try stack.append(allocator, .{ .expr = block.final_expr });
            },
            .if_ => {
                // This admits guarded wrappers, not arbitrary conditional
                // arguments inside otherwise call-through wrappers.
                if (!try isInlineableWrapperBody(allocator, program, expr_id)) return false;
                try Ast.appendChildren(allocator, lifted, expr_id, &stack);
            },
            .lambda,
            .fn_def,
            .let_,
            .match_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .comptime_exhaustiveness_failed,
            => return false,
        }
    }
    return true;
}

fn localIsArg(local: Ast.LocalId, args: anytype) bool {
    for (0..args.len) |index| {
        const arg = GuardedList.at(args, index);
        if (arg.local == local) return true;
    }
    return false;
}

/// What a wrapper position must be: a whole wrapper body, a guard arm
/// (a literal crash or an operand), or an operand a wrapper builds its
/// result from (an argument, a literal, or another wrapper body).
const WrapperRole = enum { body, arm, operand };

/// Whether a body is a checked wrapper: one operation, a constant, or a
/// single guard whose arms are each of those or a crash. Such a body does
/// less work than its call costs, and substituting it at every call site
/// exposes its guard and its constant arguments to LIR range analysis
/// before backend instruction selection. `List.get` and the byte reads
/// have this shape with a `Try` on each arm. Every position must qualify
/// in its role, so positions are checked in any order on a work stack.
fn isInlineableWrapperBody(allocator: Allocator, program: *const Ast.Program, root: Ast.ExprId) Allocator.Error!bool {
    const lifted = program;
    const Item = struct { role: WrapperRole, expr: Ast.ExprId };
    var stack: std.ArrayList(Item) = .empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, .{ .role = .body, .expr = root });
    while (stack.pop()) |item| {
        const expr = lifted.getExpr(item.expr);
        // An arm that returns an argument or a literal, the bound of a
        // saturating operation say, is as cheap as one that calls, so an
        // arm qualifies as any operand does.
        const position: WrapperRole = switch (item.role) {
            .arm => if (isLiteralCrash(program, item.expr)) continue else .operand,
            .operand, .body => item.role,
        };
        const role: WrapperRole = switch (position) {
            .arm => unreachable, // the switch above maps an arm to .operand or skips it
            .operand => switch (expr.data) {
                .local,
                .unit,
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .bytes_lit,
                => continue,
                .call_proc,
                .low_level,
                .tag,
                .nominal,
                .if_,
                .block,
                => .body,
                .@"unreachable",
                .crash,
                .checked_error,
                .def_ref,
                .fn_ref,
                .list,
                .tuple,
                .record,
                .record_update,
                .static_data_candidate,
                .comptime_value,
                .typed_boundary,
                .row_widen,
                .dbg,
                .expect,
                .return_,
                .expect_err,
                .literal_rejected,
                .call_value,
                .field_access,
                .tuple_access,
                .structural_eq,
                .structural_hash,
                .lambda,
                .fn_def,
                .let_,
                .match_,
                .uninitialized,
                .uninitialized_payload,
                .if_initialized_payload,
                .try_sequence,
                .try_record_sequence,
                .loop_,
                .break_,
                .continue_,
                .join_point,
                .jump,
                .comptime_exhaustiveness_failed,
                => return false,
            },
            .body => .body,
        };
        std.debug.assert(role == .body);
        switch (expr.data) {
            .call_proc, .low_level => {},
            .tag => |tag| {
                const payloads = lifted.exprSpan(tag.payloads);
                for (0..payloads.len) |index| try stack.append(allocator, .{ .role = .operand, .expr = GuardedList.at(payloads, index) });
            },
            .nominal => |backing| try stack.append(allocator, .{ .role = .body, .expr = backing }),
            .if_ => |if_| {
                const branches = lifted.ifBranchSpan(if_.branches);
                if (branches.len != 1) return false;
                try stack.append(allocator, .{ .role = .arm, .expr = GuardedList.at(branches, 0).body });
                try stack.append(allocator, .{ .role = .arm, .expr = if_.final_else });
            },
            .block => |block| {
                if (lifted.stmtSpan(block.statements).len != 0) return false;
                try stack.append(allocator, .{ .role = .body, .expr = block.final_expr });
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => return false,
        }
    }
    return true;
}

/// Whether an expression is a crash with a literal message, possibly
/// under blocks that hold nothing else.
fn isLiteralCrash(program: *const Ast.Program, root: Ast.ExprId) bool {
    const lifted = program;
    var expr_id = root;
    while (true) {
        switch (lifted.getExpr(expr_id).data) {
            .crash, .checked_error => return true,
            .low_level => |call| {
                if (call.op != .crash) return false;
                const args = lifted.exprSpan(call.args);
                return args.len == 1 and
                    isStringLiteral(program, GuardedList.at(args, 0));
            },
            .block => |block| {
                const stmts = lifted.stmtSpan(block.statements);
                if (stmts.len == 0) {
                    expr_id = block.final_expr;
                    continue;
                }
                // SpecConstr normalizes a terminal crash to a statement
                // followed by unreachable. No preceding work is admitted.
                if (stmts.len != 1 or lifted.getExpr(block.final_expr).data != .@"unreachable") return false;
                switch (lifted.getStmt(GuardedList.at(stmts, 0))) {
                    .crash, .checked_error => return true,
                    .expr => |child| expr_id = child,
                    .uninitialized, .let_, .expect, .dbg, .return_ => return false,
                }
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .break_, .continue_, .join_point, .jump, .return_, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => return false,
        }
    }
}

fn isStringLiteral(program: *const Ast.Program, root: Ast.ExprId) bool {
    var expr_id = root;
    while (true) {
        switch (program.getExpr(expr_id).data) {
            .str_lit => return true,
            .nominal => |backing| expr_id = backing,
            .typed_boundary => |boundary| expr_id = boundary.value,
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .bytes_lit, .static_data_candidate, .comptime_value, .row_widen, .list, .tuple, .record, .record_update, .tag, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => return false,
        }
    }
}
