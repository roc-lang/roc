//! Shared layout resolution for CIR expressions.
//!
//! Provides functions to determine the result layout (type) of CIR expressions.
//! Used by both the dev backend evaluator and the LLVM evaluator.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

pub const LayoutIdx = layout.Idx;

// Special layout indices for compound types (beyond the 16 scalar sentinels).
// These are used by the evaluator to track result types for proper formatting.
pub const list_i64_layout: LayoutIdx = @enumFromInt(100);
pub const fn_layout: LayoutIdx = @enumFromInt(101);
pub const tuple_layout: LayoutIdx = @enumFromInt(102);
pub const record_layout: LayoutIdx = @enumFromInt(103);
pub const tag_layout: LayoutIdx = @enumFromInt(104);

/// Get the Layout for a CIR expression
pub fn getExprLayoutWithTypeEnv(allocator: Allocator, module_env: *ModuleEnv, expr: CIR.Expr, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    return switch (expr) {
        .e_num => |num| switch (num.kind) {
            .i8, .i16, .i32, .i64, .num_unbound, .int_unbound => .i64,
            .u8, .u16, .u32, .u64 => .u64,
            .i128 => .i128,
            .u128 => .u128,
            .f32 => .f32,
            .f64 => .f64,
            .dec => .dec,
        },
        .e_frac_f32 => .f32,
        .e_frac_f64 => .f64,
        .e_dec, .e_dec_small => .dec,
        .e_typed_int => |ti| getTypedIntLayout(module_env, ti.type_name),
        .e_binop => |binop| getBinopLayout(allocator, module_env, binop, type_env),
        .e_unary_minus => |unary| blk: {
            const inner_expr = module_env.store.getExpr(unary.expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, inner_expr, type_env);
        },
        .e_nominal => |nom| blk: {
            const backing_expr = module_env.store.getExpr(nom.backing_expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, backing_expr, type_env);
        },
        .e_nominal_external => |nom| blk: {
            const backing_expr = module_env.store.getExpr(nom.backing_expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, backing_expr, type_env);
        },
        .e_if => |if_expr| blk: {
            const else_expr = module_env.store.getExpr(if_expr.final_else);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, else_expr, type_env);
        },
        .e_block => |block| getBlockLayout(allocator, module_env, block, type_env),
        .e_lookup_local => |lookup| blk: {
            const pattern_key = @intFromEnum(lookup.pattern_idx);
            break :blk type_env.get(pattern_key) orelse .i64;
        },
        .e_str, .e_str_segment => .str,
        .e_call => |call| blk: {
            const func_expr = module_env.store.getExpr(call.func);
            switch (func_expr) {
                .e_lambda => |lambda| {
                    const body_expr = module_env.store.getExpr(lambda.body);
                    switch (body_expr) {
                        .e_lookup_local => {
                            const args = module_env.store.sliceExpr(call.args);
                            if (args.len > 0) {
                                const arg_expr = module_env.store.getExpr(args[0]);
                                break :blk getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
                            }
                        },
                        else => {},
                    }
                    break :blk getExprLayoutWithTypeEnv(allocator, module_env, body_expr, type_env);
                },
                .e_closure => |closure| {
                    const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
                    switch (lambda_expr) {
                        .e_lambda => |lambda| {
                            const body_expr = module_env.store.getExpr(lambda.body);
                            switch (body_expr) {
                                .e_lookup_local => {
                                    const args = module_env.store.sliceExpr(call.args);
                                    if (args.len > 0) {
                                        const arg_expr = module_env.store.getExpr(args[0]);
                                        break :blk getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
                                    }
                                },
                                else => {},
                            }
                            break :blk getExprLayoutWithTypeEnv(allocator, module_env, body_expr, type_env);
                        },
                        else => {},
                    }
                    break :blk .i64;
                },
                .e_lookup_local => {
                    const args = module_env.store.sliceExpr(call.args);
                    if (args.len > 0) {
                        const arg_expr = module_env.store.getExpr(args[0]);
                        const arg_layout = getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
                        if (arg_layout == .str) {
                            break :blk .str;
                        }
                    }
                    break :blk .i64;
                },
                else => {},
            }
            break :blk .i64;
        },
        .e_dot_access => |dot| blk: {
            // Get the receiver expression and check if it's a record
            const receiver_expr = module_env.store.getExpr(dot.receiver);
            switch (receiver_expr) {
                .e_record => |rec| {
                    // Find the field with the matching name
                    const fields = module_env.store.sliceRecordFields(rec.fields);
                    for (fields) |field_idx| {
                        const field = module_env.store.getRecordField(field_idx);
                        if (@as(u32, @bitCast(field.name)) == @as(u32, @bitCast(dot.field_name))) {
                            const field_expr = module_env.store.getExpr(field.value);
                            break :blk getExprLayoutWithTypeEnv(allocator, module_env, field_expr, type_env);
                        }
                    }
                },
                else => {},
            }
            break :blk .i64;
        },
        .e_list, .e_empty_list => list_i64_layout,
        .e_typed_frac => |tf| getTypedIntLayout(module_env, tf.type_name),
        .e_unary_not => .bool,
        .e_match => |match| blk: {
            const branches = module_env.store.sliceMatchBranches(match.branches);
            if (branches.len > 0) {
                const first_branch = module_env.store.getMatchBranch(branches[0]);
                const branch_expr = module_env.store.getExpr(first_branch.value);
                break :blk getExprLayoutWithTypeEnv(allocator, module_env, branch_expr, type_env);
            }
            break :blk .i64;
        },
        .e_tuple => tuple_layout,
        .e_record, .e_empty_record => record_layout,
        .e_tag => tag_layout,
        // Zero-argument tags include Bool.true/Bool.false which are represented
        // as small integers at runtime. Using .i64 keeps boolean tests working
        // (normalizeBoolStr handles the "0"/"1" to "False"/"True" conversion).
        .e_zero_argument_tag => .i64,
        .e_lambda, .e_closure, .e_hosted_lambda, .e_low_level_lambda => fn_layout,
        .e_dbg => |dbg| blk: {
            const inner_expr = module_env.store.getExpr(dbg.expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, inner_expr, type_env);
        },
        .e_expect => |expect| blk: {
            const body_expr = module_env.store.getExpr(expect.body);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, body_expr, type_env);
        },
        .e_return => |ret| blk: {
            const ret_expr = module_env.store.getExpr(ret.expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, ret_expr, type_env);
        },
        .e_for => |for_expr| blk: {
            const body_expr = module_env.store.getExpr(for_expr.body);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, body_expr, type_env);
        },
        // Tuple access result type depends on element; default to i64
        .e_tuple_access => .i64,
        // Cross-module lookups need full module resolution to determine layout
        .e_lookup_external, .e_lookup_required, .e_lookup_pending, .e_type_var_dispatch => .i64,
        // Error/placeholder expressions have no meaningful result type
        .e_runtime_error, .e_crash, .e_ellipsis, .e_anno_only => .i64,
    };
}

/// Get the result type for a block
pub fn getBlockLayout(allocator: Allocator, module_env: *ModuleEnv, block: anytype, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    var type_annos = std.AutoHashMap(base.Ident.Idx, LayoutIdx).init(allocator);
    defer type_annos.deinit();

    const stmts = module_env.store.sliceStatements(block.stmts);

    // First pass: collect type annotations
    for (stmts) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_type_anno => |ta| {
                const type_anno = module_env.store.getTypeAnno(ta.anno);
                switch (type_anno) {
                    .apply => |apply| {
                        const result_layout = layoutFromLocalOrExternal(apply.base);
                        type_annos.put(ta.name, result_layout) catch {};
                    },
                    .lookup => |lookup| {
                        const result_layout = layoutFromLocalOrExternal(lookup.base);
                        type_annos.put(ta.name, result_layout) catch {};
                    },
                    else => {},
                }
            },
            else => {},
        }
    }

    // Second pass: process declarations
    for (stmts) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                const pattern_key = @intFromEnum(decl.pattern);
                if (decl.anno) |anno_idx| {
                    const result_layout = getAnnotationLayout(module_env, anno_idx);
                    type_env.put(pattern_key, result_layout) catch {};
                } else {
                    const pattern = module_env.store.getPattern(decl.pattern);
                    var found_annotation = false;
                    switch (pattern) {
                        .assign => |assign| {
                            if (type_annos.get(assign.ident)) |anno_layout| {
                                type_env.put(pattern_key, anno_layout) catch {};
                                found_annotation = true;
                            }
                        },
                        else => {},
                    }
                    if (!found_annotation) {
                        const decl_expr = module_env.store.getExpr(decl.expr);
                        const inferred_layout = getExprLayoutWithTypeEnv(allocator, module_env, decl_expr, type_env);
                        type_env.put(pattern_key, inferred_layout) catch {};
                    }
                }
            },
            else => {},
        }
    }

    const final_expr = module_env.store.getExpr(block.final_expr);
    return getExprLayoutWithTypeEnv(allocator, module_env, final_expr, type_env);
}

/// Get the result type from an annotation
pub fn getAnnotationLayout(module_env: *ModuleEnv, anno_idx: CIR.Annotation.Idx) LayoutIdx {
    const anno = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(anno.anno);
    switch (type_anno) {
        .apply => |apply| return layoutFromLocalOrExternal(apply.base),
        .lookup => |lookup| return layoutFromLocalOrExternal(lookup.base),
        else => return .i64,
    }
}

/// Get the result type for a typed integer
pub fn getTypedIntLayout(module_env: *ModuleEnv, type_name: base.Ident.Idx) LayoutIdx {
    const idents = &module_env.idents;
    if (type_name == idents.u8_type or
        type_name == idents.u16_type or
        type_name == idents.u32_type or
        type_name == idents.u64_type)
    {
        return .u64;
    } else if (type_name == idents.u128_type) {
        return .u128;
    } else if (type_name == idents.i128_type) {
        return .i128;
    } else if (type_name == idents.f32_type) {
        return .f32;
    } else if (type_name == idents.f64_type) {
        return .f64;
    } else if (type_name == idents.dec_type) {
        return .dec;
    }
    return .i64;
}

/// Get the result type for a binary operation
pub fn getBinopLayout(allocator: Allocator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    switch (binop.op) {
        .lt, .gt, .le, .ge, .eq, .ne, .@"and", .@"or" => return .i64,
        else => {},
    }
    const lhs_expr = module_env.store.getExpr(binop.lhs);
    return getExprLayoutWithTypeEnv(allocator, module_env, lhs_expr, type_env);
}

/// Convert a LocalOrExternal to LayoutIdx
pub fn layoutFromLocalOrExternal(loe: CIR.TypeAnno.LocalOrExternal) LayoutIdx {
    switch (loe) {
        .builtin => |b| return layoutFromBuiltin(b),
        .local, .external, .pending => return .i64,
    }
}

/// Convert a Builtin type enum to LayoutIdx
pub fn layoutFromBuiltin(b: CIR.TypeAnno.Builtin) LayoutIdx {
    return switch (b) {
        .u8, .u16, .u32, .u64 => .u64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        else => .i64,
    };
}
