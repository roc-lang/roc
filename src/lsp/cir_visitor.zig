//! Generic CIR (Canonicalized Intermediate Representation) visitor module
//! for traversing the Roc compiler's CIR tree structure.
//!
//! This module provides a comptime-generic visitor pattern that eliminates
//! duplicated traversal code across multiple LSP functions that need to
//! walk the CIR tree (find-references, go-to-definition, type-at-offset, etc.).
//!
//! Usage:
//! ```zig
//! const MyContext = struct {
//!     target_offset: u32,
//!     result: ?SomeResult,
//! };
//!
//! var ctx = MyContext{ .target_offset = 42, .result = null };
//! var visitor = CirVisitor(MyContext).init(&ctx, .{
//!     .visit_expr_pre = myExprCallback,
//! });
//! visitor.walkExpr(module_env, expr_idx);
//! ```

const std = @import("std");
const can = @import("can");
const CIR = can.CIR;
const NodeStore = can.NodeStore;

/// Action returned by visitor callbacks to control traversal flow.
pub const VisitAction = enum {
    /// Continue traversing children of the current node.
    continue_traversal,
    /// Skip children of this node but continue with siblings.
    skip_children,
    /// Stop all traversal immediately.
    stop,
};

/// A comptime-generic CIR visitor that walks expression, statement, pattern,
/// and type annotation trees, calling user-provided callbacks at each node.
///
/// The Context type parameter allows the visitor to carry state through the
/// traversal (e.g., target offset, best match tracking, result accumulators).
pub fn CirVisitor(comptime Context: type) type {
    return struct {
        const Self = @This();

        /// User context passed to all callbacks
        ctx: *Context,

        /// Callback invoked before visiting an expression's children.
        /// Return .skip_children to avoid recursing into children.
        /// Return .stop to halt all traversal.
        visit_expr_pre: ?*const fn (*Context, CIR.Expr.Idx, CIR.Expr) VisitAction = null,

        /// Callback invoked after visiting an expression's children.
        visit_expr_post: ?*const fn (*Context, CIR.Expr.Idx, CIR.Expr) void = null,

        /// Callback invoked before visiting a statement's children.
        visit_stmt_pre: ?*const fn (*Context, CIR.Statement.Idx, CIR.Statement) VisitAction = null,

        /// Callback invoked after visiting a statement's children.
        visit_stmt_post: ?*const fn (*Context, CIR.Statement.Idx, CIR.Statement) void = null,

        /// Callback invoked before visiting a pattern's children.
        visit_pattern_pre: ?*const fn (*Context, CIR.Pattern.Idx, CIR.Pattern) VisitAction = null,

        /// Callback invoked after visiting a pattern's children.
        visit_pattern_post: ?*const fn (*Context, CIR.Pattern.Idx, CIR.Pattern) void = null,

        /// Callback invoked before visiting a type annotation's children.
        visit_type_anno_pre: ?*const fn (*Context, CIR.TypeAnno.Idx, CIR.TypeAnno) VisitAction = null,

        /// Callback invoked after visiting a type annotation's children.
        visit_type_anno_post: ?*const fn (*Context, CIR.TypeAnno.Idx, CIR.TypeAnno) void = null,

        /// Internal flag to track if traversal should stop
        stopped: bool = false,

        /// Configuration options for visitor callbacks
        pub const Callbacks = struct {
            visit_expr_pre: ?*const fn (*Context, CIR.Expr.Idx, CIR.Expr) VisitAction = null,
            visit_expr_post: ?*const fn (*Context, CIR.Expr.Idx, CIR.Expr) void = null,
            visit_stmt_pre: ?*const fn (*Context, CIR.Statement.Idx, CIR.Statement) VisitAction = null,
            visit_stmt_post: ?*const fn (*Context, CIR.Statement.Idx, CIR.Statement) void = null,
            visit_pattern_pre: ?*const fn (*Context, CIR.Pattern.Idx, CIR.Pattern) VisitAction = null,
            visit_pattern_post: ?*const fn (*Context, CIR.Pattern.Idx, CIR.Pattern) void = null,
            visit_type_anno_pre: ?*const fn (*Context, CIR.TypeAnno.Idx, CIR.TypeAnno) VisitAction = null,
            visit_type_anno_post: ?*const fn (*Context, CIR.TypeAnno.Idx, CIR.TypeAnno) void = null,
        };

        /// Initialize a visitor with the given context and callbacks.
        pub fn init(ctx: *Context, callbacks: Callbacks) Self {
            return Self{
                .ctx = ctx,
                .visit_expr_pre = callbacks.visit_expr_pre,
                .visit_expr_post = callbacks.visit_expr_post,
                .visit_stmt_pre = callbacks.visit_stmt_pre,
                .visit_stmt_post = callbacks.visit_stmt_post,
                .visit_pattern_pre = callbacks.visit_pattern_pre,
                .visit_pattern_post = callbacks.visit_pattern_post,
                .visit_type_anno_pre = callbacks.visit_type_anno_pre,
                .visit_type_anno_post = callbacks.visit_type_anno_post,
            };
        }

        /// Walk an expression tree, invoking callbacks at each node.
        pub fn walkExpr(self: *Self, store: *const NodeStore, expr_idx: CIR.Expr.Idx) void {
            if (self.stopped) return;

            const expr = store.getExpr(expr_idx);

            // Pre-visit callback
            if (self.visit_expr_pre) |hook| {
                switch (hook(self.ctx, expr_idx, expr)) {
                    .continue_traversal => {},
                    .skip_children => {
                        if (self.visit_expr_post) |post| post(self.ctx, expr_idx, expr);
                        return;
                    },
                    .stop => {
                        self.stopped = true;
                        return;
                    },
                }
            }

            // Walk children based on expression type
            switch (expr) {
                .e_lambda => |lambda| {
                    for (store.slicePatterns(lambda.args)) |arg_idx| {
                        self.walkPattern(store, arg_idx);
                        if (self.stopped) return;
                    }
                    self.walkExpr(store, lambda.body);
                },
                .e_closure => |closure| {
                    self.walkExpr(store, closure.lambda_idx);
                },
                .e_block => |block| {
                    for (store.sliceStatements(block.stmts)) |stmt_idx| {
                        self.walkStatement(store, stmt_idx);
                        if (self.stopped) return;
                    }
                    self.walkExpr(store, block.final_expr);
                },
                .e_if => |if_expr| {
                    for (store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                        const branch = store.getIfBranch(branch_idx);
                        self.walkExpr(store, branch.cond);
                        if (self.stopped) return;
                        self.walkExpr(store, branch.body);
                        if (self.stopped) return;
                    }
                    self.walkExpr(store, if_expr.final_else);
                },
                .e_match => |match_expr| {
                    self.walkExpr(store, match_expr.cond);
                    if (self.stopped) return;

                    for (store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                        const branch = store.getMatchBranch(branch_idx);
                        // Walk branch patterns
                        for (store.sliceMatchBranchPatterns(branch.patterns)) |bp_idx| {
                            const bp = store.getMatchBranchPattern(bp_idx);
                            self.walkPattern(store, bp.pattern);
                            if (self.stopped) return;
                        }
                        self.walkExpr(store, branch.value);
                        if (self.stopped) return;
                        if (branch.guard) |guard| {
                            self.walkExpr(store, guard);
                            if (self.stopped) return;
                        }
                    }
                },
                .e_call => |call| {
                    self.walkExpr(store, call.func);
                    if (self.stopped) return;
                    for (store.sliceExpr(call.args)) |arg| {
                        self.walkExpr(store, arg);
                        if (self.stopped) return;
                    }
                },
                .e_binop => |binop| {
                    self.walkExpr(store, binop.lhs);
                    if (self.stopped) return;
                    self.walkExpr(store, binop.rhs);
                },
                .e_unary_minus => |u| {
                    self.walkExpr(store, u.expr);
                },
                .e_unary_not => |u| {
                    self.walkExpr(store, u.expr);
                },
                .e_dot_access => |dot| {
                    self.walkExpr(store, dot.receiver);
                    if (self.stopped) return;
                    if (dot.args) |args_span| {
                        for (store.sliceExpr(args_span)) |arg| {
                            self.walkExpr(store, arg);
                            if (self.stopped) return;
                        }
                    }
                },
                .e_tuple_access => |ta| {
                    self.walkExpr(store, ta.tuple);
                },
                .e_list => |list| {
                    for (store.sliceExpr(list.elems)) |elem| {
                        self.walkExpr(store, elem);
                        if (self.stopped) return;
                    }
                },
                .e_tuple => |tuple| {
                    for (store.sliceExpr(tuple.elems)) |elem| {
                        self.walkExpr(store, elem);
                        if (self.stopped) return;
                    }
                },
                .e_record => |rec| {
                    for (store.sliceRecordFields(rec.fields)) |field_idx| {
                        const field = store.getRecordField(field_idx);
                        self.walkExpr(store, field.value);
                        if (self.stopped) return;
                    }
                    if (rec.ext) |ext| {
                        self.walkExpr(store, ext);
                    }
                },
                .e_str => |str| {
                    for (store.sliceExpr(str.span)) |seg| {
                        self.walkExpr(store, seg);
                        if (self.stopped) return;
                    }
                },
                .e_tag => |tag| {
                    for (store.sliceExpr(tag.args)) |arg| {
                        self.walkExpr(store, arg);
                        if (self.stopped) return;
                    }
                },
                .e_nominal => |nom| {
                    self.walkExpr(store, nom.backing_expr);
                },
                .e_nominal_external => |nom| {
                    self.walkExpr(store, nom.backing_expr);
                },
                .e_dbg => |dbg| {
                    self.walkExpr(store, dbg.expr);
                },
                .e_expect => |exp| {
                    self.walkExpr(store, exp.body);
                },
                .e_return => |ret| {
                    self.walkExpr(store, ret.expr);
                },
                .e_for => |for_expr| {
                    self.walkPattern(store, for_expr.patt);
                    if (self.stopped) return;
                    self.walkExpr(store, for_expr.expr);
                    if (self.stopped) return;
                    self.walkExpr(store, for_expr.body);
                },
                .e_type_var_dispatch => |tvd| {
                    for (store.sliceExpr(tvd.args)) |arg| {
                        self.walkExpr(store, arg);
                        if (self.stopped) return;
                    }
                },
                .e_hosted_lambda => |hosted| {
                    for (store.slicePatterns(hosted.args)) |arg_idx| {
                        self.walkPattern(store, arg_idx);
                        if (self.stopped) return;
                    }
                    self.walkExpr(store, hosted.body);
                },
                .e_run_low_level => |run_low_level| {
                    for (store.sliceExpr(run_low_level.args)) |arg_idx| {
                        self.walkExpr(store, arg_idx);
                        if (self.stopped) return;
                    }
                },
                // Leaf nodes - no children to traverse
                .e_num,
                .e_frac_f32,
                .e_frac_f64,
                .e_dec,
                .e_dec_small,
                .e_typed_int,
                .e_typed_frac,
                .e_str_segment,
                .e_empty_list,
                .e_empty_record,
                .e_lookup_local,
                .e_lookup_external,
                .e_lookup_required,
                .e_lookup_pending,
                .e_zero_argument_tag,
                .e_runtime_error,
                .e_crash,
                .e_ellipsis,
                .e_anno_only,
                => {},
            }

            if (self.stopped) return;

            // Post-visit callback
            if (self.visit_expr_post) |post| post(self.ctx, expr_idx, expr);
        }

        /// Walk a statement tree, invoking callbacks at each node.
        pub fn walkStatement(self: *Self, store: *const NodeStore, stmt_idx: CIR.Statement.Idx) void {
            if (self.stopped) return;

            const stmt = store.getStatement(stmt_idx);

            // Pre-visit callback
            if (self.visit_stmt_pre) |hook| {
                switch (hook(self.ctx, stmt_idx, stmt)) {
                    .continue_traversal => {},
                    .skip_children => {
                        if (self.visit_stmt_post) |post| post(self.ctx, stmt_idx, stmt);
                        return;
                    },
                    .stop => {
                        self.stopped = true;
                        return;
                    },
                }
            }

            // Walk children based on statement type
            switch (stmt) {
                .s_decl => |d| {
                    self.walkPattern(store, d.pattern);
                    if (self.stopped) return;
                    self.walkExpr(store, d.expr);
                    if (self.stopped) return;
                    if (d.anno) |a| self.walkAnnotation(store, a);
                },
                .s_var => |v| {
                    self.walkPattern(store, v.pattern_idx);
                    if (self.stopped) return;
                    self.walkExpr(store, v.expr);
                    if (self.stopped) return;
                    if (v.anno) |a| self.walkAnnotation(store, a);
                },
                .s_reassign => |r| {
                    self.walkPattern(store, r.pattern_idx);
                    if (self.stopped) return;
                    self.walkExpr(store, r.expr);
                },
                .s_for => |f| {
                    self.walkPattern(store, f.patt);
                    if (self.stopped) return;
                    self.walkExpr(store, f.expr);
                    if (self.stopped) return;
                    self.walkExpr(store, f.body);
                },
                .s_while => |w| {
                    self.walkExpr(store, w.cond);
                    if (self.stopped) return;
                    self.walkExpr(store, w.body);
                },
                .s_expr => |e| {
                    self.walkExpr(store, e.expr);
                },
                .s_expect => |e| {
                    self.walkExpr(store, e.body);
                },
                .s_dbg => |d| {
                    self.walkExpr(store, d.expr);
                },
                .s_return => |r| {
                    self.walkExpr(store, r.expr);
                },
                .s_type_anno => |t| {
                    self.walkTypeAnno(store, t.anno);
                    if (self.stopped) return;
                    if (t.where) |where_span| {
                        self.walkWhereClauses(store, where_span);
                    }
                },
                .s_alias_decl => |a| {
                    self.walkTypeAnno(store, a.anno);
                },
                .s_nominal_decl => |n| {
                    self.walkTypeAnno(store, n.anno);
                },
                // Leaf statements - no children to traverse
                .s_crash,
                .s_break,
                .s_import,
                .s_type_var_alias,
                .s_runtime_error,
                => {},
            }

            if (self.stopped) return;

            // Post-visit callback
            if (self.visit_stmt_post) |post| post(self.ctx, stmt_idx, stmt);
        }

        /// Walk a pattern tree, invoking callbacks at each node.
        pub fn walkPattern(self: *Self, store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) void {
            if (self.stopped) return;

            const pattern = store.getPattern(pattern_idx);

            // Pre-visit callback
            if (self.visit_pattern_pre) |hook| {
                switch (hook(self.ctx, pattern_idx, pattern)) {
                    .continue_traversal => {},
                    .skip_children => {
                        if (self.visit_pattern_post) |post| post(self.ctx, pattern_idx, pattern);
                        return;
                    },
                    .stop => {
                        self.stopped = true;
                        return;
                    },
                }
            }

            // Walk children based on pattern type
            switch (pattern) {
                .as => |a| {
                    self.walkPattern(store, a.pattern);
                },
                .applied_tag => |t| {
                    for (store.slicePatterns(t.args)) |arg| {
                        self.walkPattern(store, arg);
                        if (self.stopped) return;
                    }
                },
                .nominal => |n| {
                    self.walkPattern(store, n.backing_pattern);
                },
                .nominal_external => |n| {
                    self.walkPattern(store, n.backing_pattern);
                },
                .record_destructure => |r| {
                    for (store.sliceRecordDestructs(r.destructs)) |d_idx| {
                        const destruct = store.getRecordDestruct(d_idx);
                        self.walkPattern(store, destruct.kind.toPatternIdx());
                        if (self.stopped) return;
                    }
                },
                .list => |l| {
                    for (store.slicePatterns(l.patterns)) |p| {
                        self.walkPattern(store, p);
                        if (self.stopped) return;
                    }
                    if (l.rest_info) |rest| {
                        if (rest.pattern) |rp| {
                            self.walkPattern(store, rp);
                        }
                    }
                },
                .tuple => |t| {
                    for (store.slicePatterns(t.patterns)) |p| {
                        self.walkPattern(store, p);
                        if (self.stopped) return;
                    }
                },
                // Leaf patterns - no children to traverse
                .assign,
                .num_literal,
                .small_dec_literal,
                .dec_literal,
                .frac_f32_literal,
                .frac_f64_literal,
                .str_literal,
                .underscore,
                .runtime_error,
                => {},
            }

            if (self.stopped) return;

            // Post-visit callback
            if (self.visit_pattern_post) |post| post(self.ctx, pattern_idx, pattern);
        }

        /// Walk a type annotation tree, invoking callbacks at each node.
        pub fn walkTypeAnno(self: *Self, store: *const NodeStore, anno_idx: CIR.TypeAnno.Idx) void {
            if (self.stopped) return;

            const anno = store.getTypeAnno(anno_idx);

            // Pre-visit callback
            if (self.visit_type_anno_pre) |hook| {
                switch (hook(self.ctx, anno_idx, anno)) {
                    .continue_traversal => {},
                    .skip_children => {
                        if (self.visit_type_anno_post) |post| post(self.ctx, anno_idx, anno);
                        return;
                    },
                    .stop => {
                        self.stopped = true;
                        return;
                    },
                }
            }

            // Walk children based on type annotation type
            switch (anno) {
                .apply => |a| {
                    for (store.sliceTypeAnnos(a.args)) |arg| {
                        self.walkTypeAnno(store, arg);
                        if (self.stopped) return;
                    }
                },
                .rigid_var_lookup => |r| {
                    self.walkTypeAnno(store, r.ref);
                },
                .tag_union => |tu| {
                    for (store.sliceTypeAnnos(tu.tags)) |tag| {
                        self.walkTypeAnno(store, tag);
                        if (self.stopped) return;
                    }
                    if (tu.ext) |ext| {
                        self.walkTypeAnno(store, ext);
                    }
                },
                .tag => |t| {
                    for (store.sliceTypeAnnos(t.args)) |arg| {
                        self.walkTypeAnno(store, arg);
                        if (self.stopped) return;
                    }
                },
                .tuple => |t| {
                    for (store.sliceTypeAnnos(t.elems)) |elem| {
                        self.walkTypeAnno(store, elem);
                        if (self.stopped) return;
                    }
                },
                .record => |r| {
                    for (store.sliceAnnoRecordFields(r.fields)) |f_idx| {
                        const field = store.getAnnoRecordField(f_idx);
                        self.walkTypeAnno(store, field.ty);
                        if (self.stopped) return;
                    }
                    if (r.ext) |ext| {
                        self.walkTypeAnno(store, ext);
                    }
                },
                .@"fn" => |f| {
                    for (store.sliceTypeAnnos(f.args)) |arg| {
                        self.walkTypeAnno(store, arg);
                        if (self.stopped) return;
                    }
                    self.walkTypeAnno(store, f.ret);
                },
                .parens => |p| {
                    self.walkTypeAnno(store, p.anno);
                },
                // Leaf type annotations - no children to traverse
                .rigid_var,
                .underscore,
                .lookup,
                .malformed,
                => {},
            }

            if (self.stopped) return;

            // Post-visit callback
            if (self.visit_type_anno_post) |post| post(self.ctx, anno_idx, anno);
        }

        /// Walk an annotation (type annotation with optional where clause)
        fn walkAnnotation(self: *Self, store: *const NodeStore, anno_idx: CIR.Annotation.Idx) void {
            if (self.stopped) return;

            const annotation = store.getAnnotation(anno_idx);
            self.walkTypeAnno(store, annotation.anno);

            if (self.stopped) return;

            if (annotation.where) |where_span| {
                self.walkWhereClauses(store, where_span);
            }
        }

        /// Walk where clauses
        fn walkWhereClauses(self: *Self, store: *const NodeStore, where_span: CIR.WhereClause.Span) void {
            for (store.sliceWhereClauses(where_span)) |clause_idx| {
                if (self.stopped) return;

                const clause = store.getWhereClause(clause_idx);
                switch (clause) {
                    .w_method => |m| {
                        self.walkTypeAnno(store, m.var_);
                        if (self.stopped) return;
                        for (store.sliceTypeAnnos(m.args)) |arg| {
                            self.walkTypeAnno(store, arg);
                            if (self.stopped) return;
                        }
                        self.walkTypeAnno(store, m.ret);
                    },
                    .w_alias => |a| {
                        self.walkTypeAnno(store, a.var_);
                    },
                    .w_malformed => {},
                }
            }
        }

        /// Walk all top-level statements in a module.
        /// This is the entry point for walking an entire module's CIR.
        pub fn walkModule(self: *Self, store: *const NodeStore, stmts: CIR.Statement.Span) void {
            for (store.sliceStatements(stmts)) |stmt_idx| {
                self.walkStatement(store, stmt_idx);
                if (self.stopped) return;
            }
        }

        /// Walk all definitions in a module (Def contains pattern, expr, and optional annotation).
        pub fn walkDefs(self: *Self, store: *const NodeStore, defs: []const CIR.Def) void {
            for (defs) |def| {
                self.walkPattern(store, def.pattern);
                if (self.stopped) return;
                self.walkExpr(store, def.expr);
                if (self.stopped) return;
                if (def.annotation) |anno| {
                    self.walkAnnotation(store, anno);
                    if (self.stopped) return;
                }
            }
        }
    };
}

// Tests

test "CirVisitor basic initialization" {
    const TestContext = struct {
        count: u32,
    };

    var ctx = TestContext{ .count = 0 };
    const visitor = CirVisitor(TestContext).init(&ctx, .{});

    // Just verify the visitor initializes without error
    try std.testing.expect(visitor.ctx == &ctx);
    try std.testing.expect(visitor.stopped == false);
}
