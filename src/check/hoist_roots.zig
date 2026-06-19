//! Shared checked-stage data for top-level-equivalent compile-time roots.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Var = types.Var;
const TypeStore = types.Store;
const FlatType = types.FlatType;

pub const selection_algorithm_version: u64 = 1;

pub const ValidationError = error{InvalidSelectedHoistedRootSet};

pub const SelectedHoistedRootSet = struct {
    roots: []const SelectedHoistedRoot,
};

pub const PatternExtraction = struct {
    base_expr: CIR.Expr.Idx,
    scrutinee_pattern: CIR.Pattern.Idx,
    result_pattern: CIR.Pattern.Idx,
};

pub const Body = union(enum) {
    expr,
    pattern_extraction: PatternExtraction,
};

/// A root selected during checking for compile-time evaluation.
///
/// This is producer-side data, not a permanent analysis summary. It is passed
/// into checked artifact publication, which either publishes it as a sparse
/// hoisted const root or rejects it through explicit checked root filters.
pub const SelectedHoistedRoot = struct {
    /// Expression that should be evaluated at checking finalization.
    ///
    /// For `.expr` bodies this is the existing source expression. For
    /// `.pattern_extraction` bodies this is the base expression matched against
    /// the original source pattern to materialize a binder.
    expr: CIR.Expr.Idx,
    /// Local binding pattern whose RHS is `expr`, when this root preserves local
    /// binding sharing. Null means the expression itself is the selected root.
    pattern: ?CIR.Pattern.Idx = null,
    /// The root body shape. Pattern extraction roots are sparse selected-root
    /// metadata; no per-expression hoistability summary is stored.
    body: Body = .expr,
};

pub fn validateSelectedRootSetForPublication(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    roots: []const SelectedHoistedRoot,
) (Allocator.Error || ValidationError)!void {
    var validator = try RootSetValidator.init(allocator, module_env, roots);
    defer validator.deinit();
    try validator.validate();
}

pub fn validateSelectedRootSetShape(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    roots: []const SelectedHoistedRoot,
) (Allocator.Error || ValidationError)!void {
    var shape = RootSetShapeValidator{
        .allocator = allocator,
        .module_env = module_env,
        .expr_roots = std.AutoHashMap(CIR.Expr.Idx, void).init(allocator),
        .pattern_roots = std.AutoHashMap(CIR.Pattern.Idx, void).init(allocator),
    };
    defer shape.deinit();
    try shape.validate(roots);
}

const RootSetShapeValidator = struct {
    allocator: Allocator,
    module_env: *const ModuleEnv,
    expr_roots: std.AutoHashMap(CIR.Expr.Idx, void),
    pattern_roots: std.AutoHashMap(CIR.Pattern.Idx, void),

    fn deinit(self: *RootSetShapeValidator) void {
        self.pattern_roots.deinit();
        self.expr_roots.deinit();
    }

    fn validate(self: *RootSetShapeValidator, roots: []const SelectedHoistedRoot) (Allocator.Error || ValidationError)!void {
        for (roots) |root| {
            try self.validateExprSource(root.expr);
            if (root.pattern) |pattern| {
                try self.validatePatternSource(pattern);
                const entry = try self.pattern_roots.getOrPut(pattern);
                if (entry.found_existing) return error.InvalidSelectedHoistedRootSet;
            }

            switch (root.body) {
                .expr => {
                    const entry = try self.expr_roots.getOrPut(root.expr);
                    if (entry.found_existing) return error.InvalidSelectedHoistedRootSet;
                },
                .pattern_extraction => |extraction| {
                    const pattern = root.pattern orelse return error.InvalidSelectedHoistedRootSet;
                    if (pattern != extraction.result_pattern) return error.InvalidSelectedHoistedRootSet;
                    if (root.expr != extraction.base_expr) return error.InvalidSelectedHoistedRootSet;
                    try self.validateExprSource(extraction.base_expr);
                    try self.validatePatternSource(extraction.scrutinee_pattern);
                    try self.validatePatternSource(extraction.result_pattern);
                },
            }
        }
    }

    fn validateExprSource(self: *RootSetShapeValidator, expr: CIR.Expr.Idx) ValidationError!void {
        const raw = @intFromEnum(expr);
        if (raw >= self.module_env.store.nodes.len()) return error.InvalidSelectedHoistedRootSet;
        const tag = self.module_env.store.nodes.get(@enumFromInt(raw)).tag;
        if (!nodeTagIsExpr(tag)) return error.InvalidSelectedHoistedRootSet;
    }

    fn validatePatternSource(self: *RootSetShapeValidator, pattern: CIR.Pattern.Idx) ValidationError!void {
        const raw = @intFromEnum(pattern);
        if (raw >= self.module_env.store.nodes.len()) return error.InvalidSelectedHoistedRootSet;
        const tag = self.module_env.store.nodes.get(@enumFromInt(raw)).tag;
        if (!nodeTagIsPattern(tag)) return error.InvalidSelectedHoistedRootSet;
    }
};

const DependencyContext = struct {
    patterns: std.ArrayListUnmanaged(CIR.Pattern.Idx) = .empty,

    fn deinit(self: *@This(), allocator: Allocator) void {
        self.patterns.deinit(allocator);
    }

    fn mark(self: *const @This()) usize {
        return self.patterns.items.len;
    }

    fn pop(self: *@This(), start: usize) void {
        self.patterns.shrinkRetainingCapacity(start);
    }

    fn contains(self: *const @This(), pattern: CIR.Pattern.Idx) bool {
        for (self.patterns.items) |allowed| {
            if (allowed == pattern) return true;
        }
        return false;
    }
};

const RootSetValidator = struct {
    allocator: Allocator,
    module_env: *const ModuleEnv,
    roots: []const SelectedHoistedRoot,
    expr_roots: std.AutoHashMap(CIR.Expr.Idx, u32),
    pattern_roots: std.AutoHashMap(CIR.Pattern.Idx, u32),
    top_level_patterns: std.AutoHashMap(CIR.Pattern.Idx, void),
    top_level_exprs: std.AutoHashMap(CIR.Expr.Idx, void),
    current_root_index: usize = 0,
    type_visited: std.AutoHashMap(Var, void),

    fn init(
        allocator: Allocator,
        module_env: *const ModuleEnv,
        roots: []const SelectedHoistedRoot,
    ) (Allocator.Error || ValidationError)!RootSetValidator {
        var validator = RootSetValidator{
            .allocator = allocator,
            .module_env = module_env,
            .roots = roots,
            .expr_roots = std.AutoHashMap(CIR.Expr.Idx, u32).init(allocator),
            .pattern_roots = std.AutoHashMap(CIR.Pattern.Idx, u32).init(allocator),
            .top_level_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(allocator),
            .top_level_exprs = std.AutoHashMap(CIR.Expr.Idx, void).init(allocator),
            .type_visited = std.AutoHashMap(Var, void).init(allocator),
        };
        errdefer validator.deinit();

        try validator.indexTopLevelDefs();
        try validator.indexSelectedRoots();
        return validator;
    }

    fn deinit(self: *RootSetValidator) void {
        self.type_visited.deinit();
        self.top_level_exprs.deinit();
        self.top_level_patterns.deinit();
        self.pattern_roots.deinit();
        self.expr_roots.deinit();
    }

    fn validate(self: *RootSetValidator) (Allocator.Error || ValidationError)!void {
        for (self.roots, 0..) |root, index| {
            self.current_root_index = index;
            try self.validateRoot(root);
        }
    }

    fn indexTopLevelDefs(self: *RootSetValidator) Allocator.Error!void {
        for (self.module_env.store.sliceDefs(self.module_env.global_value_defs)) |def_idx| {
            const def = self.module_env.store.getDef(def_idx);
            try self.top_level_patterns.put(def.pattern, {});
            try self.top_level_exprs.put(def.expr, {});
        }
    }

    fn indexSelectedRoots(self: *RootSetValidator) (Allocator.Error || ValidationError)!void {
        try self.expr_roots.ensureTotalCapacity(@intCast(self.roots.len));
        try self.pattern_roots.ensureTotalCapacity(@intCast(self.roots.len));
        for (self.roots, 0..) |root, index| {
            const root_index: u32 = @intCast(index);
            try self.validateExprSource(root.expr);
            if (self.top_level_exprs.contains(root.expr)) return error.InvalidSelectedHoistedRootSet;
            if (root.pattern) |pattern| {
                try self.validatePatternSource(pattern);
                if (self.top_level_patterns.contains(pattern)) return error.InvalidSelectedHoistedRootSet;
                const entry = try self.pattern_roots.getOrPut(pattern);
                if (entry.found_existing) return error.InvalidSelectedHoistedRootSet;
                entry.value_ptr.* = root_index;
            }

            switch (root.body) {
                .expr => {
                    const entry = try self.expr_roots.getOrPut(root.expr);
                    if (entry.found_existing) return error.InvalidSelectedHoistedRootSet;
                    entry.value_ptr.* = root_index;
                },
                .pattern_extraction => |extraction| {
                    const pattern = root.pattern orelse return error.InvalidSelectedHoistedRootSet;
                    if (pattern != extraction.result_pattern) return error.InvalidSelectedHoistedRootSet;
                    if (root.expr != extraction.base_expr) return error.InvalidSelectedHoistedRootSet;
                    try self.validateExprSource(extraction.base_expr);
                    try self.validatePatternSource(extraction.scrutinee_pattern);
                    try self.validatePatternSource(extraction.result_pattern);
                    if (!self.patternContainsPattern(extraction.scrutinee_pattern, extraction.result_pattern)) {
                        return error.InvalidSelectedHoistedRootSet;
                    }
                },
            }
        }
    }

    fn validateRoot(self: *RootSetValidator, root: SelectedHoistedRoot) (Allocator.Error || ValidationError)!void {
        const type_var = if (root.pattern) |pattern| ModuleEnv.varFrom(pattern) else ModuleEnv.varFrom(root.expr);
        if (self.exprIsFunctionDef(root.expr)) return error.InvalidSelectedHoistedRootSet;
        if (self.varIsFunctionType(type_var)) return error.InvalidSelectedHoistedRootSet;
        if (!try self.varIsConcreteHoistedConstType(type_var)) return error.InvalidSelectedHoistedRootSet;

        switch (root.body) {
            .expr => {
                if (root.pattern) |pattern| {
                    if (!self.patternCanOwnHoistedBindingRoot(pattern)) return error.InvalidSelectedHoistedRootSet;
                    if (!self.exprCanBeHoistedBindingRoot(root.expr)) return error.InvalidSelectedHoistedRootSet;
                } else if (!self.exprCanBeHoistedRoot(root.expr)) {
                    return error.InvalidSelectedHoistedRootSet;
                }
                try self.validateDependencies(root.expr);
            },
            .pattern_extraction => |extraction| {
                if (root.pattern == null) return error.InvalidSelectedHoistedRootSet;
                if (!self.exprCanBeHoistedBindingRoot(extraction.base_expr)) return error.InvalidSelectedHoistedRootSet;
                try self.validateDependencies(extraction.base_expr);
            },
        }
    }

    fn validateExprSource(self: *RootSetValidator, expr: CIR.Expr.Idx) ValidationError!void {
        const raw = @intFromEnum(expr);
        if (raw >= self.module_env.store.nodes.len()) return error.InvalidSelectedHoistedRootSet;
        const tag = self.module_env.store.nodes.get(@enumFromInt(raw)).tag;
        if (!nodeTagIsExpr(tag)) return error.InvalidSelectedHoistedRootSet;
    }

    fn validatePatternSource(self: *RootSetValidator, pattern: CIR.Pattern.Idx) ValidationError!void {
        const raw = @intFromEnum(pattern);
        if (raw >= self.module_env.store.nodes.len()) return error.InvalidSelectedHoistedRootSet;
        const tag = self.module_env.store.nodes.get(@enumFromInt(raw)).tag;
        if (!nodeTagIsPattern(tag)) return error.InvalidSelectedHoistedRootSet;
    }

    fn validateDependencies(self: *RootSetValidator, expr: CIR.Expr.Idx) (Allocator.Error || ValidationError)!void {
        var context = DependencyContext{};
        defer context.deinit(self.allocator);
        try self.validateExprDependencies(expr, &context);
    }

    fn validateExprDependencies(
        self: *RootSetValidator,
        expr: CIR.Expr.Idx,
        context: *DependencyContext,
    ) (Allocator.Error || ValidationError)!void {
        if (self.exprHasDedicatedLiteralConversionRoot(expr)) return error.InvalidSelectedHoistedRootSet;

        switch (self.module_env.store.getExpr(expr)) {
            .e_lookup_local => |lookup| {
                if (self.top_level_patterns.contains(lookup.pattern_idx)) return;
                if (context.contains(lookup.pattern_idx)) return;
                const dependency_index = self.pattern_roots.get(lookup.pattern_idx) orelse {
                    return error.InvalidSelectedHoistedRootSet;
                };
                if (dependency_index >= self.current_root_index) return error.InvalidSelectedHoistedRootSet;
            },
            .e_lookup_external,
            .e_str_segment,
            .e_bytes_literal,
            .e_num,
            .e_num_from_numeral,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            => {},
            .e_lookup_required,
            .e_runtime_error,
            .e_ellipsis,
            .e_anno_only,
            .e_crash,
            .e_closure,
            .e_lambda,
            .e_hosted_lambda,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_for,
            .e_return,
            .e_run_low_level,
            => return error.InvalidSelectedHoistedRootSet,
            .e_str => |str| try self.validateExprSpanDependencies(str.span, context),
            .e_list => |list| try self.validateExprSpanDependencies(list.elems, context),
            .e_tuple => |tuple| try self.validateExprSpanDependencies(tuple.elems, context),
            .e_block => |block| try self.validateBlockDependencies(block.stmts, block.final_expr, context),
            .e_match => |match| try self.validateMatchDependencies(match, context),
            .e_if => |if_expr| try self.validateIfDependencies(if_expr.branches, if_expr.final_else, context),
            .e_call => |call| {
                try self.validateExprDependencies(call.func, context);
                try self.validateExprSpanDependencies(call.args, context);
            },
            .e_method_call => |call| {
                try self.validateExprDependencies(call.receiver, context);
                try self.validateExprSpanDependencies(call.args, context);
            },
            .e_dispatch_call => |call| {
                if (self.varIsEffectfulFunction(call.constraint_fn_var)) return error.InvalidSelectedHoistedRootSet;
                try self.validateExprDependencies(call.receiver, context);
                try self.validateExprSpanDependencies(call.args, context);
            },
            .e_record => |record| try self.validateRecordDependencies(record.fields, record.ext, context),
            .e_tag => |tag| try self.validateExprSpanDependencies(tag.args, context),
            .e_nominal => |nominal| try self.validateExprDependencies(nominal.backing_expr, context),
            .e_nominal_external => |nominal| try self.validateExprDependencies(nominal.backing_expr, context),
            .e_binop => |binop| {
                try self.validateExprDependencies(binop.lhs, context);
                try self.validateExprDependencies(binop.rhs, context);
            },
            .e_unary_minus => |unary| try self.validateExprDependencies(unary.expr, context),
            .e_unary_not => |unary| try self.validateExprDependencies(unary.expr, context),
            .e_field_access => |field| try self.validateExprDependencies(field.receiver, context),
            .e_interpolation => |interpolation| {
                if (interpolation.constraint_fn_var) |fn_var| {
                    if (self.varIsEffectfulFunction(fn_var)) return error.InvalidSelectedHoistedRootSet;
                }
                try self.validateExprDependencies(interpolation.first, context);
                try self.validateExprSpanDependencies(interpolation.parts, context);
            },
            .e_structural_eq => |eq| {
                try self.validateExprDependencies(eq.lhs, context);
                try self.validateExprDependencies(eq.rhs, context);
            },
            .e_method_eq => |eq| {
                if (self.varIsEffectfulFunction(eq.constraint_fn_var)) return error.InvalidSelectedHoistedRootSet;
                try self.validateExprDependencies(eq.lhs, context);
                try self.validateExprDependencies(eq.rhs, context);
            },
            .e_type_method_call => |call| try self.validateExprSpanDependencies(call.args, context),
            .e_type_dispatch_call => |call| {
                if (self.varIsEffectfulFunction(call.constraint_fn_var)) return error.InvalidSelectedHoistedRootSet;
                try self.validateExprSpanDependencies(call.args, context);
            },
            .e_tuple_access => |access| try self.validateExprDependencies(access.tuple, context),
        }
    }

    fn validateExprSpanDependencies(
        self: *RootSetValidator,
        span: CIR.Expr.Span,
        context: *DependencyContext,
    ) (Allocator.Error || ValidationError)!void {
        for (self.module_env.store.sliceExpr(span)) |child| {
            try self.validateExprDependencies(child, context);
        }
    }

    fn validateBlockDependencies(
        self: *RootSetValidator,
        statements: CIR.Statement.Span,
        final_expr: CIR.Expr.Idx,
        context: *DependencyContext,
    ) (Allocator.Error || ValidationError)!void {
        const block_mark = context.mark();
        defer context.pop(block_mark);

        for (self.module_env.store.sliceStatements(statements)) |statement| {
            switch (self.module_env.store.getStatement(statement)) {
                .s_decl => |decl| {
                    try self.validateExprDependencies(decl.expr, context);
                    try self.appendPatternBinders(decl.pattern, context);
                },
                .s_expr => |expr_stmt| try self.validateExprDependencies(expr_stmt.expr, context),
                .s_import,
                .s_alias_decl,
                .s_nominal_decl,
                .s_type_anno,
                .s_type_var_alias,
                => {},
                .s_var,
                .s_var_uninitialized,
                .s_reassign,
                .s_crash,
                .s_dbg,
                .s_expect,
                .s_for,
                .s_while,
                .s_infinite_loop,
                .s_breakable_loop,
                .s_break,
                .s_return,
                .s_runtime_error,
                => return error.InvalidSelectedHoistedRootSet,
            }
        }
        try self.validateExprDependencies(final_expr, context);
    }

    fn validateMatchDependencies(
        self: *RootSetValidator,
        match: CIR.Expr.Match,
        context: *DependencyContext,
    ) (Allocator.Error || ValidationError)!void {
        try self.validateExprDependencies(match.cond, context);
        for (self.module_env.store.sliceMatchBranches(match.branches)) |branch_idx| {
            const branch = self.module_env.store.getMatchBranch(branch_idx);
            const mark = context.mark();
            defer context.pop(mark);
            for (self.module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
                const branch_pattern = self.module_env.store.getMatchBranchPattern(branch_pattern_idx);
                try self.appendPatternBinders(branch_pattern.pattern, context);
            }
            if (branch.guard) |guard| {
                try self.validateExprDependencies(guard, context);
            }
            try self.validateExprDependencies(branch.value, context);
        }
    }

    fn validateIfDependencies(
        self: *RootSetValidator,
        branches: CIR.Expr.IfBranch.Span,
        final_else: CIR.Expr.Idx,
        context: *DependencyContext,
    ) (Allocator.Error || ValidationError)!void {
        for (self.module_env.store.sliceIfBranches(branches)) |branch_idx| {
            const branch = self.module_env.store.getIfBranch(branch_idx);
            try self.validateExprDependencies(branch.cond, context);
            try self.validateExprDependencies(branch.body, context);
        }
        try self.validateExprDependencies(final_else, context);
    }

    fn validateRecordDependencies(
        self: *RootSetValidator,
        fields: CIR.RecordField.Span,
        ext: ?CIR.Expr.Idx,
        context: *DependencyContext,
    ) (Allocator.Error || ValidationError)!void {
        if (ext) |ext_expr| {
            try self.validateExprDependencies(ext_expr, context);
        }
        for (self.module_env.store.sliceRecordFields(fields)) |field_idx| {
            const field = self.module_env.store.getRecordField(field_idx);
            try self.validateExprDependencies(field.value, context);
        }
    }

    fn appendPatternBinders(
        self: *RootSetValidator,
        pattern: CIR.Pattern.Idx,
        context: *DependencyContext,
    ) Allocator.Error!void {
        switch (self.module_env.store.getPattern(pattern)) {
            .assign => try context.patterns.append(self.allocator, pattern),
            .as => |as_pattern| {
                try context.patterns.append(self.allocator, pattern);
                try self.appendPatternBinders(as_pattern.pattern, context);
            },
            .tuple => |tuple| {
                for (self.module_env.store.slicePatterns(tuple.patterns)) |elem_pattern| {
                    try self.appendPatternBinders(elem_pattern, context);
                }
            },
            .record_destructure => |destructure| {
                for (self.module_env.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                    const destruct = self.module_env.store.getRecordDestruct(destruct_idx);
                    try self.appendPatternBinders(destruct.kind.toPatternIdx(), context);
                }
            },
            .applied_tag => |tag| {
                for (self.module_env.store.slicePatterns(tag.args)) |arg_pattern| {
                    try self.appendPatternBinders(arg_pattern, context);
                }
            },
            .nominal => |nominal| try self.appendPatternBinders(nominal.backing_pattern, context),
            .nominal_external => |nominal| try self.appendPatternBinders(nominal.backing_pattern, context),
            .list => |list| {
                for (self.module_env.store.slicePatterns(list.patterns)) |elem_pattern| {
                    try self.appendPatternBinders(elem_pattern, context);
                }
                if (list.rest_info) |rest_info| {
                    if (rest_info.pattern) |rest_pattern| {
                        try self.appendPatternBinders(rest_pattern, context);
                    }
                }
            },
            .str_interpolation => |str| {
                var step_offset: u32 = 0;
                while (step_offset < str.steps.span.len) : (step_offset += 1) {
                    const step = self.module_env.store.getStrPatternStep(str.steps, step_offset);
                    if (step.capture) |capture| {
                        try self.appendPatternBinders(capture, context);
                    }
                }
            },
            .underscore,
            .runtime_error,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => {},
        }
    }

    fn patternContainsPattern(self: *RootSetValidator, haystack: CIR.Pattern.Idx, needle: CIR.Pattern.Idx) bool {
        if (haystack == needle) return true;
        return switch (self.module_env.store.getPattern(haystack)) {
            .assign,
            .underscore,
            .runtime_error,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => false,
            .as => |as_pattern| self.patternContainsPattern(as_pattern.pattern, needle),
            .tuple => |tuple| blk: {
                for (self.module_env.store.slicePatterns(tuple.patterns)) |elem_pattern| {
                    if (self.patternContainsPattern(elem_pattern, needle)) break :blk true;
                }
                break :blk false;
            },
            .record_destructure => |destructure| blk: {
                for (self.module_env.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                    const destruct = self.module_env.store.getRecordDestruct(destruct_idx);
                    if (self.patternContainsPattern(destruct.kind.toPatternIdx(), needle)) break :blk true;
                }
                break :blk false;
            },
            .applied_tag => |tag| blk: {
                for (self.module_env.store.slicePatterns(tag.args)) |arg_pattern| {
                    if (self.patternContainsPattern(arg_pattern, needle)) break :blk true;
                }
                break :blk false;
            },
            .nominal => |nominal| self.patternContainsPattern(nominal.backing_pattern, needle),
            .nominal_external => |nominal| self.patternContainsPattern(nominal.backing_pattern, needle),
            .list => |list| blk: {
                for (self.module_env.store.slicePatterns(list.patterns)) |elem_pattern| {
                    if (self.patternContainsPattern(elem_pattern, needle)) break :blk true;
                }
                if (list.rest_info) |rest_info| {
                    if (rest_info.pattern) |rest_pattern| {
                        if (self.patternContainsPattern(rest_pattern, needle)) break :blk true;
                    }
                }
                break :blk false;
            },
            .str_interpolation => |str| blk: {
                var step_offset: u32 = 0;
                while (step_offset < str.steps.span.len) : (step_offset += 1) {
                    const step = self.module_env.store.getStrPatternStep(str.steps, step_offset);
                    if (step.capture) |capture| {
                        if (self.patternContainsPattern(capture, needle)) break :blk true;
                    }
                }
                break :blk false;
            },
        };
    }

    fn patternCanOwnHoistedBindingRoot(self: *RootSetValidator, pattern: CIR.Pattern.Idx) bool {
        return switch (self.module_env.store.getPattern(pattern)) {
            .assign,
            .as,
            => true,
            .tuple,
            .record_destructure,
            .applied_tag,
            .nominal,
            .nominal_external,
            .list,
            .str_interpolation,
            .underscore,
            .runtime_error,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => false,
        };
    }

    fn exprCanBeHoistedRoot(self: *RootSetValidator, expr: CIR.Expr.Idx) bool {
        if (self.varIsFunctionType(ModuleEnv.varFrom(expr))) return false;
        return switch (self.module_env.store.getExpr(expr)) {
            .e_lookup_local => false,
            .e_lookup_external,
            .e_lookup_required,
            .e_str_segment,
            .e_bytes_literal,
            .e_num,
            .e_num_from_numeral,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_ellipsis,
            .e_anno_only,
            .e_crash,
            .e_closure,
            .e_lambda,
            .e_hosted_lambda,
            => false,
            .e_str => |str| self.stringHasInterpolation(str.span),
            .e_list,
            .e_tuple,
            .e_block,
            .e_match,
            .e_if,
            .e_call,
            .e_method_call,
            .e_dispatch_call,
            .e_record,
            .e_tag,
            .e_nominal,
            .e_nominal_external,
            .e_binop,
            .e_unary_minus,
            .e_unary_not,
            .e_field_access,
            .e_interpolation,
            .e_structural_eq,
            .e_method_eq,
            .e_type_method_call,
            .e_type_dispatch_call,
            .e_tuple_access,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_for,
            .e_return,
            .e_run_low_level,
            => true,
        };
    }

    fn exprCanBeHoistedBindingRoot(self: *RootSetValidator, expr: CIR.Expr.Idx) bool {
        if (self.varIsFunctionType(ModuleEnv.varFrom(expr))) return false;
        return switch (self.module_env.store.getExpr(expr)) {
            .e_lookup_local => true,
            .e_call,
            .e_method_call,
            .e_type_method_call,
            .e_type_dispatch_call,
            .e_dispatch_call,
            => true,
            .e_for,
            .e_run_low_level,
            .e_lookup_required,
            .e_runtime_error,
            .e_ellipsis,
            .e_anno_only,
            .e_crash,
            .e_closure,
            .e_lambda,
            .e_hosted_lambda,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_return,
            => false,
            .e_lookup_external,
            .e_str_segment,
            .e_bytes_literal,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_list,
            .e_tuple,
            .e_block,
            .e_match,
            .e_if,
            .e_record,
            .e_tag,
            .e_nominal,
            .e_nominal_external,
            .e_binop,
            .e_unary_minus,
            .e_unary_not,
            .e_field_access,
            .e_interpolation,
            .e_structural_eq,
            .e_method_eq,
            .e_tuple_access,
            => true,
            .e_num,
            .e_num_from_numeral,
            .e_typed_num_from_numeral,
            .e_str,
            => !self.exprHasDedicatedLiteralConversionRoot(expr),
        };
    }

    fn stringHasInterpolation(self: *RootSetValidator, span: CIR.Expr.Span) bool {
        for (self.module_env.store.sliceExpr(span)) |segment| {
            if (self.module_env.store.getExpr(segment) != .e_str_segment) return true;
        }
        return false;
    }

    fn exprIsFunctionDef(self: *RootSetValidator, expr: CIR.Expr.Idx) bool {
        return switch (self.module_env.store.getExpr(expr)) {
            .e_lambda,
            .e_closure,
            .e_hosted_lambda,
            => true,
            .e_lookup_local,
            .e_lookup_external,
            .e_lookup_required,
            .e_str_segment,
            .e_bytes_literal,
            .e_num,
            .e_num_from_numeral,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_ellipsis,
            .e_anno_only,
            .e_crash,
            .e_str,
            .e_list,
            .e_tuple,
            .e_block,
            .e_match,
            .e_if,
            .e_call,
            .e_method_call,
            .e_dispatch_call,
            .e_record,
            .e_tag,
            .e_nominal,
            .e_nominal_external,
            .e_binop,
            .e_unary_minus,
            .e_unary_not,
            .e_field_access,
            .e_interpolation,
            .e_structural_eq,
            .e_method_eq,
            .e_type_method_call,
            .e_type_dispatch_call,
            .e_tuple_access,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_for,
            .e_return,
            .e_run_low_level,
            => false,
        };
    }

    fn exprHasDedicatedLiteralConversionRoot(self: *RootSetValidator, expr: CIR.Expr.Idx) bool {
        const node = ModuleEnv.nodeIdxFrom(expr);
        if (self.module_env.numeralDispatchPlanForNode(node) != null) {
            if (self.module_env.numericSuffixTypeForNode(node)) |suffix_type| {
                return switch (suffix_type.target()) {
                    .builtin => false,
                    .local,
                    .external,
                    => true,
                };
            }
            return !self.varIsBuiltinLiteralTarget(ModuleEnv.varFrom(expr));
        }
        if (self.module_env.quoteDispatchPlanForNode(node) != null and
            !self.varIsBuiltinLiteralTarget(ModuleEnv.varFrom(expr)))
        {
            return true;
        }
        return false;
    }

    fn varIsFunctionType(self: *RootSetValidator, var_: Var) bool {
        var current = var_;
        while (true) {
            const resolved = self.typeStore().resolveVar(current);
            switch (resolved.desc.content) {
                .alias => |alias| {
                    current = self.typeStore().getAliasBackingVar(alias);
                    continue;
                },
                .structure => |flat| return switch (flat) {
                    .fn_pure, .fn_effectful, .fn_unbound => true,
                    .record,
                    .record_unbound,
                    .tuple,
                    .nominal_type,
                    .empty_record,
                    .tag_union,
                    .empty_tag_union,
                    => false,
                },
                .err, .flex, .rigid => return false,
            }
        }
    }

    fn varIsEffectfulFunction(self: *RootSetValidator, var_: Var) bool {
        var current = var_;
        while (true) {
            const resolved = self.typeStore().resolveVar(current);
            switch (resolved.desc.content) {
                .alias => |alias| {
                    current = self.typeStore().getAliasBackingVar(alias);
                    continue;
                },
                .structure => |flat| return switch (flat) {
                    .fn_effectful => true,
                    .fn_pure,
                    .fn_unbound,
                    .record,
                    .record_unbound,
                    .tuple,
                    .nominal_type,
                    .empty_record,
                    .tag_union,
                    .empty_tag_union,
                    => false,
                },
                .err, .flex, .rigid => return false,
            }
        }
    }

    fn varIsConcreteHoistedConstType(self: *RootSetValidator, var_: Var) Allocator.Error!bool {
        self.type_visited.clearRetainingCapacity();
        return try self.varIsConcreteHoistedConstTypeInternal(var_);
    }

    fn varIsConcreteHoistedConstTypeInternal(self: *RootSetValidator, var_: Var) Allocator.Error!bool {
        const resolved = self.typeStore().resolveVar(var_);
        if (self.type_visited.contains(resolved.var_)) return true;
        try self.type_visited.put(resolved.var_, {});

        return switch (resolved.desc.content) {
            .err,
            .flex,
            .rigid,
            => false,
            .alias => |alias| try self.varsAreConcreteHoistedConstTypes(self.typeStore().sliceAliasArgs(alias)) and
                try self.varIsConcreteHoistedConstTypeInternal(self.typeStore().getAliasBackingVar(alias)),
            .structure => |flat| try self.flatTypeIsConcreteHoistedConst(flat),
        };
    }

    fn varsAreConcreteHoistedConstTypes(self: *RootSetValidator, vars: []const Var) Allocator.Error!bool {
        for (vars) |var_| {
            if (!try self.varIsConcreteHoistedConstTypeInternal(var_)) return false;
        }
        return true;
    }

    fn flatTypeIsConcreteHoistedConst(self: *RootSetValidator, flat: FlatType) Allocator.Error!bool {
        return switch (flat) {
            .empty_record,
            .empty_tag_union,
            => true,
            .fn_pure,
            .fn_effectful,
            .fn_unbound,
            => false,
            .record => |record| blk: {
                const fields = self.typeStore().getRecordFieldsSlice(record.fields);
                if (!try self.varsAreConcreteHoistedConstTypes(fields.items(.var_))) break :blk false;
                break :blk try self.varIsConcreteHoistedConstTypeInternal(record.ext);
            },
            .record_unbound => |fields| blk: {
                const fields_slice = self.typeStore().getRecordFieldsSlice(fields);
                break :blk try self.varsAreConcreteHoistedConstTypes(fields_slice.items(.var_));
            },
            .tuple => |tuple| try self.varsAreConcreteHoistedConstTypes(self.typeStore().sliceVars(tuple.elems)),
            .tag_union => |tag_union| blk: {
                const tags = self.typeStore().getTagsSlice(tag_union.tags);
                for (tags.items(.args)) |tag_args| {
                    if (!try self.varsAreConcreteHoistedConstTypes(self.typeStore().sliceVars(tag_args))) break :blk false;
                }
                break :blk try self.varIsConcreteHoistedConstTypeInternal(tag_union.ext);
            },
            .nominal_type => |nominal| blk: {
                if (!try self.varsAreConcreteHoistedConstTypes(self.typeStore().sliceNominalArgs(nominal))) break :blk false;
                if (self.nominalIsBuiltinLiteralStorable(nominal)) break :blk true;
                if (nominal.isOpaque()) break :blk true;
                break :blk try self.varIsConcreteHoistedConstTypeInternal(self.typeStore().getNominalBackingVar(nominal));
            },
        };
    }

    fn varIsBuiltinLiteralTarget(self: *RootSetValidator, var_: Var) bool {
        var current = var_;
        while (true) {
            const resolved = self.typeStore().resolveVar(current);
            switch (resolved.desc.content) {
                .alias => |alias| {
                    current = self.typeStore().getAliasBackingVar(alias);
                },
                .structure => |flat| return switch (flat) {
                    .nominal_type => |nominal| self.nominalIsBuiltinNumberType(nominal) or self.nominalIsBuiltinStrType(nominal),
                    .record,
                    .record_unbound,
                    .tuple,
                    .fn_pure,
                    .fn_effectful,
                    .fn_unbound,
                    .empty_record,
                    .tag_union,
                    .empty_tag_union,
                    => false,
                },
                .err,
                .flex,
                .rigid,
                => return false,
            }
        }
    }

    fn nominalIsBuiltinLiteralStorable(self: *RootSetValidator, nominal: types.NominalType) bool {
        return self.nominalIsBuiltinNumberType(nominal) or
            self.nominalIsBuiltinStrType(nominal) or
            self.nominalIdentIs(nominal, self.module_env.idents.builtin_list) or
            self.nominalIdentIs(nominal, self.module_env.idents.builtin_box);
    }

    fn nominalIsBuiltinNumberType(self: *RootSetValidator, nominal: types.NominalType) bool {
        return self.nominalIdentIs(nominal, self.module_env.idents.u8_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.i8_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.u16_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.i16_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.u32_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.i32_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.u64_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.i64_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.u128_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.i128_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.f32_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.f64_type) or
            self.nominalIdentIs(nominal, self.module_env.idents.dec_type);
    }

    fn nominalIsBuiltinStrType(self: *RootSetValidator, nominal: types.NominalType) bool {
        return self.nominalIdentIs(nominal, self.module_env.idents.builtin_str);
    }

    fn nominalIdentIs(_: *RootSetValidator, nominal: types.NominalType, ident: Ident.Idx) bool {
        return nominal.originIsBuiltin() and nominal.ident.ident_idx.eql(ident);
    }

    fn typeStore(self: *RootSetValidator) *const TypeStore {
        return &self.module_env.types;
    }
};

fn nodeTagIsExpr(tag: CIR.Node.Tag) bool {
    return switch (tag) {
        .expr_var,
        .expr_tuple,
        .expr_tuple_access,
        .expr_list,
        .expr_empty_list,
        .expr_call,
        .expr_record,
        .expr_empty_record,
        .expr_field_access,
        .expr_method_call,
        .expr_dispatch_call,
        .expr_interpolation,
        .expr_structural_eq,
        .expr_method_eq,
        .expr_type_method_call,
        .expr_type_dispatch_call,
        .expr_static_dispatch,
        .expr_external_lookup,
        .expr_required_lookup,
        .expr_apply,
        .expr_string,
        .expr_string_segment,
        .expr_bytes_literal,
        .expr_num,
        .expr_frac_f32,
        .expr_frac_f64,
        .expr_dec,
        .expr_dec_small,
        .expr_num_from_numeral,
        .expr_typed_int,
        .expr_typed_frac,
        .expr_typed_num_from_numeral,
        .expr_tag,
        .expr_nominal,
        .expr_nominal_external,
        .expr_zero_argument_tag,
        .expr_closure,
        .expr_lambda,
        .expr_record_update,
        .expr_bin_op,
        .expr_unary_minus,
        .expr_unary_not,
        .expr_suffix_single_question,
        .expr_if_then_else,
        .expr_match,
        .expr_dbg,
        .expr_crash,
        .expr_expect_err,
        .expr_block,
        .expr_ellipsis,
        .expr_anno_only,
        .expr_hosted_lambda,
        .expr_low_level,
        .expr_run_low_level,
        .expr_expect,
        .expr_for,
        .expr_record_builder,
        .expr_return,
        => true,
        .statement_decl,
        .statement_var,
        .statement_var_uninitialized,
        .statement_reassign,
        .statement_crash,
        .statement_dbg,
        .statement_expr,
        .statement_expect,
        .statement_for,
        .statement_while,
        .statement_infinite_loop,
        .statement_breakable_loop,
        .statement_break,
        .statement_return,
        .statement_import,
        .statement_alias_decl,
        .statement_nominal_decl,
        .statement_type_anno,
        .statement_type_var_alias,
        .record_field,
        .record_destruct,
        .match_branch,
        .match_branch_pattern,
        .type_header,
        .annotation,
        .ty_apply,
        .ty_apply_external,
        .ty_rigid_var,
        .ty_rigid_var_lookup,
        .ty_lookup,
        .ty_underscore,
        .ty_tag_union,
        .ty_tag,
        .ty_tuple,
        .ty_record,
        .ty_record_field,
        .ty_fn,
        .ty_parens,
        .ty_lookup_external,
        .ty_malformed,
        .where_method,
        .where_alias,
        .where_malformed,
        .pattern_identifier,
        .pattern_as,
        .pattern_applied_tag,
        .pattern_nominal,
        .pattern_nominal_external,
        .pattern_record_destructure,
        .pattern_list,
        .pattern_tuple,
        .pattern_num_literal,
        .pattern_dec_literal,
        .pattern_f32_literal,
        .pattern_f64_literal,
        .pattern_small_dec_literal,
        .pattern_str_literal,
        .pattern_str_interpolation,
        .pattern_underscore,
        .lambda_capture,
        .def,
        .exposed_item,
        .if_branch,
        .type_var_slot,
        .malformed,
        .diag_not_implemented,
        .diag_invalid_num_literal,
        .diag_empty_single_quote,
        .diag_empty_tuple,
        .diag_ident_already_in_scope,
        .diag_ident_not_in_scope,
        .diag_read_uninitialized_var,
        .diag_self_referential_definition,
        .diag_circular_value_definition,
        .diag_local_reference_before_definition,
        .diag_mutually_recursive_local_definitions,
        .diag_erroneous_value_use,
        .diag_erroneous_value_expr,
        .diag_qualified_ident_does_not_exist,
        .diag_invalid_top_level_statement,
        .diag_expr_not_canonicalized,
        .diag_invalid_string_interpolation,
        .diag_unreachable_string_pattern_capture,
        .diag_pattern_arg_invalid,
        .diag_pattern_not_canonicalized,
        .diag_can_lambda_not_implemented,
        .diag_lambda_body_not_canonicalized,
        .diag_if_condition_not_canonicalized,
        .diag_if_then_not_canonicalized,
        .diag_if_else_not_canonicalized,
        .diag_malformed_type_annotation,
        .diag_malformed_where_clause,
        .diag_where_clause_not_allowed_in_type_decl,
        .diag_open_ext_not_allowed_in_type_decl,
        .diag_type_module_missing_matching_type,
        .diag_type_module_has_alias_not_nominal,
        .diag_default_app_missing_main,
        .diag_default_app_wrong_arity,
        .diag_cannot_import_default_app,
        .diag_execution_requires_app_or_default_app,
        .diag_type_name_case_mismatch,
        .diag_module_header_deprecated,
        .diag_redundant_expose_main_type,
        .diag_invalid_main_type_rename_in_exposing,
        .diag_var_across_function_boundary,
        .diag_shadowing_warning,
        .diag_type_redeclared,
        .diag_undeclared_type,
        .diag_undeclared_type_var,
        .diag_type_alias_but_needed_nominal,
        .diag_type_alias_redeclared,
        .diag_tuple_elem_not_canonicalized,
        .diag_file_import_not_found,
        .diag_file_import_io_error,
        .diag_file_import_not_utf8,
        .diag_module_not_found,
        .diag_value_not_exposed,
        .diag_type_not_exposed,
        .diag_private_type_in_exposed_type,
        .diag_private_type_in_exposed_field,
        .diag_type_from_missing_module,
        .diag_module_not_imported,
        .diag_nested_type_not_found,
        .diag_nested_value_not_found,
        .diag_record_builder_map2_not_found,
        .diag_too_many_exports,
        .diag_nominal_type_redeclared,
        .diag_type_shadowed_warning,
        .diag_type_parameter_conflict,
        .diag_unused_variable,
        .diag_used_underscore_variable,
        .diag_duplicate_record_field,
        .diag_crash_expects_string,
        .diag_f64_pattern_literal,
        .diag_unused_type_var_name,
        .diag_type_var_marked_unused,
        .diag_type_var_starting_with_dollar,
        .diag_underscore_in_type_declaration,
        .diagnostic_exposed_but_not_implemented,
        .diag_redundant_exposed,
        .diag_if_expr_without_else,
        .diag_break_outside_loop,
        .diag_infinite_loop_never_exits,
        .diag_return_outside_fn,
        .diag_mutually_recursive_type_aliases,
        .diag_deprecated_number_suffix,
        .diag_range_op_chained,
        => false,
    };
}

fn nodeTagIsPattern(tag: CIR.Node.Tag) bool {
    return switch (tag) {
        .pattern_identifier,
        .pattern_as,
        .pattern_applied_tag,
        .pattern_nominal,
        .pattern_nominal_external,
        .pattern_record_destructure,
        .pattern_list,
        .pattern_tuple,
        .pattern_num_literal,
        .pattern_dec_literal,
        .pattern_f32_literal,
        .pattern_f64_literal,
        .pattern_small_dec_literal,
        .pattern_str_literal,
        .pattern_str_interpolation,
        .pattern_underscore,
        => true,
        .statement_decl,
        .statement_var,
        .statement_var_uninitialized,
        .statement_reassign,
        .statement_crash,
        .statement_dbg,
        .statement_expr,
        .statement_expect,
        .statement_for,
        .statement_while,
        .statement_infinite_loop,
        .statement_breakable_loop,
        .statement_break,
        .statement_return,
        .statement_import,
        .statement_alias_decl,
        .statement_nominal_decl,
        .statement_type_anno,
        .statement_type_var_alias,
        .expr_var,
        .expr_tuple,
        .expr_tuple_access,
        .expr_list,
        .expr_empty_list,
        .expr_call,
        .expr_record,
        .expr_empty_record,
        .record_field,
        .record_destruct,
        .expr_field_access,
        .expr_method_call,
        .expr_dispatch_call,
        .expr_interpolation,
        .expr_structural_eq,
        .expr_method_eq,
        .expr_type_method_call,
        .expr_type_dispatch_call,
        .expr_static_dispatch,
        .expr_external_lookup,
        .expr_required_lookup,
        .expr_apply,
        .expr_string,
        .expr_string_segment,
        .expr_bytes_literal,
        .expr_num,
        .expr_frac_f32,
        .expr_frac_f64,
        .expr_dec,
        .expr_dec_small,
        .expr_num_from_numeral,
        .expr_typed_int,
        .expr_typed_frac,
        .expr_typed_num_from_numeral,
        .expr_tag,
        .expr_nominal,
        .expr_nominal_external,
        .expr_zero_argument_tag,
        .expr_closure,
        .expr_lambda,
        .expr_record_update,
        .expr_bin_op,
        .expr_unary_minus,
        .expr_unary_not,
        .expr_suffix_single_question,
        .expr_if_then_else,
        .expr_match,
        .expr_dbg,
        .expr_crash,
        .expr_expect_err,
        .expr_block,
        .expr_ellipsis,
        .expr_anno_only,
        .expr_hosted_lambda,
        .expr_low_level,
        .expr_run_low_level,
        .expr_expect,
        .expr_for,
        .expr_record_builder,
        .expr_return,
        .match_branch,
        .match_branch_pattern,
        .type_header,
        .annotation,
        .ty_apply,
        .ty_apply_external,
        .ty_rigid_var,
        .ty_rigid_var_lookup,
        .ty_lookup,
        .ty_underscore,
        .ty_tag_union,
        .ty_tag,
        .ty_tuple,
        .ty_record,
        .ty_record_field,
        .ty_fn,
        .ty_parens,
        .ty_lookup_external,
        .ty_malformed,
        .where_method,
        .where_alias,
        .where_malformed,
        .lambda_capture,
        .def,
        .exposed_item,
        .if_branch,
        .type_var_slot,
        .malformed,
        .diag_not_implemented,
        .diag_invalid_num_literal,
        .diag_empty_single_quote,
        .diag_empty_tuple,
        .diag_ident_already_in_scope,
        .diag_ident_not_in_scope,
        .diag_read_uninitialized_var,
        .diag_self_referential_definition,
        .diag_circular_value_definition,
        .diag_local_reference_before_definition,
        .diag_mutually_recursive_local_definitions,
        .diag_erroneous_value_use,
        .diag_erroneous_value_expr,
        .diag_qualified_ident_does_not_exist,
        .diag_invalid_top_level_statement,
        .diag_expr_not_canonicalized,
        .diag_invalid_string_interpolation,
        .diag_unreachable_string_pattern_capture,
        .diag_pattern_arg_invalid,
        .diag_pattern_not_canonicalized,
        .diag_can_lambda_not_implemented,
        .diag_lambda_body_not_canonicalized,
        .diag_if_condition_not_canonicalized,
        .diag_if_then_not_canonicalized,
        .diag_if_else_not_canonicalized,
        .diag_malformed_type_annotation,
        .diag_malformed_where_clause,
        .diag_where_clause_not_allowed_in_type_decl,
        .diag_open_ext_not_allowed_in_type_decl,
        .diag_type_module_missing_matching_type,
        .diag_type_module_has_alias_not_nominal,
        .diag_default_app_missing_main,
        .diag_default_app_wrong_arity,
        .diag_cannot_import_default_app,
        .diag_execution_requires_app_or_default_app,
        .diag_type_name_case_mismatch,
        .diag_module_header_deprecated,
        .diag_redundant_expose_main_type,
        .diag_invalid_main_type_rename_in_exposing,
        .diag_var_across_function_boundary,
        .diag_shadowing_warning,
        .diag_type_redeclared,
        .diag_undeclared_type,
        .diag_undeclared_type_var,
        .diag_type_alias_but_needed_nominal,
        .diag_type_alias_redeclared,
        .diag_tuple_elem_not_canonicalized,
        .diag_file_import_not_found,
        .diag_file_import_io_error,
        .diag_file_import_not_utf8,
        .diag_module_not_found,
        .diag_value_not_exposed,
        .diag_type_not_exposed,
        .diag_private_type_in_exposed_type,
        .diag_private_type_in_exposed_field,
        .diag_type_from_missing_module,
        .diag_module_not_imported,
        .diag_nested_type_not_found,
        .diag_nested_value_not_found,
        .diag_record_builder_map2_not_found,
        .diag_too_many_exports,
        .diag_nominal_type_redeclared,
        .diag_type_shadowed_warning,
        .diag_type_parameter_conflict,
        .diag_unused_variable,
        .diag_used_underscore_variable,
        .diag_duplicate_record_field,
        .diag_crash_expects_string,
        .diag_f64_pattern_literal,
        .diag_unused_type_var_name,
        .diag_type_var_marked_unused,
        .diag_type_var_starting_with_dollar,
        .diag_underscore_in_type_declaration,
        .diagnostic_exposed_but_not_implemented,
        .diag_redundant_exposed,
        .diag_if_expr_without_else,
        .diag_break_outside_loop,
        .diag_infinite_loop_never_exits,
        .diag_return_outside_fn,
        .diag_mutually_recursive_type_aliases,
        .diag_deprecated_number_suffix,
        .diag_range_op_chained,
        => false,
    };
}

pub fn cloneBody(allocator: Allocator, body: Body) Allocator.Error!Body {
    _ = allocator;
    return switch (body) {
        .expr => .expr,
        .pattern_extraction => |extraction| .{ .pattern_extraction = extraction },
    };
}

pub fn deinitBody(allocator: Allocator, body: Body) void {
    _ = allocator;
    switch (body) {
        .expr => {},
        .pattern_extraction => {},
    }
}

pub fn cloneSelectedRoot(allocator: Allocator, root: SelectedHoistedRoot) Allocator.Error!SelectedHoistedRoot {
    return .{
        .expr = root.expr,
        .pattern = root.pattern,
        .body = try cloneBody(allocator, root.body),
    };
}

pub fn deinitSelectedRoot(allocator: Allocator, root: *SelectedHoistedRoot) void {
    deinitBody(allocator, root.body);
    root.body = .expr;
}

pub fn deinitSelectedRootBodies(allocator: Allocator, roots: []const SelectedHoistedRoot) void {
    for (roots) |*root| {
        deinitBody(allocator, root.body);
    }
}

pub fn freeSelectedRootSlice(allocator: Allocator, roots: []const SelectedHoistedRoot) void {
    if (roots.len == 0) return;
    deinitSelectedRootBodies(allocator, roots);
    allocator.free(roots);
}
