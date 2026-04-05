//! Monotype lowering from checked CIR.
//!
//! This follows cor's strategy:
//! - lower solved checker facts into a monomorphic type graph
//! - register top-level function sources once
//! - lower top-level values/runs immediately
//! - specialize top-level functions on demand from variable references
//! - carry lexical binding facts explicitly in recursive lowering
//!
//! Roc-only constructs that cor does not have yet remain explicit extensions:
//! loops, mutable statements, blocks, returns, and low-level ops.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const instantiate = @import("types").instantiate;
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const ctx_mod = @import("ctx.zig");
const specializations_mod = @import("specializations.zig");
const symbol_mod = @import("symbol");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const Instantiator = instantiate.Instantiator;

pub const Program = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .store = ast.Store.init(allocator),
            .root_defs = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
    }
};

pub const Result = struct {
    program: Program,
    symbols: symbol_mod.Store,
    types: type_mod.Store,

    pub fn deinit(self: *Result) void {
        self.program.deinit();
        self.symbols.deinit();
        self.types.deinit();
    }
};

pub const Lowerer = struct {
    allocator: std.mem.Allocator,
    ctx: ctx_mod.Ctx,
    program: Program,
    specializations: specializations_mod.Queue,
    type_cache: std.AutoHashMap(TypeKey, type_mod.TypeId),
    top_level_defs_by_symbol: std.AutoHashMap(symbol_mod.Symbol, TopLevelDef),
    top_level_symbols_by_pattern: std.AutoHashMap(PatternKey, symbol_mod.Symbol),

    const TypeKey = struct {
        module_idx: u32,
        var_: Var,
    };

    const PatternKey = struct {
        module_idx: u32,
        pattern_idx: u32,
    };

    const TopLevelDef = struct {
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        pattern_idx: CIR.Pattern.Idx,
        is_function: bool,
    };

    const BindingEnv = std.AutoHashMap(PatternKey, ast.TypedSymbol);

    const TypeCloneScope = struct {
        var_map: std.AutoHashMap(Var, Var),
        instantiator: Instantiator,

        fn init(allocator: std.mem.Allocator, env: *ModuleEnv) TypeCloneScope {
            var var_map = std.AutoHashMap(Var, Var).init(allocator);
            return .{
                .instantiator = .{
                    .store = &env.types,
                    .idents = env.getIdentStoreConst(),
                    .var_map = &var_map,
                    .current_rank = .outermost,
                    .rigid_behavior = .fresh_flex,
                },
                .var_map = var_map,
            };
        }

        fn deinit(self: *TypeCloneScope) void {
            self.var_map.deinit();
        }
    };

    pub fn init(
        allocator: std.mem.Allocator,
        all_module_envs: []const *const ModuleEnv,
        builtin_module_idx: u32,
    ) Lowerer {
        return .{
            .allocator = allocator,
            .ctx = ctx_mod.Ctx.init(allocator, all_module_envs, builtin_module_idx),
            .program = Program.init(allocator),
            .specializations = specializations_mod.Queue.init(allocator),
            .type_cache = std.AutoHashMap(TypeKey, type_mod.TypeId).init(allocator),
            .top_level_defs_by_symbol = std.AutoHashMap(symbol_mod.Symbol, TopLevelDef).init(allocator),
            .top_level_symbols_by_pattern = std.AutoHashMap(PatternKey, symbol_mod.Symbol).init(allocator),
        };
    }

    pub fn deinit(self: *Lowerer) void {
        self.top_level_symbols_by_pattern.deinit();
        self.top_level_defs_by_symbol.deinit();
        self.type_cache.deinit();
        self.specializations.deinit();
        self.program.deinit();
        self.ctx.deinit();
    }

    pub fn run(self: *Lowerer, root_module_idx: u32) std.mem.Allocator.Error!Result {
        try self.registerAllTopLevelDefs();
        try self.lowerRootModule(root_module_idx);
        try self.drainSpecializations();

        const result = Result{
            .program = self.program,
            .symbols = self.ctx.symbols,
            .types = self.ctx.types,
        };
        self.program = Program.init(self.allocator);
        self.ctx.symbols = symbol_mod.Store.init(self.allocator);
        self.ctx.types = type_mod.Store.init(self.allocator);
        return result;
    }

    fn registerAllTopLevelDefs(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.ctx.all_module_envs, 0..) |env, module_idx| {
            for (env.store.sliceDefs(env.all_defs)) |def_idx| {
                const def = env.store.getDef(def_idx);
                if (def.kind != .let) continue;

                const pattern = env.store.getPattern(def.pattern);
                const ident = switch (pattern) {
                    .assign => |assign| assign.ident,
                    else => continue,
                };

                const symbol = try self.ctx.getOrCreateTopLevelSymbol(@intCast(module_idx), def_idx, ident);
                try self.top_level_defs_by_symbol.put(symbol, .{
                    .module_idx = @intCast(module_idx),
                    .def_idx = def_idx,
                    .pattern_idx = def.pattern,
                    .is_function = isLambdaExpr(env.store.getExpr(def.expr)),
                });
                try self.top_level_symbols_by_pattern.put(.{
                    .module_idx = @intCast(module_idx),
                    .pattern_idx = @intFromEnum(def.pattern),
                }, symbol);
            }
        }
    }

    fn lowerRootModule(self: *Lowerer, module_idx: u32) std.mem.Allocator.Error!void {
        const env = self.ctx.env(module_idx);
        for (env.store.sliceDefs(env.all_defs)) |def_idx| {
            const lowered = try self.lowerTopLevelDef(module_idx, def_idx);
            if (lowered) |def_id| {
                try self.program.root_defs.append(self.allocator, def_id);
            }
        }
    }

    fn lowerTopLevelDef(
        self: *Lowerer,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
    ) std.mem.Allocator.Error!?ast.DefId {
        const env = self.ctx.env(module_idx);
        const def = env.store.getDef(def_idx);

        return switch (def.kind) {
            .let => blk: {
                const bind_symbol = self.lookupTopLevelSymbol(module_idx, def.pattern) orelse break :blk null;
                if (self.isTopLevelFunction(bind_symbol)) break :blk null;

                var type_scope = TypeCloneScope.init(self.allocator, @constCast(env));
                defer type_scope.deinit();
                var binding_env = BindingEnv.init(self.allocator);
                defer binding_env.deinit();

                const bind_ty = try self.lowerSolvedType(module_idx, &type_scope, ModuleEnv.varFrom(def.pattern));
                const body = try self.lowerExpr(module_idx, &type_scope, binding_env, def.expr);
                break :blk try self.program.store.addDef(.{
                    .bind = .{ .ty = bind_ty, .symbol = bind_symbol },
                    .value = .{ .val = body },
                });
            },
            .stmt => |fx_var| try self.lowerTopLevelRunDef(module_idx, def_idx, fx_var),
            .ignored => |fx_var| try self.lowerTopLevelRunDef(module_idx, def_idx, fx_var),
        };
    }

    fn lowerTopLevelRunDef(
        self: *Lowerer,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        fx_var: Var,
    ) std.mem.Allocator.Error!ast.DefId {
        const env = self.ctx.env(module_idx);
        const def = env.store.getDef(def_idx);
        const bind_symbol = self.lookupTopLevelSymbol(module_idx, def.pattern) orelse try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE);

        var type_scope = TypeCloneScope.init(self.allocator, @constCast(env));
        defer type_scope.deinit();
        var binding_env = BindingEnv.init(self.allocator);
        defer binding_env.deinit();

        const bind_ty = try self.lowerSolvedType(module_idx, &type_scope, ModuleEnv.varFrom(def.pattern));
        const body = try self.lowerExpr(module_idx, &type_scope, binding_env, def.expr);
        return try self.program.store.addDef(.{
            .bind = .{ .ty = bind_ty, .symbol = bind_symbol },
            .value = .{ .run = .{
                .bind = .{ .ty = bind_ty, .symbol = bind_symbol },
                .body = body,
                .entry_ty = fx_var,
            } },
        });
    }

    fn drainSpecializations(self: *Lowerer) std.mem.Allocator.Error!void {
        while (self.specializations.nextNeededSpecialization()) |pending_idx| {
            const pending = self.specializations.get(pending_idx);
            const def_id = try self.lowerSpecializedTopLevelFn(pending.*);
            _ = def_id;
            pending.emitted = true;
        }
    }

    fn lowerSpecializedTopLevelFn(
        self: *Lowerer,
        pending: specializations_mod.Pending,
    ) std.mem.Allocator.Error!ast.DefId {
        const env = self.ctx.env(pending.source.module_idx);
        const def = env.store.getDef(pending.source.def_idx);
        const bind: ast.TypedSymbol = .{
            .ty = pending.ty,
            .symbol = pending.specialized_symbol,
        };

        var type_scope = TypeCloneScope.init(self.allocator, @constCast(env));
        defer type_scope.deinit();

        const letfn = try self.lowerTopLevelLambdaLikeDef(
            pending.source.module_idx,
            &type_scope,
            bind,
            def.expr,
            isRecursiveTopLevelDef(env, pending.source.def_idx),
        );
        return try self.program.store.addDef(.{
            .bind = bind,
            .value = .{ .fn_ = letfn },
        });
    }

    fn lowerTopLevelLambdaLikeDef(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        bind: ast.TypedSymbol,
        expr_idx: CIR.Expr.Idx,
        recursive: bool,
    ) std.mem.Allocator.Error!ast.LetFn {
        const env = self.ctx.env(module_idx);
        const expr = env.store.getExpr(expr_idx);

        const lambda = switch (expr) {
            .e_lambda => |lambda| lambda,
            .e_closure => |closure| blk: {
                const lambda_expr = env.store.getExpr(closure.lambda_idx);
                if (lambda_expr != .e_lambda) unreachable;
                break :blk lambda_expr.e_lambda;
            },
            .e_hosted_lambda => |hosted| {
                if (hosted.args.span.len == 0) {
                    const unit_arg = try self.makeUnitArg(module_idx, type_scope, expr_idx);
                    return .{
                        .recursive = recursive,
                        .bind = bind,
                        .arg = unit_arg,
                        .body = try self.program.store.addExpr(.{
                            .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(hosted.body)),
                            .data = .{ .runtime_error = try @constCast(env).insertString("TODO monotype hosted lambda") },
                        }),
                    };
                }

                const arg_patterns = env.store.slicePatterns(hosted.args);
                const first_arg = try self.bindLambdaArg(module_idx, type_scope, arg_patterns[0]);
                var body_env = BindingEnv.init(self.allocator);
                defer body_env.deinit();
                try self.bindPatternEnv(module_idx, type_scope, arg_patterns[0], &body_env);

                const body = if (arg_patterns.len == 1)
                    try self.lowerExpr(module_idx, type_scope, body_env, hosted.body)
                else
                    try self.lowerCurriedClosureChain(module_idx, type_scope, body_env, arg_patterns[1..], hosted.body);

                return .{
                    .recursive = recursive,
                    .bind = bind,
                    .arg = first_arg,
                    .body = body,
                };
            },
            else => unreachable,
        };

        const arg_patterns = env.store.slicePatterns(lambda.args);
        const first_arg_pattern = if (arg_patterns.len == 0) null else arg_patterns[0];
        const first_arg = if (first_arg_pattern) |pattern_idx|
            try self.bindLambdaArg(module_idx, type_scope, pattern_idx)
        else
            try self.makeUnitArg(module_idx, type_scope, expr_idx);

        var body_env = BindingEnv.init(self.allocator);
        defer body_env.deinit();

        if (first_arg_pattern) |pattern_idx| {
            try self.bindPatternEnv(module_idx, type_scope, pattern_idx, &body_env);
        }

        const body = if (arg_patterns.len <= 1)
            try self.lowerExpr(module_idx, type_scope, body_env, lambda.body)
        else
            try self.lowerCurriedClosureChain(module_idx, type_scope, body_env, arg_patterns[1..], lambda.body);

        return .{
            .recursive = recursive,
            .bind = bind,
            .arg = first_arg,
            .body = body,
        };
    }

    fn lowerCurriedClosureChain(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        remaining_arg_patterns: []const CIR.Pattern.Idx,
        body_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.ExprId {
        std.debug.assert(remaining_arg_patterns.len > 0);

        const first_pattern = remaining_arg_patterns[0];
        const first_arg = try self.bindLambdaArg(module_idx, type_scope, first_pattern);
        var next_env = try self.cloneEnv(incoming_env);
        defer next_env.deinit();
        try self.bindPatternEnv(module_idx, type_scope, first_pattern, &next_env);

        const body = if (remaining_arg_patterns.len == 1)
            try self.lowerExpr(module_idx, type_scope, next_env, body_expr_idx)
        else
            try self.lowerCurriedClosureChain(module_idx, type_scope, next_env, remaining_arg_patterns[1..], body_expr_idx);

        const closure_ty = try self.ctx.types.addType(.{
            .func = .{
                .arg = first_arg.ty,
                .ret = self.program.store.getExpr(body).ty,
            },
        });

        return try self.program.store.addExpr(.{
            .ty = closure_ty,
            .data = .{ .clos = .{
                .arg = first_arg,
                .body = body,
            } },
        });
    }

    fn lowerExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.ExprId {
        const cir_env = self.ctx.env(module_idx);
        const expr = cir_env.store.getExpr(expr_idx);
        const ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(expr_idx));

        const data: ast.Expr.Data = switch (expr) {
            .e_num => |num| .{ .int_lit = @bitCast(num.value.bytes) },
            .e_frac_f32 => |frac| .{ .frac_f32_lit = frac.value },
            .e_frac_f64 => |frac| .{ .frac_f64_lit = frac.value },
            .e_dec => |dec| .{ .dec_lit = dec.value.num },
            .e_dec_small => |dec| .{ .dec_lit = dec.value.numerator },
            .e_typed_int => |num| .{ .int_lit = @bitCast(num.value.bytes) },
            .e_typed_frac => |frac| .{ .dec_lit = @bitCast(frac.value.bytes) },
            .e_str_segment => |seg| .{ .str_lit = seg.literal },
            .e_str => |str_expr| blk: {
                const parts = cir_env.store.sliceExpr(str_expr.span);
                if (parts.len != 1) {
                    debugTodo("monotype.lowerExpr string interpolation");
                }
                break :blk self.program.store.getExpr(try self.lowerExpr(module_idx, type_scope, env, parts[0])).data;
            },
            .e_lookup_local => |lookup| .{ .var_ = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, ty) },
            .e_lookup_external => |lookup| .{ .var_ = try self.lookupOrSpecializeExternal(module_idx, lookup, ty) },
            .e_lookup_required => |_| debugTodo("monotype.lowerExpr required lookup"),
            .e_call => |call| .{ .call = blk: {
                const lowered = try self.lowerCurriedCall(module_idx, type_scope, env, call.func, cir_env.store.sliceExpr(call.args));
                break :blk lowered;
            } },
            .e_lambda => |lambda| .{ .clos = try self.lowerAnonymousClosure(module_idx, type_scope, env, lambda.args, lambda.body) },
            .e_closure => |closure| .{ .clos = try self.lowerClosureExpr(module_idx, type_scope, env, closure) },
            .e_hosted_lambda => |_| debugTodo("monotype.lowerExpr hosted lambda"),
            .e_record => |record| .{ .record = try self.lowerRecordFields(module_idx, type_scope, env, record.fields) },
            .e_empty_record => .unit,
            .e_tuple => |tuple| .{ .tuple = try self.lowerExprList(module_idx, type_scope, env, tuple.elems) },
            .e_list => |list| .{ .list = try self.lowerExprList(module_idx, type_scope, env, list.elems) },
            .e_empty_list => .{ .list = ast.Span(ast.ExprId).empty() },
            .e_if => |if_expr| .{ .if_ = try self.lowerIfExpr(module_idx, type_scope, env, if_expr) },
            .e_match => |match_expr| .{ .when = try self.lowerMatchExpr(module_idx, type_scope, env, match_expr) },
            .e_block => |block| .{ .block = try self.lowerBlockExpr(module_idx, type_scope, env, block.stmts, block.final_expr) },
            .e_binop => |binop| .{ .low_level = .{
                .op = binopToLowLevel(binop.op),
                .args = try self.lowerExprSlice(module_idx, type_scope, env, &.{ binop.lhs, binop.rhs }),
            } },
            .e_unary_minus => |unary| .{ .low_level = .{
                .op = .num_negate,
                .args = try self.lowerExprSlice(module_idx, type_scope, env, &.{unary.expr}),
            } },
            .e_unary_not => |unary| .{ .low_level = .{
                .op = .bool_not,
                .args = try self.lowerExprSlice(module_idx, type_scope, env, &.{unary.expr}),
            } },
            .e_dot_access => |dot| blk: {
                if (dot.args != null) debugTodo("monotype.dot access method call");
                break :blk .{ .access = .{
                    .record = try self.lowerExpr(module_idx, type_scope, env, dot.receiver),
                    .field = dot.field_name,
                } };
            },
            .e_tag => |tag| .{ .tag = .{
                .name = tag.name,
                .args = try self.lowerExprList(module_idx, type_scope, env, tag.args),
            } },
            .e_zero_argument_tag => |tag| .{ .tag = .{
                .name = tag.name,
                .args = ast.Span(ast.ExprId).empty(),
            } },
            .e_runtime_error => .{ .runtime_error = try @constCast(cir_env).insertString("runtime error") },
            .e_crash => |crash| .{ .runtime_error = crash.msg },
            .e_expect => |expect| .{ .block = .{
                .stmts = try self.program.store.addStmtSpan(&.{try self.program.store.addStmt(.{
                    .expect = try self.lowerExpr(module_idx, type_scope, env, expect.body),
                })}),
                .final_expr = try self.program.store.addExpr(.{ .ty = ty, .data = .unit }),
            } },
            .e_return => |ret| .{ .return_ = try self.lowerExpr(module_idx, type_scope, env, ret.expr) },
            .e_for => |for_expr| .{ .for_ = try self.lowerForExpr(module_idx, type_scope, env, for_expr.patt, for_expr.expr, for_expr.body) },
            .e_run_low_level => |ll| .{ .low_level = .{
                .op = ll.op,
                .args = try self.lowerExprList(module_idx, type_scope, env, ll.args),
            } },
            else => debugTodoExpr(expr),
        };

        return try self.program.store.addExpr(.{ .ty = ty, .data = data });
    }

    fn lowerAnonymousClosure(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        args_span: CIR.Pattern.Span,
        body_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "clos") {
        const cir_env = self.ctx.env(module_idx);
        const arg_patterns = cir_env.store.slicePatterns(args_span);
        const first_arg_pattern = if (arg_patterns.len == 0) null else arg_patterns[0];
        const arg = if (first_arg_pattern) |pattern_idx|
            try self.bindLambdaArg(module_idx, type_scope, pattern_idx)
        else
            try self.makeUnitArg(module_idx, type_scope, body_expr_idx);

        var body_env = try self.cloneEnv(env);
        defer body_env.deinit();
        if (first_arg_pattern) |pattern_idx| {
            try self.bindPatternEnv(module_idx, type_scope, pattern_idx, &body_env);
        }

        const body = if (arg_patterns.len <= 1)
            try self.lowerExpr(module_idx, type_scope, body_env, body_expr_idx)
        else
            try self.lowerCurriedClosureChain(module_idx, type_scope, body_env, arg_patterns[1..], body_expr_idx);

        return .{
            .arg = arg,
            .body = body,
        };
    }

    fn lowerClosureExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        closure: CIR.Expr.Closure,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "clos") {
        _ = closure.captures;
        const lambda_expr = self.ctx.env(module_idx).store.getExpr(closure.lambda_idx);
        if (lambda_expr != .e_lambda) unreachable;
        return try self.lowerAnonymousClosure(module_idx, type_scope, env, lambda_expr.e_lambda.args, lambda_expr.e_lambda.body);
    }

    fn lowerIfExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        if_expr: anytype,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "if_") {
        const cir_env = self.ctx.env(module_idx);
        const branch_ids = cir_env.store.sliceIfBranches(if_expr.branches);
        if (branch_ids.len != 1) debugTodo("monotype.lowerIfExpr multi-branch if");
        const branch = cir_env.store.getIfBranch(branch_ids[0]);

        return .{
            .cond = try self.lowerExpr(module_idx, type_scope, env, branch.cond),
            .then_body = try self.lowerExpr(module_idx, type_scope, env, branch.body),
            .else_body = try self.lowerExpr(module_idx, type_scope, env, if_expr.final_else),
        };
    }

    fn lowerMatchExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "when") {
        const cir_env = self.ctx.env(module_idx);
        const branches = cir_env.store.matchBranchSlice(match_expr.branches);
        const out = try self.allocator.alloc(ast.Branch, branches.len);
        defer self.allocator.free(out);

        for (branches, 0..) |branch_idx, i| {
            const branch = cir_env.store.getMatchBranch(branch_idx);
            if (branch.guard != null) debugTodo("monotype.lowerMatchExpr guard");
            const branch_pattern_ids = cir_env.store.sliceMatchBranchPatterns(branch.patterns);
            if (branch_pattern_ids.len != 1) debugTodo("monotype.lowerMatchExpr or-pattern");
            const branch_pattern = cir_env.store.getMatchBranchPattern(branch_pattern_ids[0]);
            if (branch_pattern.degenerate) debugTodo("monotype.lowerMatchExpr degenerate branch");

            var branch_env = try self.cloneEnv(env);
            defer branch_env.deinit();
            try self.bindPatternEnv(module_idx, type_scope, branch_pattern.pattern, &branch_env);

            out[i] = .{
                .pat = try self.lowerPat(module_idx, type_scope, branch_pattern.pattern),
                .body = try self.lowerExpr(module_idx, type_scope, branch_env, branch.value),
            };
        }

        return .{
            .cond = try self.lowerExpr(module_idx, type_scope, env, match_expr.cond),
            .branches = try self.program.store.addBranchSpan(out),
        };
    }

    fn lowerBlockExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        stmts_span: CIR.Statement.Span,
        final_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        var env = try self.cloneEnv(incoming_env);
        defer env.deinit();

        const cir_env = self.ctx.env(module_idx);
        const cir_stmts = cir_env.store.sliceStatements(stmts_span);
        var lowered = std.ArrayList(ast.StmtId).empty;
        defer lowered.deinit(self.allocator);

        var i: usize = 0;
        while (i < cir_stmts.len) {
            if (try self.prebindLocalLambdaDeclGroup(module_idx, type_scope, &env, cir_stmts, i)) |group_end| {
                var group_i = i;
                while (group_i < group_end) : (group_i += 1) {
                    try lowered.append(self.allocator, try self.lowerStmt(module_idx, type_scope, &env, cir_stmts[group_i]));
                }
                i = group_end;
                continue;
            }

            try lowered.append(self.allocator, try self.lowerStmt(module_idx, type_scope, &env, cir_stmts[i]));
            i += 1;
        }

        return .{
            .stmts = try self.program.store.addStmtSpan(lowered.items),
            .final_expr = try self.lowerExpr(module_idx, type_scope, env, final_expr_idx),
        };
    }

    fn prebindLocalLambdaDeclGroup(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: *BindingEnv,
        cir_stmts: []const CIR.Statement.Idx,
        start_idx: usize,
    ) std.mem.Allocator.Error!?usize {
        if (start_idx >= cir_stmts.len) return null;

        const cir_env = self.ctx.env(module_idx);
        const first_stmt = cir_env.store.getStatement(cir_stmts[start_idx]);
        if (first_stmt != .s_decl) return null;
        if (!isLambdaExpr(cir_env.store.getExpr(first_stmt.s_decl.expr))) return null;

        var end_idx = start_idx;
        while (end_idx < cir_stmts.len) : (end_idx += 1) {
            const stmt = cir_env.store.getStatement(cir_stmts[end_idx]);
            if (stmt != .s_decl) break;
            if (!isLambdaExpr(cir_env.store.getExpr(stmt.s_decl.expr))) break;
        }

        for (cir_stmts[start_idx..end_idx]) |stmt_idx| {
            const decl = cir_env.store.getStatement(stmt_idx).s_decl;
            try self.bindPatternEnv(module_idx, type_scope, decl.pattern, env);
        }

        return end_idx;
    }

    fn lowerStmt(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: *BindingEnv,
        stmt_idx: CIR.Statement.Idx,
    ) std.mem.Allocator.Error!ast.StmtId {
        const cir_env = self.ctx.env(module_idx);
        const stmt = cir_env.store.getStatement(stmt_idx);

        const lowered: ast.Stmt = switch (stmt) {
            .s_decl => |decl| blk: {
                try self.bindPatternEnv(module_idx, type_scope, decl.pattern, env);
                const bind = try self.requirePatternBinder(module_idx, type_scope, decl.pattern);
                break :blk .{ .decl = .{
                    .bind = bind,
                    .body = try self.lowerExpr(module_idx, type_scope, env.*, decl.expr),
                } };
            },
            .s_var => |decl| blk: {
                try self.bindPatternEnv(module_idx, type_scope, decl.pattern_idx, env);
                const bind = try self.requirePatternBinder(module_idx, type_scope, decl.pattern_idx);
                break :blk .{ .var_decl = .{
                    .bind = bind,
                    .body = try self.lowerExpr(module_idx, type_scope, env.*, decl.expr),
                } };
            },
            .s_reassign => |reassign| .{ .reassign = .{
                .target = try self.requirePatternSymbol(module_idx, env.*, reassign.pattern_idx),
                .body = try self.lowerExpr(module_idx, type_scope, env.*, reassign.expr),
            } },
            .s_expr => |expr_stmt| .{ .expr = try self.lowerExpr(module_idx, type_scope, env.*, expr_stmt.expr) },
            .s_expect => |expect| .{ .expect = try self.lowerExpr(module_idx, type_scope, env.*, expect.body) },
            .s_crash => |crash| .{ .crash = crash.msg },
            .s_return => |ret| .{ .return_ = try self.lowerExpr(module_idx, type_scope, env.*, ret.expr) },
            .s_for => |for_stmt| blk: {
                var body_env = try self.cloneEnv(env.*);
                defer body_env.deinit();
                try self.bindPatternEnv(module_idx, type_scope, for_stmt.patt, &body_env);

                break :blk .{ .for_ = .{
                    .patt = try self.lowerPat(module_idx, type_scope, for_stmt.patt),
                    .iterable = try self.lowerExpr(module_idx, type_scope, env.*, for_stmt.expr),
                    .body = try self.lowerExpr(module_idx, type_scope, body_env, for_stmt.body),
                } };
            },
            else => debugTodoStmt(stmt),
        };

        return try self.program.store.addStmt(lowered);
    }

    fn lowerForExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        patt_idx: CIR.Pattern.Idx,
        iterable_expr_idx: CIR.Expr.Idx,
        body_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "for_") {
        var body_env = try self.cloneEnv(incoming_env);
        defer body_env.deinit();
        try self.bindPatternEnv(module_idx, type_scope, patt_idx, &body_env);

        return .{
            .patt = try self.lowerPat(module_idx, type_scope, patt_idx),
            .iterable = try self.lowerExpr(module_idx, type_scope, incoming_env, iterable_expr_idx),
            .body = try self.lowerExpr(module_idx, type_scope, body_env, body_expr_idx),
        };
    }

    fn lowerPat(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!ast.PatId {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        const ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx));
        return switch (pattern) {
            .assign => |assign| self.program.store.addPat(.{
                .ty = ty,
                .data = .{
                    .var_ = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
                },
            }),
            .as => |as_pat| self.program.store.addPat(.{
                .ty = ty,
                .data = .{
                    .var_ = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident),
                },
            }),
            .underscore => self.program.store.addPat(.{
                .ty = ty,
                .data = .{ .var_ = symbol_mod.Symbol.none },
            }),
            .applied_tag => |tag| blk: {
                const lowered_args = try self.allocator.alloc(ast.PatId, tag.args.span.len);
                defer self.allocator.free(lowered_args);
                for (self.ctx.env(module_idx).store.slicePatterns(tag.args), 0..) |arg_pat, i| {
                    lowered_args[i] = try self.lowerPat(module_idx, type_scope, arg_pat);
                }
                break :blk self.program.store.addPat(.{
                    .ty = ty,
                    .data = .{ .tag = .{
                        .name = tag.name,
                        .args = try self.program.store.addPatSpan(lowered_args),
                    } },
                });
            },
            .nominal => |nominal| blk: {
                const lowered = try self.lowerPat(module_idx, type_scope, nominal.backing_pattern);
                const backing = self.program.store.getPat(lowered);
                break :blk self.program.store.addPat(.{
                    .ty = ty,
                    .data = backing.data,
                });
            },
            .nominal_external => |nominal| blk: {
                const lowered = try self.lowerPat(module_idx, type_scope, nominal.backing_pattern);
                const backing = self.program.store.getPat(lowered);
                break :blk self.program.store.addPat(.{
                    .ty = ty,
                    .data = backing.data,
                });
            },
            else => debugTodoPattern(pattern),
        };
    }

    fn lowerExprList(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        return self.lowerExprSlice(module_idx, type_scope, env, self.ctx.env(module_idx).store.sliceExpr(span));
    }

    fn lowerExprSlice(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        exprs: []const CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const out = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(out);
        for (exprs, 0..) |expr_idx, i| {
            out[i] = try self.lowerExpr(module_idx, type_scope, env, expr_idx);
        }
        return try self.program.store.addExprSpan(out);
    }

    fn lowerRecordFields(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        span: CIR.RecordField.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const cir_env = self.ctx.env(module_idx);
        const fields = cir_env.store.sliceRecordFields(span);
        const out = try self.allocator.alloc(ast.FieldExpr, fields.len);
        defer self.allocator.free(out);

        for (fields, 0..) |field_idx, i| {
            const field = cir_env.store.getRecordField(field_idx);
            out[i] = .{
                .name = field.name,
                .value = try self.lowerExpr(module_idx, type_scope, env, field.value),
            };
        }

        return try self.program.store.addFieldExprSpan(out);
    }

    fn lowerCurriedCall(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "call") {
        var current = try self.lowerExpr(module_idx, type_scope, env, func_expr_idx);
        if (arg_exprs.len == 0) {
            const unit_expr = try self.program.store.addExpr(.{
                .ty = try self.ctx.types.addType(.{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } }),
                .data = .unit,
            });
            return .{ .func = current, .arg = unit_expr };
        }

        for (arg_exprs[0 .. arg_exprs.len - 1]) |arg_expr_idx| {
            const arg_expr = try self.lowerExpr(module_idx, type_scope, env, arg_expr_idx);
            const result_ty = self.callResultType(current);
            current = try self.program.store.addExpr(.{
                .ty = result_ty,
                .data = .{ .call = .{
                    .func = current,
                    .arg = arg_expr,
                } },
            });
        }

        return .{
            .func = current,
            .arg = try self.lowerExpr(module_idx, type_scope, env, arg_exprs[arg_exprs.len - 1]),
        };
    }

    fn lowerSolvedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        original_var: Var,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const var_to_lower = try type_scope.instantiator.instantiateVar(original_var);
        return self.lowerInstantiatedType(module_idx, var_to_lower);
    }

    fn lowerInstantiatedType(
        self: *Lowerer,
        module_idx: u32,
        var_: Var,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const env = self.ctx.env(module_idx);
        const resolved = env.types.resolveVar(var_);
        const key: TypeKey = .{ .module_idx = module_idx, .var_ = resolved.var_ };
        if (self.type_cache.get(key)) |cached| return cached;

        const placeholder = try self.ctx.types.addType(.{ .primitive = .erased });
        try self.type_cache.put(key, placeholder);

        const lowered: type_mod.Content = switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                    const arg_slice = env.types.sliceVars(func.args);
                    const arg_ids = try self.allocator.alloc(type_mod.TypeId, arg_slice.len);
                    defer self.allocator.free(arg_ids);
                    for (arg_slice, 0..) |arg_var, i| {
                        arg_ids[i] = try self.lowerInstantiatedType(module_idx, arg_var);
                    }
                    break :blk try self.buildCurriedFuncType(arg_ids, try self.lowerInstantiatedType(module_idx, func.ret));
                },
                .tag_union => |tag_union| blk: {
                    const tags_slice = env.types.getTagsSlice(tag_union.tags);
                    const lowered_tags = try self.allocator.alloc(type_mod.Tag, tags_slice.len);
                    defer self.allocator.free(lowered_tags);
                    for (0..tags_slice.len) |i| {
                        const tag_name = tags_slice.items(.name)[i];
                        const tag_args = env.types.sliceVars(tags_slice.items(.args)[i]);
                        const arg_ids = try self.allocator.alloc(type_mod.TypeId, tag_args.len);
                        defer self.allocator.free(arg_ids);
                        for (tag_args, 0..) |arg_var, j| {
                            arg_ids[j] = try self.lowerInstantiatedType(module_idx, arg_var);
                        }
                        lowered_tags[i] = .{
                            .name = tag_name,
                            .args = try self.ctx.types.addTypeSpan(arg_ids),
                        };
                    }
                    break :blk .{ .tag_union = .{
                        .tags = try self.ctx.types.addTags(lowered_tags),
                    } };
                },
                .record => |record| blk: {
                    const fields_slice = env.types.getRecordFieldsSlice(record.fields);
                    const lowered_fields = try self.allocator.alloc(type_mod.Field, fields_slice.len);
                    defer self.allocator.free(lowered_fields);
                    for (0..fields_slice.len) |i| {
                        lowered_fields[i] = .{
                            .name = fields_slice.items(.name)[i],
                            .ty = try self.lowerInstantiatedType(module_idx, fields_slice.items(.var_)[i]),
                        };
                    }
                    break :blk .{ .record = .{
                        .fields = try self.ctx.types.addFields(lowered_fields),
                    } };
                },
                .record_unbound => |fields| blk: {
                    const fields_slice = env.types.getRecordFieldsSlice(fields);
                    const lowered_fields = try self.allocator.alloc(type_mod.Field, fields_slice.len);
                    defer self.allocator.free(lowered_fields);
                    for (0..fields_slice.len) |i| {
                        lowered_fields[i] = .{
                            .name = fields_slice.items(.name)[i],
                            .ty = try self.lowerInstantiatedType(module_idx, fields_slice.items(.var_)[i]),
                        };
                    }
                    break :blk .{ .record = .{
                        .fields = try self.ctx.types.addFields(lowered_fields),
                    } };
                },
                .empty_record => .{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } },
                .empty_tag_union => .{ .tag_union = .{ .tags = type_mod.Span(type_mod.Tag).empty() } },
                .tuple => |_| debugTodo("monotype.lowerInstantiatedType tuple"),
                .nominal_type => |nominal| try self.lowerNominalType(module_idx, nominal),
            },
            .alias => |alias| self.ctx.types.getType(try self.lowerInstantiatedType(module_idx, env.types.getAliasBackingVar(alias))),
            .flex, .rigid => debugPanic(
                "monotype invariant violated: unresolved type var {d} in module {d}",
                .{ @intFromEnum(resolved.var_), module_idx },
            ),
            .err => .{ .primitive = .erased },
        };

        self.ctx.types.types.items[@intFromEnum(placeholder)] = lowered;
        return placeholder;
    }

    fn lowerNominalType(
        self: *Lowerer,
        module_idx: u32,
        nominal: types.NominalType,
    ) std.mem.Allocator.Error!type_mod.Content {
        const env = self.ctx.env(module_idx);
        const ident = nominal.ident.ident_idx;

        if (ident.eql(env.idents.str) or ident.eql(env.idents.builtin_str)) {
            return .{ .primitive = .str };
        }
        if (ident.eql(env.idents.list)) {
            const args = env.types.sliceNominalArgs(nominal);
            if (args.len != 1) debugPanic("monotype.lowerNominalType List expected one type argument", .{});
            return .{ .list = try self.lowerInstantiatedType(module_idx, args[0]) };
        }
        if (ident.eql(env.idents.bool_type)) {
            return .{ .tag_union = .{ .tags = type_mod.Span(type_mod.Tag).empty() } };
        }
        if (builtinNumPrim(env, ident)) |prim| return .{ .primitive = prim };

        const backing_var = env.types.getNominalBackingVar(nominal);
        return self.ctx.types.getType(try self.lowerInstantiatedType(module_idx, backing_var));
    }

    fn bindLambdaArg(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        return switch (self.ctx.env(module_idx).store.getPattern(pattern_idx)) {
            .assign => |assign| .{
                .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx)),
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            },
            .as => |as_pat| .{
                .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx)),
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident),
            },
            .underscore => try self.makeUnitArg(module_idx, type_scope, pattern_idx),
            else => debugTodoPattern(self.ctx.env(module_idx).store.getPattern(pattern_idx)),
        };
    }

    fn makeUnitArg(
        self: *Lowerer,
        _: u32,
        _: *TypeCloneScope,
        source_node: anytype,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        _ = source_node;
        return .{
            .ty = try self.ctx.types.addType(.{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } }),
            .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
        };
    }

    fn bindPatternEnv(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign| {
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident);
                try env.put(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx)),
                    .symbol = symbol,
                });
            },
            .as => |as_pat| {
                try self.bindPatternEnv(module_idx, type_scope, as_pat.pattern, env);
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident);
                try env.put(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx)),
                    .symbol = symbol,
                });
            },
            .applied_tag => |tag| {
                for (self.ctx.env(module_idx).store.slicePatterns(tag.args)) |arg_pat| {
                    try self.bindPatternEnv(module_idx, type_scope, arg_pat, env);
                }
            },
            .nominal => |nominal| try self.bindPatternEnv(module_idx, type_scope, nominal.backing_pattern, env),
            .nominal_external => |nominal| try self.bindPatternEnv(module_idx, type_scope, nominal.backing_pattern, env),
            .underscore => {},
            else => debugTodoPattern(pattern),
        }
    }

    fn requirePatternBinder(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign => |assign| .{
                .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx)),
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            },
            .as => |as_pat| .{
                .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx)),
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident),
            },
            else => debugTodoPattern(pattern),
        };
    }

    fn lookupOrSpecializeLocal(
        self: *Lowerer,
        module_idx: u32,
        _: *TypeCloneScope,
        env: BindingEnv,
        pattern_idx: CIR.Pattern.Idx,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        if (env.get(.{ .module_idx = module_idx, .pattern_idx = @intFromEnum(pattern_idx) })) |typed| {
            return typed.symbol;
        }

        const top_level_symbol = self.lookupTopLevelSymbol(module_idx, pattern_idx) orelse debugPanic(
            "monotype invariant violated: missing local binding symbol for module {d} pattern {d}",
            .{ module_idx, @intFromEnum(pattern_idx) },
        );

        if (self.top_level_defs_by_symbol.get(top_level_symbol)) |top_level| {
            if (top_level.is_function) {
                return try self.specializations.specializeFn(&self.ctx, top_level_symbol, .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                }, expected_ty);
            }
        }

        return top_level_symbol;
    }

    fn lookupOrSpecializeExternal(
        self: *Lowerer,
        current_module_idx: u32,
        lookup: anytype,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const current_env = self.ctx.env(current_module_idx);
        const module_idx = current_env.imports.getResolvedModule(lookup.module_idx) orelse debugPanic(
            "monotype invariant violated: unresolved import {d}",
            .{@intFromEnum(lookup.module_idx)},
        );
        const symbol = self.lookupTopLevelDefSymbol(module_idx, @enumFromInt(lookup.target_node_idx)) orelse debugPanic(
            "monotype invariant violated: unresolved external def symbol {d}:{d}",
            .{ module_idx, lookup.target_node_idx },
        );

        if (self.top_level_defs_by_symbol.get(symbol)) |top_level| {
            if (top_level.is_function) {
                return try self.specializations.specializeFn(&self.ctx, symbol, .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                }, expected_ty);
            }
        }

        return symbol;
    }

    fn buildCurriedFuncType(
        self: *Lowerer,
        arg_ids: []const type_mod.TypeId,
        ret_id: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.Content {
        if (arg_ids.len == 0) {
            const unit_ty = try self.ctx.types.addType(.{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } });
            return .{ .func = .{ .arg = unit_ty, .ret = ret_id } };
        }

        var current_ret = ret_id;
        var i = arg_ids.len;
        while (i > 0) : (i -= 1) {
            current_ret = try self.ctx.types.addType(.{
                .func = .{
                    .arg = arg_ids[i - 1],
                    .ret = current_ret,
                },
            });
        }
        return self.ctx.types.getType(current_ret);
    }

    fn callResultType(self: *Lowerer, func_expr: ast.ExprId) type_mod.TypeId {
        const func_ty = self.program.store.getExpr(func_expr).ty;
        const content = self.ctx.types.getType(func_ty);
        return switch (content) {
            .func => |func| func.ret,
            else => debugPanic(
                "monotype invariant violated: attempted to call non-function type {d}",
                .{@intFromEnum(func_ty)},
            ),
        };
    }

    fn lookupTopLevelDefSymbol(self: *const Lowerer, module_idx: u32, def_idx: CIR.Def.Idx) ?symbol_mod.Symbol {
        var iter = self.top_level_defs_by_symbol.iterator();
        while (iter.next()) |entry| {
            const top_level = entry.value_ptr.*;
            if (top_level.module_idx == module_idx and top_level.def_idx == def_idx) {
                return entry.key_ptr.*;
            }
        }
        return null;
    }

    fn lookupTopLevelSymbol(self: *const Lowerer, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?symbol_mod.Symbol {
        return self.top_level_symbols_by_pattern.get(.{
            .module_idx = module_idx,
            .pattern_idx = @intFromEnum(pattern_idx),
        });
    }

    fn requirePatternSymbol(
        self: *const Lowerer,
        module_idx: u32,
        env: BindingEnv,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        if (env.get(.{ .module_idx = module_idx, .pattern_idx = @intFromEnum(pattern_idx) })) |typed| {
            return typed.symbol;
        }
        return self.lookupTopLevelSymbol(module_idx, pattern_idx) orelse debugPanic(
            "monotype invariant violated: missing symbol for module {d} pattern {d}",
            .{ module_idx, @intFromEnum(pattern_idx) },
        );
    }

    fn cloneEnv(self: *Lowerer, env: BindingEnv) std.mem.Allocator.Error!BindingEnv {
        var out = BindingEnv.init(self.allocator);
        errdefer out.deinit();

        var iter = env.iterator();
        while (iter.next()) |entry| {
            try out.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        return out;
    }

    fn isTopLevelFunction(self: *const Lowerer, symbol: symbol_mod.Symbol) bool {
        return if (self.top_level_defs_by_symbol.get(symbol)) |top_level| top_level.is_function else false;
    }
};

fn isLambdaExpr(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_lambda, .e_closure, .e_hosted_lambda => true,
        else => false,
    };
}

fn isRecursiveTopLevelDef(env: *const ModuleEnv, def_idx: CIR.Def.Idx) bool {
    const evaluation_order = env.evaluation_order orelse return false;
    for (evaluation_order.sccs) |scc| {
        for (scc.defs) |member| {
            if (member == def_idx) return scc.is_recursive;
        }
    }
    return false;
}

fn builtinNumPrim(env: *const ModuleEnv, ident: base.Ident.Idx) ?type_mod.Prim {
    if (ident.eql(env.idents.u8_type)) return .u8;
    if (ident.eql(env.idents.i8_type)) return .i8;
    if (ident.eql(env.idents.u16_type)) return .u16;
    if (ident.eql(env.idents.i16_type)) return .i16;
    if (ident.eql(env.idents.u32_type)) return .u32;
    if (ident.eql(env.idents.i32_type)) return .i32;
    if (ident.eql(env.idents.u64_type)) return .u64;
    if (ident.eql(env.idents.i64_type)) return .i64;
    if (ident.eql(env.idents.u128_type)) return .u128;
    if (ident.eql(env.idents.i128_type)) return .i128;
    if (ident.eql(env.idents.f32_type)) return .f32;
    if (ident.eql(env.idents.f64_type)) return .f64;
    if (ident.eql(env.idents.dec_type)) return .dec;
    return null;
}

fn binopToLowLevel(op: CIR.Expr.Binop.Op) base.LowLevel {
    return switch (op) {
        .add => .num_plus,
        .sub => .num_minus,
        .mul => .num_times,
        .div => .num_div_by,
        .rem => .num_rem_by,
        .lt => .num_is_lt,
        .gt => .num_is_gt,
        .le => .num_is_lte,
        .ge => .num_is_gte,
        .eq => .num_is_eq,
        .div_trunc => .num_div_trunc_by,
        .ne => debugTodo("monotype.binop !="),
        .@"and" => debugTodo("monotype.binop and"),
        .@"or" => debugTodo("monotype.binop or"),
    };
}

fn debugTodo(comptime msg: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("TODO {s}", .{msg});
    } else unreachable;
}

fn debugPanic(comptime fmt: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(fmt, args);
    } else unreachable;
}

fn debugTodoExpr(expr: CIR.Expr) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("TODO monotype.lowerExpr expr {s}", .{@tagName(expr)});
    } else unreachable;
}

fn debugTodoStmt(stmt: CIR.Statement) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("TODO monotype.lowerStmt stmt {s}", .{@tagName(stmt)});
    } else unreachable;
}

fn debugTodoPattern(pattern: CIR.Pattern) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("TODO monotype.lowerPattern {s}", .{@tagName(pattern)});
    } else unreachable;
}

test "monotype lower tests" {
    std.testing.refAllDecls(@This());
}
