//! Monotype lowering from checked CIR.
//!
//! This follows cor's strategy:
//! - lower solved checker facts into a monomorphic type graph
//! - register top-level function sources once
//! - lower top-level values/runs immediately
//! - specialize top-level functions on demand from variable references
//! - keep local lambda defs as lexical function bindings for later lifting
//! - carry lexical binding facts explicitly in recursive lowering
//!
//! Roc-only constructs that cor does not have yet remain explicit extensions:
//! loops, mutable statements, blocks, returns, and low-level ops.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const types = @import("types");
const instantiate = @import("types").instantiate;
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const specializations_mod = @import("specializations.zig");
const symbol_mod = @import("symbol");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const Instantiator = instantiate.Instantiator;
const dec_scale_i128: i128 = 1_000_000_000_000_000_000;

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
    strings: base.StringLiteral.Store,
    idents: base.Ident.Store,

    pub fn deinit(self: *Result) void {
        self.program.deinit();
        self.symbols.deinit();
        self.types.deinit();
        self.strings.deinit(self.program.store.allocator);
        self.idents.deinit(self.program.store.allocator);
    }
};

const Ctx = struct {
    allocator: std.mem.Allocator,
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    idents: base.Ident.Store,
    all_module_envs: []const *const ModuleEnv,
    builtin_module_idx: u32,
    top_level_symbols: std.AutoHashMap(TopLevelKey, symbol_mod.Symbol),
    pattern_symbols: std.AutoHashMap(PatternKey, symbol_mod.Symbol),

    const TopLevelKey = struct {
        module_idx: u32,
        def_idx: u32,
    };

    const PatternKey = struct {
        module_idx: u32,
        pattern_idx: u32,
    };

    fn init(
        allocator: std.mem.Allocator,
        all_module_envs: []const *const ModuleEnv,
        builtin_module_idx: u32,
    ) std.mem.Allocator.Error!Ctx {
        return .{
            .allocator = allocator,
            .symbols = symbol_mod.Store.init(allocator),
            .types = type_mod.Store.init(allocator),
            .idents = try base.Ident.Store.initCapacity(allocator, 256),
            .all_module_envs = all_module_envs,
            .builtin_module_idx = builtin_module_idx,
            .top_level_symbols = std.AutoHashMap(TopLevelKey, symbol_mod.Symbol).init(allocator),
            .pattern_symbols = std.AutoHashMap(PatternKey, symbol_mod.Symbol).init(allocator),
        };
    }

    fn deinit(self: *Ctx) void {
        self.pattern_symbols.deinit();
        self.top_level_symbols.deinit();
        self.idents.deinit(self.allocator);
        self.symbols.deinit();
        self.types.deinit();
    }

    fn env(self: *const Ctx, module_idx: u32) *const ModuleEnv {
        return self.all_module_envs[module_idx];
    }

    fn getTypeIdentText(self: *const Ctx, module_idx: u32, ident: base.Ident.Idx) []const u8 {
        return self.env(module_idx).getIdent(ident);
    }

    fn getOrCreateTopLevelSymbol(
        self: *Ctx,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        name: base.Ident.Idx,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const key: TopLevelKey = .{
            .module_idx = module_idx,
            .def_idx = @intFromEnum(def_idx),
        };
        if (self.top_level_symbols.get(key)) |symbol| return symbol;

        const symbol = try self.symbols.add(try self.copyExecutableIdent(module_idx, name), .{
            .top_level_def = .{
                .module_idx = module_idx,
                .def_idx = @intFromEnum(def_idx),
            },
        });
        try self.top_level_symbols.put(key, symbol);
        return symbol;
    }

    fn getOrCreatePatternSymbol(
        self: *Ctx,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        name: base.Ident.Idx,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const key: PatternKey = .{
            .module_idx = module_idx,
            .pattern_idx = @intFromEnum(pattern_idx),
        };
        if (self.pattern_symbols.get(key)) |symbol| return symbol;

        const symbol = try self.symbols.add(try self.copyExecutableIdent(module_idx, name), .{
            .local_pattern = .{
                .module_idx = module_idx,
                .pattern_idx = @intFromEnum(pattern_idx),
            },
        });
        try self.pattern_symbols.put(key, symbol);
        return symbol;
    }

    fn addSyntheticSymbol(self: *Ctx, name: base.Ident.Idx) std.mem.Allocator.Error!symbol_mod.Symbol {
        return self.symbols.add(name, .synthetic);
    }

    fn copyExecutableIdent(
        self: *Ctx,
        module_idx: u32,
        ident: base.Ident.Idx,
    ) std.mem.Allocator.Error!base.Ident.Idx {
        if (ident.isNone()) return ident;
        const text = self.env(module_idx).getIdent(ident);
        return self.idents.insert(self.allocator, base.Ident.for_text(text));
    }
};

const ResolvedTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const ExpectedTypeFact = struct {
    ty: type_mod.TypeId,
    solved_var: ?Var,
};

pub const Lowerer = struct {
    allocator: std.mem.Allocator,
    ctx: Ctx,
    program: Program,
    strings: base.StringLiteral.Store,
    specializations: specializations_mod.Queue,
    top_level_defs_by_symbol: std.AutoHashMap(symbol_mod.Symbol, TopLevelDef),
    top_level_symbols_by_pattern: std.AutoHashMap(PatternKey, symbol_mod.Symbol),
    emitted_defs_by_symbol: std.AutoHashMap(symbol_mod.Symbol, ast.DefId),
    emitting_value_defs: std.AutoHashMap(symbol_mod.Symbol, void),
    local_fn_groups: std.ArrayList(*LocalFnGroupState),

    const PatternKey = struct {
        module_idx: u32,
        pattern_idx: u32,
    };

    const TopLevelDef = struct {
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        pattern_idx: CIR.Pattern.Idx,
        is_function: bool,
        needs_specialization: bool,
    };

    const BindingDecl = struct {
        bind: ast.TypedSymbol,
        body: ast.ExprId,
    };

    const LocalFnSourceRef = struct {
        group_index: u32,
        source_index: u32,
    };

    const TypedBinding = struct {
        bind: ast.TypedSymbol,
        solved_var: ?Var = null,
    };

    const BindingValue = union(enum) {
        typed: TypedBinding,
        local_fn_source: LocalFnSourceRef,
    };

    const BindingEnv = std.AutoHashMap(PatternKey, BindingValue);

    const LocalFnSource = struct {
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
        source_symbol: symbol_mod.Symbol,
    };

    const LocalFnPending = struct {
        source_index: u32,
        ty: type_mod.TypeId,
        symbol: symbol_mod.Symbol,
        stmt_id: ?ast.StmtId = null,
    };

    const LocalFnGroupState = struct {
        module_idx: u32,
        declaration_env: BindingEnv,
        insertion_index: usize,
        sources: []LocalFnSource,
        pending: std.ArrayList(LocalFnPending),

        fn deinit(self: *LocalFnGroupState, allocator: std.mem.Allocator) void {
            self.declaration_env.deinit();
            allocator.free(self.sources);
            self.pending.deinit(allocator);
        }
    };

    const TypeCloneScope = struct {
        const TypeKey = struct {
            module_idx: u32,
            var_: Var,
        };

        var_map: std.AutoHashMap(Var, Var),
        type_cache: std.AutoHashMap(TypeKey, type_mod.TypeId),
        nominal_type_cache: std.StringHashMap(type_mod.TypeId),
        scratch_nominal_key: std.ArrayList(u8),
        instantiator: Instantiator,

        fn initCloneAll(
            self: *TypeCloneScope,
            allocator: std.mem.Allocator,
            env: *ModuleEnv,
        ) void {
            self.initCloneAllFromParent(allocator, env, null);
        }

        fn initCloneAllFromParent(
            self: *TypeCloneScope,
            allocator: std.mem.Allocator,
            env: *ModuleEnv,
            parent: ?*const TypeCloneScope,
        ) void {
            self.initWithRankBehavior(allocator, env, .ignore_rank);
            if (parent) |scope| {
                var iter = scope.var_map.iterator();
                while (iter.next()) |entry| {
                    self.var_map.put(entry.key_ptr.*, entry.value_ptr.*) catch unreachable;
                }
                var type_iter = scope.type_cache.iterator();
                while (type_iter.next()) |entry| {
                    self.type_cache.put(entry.key_ptr.*, entry.value_ptr.*) catch unreachable;
                }
            }
        }

        fn initWithRankBehavior(
            self: *TypeCloneScope,
            allocator: std.mem.Allocator,
            env: *ModuleEnv,
            rank_behavior: Instantiator.RankBehavior,
        ) void {
            self.var_map = std.AutoHashMap(Var, Var).init(allocator);
            self.type_cache = std.AutoHashMap(TypeKey, type_mod.TypeId).init(allocator);
            self.nominal_type_cache = std.StringHashMap(type_mod.TypeId).init(allocator);
            self.scratch_nominal_key = .empty;
            self.instantiator = .{
                .store = &env.types,
                .idents = env.getIdentStoreConst(),
                .var_map = &self.var_map,
                .current_rank = .outermost,
                .rigid_behavior = .fresh_flex,
                .rank_behavior = rank_behavior,
            };
        }

        fn deinit(self: *TypeCloneScope) void {
            var nominal_keys = self.nominal_type_cache.keyIterator();
            while (nominal_keys.next()) |key_ptr| {
                self.instantiator.store.gpa.free(key_ptr.*);
            }
            self.nominal_type_cache.deinit();
            self.scratch_nominal_key.deinit(self.instantiator.store.gpa);
            self.type_cache.deinit();
            self.var_map.deinit();
        }
    };

    pub fn init(
        allocator: std.mem.Allocator,
        all_module_envs: []const *const ModuleEnv,
        builtin_module_idx: u32,
    ) std.mem.Allocator.Error!Lowerer {
        for (all_module_envs) |env| {
            const mutable_env = @constCast(env);
            try mutable_env.getIdentStore().enableRuntimeInserts(mutable_env.gpa);
        }

        return .{
            .allocator = allocator,
            .ctx = try Ctx.init(allocator, all_module_envs, builtin_module_idx),
            .program = Program.init(allocator),
            .strings = .{},
            .specializations = specializations_mod.Queue.init(allocator),
            .top_level_defs_by_symbol = std.AutoHashMap(symbol_mod.Symbol, TopLevelDef).init(allocator),
            .top_level_symbols_by_pattern = std.AutoHashMap(PatternKey, symbol_mod.Symbol).init(allocator),
            .emitted_defs_by_symbol = std.AutoHashMap(symbol_mod.Symbol, ast.DefId).init(allocator),
            .emitting_value_defs = std.AutoHashMap(symbol_mod.Symbol, void).init(allocator),
            .local_fn_groups = .empty,
        };
    }

    pub fn deinit(self: *Lowerer) void {
        for (self.local_fn_groups.items) |group| {
            group.deinit(self.allocator);
            self.allocator.destroy(group);
        }
        self.local_fn_groups.deinit(self.allocator);
        self.emitting_value_defs.deinit();
        self.emitted_defs_by_symbol.deinit();
        self.top_level_symbols_by_pattern.deinit();
        self.top_level_defs_by_symbol.deinit();
        self.specializations.deinit();
        self.program.deinit();
        self.strings.deinit(self.allocator);
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
            .strings = self.strings,
            .idents = self.ctx.idents,
        };
        self.program = Program.init(self.allocator);
        self.ctx.symbols = symbol_mod.Store.init(self.allocator);
        self.ctx.types = type_mod.Store.init(self.allocator);
        self.ctx.idents = try base.Ident.Store.initCapacity(self.allocator, 1);
        self.strings = .{};
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
                    .needs_specialization = env.types.needsInstantiation(ModuleEnv.varFrom(def.expr)),
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
                if (self.emitted_defs_by_symbol.get(bind_symbol)) |existing| break :blk existing;
                if (self.isTopLevelFunction(bind_symbol)) break :blk null;

                var type_scope: TypeCloneScope = undefined;
                type_scope.initCloneAll(self.allocator, @constCast(env));
                defer type_scope.deinit();
                var binding_env = BindingEnv.init(self.allocator);
                defer binding_env.deinit();

                const bind_ty = try self.lowerSolvedType(module_idx, &type_scope, ModuleEnv.varFrom(def.expr));
                const body = try self.lowerExpr(module_idx, &type_scope, binding_env, def.expr);
                const lowered = try self.program.store.addDef(.{
                    .bind = .{ .ty = bind_ty, .symbol = bind_symbol },
                    .value = .{ .val = body },
                });
                try self.emitted_defs_by_symbol.put(bind_symbol, lowered);
                break :blk lowered;
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

        var type_scope: TypeCloneScope = undefined;
        type_scope.initCloneAll(self.allocator, @constCast(env));
        defer type_scope.deinit();
        var binding_env = BindingEnv.init(self.allocator);
        defer binding_env.deinit();

        const bind_ty = try self.lowerSolvedType(module_idx, &type_scope, ModuleEnv.varFrom(def.expr));
        const body = try self.lowerExpr(module_idx, &type_scope, binding_env, def.expr);
        const lowered = try self.program.store.addDef(.{
            .bind = .{ .ty = bind_ty, .symbol = bind_symbol },
            .value = .{ .run = .{
                .bind = .{ .ty = bind_ty, .symbol = bind_symbol },
                .body = body,
                .entry_ty = fx_var,
            } },
        });
        if (!bind_symbol.isNone()) {
            try self.emitted_defs_by_symbol.put(bind_symbol, lowered);
        }
        return lowered;
    }

    fn ensureTopLevelValueDefEmitted(
        self: *Lowerer,
        symbol: symbol_mod.Symbol,
    ) std.mem.Allocator.Error!void {
        if (self.emitted_defs_by_symbol.contains(symbol)) return;

        const top_level = self.top_level_defs_by_symbol.get(symbol) orelse debugPanic(
            "monotype invariant violated: missing top-level facts for global symbol {d}",
            .{symbol.raw()},
        );
        if (top_level.is_function) {
            return debugPanic(
                "monotype invariant violated: attempted to emit top-level value def for function symbol {d}",
                .{symbol.raw()},
            );
        }
        if (self.emitting_value_defs.contains(symbol)) {
            return debugPanic(
                "monotype invariant violated: recursive top-level value survived into monotype lowering for symbol {d}",
                .{symbol.raw()},
            );
        }

        try self.emitting_value_defs.put(symbol, {});
        defer _ = self.emitting_value_defs.remove(symbol);

        _ = try self.lowerTopLevelDef(top_level.module_idx, top_level.def_idx) orelse debugPanic(
            "monotype invariant violated: failed to emit reachable top-level value symbol {d}",
            .{symbol.raw()},
        );
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
        if (self.emitted_defs_by_symbol.get(pending.specialized_symbol)) |existing| return existing;
        const env = self.ctx.env(pending.source.module_idx);
        const def = env.store.getDef(pending.source.def_idx);
        const specialized_checker_var = try self.requireSpecializedTopLevelCheckerVar(pending);

        var type_scope: TypeCloneScope = undefined;
        type_scope.initCloneAll(self.allocator, @constCast(env));
        defer type_scope.deinit();

        const bind: ast.TypedSymbol = .{
            .ty = try self.lowerInstantiatedType(pending.source.module_idx, &type_scope, specialized_checker_var),
            .symbol = pending.specialized_symbol,
        };

        var binding_env = BindingEnv.init(self.allocator);
        defer binding_env.deinit();

        const letfn = try self.lowerLambdaLikeDefWithEnv(
            pending.source.module_idx,
            &type_scope,
            binding_env,
            bind,
            def.expr,
            isRecursiveTopLevelDef(env, pending.source.def_idx),
            specialized_checker_var,
        );
        const lowered = try self.program.store.addDef(.{
            .bind = bind,
            .value = .{ .fn_ = letfn },
        });
        try self.emitted_defs_by_symbol.put(bind.symbol, lowered);
        return lowered;
    }

    fn requireSpecializedTopLevelCheckerVar(
        _: *Lowerer,
        pending: specializations_mod.Pending,
    ) std.mem.Allocator.Error!Var {
        return pending.expected_checker_var orelse debugPanic(
            "monotype specialization invariant violated: missing checker specialization facts for top-level def {d}:{d}",
            .{ pending.source.module_idx, @intFromEnum(pending.source.def_idx) },
        );
    }

    fn lowerLambdaLikeDefWithEnv(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        bind: ast.TypedSymbol,
        expr_idx: CIR.Expr.Idx,
        recursive: bool,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!ast.LetFn {
        const env = self.ctx.env(module_idx);
        const expr = env.store.getExpr(expr_idx);
        if (expected_var) |var_| {
            const source_expr_var = ModuleEnv.varFrom(expr_idx);
            try type_scope.var_map.put(source_expr_var, var_);
            var arg_index: usize = 0;
            while (true) : (arg_index += 1) {
                const source_arg_var = self.lookupFunctionArgVar(module_idx, source_expr_var, arg_index) orelse break;
                const specialized_arg_var = self.lookupFunctionArgVar(module_idx, var_, arg_index) orelse break;
                try type_scope.var_map.put(source_arg_var, specialized_arg_var);
            }
            if (self.lookupFunctionRetVar(module_idx, source_expr_var)) |source_ret_var| {
                if (self.lookupFunctionRetVar(module_idx, var_)) |specialized_ret_var| {
                    try type_scope.var_map.put(source_ret_var, specialized_ret_var);
                }
            }
        }
        const fn_parts = self.requireFunctionType(bind.ty);
        const source_fn_var = expected_var orelse ModuleEnv.varFrom(expr_idx);

        const lambda = switch (expr) {
            .e_lambda => |lambda| lambda,
            .e_closure => |closure| blk: {
                const lambda_expr = env.store.getExpr(closure.lambda_idx);
                if (lambda_expr != .e_lambda) unreachable;
                break :blk lambda_expr.e_lambda;
            },
            .e_hosted_lambda => |hosted| {
                if (hosted.args.span.len == 0) {
                    const unit_arg = try self.makeUnitArgWithType(fn_parts.arg);
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
                const first_arg = try self.bindLambdaArg(module_idx, type_scope, arg_patterns[0], fn_parts.arg);
                const first_arg_var = self.requireFunctionArgVar(module_idx, source_fn_var, 0);
                var body_env = try self.cloneEnv(incoming_env);
                defer body_env.deinit();
                var binding_decls = std.ArrayList(BindingDecl).empty;
                defer binding_decls.deinit(self.allocator);
                try self.collectStructuralBindingDeclsWithSolvedVar(
                    module_idx,
                    type_scope,
                    first_arg,
                    first_arg_var,
                    arg_patterns[0],
                    &body_env,
                    &binding_decls,
                );

                const body = if (arg_patterns.len == 1)
                    try self.wrapExprWithBindingDecls(
                        try self.lowerExprWithExpectedType(module_idx, type_scope, body_env, hosted.body, fn_parts.ret, self.lookupFunctionRetVar(module_idx, source_fn_var)),
                        binding_decls.items,
                    )
                else
                    try self.wrapExprWithBindingDecls(
                        try self.lowerCurriedClosureChain(module_idx, type_scope, body_env, fn_parts.ret, source_fn_var, 1, arg_patterns[1..], hosted.body),
                        binding_decls.items,
                    );

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
            try self.bindLambdaArg(module_idx, type_scope, pattern_idx, fn_parts.arg)
        else
            try self.makeUnitArgWithType(fn_parts.arg);

        var body_env = try self.cloneEnv(incoming_env);
        defer body_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);

        if (first_arg_pattern) |pattern_idx| {
            try self.collectStructuralBindingDeclsWithSolvedVar(
                module_idx,
                type_scope,
                first_arg,
                self.requireFunctionArgVar(module_idx, source_fn_var, 0),
                pattern_idx,
                &body_env,
                &binding_decls,
            );
        }

        const body = if (arg_patterns.len <= 1)
            try self.wrapExprWithBindingDecls(
                try self.lowerExprWithExpectedType(module_idx, type_scope, body_env, lambda.body, fn_parts.ret, self.lookupFunctionRetVar(module_idx, source_fn_var)),
                binding_decls.items,
            )
        else
            try self.wrapExprWithBindingDecls(
                try self.lowerCurriedClosureChain(module_idx, type_scope, body_env, fn_parts.ret, source_fn_var, 1, arg_patterns[1..], lambda.body),
                binding_decls.items,
            );

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
        current_fn_ty: type_mod.TypeId,
        source_fn_var: ?Var,
        next_arg_index: usize,
        remaining_arg_patterns: []const CIR.Pattern.Idx,
        body_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.ExprId {
        std.debug.assert(remaining_arg_patterns.len > 0);
        const fn_parts = self.requireFunctionType(current_fn_ty);

        const first_pattern = remaining_arg_patterns[0];
        const first_arg = try self.bindLambdaArg(module_idx, type_scope, first_pattern, fn_parts.arg);
        var next_env = try self.cloneEnv(incoming_env);
        defer next_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);
        try self.collectStructuralBindingDeclsWithSolvedVar(
            module_idx,
            type_scope,
            first_arg,
            self.requireFunctionArgVar(module_idx, source_fn_var, next_arg_index),
            first_pattern,
            &next_env,
            &binding_decls,
        );

        const body = if (remaining_arg_patterns.len == 1)
            try self.wrapExprWithBindingDecls(
                try self.lowerExprWithExpectedType(module_idx, type_scope, next_env, body_expr_idx, fn_parts.ret, self.lookupFunctionRetVar(module_idx, source_fn_var)),
                binding_decls.items,
            )
        else
            try self.wrapExprWithBindingDecls(
                try self.lowerCurriedClosureChain(module_idx, type_scope, next_env, fn_parts.ret, source_fn_var, next_arg_index + 1, remaining_arg_patterns[1..], body_expr_idx),
                binding_decls.items,
            );

        return try self.program.store.addExpr(.{
            .ty = current_fn_ty,
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
        switch (expr) {
            .e_nominal => |nominal| return self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                nominal.backing_expr,
                try self.lowerExprType(module_idx, type_scope, env, expr_idx, expr),
            ),
            .e_nominal_external => |nominal| return self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                nominal.backing_expr,
                try self.lowerExprType(module_idx, type_scope, env, expr_idx, expr),
            ),
            else => {},
        }

        if (expr == .e_call) {
            if (try self.maybeLowerSpecialCallExpr(module_idx, type_scope, env, expr_idx, expr.e_call, null)) |special| {
                return special;
            }

            const lowered_call = try self.lowerCurriedCall(
                module_idx,
                type_scope,
                env,
                expr_idx,
                expr.e_call.func,
                cir_env.store.sliceExpr(expr.e_call.args),
                null,
            );
            return try self.program.store.addExpr(.{
                .ty = lowered_call.result_ty,
                .data = .{ .call = lowered_call.data },
            });
        }

        const env_lookup_ty: ?type_mod.TypeId = switch (expr) {
            .e_lookup_local => |lookup| if (env.get(.{
                .module_idx = module_idx,
                .pattern_idx = @intFromEnum(lookup.pattern_idx),
            })) |entry| switch (entry) {
                .typed => |typed| typed.bind.ty,
                .local_fn_source => null,
            } else null,
            else => null,
        };
        const ty = if (env_lookup_ty) |lookup_ty|
            lookup_ty
        else
            try self.lowerExprType(module_idx, type_scope, env, expr_idx, expr);

        if (expr == .e_str) {
            return self.lowerStringExpr(module_idx, type_scope, env, ty, expr.e_str);
        }

        const data: ast.Expr.Data = switch (expr) {
            .e_num => |num| try self.lowerNumericIntLiteralData(ty, num.value),
            .e_frac_f32 => |frac| .{ .frac_f32_lit = frac.value },
            .e_frac_f64 => |frac| .{ .frac_f64_lit = frac.value },
            .e_dec => |dec| try self.lowerNumericDecLiteralData(ty, dec.value.toI128()),
            .e_dec_small => |dec| try self.lowerNumericSmallDecLiteralData(ty, dec.value),
            .e_typed_int => |num| try self.lowerNumericIntLiteralData(ty, num.value),
            .e_typed_frac => |frac| try self.lowerNumericDecLiteralData(ty, @bitCast(frac.value.bytes)),
            .e_str_segment => |seg| .{ .str_lit = try self.copySourceStringLiteral(module_idx, seg.literal) },
            .e_str => unreachable,
            .e_lookup_local => |lookup| .{ .var_ = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, ty, null) },
            .e_lookup_external => |lookup| .{ .var_ = try self.lookupOrSpecializeExternal(module_idx, lookup, ty, null) },
            .e_lookup_required => |_| debugTodo("monotype.lowerExpr required lookup"),
            .e_call => unreachable,
            .e_lambda => |lambda| .{ .clos = try self.lowerAnonymousClosure(module_idx, type_scope, env, expr_idx, ty, lambda.args, lambda.body, null) },
            .e_closure => |closure| .{ .clos = try self.lowerClosureExpr(module_idx, type_scope, env, expr_idx, ty, closure, null) },
            .e_hosted_lambda => |_| debugTodo("monotype.lowerExpr hosted lambda"),
            .e_record => |record| return self.lowerRecordExpr(module_idx, type_scope, env, ty, record),
            .e_empty_record => .unit,
            .e_tuple => |tuple| .{ .tuple = try self.lowerExprList(module_idx, type_scope, env, tuple.elems) },
            .e_list => |list| .{ .list = try self.lowerExprList(module_idx, type_scope, env, list.elems) },
            .e_empty_list => .{ .list = ast.Span(ast.ExprId).empty() },
            .e_if => |if_expr| .{ .if_ = try self.lowerIfExpr(module_idx, type_scope, env, if_expr) },
            .e_match => |match_expr| try self.lowerMatchExprData(module_idx, type_scope, env, ty, null, match_expr),
            .e_block => |block| .{ .block = try self.lowerBlockExpr(module_idx, type_scope, env, block.stmts, block.final_expr) },
            .e_binop => |binop| blk: {
                if (binop.op == .ne) {
                    const eq_args = try self.lowerExprSlice(module_idx, type_scope, env, &.{ binop.lhs, binop.rhs });
                    const eq_expr = try self.program.store.addExpr(.{
                        .ty = try self.makePrimitiveType(.bool),
                        .data = .{ .low_level = .{
                            .op = .num_is_eq,
                            .args = eq_args,
                        } },
                    });
                    break :blk .{ .low_level = .{
                        .op = .bool_not,
                        .args = try self.program.store.addExprSpan(&.{eq_expr}),
                    } };
                }
                if (binop.op == .@"and" or binop.op == .@"or") {
                    const lhs_expr = try self.lowerExpr(module_idx, type_scope, env, binop.lhs);
                    const lhs_ty = self.program.store.getExpr(lhs_expr).ty;
                    const lhs_bind: ast.TypedSymbol = .{
                        .ty = lhs_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    const lhs_var = try self.makeVarExpr(lhs_ty, lhs_bind.symbol);
                    const rhs_expr = try self.lowerExpr(module_idx, type_scope, env, binop.rhs);
                    const rest = try self.program.store.addExpr(.{
                        .ty = ty,
                        .data = .{ .if_ = switch (binop.op) {
                            .@"and" => .{
                                .cond = lhs_var,
                                .then_body = rhs_expr,
                                .else_body = lhs_var,
                            },
                            .@"or" => .{
                                .cond = lhs_var,
                                .then_body = lhs_var,
                                .else_body = rhs_expr,
                            },
                            else => unreachable,
                        } },
                    });
                    break :blk .{ .let_ = .{
                        .def = .{ .let_val = .{
                            .bind = lhs_bind,
                            .body = lhs_expr,
                        } },
                        .rest = rest,
                    } };
                }
                break :blk .{ .low_level = .{
                    .op = binopToLowLevel(binop.op),
                    .args = try self.lowerExprSlice(module_idx, type_scope, env, &.{ binop.lhs, binop.rhs }),
                } };
            },
            .e_unary_minus => |unary| .{ .low_level = .{
                .op = .num_negate,
                .args = try self.lowerExprSlice(module_idx, type_scope, env, &.{unary.expr}),
            } },
            .e_unary_not => |unary| .{ .low_level = .{
                .op = .bool_not,
                .args = try self.lowerExprSlice(module_idx, type_scope, env, &.{unary.expr}),
            } },
            .e_dot_access => |dot| blk: {
                if (dot.args) |dispatch_args| {
                    _ = dispatch_args;
                    break :blk .{ .call = try self.lowerRecordedMethodCall(
                        module_idx,
                        type_scope,
                        env,
                        expr_idx,
                        dot.field_name,
                        null,
                    ) };
                }
                break :blk .{ .access = .{
                    .record = try self.lowerExpr(module_idx, type_scope, env, dot.receiver),
                    .field = try self.ctx.copyExecutableIdent(module_idx, dot.field_name),
                } };
            },
            .e_tag => |tag| blk: {
                if (self.ctx.types.getType(ty) == .primitive and self.ctx.types.getType(ty).primitive == .bool) {
                    if (cir_env.store.sliceExpr(tag.args).len != 0) {
                        return debugPanic("monotype bool tag invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    break :blk .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag.name) };
                }
                break :blk .{ .tag = .{
                    .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
                    .args = try self.lowerExprList(module_idx, type_scope, env, tag.args),
                } };
            },
            .e_zero_argument_tag => |tag| blk: {
                if (self.ctx.types.getType(ty) == .primitive and self.ctx.types.getType(ty).primitive == .bool) {
                    break :blk .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag.name) };
                }
                break :blk .{ .tag = .{
                    .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
                    .args = ast.Span(ast.ExprId).empty(),
                } };
            },
            .e_runtime_error => .{ .runtime_error = try self.internStringLiteral("runtime error") },
            .e_crash => |crash| .{ .runtime_error = try self.copySourceStringLiteral(module_idx, crash.msg) },
            .e_expect => |expect| .{ .block = .{
                .stmts = try self.program.store.addStmtSpan(&.{try self.program.store.addStmt(.{
                    .expect = try self.lowerExpr(module_idx, type_scope, env, expect.body),
                })}),
                .final_expr = try self.program.store.addExpr(.{ .ty = ty, .data = .unit }),
            } },
            .e_dbg => |dbg| blk: {
                const debug_stmt = try self.program.store.addStmt(.{
                    .debug = try self.lowerDebugMessageExpr(module_idx, type_scope, env, dbg.expr),
                });
                break :blk .{ .block = .{
                    .stmts = try self.program.store.addStmtSpan(&.{debug_stmt}),
                    .final_expr = try self.makeUnitExpr(),
                } };
            },
            .e_return => |ret| .{ .return_ = try self.lowerExpr(module_idx, type_scope, env, ret.expr) },
            .e_for => |for_expr| .{ .for_ = try self.lowerForExpr(module_idx, type_scope, env, for_expr.patt, for_expr.expr, for_expr.body) },
            .e_run_low_level => |ll| .{ .low_level = .{
                .op = ll.op,
                .args = try self.lowerExprList(module_idx, type_scope, env, ll.args),
            } },
            .e_type_var_dispatch => |dispatch| .{ .call = try self.lowerRecordedMethodCall(
                module_idx,
                type_scope,
                env,
                expr_idx,
                dispatch.method_name,
                null,
            ) },
            else => debugTodoExpr(expr),
        };

        return try self.program.store.addExpr(.{ .ty = ty, .data = data });
    }

    fn lowerStringExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        result_ty: type_mod.TypeId,
        str_expr: @FieldType(CIR.Expr, "e_str"),
    ) std.mem.Allocator.Error!ast.ExprId {
        const parts = self.ctx.env(module_idx).store.sliceExpr(str_expr.span);
        if (parts.len == 0) {
            return self.makeStringLiteralExpr(module_idx, result_ty, "");
        }

        var current = try self.lowerExpr(module_idx, type_scope, env, parts[0]);
        for (parts[1..]) |part| {
            const next = try self.lowerExpr(module_idx, type_scope, env, part);
            current = try self.makeStringConcatExpr(result_ty, current, next);
        }
        return current;
    }

    const ResolvedTopLevelSource = struct {
        module_idx: u32,
        def_idx: CIR.Def.Idx,
    };

    fn maybeLowerBuiltinSpecialCall(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        call: @FieldType(CIR.Expr, "e_call"),
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!?ast.ExprId {
        const cir_env = self.ctx.env(module_idx);
        const arg_exprs = cir_env.store.sliceExpr(call.args);
        if (arg_exprs.len != 1) return null;

        const source = self.resolveDirectTopLevelSource(module_idx, env, call.func) orelse return null;
        if (!self.isBuiltinStrInspectSource(source.module_idx, source.def_idx)) return null;

        if (self.exprVarIsErroneous(module_idx, arg_exprs[0])) {
            return try self.lowerErroneousExprWithType(module_idx, type_scope, env, arg_exprs[0], result_ty);
        }

        const lowered_arg = try self.lowerExpr(module_idx, type_scope, env, arg_exprs[0]);
        return try self.program.store.addExpr(.{
            .ty = result_ty,
            .data = .{ .inspect = lowered_arg },
        });
    }

    fn resolveDirectTopLevelSource(
        self: *Lowerer,
        current_module_idx: u32,
        env: BindingEnv,
        func_expr_idx: CIR.Expr.Idx,
    ) ?ResolvedTopLevelSource {
        const current_env = self.ctx.env(current_module_idx);
        return switch (current_env.store.getExpr(func_expr_idx)) {
            .e_lookup_local => |lookup| blk: {
                if (env.get(.{
                    .module_idx = current_module_idx,
                    .pattern_idx = @intFromEnum(lookup.pattern_idx),
                })) |_| break :blk null;

                const symbol = self.lookupTopLevelSymbol(current_module_idx, lookup.pattern_idx) orelse break :blk null;
                const top_level = self.top_level_defs_by_symbol.get(symbol) orelse break :blk null;
                break :blk .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                };
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = current_env.imports.getResolvedModule(lookup.module_idx) orelse debugPanic(
                    "monotype invariant violated: unresolved import {d}",
                    .{@intFromEnum(lookup.module_idx)},
                );
                break :blk .{
                    .module_idx = target_module_idx,
                    .def_idx = @enumFromInt(lookup.target_node_idx),
                };
            },
            else => null,
        };
    }

    fn isBuiltinStrInspectSource(
        self: *const Lowerer,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
    ) bool {
        if (module_idx != self.ctx.builtin_module_idx) return false;

        const env = self.ctx.env(module_idx);
        const inspect_ident = env.common.findIdent("Builtin.Str.inspect") orelse return false;
        const def = env.store.getDef(def_idx);
        return switch (env.store.getPattern(def.pattern)) {
            .assign => |assign| assign.ident.eql(inspect_ident),
            else => false,
        };
    }

    fn lowerNumericIntLiteralData(
        self: *Lowerer,
        target_ty: type_mod.TypeId,
        value: CIR.IntValue,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        return switch (self.ctx.types.getType(target_ty)) {
            .placeholder => debugPanic(
                "monotype numeric literal invariant violated: integer literal lowered to placeholder type",
                .{},
            ),
            .primitive => |prim| switch (prim) {
                .dec => blk: {
                    const dec_value = try self.decFromWholeInt(value.toI128());
                    break :blk .{ .dec_lit = dec_value };
                },
                .f32 => .{ .frac_f32_lit = @as(f32, @floatFromInt(value.toI128())) },
                .f64 => .{ .frac_f64_lit = @as(f64, @floatFromInt(value.toI128())) },
                .u8,
                .i8,
                .u16,
                .i16,
                .u32,
                .i32,
                .u64,
                .i64,
                .u128,
                .i128,
                => .{ .int_lit = @bitCast(value.bytes) },
                .bool, .str, .erased => debugPanic(
                    "monotype numeric literal invariant violated: integer literal lowered to non-numeric primitive {s}",
                    .{@tagName(prim)},
                ),
            },
            else => debugTodo("monotype.lowerNumericIntLiteralData custom from_numeral target"),
        };
    }

    fn lowerNumericDecLiteralData(
        self: *Lowerer,
        target_ty: type_mod.TypeId,
        scaled_dec: i128,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        return switch (self.ctx.types.getType(target_ty)) {
            .placeholder => debugPanic(
                "monotype numeric literal invariant violated: decimal literal lowered to placeholder type",
                .{},
            ),
            .primitive => |prim| switch (prim) {
                .dec => .{ .dec_lit = scaled_dec },
                .f32 => .{ .frac_f32_lit = @floatCast(self.decToF64(scaled_dec)) },
                .f64 => .{ .frac_f64_lit = self.decToF64(scaled_dec) },
                .bool, .str, .erased => debugPanic(
                    "monotype numeric literal invariant violated: decimal literal lowered to non-numeric primitive {s}",
                    .{@tagName(prim)},
                ),
                .u8,
                .i8,
                .u16,
                .i16,
                .u32,
                .i32,
                .u64,
                .i64,
                .u128,
                .i128,
                => debugTodo("monotype.lowerNumericDecLiteralData fractional literal to integer target"),
            },
            else => debugTodo("monotype.lowerNumericDecLiteralData custom from_numeral target"),
        };
    }

    fn lowerNumericSmallDecLiteralData(
        self: *Lowerer,
        target_ty: type_mod.TypeId,
        value: CIR.SmallDecValue,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        return switch (self.ctx.types.getType(target_ty)) {
            .placeholder => debugPanic(
                "monotype numeric literal invariant violated: small decimal literal lowered to placeholder type",
                .{},
            ),
            .primitive => |prim| switch (prim) {
                .dec => .{ .dec_lit = try self.decFromFraction(value.numerator, value.denominator_power_of_ten) },
                .f32 => .{ .frac_f32_lit = @floatCast(value.toF64()) },
                .f64 => .{ .frac_f64_lit = value.toF64() },
                .bool, .str, .erased => debugPanic(
                    "monotype numeric literal invariant violated: small decimal literal lowered to non-numeric primitive {s}",
                    .{@tagName(prim)},
                ),
                .u8,
                .i8,
                .u16,
                .i16,
                .u32,
                .i32,
                .u64,
                .i64,
                .u128,
                .i128,
                => debugTodo("monotype.lowerNumericSmallDecLiteralData fractional literal to integer target"),
            },
            else => debugTodo("monotype.lowerNumericSmallDecLiteralData custom from_numeral target"),
        };
    }

    fn decFromWholeInt(self: *Lowerer, value: i128) std.mem.Allocator.Error!i128 {
        _ = self;
        const result = @mulWithOverflow(value, dec_scale_i128);
        if (result[1] != 0) {
            debugPanic(
                "monotype numeric literal invariant violated: Dec whole-int literal overflowed i128 representation",
                .{},
            );
        }
        return result[0];
    }

    fn decFromFraction(
        self: *Lowerer,
        numerator: i128,
        denominator_power_of_ten: u8,
    ) std.mem.Allocator.Error!i128 {
        _ = self;
        const scale_power: u8 = if (denominator_power_of_ten >= 18) 0 else 18 - denominator_power_of_ten;
        var scale: i128 = 1;
        var i: u8 = 0;
        while (i < scale_power) : (i += 1) {
            const mul_result = @mulWithOverflow(scale, @as(i128, 10));
            if (mul_result[1] != 0) {
                debugPanic("monotype numeric literal invariant violated: Dec fractional scale overflowed", .{});
            }
            scale = mul_result[0];
        }

        const result = @mulWithOverflow(numerator, scale);
        if (result[1] != 0) {
            debugPanic(
                "monotype numeric literal invariant violated: Dec fractional literal overflowed i128 representation",
                .{},
            );
        }
        return result[0];
    }

    fn decToF64(self: *Lowerer, scaled_dec: i128) f64 {
        _ = self;
        return @as(f64, @floatFromInt(scaled_dec)) / 1_000_000_000_000_000_000.0;
    }

    fn makePrimitiveType(self: *Lowerer, prim: type_mod.Prim) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.ctx.types.addType(.{ .primitive = prim });
    }

    fn makeUnitType(self: *Lowerer) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.ctx.types.addType(.{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } });
    }

    fn makeUnitExpr(self: *Lowerer) std.mem.Allocator.Error!ast.ExprId {
        return try self.program.store.addExpr(.{
            .ty = try self.makeUnitType(),
            .data = .unit,
        });
    }

    fn makeVarExpr(
        self: *Lowerer,
        ty: type_mod.TypeId,
        symbol: symbol_mod.Symbol,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.program.store.addExpr(.{
            .ty = ty,
            .data = .{ .var_ = symbol },
        });
    }

    fn makeStringLiteralExpr(
        self: *Lowerer,
        module_idx: u32,
        str_ty: type_mod.TypeId,
        bytes: []const u8,
    ) std.mem.Allocator.Error!ast.ExprId {
        _ = module_idx;
        return try self.program.store.addExpr(.{
            .ty = str_ty,
            .data = .{ .str_lit = try self.internStringLiteral(bytes) },
        });
    }

    fn internStringLiteral(
        self: *Lowerer,
        bytes: []const u8,
    ) std.mem.Allocator.Error!base.StringLiteral.Idx {
        return self.strings.insert(self.allocator, bytes);
    }

    fn copySourceStringLiteral(
        self: *Lowerer,
        module_idx: u32,
        idx: base.StringLiteral.Idx,
    ) std.mem.Allocator.Error!base.StringLiteral.Idx {
        return self.internStringLiteral(self.ctx.env(module_idx).getString(idx));
    }

    fn requireBoolLiteralValue(
        self: *Lowerer,
        module_idx: u32,
        tag_name: base.Ident.Idx,
    ) bool {
        const idents = self.ctx.env(module_idx).idents;
        if (tag_name.eql(idents.true_tag)) return true;
        if (tag_name.eql(idents.false_tag)) return false;
        debugPanic("monotype bool literal invariant violated: expected True or False", .{});
    }

    fn makeStringConcatExpr(
        self: *Lowerer,
        str_ty: type_mod.TypeId,
        left: ast.ExprId,
        right: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        return self.makeLowLevelExpr(str_ty, .str_concat, &.{ left, right });
    }

    fn makeLowLevelExpr(
        self: *Lowerer,
        ret_ty: type_mod.TypeId,
        op: base.LowLevel,
        args: []const ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.program.store.addExpr(.{
            .ty = ret_ty,
            .data = .{ .low_level = .{
                .op = op,
                .args = try self.program.store.addExprSpan(args),
            } },
        });
    }

    fn makeU64LiteralExpr(self: *Lowerer, value: u64) std.mem.Allocator.Error!ast.ExprId {
        return try self.program.store.addExpr(.{
            .ty = try self.makePrimitiveType(.u64),
            .data = .{ .int_lit = value },
        });
    }

    fn makeTupleAccessExpr(
        self: *Lowerer,
        tuple_expr: ast.ExprId,
        tuple_ty: type_mod.TypeId,
        elem_index: usize,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.program.store.addExpr(.{
            .ty = self.lookupTupleElemType(tuple_ty, elem_index),
            .data = .{ .tuple_access = .{
                .tuple = tuple_expr,
                .elem_index = @intCast(elem_index),
            } },
        });
    }

    fn makeListSliceBoundsType(
        self: *Lowerer,
        len_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const start_ident = try self.ctx.idents.insert(self.allocator, base.Ident.for_text("start"));
        const len_ident = try self.ctx.idents.insert(self.allocator, base.Ident.for_text("len"));

        var fields = [_]type_mod.Field{
            .{ .name = len_ident, .ty = len_ty },
            .{ .name = start_ident, .ty = len_ty },
        };
        std.mem.sort(type_mod.Field, &fields, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);

        return try self.ctx.types.addType(.{ .record = .{
            .fields = try self.ctx.types.addFields(&fields),
        } });
    }

    fn makeListSliceBoundsExpr(
        self: *Lowerer,
        start_expr: ast.ExprId,
        len_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const bounds_ty = try self.makeListSliceBoundsType(self.program.store.getExpr(len_expr).ty);
        const start_ident = self.ctx.idents.findByString("start") orelse
            debugPanic("monotype invariant violated: missing executable start ident", .{});
        const len_ident = self.ctx.idents.findByString("len") orelse
            debugPanic("monotype invariant violated: missing executable len ident", .{});
        return try self.program.store.addExpr(.{
            .ty = bounds_ty,
            .data = .{ .record = try self.program.store.addFieldExprSpan(&.{
                .{ .name = len_ident, .value = len_expr },
                .{ .name = start_ident, .value = start_expr },
            }) },
        });
    }

    fn lowerAnonymousClosure(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        source_expr_idx: CIR.Expr.Idx,
        closure_ty: type_mod.TypeId,
        args_span: CIR.Pattern.Span,
        body_expr_idx: CIR.Expr.Idx,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "clos") {
        const cir_env = self.ctx.env(module_idx);
        const arg_patterns = cir_env.store.slicePatterns(args_span);
        const fn_parts = self.requireFunctionType(closure_ty);
        const source_fn_var = expected_var orelse ModuleEnv.varFrom(source_expr_idx);
        const first_arg_pattern = if (arg_patterns.len == 0) null else arg_patterns[0];
        const arg = if (first_arg_pattern) |pattern_idx|
            try self.bindLambdaArg(module_idx, type_scope, pattern_idx, fn_parts.arg)
        else
            try self.makeUnitArgWithType(fn_parts.arg);

        var body_env = try self.cloneEnv(env);
        defer body_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);
        if (first_arg_pattern) |pattern_idx| {
            try self.collectStructuralBindingDeclsWithSolvedVar(
                module_idx,
                type_scope,
                arg,
                self.requireFunctionArgVar(module_idx, source_fn_var, 0),
                pattern_idx,
                &body_env,
                &binding_decls,
            );
        }

        const body = if (arg_patterns.len <= 1)
            try self.wrapExprWithBindingDecls(
                try self.lowerExprWithExpectedType(module_idx, type_scope, body_env, body_expr_idx, fn_parts.ret, self.lookupFunctionRetVar(module_idx, source_fn_var)),
                binding_decls.items,
            )
        else
            try self.wrapExprWithBindingDecls(
                try self.lowerCurriedClosureChain(module_idx, type_scope, body_env, fn_parts.ret, source_fn_var, 1, arg_patterns[1..], body_expr_idx),
                binding_decls.items,
            );

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
        source_expr_idx: CIR.Expr.Idx,
        closure_ty: type_mod.TypeId,
        closure: CIR.Expr.Closure,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "clos") {
        _ = closure.captures;
        const lambda_expr = self.ctx.env(module_idx).store.getExpr(closure.lambda_idx);
        if (lambda_expr != .e_lambda) unreachable;
        return try self.lowerAnonymousClosure(module_idx, type_scope, env, source_expr_idx, closure_ty, lambda_expr.e_lambda.args, lambda_expr.e_lambda.body, expected_var);
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
        if (branch_ids.len == 0) debugPanic("monotype invariant violated: if expression missing branches", .{});

        var else_body = try self.lowerExpr(module_idx, type_scope, env, if_expr.final_else);
        var idx = branch_ids.len;
        while (idx > 1) {
            idx -= 1;
            const branch = cir_env.store.getIfBranch(branch_ids[idx]);
            else_body = try self.program.store.addExpr(.{
                .ty = self.program.store.getExpr(else_body).ty,
                .data = .{ .if_ = .{
                    .cond = try self.lowerExpr(module_idx, type_scope, env, branch.cond),
                    .then_body = try self.lowerExpr(module_idx, type_scope, env, branch.body),
                    .else_body = else_body,
                } },
            });
        }

        const branch = cir_env.store.getIfBranch(branch_ids[0]);

        return .{
            .cond = try self.lowerExpr(module_idx, type_scope, env, branch.cond),
            .then_body = try self.lowerExpr(module_idx, type_scope, env, branch.body),
            .else_body = else_body,
        };
    }

    fn lowerMatchExprData(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        if (try self.matchExprCanLowerDirect(module_idx, match_expr)) {
            return .{ .block = try self.lowerDirectMatchExpr(module_idx, type_scope, env, result_ty, expected_result_var, match_expr) };
        }
        if (self.matchExprNeedsListDesugaring(module_idx, match_expr)) {
            return .{ .block = try self.lowerListMatchExpr(module_idx, type_scope, env, result_ty, expected_result_var, match_expr) };
        }
        return .{ .when = try self.lowerMatchExpr(module_idx, type_scope, env, result_ty, expected_result_var, match_expr) };
    }

    fn matchExprCanLowerDirect(
        self: *Lowerer,
        module_idx: u32,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!bool {
        const cir_env = self.ctx.env(module_idx);
        const branches = cir_env.store.matchBranchSlice(match_expr.branches);
        if (branches.len != 1) return false;

        const branch = cir_env.store.getMatchBranch(branches[0]);
        if (branch.guard != null) return false;

        const branch_pattern_ids = cir_env.store.sliceMatchBranchPatterns(branch.patterns);
        if (branch_pattern_ids.len != 1) return false;

        const branch_pattern = cir_env.store.getMatchBranchPattern(branch_pattern_ids[0]);
        if (branch_pattern.degenerate) return false;

        return self.patternIsIrrefutableStructural(module_idx, branch_pattern.pattern);
    }

    fn patternIsIrrefutableStructural(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) bool {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign, .underscore, .as, .record_destructure, .tuple => true,
            .nominal => |nominal| self.patternIsIrrefutableStructural(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.patternIsIrrefutableStructural(module_idx, nominal.backing_pattern),
            else => false,
        };
    }

    fn lowerDirectMatchExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        const cir_env = self.ctx.env(module_idx);
        const branch_id = cir_env.store.matchBranchSlice(match_expr.branches)[0];
        const branch = cir_env.store.getMatchBranch(branch_id);
        const branch_pattern_id = cir_env.store.sliceMatchBranchPatterns(branch.patterns)[0];
        const branch_pattern = cir_env.store.getMatchBranchPattern(branch_pattern_id);

        const scrutinee_expr = try self.lowerExpr(module_idx, type_scope, env, match_expr.cond);
        const scrutinee_ty = self.program.store.getExpr(scrutinee_expr).ty;

        var branch_env = try self.cloneEnv(env);
        defer branch_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);

        try self.bindPatternFromSourceExpr(
            module_idx,
            type_scope,
            branch_pattern.pattern,
            scrutinee_expr,
            scrutinee_ty,
            &branch_env,
            &binding_decls,
        );

        return .{
            .stmts = try self.program.store.addStmtSpan(&.{}),
            .final_expr = try self.wrapExprWithBindingDecls(
                try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    branch_env,
                    branch.value,
                    result_ty,
                    expected_result_var,
                ),
                binding_decls.items,
            ),
        };
    }

    fn lowerMatchExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "when") {
        const cir_env = self.ctx.env(module_idx);
        const branches = cir_env.store.matchBranchSlice(match_expr.branches);
        const out = try self.allocator.alloc(ast.Branch, branches.len);
        defer self.allocator.free(out);
        const cond = try self.lowerExpr(module_idx, type_scope, env, match_expr.cond);
        const cond_ty = self.program.store.getExpr(cond).ty;

        for (branches, 0..) |branch_idx, i| {
            const branch = cir_env.store.getMatchBranch(branch_idx);
            if (branch.guard != null) debugTodo("monotype.lowerMatchExpr guard");
            const branch_pattern_ids = cir_env.store.sliceMatchBranchPatterns(branch.patterns);
            if (branch_pattern_ids.len != 1) debugTodo("monotype.lowerMatchExpr or-pattern");
            const branch_pattern = cir_env.store.getMatchBranchPattern(branch_pattern_ids[0]);
            if (branch_pattern.degenerate) debugTodo("monotype.lowerMatchExpr degenerate branch");

            var branch_env = try self.cloneEnv(env);
            defer branch_env.deinit();
            var binding_decls = std.ArrayList(BindingDecl).empty;
            defer binding_decls.deinit(self.allocator);

            out[i] = .{
                .pat = try self.lowerMatchPatWithType(
                    module_idx,
                    type_scope,
                    branch_pattern.pattern,
                    cond_ty,
                    &branch_env,
                    &binding_decls,
                ),
                .body = try self.wrapExprWithBindingDecls(
                    try self.lowerExprWithExpectedType(
                        module_idx,
                        type_scope,
                        branch_env,
                        branch.value,
                        result_ty,
                        expected_result_var,
                    ),
                    binding_decls.items,
                ),
            };
        }

        return .{
            .cond = cond,
            .branches = try self.program.store.addBranchSpan(out),
        };
    }

    fn matchExprNeedsListDesugaring(
        self: *Lowerer,
        module_idx: u32,
        match_expr: CIR.Expr.Match,
    ) bool {
        const cir_env = self.ctx.env(module_idx);
        for (cir_env.store.matchBranchSlice(match_expr.branches)) |branch_idx| {
            const branch = cir_env.store.getMatchBranch(branch_idx);
            if (branch.guard != null) return true;
            const branch_pattern_ids = cir_env.store.sliceMatchBranchPatterns(branch.patterns);
            if (branch_pattern_ids.len != 1) return true;
            const branch_pattern = cir_env.store.getMatchBranchPattern(branch_pattern_ids[0]);
            if (branch_pattern.degenerate) return true;
            if (self.patternNeedsListDesugaring(module_idx, branch_pattern.pattern)) return true;
        }
        return false;
    }

    fn patternNeedsListDesugaring(self: *Lowerer, module_idx: u32, pattern_idx: CIR.Pattern.Idx) bool {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .list => true,
            .as => |as_pat| self.patternNeedsListDesugaring(module_idx, as_pat.pattern),
            .nominal => |nominal| self.patternNeedsListDesugaring(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.patternNeedsListDesugaring(module_idx, nominal.backing_pattern),
            else => false,
        };
    }

    fn lowerListMatchExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        const scrutinee = try self.lowerExpr(module_idx, type_scope, env, match_expr.cond);
        const scrutinee_ty = self.program.store.getExpr(scrutinee).ty;
        const scrutinee_bind: ast.TypedSymbol = .{
            .ty = scrutinee_ty,
            .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
        };
        const scrutinee_decl = try self.program.store.addStmt(.{ .decl = .{
            .bind = scrutinee_bind,
            .body = scrutinee,
        } });
        const scrutinee_expr = try self.makeVarExpr(scrutinee_ty, scrutinee_bind.symbol);

        var current = try self.program.store.addExpr(.{
            .ty = result_ty,
            .data = .{ .runtime_error = try self.internStringLiteral("non-exhaustive match") },
        });

        const cir_env = self.ctx.env(module_idx);
        const branches = cir_env.store.matchBranchSlice(match_expr.branches);
        var idx = branches.len;
        while (idx > 0) {
            idx -= 1;
            const branch = cir_env.store.getMatchBranch(branches[idx]);
            const branch_pattern = cir_env.store.getMatchBranchPattern(cir_env.store.sliceMatchBranchPatterns(branch.patterns)[0]);
            current = try self.lowerListAwareMatchBranch(
                module_idx,
                type_scope,
                env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                branch_pattern.pattern,
                branch.value,
                current,
            );
        }

        return .{
            .stmts = try self.program.store.addStmtSpan(&.{scrutinee_decl}),
            .final_expr = current,
        };
    }

    fn lowerListAwareMatchBranch(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        pattern_idx: CIR.Pattern.Idx,
        branch_value: CIR.Expr.Idx,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .list => |list| try self.lowerExactListPatternBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                list,
                branch_value,
                else_expr,
            ),
            .assign, .as, .underscore, .record_destructure, .nominal, .nominal_external => try self.lowerWholeValuePatternBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                pattern_idx,
                branch_value,
            ),
            else => debugTodoPattern(pattern),
        };
    }

    fn lowerWholeValuePatternBranch(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        pattern_idx: CIR.Pattern.Idx,
        branch_value: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.ExprId {
        var branch_env = try self.cloneEnv(incoming_env);
        defer branch_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);

        try self.bindPatternFromSourceExpr(
            module_idx,
            type_scope,
            pattern_idx,
            scrutinee_expr,
            scrutinee_ty,
            &branch_env,
            &binding_decls,
        );

        return try self.wrapExprWithBindingDecls(
            try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                branch_env,
                branch_value,
                result_ty,
                expected_result_var,
            ),
            binding_decls.items,
        );
    }

    fn lowerExactListPatternBranch(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        list_pattern: @FieldType(CIR.Pattern, "list"),
        branch_value: CIR.Expr.Idx,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const elem_ty = self.requireListElemType(scrutinee_ty);
        const bool_ty = try self.makePrimitiveType(.bool);
        const u64_ty = try self.makePrimitiveType(.u64);
        const patterns = self.ctx.env(module_idx).store.slicePatterns(list_pattern.patterns);
        const prefix_len: usize = if (list_pattern.rest_info) |rest| @intCast(rest.index) else patterns.len;
        const suffix_len = patterns.len - prefix_len;

        const len_expr = try self.makeLowLevelExpr(u64_ty, .list_len, &.{scrutinee_expr});
        const expected_len_expr = try self.makeU64LiteralExpr(@intCast(patterns.len));
        const cond_expr = if (list_pattern.rest_info != null)
            try self.makeLowLevelExpr(bool_ty, .num_is_gte, &.{ len_expr, expected_len_expr })
        else
            try self.makeLowLevelExpr(bool_ty, .num_is_eq, &.{ len_expr, expected_len_expr });

        var branch_env = try self.cloneEnv(incoming_env);
        defer branch_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);

        for (patterns[0..prefix_len], 0..) |elem_pattern_idx, i| {
            const index_expr = try self.makeU64LiteralExpr(@intCast(i));
            const elem_expr = try self.makeLowLevelExpr(elem_ty, .list_get_unsafe, &.{ scrutinee_expr, index_expr });
            try self.bindPatternFromSourceExpr(
                module_idx,
                type_scope,
                elem_pattern_idx,
                elem_expr,
                elem_ty,
                &branch_env,
                &binding_decls,
            );
        }

        if (suffix_len != 0) {
            const suffix_start = try self.makeLowLevelExpr(
                u64_ty,
                .num_minus,
                &.{ len_expr, try self.makeU64LiteralExpr(@intCast(suffix_len)) },
            );
            for (patterns[prefix_len..], 0..) |elem_pattern_idx, i| {
                const index_expr = if (i == 0)
                    suffix_start
                else
                    try self.makeLowLevelExpr(
                        u64_ty,
                        .num_plus,
                        &.{ suffix_start, try self.makeU64LiteralExpr(@intCast(i)) },
                    );
                const elem_expr = try self.makeLowLevelExpr(elem_ty, .list_get_unsafe, &.{ scrutinee_expr, index_expr });
                try self.bindPatternFromSourceExpr(
                    module_idx,
                    type_scope,
                    elem_pattern_idx,
                    elem_expr,
                    elem_ty,
                    &branch_env,
                    &binding_decls,
                );
            }
        }

        if (list_pattern.rest_info) |rest| {
            if (rest.pattern) |rest_pattern_idx| {
                const start_expr = try self.makeU64LiteralExpr(rest.index);
                const rest_len_expr = try self.makeLowLevelExpr(u64_ty, .num_minus, &.{ len_expr, expected_len_expr });
                const bounds_expr = try self.makeListSliceBoundsExpr(start_expr, rest_len_expr);
                const rest_expr = try self.makeLowLevelExpr(scrutinee_ty, .list_sublist, &.{ scrutinee_expr, bounds_expr });
                try self.bindPatternFromSourceExpr(
                    module_idx,
                    type_scope,
                    rest_pattern_idx,
                    rest_expr,
                    scrutinee_ty,
                    &branch_env,
                    &binding_decls,
                );
            }
        }

        const then_expr = try self.wrapExprWithBindingDecls(
            try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                branch_env,
                branch_value,
                result_ty,
                expected_result_var,
            ),
            binding_decls.items,
        );
        return try self.program.store.addExpr(.{
            .ty = result_ty,
            .data = .{ .if_ = .{
                .cond = cond_expr,
                .then_body = then_expr,
                .else_body = else_expr,
            } },
        });
    }

    fn bindPatternFromSourceExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_expr: ast.ExprId,
        source_ty: type_mod.TypeId,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!void {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign| {
                const bind: ast.TypedSymbol = .{
                    .ty = source_ty,
                    .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
                };
                try decls.append(self.allocator, .{
                    .bind = bind,
                    .body = source_expr,
                });
                try env.put(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{ .typed = .{ .bind = bind } });
            },
            .as, .record_destructure, .tuple => {
                const source = try self.makePatternSourceBindWithType(module_idx, pattern_idx, source_ty);
                try decls.append(self.allocator, .{
                    .bind = source,
                    .body = source_expr,
                });
                try self.collectStructuralBindingDecls(module_idx, type_scope, source, pattern_idx, env, decls);
            },
            .underscore => {},
            .nominal => |nominal| try self.bindPatternFromSourceExpr(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                source_expr,
                source_ty,
                env,
                decls,
            ),
            .nominal_external => |nominal| try self.bindPatternFromSourceExpr(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                source_expr,
                source_ty,
                env,
                decls,
            ),
            else => debugTodoPattern(pattern),
        }
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
        var local_fn_group_indices = std.ArrayList(u32).empty;
        defer local_fn_group_indices.deinit(self.allocator);

        var i: usize = 0;
        while (i < cir_stmts.len) {
            if (try self.lowerLocalLambdaDeclGroup(module_idx, type_scope, &env, cir_stmts, i, &lowered)) |prepared| {
                try local_fn_group_indices.append(self.allocator, prepared.group_index);
                i = prepared.group_end;
                continue;
            }
            try self.lowerStmtInto(module_idx, type_scope, &env, cir_stmts[i], &lowered);
            i += 1;
        }

        const final_expr = try self.lowerExpr(module_idx, type_scope, env, final_expr_idx);
        try self.finalizeLocalLambdaGroups(&lowered, local_fn_group_indices.items);

        return .{
            .stmts = try self.program.store.addStmtSpan(lowered.items),
            .final_expr = final_expr,
        };
    }

    fn lowerBlockExprWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        stmts_span: CIR.Statement.Span,
        final_expr_idx: CIR.Expr.Idx,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        var env = try self.cloneEnv(incoming_env);
        defer env.deinit();

        const cir_env = self.ctx.env(module_idx);
        const cir_stmts = cir_env.store.sliceStatements(stmts_span);
        var lowered = std.ArrayList(ast.StmtId).empty;
        defer lowered.deinit(self.allocator);
        var local_fn_group_indices = std.ArrayList(u32).empty;
        defer local_fn_group_indices.deinit(self.allocator);

        var i: usize = 0;
        while (i < cir_stmts.len) {
            if (try self.lowerLocalLambdaDeclGroup(module_idx, type_scope, &env, cir_stmts, i, &lowered)) |prepared| {
                try local_fn_group_indices.append(self.allocator, prepared.group_index);
                i = prepared.group_end;
                continue;
            }
            try self.lowerStmtInto(module_idx, type_scope, &env, cir_stmts[i], &lowered);
            i += 1;
        }

        const final_expr = try self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            final_expr_idx,
            result_ty,
            expected_result_var,
        );
        try self.finalizeLocalLambdaGroups(&lowered, local_fn_group_indices.items);

        return .{
            .stmts = try self.program.store.addStmtSpan(lowered.items),
            .final_expr = final_expr,
        };
    }

    const PreparedLocalLambdaGroup = struct {
        group_index: u32,
        group_end: usize,
    };

    fn lowerLocalLambdaDeclGroup(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: *BindingEnv,
        cir_stmts: []const CIR.Statement.Idx,
        start_idx: usize,
        lowered: *std.ArrayList(ast.StmtId),
    ) std.mem.Allocator.Error!?PreparedLocalLambdaGroup {
        _ = type_scope;
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

        const group = try self.allocator.create(LocalFnGroupState);
        errdefer self.allocator.destroy(group);
        const sources = try self.allocator.alloc(LocalFnSource, end_idx - start_idx);
        errdefer self.allocator.free(sources);

        group.* = .{
            .module_idx = module_idx,
            .declaration_env = BindingEnv.init(self.allocator),
            .insertion_index = lowered.items.len,
            .sources = sources,
            .pending = .empty,
        };
        errdefer group.deinit(self.allocator);

        for (cir_stmts[start_idx..end_idx], 0..) |stmt_idx, group_i| {
            const decl = cir_env.store.getStatement(stmt_idx).s_decl;
            group.sources[group_i] = .{
                .pattern_idx = decl.pattern,
                .expr_idx = decl.expr,
                .source_symbol = try self.requirePatternSymbolOnly(module_idx, decl.pattern),
            };
        }

        const group_index: u32 = @intCast(self.local_fn_groups.items.len);
        try self.local_fn_groups.append(self.allocator, group);

        for (group.sources, 0..) |source, group_i| {
            try env.put(.{
                .module_idx = module_idx,
                .pattern_idx = @intFromEnum(source.pattern_idx),
            }, .{ .local_fn_source = .{
                .group_index = group_index,
                .source_index = @intCast(group_i),
            } });
        }

        group.declaration_env = try self.cloneEnv(env.*);
        return .{
            .group_index = group_index,
            .group_end = end_idx,
        };
    }

    fn finalizeLocalLambdaGroups(
        self: *Lowerer,
        lowered: *std.ArrayList(ast.StmtId),
        group_indices: []const u32,
    ) std.mem.Allocator.Error!void {
        var i = group_indices.len;
        while (i > 0) : (i -= 1) {
            const group = self.local_fn_groups.items[group_indices[i - 1]];
            if (group.pending.items.len == 0) continue;

            const stmt_ids = try self.allocator.alloc(ast.StmtId, group.pending.items.len);
            defer self.allocator.free(stmt_ids);
            for (group.pending.items, 0..) |pending, pending_i| {
                stmt_ids[pending_i] = pending.stmt_id orelse debugPanic(
                    "monotype invariant violated: local fn specialization missing emitted stmt",
                    .{},
                );
            }

            try lowered.insertSlice(self.allocator, group.insertion_index, stmt_ids);
        }
    }

    fn lowerStmtInto(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: *BindingEnv,
        stmt_idx: CIR.Statement.Idx,
        lowered: *std.ArrayList(ast.StmtId),
    ) std.mem.Allocator.Error!void {
        const cir_env = self.ctx.env(module_idx);
        const stmt = cir_env.store.getStatement(stmt_idx);

        switch (stmt) {
            .s_decl => |decl| {
                const body_solved_var = try self.scopedExprResultVar(module_idx, type_scope, env.*, decl.expr);
                const body_ty = try self.lowerInstantiatedType(module_idx, type_scope, body_solved_var);
                const lowered_body = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env.*,
                    decl.expr,
                    body_ty,
                    body_solved_var,
                );
                const lowered_body_ty = self.program.store.getExpr(lowered_body).ty;
                if (self.patternNeedsBindingDecls(module_idx, decl.pattern)) {
                    const root_bind = try self.makePatternSourceBindWithType(module_idx, decl.pattern, lowered_body_ty);
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                        .bind = root_bind,
                        .body = lowered_body,
                    } }));
                    var binding_decls = std.ArrayList(BindingDecl).empty;
                    defer binding_decls.deinit(self.allocator);
                    try self.collectStructuralBindingDeclsWithSolvedVar(module_idx, type_scope, root_bind, body_solved_var, decl.pattern, env, &binding_decls);
                    try self.appendBindingDeclStmts(lowered, binding_decls.items, false);
                    return;
                }

                try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, decl.pattern, lowered_body_ty, body_solved_var, env);
                const bind = try self.requirePatternBinderWithType(module_idx, decl.pattern, lowered_body_ty);
                try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                    .bind = bind,
                    .body = lowered_body,
                } }));
            },
            .s_var => |decl| {
                const body_solved_var = try self.scopedExprResultVar(module_idx, type_scope, env.*, decl.expr);
                const body_ty = try self.lowerInstantiatedType(module_idx, type_scope, body_solved_var);
                const lowered_body = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env.*,
                    decl.expr,
                    body_ty,
                    body_solved_var,
                );
                const lowered_body_ty = self.program.store.getExpr(lowered_body).ty;
                if (self.patternNeedsBindingDecls(module_idx, decl.pattern_idx)) {
                    const root_bind = try self.makePatternSourceBindWithType(module_idx, decl.pattern_idx, lowered_body_ty);
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .var_decl = .{
                        .bind = root_bind,
                        .body = lowered_body,
                    } }));
                    var binding_decls = std.ArrayList(BindingDecl).empty;
                    defer binding_decls.deinit(self.allocator);
                    try self.collectStructuralBindingDeclsWithSolvedVar(module_idx, type_scope, root_bind, body_solved_var, decl.pattern_idx, env, &binding_decls);
                    try self.appendBindingDeclStmts(lowered, binding_decls.items, true);
                    return;
                }

                try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, decl.pattern_idx, lowered_body_ty, body_solved_var, env);
                const bind = try self.requirePatternBinderWithType(module_idx, decl.pattern_idx, lowered_body_ty);
                try lowered.append(self.allocator, try self.program.store.addStmt(.{ .var_decl = .{
                    .bind = bind,
                    .body = lowered_body,
                } }));
            },
            .s_reassign => |reassign| {
                const body_ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(reassign.pattern_idx));
                const lowered_body = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env.*,
                    reassign.expr,
                    body_ty,
                    try self.scopedExprResultVar(module_idx, type_scope, env.*, reassign.expr),
                );
                if (self.patternNeedsBindingDecls(module_idx, reassign.pattern_idx)) {
                    const root_bind = try self.makePatternSourceBindWithType(module_idx, reassign.pattern_idx, body_ty);
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                        .bind = root_bind,
                        .body = lowered_body,
                    } }));
                    const root_expr = try self.makeVarExpr(root_bind.ty, root_bind.symbol);
                    try self.lowerPatternReassignFromExpr(
                        module_idx,
                        reassign.pattern_idx,
                        root_expr,
                        root_bind.ty,
                        env,
                        lowered,
                    );
                    return;
                }
                try lowered.append(self.allocator, try self.program.store.addStmt(.{ .reassign = .{
                    .target = try self.requirePatternSymbol(module_idx, env.*, reassign.pattern_idx),
                    .body = lowered_body,
                } }));
            },
            .s_expr => |expr_stmt| try lowered.append(self.allocator, try self.program.store.addStmt(.{ .expr = try self.lowerExpr(module_idx, type_scope, env.*, expr_stmt.expr) })),
            .s_dbg => |dbg| try lowered.append(self.allocator, try self.program.store.addStmt(.{
                .debug = try self.lowerDebugMessageExpr(module_idx, type_scope, env.*, dbg.expr),
            })),
            .s_expect => |expect| try lowered.append(self.allocator, try self.program.store.addStmt(.{ .expect = try self.lowerExpr(module_idx, type_scope, env.*, expect.body) })),
            .s_crash => |crash| try lowered.append(self.allocator, try self.program.store.addStmt(.{ .crash = try self.copySourceStringLiteral(module_idx, crash.msg) })),
            .s_return => |ret| try lowered.append(self.allocator, try self.program.store.addStmt(.{ .return_ = try self.lowerExpr(module_idx, type_scope, env.*, ret.expr) })),
            .s_break => |_| try lowered.append(self.allocator, try self.program.store.addStmt(.break_)),
            .s_for => |for_stmt| {
                var body_env = try self.cloneEnv(env.*);
                defer body_env.deinit();
                var binding_decls = std.ArrayList(BindingDecl).empty;
                defer binding_decls.deinit(self.allocator);
                const iterable_ty = try self.lowerExprType(
                    module_idx,
                    type_scope,
                    env.*,
                    for_stmt.expr,
                    cir_env.store.getExpr(for_stmt.expr),
                );
                const elem_ty = self.requireListElemType(iterable_ty);

                const patt = if (self.patternNeedsBindingDecls(module_idx, for_stmt.patt)) blk: {
                    const root_bind = try self.makePatternSourceBindWithType(module_idx, for_stmt.patt, elem_ty);
                    try self.collectStructuralBindingDecls(module_idx, type_scope, root_bind, for_stmt.patt, &body_env, &binding_decls);
                    break :blk try self.program.store.addPat(.{
                        .ty = root_bind.ty,
                        .data = .{ .var_ = root_bind.symbol },
                    });
                } else blk: {
                    try self.bindPatternEnvFromType(module_idx, for_stmt.patt, elem_ty, &body_env);
                    break :blk try self.lowerPatWithType(module_idx, for_stmt.patt, elem_ty);
                };

                const body = try self.wrapExprWithBindingDecls(
                    try self.lowerExpr(module_idx, type_scope, body_env, for_stmt.body),
                    binding_decls.items,
                );
                try lowered.append(self.allocator, try self.program.store.addStmt(.{ .for_ = .{
                    .patt = patt,
                    .iterable = try self.lowerExpr(module_idx, type_scope, env.*, for_stmt.expr),
                    .body = body,
                } }));
            },
            .s_while => |while_stmt| {
                var body_env = try self.cloneEnv(env.*);
                defer body_env.deinit();
                try lowered.append(self.allocator, try self.program.store.addStmt(.{ .while_ = .{
                    .cond = try self.lowerExpr(module_idx, type_scope, env.*, while_stmt.cond),
                    .body = try self.lowerExpr(module_idx, type_scope, body_env, while_stmt.body),
                } }));
            },
            else => debugTodoStmt(stmt),
        }
    }

    fn lowerIfExprWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        if_expr: anytype,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "if_") {
        const cir_env = self.ctx.env(module_idx);
        const branch_ids = cir_env.store.sliceIfBranches(if_expr.branches);
        if (branch_ids.len == 0) debugPanic("monotype invariant violated: if expression missing branches", .{});

        var else_body = try self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            if_expr.final_else,
            result_ty,
            expected_result_var,
        );
        var idx = branch_ids.len;
        while (idx > 1) {
            idx -= 1;
            const branch = cir_env.store.getIfBranch(branch_ids[idx]);
            else_body = try self.program.store.addExpr(.{
                .ty = result_ty,
                .data = .{ .if_ = .{
                    .cond = try self.lowerExpr(module_idx, type_scope, env, branch.cond),
                    .then_body = try self.lowerExprWithExpectedType(
                        module_idx,
                        type_scope,
                        env,
                        branch.body,
                        result_ty,
                        expected_result_var,
                    ),
                    .else_body = else_body,
                } },
            });
        }

        const branch = cir_env.store.getIfBranch(branch_ids[0]);
        return .{
            .cond = try self.lowerExpr(module_idx, type_scope, env, branch.cond),
            .then_body = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                branch.body,
                result_ty,
                expected_result_var,
            ),
            .else_body = else_body,
        };
    }

    fn lowerDebugMessageExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.ExprId {
        const value_expr = try self.lowerExpr(module_idx, type_scope, env, expr_idx);
        const str_ty = try self.makePrimitiveType(.str);
        return try self.program.store.addExpr(.{
            .ty = str_ty,
            .data = .{ .inspect = value_expr },
        });
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
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);
        const iterable_ty = try self.lowerExprType(
            module_idx,
            type_scope,
            incoming_env,
            iterable_expr_idx,
            self.ctx.env(module_idx).store.getExpr(iterable_expr_idx),
        );
        const elem_ty = self.requireListElemType(iterable_ty);

        const patt = if (self.patternNeedsBindingDecls(module_idx, patt_idx)) blk: {
            const root_bind = try self.makePatternSourceBindWithType(module_idx, patt_idx, elem_ty);
            try self.collectStructuralBindingDecls(module_idx, type_scope, root_bind, patt_idx, &body_env, &binding_decls);
            break :blk try self.program.store.addPat(.{
                .ty = root_bind.ty,
                .data = .{ .var_ = root_bind.symbol },
            });
        } else blk: {
            try self.bindPatternEnvFromType(module_idx, patt_idx, elem_ty, &body_env);
            break :blk try self.lowerPatWithType(module_idx, patt_idx, elem_ty);
        };

        return .{
            .patt = patt,
            .iterable = try self.lowerExpr(module_idx, type_scope, incoming_env, iterable_expr_idx),
            .body = try self.wrapExprWithBindingDecls(
                try self.lowerExpr(module_idx, type_scope, body_env, body_expr_idx),
                binding_decls.items,
            ),
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
                        .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
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

    fn lowerPatWithType(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.PatId {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign => |assign| try self.program.store.addPat(.{
                .ty = ty,
                .data = .{ .var_ = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident) },
            }),
            .as => |as_pat| try self.program.store.addPat(.{
                .ty = ty,
                .data = .{ .var_ = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident) },
            }),
            .underscore => try self.program.store.addPat(.{
                .ty = ty,
                .data = .{ .var_ = symbol_mod.Symbol.none },
            }),
            .nominal => |nominal| try self.lowerPatWithType(module_idx, nominal.backing_pattern, ty),
            .nominal_external => |nominal| try self.lowerPatWithType(module_idx, nominal.backing_pattern, ty),
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
        record_ty: type_mod.TypeId,
        span: CIR.RecordField.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const cir_env = self.ctx.env(module_idx);
        const fields = cir_env.store.sliceRecordFields(span);
        const out = try self.allocator.alloc(ast.FieldExpr, fields.len);
        defer self.allocator.free(out);

        for (fields, 0..) |field_idx, i| {
            const field = cir_env.store.getRecordField(field_idx);
            out[i] = .{
                .name = try self.ctx.copyExecutableIdent(module_idx, field.name),
                .value = try self.lowerExpr(module_idx, type_scope, env, field.value),
            };
        }

        switch (self.ctx.types.getType(record_ty)) {
            .record => {},
            else => debugPanic("monotype invariant violated: record expression lowered with non-record type", .{}),
        }
        std.mem.sort(ast.FieldExpr, out, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: ast.FieldExpr, b: ast.FieldExpr) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);

        return try self.program.store.addFieldExprSpan(out);
    }

    fn lowerRecordExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        record_ty: type_mod.TypeId,
        record: @FieldType(CIR.Expr, "e_record"),
    ) std.mem.Allocator.Error!ast.ExprId {
        if (record.ext) |base_expr_idx| {
            return self.lowerRecordUpdateExpr(
                module_idx,
                type_scope,
                env,
                record_ty,
                base_expr_idx,
                record.fields,
            );
        }

        return try self.program.store.addExpr(.{
            .ty = record_ty,
            .data = .{ .record = try self.lowerRecordFields(
                module_idx,
                type_scope,
                env,
                record_ty,
                record.fields,
            ) },
        });
    }

    fn lowerRecordUpdateExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        record_ty: type_mod.TypeId,
        base_expr_idx: CIR.Expr.Idx,
        update_fields_span: CIR.RecordField.Span,
    ) std.mem.Allocator.Error!ast.ExprId {
        const record = switch (self.ctx.types.getType(record_ty)) {
            .record => |record| record,
            else => debugPanic("monotype invariant violated: record update lowered with non-record type", .{}),
        };

        const base_symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE);
        const base_bind: ast.TypedSymbol = .{
            .ty = record_ty,
            .symbol = base_symbol,
        };
        const lowered_base = try self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            base_expr_idx,
            record_ty,
            null,
        );
        const base_decl = try self.program.store.addStmt(.{ .decl = .{
            .bind = base_bind,
            .body = lowered_base,
        } });
        const base_expr = try self.makeVarExpr(record_ty, base_symbol);

        const cir_env = self.ctx.env(module_idx);
        const update_field_ids = cir_env.store.sliceRecordFields(update_fields_span);
        const lowered_updates = try self.allocator.alloc(ast.FieldExpr, update_field_ids.len);
        defer self.allocator.free(lowered_updates);
        for (update_field_ids, 0..) |field_idx, i| {
            const field = cir_env.store.getRecordField(field_idx);
            lowered_updates[i] = .{
                .name = try self.ctx.copyExecutableIdent(module_idx, field.name),
                .value = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    field.value,
                    try self.lookupRecordFieldType(module_idx, record_ty, field.name),
                    null,
                ),
            };
        }

        const final_fields = self.ctx.types.sliceFields(record.fields);
        const lowered_fields = try self.allocator.alloc(ast.FieldExpr, final_fields.len);
        defer self.allocator.free(lowered_fields);
        for (final_fields, 0..) |field, i| {
            const value = blk: {
                for (lowered_updates) |updated| {
                    if (updated.name == field.name) break :blk updated.value;
                }
                break :blk try self.program.store.addExpr(.{
                    .ty = field.ty,
                    .data = .{ .access = .{
                        .record = base_expr,
                        .field = field.name,
                    } },
                });
            };
            lowered_fields[i] = .{
                .name = field.name,
                .value = value,
            };
        }

        const rebuilt_record = try self.program.store.addExpr(.{
            .ty = record_ty,
            .data = .{ .record = try self.program.store.addFieldExprSpan(lowered_fields) },
        });
        return try self.program.store.addExpr(.{
            .ty = record_ty,
            .data = .{ .block = .{
                .stmts = try self.program.store.addStmtSpan(&.{base_decl}),
                .final_expr = rebuilt_record,
            } },
        });
    }

    const RecordedMethodArgs = struct {
        implicit_receiver: ?CIR.Expr.Idx,
        explicit_args: []const CIR.Expr.Idx,
    };

    const RecordedMethodFact = struct {
        target: ResolvedTarget,
        fn_ty: type_mod.TypeId,
        solved_var: Var,
    };

    fn getRecordedMethodArgs(
        self: *const Lowerer,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
    ) RecordedMethodArgs {
        const cir_env = self.ctx.env(module_idx);
        return switch (cir_env.store.getExpr(expr_idx)) {
            .e_dot_access => |dot| blk: {
                if (dot.args == null or !dot.field_name.eql(method_name)) {
                    debugPanic("monotype static dispatch invariant violated: unexpected dot-access dispatch shape", .{});
                }
                break :blk .{
                    .implicit_receiver = dot.receiver,
                    .explicit_args = cir_env.store.sliceExpr(dot.args.?),
                };
            },
            .e_type_var_dispatch => |dispatch| blk: {
                if (!dispatch.method_name.eql(method_name)) {
                    debugPanic("monotype static dispatch invariant violated: unexpected type-var dispatch shape", .{});
                }
                break :blk .{
                    .implicit_receiver = null,
                    .explicit_args = cir_env.store.sliceExpr(dispatch.args),
                };
            },
            .e_call => |call| switch (cir_env.store.getExpr(call.func)) {
                .e_dot_access => |dot| blk: {
                    if (!dot.field_name.eql(method_name)) {
                        debugPanic("monotype static dispatch invariant violated: mismatched dot-access method name", .{});
                    }
                    break :blk .{
                        .implicit_receiver = dot.receiver,
                        .explicit_args = cir_env.store.sliceExpr(call.args),
                    };
                },
                .e_type_var_dispatch => |dispatch| blk: {
                    if (!dispatch.method_name.eql(method_name)) {
                        debugPanic("monotype static dispatch invariant violated: mismatched type-var dispatch method name", .{});
                    }
                    break :blk .{
                        .implicit_receiver = null,
                        .explicit_args = cir_env.store.sliceExpr(call.args),
                    };
                },
                else => debugPanic("monotype static dispatch invariant violated: call head is not recorded dispatch", .{}),
            },
            else => debugPanic("monotype static dispatch invariant violated: expr is not recorded dispatch", .{}),
        };
    }

    fn lowerRecordedMethodFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
        expected_result_var: ?Var,
    ) std.mem.Allocator.Error!RecordedMethodFact {
        const module_env = @constCast(self.ctx.env(module_idx));
        const args = self.getRecordedMethodArgs(module_idx, expr_idx, method_name);
        var call_scope: TypeCloneScope = undefined;
        call_scope.initCloneAll(self.allocator, module_env);
        defer call_scope.deinit();

        const target = self.lookupResolvedDispatchTarget(module_idx, expr_idx) orelse
            self.resolveMonomorphicDispatchTarget(module_idx, type_scope, env, expr_idx, method_name);
        const source_fn_var = try self.copyTopLevelDefExprVarToModule(target.module_idx, target.def_idx, module_idx);
        const cloned_func_var = try call_scope.instantiator.instantiateVar(source_fn_var);
        const cloned_ret_var = self.lookupFunctionRetVar(module_idx, cloned_func_var) orelse debugPanic(
            "monotype static dispatch invariant violated: method target missing return var in module {d}",
            .{module_idx},
        );

        var arg_index: usize = 0;
        if (args.implicit_receiver) |receiver_expr_idx| {
            const source_arg_var = try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, receiver_expr_idx);
            const cloned_arg_var = try call_scope.instantiator.instantiateVar(source_arg_var);
            const expected_arg_var = self.lookupFunctionArgVar(module_idx, cloned_func_var, arg_index) orelse debugPanic(
                "monotype static dispatch invariant violated: missing receiver argument on method target",
                .{},
            );
            try self.unifyClonedCheckerVars(module_idx, expected_arg_var, cloned_arg_var);
            arg_index += 1;
        }
        for (args.explicit_args, 0..) |arg_expr_idx, i| {
            const source_arg_var = try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, arg_expr_idx);
            const cloned_arg_var = try call_scope.instantiator.instantiateVar(source_arg_var);
            const expected_arg_var = self.lookupFunctionArgVar(module_idx, cloned_func_var, arg_index) orelse debugPanic(
                "monotype static dispatch invariant violated: method target missing argument {d}",
                .{i},
            );
            try self.unifyClonedCheckerVars(module_idx, expected_arg_var, cloned_arg_var);
            arg_index += 1;
        }

        if (expected_result_var) |result_var| {
            try self.unifyClonedCheckerVars(module_idx, cloned_ret_var, result_var);
        }

        return .{
            .target = target,
            .fn_ty = try self.lowerInstantiatedType(module_idx, &call_scope, cloned_func_var),
            .solved_var = cloned_func_var,
        };
    }

    fn lowerRecordedMethodCall(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
        expected_result_var: ?Var,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "call") {
        const args = self.getRecordedMethodArgs(module_idx, expr_idx, method_name);
        const fact = try self.lowerRecordedMethodFact(
            module_idx,
            type_scope,
            env,
            expr_idx,
            method_name,
            expected_result_var,
        );
        const implicit_arg_len: usize = if (args.implicit_receiver == null) 0 else 1;
        const total_args_len = implicit_arg_len + args.explicit_args.len;
        const lowered_args = try self.allocator.alloc(ast.ExprId, total_args_len);
        defer self.allocator.free(lowered_args);

        const callee = try self.lowerResolvedTargetCallee(fact.target, fact.fn_ty);

        var current_fn_ty = fact.fn_ty;
        var arg_index: usize = 0;
        if (args.implicit_receiver) |receiver_expr_idx| {
            const fn_parts = self.requireFunctionType(current_fn_ty);
            lowered_args[arg_index] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                receiver_expr_idx,
                fn_parts.arg,
                self.lookupFunctionArgVar(module_idx, fact.solved_var, arg_index),
            );
            current_fn_ty = fn_parts.ret;
            arg_index += 1;
        }
        for (args.explicit_args, 0..) |arg_expr_idx, i| {
            const fn_parts = self.requireFunctionType(current_fn_ty);
            lowered_args[arg_index + i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                arg_expr_idx,
                fn_parts.arg,
                self.lookupFunctionArgVar(module_idx, fact.solved_var, arg_index + i),
            );
            current_fn_ty = fn_parts.ret;
        }

        return try self.buildCurriedCallFromLowered(callee, lowered_args);
    }

    fn lowerCurriedCall(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        call_expr_idx: CIR.Expr.Idx,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        expected_result_var: ?Var,
    ) std.mem.Allocator.Error!struct {
        data: @FieldType(ast.Expr.Data, "call"),
        result_ty: type_mod.TypeId,
    } {
        const expected = try self.lowerCallExpectedFnType(
            module_idx,
            type_scope,
            env,
            call_expr_idx,
            func_expr_idx,
            arg_exprs,
            expected_result_var,
        );

        const callee = try self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            func_expr_idx,
            expected.ty,
            expected.solved_var,
        );

        const lowered_args = try self.allocator.alloc(ast.ExprId, arg_exprs.len);
        defer self.allocator.free(lowered_args);
        var current_fn_ty = expected.ty;
        const current_fn_var = expected.solved_var;
        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const fn_parts = self.requireFunctionType(current_fn_ty);
            const arg_expected_var = if (current_fn_var) |var_|
                self.lookupFunctionArgVar(module_idx, var_, i)
            else
                null;
            lowered_args[i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                arg_expr_idx,
                fn_parts.arg,
                arg_expected_var,
            );
            current_fn_ty = fn_parts.ret;
        }

        return .{
            .data = try self.buildCurriedCallFromLowered(callee, lowered_args),
            .result_ty = current_fn_ty,
        };
    }

    fn lowerCallExpectedFnType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        call_expr_idx: CIR.Expr.Idx,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        expected_result_var: ?Var,
    ) std.mem.Allocator.Error!ExpectedTypeFact {
        const cir_env = self.ctx.env(module_idx);
        return if (self.calleeNeedsSourceFnSpecialization(module_idx, env, func_expr_idx))
            try self.lowerExpectedCallFactFromSolvedFacts(
                module_idx,
                type_scope,
                env,
                call_expr_idx,
                func_expr_idx,
                arg_exprs,
                expected_result_var,
            )
        else
            .{
                .ty = try self.lowerExprType(
                    module_idx,
                    type_scope,
                    env,
                    func_expr_idx,
                    cir_env.store.getExpr(func_expr_idx),
                ),
                .solved_var = null,
            };
    }

    fn calleeNeedsSourceFnSpecialization(
        self: *Lowerer,
        module_idx: u32,
        env: BindingEnv,
        func_expr_idx: CIR.Expr.Idx,
    ) bool {
        const cir_env = self.ctx.env(module_idx);
        return switch (cir_env.store.getExpr(func_expr_idx)) {
            .e_lookup_local => |lookup| blk: {
                if (env.get(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(lookup.pattern_idx),
                })) |entry| switch (entry) {
                    .typed => break :blk false,
                    .local_fn_source => break :blk true,
                };
                const symbol = self.lookupTopLevelSymbol(module_idx, lookup.pattern_idx) orelse break :blk false;
                break :blk self.isTopLevelFunction(symbol);
            },
            .e_lookup_external => |lookup| blk: {
                const current_env = self.ctx.env(module_idx);
                const target_module_idx = current_env.imports.getResolvedModule(lookup.module_idx) orelse break :blk false;
                const symbol = self.lookupTopLevelDefSymbol(target_module_idx, @enumFromInt(lookup.target_node_idx)) orelse break :blk false;
                break :blk self.isTopLevelFunction(symbol);
            },
            else => false,
        };
    }

    fn lowerExpectedCallFactFromSolvedFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        env: BindingEnv,
        call_expr_idx: CIR.Expr.Idx,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        expected_result_var: ?Var,
    ) std.mem.Allocator.Error!ExpectedTypeFact {
        const module_env = @constCast(self.ctx.env(module_idx));
        var call_scope: TypeCloneScope = undefined;
        call_scope.initCloneAll(self.allocator, module_env);
        defer call_scope.deinit();

        const source_func_var = if (try self.synthesizeLocalFnCallVar(
            module_idx,
            type_scope,
            env,
            call_expr_idx,
            func_expr_idx,
            arg_exprs,
            expected_result_var,
        )) |synthetic|
            synthetic
        else
            try self.lookupSourceFunctionVarForSpecialization(
                module_idx,
                env,
                call_expr_idx,
                func_expr_idx,
            );

        const cloned_func_var = try call_scope.instantiator.instantiateVar(source_func_var);
        const cloned_ret_var = self.lookupFunctionRetVar(module_idx, cloned_func_var) orelse debugPanic(
            "monotype invariant violated: specialized callee missing return var in module {d}",
            .{module_idx},
        );
        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const source_arg_var = try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, arg_expr_idx);
            const cloned_arg_var = try call_scope.instantiator.instantiateVar(source_arg_var);
            const expected_arg_var = self.lookupFunctionArgVar(module_idx, cloned_func_var, i) orelse debugPanic(
                "monotype invariant violated: specialized callee missing argument {d} in module {d}",
                .{ i, module_idx },
            );
            try self.unifyClonedCheckerVars(module_idx, expected_arg_var, cloned_arg_var);
        }
        if (expected_result_var) |result_var| {
            try self.unifyClonedCheckerVars(module_idx, cloned_ret_var, result_var);
        } else {
            const cloned_call_result_var = try call_scope.instantiator.instantiateVar(ModuleEnv.varFrom(call_expr_idx));
            try self.unifyClonedCheckerVars(module_idx, cloned_ret_var, cloned_call_result_var);
        }

        return .{
            .ty = try self.lowerInstantiatedType(module_idx, &call_scope, cloned_func_var),
            .solved_var = cloned_func_var,
        };
    }

    fn lookupSourceFunctionVarForSpecialization(
        self: *Lowerer,
        current_module_idx: u32,
        env: BindingEnv,
        call_expr_idx: CIR.Expr.Idx,
        func_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!Var {
        const current_env = self.ctx.env(current_module_idx);
        if (current_env.types.findResolvedStaticDispatchSite(ModuleEnv.varFrom(call_expr_idx))) |resolved_site| {
            return try self.resolveRecordedDispatchSourceFnVar(current_module_idx, resolved_site);
        }
        if (current_env.types.findResolvedStaticDispatchSite(ModuleEnv.varFrom(func_expr_idx))) |resolved_site| {
            return try self.resolveRecordedDispatchSourceFnVar(current_module_idx, resolved_site);
        }
        const func_expr = current_env.store.getExpr(func_expr_idx);

        return switch (func_expr) {
            .e_lookup_local => |lookup| blk: {
                if (env.get(.{
                    .module_idx = current_module_idx,
                    .pattern_idx = @intFromEnum(lookup.pattern_idx),
                })) |entry| switch (entry) {
                    .typed => |typed| {
                        if (typed.solved_var) |solved_var| break :blk solved_var;
                    },
                    .local_fn_source => |source| {
                        const group = self.local_fn_groups.items[source.group_index];
                        break :blk ModuleEnv.varFrom(group.sources[source.source_index].pattern_idx);
                    },
                };

                const symbol = self.lookupTopLevelSymbol(current_module_idx, lookup.pattern_idx) orelse debugPanic(
                    "monotype invariant violated: missing top-level symbol for function lookup in module {d}",
                    .{current_module_idx},
                );
                const top_level = self.top_level_defs_by_symbol.get(symbol) orelse debugPanic(
                    "monotype invariant violated: missing top-level def facts for symbol {d}",
                    .{symbol.raw()},
                );
                const def = self.ctx.env(top_level.module_idx).store.getDef(top_level.def_idx);
                break :blk ModuleEnv.varFrom(def.pattern);
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = current_env.imports.getResolvedModule(lookup.module_idx) orelse debugPanic(
                    "monotype invariant violated: unresolved external function import {d}",
                    .{@intFromEnum(lookup.module_idx)},
                );
                break :blk try self.copyTopLevelDefExprVarToModule(
                    target_module_idx,
                    @enumFromInt(lookup.target_node_idx),
                    current_module_idx,
                );
            },
            .e_dot_access => |dot| blk: {
                if (dot.args == null) break :blk ModuleEnv.varFrom(func_expr_idx);
                const resolved_site = current_env.types.findResolvedStaticDispatchSite(ModuleEnv.varFrom(func_expr_idx)) orelse debugPanic(
                    "monotype static dispatch invariant violated: missing resolved site for expr {d} in module {d}",
                    .{ @intFromEnum(func_expr_idx), current_module_idx },
                );
                break :blk try self.resolveRecordedDispatchSourceFnVar(current_module_idx, resolved_site);
            },
            .e_type_var_dispatch => |_| blk: {
                const resolved_site = current_env.types.findResolvedStaticDispatchSite(ModuleEnv.varFrom(func_expr_idx)) orelse debugPanic(
                    "monotype static dispatch invariant violated: missing resolved site for expr {d} in module {d}",
                    .{ @intFromEnum(func_expr_idx), current_module_idx },
                );
                break :blk try self.resolveRecordedDispatchSourceFnVar(current_module_idx, resolved_site);
            },
            else => ModuleEnv.varFrom(func_expr_idx),
        };
    }

    fn resolveRecordedDispatchSourceFnVar(
        self: *Lowerer,
        current_module_idx: u32,
        resolved_site: types.ResolvedStaticDispatchSite,
    ) std.mem.Allocator.Error!Var {
        const current_env = self.ctx.env(current_module_idx);
        const target_module_idx = findModuleIdxByName(self.ctx.all_module_envs, current_env.getIdent(resolved_site.target_module_name));
        return try self.copyTopLevelDefExprVarToModule(
            target_module_idx,
            @enumFromInt(resolved_site.target_def_idx),
            current_module_idx,
        );
    }

    fn synthesizeLocalFnCallVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        env: BindingEnv,
        _: CIR.Expr.Idx,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        expected_result_var: ?Var,
    ) std.mem.Allocator.Error!?Var {
        const func_expr = self.ctx.env(module_idx).store.getExpr(func_expr_idx);
        const lookup = switch (func_expr) {
            .e_lookup_local => |local| local,
            else => return null,
        };

        const entry = env.get(.{
            .module_idx = module_idx,
            .pattern_idx = @intFromEnum(lookup.pattern_idx),
        }) orelse return null;
        if (entry != .local_fn_source) return null;

        const module_env = @constCast(self.ctx.env(module_idx));
        const arg_vars = try self.allocator.alloc(Var, arg_exprs.len);
        defer self.allocator.free(arg_vars);
        for (arg_exprs, 0..) |arg_expr_idx, i| {
            arg_vars[i] = try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, arg_expr_idx);
        }
        const args_range = try module_env.types.appendVars(arg_vars);
        const ret_var = if (expected_result_var) |result_var|
            result_var
        else
            try module_env.types.fresh();

        return try module_env.types.freshFromContent(.{ .structure = .{ .fn_unbound = types.Func{
            .args = args_range,
            .ret = ret_var,
            .needs_instantiation = false,
        } } });
    }

    fn copyTopLevelDefExprVarToModule(
        self: *Lowerer,
        source_module_idx: u32,
        def_idx: CIR.Def.Idx,
        dest_module_idx: u32,
    ) std.mem.Allocator.Error!Var {
        const def = self.ctx.env(source_module_idx).store.getDef(def_idx);
        const source_var = ModuleEnv.varFrom(def.pattern);
        return if (source_module_idx == dest_module_idx)
            source_var
        else
            try self.copyCheckerVarToModule(source_module_idx, dest_module_idx, source_var);
    }

    fn lookupCallArgSpecializationVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!Var {
        _ = type_scope;
        if (self.lookupSpecializedExprVar(module_idx, env, expr_idx)) |specialized| {
            return specialized;
        }
        return ModuleEnv.varFrom(expr_idx);
    }

    fn lowerDirectCallCallee(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        func_expr_idx: CIR.Expr.Idx,
        expected_fn_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!?ast.ExprId {
        const func_expr = self.ctx.env(module_idx).store.getExpr(func_expr_idx);

        switch (func_expr) {
            .e_lookup_local => |lookup| {
                if (env.get(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(lookup.pattern_idx),
                })) |entry| switch (entry) {
                    .typed => return null,
                    .local_fn_source => {},
                };

                const symbol = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, expected_fn_ty, null);
                return try self.program.store.addExpr(.{
                    .ty = expected_fn_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_lookup_external => |lookup| {
                const symbol = try self.lookupOrSpecializeExternal(module_idx, lookup, expected_fn_ty, null);
                return try self.program.store.addExpr(.{
                    .ty = expected_fn_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            else => return null,
        }
    }

    fn lowerExprWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!ast.ExprId {
        const cir_env = self.ctx.env(module_idx);
        const expr = cir_env.store.getExpr(expr_idx);

        if (self.exprVarIsErroneous(module_idx, expr_idx)) {
            return self.lowerErroneousExprWithType(module_idx, type_scope, env, expr_idx, expected_ty);
        }

        return switch (expr) {
            .e_nominal => |nominal| self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                nominal.backing_expr,
                expected_ty,
            ),
            .e_nominal_external => |nominal| self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                nominal.backing_expr,
                expected_ty,
            ),
            .e_num => |num| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = try self.lowerNumericIntLiteralData(expected_ty, num.value),
            }),
            .e_dec => |dec| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = try self.lowerNumericDecLiteralData(expected_ty, dec.value.toI128()),
            }),
            .e_dec_small => |dec| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = try self.lowerNumericSmallDecLiteralData(expected_ty, dec.value),
            }),
            .e_typed_int => |num| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = try self.lowerNumericIntLiteralData(expected_ty, num.value),
            }),
            .e_typed_frac => |frac| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = try self.lowerNumericDecLiteralData(expected_ty, @bitCast(frac.value.bytes)),
            }),
            .e_call => |call| blk: {
                if (try self.maybeLowerSpecialCallExpr(module_idx, type_scope, env, expr_idx, call, expected_ty)) |special| {
                    break :blk try self.program.store.addExpr(.{
                        .ty = expected_ty,
                        .data = self.program.store.getExpr(special).data,
                    });
                }
                const lowered_call = try self.lowerCurriedCall(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    call.func,
                    cir_env.store.sliceExpr(call.args),
                    expected_var,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = expected_ty,
                    .data = .{ .call = lowered_call.data },
                });
            },
            .e_lambda => |lambda| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .clos = try self.lowerAnonymousClosure(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    expected_ty,
                    lambda.args,
                    lambda.body,
                    expected_var,
                ) },
            }),
            .e_closure => |closure| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .clos = try self.lowerClosureExpr(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    expected_ty,
                    closure,
                    expected_var,
                ) },
            }),
            .e_hosted_lambda => debugTodo("monotype.lowerExprWithExpectedType hosted lambda"),
            .e_run_low_level => |ll| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .low_level = .{
                    .op = ll.op,
                    .args = try self.lowerExprList(module_idx, type_scope, env, ll.args),
                } },
            }),
            .e_record => |record| self.lowerRecordExpr(module_idx, type_scope, env, expected_ty, record),
            .e_tuple => |tuple| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .tuple = try self.lowerTupleExprsWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    expected_ty,
                    tuple.elems,
                ) },
            }),
            .e_list => |list| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .list = try self.lowerListExprsWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    expected_ty,
                    list.elems,
                ) },
            }),
            .e_if => |if_expr| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .if_ = try self.lowerIfExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    expected_ty,
                    expected_var,
                    if_expr,
                ) },
            }),
            .e_match => |match_expr| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = try self.lowerMatchExprData(
                    module_idx,
                    type_scope,
                    env,
                    expected_ty,
                    expected_var,
                    match_expr,
                ),
            }),
            .e_block => |block| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .block = try self.lowerBlockExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    block.stmts,
                    block.final_expr,
                    expected_ty,
                    expected_var,
                ) },
            }),
            .e_tag => |tag| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = try self.lowerTagExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    expected_ty,
                    tag.name,
                    tag.args,
                ),
            }),
            .e_zero_argument_tag => |tag| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = try self.lowerTagExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    expected_ty,
                    tag.name,
                    .{ .span = .{ .start = 0, .len = 0 } },
                ),
            }),
            .e_lookup_local => |lookup| blk: {
                if (self.ctx.types.getType(expected_ty) != .func) {
                    break :blk try self.lowerExpr(module_idx, type_scope, env, expr_idx);
                }
                const symbol = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, expected_ty, expected_var);
                break :blk try self.program.store.addExpr(.{
                    .ty = expected_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_lookup_external => |lookup| blk: {
                if (self.ctx.types.getType(expected_ty) != .func) {
                    break :blk try self.lowerExpr(module_idx, type_scope, env, expr_idx);
                }
                const symbol = try self.lookupOrSpecializeExternal(module_idx, lookup, expected_ty, expected_var);
                break :blk try self.program.store.addExpr(.{
                    .ty = expected_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_dot_access => |dot| blk: {
                if (dot.args == null) {
                    break :blk try self.lowerExpr(module_idx, type_scope, env, expr_idx);
                }
                break :blk try self.program.store.addExpr(.{
                    .ty = expected_ty,
                    .data = .{ .call = try self.lowerRecordedMethodCall(
                        module_idx,
                        type_scope,
                        env,
                        expr_idx,
                        dot.field_name,
                        expected_var,
                    ) },
                });
            },
            .e_type_var_dispatch => |dispatch| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    dispatch.method_name,
                    expected_var,
                ) },
            }),
            else => try self.lowerExpr(module_idx, type_scope, env, expr_idx),
        };
    }

    fn lowerTransparentNominalExprWithType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        backing_expr_idx: CIR.Expr.Idx,
        nominal_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        return self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            backing_expr_idx,
            nominal_ty,
            ModuleEnv.varFrom(expr_idx),
        );
    }

    fn maybeLowerRecordedDispatchCallExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        call_expr_idx: CIR.Expr.Idx,
        call: anytype,
    ) std.mem.Allocator.Error!?ast.ExprId {
        const cir_env = self.ctx.env(module_idx);
        return switch (cir_env.store.getExpr(call.func)) {
            .e_dot_access => |dot| if (dot.args != null) try self.program.store.addExpr(.{
                .ty = try self.lowerExprType(module_idx, type_scope, env, call_expr_idx, .{ .e_call = call }),
                .data = .{ .call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    call_expr_idx,
                    dot.field_name,
                    null,
                ) },
            }) else null,
            .e_type_var_dispatch => |dispatch| try self.program.store.addExpr(.{
                .ty = try self.lowerExprType(module_idx, type_scope, env, call_expr_idx, .{ .e_call = call }),
                .data = .{ .call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    call_expr_idx,
                    dispatch.method_name,
                    null,
                ) },
            }),
            else => null,
        };
    }

    fn maybeLowerSpecialCallExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        call: anytype,
        result_ty_opt: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!?ast.ExprId {
        if (self.resolveDirectTopLevelSource(module_idx, env, call.func)) |source| {
            if (self.isBuiltinStrInspectSource(source.module_idx, source.def_idx)) {
                const result_ty = result_ty_opt orelse try self.lowerExprType(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    self.ctx.env(module_idx).store.getExpr(expr_idx),
                );
                if (try self.maybeLowerBuiltinSpecialCall(module_idx, type_scope, env, call, result_ty)) |special| {
                    return special;
                }
            }
        }

        return try self.maybeLowerRecordedDispatchCallExpr(module_idx, type_scope, env, expr_idx, call);
    }

    fn lowerListExprsWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        list_ty: type_mod.TypeId,
        span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const elem_ty = self.requireListElemType(list_ty);
        return try self.lowerExprSliceWithExpectedType(
            module_idx,
            type_scope,
            env,
            self.ctx.env(module_idx).store.sliceExpr(span),
            elem_ty,
        );
    }

    fn lowerTupleExprsWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        tuple_ty: type_mod.TypeId,
        span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const elem_tys = switch (self.ctx.types.getType(tuple_ty)) {
            .tuple => |tuple| self.ctx.types.sliceTypeSpan(tuple),
            else => debugPanic("monotype invariant violated: expected tuple type for tuple literal lowering", .{}),
        };
        const exprs = self.ctx.env(module_idx).store.sliceExpr(span);
        if (exprs.len != elem_tys.len) {
            return debugPanic("monotype invariant violated: tuple literal arity mismatch during expected-type lowering", .{});
        }

        const out = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(out);
        for (exprs, elem_tys, 0..) |expr_idx, elem_ty, i| {
            out[i] = try self.lowerExprWithExpectedType(module_idx, type_scope, env, expr_idx, elem_ty, null);
        }
        return try self.program.store.addExprSpan(out);
    }

    fn lowerExprSliceWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        exprs: []const CIR.Expr.Idx,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const out = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(out);
        for (exprs, 0..) |expr_idx, i| {
            out[i] = try self.lowerExprWithExpectedType(module_idx, type_scope, env, expr_idx, expected_ty, null);
        }
        return try self.program.store.addExprSpan(out);
    }

    fn lowerRecordFieldsWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        record_ty: type_mod.TypeId,
        span: CIR.RecordField.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const cir_env = self.ctx.env(module_idx);
        const fields = cir_env.store.sliceRecordFields(span);
        const out = try self.allocator.alloc(ast.FieldExpr, fields.len);
        defer self.allocator.free(out);

        for (fields, 0..) |field_idx, i| {
            const field = cir_env.store.getRecordField(field_idx);
            out[i] = .{
                .name = try self.ctx.copyExecutableIdent(module_idx, field.name),
                .value = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    field.value,
                    try self.lookupRecordFieldType(module_idx, record_ty, field.name),
                    null,
                ),
            };
        }

        std.mem.sort(ast.FieldExpr, out, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: ast.FieldExpr, b: ast.FieldExpr) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);

        return try self.program.store.addFieldExprSpan(out);
    }

    fn lowerTagExprWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expected_ty: type_mod.TypeId,
        tag_name: base.Ident.Idx,
        args_span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        if (self.ctx.types.getType(expected_ty) == .primitive and self.ctx.types.getType(expected_ty).primitive == .bool) {
            if (self.ctx.env(module_idx).store.sliceExpr(args_span).len != 0) {
                return debugPanic("monotype bool tag invariant violated: Bool tags cannot carry arguments", .{});
            }
            return .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag_name) };
        }

        const arg_tys = try self.lookupTagArgTypesOwned(module_idx, expected_ty, tag_name);
        defer self.allocator.free(arg_tys);
        const arg_exprs = self.ctx.env(module_idx).store.sliceExpr(args_span);
        if (arg_exprs.len != arg_tys.len) {
            return debugPanic("monotype invariant violated: tag constructor arity mismatch during expected-type lowering", .{});
        }
        const lowered_args = try self.allocator.alloc(ast.ExprId, arg_exprs.len);
        defer self.allocator.free(lowered_args);
        for (arg_exprs, arg_tys, 0..) |arg_expr_idx, arg_ty, i| {
            lowered_args[i] = try self.lowerExprWithExpectedType(module_idx, type_scope, env, arg_expr_idx, arg_ty, null);
        }
        return .{ .tag = .{
            .name = try self.ctx.copyExecutableIdent(module_idx, tag_name),
            .args = try self.program.store.addExprSpan(lowered_args),
        } };
    }

    fn exprVarIsErroneous(self: *Lowerer, module_idx: u32, expr_idx: CIR.Expr.Idx) bool {
        return self.ctx.env(module_idx).types.resolveVar(ModuleEnv.varFrom(expr_idx)).desc.content == .err;
    }

    fn lowerErroneousExprWithType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.ctx.env(module_idx).store.getExpr(expr_idx);
        return switch (expr) {
            .e_block => |block| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .block = try self.lowerErroneousBlockExpr(module_idx, type_scope, env, block.stmts, expected_ty) },
            }),
            .e_crash => |crash| try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .runtime_error = try self.copySourceStringLiteral(module_idx, crash.msg) },
            }),
            else => try self.program.store.addExpr(.{
                .ty = expected_ty,
                .data = .{ .runtime_error = try self.internStringLiteral("runtime error") },
            }),
        };
    }

    fn lowerErroneousBlockExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        stmts_span: CIR.Statement.Span,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        var env = try self.cloneEnv(incoming_env);
        defer env.deinit();

        const cir_env = self.ctx.env(module_idx);
        const cir_stmts = cir_env.store.sliceStatements(stmts_span);
        var lowered = std.ArrayList(ast.StmtId).empty;
        defer lowered.deinit(self.allocator);
        var local_fn_group_indices = std.ArrayList(u32).empty;
        defer local_fn_group_indices.deinit(self.allocator);

        var i: usize = 0;
        while (i < cir_stmts.len) {
            if (try self.lowerLocalLambdaDeclGroup(module_idx, type_scope, &env, cir_stmts, i, &lowered)) |prepared| {
                try local_fn_group_indices.append(self.allocator, prepared.group_index);
                i = prepared.group_end;
                continue;
            }
            try self.lowerStmtInto(module_idx, type_scope, &env, cir_stmts[i], &lowered);
            i += 1;
        }

        try self.finalizeLocalLambdaGroups(&lowered, local_fn_group_indices.items);

        return .{
            .stmts = try self.program.store.addStmtSpan(lowered.items),
            .final_expr = try self.program.store.addExpr(.{
                .ty = result_ty,
                .data = .{ .runtime_error = try self.internStringLiteral("runtime error") },
            }),
        };
    }

    fn buildCurriedCallFromLowered(
        self: *Lowerer,
        callee: ast.ExprId,
        arg_exprs: []const ast.ExprId,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "call") {
        var current = callee;
        if (arg_exprs.len == 0) {
            const unit_expr = try self.program.store.addExpr(.{
                .ty = try self.ctx.types.addType(.{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } }),
                .data = .unit,
            });
            return .{ .func = current, .arg = unit_expr };
        }

        for (arg_exprs[0 .. arg_exprs.len - 1]) |arg_expr| {
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
            .arg = arg_exprs[arg_exprs.len - 1],
        };
    }

    fn lowerSolvedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        original_var: Var,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const var_to_lower = try type_scope.instantiator.instantiateVar(original_var);
        return self.lowerInstantiatedType(module_idx, type_scope, var_to_lower);
    }

    fn lowerExprType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (try self.lowerExplicitExprType(module_idx, type_scope, env, expr_idx, expr)) |explicit| {
            return explicit;
        }
        const lowered = if (type_scope.var_map.get(ModuleEnv.varFrom(expr_idx))) |scoped_var|
            try self.lowerInstantiatedType(module_idx, type_scope, scoped_var)
        else
            try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(expr_idx));
        return self.normalizeDefaultNumericLiteralType(module_idx, expr_idx, expr, lowered);
    }

    fn lowerExplicitExprType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        const cir_env = self.ctx.env(module_idx);
        return switch (expr) {
            .e_lookup_local => |lookup| blk: {
                if (env.get(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(lookup.pattern_idx),
                })) |entry| switch (entry) {
                    .typed => |typed| break :blk typed.bind.ty,
                    .local_fn_source => {},
                };

                break :blk null;
            },
            .e_typed_int => |typed| if (builtinNumPrim(cir_env, typed.type_name)) |prim|
                try self.makePrimitiveType(prim)
            else
                null,
            .e_typed_frac => |typed| if (builtinNumPrim(cir_env, typed.type_name)) |prim|
                try self.makePrimitiveType(prim)
            else
                null,
            .e_record => |record| if (record.ext != null)
                try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(expr_idx))
            else
                try self.lowerExactRecordExprType(module_idx, type_scope, env, record.fields),
            .e_empty_record => try self.makeEmptyRecordType(),
            .e_call => |call| blk: {
                var current = (try self.lowerCallExpectedFnType(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    call.func,
                    cir_env.store.sliceExpr(call.args),
                    null,
                )).ty;
                for (cir_env.store.sliceExpr(call.args)) |_| {
                    if (self.ctx.types.getType(current) != .func) break :blk null;
                    current = self.requireFunctionType(current).ret;
                }
                break :blk current;
            },
            .e_dot_access => |dot| if (dot.args != null)
                try self.lowerRecordedMethodResultType(module_idx, type_scope, env, expr_idx, dot.field_name)
            else
                null,
            .e_type_var_dispatch => |dispatch| try self.lowerRecordedMethodResultType(module_idx, type_scope, env, expr_idx, dispatch.method_name),
            .e_binop => |binop| switch (binop.op) {
                .lt, .gt, .le, .ge, .eq, .ne => try self.makePrimitiveType(.bool),
                .add, .sub, .mul, .div, .rem, .div_trunc => try self.lowerExprType(
                    module_idx,
                    type_scope,
                    env,
                    binop.lhs,
                    cir_env.store.getExpr(binop.lhs),
                ),
                .@"and", .@"or" => try self.makePrimitiveType(.bool),
            },
            .e_unary_minus => |unary| try self.lowerExprType(
                module_idx,
                type_scope,
                env,
                unary.expr,
                cir_env.store.getExpr(unary.expr),
            ),
            .e_unary_not => try self.makePrimitiveType(.bool),
            else => null,
        };
    }

    fn lowerExactRecordExprType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        span: CIR.RecordField.Span,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const cir_env = self.ctx.env(module_idx);
        const fields = cir_env.store.sliceRecordFields(span);
        if (fields.len == 0) return self.makeEmptyRecordType();

        const lowered_fields = try self.allocator.alloc(type_mod.Field, fields.len);
        defer self.allocator.free(lowered_fields);

        for (fields, 0..) |field_idx, i| {
            const field = cir_env.store.getRecordField(field_idx);
            lowered_fields[i] = .{
                .name = try self.ctx.copyExecutableIdent(module_idx, field.name),
                .ty = try self.lowerExprType(
                    module_idx,
                    type_scope,
                    env,
                    field.value,
                    cir_env.store.getExpr(field.value),
                ),
            };
        }

        std.mem.sort(type_mod.Field, lowered_fields, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);

        return try self.ctx.types.addType(.{ .record = .{
            .fields = try self.ctx.types.addFields(lowered_fields),
        } });
    }

    fn makeEmptyRecordType(self: *Lowerer) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.ctx.types.addType(.{ .record = .{
            .fields = type_mod.Span(type_mod.Field).empty(),
        } });
    }

    fn normalizeDefaultNumericLiteralType(
        self: *Lowerer,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        lowered: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (!isDefaultDecLiteralExpr(expr)) return lowered;

        switch (self.ctx.types.getType(lowered)) {
            .primitive => return lowered,
            .tag_union => |tag_union| {
                if (self.ctx.types.sliceTags(tag_union.tags).len == 0) {
                    return self.makePrimitiveType(.dec);
                }
            },
            else => {},
        }

        const resolved = self.ctx.env(module_idx).types.resolveVar(ModuleEnv.varFrom(expr_idx));
        if (resolved.desc.content == .flex) {
            const constraints = self.ctx.env(module_idx).types.sliceStaticDispatchConstraints(
                resolved.desc.content.flex.constraints,
            );
            for (constraints) |constraint| {
                if (constraint.origin == .from_numeral) {
                    return self.makePrimitiveType(.dec);
                }
            }
        }

        return lowered;
    }

    fn lowerInstantiatedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        var_: Var,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const env = self.ctx.env(module_idx);
        const resolved = env.types.resolveVar(var_);
        if (self.defaultNumeralPrimitiveForContent(env, resolved.desc.content)) |prim| {
            return self.makePrimitiveType(prim);
        }
        const key: TypeCloneScope.TypeKey = .{ .module_idx = module_idx, .var_ = resolved.var_ };
        if (type_scope.type_cache.get(key)) |cached| return cached;

        const placeholder = try self.ctx.types.addType(.placeholder);
        try type_scope.type_cache.put(key, placeholder);

        const lowered: type_mod.Content = switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                    const arg_slice = env.types.sliceVars(func.args);
                    const arg_ids = try self.allocator.alloc(type_mod.TypeId, arg_slice.len);
                    defer self.allocator.free(arg_ids);
                    for (arg_slice, 0..) |arg_var, i| {
                        arg_ids[i] = try self.lowerInstantiatedType(module_idx, type_scope, arg_var);
                    }
                    break :blk try self.buildCurriedFuncType(arg_ids, try self.lowerInstantiatedType(module_idx, type_scope, func.ret));
                },
                .tag_union => |tag_union| try self.lowerTagUnionContent(module_idx, type_scope, tag_union.tags, tag_union.ext),
                .record => |record| try self.lowerRecordContent(module_idx, type_scope, record.fields, record.ext),
                .record_unbound => |fields| try self.lowerRecordUnboundContent(module_idx, type_scope, fields),
                .empty_record => .{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } },
                .empty_tag_union => .{ .tag_union = .{ .tags = type_mod.Span(type_mod.Tag).empty() } },
                .tuple => |tuple| blk: {
                    const elems = env.types.sliceVars(tuple.elems);
                    const lowered_elems = try self.allocator.alloc(type_mod.TypeId, elems.len);
                    defer self.allocator.free(lowered_elems);
                    for (elems, 0..) |elem_var, i| {
                        lowered_elems[i] = try self.lowerInstantiatedType(module_idx, type_scope, elem_var);
                    }
                    break :blk .{ .tuple = try self.ctx.types.addTypeSpan(lowered_elems) };
                },
                .nominal_type => |nominal| try self.lowerNominalType(module_idx, type_scope, placeholder, nominal),
            },
            .alias => |alias| .{ .link = try self.lowerInstantiatedType(module_idx, type_scope, env.types.getAliasBackingVar(alias)) },
            .flex => |flex| blk: {
                if (try self.defaultPrimitiveForConstraints(module_idx, env, flex.constraints)) |prim| {
                    break :blk .{ .primitive = prim };
                }
                break :blk .placeholder;
            },
            .rigid => |rigid| blk: {
                if (try self.defaultPrimitiveForConstraints(module_idx, env, rigid.constraints)) |prim| {
                    break :blk .{ .primitive = prim };
                }
                break :blk .placeholder;
            },
            .err => .placeholder,
        };

        self.ctx.types.types.items[@intFromEnum(placeholder)] = lowered;
        return placeholder;
    }

    fn lowerTagUnionContent(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        initial_tags: types.Tag.SafeMultiList.Range,
        initial_ext: Var,
    ) std.mem.Allocator.Error!type_mod.Content {
        const env = self.ctx.env(module_idx);
        var lowered_tags = std.ArrayList(type_mod.Tag).empty;
        defer lowered_tags.deinit(self.allocator);

        var next_tags = initial_tags;
        var next_ext = initial_ext;
        while (true) {
            const tags_slice = env.types.getTagsSlice(next_tags);
            try lowered_tags.ensureUnusedCapacity(self.allocator, next_tags.len());
            for (0..tags_slice.len) |i| {
                const tag_name = tags_slice.items(.name)[i];
                const tag_args = env.types.sliceVars(tags_slice.items(.args)[i]);
                const arg_ids = try self.allocator.alloc(type_mod.TypeId, tag_args.len);
                defer self.allocator.free(arg_ids);
                for (tag_args, 0..) |arg_var, j| {
                    arg_ids[j] = try self.lowerInstantiatedType(module_idx, type_scope, arg_var);
                }
                lowered_tags.appendAssumeCapacity(.{
                    .name = try self.ctx.copyExecutableIdent(module_idx, tag_name),
                    .args = try self.ctx.types.addTypeSpan(arg_ids),
                });
            }

            const resolved_ext = env.types.resolveVar(next_ext);
            switch (resolved_ext.desc.content) {
                .alias => |alias| {
                    next_ext = env.types.getAliasBackingVar(alias);
                },
                .structure => |flat| switch (flat) {
                    .tag_union => |tag_union| {
                        next_tags = tag_union.tags;
                        next_ext = tag_union.ext;
                    },
                    .empty_tag_union => break,
                    else => debugPanic("monotype invariant violated: tag union ext did not resolve to another tag union", .{}),
                },
                .flex => |flex| {
                    if (flex.constraints.len() == 0) break;
                    self.debugPanicUnresolvedTypeVar(env, module_idx, resolved_ext.var_);
                },
                .rigid => |rigid| {
                    if (rigid.constraints.len() == 0) break;
                    self.debugPanicUnresolvedTypeVar(env, module_idx, resolved_ext.var_);
                },
                .err => return .placeholder,
            }
        }

        if (isBuiltinBoolTagUnionSlice(env, lowered_tags.items)) {
            return .{ .primitive = .bool };
        }

        std.mem.sort(type_mod.Tag, lowered_tags.items, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: type_mod.Tag, b: type_mod.Tag) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);
        const canonical_len = try self.dedupeSortedLoweredTags(lowered_tags.items);

        return .{ .tag_union = .{
            .tags = try self.ctx.types.addTags(lowered_tags.items[0..canonical_len]),
        } };
    }

    fn dedupeSortedLoweredTags(
        self: *Lowerer,
        tags: []type_mod.Tag,
    ) std.mem.Allocator.Error!usize {
        if (tags.len <= 1) return tags.len;

        var write_index: usize = 1;
        var prev = tags[0];

        for (tags[1..]) |tag| {
            if (tag.name != prev.name) {
                tags[write_index] = tag;
                write_index += 1;
                prev = tag;
                continue;
            }

            const prev_args = self.ctx.types.sliceTypeSpan(prev.args);
            const tag_args = self.ctx.types.sliceTypeSpan(tag.args);
            if (prev_args.len != tag_args.len) {
                debugPanic("monotype invariant violated: duplicate tag constructors had different arity after row flattening", .{});
            }
            for (prev_args, tag_args) |prev_arg, tag_arg| {
                if (!self.ctx.types.equalIds(prev_arg, tag_arg)) {
                    debugPanic("monotype invariant violated: duplicate tag constructors had different payload types after row flattening", .{});
                }
            }
        }

        return write_index;
    }

    fn lowerRecordContent(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        initial_fields: types.RecordField.SafeMultiList.Range,
        initial_ext: Var,
    ) std.mem.Allocator.Error!type_mod.Content {
        const env = self.ctx.env(module_idx);
        var lowered_fields = std.ArrayList(type_mod.Field).empty;
        defer lowered_fields.deinit(self.allocator);

        var next_fields = initial_fields;
        var next_ext = initial_ext;
        while (true) {
            const fields_slice = env.types.getRecordFieldsSlice(next_fields);
            try lowered_fields.ensureUnusedCapacity(self.allocator, next_fields.len());
            for (0..fields_slice.len) |i| {
                lowered_fields.appendAssumeCapacity(.{
                    .name = try self.ctx.copyExecutableIdent(module_idx, fields_slice.items(.name)[i]),
                    .ty = try self.lowerInstantiatedType(module_idx, type_scope, fields_slice.items(.var_)[i]),
                });
            }

            const resolved_ext = env.types.resolveVar(next_ext);
            switch (resolved_ext.desc.content) {
                .alias => |alias| {
                    next_ext = env.types.getAliasBackingVar(alias);
                },
                .structure => |flat| switch (flat) {
                    .record => |record| {
                        next_fields = record.fields;
                        next_ext = record.ext;
                    },
                    .record_unbound => |fields| {
                        next_fields = fields;
                        const ext_fields_slice = env.types.getRecordFieldsSlice(next_fields);
                        try lowered_fields.ensureUnusedCapacity(self.allocator, next_fields.len());
                        for (0..ext_fields_slice.len) |i| {
                            lowered_fields.appendAssumeCapacity(.{
                                .name = try self.ctx.copyExecutableIdent(module_idx, ext_fields_slice.items(.name)[i]),
                                .ty = try self.lowerInstantiatedType(module_idx, type_scope, ext_fields_slice.items(.var_)[i]),
                            });
                        }
                        break;
                    },
                    .empty_record => break,
                    else => debugPanic("monotype invariant violated: record ext did not resolve to another record", .{}),
                },
                .flex => |flex| {
                    if (flex.constraints.len() == 0) break;
                    self.debugPanicUnresolvedTypeVar(env, module_idx, resolved_ext.var_);
                },
                .rigid => |rigid| {
                    if (rigid.constraints.len() == 0) break;
                    self.debugPanicUnresolvedTypeVar(env, module_idx, resolved_ext.var_);
                },
                .err => return .placeholder,
            }
        }

        std.mem.sort(type_mod.Field, lowered_fields.items, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);

        return .{ .record = .{
            .fields = try self.ctx.types.addFields(lowered_fields.items),
        } };
    }

    fn lowerRecordUnboundContent(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fields: types.RecordField.SafeMultiList.Range,
    ) std.mem.Allocator.Error!type_mod.Content {
        const env = self.ctx.env(module_idx);
        const fields_slice = env.types.getRecordFieldsSlice(fields);
        const lowered_fields = try self.allocator.alloc(type_mod.Field, fields_slice.len);
        defer self.allocator.free(lowered_fields);
        for (0..fields_slice.len) |i| {
            lowered_fields[i] = .{
                .name = try self.ctx.copyExecutableIdent(module_idx, fields_slice.items(.name)[i]),
                .ty = try self.lowerInstantiatedType(module_idx, type_scope, fields_slice.items(.var_)[i]),
            };
        }
        std.mem.sort(type_mod.Field, lowered_fields, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);
        return .{ .record = .{
            .fields = try self.ctx.types.addFields(lowered_fields),
        } };
    }

    fn lowerNominalType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        nominal_type_id: type_mod.TypeId,
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
            return .{ .list = try self.lowerInstantiatedType(module_idx, type_scope, args[0]) };
        }
        if (ident.eql(env.idents.box)) {
            const args = env.types.sliceNominalArgs(nominal);
            if (args.len != 1) debugPanic("monotype.lowerNominalType Box expected one type argument", .{});
            return .{ .box = try self.lowerInstantiatedType(module_idx, type_scope, args[0]) };
        }
        if (ident.eql(env.idents.bool_type)) return .{ .primitive = .bool };
        if (builtinNumPrim(env, ident)) |prim| return .{ .primitive = prim };

        const target_module_idx = findModuleIdxByName(self.ctx.all_module_envs, env.getIdent(nominal.origin_module));
        const target_env = self.ctx.env(target_module_idx);
        const target_ident = target_env.common.findIdent(env.getIdent(ident)) orelse debugPanic(
            "monotype.lowerNominalType missing target ident in defining module",
            .{},
        );
        const nominal_args = env.types.sliceNominalArgs(nominal);
        const lowered_args = try self.allocator.alloc(type_mod.TypeId, nominal_args.len);
        defer self.allocator.free(lowered_args);
        for (nominal_args, 0..) |arg_var, i| {
            lowered_args[i] = try self.lowerInstantiatedType(module_idx, type_scope, arg_var);
        }

        try self.buildNominalTypeCacheKey(type_scope, target_module_idx, target_ident, lowered_args);
        if (type_scope.nominal_type_cache.get(type_scope.scratch_nominal_key.items)) |cached| {
            return .{ .link = cached };
        }

        const owned_key = try self.allocator.dupe(u8, type_scope.scratch_nominal_key.items);
        errdefer self.allocator.free(owned_key);
        try type_scope.nominal_type_cache.put(owned_key, nominal_type_id);

        const backing_var = env.types.getNominalBackingVar(nominal);
        return .{ .nominal = try self.lowerInstantiatedType(module_idx, type_scope, backing_var) };
    }

    fn appendNominalKeyValue(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        value: anytype,
    ) std.mem.Allocator.Error!void {
        var copy = value;
        try type_scope.scratch_nominal_key.appendSlice(self.allocator, std.mem.asBytes(&copy));
    }

    fn buildNominalTypeCacheKey(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        target_module_idx: u32,
        target_ident: base.Ident.Idx,
        lowered_args: []const type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        type_scope.scratch_nominal_key.clearRetainingCapacity();
        try type_scope.scratch_nominal_key.appendSlice(self.allocator, "NOM");
        try self.appendNominalKeyValue(type_scope, target_module_idx);
        try self.appendNominalKeyValue(type_scope, @as(u32, @bitCast(target_ident)));
        try self.appendNominalKeyValue(type_scope, @as(u32, @intCast(lowered_args.len)));
        for (lowered_args) |arg_ty| {
            try self.appendNominalKeyValue(type_scope, @as(u32, @intCast(@intFromEnum(arg_ty))));
        }
    }

    fn defaultNumeralPrimitiveForContent(
        self: *Lowerer,
        env: *const ModuleEnv,
        content: types.Content,
    ) ?type_mod.Prim {
        _ = self;
        return switch (content) {
            .flex => |flex| if (constraintsContainFromNumeral(env, flex.constraints)) .dec else null,
            .rigid => |rigid| if (constraintsContainFromNumeral(env, rigid.constraints)) .dec else null,
            else => null,
        };
    }

    fn defaultPrimitiveForConstraints(
        self: *Lowerer,
        module_idx: u32,
        env: *const ModuleEnv,
        constraints: types.StaticDispatchConstraint.SafeList.Range,
    ) std.mem.Allocator.Error!?type_mod.Prim {
        if (constraints.len() == 0) return null;

        if (constraintsContainFromNumeral(env, constraints)) {
            return .dec;
        }

        _ = self;
        _ = module_idx;
        return null;
    }

    fn debugPanicUnresolvedTypeVar(
        self: *Lowerer,
        env: *const ModuleEnv,
        module_idx: u32,
        var_: Var,
    ) noreturn {
        const resolved = env.types.resolveVar(var_);
        switch (resolved.desc.content) {
            .flex => |flex| {
                std.debug.print(
                    "UNRESOLVED flex var={d} root={d} module={d} name={any} constraints_len={d}\n",
                    .{
                        @intFromEnum(var_),
                        @intFromEnum(resolved.var_),
                        module_idx,
                        flex.name,
                        flex.constraints.len(),
                    },
                );
                for (env.types.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                    std.debug.print(
                        "  constraint fn={s} origin={s}\n",
                        .{
                            self.ctx.getTypeIdentText(module_idx, constraint.fn_name),
                            @tagName(constraint.origin),
                        },
                    );
                }
            },
            .rigid => |rigid| {
                std.debug.print(
                    "UNRESOLVED rigid var={d} root={d} module={d} name={s} constraints_len={d}\n",
                    .{
                        @intFromEnum(var_),
                        @intFromEnum(resolved.var_),
                        module_idx,
                        self.ctx.getTypeIdentText(module_idx, rigid.name),
                        rigid.constraints.len(),
                    },
                );
                for (env.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    std.debug.print(
                        "  constraint fn={s} origin={s}\n",
                        .{
                            self.ctx.getTypeIdentText(module_idx, constraint.fn_name),
                            @tagName(constraint.origin),
                        },
                    );
                }
            },
            else => {},
        }
        const raw: u32 = @intFromEnum(var_);
        if (raw < env.store.nodes.len()) {
            const node_idx: CIR.Node.Idx = @enumFromInt(raw);
            const node_tag = env.store.nodes.fieldItem(.tag, node_idx);
            const region = env.store.getRegionAt(node_idx);
            debugPanic(
                "monotype invariant violated: unresolved type var {d} in module {d} (node tag {s}, start {d}, end {d})",
                .{
                    raw,
                    module_idx,
                    @tagName(node_tag),
                    region.start.offset,
                    region.end.offset,
                },
            );
        }

        debugPanic(
            "monotype invariant violated: unresolved non-node type var {d} in module {d}",
            .{ raw, module_idx },
        );
    }

    fn bindLambdaArg(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        arg_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        _ = type_scope;
        return switch (self.ctx.env(module_idx).store.getPattern(pattern_idx)) {
            .assign => |assign| .{
                .ty = arg_ty,
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            },
            .as => |as_pat| .{
                .ty = arg_ty,
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident),
            },
            .underscore => try self.makeUnitArgWithType(arg_ty),
            else => .{
                .ty = arg_ty,
                .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
            },
        };
    }

    fn isDefaultDecLiteralExpr(expr: CIR.Expr) bool {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                .num_unbound, .int_unbound => true,
                else => false,
            },
            .e_dec, .e_dec_small => true,
            else => false,
        };
    }

    fn makeUnitArgWithType(self: *Lowerer, unit_ty: type_mod.TypeId) std.mem.Allocator.Error!ast.TypedSymbol {
        return .{
            .ty = unit_ty,
            .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
        };
    }

    fn patternNeedsBindingDecls(self: *Lowerer, module_idx: u32, pattern_idx: CIR.Pattern.Idx) bool {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign, .underscore => false,
            .as => true,
            .record_destructure => true,
            .tuple => true,
            .nominal => |nominal| self.patternNeedsBindingDecls(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.patternNeedsBindingDecls(module_idx, nominal.backing_pattern),
            else => false,
        };
    }

    fn makePatternSourceBind(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        const ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx));
        return self.makePatternSourceBindWithType(module_idx, pattern_idx, ty);
    }

    fn makePatternSourceBindWithType(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        const symbol = switch (pattern) {
            .assign => |assign| try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            else => try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
        };
        return .{ .ty = ty, .symbol = symbol };
    }

    fn collectStructuralBindingDecls(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        source: ast.TypedSymbol,
        pattern_idx: CIR.Pattern.Idx,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!void {
        return self.collectStructuralBindingDeclsWithSolvedVar(
            module_idx,
            type_scope,
            source,
            null,
            pattern_idx,
            env,
            decls,
        );
    }

    fn collectStructuralBindingDeclsWithSolvedVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        source: ast.TypedSymbol,
        source_solved_var: ?Var,
        pattern_idx: CIR.Pattern.Idx,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!void {
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => {
                try env.put(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{ .typed = .{
                    .bind = source,
                    .solved_var = source_solved_var,
                } });
            },
            .as => |as_pat| {
                try env.put(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{ .typed = .{
                    .bind = source,
                    .solved_var = source_solved_var,
                } });
                try self.collectStructuralBindingDeclsWithSolvedVar(
                    module_idx,
                    type_scope,
                    source,
                    source_solved_var,
                    as_pat.pattern,
                    env,
                    decls,
                );
            },
            .underscore => {},
            .record_destructure => |record| {
                switch (self.ctx.types.getType(source.ty)) {
                    .record => {},
                    else => debugPanic(
                        "monotype invariant violated: record destructure source symbol {d} has non-record type {d}",
                        .{ source.symbol.raw(), @intFromEnum(source.ty) },
                    ),
                }
                const source_expr = try self.makeVarExpr(source.ty, source.symbol);
                for (cir_env.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = cir_env.store.getRecordDestruct(destruct_idx);
                    const field_ty = try self.lookupRecordFieldType(module_idx, source.ty, destruct.label);
                    const field_solved_var = self.lookupRecordFieldSolvedVar(module_idx, source_solved_var, destruct.label);
                    const field_bind: ast.TypedSymbol = .{
                        .ty = field_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    const field_expr = try self.program.store.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .access = .{
                            .record = source_expr,
                            .field = try self.ctx.copyExecutableIdent(module_idx, destruct.label),
                        } },
                    });
                    try decls.append(self.allocator, .{
                        .bind = field_bind,
                        .body = field_expr,
                    });
                    try self.collectStructuralBindingDeclsWithSolvedVar(
                        module_idx,
                        type_scope,
                        field_bind,
                        field_solved_var,
                        destruct.kind.toPatternIdx(),
                        env,
                        decls,
                    );
                }
            },
            .tuple => |tuple| {
                const elem_patterns = cir_env.store.slicePatterns(tuple.patterns);
                const elem_tys = try self.lookupTupleElemTypesOwned(source.ty);
                defer self.allocator.free(elem_tys);
                if (elem_patterns.len != elem_tys.len) {
                    return debugPanic("monotype invariant violated: tuple pattern arity mismatch for lowered source type", .{});
                }
                const source_expr = try self.makeVarExpr(source.ty, source.symbol);
                for (elem_patterns, elem_tys, 0..) |elem_pattern_idx, elem_ty, i| {
                    const elem_bind: ast.TypedSymbol = .{
                        .ty = elem_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    try decls.append(self.allocator, .{
                        .bind = elem_bind,
                        .body = try self.makeTupleAccessExpr(source_expr, source.ty, i),
                    });
                    try self.collectStructuralBindingDeclsWithSolvedVar(
                        module_idx,
                        type_scope,
                        elem_bind,
                        self.lookupTupleElemSolvedVar(module_idx, source_solved_var, i),
                        elem_pattern_idx,
                        env,
                        decls,
                    );
                }
            },
            .nominal => |nominal| try self.collectStructuralBindingDeclsWithSolvedVar(
                module_idx,
                type_scope,
                source,
                source_solved_var,
                nominal.backing_pattern,
                env,
                decls,
            ),
            .nominal_external => |nominal| try self.collectStructuralBindingDeclsWithSolvedVar(
                module_idx,
                type_scope,
                source,
                source_solved_var,
                nominal.backing_pattern,
                env,
                decls,
            ),
            else => debugTodoPattern(pattern),
        }
    }

    fn wrapExprWithBindingDecls(
        self: *Lowerer,
        body: ast.ExprId,
        decls: []const BindingDecl,
    ) std.mem.Allocator.Error!ast.ExprId {
        var current = body;
        var i = decls.len;
        while (i > 0) : (i -= 1) {
            current = try self.program.store.addExpr(.{
                .ty = self.program.store.getExpr(current).ty,
                .data = .{ .let_ = .{
                    .def = .{ .let_val = .{
                        .bind = decls[i - 1].bind,
                        .body = decls[i - 1].body,
                    } },
                    .rest = current,
                } },
            });
        }
        return current;
    }

    fn appendBindingDeclStmts(
        self: *Lowerer,
        lowered: *std.ArrayList(ast.StmtId),
        decls: []const BindingDecl,
        mutable: bool,
    ) std.mem.Allocator.Error!void {
        for (decls) |decl| {
            const stmt: ast.Stmt = if (mutable)
                .{ .var_decl = .{
                    .bind = decl.bind,
                    .body = decl.body,
                } }
            else
                .{ .decl = .{
                    .bind = decl.bind,
                    .body = decl.body,
                } };
            try lowered.append(self.allocator, try self.program.store.addStmt(stmt));
        }
    }

    fn appendPatternBindingOrReassign(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        source_expr: ast.ExprId,
        env: *BindingEnv,
        lowered: *std.ArrayList(ast.StmtId),
    ) std.mem.Allocator.Error!void {
        const key: PatternKey = .{
            .module_idx = module_idx,
            .pattern_idx = @intFromEnum(pattern_idx),
        };
        if (env.get(key)) |entry| {
            return switch (entry) {
                .typed => |typed| lowered.append(self.allocator, try self.program.store.addStmt(.{ .reassign = .{
                    .target = typed.bind.symbol,
                    .body = source_expr,
                } })),
                .local_fn_source => debugPanic("monotype invariant violated: cannot reassign local fn source", .{}),
            };
        }

        const bind = try self.requirePatternBinderWithType(module_idx, pattern_idx, source_ty);
        try env.put(key, .{ .typed = .{ .bind = bind } });
        try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
            .bind = bind,
            .body = source_expr,
        } }));
    }

    fn lowerPatternReassignFromExpr(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        source_expr: ast.ExprId,
        source_ty: type_mod.TypeId,
        env: *BindingEnv,
        lowered: *std.ArrayList(ast.StmtId),
    ) std.mem.Allocator.Error!void {
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => try self.appendPatternBindingOrReassign(module_idx, pattern_idx, source_ty, source_expr, env, lowered),
            .as => |as_pat| {
                try self.appendPatternBindingOrReassign(module_idx, pattern_idx, source_ty, source_expr, env, lowered);
                try self.lowerPatternReassignFromExpr(module_idx, as_pat.pattern, source_expr, source_ty, env, lowered);
            },
            .underscore => {},
            .record_destructure => |record| {
                for (cir_env.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = cir_env.store.getRecordDestruct(destruct_idx);
                    const field_ty = try self.lookupRecordFieldType(module_idx, source_ty, destruct.label);
                    const field_bind: ast.TypedSymbol = .{
                        .ty = field_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    const field_expr = try self.program.store.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .access = .{
                            .record = source_expr,
                            .field = try self.ctx.copyExecutableIdent(module_idx, destruct.label),
                        } },
                    });
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                        .bind = field_bind,
                        .body = field_expr,
                    } }));
                    try self.lowerPatternReassignFromExpr(
                        module_idx,
                        destruct.kind.toPatternIdx(),
                        try self.makeVarExpr(field_ty, field_bind.symbol),
                        field_ty,
                        env,
                        lowered,
                    );
                }
            },
            .tuple => |tuple| {
                const elem_patterns = cir_env.store.slicePatterns(tuple.patterns);
                const elem_tys = try self.lookupTupleElemTypesOwned(source_ty);
                defer self.allocator.free(elem_tys);
                if (elem_patterns.len != elem_tys.len) {
                    return debugPanic("monotype invariant violated: tuple reassign arity mismatch for lowered source type", .{});
                }
                for (elem_patterns, elem_tys, 0..) |elem_pattern_idx, elem_ty, i| {
                    const elem_bind: ast.TypedSymbol = .{
                        .ty = elem_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                        .bind = elem_bind,
                        .body = try self.makeTupleAccessExpr(source_expr, source_ty, i),
                    } }));
                    try self.lowerPatternReassignFromExpr(
                        module_idx,
                        elem_pattern_idx,
                        try self.makeVarExpr(elem_ty, elem_bind.symbol),
                        elem_ty,
                        env,
                        lowered,
                    );
                }
            },
            .nominal => |nominal| try self.lowerPatternReassignFromExpr(
                module_idx,
                nominal.backing_pattern,
                source_expr,
                source_ty,
                env,
                lowered,
            ),
            .nominal_external => |nominal| try self.lowerPatternReassignFromExpr(
                module_idx,
                nominal.backing_pattern,
                source_expr,
                source_ty,
                env,
                lowered,
            ),
            else => debugTodoPattern(pattern),
        }
    }

    fn lowerStructuralPatWithSource(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!ast.PatId {
        const source = try self.makePatternSourceBind(module_idx, type_scope, pattern_idx);
        try self.collectStructuralBindingDecls(module_idx, type_scope, source, pattern_idx, env, decls);
        return try self.program.store.addPat(.{
            .ty = source.ty,
            .data = .{ .var_ = source.symbol },
        });
    }

    fn lowerStructuralPatWithType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!ast.PatId {
        const source = try self.makePatternSourceBindWithType(module_idx, pattern_idx, source_ty);
        try self.collectStructuralBindingDecls(module_idx, type_scope, source, pattern_idx, env, decls);
        return try self.program.store.addPat(.{
            .ty = source.ty,
            .data = .{ .var_ = source.symbol },
        });
    }

    fn lowerMatchPatWithType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!ast.PatId {
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);

        if (self.patternNeedsBindingDecls(module_idx, pattern_idx)) {
            return self.lowerStructuralPatWithType(module_idx, type_scope, pattern_idx, source_ty, env, decls);
        }

        return switch (pattern) {
            .assign => |assign| blk: {
                const source: ast.TypedSymbol = .{
                    .ty = source_ty,
                    .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
                };
                try self.collectStructuralBindingDecls(module_idx, type_scope, source, pattern_idx, env, decls);
                break :blk try self.program.store.addPat(.{
                    .ty = source_ty,
                    .data = .{ .var_ = source.symbol },
                });
            },
            .underscore => try self.program.store.addPat(.{
                .ty = source_ty,
                .data = .{ .var_ = symbol_mod.Symbol.none },
            }),
            .applied_tag => |tag| blk: {
                if (self.ctx.types.getType(source_ty) == .primitive and self.ctx.types.getType(source_ty).primitive == .bool) {
                    if (cir_env.store.slicePatterns(tag.args).len != 0) {
                        return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    break :blk try self.program.store.addPat(.{
                        .ty = source_ty,
                        .data = .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag.name) },
                    });
                }
                const arg_pats = cir_env.store.slicePatterns(tag.args);
                const arg_tys = try self.lookupTagArgTypesOwned(module_idx, source_ty, tag.name);
                defer self.allocator.free(arg_tys);
                if (arg_pats.len != arg_tys.len) {
                    return debugPanic("monotype invariant violated: tag pattern arity mismatch for lowered source type", .{});
                }
                const lowered_args = try self.allocator.alloc(ast.PatId, arg_pats.len);
                defer self.allocator.free(lowered_args);
                for (arg_pats, arg_tys, 0..) |arg_pat, arg_ty, i| {
                    lowered_args[i] = try self.lowerMatchPatWithType(module_idx, type_scope, arg_pat, arg_ty, env, decls);
                }
                break :blk try self.program.store.addPat(.{
                    .ty = source_ty,
                    .data = .{ .tag = .{
                        .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
                        .args = try self.program.store.addPatSpan(lowered_args),
                    } },
                });
            },
            .nominal => |nominal| blk: {
                const lowered = try self.lowerMatchPatWithType(module_idx, type_scope, nominal.backing_pattern, source_ty, env, decls);
                const backing = self.program.store.getPat(lowered);
                break :blk try self.program.store.addPat(.{
                    .ty = source_ty,
                    .data = backing.data,
                });
            },
            .nominal_external => |nominal| blk: {
                const lowered = try self.lowerMatchPatWithType(module_idx, type_scope, nominal.backing_pattern, source_ty, env, decls);
                const backing = self.program.store.getPat(lowered);
                break :blk try self.program.store.addPat(.{
                    .ty = source_ty,
                    .data = backing.data,
                });
            },
            else => debugTodoPattern(pattern),
        };
    }

    fn normalizeNominalChildType(
        self: *Lowerer,
        parent_ty: type_mod.TypeId,
        child_ty: type_mod.TypeId,
    ) type_mod.TypeId {
        return switch (self.ctx.types.getTypePreservingNominal(parent_ty)) {
            .nominal => |backing| if (self.ctx.types.equalIds(child_ty, backing)) parent_ty else child_ty,
            else => child_ty,
        };
    }

    fn lookupTagArgTypesOwned(
        self: *Lowerer,
        module_idx: u32,
        source_ty: type_mod.TypeId,
        tag_name: base.Ident.Idx,
    ) std.mem.Allocator.Error![]type_mod.TypeId {
        const executable_tag_name = try self.ctx.copyExecutableIdent(module_idx, tag_name);
        return switch (self.ctx.types.getType(source_ty)) {
            .tag_union => |tag_union| blk: {
                for (self.ctx.types.sliceTags(tag_union.tags)) |tag| {
                    if (tag.name.idx == executable_tag_name.idx) {
                        const args = self.ctx.types.sliceTypeSpan(tag.args);
                        const out = try self.allocator.alloc(type_mod.TypeId, args.len);
                        for (args, 0..) |arg_ty, i| {
                            out[i] = self.normalizeNominalChildType(source_ty, arg_ty);
                        }
                        break :blk out;
                    }
                }
                debugPanic("monotype invariant violated: missing tag in lowered tag union type", .{});
            },
            else => debugPanic("monotype invariant violated: attempted to read tag args from non-tag type", .{}),
        };
    }

    fn lookupRecordFieldType(
        self: *Lowerer,
        module_idx: u32,
        source_ty: type_mod.TypeId,
        field_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const executable_field_name = try self.ctx.copyExecutableIdent(module_idx, field_name);
        return switch (self.ctx.types.getType(source_ty)) {
            .record => |record| blk: {
                for (self.ctx.types.sliceFields(record.fields)) |field| {
                    if (field.name.idx == executable_field_name.idx) {
                        break :blk self.normalizeNominalChildType(source_ty, field.ty);
                    }
                }
                debugPanic("monotype invariant violated: missing field in lowered record type", .{});
            },
            else => debugPanic("monotype invariant violated: attempted to read record field type from non-record type", .{}),
        };
    }

    fn lookupTupleElemTypesOwned(self: *Lowerer, source_ty: type_mod.TypeId) std.mem.Allocator.Error![]type_mod.TypeId {
        return switch (self.ctx.types.getType(source_ty)) {
            .tuple => |tuple| blk: {
                const elems = self.ctx.types.sliceTypeSpan(tuple);
                const out = try self.allocator.alloc(type_mod.TypeId, elems.len);
                for (elems, 0..) |elem_ty, i| {
                    out[i] = self.normalizeNominalChildType(source_ty, elem_ty);
                }
                break :blk out;
            },
            else => debugPanic("monotype invariant violated: attempted to read tuple element types from non-tuple type", .{}),
        };
    }

    fn lookupTupleElemType(self: *Lowerer, source_ty: type_mod.TypeId, elem_index: usize) type_mod.TypeId {
        const elems = self.lookupTupleElemTypesOwned(source_ty) catch |err|
            std.debug.panic("monotype invariant violated: tuple elem allocation failed: {s}", .{@errorName(err)});
        defer self.allocator.free(elems);
        if (elem_index >= elems.len) {
            debugPanic("monotype invariant violated: tuple element index out of bounds", .{});
        }
        return elems[elem_index];
    }

    fn lookupTupleElemSolvedVar(self: *const Lowerer, module_idx: u32, source_solved_var: ?Var, elem_index: usize) ?Var {
        const root = source_solved_var orelse return null;
        const env = self.ctx.env(module_idx);
        const resolved = env.types.resolveVar(root);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .tuple => |tuple| blk: {
                    const elems = env.types.sliceVars(tuple.elems);
                    if (elem_index >= elems.len) break :blk null;
                    break :blk elems[elem_index];
                },
                else => null,
            },
            .alias => |alias| self.lookupTupleElemSolvedVar(module_idx, env.types.getAliasBackingVar(alias), elem_index),
            else => null,
        };
    }

    fn lookupRecordFieldSolvedVar(self: *const Lowerer, module_idx: u32, source_solved_var: ?Var, field_name: base.Ident.Idx) ?Var {
        const root = source_solved_var orelse return null;
        const env = self.ctx.env(module_idx);
        const resolved = env.types.resolveVar(root);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .record => |record| blk: {
                    const fields = env.types.getRecordFieldsSlice(record.fields);
                    for (fields.items(.name), fields.items(.var_)) |name, field_var| {
                        if (name.eql(field_name)) break :blk field_var;
                    }
                    break :blk null;
                },
                else => null,
            },
            .alias => |alias| self.lookupRecordFieldSolvedVar(module_idx, env.types.getAliasBackingVar(alias), field_name),
            else => null,
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
                }, .{ .typed = .{
                    .bind = .{
                        .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx)),
                        .symbol = symbol,
                    },
                } });
            },
            .as => |as_pat| {
                try self.bindPatternEnv(module_idx, type_scope, as_pat.pattern, env);
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident);
                try env.put(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{ .typed = .{
                    .bind = .{
                        .ty = try self.lowerSolvedType(module_idx, type_scope, ModuleEnv.varFrom(pattern_idx)),
                        .symbol = symbol,
                    },
                } });
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

    fn bindPatternEnvFromType(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        return self.bindPatternEnvFromTypeWithSolvedVar(module_idx, pattern_idx, source_ty, null, env);
    }

    fn bindPatternEnvFromTypeWithSolvedVar(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        source_solved_var: ?Var,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign| {
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident);
                try env.put(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{ .typed = .{
                    .bind = .{
                        .ty = source_ty,
                        .symbol = symbol,
                    },
                    .solved_var = source_solved_var,
                } });
            },
            .as => |as_pat| {
                try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, as_pat.pattern, source_ty, source_solved_var, env);
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident);
                try env.put(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{ .typed = .{
                    .bind = .{
                        .ty = source_ty,
                        .symbol = symbol,
                    },
                    .solved_var = source_solved_var,
                } });
            },
            .nominal => |nominal| try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, nominal.backing_pattern, source_ty, source_solved_var, env),
            .nominal_external => |nominal| try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, nominal.backing_pattern, source_ty, source_solved_var, env),
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

    fn requirePatternSymbolOnly(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign => |assign| try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            .as => |as_pat| try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident),
            .nominal => |nominal| try self.requirePatternSymbolOnly(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| try self.requirePatternSymbolOnly(module_idx, nominal.backing_pattern),
            else => debugTodoPattern(pattern),
        };
    }

    fn requirePatternBinderWithType(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign => |assign| .{
                .ty = ty,
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            },
            .as => |as_pat| .{
                .ty = ty,
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident),
            },
            .nominal => |nominal| try self.requirePatternBinderWithType(module_idx, nominal.backing_pattern, ty),
            .nominal_external => |nominal| try self.requirePatternBinderWithType(module_idx, nominal.backing_pattern, ty),
            else => debugTodoPattern(pattern),
        };
    }

    fn specializeLocalFnSource(
        self: *Lowerer,
        source_ref: LocalFnSourceRef,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const group = self.local_fn_groups.items[source_ref.group_index];
        for (group.pending.items) |pending| {
            if (pending.source_index == source_ref.source_index and self.ctx.types.equalIds(pending.ty, expected_ty)) {
                return pending.symbol;
            }
        }

        const source = group.sources[source_ref.source_index];
        const source_entry = self.ctx.symbols.get(source.source_symbol);
        const specialized_symbol = try self.ctx.symbols.add(source_entry.name, .{
            .specialized_local_fn = .{
                .source_symbol = source.source_symbol.raw(),
            },
        });
        try group.pending.append(self.allocator, .{
            .source_index = source_ref.source_index,
            .ty = expected_ty,
            .symbol = specialized_symbol,
        });
        const pending_idx = group.pending.items.len - 1;

        var type_scope: TypeCloneScope = undefined;
        type_scope.initCloneAll(self.allocator, @constCast(self.ctx.env(group.module_idx)));
        defer type_scope.deinit();
        const bind: ast.TypedSymbol = .{
            .ty = if (expected_var) |specialized_checker_var|
                try self.lowerInstantiatedType(group.module_idx, &type_scope, specialized_checker_var)
            else
                expected_ty,
            .symbol = specialized_symbol,
        };
        const letfn = try self.lowerLambdaLikeDefWithEnv(
            group.module_idx,
            &type_scope,
            group.declaration_env,
            bind,
            source.expr_idx,
            false,
            expected_var,
        );
        group.pending.items[pending_idx].stmt_id = try self.program.store.addStmt(.{
            .local_fn = letfn,
        });
        return specialized_symbol;
    }

    fn lookupOrSpecializeLocal(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        pattern_idx: CIR.Pattern.Idx,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        _ = type_scope;
        if (env.get(.{ .module_idx = module_idx, .pattern_idx = @intFromEnum(pattern_idx) })) |entry| {
            return switch (entry) {
                .typed => |typed| typed.bind.symbol,
                .local_fn_source => |source| try self.specializeLocalFnSource(source, expected_ty, expected_var),
            };
        }

        const top_level_symbol = self.lookupTopLevelSymbol(module_idx, pattern_idx) orelse debugPanic(
            "monotype invariant violated: missing local binding symbol for module {d} pattern {d}",
            .{ module_idx, @intFromEnum(pattern_idx) },
        );

        if (self.top_level_defs_by_symbol.get(top_level_symbol)) |top_level| {
            self.assertNoFirstClassBuiltinStrInspect(top_level.module_idx, top_level.def_idx, "local");
            if (top_level.is_function) {
                const source_expected_var = expected_var orelse ModuleEnv.varFrom(self.ctx.env(top_level.module_idx).store.getDef(top_level.def_idx).expr);
                return try self.specializations.specializeFn(&self.ctx.symbols, &self.ctx.types, top_level_symbol, .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                }, expected_ty, source_expected_var);
            }
            try self.ensureTopLevelValueDefEmitted(top_level_symbol);
        }

        return top_level_symbol;
    }

    fn lookupFunctionArgVar(
        self: *const Lowerer,
        module_idx: u32,
        fn_var: Var,
        arg_index: usize,
    ) ?Var {
        const env = self.ctx.env(module_idx);
        const resolved = env.types.resolveVar(fn_var);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                    const args = env.types.sliceVars(func.args);
                    if (arg_index >= args.len) break :blk null;
                    break :blk args[arg_index];
                },
                else => null,
            },
            .alias => |alias| self.lookupFunctionArgVar(module_idx, env.types.getAliasBackingVar(alias), arg_index),
            else => null,
        };
    }

    fn requireFunctionArgVar(
        self: *const Lowerer,
        module_idx: u32,
        fn_var: ?Var,
        arg_index: usize,
    ) Var {
        const function_var = fn_var orelse debugPanic(
            "monotype invariant violated: missing specialized function var in module {d}",
            .{module_idx},
        );
        return self.lookupFunctionArgVar(module_idx, function_var, arg_index) orelse debugPanic(
            "monotype invariant violated: missing specialized function argument {d} in module {d}",
            .{ arg_index, module_idx },
        );
    }

    fn lookupSpecializedExprVar(
        self: *const Lowerer,
        module_idx: u32,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) ?Var {
        const expr = self.ctx.env(module_idx).store.getExpr(expr_idx);
        return switch (expr) {
            .e_lookup_local => |lookup| if (env.get(.{
                .module_idx = module_idx,
                .pattern_idx = @intFromEnum(lookup.pattern_idx),
            })) |entry| switch (entry) {
                .typed => |typed| typed.solved_var,
                .local_fn_source => null,
            } else null,
            else => null,
        };
    }

    fn scopedExprResultVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!Var {
        const cir_env = self.ctx.env(module_idx);
        const expr = cir_env.store.getExpr(expr_idx);
        return switch (expr) {
            .e_lookup_local => self.lookupSpecializedExprVar(module_idx, env, expr_idx) orelse try self.scopeVar(type_scope, ModuleEnv.varFrom(expr_idx)),
            .e_call => |call| blk: {
                const expected = try self.lowerCallExpectedFnType(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    call.func,
                    cir_env.store.sliceExpr(call.args),
                    null,
                );
                if (expected.solved_var) |fn_var| {
                    break :blk self.lookupFunctionRetVar(module_idx, fn_var) orelse debugPanic(
                        "monotype invariant violated: specialized call missing return var in module {d}",
                        .{module_idx},
                    );
                }
                break :blk try self.scopeVar(type_scope, ModuleEnv.varFrom(expr_idx));
            },
            .e_dot_access => |dot| if (dot.args != null)
                try self.lowerRecordedMethodResultVar(module_idx, type_scope, env, expr_idx, dot.field_name)
            else
                try self.scopeVar(type_scope, ModuleEnv.varFrom(expr_idx)),
            .e_type_var_dispatch => |dispatch| try self.lowerRecordedMethodResultVar(module_idx, type_scope, env, expr_idx, dispatch.method_name),
            else => try self.scopeVar(type_scope, ModuleEnv.varFrom(expr_idx)),
        };
    }

    fn scopeVar(
        _: *Lowerer,
        type_scope: *TypeCloneScope,
        source_var: Var,
    ) std.mem.Allocator.Error!Var {
        if (type_scope.var_map.get(source_var)) |scoped| return scoped;
        return try type_scope.instantiator.instantiateVar(source_var);
    }

    fn lowerRecordedMethodResultVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!Var {
        const fact = try self.lowerRecordedMethodFact(
            module_idx,
            type_scope,
            env,
            expr_idx,
            method_name,
            null,
        );
        return self.lookupFunctionRetVar(module_idx, fact.solved_var) orelse debugPanic(
            "monotype static dispatch invariant violated: missing method result var",
            .{},
        );
    }

    fn lowerRecordedMethodResultType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const result_var = try self.lowerRecordedMethodResultVar(module_idx, type_scope, env, expr_idx, method_name);
        return try self.lowerInstantiatedType(module_idx, type_scope, result_var);
    }

    fn lookupResolvedDispatchTarget(
        self: *const Lowerer,
        source_module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedTarget {
        const source_env = self.ctx.env(source_module_idx);
        const resolved_site = source_env.types.findResolvedStaticDispatchSite(ModuleEnv.varFrom(expr_idx)) orelse return null;
        return .{
            .module_idx = findModuleIdxByName(self.ctx.all_module_envs, source_env.getIdent(resolved_site.target_module_name)),
            .def_idx = @enumFromInt(resolved_site.target_def_idx),
        };
    }

    fn lookupResolvedTargetFnVar(
        self: *Lowerer,
        current_module_idx: u32,
        target: ResolvedTarget,
    ) std.mem.Allocator.Error!?Var {
        const source_symbol = self.lookupTopLevelDefSymbol(target.module_idx, target.def_idx) orelse return null;
        const top_level = self.top_level_defs_by_symbol.get(source_symbol) orelse return null;
        if (top_level.needs_specialization) return null;

        const target_env = self.ctx.env(target.module_idx);
        const target_def = target_env.store.getDef(target.def_idx);
        const target_fn_var = ModuleEnv.varFrom(target_def.expr);
        return if (current_module_idx == target.module_idx)
            target_fn_var
        else
            try self.copyCheckerVarToModule(target.module_idx, current_module_idx, target_fn_var);
    }

    fn lookupFunctionRetVar(
        self: *const Lowerer,
        module_idx: u32,
        fn_var: ?Var,
    ) ?Var {
        const var_ = fn_var orelse return null;
        const env = self.ctx.env(module_idx);
        const resolved = env.types.resolveVar(var_);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| func.ret,
                else => null,
            },
            .alias => |alias| self.lookupFunctionRetVar(module_idx, env.types.getAliasBackingVar(alias)),
            else => null,
        };
    }

    fn unifyClonedCheckerVars(
        self: *Lowerer,
        module_idx: u32,
        left: Var,
        right: Var,
    ) std.mem.Allocator.Error!void {
        const env = @constCast(self.ctx.env(module_idx));
        var problems = try check.problem.Store.init(self.allocator);
        defer problems.deinit(self.allocator);
        var snapshots = try check.snapshot.Store.initCapacity(self.allocator, 16);
        defer snapshots.deinit();
        var type_writer = try types.TypeWriter.initFromParts(
            self.allocator,
            &env.types,
            env.getIdentStoreConst(),
            null,
        );
        defer type_writer.deinit();
        var unify_scratch = try check.unifier.Scratch.init(self.allocator);
        defer unify_scratch.deinit();
        var occurs_scratch = try check.occurs.Scratch.init(self.allocator);
        defer occurs_scratch.deinit();

        const result = try check.unifier.unify(
            env,
            &env.types,
            &problems,
            &snapshots,
            &type_writer,
            &unify_scratch,
            &occurs_scratch,
            left,
            right,
        );
        switch (result) {
            .ok => {},
            .problem => {
                const left_text = try type_writer.writeGet(left, .one_line);
                const left_snapshot = try self.allocator.dupe(u8, left_text);
                defer self.allocator.free(left_snapshot);
                const right_text = try type_writer.writeGet(right, .one_line);
                const right_snapshot = try self.allocator.dupe(u8, right_text);
                defer self.allocator.free(right_snapshot);
                debugPanic(
                    "monotype invariant violated: function specialization unify failed in module {d}: {s} vs {s}",
                    .{ module_idx, left_snapshot, right_snapshot },
                );
            },
        }
    }

    fn lookupOrSpecializeExternal(
        self: *Lowerer,
        current_module_idx: u32,
        lookup: anytype,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
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
            self.assertNoFirstClassBuiltinStrInspect(top_level.module_idx, top_level.def_idx, "external");
            if (top_level.is_function) {
                const source_expected_var = if (expected_var) |var_|
                    try self.copyCheckerVarToModule(current_module_idx, top_level.module_idx, var_)
                else
                    ModuleEnv.varFrom(self.ctx.env(top_level.module_idx).store.getDef(top_level.def_idx).expr);
                return try self.specializations.specializeFn(&self.ctx.symbols, &self.ctx.types, symbol, .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                }, expected_ty, source_expected_var);
            }
            try self.ensureTopLevelValueDefEmitted(symbol);
        }

        return symbol;
    }

    fn assertNoFirstClassBuiltinStrInspect(
        self: *const Lowerer,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        comptime site: []const u8,
    ) void {
        if (comptime builtin.mode == .Debug) {
            if (self.isBuiltinStrInspectSource(module_idx, def_idx)) {
                debugPanic(
                    "monotype invariant violated: Str.inspect must lower through direct-call inspect lowering, not as a first-class " ++ site ++ " value",
                    .{},
                );
            }
        }
    }

    fn copyCheckerVarToModule(
        self: *Lowerer,
        source_module_idx: u32,
        dest_module_idx: u32,
        source_var: Var,
    ) std.mem.Allocator.Error!Var {
        if (source_module_idx == dest_module_idx) return source_var;

        const source_env = self.ctx.env(source_module_idx);
        const dest_env = @constCast(self.ctx.env(dest_module_idx));
        var var_map = std.AutoHashMap(Var, Var).init(self.allocator);
        defer var_map.deinit();

        return try check.copy_import.copyVar(
            &source_env.types,
            &dest_env.types,
            source_var,
            &var_map,
            source_env.getIdentStoreConst(),
            dest_env.getIdentStore(),
            self.allocator,
        );
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

    fn requireFunctionType(self: *Lowerer, fn_ty: type_mod.TypeId) @FieldType(type_mod.Content, "func") {
        return switch (self.ctx.types.getType(fn_ty)) {
            .func => |func| func,
            else => debugPanic("monotype invariant violated: attempted to use non-function type {d} as function", .{@intFromEnum(fn_ty)}),
        };
    }

    fn requireListElemType(self: *Lowerer, list_ty: type_mod.TypeId) type_mod.TypeId {
        return switch (self.ctx.types.getType(list_ty)) {
            .list => |elem| elem,
            else => debugPanic(
                "monotype invariant violated: attempted to use non-list type {d} as loop iterable",
                .{@intFromEnum(list_ty)},
            ),
        };
    }

    fn callResultType(self: *Lowerer, func_expr: ast.ExprId) type_mod.TypeId {
        const func_ty = self.program.store.getExpr(func_expr).ty;
        return self.requireFunctionType(func_ty).ret;
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

    fn lowerResolvedTargetCallee(
        self: *Lowerer,
        target: ResolvedTarget,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const source_symbol = self.lookupTopLevelDefSymbol(target.module_idx, target.def_idx) orelse debugPanic(
            "monotype static dispatch invariant violated: unresolved target symbol {d}:{d}",
            .{ target.module_idx, @intFromEnum(target.def_idx) },
        );

        const symbol = if (self.top_level_defs_by_symbol.get(source_symbol)) |top_level|
            if (top_level.is_function) blk: {
                const source_expected_var = ModuleEnv.varFrom(self.ctx.env(top_level.module_idx).store.getDef(top_level.def_idx).expr);
                break :blk try self.specializations.specializeFn(&self.ctx.symbols, &self.ctx.types, source_symbol, .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                }, expected_ty, source_expected_var);
            } else blk: {
                try self.ensureTopLevelValueDefEmitted(source_symbol);
                break :blk source_symbol;
            }
        else
            source_symbol;

        return try self.program.store.addExpr(.{
            .ty = expected_ty,
            .data = .{ .var_ = symbol },
        });
    }

    fn resolveMonomorphicDispatchTarget(
        self: *const Lowerer,
        source_module_idx: u32,
        type_scope: *const TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
    ) ResolvedTarget {
        const source_env = self.ctx.env(source_module_idx);
        const receiver = self.resolveDispatchReceiverForExpr(
            source_module_idx,
            type_scope,
            env,
            expr_idx,
            method_name,
        );
        const method_ident = receiver.target_env.lookupMethodIdentFromEnvConst(
            source_env,
            receiver.type_ident,
            method_name,
        ) orelse debugPanic(
            "monotype static dispatch invariant violated: missing method {s} for receiver type {s} in module {s}",
            .{
                source_env.getIdent(method_name),
                source_env.getIdent(receiver.type_ident),
                moduleName(receiver.target_env),
            },
        );
        const target_node_idx = receiver.target_env.getExposedNodeIndexById(method_ident) orelse debugPanic(
            "monotype static dispatch invariant violated: method {s} is not exposed in module {s}",
            .{
                receiver.target_env.getIdent(method_ident),
                moduleName(receiver.target_env),
            },
        );
        return .{
            .module_idx = receiver.target_module_idx,
            .def_idx = @enumFromInt(@as(u32, @intCast(target_node_idx))),
        };
    }

    const DispatchReceiver = struct {
        target_module_idx: u32,
        target_env: *const ModuleEnv,
        type_ident: base.Ident.Idx,
    };

    fn resolveDispatchReceiverForExpr(
        self: *const Lowerer,
        source_module_idx: u32,
        type_scope: *const TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
    ) DispatchReceiver {
        const source_env = self.ctx.env(source_module_idx);
        const expr = source_env.store.getExpr(expr_idx);
        return switch (expr) {
            .e_dot_access => |dot| self.resolveDispatchReceiverExprVar(
                source_module_idx,
                type_scope,
                env,
                dot.receiver,
                method_name,
            ),
            .e_type_var_dispatch => |dispatch| blk: {
                const stmt = source_env.store.getStatement(dispatch.type_var_alias_stmt);
                const type_var = ModuleEnv.varFrom(stmt.s_type_var_alias.type_var_anno);
                break :blk self.resolveDispatchReceiverVar(source_module_idx, type_scope, type_var, method_name);
            },
            else => debugPanic(
                "monotype static dispatch invariant violated: unsupported dispatch expr {s} for method {s}",
                .{ @tagName(expr), source_env.getIdent(method_name) },
            ),
        };
    }

    fn resolveDispatchReceiverExprVar(
        self: *const Lowerer,
        source_module_idx: u32,
        type_scope: *const TypeCloneScope,
        env: BindingEnv,
        receiver_expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
    ) DispatchReceiver {
        const receiver_var = self.lookupSpecializedExprVar(source_module_idx, env, receiver_expr_idx) orelse
            type_scope.var_map.get(ModuleEnv.varFrom(receiver_expr_idx)) orelse
            ModuleEnv.varFrom(receiver_expr_idx);
        return self.resolveDispatchReceiverVar(source_module_idx, type_scope, receiver_var, method_name);
    }

    fn lookupScopedFunctionArgVar(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        fn_var: Var,
        arg_index: usize,
    ) ?Var {
        const scoped_fn_var = type_scope.var_map.get(fn_var) orelse fn_var;
        return self.lookupFunctionArgVar(module_idx, scoped_fn_var, arg_index);
    }

    fn resolveDispatchReceiverVar(
        self: *const Lowerer,
        source_module_idx: u32,
        type_scope: *const TypeCloneScope,
        receiver_var: Var,
        method_name: base.Ident.Idx,
    ) DispatchReceiver {
        const source_env = self.ctx.env(source_module_idx);
        const scoped_receiver_var = type_scope.var_map.get(receiver_var) orelse receiver_var;
        const resolved = source_env.types.resolveVar(scoped_receiver_var);
        return switch (resolved.desc.content) {
            .alias => |alias| self.resolveDispatchReceiverVar(source_module_idx, type_scope, source_env.types.getAliasBackingVar(alias), method_name),
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| {
                    const target_module_idx = findModuleIdxByName(self.ctx.all_module_envs, source_env.getIdent(nominal.origin_module));
                    return .{
                        .target_module_idx = target_module_idx,
                        .target_env = self.ctx.env(target_module_idx),
                        .type_ident = nominal.ident.ident_idx,
                    };
                },
                else => debugPanic(
                    "monotype static dispatch invariant violated: receiver for method {s} resolved to non-nominal {s}",
                    .{ source_env.getIdent(method_name), @tagName(flat) },
                ),
            },
            .err => debugPanic(
                "monotype static dispatch invariant violated: receiver for method {s} is erroneous during lowering",
                .{source_env.getIdent(method_name)},
            ),
            .flex => debugPanic(
                "monotype static dispatch invariant violated: receiver for method {s} remained flex during lowering",
                .{source_env.getIdent(method_name)},
            ),
            .rigid => debugPanic(
                "monotype static dispatch invariant violated: receiver for method {s} remained rigid during lowering",
                .{source_env.getIdent(method_name)},
            ),
        };
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
        if (env.get(.{ .module_idx = module_idx, .pattern_idx = @intFromEnum(pattern_idx) })) |entry| {
            return switch (entry) {
                .typed => |typed| typed.bind.symbol,
                .local_fn_source => |source| self.local_fn_groups.items[source.group_index].sources[source.source_index].source_symbol,
            };
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
    if (ident.eql(env.idents.u8) or ident.eql(env.idents.u8_type)) return .u8;
    if (ident.eql(env.idents.i8) or ident.eql(env.idents.i8_type)) return .i8;
    if (ident.eql(env.idents.u16) or ident.eql(env.idents.u16_type)) return .u16;
    if (ident.eql(env.idents.i16) or ident.eql(env.idents.i16_type)) return .i16;
    if (ident.eql(env.idents.u32) or ident.eql(env.idents.u32_type)) return .u32;
    if (ident.eql(env.idents.i32) or ident.eql(env.idents.i32_type)) return .i32;
    if (ident.eql(env.idents.u64) or ident.eql(env.idents.u64_type)) return .u64;
    if (ident.eql(env.idents.i64) or ident.eql(env.idents.i64_type)) return .i64;
    if (ident.eql(env.idents.u128) or ident.eql(env.idents.u128_type)) return .u128;
    if (ident.eql(env.idents.i128) or ident.eql(env.idents.i128_type)) return .i128;
    if (ident.eql(env.idents.f32) or ident.eql(env.idents.f32_type)) return .f32;
    if (ident.eql(env.idents.f64) or ident.eql(env.idents.f64_type)) return .f64;
    if (ident.eql(env.idents.dec) or ident.eql(env.idents.dec_type)) return .dec;
    return null;
}

fn isBuiltinBoolTagUnion(env: *const ModuleEnv, tags_slice: anytype) bool {
    if (tags_slice.len != 2) return false;

    var saw_true = false;
    var saw_false = false;
    for (0..tags_slice.len) |i| {
        const name = tags_slice.items(.name)[i];
        const args = env.types.sliceVars(tags_slice.items(.args)[i]);
        if (args.len != 0) return false;
        if (name.eql(env.idents.true_tag)) {
            saw_true = true;
            continue;
        }
        if (name.eql(env.idents.false_tag)) {
            saw_false = true;
            continue;
        }
        return false;
    }

    return saw_true and saw_false;
}

fn isBuiltinBoolTagUnionSlice(env: *const ModuleEnv, tags: []const type_mod.Tag) bool {
    if (tags.len != 2) return false;

    var saw_true = false;
    var saw_false = false;
    for (tags) |tag| {
        const args_len = tag.args.len;
        if (args_len != 0) return false;
        if (tag.name.eql(env.idents.true_tag)) {
            saw_true = true;
            continue;
        }
        if (tag.name.eql(env.idents.false_tag)) {
            saw_false = true;
            continue;
        }
        return false;
    }

    return saw_true and saw_false;
}

fn constraintsContainFromNumeral(
    env: *const ModuleEnv,
    range: types.StaticDispatchConstraint.SafeList.Range,
) bool {
    for (env.types.sliceStaticDispatchConstraints(range)) |constraint| {
        if (constraint.origin == .from_numeral) return true;
    }
    return false;
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

fn findModuleIdxByName(all_module_envs: []const *const ModuleEnv, target_name: []const u8) u32 {
    for (all_module_envs, 0..) |env, idx| {
        if (std.mem.eql(u8, moduleName(env), target_name)) return @intCast(idx);
    }

    debugPanic(
        "monotype static dispatch invariant violated: missing target module {s}",
        .{target_name},
    );
}

fn moduleName(env: *const ModuleEnv) []const u8 {
    if (!env.qualified_module_ident.isNone()) {
        return env.getIdent(env.qualified_module_ident);
    }
    return env.module_name;
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
