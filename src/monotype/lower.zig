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
const semantic_facts = @import("semantic_facts.zig");
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

const LoweredExprFact = struct {
    expr: ast.ExprId,
    ty: type_mod.TypeId,
    solved_var: Var,
};

const LoweredCall = struct {
    data: @FieldType(ast.Expr.Data, "call"),
    result_ty: type_mod.TypeId,
};

const ExplicitCallFact = semantic_facts.ExplicitCallFact;
const ExplicitCallTypeFact = semantic_facts.ExplicitCallTypeFact;
const ExplicitFunctionFact = semantic_facts.ExplicitFunctionFact;
const ExplicitFunctionTypeFact = semantic_facts.ExplicitFunctionTypeFact;
const ExprSourceFunctionFact = semantic_facts.ExprSourceFunctionFact;
const ArithmeticBinopFact = semantic_facts.ArithmeticBinopFact;
const ArithmeticBinopTypeFact = semantic_facts.ArithmeticBinopTypeFact;

const SpecializableCallChain = struct {
    func_expr_idx: CIR.Expr.Idx,
    arg_exprs: []const CIR.Expr.Idx,
    owned_args: ?[]CIR.Expr.Idx = null,

    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        if (self.owned_args) |owned| allocator.free(owned);
    }
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

    const LoweredClosure = struct {
        ty: type_mod.TypeId,
        data: @FieldType(ast.Expr.Data, "clos"),
    };

    const LocalFnSourceRef = struct {
        group_index: u32,
        source_index: u32,
    };

    const TypedBinding = struct {
        symbol: symbol_mod.Symbol,
        solved_var: ?Var = null,
    };

    const BindingValue = struct {
        typed: ?TypedBinding = null,
        local_fn_source: ?LocalFnSourceRef = null,
        local_fn_seed_var: ?Var = null,
    };

    const BindingEnv = std.AutoHashMap(PatternKey, BindingValue);

    fn putTypedBinding(
        self: *Lowerer,
        env: *BindingEnv,
        key: PatternKey,
        typed: TypedBinding,
    ) std.mem.Allocator.Error!void {
        _ = self;
        if (env.get(key)) |existing| {
            try env.put(key, .{
                .typed = typed,
                .local_fn_source = existing.local_fn_source,
                .local_fn_seed_var = existing.local_fn_seed_var,
            });
            return;
        }
        try env.put(key, .{ .typed = typed });
    }

    fn putLocalFnSourceBinding(
        self: *Lowerer,
        env: *BindingEnv,
        key: PatternKey,
        source: LocalFnSourceRef,
    ) std.mem.Allocator.Error!void {
        _ = self;
        if (env.get(key)) |existing| {
            try env.put(key, .{
                .typed = existing.typed,
                .local_fn_source = source,
                .local_fn_seed_var = existing.local_fn_seed_var,
            });
            return;
        }
        try env.put(key, .{ .local_fn_source = source });
    }

    fn putLocalFnSeedBinding(
        self: *Lowerer,
        env: *BindingEnv,
        key: PatternKey,
        seed_var: Var,
    ) std.mem.Allocator.Error!void {
        _ = self;
        if (env.get(key)) |existing| {
            try env.put(key, .{
                .typed = existing.typed,
                .local_fn_source = existing.local_fn_source,
                .local_fn_seed_var = seed_var,
            });
            return;
        }
        try env.put(key, .{ .local_fn_seed_var = seed_var });
    }

    const LocalFnSource = struct {
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
        seed_var: Var,
        source_symbol: symbol_mod.Symbol,
    };

    const LocalFnPending = struct {
        source_index: u32,
        arg_tys: []type_mod.TypeId,
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
            for (self.pending.items) |pending| allocator.free(pending.arg_tys);
            self.pending.deinit(allocator);
        }
    };

    const TypeCloneScope = struct {
        const TypeKey = semantic_facts.TypeKey;
        const ExprKey = semantic_facts.ExprKey;
        const PatternTypeKey = semantic_facts.PatternTypeKey;
        const RecordDestructKey = semantic_facts.RecordDestructKey;

        var_map: std.AutoHashMap(Var, Var),
        active_type_cache: std.AutoHashMap(TypeKey, type_mod.TypeId),
        provisional_type_cache: std.AutoHashMap(TypeKey, type_mod.TypeId),
        type_cache: std.AutoHashMap(TypeKey, type_mod.TypeId),
        facts: semantic_facts.Mutable,
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
                var provisional_iter = scope.provisional_type_cache.iterator();
                while (provisional_iter.next()) |entry| {
                    self.provisional_type_cache.put(entry.key_ptr.*, entry.value_ptr.*) catch unreachable;
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
            self.active_type_cache = std.AutoHashMap(TypeKey, type_mod.TypeId).init(allocator);
            self.provisional_type_cache = std.AutoHashMap(TypeKey, type_mod.TypeId).init(allocator);
            self.type_cache = std.AutoHashMap(TypeKey, type_mod.TypeId).init(allocator);
            self.facts = semantic_facts.Mutable.init(allocator);
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
            self.facts.deinit();
            self.active_type_cache.deinit();
            self.provisional_type_cache.deinit();
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
        try self.finalizeProgramTypes();

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

    fn publishMonotypeType(self: *Lowerer, ty: type_mod.TypeId) std.mem.Allocator.Error!type_mod.TypeId {
        const published = try self.ctx.types.canonicalizePublished(ty);
        if (comptime builtin.mode == .Debug) {
            if (self.ctx.types.publishedContainsPlaceholder(published)) {
                debugPanic(
                    "monotype invariant violated: publishMonotypeType leaked builder placeholder for type {d}",
                    .{@intFromEnum(published)},
                );
            }
        }
        return published;
    }

    fn finalizeTypedSymbol(self: *Lowerer, bind: *ast.TypedSymbol) std.mem.Allocator.Error!void {
        bind.ty = try self.publishMonotypeType(bind.ty);
    }

    const FinalizeVisited = struct {
        defs: []bool,
        exprs: []bool,
        pats: []bool,
        stmts: []bool,
        branches: []bool,
    };

    fn markVisited(visited: []bool, idx: usize) bool {
        if (visited[idx]) return true;
        visited[idx] = true;
        return false;
    }

    fn finalizeLetDef(self: *Lowerer, def: *ast.LetDef, visited: *FinalizeVisited) std.mem.Allocator.Error!void {
        switch (def.*) {
            .let_fn => |*let_fn| {
                try self.finalizeTypedSymbol(&let_fn.bind);
                try self.finalizeTypedSymbol(&let_fn.arg);
                try self.finalizeExprById(let_fn.body, visited);
            },
            .let_val => |*let_val| {
                try self.finalizeTypedSymbol(&let_val.bind);
                try self.finalizeExprById(let_val.body, visited);
            },
        }
    }

    fn finalizePatById(self: *Lowerer, pat_id: ast.PatId, visited: *FinalizeVisited) std.mem.Allocator.Error!void {
        const idx = @intFromEnum(pat_id);
        if (markVisited(visited.pats, idx)) return;

        var pat = &self.program.store.pats.items[idx];
        pat.ty = try self.publishMonotypeType(pat.ty);
        switch (pat.data) {
            .tag => |tag| {
                for (self.program.store.slicePatSpan(tag.args)) |arg_pat| {
                    try self.finalizePatById(arg_pat, visited);
                }
            },
            else => {},
        }
    }

    fn finalizeStmtById(self: *Lowerer, stmt_id: ast.StmtId, visited: *FinalizeVisited) std.mem.Allocator.Error!void {
        const idx = @intFromEnum(stmt_id);
        if (markVisited(visited.stmts, idx)) return;

        const stmt = &self.program.store.stmts.items[idx];
        switch (stmt.*) {
            .local_fn => |*let_fn| {
                try self.finalizeTypedSymbol(&let_fn.bind);
                try self.finalizeTypedSymbol(&let_fn.arg);
                try self.finalizeExprById(let_fn.body, visited);
            },
            .decl => |*decl| {
                try self.finalizeTypedSymbol(&decl.bind);
                try self.finalizeExprById(decl.body, visited);
            },
            .var_decl => |*decl| {
                try self.finalizeTypedSymbol(&decl.bind);
                try self.finalizeExprById(decl.body, visited);
            },
            .reassign => |reassign| try self.finalizeExprById(reassign.body, visited),
            .expr => |expr_id| try self.finalizeExprById(expr_id, visited),
            .debug => |expr_id| try self.finalizeExprById(expr_id, visited),
            .expect => |expr_id| try self.finalizeExprById(expr_id, visited),
            .return_ => |expr_id| try self.finalizeExprById(expr_id, visited),
            .for_ => |for_stmt| {
                try self.finalizePatById(for_stmt.patt, visited);
                try self.finalizeExprById(for_stmt.iterable, visited);
                try self.finalizeExprById(for_stmt.body, visited);
            },
            .while_ => |while_stmt| {
                try self.finalizeExprById(while_stmt.cond, visited);
                try self.finalizeExprById(while_stmt.body, visited);
            },
            else => {},
        }
    }

    fn finalizeBranchById(self: *Lowerer, branch_id: ast.BranchId, visited: *FinalizeVisited) std.mem.Allocator.Error!void {
        const idx = @intFromEnum(branch_id);
        if (markVisited(visited.branches, idx)) return;

        const branch = self.program.store.branches.items[idx];
        try self.finalizePatById(branch.pat, visited);
        try self.finalizeExprById(branch.body, visited);
    }

    fn finalizeExprById(self: *Lowerer, expr_id: ast.ExprId, visited: *FinalizeVisited) std.mem.Allocator.Error!void {
        const idx = @intFromEnum(expr_id);
        if (markVisited(visited.exprs, idx)) return;

        var expr = &self.program.store.exprs.items[idx];
        expr.ty = try self.publishMonotypeType(expr.ty);
        switch (expr.data) {
            .tag => |tag| {
                for (self.program.store.sliceExprSpan(tag.args)) |arg_expr| {
                    try self.finalizeExprById(arg_expr, visited);
                }
            },
            .record => |fields| {
                for (self.program.store.sliceFieldExprSpan(fields)) |field| {
                    try self.finalizeExprById(field.value, visited);
                }
            },
            .access => |access| try self.finalizeExprById(access.record, visited),
            .let_ => |*let_expr| {
                try self.finalizeLetDef(&let_expr.def, visited);
                try self.finalizeExprById(let_expr.rest, visited);
            },
            .clos => |*clos| {
                try self.finalizeTypedSymbol(&clos.arg);
                try self.finalizeExprById(clos.body, visited);
            },
            .call => |call| {
                try self.finalizeExprById(call.func, visited);
                try self.finalizeExprById(call.arg, visited);
            },
            .inspect => |inner| try self.finalizeExprById(inner, visited),
            .low_level => |low_level| {
                for (self.program.store.sliceExprSpan(low_level.args)) |arg_expr| {
                    try self.finalizeExprById(arg_expr, visited);
                }
            },
            .when => |when_expr| {
                try self.finalizeExprById(when_expr.cond, visited);
                for (self.program.store.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    try self.finalizeBranchById(branch_id, visited);
                }
            },
            .if_ => |if_expr| {
                try self.finalizeExprById(if_expr.cond, visited);
                try self.finalizeExprById(if_expr.then_body, visited);
                try self.finalizeExprById(if_expr.else_body, visited);
            },
            .block => |block| {
                for (self.program.store.sliceStmtSpan(block.stmts)) |stmt_id| {
                    try self.finalizeStmtById(stmt_id, visited);
                }
                try self.finalizeExprById(block.final_expr, visited);
            },
            .tuple => |elems| {
                for (self.program.store.sliceExprSpan(elems)) |elem_expr| {
                    try self.finalizeExprById(elem_expr, visited);
                }
            },
            .tuple_access => |access| try self.finalizeExprById(access.tuple, visited),
            .list => |elems| {
                for (self.program.store.sliceExprSpan(elems)) |elem_expr| {
                    try self.finalizeExprById(elem_expr, visited);
                }
            },
            .return_ => |inner| try self.finalizeExprById(inner, visited),
            .for_ => |for_expr| {
                try self.finalizePatById(for_expr.patt, visited);
                try self.finalizeExprById(for_expr.iterable, visited);
                try self.finalizeExprById(for_expr.body, visited);
            },
            else => {},
        }
    }

    fn finalizeDefById(self: *Lowerer, def_id: ast.DefId, visited: *FinalizeVisited) std.mem.Allocator.Error!void {
        const idx = @intFromEnum(def_id);
        if (markVisited(visited.defs, idx)) return;

        var def = &self.program.store.defs.items[idx];
        try self.finalizeTypedSymbol(&def.bind);
        switch (def.value) {
            .fn_ => |*let_fn| {
                try self.finalizeTypedSymbol(&let_fn.bind);
                try self.finalizeTypedSymbol(&let_fn.arg);
                try self.finalizeExprById(let_fn.body, visited);
            },
            .val => |expr_id| try self.finalizeExprById(expr_id, visited),
            .run => |*run_def| {
                try self.finalizeTypedSymbol(&run_def.bind);
                try self.finalizeExprById(run_def.body, visited);
            },
        }
    }

    fn finalizeProgramTypes(self: *Lowerer) std.mem.Allocator.Error!void {
        var visited = FinalizeVisited{
            .defs = try self.allocator.alloc(bool, self.program.store.defs.items.len),
            .exprs = try self.allocator.alloc(bool, self.program.store.exprs.items.len),
            .pats = try self.allocator.alloc(bool, self.program.store.pats.items.len),
            .stmts = try self.allocator.alloc(bool, self.program.store.stmts.items.len),
            .branches = try self.allocator.alloc(bool, self.program.store.branches.items.len),
        };
        defer self.allocator.free(visited.defs);
        defer self.allocator.free(visited.exprs);
        defer self.allocator.free(visited.pats);
        defer self.allocator.free(visited.stmts);
        defer self.allocator.free(visited.branches);
        @memset(visited.defs, false);
        @memset(visited.exprs, false);
        @memset(visited.pats, false);
        @memset(visited.stmts, false);
        @memset(visited.branches, false);

        for (self.program.root_defs.items) |def_id| {
            try self.finalizeDefById(def_id, &visited);
        }
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
                const bind_var = ModuleEnv.varFrom(def.pattern);
                const bind_expected_var = try self.scopeVar(&type_scope, bind_var);
                try self.collectExprFactsWithExplicitResultVar(
                    module_idx,
                    &type_scope,
                    binding_env,
                    def.expr,
                    bind_expected_var,
                );
                try self.freezeLoweringSemanticTypes(module_idx, &type_scope);
                const bind_expected_ty = try self.requireExprTypeFact(module_idx, &type_scope, def.expr);
                const body = try self.lowerExprFactWithExpectedType(
                    module_idx,
                    &type_scope,
                    binding_env,
                    def.expr,
                    bind_expected_ty,
                    bind_expected_var,
                );
                const lowered = try self.program.store.addDef(.{
                    .bind = .{ .ty = body.ty, .symbol = bind_symbol },
                    .value = .{ .val = body.expr },
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
        const bind_var = ModuleEnv.varFrom(def.pattern);
        const bind_expected_var = try self.scopeVar(&type_scope, bind_var);
        try self.collectExprFactsWithExplicitResultVar(
            module_idx,
            &type_scope,
            binding_env,
            def.expr,
            bind_expected_var,
        );
        try self.freezeLoweringSemanticTypes(module_idx, &type_scope);
        const bind_expected_ty = try self.requireExprTypeFact(module_idx, &type_scope, def.expr);
        const body = try self.lowerExprFactWithExpectedType(
            module_idx,
            &type_scope,
            binding_env,
            def.expr,
            bind_expected_ty,
            bind_expected_var,
        );
        const lowered = try self.program.store.addDef(.{
            .bind = .{ .ty = body.ty, .symbol = bind_symbol },
            .value = .{ .run = .{
                .bind = .{ .ty = body.ty, .symbol = bind_symbol },
                .body = body.expr,
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
            try self.program.root_defs.append(self.allocator, def_id);
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

        var binding_env = BindingEnv.init(self.allocator);
        defer binding_env.deinit();

        const letfn = try self.lowerLambdaLikeDefWithEnv(
            pending.source.module_idx,
            &type_scope,
            binding_env,
            pending.specialized_symbol,
            def.expr,
            isRecursiveTopLevelDef(env, pending.source.def_idx),
            null,
            specialized_checker_var,
        );
        const lowered = try self.program.store.addDef(.{
            .bind = letfn.bind,
            .value = .{ .fn_ = letfn },
        });
        try self.emitted_defs_by_symbol.put(letfn.bind.symbol, lowered);
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
        bind_symbol: symbol_mod.Symbol,
        expr_idx: CIR.Expr.Idx,
        recursive: bool,
        source_seed_var: ?Var,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!ast.LetFn {
        const env = self.ctx.env(module_idx);
        const expr = env.store.getExpr(expr_idx);
        const source_expr_var = ModuleEnv.varFrom(expr_idx);
        const source_fn_var = try self.scopeVar(type_scope, source_seed_var orelse source_expr_var);
        try type_scope.var_map.put(source_expr_var, source_fn_var);
        if (expected_var) |var_| {
            try self.unifySpecializedCheckerVars(module_idx, source_fn_var, var_);
        }
        var arg_index: usize = 0;
        while (true) : (arg_index += 1) {
            const source_arg_var = self.lookupFunctionArgVar(module_idx, source_expr_var, arg_index) orelse break;
            const scoped_arg_var = self.lookupFunctionArgVar(module_idx, source_fn_var, arg_index) orelse break;
            try type_scope.var_map.put(source_arg_var, scoped_arg_var);
        }
        const result_ty = try self.requireFunctionRetType(module_idx, type_scope, source_fn_var);
        const result_var = self.lookupFunctionNodeRetVar(module_idx, type_scope, source_fn_var);

        const lambda = switch (expr) {
            .e_lambda => |lambda| lambda,
            .e_closure => |closure| blk: {
                const lambda_expr = env.store.getExpr(closure.lambda_idx);
                if (lambda_expr != .e_lambda) unreachable;
                break :blk lambda_expr.e_lambda;
            },
            .e_hosted_lambda => |hosted| {
                const arg_patterns = env.store.slicePatterns(hosted.args);
                const first_arg_ty = if (arg_patterns.len == 0)
                    try self.requireFunctionArgType(module_idx, type_scope, source_fn_var, 0)
                else
                    try self.requireFunctionArgType(module_idx, type_scope, source_fn_var, 0);
                if (hosted.args.span.len == 0) {
                    const unit_arg = try self.makeUnitArgWithType(first_arg_ty);
                    const body = try self.program.store.addExpr(.{
                        .ty = try self.requireExprTypeFact(module_idx, type_scope, hosted.body),
                        .data = .{ .runtime_error = try @constCast(env).insertString("TODO monotype hosted lambda") },
                    });
                    return .{
                        .recursive = recursive,
                        .bind = .{
                            .ty = try self.ctx.types.addType(.{
                                .func = .{
                                    .arg = unit_arg.ty,
                                    .ret = self.program.store.getExpr(body).ty,
                                },
                            }),
                            .symbol = bind_symbol,
                        },
                        .arg = unit_arg,
                        .body = body,
                    };
                }

                const first_arg = try self.bindLambdaArg(module_idx, type_scope, arg_patterns[0], first_arg_ty);
                const body = try self.lowerLambdaBodyWithPattern(
                    module_idx,
                    type_scope,
                    incoming_env,
                    first_arg,
                    try self.requireFunctionArgVar(module_idx, type_scope, source_fn_var, 0),
                    arg_patterns[0],
                    result_ty,
                    result_var,
                    arg_patterns.len == 1,
                    hosted.body,
                    if (arg_patterns.len == 1) null else arg_patterns[1..],
                    source_fn_var,
                    1,
                );

                return .{
                    .recursive = recursive,
                    .bind = .{
                        .ty = try self.ctx.types.addType(.{
                            .func = .{
                                .arg = first_arg.ty,
                                .ret = self.program.store.getExpr(body).ty,
                            },
                        }),
                        .symbol = bind_symbol,
                    },
                    .arg = first_arg,
                    .body = body,
                };
            },
            else => unreachable,
        };

        const arg_patterns = env.store.slicePatterns(lambda.args);
        const first_arg_ty = if (arg_patterns.len == 0)
            try self.requireFunctionArgType(module_idx, type_scope, source_fn_var, 0)
        else
            try self.requireFunctionArgType(module_idx, type_scope, source_fn_var, 0);
        const first_arg_pattern = if (arg_patterns.len == 0) null else arg_patterns[0];
        const first_arg = if (first_arg_pattern) |pattern_idx|
            try self.bindLambdaArg(module_idx, type_scope, pattern_idx, first_arg_ty)
        else
            try self.makeUnitArgWithType(first_arg_ty);

        const body = if (first_arg_pattern) |pattern_idx| blk: {
            break :blk try self.lowerLambdaBodyWithPattern(
                module_idx,
                type_scope,
                incoming_env,
                first_arg,
                try self.requireFunctionArgVar(module_idx, type_scope, source_fn_var, 0),
                pattern_idx,
                result_ty,
                result_var,
                arg_patterns.len <= 1,
                lambda.body,
                if (arg_patterns.len <= 1) null else arg_patterns[1..],
                source_fn_var,
                1,
            );
        } else if (arg_patterns.len <= 1) blk: {
            const body_expect = self.requireLambdaBodyReturnFact(module_idx, result_ty, result_var);
            try self.collectExprFactsWithExplicitResultVar(
                module_idx,
                type_scope,
                incoming_env,
                lambda.body,
                body_expect.solved_var,
            );
            try self.freezeLoweringSemanticTypes(module_idx, type_scope);
            break :blk try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                incoming_env,
                lambda.body,
                body_expect.ty,
                body_expect.solved_var,
            );
        } else try self.lowerCurriedClosureChain(
            module_idx,
            type_scope,
            incoming_env,
            source_fn_var,
            1,
            arg_patterns[1..],
            lambda.body,
        );

        return .{
            .recursive = recursive,
            .bind = .{
                .ty = try self.ctx.types.addType(.{
                    .func = .{
                        .arg = first_arg.ty,
                        .ret = self.program.store.getExpr(body).ty,
                    },
                }),
                .symbol = bind_symbol,
            },
            .arg = first_arg,
            .body = body,
        };
    }

    fn lowerCurriedClosureChain(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        source_fn_var: ?Var,
        next_arg_index: usize,
        remaining_arg_patterns: []const CIR.Pattern.Idx,
        body_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.ExprId {
        std.debug.assert(remaining_arg_patterns.len > 0);
        const first_arg_ty = try self.requireFunctionArgType(module_idx, type_scope, source_fn_var, next_arg_index);
        const source_result_var = self.lookupFunctionNodeRetVar(module_idx, type_scope, source_fn_var) orelse debugPanic(
            "monotype lambda invariant violated: missing explicit curried source function return fact in module {d}",
            .{module_idx},
        );
        const source_result_ty = try self.requireFunctionRetType(module_idx, type_scope, source_fn_var);
        const remaining_after_current = remaining_arg_patterns.len - 1;
        const result_ty = if (remaining_after_current == 0)
            source_result_ty
        else
            try self.buildRemainingCurriedClosureTypeFromExplicitFacts(
                module_idx,
                type_scope,
                source_fn_var,
                next_arg_index + 1,
                remaining_after_current,
                source_result_ty,
            );
        const final_result_var = if (remaining_after_current == 0) source_result_var else null;
        const first_pattern = remaining_arg_patterns[0];
        const first_arg = try self.bindLambdaArg(
            module_idx,
            type_scope,
            first_pattern,
            first_arg_ty,
        );
        const body = try self.lowerLambdaBodyWithPattern(
            module_idx,
            type_scope,
            incoming_env,
            first_arg,
            try self.requireFunctionArgVar(module_idx, type_scope, source_fn_var, next_arg_index),
            first_pattern,
            result_ty,
            final_result_var,
            remaining_arg_patterns.len == 1,
            body_expr_idx,
            if (remaining_arg_patterns.len == 1) null else remaining_arg_patterns[1..],
            source_fn_var,
            next_arg_index + 1,
        );

        return try self.program.store.addExpr(.{
            .ty = try self.ctx.types.addType(.{
                .func = .{
                    .arg = first_arg.ty,
                    .ret = self.program.store.getExpr(body).ty,
                },
            }),
            .data = .{ .clos = .{
                .arg = first_arg,
                .body = body,
            } },
        });
    }

    fn lowerLambdaBodyWithPattern(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        arg_bind: ast.TypedSymbol,
        arg_solved_var: ?Var,
        pattern_idx: CIR.Pattern.Idx,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        is_final_arg: bool,
        body_expr_idx: CIR.Expr.Idx,
        remaining_arg_patterns: ?[]const CIR.Pattern.Idx,
        source_fn_var: ?Var,
        next_arg_index: usize,
    ) std.mem.Allocator.Error!ast.ExprId {
        try self.collectPatternFacts(module_idx, type_scope, pattern_idx);
        try self.recordPatternStructuralFactsFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            arg_bind.ty,
            arg_solved_var,
        );
        if (self.patternIsIrrefutableStructural(module_idx, pattern_idx)) {
            var body_env = try self.cloneEnv(incoming_env);
            defer body_env.deinit();
            var binding_decls = std.ArrayList(BindingDecl).empty;
            defer binding_decls.deinit(self.allocator);
            try self.collectStructuralBindingDeclsWithSolvedVar(
                module_idx,
                type_scope,
                arg_bind,
                arg_solved_var,
                pattern_idx,
                &body_env,
                &binding_decls,
            );
            const body = if (is_final_arg) blk: {
                const body_expect = self.requireLambdaBodyReturnFact(module_idx, result_ty, expected_result_var);
                try self.collectExprFactsWithExplicitResultVar(
                    module_idx,
                    type_scope,
                    body_env,
                    body_expr_idx,
                    body_expect.solved_var,
                );
                try self.freezeLoweringSemanticTypes(module_idx, type_scope);
                break :blk try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    body_env,
                    body_expr_idx,
                    body_expect.ty,
                    body_expect.solved_var,
                );
            } else try self.lowerCurriedClosureChain(
                module_idx,
                type_scope,
                body_env,
                source_fn_var,
                next_arg_index,
                remaining_arg_patterns.?,
                body_expr_idx,
            );
            return self.wrapExprWithBindingDecls(body, binding_decls.items);
        }

        const arg_expr = try self.makeVarExpr(arg_bind.ty, arg_bind.symbol);
        const predicate_mismatch_expr = try self.program.store.addExpr(.{
            .ty = result_ty,
            .data = .{ .runtime_error = try self.internStringLiteral("non-exhaustive function argument pattern") },
        });

        if (self.patternNeedsPredicateDesugaring(module_idx, pattern_idx)) {
            return self.lowerPredicateMatchBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                arg_expr,
                arg_bind.ty,
                arg_solved_var,
                pattern_idx,
                pattern_idx,
                if (is_final_arg) body_expr_idx else body_expr_idx,
                predicate_mismatch_expr,
            );
        }

        var branch_env = try self.cloneEnv(incoming_env);
        defer branch_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);
        const branch_pat = try self.lowerMatchPatWithType(
            module_idx,
            type_scope,
            pattern_idx,
            arg_bind.ty,
            arg_solved_var,
            &branch_env,
            &binding_decls,
        );
        const matched_body = if (is_final_arg) blk: {
            const body_expect = self.requireLambdaBodyReturnFact(module_idx, result_ty, expected_result_var);
            try self.collectExprFactsWithExplicitResultVar(
                module_idx,
                type_scope,
                branch_env,
                body_expr_idx,
                body_expect.solved_var,
            );
            try self.freezeLoweringSemanticTypes(module_idx, type_scope);
            break :blk try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                branch_env,
                body_expr_idx,
                body_expect.ty,
                body_expect.solved_var,
            );
        } else try self.lowerCurriedClosureChain(
            module_idx,
            type_scope,
            branch_env,
            source_fn_var,
            next_arg_index,
            remaining_arg_patterns.?,
            body_expr_idx,
        );
        const branch_result_ty = self.program.store.getExpr(matched_body).ty;
        const mismatch_expr = try self.program.store.addExpr(.{
            .ty = branch_result_ty,
            .data = .{ .runtime_error = try self.internStringLiteral("non-exhaustive function argument pattern") },
        });
        const then_expr = try self.wrapExprWithBindingDecls(matched_body, binding_decls.items);
        const else_pat = try self.program.store.addPat(.{
            .ty = arg_bind.ty,
            .data = .{ .var_ = symbol_mod.Symbol.none },
        });
        const branches = [_]ast.Branch{
            .{ .pat = branch_pat, .body = then_expr },
            .{ .pat = else_pat, .body = mismatch_expr },
        };
        return self.program.store.addExpr(.{
            .ty = branch_result_ty,
            .data = .{ .when = .{
                .cond = arg_expr,
                .branches = try self.program.store.addBranchSpan(&branches),
            } },
        });
    }

    fn buildRemainingCurriedClosureTypeFromExplicitFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        source_fn_var: ?Var,
        next_arg_index: usize,
        remaining_arity: usize,
        final_result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const function_var = source_fn_var orelse debugPanic(
            "monotype lambda invariant violated: missing explicit source function facts for curried closure type in module {d}",
            .{module_idx},
        );
        const total_args = self.functionArgCount(module_idx, type_scope, function_var);
        if (next_arg_index + remaining_arity > total_args) {
            return debugPanic(
                "monotype lambda invariant violated: lambda arg range [{d}, {d}) exceeded function arity {d} in module {d}",
                .{ next_arg_index, next_arg_index + remaining_arity, total_args, module_idx },
            );
        }

        if (remaining_arity == 0) {
            return final_result_ty;
        }
        const arg_tys = try self.allocator.alloc(type_mod.TypeId, remaining_arity);
        defer self.allocator.free(arg_tys);

        for (0..remaining_arity) |i| {
            arg_tys[i] = try self.requireFunctionArgType(module_idx, type_scope, function_var, next_arg_index + i);
        }

        var actual_ret_ty = final_result_ty;
        var i = remaining_arity;
        while (i > 0) : (i -= 1) {
            actual_ret_ty = try self.ctx.types.addType(.{
                .func = .{
                    .arg = arg_tys[i - 1],
                    .ret = actual_ret_ty,
                },
            });
        }
        return actual_ret_ty;
    }

    fn bindExplicitExprResultFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        body_expr_idx: CIR.Expr.Idx,
        result_var_opt: ?Var,
    ) std.mem.Allocator.Error!void {
        const result_var = result_var_opt orelse debugPanic(
            "monotype lambda invariant violated: final lambda body missing explicit function return fact in module {d}",
            .{module_idx},
        );
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = body_expr_idx,
        };
        if (type_scope.facts.expr_result_var_facts.get(key)) |existing| {
            if (existing != result_var) {
                debugPanic(
                    "monotype explicit expr fact invariant violated: conflicting explicit expr result vars in module {d}",
                    .{module_idx},
                );
            }
            return;
        }
        try type_scope.facts.expr_result_var_facts.put(key, result_var);
    }

    fn putExplicitCallFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        call_expr_idx: CIR.Expr.Idx,
        fact: ExplicitCallFact,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = call_expr_idx,
        };
        if (type_scope.facts.call_facts.get(key)) |existing| {
            defer {
                self.allocator.free(fact.arg_vars);
            }
            if (existing.fn_var != fact.fn_var or
                existing.result_var != fact.result_var or
                !std.mem.eql(Var, existing.arg_vars, fact.arg_vars))
            {
                debugPanic(
                    "monotype explicit call fact invariant violated: conflicting call facts in module {d}",
                    .{module_idx},
                );
            }
            return;
        }
        try type_scope.facts.call_facts.put(key, fact);
    }

    fn putExprSourceFunctionFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
        fact: ExprSourceFunctionFact,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        if (type_scope.facts.expr_source_function_facts.get(key)) |existing| {
            if (existing.seed_var != fact.seed_var or existing.arity != fact.arity) {
                debugPanic(
                    "monotype explicit source-function fact invariant violated: conflicting facts in module {d}",
                    .{module_idx},
                );
            }
            return;
        }
        try type_scope.facts.expr_source_function_facts.put(key, fact);
    }

    fn lookupExprSourceFunctionFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) ?ExprSourceFunctionFact {
        _ = self;
        return type_scope.facts.expr_source_function_facts.get(.{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        });
    }

    fn recordExprSourceFunctionFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        if (type_scope.facts.expr_source_function_facts.contains(key)) return;

        const current_env = self.ctx.env(module_idx);
        const seed_var_opt: ?Var = switch (current_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| blk: {
                if (env.get(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(lookup.pattern_idx),
                })) |entry| {
                    if (entry.local_fn_seed_var) |seed_var| {
                        break :blk seed_var;
                    }
                    if (entry.local_fn_source) |source| {
                        const group = self.local_fn_groups.items[source.group_index];
                        break :blk group.sources[source.source_index].seed_var;
                    }
                    return;
                }

                const symbol = self.lookupTopLevelSymbol(module_idx, lookup.pattern_idx) orelse return;
                const top_level = self.top_level_defs_by_symbol.get(symbol) orelse return;
                break :blk try self.copyTopLevelDefExprVarToModule(top_level.module_idx, top_level.def_idx, module_idx);
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = current_env.imports.getResolvedModule(lookup.module_idx) orelse return;
                break :blk try self.copyTopLevelDefExprVarToModule(target_module_idx, @enumFromInt(lookup.target_node_idx), module_idx);
            },
            else => return,
        };
        const seed_var = seed_var_opt orelse return;
        if (self.lookupFunctionRetVar(module_idx, seed_var) == null) return;
        const fact: ExprSourceFunctionFact = .{
            .seed_var = seed_var,
            .arity = self.functionArgCount(module_idx, type_scope, seed_var),
        };
        try self.putExprSourceFunctionFact(module_idx, type_scope, expr_idx, fact);
    }

    fn requireExplicitCallFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        call_expr_idx: CIR.Expr.Idx,
    ) ExplicitCallFact {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = call_expr_idx,
        };
        return type_scope.facts.call_facts.get(key) orelse
            debugPanic("monotype explicit call fact invariant violated: missing explicit call fact", .{});
    }

    fn putExplicitCallTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        call_expr_idx: CIR.Expr.Idx,
        fact: ExplicitCallTypeFact,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = call_expr_idx,
        };
        if (type_scope.facts.call_type_facts.get(key)) |existing| {
            defer {
                self.allocator.free(fact.arg_tys);
                self.allocator.free(fact.applied_result_tys);
            }
            if (existing.fn_ty != fact.fn_ty or
                existing.result_ty != fact.result_ty or
                !std.mem.eql(type_mod.TypeId, existing.arg_tys, fact.arg_tys) or
                !std.mem.eql(type_mod.TypeId, existing.applied_result_tys, fact.applied_result_tys))
            {
                debugPanic(
                    "monotype explicit call type fact invariant violated: conflicting typed call facts in module {d}",
                    .{module_idx},
                );
            }
            return;
        }
        try type_scope.facts.call_type_facts.put(key, fact);
    }

    fn requireExplicitCallTypeFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        call_expr_idx: CIR.Expr.Idx,
    ) ExplicitCallTypeFact {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = call_expr_idx,
        };
        return type_scope.facts.call_type_facts.get(key) orelse
            debugPanic("monotype explicit call type fact invariant violated: missing explicit typed call fact", .{});
    }

    fn freezeExplicitCallTypeFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!void {
        var iter = type_scope.facts.call_facts.keyIterator();
        while (iter.next()) |key_ptr| {
            if (type_scope.facts.call_type_facts.contains(key_ptr.*)) continue;

            const call_fact = self.requireExplicitCallFact(module_idx, type_scope, key_ptr.expr_idx);
            const fn_ty = try self.publishMonotypeType(try self.lowerInstantiatedType(module_idx, type_scope, call_fact.fn_var));
            const arg_tys = try self.allocator.alloc(type_mod.TypeId, call_fact.arg_vars.len);
            errdefer self.allocator.free(arg_tys);
            for (call_fact.arg_vars, 0..) |arg_var, i| {
                arg_tys[i] = try self.publishMonotypeType(try self.lowerInstantiatedType(module_idx, type_scope, arg_var));
            }

            const applied_result_tys = try self.collectCurriedAppliedResultTypes(fn_ty, call_fact.arg_vars.len);
            errdefer self.allocator.free(applied_result_tys);
            for (applied_result_tys) |*result_ty| {
                result_ty.* = try self.publishMonotypeType(result_ty.*);
            }

            try self.putExplicitCallTypeFact(module_idx, type_scope, key_ptr.expr_idx, .{
                .fn_ty = fn_ty,
                .result_ty = try self.requireExprTypeFact(module_idx, type_scope, key_ptr.expr_idx),
                .arg_tys = arg_tys,
                .applied_result_tys = applied_result_tys,
            });
        }
    }

    fn freezeArithmeticBinopTypeFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!void {
        var iter = type_scope.facts.arithmetic_binop_facts.keyIterator();
        while (iter.next()) |key_ptr| {
            if (type_scope.facts.arithmetic_binop_type_facts.contains(key_ptr.*)) continue;

            const fact = self.requireArithmeticBinopFact(module_idx, type_scope, key_ptr.expr_idx);
            try self.putArithmeticBinopTypeFact(module_idx, type_scope, key_ptr.expr_idx, .{
                .operand_ty = try self.publishMonotypeType(
                    try self.lowerInstantiatedType(module_idx, type_scope, fact.operand_var),
                ),
            });
        }
    }

    fn collectExprFactsWithExplicitResultVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        result_var: ?Var,
    ) std.mem.Allocator.Error!void {
        try self.bindExplicitExprResultFact(module_idx, type_scope, expr_idx, result_var);
        try self.collectExprFacts(module_idx, type_scope, env, expr_idx);
    }

    fn freezeLoweringSemanticTypes(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!void {
        try self.freezeExprTypeFacts(module_idx, type_scope);
        try self.freezeExprStructuralFacts(module_idx, type_scope);
        try self.freezeExplicitFunctionTypeFacts(module_idx, type_scope);
        try self.freezeExplicitCallTypeFacts(module_idx, type_scope);
        try self.freezeArithmeticBinopTypeFacts(module_idx, type_scope);
    }

    fn buildExplicitCallFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: Var,
        result_var: Var,
        applied_arg_count: usize,
    ) std.mem.Allocator.Error!ExplicitCallFact {
        try self.unifyAppliedFunctionResultVar(module_idx, type_scope, result_var, fn_var, applied_arg_count);

        const fn_fact = try self.ensureExplicitFunctionFact(module_idx, type_scope, fn_var);
        if (applied_arg_count > fn_fact.arg_vars.len) {
            debugPanic(
                "monotype explicit call fact invariant violated: applied {d} args to function with arity {d} in module {d}",
                .{ applied_arg_count, fn_fact.arg_vars.len, module_idx },
            );
        }

        const arg_vars = try self.allocator.dupe(Var, fn_fact.arg_vars[0..applied_arg_count]);
        errdefer self.allocator.free(arg_vars);

        return .{
            .fn_var = fn_var,
            .result_var = result_var,
            .arg_vars = arg_vars,
        };
    }

    fn requirePatternSourceTypeFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) type_mod.TypeId {
        _ = self;
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        return type_scope.facts.pattern_source_type_facts.get(key) orelse
            debugPanic("monotype explicit pattern fact invariant violated: missing explicit pattern source type fact", .{});
    }

    fn bindPatternSourceTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.facts.pattern_source_type_facts.get(key)) |existing| {
            if (existing != source_ty) {
                debugPanic(
                    "monotype explicit pattern fact invariant violated: conflicting pattern source type facts in module {d}",
                    .{module_idx},
                );
            }
            return;
        }
        try type_scope.facts.pattern_source_type_facts.put(key, source_ty);
    }

    fn bindExprFieldIndexFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
        field_index: u16,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        if (type_scope.facts.expr_field_index_facts.get(key)) |existing| {
            if (existing != field_index) {
                debugPanic("monotype explicit expr fact invariant violated: conflicting field-index facts", .{});
            }
            return;
        }
        try type_scope.facts.expr_field_index_facts.put(key, field_index);
    }

    fn requireExprFieldIndexFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) u16 {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        return type_scope.facts.expr_field_index_facts.get(key) orelse
            debugPanic("monotype explicit expr fact invariant violated: missing explicit field-index fact", .{});
    }

    fn bindExprTagDiscriminantFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
        discriminant: u16,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        if (type_scope.facts.expr_tag_discriminant_facts.get(key)) |existing| {
            if (existing != discriminant) {
                debugPanic("monotype explicit expr fact invariant violated: conflicting tag discriminant facts", .{});
            }
            return;
        }
        try type_scope.facts.expr_tag_discriminant_facts.put(key, discriminant);
    }

    fn requireExprTagDiscriminantFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) u16 {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        return type_scope.facts.expr_tag_discriminant_facts.get(key) orelse
            debugPanic("monotype explicit expr fact invariant violated: missing explicit tag discriminant fact", .{});
    }

    fn bindPatternTagDiscriminantFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        discriminant: u16,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.facts.pattern_tag_discriminant_facts.get(key)) |existing| {
            if (existing != discriminant) {
                debugPanic("monotype explicit pattern fact invariant violated: conflicting tag discriminant facts", .{});
            }
            return;
        }
        try type_scope.facts.pattern_tag_discriminant_facts.put(key, discriminant);
    }

    fn requirePatternTagDiscriminantFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) u16 {
        _ = self;
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        return type_scope.facts.pattern_tag_discriminant_facts.get(key) orelse
            debugPanic("monotype explicit pattern fact invariant violated: missing explicit tag discriminant fact", .{});
    }

    fn bindPatternListElemTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        elem_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.facts.pattern_list_elem_type_facts.get(key)) |existing| {
            if (existing != elem_ty) {
                debugPanic("monotype explicit pattern fact invariant violated: conflicting list element type facts", .{});
            }
            return;
        }
        try type_scope.facts.pattern_list_elem_type_facts.put(key, elem_ty);
    }

    fn requirePatternListElemTypeFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) type_mod.TypeId {
        _ = self;
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        return type_scope.facts.pattern_list_elem_type_facts.get(key) orelse
            debugPanic("monotype explicit pattern fact invariant violated: missing explicit list element type fact", .{});
    }

    fn bindRecordDestructFieldIndexFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        destruct_idx: CIR.Pattern.RecordDestruct.Idx,
        field_index: u16,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.RecordDestructKey = .{
            .module_idx = module_idx,
            .destruct_idx = destruct_idx,
        };
        if (type_scope.facts.record_destruct_field_index_facts.get(key)) |existing| {
            if (existing != field_index) {
                debugPanic("monotype explicit pattern fact invariant violated: conflicting record destruct field-index facts", .{});
            }
            return;
        }
        try type_scope.facts.record_destruct_field_index_facts.put(key, field_index);
    }

    fn requireRecordDestructFieldIndexFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        destruct_idx: CIR.Pattern.RecordDestruct.Idx,
    ) u16 {
        _ = self;
        const key: TypeCloneScope.RecordDestructKey = .{
            .module_idx = module_idx,
            .destruct_idx = destruct_idx,
        };
        return type_scope.facts.record_destruct_field_index_facts.get(key) orelse
            debugPanic("monotype explicit pattern fact invariant violated: missing record destruct field-index fact", .{});
    }

    fn putArithmeticBinopFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
        fact: ArithmeticBinopFact,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        if (type_scope.facts.arithmetic_binop_facts.get(key)) |existing| {
            if (existing.operand_var != fact.operand_var or
                existing.result_var != fact.result_var)
            {
                debugPanic("monotype explicit arithmetic fact invariant violated: conflicting arithmetic facts", .{});
            }
            return;
        }
        try type_scope.facts.arithmetic_binop_facts.put(key, fact);
    }

    fn putArithmeticBinopTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
        fact: ArithmeticBinopTypeFact,
    ) std.mem.Allocator.Error!void {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        if (type_scope.facts.arithmetic_binop_type_facts.get(key)) |existing| {
            if (existing.operand_ty != fact.operand_ty) {
                debugPanic("monotype explicit arithmetic type fact invariant violated: conflicting arithmetic type facts", .{});
            }
            return;
        }
        try type_scope.facts.arithmetic_binop_type_facts.put(key, fact);
    }

    fn requireArithmeticBinopFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) ArithmeticBinopFact {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        return type_scope.facts.arithmetic_binop_facts.get(key) orelse
            debugPanic("monotype explicit arithmetic fact invariant violated: missing arithmetic fact", .{});
    }

    fn requireArithmeticBinopTypeFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) ArithmeticBinopTypeFact {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        return type_scope.facts.arithmetic_binop_type_facts.get(key) orelse
            debugPanic("monotype explicit arithmetic type fact invariant violated: missing arithmetic type fact", .{});
    }

    fn collectSourceSeededCallFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        call_expr_idx: CIR.Expr.Idx,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        explicit_result_var: ?Var,
    ) std.mem.Allocator.Error!?ExplicitCallFact {
        const chain = try self.resolveSpecializableCallChain(module_idx, type_scope, env, func_expr_idx, arg_exprs);
        try self.recordExprSourceFunctionFact(module_idx, type_scope, env, chain.func_expr_idx);
        const source_fact = self.lookupExprSourceFunctionFact(module_idx, type_scope, chain.func_expr_idx) orelse return null;
        const source_func_var = source_fact.seed_var;

        try self.collectExprFacts(module_idx, type_scope, env, chain.func_expr_idx);

        var call_scope: TypeCloneScope = undefined;
        call_scope.initCloneAll(self.allocator, @constCast(self.ctx.env(module_idx)));
        defer call_scope.deinit();

        const cloned_func_var = try call_scope.instantiator.instantiateVar(source_func_var);
        const call_result_var = explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, call_expr_idx);

        for (chain.arg_exprs, 0..) |arg_expr_idx, i| {
            const cloned_arg_var = try self.requireFunctionArgVar(module_idx, &call_scope, cloned_func_var, i);
            try self.collectExprFacts(module_idx, type_scope, env, arg_expr_idx);
            if (try self.specializeFunctionArgumentFromExpectedType(module_idx, type_scope, env, arg_expr_idx, cloned_arg_var)) {
                continue;
            }
            const actual_arg_var = try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, arg_expr_idx);
            try self.unifySpecializedCheckerVars(module_idx, cloned_arg_var, actual_arg_var);
        }

        const fact = try self.buildExplicitCallFact(
            module_idx,
            &call_scope,
            cloned_func_var,
            call_result_var,
            chain.arg_exprs.len,
        );
        try self.putExplicitCallFact(module_idx, type_scope, call_expr_idx, fact);
        return fact;
    }

    fn collectRecordedMethodCallFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
        explicit_result_var: ?Var,
    ) std.mem.Allocator.Error!ExplicitCallFact {
        const args = self.getRecordedMethodArgs(module_idx, expr_idx, method_name);
        const target = if (args.implicit_receiver) |receiver_expr_idx| blk: {
            const receiver_var = try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, receiver_expr_idx);
            break :blk self.resolveMonomorphicDispatchTargetFromReceiverVar(
                module_idx,
                type_scope,
                receiver_var,
                method_name,
            );
        } else self.lookupResolvedDispatchTarget(module_idx, expr_idx) orelse
            self.resolveMonomorphicDispatchTarget(module_idx, type_scope, env, expr_idx, method_name);
        const source_fn_var = try self.copyTopLevelDefExprVarToModule(
            target.module_idx,
            target.def_idx,
            module_idx,
        );

        var call_scope: TypeCloneScope = undefined;
        call_scope.initCloneAll(self.allocator, @constCast(self.ctx.env(module_idx)));
        defer call_scope.deinit();

        const cloned_func_var = try call_scope.instantiator.instantiateVar(source_fn_var);
        const result_var = explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
        try self.unifySpecializedRecordedMethodArgsWithActuals(
            module_idx,
            type_scope,
            env,
            cloned_func_var,
            args,
        );
        const applied_arg_count: usize = args.explicit_args.len + if (args.implicit_receiver == null) @as(usize, 0) else 1;
        const fact = try self.buildExplicitCallFact(
            module_idx,
            &call_scope,
            cloned_func_var,
            result_var,
            applied_arg_count,
        );
        try self.putExplicitCallFact(module_idx, type_scope, expr_idx, fact);
        return fact;
    }

    fn exprNeedsExplicitCallFact(expr: CIR.Expr) bool {
        return switch (expr) {
            .e_call, .e_type_var_dispatch => true,
            .e_dot_access => |dot| dot.args != null,
            else => false,
        };
    }

    fn requireLambdaBodyReturnFact(
        self: *const Lowerer,
        module_idx: u32,
        result_ty: type_mod.TypeId,
        result_var_opt: ?Var,
    ) ExpectedTypeFact {
        _ = self;
        return .{
            .ty = result_ty,
            .solved_var = result_var_opt orelse debugPanic(
                "monotype lambda invariant violated: final lambda body missing explicit function return fact in module {d}",
                .{module_idx},
            ),
        };
    }

    fn lookupLambdaReturnExpectation(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        lambda_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ExpectedTypeFact {
        const lambda_var = try self.scopedExprResultVar(module_idx, type_scope, env, lambda_expr_idx);
        const result_var = self.lookupFunctionNodeRetVar(module_idx, type_scope, lambda_var) orelse debugPanic(
            "monotype return invariant violated: lambda {} in module {d} is missing an explicit return fact",
            .{ lambda_expr_idx, module_idx },
        );
        return .{
            .ty = try self.requireFunctionRetType(module_idx, type_scope, lambda_var),
            .solved_var = result_var,
        };
    }

    fn lowerExprFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!LoweredExprFact {
        const lowered = try self.lowerExpr(module_idx, type_scope, env, expr_idx);
        return .{
            .expr = lowered,
            .ty = self.program.store.getExpr(lowered).ty,
            .solved_var = self.requireExprResultFact(module_idx, type_scope, expr_idx),
        };
    }

    fn lowerExprFactWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!LoweredExprFact {
        if (expected_var) |result_var| {
            const actual = self.requireExprResultFact(module_idx, type_scope, expr_idx);
            if (actual != result_var) {
                debugPanic(
                    "monotype explicit expr fact invariant violated: lowering expected expr {d} in module {d} to reuse explicit result var {d}, but collected facts stored {d}",
                    .{ @intFromEnum(expr_idx), module_idx, @intFromEnum(result_var), @intFromEnum(actual) },
                );
            }
        }
        const lowered = try self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            expr_idx,
            expected_ty,
            expected_var,
        );
        return .{
            .expr = lowered,
            .ty = self.program.store.getExpr(lowered).ty,
            .solved_var = expected_var orelse self.requireExprResultFact(module_idx, type_scope, expr_idx),
        };
    }

    fn exprResultVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!Var {
        _ = env;
        return self.requireExprResultFact(module_idx, type_scope, expr_idx);
    }

    fn lowerExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.ExprId {
        try self.freezeLoweringSemanticTypes(module_idx, type_scope);
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
                ModuleEnv.varFrom(expr_idx),
            ),
            .e_nominal_external => |nominal| return self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                nominal.backing_expr,
                try self.lowerExprType(module_idx, type_scope, env, expr_idx, expr),
                ModuleEnv.varFrom(expr_idx),
            ),
            else => {},
        }

        if (expr == .e_call) {
            if (self.callRootCalleeIsRuntimeError(module_idx, expr.e_call.func)) {
                return self.lowerErroneousExprWithType(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    try self.lowerExprType(module_idx, type_scope, env, expr_idx, expr),
                );
            }

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
            );
            return try self.program.store.addExpr(.{
                .ty = lowered_call.result_ty,
                .data = .{ .call = lowered_call.data },
            });
        }

        const ty = try self.lowerExprType(module_idx, type_scope, env, expr_idx, expr);

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
            .e_lookup_local => |lookup| {
                const symbol = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, ty, null);
                return try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_lookup_external => |lookup| {
                const symbol = try self.lookupOrSpecializeExternal(module_idx, lookup, ty, null);
                return try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_lookup_required => |_| debugTodo("monotype.lowerExpr required lookup"),
            .e_call => unreachable,
            .e_lambda => |lambda| {
                const lowered = try self.lowerAnonymousClosure(module_idx, type_scope, env, expr_idx, ty, lambda.args, lambda.body, null);
                return try self.program.store.addExpr(.{
                    .ty = lowered.ty,
                    .data = .{ .clos = lowered.data },
                });
            },
            .e_closure => |closure| {
                const lowered = try self.lowerClosureExpr(module_idx, type_scope, env, expr_idx, ty, closure, null);
                return try self.program.store.addExpr(.{
                    .ty = lowered.ty,
                    .data = .{ .clos = lowered.data },
                });
            },
            .e_hosted_lambda => |_| debugTodo("monotype.lowerExpr hosted lambda"),
            .e_record => |record| return self.lowerRecordExpr(
                module_idx,
                type_scope,
                env,
                ty,
                try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                record,
            ),
            .e_empty_record => .unit,
            .e_tuple => |tuple| .{ .tuple = try self.lowerTupleExprsWithExpectedType(
                module_idx,
                type_scope,
                env,
                ty,
                try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                tuple.elems,
            ) },
            .e_list => |list| .{ .list = try self.lowerListExprsWithExpectedType(
                module_idx,
                type_scope,
                env,
                ty,
                try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                list.elems,
            ) },
            .e_empty_list => .{ .list = ast.Span(ast.ExprId).empty() },
            .e_if => |if_expr| .{ .if_ = try self.lowerIfExpr(module_idx, type_scope, env, if_expr) },
            .e_match => |match_expr| try self.lowerMatchExprData(module_idx, type_scope, env, ty, null, match_expr),
            .e_block => |block| {
                const lowered = try self.lowerBlockExpr(module_idx, type_scope, env, block.stmts, block.final_expr);
                return try self.program.store.addExpr(.{
                    .ty = self.program.store.getExpr(lowered.final_expr).ty,
                    .data = .{ .block = lowered },
                });
            },
            .e_binop => |binop| blk: {
                if (binop.op == .ne) {
                    const eq_args = try self.lowerHomogeneousBinopArgs(
                        module_idx,
                        type_scope,
                        env,
                        try self.requireExprTypeFact(module_idx, type_scope, binop.lhs),
                        try self.scopedExprResultVar(module_idx, type_scope, env, binop.lhs),
                        binop.lhs,
                        binop.rhs,
                    );
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
                const arithmetic_fact = switch (binop.op) {
                    .add, .sub, .mul, .div, .rem, .div_trunc, .lt, .gt, .le, .ge, .eq => self.requireArithmeticBinopFact(module_idx, type_scope, expr_idx),
                    else => null,
                };
                const arithmetic_type_fact = switch (binop.op) {
                    .add, .sub, .mul, .div, .rem, .div_trunc, .lt, .gt, .le, .ge, .eq => self.requireArithmeticBinopTypeFact(module_idx, type_scope, expr_idx),
                    else => null,
                };
                break :blk .{ .low_level = .{
                    .op = binopToLowLevel(binop.op),
                    .args = try self.lowerHomogeneousBinopArgs(
                        module_idx,
                        type_scope,
                        env,
                        switch (binop.op) {
                            .add, .sub, .mul, .div, .rem, .div_trunc, .lt, .gt, .le, .ge, .eq => arithmetic_type_fact.?.operand_ty,
                            else => unreachable,
                        },
                        switch (binop.op) {
                            .add, .sub, .mul, .div, .rem, .div_trunc, .lt, .gt, .le, .ge, .eq => arithmetic_fact.?.operand_var,
                            else => unreachable,
                        },
                        binop.lhs,
                        binop.rhs,
                    ),
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
                    const lowered_call = try self.lowerRecordedMethodCall(
                        module_idx,
                        type_scope,
                        env,
                        expr_idx,
                        dot.field_name,
                    );
                    return try self.program.store.addExpr(.{
                        .ty = lowered_call.result_ty,
                        .data = .{ .call = lowered_call.data },
                    });
                }
                break :blk .{ .access = .{
                    .record = try self.lowerExpr(module_idx, type_scope, env, dot.receiver),
                    .field = try self.ctx.copyExecutableIdent(module_idx, dot.field_name),
                    .field_index = self.requireExprFieldIndexFact(module_idx, type_scope, expr_idx),
                } };
            },
            .e_tag => |tag| try self.lowerTagExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                ty,
                try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                tag.name,
                tag.args,
            ),
            .e_zero_argument_tag => |tag| try self.lowerTagExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                ty,
                try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                tag.name,
                .{ .span = .{ .start = 0, .len = 0 } },
            ),
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
            .e_return => |ret| blk: {
                const expected = try self.lookupLambdaReturnExpectation(module_idx, type_scope, env, ret.lambda);
                break :blk .{ .return_ = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    ret.expr,
                    expected.ty,
                    expected.solved_var,
                ) };
            },
            .e_for => |for_expr| .{ .for_ = try self.lowerForExpr(module_idx, type_scope, env, for_expr.patt, for_expr.expr, for_expr.body) },
            .e_run_low_level => |ll| .{ .low_level = .{
                .op = ll.op,
                .args = try self.lowerExprList(module_idx, type_scope, env, ll.args),
            } },
            .e_type_var_dispatch => |dispatch| {
                const lowered_call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    dispatch.method_name,
                );
                return try self.program.store.addExpr(.{
                    .ty = lowered_call.result_ty,
                    .data = .{ .call = lowered_call.data },
                });
            },
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
        return try self.ctx.types.internResolved(.{ .primitive = prim });
    }

    fn makeUnitType(self: *Lowerer) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.ctx.types.internResolved(.{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } });
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
        elem_ty: type_mod.TypeId,
        elem_index: usize,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.program.store.addExpr(.{
            .ty = elem_ty,
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

        return try self.ctx.types.internResolved(.{ .record = .{
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
    ) std.mem.Allocator.Error!LoweredClosure {
        const cir_env = self.ctx.env(module_idx);
        const arg_patterns = cir_env.store.slicePatterns(args_span);
        const source_expr_var = ModuleEnv.varFrom(source_expr_idx);
        const source_fn_var = try self.scopeVar(type_scope, source_expr_var);
        try type_scope.var_map.put(source_expr_var, source_fn_var);
        if (expected_var) |var_| {
            try self.unifySpecializedCheckerVars(module_idx, source_fn_var, var_);
        }
        const first_arg_ty = if (arg_patterns.len == 0)
            self.requireFunctionType(closure_ty).arg
        else
            try self.requireFunctionArgType(module_idx, type_scope, source_fn_var, 0);
        const result_ty = try self.requireFunctionRetType(module_idx, type_scope, source_fn_var);
        const result_var = self.lookupFunctionNodeRetVar(module_idx, type_scope, source_fn_var);
        const first_arg_pattern = if (arg_patterns.len == 0) null else arg_patterns[0];
        const arg = if (first_arg_pattern) |pattern_idx|
            try self.bindLambdaArg(module_idx, type_scope, pattern_idx, first_arg_ty)
        else
            try self.makeUnitArgWithType(first_arg_ty);

        var body_env = try self.cloneEnv(env);
        defer body_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);
        if (first_arg_pattern) |pattern_idx| {
            try self.collectStructuralBindingDeclsWithSolvedVar(
                module_idx,
                type_scope,
                arg,
                try self.requireFunctionArgVar(module_idx, type_scope, source_fn_var, 0),
                pattern_idx,
                &body_env,
                &binding_decls,
            );
        }

        const body = if (arg_patterns.len <= 1) blk: {
            const body_expect = self.requireLambdaBodyReturnFact(module_idx, result_ty, result_var);
            try self.collectExprFactsWithExplicitResultVar(
                module_idx,
                type_scope,
                body_env,
                body_expr_idx,
                body_expect.solved_var,
            );
            try self.freezeLoweringSemanticTypes(module_idx, type_scope);
            break :blk try self.wrapExprWithBindingDecls(
                try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    body_env,
                    body_expr_idx,
                    body_expect.ty,
                    body_expect.solved_var,
                ),
                binding_decls.items,
            );
        } else try self.wrapExprWithBindingDecls(
            try self.lowerCurriedClosureChain(
                module_idx,
                type_scope,
                body_env,
                source_fn_var,
                1,
                arg_patterns[1..],
                body_expr_idx,
            ),
            binding_decls.items,
        );

        return .{
            .ty = try self.ctx.types.addType(.{
                .func = .{
                    .arg = arg.ty,
                    .ret = self.program.store.getExpr(body).ty,
                },
            }),
            .data = .{
                .arg = arg,
                .body = body,
            },
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
    ) std.mem.Allocator.Error!LoweredClosure {
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
        if (self.matchExprNeedsPredicateDesugaring(module_idx, match_expr)) {
            return .{ .block = try self.lowerPredicateMatchExpr(module_idx, type_scope, env, result_ty, expected_result_var, match_expr) };
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
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign, .underscore => true,
            .as => |as_pat| self.patternIsIrrefutableStructural(module_idx, as_pat.pattern),
            .record_destructure => |record| blk: {
                for (cir_env.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    if (!self.patternIsIrrefutableStructural(module_idx, cir_env.store.getRecordDestruct(destruct_idx).kind.toPatternIdx())) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (cir_env.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                    if (!self.patternIsIrrefutableStructural(module_idx, elem_pattern_idx)) break :blk false;
                }
                break :blk true;
            },
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

        const scrutinee = try self.lowerExprFact(module_idx, type_scope, env, match_expr.cond);

        var branch_env = try self.cloneEnv(env);
        defer branch_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);

        try self.bindPatternFromSourceExpr(
            module_idx,
            type_scope,
            branch_pattern.pattern,
            scrutinee.expr,
            scrutinee.ty,
            scrutinee.solved_var,
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
        const cond = try self.lowerExprFact(module_idx, type_scope, env, match_expr.cond);

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
                    cond.ty,
                    cond.solved_var,
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
            .cond = cond.expr,
            .branches = try self.program.store.addBranchSpan(out),
        };
    }

    fn matchExprNeedsPredicateDesugaring(
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
            if (self.patternNeedsPredicateDesugaring(module_idx, branch_pattern.pattern)) return true;
        }
        return false;
    }

    fn patternNeedsPredicateDesugaring(self: *Lowerer, module_idx: u32, pattern_idx: CIR.Pattern.Idx) bool {
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        return switch (pattern) {
            .list => true,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => true,
            .record_destructure => |record| blk: {
                for (cir_env.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    if (self.patternNeedsPredicateDesugaring(module_idx, cir_env.store.getRecordDestruct(destruct_idx).kind.toPatternIdx())) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            .tuple => |tuple| blk: {
                for (cir_env.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                    if (self.patternNeedsPredicateDesugaring(module_idx, elem_pattern_idx)) break :blk true;
                }
                break :blk false;
            },
            .applied_tag => |tag| blk: {
                for (cir_env.store.slicePatterns(tag.args)) |arg_pattern_idx| {
                    if (self.patternNeedsPredicateDesugaring(module_idx, arg_pattern_idx)) break :blk true;
                }
                break :blk false;
            },
            .as => |as_pat| self.patternNeedsPredicateDesugaring(module_idx, as_pat.pattern),
            .nominal => |nominal| self.patternNeedsPredicateDesugaring(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.patternNeedsPredicateDesugaring(module_idx, nominal.backing_pattern),
            else => false,
        };
    }

    fn lowerPredicateMatchExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        const scrutinee = try self.lowerExprFact(module_idx, type_scope, env, match_expr.cond);
        const scrutinee_bind: ast.TypedSymbol = .{
            .ty = scrutinee.ty,
            .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
        };
        const scrutinee_decl = try self.program.store.addStmt(.{ .decl = .{
            .bind = scrutinee_bind,
            .body = scrutinee.expr,
        } });
        const scrutinee_expr = try self.makeVarExpr(scrutinee.ty, scrutinee_bind.symbol);

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
            current = try self.lowerPredicateMatchBranch(
                module_idx,
                type_scope,
                env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee.ty,
                scrutinee.solved_var,
                branch_pattern.pattern,
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

    fn lowerPredicateMatchBranch(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        scrutinee_solved_var: ?Var,
        match_pattern_idx: CIR.Pattern.Idx,
        bind_pattern_idx: CIR.Pattern.Idx,
        branch_value: CIR.Expr.Idx,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const pattern = self.ctx.env(module_idx).store.getPattern(match_pattern_idx);
        return switch (pattern) {
            .list => |list| try self.lowerExactListPatternBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                scrutinee_solved_var,
                bind_pattern_idx,
                list,
                branch_value,
                else_expr,
            ),
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => try self.lowerLiteralPatternBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                scrutinee_solved_var,
                pattern,
                bind_pattern_idx,
                branch_value,
                else_expr,
            ),
            .as => |as_pat| try self.lowerPredicateMatchBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                scrutinee_solved_var,
                as_pat.pattern,
                bind_pattern_idx,
                branch_value,
                else_expr,
            ),
            .nominal => |nominal| try self.lowerPredicateMatchBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                scrutinee_solved_var,
                nominal.backing_pattern,
                bind_pattern_idx,
                branch_value,
                else_expr,
            ),
            .nominal_external => |nominal| try self.lowerPredicateMatchBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                scrutinee_solved_var,
                nominal.backing_pattern,
                bind_pattern_idx,
                branch_value,
                else_expr,
            ),
            .assign, .underscore, .record_destructure, .tuple => try self.lowerStructuredPredicateMatchBranch(
                module_idx,
                type_scope,
                incoming_env,
                result_ty,
                expected_result_var,
                scrutinee_expr,
                scrutinee_ty,
                scrutinee_solved_var,
                match_pattern_idx,
                bind_pattern_idx,
                branch_value,
                else_expr,
            ),
            else => debugTodoPattern(pattern),
        };
    }

    fn lowerStructuredPredicateMatchBranch(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        scrutinee_solved_var: ?Var,
        match_pattern_idx: CIR.Pattern.Idx,
        bind_pattern_idx: CIR.Pattern.Idx,
        branch_value: CIR.Expr.Idx,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        var branch_env = try self.cloneEnv(incoming_env);
        defer branch_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);

        try self.bindPatternFromSourceExpr(
            module_idx,
            type_scope,
            bind_pattern_idx,
            scrutinee_expr,
            scrutinee_ty,
            scrutinee_solved_var,
            &branch_env,
            &binding_decls,
        );

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

        return self.lowerPatternGuardExpr(
            module_idx,
            type_scope,
            scrutinee_expr,
            scrutinee_ty,
            match_pattern_idx,
            then_expr,
            else_expr,
        );
    }

    fn lowerPatternGuardExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        pattern_idx: CIR.Pattern.Idx,
        then_expr: ast.ExprId,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign, .underscore => then_expr,
            .as => |as_pat| self.lowerPatternGuardExpr(
                module_idx,
                type_scope,
                scrutinee_expr,
                scrutinee_ty,
                as_pat.pattern,
                then_expr,
                else_expr,
            ),
            .nominal => |nominal| self.lowerPatternGuardExpr(
                module_idx,
                type_scope,
                scrutinee_expr,
                scrutinee_ty,
                nominal.backing_pattern,
                then_expr,
                else_expr,
            ),
            .nominal_external => |nominal| self.lowerPatternGuardExpr(
                module_idx,
                type_scope,
                scrutinee_expr,
                scrutinee_ty,
                nominal.backing_pattern,
                then_expr,
                else_expr,
            ),
            .record_destructure => |record| blk: {
                var current = then_expr;
                const destructs = cir_env.store.sliceRecordDestructs(record.destructs);
                var idx = destructs.len;
                while (idx > 0) {
                    idx -= 1;
                    const destruct = cir_env.store.getRecordDestruct(destructs[idx]);
                    const child_pattern_idx = destruct.kind.toPatternIdx();
                    if (!self.patternNeedsAnyExplicitMatch(module_idx, child_pattern_idx)) continue;
                    const field_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, child_pattern_idx);
                    const field_expr = try self.program.store.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .access = .{
                            .record = scrutinee_expr,
                            .field = try self.ctx.copyExecutableIdent(module_idx, destruct.label),
                            .field_index = self.requireRecordDestructFieldIndexFact(module_idx, type_scope, destructs[idx]),
                        } },
                    });
                    current = try self.lowerPatternGuardExpr(
                        module_idx,
                        type_scope,
                        field_expr,
                        field_ty,
                        child_pattern_idx,
                        current,
                        else_expr,
                    );
                }
                break :blk current;
            },
            .tuple => |tuple| blk: {
                var current = then_expr;
                const elem_patterns = cir_env.store.slicePatterns(tuple.patterns);
                var idx = elem_patterns.len;
                while (idx > 0) {
                    idx -= 1;
                    const child_pattern_idx = elem_patterns[idx];
                    if (!self.patternNeedsAnyExplicitMatch(module_idx, child_pattern_idx)) continue;
                    const elem_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, child_pattern_idx);
                    current = try self.lowerPatternGuardExpr(
                        module_idx,
                        type_scope,
                        try self.makeTupleAccessExpr(
                            scrutinee_expr,
                            elem_ty,
                            idx,
                        ),
                        elem_ty,
                        child_pattern_idx,
                        current,
                        else_expr,
                    );
                }
                break :blk current;
            },
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => blk: {
                const cond_expr = try self.makeLiteralPatternConditionExpr(
                    module_idx,
                    scrutinee_expr,
                    scrutinee_ty,
                    pattern,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = self.program.store.getExpr(then_expr).ty,
                    .data = .{ .if_ = .{
                        .cond = cond_expr,
                        .then_body = then_expr,
                        .else_body = else_expr,
                    } },
                });
            },
            .list => debugTodo("monotype.lowerPatternGuardExpr nested list pattern"),
            .applied_tag => debugTodo("monotype.lowerPatternGuardExpr nested tag pattern"),
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
        scrutinee_solved_var: ?Var,
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
            scrutinee_solved_var,
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
        scrutinee_solved_var: ?Var,
        bind_pattern_idx: CIR.Pattern.Idx,
        list_pattern: @FieldType(CIR.Pattern, "list"),
        branch_value: CIR.Expr.Idx,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        try self.recordPatternStructuralFactsFromSourceType(
            module_idx,
            type_scope,
            bind_pattern_idx,
            scrutinee_ty,
            scrutinee_solved_var,
        );
        const elem_ty = self.requirePatternListElemTypeFact(module_idx, type_scope, bind_pattern_idx);
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
                self.lookupListElemSolvedVar(module_idx, scrutinee_solved_var),
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
                    self.lookupListElemSolvedVar(module_idx, scrutinee_solved_var),
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
                    scrutinee_solved_var,
                    &branch_env,
                    &binding_decls,
                );
            }
        }

        try self.bindPatternFromSourceExpr(
            module_idx,
            type_scope,
            bind_pattern_idx,
            scrutinee_expr,
            scrutinee_ty,
            scrutinee_solved_var,
            &branch_env,
            &binding_decls,
        );

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

    fn lowerLiteralPatternBranch(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        result_ty: type_mod.TypeId,
        expected_result_var: ?Var,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        scrutinee_solved_var: ?Var,
        pattern: CIR.Pattern,
        bind_pattern_idx: CIR.Pattern.Idx,
        branch_value: CIR.Expr.Idx,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const cond_expr = try self.makeLiteralPatternConditionExpr(
            module_idx,
            scrutinee_expr,
            scrutinee_ty,
            pattern,
        );
        const then_expr = try self.lowerWholeValuePatternBranch(
            module_idx,
            type_scope,
            incoming_env,
            result_ty,
            expected_result_var,
            scrutinee_expr,
            scrutinee_ty,
            scrutinee_solved_var,
            bind_pattern_idx,
            branch_value,
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

    fn makeLiteralPatternConditionExpr(
        self: *Lowerer,
        module_idx: u32,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        pattern: CIR.Pattern,
    ) std.mem.Allocator.Error!ast.ExprId {
        const bool_ty = try self.makePrimitiveType(.bool);
        const literal_expr = try self.makeLiteralPatternExpr(module_idx, scrutinee_ty, pattern);
        const op: base.LowLevel = switch (pattern) {
            .str_literal => .str_is_eq,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            => .num_is_eq,
            else => debugPanic(
                "monotype match invariant violated: non-literal pattern reached literal predicate lowering",
                .{},
            ),
        };
        return self.makeLowLevelExpr(bool_ty, op, &.{ scrutinee_expr, literal_expr });
    }

    fn makeLiteralPatternExpr(
        self: *Lowerer,
        module_idx: u32,
        target_ty: type_mod.TypeId,
        pattern: CIR.Pattern,
    ) std.mem.Allocator.Error!ast.ExprId {
        const data: ast.Expr.Data = switch (pattern) {
            .num_literal => |num| try self.lowerNumericIntLiteralData(target_ty, num.value),
            .small_dec_literal => |dec| try self.lowerNumericSmallDecLiteralData(target_ty, dec.value),
            .dec_literal => |dec| try self.lowerNumericDecLiteralData(target_ty, dec.value.toI128()),
            .frac_f32_literal => |frac| .{ .frac_f32_lit = frac.value },
            .frac_f64_literal => |frac| .{ .frac_f64_lit = frac.value },
            .str_literal => |str| .{ .str_lit = try self.copySourceStringLiteral(module_idx, str.literal) },
            else => debugPanic(
                "monotype match invariant violated: non-literal pattern reached literal expression lowering",
                .{},
            ),
        };
        return try self.program.store.addExpr(.{
            .ty = target_ty,
            .data = data,
        });
    }

    fn bindPatternFromSourceExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_expr: ast.ExprId,
        source_ty: type_mod.TypeId,
        source_solved_var: ?Var,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!void {
        try self.recordPatternStructuralFactsFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const effective_source_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, pattern_idx);
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign| {
                const bind: ast.TypedSymbol = .{
                    .ty = effective_source_ty,
                    .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
                };
                try decls.append(self.allocator, .{
                    .bind = bind,
                    .body = source_expr,
                });
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .symbol = bind.symbol,
                    .solved_var = source_solved_var,
                });
            },
            .as, .record_destructure, .tuple => {
                const source = try self.makePatternSourceBindWithType(module_idx, pattern_idx, effective_source_ty);
                try decls.append(self.allocator, .{
                    .bind = source,
                    .body = source_expr,
                });
                try self.collectStructuralBindingDeclsWithSolvedVar(
                    module_idx,
                    type_scope,
                    source,
                    source_solved_var,
                    pattern_idx,
                    env,
                    decls,
                );
            },
            .underscore => {},
            .nominal => |nominal| try self.bindPatternFromSourceExpr(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                source_expr,
                effective_source_ty,
                source_solved_var,
                env,
                decls,
            ),
            .nominal_external => |nominal| try self.bindPatternFromSourceExpr(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                source_expr,
                effective_source_ty,
                source_solved_var,
                env,
                decls,
            ),
            .list,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => {},
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
        var fact_env = try self.cloneEnv(incoming_env);
        defer fact_env.deinit();
        const fact_stmts = self.ctx.env(module_idx).store.sliceStatements(stmts_span);
        var fact_i: usize = 0;
        while (fact_i < fact_stmts.len) {
            if (try self.seedLocalLambdaDeclGroupFacts(module_idx, &fact_env, fact_stmts, fact_i)) |group_end| {
                fact_i = group_end;
                continue;
            }
            try self.collectStmtFacts(module_idx, type_scope, &fact_env, fact_stmts[fact_i]);
            fact_i += 1;
        }
        try self.collectExprFacts(module_idx, type_scope, fact_env, final_expr_idx);
        try self.freezeLoweringSemanticTypes(module_idx, type_scope);

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
        var fact_env = try self.cloneEnv(incoming_env);
        defer fact_env.deinit();
        const fact_stmts = self.ctx.env(module_idx).store.sliceStatements(stmts_span);
        var fact_i: usize = 0;
        while (fact_i < fact_stmts.len) {
            if (try self.seedLocalLambdaDeclGroupFacts(module_idx, &fact_env, fact_stmts, fact_i)) |group_end| {
                fact_i = group_end;
                continue;
            }
            try self.collectStmtFacts(module_idx, type_scope, &fact_env, fact_stmts[fact_i]);
            fact_i += 1;
        }
        _ = result_ty;
        _ = expected_result_var;
        try self.collectExprFacts(module_idx, type_scope, fact_env, final_expr_idx);
        try self.freezeLoweringSemanticTypes(module_idx, type_scope);

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

        const final_expr = try self.lowerExpr(
            module_idx,
            type_scope,
            env,
            final_expr_idx,
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

    fn seedLocalLambdaDeclGroupFacts(
        self: *Lowerer,
        module_idx: u32,
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

        var seed_scope: TypeCloneScope = undefined;
        seed_scope.initCloneAll(self.allocator, @constCast(cir_env));
        defer seed_scope.deinit();

        for (cir_stmts[start_idx..end_idx]) |stmt_idx| {
            const decl = cir_env.store.getStatement(stmt_idx).s_decl;
            try self.putLocalFnSeedBinding(env, .{
                .module_idx = module_idx,
                .pattern_idx = @intFromEnum(decl.pattern),
            }, try seed_scope.instantiator.instantiateVar(ModuleEnv.varFrom(decl.expr)));
        }

        return end_idx;
    }

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

        var seed_scope: TypeCloneScope = undefined;
        seed_scope.initCloneAll(self.allocator, @constCast(cir_env));
        defer seed_scope.deinit();

        for (cir_stmts[start_idx..end_idx], 0..) |stmt_idx, group_i| {
            const decl = cir_env.store.getStatement(stmt_idx).s_decl;
            group.sources[group_i] = .{
                .pattern_idx = decl.pattern,
                .expr_idx = decl.expr,
                .seed_var = try seed_scope.instantiator.instantiateVar(ModuleEnv.varFrom(decl.expr)),
                .source_symbol = try self.requirePatternSymbolOnly(module_idx, decl.pattern),
            };
        }

        const group_index: u32 = @intCast(self.local_fn_groups.items.len);
        try self.local_fn_groups.append(self.allocator, group);

        for (group.sources, 0..) |source, group_i| {
            try self.putLocalFnSourceBinding(env, .{
                .module_idx = module_idx,
                .pattern_idx = @intFromEnum(source.pattern_idx),
            }, .{
                .group_index = group_index,
                .source_index = @intCast(group_i),
            });
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
                const body_ty = try self.requireExprTypeFact(module_idx, type_scope, decl.expr);
                const body_var = self.requireExprResultFact(module_idx, type_scope, decl.expr);
                const lowered_body = try self.lowerExprFactWithExpectedType(
                    module_idx,
                    type_scope,
                    env.*,
                    decl.expr,
                    body_ty,
                    body_var,
                );
                const bind_ty = lowered_body.ty;
                if (self.patternNeedsBindingDecls(module_idx, decl.pattern)) {
                    const root_bind = try self.makePatternSourceBindWithType(module_idx, decl.pattern, bind_ty);
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                        .bind = root_bind,
                        .body = lowered_body.expr,
                    } }));
                    var binding_decls = std.ArrayList(BindingDecl).empty;
                    defer binding_decls.deinit(self.allocator);
                    try self.collectStructuralBindingDeclsWithSolvedVar(module_idx, type_scope, root_bind, lowered_body.solved_var, decl.pattern, env, &binding_decls);
                    try self.appendBindingDeclStmts(lowered, binding_decls.items, false);
                    return;
                }

                try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, type_scope, decl.pattern, bind_ty, lowered_body.solved_var, env);
                const bind = try self.requirePatternBinderWithType(module_idx, decl.pattern, bind_ty);
                try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                    .bind = bind,
                    .body = lowered_body.expr,
                } }));
            },
            .s_var => |decl| {
                const body_ty = try self.requireExprTypeFact(module_idx, type_scope, decl.expr);
                const body_var = self.requireExprResultFact(module_idx, type_scope, decl.expr);
                const lowered_body = try self.lowerExprFactWithExpectedType(
                    module_idx,
                    type_scope,
                    env.*,
                    decl.expr,
                    body_ty,
                    body_var,
                );
                const bind_ty = lowered_body.ty;
                if (self.patternNeedsBindingDecls(module_idx, decl.pattern_idx)) {
                    const root_bind = try self.makePatternSourceBindWithType(module_idx, decl.pattern_idx, bind_ty);
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .var_decl = .{
                        .bind = root_bind,
                        .body = lowered_body.expr,
                    } }));
                    var binding_decls = std.ArrayList(BindingDecl).empty;
                    defer binding_decls.deinit(self.allocator);
                    try self.collectStructuralBindingDeclsWithSolvedVar(module_idx, type_scope, root_bind, lowered_body.solved_var, decl.pattern_idx, env, &binding_decls);
                    try self.appendBindingDeclStmts(lowered, binding_decls.items, true);
                    return;
                }

                try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, type_scope, decl.pattern_idx, bind_ty, lowered_body.solved_var, env);
                const bind = try self.requirePatternBinderWithType(module_idx, decl.pattern_idx, bind_ty);
                try lowered.append(self.allocator, try self.program.store.addStmt(.{ .var_decl = .{
                    .bind = bind,
                    .body = lowered_body.expr,
                } }));
            },
            .s_reassign => |reassign| {
                const body_ty = try self.requirePatternTypeFact(module_idx, type_scope, reassign.pattern_idx);
                const lowered_body = try self.lowerExprFactWithExpectedType(
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
                        .body = lowered_body.expr,
                    } }));
                    const root_expr = try self.makeVarExpr(root_bind.ty, root_bind.symbol);
                    try self.lowerPatternReassignFromExpr(
                        module_idx,
                        type_scope,
                        reassign.pattern_idx,
                        root_expr,
                        root_bind.ty,
                        lowered_body.solved_var,
                        env,
                        lowered,
                    );
                    return;
                }
                try lowered.append(self.allocator, try self.program.store.addStmt(.{ .reassign = .{
                    .target = try self.requirePatternSymbol(module_idx, env.*, reassign.pattern_idx),
                    .body = lowered_body.expr,
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
                const elem_ty = try self.requirePatternTypeFact(module_idx, type_scope, for_stmt.patt);
                const elem_solved_var = self.lookupListElemSolvedVar(
                    module_idx,
                    try self.exprResultVar(module_idx, type_scope, env.*, for_stmt.expr),
                );

                const patt = if (self.patternNeedsBindingDecls(module_idx, for_stmt.patt)) blk: {
                    const root_bind = try self.makePatternSourceBindWithType(module_idx, for_stmt.patt, elem_ty);
                    try self.collectStructuralBindingDeclsWithSolvedVar(
                        module_idx,
                        type_scope,
                        root_bind,
                        elem_solved_var,
                        for_stmt.patt,
                        &body_env,
                        &binding_decls,
                    );
                    break :blk try self.program.store.addPat(.{
                        .ty = root_bind.ty,
                        .data = .{ .var_ = root_bind.symbol },
                    });
                } else blk: {
                    try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, type_scope, for_stmt.patt, elem_ty, elem_solved_var, &body_env);
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
        const elem_ty = try self.requirePatternTypeFact(module_idx, type_scope, patt_idx);
        const elem_solved_var = self.lookupListElemSolvedVar(
            module_idx,
            try self.exprResultVar(module_idx, type_scope, incoming_env, iterable_expr_idx),
        );

        const patt = if (self.patternNeedsBindingDecls(module_idx, patt_idx)) blk: {
            const root_bind = try self.makePatternSourceBindWithType(module_idx, patt_idx, elem_ty);
            try self.collectStructuralBindingDeclsWithSolvedVar(
                module_idx,
                type_scope,
                root_bind,
                elem_solved_var,
                patt_idx,
                &body_env,
                &binding_decls,
            );
            break :blk try self.program.store.addPat(.{
                .ty = root_bind.ty,
                .data = .{ .var_ = root_bind.symbol },
            });
        } else blk: {
            try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, type_scope, patt_idx, elem_ty, elem_solved_var, &body_env);
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
        const ty = try self.requirePatternTypeFact(module_idx, type_scope, pattern_idx);
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
                        .discriminant = self.requirePatternTagDiscriminantFact(module_idx, type_scope, pattern_idx),
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
                .value = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    field.value,
                    try self.requireExprTypeFact(module_idx, type_scope, field.value),
                    try self.exprResultVar(module_idx, type_scope, env, field.value),
                ),
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
        record_var: ?Var,
        record: @FieldType(CIR.Expr, "e_record"),
    ) std.mem.Allocator.Error!ast.ExprId {
        _ = record_var;
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
                    try self.requireExprTypeFact(module_idx, type_scope, field.value),
                    try self.exprResultVar(module_idx, type_scope, env, field.value),
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
                        .field_index = @intCast(i),
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

    fn arithmeticBinopMethodName(
        self: *const Lowerer,
        module_idx: u32,
        op: CIR.Expr.Binop.Op,
    ) base.Ident.Idx {
        const idents = self.ctx.env(module_idx).idents;
        return switch (op) {
            .add => idents.plus,
            .sub => idents.minus,
            .mul => idents.times,
            .div => idents.div_by,
            .div_trunc => idents.div_trunc_by,
            .rem => idents.rem_by,
            .lt => idents.is_lt,
            .gt => idents.is_gt,
            .le => idents.is_lte,
            .ge => idents.is_gte,
            .eq => idents.is_eq,
            else => debugPanic("monotype invariant violated: unsupported homogeneous binop fact", .{}),
        };
    }

    fn lowerArithmeticBinopFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        binop: CIR.Expr.Binop,
    ) std.mem.Allocator.Error!ArithmeticBinopFact {
        const source_env = @constCast(self.ctx.env(module_idx));
        const site_requirement = source_env.types.findStaticDispatchSiteRequirement(
            ModuleEnv.varFrom(expr_idx),
            self.arithmeticBinopMethodName(module_idx, binop.op),
        ) orelse debugPanic(
            "monotype invariant violated: missing arithmetic static dispatch site for expr {d} in module {d}",
            .{ expr_idx, module_idx },
        );

        var call_scope: TypeCloneScope = undefined;
        call_scope.initCloneAll(self.allocator, source_env);
        defer call_scope.deinit();

        const cloned_func_var = try call_scope.instantiator.instantiateVar(site_requirement.fn_var);
        const lhs_expected_var = self.lookupCurriedFunctionArgVar(module_idx, &call_scope, cloned_func_var, 0) orelse debugPanic(
            "monotype invariant violated: arithmetic fact missing lhs arg for expr {d} in module {d}",
            .{ expr_idx, module_idx },
        );
        const rhs_expected_var = self.lookupCurriedFunctionArgVar(module_idx, &call_scope, cloned_func_var, 1) orelse debugPanic(
            "monotype invariant violated: arithmetic fact missing rhs arg for expr {d} in module {d}",
            .{ expr_idx, module_idx },
        );

        try self.unifySpecializedCheckerVars(
            module_idx,
            lhs_expected_var,
            try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, binop.lhs),
        );
        try self.unifySpecializedCheckerVars(
            module_idx,
            rhs_expected_var,
            try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, binop.rhs),
        );

        const result_var = try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
        try self.unifyAppliedFunctionResultVar(module_idx, &call_scope, result_var, cloned_func_var, 2);
        _ = self.lookupCurriedFunctionFinalRetVar(module_idx, &call_scope, cloned_func_var) orelse debugPanic(
            "monotype invariant violated: arithmetic fact missing return var for expr {d} in module {d}",
            .{ expr_idx, module_idx },
        );

        return .{
            .operand_var = lhs_expected_var,
            .result_var = result_var,
        };
    }

    fn copyTopLevelDefBindingVarToModule(
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

    fn copyTopLevelDefExprVarToModule(
        self: *Lowerer,
        source_module_idx: u32,
        def_idx: CIR.Def.Idx,
        dest_module_idx: u32,
    ) std.mem.Allocator.Error!Var {
        const def = self.ctx.env(source_module_idx).store.getDef(def_idx);
        const source_var = ModuleEnv.varFrom(def.expr);
        return if (source_module_idx == dest_module_idx)
            source_var
        else
            try self.copyCheckerVarToModule(source_module_idx, dest_module_idx, source_var);
    }

    fn lowerRecordedMethodCall(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!LoweredCall {
        const args = self.getRecordedMethodArgs(module_idx, expr_idx, method_name);
        const fact = self.requireExplicitCallFact(module_idx, type_scope, expr_idx);
        const typed_fact = self.requireExplicitCallTypeFact(module_idx, type_scope, expr_idx);
        const implicit_arg_len: usize = if (args.implicit_receiver == null) 0 else 1;
        const total_args_len = implicit_arg_len + args.explicit_args.len;
        const lowered_args = try self.allocator.alloc(ast.ExprId, total_args_len);
        defer self.allocator.free(lowered_args);
        const result_ty = typed_fact.result_ty;
        const fn_ty = typed_fact.fn_ty;
        const applied_result_tys = typed_fact.applied_result_tys;

        var arg_index: usize = 0;
        if (args.implicit_receiver) |receiver_expr_idx| {
            lowered_args[arg_index] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                receiver_expr_idx,
                typed_fact.arg_tys[arg_index],
                fact.arg_vars[arg_index],
            );
            arg_index += 1;
        }
        for (args.explicit_args, 0..) |arg_expr_idx, i| {
            lowered_args[arg_index + i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                arg_expr_idx,
                typed_fact.arg_tys[arg_index + i],
                fact.arg_vars[arg_index + i],
            );
        }

        const target = if (args.implicit_receiver != null) blk: {
            break :blk self.resolveMonomorphicDispatchTargetFromReceiverVar(
                module_idx,
                type_scope,
                fact.arg_vars[0],
                method_name,
            );
        } else self.lookupResolvedDispatchTarget(module_idx, expr_idx) orelse
            self.resolveMonomorphicDispatchTarget(module_idx, type_scope, env, expr_idx, method_name);
        const callee = try self.lowerResolvedTargetCallee(module_idx, target, fn_ty, fact.fn_var);

        return .{
            .data = try self.buildCurriedCallFromLowered(callee, lowered_args, applied_result_tys),
            .result_ty = result_ty,
        };
    }

    fn lowerCurriedCall(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        call_expr_idx: CIR.Expr.Idx,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
    ) std.mem.Allocator.Error!LoweredCall {
        const chain = try self.resolveSpecializableCallChain(module_idx, type_scope, env, func_expr_idx, arg_exprs);
        defer chain.deinit(self.allocator);

        const call_fact = self.requireExplicitCallFact(module_idx, type_scope, call_expr_idx);
        const typed_fact = self.requireExplicitCallTypeFact(module_idx, type_scope, call_expr_idx);
        const result_ty = typed_fact.result_ty;
        const fn_ty = typed_fact.fn_ty;
        const applied_result_tys = typed_fact.applied_result_tys;

        const callee = try self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            chain.func_expr_idx,
            fn_ty,
            call_fact.fn_var,
        );

        const lowered_args = try self.allocator.alloc(ast.ExprId, chain.arg_exprs.len);
        defer self.allocator.free(lowered_args);
        for (chain.arg_exprs, 0..) |arg_expr_idx, i| {
            lowered_args[i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                arg_expr_idx,
                typed_fact.arg_tys[i],
                call_fact.arg_vars[i],
            );
        }
        return .{
            .data = try self.buildCurriedCallFromLowered(callee, lowered_args, applied_result_tys),
            .result_ty = result_ty,
        };
    }

    fn resolveSpecializableCallChain(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
    ) std.mem.Allocator.Error!SpecializableCallChain {
        const cir_env = self.ctx.env(module_idx);
        var frames = std.ArrayList([]const CIR.Expr.Idx).empty;
        defer frames.deinit(self.allocator);

        try frames.append(self.allocator, arg_exprs);

        var root_func_expr_idx = func_expr_idx;
        while (cir_env.store.getExpr(root_func_expr_idx) == .e_call) {
            const nested_call = cir_env.store.getExpr(root_func_expr_idx).e_call;
            try frames.append(self.allocator, cir_env.store.sliceExpr(nested_call.args));
            root_func_expr_idx = nested_call.func;
        }

        if (frames.items.len == 1) {
            return .{
                .func_expr_idx = func_expr_idx,
                .arg_exprs = arg_exprs,
            };
        }

        try self.recordExprSourceFunctionFact(module_idx, type_scope, env, root_func_expr_idx);
        const source_fact = self.lookupExprSourceFunctionFact(module_idx, type_scope, root_func_expr_idx) orelse {
            return .{
                .func_expr_idx = func_expr_idx,
                .arg_exprs = arg_exprs,
            };
        };

        const total_args = blk: {
            var total: usize = 0;
            for (frames.items) |frame| total += frame.len;
            break :blk total;
        };

        if (total_args > source_fact.arity) {
            return .{
                .func_expr_idx = func_expr_idx,
                .arg_exprs = arg_exprs,
            };
        }

        const flattened = try self.allocator.alloc(CIR.Expr.Idx, total_args);
        var out_index: usize = 0;
        var frame_index = frames.items.len;
        while (frame_index > 0) : (frame_index -= 1) {
            const frame = frames.items[frame_index - 1];
            @memcpy(flattened[out_index .. out_index + frame.len], frame);
            out_index += frame.len;
        }

        return .{
            .func_expr_idx = root_func_expr_idx,
            .arg_exprs = flattened,
            .owned_args = flattened,
        };
    }

    fn lowerCallResultFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        call_expr_idx: CIR.Expr.Idx,
        func_expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ExplicitCallFact {
        _ = env;
        _ = func_expr_idx;
        _ = arg_exprs;
        return self.requireExplicitCallFact(module_idx, type_scope, call_expr_idx);
    }

    fn unifySpecializedRecordedMethodArgsWithActuals(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        cloned_func_var: Var,
        args: RecordedMethodArgs,
    ) std.mem.Allocator.Error!void {
        var arg_index: usize = 0;
        if (args.implicit_receiver) |receiver_expr_idx| {
            const cloned_arg_var = self.lookupCurriedFunctionArgVar(module_idx, type_scope, cloned_func_var, arg_index) orelse debugPanic(
                "monotype static dispatch invariant violated: specialized method missing receiver arg in module {d}",
                .{module_idx},
            );
            try self.collectExprFacts(module_idx, type_scope, env, receiver_expr_idx);
            if (!try self.specializeFunctionArgumentFromExpectedType(module_idx, type_scope, env, receiver_expr_idx, cloned_arg_var)) {
                const actual_arg_var = try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, receiver_expr_idx);
                try self.unifySpecializedCheckerVars(module_idx, cloned_arg_var, actual_arg_var);
            }
            arg_index += 1;
        }

        for (args.explicit_args, 0..) |arg_expr_idx, i| {
            const cloned_arg_var = self.lookupCurriedFunctionArgVar(module_idx, type_scope, cloned_func_var, arg_index + i) orelse debugPanic(
                "monotype static dispatch invariant violated: specialized method missing arg {d} in module {d}",
                .{ arg_index + i, module_idx },
            );
            try self.collectExprFacts(module_idx, type_scope, env, arg_expr_idx);
            if (try self.specializeFunctionArgumentFromExpectedType(module_idx, type_scope, env, arg_expr_idx, cloned_arg_var)) {
                continue;
            }
            const actual_arg_var = try self.lookupCallArgSpecializationVar(module_idx, type_scope, env, arg_expr_idx);
            try self.unifySpecializedCheckerVars(module_idx, cloned_arg_var, actual_arg_var);
        }
    }

    fn specializeFunctionArgumentFromExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        expected_arg_var: Var,
    ) std.mem.Allocator.Error!bool {
        const expected_ty = try self.lowerInstantiatedType(module_idx, type_scope, expected_arg_var);
        if (self.ctx.types.getType(expected_ty) != .func) return false;

        const cir_env = self.ctx.env(module_idx);
        return switch (cir_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| blk: {
                if (env.get(.{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(lookup.pattern_idx),
                })) |entry| {
                    if (entry.local_fn_source) |source| {
                        _ = try self.specializeLocalFnSource(source, expected_ty, expected_arg_var);
                        break :blk true;
                    }
                    break :blk false;
                }

                const symbol = self.lookupTopLevelSymbol(module_idx, lookup.pattern_idx) orelse break :blk false;
                if (!self.topLevelNeedsSpecialization(symbol)) break :blk false;
                _ = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, expected_ty, expected_arg_var);
                break :blk true;
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = cir_env.imports.getResolvedModule(lookup.module_idx) orelse break :blk false;
                const symbol = self.lookupTopLevelDefSymbol(target_module_idx, @enumFromInt(lookup.target_node_idx)) orelse break :blk false;
                if (!self.topLevelNeedsSpecialization(symbol)) break :blk false;
                _ = try self.lookupOrSpecializeExternal(module_idx, lookup, expected_ty, expected_arg_var);
                break :blk true;
            },
            else => false,
        };
    }

    fn lookupCallArgSpecializationVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!Var {
        _ = env;
        return self.requireExprResultFact(module_idx, type_scope, expr_idx);
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
                })) |entry| {
                    if (entry.local_fn_source == null) return null;
                }

                const symbol = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, expected_fn_ty, null);
                return try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse expected_fn_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_lookup_external => |lookup| {
                const symbol = try self.lookupOrSpecializeExternal(module_idx, lookup, expected_fn_ty, null);
                return try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse expected_fn_ty,
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
        try self.freezeLoweringSemanticTypes(module_idx, type_scope);
        const cir_env = self.ctx.env(module_idx);
        const expr = cir_env.store.getExpr(expr_idx);
        if (expected_var != null) {
            switch (expr) {
                .e_call => |call| {
                    _ = try self.lowerCallResultFact(
                        module_idx,
                        type_scope,
                        env,
                        expr_idx,
                        call.func,
                        cir_env.store.sliceExpr(call.args),
                    );
                },
                .e_dot_access => |dot| {
                    if (dot.args != null) {
                        _ = self.requireExplicitCallFact(module_idx, type_scope, expr_idx);
                    }
                },
                .e_type_var_dispatch => |dispatch| {
                    _ = dispatch;
                    _ = self.requireExplicitCallFact(module_idx, type_scope, expr_idx);
                },
                .e_binop => |_| {},
                else => {},
            }
        }
        const target_ty = blk: {
            if (self.ctx.types.getType(expected_ty) != .placeholder) break :blk expected_ty;
            break :blk try self.requireExprTypeFact(module_idx, type_scope, expr_idx);
        };

        return switch (expr) {
            .e_nominal => |nominal| self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                nominal.backing_expr,
                target_ty,
                expected_var,
            ),
            .e_nominal_external => |nominal| self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                nominal.backing_expr,
                target_ty,
                expected_var,
            ),
            .e_num => |num| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerNumericIntLiteralData(target_ty, num.value),
            }),
            .e_dec => |dec| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerNumericDecLiteralData(target_ty, dec.value.toI128()),
            }),
            .e_dec_small => |dec| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerNumericSmallDecLiteralData(target_ty, dec.value),
            }),
            .e_typed_int => |num| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerNumericIntLiteralData(target_ty, num.value),
            }),
            .e_typed_frac => |frac| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerNumericDecLiteralData(target_ty, @bitCast(frac.value.bytes)),
            }),
            .e_call => |call| blk: {
                if (self.callRootCalleeIsRuntimeError(module_idx, call.func)) {
                    break :blk try self.lowerErroneousExprWithType(
                        module_idx,
                        type_scope,
                        env,
                        expr_idx,
                        target_ty,
                    );
                }

                if (try self.maybeLowerSpecialCallExpr(module_idx, type_scope, env, expr_idx, call, target_ty)) |special| {
                    break :blk try self.program.store.addExpr(.{
                        .ty = self.program.store.getExpr(special).ty,
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
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered_call.result_ty,
                    .data = .{ .call = lowered_call.data },
                });
            },
            .e_lambda => |lambda| blk: {
                const lowered = try self.lowerAnonymousClosure(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    target_ty,
                    lambda.args,
                    lambda.body,
                    expected_var,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered.ty,
                    .data = .{ .clos = lowered.data },
                });
            },
            .e_closure => |closure| blk: {
                const lowered = try self.lowerClosureExpr(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    target_ty,
                    closure,
                    expected_var,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered.ty,
                    .data = .{ .clos = lowered.data },
                });
            },
            .e_hosted_lambda => debugTodo("monotype.lowerExprWithExpectedType hosted lambda"),
            .e_run_low_level => |ll| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = .{ .low_level = .{
                    .op = ll.op,
                    .args = try self.lowerExprList(module_idx, type_scope, env, ll.args),
                } },
            }),
            .e_record => |record| self.lowerRecordExpr(
                module_idx,
                type_scope,
                env,
                target_ty,
                expected_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                record,
            ),
            .e_tuple => |tuple| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = .{ .tuple = try self.lowerTupleExprsWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    target_ty,
                    expected_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                    tuple.elems,
                ) },
            }),
            .e_list => |list| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = .{ .list = try self.lowerListExprsWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    target_ty,
                    expected_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                    list.elems,
                ) },
            }),
            .e_if => |if_expr| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = .{ .if_ = try self.lowerIfExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    target_ty,
                    expected_var,
                    if_expr,
                ) },
            }),
            .e_match => |match_expr| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerMatchExprData(
                    module_idx,
                    type_scope,
                    env,
                    target_ty,
                    expected_var,
                    match_expr,
                ),
            }),
            .e_block => |block| blk: {
                const lowered_block = try self.lowerBlockExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    block.stmts,
                    block.final_expr,
                    target_ty,
                    expected_var,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = self.program.store.getExpr(lowered_block.final_expr).ty,
                    .data = .{ .block = lowered_block },
                });
            },
            .e_tag => |tag| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerTagExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    target_ty,
                    expected_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                    tag.name,
                    tag.args,
                ),
            }),
            .e_zero_argument_tag => |tag| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerTagExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    target_ty,
                    expected_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
                    tag.name,
                    .{ .span = .{ .start = 0, .len = 0 } },
                ),
            }),
            .e_lookup_local => |lookup| blk: {
                if (self.ctx.types.getType(target_ty) != .func) {
                    break :blk try self.lowerExpr(module_idx, type_scope, env, expr_idx);
                }
                const symbol = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, target_ty, expected_var);
                break :blk try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse target_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_lookup_external => |lookup| blk: {
                if (self.ctx.types.getType(target_ty) != .func) {
                    break :blk try self.lowerExpr(module_idx, type_scope, env, expr_idx);
                }
                const symbol = try self.lookupOrSpecializeExternal(module_idx, lookup, target_ty, expected_var);
                break :blk try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse target_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_dot_access => |dot| blk: {
                if (dot.args == null) {
                    break :blk try self.lowerExpr(module_idx, type_scope, env, expr_idx);
                }
                const lowered_call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    dot.field_name,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered_call.result_ty,
                    .data = .{ .call = lowered_call.data },
                });
            },
            .e_type_var_dispatch => |dispatch| blk: {
                const lowered_call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    dispatch.method_name,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered_call.result_ty,
                    .data = .{ .call = lowered_call.data },
                });
            },
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
        expected_var: ?Var,
    ) std.mem.Allocator.Error!ast.ExprId {
        const backing_var = self.resolveTransparentBackingVar(
            module_idx,
            expected_var orelse try self.exprResultVar(module_idx, type_scope, env, expr_idx),
        );
        const backing_ty = if (self.ctx.types.getType(nominal_ty) != .placeholder)
            nominal_ty
        else
            try self.requireExprTypeFact(module_idx, type_scope, backing_expr_idx);
        return self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            backing_expr_idx,
            backing_ty,
            backing_var,
        );
    }

    fn resolveTransparentBackingVar(
        self: *const Lowerer,
        module_idx: u32,
        source_var: ?Var,
    ) ?Var {
        const root = source_var orelse return null;
        const env = self.ctx.env(module_idx);
        const resolved = env.types.resolveVar(root);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| self.resolveTransparentBackingVar(module_idx, env.types.getNominalBackingVar(nominal)),
                else => root,
            },
            .alias => |alias| self.resolveTransparentBackingVar(module_idx, env.types.getAliasBackingVar(alias)),
            else => root,
        };
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
            .e_dot_access => |dot| if (dot.args != null) blk: {
                const lowered_call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    call_expr_idx,
                    dot.field_name,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered_call.result_ty,
                    .data = .{ .call = lowered_call.data },
                });
            } else null,
            .e_type_var_dispatch => |dispatch| blk: {
                const lowered_call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    call_expr_idx,
                    dispatch.method_name,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered_call.result_ty,
                    .data = .{ .call = lowered_call.data },
                });
            },
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
                _ = result_ty_opt;
                const result_ty = try self.makePrimitiveType(.str);
                std.debug.assert(self.ctx.types.getType(result_ty) == .primitive and self.ctx.types.getType(result_ty).primitive == .str);
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
        list_var: ?Var,
        span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        _ = list_ty;
        _ = list_var;
        const exprs = self.ctx.env(module_idx).store.sliceExpr(span);
        const out = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(out);
        for (exprs, 0..) |expr_idx, i| {
            out[i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                try self.requireExprTypeFact(module_idx, type_scope, expr_idx),
                try self.exprResultVar(module_idx, type_scope, env, expr_idx),
            );
        }
        return try self.program.store.addExprSpan(out);
    }

    fn lowerTupleExprsWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        tuple_ty: type_mod.TypeId,
        tuple_var: ?Var,
        span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        _ = tuple_ty;
        _ = tuple_var;
        const exprs = self.ctx.env(module_idx).store.sliceExpr(span);
        const out = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(out);
        for (exprs, 0..) |expr_idx, i| {
            out[i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                try self.requireExprTypeFact(module_idx, type_scope, expr_idx),
                try self.exprResultVar(module_idx, type_scope, env, expr_idx),
            );
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
        expected_var: ?Var,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const out = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(out);
        for (exprs, 0..) |expr_idx, i| {
            out[i] = try self.lowerExprWithExpectedType(module_idx, type_scope, env, expr_idx, expected_ty, expected_var);
        }
        return try self.program.store.addExprSpan(out);
    }

    fn lowerHomogeneousBinopArgs(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        operand_ty: type_mod.TypeId,
        operand_var: ?Var,
        lhs_expr_idx: CIR.Expr.Idx,
        rhs_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const lowered = [_]ast.ExprId{
            try self.lowerExprWithExpectedType(module_idx, type_scope, env, lhs_expr_idx, operand_ty, operand_var),
            try self.lowerExprWithExpectedType(module_idx, type_scope, env, rhs_expr_idx, operand_ty, operand_var),
        };
        return self.program.store.addExprSpan(&lowered);
    }

    fn lowerRecordFieldsWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        record_ty: type_mod.TypeId,
        span: CIR.RecordField.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        _ = record_ty;
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
                    try self.requireExprTypeFact(module_idx, type_scope, field.value),
                    try self.exprResultVar(module_idx, type_scope, env, field.value),
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
        expr_idx: CIR.Expr.Idx,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
        tag_name: base.Ident.Idx,
        args_span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        _ = expected_var;
        if (self.ctx.types.getType(expected_ty) == .primitive and self.ctx.types.getType(expected_ty).primitive == .bool) {
            if (self.ctx.env(module_idx).store.sliceExpr(args_span).len != 0) {
                return debugPanic("monotype bool tag invariant violated: Bool tags cannot carry arguments", .{});
            }
            return .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag_name) };
        }

        const arg_exprs = self.ctx.env(module_idx).store.sliceExpr(args_span);
        const lowered_args = try self.allocator.alloc(ast.ExprId, arg_exprs.len);
        defer self.allocator.free(lowered_args);
        for (arg_exprs, 0..) |arg_expr_idx, i| {
            lowered_args[i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                arg_expr_idx,
                try self.requireExprTypeFact(module_idx, type_scope, arg_expr_idx),
                try self.exprResultVar(module_idx, type_scope, env, arg_expr_idx),
            );
        }
        return .{ .tag = .{
            .name = try self.ctx.copyExecutableIdent(module_idx, tag_name),
            .discriminant = self.requireExprTagDiscriminantFact(module_idx, type_scope, expr_idx),
            .args = try self.program.store.addExprSpan(lowered_args),
        } };
    }

    fn exprVarIsErroneous(self: *Lowerer, module_idx: u32, expr_idx: CIR.Expr.Idx) bool {
        return self.ctx.env(module_idx).types.resolveVar(ModuleEnv.varFrom(expr_idx)).desc.content == .err;
    }

    fn callRootCalleeIsRuntimeError(self: *const Lowerer, module_idx: u32, func_expr_idx: CIR.Expr.Idx) bool {
        const cir_env = self.ctx.env(module_idx);
        var current = func_expr_idx;
        while (cir_env.store.getExpr(current) == .e_call) {
            current = cir_env.store.getExpr(current).e_call.func;
        }
        return switch (cir_env.store.getExpr(current)) {
            .e_runtime_error, .e_crash => true,
            else => false,
        };
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
        applied_result_tys: []const type_mod.TypeId,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "call") {
        var current = callee;
        if (arg_exprs.len == 0) {
            const unit_expr = try self.program.store.addExpr(.{
                .ty = try self.ctx.types.addType(.{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } }),
                .data = .unit,
            });
            return .{ .func = current, .arg = unit_expr };
        }

        for (arg_exprs[0 .. arg_exprs.len - 1], 0..) |arg_expr, i| {
            const result_ty = applied_result_tys[i];
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

    fn requireExprResultFact(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) Var {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        return type_scope.facts.expr_result_var_facts.get(key) orelse
            debugPanic("monotype explicit expr fact invariant violated: missing explicit expr result var fact", .{});
    }

    fn recordExprTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        if (type_scope.facts.expr_type_facts.contains(key)) return;

        const lowered = try self.lowerInstantiatedType(
            module_idx,
            type_scope,
            self.requireExprResultFact(module_idx, type_scope, expr_idx),
        );
        const expr = self.ctx.env(module_idx).store.getExpr(expr_idx);
        const normalized = try self.normalizeDefaultNumericLiteralType(module_idx, expr_idx, expr, lowered);
        try type_scope.facts.expr_type_facts.put(key, try self.publishMonotypeType(normalized));
    }

    fn freezeExprTypeFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!void {
        var iter = type_scope.facts.collected_expr_facts.keyIterator();
        while (iter.next()) |key_ptr| {
            if (type_scope.facts.expr_type_facts.contains(key_ptr.*)) continue;
            try self.recordExprTypeFact(module_idx, type_scope, key_ptr.expr_idx);
        }
    }

    fn recordPatternTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.facts.pattern_type_facts.contains(key)) return;

        const lowered = try self.lowerInstantiatedType(
            module_idx,
            type_scope,
            try self.requirePatternSolvedVar(type_scope, pattern_idx),
        );
        try type_scope.facts.pattern_type_facts.put(key, try self.publishMonotypeType(lowered));
    }

    fn collectPatternFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.facts.pattern_type_facts.contains(key)) return;

        try self.recordPatternTypeFact(module_idx, type_scope, pattern_idx);
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        switch (pattern) {
            .assign,
            .underscore,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .runtime_error,
            => {},
            .as => |as_pat| try self.collectPatternFacts(module_idx, type_scope, as_pat.pattern),
            .applied_tag => |tag| {
                for (self.ctx.env(module_idx).store.slicePatterns(tag.args)) |arg_pat| {
                    try self.collectPatternFacts(module_idx, type_scope, arg_pat);
                }
            },
            .record_destructure => |record| {
                for (self.ctx.env(module_idx).store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    try self.collectPatternFacts(
                        module_idx,
                        type_scope,
                        self.ctx.env(module_idx).store.getRecordDestruct(destruct_idx).kind.toPatternIdx(),
                    );
                }
            },
            .list => |list| {
                for (self.ctx.env(module_idx).store.slicePatterns(list.patterns)) |child_pat| {
                    try self.collectPatternFacts(module_idx, type_scope, child_pat);
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pat| {
                        try self.collectPatternFacts(module_idx, type_scope, rest_pat);
                    }
                }
            },
            .tuple => |tuple| {
                for (self.ctx.env(module_idx).store.slicePatterns(tuple.patterns)) |elem_pat| {
                    try self.collectPatternFacts(module_idx, type_scope, elem_pat);
                }
            },
            .nominal => |nominal| try self.collectPatternFacts(module_idx, type_scope, nominal.backing_pattern),
            .nominal_external => |nominal| try self.collectPatternFacts(module_idx, type_scope, nominal.backing_pattern),
        }
    }

    fn recordPatternStructuralFactsFromSourceType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        source_solved_var: ?Var,
    ) std.mem.Allocator.Error!void {
        if (!self.ctx.types.isFullyResolved(source_ty)) {
            return debugPanic(
                "monotype explicit pattern fact invariant violated: structural pattern source type was not frozen before structural fact recording in module {d}",
                .{module_idx},
            );
        }
        const effective_source_ty = source_ty;
        try self.bindPatternSourceTypeFact(module_idx, type_scope, pattern_idx, effective_source_ty);

        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign,
            .underscore,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .runtime_error,
            => {},
            .as => |as_pat| try self.recordPatternStructuralFactsFromSourceType(
                module_idx,
                type_scope,
                as_pat.pattern,
                effective_source_ty,
                source_solved_var,
            ),
            .applied_tag => |tag| {
                if (self.ctx.types.getType(effective_source_ty) == .primitive and self.ctx.types.getType(effective_source_ty).primitive == .bool) {
                    if (cir_env.store.slicePatterns(tag.args).len != 0) {
                        return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    return;
                }
                const source_var = source_solved_var orelse debugPanic(
                    "monotype explicit pattern fact invariant violated: tag pattern missing explicit source solved var in module {d}",
                    .{module_idx},
                );
                const expected_discriminant = try self.requireTagDiscriminantForSolvedVar(module_idx, source_var, tag.name);
                try self.bindPatternTagDiscriminantFact(module_idx, type_scope, pattern_idx, expected_discriminant);
                for (cir_env.store.slicePatterns(tag.args)) |arg_pat| {
                    try self.recordPatternStructuralFactsFromSourceType(
                        module_idx,
                        type_scope,
                        arg_pat,
                        try self.requirePatternTypeFact(module_idx, type_scope, arg_pat),
                        try self.requirePatternSolvedVar(type_scope, arg_pat),
                    );
                }
            },
            .record_destructure => |record| {
                for (cir_env.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = cir_env.store.getRecordDestruct(destruct_idx);
                    const child_pattern_idx = destruct.kind.toPatternIdx();
                    try self.bindRecordDestructFieldIndexFact(
                        module_idx,
                        type_scope,
                        destruct_idx,
                        try self.requireRecordFieldIndexForSolvedVar(
                            module_idx,
                            source_solved_var orelse debugPanic(
                                "monotype explicit pattern fact invariant violated: record destructure missing explicit source solved var in module {d}",
                                .{module_idx},
                            ),
                            destruct.label,
                        ),
                    );
                    const field_index = self.requireRecordDestructFieldIndexFact(module_idx, type_scope, destruct_idx);
                    try self.recordPatternStructuralFactsFromSourceType(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        self.requireRecordFieldTypeFromMonotype(effective_source_ty, field_index),
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                    );
                }
            },
            .list => |list| {
                const elem_ty = self.requireListElemTypeFromMonotype(effective_source_ty);
                try self.bindPatternListElemTypeFact(module_idx, type_scope, pattern_idx, elem_ty);
                for (cir_env.store.slicePatterns(list.patterns)) |child_pattern_idx| {
                    try self.recordPatternStructuralFactsFromSourceType(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        elem_ty,
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                    );
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        try self.recordPatternStructuralFactsFromSourceType(
                            module_idx,
                            type_scope,
                            rest_pattern_idx,
                            try self.requirePatternTypeFact(module_idx, type_scope, rest_pattern_idx),
                            try self.requirePatternSolvedVar(type_scope, rest_pattern_idx),
                        );
                    }
                }
            },
            .tuple => |tuple| {
                const elem_patterns = cir_env.store.slicePatterns(tuple.patterns);
                const elem_tys = self.requireTupleElemTypesFromMonotype(effective_source_ty);
                if (elem_patterns.len != elem_tys.len) {
                    return debugPanic(
                        "monotype explicit pattern fact invariant violated: tuple pattern arity {d} did not match frozen source type arity {d} in module {d}",
                        .{ elem_patterns.len, elem_tys.len, module_idx },
                    );
                }
                for (elem_patterns, elem_tys) |child_pattern_idx, elem_ty| {
                    try self.recordPatternStructuralFactsFromSourceType(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        elem_ty,
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                    );
                }
            },
            .nominal => |nominal| try self.recordPatternStructuralFactsFromSourceType(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                effective_source_ty,
                source_solved_var,
            ),
            .nominal_external => |nominal| try self.recordPatternStructuralFactsFromSourceType(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                effective_source_ty,
                source_solved_var,
            ),
        }
    }

    fn bindPatternEnvWithSolvedVarOnly(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_solved_var: Var,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign| {
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident);
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .symbol = symbol,
                    .solved_var = source_solved_var,
                });
            },
            .as => |as_pat| {
                try self.bindPatternEnvWithSolvedVarOnly(
                    module_idx,
                    type_scope,
                    as_pat.pattern,
                    source_solved_var,
                    env,
                );
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident);
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .symbol = symbol,
                    .solved_var = source_solved_var,
                });
            },
            .applied_tag => |tag| {
                for (self.ctx.env(module_idx).store.slicePatterns(tag.args)) |arg_pat| {
                    try self.bindPatternEnvWithSolvedVarOnly(
                        module_idx,
                        type_scope,
                        arg_pat,
                        try self.requirePatternSolvedVar(type_scope, arg_pat),
                        env,
                    );
                }
            },
            .record_destructure => |record| {
                for (self.ctx.env(module_idx).store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const child_pattern_idx = self.ctx.env(module_idx).store.getRecordDestruct(destruct_idx).kind.toPatternIdx();
                    try self.bindPatternEnvWithSolvedVarOnly(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                        env,
                    );
                }
            },
            .tuple => |tuple| {
                for (self.ctx.env(module_idx).store.slicePatterns(tuple.patterns)) |child_pattern_idx| {
                    try self.bindPatternEnvWithSolvedVarOnly(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                        env,
                    );
                }
            },
            .list => |list| {
                for (self.ctx.env(module_idx).store.slicePatterns(list.patterns)) |child_pattern_idx| {
                    try self.bindPatternEnvWithSolvedVarOnly(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                        env,
                    );
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        try self.bindPatternEnvWithSolvedVarOnly(
                            module_idx,
                            type_scope,
                            rest_pattern_idx,
                            try self.requirePatternSolvedVar(type_scope, rest_pattern_idx),
                            env,
                        );
                    }
                }
            },
            .nominal => |nominal| try self.bindPatternEnvWithSolvedVarOnly(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                source_solved_var,
                env,
            ),
            .nominal_external => |nominal| try self.bindPatternEnvWithSolvedVarOnly(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                source_solved_var,
                env,
            ),
            .underscore,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .runtime_error,
            => {},
        }
    }

    fn collectPatternBindingsIntoEnvWithSolvedVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_solved_var: Var,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        try self.collectPatternFacts(module_idx, type_scope, pattern_idx);
        try self.bindPatternEnvWithSolvedVarOnly(
            module_idx,
            type_scope,
            pattern_idx,
            source_solved_var,
            env,
        );
    }

    fn collectStmtFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: *BindingEnv,
        stmt_idx: CIR.Statement.Idx,
    ) std.mem.Allocator.Error!void {
        const cir_env = self.ctx.env(module_idx);
        const stmt = cir_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                try self.collectExprFactsWithExplicitResultVar(
                    module_idx,
                    type_scope,
                    env.*,
                    decl.expr,
                    try self.requirePatternSolvedVar(type_scope, decl.pattern),
                );
                const body_var = self.requireExprResultFact(module_idx, type_scope, decl.expr);
                try self.collectPatternBindingsIntoEnvWithSolvedVar(
                    module_idx,
                    type_scope,
                    decl.pattern,
                    body_var,
                    env,
                );
            },
            .s_var => |decl| {
                try self.collectExprFactsWithExplicitResultVar(
                    module_idx,
                    type_scope,
                    env.*,
                    decl.expr,
                    try self.requirePatternSolvedVar(type_scope, decl.pattern_idx),
                );
                const body_var = self.requireExprResultFact(module_idx, type_scope, decl.expr);
                try self.collectPatternBindingsIntoEnvWithSolvedVar(
                    module_idx,
                    type_scope,
                    decl.pattern_idx,
                    body_var,
                    env,
                );
            },
            .s_reassign => |reassign| {
                try self.collectPatternFacts(module_idx, type_scope, reassign.pattern_idx);
                try self.collectExprFacts(module_idx, type_scope, env.*, reassign.expr);
            },
            .s_expr => |expr_stmt| try self.collectExprFacts(module_idx, type_scope, env.*, expr_stmt.expr),
            .s_dbg => |dbg| try self.collectExprFacts(module_idx, type_scope, env.*, dbg.expr),
            .s_expect => |expect| try self.collectExprFacts(module_idx, type_scope, env.*, expect.body),
            .s_crash => {},
            .s_return => |ret| {
                try self.collectExprFacts(module_idx, type_scope, env.*, ret.expr);
            },
            .s_break => {},
            .s_for => |for_stmt| {
                try self.collectExprFacts(module_idx, type_scope, env.*, for_stmt.expr);
                try self.collectPatternFacts(module_idx, type_scope, for_stmt.patt);
                const elem_var = self.lookupListElemSolvedVar(
                    module_idx,
                    self.requireExprResultFact(module_idx, type_scope, for_stmt.expr),
                ) orelse debugPanic(
                    "monotype explicit pattern fact invariant violated: for statement missing explicit iterable element solved var in module {d}",
                    .{module_idx},
                );
                var body_env = try self.cloneEnv(env.*);
                defer body_env.deinit();
                try self.collectPatternBindingsIntoEnvWithSolvedVar(
                    module_idx,
                    type_scope,
                    for_stmt.patt,
                    elem_var,
                    &body_env,
                );
                try self.collectExprFacts(module_idx, type_scope, body_env, for_stmt.body);
            },
            .s_while => |while_stmt| {
                try self.collectExprFacts(module_idx, type_scope, env.*, while_stmt.cond);
                try self.collectExprFacts(module_idx, type_scope, env.*, while_stmt.body);
            },
            else => {},
        }
    }

    fn freezeExprStructuralFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!void {
        var iter = type_scope.facts.collected_expr_facts.keyIterator();
        while (iter.next()) |key_ptr| {
            const expr_idx = key_ptr.expr_idx;
            const expr = self.ctx.env(module_idx).store.getExpr(expr_idx);
            switch (expr) {
                .e_dot_access => |dot| {
                    if (dot.args == null) {
                        try self.bindExprFieldIndexFact(
                            module_idx,
                            type_scope,
                            expr_idx,
                            try self.requireRecordFieldIndexForSolvedVar(
                                module_idx,
                                self.requireExprResultFact(module_idx, type_scope, dot.receiver),
                                dot.field_name,
                            ),
                        );
                    }
                },
                .e_tag => |tag| {
                    const expr_ty = try self.requireExprTypeFact(module_idx, type_scope, expr_idx);
                    if (self.ctx.types.getType(expr_ty) == .primitive and self.ctx.types.getType(expr_ty).primitive == .bool) continue;

                    const expected_discriminant = try self.requireTagDiscriminantForSolvedVar(
                        module_idx,
                        self.requireExprResultFact(module_idx, type_scope, expr_idx),
                        tag.name,
                    );
                    try self.bindExprTagDiscriminantFact(module_idx, type_scope, expr_idx, expected_discriminant);
                },
                .e_zero_argument_tag => |tag| {
                    const expr_ty = try self.requireExprTypeFact(module_idx, type_scope, expr_idx);
                    if (self.ctx.types.getType(expr_ty) == .primitive and self.ctx.types.getType(expr_ty).primitive == .bool) continue;

                    const expected_discriminant = try self.requireTagDiscriminantForSolvedVar(
                        module_idx,
                        self.requireExprResultFact(module_idx, type_scope, expr_idx),
                        tag.name,
                    );
                    try self.bindExprTagDiscriminantFact(module_idx, type_scope, expr_idx, expected_discriminant);
                },
                else => {},
            }
        }
    }

    fn collectExprFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        const cir_env = self.ctx.env(module_idx);
        const expr = cir_env.store.getExpr(expr_idx);
        if (type_scope.facts.collected_expr_facts.contains(key) and type_scope.facts.expr_result_var_facts.contains(key) and (!exprNeedsExplicitCallFact(expr) or type_scope.facts.call_facts.contains(key))) return;
        const explicit_result_var = type_scope.facts.expr_result_var_facts.get(key);

        const result_var: Var = switch (expr) {
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_str_segment,
            .e_bytes_literal,
            .e_lookup_local,
            .e_lookup_external,
            .e_lookup_pending,
            .e_lookup_required,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_lambda,
            .e_closure,
            .e_hosted_lambda,
            .e_runtime_error,
            .e_crash,
            => explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx),
            .e_str => |str_expr| blk: {
                for (cir_env.store.sliceExpr(str_expr.span)) |part| {
                    try self.collectExprFacts(module_idx, type_scope, env, part);
                }
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_list => |list| blk: {
                const list_result_var = explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
                const elem_result_var = self.lookupListElemSolvedVar(module_idx, list_result_var);
                for (cir_env.store.sliceExpr(list.elems)) |elem| {
                    try self.collectExprFactsWithExplicitResultVar(
                        module_idx,
                        type_scope,
                        env,
                        elem,
                        elem_result_var,
                    );
                }
                break :blk list_result_var;
            },
            .e_tuple => |tuple| blk: {
                for (cir_env.store.sliceExpr(tuple.elems)) |elem| {
                    try self.collectExprFacts(module_idx, type_scope, env, elem);
                }
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_call => |call| blk: {
                if (try self.collectSourceSeededCallFacts(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    call.func,
                    cir_env.store.sliceExpr(call.args),
                    explicit_result_var,
                )) |call_fact| {
                    break :blk call_fact.result_var;
                }
                switch (cir_env.store.getExpr(call.func)) {
                    .e_dot_access => |dot| {
                        if (dot.args != null) {
                            const call_fact = try self.collectRecordedMethodCallFact(
                                module_idx,
                                type_scope,
                                env,
                                expr_idx,
                                dot.field_name,
                                explicit_result_var,
                            );
                            break :blk call_fact.result_var;
                        }
                    },
                    .e_type_var_dispatch => |dispatch| {
                        const call_fact = try self.collectRecordedMethodCallFact(
                            module_idx,
                            type_scope,
                            env,
                            expr_idx,
                            dispatch.method_name,
                            explicit_result_var,
                        );
                        break :blk call_fact.result_var;
                    },
                    else => {},
                }
                try self.collectExprFacts(module_idx, type_scope, env, call.func);
                for (cir_env.store.sliceExpr(call.args)) |arg_expr| {
                    try self.collectExprFacts(module_idx, type_scope, env, arg_expr);
                }
                const call_result_var = explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
                const callee_var = self.requireExprResultFact(module_idx, type_scope, call.func);
                try self.putExplicitCallFact(
                    module_idx,
                    type_scope,
                    expr_idx,
                    try self.buildExplicitCallFact(
                        module_idx,
                        type_scope,
                        callee_var,
                        call_result_var,
                        cir_env.store.sliceExpr(call.args).len,
                    ),
                );
                break :blk call_result_var;
            },
            .e_record => |record| blk: {
                for (cir_env.store.sliceRecordFields(record.fields)) |field_idx| {
                    const field = cir_env.store.getRecordField(field_idx);
                    try self.collectExprFacts(module_idx, type_scope, env, field.value);
                }
                if (record.ext) |ext_expr| {
                    try self.collectExprFacts(module_idx, type_scope, env, ext_expr);
                }
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_block => |block| blk: {
                var body_env = try self.cloneEnv(env);
                defer body_env.deinit();
                const block_stmts = cir_env.store.sliceStatements(block.stmts);
                var stmt_index: usize = 0;
                while (stmt_index < block_stmts.len) {
                    if (try self.seedLocalLambdaDeclGroupFacts(module_idx, &body_env, block_stmts, stmt_index)) |group_end| {
                        stmt_index = group_end;
                        continue;
                    }
                    try self.collectStmtFacts(module_idx, type_scope, &body_env, block_stmts[stmt_index]);
                    stmt_index += 1;
                }
                try self.collectExprFacts(module_idx, type_scope, body_env, block.final_expr);
                break :blk explicit_result_var orelse self.requireExprResultFact(module_idx, type_scope, block.final_expr);
            },
            .e_tag => |tag| blk: {
                for (cir_env.store.sliceExpr(tag.args)) |arg_expr| {
                    try self.collectExprFacts(module_idx, type_scope, env, arg_expr);
                }
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_nominal => |nominal| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, nominal.backing_expr);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_nominal_external => |nominal| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, nominal.backing_expr);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_binop => |binop| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, binop.lhs);
                try self.collectExprFacts(module_idx, type_scope, env, binop.rhs);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_unary_minus => |unary| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, unary.expr);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_unary_not => |unary| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, unary.expr);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_dot_access => |dot| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, dot.receiver);
                if (dot.args) |dispatch_args| {
                    for (cir_env.store.sliceExpr(dispatch_args)) |arg_expr| {
                        try self.collectExprFacts(module_idx, type_scope, env, arg_expr);
                    }
                    const call_fact = try self.collectRecordedMethodCallFact(
                        module_idx,
                        type_scope,
                        env,
                        expr_idx,
                        dot.field_name,
                        explicit_result_var,
                    );
                    break :blk call_fact.result_var;
                }
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_type_var_dispatch => |dispatch| blk: {
                for (cir_env.store.sliceExpr(dispatch.args)) |arg_expr| {
                    try self.collectExprFacts(module_idx, type_scope, env, arg_expr);
                }
                const call_fact = try self.collectRecordedMethodCallFact(
                    module_idx,
                    type_scope,
                    env,
                    expr_idx,
                    dispatch.method_name,
                    explicit_result_var,
                );
                break :blk call_fact.result_var;
            },
            .e_match => |match_expr| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, match_expr.cond);
                const cond_var = self.requireExprResultFact(module_idx, type_scope, match_expr.cond);
                const branches = cir_env.store.matchBranchSlice(match_expr.branches);
                for (branches) |branch_idx| {
                    const branch = cir_env.store.getMatchBranch(branch_idx);
                    var branch_env = try self.cloneEnv(env);
                    defer branch_env.deinit();
                    const branch_pattern_ids = cir_env.store.sliceMatchBranchPatterns(branch.patterns);
                    if (branch_pattern_ids.len != 0) {
                        const first_pattern = cir_env.store.getMatchBranchPattern(branch_pattern_ids[0]).pattern;
                        try self.collectPatternBindingsIntoEnvWithSolvedVar(
                            module_idx,
                            type_scope,
                            first_pattern,
                            cond_var,
                            &branch_env,
                        );
                    }
                    if (branch.guard) |guard_expr| {
                        try self.collectExprFacts(module_idx, type_scope, branch_env, guard_expr);
                    }
                    try self.collectExprFacts(module_idx, type_scope, branch_env, branch.value);
                }
                if (branches.len == 0) {
                    return debugPanic("monotype explicit expr fact invariant violated: match expression missing branches", .{});
                }
                const first_branch = cir_env.store.getMatchBranch(branches[0]);
                break :blk explicit_result_var orelse self.requireExprResultFact(module_idx, type_scope, first_branch.value);
            },
            .e_if => |if_expr| blk: {
                for (cir_env.store.sliceIfBranches(if_expr.branches)) |branch_id| {
                    const branch = cir_env.store.getIfBranch(branch_id);
                    try self.collectExprFacts(module_idx, type_scope, env, branch.cond);
                    try self.collectExprFacts(module_idx, type_scope, env, branch.body);
                }
                try self.collectExprFacts(module_idx, type_scope, env, if_expr.final_else);
                break :blk explicit_result_var orelse self.requireExprResultFact(module_idx, type_scope, if_expr.final_else);
            },
            .e_expect => |expect| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, expect.body);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_dbg => |dbg| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, dbg.expr);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_return => |ret| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, ret.expr);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_for => |for_expr| blk: {
                try self.collectExprFacts(module_idx, type_scope, env, for_expr.expr);
                try self.collectPatternFacts(module_idx, type_scope, for_expr.patt);
                const elem_var = self.lookupListElemSolvedVar(
                    module_idx,
                    self.requireExprResultFact(module_idx, type_scope, for_expr.expr),
                ) orelse debugPanic(
                    "monotype explicit pattern fact invariant violated: for expression missing explicit iterable element solved var in module {d}",
                    .{module_idx},
                );
                var body_env = try self.cloneEnv(env);
                defer body_env.deinit();
                try self.collectPatternBindingsIntoEnvWithSolvedVar(
                    module_idx,
                    type_scope,
                    for_expr.patt,
                    elem_var,
                    &body_env,
                );
                try self.collectExprFacts(module_idx, type_scope, body_env, for_expr.body);
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            .e_run_low_level => |ll| blk: {
                for (cir_env.store.sliceExpr(ll.args)) |arg_expr| {
                    try self.collectExprFacts(module_idx, type_scope, env, arg_expr);
                }
                break :blk explicit_result_var orelse try self.scopedExprResultVar(module_idx, type_scope, env, expr_idx);
            },
            else => debugTodoExpr(expr),
        };

        try type_scope.facts.expr_result_var_facts.put(key, result_var);
        try type_scope.facts.collected_expr_facts.put(key, {});
        try self.recordExprSourceFunctionFact(module_idx, type_scope, env, expr_idx);
        if (expr == .e_binop) {
            const binop = expr.e_binop;
            switch (binop.op) {
                .add, .sub, .mul, .div, .rem, .div_trunc, .lt, .gt, .le, .ge, .eq => try self.putArithmeticBinopFact(
                    module_idx,
                    type_scope,
                    expr_idx,
                    try self.lowerArithmeticBinopFact(module_idx, type_scope, env, expr_idx, binop),
                ),
                else => {},
            }
        }
    }

    fn requireExprTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        _ = self;
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        return type_scope.facts.expr_type_facts.get(key) orelse
            debugPanic("monotype explicit expr fact invariant violated: missing explicit expr type fact", .{});
    }

    fn requirePatternSolvedVar(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!Var {
        return try self.scopeVar(type_scope, ModuleEnv.varFrom(pattern_idx));
    }

    fn requirePatternTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        _ = self;
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        return type_scope.facts.pattern_type_facts.get(key) orelse
            debugPanic("monotype explicit pattern fact invariant violated: missing explicit pattern type fact", .{});
    }

    fn lowerExprType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        _ = env;
        _ = expr;
        return try self.requireExprTypeFact(module_idx, type_scope, expr_idx);
    }

    fn lookupTrySuffixPayloadType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        const cir_env = self.ctx.env(module_idx);
        const branches = cir_env.store.matchBranchSlice(match_expr.branches);
        if (branches.len == 0) return null;

        const ok_branch = cir_env.store.getMatchBranch(branches[0]);
        const branch_patterns = cir_env.store.sliceMatchBranchPatterns(ok_branch.patterns);
        if (branch_patterns.len == 0) return null;

        const branch_pattern = cir_env.store.getMatchBranchPattern(branch_patterns[0]);
        const tag_info = self.lookupSingleTagPayloadPattern(module_idx, branch_pattern.pattern) orelse return null;
        _ = env;
        return try self.requirePatternTypeFact(module_idx, type_scope, tag_info.payload_pattern);
    }

    fn lookupSingleTagPayloadPattern(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?struct { tag_name: base.Ident.Idx, payload_pattern: CIR.Pattern.Idx } {
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        return switch (pattern) {
            .applied_tag => |tag| blk: {
                const args = cir_env.store.slicePatterns(tag.args);
                if (args.len != 1) break :blk null;
                break :blk .{ .tag_name = tag.name, .payload_pattern = args[0] };
            },
            .nominal => |nominal| self.lookupSingleTagPayloadPattern(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.lookupSingleTagPayloadPattern(module_idx, nominal.backing_pattern),
            else => null,
        };
    }

    fn makeEmptyRecordType(self: *Lowerer) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.makeUnitType();
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
        if (type_scope.active_type_cache.get(key)) |active| return active;
        if (type_scope.type_cache.get(key)) |cached| return cached;
        if (type_scope.provisional_type_cache.get(key)) |provisional| {
            if (self.ctx.types.isFullyResolved(provisional)) {
                const canonical = try self.ctx.types.canonicalizeResolved(provisional);
                _ = type_scope.provisional_type_cache.remove(key);
                try type_scope.type_cache.put(key, canonical);
                return canonical;
            }
            return provisional;
        }

        const placeholder = try self.ctx.types.addType(.placeholder);
        try type_scope.active_type_cache.put(key, placeholder);

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

        self.ctx.types.setType(placeholder, lowered);
        if (lowered == .placeholder) {
            _ = type_scope.active_type_cache.remove(key);
            return placeholder;
        }

        _ = type_scope.active_type_cache.remove(key);
        if (self.ctx.types.isFullyResolved(placeholder)) {
            const canonical = try self.ctx.types.canonicalizeResolved(placeholder);
            try type_scope.type_cache.put(key, canonical);
            return canonical;
        }

        try type_scope.provisional_type_cache.put(key, placeholder);
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
                if (prev_arg != tag_arg) {
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
        const defining = try self.resolveNominalDefiningIdentity(module_idx, nominal);
        const defining_ident = defining.ident;
        const defining_env = defining.env;

        if (defining_ident.eql(defining_env.idents.str) or defining_ident.eql(defining_env.idents.builtin_str)) {
            return .{ .primitive = .str };
        }
        if (defining_ident.eql(defining_env.idents.list)) {
            const args = env.types.sliceNominalArgs(nominal);
            if (args.len != 1) debugPanic("monotype.lowerNominalType List expected one type argument", .{});
            return .{ .list = try self.lowerInstantiatedType(module_idx, type_scope, args[0]) };
        }
        if (defining_ident.eql(defining_env.idents.box)) {
            const args = env.types.sliceNominalArgs(nominal);
            if (args.len != 1) debugPanic("monotype.lowerNominalType Box expected one type argument", .{});
            return .{ .box = try self.lowerInstantiatedType(module_idx, type_scope, args[0]) };
        }
        if (defining_ident.eql(defining_env.idents.bool_type)) return .{ .primitive = .bool };
        if (builtinNumPrim(defining_env, defining_ident)) |prim| return .{ .primitive = prim };

        const nominal_args = env.types.sliceNominalArgs(nominal);
        const lowered_args = try self.allocator.alloc(type_mod.TypeId, nominal_args.len);
        defer self.allocator.free(lowered_args);
        for (nominal_args, 0..) |arg_var, i| {
            lowered_args[i] = try self.lowerInstantiatedType(module_idx, type_scope, arg_var);
        }

        try self.buildNominalTypeCacheKey(type_scope, defining.module_idx, defining_ident, lowered_args);
        if (type_scope.nominal_type_cache.get(type_scope.scratch_nominal_key.items)) |cached| {
            return .{ .link = cached };
        }

        const owned_key = try self.allocator.dupe(u8, type_scope.scratch_nominal_key.items);
        errdefer self.allocator.free(owned_key);
        try type_scope.nominal_type_cache.put(owned_key, nominal_type_id);

        const backing_var = env.types.getNominalBackingVar(nominal);
        return .{ .nominal = try self.lowerInstantiatedType(module_idx, type_scope, backing_var) };
    }

    const NominalDefiningIdentity = struct {
        module_idx: u32,
        env: *const ModuleEnv,
        ident: base.Ident.Idx,
    };

    fn resolveNominalDefiningIdentity(
        self: *const Lowerer,
        module_idx: u32,
        nominal: types.NominalType,
    ) std.mem.Allocator.Error!NominalDefiningIdentity {
        const env = self.ctx.env(module_idx);
        const defining_module_idx = findModuleIdxByName(self.ctx.all_module_envs, env.getIdent(nominal.origin_module));
        const defining_env = self.ctx.env(defining_module_idx);
        const defining_ident = defining_env.common.findIdent(env.getIdent(nominal.ident.ident_idx)) orelse debugPanic(
            "monotype.lowerNominalType missing target ident in defining module",
            .{},
        );
        return .{
            .module_idx = defining_module_idx,
            .env = defining_env,
            .ident = defining_ident,
        };
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
            .flex => |flex| if (flex.name) |name|
                builtinNumPrim(env, name) orelse
                    (if (constraintsContainFromNumeral(env, flex.constraints)) .dec else null)
            else if (constraintsContainFromNumeral(env, flex.constraints)) .dec else null,
            .rigid => |rigid| builtinNumPrim(env, rigid.name) orelse
                (if (constraintsContainFromNumeral(env, rigid.constraints)) .dec else null),
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

    fn patternCanCollectStructuralBindings(self: *Lowerer, module_idx: u32, pattern_idx: CIR.Pattern.Idx) bool {
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        return switch (pattern) {
            .assign, .as, .underscore, .record_destructure, .tuple => true,
            .nominal => |nominal| self.patternCanCollectStructuralBindings(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.patternCanCollectStructuralBindings(module_idx, nominal.backing_pattern),
            else => false,
        };
    }

    fn patternNeedsAnyExplicitMatch(self: *Lowerer, module_idx: u32, pattern_idx: CIR.Pattern.Idx) bool {
        return self.patternNeedsPredicateDesugaring(module_idx, pattern_idx) or !self.patternIsIrrefutableStructural(module_idx, pattern_idx);
    }

    fn makePatternSourceBind(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        const ty = try self.requirePatternTypeFact(module_idx, type_scope, pattern_idx);
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
        try self.recordPatternStructuralFactsFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source.ty,
            source_solved_var,
        );
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => {
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .symbol = source.symbol,
                    .solved_var = source_solved_var,
                });
            },
            .as => |as_pat| {
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .symbol = source.symbol,
                    .solved_var = source_solved_var,
                });
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
                    const child_pattern_idx = destruct.kind.toPatternIdx();
                    const field_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, child_pattern_idx);
                    const field_solved_var = try self.requirePatternSolvedVar(type_scope, child_pattern_idx);
                    const field_bind: ast.TypedSymbol = .{
                        .ty = field_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    const field_expr = try self.program.store.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .access = .{
                            .record = source_expr,
                            .field = try self.ctx.copyExecutableIdent(module_idx, destruct.label),
                            .field_index = self.requireRecordDestructFieldIndexFact(module_idx, type_scope, destruct_idx),
                        } },
                    });
                    try decls.append(self.allocator, .{
                        .bind = field_bind,
                        .body = field_expr,
                    });
                    if (!self.patternCanCollectStructuralBindings(module_idx, child_pattern_idx)) continue;
                    try self.collectStructuralBindingDeclsWithSolvedVar(
                        module_idx,
                        type_scope,
                        field_bind,
                        field_solved_var,
                        child_pattern_idx,
                        env,
                        decls,
                    );
                }
            },
            .tuple => |tuple| {
                const elem_patterns = cir_env.store.slicePatterns(tuple.patterns);
                const source_expr = try self.makeVarExpr(source.ty, source.symbol);
                for (elem_patterns, 0..) |elem_pattern_idx, i| {
                    const elem_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, elem_pattern_idx);
                    const elem_bind: ast.TypedSymbol = .{
                        .ty = elem_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    try decls.append(self.allocator, .{
                        .bind = elem_bind,
                        .body = try self.makeTupleAccessExpr(source_expr, elem_ty, i),
                    });
                    if (!self.patternCanCollectStructuralBindings(module_idx, elem_pattern_idx)) continue;
                    try self.collectStructuralBindingDeclsWithSolvedVar(
                        module_idx,
                        type_scope,
                        elem_bind,
                        try self.requirePatternSolvedVar(type_scope, elem_pattern_idx),
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
            if (entry.typed) |typed| {
                return lowered.append(self.allocator, try self.program.store.addStmt(.{ .reassign = .{
                    .target = typed.symbol,
                    .body = source_expr,
                } }));
            }
            if (entry.local_fn_source != null) {
                debugPanic("monotype invariant violated: cannot reassign local fn source", .{});
            }
        }

        const bind = try self.requirePatternBinderWithType(module_idx, pattern_idx, source_ty);
        try self.putTypedBinding(env, key, .{ .symbol = bind.symbol });
        try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
            .bind = bind,
            .body = source_expr,
        } }));
    }

    fn lowerPatternReassignFromExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_expr: ast.ExprId,
        source_ty: type_mod.TypeId,
        source_solved_var: ?Var,
        env: *BindingEnv,
        lowered: *std.ArrayList(ast.StmtId),
    ) std.mem.Allocator.Error!void {
        try self.recordPatternStructuralFactsFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => try self.appendPatternBindingOrReassign(module_idx, pattern_idx, source_ty, source_expr, env, lowered),
            .as => |as_pat| {
                try self.appendPatternBindingOrReassign(module_idx, pattern_idx, source_ty, source_expr, env, lowered);
                try self.lowerPatternReassignFromExpr(module_idx, type_scope, as_pat.pattern, source_expr, source_ty, source_solved_var, env, lowered);
            },
            .underscore => {},
            .record_destructure => |record| {
                for (cir_env.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = cir_env.store.getRecordDestruct(destruct_idx);
                    const child_pattern_idx = destruct.kind.toPatternIdx();
                    const field_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, child_pattern_idx);
                    const field_bind: ast.TypedSymbol = .{
                        .ty = field_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    const field_expr = try self.program.store.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .access = .{
                            .record = source_expr,
                            .field = try self.ctx.copyExecutableIdent(module_idx, destruct.label),
                            .field_index = self.requireRecordDestructFieldIndexFact(module_idx, type_scope, destruct_idx),
                        } },
                    });
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                        .bind = field_bind,
                        .body = field_expr,
                    } }));
                    try self.lowerPatternReassignFromExpr(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        try self.makeVarExpr(field_ty, field_bind.symbol),
                        field_ty,
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                        env,
                        lowered,
                    );
                }
            },
            .tuple => |tuple| {
                const elem_patterns = cir_env.store.slicePatterns(tuple.patterns);
                for (elem_patterns, 0..) |elem_pattern_idx, i| {
                    const elem_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, elem_pattern_idx);
                    const elem_bind: ast.TypedSymbol = .{
                        .ty = elem_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    try lowered.append(self.allocator, try self.program.store.addStmt(.{ .decl = .{
                        .bind = elem_bind,
                        .body = try self.makeTupleAccessExpr(source_expr, elem_ty, i),
                    } }));
                    try self.lowerPatternReassignFromExpr(
                        module_idx,
                        type_scope,
                        elem_pattern_idx,
                        try self.makeVarExpr(elem_ty, elem_bind.symbol),
                        elem_ty,
                        try self.requirePatternSolvedVar(type_scope, elem_pattern_idx),
                        env,
                        lowered,
                    );
                }
            },
            .nominal => |nominal| try self.lowerPatternReassignFromExpr(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                source_expr,
                source_ty,
                source_solved_var,
                env,
                lowered,
            ),
            .nominal_external => |nominal| try self.lowerPatternReassignFromExpr(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                source_expr,
                source_ty,
                source_solved_var,
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
        source_solved_var: ?Var,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!ast.PatId {
        try self.recordPatternStructuralFactsFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const effective_source_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, pattern_idx);
        const source = try self.makePatternSourceBindWithType(module_idx, pattern_idx, effective_source_ty);
        try self.collectStructuralBindingDeclsWithSolvedVar(
            module_idx,
            type_scope,
            source,
            source_solved_var,
            pattern_idx,
            env,
            decls,
        );
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
        source_solved_var: ?Var,
        env: *BindingEnv,
        decls: *std.ArrayList(BindingDecl),
    ) std.mem.Allocator.Error!ast.PatId {
        try self.recordPatternStructuralFactsFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const effective_source_ty = self.requirePatternSourceTypeFact(module_idx, type_scope, pattern_idx);
        const cir_env = self.ctx.env(module_idx);
        const pattern = cir_env.store.getPattern(pattern_idx);

        if (self.patternNeedsBindingDecls(module_idx, pattern_idx)) {
            return self.lowerStructuralPatWithType(module_idx, type_scope, pattern_idx, effective_source_ty, source_solved_var, env, decls);
        }

        return switch (pattern) {
            .assign => |assign| blk: {
                const source: ast.TypedSymbol = .{
                    .ty = effective_source_ty,
                    .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
                };
                try self.collectStructuralBindingDeclsWithSolvedVar(
                    module_idx,
                    type_scope,
                    source,
                    source_solved_var,
                    pattern_idx,
                    env,
                    decls,
                );
                break :blk try self.program.store.addPat(.{
                    .ty = effective_source_ty,
                    .data = .{ .var_ = source.symbol },
                });
            },
            .underscore => try self.program.store.addPat(.{
                .ty = effective_source_ty,
                .data = .{ .var_ = symbol_mod.Symbol.none },
            }),
            .applied_tag => |tag| blk: {
                if (self.ctx.types.getType(effective_source_ty) == .primitive and self.ctx.types.getType(effective_source_ty).primitive == .bool) {
                    if (cir_env.store.slicePatterns(tag.args).len != 0) {
                        return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    break :blk try self.program.store.addPat(.{
                        .ty = effective_source_ty,
                        .data = .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag.name) },
                    });
                }
                const arg_pats = cir_env.store.slicePatterns(tag.args);
                const lowered_args = try self.allocator.alloc(ast.PatId, arg_pats.len);
                defer self.allocator.free(lowered_args);
                for (arg_pats, 0..) |arg_pat, i| {
                    lowered_args[i] = try self.lowerMatchPatWithType(
                        module_idx,
                        type_scope,
                        arg_pat,
                        self.requirePatternSourceTypeFact(module_idx, type_scope, arg_pat),
                        try self.requirePatternSolvedVar(type_scope, arg_pat),
                        env,
                        decls,
                    );
                }
                break :blk try self.program.store.addPat(.{
                    .ty = effective_source_ty,
                    .data = .{ .tag = .{
                        .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
                        .discriminant = self.requirePatternTagDiscriminantFact(module_idx, type_scope, pattern_idx),
                        .args = try self.program.store.addPatSpan(lowered_args),
                    } },
                });
            },
            .nominal => |nominal| blk: {
                const lowered = try self.lowerMatchPatWithType(module_idx, type_scope, nominal.backing_pattern, effective_source_ty, source_solved_var, env, decls);
                const backing = self.program.store.getPat(lowered);
                break :blk try self.program.store.addPat(.{
                    .ty = effective_source_ty,
                    .data = backing.data,
                });
            },
            .nominal_external => |nominal| blk: {
                const lowered = try self.lowerMatchPatWithType(module_idx, type_scope, nominal.backing_pattern, effective_source_ty, source_solved_var, env, decls);
                const backing = self.program.store.getPat(lowered);
                break :blk try self.program.store.addPat(.{
                    .ty = effective_source_ty,
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
            .nominal => |backing| if (child_ty == backing) parent_ty else child_ty,
            else => child_ty,
        };
    }

    fn lookupListElemSolvedVar(self: *const Lowerer, module_idx: u32, source_solved_var: ?Var) ?Var {
        const root = source_solved_var orelse return null;
        const env = self.ctx.env(module_idx);
        const resolved = env.types.resolveVar(root);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| blk: {
                    if (!nominal.ident.ident_idx.eql(self.ctx.env(module_idx).idents.list)) {
                        break :blk self.lookupListElemSolvedVar(module_idx, env.types.getNominalBackingVar(nominal));
                    }
                    const args = env.types.sliceNominalArgs(nominal);
                    if (args.len != 1) break :blk null;
                    break :blk args[0];
                },
                else => null,
            },
            .alias => |alias| self.lookupListElemSolvedVar(module_idx, env.types.getAliasBackingVar(alias)),
            else => null,
        };
    }

    fn requireListElemTypeFromMonotype(
        self: *const Lowerer,
        list_ty: type_mod.TypeId,
    ) type_mod.TypeId {
        return switch (self.ctx.types.getType(list_ty)) {
            .list => |elem_ty| elem_ty,
            else => debugPanic(
                "monotype explicit pattern fact invariant violated: expected frozen list source type, found non-list type {d}",
                .{@intFromEnum(list_ty)},
            ),
        };
    }

    fn requireTupleElemTypesFromMonotype(
        self: *const Lowerer,
        tuple_ty: type_mod.TypeId,
    ) []const type_mod.TypeId {
        return switch (self.ctx.types.getType(tuple_ty)) {
            .tuple => |elems| self.ctx.types.sliceTypeSpan(elems),
            else => debugPanic(
                "monotype explicit pattern fact invariant violated: expected frozen tuple source type, found non-tuple type {d}",
                .{@intFromEnum(tuple_ty)},
            ),
        };
    }

    fn requireRecordFieldTypeFromMonotype(
        self: *const Lowerer,
        record_ty: type_mod.TypeId,
        field_index: u16,
    ) type_mod.TypeId {
        return switch (self.ctx.types.getType(record_ty)) {
            .record => |record| blk: {
                const fields = self.ctx.types.sliceFields(record.fields);
                if (field_index >= fields.len) {
                    debugPanic(
                        "monotype explicit pattern fact invariant violated: frozen record source type missing field index {d}",
                        .{field_index},
                    );
                }
                break :blk fields[field_index].ty;
            },
            else => debugPanic(
                "monotype explicit pattern fact invariant violated: expected frozen record source type, found non-record type {d}",
                .{@intFromEnum(record_ty)},
            ),
        };
    }

    fn requireTagDiscriminantForSolvedVar(
        self: *Lowerer,
        module_idx: u32,
        union_var: Var,
        tag_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!u16 {
        const env = self.ctx.env(module_idx);
        var names = std.ArrayList(base.Ident.Idx).empty;
        defer names.deinit(self.allocator);

        var pending = std.ArrayList(Var).empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, union_var);

        while (pending.pop()) |current| {
            const resolved = env.types.resolveVar(current);
            switch (resolved.desc.content) {
                .structure => |flat| switch (flat) {
                    .nominal_type => |nominal| try pending.append(self.allocator, env.types.getNominalBackingVar(nominal)),
                    .tag_union => |tag_union| {
                        const tags = env.types.getTagsSlice(tag_union.tags);
                        try names.ensureUnusedCapacity(self.allocator, tags.len);
                        for (0..tags.len) |i| {
                            names.appendAssumeCapacity(tags.items(.name)[i]);
                        }
                        try pending.append(self.allocator, tag_union.ext);
                    },
                    .empty_tag_union => {},
                    else => debugPanic("monotype explicit semantic fact invariant violated: attempted to read tag discriminant from non-tag-union solved var", .{}),
                },
                .alias => |alias| try pending.append(self.allocator, env.types.getAliasBackingVar(alias)),
                .flex => |flex| {
                    if (flex.constraints.len() == 0) continue;
                    debugPanic("monotype explicit semantic fact invariant violated: attempted to read tag discriminant from constrained flex var", .{});
                },
                .rigid => |rigid| {
                    if (rigid.constraints.len() == 0) continue;
                    debugPanic("monotype explicit semantic fact invariant violated: attempted to read tag discriminant from constrained rigid var", .{});
                },
                .err => debugPanic("monotype explicit semantic fact invariant violated: attempted to read tag discriminant from error solved var", .{}),
            }
        }

        std.mem.sort(base.Ident.Idx, names.items, env.getIdentStoreConst(), struct {
            fn lessThan(idents: *const base.Ident.Store, a: base.Ident.Idx, b: base.Ident.Idx) bool {
                return std.mem.order(u8, idents.getText(a), idents.getText(b)) == .lt;
            }
        }.lessThan);

        var write_index: usize = 0;
        var prev: ?base.Ident.Idx = null;
        for (names.items) |name| {
            if (prev) |prev_name| {
                if (std.mem.eql(u8, env.getIdent(name), env.getIdent(prev_name))) continue;
            }
            names.items[write_index] = name;
            write_index += 1;
            prev = name;
        }

        for (names.items[0..write_index], 0..) |name, i| {
            if (name == tag_name) return @intCast(i);
        }

        debugPanic("monotype explicit semantic fact invariant violated: missing tag discriminant in solved type", .{});
    }

    fn requireRecordFieldIndexForSolvedVar(
        self: *Lowerer,
        module_idx: u32,
        record_var: Var,
        field_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!u16 {
        const env = self.ctx.env(module_idx);
        var names = std.ArrayList(base.Ident.Idx).empty;
        defer names.deinit(self.allocator);

        var pending = std.ArrayList(Var).empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, record_var);

        while (pending.pop()) |current| {
            const resolved = env.types.resolveVar(current);
            switch (resolved.desc.content) {
                .structure => |flat| switch (flat) {
                    .nominal_type => |nominal| try pending.append(self.allocator, env.types.getNominalBackingVar(nominal)),
                    .record => |record| {
                        const fields = env.types.getRecordFieldsSlice(record.fields);
                        try names.ensureUnusedCapacity(self.allocator, fields.len);
                        for (fields.items(.name)) |name| {
                            names.appendAssumeCapacity(name);
                        }
                        try pending.append(self.allocator, record.ext);
                    },
                    .record_unbound => |fields_range| {
                        const fields = env.types.getRecordFieldsSlice(fields_range);
                        try names.ensureUnusedCapacity(self.allocator, fields.len);
                        for (fields.items(.name)) |name| {
                            names.appendAssumeCapacity(name);
                        }
                    },
                    .empty_record => {},
                    else => debugPanic("monotype explicit semantic fact invariant violated: attempted to read field index from non-record solved var", .{}),
                },
                .alias => |alias| try pending.append(self.allocator, env.types.getAliasBackingVar(alias)),
                .flex => |flex| {
                    if (flex.constraints.len() == 0) continue;
                    debugPanic("monotype explicit semantic fact invariant violated: attempted to read field index from constrained flex var", .{});
                },
                .rigid => |rigid| {
                    if (rigid.constraints.len() == 0) continue;
                    debugPanic("monotype explicit semantic fact invariant violated: attempted to read field index from constrained rigid var", .{});
                },
                .err => debugPanic("monotype explicit semantic fact invariant violated: attempted to read field index from error solved var", .{}),
            }
        }

        std.mem.sort(base.Ident.Idx, names.items, env.getIdentStoreConst(), struct {
            fn lessThan(idents: *const base.Ident.Store, a: base.Ident.Idx, b: base.Ident.Idx) bool {
                return std.mem.order(u8, idents.getText(a), idents.getText(b)) == .lt;
            }
        }.lessThan);

        var write_index: usize = 0;
        var prev: ?base.Ident.Idx = null;
        for (names.items) |name| {
            if (prev) |prev_name| {
                if (std.mem.eql(u8, env.getIdent(name), env.getIdent(prev_name))) {
                    debugPanic("monotype explicit semantic fact invariant violated: duplicate record field after solved row flattening", .{});
                }
            }
            names.items[write_index] = name;
            write_index += 1;
            prev = name;
        }

        for (names.items[0..write_index], 0..) |name, i| {
            if (name == field_name) return @intCast(i);
        }

        debugPanic("monotype explicit semantic fact invariant violated: missing record field index in solved type", .{});
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
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .symbol = symbol,
                });
            },
            .as => |as_pat| {
                try self.bindPatternEnv(module_idx, type_scope, as_pat.pattern, env);
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident);
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
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

    fn bindPatternEnvFromType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        return self.bindPatternEnvFromTypeWithSolvedVar(module_idx, type_scope, pattern_idx, source_ty, null, env);
    }

    fn bindPatternEnvFromTypeWithSolvedVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        source_solved_var: ?Var,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        try self.recordPatternStructuralFactsFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const pattern = self.ctx.env(module_idx).store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign| {
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident);
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .symbol = symbol,
                    .solved_var = source_solved_var,
                });
            },
            .as => |as_pat| {
                try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, type_scope, as_pat.pattern, source_ty, source_solved_var, env);
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident);
                try self.putTypedBinding(env, .{
                    .module_idx = module_idx,
                    .pattern_idx = @intFromEnum(pattern_idx),
                }, .{
                    .symbol = symbol,
                    .solved_var = source_solved_var,
                });
            },
            .applied_tag => |tag| {
                for (self.ctx.env(module_idx).store.slicePatterns(tag.args)) |arg_pat| {
                    try self.bindPatternEnvFromTypeWithSolvedVar(
                        module_idx,
                        type_scope,
                        arg_pat,
                        self.requirePatternSourceTypeFact(module_idx, type_scope, arg_pat),
                        try self.requirePatternSolvedVar(type_scope, arg_pat),
                        env,
                    );
                }
            },
            .record_destructure => |record| {
                for (self.ctx.env(module_idx).store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const child_pattern_idx = self.ctx.env(module_idx).store.getRecordDestruct(destruct_idx).kind.toPatternIdx();
                    try self.bindPatternEnvFromTypeWithSolvedVar(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        self.requirePatternSourceTypeFact(module_idx, type_scope, child_pattern_idx),
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                        env,
                    );
                }
            },
            .tuple => |tuple| {
                for (self.ctx.env(module_idx).store.slicePatterns(tuple.patterns)) |child_pattern_idx| {
                    try self.bindPatternEnvFromTypeWithSolvedVar(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        self.requirePatternSourceTypeFact(module_idx, type_scope, child_pattern_idx),
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                        env,
                    );
                }
            },
            .list => |list| {
                for (self.ctx.env(module_idx).store.slicePatterns(list.patterns)) |child_pattern_idx| {
                    try self.bindPatternEnvFromTypeWithSolvedVar(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        self.requirePatternSourceTypeFact(module_idx, type_scope, child_pattern_idx),
                        try self.requirePatternSolvedVar(type_scope, child_pattern_idx),
                        env,
                    );
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        try self.bindPatternEnvFromTypeWithSolvedVar(
                            module_idx,
                            type_scope,
                            rest_pattern_idx,
                            self.requirePatternSourceTypeFact(module_idx, type_scope, rest_pattern_idx),
                            try self.requirePatternSolvedVar(type_scope, rest_pattern_idx),
                            env,
                        );
                    }
                }
            },
            .nominal => |nominal| try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, type_scope, nominal.backing_pattern, source_ty, source_solved_var, env),
            .nominal_external => |nominal| try self.bindPatternEnvFromTypeWithSolvedVar(module_idx, type_scope, nominal.backing_pattern, source_ty, source_solved_var, env),
            .underscore,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .runtime_error,
            => {},
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
                .ty = try self.requirePatternTypeFact(module_idx, type_scope, pattern_idx),
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            },
            .as => |as_pat| .{
                .ty = try self.requirePatternTypeFact(module_idx, type_scope, pattern_idx),
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
        const published_expected_ty = try self.publishMonotypeType(expected_ty);
        const expected_arg_tys = try self.collectCurriedArgTypes(published_expected_ty);
        errdefer self.allocator.free(expected_arg_tys);
        for (group.pending.items) |pending| {
            if (pending.source_index == source_ref.source_index and self.argTypeKeysEqual(pending.arg_tys, expected_arg_tys)) {
                self.allocator.free(expected_arg_tys);
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
            .arg_tys = expected_arg_tys,
            .symbol = specialized_symbol,
        });
        const pending_idx = group.pending.items.len - 1;

        var type_scope: TypeCloneScope = undefined;
        type_scope.initCloneAll(self.allocator, @constCast(self.ctx.env(group.module_idx)));
        defer type_scope.deinit();
        const letfn = try self.lowerLambdaLikeDefWithEnv(
            group.module_idx,
            &type_scope,
            group.declaration_env,
            specialized_symbol,
            source.expr_idx,
            false,
            source.seed_var,
            expected_var,
        );
        group.pending.items[pending_idx].stmt_id = try self.program.store.addStmt(.{
            .local_fn = letfn,
        });
        return specialized_symbol;
    }

    fn collectCurriedArgTypes(
        self: *Lowerer,
        fn_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error![]type_mod.TypeId {
        var arity: usize = 0;
        var current_ty = fn_ty;
        while (self.ctx.types.getType(current_ty) == .func) {
            arity += 1;
            current_ty = self.requireFunctionType(current_ty).ret;
        }

        const arg_tys = try self.allocator.alloc(type_mod.TypeId, arity);
        current_ty = fn_ty;
        for (0..arity) |i| {
            const fn_parts = self.requireFunctionType(current_ty);
            arg_tys[i] = fn_parts.arg;
            current_ty = fn_parts.ret;
        }
        return arg_tys;
    }

    fn collectCurriedAppliedResultTypes(
        self: *Lowerer,
        fn_ty: type_mod.TypeId,
        applied_arg_count: usize,
    ) std.mem.Allocator.Error![]type_mod.TypeId {
        const result_tys = try self.allocator.alloc(type_mod.TypeId, applied_arg_count);
        errdefer self.allocator.free(result_tys);

        var current_ty = fn_ty;
        for (0..applied_arg_count) |i| {
            const fn_parts = self.requireFunctionType(current_ty);
            result_tys[i] = fn_parts.ret;
            current_ty = fn_parts.ret;
        }

        return result_tys;
    }

    fn argTypeKeysEqual(
        self: *Lowerer,
        left: []const type_mod.TypeId,
        right: []const type_mod.TypeId,
    ) bool {
        _ = self;
        if (left.len != right.len) return false;
        for (left, right) |left_ty, right_ty| {
            if (left_ty != right_ty) return false;
        }
        return true;
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
            if (entry.local_fn_source) |source| {
                return try self.specializeLocalFnSource(source, expected_ty, expected_var);
            }
            if (entry.typed) |typed| {
                return typed.symbol;
            }
        }

        const top_level_symbol = self.lookupTopLevelSymbol(module_idx, pattern_idx) orelse debugPanic(
            "monotype invariant violated: missing local binding symbol for module {d} pattern {d}",
            .{ module_idx, @intFromEnum(pattern_idx) },
        );

        if (self.top_level_defs_by_symbol.get(top_level_symbol)) |top_level| {
            self.assertNoFirstClassBuiltinStrInspect(top_level.module_idx, top_level.def_idx, "local");
            if (top_level.is_function) {
                const source_expected_var = expected_var orelse ModuleEnv.varFrom(self.ctx.env(top_level.module_idx).store.getDef(top_level.def_idx).expr);
                const published_expected_ty = try self.publishMonotypeType(expected_ty);
                return try self.specializations.specializeFn(&self.ctx.symbols, &self.ctx.types, top_level_symbol, .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                }, published_expected_ty, source_expected_var);
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

    fn putExplicitFunctionFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: Var,
        fact: ExplicitFunctionFact,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.TypeKey = .{
            .module_idx = module_idx,
            .var_ = fn_var,
        };
        if (type_scope.facts.function_facts.get(key)) |existing| {
            defer self.allocator.free(fact.arg_vars);
            if (existing.synthetic_unit_arg != fact.synthetic_unit_arg or
                existing.node_ret_var != fact.node_ret_var or
                existing.final_ret_var != fact.final_ret_var or
                !std.mem.eql(Var, existing.arg_vars, fact.arg_vars))
            {
                debugPanic(
                    "monotype explicit function fact invariant violated: conflicting function facts in module {d}",
                    .{module_idx},
                );
            }
            return;
        }
        try type_scope.facts.function_facts.put(key, fact);
    }

    fn putExplicitFunctionTypeFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: Var,
        fact: ExplicitFunctionTypeFact,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.TypeKey = .{
            .module_idx = module_idx,
            .var_ = fn_var,
        };
        if (type_scope.facts.function_type_facts.get(key)) |existing| {
            defer self.allocator.free(fact.arg_tys);
            if (existing.synthetic_unit_arg != fact.synthetic_unit_arg or
                existing.node_ret_ty != fact.node_ret_ty or
                existing.final_ret_ty != fact.final_ret_ty or
                !std.mem.eql(type_mod.TypeId, existing.arg_tys, fact.arg_tys))
            {
                debugPanic(
                    "monotype explicit function type fact invariant violated: conflicting typed function facts in module {d}",
                    .{module_idx},
                );
            }
            return;
        }
        try type_scope.facts.function_type_facts.put(key, fact);
    }

    fn ensureExplicitFunctionFact(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: Var,
    ) std.mem.Allocator.Error!ExplicitFunctionFact {
        const key: TypeCloneScope.TypeKey = .{
            .module_idx = module_idx,
            .var_ = fn_var,
        };
        if (type_scope.facts.function_facts.get(key)) |fact| return fact;

        var arg_vars = std.ArrayList(Var).empty;
        defer arg_vars.deinit(self.allocator);

        const node_ret_var = self.lookupFunctionRetVar(module_idx, fn_var) orelse debugPanic(
            "monotype explicit function fact invariant violated: missing node return var in module {d}",
            .{module_idx},
        );
        const synthetic_unit_arg = self.lookupFunctionArgVar(module_idx, fn_var, 0) == null;
        var current = fn_var;
        while (true) {
            var local_index: usize = 0;
            while (self.lookupFunctionArgVar(module_idx, current, local_index)) |arg_var| : (local_index += 1) {
                try arg_vars.append(self.allocator, arg_var);
            }

            const ret_var = self.lookupFunctionRetVar(module_idx, current) orelse debugPanic(
                "monotype explicit function fact invariant violated: missing function return var in module {d}",
                .{module_idx},
            );
            if (self.lookupFunctionArgVar(module_idx, ret_var, 0) == null) {
                const fact: ExplicitFunctionFact = .{
                    .arg_vars = try arg_vars.toOwnedSlice(self.allocator),
                    .synthetic_unit_arg = synthetic_unit_arg,
                    .node_ret_var = node_ret_var,
                    .final_ret_var = ret_var,
                };
                try self.putExplicitFunctionFact(module_idx, type_scope, fn_var, fact);
                return type_scope.facts.function_facts.get(key).?;
            }
            current = ret_var;
        }
    }

    fn freezeExplicitFunctionTypeFacts(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!void {
        var iter = type_scope.facts.function_facts.keyIterator();
        while (iter.next()) |key_ptr| {
            if (type_scope.facts.function_type_facts.contains(key_ptr.*)) continue;

            const fn_fact = try self.ensureExplicitFunctionFact(module_idx, type_scope, key_ptr.var_);
            const arg_tys = try self.allocator.alloc(type_mod.TypeId, fn_fact.arg_vars.len);
            errdefer self.allocator.free(arg_tys);
            for (fn_fact.arg_vars, 0..) |arg_var, i| {
                arg_tys[i] = try self.publishMonotypeType(try self.lowerInstantiatedType(module_idx, type_scope, arg_var));
            }

            try self.putExplicitFunctionTypeFact(module_idx, type_scope, key_ptr.var_, .{
                .arg_tys = arg_tys,
                .synthetic_unit_arg = fn_fact.synthetic_unit_arg,
                .node_ret_ty = try self.publishMonotypeType(try self.lowerInstantiatedType(module_idx, type_scope, fn_fact.node_ret_var)),
                .final_ret_ty = try self.publishMonotypeType(try self.lowerInstantiatedType(module_idx, type_scope, fn_fact.final_ret_var)),
            });
        }
    }

    fn requireFunctionArgVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: ?Var,
        arg_index: usize,
    ) std.mem.Allocator.Error!Var {
        const function_var = fn_var orelse debugPanic(
            "monotype invariant violated: missing specialized function var in module {d}",
            .{module_idx},
        );
        return self.lookupCurriedFunctionArgVar(module_idx, type_scope, function_var, arg_index) orelse debugPanic(
            "monotype invariant violated: missing specialized function argument {d} in module {d}",
            .{ arg_index, module_idx },
        );
    }

    fn lookupCurriedFunctionArgVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: ?Var,
        arg_index: usize,
    ) ?Var {
        const function_var = fn_var orelse return null;
        const fact = self.ensureExplicitFunctionFact(module_idx, type_scope, function_var) catch return null;
        const adjusted_index = if (fact.synthetic_unit_arg) blk: {
            if (arg_index == 0) return null;
            break :blk arg_index - 1;
        } else arg_index;
        if (adjusted_index >= fact.arg_vars.len) return null;
        return fact.arg_vars[adjusted_index];
    }

    fn lookupCurriedFunctionFinalRetVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: ?Var,
    ) ?Var {
        const function_var = fn_var orelse return null;
        const fact = self.ensureExplicitFunctionFact(module_idx, type_scope, function_var) catch return null;
        return fact.final_ret_var;
    }

    fn lookupFunctionNodeRetVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: ?Var,
    ) ?Var {
        const function_var = fn_var orelse return null;
        const fact = self.ensureExplicitFunctionFact(module_idx, type_scope, function_var) catch return null;
        return fact.node_ret_var;
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
            })) |entry|
                if (entry.typed) |typed| typed.solved_var else null
            else
                null,
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
        return self.lookupSpecializedExprVar(module_idx, env, expr_idx) orelse
            try self.scopeVar(type_scope, ModuleEnv.varFrom(expr_idx));
    }

    fn scopeVar(
        _: *Lowerer,
        type_scope: *TypeCloneScope,
        source_var: Var,
    ) std.mem.Allocator.Error!Var {
        if (type_scope.var_map.get(source_var)) |scoped| return scoped;
        return try type_scope.instantiator.instantiateVar(source_var);
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

    fn unifyAppliedFunctionResultVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        result_var: Var,
        fn_var_opt: ?Var,
        applied_arg_count: usize,
    ) std.mem.Allocator.Error!void {
        const fn_var = fn_var_opt orelse debugPanic(
            "monotype invariant violated: missing explicit function fact for applied call result in module {d}",
            .{module_idx},
        );
        const fn_fact = try self.ensureExplicitFunctionFact(module_idx, type_scope, fn_var);
        const total_args = fn_fact.arg_vars.len;
        if (applied_arg_count > total_args) {
            return debugPanic(
                "monotype invariant violated: applied {d} args to function with arity {d} in module {d}",
                .{ applied_arg_count, total_args, module_idx },
            );
        }

        if (applied_arg_count == total_args) {
            return self.unifySpecializedCheckerVars(module_idx, result_var, fn_fact.final_ret_var);
        }

        const remaining_args = total_args - applied_arg_count;
        const result_fact = try self.ensureExplicitFunctionFact(module_idx, type_scope, result_var);
        if (result_fact.arg_vars.len != remaining_args) {
            debugPanic(
                "monotype invariant violated: partial-call result expected {d} remaining args but found {d} in module {d}",
                .{ remaining_args, result_fact.arg_vars.len, module_idx },
            );
        }
        for (0..remaining_args) |i| {
            const result_arg_var = result_fact.arg_vars[i];
            const fn_arg_var = fn_fact.arg_vars[applied_arg_count + i];
            try self.unifySpecializedCheckerVars(module_idx, result_arg_var, fn_arg_var);
        }

        try self.unifySpecializedCheckerVars(module_idx, result_fact.final_ret_var, fn_fact.final_ret_var);
    }

    fn requireFunctionArgType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: ?Var,
        arg_index: usize,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const function_var = fn_var orelse debugPanic(
            "monotype invariant violated: missing specialized function var in module {d}",
            .{module_idx},
        );
        const key: TypeCloneScope.TypeKey = .{
            .module_idx = module_idx,
            .var_ = function_var,
        };
        if (type_scope.facts.function_type_facts.get(key)) |typed_fact| {
            if (typed_fact.synthetic_unit_arg) {
                if (arg_index == 0) return self.makeUnitType();
                return typed_fact.arg_tys[arg_index - 1];
            }
            return typed_fact.arg_tys[arg_index];
        }

        const fact = try self.ensureExplicitFunctionFact(module_idx, type_scope, function_var);
        if (fact.synthetic_unit_arg) {
            if (arg_index == 0) return self.makeUnitType();
            return try self.lowerInstantiatedType(
                module_idx,
                type_scope,
                fact.arg_vars[arg_index - 1],
            );
        }
        return try self.lowerInstantiatedType(
            module_idx,
            type_scope,
            try self.requireFunctionArgVar(module_idx, type_scope, function_var, arg_index),
        );
    }

    fn requireFunctionRetType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: ?Var,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (fn_var) |function_var| {
            const key: TypeCloneScope.TypeKey = .{
                .module_idx = module_idx,
                .var_ = function_var,
            };
            if (type_scope.facts.function_type_facts.get(key)) |typed_fact| {
                return typed_fact.node_ret_ty;
            }
        }
        return try self.lowerInstantiatedType(
            module_idx,
            type_scope,
            self.lookupFunctionNodeRetVar(module_idx, type_scope, fn_var) orelse debugPanic(
                "monotype invariant violated: missing function return fact in module {d}",
                .{module_idx},
            ),
        );
    }

    fn functionArgCount(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fn_var: Var,
    ) usize {
        const fact = self.ensureExplicitFunctionFact(module_idx, type_scope, fn_var) catch unreachable;
        return fact.arg_vars.len + @intFromBool(fact.synthetic_unit_arg);
    }

    fn unifySpecializedCheckerVars(
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
        const before_left = try self.allocator.dupe(u8, try type_writer.writeGet(left, .one_line));
        defer self.allocator.free(before_left);
        const before_right = try self.allocator.dupe(u8, try type_writer.writeGet(right, .one_line));
        defer self.allocator.free(before_right);
        const before_left_resolved = env.types.resolveVar(left);
        const before_right_resolved = env.types.resolveVar(right);
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
                    "monotype invariant violated: specialized checker fact unify failed in module {d}: before {s} ({s}) vs {s} ({s}); after {s} vs {s}",
                    .{
                        module_idx,
                        before_left,
                        @tagName(before_left_resolved.desc.content),
                        before_right,
                        @tagName(before_right_resolved.desc.content),
                        left_snapshot,
                        right_snapshot,
                    },
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
                const published_expected_ty = try self.publishMonotypeType(expected_ty);
                return try self.specializations.specializeFn(&self.ctx.symbols, &self.ctx.types, symbol, .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                }, published_expected_ty, source_expected_var);
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

    fn lookupKnownSymbolType(self: *const Lowerer, symbol: symbol_mod.Symbol) ?type_mod.TypeId {
        if (self.emitted_defs_by_symbol.get(symbol)) |def_id| {
            return self.program.store.getDef(def_id).bind.ty;
        }

        for (self.local_fn_groups.items) |group| {
            for (group.pending.items) |pending| {
                if (pending.symbol != symbol) continue;
                const stmt_id = pending.stmt_id orelse return null;
                return self.program.store.getStmt(stmt_id).local_fn.bind.ty;
            }
        }

        return null;
    }

    fn lowerResolvedTargetCallee(
        self: *Lowerer,
        current_module_idx: u32,
        target: ResolvedTarget,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!ast.ExprId {
        const source_symbol = self.lookupTopLevelDefSymbol(target.module_idx, target.def_idx) orelse debugPanic(
            "monotype static dispatch invariant violated: unresolved target symbol {d}:{d}",
            .{ target.module_idx, @intFromEnum(target.def_idx) },
        );

        const symbol = if (self.top_level_defs_by_symbol.get(source_symbol)) |top_level|
            if (top_level.is_function) blk: {
                const source_expected_var = if (expected_var) |var_|
                    if (current_module_idx == top_level.module_idx)
                        var_
                    else
                        try self.copyCheckerVarToModule(current_module_idx, top_level.module_idx, var_)
                else
                    ModuleEnv.varFrom(self.ctx.env(top_level.module_idx).store.getDef(top_level.def_idx).expr);
                const published_expected_ty = try self.publishMonotypeType(expected_ty);
                break :blk try self.specializations.specializeFn(&self.ctx.symbols, &self.ctx.types, source_symbol, .{
                    .module_idx = top_level.module_idx,
                    .def_idx = top_level.def_idx,
                }, published_expected_ty, source_expected_var);
            } else blk: {
                try self.ensureTopLevelValueDefEmitted(source_symbol);
                break :blk source_symbol;
            }
        else
            source_symbol;

        return try self.program.store.addExpr(.{
            .ty = self.lookupKnownSymbolType(symbol) orelse expected_ty,
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
        const receiver = self.resolveDispatchReceiverForExpr(
            source_module_idx,
            type_scope,
            env,
            expr_idx,
            method_name,
        );
        return self.resolveMonomorphicDispatchTargetFromResolvedReceiver(
            source_module_idx,
            receiver,
            method_name,
        );
    }

    fn resolveMonomorphicDispatchTargetFromReceiverVar(
        self: *const Lowerer,
        source_module_idx: u32,
        type_scope: *const TypeCloneScope,
        receiver_var: Var,
        method_name: base.Ident.Idx,
    ) ResolvedTarget {
        const receiver = self.resolveDispatchReceiverVar(
            source_module_idx,
            type_scope,
            receiver_var,
            method_name,
        );
        return self.resolveMonomorphicDispatchTargetFromResolvedReceiver(
            source_module_idx,
            receiver,
            method_name,
        );
    }

    fn resolveMonomorphicDispatchTargetFromResolvedReceiver(
        self: *const Lowerer,
        source_module_idx: u32,
        receiver: DispatchReceiver,
        method_name: base.Ident.Idx,
    ) ResolvedTarget {
        const source_env = self.ctx.env(source_module_idx);
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
            if (entry.typed) |typed| return typed.symbol;
            if (entry.local_fn_source) |source| {
                return self.local_fn_groups.items[source.group_index].sources[source.source_index].source_symbol;
            }
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

    fn topLevelNeedsSpecialization(self: *const Lowerer, symbol: symbol_mod.Symbol) bool {
        return if (self.top_level_defs_by_symbol.get(symbol)) |top_level|
            top_level.is_function and top_level.needs_specialization
        else
            false;
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
