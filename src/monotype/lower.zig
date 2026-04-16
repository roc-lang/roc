//! Monotype lowering from typed CIR.
//!
//! This follows cor's strategy:
//! - lower solved checker types into a monomorphic type graph
//! - register top-level function sources once
//! - lower top-level values/runs immediately
//! - specialize top-level functions on demand from variable references
//! - keep local lambda defs as lexical function bindings for later lifting
//! - carry lexical binding metadata explicitly in recursive lowering
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
const type_clone_source = @import("type_clone_source.zig");
const specializations_mod = @import("specializations.zig");
const symbol_mod = @import("symbol");
const typed_cir = check.TypedCIR;

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const CommonIdents = ModuleEnv.CommonIdents;
const EvaluationOrder = can.DependencyGraph.EvaluationOrder;
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
    runtime_inspect_symbols: std.AutoHashMap(symbol_mod.Symbol, symbol_mod.Symbol),

    pub fn deinit(self: *Result) void {
        self.program.deinit();
        self.symbols.deinit();
        self.types.deinit();
        self.strings.deinit(self.program.store.allocator);
        self.idents.deinit(self.program.store.allocator);
        self.runtime_inspect_symbols.deinit();
    }
};

const Ctx = struct {
    allocator: std.mem.Allocator,
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    idents: base.Ident.Store,
    source_modules: *typed_cir.Modules,
    builtin_module_idx: u32,
    top_level_symbols: std.AutoHashMap(TopLevelKey, symbol_mod.Symbol),
    pattern_symbols: std.AutoHashMap(PatternKey, symbol_mod.Symbol),

    const Module = struct {
        source_module: typed_cir.Module,

        pub fn allDefs(self: @This()) []const CIR.Def.Idx {
            return self.source_module.allDefs();
        }

        pub fn nodeCount(self: @This()) usize {
            return self.source_module.nodeCount();
        }

        pub fn typeStoreConst(self: @This()) *const types.Store {
            return self.source_module.typeStoreConst();
        }

        pub fn identStoreConst(self: @This()) *const base.Ident.Store {
            return self.source_module.identStoreConst();
        }

        pub fn commonIdents(self: @This()) CommonIdents {
            return self.source_module.commonIdents();
        }

        pub fn qualifiedModuleIdent(self: @This()) Ident.Idx {
            return self.source_module.qualifiedModuleIdent();
        }

        pub fn evaluationOrder(self: @This()) ?*const EvaluationOrder {
            return self.source_module.evaluationOrder();
        }

        pub fn findCommonIdent(self: @This(), text: []const u8) ?Ident.Idx {
            return self.identStoreConst().findByString(text);
        }

        pub fn getIdent(self: @This(), idx: Ident.Idx) []const u8 {
            return self.identStoreConst().getText(idx);
        }

        pub fn getString(self: @This(), idx: StringLiteral.Idx) []const u8 {
            return self.source_module.getString(idx);
        }

        pub fn name(self: @This()) []const u8 {
            return self.source_module.name();
        }

        pub fn resolvedImportModule(self: @This(), import_idx: CIR.Import.Idx) ?u32 {
            return self.source_module.resolvedImportModule(import_idx);
        }

        pub fn requiresTypes(self: @This()) []const ModuleEnv.RequiredType {
            return self.source_module.requiresTypes();
        }

        pub fn nodeTag(self: @This(), idx: CIR.Node.Idx) CIR.Node.Tag {
            return self.source_module.nodeTag(idx);
        }

        pub fn regionAt(self: @This(), idx: CIR.Node.Idx) base.Region {
            return self.source_module.regionAt(idx);
        }

        pub fn sliceDefs(self: @This(), span: CIR.Def.Span) []const CIR.Def.Idx {
            return self.source_module.sliceDefs(span);
        }

        pub fn def(self: @This(), idx: CIR.Def.Idx) typed_cir.Def {
            return self.source_module.def(idx);
        }

        pub fn defType(self: @This(), idx: CIR.Def.Idx) Var {
            return self.source_module.defType(idx);
        }

        pub fn expr(self: @This(), idx: CIR.Expr.Idx) typed_cir.Expr {
            return self.source_module.expr(idx);
        }

        pub fn pattern(self: @This(), idx: CIR.Pattern.Idx) typed_cir.Pattern {
            return self.source_module.pattern(idx);
        }

        pub fn exprType(self: @This(), idx: CIR.Expr.Idx) Var {
            return self.source_module.exprType(idx);
        }

        pub fn patternType(self: @This(), idx: CIR.Pattern.Idx) Var {
            return self.source_module.patternType(idx);
        }

        pub fn typeAnnoType(self: @This(), idx: CIR.TypeAnno.Idx) Var {
            return self.source_module.typeAnnoType(idx);
        }

        pub fn exprIdxFromTypeVar(self: @This(), var_: Var) ?CIR.Expr.Idx {
            return self.source_module.exprIdxFromTypeVar(var_);
        }

        pub fn resolveAttachedMethodTargetByIdents(
            self: @This(),
            local_type_ident: Ident.Idx,
            local_method_ident: Ident.Idx,
        ) ?typed_cir.Modules.ResolvedMethodTarget {
            return self.source_module.resolveAttachedMethodTargetByIdents(local_type_ident, local_method_ident);
        }

        pub fn resolveAttachedMethodTargetByText(
            self: @This(),
            type_name: []const u8,
            method_name: []const u8,
        ) ?typed_cir.Modules.ResolvedMethodTarget {
            const local_type_ident = self.findCommonIdent(type_name) orelse return null;
            const local_method_ident = self.findCommonIdent(method_name) orelse return null;
            return self.resolveAttachedMethodTargetByIdents(local_type_ident, local_method_ident);
        }

        pub fn getStatement(self: @This(), idx: CIR.Statement.Idx) CIR.Statement {
            return self.source_module.getStatement(idx);
        }

        pub fn getRecordField(self: @This(), idx: CIR.RecordField.Idx) CIR.RecordField {
            return self.source_module.getRecordField(idx);
        }

        pub fn getRecordDestruct(self: @This(), idx: CIR.Pattern.RecordDestruct.Idx) CIR.Pattern.RecordDestruct {
            return self.source_module.getRecordDestruct(idx);
        }

        pub fn getIfBranch(self: @This(), idx: CIR.Expr.IfBranch.Idx) CIR.Expr.IfBranch {
            return self.source_module.getIfBranch(idx);
        }

        pub fn getMatchBranch(self: @This(), idx: CIR.Expr.Match.Branch.Idx) CIR.Expr.Match.Branch {
            return self.source_module.getMatchBranch(idx);
        }

        pub fn getMatchBranchPattern(self: @This(), idx: CIR.Expr.Match.BranchPattern.Idx) CIR.Expr.Match.BranchPattern {
            return self.source_module.getMatchBranchPattern(idx);
        }

        pub fn sliceExpr(self: @This(), span: CIR.Expr.Span) []const CIR.Expr.Idx {
            return self.source_module.sliceExpr(span);
        }

        pub fn slicePatterns(self: @This(), span: CIR.Pattern.Span) []const CIR.Pattern.Idx {
            return self.source_module.slicePatterns(span);
        }

        pub fn sliceStatements(self: @This(), span: CIR.Statement.Span) []const CIR.Statement.Idx {
            return self.source_module.sliceStatements(span);
        }

        pub fn sliceRecordFields(self: @This(), span: CIR.RecordField.Span) []const CIR.RecordField.Idx {
            return self.source_module.sliceRecordFields(span);
        }

        pub fn sliceRecordDestructs(self: @This(), span: CIR.Pattern.RecordDestruct.Span) []const CIR.Pattern.RecordDestruct.Idx {
            return self.source_module.sliceRecordDestructs(span);
        }

        pub fn sliceIfBranches(self: @This(), span: CIR.Expr.IfBranch.Span) []const CIR.Expr.IfBranch.Idx {
            return self.source_module.sliceIfBranches(span);
        }

        pub fn matchBranchSlice(self: @This(), span: CIR.Expr.Match.Branch.Span) []const CIR.Expr.Match.Branch.Idx {
            return self.source_module.matchBranchSlice(span);
        }

        pub fn sliceMatchBranchPatterns(self: @This(), span: CIR.Expr.Match.BranchPattern.Span) []const CIR.Expr.Match.BranchPattern.Idx {
            return self.source_module.sliceMatchBranchPatterns(span);
        }
    };

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
        typed_cir_modules: *typed_cir.Modules,
        builtin_module_idx: u32,
    ) std.mem.Allocator.Error!Ctx {
        return .{
            .allocator = allocator,
            .symbols = symbol_mod.Store.init(allocator),
            .types = type_mod.Store.init(allocator),
            .idents = try base.Ident.Store.initCapacity(allocator, 256),
            .source_modules = typed_cir_modules,
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

    fn typedCirModule(self: *const Ctx, module_idx: u32) Module {
        return .{
            .source_module = self.source_modules.module(module_idx),
        };
    }

    fn moduleCount(self: *const Ctx) usize {
        return self.source_modules.moduleCount();
    }

    fn findModuleIdxByName(self: *const Ctx, target_name: []const u8) u32 {
        for (0..self.source_modules.moduleCount()) |idx| {
            if (std.mem.eql(u8, self.source_modules.module(@intCast(idx)).name(), target_name)) return @intCast(idx);
        }
        return debugPanic(
            "monotype invariant violated: missing target module {s}",
            .{target_name},
        );
    }

    fn getTypeIdentText(self: *const Ctx, module_idx: u32, ident: base.Ident.Idx) []const u8 {
        return self.typedCirModule(module_idx).getIdent(ident);
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
        const text = self.typedCirModule(module_idx).getIdent(ident);
        return self.copyExecutableIdentText(text);
    }

    fn copyExecutableIdentFromStore(
        self: *Ctx,
        idents: *const base.Ident.Store,
        ident: base.Ident.Idx,
    ) std.mem.Allocator.Error!base.Ident.Idx {
        if (ident.isNone()) return ident;
        return self.copyExecutableIdentText(idents.getText(ident));
    }

    fn copyExecutableIdentText(
        self: *Ctx,
        text: []const u8,
    ) std.mem.Allocator.Error!base.Ident.Idx {
        return self.idents.insert(self.allocator, base.Ident.for_text(text));
    }
};

const ResolvedTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const ExpectedType = struct {
    ty: type_mod.TypeId,
    solved_var: ?Var,
};

const LoweredExprInfo = struct {
    expr: ast.ExprId,
    ty: type_mod.TypeId,
    solved_var: Var,
};

const LoweredCall = struct {
    data: @FieldType(ast.Expr.Data, "call"),
    result_ty: type_mod.TypeId,
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
    forced_runtime_symbols: std.AutoHashMap(symbol_mod.Symbol, void),
    runtime_inspect_symbols: std.AutoHashMap(symbol_mod.Symbol, symbol_mod.Symbol),
    local_fn_groups: std.ArrayList(*LocalFnGroupState),
    required_app_module_idx: ?u32 = null,

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

    const FrozenCheckerVar = specializations_mod.FrozenCheckerVar;

    const FrozenTypedBinding = struct {
        symbol: symbol_mod.Symbol,
        checker_var: ?FrozenCheckerVar = null,

        fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            if (self.checker_var) |*checker_var| {
                checker_var.deinit(allocator);
            }
        }
    };

    const BindingValue = struct {
        typed: ?TypedBinding = null,
        local_fn_source: ?LocalFnSourceRef = null,
        local_fn_seed_var: ?Var = null,
    };

    const BindingEnv = std.AutoHashMap(PatternKey, BindingValue);

    const FrozenBindingValue = struct {
        typed: ?FrozenTypedBinding = null,
        local_fn_source: ?LocalFnSourceRef = null,
        local_fn_seed_var: ?FrozenCheckerVar = null,

        fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            if (self.typed) |*typed| {
                typed.deinit(allocator);
            }
            if (self.local_fn_seed_var) |*seed| {
                seed.deinit(allocator);
            }
        }
    };

    const FrozenBindingEnv = std.AutoHashMap(PatternKey, FrozenBindingValue);

    fn putTypedBinding(
        _: *Lowerer,
        env: *BindingEnv,
        key: PatternKey,
        typed: TypedBinding,
    ) std.mem.Allocator.Error!void {
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
        _: *Lowerer,
        env: *BindingEnv,
        key: PatternKey,
        source: LocalFnSourceRef,
    ) std.mem.Allocator.Error!void {
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
        _: *Lowerer,
        env: *BindingEnv,
        key: PatternKey,
        seed_var: Var,
    ) std.mem.Allocator.Error!void {
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
        declaration_env: FrozenBindingEnv,
        insertion_index: usize,
        sources: []LocalFnSource,
        pending: std.ArrayList(LocalFnPending),

        fn deinit(self: *LocalFnGroupState, allocator: std.mem.Allocator) void {
            var env_iter = self.declaration_env.valueIterator();
            while (env_iter.next()) |value| {
                value.deinit(allocator);
            }
            self.declaration_env.deinit();
            allocator.free(self.sources);
            for (self.pending.items) |pending| allocator.free(pending.arg_tys);
            self.pending.deinit(allocator);
        }
    };

    const TypeWorkspace = struct {
        module_idx: u32,
        type_store: types.Store,
        ident_store: base.Ident.Store,

        fn initEmpty(
            allocator: std.mem.Allocator,
            module: Ctx.Module,
        ) std.mem.Allocator.Error!TypeWorkspace {
            return .{
                .module_idx = module.source_module.module_idx,
                .type_store = try types.Store.init(allocator),
                .ident_store = try base.Ident.Store.initCapacity(allocator, 16),
            };
        }

        fn deinit(self: *TypeWorkspace, allocator: std.mem.Allocator) void {
            self.ident_store.deinit(allocator);
            self.type_store.deinit();
        }
    };

    const TypeCloneScope = struct {
        const TypeKey = struct {
            module_idx: u32,
            var_: Var,
        };
        const ExprKey = struct {
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
        };
        const PatternTypeKey = struct {
            module_idx: u32,
            pattern_idx: CIR.Pattern.Idx,
        };
        const RecordDestructKey = struct {
            module_idx: u32,
            destruct_idx: CIR.Pattern.RecordDestruct.Idx,
        };

        const Memo = struct {
            expr_result_var_map: std.AutoHashMap(ExprKey, Var),
            expr_type_cache: std.AutoHashMap(ExprKey, type_mod.TypeId),
            pattern_type_cache: std.AutoHashMap(PatternTypeKey, type_mod.TypeId),
            pattern_source_type_cache: std.AutoHashMap(PatternTypeKey, type_mod.TypeId),
            pattern_tag_discriminant_cache: std.AutoHashMap(PatternTypeKey, u16),
            pattern_list_elem_type_cache: std.AutoHashMap(PatternTypeKey, type_mod.TypeId),
            record_destruct_field_index_cache: std.AutoHashMap(RecordDestructKey, u16),
            collected_expr: std.AutoHashMap(ExprKey, void),
            collected_pattern: std.AutoHashMap(PatternTypeKey, void),

            fn init(allocator: std.mem.Allocator) Memo {
                return .{
                    .expr_result_var_map = std.AutoHashMap(ExprKey, Var).init(allocator),
                    .expr_type_cache = std.AutoHashMap(ExprKey, type_mod.TypeId).init(allocator),
                    .pattern_type_cache = std.AutoHashMap(PatternTypeKey, type_mod.TypeId).init(allocator),
                    .pattern_source_type_cache = std.AutoHashMap(PatternTypeKey, type_mod.TypeId).init(allocator),
                    .pattern_tag_discriminant_cache = std.AutoHashMap(PatternTypeKey, u16).init(allocator),
                    .pattern_list_elem_type_cache = std.AutoHashMap(PatternTypeKey, type_mod.TypeId).init(allocator),
                    .record_destruct_field_index_cache = std.AutoHashMap(RecordDestructKey, u16).init(allocator),
                    .collected_expr = std.AutoHashMap(ExprKey, void).init(allocator),
                    .collected_pattern = std.AutoHashMap(PatternTypeKey, void).init(allocator),
                };
            }

            fn deinit(self: *Memo) void {
                self.record_destruct_field_index_cache.deinit();
                self.pattern_list_elem_type_cache.deinit();
                self.pattern_tag_discriminant_cache.deinit();
                self.pattern_source_type_cache.deinit();
                self.collected_pattern.deinit();
                self.collected_expr.deinit();
                self.pattern_type_cache.deinit();
                self.expr_type_cache.deinit();
                self.expr_result_var_map.deinit();
            }
        };

        allocator: std.mem.Allocator,
        workspace: *TypeWorkspace,
        owns_workspace: bool,
        copied_source_var_map: type_clone_source.ScopedVarMapping,
        source_var_map: std.AutoHashMap(TypeKey, Var),
        instantiation_var_map: std.AutoHashMap(Var, Var),
        active_type_cache: std.AutoHashMap(TypeKey, type_mod.TypeId),
        provisional_type_cache: std.AutoHashMap(TypeKey, type_mod.TypeId),
        type_cache: std.AutoHashMap(TypeKey, type_mod.TypeId),
        memo: Memo,
        nominal_type_cache: std.StringHashMap(type_mod.TypeId),
        scratch_nominal_key: std.ArrayList(u8),
        instantiator: Instantiator,

        fn initCloneAll(
            self: *TypeCloneScope,
            allocator: std.mem.Allocator,
            module: Ctx.Module,
        ) std.mem.Allocator.Error!void {
            try self.initCloneAllFromParent(allocator, module, null, false);
        }

        fn initCloneAllFromParent(
            self: *TypeCloneScope,
            allocator: std.mem.Allocator,
            module: Ctx.Module,
            parent: ?*const TypeCloneScope,
            inherit_specialization_state: bool,
        ) std.mem.Allocator.Error!void {
            self.allocator = allocator;
            if (parent) |scope| {
                self.workspace = scope.workspace;
                self.owns_workspace = false;
            } else {
                self.workspace = try allocator.create(TypeWorkspace);
                errdefer allocator.destroy(self.workspace);
                self.workspace.* = try TypeWorkspace.initEmpty(allocator, module);
                self.owns_workspace = true;
            }

            self.initWithRankBehavior(.ignore_rank);
            if (parent) |scope| {
                var expr_result_iter = scope.memo.expr_result_var_map.iterator();
                while (expr_result_iter.next()) |entry| {
                    try self.memo.expr_result_var_map.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var expr_type_iter = scope.memo.expr_type_cache.iterator();
                while (expr_type_iter.next()) |entry| {
                    try self.memo.expr_type_cache.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var pattern_type_iter = scope.memo.pattern_type_cache.iterator();
                while (pattern_type_iter.next()) |entry| {
                    try self.memo.pattern_type_cache.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var pattern_source_iter = scope.memo.pattern_source_type_cache.iterator();
                while (pattern_source_iter.next()) |entry| {
                    try self.memo.pattern_source_type_cache.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var pattern_tag_iter = scope.memo.pattern_tag_discriminant_cache.iterator();
                while (pattern_tag_iter.next()) |entry| {
                    try self.memo.pattern_tag_discriminant_cache.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var pattern_list_iter = scope.memo.pattern_list_elem_type_cache.iterator();
                while (pattern_list_iter.next()) |entry| {
                    try self.memo.pattern_list_elem_type_cache.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var record_destruct_iter = scope.memo.record_destruct_field_index_cache.iterator();
                while (record_destruct_iter.next()) |entry| {
                    try self.memo.record_destruct_field_index_cache.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var collected_expr_iter = scope.memo.collected_expr.iterator();
                while (collected_expr_iter.next()) |entry| {
                    try self.memo.collected_expr.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var collected_pattern_iter = scope.memo.collected_pattern.iterator();
                while (collected_pattern_iter.next()) |entry| {
                    try self.memo.collected_pattern.put(entry.key_ptr.*, entry.value_ptr.*);
                }

                if (!inherit_specialization_state) return;
                var iter = scope.source_var_map.iterator();
                while (iter.next()) |entry| {
                    try self.source_var_map.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var copied_iter = scope.copied_source_var_map.iterator();
                while (copied_iter.next()) |entry| {
                    try self.copied_source_var_map.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var inst_iter = scope.instantiation_var_map.iterator();
                while (inst_iter.next()) |entry| {
                    try self.instantiation_var_map.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var type_iter = scope.type_cache.iterator();
                while (type_iter.next()) |entry| {
                    try self.type_cache.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                var provisional_iter = scope.provisional_type_cache.iterator();
                while (provisional_iter.next()) |entry| {
                    try self.provisional_type_cache.put(entry.key_ptr.*, entry.value_ptr.*);
                }
            }
        }

        fn initWithRankBehavior(
            self: *TypeCloneScope,
            rank_behavior: Instantiator.RankBehavior,
        ) void {
            self.source_var_map = std.AutoHashMap(TypeKey, Var).init(self.allocator);
            self.copied_source_var_map = type_clone_source.ScopedVarMapping.init(self.allocator);
            self.instantiation_var_map = std.AutoHashMap(Var, Var).init(self.allocator);
            self.active_type_cache = std.AutoHashMap(TypeKey, type_mod.TypeId).init(self.allocator);
            self.provisional_type_cache = std.AutoHashMap(TypeKey, type_mod.TypeId).init(self.allocator);
            self.type_cache = std.AutoHashMap(TypeKey, type_mod.TypeId).init(self.allocator);
            self.memo = Memo.init(self.allocator);
            self.nominal_type_cache = std.StringHashMap(type_mod.TypeId).init(self.allocator);
            self.scratch_nominal_key = .empty;
            self.instantiator = .{
                .store = &self.workspace.type_store,
                .idents = &self.workspace.ident_store,
                .var_map = &self.instantiation_var_map,
                .current_rank = .outermost,
                .rigid_behavior = .fresh_flex,
                .rank_behavior = rank_behavior,
            };
        }

        fn typeStoreConst(self: *const TypeCloneScope) *const types.Store {
            return &self.workspace.type_store;
        }

        fn typeStoreMut(self: *TypeCloneScope) *types.Store {
            return &self.workspace.type_store;
        }

        fn identStoreConst(self: *const TypeCloneScope) *const base.Ident.Store {
            return &self.workspace.ident_store;
        }

        fn identStoreMut(self: *TypeCloneScope) *base.Ident.Store {
            return &self.workspace.ident_store;
        }

        fn getIdent(self: *const TypeCloneScope, idx: base.Ident.Idx) []const u8 {
            return self.identStoreConst().getText(idx);
        }

        fn deinit(self: *TypeCloneScope) void {
            var nominal_keys = self.nominal_type_cache.keyIterator();
            while (nominal_keys.next()) |key_ptr| {
                self.instantiator.store.gpa.free(key_ptr.*);
            }
            self.nominal_type_cache.deinit();
            self.scratch_nominal_key.deinit(self.instantiator.store.gpa);
            self.memo.deinit();
            self.active_type_cache.deinit();
            self.provisional_type_cache.deinit();
            self.type_cache.deinit();
            self.instantiation_var_map.deinit();
            self.copied_source_var_map.deinit();
            self.source_var_map.deinit();
            if (self.owns_workspace) {
                self.workspace.deinit(self.allocator);
                self.allocator.destroy(self.workspace);
            }
        }
    };

    const CallInfo = struct {
        call_scope: *TypeCloneScope,
        fn_var: Var,
        fn_ty: type_mod.TypeId,
        arg_vars: []Var,
        arg_tys: []type_mod.TypeId,
        applied_result_tys: []type_mod.TypeId,
        result_ty: type_mod.TypeId,

        fn deinit(self: *CallInfo, allocator: std.mem.Allocator) void {
            allocator.free(self.applied_result_tys);
            allocator.free(self.arg_tys);
            allocator.free(self.arg_vars);
            self.call_scope.deinit();
            allocator.destroy(self.call_scope);
        }
    };

    pub fn init(
        allocator: std.mem.Allocator,
        typed_cir_modules: *typed_cir.Modules,
        builtin_module_idx: u32,
        app_module_idx: ?u32,
    ) std.mem.Allocator.Error!Lowerer {
        return .{
            .allocator = allocator,
            .ctx = try Ctx.init(allocator, typed_cir_modules, builtin_module_idx),
            .program = Program.init(allocator),
            .strings = .{},
            .specializations = specializations_mod.Queue.init(allocator),
            .top_level_defs_by_symbol = std.AutoHashMap(symbol_mod.Symbol, TopLevelDef).init(allocator),
            .top_level_symbols_by_pattern = std.AutoHashMap(PatternKey, symbol_mod.Symbol).init(allocator),
            .emitted_defs_by_symbol = std.AutoHashMap(symbol_mod.Symbol, ast.DefId).init(allocator),
            .emitting_value_defs = std.AutoHashMap(symbol_mod.Symbol, void).init(allocator),
            .forced_runtime_symbols = std.AutoHashMap(symbol_mod.Symbol, void).init(allocator),
            .runtime_inspect_symbols = std.AutoHashMap(symbol_mod.Symbol, symbol_mod.Symbol).init(allocator),
            .local_fn_groups = .empty,
            .required_app_module_idx = app_module_idx,
        };
    }

    pub fn deinit(self: *Lowerer) void {
        for (self.local_fn_groups.items) |group| {
            group.deinit(self.allocator);
            self.allocator.destroy(group);
        }
        self.local_fn_groups.deinit(self.allocator);
        self.runtime_inspect_symbols.deinit();
        self.forced_runtime_symbols.deinit();
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
        try self.drainPendingLoweringWork();
        try self.finalizeProgramTypes();

        const result = Result{
            .program = self.program,
            .symbols = self.ctx.symbols,
            .types = self.ctx.types,
            .strings = self.strings,
            .idents = self.ctx.idents,
            .runtime_inspect_symbols = self.runtime_inspect_symbols,
        };
        self.program = Program.init(self.allocator);
        self.ctx.symbols = symbol_mod.Store.init(self.allocator);
        self.ctx.types = type_mod.Store.init(self.allocator);
        self.ctx.idents = try base.Ident.Store.initCapacity(self.allocator, 1);
        self.strings = .{};
        self.runtime_inspect_symbols = std.AutoHashMap(symbol_mod.Symbol, symbol_mod.Symbol).init(self.allocator);
        return result;
    }

    fn drainPendingLoweringWork(self: *Lowerer) std.mem.Allocator.Error!void {
        while (true) {
            const forced_any = try self.forcePendingRuntimeSymbols();
            const specialized_any = try self.drainSpecializations();
            if (!forced_any and !specialized_any) break;
        }
    }

    fn forcePendingRuntimeSymbols(self: *Lowerer) std.mem.Allocator.Error!bool {
        var any_forced = false;
        var iter = self.forced_runtime_symbols.iterator();
        while (iter.next()) |entry| {
            const symbol = entry.key_ptr.*;
            if (self.runtime_inspect_symbols.contains(symbol)) continue;
            const top_level = self.top_level_defs_by_symbol.get(symbol) orelse
                debugPanic("monotype invariant violated: missing forced runtime symbol in top-level defs", .{});
            if (top_level.is_function) {
                const runtime_symbol = try self.specializeTopLevelDef(top_level.module_idx, top_level.def_idx);
                try self.runtime_inspect_symbols.put(symbol, runtime_symbol);
            } else {
                try self.ensureTopLevelValueDefEmitted(symbol);
                try self.runtime_inspect_symbols.put(symbol, symbol);
            }
            any_forced = true;
        }
        return any_forced;
    }

    /// Force specialization (or emission) of a top-level definition.
    /// Returns the symbol that should be used to call the def at runtime.
    pub fn specializeTopLevelDef(
        self: *Lowerer,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        try self.registerAllTopLevelDefs();
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const solved_def = typed_cir_module.def(def_idx);

        const symbol = self.lookupTopLevelDefSymbol(module_idx, def_idx) orelse debugPanic(
            "monotype invariant violated: missing top-level symbol for module {d} def {d}",
            .{ module_idx, @intFromEnum(def_idx) },
        );
        const top_level = self.top_level_defs_by_symbol.get(symbol) orelse debugPanic(
            "monotype invariant violated: missing top-level def for symbol {d}",
            .{symbol.raw()},
        );

        if (!top_level.is_function) {
            try self.ensureTopLevelValueDefEmitted(symbol);
            return symbol;
        }

        var type_scope: TypeCloneScope = undefined;
        try type_scope.initCloneAll(self.allocator, typed_cir_module);
        defer type_scope.deinit();
        var binding_env = BindingEnv.init(self.allocator);
        defer binding_env.deinit();

        const bind_var = solved_def.pattern.ty();
        const expected_var = try self.scopeVar(&type_scope, module_idx, bind_var);
        try self.collectExprInfoWithResultVar(
            module_idx,
            &type_scope,
            binding_env,
            solved_def.expr.idx,
            expected_var,
        );
        try self.finalizeExprTypes(module_idx, &type_scope);
        try self.finalizePatternTypes(module_idx, &type_scope);

        const expected_ty = try self.requireExprType(module_idx, &type_scope, solved_def.expr.idx);
        const expected_checker_seed = try self.freezeCheckerVarFromScope(&type_scope, expected_var);

        return try self.specializations.specializeFn(
            &self.ctx.symbols,
            &self.ctx.types,
            symbol,
            .{ .module_idx = module_idx, .def_idx = def_idx },
            expected_ty,
            expected_checker_seed,
        );
    }

    fn specializeTopLevelFromCallSite(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        top_level_symbol: symbol_mod.Symbol,
        top_level: TopLevelDef,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const source_root_var = self.ctx.typedCirModule(top_level.module_idx).defType(top_level.def_idx);
        const specialization_root_var = if (expected_var) |workspace_expected_var|
            workspace_expected_var
        else
            try self.scopeVar(type_scope, top_level.module_idx, source_root_var);
        const expected_checker_seed = try self.freezeCheckerVarFromScope(
            type_scope,
            specialization_root_var,
        );
        return try self.specializations.specializeFn(
            &self.ctx.symbols,
            &self.ctx.types,
            top_level_symbol,
            .{
                .module_idx = top_level.module_idx,
                .def_idx = top_level.def_idx,
            },
            expected_ty,
            expected_checker_seed,
        );
    }

    /// Lower a single expression as the root entrypoint for testing/evaluation.
    /// This preserves the normal specialization pipeline while avoiding reparsing.
    pub fn runRootExpr(
        self: *Lowerer,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!Result {
        try self.registerAllTopLevelDefs();
        const def_id = try self.lowerRootExpr(module_idx, expr_idx);
        try self.program.root_defs.append(self.allocator, def_id);
        try self.drainPendingLoweringWork();
        try self.finalizeProgramTypes();

        const result = Result{
            .program = self.program,
            .symbols = self.ctx.symbols,
            .types = self.ctx.types,
            .strings = self.strings,
            .idents = self.ctx.idents,
            .runtime_inspect_symbols = self.runtime_inspect_symbols,
        };
        self.program = Program.init(self.allocator);
        self.ctx.symbols = symbol_mod.Store.init(self.allocator);
        self.ctx.types = type_mod.Store.init(self.allocator);
        self.ctx.idents = try base.Ident.Store.initCapacity(self.allocator, 1);
        self.runtime_inspect_symbols = std.AutoHashMap(symbol_mod.Symbol, symbol_mod.Symbol).init(self.allocator);
        self.strings = .{};
        return result;
    }

    fn finalizeTypedSymbol(self: *Lowerer, bind: *ast.TypedSymbol) std.mem.Allocator.Error!void {
        _ = self;
        _ = bind;
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

        const pat = &self.program.store.pats.items[idx];
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

        const expr = &self.program.store.exprs.items[idx];
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
            .tag_payload => |tag_payload| try self.finalizeExprById(tag_payload.tag_union, visited),
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
            .hosted_fn => |*hosted_fn| {
                try self.finalizeTypedSymbol(&hosted_fn.bind);
                for (self.program.store.sliceTypedSymbolSpanMut(hosted_fn.args)) |*arg| {
                    try self.finalizeTypedSymbol(arg);
                }
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
        for (0..self.ctx.moduleCount()) |module_idx| {
            const typed_cir_module = self.ctx.typedCirModule(@intCast(module_idx));
            for (typed_cir_module.allDefs()) |def_idx| {
                const typed_cir_def = typed_cir_module.def(def_idx);
                if (typed_cir_def.data.kind != .let) continue;

                const ident = switch (typed_cir_def.pattern.data) {
                    .assign => |assign| assign.ident,
                    else => continue,
                };

                const symbol = try self.ctx.getOrCreateTopLevelSymbol(@intCast(module_idx), def_idx, ident);
                try self.top_level_defs_by_symbol.put(symbol, .{
                    .module_idx = @intCast(module_idx),
                    .def_idx = def_idx,
                    .pattern_idx = typed_cir_def.pattern.idx,
                    .is_function = isLambdaExpr(typed_cir_def.expr.data),
                    .needs_specialization = typed_cir_module.typeStoreConst().needsInstantiation(typed_cir_def.expr.ty()),
                });
                try self.top_level_symbols_by_pattern.put(.{
                    .module_idx = @intCast(module_idx),
                    .pattern_idx = @intFromEnum(typed_cir_def.pattern.idx),
                }, symbol);
            }
        }
    }

    fn lowerRootModule(self: *Lowerer, module_idx: u32) std.mem.Allocator.Error!void {
        for (self.ctx.typedCirModule(module_idx).allDefs()) |def_idx| {
            const lowered = try self.lowerTopLevelDef(module_idx, def_idx);
            if (lowered) |def_id| {
                try self.program.root_defs.append(self.allocator, def_id);
            }
        }
    }

    fn lowerRootExpr(
        self: *Lowerer,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.DefId {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);

        var type_scope: TypeCloneScope = undefined;
        try type_scope.initCloneAll(self.allocator, typed_cir_module);
        defer type_scope.deinit();
        var binding_env = BindingEnv.init(self.allocator);
        defer binding_env.deinit();

        const expr_var = typed_cir_module.expr(expr_idx).ty();
        const expected_var = try self.scopeVar(&type_scope, module_idx, expr_var);
        try self.collectExprInfoWithResultVar(
            module_idx,
            &type_scope,
            binding_env,
            expr_idx,
            expected_var,
        );
        try self.finalizeExprTypes(module_idx, &type_scope);
        try self.finalizePatternTypes(module_idx, &type_scope);
        const expected_ty = try self.requireExprType(module_idx, &type_scope, expr_idx);
        const body = try self.lowerExprInfoWithExpectedType(
            module_idx,
            &type_scope,
            binding_env,
            expr_idx,
            expected_ty,
            expected_var,
        );

        const bind_symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE);
        return try self.program.store.addDef(.{
            .bind = .{ .ty = body.ty, .symbol = bind_symbol },
            .value = .{ .run = .{
                .bind = .{ .ty = body.ty, .symbol = bind_symbol },
                .body = body.expr,
                .entry_ty = expr_var,
            } },
        });
    }

    fn lowerTopLevelDef(
        self: *Lowerer,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
    ) std.mem.Allocator.Error!?ast.DefId {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const solved_def = typed_cir_module.def(def_idx);

        if (solved_def.data.kind == .let) {
            const bind_symbol = self.lookupTopLevelSymbol(module_idx, solved_def.pattern.idx) orelse return null;
            if (self.emitted_defs_by_symbol.get(bind_symbol)) |existing| return existing;
            if (solved_def.expr.data == .e_hosted_lambda) {
                return try self.lowerHostedTopLevelDef(module_idx, def_idx, bind_symbol, null);
            }
        }

        return switch (solved_def.data.kind) {
            .let => blk: {
                const bind_symbol = self.lookupTopLevelSymbol(module_idx, solved_def.pattern.idx) orelse break :blk null;
                if (self.emitted_defs_by_symbol.get(bind_symbol)) |existing| break :blk existing;
                if (self.isTopLevelFunction(bind_symbol)) break :blk null;

                var type_scope: TypeCloneScope = undefined;
                try type_scope.initCloneAll(self.allocator, typed_cir_module);
                defer type_scope.deinit();
                var binding_env = BindingEnv.init(self.allocator);
                defer binding_env.deinit();
                const bind_var = solved_def.pattern.ty();
                const bind_expected_var = try self.scopeVar(&type_scope, module_idx, bind_var);
                try self.collectExprInfoWithResultVar(
                    module_idx,
                    &type_scope,
                    binding_env,
                    solved_def.expr.idx,
                    bind_expected_var,
                );
                try self.finalizeExprTypes(module_idx, &type_scope);
                try self.finalizePatternTypes(module_idx, &type_scope);
                const bind_expected_ty = try self.requireExprType(module_idx, &type_scope, solved_def.expr.idx);
                const body = try self.lowerExprInfoWithExpectedType(
                    module_idx,
                    &type_scope,
                    binding_env,
                    solved_def.expr.idx,
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const solved_def = typed_cir_module.def(def_idx);
        const bind_symbol = self.lookupTopLevelSymbol(module_idx, solved_def.pattern.idx) orelse try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE);

        var type_scope: TypeCloneScope = undefined;
        try type_scope.initCloneAll(self.allocator, typed_cir_module);
        defer type_scope.deinit();
        var binding_env = BindingEnv.init(self.allocator);
        defer binding_env.deinit();
        const bind_var = solved_def.pattern.ty();
        const bind_expected_var = try self.scopeVar(&type_scope, module_idx, bind_var);
        try self.collectExprInfoWithResultVar(
            module_idx,
            &type_scope,
            binding_env,
            solved_def.expr.idx,
            bind_expected_var,
        );
        try self.finalizeExprTypes(module_idx, &type_scope);
        try self.finalizePatternTypes(module_idx, &type_scope);
        const bind_expected_ty = try self.requireExprType(module_idx, &type_scope, solved_def.expr.idx);
        const body = try self.lowerExprInfoWithExpectedType(
            module_idx,
            &type_scope,
            binding_env,
            solved_def.expr.idx,
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
            "monotype invariant violated: missing top-level entry for global symbol {d}",
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

        const def_id = try self.lowerTopLevelDef(top_level.module_idx, top_level.def_idx) orelse debugPanic(
            "monotype invariant violated: failed to emit reachable top-level value symbol {d}",
            .{symbol.raw()},
        );
        if (!self.rootDefsContain(def_id)) {
            try self.program.root_defs.append(self.allocator, def_id);
        }
    }

    fn rootDefsContain(self: *const Lowerer, def_id: ast.DefId) bool {
        for (self.program.root_defs.items) |existing| {
            if (existing == def_id) return true;
        }
        return false;
    }

    fn drainSpecializations(self: *Lowerer) std.mem.Allocator.Error!bool {
        var any_specialized = false;
        while (self.specializations.nextNeededSpecialization()) |pending_idx| {
            const pending = self.specializations.get(pending_idx).*;
            const def_id = try self.lowerSpecializedTopLevelFn(pending);
            try self.program.root_defs.append(self.allocator, def_id);
            self.specializations.get(pending_idx).emitted = true;
            any_specialized = true;
        }
        return any_specialized;
    }

    fn lowerSpecializedTopLevelFn(
        self: *Lowerer,
        pending: specializations_mod.Pending,
    ) std.mem.Allocator.Error!ast.DefId {
        if (self.emitted_defs_by_symbol.get(pending.specialized_symbol)) |existing| return existing;
        const typed_cir_module = self.ctx.typedCirModule(pending.source.module_idx);
        const solved_def = typed_cir_module.def(pending.source.def_idx);
        var type_scope: TypeCloneScope = undefined;
        try type_scope.initCloneAll(self.allocator, typed_cir_module);
        defer type_scope.deinit();
        const specialized_checker_var = try self.materializeSpecializedTopLevelCheckerVar(&type_scope, pending);

        if (solved_def.expr.data == .e_hosted_lambda) {
            return try self.lowerHostedTopLevelDefWithScope(
                pending.source.module_idx,
                pending.source.def_idx,
                pending.specialized_symbol,
                specialized_checker_var,
                &type_scope,
            );
        }

        var binding_env = BindingEnv.init(self.allocator);
        defer binding_env.deinit();
        const recursive = isRecursiveTopLevelDef(typed_cir_module, pending.source.def_idx);

        const letfn = try self.lowerLambdaLikeDefWithEnv(
            pending.source.module_idx,
            &type_scope,
            binding_env,
            pending.specialized_symbol,
            solved_def.expr.idx,
            recursive,
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

    fn lowerHostedTopLevelDef(
        self: *Lowerer,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        bind_symbol: symbol_mod.Symbol,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!ast.DefId {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        var type_scope: TypeCloneScope = undefined;
        try type_scope.initCloneAll(self.allocator, typed_cir_module);
        defer type_scope.deinit();
        return self.lowerHostedTopLevelDefWithScope(module_idx, def_idx, bind_symbol, expected_var, &type_scope);
    }

    fn lowerHostedTopLevelDefWithScope(
        self: *Lowerer,
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        bind_symbol: symbol_mod.Symbol,
        expected_var: ?Var,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!ast.DefId {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const solved_def = typed_cir_module.def(def_idx);
        const hosted = switch (solved_def.expr.data) {
            .e_hosted_lambda => |hosted| hosted,
            else => debugPanic("monotype invariant violated: attempted to lower non-hosted def as hosted top-level", .{}),
        };

        const source_expr_var = solved_def.expr.ty();
        _ = expected_var;
        const source_fn_var = source_expr_var;

        const arg_patterns = self.ctx.typedCirModule(module_idx).slicePatterns(hosted.args);
        const arg_count = self.functionArgCountInStore(typed_cir_module.typeStoreConst(), source_fn_var);
        const arg_tys = try self.allocator.alloc(type_mod.TypeId, arg_count);
        defer self.allocator.free(arg_tys);
        for (0..arg_count) |i| {
            const source_arg_var = self.lookupCurriedFunctionArgVarInStore(typed_cir_module.typeStoreConst(), source_fn_var, i) orelse
                debugPanic("monotype invariant violated: missing hosted arg {d} in module {d}", .{ i, module_idx });
            arg_tys[i] = try self.instantiateSourceVarType(module_idx, type_scope, source_arg_var);
        }
        const lowered_args = try self.allocator.alloc(ast.TypedSymbol, arg_count);
        defer self.allocator.free(lowered_args);
        for (0..arg_count) |i| {
            lowered_args[i] = if (i < arg_patterns.len)
                try self.bindLambdaArg(module_idx, type_scope, arg_patterns[i], arg_tys[i])
            else
                try self.makeUnitArgWithType(arg_tys[i]);
        }

        const source_ret_var = self.lookupFunctionRetVarInStore(typed_cir_module.typeStoreConst(), source_fn_var) orelse
            debugPanic("monotype invariant violated: hosted top-level missing return var in module {d}", .{module_idx});
        const bind_ty = try self.buildCurriedFuncType(arg_tys, try self.instantiateSourceVarType(module_idx, type_scope, source_ret_var));
        const lowered = try self.program.store.addDef(.{
            .bind = .{
                .ty = try self.ctx.types.addType(bind_ty),
                .symbol = bind_symbol,
            },
            .value = .{ .hosted_fn = .{
                .bind = .{
                    .ty = try self.ctx.types.addType(bind_ty),
                    .symbol = bind_symbol,
                },
                .args = try self.program.store.addTypedSymbolSpan(lowered_args),
                .hosted = .{
                    .symbol_name = hosted.symbol_name,
                    .index = hosted.index,
                },
            } },
        });
        try self.emitted_defs_by_symbol.put(bind_symbol, lowered);
        return lowered;
    }

    fn materializeSpecializedTopLevelCheckerVar(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        pending: specializations_mod.Pending,
    ) std.mem.Allocator.Error!Var {
        return try self.materializeFrozenCheckerVar(type_scope, pending.expected_checker_seed);
    }

    fn freezeCheckerVarFromStores(
        self: *Lowerer,
        source_store: *const types.Store,
        source_idents: *const base.Ident.Store,
        source_var: Var,
    ) std.mem.Allocator.Error!FrozenCheckerVar {
        var type_store = try types.Store.init(self.allocator);
        errdefer type_store.deinit();
        var ident_store = try base.Ident.Store.initCapacity(self.allocator, 16);
        errdefer ident_store.deinit(self.allocator);
        var var_map = std.AutoHashMap(Var, Var).init(self.allocator);
        defer var_map.deinit();
        const root_var = try type_clone_source.copyVar(
            source_store,
            &type_store,
            source_var,
            &var_map,
            source_idents,
            &ident_store,
            self.allocator,
        );
        return .{
            .type_store = type_store,
            .ident_store = ident_store,
            .root_var = root_var,
        };
    }

    fn freezeCheckerVarFromScope(
        self: *Lowerer,
        type_scope: *const TypeCloneScope,
        source_var: Var,
    ) std.mem.Allocator.Error!FrozenCheckerVar {
        return self.freezeCheckerVarFromStores(
            type_scope.typeStoreConst(),
            type_scope.identStoreConst(),
            source_var,
        );
    }

    fn materializeFrozenCheckerVar(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        frozen: FrozenCheckerVar,
    ) std.mem.Allocator.Error!Var {
        var var_map = std.AutoHashMap(Var, Var).init(self.allocator);
        defer var_map.deinit();
        const copied_root = try type_clone_source.copyVar(
            &frozen.type_store,
            type_scope.typeStoreMut(),
            frozen.root_var,
            &var_map,
            &frozen.ident_store,
            type_scope.identStoreMut(),
            self.allocator,
        );
        return try type_scope.instantiator.instantiateVar(copied_root);
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
        const solved_expr = self.ctx.typedCirModule(module_idx).expr(expr_idx);
        const expr = solved_expr.data;
        const source_expr_var = solved_expr.ty();
        const source_seed_source_var = source_seed_var orelse source_expr_var;
        _ = expected_var;
        _ = source_expr_var;
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const source_fn_var = source_seed_source_var;
        const source_result_var = self.lookupFunctionRetVarInStore(typed_cir_module.typeStoreConst(), source_fn_var) orelse
            debugPanic("monotype lambda invariant violated: missing function return var in module {d}", .{module_idx});
        const result_ty = try self.instantiateSourceVarType(module_idx, type_scope, source_result_var);
        const result_var = source_result_var;

        const lambda = switch (expr) {
            .e_lambda => |lambda| lambda,
            .e_closure => |closure| blk: {
                const lambda_expr = self.ctx.typedCirModule(module_idx).expr(closure.lambda_idx).data;
                if (lambda_expr != .e_lambda) unreachable;
                break :blk lambda_expr.e_lambda;
            },
            .e_hosted_lambda => debugPanic("monotype invariant violated: hosted top-level proc escaped into lambda lowering", .{}),
            else => unreachable,
        };

        const arg_patterns = typed_cir_module.slicePatterns(lambda.args);
        const first_arg_source_var = self.lookupCurriedFunctionArgVarInStore(typed_cir_module.typeStoreConst(), source_fn_var, 0);
        const first_arg_ty = if (first_arg_source_var) |source_arg_var|
            try self.instantiateSourceVarType(module_idx, type_scope, source_arg_var)
        else
            try self.makeUnitType();
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
                first_arg_source_var,
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
            const body_expect = self.requireLambdaBodyReturnExpectation(module_idx, result_ty, result_var);
            try self.collectExprInfoWithResultVar(
                module_idx,
                type_scope,
                incoming_env,
                lambda.body,
                body_expect.solved_var,
            );
            try self.finalizeExprTypes(module_idx, type_scope);
            try self.finalizePatternTypes(module_idx, type_scope);
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const first_arg_source_var = self.lookupCurriedFunctionArgVarInStore(typed_cir_module.typeStoreConst(), source_fn_var, next_arg_index) orelse
            debugPanic("monotype lambda invariant violated: missing curried arg {d} in module {d}", .{ next_arg_index, module_idx });
        const first_arg_ty = try self.instantiateSourceVarType(module_idx, type_scope, first_arg_source_var);
        const source_result_var = self.lookupCurriedFunctionResultVarInStore(typed_cir_module.typeStoreConst(), source_fn_var) orelse debugPanic(
            "monotype lambda invariant violated: missing curried source function final return in module {d}",
            .{module_idx},
        );
        const source_result_ty = try self.instantiateSourceVarType(module_idx, type_scope, source_result_var);
        const remaining_after_current = remaining_arg_patterns.len - 1;
        const result_ty = if (remaining_after_current == 0)
            source_result_ty
        else
            try self.buildRemainingCurriedClosureTypeFromFunctionType(
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
            first_arg_source_var,
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
        try self.collectPatternInfo(module_idx, type_scope, pattern_idx);
        try self.recordPatternStructuralInfoFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            arg_bind.ty,
            arg_solved_var,
        );
        if (try self.patternIsIrrefutableStructural(module_idx, type_scope, pattern_idx)) {
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
                const body_expect = self.requireLambdaBodyReturnExpectation(module_idx, result_ty, expected_result_var);
                try self.collectExprInfoWithResultVar(
                    module_idx,
                    type_scope,
                    body_env,
                    body_expr_idx,
                    body_expect.solved_var,
                );
                try self.finalizeExprTypes(module_idx, type_scope);
                try self.finalizePatternTypes(module_idx, type_scope);
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
                null,
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
            const body_expect = self.requireLambdaBodyReturnExpectation(module_idx, result_ty, expected_result_var);
            try self.collectExprInfoWithResultVar(
                module_idx,
                type_scope,
                branch_env,
                body_expr_idx,
                body_expect.solved_var,
            );
            try self.finalizeExprTypes(module_idx, type_scope);
            try self.finalizePatternTypes(module_idx, type_scope);
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

    fn buildRemainingCurriedClosureTypeFromFunctionType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        source_fn_var: ?Var,
        next_arg_index: usize,
        remaining_arity: usize,
        final_result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const function_var = source_fn_var orelse debugPanic(
            "monotype lambda invariant violated: missing source function for curried closure type in module {d}",
            .{module_idx},
        );
        const total_args = self.functionArgCountInStore(type_scope.typeStoreConst(), function_var);
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

    fn bindExprResultVar(
        _: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        body_expr_idx: CIR.Expr.Idx,
        result_var_opt: ?Var,
    ) std.mem.Allocator.Error!void {
        const result_var = result_var_opt orelse debugPanic(
            "monotype lambda invariant violated: final lambda body missing function return var in module {d}",
            .{module_idx},
        );
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = body_expr_idx,
        };
        if (type_scope.memo.expr_result_var_map.contains(key)) {
            return;
        }
        try type_scope.memo.expr_result_var_map.put(key, result_var);
    }

    fn collectExprInfoWithResultVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        result_var: ?Var,
    ) std.mem.Allocator.Error!void {
        const bound_result_var = if (result_var) |explicit|
            try self.scopeVar(type_scope, module_idx, explicit)
        else
            null;
        try self.bindExprResultVar(module_idx, type_scope, expr_idx, bound_result_var);
        try self.collectSolvedExprInfo(module_idx, type_scope, env, self.ctx.typedCirModule(module_idx).expr(expr_idx));
    }

    fn alignWorkspaceVarToCanonical(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        canonical_var: Var,
        aligned_var: Var,
    ) std.mem.Allocator.Error!void {
        var visited = std.AutoHashMap(AlignVarPair, void).init(self.allocator);
        defer visited.deinit();
        try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_var, aligned_var, &visited);
    }

    const AlignVarPair = struct {
        canonical: Var,
        aligned: Var,
    };

    fn alignWorkspaceVarToCanonicalVisited(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        canonical_var: Var,
        aligned_var: Var,
        visited: *std.AutoHashMap(AlignVarPair, void),
    ) std.mem.Allocator.Error!void {
        const store = type_scope.typeStoreConst();
        const canonical = store.resolveVar(canonical_var);
        const aligned = store.resolveVar(aligned_var);
        if (canonical.var_ == aligned.var_) return;
        const pair: AlignVarPair = .{
            .canonical = canonical.var_,
            .aligned = aligned.var_,
        };
        if (visited.contains(pair)) return;
        try visited.put(pair, {});

        switch (aligned.desc.content) {
            .structure => |aligned_flat| {
                if (aligned_flat == .nominal_type) {
                    const aligned_nominal = aligned_flat.nominal_type;
                    const canonical_is_nominal = switch (canonical.desc.content) {
                        .structure => |canonical_flat| canonical_flat == .nominal_type,
                        else => false,
                    };
                    if (!canonical_is_nominal) {
                        try self.alignWorkspaceVarToCanonicalVisited(
                            type_scope,
                            canonical.var_,
                            store.getNominalBackingVar(aligned_nominal),
                            visited,
                        );
                        return;
                    }
                }
            },
            else => {},
        }

        switch (aligned.desc.content) {
            .flex, .rigid, .err => {
                try type_scope.typeStoreMut().dangerousSetVarDesc(aligned.var_, .{
                    .content = canonical.desc.content,
                    .rank = types.Rank.generalized,
                });
                return;
            },
            .alias => |aligned_alias| {
                try self.alignWorkspaceVarToCanonicalVisited(
                    type_scope,
                    canonical.var_,
                    store.getAliasBackingVar(aligned_alias),
                    visited,
                );
                return;
            },
            .structure => {},
        }

        switch (canonical.desc.content) {
            .flex, .rigid, .err => {
                try type_scope.typeStoreMut().dangerousSetVarDesc(canonical.var_, .{
                    .content = aligned.desc.content,
                    .rank = types.Rank.generalized,
                });
                return;
            },
            .alias => |canonical_alias| {
                try self.alignWorkspaceVarToCanonicalVisited(
                    type_scope,
                    store.getAliasBackingVar(canonical_alias),
                    aligned.var_,
                    visited,
                );
                return;
            },
            .structure => |canonical_flat| switch (canonical_flat) {
                .nominal_type => |canonical_nominal| {
                    try self.alignWorkspaceVarToCanonicalVisited(
                        type_scope,
                        store.getNominalBackingVar(canonical_nominal),
                        aligned.var_,
                        visited,
                    );
                    return;
                },
                else => {},
            },
        }

        const canonical_first_arg = self.lookupFunctionArgVarInStore(store, canonical.var_, 0);
        const aligned_first_arg = self.lookupFunctionArgVarInStore(store, aligned.var_, 0);
        if ((canonical_first_arg == null) != (aligned_first_arg == null)) {
            debugPanic(
                "monotype specialization invariant violated: function arity shape mismatch while aligning workspace vars",
                .{},
            );
        }
        if (canonical_first_arg != null) {
            var arg_index: usize = 0;
            while (true) : (arg_index += 1) {
                const canonical_arg = self.lookupFunctionArgVarInStore(store, canonical.var_, arg_index);
                const aligned_arg = self.lookupFunctionArgVarInStore(store, aligned.var_, arg_index);
                if (canonical_arg == null or aligned_arg == null) {
                    if (canonical_arg != aligned_arg) {
                        debugPanic(
                            "monotype specialization invariant violated: function arity mismatch while aligning workspace vars",
                            .{},
                        );
                    }
                    break;
                }
                try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_arg.?, aligned_arg.?, visited);
            }
            const canonical_ret = self.lookupFunctionRetVarInStore(store, canonical.var_) orelse debugPanic(
                "monotype specialization invariant violated: missing canonical function return while aligning workspace vars",
                .{},
            );
            const aligned_ret = self.lookupFunctionRetVarInStore(store, aligned.var_) orelse debugPanic(
                "monotype specialization invariant violated: missing aligned function return while aligning workspace vars",
                .{},
            );
            try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_ret, aligned_ret, visited);
            return;
        }

        switch (canonical.desc.content) {
            .alias => |canonical_alias| switch (aligned.desc.content) {
                .alias => |aligned_alias| {
                    const canonical_args = store.sliceAliasArgs(canonical_alias);
                    const aligned_args = store.sliceAliasArgs(aligned_alias);
                    if (canonical_args.len != aligned_args.len) {
                        debugPanic(
                            "monotype specialization invariant violated: alias arg mismatch while aligning workspace vars",
                            .{},
                        );
                    }
                    for (canonical_args, aligned_args) |canonical_arg, aligned_arg| {
                        try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_arg, aligned_arg, visited);
                    }
                    try self.alignWorkspaceVarToCanonicalVisited(
                        type_scope,
                        store.getAliasBackingVar(canonical_alias),
                        store.getAliasBackingVar(aligned_alias),
                        visited,
                    );
                },
                else => {
                    try self.alignWorkspaceVarToCanonicalVisited(
                        type_scope,
                        store.getAliasBackingVar(canonical_alias),
                        aligned.var_,
                        visited,
                    );
                },
            },
            .structure => |canonical_flat| switch (canonical_flat) {
                .nominal_type => |canonical_nominal| switch (aligned.desc.content) {
                    .structure => |aligned_flat| switch (aligned_flat) {
                        .nominal_type => |aligned_nominal| {
                            if (!canonical_nominal.ident.ident_idx.eql(aligned_nominal.ident.ident_idx) or
                                !canonical_nominal.origin_module.eql(aligned_nominal.origin_module) or
                                canonical_nominal.is_opaque != aligned_nominal.is_opaque)
                            {
                                debugPanic(
                                    "monotype specialization invariant violated: nominal identity mismatch while aligning workspace vars",
                                    .{},
                                );
                            }
                            const canonical_args = store.sliceNominalArgs(canonical_nominal);
                            const aligned_args = store.sliceNominalArgs(aligned_nominal);
                            if (canonical_args.len != aligned_args.len) {
                                debugPanic(
                                    "monotype specialization invariant violated: nominal arg mismatch while aligning workspace vars",
                                    .{},
                                );
                            }
                            for (canonical_args, aligned_args) |canonical_arg, aligned_arg| {
                                try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_arg, aligned_arg, visited);
                            }
                            try self.alignWorkspaceVarToCanonicalVisited(
                                type_scope,
                                store.getNominalBackingVar(canonical_nominal),
                                store.getNominalBackingVar(aligned_nominal),
                                visited,
                            );
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: nominal/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    },
                    else => debugPanic(
                        "monotype specialization invariant violated: nominal/content mismatch while aligning workspace vars",
                        .{},
                    ),
                },
                .tuple => |canonical_tuple| switch (aligned.desc.content) {
                    .structure => |aligned_flat| switch (aligned_flat) {
                        .tuple => |aligned_tuple| {
                            const canonical_elems = store.sliceVars(canonical_tuple.elems);
                            const aligned_elems = store.sliceVars(aligned_tuple.elems);
                            if (canonical_elems.len != aligned_elems.len) {
                                debugPanic(
                                    "monotype specialization invariant violated: tuple arity mismatch while aligning workspace vars",
                                    .{},
                                );
                            }
                            for (canonical_elems, aligned_elems) |canonical_elem, aligned_elem| {
                                try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_elem, aligned_elem, visited);
                            }
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: tuple/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    },
                    else => debugPanic(
                        "monotype specialization invariant violated: tuple/content mismatch while aligning workspace vars",
                        .{},
                    ),
                },
                .record => |canonical_record| switch (aligned.desc.content) {
                    .structure => {
                        var canonical_flattened = std.ArrayList(types.RecordField).empty;
                        defer canonical_flattened.deinit(self.allocator);
                        const canonical_terminal_ext = try self.gatherFlattenedRecordContent(
                            store,
                            canonical.var_,
                            .{ .structure = .{ .record = canonical_record } },
                            &canonical_flattened,
                        );

                        var aligned_flattened = std.ArrayList(types.RecordField).empty;
                        defer aligned_flattened.deinit(self.allocator);
                        const aligned_terminal_ext = try self.gatherFlattenedRecordContent(
                            store,
                            aligned.var_,
                            aligned.desc.content,
                            &aligned_flattened,
                        );

                        var matched_canonical = try self.allocator.alloc(bool, canonical_flattened.items.len);
                        defer self.allocator.free(matched_canonical);
                        @memset(matched_canonical, false);

                        var residual_aligned_fields = std.ArrayList(types.RecordField).empty;
                        defer residual_aligned_fields.deinit(self.allocator);

                        for (aligned_flattened.items) |aligned_field| {
                            var matched = false;
                            for (canonical_flattened.items, 0..) |canonical_field, canonical_idx| {
                                if (!std.mem.eql(
                                    u8,
                                    type_scope.identStoreConst().getText(canonical_field.name),
                                    type_scope.identStoreConst().getText(aligned_field.name),
                                )) continue;
                                try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_field.var_, aligned_field.var_, visited);
                                matched_canonical[canonical_idx] = true;
                                matched = true;
                                break;
                            }
                            if (!matched) {
                                try residual_aligned_fields.append(self.allocator, aligned_field);
                            }
                        }

                        var residual_canonical_fields = std.ArrayList(types.RecordField).empty;
                        defer residual_canonical_fields.deinit(self.allocator);
                        for (canonical_flattened.items, matched_canonical) |canonical_field, was_matched| {
                            if (!was_matched) {
                                try residual_canonical_fields.append(self.allocator, canonical_field);
                            }
                        }

                        if (residual_canonical_fields.items.len == 0 and residual_aligned_fields.items.len == 0) {
                            try self.alignFlattenedRecordTerminal(type_scope, canonical_terminal_ext, aligned_terminal_ext);
                        } else {
                            const canonical_residual_var = if (residual_canonical_fields.items.len == 0)
                                try self.flattenedRecordTerminalAsVar(canonical_terminal_ext, type_scope)
                            else
                                try self.materializeWorkspaceRecordResidualVar(
                                    residual_canonical_fields.items,
                                    canonical_terminal_ext,
                                    type_scope,
                                );
                            const aligned_residual_var = if (residual_aligned_fields.items.len == 0)
                                try self.flattenedRecordTerminalAsVar(aligned_terminal_ext, type_scope)
                            else
                                try self.materializeWorkspaceRecordResidualVar(
                                    residual_aligned_fields.items,
                                    aligned_terminal_ext,
                                    type_scope,
                                );
                            try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_residual_var, aligned_residual_var, visited);
                        }
                    },
                    else => debugPanic(
                        "monotype specialization invariant violated: record/content mismatch while aligning workspace vars",
                        .{},
                    ),
                },
                .record_unbound => |canonical_fields_range| switch (aligned.desc.content) {
                    .structure => {
                        var canonical_flattened = std.ArrayList(types.RecordField).empty;
                        defer canonical_flattened.deinit(self.allocator);
                        const canonical_terminal_ext = try self.gatherFlattenedRecordContent(
                            store,
                            canonical.var_,
                            .{ .structure = .{ .record_unbound = canonical_fields_range } },
                            &canonical_flattened,
                        );

                        var aligned_flattened = std.ArrayList(types.RecordField).empty;
                        defer aligned_flattened.deinit(self.allocator);
                        const aligned_terminal_ext = try self.gatherFlattenedRecordContent(
                            store,
                            aligned.var_,
                            aligned.desc.content,
                            &aligned_flattened,
                        );

                        var matched_canonical = try self.allocator.alloc(bool, canonical_flattened.items.len);
                        defer self.allocator.free(matched_canonical);
                        @memset(matched_canonical, false);

                        var residual_aligned_fields = std.ArrayList(types.RecordField).empty;
                        defer residual_aligned_fields.deinit(self.allocator);

                        for (aligned_flattened.items) |aligned_field| {
                            var matched = false;
                            for (canonical_flattened.items, 0..) |canonical_field, canonical_idx| {
                                if (!std.mem.eql(
                                    u8,
                                    type_scope.identStoreConst().getText(canonical_field.name),
                                    type_scope.identStoreConst().getText(aligned_field.name),
                                )) continue;
                                try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_field.var_, aligned_field.var_, visited);
                                matched_canonical[canonical_idx] = true;
                                matched = true;
                                break;
                            }
                            if (!matched) {
                                try residual_aligned_fields.append(self.allocator, aligned_field);
                            }
                        }

                        var residual_canonical_fields = std.ArrayList(types.RecordField).empty;
                        defer residual_canonical_fields.deinit(self.allocator);
                        for (canonical_flattened.items, matched_canonical) |canonical_field, was_matched| {
                            if (!was_matched) {
                                try residual_canonical_fields.append(self.allocator, canonical_field);
                            }
                        }

                        if (residual_canonical_fields.items.len == 0 and residual_aligned_fields.items.len == 0) {
                            try self.alignFlattenedRecordTerminal(type_scope, canonical_terminal_ext, aligned_terminal_ext);
                        } else {
                            const canonical_residual_var = if (residual_canonical_fields.items.len == 0)
                                try self.flattenedRecordTerminalAsVar(canonical_terminal_ext, type_scope)
                            else
                                try self.materializeWorkspaceRecordResidualVar(
                                    residual_canonical_fields.items,
                                    canonical_terminal_ext,
                                    type_scope,
                                );
                            const aligned_residual_var = if (residual_aligned_fields.items.len == 0)
                                try self.flattenedRecordTerminalAsVar(aligned_terminal_ext, type_scope)
                            else
                                try self.materializeWorkspaceRecordResidualVar(
                                    residual_aligned_fields.items,
                                    aligned_terminal_ext,
                                    type_scope,
                                );
                            try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_residual_var, aligned_residual_var, visited);
                        }
                    },
                    else => debugPanic(
                        "monotype specialization invariant violated: open-record/content mismatch while aligning workspace vars",
                        .{},
                    ),
                },
                .tag_union => |canonical_union| switch (aligned.desc.content) {
                    .structure => |aligned_flat| switch (aligned_flat) {
                        .tag_union => |aligned_union| {
                            var canonical_flattened = std.ArrayList(types.Tag).empty;
                            defer canonical_flattened.deinit(self.allocator);
                            const canonical_terminal_ext = try self.gatherFlattenedTagUnion(
                                store,
                                canonical_union.tags,
                                canonical_union.ext,
                                &canonical_flattened,
                            );

                            var aligned_flattened = std.ArrayList(types.Tag).empty;
                            defer aligned_flattened.deinit(self.allocator);
                            const aligned_terminal_ext = try self.gatherFlattenedTagUnion(
                                store,
                                aligned_union.tags,
                                aligned_union.ext,
                                &aligned_flattened,
                            );

                            var matched_canonical = try self.allocator.alloc(bool, canonical_flattened.items.len);
                            defer self.allocator.free(matched_canonical);
                            @memset(matched_canonical, false);

                            var residual_aligned_tags = std.ArrayList(types.Tag).empty;
                            defer residual_aligned_tags.deinit(self.allocator);

                            for (aligned_flattened.items) |aligned_tag| {
                                var matched = false;
                                for (canonical_flattened.items, 0..) |canonical_tag, canonical_idx| {
                                    if (!std.mem.eql(u8, type_scope.identStoreConst().getText(canonical_tag.name), type_scope.identStoreConst().getText(aligned_tag.name))) continue;
                                    const canonical_args = store.sliceVars(canonical_tag.args);
                                    const aligned_args = store.sliceVars(aligned_tag.args);
                                    if (canonical_args.len != aligned_args.len) {
                                        debugPanic(
                                            "monotype specialization invariant violated: tag payload mismatch while aligning workspace vars",
                                            .{},
                                        );
                                    }
                                    for (canonical_args, aligned_args) |canonical_arg, aligned_arg| {
                                        try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_arg, aligned_arg, visited);
                                    }
                                    matched_canonical[canonical_idx] = true;
                                    matched = true;
                                    break;
                                }
                                if (!matched) {
                                    try residual_aligned_tags.append(self.allocator, aligned_tag);
                                }
                            }

                            var residual_canonical_tags = std.ArrayList(types.Tag).empty;
                            defer residual_canonical_tags.deinit(self.allocator);
                            for (canonical_flattened.items, matched_canonical) |canonical_tag, was_matched| {
                                if (!was_matched) {
                                    try residual_canonical_tags.append(self.allocator, canonical_tag);
                                }
                            }

                            const canonical_residual_var = if (residual_canonical_tags.items.len == 0)
                                canonical_terminal_ext
                            else
                                try self.materializeWorkspaceTagUnionResidualVar(
                                    residual_canonical_tags.items,
                                    canonical_terminal_ext,
                                    type_scope,
                                );

                            const aligned_residual_var = if (residual_aligned_tags.items.len == 0)
                                aligned_terminal_ext
                            else
                                try self.materializeWorkspaceTagUnionResidualVar(
                                    residual_aligned_tags.items,
                                    aligned_terminal_ext,
                                    type_scope,
                                );

                            try self.alignWorkspaceVarToCanonicalVisited(
                                type_scope,
                                canonical_residual_var,
                                aligned_residual_var,
                                visited,
                            );
                        },
                        .empty_tag_union => {
                            var canonical_flattened = std.ArrayList(types.Tag).empty;
                            defer canonical_flattened.deinit(self.allocator);
                            const canonical_terminal_ext = try self.gatherFlattenedTagUnion(
                                store,
                                canonical_union.tags,
                                canonical_union.ext,
                                &canonical_flattened,
                            );
                            if (canonical_flattened.items.len != 0) {
                                debugPanic(
                                    "monotype specialization invariant violated: tag-union/content mismatch while aligning workspace vars",
                                    .{},
                                );
                            }
                            const resolved_terminal = store.resolveVar(canonical_terminal_ext);
                            switch (resolved_terminal.desc.content) {
                                .structure => |flat| if (flat != .empty_tag_union) {
                                    debugPanic(
                                        "monotype specialization invariant violated: tag-union/content mismatch while aligning workspace vars",
                                        .{},
                                    );
                                },
                                else => debugPanic(
                                    "monotype specialization invariant violated: tag-union/content mismatch while aligning workspace vars",
                                    .{},
                                ),
                            }
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: tag-union/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    },
                    else => debugPanic(
                        "monotype specialization invariant violated: tag-union/content mismatch while aligning workspace vars",
                        .{},
                    ),
                },
                .empty_record => switch (aligned.desc.content) {
                    .structure => {
                        var aligned_flattened = std.ArrayList(types.RecordField).empty;
                        defer aligned_flattened.deinit(self.allocator);
                        const aligned_terminal_ext = try self.gatherFlattenedRecordContent(
                            store,
                            aligned.var_,
                            aligned.desc.content,
                            &aligned_flattened,
                        );
                        if (aligned_flattened.items.len != 0) {
                            debugPanic(
                                "monotype specialization invariant violated: empty-record/content mismatch while aligning workspace vars",
                                .{},
                            );
                        }
                        try self.alignFlattenedRecordTerminal(type_scope, .empty_record, aligned_terminal_ext);
                    },
                    else => debugPanic(
                        "monotype specialization invariant violated: empty-record/content mismatch while aligning workspace vars",
                        .{},
                    ),
                },
                .empty_tag_union => switch (aligned.desc.content) {
                    .structure => |aligned_flat| switch (aligned_flat) {
                        .empty_tag_union => {},
                        .tag_union => |aligned_union| {
                            var aligned_flattened = std.ArrayList(types.Tag).empty;
                            defer aligned_flattened.deinit(self.allocator);
                            const aligned_terminal_ext = try self.gatherFlattenedTagUnion(
                                store,
                                aligned_union.tags,
                                aligned_union.ext,
                                &aligned_flattened,
                            );
                            if (aligned_flattened.items.len != 0) {
                                debugPanic(
                                    "monotype specialization invariant violated: empty-tag-union/content mismatch while aligning workspace vars",
                                    .{},
                                );
                            }
                            const resolved_terminal = store.resolveVar(aligned_terminal_ext);
                            switch (resolved_terminal.desc.content) {
                                .structure => |flat| if (flat != .empty_tag_union) {
                                    debugPanic(
                                        "monotype specialization invariant violated: empty-tag-union/content mismatch while aligning workspace vars",
                                        .{},
                                    );
                                },
                                else => debugPanic(
                                    "monotype specialization invariant violated: empty-tag-union/content mismatch while aligning workspace vars",
                                    .{},
                                ),
                            }
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: empty-tag-union/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    },
                    else => debugPanic(
                        "monotype specialization invariant violated: empty-tag-union/content mismatch while aligning workspace vars",
                        .{},
                    ),
                },
                .fn_pure, .fn_effectful, .fn_unbound => |canonical_fn| switch (aligned.desc.content) {
                    .structure => |aligned_flat| switch (aligned_flat) {
                        .fn_pure, .fn_effectful, .fn_unbound => |aligned_fn| {
                            const canonical_args = store.sliceVars(canonical_fn.args);
                            const aligned_args = store.sliceVars(aligned_fn.args);
                            if (canonical_args.len != 0 or aligned_args.len != 0) {
                                debugPanic(
                                    "monotype specialization invariant violated: nullary function arg mismatch while aligning workspace vars",
                                    .{},
                                );
                            }
                            try self.alignWorkspaceVarToCanonicalVisited(type_scope, canonical_fn.ret, aligned_fn.ret, visited);
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: function/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    },
                    else => debugPanic(
                        "monotype specialization invariant violated: function/content mismatch while aligning workspace vars",
                        .{},
                    ),
                },
            },
            .flex, .rigid, .err => unreachable,
        }
    }

    fn requirePatternSourceType(
        _: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) type_mod.TypeId {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        return type_scope.memo.pattern_source_type_cache.get(key) orelse
            debugPanic("monotype pattern invariant violated: missing pattern source type cache entry", .{});
    }

    fn recordPatternSourceType(
        _: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.memo.pattern_source_type_cache.get(key)) |existing| {
            if (existing != source_ty) {
                debugPanic(
                    "monotype pattern invariant violated: conflicting pattern source types in module {d}",
                    .{module_idx},
                );
            }
            return;
        }
        try type_scope.memo.pattern_source_type_cache.put(key, source_ty);
    }

    fn requireExprFieldIndex(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!u16 {
        const expr = self.ctx.typedCirModule(module_idx).expr(expr_idx).data;
        const dot = switch (expr) {
            .e_dot_access => |dot| dot,
            else => debugPanic("monotype invariant violated: expected dot access for field index lookup", .{}),
        };
        const receiver_ty = try self.requireExprType(module_idx, type_scope, dot.receiver);
        return self.requireRecordFieldIndexFromMonotypeType(
            receiver_ty,
            self.ctx.typedCirModule(module_idx).getIdent(dot.field_name),
        );
    }

    fn requireExprTagDiscriminant(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!u16 {
        const expr_ty = self.requireExprType(module_idx, type_scope, expr_idx) catch |err| {
            std.debug.panic("monotype invariant violated: missing expr type for tag discriminant ({any})", .{err});
        };
        const expr = self.ctx.typedCirModule(module_idx).expr(expr_idx).data;
        const tag_name = switch (expr) {
            .e_tag => |tag| tag.name,
            .e_zero_argument_tag => |tag| tag.name,
            else => debugPanic("monotype invariant violated: expected tag for discriminant lookup", .{}),
        };
        if (self.ctx.types.getType(expr_ty) == .primitive and self.ctx.types.getType(expr_ty).primitive == .bool) {
            return debugPanic("monotype invariant violated: attempted tag discriminant lookup for Bool", .{});
        }
        return try self.requireTagDiscriminantFromMonotype(module_idx, expr_ty, tag_name);
    }

    fn recordPatternTagDiscriminant(
        _: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        discriminant: u16,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.memo.pattern_tag_discriminant_cache.get(key)) |existing| {
            if (existing != discriminant) {
                debugPanic("monotype pattern invariant violated: conflicting tag discriminants", .{});
            }
            return;
        }
        try type_scope.memo.pattern_tag_discriminant_cache.put(key, discriminant);
    }

    fn requirePatternTagDiscriminant(
        _: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) u16 {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        return type_scope.memo.pattern_tag_discriminant_cache.get(key) orelse
            debugPanic("monotype pattern invariant violated: missing tag discriminant cache entry", .{});
    }

    fn recordPatternListElemType(
        _: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        elem_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.memo.pattern_list_elem_type_cache.get(key)) |existing| {
            if (existing != elem_ty) {
                debugPanic("monotype pattern invariant violated: conflicting list element types", .{});
            }
            return;
        }
        try type_scope.memo.pattern_list_elem_type_cache.put(key, elem_ty);
    }

    fn requirePatternListElemType(
        _: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) type_mod.TypeId {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        return type_scope.memo.pattern_list_elem_type_cache.get(key) orelse
            debugPanic("monotype pattern invariant violated: missing list element type cache entry", .{});
    }

    fn recordRecordDestructFieldIndex(
        _: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        destruct_idx: CIR.Pattern.RecordDestruct.Idx,
        field_index: u16,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.RecordDestructKey = .{
            .module_idx = module_idx,
            .destruct_idx = destruct_idx,
        };
        if (type_scope.memo.record_destruct_field_index_cache.get(key)) |existing| {
            if (existing != field_index) {
                debugPanic("monotype pattern invariant violated: conflicting record destruct field indices", .{});
            }
            return;
        }
        try type_scope.memo.record_destruct_field_index_cache.put(key, field_index);
    }

    fn requireRecordDestructFieldIndex(
        _: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        destruct_idx: CIR.Pattern.RecordDestruct.Idx,
    ) u16 {
        const key: TypeCloneScope.RecordDestructKey = .{
            .module_idx = module_idx,
            .destruct_idx = destruct_idx,
        };
        return type_scope.memo.record_destruct_field_index_cache.get(key) orelse
            debugPanic("monotype pattern invariant violated: missing record destruct field index cache entry", .{});
    }

    fn prepareCallInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        call_expr_idx: CIR.Expr.Idx,
        fn_var: Var,
        arg_exprs: []const CIR.Expr.Idx,
    ) std.mem.Allocator.Error!CallInfo {
        const call_scope = try self.allocator.create(TypeCloneScope);
        errdefer self.allocator.destroy(call_scope);
        try call_scope.initCloneAllFromParent(self.allocator, self.ctx.typedCirModule(module_idx), type_scope, false);
        errdefer call_scope.deinit();

        const cloned_fn_var = try call_scope.instantiator.instantiateVar(fn_var);
        const arg_vars = try self.allocator.alloc(Var, arg_exprs.len);
        errdefer self.allocator.free(arg_vars);
        const arg_tys = try self.allocator.alloc(type_mod.TypeId, arg_exprs.len);
        errdefer self.allocator.free(arg_tys);

        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const source_arg_var = self.ctx.typedCirModule(module_idx).exprType(arg_expr_idx);
            arg_vars[i] = try self.scopeVar(call_scope, module_idx, source_arg_var);
            arg_tys[i] = try self.instantiateSourceVarType(module_idx, call_scope, source_arg_var);
        }

        const fn_ty = try self.instantiateVarType(module_idx, call_scope, cloned_fn_var);
        try self.finalizeExprTypes(module_idx, call_scope);
        try self.finalizePatternTypes(module_idx, call_scope);

        const applied_arg_count = blk: {
            if (arg_exprs.len != 0) break :blk arg_exprs.len;
            const store = call_scope.typeStoreConst();
            if (self.lookupFunctionArgVarInStore(store, cloned_fn_var, 0) == null) {
                if (self.lookupFunctionRetVarInStore(store, cloned_fn_var) != null) break :blk 1;
                debugPanic(
                    "monotype invariant violated: attempted to lower a call with no args to a non-function in module {d}",
                    .{module_idx},
                );
            }
            debugPanic(
                "monotype invariant violated: attempted to lower a call with no args to a non-nullary function in module {d}",
                .{module_idx},
            );
        };
        const applied_result_tys = try self.collectCurriedAppliedResultTypes(fn_ty, applied_arg_count);
        errdefer self.allocator.free(applied_result_tys);
        const result_ty = try self.instantiateSourceVarType(
            module_idx,
            call_scope,
            self.ctx.typedCirModule(module_idx).exprType(call_expr_idx),
        );

        return .{
            .call_scope = call_scope,
            .fn_var = cloned_fn_var,
            .fn_ty = fn_ty,
            .arg_vars = arg_vars,
            .arg_tys = arg_tys,
            .applied_result_tys = applied_result_tys,
            .result_ty = result_ty,
        };
    }

    fn requireLambdaBodyReturnExpectation(
        _: *const Lowerer,
        module_idx: u32,
        result_ty: type_mod.TypeId,
        result_var_opt: ?Var,
    ) ExpectedType {
        return .{
            .ty = result_ty,
            .solved_var = result_var_opt orelse debugPanic(
                "monotype lambda invariant violated: final lambda body missing explicit function return expectation in module {d}",
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
    ) std.mem.Allocator.Error!ExpectedType {
        _ = env;
        const lambda_var = self.ctx.typedCirModule(module_idx).exprType(lambda_expr_idx);
        const result_var = self.lookupFunctionRetVarInStore(self.ctx.typedCirModule(module_idx).typeStoreConst(), lambda_var) orelse debugPanic(
            "monotype return invariant violated: lambda {} in module {d} is missing an explicit return expectation",
            .{ lambda_expr_idx, module_idx },
        );
        return .{
            .ty = try self.instantiateSourceVarType(module_idx, type_scope, result_var),
            .solved_var = result_var,
        };
    }

    fn lowerExprInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!LoweredExprInfo {
        const lowered = try self.lowerExpr(module_idx, type_scope, env, expr_idx);
        return .{
            .expr = lowered,
            .ty = self.program.store.getExpr(lowered).ty,
            .solved_var = self.requireExprResultVar(module_idx, type_scope, expr_idx),
        };
    }

    fn lowerExprInfoWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!LoweredExprInfo {
        if (expected_var) |result_var| {
            const actual = self.requireExprResultVar(module_idx, type_scope, expr_idx);
            if (actual != result_var) {
                debugPanic(
                    "monotype expr invariant violated: lowering expected expr {d} in module {d} to reuse explicit result var {d}, but collected metadata stored {d}",
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
            .solved_var = expected_var orelse self.requireExprResultVar(module_idx, type_scope, expr_idx),
        };
    }

    fn exprResultVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        _: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!Var {
        return self.requireExprResultVar(module_idx, type_scope, expr_idx);
    }

    fn lowerExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!ast.ExprId {
        return self.lowerSolvedExpr(module_idx, type_scope, env, self.ctx.typedCirModule(module_idx).expr(expr_idx));
    }

    fn lowerSolvedExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr: typed_cir.Expr,
    ) std.mem.Allocator.Error!ast.ExprId {
        const typed_cir_module = expr.module();
        switch (expr.data) {
            .e_nominal => |nominal| return self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr.idx,
                nominal.backing_expr,
                try self.lowerExprType(module_idx, type_scope, env, expr.idx, expr.data),
                try self.exprResultVar(module_idx, type_scope, env, expr.idx),
            ),
            .e_nominal_external => |nominal| return self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr.idx,
                nominal.backing_expr,
                try self.lowerExprType(module_idx, type_scope, env, expr.idx, expr.data),
                try self.exprResultVar(module_idx, type_scope, env, expr.idx),
            ),
            else => {},
        }

        if (expr.data == .e_call) {
            if (self.callRootCalleeIsRuntimeError(module_idx, expr.data.e_call.func)) {
                return self.lowerErroneousExprWithType(
                    module_idx,
                    type_scope,
                    env,
                    expr.idx,
                    try self.lowerExprType(module_idx, type_scope, env, expr.idx, expr.data),
                );
            }

            if (try self.maybeLowerSpecialCallExpr(module_idx, type_scope, env, expr.idx, expr.data.e_call, null)) |special| {
                return special;
            }

            const lowered_call = try self.lowerCurriedCall(
                module_idx,
                type_scope,
                env,
                expr.idx,
                expr.data.e_call.func,
                typed_cir_module.sliceExpr(expr.data.e_call.args),
            );
            return try self.program.store.addExpr(.{
                .ty = lowered_call.result_ty,
                .data = .{ .call = lowered_call.data },
            });
        }

        const ty = try self.lowerExprType(module_idx, type_scope, env, expr.idx, expr.data);

        if (expr.data == .e_str) {
            return self.lowerStringExpr(module_idx, type_scope, env, ty, expr.data.e_str);
        }

        const data: ast.Expr.Data = switch (expr.data) {
            .e_num => |num| blk: {
                break :blk try self.lowerNumericIntLiteralData(ty, num.value);
            },
            .e_frac_f32 => |frac| .{ .frac_f32_lit = frac.value },
            .e_frac_f64 => |frac| .{ .frac_f64_lit = frac.value },
            .e_dec => |dec| try self.lowerNumericDecLiteralData(ty, dec.value.toI128()),
            .e_dec_small => |dec| try self.lowerNumericSmallDecLiteralData(ty, dec.value),
            .e_typed_int => |num| blk: {
                break :blk try self.lowerNumericIntLiteralData(ty, num.value);
            },
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
                const symbol = try self.lookupOrSpecializeExternal(module_idx, type_scope, lookup, ty, null);
                return try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_lookup_required => |lookup| {
                const app_module_idx = self.required_app_module_idx orelse debugPanic(
                    "monotype invariant violated: required lookup in module {d} without app module index",
                    .{module_idx},
                );
                const requires_items = self.ctx.typedCirModule(module_idx).requiresTypes();
                const req_idx = lookup.requires_idx.toU32();
                if (req_idx >= requires_items.len) {
                    debugPanic(
                        "monotype invariant violated: required lookup index {d} out of bounds for module {d}",
                        .{ req_idx, module_idx },
                    );
                }
                const req = requires_items[req_idx];
                const required_name = self.ctx.typedCirModule(module_idx).getIdent(req.ident);
                const app_module = self.ctx.typedCirModule(app_module_idx);
                const app_ident = app_module.findCommonIdent(required_name) orelse debugPanic(
                    "monotype invariant violated: required lookup '{s}' missing from app module {d}",
                    .{ required_name, app_module_idx },
                );
                const app_def_idx = self.findTopLevelDefByIdent(app_module_idx, app_ident) orelse debugPanic(
                    "monotype invariant violated: required lookup '{s}' missing def in app module {d}",
                    .{ required_name, app_module_idx },
                );
                const expected_var = try self.scopeVar(
                    type_scope,
                    module_idx,
                    self.ctx.typedCirModule(module_idx).exprType(expr.idx),
                );
                return self.lowerResolvedTargetCallee(
                    module_idx,
                    type_scope,
                    .{ .module_idx = app_module_idx, .def_idx = app_def_idx },
                    ty,
                    expected_var,
                );
            },
            .e_call => unreachable,
            .e_lambda => |lambda| {
                const lowered = try self.lowerAnonymousClosure(module_idx, type_scope, env, expr.idx, ty, lambda.args, lambda.body, null);
                return try self.program.store.addExpr(.{
                    .ty = lowered.ty,
                    .data = .{ .clos = lowered.data },
                });
            },
            .e_closure => |closure| {
                const lowered = try self.lowerClosureExpr(module_idx, type_scope, env, expr.idx, ty, closure, null);
                return try self.program.store.addExpr(.{
                    .ty = lowered.ty,
                    .data = .{ .clos = lowered.data },
                });
            },
            .e_hosted_lambda => debugPanic("monotype invariant violated: hosted top-level proc escaped into monotype.lowerExpr", .{}),
            .e_record => |record| return self.lowerRecordExpr(
                module_idx,
                type_scope,
                env,
                ty,
                try self.scopedSolvedExprResultVar(type_scope, env, expr),
                record,
            ),
            .e_empty_record => .unit,
            .e_tuple => |tuple| .{ .tuple = try self.lowerTupleExprsWithExpectedType(
                module_idx,
                type_scope,
                env,
                ty,
                try self.scopedSolvedExprResultVar(type_scope, env, expr),
                tuple.elems,
            ) },
            .e_list => |list| .{ .list = try self.lowerListExprsWithExpectedType(
                module_idx,
                type_scope,
                env,
                ty,
                try self.scopedSolvedExprResultVar(type_scope, env, expr),
                list.elems,
            ) },
            .e_empty_list => .{ .list = ast.Span(ast.ExprId).empty() },
            .e_tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    access.tuple,
                    try self.requireExprType(module_idx, type_scope, access.tuple),
                    try self.exprResultVar(module_idx, type_scope, env, access.tuple),
                ),
                .elem_index = access.elem_index,
            } },
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
                if (binop.op == .eq or binop.op == .ne) {
                    if (try self.maybeLowerNominalEqBinop(module_idx, type_scope, env, expr.idx, binop)) |lowered| {
                        return lowered;
                    }
                }
                if (binop.op == .ne) {
                    const eq_args = try self.lowerHomogeneousBinopArgs(
                        module_idx,
                        type_scope,
                        env,
                        try self.requireExprType(module_idx, type_scope, binop.lhs),
                        null,
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
                const operand_ty = switch (binop.op) {
                    .add, .sub, .mul, .div, .rem, .div_trunc, .lt, .gt, .le, .ge, .eq => try self.requireExprType(module_idx, type_scope, binop.lhs),
                    else => unreachable,
                };
                break :blk .{ .low_level = .{
                    .op = binopToLowLevel(binop.op),
                    .args = try self.lowerHomogeneousBinopArgs(
                        module_idx,
                        type_scope,
                        env,
                        operand_ty,
                        null,
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
                if (dot.args) |_| {
                    const lowered_call = try self.lowerRecordedMethodCall(
                        module_idx,
                        type_scope,
                        env,
                        expr.idx,
                        dot.field_name,
                    ) orelse {
                        return self.makeRuntimeErrorExprAt(
                            try self.lowerExprType(module_idx, type_scope, env, expr.idx, expr.data),
                        );
                    };
                    return try self.program.store.addExpr(.{
                        .ty = lowered_call.result_ty,
                        .data = .{ .call = lowered_call.data },
                    });
                }
                break :blk .{ .access = .{
                    .record = try self.lowerExpr(module_idx, type_scope, env, dot.receiver),
                    .field = try self.ctx.copyExecutableIdent(module_idx, dot.field_name),
                    .field_index = try self.requireExprFieldIndex(module_idx, type_scope, expr.idx),
                } };
            },
            .e_tag => |tag| try self.lowerTagExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                expr.idx,
                ty,
                try self.scopedSolvedExprResultVar(type_scope, env, expr),
                tag.name,
                tag.args,
            ),
            .e_zero_argument_tag => |tag| try self.lowerTagExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                expr.idx,
                ty,
                try self.scopedSolvedExprResultVar(type_scope, env, expr),
                tag.name,
                .{ .span = .{ .start = 0, .len = 0 } },
            ),
            .e_runtime_error => blk: {
                break :blk .{ .runtime_error = try self.internStringLiteral("runtime error") };
            },
            .e_crash => |crash| blk: {
                break :blk .{ .runtime_error = try self.copySourceStringLiteral(module_idx, crash.msg) };
            },
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
                    expr.idx,
                    dispatch.method_name,
                ) orelse {
                    return self.makeRuntimeErrorExprAt(
                        try self.lowerExprType(module_idx, type_scope, env, expr.idx, expr.data),
                    );
                };
                return try self.program.store.addExpr(.{
                    .ty = lowered_call.result_ty,
                    .data = .{ .call = lowered_call.data },
                });
            },
            else => debugTodoExpr(expr.data),
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
        const parts = self.ctx.typedCirModule(module_idx).sliceExpr(str_expr.span);
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const arg_exprs = typed_cir_module.sliceExpr(call.args);
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
        const typed_cir_module = self.ctx.typedCirModule(current_module_idx);
        return switch (typed_cir_module.expr(func_expr_idx).data) {
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
                const target_module_idx = typed_cir_module.resolvedImportModule(lookup.module_idx) orelse debugPanic(
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

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const inspect_ident = typed_cir_module.findCommonIdent("Builtin.Str.inspect") orelse return false;
        const def = typed_cir_module.def(def_idx).data;
        return switch (typed_cir_module.pattern(def.pattern).data) {
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
            else => debugPanic(
                "monotype numeric literal invariant violated: integer literal lowered to unsupported target type {d} ({s} preserving nominal, {s} normalized)",
                .{
                    @intFromEnum(target_ty),
                    @tagName(self.ctx.types.getTypePreservingNominal(target_ty)),
                    @tagName(self.ctx.types.getType(target_ty)),
                },
            ),
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
            else => debugPanic(
                "monotype numeric literal invariant violated: decimal literal lowered to unsupported target type {d} ({s} preserving nominal, {s} normalized)",
                .{
                    @intFromEnum(target_ty),
                    @tagName(self.ctx.types.getTypePreservingNominal(target_ty)),
                    @tagName(self.ctx.types.getType(target_ty)),
                },
            ),
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
            else => debugPanic(
                "monotype numeric literal invariant violated: small decimal literal lowered to unsupported target type {d} ({s} preserving nominal, {s} normalized)",
                .{
                    @intFromEnum(target_ty),
                    @tagName(self.ctx.types.getTypePreservingNominal(target_ty)),
                    @tagName(self.ctx.types.getType(target_ty)),
                },
            ),
        };
    }

    fn decFromWholeInt(_: *Lowerer, value: i128) std.mem.Allocator.Error!i128 {
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
        _: *Lowerer,
        numerator: i128,
        denominator_power_of_ten: u8,
    ) std.mem.Allocator.Error!i128 {
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

    fn decToF64(_: *Lowerer, scaled_dec: i128) f64 {
        return @as(f64, @floatFromInt(scaled_dec)) / 1_000_000_000_000_000_000.0;
    }

    fn makePrimitiveType(self: *Lowerer, prim: type_mod.Prim) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.ctx.types.internResolved(.{ .primitive = prim });
    }

    fn makeUnitType(self: *Lowerer) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.ctx.types.internResolved(.{ .record = .{ .fields = &.{} } });
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
        _: u32,
        str_ty: type_mod.TypeId,
        bytes: []const u8,
    ) std.mem.Allocator.Error!ast.ExprId {
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

    fn makeRuntimeErrorExpr(
        self: *Lowerer,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.program.store.addExpr(.{
            .ty = ty,
            .data = .{ .runtime_error = try self.internStringLiteral("runtime error") },
        });
    }

    fn makeRuntimeErrorExprAt(
        self: *Lowerer,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        return self.makeRuntimeErrorExpr(ty);
    }

    fn copySourceStringLiteral(
        self: *Lowerer,
        module_idx: u32,
        idx: base.StringLiteral.Idx,
    ) std.mem.Allocator.Error!base.StringLiteral.Idx {
        return self.internStringLiteral(self.ctx.typedCirModule(module_idx).getString(idx));
    }

    fn requireBoolLiteralValue(
        self: *Lowerer,
        module_idx: u32,
        tag_name: base.Ident.Idx,
    ) bool {
        const idents = self.ctx.typedCirModule(module_idx).commonIdents();
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
        if (builtin.mode == .Debug) {
            if (fields.len != 2 or fields[0].name != len_ident or fields[1].name != start_ident) {
                debugPanic("monotype invariant violated: list slice bounds field order mismatch", .{});
            }
        }

        return try self.ctx.types.internResolved(.{ .record = .{
            .fields = try self.ctx.types.dupeFields(&fields),
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
        const record = self.ctx.types.getType(bounds_ty);
        if (record != .record) {
            debugPanic("monotype invariant violated: list slice bounds expected record type", .{});
        }

        const record_fields = record.record.fields;
        if (builtin.mode == .Debug) {
            if (record_fields.len != 2 or record_fields[0].name != len_ident or record_fields[1].name != start_ident) {
                debugPanic("monotype invariant violated: list slice bounds record fields not len/start", .{});
            }
        }
        const ordered_fields = try self.allocator.alloc(ast.FieldExpr, record_fields.len);
        defer self.allocator.free(ordered_fields);
        for (record_fields, 0..) |field, i| {
            const value = if (field.name == len_ident)
                len_expr
            else if (field.name == start_ident)
                start_expr
            else
                debugPanic("monotype invariant violated: unexpected list slice bounds field", .{});
            ordered_fields[i] = .{ .name = field.name, .value = value };
        }
        return try self.program.store.addExpr(.{
            .ty = bounds_ty,
            .data = .{ .record = try self.program.store.addFieldExprSpan(ordered_fields) },
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        var closure_scope: TypeCloneScope = undefined;
        try closure_scope.initCloneAllFromParent(self.allocator, typed_cir_module, type_scope, true);
        defer closure_scope.deinit();
        const scope = &closure_scope;
        const arg_patterns = typed_cir_module.slicePatterns(args_span);
        const source_expr_var = self.ctx.typedCirModule(module_idx).expr(source_expr_idx).ty();
        _ = expected_var;
        const source_fn_var = source_expr_var;
        const first_arg_ty = if (arg_patterns.len == 0)
            self.requireFunctionType(closure_ty).arg
        else
            try self.instantiateSourceVarType(
                module_idx,
                scope,
                self.lookupCurriedFunctionArgVarInStore(typed_cir_module.typeStoreConst(), source_fn_var, 0) orelse
                    debugPanic("monotype closure invariant violated: missing first arg in module {d}", .{module_idx}),
            );
        const result_var = self.lookupFunctionRetVarInStore(typed_cir_module.typeStoreConst(), source_fn_var) orelse
            debugPanic("monotype closure invariant violated: missing return var in module {d}", .{module_idx});
        const result_ty = try self.instantiateSourceVarType(module_idx, scope, result_var);
        const first_arg_pattern = if (arg_patterns.len == 0) null else arg_patterns[0];
        const arg = if (first_arg_pattern) |pattern_idx|
            try self.bindLambdaArg(module_idx, scope, pattern_idx, first_arg_ty)
        else
            try self.makeUnitArgWithType(first_arg_ty);

        var body_env = try self.cloneEnv(env);
        defer body_env.deinit();
        var binding_decls = std.ArrayList(BindingDecl).empty;
        defer binding_decls.deinit(self.allocator);
        if (first_arg_pattern) |pattern_idx| {
            try self.collectStructuralBindingDeclsWithSolvedVar(
                module_idx,
                scope,
                arg,
                self.lookupCurriedFunctionArgVarInStore(typed_cir_module.typeStoreConst(), source_fn_var, 0),
                pattern_idx,
                &body_env,
                &binding_decls,
            );
        }

        const body = if (arg_patterns.len <= 1) blk: {
            const body_expect = self.requireLambdaBodyReturnExpectation(module_idx, result_ty, result_var);
            try self.collectExprInfoWithResultVar(
                module_idx,
                scope,
                body_env,
                body_expr_idx,
                body_expect.solved_var,
            );
            try self.finalizeExprTypes(module_idx, scope);
            try self.finalizePatternTypes(module_idx, scope);
            break :blk try self.wrapExprWithBindingDecls(
                try self.lowerExprWithExpectedType(
                    module_idx,
                    scope,
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
                scope,
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
        const lambda_expr = self.ctx.typedCirModule(module_idx).expr(closure.lambda_idx).data;
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const branch_ids = typed_cir_module.sliceIfBranches(if_expr.branches);
        if (branch_ids.len == 0) debugPanic("monotype invariant violated: if expression missing branches", .{});

        var else_body = try self.lowerExpr(module_idx, type_scope, env, if_expr.final_else);
        var idx = branch_ids.len;
        while (idx > 1) {
            idx -= 1;
            const branch = typed_cir_module.getIfBranch(branch_ids[idx]);
            else_body = try self.program.store.addExpr(.{
                .ty = self.program.store.getExpr(else_body).ty,
                .data = .{ .if_ = .{
                    .cond = try self.lowerExpr(module_idx, type_scope, env, branch.cond),
                    .then_body = try self.lowerExpr(module_idx, type_scope, env, branch.body),
                    .else_body = else_body,
                } },
            });
        }

        const branch = typed_cir_module.getIfBranch(branch_ids[0]);

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
        if (try self.matchExprCanLowerDirect(module_idx, type_scope, match_expr)) {
            return .{ .block = try self.lowerDirectMatchExpr(module_idx, type_scope, env, result_ty, expected_result_var, match_expr) };
        }
        if (try self.matchExprNeedsPredicateDesugaring(module_idx, type_scope, match_expr)) {
            return .{ .block = try self.lowerPredicateMatchExpr(module_idx, type_scope, env, result_ty, expected_result_var, match_expr) };
        }
        return .{ .when = try self.lowerMatchExpr(module_idx, type_scope, env, result_ty, expected_result_var, match_expr) };
    }

    fn matchExprCanLowerDirect(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!bool {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const branches = typed_cir_module.matchBranchSlice(match_expr.branches);
        if (branches.len != 1) return false;

        const branch = typed_cir_module.getMatchBranch(branches[0]);
        if (branch.guard != null) return false;

        const branch_pattern_ids = typed_cir_module.sliceMatchBranchPatterns(branch.patterns);
        if (branch_pattern_ids.len != 1) return false;

        const branch_pattern = typed_cir_module.getMatchBranchPattern(branch_pattern_ids[0]);
        if (branch_pattern.degenerate) return false;

        return try self.patternIsIrrefutableStructural(module_idx, type_scope, branch_pattern.pattern);
    }

    fn patternIsIrrefutableStructural(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!bool {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
        return switch (pattern) {
            .assign, .underscore => true,
            .as => |as_pat| try self.patternIsIrrefutableStructural(module_idx, type_scope, as_pat.pattern),
            .record_destructure => |record| blk: {
                for (typed_cir_module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    if (!try self.patternIsIrrefutableStructural(
                        module_idx,
                        type_scope,
                        typed_cir_module.getRecordDestruct(destruct_idx).kind.toPatternIdx(),
                    )) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (typed_cir_module.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                    if (!try self.patternIsIrrefutableStructural(module_idx, type_scope, elem_pattern_idx)) break :blk false;
                }
                break :blk true;
            },
            .applied_tag => |tag| blk: {
                if (!try self.patternIsSingleTagUnion(module_idx, type_scope, pattern_idx)) break :blk false;
                for (typed_cir_module.slicePatterns(tag.args)) |arg_pattern_idx| {
                    if (!try self.patternIsIrrefutableStructural(module_idx, type_scope, arg_pattern_idx)) break :blk false;
                }
                break :blk true;
            },
            .nominal => |nominal| try self.patternIsIrrefutableStructural(module_idx, type_scope, nominal.backing_pattern),
            .nominal_external => |nominal| try self.patternIsIrrefutableStructural(module_idx, type_scope, nominal.backing_pattern),
            else => false,
        };
    }

    fn patternIsSingleTagUnion(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!bool {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.memo.pattern_source_type_cache.get(key)) |cached| {
            switch (self.ctx.types.getType(cached)) {
                .tag_union => |tag_union| return tag_union.tags.len == 1,
                else => {},
            }
        }

        const pattern_var = self.ctx.typedCirModule(module_idx).patternType(pattern_idx);
        const scoped_var = try self.scopeVar(type_scope, module_idx, pattern_var);
        return try self.patternVarIsSingleTagUnion(module_idx, type_scope, scoped_var);
    }

    fn patternVarIsSingleTagUnion(
        _: *Lowerer,
        _: u32,
        type_scope: *TypeCloneScope,
        var_: Var,
    ) std.mem.Allocator.Error!bool {
        const store = type_scope.typeStoreMut();
        var root = store.resolveVar(var_);
        while (true) {
            switch (root.desc.content) {
                .alias => |alias| root = store.resolveVar(store.getAliasBackingVar(alias)),
                .structure => |flat| switch (flat) {
                    .tag_union => |tag_union| return store.getTagsSlice(tag_union.tags).len == 1,
                    else => return false,
                },
                .flex, .rigid, .err => return false,
            }
        }
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const branch_id = typed_cir_module.matchBranchSlice(match_expr.branches)[0];
        const branch = typed_cir_module.getMatchBranch(branch_id);
        const branch_pattern_id = typed_cir_module.sliceMatchBranchPatterns(branch.patterns)[0];
        const branch_pattern = typed_cir_module.getMatchBranchPattern(branch_pattern_id);

        const scrutinee = try self.lowerExprInfo(module_idx, type_scope, env, match_expr.cond);

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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const branches = typed_cir_module.matchBranchSlice(match_expr.branches);
        const out = try self.allocator.alloc(ast.Branch, branches.len);
        defer self.allocator.free(out);
        const cond = try self.lowerExprInfo(module_idx, type_scope, env, match_expr.cond);

        for (branches, 0..) |branch_idx, i| {
            const branch = typed_cir_module.getMatchBranch(branch_idx);
            if (branch.guard != null) debugTodo("monotype.lowerMatchExpr guard");
            const branch_pattern_ids = typed_cir_module.sliceMatchBranchPatterns(branch.patterns);
            if (branch_pattern_ids.len != 1) debugTodo("monotype.lowerMatchExpr or-pattern");
            const branch_pattern = typed_cir_module.getMatchBranchPattern(branch_pattern_ids[0]);
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
        type_scope: *TypeCloneScope,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!bool {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        for (typed_cir_module.matchBranchSlice(match_expr.branches)) |branch_idx| {
            const branch = typed_cir_module.getMatchBranch(branch_idx);
            if (branch.guard != null) return true;
            const branch_pattern_ids = typed_cir_module.sliceMatchBranchPatterns(branch.patterns);
            if (branch_pattern_ids.len != 1) return true;
            const branch_pattern = typed_cir_module.getMatchBranchPattern(branch_pattern_ids[0]);
            if (branch_pattern.degenerate) return true;
            if (self.patternNeedsPredicateDesugaring(module_idx, branch_pattern.pattern)) return true;
            if (self.patternNeedsBindingDecls(module_idx, branch_pattern.pattern) and
                !try self.patternIsIrrefutableStructural(module_idx, type_scope, branch_pattern.pattern))
            {
                return true;
            }
        }
        return false;
    }

    fn patternNeedsPredicateDesugaring(self: *Lowerer, module_idx: u32, pattern_idx: CIR.Pattern.Idx) bool {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
        return switch (pattern) {
            .list => true,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => true,
            .record_destructure => true,
            .tuple => |tuple| blk: {
                for (typed_cir_module.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                    if (self.patternNeedsPredicateDesugaring(module_idx, elem_pattern_idx)) break :blk true;
                }
                break :blk false;
            },
            .applied_tag => |tag| blk: {
                for (typed_cir_module.slicePatterns(tag.args)) |arg_pattern_idx| {
                    if (self.patternNeedsPredicateDesugaringInTagPayload(module_idx, arg_pattern_idx)) break :blk true;
                }
                break :blk false;
            },
            .as => |as_pat| self.patternNeedsPredicateDesugaring(module_idx, as_pat.pattern),
            .nominal => |nominal| self.patternNeedsPredicateDesugaring(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.patternNeedsPredicateDesugaring(module_idx, nominal.backing_pattern),
            else => false,
        };
    }

    fn patternNeedsPredicateDesugaringInTagPayload(self: *Lowerer, module_idx: u32, pattern_idx: CIR.Pattern.Idx) bool {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
                for (typed_cir_module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    if (self.patternNeedsPredicateDesugaringInTagPayload(
                        module_idx,
                        typed_cir_module.getRecordDestruct(destruct_idx).kind.toPatternIdx(),
                    )) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            .tuple => |tuple| blk: {
                for (typed_cir_module.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                    if (self.patternNeedsPredicateDesugaringInTagPayload(module_idx, elem_pattern_idx)) break :blk true;
                }
                break :blk false;
            },
            .applied_tag => |tag| blk: {
                for (typed_cir_module.slicePatterns(tag.args)) |arg_pattern_idx| {
                    if (self.patternNeedsPredicateDesugaringInTagPayload(module_idx, arg_pattern_idx)) break :blk true;
                }
                break :blk false;
            },
            .as => |as_pat| self.patternNeedsPredicateDesugaringInTagPayload(module_idx, as_pat.pattern),
            .nominal => |nominal| self.patternNeedsPredicateDesugaringInTagPayload(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.patternNeedsPredicateDesugaringInTagPayload(module_idx, nominal.backing_pattern),
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
        const scrutinee = try self.lowerExprInfo(module_idx, type_scope, env, match_expr.cond);
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

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const branches = typed_cir_module.matchBranchSlice(match_expr.branches);
        var idx = branches.len;
        while (idx > 0) {
            idx -= 1;
            const branch = typed_cir_module.getMatchBranch(branches[idx]);
            const branch_pattern_ids = typed_cir_module.sliceMatchBranchPatterns(branch.patterns);
            const bind_pattern_idx = blk: {
                for (branch_pattern_ids) |pattern_id| {
                    const pattern = typed_cir_module.getMatchBranchPattern(pattern_id);
                    if (!pattern.degenerate) break :blk pattern.pattern;
                }
                break :blk typed_cir_module.getMatchBranchPattern(branch_pattern_ids[0]).pattern;
            };
            var pat_idx = branch_pattern_ids.len;
            while (pat_idx > 0) {
                pat_idx -= 1;
                const branch_pattern = typed_cir_module.getMatchBranchPattern(branch_pattern_ids[pat_idx]);
                if (branch_pattern.degenerate) {
                    const runtime_error = try self.program.store.addExpr(.{
                        .ty = result_ty,
                        .data = .{ .runtime_error = try self.internStringLiteral("degenerate match branch") },
                    });
                    current = try self.lowerPatternGuardExpr(
                        module_idx,
                        type_scope,
                        scrutinee_expr,
                        scrutinee.ty,
                        branch_pattern.pattern,
                        runtime_error,
                        current,
                    );
                    continue;
                }

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
                    bind_pattern_idx,
                    branch.guard,
                    branch.value,
                    current,
                );
            }
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
        guard_expr: ?CIR.Expr.Idx,
        branch_value: CIR.Expr.Idx,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const pattern = self.ctx.typedCirModule(module_idx).pattern(match_pattern_idx).data;
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
                match_pattern_idx,
                bind_pattern_idx,
                guard_expr,
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
                match_pattern_idx,
                pattern,
                bind_pattern_idx,
                guard_expr,
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
                guard_expr,
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
                guard_expr,
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
                guard_expr,
                branch_value,
                else_expr,
            ),
            .assign, .underscore, .record_destructure, .tuple, .applied_tag => try self.lowerStructuredPredicateMatchBranch(
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
                guard_expr,
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
        guard_expr: ?CIR.Expr.Idx,
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
            match_pattern_idx,
            scrutinee_expr,
            scrutinee_ty,
            scrutinee_solved_var,
            &branch_env,
            &binding_decls,
        );
        if (match_pattern_idx != bind_pattern_idx) {
            try self.recordPatternStructuralInfoFromSourceType(
                module_idx,
                type_scope,
                bind_pattern_idx,
                scrutinee_ty,
                scrutinee_solved_var,
            );
            try self.aliasPatternBindings(module_idx, match_pattern_idx, bind_pattern_idx, &branch_env);
        }
        try self.collectBranchValueInfo(module_idx, type_scope, branch_env, branch_value, expected_result_var);

        var branch_body = try self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            branch_env,
            branch_value,
            result_ty,
            expected_result_var,
        );
        if (guard_expr) |guard_idx| {
            const guard_bool_ty = try self.makePrimitiveType(.bool);
            const lowered_guard = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                branch_env,
                guard_idx,
                guard_bool_ty,
                null,
            );
            branch_body = try self.program.store.addExpr(.{
                .ty = result_ty,
                .data = .{ .if_ = .{
                    .cond = lowered_guard,
                    .then_body = branch_body,
                    .else_body = else_expr,
                } },
            });
        }
        const then_expr = try self.wrapExprWithBindingDecls(branch_body, binding_decls.items);

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

    const GuardEntry = struct {
        pattern_idx: CIR.Pattern.Idx,
        expr: ast.ExprId,
    };

    fn lowerTagPatternGuardExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        pattern_idx: CIR.Pattern.Idx,
        tag: @FieldType(CIR.Pattern, "applied_tag"),
        then_expr: ast.ExprId,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        if (self.ctx.types.getType(scrutinee_ty) == .primitive and self.ctx.types.getType(scrutinee_ty).primitive == .bool) {
            if (self.ctx.typedCirModule(module_idx).slicePatterns(tag.args).len != 0) {
                return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
            }
            const bool_pat = try self.program.store.addPat(.{
                .ty = scrutinee_ty,
                .data = .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag.name) },
            });
            const branches = [_]ast.Branch{
                .{ .pat = bool_pat, .body = then_expr },
                .{ .pat = try self.program.store.addPat(.{ .ty = scrutinee_ty, .data = .{ .var_ = symbol_mod.Symbol.none } }), .body = else_expr },
            };
            return try self.program.store.addExpr(.{
                .ty = self.program.store.getExpr(then_expr).ty,
                .data = .{ .when = .{
                    .cond = scrutinee_expr,
                    .branches = try self.program.store.addBranchSpan(&branches),
                } },
            });
        }

        var guard_entries = std.ArrayList(GuardEntry).empty;
        defer guard_entries.deinit(self.allocator);

        const tag_pat = try self.lowerGuardablePattern(module_idx, type_scope, pattern_idx, &guard_entries);

        var current = then_expr;
        var idx = guard_entries.items.len;
        while (idx > 0) {
            idx -= 1;
            const guard = guard_entries.items[idx];
            const guard_ty = self.requirePatternSourceType(module_idx, type_scope, guard.pattern_idx);
            current = try self.lowerPatternGuardExpr(
                module_idx,
                type_scope,
                guard.expr,
                guard_ty,
                guard.pattern_idx,
                current,
                else_expr,
            );
        }

        const branches = [_]ast.Branch{
            .{ .pat = tag_pat, .body = current },
            .{ .pat = try self.program.store.addPat(.{ .ty = scrutinee_ty, .data = .{ .var_ = symbol_mod.Symbol.none } }), .body = else_expr },
        };
        return try self.program.store.addExpr(.{
            .ty = self.program.store.getExpr(then_expr).ty,
            .data = .{ .when = .{
                .cond = scrutinee_expr,
                .branches = try self.program.store.addBranchSpan(&branches),
            } },
        });
    }

    fn lowerGuardablePattern(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        guard_entries: *std.ArrayList(GuardEntry),
    ) std.mem.Allocator.Error!ast.PatId {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = typed_cir_module.pattern(pattern_idx).data;
        const pattern_ty = self.requirePatternSourceType(module_idx, type_scope, pattern_idx);
        switch (pattern) {
            .assign => |assign| return self.program.store.addPat(.{
                .ty = pattern_ty,
                .data = .{ .var_ = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident) },
            }),
            .as => |as_pat| {
                const symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident);
                const pat_id = try self.program.store.addPat(.{
                    .ty = pattern_ty,
                    .data = .{ .var_ = symbol },
                });
                try guard_entries.append(self.allocator, .{
                    .pattern_idx = as_pat.pattern,
                    .expr = try self.makeVarExpr(pattern_ty, symbol),
                });
                return pat_id;
            },
            .underscore => return self.program.store.addPat(.{
                .ty = pattern_ty,
                .data = .{ .var_ = symbol_mod.Symbol.none },
            }),
            .nominal => |nominal| return self.lowerGuardablePattern(module_idx, type_scope, nominal.backing_pattern, guard_entries),
            .nominal_external => |nominal| return self.lowerGuardablePattern(module_idx, type_scope, nominal.backing_pattern, guard_entries),
            .applied_tag => |tag| {
                if (self.ctx.types.getType(pattern_ty) == .primitive and self.ctx.types.getType(pattern_ty).primitive == .bool) {
                    if (typed_cir_module.slicePatterns(tag.args).len != 0) {
                        return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    return self.program.store.addPat(.{
                        .ty = pattern_ty,
                        .data = .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag.name) },
                    });
                }
                const arg_patterns = typed_cir_module.slicePatterns(tag.args);
                const lowered_args = try self.allocator.alloc(ast.PatId, arg_patterns.len);
                defer self.allocator.free(lowered_args);
                for (arg_patterns, 0..) |arg_pat, i| {
                    lowered_args[i] = try self.lowerGuardablePattern(module_idx, type_scope, arg_pat, guard_entries);
                }
                return self.program.store.addPat(.{
                    .ty = pattern_ty,
                    .data = .{ .tag = .{
                        .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
                        .discriminant = self.requirePatternTagDiscriminant(module_idx, type_scope, pattern_idx),
                        .args = try self.program.store.addPatSpan(lowered_args),
                    } },
                });
            },
            .list,
            .record_destructure,
            .tuple,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .runtime_error,
            => {
                const symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE);
                const pat_id = try self.program.store.addPat(.{
                    .ty = pattern_ty,
                    .data = .{ .var_ = symbol },
                });
                try guard_entries.append(self.allocator, .{
                    .pattern_idx = pattern_idx,
                    .expr = try self.makeVarExpr(pattern_ty, symbol),
                });
                return pat_id;
            },
        }
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
                if (self.ctx.types.getType(scrutinee_ty) != .record) {
                    return self.makeRuntimeErrorExprAt(self.program.store.getExpr(then_expr).ty);
                }
                var current = then_expr;
                const destructs = typed_cir_module.sliceRecordDestructs(record.destructs);
                var idx = destructs.len;
                while (idx > 0) {
                    idx -= 1;
                    const destruct = typed_cir_module.getRecordDestruct(destructs[idx]);
                    const child_pattern_idx = destruct.kind.toPatternIdx();
                    if (!try self.patternNeedsAnyExplicitMatch(module_idx, type_scope, child_pattern_idx)) continue;
                    const field_ty = self.requirePatternSourceType(module_idx, type_scope, child_pattern_idx);
                    const field_expr = try self.program.store.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .access = .{
                            .record = scrutinee_expr,
                            .field = try self.ctx.copyExecutableIdent(module_idx, destruct.label),
                            .field_index = self.requireRecordDestructFieldIndex(module_idx, type_scope, destructs[idx]),
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
                if (self.ctx.types.getType(scrutinee_ty) != .tuple) {
                    return self.makeRuntimeErrorExprAt(self.program.store.getExpr(then_expr).ty);
                }
                var current = then_expr;
                const elem_patterns = typed_cir_module.slicePatterns(tuple.patterns);
                var idx = elem_patterns.len;
                while (idx > 0) {
                    idx -= 1;
                    const child_pattern_idx = elem_patterns[idx];
                    if (!try self.patternNeedsAnyExplicitMatch(module_idx, type_scope, child_pattern_idx)) continue;
                    const elem_ty = self.requirePatternSourceType(module_idx, type_scope, child_pattern_idx);
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
            .list => |list| blk: {
                const elem_ty = self.requirePatternListElemType(module_idx, type_scope, pattern_idx);
                const bool_ty = try self.makePrimitiveType(.bool);
                const u64_ty = try self.makePrimitiveType(.u64);
                const patterns = typed_cir_module.slicePatterns(list.patterns);
                const prefix_len: usize = if (list.rest_info) |rest| @intCast(rest.index) else patterns.len;
                const suffix_len = patterns.len - prefix_len;

                const len_expr = try self.makeLowLevelExpr(u64_ty, .list_len, &.{scrutinee_expr});
                const expected_len_expr = try self.makeU64LiteralExpr(@intCast(patterns.len));
                const cond_expr = if (list.rest_info != null)
                    try self.makeLowLevelExpr(bool_ty, .num_is_gte, &.{ len_expr, expected_len_expr })
                else
                    try self.makeLowLevelExpr(bool_ty, .num_is_eq, &.{ len_expr, expected_len_expr });

                var current = then_expr;
                var idx = patterns.len;
                while (idx > 0) {
                    idx -= 1;
                    const elem_pattern_idx = patterns[idx];
                    if (!try self.patternNeedsAnyExplicitMatch(module_idx, type_scope, elem_pattern_idx)) continue;
                    const elem_index_expr = if (idx < prefix_len)
                        try self.makeU64LiteralExpr(@intCast(idx))
                    else blk_index: {
                        const suffix_index = idx - prefix_len;
                        const suffix_start = try self.makeLowLevelExpr(
                            u64_ty,
                            .num_minus,
                            &.{ len_expr, try self.makeU64LiteralExpr(@intCast(suffix_len)) },
                        );
                        if (suffix_index == 0) break :blk_index suffix_start;
                        break :blk_index try self.makeLowLevelExpr(
                            u64_ty,
                            .num_plus,
                            &.{ suffix_start, try self.makeU64LiteralExpr(@intCast(suffix_index)) },
                        );
                    };
                    const elem_expr = try self.makeLowLevelExpr(elem_ty, .list_get_unsafe, &.{ scrutinee_expr, elem_index_expr });
                    current = try self.lowerPatternGuardExpr(
                        module_idx,
                        type_scope,
                        elem_expr,
                        elem_ty,
                        elem_pattern_idx,
                        current,
                        else_expr,
                    );
                }

                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        if (try self.patternNeedsAnyExplicitMatch(module_idx, type_scope, rest_pattern_idx)) {
                            const start_expr = try self.makeU64LiteralExpr(rest.index);
                            const rest_len_expr = try self.makeLowLevelExpr(u64_ty, .num_minus, &.{ len_expr, expected_len_expr });
                            const bounds_expr = try self.makeListSliceBoundsExpr(start_expr, rest_len_expr);
                            const rest_expr = try self.makeLowLevelExpr(scrutinee_ty, .list_sublist, &.{ scrutinee_expr, bounds_expr });
                            current = try self.lowerPatternGuardExpr(
                                module_idx,
                                type_scope,
                                rest_expr,
                                scrutinee_ty,
                                rest_pattern_idx,
                                current,
                                else_expr,
                            );
                        }
                    }
                }

                break :blk try self.program.store.addExpr(.{
                    .ty = self.program.store.getExpr(then_expr).ty,
                    .data = .{ .if_ = .{
                        .cond = cond_expr,
                        .then_body = current,
                        .else_body = else_expr,
                    } },
                });
            },
            .applied_tag => |tag| try self.lowerTagPatternGuardExpr(
                module_idx,
                type_scope,
                scrutinee_expr,
                scrutinee_ty,
                pattern_idx,
                tag,
                then_expr,
                else_expr,
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
        scrutinee_solved_var: ?Var,
        match_pattern_idx: CIR.Pattern.Idx,
        bind_pattern_idx: CIR.Pattern.Idx,
        guard_expr: ?CIR.Expr.Idx,
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
            match_pattern_idx,
            scrutinee_expr,
            scrutinee_ty,
            scrutinee_solved_var,
            &branch_env,
            &binding_decls,
        );
        if (match_pattern_idx != bind_pattern_idx) {
            try self.recordPatternStructuralInfoFromSourceType(
                module_idx,
                type_scope,
                bind_pattern_idx,
                scrutinee_ty,
                scrutinee_solved_var,
            );
            try self.aliasPatternBindings(module_idx, match_pattern_idx, bind_pattern_idx, &branch_env);
        }
        try self.collectBranchValueInfo(module_idx, type_scope, branch_env, branch_value, expected_result_var);

        var branch_body = try self.lowerExprWithExpectedType(
            module_idx,
            type_scope,
            branch_env,
            branch_value,
            result_ty,
            expected_result_var,
        );
        if (guard_expr) |guard_idx| {
            const guard_bool_ty = try self.makePrimitiveType(.bool);
            const lowered_guard = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                branch_env,
                guard_idx,
                guard_bool_ty,
                null,
            );
            branch_body = try self.program.store.addExpr(.{
                .ty = result_ty,
                .data = .{ .if_ = .{
                    .cond = lowered_guard,
                    .then_body = branch_body,
                    .else_body = else_expr,
                } },
            });
        }
        return try self.wrapExprWithBindingDecls(branch_body, binding_decls.items);
    }

    fn collectBranchValueInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        branch_env: BindingEnv,
        branch_value: CIR.Expr.Idx,
        expected_result_var: ?Var,
    ) std.mem.Allocator.Error!void {
        if (expected_result_var) |result_var| {
            try self.collectExprInfoWithResultVar(
                module_idx,
                type_scope,
                branch_env,
                branch_value,
                result_var,
            );
        } else {
            try self.collectExprInfo(module_idx, type_scope, branch_env, branch_value);
        }
        try self.finalizeExprTypes(module_idx, type_scope);
        try self.finalizePatternTypes(module_idx, type_scope);
        try self.recordPatternTypesFromBindings(module_idx, type_scope, branch_env);
    }

    fn recordPatternTypesFromBindings(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
    ) std.mem.Allocator.Error!void {
        var iter = env.iterator();
        while (iter.next()) |entry| {
            if (entry.key_ptr.*.module_idx != module_idx) continue;
            const pattern_idx: CIR.Pattern.Idx = @enumFromInt(entry.key_ptr.*.pattern_idx);
            const key: TypeCloneScope.PatternTypeKey = .{
                .module_idx = module_idx,
                .pattern_idx = pattern_idx,
            };
            if (type_scope.memo.pattern_source_type_cache.contains(key)) continue;
            const pattern_var = self.ctx.typedCirModule(module_idx).patternType(pattern_idx);
            const scoped_var = try self.scopeVar(type_scope, module_idx, pattern_var);
            const mono_ty = try self.instantiateVarType(module_idx, type_scope, scoped_var);
            try self.recordPatternSourceType(module_idx, type_scope, pattern_idx, mono_ty);
        }
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
        match_pattern_idx: CIR.Pattern.Idx,
        bind_pattern_idx: CIR.Pattern.Idx,
        guard_expr: ?CIR.Expr.Idx,
        list_pattern: @FieldType(CIR.Pattern, "list"),
        branch_value: CIR.Expr.Idx,
        else_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        _ = list_pattern;
        const then_expr = try self.lowerWholeValuePatternBranch(
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
            guard_expr,
            branch_value,
            else_expr,
        );
        return try self.lowerPatternGuardExpr(
            module_idx,
            type_scope,
            scrutinee_expr,
            scrutinee_ty,
            match_pattern_idx,
            then_expr,
            else_expr,
        );
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
        match_pattern_idx: CIR.Pattern.Idx,
        pattern: CIR.Pattern,
        bind_pattern_idx: CIR.Pattern.Idx,
        guard_expr: ?CIR.Expr.Idx,
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
            match_pattern_idx,
            bind_pattern_idx,
            guard_expr,
            branch_value,
            else_expr,
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
            .num_literal => |num| blk: {
                break :blk try self.lowerNumericIntLiteralData(target_ty, num.value);
            },
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
        try self.recordPatternStructuralInfoFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const effective_source_ty = self.requirePatternSourceType(module_idx, type_scope, pattern_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
            .applied_tag => |tag| {
                if (self.ctx.types.getType(effective_source_ty) == .primitive and self.ctx.types.getType(effective_source_ty).primitive == .bool) {
                    if (self.ctx.typedCirModule(module_idx).slicePatterns(tag.args).len != 0) {
                        return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    return;
                }
                const arg_patterns = self.ctx.typedCirModule(module_idx).slicePatterns(tag.args);
                const arg_tys = try self.requireTagPayloadTypesFromMonotype(
                    module_idx,
                    effective_source_ty,
                    tag.name,
                );
                if (arg_patterns.len != arg_tys.len) {
                    return debugPanic(
                        "monotype pattern invariant violated: tag pattern arity {d} did not match payload arity {d} in module {d}",
                        .{ arg_patterns.len, arg_tys.len, module_idx },
                    );
                }
                for (arg_patterns, arg_tys, 0..) |arg_pattern_idx, arg_ty, i| {
                    const payload_expr = try self.makeMatchedTagPayloadExpr(
                        module_idx,
                        type_scope,
                        source_expr,
                        effective_source_ty,
                        pattern_idx,
                        tag,
                        i,
                        arg_ty,
                    );
                    try self.bindPatternFromSourceExpr(
                        module_idx,
                        type_scope,
                        arg_pattern_idx,
                        payload_expr,
                        arg_ty,
                        try self.requirePatternSolvedVar(module_idx, type_scope, arg_pattern_idx),
                        env,
                        decls,
                    );
                }
            },
            .list => |list| {
                const elem_ty = self.requirePatternListElemType(module_idx, type_scope, pattern_idx);
                const u64_ty = try self.makePrimitiveType(.u64);
                const patterns = self.ctx.typedCirModule(module_idx).slicePatterns(list.patterns);
                const prefix_len: usize = if (list.rest_info) |rest| @intCast(rest.index) else patterns.len;
                const suffix_len = patterns.len - prefix_len;
                const len_expr = try self.makeLowLevelExpr(u64_ty, .list_len, &.{source_expr});
                const elem_solved_var = self.lookupListElemSolvedVar(module_idx, type_scope, source_solved_var);

                for (patterns[0..prefix_len], 0..) |elem_pattern_idx, i| {
                    const index_expr = try self.makeU64LiteralExpr(@intCast(i));
                    const elem_expr = try self.makeLowLevelExpr(elem_ty, .list_get_unsafe, &.{ source_expr, index_expr });
                    try self.bindPatternFromSourceExpr(
                        module_idx,
                        type_scope,
                        elem_pattern_idx,
                        elem_expr,
                        elem_ty,
                        elem_solved_var,
                        env,
                        decls,
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
                        const elem_expr = try self.makeLowLevelExpr(elem_ty, .list_get_unsafe, &.{ source_expr, index_expr });
                        try self.bindPatternFromSourceExpr(
                            module_idx,
                            type_scope,
                            elem_pattern_idx,
                            elem_expr,
                            elem_ty,
                            elem_solved_var,
                            env,
                            decls,
                        );
                    }
                }

                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        const expected_len_expr = try self.makeU64LiteralExpr(@intCast(patterns.len));
                        const start_expr = try self.makeU64LiteralExpr(rest.index);
                        const rest_len_expr = try self.makeLowLevelExpr(u64_ty, .num_minus, &.{ len_expr, expected_len_expr });
                        const bounds_expr = try self.makeListSliceBoundsExpr(start_expr, rest_len_expr);
                        const rest_expr = try self.makeLowLevelExpr(effective_source_ty, .list_sublist, &.{ source_expr, bounds_expr });
                        try self.bindPatternFromSourceExpr(
                            module_idx,
                            type_scope,
                            rest_pattern_idx,
                            rest_expr,
                            effective_source_ty,
                            source_solved_var,
                            env,
                            decls,
                        );
                    }
                }
            },
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

    fn aliasPatternBindings(
        self: *Lowerer,
        module_idx: u32,
        match_pattern_idx: CIR.Pattern.Idx,
        bind_pattern_idx: CIR.Pattern.Idx,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        if (match_pattern_idx == bind_pattern_idx) return;
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const match_pattern = typed_cir_module.pattern(match_pattern_idx).data;
        const bind_pattern = typed_cir_module.pattern(bind_pattern_idx).data;

        switch (match_pattern) {
            .assign => {
                if (bind_pattern != .assign) {
                    debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{});
                }
                try self.aliasBindingEntry(module_idx, match_pattern_idx, bind_pattern_idx, env);
            },
            .as => |match_as| {
                if (bind_pattern != .as) {
                    debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{});
                }
                const bind_as = bind_pattern.as;
                try self.aliasBindingEntry(module_idx, match_pattern_idx, bind_pattern_idx, env);
                try self.aliasPatternBindings(module_idx, match_as.pattern, bind_as.pattern, env);
            },
            .underscore => {
                if (bind_pattern != .underscore) {
                    debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{});
                }
            },
            .nominal => |match_nominal| {
                const bind_backing = switch (bind_pattern) {
                    .nominal => |nominal| nominal.backing_pattern,
                    .nominal_external => |nominal| nominal.backing_pattern,
                    else => debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{}),
                };
                try self.aliasPatternBindings(module_idx, match_nominal.backing_pattern, bind_backing, env);
            },
            .nominal_external => |match_nominal| {
                const bind_backing = switch (bind_pattern) {
                    .nominal => |nominal| nominal.backing_pattern,
                    .nominal_external => |nominal| nominal.backing_pattern,
                    else => debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{}),
                };
                try self.aliasPatternBindings(module_idx, match_nominal.backing_pattern, bind_backing, env);
            },
            .record_destructure => |match_record| {
                if (bind_pattern != .record_destructure) {
                    debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{});
                }
                const bind_record = bind_pattern.record_destructure;
                const match_destructs = typed_cir_module.sliceRecordDestructs(match_record.destructs);
                const bind_destructs = typed_cir_module.sliceRecordDestructs(bind_record.destructs);
                for (match_destructs) |match_destruct_idx| {
                    const match_destruct = typed_cir_module.getRecordDestruct(match_destruct_idx);
                    const match_label = typed_cir_module.getIdent(match_destruct.label);
                    var bind_destruct_idx: ?CIR.Pattern.RecordDestruct.Idx = null;
                    for (bind_destructs) |candidate_idx| {
                        const candidate = typed_cir_module.getRecordDestruct(candidate_idx);
                        if (std.mem.eql(u8, typed_cir_module.getIdent(candidate.label), match_label)) {
                            bind_destruct_idx = candidate_idx;
                            break;
                        }
                    }
                    const resolved_bind_destruct = bind_destruct_idx orelse debugPanic(
                        "monotype invariant violated: mismatched or-pattern record destructure fields",
                        .{},
                    );
                    const bind_destruct = typed_cir_module.getRecordDestruct(resolved_bind_destruct);
                    try self.aliasPatternBindings(
                        module_idx,
                        match_destruct.kind.toPatternIdx(),
                        bind_destruct.kind.toPatternIdx(),
                        env,
                    );
                }
            },
            .tuple => |match_tuple| {
                if (bind_pattern != .tuple) {
                    debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{});
                }
                const bind_tuple = bind_pattern.tuple;
                const match_elems = typed_cir_module.slicePatterns(match_tuple.patterns);
                const bind_elems = typed_cir_module.slicePatterns(bind_tuple.patterns);
                if (match_elems.len != bind_elems.len) {
                    debugPanic("monotype invariant violated: mismatched or-pattern tuple arity", .{});
                }
                for (match_elems, bind_elems) |match_elem, bind_elem| {
                    try self.aliasPatternBindings(module_idx, match_elem, bind_elem, env);
                }
            },
            .list => |match_list| {
                if (bind_pattern != .list) {
                    debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{});
                }
                const bind_list = bind_pattern.list;
                const match_patterns = typed_cir_module.slicePatterns(match_list.patterns);
                const bind_patterns = typed_cir_module.slicePatterns(bind_list.patterns);
                if (match_patterns.len != bind_patterns.len) {
                    debugPanic("monotype invariant violated: mismatched or-pattern list arity", .{});
                }
                const match_rest = match_list.rest_info;
                const bind_rest = bind_list.rest_info;
                if ((match_rest == null) != (bind_rest == null)) {
                    debugPanic("monotype invariant violated: mismatched or-pattern list rest shape", .{});
                }
                if (match_rest != null and bind_rest != null) {
                    if (match_rest.?.index != bind_rest.?.index) {
                        debugPanic("monotype invariant violated: mismatched or-pattern list rest index", .{});
                    }
                    if ((match_rest.?.pattern == null) != (bind_rest.?.pattern == null)) {
                        debugPanic("monotype invariant violated: mismatched or-pattern list rest pattern", .{});
                    }
                }
                for (match_patterns, bind_patterns) |match_elem, bind_elem| {
                    try self.aliasPatternBindings(module_idx, match_elem, bind_elem, env);
                }
                if (match_rest) |rest| {
                    if (rest.pattern) |match_rest_pattern| {
                        try self.aliasPatternBindings(module_idx, match_rest_pattern, bind_rest.?.pattern.?, env);
                    }
                }
            },
            .applied_tag => |match_tag| {
                if (bind_pattern != .applied_tag) {
                    debugPanic("monotype invariant violated: mismatched or-pattern binding shapes", .{});
                }
                const bind_tag = bind_pattern.applied_tag;
                const match_args = typed_cir_module.slicePatterns(match_tag.args);
                const bind_args = typed_cir_module.slicePatterns(bind_tag.args);
                if (match_args.len != bind_args.len) {
                    debugPanic("monotype invariant violated: mismatched or-pattern tag arity", .{});
                }
                for (match_args, bind_args) |match_arg, bind_arg| {
                    try self.aliasPatternBindings(module_idx, match_arg, bind_arg, env);
                }
            },
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => {},
            else => debugTodoPattern(match_pattern),
        }
    }

    fn aliasBindingEntry(
        _: *Lowerer,
        module_idx: u32,
        match_pattern_idx: CIR.Pattern.Idx,
        bind_pattern_idx: CIR.Pattern.Idx,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        const match_key: PatternKey = .{
            .module_idx = module_idx,
            .pattern_idx = @intFromEnum(match_pattern_idx),
        };
        const bind_key: PatternKey = .{
            .module_idx = module_idx,
            .pattern_idx = @intFromEnum(bind_pattern_idx),
        };
        const entry = env.get(match_key) orelse debugPanic(
            "monotype invariant violated: missing match binding for or-pattern aliasing",
            .{},
        );
        try env.put(bind_key, entry);
    }

    fn makeMatchedTagPayloadExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        scrutinee_expr: ast.ExprId,
        scrutinee_ty: type_mod.TypeId,
        pattern_idx: CIR.Pattern.Idx,
        tag: @FieldType(CIR.Pattern, "applied_tag"),
        payload_index: usize,
        payload_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const arg_patterns = typed_cir_module.slicePatterns(tag.args);
        const arg_tys = try self.requireTagPayloadTypesFromMonotype(module_idx, scrutinee_ty, tag.name);
        if (arg_patterns.len != arg_tys.len) {
            return debugPanic(
                "monotype pattern invariant violated: tag payload arity {d} did not match payload types {d} in module {d}",
                .{ arg_patterns.len, arg_tys.len, module_idx },
            );
        }
        if (payload_index >= arg_tys.len) {
            return debugPanic("monotype pattern invariant violated: tag payload index out of bounds", .{});
        }
        return try self.program.store.addExpr(.{
            .ty = payload_ty,
            .data = .{ .tag_payload = .{
                .tag_union = scrutinee_expr,
                .tag_discriminant = self.requirePatternTagDiscriminant(module_idx, type_scope, pattern_idx),
                .payload_index = @intCast(payload_index),
            } },
        });
    }

    fn lowerBlockExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        incoming_env: BindingEnv,
        stmts_span: CIR.Statement.Span,
        final_expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        var info_env = try self.cloneEnv(incoming_env);
        defer info_env.deinit();
        const info_stmts = self.ctx.typedCirModule(module_idx).sliceStatements(stmts_span);
        var stmt_index: usize = 0;
        while (stmt_index < info_stmts.len) {
            if (try self.seedLocalLambdaDeclGroupInfo(module_idx, type_scope, &info_env, info_stmts, stmt_index)) |group_end| {
                stmt_index = group_end;
                continue;
            }
            try self.collectStmtInfo(module_idx, type_scope, &info_env, info_stmts[stmt_index]);
            stmt_index += 1;
        }
        try self.collectExprInfo(module_idx, type_scope, info_env, final_expr_idx);
        try self.finalizeExprTypes(module_idx, type_scope);
        try self.finalizePatternTypes(module_idx, type_scope);

        var env = try self.cloneEnv(incoming_env);
        defer env.deinit();

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const cir_stmts = typed_cir_module.sliceStatements(stmts_span);
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
        _: type_mod.TypeId,
        _: ?Var,
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        var info_env = try self.cloneEnv(incoming_env);
        defer info_env.deinit();
        const info_stmts = self.ctx.typedCirModule(module_idx).sliceStatements(stmts_span);
        var stmt_index: usize = 0;
        while (stmt_index < info_stmts.len) {
            if (try self.seedLocalLambdaDeclGroupInfo(module_idx, type_scope, &info_env, info_stmts, stmt_index)) |group_end| {
                stmt_index = group_end;
                continue;
            }
            try self.collectStmtInfo(module_idx, type_scope, &info_env, info_stmts[stmt_index]);
            stmt_index += 1;
        }
        try self.collectExprInfo(module_idx, type_scope, info_env, final_expr_idx);
        try self.finalizeExprTypes(module_idx, type_scope);
        try self.finalizePatternTypes(module_idx, type_scope);

        var env = try self.cloneEnv(incoming_env);
        defer env.deinit();

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const cir_stmts = typed_cir_module.sliceStatements(stmts_span);
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

    fn seedLocalLambdaDeclGroupInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: *BindingEnv,
        cir_stmts: []const CIR.Statement.Idx,
        start_idx: usize,
    ) std.mem.Allocator.Error!?usize {
        if (start_idx >= cir_stmts.len) return null;

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const first_stmt = typed_cir_module.getStatement(cir_stmts[start_idx]);
        if (first_stmt != .s_decl) return null;
        if (!isLambdaExpr(self.ctx.typedCirModule(module_idx).expr(first_stmt.s_decl.expr).data)) return null;

        var end_idx = start_idx;
        while (end_idx < cir_stmts.len) : (end_idx += 1) {
            const stmt = typed_cir_module.getStatement(cir_stmts[end_idx]);
            if (stmt != .s_decl) break;
            if (!isLambdaExpr(self.ctx.typedCirModule(module_idx).expr(stmt.s_decl.expr).data)) break;
        }

        for (cir_stmts[start_idx..end_idx]) |stmt_idx| {
            const decl = typed_cir_module.getStatement(stmt_idx).s_decl;
            try self.putLocalFnSeedBinding(env, .{
                .module_idx = module_idx,
                .pattern_idx = @intFromEnum(decl.pattern),
            }, try self.scopeVar(type_scope, module_idx, typed_cir_module.expr(decl.expr).ty()));
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
        if (start_idx >= cir_stmts.len) return null;

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const first_stmt = typed_cir_module.getStatement(cir_stmts[start_idx]);
        if (first_stmt != .s_decl) return null;
        if (!isLambdaExpr(self.ctx.typedCirModule(module_idx).expr(first_stmt.s_decl.expr).data)) return null;

        var end_idx = start_idx;
        while (end_idx < cir_stmts.len) : (end_idx += 1) {
            const stmt = typed_cir_module.getStatement(cir_stmts[end_idx]);
            if (stmt != .s_decl) break;
            if (!isLambdaExpr(self.ctx.typedCirModule(module_idx).expr(stmt.s_decl.expr).data)) break;
        }

        const group = try self.allocator.create(LocalFnGroupState);
        errdefer self.allocator.destroy(group);
        const sources = try self.allocator.alloc(LocalFnSource, end_idx - start_idx);
        errdefer self.allocator.free(sources);

        group.* = .{
            .module_idx = module_idx,
            .declaration_env = FrozenBindingEnv.init(self.allocator),
            .insertion_index = lowered.items.len,
            .sources = sources,
            .pending = .empty,
        };
        errdefer group.deinit(self.allocator);

        for (cir_stmts[start_idx..end_idx], 0..) |stmt_idx, group_i| {
            const decl = typed_cir_module.getStatement(stmt_idx).s_decl;
            group.sources[group_i] = .{
                .pattern_idx = decl.pattern,
                .expr_idx = decl.expr,
                .seed_var = typed_cir_module.expr(decl.expr).ty(),
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

        group.declaration_env = try self.freezeBindingEnv(type_scope, env.*);
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const stmt = typed_cir_module.getStatement(stmt_idx);

        switch (stmt) {
            .s_decl => |decl| {
                const body_ty = try self.requireExprType(module_idx, type_scope, decl.expr);
                const body_var = self.requireExprResultVar(module_idx, type_scope, decl.expr);
                const lowered_body = try self.lowerExprInfoWithExpectedType(
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
                const body_ty = try self.requireExprType(module_idx, type_scope, decl.expr);
                const body_var = self.requireExprResultVar(module_idx, type_scope, decl.expr);
                const lowered_body = try self.lowerExprInfoWithExpectedType(
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
                const body_ty = try self.requirePatternType(module_idx, type_scope, reassign.pattern_idx);
                const lowered_body = try self.lowerExprInfoWithExpectedType(
                    module_idx,
                    type_scope,
                    env.*,
                    reassign.expr,
                    body_ty,
                    self.requireExprResultVar(module_idx, type_scope, reassign.expr),
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
                const elem_ty = self.requireListElemTypeFromMonotype(
                    try self.requireExprType(module_idx, type_scope, for_stmt.expr),
                );
                const elem_solved_var = self.lookupListElemSolvedVar(
                    module_idx,
                    type_scope,
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
                    break :blk try self.lowerPatWithType(module_idx, type_scope, for_stmt.patt, elem_ty);
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
            .s_alias_decl => |_| {
                // Type declarations are compile-time only; no runtime statement.
            },
            .s_nominal_decl => |_| {
                // Type declarations are compile-time only; no runtime statement.
            },
            .s_type_anno => |_| {
                // Type annotations are compile-time only; no runtime statement.
            },
            .s_type_var_alias => |_| {
                // Type var aliases are compile-time only; no runtime statement.
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const branch_ids = typed_cir_module.sliceIfBranches(if_expr.branches);
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
            const branch = typed_cir_module.getIfBranch(branch_ids[idx]);
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

        const branch = typed_cir_module.getIfBranch(branch_ids[0]);
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
        const elem_ty = self.requireListElemTypeFromMonotype(
            try self.requireExprType(module_idx, type_scope, iterable_expr_idx),
        );
        const elem_solved_var = self.lookupListElemSolvedVar(
            module_idx,
            type_scope,
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
            break :blk try self.lowerPatWithType(module_idx, type_scope, patt_idx, elem_ty);
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
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
        const ty = try self.requirePatternType(module_idx, type_scope, pattern_idx);
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
                for (self.ctx.typedCirModule(module_idx).slicePatterns(tag.args), 0..) |arg_pat, i| {
                    lowered_args[i] = try self.lowerPat(module_idx, type_scope, arg_pat);
                }
                break :blk self.program.store.addPat(.{
                    .ty = ty,
                    .data = .{ .tag = .{
                        .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
                        .discriminant = self.requirePatternTagDiscriminant(module_idx, type_scope, pattern_idx),
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
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.PatId {
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
            .applied_tag => |tag| blk: {
                const lowered_args = try self.allocator.alloc(ast.PatId, tag.args.span.len);
                defer self.allocator.free(lowered_args);
                for (self.ctx.typedCirModule(module_idx).slicePatterns(tag.args), 0..) |arg_pat, i| {
                    lowered_args[i] = try self.lowerPat(module_idx, type_scope, arg_pat);
                }
                break :blk self.program.store.addPat(.{
                    .ty = ty,
                    .data = .{ .tag = .{
                        .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
                        .discriminant = self.requirePatternTagDiscriminant(module_idx, type_scope, pattern_idx),
                        .args = try self.program.store.addPatSpan(lowered_args),
                    } },
                });
            },
            .nominal => |nominal| try self.lowerPatWithType(module_idx, type_scope, nominal.backing_pattern, ty),
            .nominal_external => |nominal| try self.lowerPatWithType(module_idx, type_scope, nominal.backing_pattern, ty),
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
        return self.lowerExprSlice(module_idx, type_scope, env, self.ctx.typedCirModule(module_idx).sliceExpr(span));
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const fields = typed_cir_module.sliceRecordFields(span);
        const out = try self.allocator.alloc(ast.FieldExpr, fields.len);
        defer self.allocator.free(out);

        for (fields, 0..) |field_idx, i| {
            const field = typed_cir_module.getRecordField(field_idx);
            out[i] = .{
                .name = try self.ctx.copyExecutableIdent(module_idx, field.name),
                .value = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    field.value,
                    try self.requireExprType(module_idx, type_scope, field.value),
                    try self.exprResultVar(module_idx, type_scope, env, field.value),
                ),
            };
        }

        switch (self.ctx.types.getType(record_ty)) {
            .record => {},
            else => debugPanic("monotype invariant violated: record expression lowered with non-record type", .{}),
        }
        const OrderedField = struct {
            index: u16,
            field: ast.FieldExpr,
        };
        const ordered = try self.allocator.alloc(OrderedField, out.len);
        defer self.allocator.free(ordered);
        for (out, 0..) |field, i| {
            ordered[i] = .{
                .index = self.requireRecordFieldIndexFromMonotypeType(record_ty, self.ctx.idents.getText(field.name)),
                .field = field,
            };
        }
        std.mem.sort(OrderedField, ordered, {}, struct {
            fn lessThan(_: void, a: OrderedField, b: OrderedField) bool {
                return a.index < b.index;
            }
        }.lessThan);

        const sorted = try self.allocator.alloc(ast.FieldExpr, ordered.len);
        defer self.allocator.free(sorted);
        for (ordered, 0..) |entry, i| {
            sorted[i] = entry.field;
        }

        return try self.program.store.addFieldExprSpan(sorted);
    }

    fn lowerRecordExpr(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        record_ty: type_mod.TypeId,
        record_var: Var,
        record: @FieldType(CIR.Expr, "e_record"),
    ) std.mem.Allocator.Error!ast.ExprId {
        if (record.ext) |base_expr_idx| {
            return self.lowerRecordUpdateExpr(
                module_idx,
                type_scope,
                env,
                record_ty,
                record_var,
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
        _: Var,
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

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const update_field_ids = typed_cir_module.sliceRecordFields(update_fields_span);
        const lowered_updates = try self.allocator.alloc(struct {
            name_text: []const u8,
            value: ast.ExprId,
        }, update_field_ids.len);
        defer self.allocator.free(lowered_updates);
        for (update_field_ids, 0..) |field_idx, i| {
            const field = typed_cir_module.getRecordField(field_idx);
            lowered_updates[i] = .{
                .name_text = typed_cir_module.getIdent(field.name),
                .value = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    field.value,
                    try self.requireExprType(module_idx, type_scope, field.value),
                    try self.exprResultVar(module_idx, type_scope, env, field.value),
                ),
            };
        }

        const final_fields = record.fields;
        const OrderedField = struct {
            index: u16,
            field: ast.FieldExpr,
        };
        const ordered_fields = try self.allocator.alloc(OrderedField, final_fields.len);
        defer self.allocator.free(ordered_fields);
        for (final_fields, 0..) |field, i| {
            const field_name_text = self.ctx.idents.getText(field.name);
            const field_index = self.requireRecordFieldIndexFromMonotypeType(record_ty, field_name_text);
            const value = blk: {
                for (lowered_updates) |updated| {
                    if (std.mem.eql(u8, updated.name_text, field_name_text)) {
                        break :blk updated.value;
                    }
                }
                break :blk try self.program.store.addExpr(.{
                    .ty = field.ty,
                    .data = .{ .access = .{
                        .record = base_expr,
                        .field = field.name,
                        .field_index = field_index,
                    } },
                });
            };
            ordered_fields[i] = .{
                .index = field_index,
                .field = .{
                    .name = field.name,
                    .value = value,
                },
            };
        }

        std.mem.sort(OrderedField, ordered_fields, {}, struct {
            fn lessThan(_: void, a: OrderedField, b: OrderedField) bool {
                return a.index < b.index;
            }
        }.lessThan);

        const lowered_fields = try self.allocator.alloc(ast.FieldExpr, ordered_fields.len);
        defer self.allocator.free(lowered_fields);
        for (ordered_fields, 0..) |entry, i| {
            lowered_fields[i] = entry.field;
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        return switch (self.ctx.typedCirModule(module_idx).expr(expr_idx).data) {
            .e_dot_access => |dot| blk: {
                if (dot.args == null or !dot.field_name.eql(method_name)) {
                    debugPanic("monotype static dispatch invariant violated: unexpected dot-access dispatch shape", .{});
                }
                break :blk .{
                    .implicit_receiver = dot.receiver,
                    .explicit_args = typed_cir_module.sliceExpr(dot.args.?),
                };
            },
            .e_type_var_dispatch => |dispatch| blk: {
                if (!dispatch.method_name.eql(method_name)) {
                    debugPanic("monotype static dispatch invariant violated: unexpected type-var dispatch shape", .{});
                }
                break :blk .{
                    .implicit_receiver = null,
                    .explicit_args = typed_cir_module.sliceExpr(dispatch.args),
                };
            },
            .e_call => |call| switch (self.ctx.typedCirModule(module_idx).expr(call.func).data) {
                .e_dot_access => |dot| blk: {
                    if (!dot.field_name.eql(method_name)) {
                        debugPanic("monotype static dispatch invariant violated: mismatched dot-access method name", .{});
                    }
                    break :blk .{
                        .implicit_receiver = dot.receiver,
                        .explicit_args = typed_cir_module.sliceExpr(call.args),
                    };
                },
                .e_type_var_dispatch => |dispatch| blk: {
                    if (!dispatch.method_name.eql(method_name)) {
                        debugPanic("monotype static dispatch invariant violated: mismatched type-var dispatch method name", .{});
                    }
                    break :blk .{
                        .implicit_receiver = null,
                        .explicit_args = typed_cir_module.sliceExpr(call.args),
                    };
                },
                else => debugPanic("monotype static dispatch invariant violated: call head is not recorded dispatch", .{}),
            },
            else => debugPanic("monotype static dispatch invariant violated: expr is not recorded dispatch", .{}),
        };
    }

    fn copyTopLevelDefExprVarToScope(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        source_module_idx: u32,
        def_idx: CIR.Def.Idx,
    ) std.mem.Allocator.Error!Var {
        const source_var = self.ctx.typedCirModule(source_module_idx).defType(def_idx);
        return try self.scopeVar(type_scope, source_module_idx, source_var);
    }

    fn lowerRecordedMethodCall(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        method_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!?LoweredCall {
        const args = self.getRecordedMethodArgs(module_idx, expr_idx, method_name);
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const alias_stmt = switch (typed_cir_module.expr(expr_idx).data) {
            .e_type_var_dispatch => |dispatch| dispatch.type_var_alias_stmt,
            .e_call => |call| switch (typed_cir_module.expr(call.func).data) {
                .e_type_var_dispatch => |dispatch| dispatch.type_var_alias_stmt,
                else => null,
            },
            else => null,
        };

        const source_receiver_var = if (args.implicit_receiver) |receiver_expr_idx|
            typed_cir_module.exprType(receiver_expr_idx)
        else if (alias_stmt) |stmt_idx| blk: {
            const stmt = typed_cir_module.getStatement(stmt_idx);
            const type_var_anno = switch (stmt) {
                .s_type_var_alias => |alias| alias.type_var_anno,
                else => return debugPanic(
                    "monotype static dispatch invariant violated: type-var dispatch missing alias statement in module {d}",
                    .{module_idx},
                ),
            };
            const source_var = typed_cir_module.typeAnnoType(type_var_anno);
            break :blk source_var;
        } else null;

        const receiver_var = if (source_receiver_var) |source_var| blk: {
            break :blk try self.scopeVar(type_scope, module_idx, source_var);
        } else blk: {
            const receiver_expr_idx = args.implicit_receiver orelse blk_inner: {
                if (args.explicit_args.len == 0) {
                    return debugPanic(
                        "monotype static dispatch invariant violated: method call missing receiver in module {d}",
                        .{module_idx},
                    );
                }
                break :blk_inner args.explicit_args[0];
            };
            break :blk self.requireExprResultVar(module_idx, type_scope, receiver_expr_idx);
        };

        const resolved_target = blk: {
            if (source_receiver_var) |source_var| {
                if (try self.resolveAttachedMethodTargetFromSourceVar(module_idx, source_var, method_name)) |target| {
                    break :blk target;
                }
            }

            const target = try self.resolveAttachedMethodTargetFromWorkspaceVar(
                module_idx,
                type_scope,
                receiver_var,
                method_name,
            ) orelse return null;
            break :blk target;
        };
        const source_fn_var = try self.copyTopLevelDefExprVarToScope(type_scope, resolved_target.module_idx, resolved_target.def_idx);

        const arg_exprs = blk: {
            if (args.implicit_receiver) |receiver| {
                const total_args_len = args.explicit_args.len + 1;
                const merged = try self.allocator.alloc(CIR.Expr.Idx, total_args_len);
                merged[0] = receiver;
                @memcpy(merged[1..], args.explicit_args);
                break :blk merged;
            }
            break :blk args.explicit_args;
        };
        defer if (args.implicit_receiver != null) self.allocator.free(arg_exprs);

        var call_info = try self.prepareCallInfo(module_idx, type_scope, expr_idx, source_fn_var, arg_exprs);
        defer call_info.deinit(self.allocator);

        const callee = try self.lowerResolvedTargetCallee(module_idx, call_info.call_scope, resolved_target, call_info.fn_ty, call_info.fn_var);

        const lowered_args = try self.allocator.alloc(ast.ExprId, arg_exprs.len);
        defer self.allocator.free(lowered_args);
        for (arg_exprs, 0..) |arg_expr_idx, i| {
            lowered_args[i] = try self.lowerExprWithExpectedType(
                module_idx,
                call_info.call_scope,
                env,
                arg_expr_idx,
                call_info.arg_tys[i],
                call_info.arg_vars[i],
            );
        }

        return .{
            .data = try self.buildCurriedCallFromLowered(callee, lowered_args, call_info.applied_result_tys),
            .result_ty = call_info.result_ty,
        };
    }

    fn maybeLowerNominalEqBinop(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        binop: CIR.Expr.Binop,
    ) std.mem.Allocator.Error!?ast.ExprId {
        if (binop.op != .eq and binop.op != .ne) return null;

        const operand_ty = try self.requireExprType(module_idx, type_scope, binop.lhs);
        switch (self.ctx.types.getTypePreservingNominal(operand_ty)) {
            .nominal => |_| {
                const method_name = self.ctx.typedCirModule(module_idx).commonIdents().is_eq;
                const receiver_var = self.requireExprResultVar(module_idx, type_scope, binop.lhs);
                const target = try self.resolveAttachedMethodTargetFromWorkspaceVar(
                    module_idx,
                    type_scope,
                    receiver_var,
                    method_name,
                ) orelse {
                    return null;
                };
                const source_fn_var = try self.copyTopLevelDefExprVarToScope(type_scope, target.module_idx, target.def_idx);
                const arg_exprs = [_]CIR.Expr.Idx{ binop.lhs, binop.rhs };

                var call_info = try self.prepareCallInfo(module_idx, type_scope, expr_idx, source_fn_var, &arg_exprs);
                defer call_info.deinit(self.allocator);

                const callee = try self.lowerResolvedTargetCallee(module_idx, call_info.call_scope, target, call_info.fn_ty, call_info.fn_var);

                const lowered_args = try self.allocator.alloc(ast.ExprId, arg_exprs.len);
                defer self.allocator.free(lowered_args);
                for (arg_exprs, 0..) |arg_expr_idx, i| {
                    lowered_args[i] = try self.lowerExprWithExpectedType(
                        module_idx,
                        call_info.call_scope,
                        env,
                        arg_expr_idx,
                        call_info.arg_tys[i],
                        call_info.arg_vars[i],
                    );
                }

                const eq_expr = try self.program.store.addExpr(.{
                    .ty = call_info.result_ty,
                    .data = .{ .call = try self.buildCurriedCallFromLowered(callee, lowered_args, call_info.applied_result_tys) },
                });

                if (binop.op == .ne) {
                    const bool_ty = try self.makePrimitiveType(.bool);
                    return try self.program.store.addExpr(.{
                        .ty = bool_ty,
                        .data = .{ .low_level = .{
                            .op = .bool_not,
                            .args = try self.program.store.addExprSpan(&.{eq_expr}),
                        } },
                    });
                }

                return eq_expr;
            },
            else => return null,
        }
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
        const fn_var = self.requireExprResultVar(module_idx, type_scope, func_expr_idx);
        var call_info = try self.prepareCallInfo(module_idx, type_scope, call_expr_idx, fn_var, arg_exprs);
        defer call_info.deinit(self.allocator);

        const callee = try self.lowerExprWithExpectedType(
            module_idx,
            call_info.call_scope,
            env,
            func_expr_idx,
            call_info.fn_ty,
            call_info.fn_var,
        );

        const lowered_args = try self.allocator.alloc(ast.ExprId, arg_exprs.len);
        defer self.allocator.free(lowered_args);
        for (arg_exprs, 0..) |arg_expr_idx, i| {
            lowered_args[i] = try self.lowerExprWithExpectedType(
                module_idx,
                call_info.call_scope,
                env,
                arg_expr_idx,
                call_info.arg_tys[i],
                call_info.arg_vars[i],
            );
        }
        return .{
            .data = try self.buildCurriedCallFromLowered(callee, lowered_args, call_info.applied_result_tys),
            .result_ty = call_info.result_ty,
        };
    }

    fn lowerDirectCallCallee(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        func_expr_idx: CIR.Expr.Idx,
        expected_fn_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!?ast.ExprId {
        const func_expr = self.ctx.typedCirModule(module_idx).expr(func_expr_idx).data;

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
                const symbol = try self.lookupOrSpecializeExternal(module_idx, type_scope, lookup, expected_fn_ty, null);
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
        return self.lowerSolvedExprWithExpectedType(
            module_idx,
            type_scope,
            env,
            self.ctx.typedCirModule(module_idx).expr(expr_idx),
            expected_ty,
            expected_var,
        );
    }

    fn lowerSolvedExprWithExpectedType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr: typed_cir.Expr,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!ast.ExprId {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const target_ty = blk: {
            const expected_kind = self.ctx.types.getType(expected_ty);
            if (expected_kind != .placeholder and expected_kind != .unbd) break :blk expected_ty;
            break :blk try self.requireExprType(module_idx, type_scope, expr.idx);
        };

        return switch (expr.data) {
            .e_nominal => |nominal| self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr.idx,
                nominal.backing_expr,
                target_ty,
                expected_var,
            ),
            .e_nominal_external => |nominal| self.lowerTransparentNominalExprWithType(
                module_idx,
                type_scope,
                env,
                expr.idx,
                nominal.backing_expr,
                target_ty,
                expected_var,
            ),
            .e_num => |num| blk: {
                break :blk try self.program.store.addExpr(.{
                    .ty = target_ty,
                    .data = try self.lowerNumericIntLiteralData(target_ty, num.value),
                });
            },
            .e_dec => |dec| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerNumericDecLiteralData(target_ty, dec.value.toI128()),
            }),
            .e_dec_small => |dec| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = try self.lowerNumericSmallDecLiteralData(target_ty, dec.value),
            }),
            .e_typed_int => |num| blk: {
                break :blk try self.program.store.addExpr(.{
                    .ty = target_ty,
                    .data = try self.lowerNumericIntLiteralData(target_ty, num.value),
                });
            },
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
                        expr.idx,
                        target_ty,
                    );
                }

                if (try self.maybeLowerSpecialCallExpr(module_idx, type_scope, env, expr.idx, call, target_ty)) |special| {
                    break :blk try self.program.store.addExpr(.{
                        .ty = self.program.store.getExpr(special).ty,
                        .data = self.program.store.getExpr(special).data,
                    });
                }
                const lowered_call = try self.lowerCurriedCall(
                    module_idx,
                    type_scope,
                    env,
                    expr.idx,
                    call.func,
                    typed_cir_module.sliceExpr(call.args),
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
                    expr.idx,
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
                    expr.idx,
                    target_ty,
                    closure,
                    expected_var,
                );
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered.ty,
                    .data = .{ .clos = lowered.data },
                });
            },
            .e_hosted_lambda => debugPanic("monotype invariant violated: hosted top-level proc escaped into monotype.lowerExprWithExpectedType", .{}),
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
                expected_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr),
                record,
            ),
            .e_tuple => |tuple| try self.program.store.addExpr(.{
                .ty = target_ty,
                .data = .{ .tuple = try self.lowerTupleExprsWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    target_ty,
                    expected_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr),
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
                    expected_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr),
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
                    expr.idx,
                    target_ty,
                    expected_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr),
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
                    expr.idx,
                    target_ty,
                    expected_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr),
                    tag.name,
                    .{ .span = .{ .start = 0, .len = 0 } },
                ),
            }),
            .e_lookup_local => |lookup| blk: {
                if (self.ctx.types.getType(target_ty) != .func) {
                    break :blk try self.lowerSolvedExpr(module_idx, type_scope, env, expr);
                }
                const symbol = try self.lookupOrSpecializeLocal(module_idx, type_scope, env, lookup.pattern_idx, target_ty, expected_var);
                break :blk try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse target_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_lookup_external => |lookup| blk: {
                if (self.ctx.types.getType(target_ty) != .func) {
                    break :blk try self.lowerSolvedExpr(module_idx, type_scope, env, expr);
                }
                const symbol = try self.lookupOrSpecializeExternal(module_idx, type_scope, lookup, target_ty, expected_var);
                break :blk try self.program.store.addExpr(.{
                    .ty = self.lookupKnownSymbolType(symbol) orelse target_ty,
                    .data = .{ .var_ = symbol },
                });
            },
            .e_dot_access => |dot| blk: {
                if (dot.args == null) {
                    break :blk try self.lowerSolvedExpr(module_idx, type_scope, env, expr);
                }
                const lowered_call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    expr.idx,
                    dot.field_name,
                ) orelse break :blk try self.makeRuntimeErrorExprAt(target_ty);
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
                    expr.idx,
                    dispatch.method_name,
                ) orelse break :blk try self.makeRuntimeErrorExprAt(target_ty);
                break :blk try self.program.store.addExpr(.{
                    .ty = lowered_call.result_ty,
                    .data = .{ .call = lowered_call.data },
                });
            },
            else => try self.lowerSolvedExpr(module_idx, type_scope, env, expr),
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
            type_scope,
            expected_var orelse try self.exprResultVar(module_idx, type_scope, env, expr_idx),
        );
        const backing_ty = if (self.ctx.types.getType(nominal_ty) != .placeholder)
            nominal_ty
        else
            try self.requireExprType(module_idx, type_scope, backing_expr_idx);
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
        type_scope: *const TypeCloneScope,
        source_var: ?Var,
    ) ?Var {
        const root = source_var orelse return null;
        const resolved = type_scope.typeStoreConst().resolveVar(root);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| self.resolveTransparentBackingVar(type_scope, type_scope.typeStoreConst().getNominalBackingVar(nominal)),
                else => root,
            },
            .alias => |alias| self.resolveTransparentBackingVar(type_scope, type_scope.typeStoreConst().getAliasBackingVar(alias)),
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
        return switch (self.ctx.typedCirModule(module_idx).expr(call.func).data) {
            .e_dot_access => |dot| if (dot.args != null) blk: {
                const lowered_call = try self.lowerRecordedMethodCall(
                    module_idx,
                    type_scope,
                    env,
                    call_expr_idx,
                    dot.field_name,
                ) orelse break :blk try self.makeRuntimeErrorExprAt(
                    try self.requireExprType(module_idx, type_scope, call_expr_idx),
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
                ) orelse break :blk try self.makeRuntimeErrorExprAt(
                    try self.requireExprType(module_idx, type_scope, call_expr_idx),
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
        _: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!?ast.ExprId {
        if (self.resolveDirectTopLevelSource(module_idx, env, call.func)) |source| {
            if (self.isBuiltinStrInspectSource(source.module_idx, source.def_idx)) {
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
        _: type_mod.TypeId,
        _: ?Var,
        span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const exprs = self.ctx.typedCirModule(module_idx).sliceExpr(span);
        const out = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(out);
        for (exprs, 0..) |expr_idx, i| {
            out[i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                try self.requireExprType(module_idx, type_scope, expr_idx),
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
        _: type_mod.TypeId,
        _: ?Var,
        span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const exprs = self.ctx.typedCirModule(module_idx).sliceExpr(span);
        const out = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(out);
        for (exprs, 0..) |expr_idx, i| {
            out[i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                expr_idx,
                try self.requireExprType(module_idx, type_scope, expr_idx),
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
        _: type_mod.TypeId,
        span: CIR.RecordField.Span,
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const fields = typed_cir_module.sliceRecordFields(span);
        const out = try self.allocator.alloc(ast.FieldExpr, fields.len);
        defer self.allocator.free(out);

        for (fields, 0..) |field_idx, i| {
            const field = typed_cir_module.getRecordField(field_idx);
            out[i] = .{
                .name = try self.ctx.copyExecutableIdent(module_idx, field.name),
                .value = try self.lowerExprWithExpectedType(
                    module_idx,
                    type_scope,
                    env,
                    field.value,
                    try self.requireExprType(module_idx, type_scope, field.value),
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
        _: ?Var,
        tag_name: base.Ident.Idx,
        args_span: CIR.Expr.Span,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        if (self.ctx.types.getType(expected_ty) == .primitive and self.ctx.types.getType(expected_ty).primitive == .bool) {
            if (self.ctx.typedCirModule(module_idx).sliceExpr(args_span).len != 0) {
                return debugPanic("monotype bool tag invariant violated: Bool tags cannot carry arguments", .{});
            }
            return .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag_name) };
        }

        const arg_exprs = self.ctx.typedCirModule(module_idx).sliceExpr(args_span);
        const lowered_args = try self.allocator.alloc(ast.ExprId, arg_exprs.len);
        defer self.allocator.free(lowered_args);
        for (arg_exprs, 0..) |arg_expr_idx, i| {
            lowered_args[i] = try self.lowerExprWithExpectedType(
                module_idx,
                type_scope,
                env,
                arg_expr_idx,
                try self.requireExprType(module_idx, type_scope, arg_expr_idx),
                try self.exprResultVar(module_idx, type_scope, env, arg_expr_idx),
            );
        }
        return .{ .tag = .{
            .name = try self.ctx.copyExecutableIdent(module_idx, tag_name),
            .discriminant = try self.requireExprTagDiscriminant(module_idx, type_scope, expr_idx),
            .args = try self.program.store.addExprSpan(lowered_args),
        } };
    }

    fn exprVarIsErroneous(self: *Lowerer, module_idx: u32, expr_idx: CIR.Expr.Idx) bool {
        return self.ctx.typedCirModule(module_idx).typeStoreConst().resolveVar(self.ctx.typedCirModule(module_idx).exprType(expr_idx)).desc.content == .err;
    }

    fn callRootCalleeIsRuntimeError(self: *const Lowerer, module_idx: u32, func_expr_idx: CIR.Expr.Idx) bool {
        var current = func_expr_idx;
        while (self.ctx.typedCirModule(module_idx).expr(current).data == .e_call) {
            current = self.ctx.typedCirModule(module_idx).expr(current).data.e_call.func;
        }
        return switch (self.ctx.typedCirModule(module_idx).expr(current).data) {
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
        const expr = self.ctx.typedCirModule(module_idx).expr(expr_idx).data;
        const out = switch (expr) {
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
        return out;
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

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const cir_stmts = typed_cir_module.sliceStatements(stmts_span);
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

        const out: @FieldType(ast.Expr.Data, "block") = .{
            .stmts = try self.program.store.addStmtSpan(lowered.items),
            .final_expr = try self.program.store.addExpr(.{
                .ty = result_ty,
                .data = .{ .runtime_error = try self.internStringLiteral("runtime error") },
            }),
        };
        return out;
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
                .ty = try self.ctx.types.addType(.{ .record = .{ .fields = &.{} } }),
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

    fn requireExprResultVar(
        _: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) Var {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        return type_scope.memo.expr_result_var_map.get(key) orelse
            debugPanic("monotype expr invariant violated: missing expr result var metadata", .{});
    }

    fn instantiateVarType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        var_: Var,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const resolved = type_scope.typeStoreConst().resolveVar(var_);
        return try self.lowerInstantiatedType(module_idx, type_scope, resolved.var_);
    }

    fn instantiateSourceVarType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        source_var: Var,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const scoped = try self.scopeVar(type_scope, module_idx, source_var);
        return try self.instantiateVarType(module_idx, type_scope, scoped);
    }

    fn recordExprType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        if (type_scope.memo.expr_type_cache.contains(key)) return;

        const lowered = try self.instantiateVarType(
            module_idx,
            type_scope,
            try self.scopeVar(type_scope, module_idx, self.ctx.typedCirModule(module_idx).exprType(expr_idx)),
        );
        const expr = self.ctx.typedCirModule(module_idx).expr(expr_idx).data;
        try type_scope.memo.expr_type_cache.put(
            key,
            try self.normalizeDefaultNumericLiteralType(module_idx, expr_idx, expr, lowered),
        );
    }

    fn finalizeExprTypes(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!void {
        var iter = type_scope.memo.collected_expr.keyIterator();
        while (iter.next()) |key_ptr| {
            if (type_scope.memo.expr_type_cache.contains(key_ptr.*)) continue;
            try self.recordExprType(module_idx, type_scope, key_ptr.expr_idx);
        }
    }

    fn recordPatternType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        if (type_scope.memo.pattern_type_cache.contains(key)) return;

        try type_scope.memo.pattern_type_cache.put(
            key,
            try self.instantiateVarType(
                module_idx,
                type_scope,
                try self.requirePatternSolvedVar(module_idx, type_scope, pattern_idx),
            ),
        );
    }

    fn finalizePatternTypes(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!void {
        var iter = type_scope.memo.collected_pattern.keyIterator();
        while (iter.next()) |key_ptr| {
            if (type_scope.memo.pattern_type_cache.contains(key_ptr.*)) continue;
            try self.recordPatternType(module_idx, type_scope, key_ptr.pattern_idx);
        }
    }

    fn collectPatternInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!void {
        return self.collectSolvedPatternInfo(module_idx, type_scope, self.ctx.typedCirModule(module_idx).pattern(pattern_idx));
    }

    fn collectSolvedPatternInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern: typed_cir.Pattern,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern.idx,
        };
        if (type_scope.memo.collected_pattern.contains(key)) return;

        try type_scope.memo.collected_pattern.put(key, {});
        const solved_module = self.ctx.typedCirModule(module_idx);
        switch (pattern.data) {
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
            .as => |as_pat| try self.collectSolvedPatternInfo(module_idx, type_scope, solved_module.pattern(as_pat.pattern)),
            .applied_tag => |tag| {
                for (solved_module.slicePatterns(tag.args)) |arg_pat| {
                    try self.collectSolvedPatternInfo(module_idx, type_scope, solved_module.pattern(arg_pat));
                }
            },
            .record_destructure => |record| {
                for (solved_module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    try self.collectSolvedPatternInfo(
                        module_idx,
                        type_scope,
                        solved_module.pattern(solved_module.getRecordDestruct(destruct_idx).kind.toPatternIdx()),
                    );
                }
            },
            .list => |list| {
                for (solved_module.slicePatterns(list.patterns)) |child_pat| {
                    try self.collectSolvedPatternInfo(module_idx, type_scope, solved_module.pattern(child_pat));
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pat| {
                        try self.collectSolvedPatternInfo(module_idx, type_scope, solved_module.pattern(rest_pat));
                    }
                }
            },
            .tuple => |tuple| {
                for (solved_module.slicePatterns(tuple.patterns)) |elem_pat| {
                    try self.collectSolvedPatternInfo(module_idx, type_scope, solved_module.pattern(elem_pat));
                }
            },
            .nominal => |nominal| try self.collectSolvedPatternInfo(module_idx, type_scope, solved_module.pattern(nominal.backing_pattern)),
            .nominal_external => |nominal| try self.collectSolvedPatternInfo(module_idx, type_scope, solved_module.pattern(nominal.backing_pattern)),
        }
    }

    fn recordPatternStructuralInfoFromSourceType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        source_ty: type_mod.TypeId,
        source_solved_var: ?Var,
    ) std.mem.Allocator.Error!void {
        var effective_source_ty = source_ty;
        if (source_solved_var) |var_| {
            const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
            const source_kind = self.ctx.types.getTypePreservingNominal(source_ty);
            const needs_solved_ty = switch (pattern) {
                .applied_tag => |tag| switch (source_kind) {
                    .tag_union => false,
                    .primitive => |prim| blk: {
                        if (prim != .bool) break :blk true;
                        const tag_args_len = self.ctx.typedCirModule(module_idx).slicePatterns(tag.args).len;
                        break :blk tag_args_len != 0;
                    },
                    else => true,
                },
                .record_destructure => source_kind != .record,
                .list => source_kind != .list,
                .tuple => source_kind != .tuple,
                else => false,
            };
            if (needs_solved_ty) {
                effective_source_ty = try self.instantiateVarType(
                    module_idx,
                    type_scope,
                    var_,
                );
            }
        }
        if (!self.ctx.types.isFullyResolved(effective_source_ty)) {
            return debugPanic(
                "monotype pattern invariant violated: structural pattern source type was not fully resolved before recording in module {d}",
                .{module_idx},
            );
        }
        try self.recordPatternSourceType(module_idx, type_scope, pattern_idx, effective_source_ty);

        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
            .as => |as_pat| try self.recordPatternStructuralInfoFromSourceType(
                module_idx,
                type_scope,
                as_pat.pattern,
                effective_source_ty,
                source_solved_var,
            ),
            .applied_tag => |tag| {
                if (self.ctx.types.getType(effective_source_ty) == .primitive and self.ctx.types.getType(effective_source_ty).primitive == .bool) {
                    if (typed_cir_module.slicePatterns(tag.args).len != 0) {
                        return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    return;
                }
                const expected_discriminant = try self.requireTagDiscriminantFromMonotype(
                    module_idx,
                    effective_source_ty,
                    tag.name,
                );
                try self.recordPatternTagDiscriminant(module_idx, type_scope, pattern_idx, expected_discriminant);
                const arg_patterns = typed_cir_module.slicePatterns(tag.args);
                const arg_tys = try self.requireTagPayloadTypesFromMonotype(
                    module_idx,
                    effective_source_ty,
                    tag.name,
                );
                if (arg_patterns.len != arg_tys.len) {
                    return debugPanic(
                        "monotype pattern invariant violated: tag pattern arity {d} did not match payload arity {d} in module {d}",
                        .{ arg_patterns.len, arg_tys.len, module_idx },
                    );
                }
                for (arg_patterns, arg_tys) |arg_pat, arg_ty| {
                    try self.recordPatternStructuralInfoFromSourceType(
                        module_idx,
                        type_scope,
                        arg_pat,
                        arg_ty,
                        try self.requirePatternSolvedVar(module_idx, type_scope, arg_pat),
                    );
                }
            },
            .record_destructure => |record| {
                for (typed_cir_module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = typed_cir_module.getRecordDestruct(destruct_idx);
                    const child_pattern_idx = destruct.kind.toPatternIdx();
                    try self.recordRecordDestructFieldIndex(
                        module_idx,
                        type_scope,
                        destruct_idx,
                        try self.requireRecordFieldIndexForSolvedVar(
                            module_idx,
                            type_scope,
                            source_solved_var orelse debugPanic(
                                "monotype pattern invariant violated: record destructure missing source solved var in module {d}",
                                .{module_idx},
                            ),
                            typed_cir_module.getIdent(destruct.label),
                        ),
                    );
                    const field_index = self.requireRecordDestructFieldIndex(module_idx, type_scope, destruct_idx);
                    try self.recordPatternStructuralInfoFromSourceType(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        self.requireRecordFieldTypeFromMonotype(effective_source_ty, field_index),
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                    );
                }
            },
            .list => |list| {
                const elem_ty = self.requireListElemTypeFromMonotype(effective_source_ty);
                try self.recordPatternListElemType(module_idx, type_scope, pattern_idx, elem_ty);
                for (typed_cir_module.slicePatterns(list.patterns)) |child_pattern_idx| {
                    try self.recordPatternStructuralInfoFromSourceType(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        elem_ty,
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                    );
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        try self.recordPatternStructuralInfoFromSourceType(
                            module_idx,
                            type_scope,
                            rest_pattern_idx,
                            effective_source_ty,
                            try self.requirePatternSolvedVar(module_idx, type_scope, rest_pattern_idx),
                        );
                    }
                }
            },
            .tuple => |tuple| {
                const elem_patterns = typed_cir_module.slicePatterns(tuple.patterns);
                if (self.ctx.types.getTypePreservingNominal(effective_source_ty) != .tuple) {
                    for (elem_patterns) |child_pattern_idx| {
                        const elem_solved_var = try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx);
                        const elem_ty = try self.instantiateVarType(module_idx, type_scope, elem_solved_var);
                        try self.recordPatternStructuralInfoFromSourceType(
                            module_idx,
                            type_scope,
                            child_pattern_idx,
                            elem_ty,
                            elem_solved_var,
                        );
                    }
                    return;
                }
                const elem_tys = self.requireTupleElemTypesFromMonotype(effective_source_ty);
                if (elem_patterns.len != elem_tys.len) {
                    return debugPanic(
                        "monotype pattern invariant violated: tuple pattern arity {d} did not match source type arity {d} in module {d}",
                        .{ elem_patterns.len, elem_tys.len, module_idx },
                    );
                }
                for (elem_patterns, elem_tys) |child_pattern_idx, elem_ty| {
                    try self.recordPatternStructuralInfoFromSourceType(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        elem_ty,
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                    );
                }
            },
            .nominal => |nominal| try self.recordPatternStructuralInfoFromSourceType(
                module_idx,
                type_scope,
                nominal.backing_pattern,
                effective_source_ty,
                source_solved_var,
            ),
            .nominal_external => |nominal| try self.recordPatternStructuralInfoFromSourceType(
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
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
                for (self.ctx.typedCirModule(module_idx).slicePatterns(tag.args)) |arg_pat| {
                    try self.bindPatternEnvWithSolvedVarOnly(
                        module_idx,
                        type_scope,
                        arg_pat,
                        try self.requirePatternSolvedVar(module_idx, type_scope, arg_pat),
                        env,
                    );
                }
            },
            .record_destructure => |record| {
                for (self.ctx.typedCirModule(module_idx).sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const child_pattern_idx = self.ctx.typedCirModule(module_idx).getRecordDestruct(destruct_idx).kind.toPatternIdx();
                    try self.bindPatternEnvWithSolvedVarOnly(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                        env,
                    );
                }
            },
            .tuple => |tuple| {
                for (self.ctx.typedCirModule(module_idx).slicePatterns(tuple.patterns)) |child_pattern_idx| {
                    try self.bindPatternEnvWithSolvedVarOnly(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                        env,
                    );
                }
            },
            .list => |list| {
                for (self.ctx.typedCirModule(module_idx).slicePatterns(list.patterns)) |child_pattern_idx| {
                    try self.bindPatternEnvWithSolvedVarOnly(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                        env,
                    );
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        try self.bindPatternEnvWithSolvedVarOnly(
                            module_idx,
                            type_scope,
                            rest_pattern_idx,
                            try self.requirePatternSolvedVar(module_idx, type_scope, rest_pattern_idx),
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
        try self.collectPatternInfo(module_idx, type_scope, pattern_idx);
        try self.bindPatternEnvWithSolvedVarOnly(
            module_idx,
            type_scope,
            pattern_idx,
            source_solved_var,
            env,
        );
    }

    fn collectStmtInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: *BindingEnv,
        stmt_idx: CIR.Statement.Idx,
    ) std.mem.Allocator.Error!void {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const stmt = typed_cir_module.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                try self.collectExprInfo(module_idx, type_scope, env.*, decl.expr);
                const body_var = self.requireExprResultVar(module_idx, type_scope, decl.expr);
                try self.collectPatternBindingsIntoEnvWithSolvedVar(
                    module_idx,
                    type_scope,
                    decl.pattern,
                    body_var,
                    env,
                );
            },
            .s_var => |decl| {
                try self.collectExprInfo(module_idx, type_scope, env.*, decl.expr);
                const body_var = self.requireExprResultVar(module_idx, type_scope, decl.expr);
                try self.collectPatternBindingsIntoEnvWithSolvedVar(
                    module_idx,
                    type_scope,
                    decl.pattern_idx,
                    body_var,
                    env,
                );
            },
            .s_reassign => |reassign| {
                try self.collectPatternInfo(module_idx, type_scope, reassign.pattern_idx);
                try self.collectExprInfo(module_idx, type_scope, env.*, reassign.expr);
            },
            .s_expr => |expr_stmt| try self.collectExprInfo(module_idx, type_scope, env.*, expr_stmt.expr),
            .s_dbg => |dbg| try self.collectExprInfo(module_idx, type_scope, env.*, dbg.expr),
            .s_expect => |expect| try self.collectExprInfo(module_idx, type_scope, env.*, expect.body),
            .s_crash => {},
            .s_return => |ret| {
                try self.collectExprInfo(module_idx, type_scope, env.*, ret.expr);
            },
            .s_break => {},
            .s_for => |for_stmt| {
                try self.collectExprInfo(module_idx, type_scope, env.*, for_stmt.expr);
                try self.collectPatternInfo(module_idx, type_scope, for_stmt.patt);
                const elem_var = self.lookupListElemSolvedVar(
                    module_idx,
                    type_scope,
                    self.requireExprResultVar(module_idx, type_scope, for_stmt.expr),
                ) orelse debugPanic(
                    "monotype pattern invariant violated: for statement missing iterable element var in module {d}",
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
                try self.collectExprInfo(module_idx, type_scope, body_env, for_stmt.body);
            },
            .s_while => |while_stmt| {
                try self.collectExprInfo(module_idx, type_scope, env.*, while_stmt.cond);
                try self.collectExprInfo(module_idx, type_scope, env.*, while_stmt.body);
            },
            else => {},
        }
    }

    fn collectExprInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!void {
        return self.collectSolvedExprInfo(module_idx, type_scope, env, self.ctx.typedCirModule(module_idx).expr(expr_idx));
    }

    fn collectSolvedExprInfo(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr: typed_cir.Expr,
    ) std.mem.Allocator.Error!void {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr.idx,
        };
        const typed_cir_module = expr.module();
        if (type_scope.memo.collected_expr.contains(key) and type_scope.memo.expr_result_var_map.contains(key)) return;
        const explicit_result_var = type_scope.memo.expr_result_var_map.get(key);
        const solved_module = self.ctx.typedCirModule(module_idx);

        var result_var: Var = switch (expr.data) {
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
            => explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr),
            .e_str => |str_expr| blk: {
                for (typed_cir_module.sliceExpr(str_expr.span)) |part| {
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(part));
                }
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_list => |list| blk: {
                const list_result_var = explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
                const elem_result_var = self.lookupListElemSolvedVar(module_idx, type_scope, list_result_var);
                for (typed_cir_module.sliceExpr(list.elems)) |elem| {
                    try self.collectExprInfoWithResultVar(
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
                for (typed_cir_module.sliceExpr(tuple.elems)) |elem| {
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(elem));
                }
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_call => |call| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(call.func));
                for (typed_cir_module.sliceExpr(call.args)) |arg_expr| {
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(arg_expr));
                }
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_record => |record| blk: {
                for (typed_cir_module.sliceRecordFields(record.fields)) |field_idx| {
                    const field = typed_cir_module.getRecordField(field_idx);
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(field.value));
                }
                if (record.ext) |ext_expr| {
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(ext_expr));
                }
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_block => |block| blk: {
                var body_env = try self.cloneEnv(env);
                defer body_env.deinit();
                const block_stmts = typed_cir_module.sliceStatements(block.stmts);
                var stmt_index: usize = 0;
                while (stmt_index < block_stmts.len) {
                    if (try self.seedLocalLambdaDeclGroupInfo(module_idx, type_scope, &body_env, block_stmts, stmt_index)) |group_end| {
                        stmt_index = group_end;
                        continue;
                    }
                    try self.collectStmtInfo(module_idx, type_scope, &body_env, block_stmts[stmt_index]);
                    stmt_index += 1;
                }
                try self.collectSolvedExprInfo(module_idx, type_scope, body_env, solved_module.expr(block.final_expr));
                break :blk explicit_result_var orelse self.requireExprResultVar(module_idx, type_scope, block.final_expr);
            },
            .e_tag => |tag| blk: {
                for (typed_cir_module.sliceExpr(tag.args)) |arg_expr| {
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(arg_expr));
                }
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_nominal => |nominal| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(nominal.backing_expr));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_nominal_external => |nominal| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(nominal.backing_expr));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_binop => |binop| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(binop.lhs));
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(binop.rhs));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_unary_minus => |unary| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(unary.expr));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_unary_not => |unary| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(unary.expr));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_dot_access => |dot| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(dot.receiver));
                if (dot.args) |dispatch_args| {
                    for (typed_cir_module.sliceExpr(dispatch_args)) |arg_expr| {
                        try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(arg_expr));
                    }
                }
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_type_var_dispatch => |dispatch| blk: {
                for (typed_cir_module.sliceExpr(dispatch.args)) |arg_expr| {
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(arg_expr));
                }
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_match => |match_expr| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(match_expr.cond));
                const cond_var = self.requireExprResultVar(module_idx, type_scope, match_expr.cond);
                const branches = typed_cir_module.matchBranchSlice(match_expr.branches);
                for (branches) |branch_idx| {
                    const branch = typed_cir_module.getMatchBranch(branch_idx);
                    var branch_env = try self.cloneEnv(env);
                    defer branch_env.deinit();
                    const branch_pattern_ids = typed_cir_module.sliceMatchBranchPatterns(branch.patterns);
                    if (branch_pattern_ids.len != 0) {
                        const first_pattern = typed_cir_module.getMatchBranchPattern(branch_pattern_ids[0]).pattern;
                        try self.collectPatternBindingsIntoEnvWithSolvedVar(
                            module_idx,
                            type_scope,
                            first_pattern,
                            cond_var,
                            &branch_env,
                        );
                    }
                    if (branch.guard) |guard_expr| {
                        try self.collectSolvedExprInfo(module_idx, type_scope, branch_env, solved_module.expr(guard_expr));
                    }
                    try self.collectSolvedExprInfo(module_idx, type_scope, branch_env, solved_module.expr(branch.value));
                }
                if (branches.len == 0) {
                    return debugPanic("monotype expr invariant violated: match expression missing branches", .{});
                }
                const first_branch = typed_cir_module.getMatchBranch(branches[0]);
                break :blk explicit_result_var orelse self.requireExprResultVar(module_idx, type_scope, first_branch.value);
            },
            .e_if => |if_expr| blk: {
                for (typed_cir_module.sliceIfBranches(if_expr.branches)) |branch_id| {
                    const branch = typed_cir_module.getIfBranch(branch_id);
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(branch.cond));
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(branch.body));
                }
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(if_expr.final_else));
                break :blk explicit_result_var orelse self.requireExprResultVar(module_idx, type_scope, if_expr.final_else);
            },
            .e_expect => |expect| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(expect.body));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_tuple_access => |access| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(access.tuple));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_dbg => |dbg| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(dbg.expr));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_return => |ret| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(ret.expr));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_for => |for_expr| blk: {
                try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(for_expr.expr));
                try self.collectPatternInfo(module_idx, type_scope, for_expr.patt);
                const elem_var = self.lookupListElemSolvedVar(
                    module_idx,
                    type_scope,
                    self.requireExprResultVar(module_idx, type_scope, for_expr.expr),
                ) orelse debugPanic(
                    "monotype pattern invariant violated: for expression missing iterable element var in module {d}",
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
                try self.collectSolvedExprInfo(module_idx, type_scope, body_env, solved_module.expr(for_expr.body));
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            .e_run_low_level => |ll| blk: {
                for (typed_cir_module.sliceExpr(ll.args)) |arg_expr| {
                    try self.collectSolvedExprInfo(module_idx, type_scope, env, solved_module.expr(arg_expr));
                }
                break :blk explicit_result_var orelse try self.scopedSolvedExprResultVar(type_scope, env, expr);
            },
            else => debugTodoExpr(expr.data),
        };

        if (type_scope.memo.expr_result_var_map.get(key)) |existing| {
            result_var = existing;
        }
        try type_scope.memo.expr_result_var_map.put(key, result_var);
        try type_scope.memo.collected_expr.put(key, {});
    }

    fn requireExprType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const key: TypeCloneScope.ExprKey = .{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        };
        return type_scope.memo.expr_type_cache.get(key) orelse
            debugPanic(
                "monotype expr invariant violated: missing expr type cache entry (module {d} expr {d} tag {s})",
                .{
                    module_idx,
                    @intFromEnum(expr_idx),
                    @tagName(self.ctx.typedCirModule(module_idx).expr(expr_idx).data),
                },
            );
    }

    fn requirePatternSolvedVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!Var {
        return try self.scopeVar(type_scope, module_idx, self.ctx.typedCirModule(module_idx).patternType(pattern_idx));
    }

    fn requirePatternType(
        _: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const key: TypeCloneScope.PatternTypeKey = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        };
        return type_scope.memo.pattern_type_cache.get(key) orelse
            debugPanic("monotype pattern invariant violated: missing pattern type cache entry", .{});
    }

    fn lowerExprType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        _: BindingEnv,
        expr_idx: CIR.Expr.Idx,
        _: CIR.Expr,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.requireExprType(module_idx, type_scope, expr_idx);
    }

    fn lookupTrySuffixPayloadType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        _: BindingEnv,
        match_expr: CIR.Expr.Match,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const branches = typed_cir_module.matchBranchSlice(match_expr.branches);
        if (branches.len == 0) return null;

        const ok_branch = typed_cir_module.getMatchBranch(branches[0]);
        const branch_patterns = typed_cir_module.sliceMatchBranchPatterns(ok_branch.patterns);
        if (branch_patterns.len == 0) return null;

        const branch_pattern = typed_cir_module.getMatchBranchPattern(branch_patterns[0]);
        const tag_info = self.lookupSingleTagPayloadPattern(module_idx, branch_pattern.pattern) orelse return null;
        const cond_ty = try self.requireExprType(module_idx, type_scope, match_expr.cond);
        const payload_tys = try self.requireTagPayloadTypesFromMonotype(module_idx, cond_ty, tag_info.tag_name);
        if (payload_tys.len != 1) {
            return debugPanic(
                "monotype try-suffix invariant violated: expected exactly one payload type for single-payload tag pattern",
                .{},
            );
        }
        return payload_tys[0];
    }

    fn lookupSingleTagPayloadPattern(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?struct { tag_name: base.Ident.Idx, payload_pattern: CIR.Pattern.Idx } {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
        return switch (pattern) {
            .applied_tag => |tag| blk: {
                const args = typed_cir_module.slicePatterns(tag.args);
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
            .unbd => {
                return self.makePrimitiveType(.dec);
            },
            .tag_union => |tag_union| {
                if (tag_union.tags.len == 0) {
                    return self.makePrimitiveType(.dec);
                }
            },
            else => {},
        }

        const resolved = self.ctx.typedCirModule(module_idx).typeStoreConst().resolveVar(self.ctx.typedCirModule(module_idx).exprType(expr_idx));
        if (resolved.desc.content == .flex) {
            const constraints = self.ctx.typedCirModule(module_idx).typeStoreConst().sliceStaticDispatchConstraints(
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
        const resolved = type_scope.typeStoreConst().resolveVar(var_);
        if (self.defaultNumeralPrimitiveForContent(type_scope, resolved.desc.content)) |prim| {
            return self.makePrimitiveType(prim);
        }
        const key: TypeCloneScope.TypeKey = .{ .module_idx = module_idx, .var_ = resolved.var_ };
        if (type_scope.active_type_cache.get(key)) |active| return active;
        if (type_scope.type_cache.get(key)) |cached| return cached;
        if (type_scope.provisional_type_cache.get(key)) |provisional| {
            if (self.ctx.types.isFullyResolved(provisional)) {
                const canonical = try self.ctx.types.internTypeId(provisional);
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
                    const arg_slice = type_scope.typeStoreConst().sliceVars(func.args);
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
                .empty_record => .{ .record = .{ .fields = &.{} } },
                .empty_tag_union => .{ .tag_union = .{ .tags = &.{} } },
                .tuple => |tuple| blk: {
                    const elems = type_scope.typeStoreConst().sliceVars(tuple.elems);
                    const lowered_elems = try self.allocator.alloc(type_mod.TypeId, elems.len);
                    defer self.allocator.free(lowered_elems);
                    for (elems, 0..) |elem_var, i| {
                        lowered_elems[i] = try self.lowerInstantiatedType(module_idx, type_scope, elem_var);
                    }
                    break :blk .{ .tuple = try self.ctx.types.dupeTypeIds(lowered_elems) };
                },
                .nominal_type => |nominal| try self.lowerNominalType(module_idx, type_scope, placeholder, nominal),
            },
            .alias => |alias| .{ .link = try self.lowerInstantiatedType(module_idx, type_scope, type_scope.typeStoreConst().getAliasBackingVar(alias)) },
            .flex => |flex| blk: {
                if (try self.defaultPrimitiveForConstraints(module_idx, type_scope, flex.constraints)) |prim| {
                    break :blk .{ .primitive = prim };
                }
                break :blk .placeholder;
            },
            .rigid => |rigid| blk: {
                if (try self.defaultPrimitiveForConstraints(module_idx, type_scope, rigid.constraints)) |prim| {
                    break :blk .{ .primitive = prim };
                }
                break :blk .placeholder;
            },
            .err => .placeholder,
        };

        self.ctx.types.setType(placeholder, lowered);
        if (lowered == .placeholder) {
            _ = type_scope.active_type_cache.remove(key);
            try type_scope.provisional_type_cache.put(key, placeholder);
            return placeholder;
        }

        _ = type_scope.active_type_cache.remove(key);
        if (self.ctx.types.isFullyResolved(placeholder)) {
            const canonical = try self.ctx.types.internTypeId(placeholder);
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
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        var lowered_tags = std.ArrayList(type_mod.Tag).empty;
        defer lowered_tags.deinit(self.allocator);

        var next_tags = initial_tags;
        var next_ext = initial_ext;
        while (true) {
            const tags_slice = type_scope.typeStoreConst().getTagsSlice(next_tags);
            try lowered_tags.ensureUnusedCapacity(self.allocator, next_tags.len());
            for (0..tags_slice.len) |i| {
                const tag_name = tags_slice.items(.name)[i];
                const tag_args = type_scope.typeStoreConst().sliceVars(tags_slice.items(.args)[i]);
                const arg_ids = try self.allocator.alloc(type_mod.TypeId, tag_args.len);
                defer self.allocator.free(arg_ids);
                for (tag_args, 0..) |arg_var, j| {
                    arg_ids[j] = try self.lowerInstantiatedType(module_idx, type_scope, arg_var);
                }
                lowered_tags.appendAssumeCapacity(.{
                    .name = try self.ctx.copyExecutableIdentFromStore(type_scope.identStoreConst(), tag_name),
                    .args = try self.ctx.types.dupeTypeIds(arg_ids),
                });
            }

            const resolved_ext = type_scope.typeStoreConst().resolveVar(next_ext);
            switch (resolved_ext.desc.content) {
                .alias => |alias| {
                    next_ext = type_scope.typeStoreConst().getAliasBackingVar(alias);
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
                    self.debugPanicUnresolvedTypeVar(module_idx, type_scope, resolved_ext.var_);
                },
                .rigid => |rigid| {
                    if (rigid.constraints.len() == 0) break;
                    self.debugPanicUnresolvedTypeVar(module_idx, type_scope, resolved_ext.var_);
                },
                .err => return .placeholder,
            }
        }

        if (isBuiltinBoolTagUnionSlice(typed_cir_module, &self.ctx.idents, lowered_tags.items)) {
            return .{ .primitive = .bool };
        }

        std.mem.sort(type_mod.Tag, lowered_tags.items, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: type_mod.Tag, b: type_mod.Tag) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);
        self.assertDistinctSortedLoweredTags(lowered_tags.items);

        return .{ .tag_union = .{
            .tags = try self.ctx.types.dupeTags(lowered_tags.items),
        } };
    }

    fn assertDistinctSortedLoweredTags(
        self: *Lowerer,
        tags: []const type_mod.Tag,
    ) void {
        if (tags.len <= 1) return;

        var prev = tags[0];

        for (tags[1..]) |tag| {
            if (tag.name != prev.name) {
                prev = tag;
                continue;
            }

            const prev_args = prev.args;
            const tag_args = tag.args;
            if (prev_args.len != tag_args.len) {
                debugPanic("monotype invariant violated: duplicate tag constructors had different arity after row flattening", .{});
            }
            for (prev_args, tag_args) |prev_arg, tag_arg| {
                if (prev_arg != tag_arg) {
                    debugPanic("monotype invariant violated: duplicate tag constructors had different payload types after row flattening", .{});
                }
            }
            debugPanic("monotype invariant violated: duplicate tag constructor reached monotype row flattening", .{});
        }
    }

    fn lowerRecordContent(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        initial_fields: types.RecordField.SafeMultiList.Range,
        initial_ext: Var,
    ) std.mem.Allocator.Error!type_mod.Content {
        var lowered_fields = std.ArrayList(type_mod.Field).empty;
        defer lowered_fields.deinit(self.allocator);

        var next_fields = initial_fields;
        var next_ext = initial_ext;
        while (true) {
            const fields_slice = type_scope.typeStoreConst().getRecordFieldsSlice(next_fields);
            try lowered_fields.ensureUnusedCapacity(self.allocator, next_fields.len());
            for (0..fields_slice.len) |i| {
                lowered_fields.appendAssumeCapacity(.{
                    .name = try self.ctx.copyExecutableIdentFromStore(type_scope.identStoreConst(), fields_slice.items(.name)[i]),
                    .ty = try self.lowerInstantiatedType(module_idx, type_scope, fields_slice.items(.var_)[i]),
                });
            }

            const resolved_ext = type_scope.typeStoreConst().resolveVar(next_ext);
            switch (resolved_ext.desc.content) {
                .alias => |alias| {
                    next_ext = type_scope.typeStoreConst().getAliasBackingVar(alias);
                },
                .structure => |flat| switch (flat) {
                    .record => |record| {
                        next_fields = record.fields;
                        next_ext = record.ext;
                    },
                    .record_unbound => |fields| {
                        next_fields = fields;
                        const ext_fields_slice = type_scope.typeStoreConst().getRecordFieldsSlice(next_fields);
                        try lowered_fields.ensureUnusedCapacity(self.allocator, next_fields.len());
                        for (0..ext_fields_slice.len) |i| {
                            lowered_fields.appendAssumeCapacity(.{
                                .name = try self.ctx.copyExecutableIdentFromStore(type_scope.identStoreConst(), ext_fields_slice.items(.name)[i]),
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
                    self.debugPanicUnresolvedTypeVar(module_idx, type_scope, resolved_ext.var_);
                },
                .rigid => |rigid| {
                    if (rigid.constraints.len() == 0) break;
                    self.debugPanicUnresolvedTypeVar(module_idx, type_scope, resolved_ext.var_);
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
            .fields = try self.ctx.types.dupeFields(lowered_fields.items),
        } };
    }

    fn lowerRecordUnboundContent(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        fields: types.RecordField.SafeMultiList.Range,
    ) std.mem.Allocator.Error!type_mod.Content {
        const fields_slice = type_scope.typeStoreConst().getRecordFieldsSlice(fields);
        const lowered_fields = try self.allocator.alloc(type_mod.Field, fields_slice.len);
        defer self.allocator.free(lowered_fields);
        for (0..fields_slice.len) |i| {
            lowered_fields[i] = .{
                .name = try self.ctx.copyExecutableIdentFromStore(type_scope.identStoreConst(), fields_slice.items(.name)[i]),
                .ty = try self.lowerInstantiatedType(module_idx, type_scope, fields_slice.items(.var_)[i]),
            };
        }
        std.mem.sort(type_mod.Field, lowered_fields, &self.ctx.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
                return std.mem.order(u8, idents.getText(a.name), idents.getText(b.name)) == .lt;
            }
        }.lessThan);
        return .{ .record = .{
            .fields = try self.ctx.types.dupeFields(lowered_fields),
        } };
    }

    fn lowerNominalType(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        nominal_type_id: type_mod.TypeId,
        nominal: types.NominalType,
    ) std.mem.Allocator.Error!type_mod.Content {
        const defining = try self.resolveNominalDefiningIdentity(type_scope, nominal);
        const defining_ident = defining.ident;
        const defining_module = defining.module;

        if (defining_ident.eql(defining_module.commonIdents().str) or defining_ident.eql(defining_module.commonIdents().builtin_str)) {
            return .{ .primitive = .str };
        }
        if (defining_ident.eql(defining_module.commonIdents().list)) {
            const args = type_scope.typeStoreConst().sliceNominalArgs(nominal);
            if (args.len != 1) debugPanic("monotype.lowerNominalType List expected one type argument", .{});
            return .{ .list = try self.lowerInstantiatedType(module_idx, type_scope, args[0]) };
        }
        if (defining_ident.eql(defining_module.commonIdents().box)) {
            const args = type_scope.typeStoreConst().sliceNominalArgs(nominal);
            if (args.len != 1) debugPanic("monotype.lowerNominalType Box expected one type argument", .{});
            return .{ .box = try self.lowerInstantiatedType(module_idx, type_scope, args[0]) };
        }
        if (defining_ident.eql(defining_module.commonIdents().bool_type)) return .{ .primitive = .bool };
        if (builtinNumPrim(defining_module, defining_ident)) |prim| return .{ .primitive = prim };

        const nominal_args = type_scope.typeStoreConst().sliceNominalArgs(nominal);
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

        const backing_var = type_scope.typeStoreConst().getNominalBackingVar(nominal);
        const to_inspect_symbol = blk: {
            const target = defining.module.resolveAttachedMethodTargetByText(
                defining.module.getIdent(defining_ident),
                "to_inspect",
            ) orelse break :blk symbol_mod.Symbol.none;
            const symbol = self.lookupTopLevelDefSymbol(target.module_idx, target.def_idx) orelse debugPanic(
                "monotype.lowerNominalType missing symbol for resolved to_inspect method",
                .{},
            );
            try self.forced_runtime_symbols.put(symbol, {});
            break :blk symbol;
        };
        return .{ .nominal = .{
            .module_idx = defining.module_idx,
            .ident = try self.ctx.copyExecutableIdent(defining.module_idx, defining_ident),
            .is_opaque = nominal.is_opaque,
            .to_inspect_symbol = to_inspect_symbol,
            .args = try self.ctx.types.dupeTypeIds(lowered_args),
            .backing = try self.lowerInstantiatedType(module_idx, type_scope, backing_var),
        } };
    }

    const NominalDefiningIdentity = struct {
        module_idx: u32,
        module: Ctx.Module,
        ident: base.Ident.Idx,
    };

    fn resolveNominalDefiningIdentity(
        self: *const Lowerer,
        type_scope: *const TypeCloneScope,
        nominal: types.NominalType,
    ) std.mem.Allocator.Error!NominalDefiningIdentity {
        const defining_module_idx = self.ctx.findModuleIdxByName(type_scope.getIdent(nominal.origin_module));
        const defining_module = self.ctx.typedCirModule(defining_module_idx);
        const defining_ident = defining_module.findCommonIdent(type_scope.getIdent(nominal.ident.ident_idx)) orelse debugPanic(
            "monotype.lowerNominalType missing target ident in defining module",
            .{},
        );
        return .{
            .module_idx = defining_module_idx,
            .module = defining_module,
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
        _: *Lowerer,
        type_scope: *const TypeCloneScope,
        content: types.Content,
    ) ?type_mod.Prim {
        return switch (content) {
            .flex => |flex| if (flex.name) |name|
                builtinNumPrimInStore(type_scope.identStoreConst(), name) orelse
                    (if (constraintsContainFromNumeral(type_scope.typeStoreConst(), flex.constraints)) .dec else null)
            else if (constraintsContainFromNumeral(type_scope.typeStoreConst(), flex.constraints)) .dec else null,
            .rigid => |rigid| builtinNumPrimInStore(type_scope.identStoreConst(), rigid.name) orelse
                (if (constraintsContainFromNumeral(type_scope.typeStoreConst(), rigid.constraints)) .dec else null),
            else => null,
        };
    }

    fn defaultPrimitiveForConstraints(
        _: *Lowerer,
        _: u32,
        type_scope: *const TypeCloneScope,
        constraints: types.StaticDispatchConstraint.SafeList.Range,
    ) std.mem.Allocator.Error!?type_mod.Prim {
        if (constraints.len() == 0) return null;

        if (constraintsContainFromNumeral(type_scope.typeStoreConst(), constraints)) {
            return .dec;
        }

        return null;
    }

    fn debugPanicUnresolvedTypeVar(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        var_: Var,
    ) noreturn {
        const source_module = self.ctx.typedCirModule(module_idx);
        const resolved = type_scope.typeStoreConst().resolveVar(var_);
        switch (resolved.desc.content) {
            .flex => |flex| {
                if (comptime builtin.target.os.tag != .freestanding) {
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
                    for (type_scope.typeStoreConst().sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                        std.debug.print(
                            "  constraint fn={s} origin={s}\n",
                            .{
                                type_scope.getIdent(constraint.fn_name),
                                @tagName(constraint.origin),
                            },
                        );
                    }
                }
            },
            .rigid => |rigid| {
                if (comptime builtin.target.os.tag != .freestanding) {
                    std.debug.print(
                        "UNRESOLVED rigid var={d} root={d} module={d} name={s} constraints_len={d}\n",
                        .{
                            @intFromEnum(var_),
                            @intFromEnum(resolved.var_),
                            module_idx,
                            type_scope.getIdent(rigid.name),
                            rigid.constraints.len(),
                        },
                    );
                    for (type_scope.typeStoreConst().sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                        std.debug.print(
                            "  constraint fn={s} origin={s}\n",
                            .{
                                type_scope.getIdent(constraint.fn_name),
                                @tagName(constraint.origin),
                            },
                        );
                    }
                }
            },
            else => {},
        }
        const raw: u32 = @intFromEnum(var_);
        if (raw < source_module.nodeCount()) {
            const node_idx: CIR.Node.Idx = @enumFromInt(raw);
            const node_tag = source_module.nodeTag(node_idx);
            const region = source_module.regionAt(node_idx);
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
        _: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        arg_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        return switch (self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data) {
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
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
        return switch (pattern) {
            .assign, .as, .underscore, .record_destructure, .tuple, .applied_tag => true,
            .nominal => |nominal| self.patternCanCollectStructuralBindings(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| self.patternCanCollectStructuralBindings(module_idx, nominal.backing_pattern),
            else => false,
        };
    }

    fn patternNeedsAnyExplicitMatch(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!bool {
        return self.patternNeedsPredicateDesugaring(module_idx, pattern_idx) or
            !try self.patternIsIrrefutableStructural(module_idx, type_scope, pattern_idx);
    }

    fn makePatternSourceBind(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        const ty = try self.requirePatternType(module_idx, type_scope, pattern_idx);
        return self.makePatternSourceBindWithType(module_idx, pattern_idx, ty);
    }

    fn makePatternSourceBindWithType(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
        try self.recordPatternStructuralInfoFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source.ty,
            source_solved_var,
        );
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
                const source_is_record = self.ctx.types.getType(source.ty) == .record;
                const source_expr = if (source_is_record)
                    try self.makeVarExpr(source.ty, source.symbol)
                else
                    null;
                for (typed_cir_module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = typed_cir_module.getRecordDestruct(destruct_idx);
                    const child_pattern_idx = destruct.kind.toPatternIdx();
                    const field_ty = self.requirePatternSourceType(module_idx, type_scope, child_pattern_idx);
                    const field_solved_var = try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx);
                    const field_bind: ast.TypedSymbol = .{
                        .ty = field_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    const field_expr = if (source_is_record)
                        try self.program.store.addExpr(.{
                            .ty = field_ty,
                            .data = .{ .access = .{
                                .record = source_expr.?,
                                .field = try self.ctx.copyExecutableIdent(module_idx, destruct.label),
                                .field_index = self.requireRecordDestructFieldIndex(module_idx, type_scope, destruct_idx),
                            } },
                        })
                    else
                        try self.makeRuntimeErrorExprAt(field_ty);
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
                const elem_patterns = typed_cir_module.slicePatterns(tuple.patterns);
                const source_is_tuple = self.ctx.types.getType(source.ty) == .tuple;
                const source_expr = if (source_is_tuple)
                    try self.makeVarExpr(source.ty, source.symbol)
                else
                    null;
                for (elem_patterns, 0..) |elem_pattern_idx, i| {
                    const elem_ty = self.requirePatternSourceType(module_idx, type_scope, elem_pattern_idx);
                    const elem_bind: ast.TypedSymbol = .{
                        .ty = elem_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    try decls.append(self.allocator, .{
                        .bind = elem_bind,
                        .body = if (source_is_tuple)
                            try self.makeTupleAccessExpr(source_expr.?, elem_ty, i)
                        else
                            try self.makeRuntimeErrorExprAt(elem_ty),
                    });
                    if (!self.patternCanCollectStructuralBindings(module_idx, elem_pattern_idx)) continue;
                    try self.collectStructuralBindingDeclsWithSolvedVar(
                        module_idx,
                        type_scope,
                        elem_bind,
                        try self.requirePatternSolvedVar(module_idx, type_scope, elem_pattern_idx),
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
            .applied_tag => |tag| {
                if (self.ctx.types.getType(source.ty) == .primitive and self.ctx.types.getType(source.ty).primitive == .bool) {
                    if (typed_cir_module.slicePatterns(tag.args).len != 0) {
                        return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    return;
                }
                const arg_patterns = typed_cir_module.slicePatterns(tag.args);
                const arg_tys = try self.requireTagPayloadTypesFromMonotype(
                    module_idx,
                    source.ty,
                    tag.name,
                );
                if (arg_patterns.len != arg_tys.len) {
                    return debugPanic(
                        "monotype pattern invariant violated: tag pattern arity {d} did not match payload arity {d} in module {d}",
                        .{ arg_patterns.len, arg_tys.len, module_idx },
                    );
                }
                const source_expr = try self.makeVarExpr(source.ty, source.symbol);
                for (arg_patterns, arg_tys, 0..) |arg_pattern_idx, arg_ty, i| {
                    const payload_expr = try self.makeMatchedTagPayloadExpr(
                        module_idx,
                        type_scope,
                        source_expr,
                        source.ty,
                        pattern_idx,
                        tag,
                        i,
                        arg_ty,
                    );
                    const arg_bind: ast.TypedSymbol = .{
                        .ty = arg_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    try decls.append(self.allocator, .{
                        .bind = arg_bind,
                        .body = payload_expr,
                    });
                    if (!self.patternCanCollectStructuralBindings(module_idx, arg_pattern_idx)) continue;
                    try self.collectStructuralBindingDeclsWithSolvedVar(
                        module_idx,
                        type_scope,
                        arg_bind,
                        try self.requirePatternSolvedVar(module_idx, type_scope, arg_pattern_idx),
                        arg_pattern_idx,
                        env,
                        decls,
                    );
                }
            },
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
        try self.recordPatternStructuralInfoFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
        switch (pattern) {
            .assign => try self.appendPatternBindingOrReassign(module_idx, pattern_idx, source_ty, source_expr, env, lowered),
            .as => |as_pat| {
                try self.appendPatternBindingOrReassign(module_idx, pattern_idx, source_ty, source_expr, env, lowered);
                try self.lowerPatternReassignFromExpr(module_idx, type_scope, as_pat.pattern, source_expr, source_ty, source_solved_var, env, lowered);
            },
            .underscore => {},
            .record_destructure => |record| {
                for (typed_cir_module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = typed_cir_module.getRecordDestruct(destruct_idx);
                    const child_pattern_idx = destruct.kind.toPatternIdx();
                    const field_ty = self.requirePatternSourceType(module_idx, type_scope, child_pattern_idx);
                    const field_bind: ast.TypedSymbol = .{
                        .ty = field_ty,
                        .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
                    };
                    const field_expr = try self.program.store.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .access = .{
                            .record = source_expr,
                            .field = try self.ctx.copyExecutableIdent(module_idx, destruct.label),
                            .field_index = self.requireRecordDestructFieldIndex(module_idx, type_scope, destruct_idx),
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
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                        env,
                        lowered,
                    );
                }
            },
            .tuple => |tuple| {
                const elem_patterns = typed_cir_module.slicePatterns(tuple.patterns);
                for (elem_patterns, 0..) |elem_pattern_idx, i| {
                    const elem_ty = self.requirePatternSourceType(module_idx, type_scope, elem_pattern_idx);
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
                        try self.requirePatternSolvedVar(module_idx, type_scope, elem_pattern_idx),
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
        try self.recordPatternStructuralInfoFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const effective_source_ty = self.requirePatternSourceType(module_idx, type_scope, pattern_idx);
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
        try self.recordPatternStructuralInfoFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const effective_source_ty = self.requirePatternSourceType(module_idx, type_scope, pattern_idx);
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;

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
                    if (typed_cir_module.slicePatterns(tag.args).len != 0) {
                        return debugPanic("monotype bool pattern invariant violated: Bool tags cannot carry arguments", .{});
                    }
                    break :blk try self.program.store.addPat(.{
                        .ty = effective_source_ty,
                        .data = .{ .bool_lit = self.requireBoolLiteralValue(module_idx, tag.name) },
                    });
                }
                const arg_pats = typed_cir_module.slicePatterns(tag.args);
                const lowered_args = try self.allocator.alloc(ast.PatId, arg_pats.len);
                defer self.allocator.free(lowered_args);
                for (arg_pats, 0..) |arg_pat, i| {
                    lowered_args[i] = try self.lowerMatchPatWithType(
                        module_idx,
                        type_scope,
                        arg_pat,
                        self.requirePatternSourceType(module_idx, type_scope, arg_pat),
                        try self.requirePatternSolvedVar(module_idx, type_scope, arg_pat),
                        env,
                        decls,
                    );
                }
                break :blk try self.program.store.addPat(.{
                    .ty = effective_source_ty,
                    .data = .{ .tag = .{
                        .name = try self.ctx.copyExecutableIdent(module_idx, tag.name),
                        .discriminant = self.requirePatternTagDiscriminant(module_idx, type_scope, pattern_idx),
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
            .nominal => |nominal| if (child_ty == nominal.backing) parent_ty else child_ty,
            else => child_ty,
        };
    }

    fn lookupListElemSolvedVar(
        self: *const Lowerer,
        module_idx: u32,
        type_scope: *const TypeCloneScope,
        source_solved_var: ?Var,
    ) ?Var {
        const root = source_solved_var orelse return null;
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        const resolved = type_scope.typeStoreConst().resolveVar(root);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| blk: {
                    if (!std.mem.eql(
                        u8,
                        type_scope.getIdent(nominal.ident.ident_idx),
                        typed_cir_module.getIdent(typed_cir_module.commonIdents().list),
                    )) {
                        break :blk self.lookupListElemSolvedVar(module_idx, type_scope, type_scope.typeStoreConst().getNominalBackingVar(nominal));
                    }
                    const args = type_scope.typeStoreConst().sliceNominalArgs(nominal);
                    if (args.len != 1) break :blk null;
                    break :blk args[0];
                },
                else => null,
            },
            .alias => |alias| self.lookupListElemSolvedVar(module_idx, type_scope, type_scope.typeStoreConst().getAliasBackingVar(alias)),
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
                "monotype pattern invariant violated: expected frozen list source type, found non-list type {d}",
                .{@intFromEnum(list_ty)},
            ),
        };
    }

    fn requireTupleElemTypesFromMonotype(
        self: *const Lowerer,
        tuple_ty: type_mod.TypeId,
    ) []const type_mod.TypeId {
        return switch (self.ctx.types.getType(tuple_ty)) {
            .tuple => |elems| elems,
            else => debugPanic(
                "monotype pattern invariant violated: expected frozen tuple source type, found non-tuple type {d}",
                .{@intFromEnum(tuple_ty)},
            ),
        };
    }

    fn requireTagPayloadTypesFromMonotype(
        self: *const Lowerer,
        module_idx: u32,
        union_ty: type_mod.TypeId,
        tag_name: base.Ident.Idx,
    ) std.mem.Allocator.Error![]const type_mod.TypeId {
        const source_tag_name = self.ctx.typedCirModule(module_idx).getIdent(tag_name);
        return switch (self.ctx.types.getType(union_ty)) {
            .tag_union => |tag_union| blk: {
                const tags = tag_union.tags;
                for (tags) |tag| {
                    if (std.mem.eql(u8, self.ctx.idents.getText(tag.name), source_tag_name)) {
                        break :blk tag.args;
                    }
                }
                debugPanic(
                    "monotype pattern invariant violated: frozen tag union source type missing payload tag",
                    .{},
                );
            },
            else => debugPanic(
                "monotype pattern invariant violated: expected frozen tag union source type, found non-tag-union type {d}",
                .{@intFromEnum(union_ty)},
            ),
        };
    }

    fn requireTagDiscriminantFromMonotype(
        self: *const Lowerer,
        module_idx: u32,
        union_ty: type_mod.TypeId,
        tag_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!u16 {
        const source_tag_name = self.ctx.typedCirModule(module_idx).getIdent(tag_name);
        return switch (self.ctx.types.getType(union_ty)) {
            .tag_union => |tag_union| blk: {
                const tags = tag_union.tags;
                for (tags, 0..) |tag, i| {
                    if (std.mem.eql(u8, self.ctx.idents.getText(tag.name), source_tag_name)) {
                        break :blk @intCast(i);
                    }
                }
                debugPanic(
                    "monotype invariant violated: frozen tag union type missing tag discriminant",
                    .{},
                );
            },
            else => debugPanic(
                "monotype invariant violated: expected frozen tag union type for discriminant, found non-tag-union type {d}",
                .{@intFromEnum(union_ty)},
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
                const fields = record.fields;
                self.assertSortedRecordFields(fields);
                if (field_index >= fields.len) {
                    debugPanic(
                        "monotype pattern invariant violated: frozen record source type missing field index {d}",
                        .{field_index},
                    );
                }
                break :blk fields[field_index].ty;
            },
            else => debugPanic(
                "monotype pattern invariant violated: expected frozen record source type, found non-record type {d}",
                .{@intFromEnum(record_ty)},
            ),
        };
    }

    fn requireRecordFieldIndexFromMonotypeType(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        field_name_text: []const u8,
    ) u16 {
        const record = switch (self.ctx.types.getType(record_ty)) {
            .record => |record| record,
            else => debugPanic("monotype invariant violated: expected record type for field index lookup", .{}),
        };
        const fields = record.fields;
        self.assertSortedRecordFields(fields);
        for (fields, 0..) |field, i| {
            if (std.mem.eql(u8, self.ctx.idents.getText(field.name), field_name_text)) {
                return @intCast(i);
            }
        }
        debugPanic("monotype invariant violated: missing record field index in monotype type", .{});
    }

    fn assertSortedRecordFields(
        self: *const Lowerer,
        fields: []const type_mod.Field,
    ) void {
        if (fields.len <= 1) return;
        var prev = fields[0];
        for (fields[1..]) |field| {
            switch (std.mem.order(
                u8,
                self.ctx.idents.getText(prev.name),
                self.ctx.idents.getText(field.name),
            )) {
                .lt => prev = field,
                .eq => debugPanic("monotype invariant violated: duplicate frozen record field reached field lookup", .{}),
                .gt => debugPanic("monotype invariant violated: frozen record fields were not pre-sorted", .{}),
            }
        }
    }

    fn requireRecordFieldIndexForSolvedVar(
        self: *Lowerer,
        _: u32,
        type_scope: *const TypeCloneScope,
        record_var: Var,
        field_name_text: []const u8,
    ) std.mem.Allocator.Error!u16 {
        var names = std.ArrayList(base.Ident.Idx).empty;
        defer names.deinit(self.allocator);

        var pending = std.ArrayList(Var).empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, record_var);

        while (pending.pop()) |current| {
            const resolved = type_scope.typeStoreConst().resolveVar(current);
            switch (resolved.desc.content) {
                .structure => |flat| switch (flat) {
                    .nominal_type => |nominal| try pending.append(self.allocator, type_scope.typeStoreConst().getNominalBackingVar(nominal)),
                    .record => |record| {
                        const fields = type_scope.typeStoreConst().getRecordFieldsSlice(record.fields);
                        try names.ensureUnusedCapacity(self.allocator, fields.len);
                        for (fields.items(.name)) |name| {
                            names.appendAssumeCapacity(name);
                        }
                        try pending.append(self.allocator, record.ext);
                    },
                    .record_unbound => |fields_range| {
                        const fields = type_scope.typeStoreConst().getRecordFieldsSlice(fields_range);
                        try names.ensureUnusedCapacity(self.allocator, fields.len);
                        for (fields.items(.name)) |name| {
                            names.appendAssumeCapacity(name);
                        }
                    },
                    .empty_record => {},
                    else => debugPanic("monotype invariant violated: attempted to read field index from non-record solved var", .{}),
                },
                .alias => |alias| try pending.append(self.allocator, type_scope.typeStoreConst().getAliasBackingVar(alias)),
                .flex => |flex| {
                    if (flex.constraints.len() == 0) continue;
                    debugPanic("monotype invariant violated: attempted to read field index from constrained flex var", .{});
                },
                .rigid => |rigid| {
                    if (rigid.constraints.len() == 0) continue;
                    debugPanic("monotype invariant violated: attempted to read field index from constrained rigid var", .{});
                },
                .err => debugPanic("monotype invariant violated: attempted to read field index from error solved var", .{}),
            }
        }

        std.mem.sort(base.Ident.Idx, names.items, type_scope.identStoreConst(), struct {
            fn lessThan(idents: *const base.Ident.Store, a: base.Ident.Idx, b: base.Ident.Idx) bool {
                return std.mem.order(u8, idents.getText(a), idents.getText(b)) == .lt;
            }
        }.lessThan);

        var write_index: usize = 0;
        var prev: ?base.Ident.Idx = null;
        for (names.items) |name| {
            if (prev) |prev_name| {
                if (std.mem.eql(u8, type_scope.getIdent(name), type_scope.getIdent(prev_name))) {
                    debugPanic("monotype invariant violated: duplicate record field after solved row flattening", .{});
                }
            }
            names.items[write_index] = name;
            write_index += 1;
            prev = name;
        }

        for (names.items[0..write_index], 0..) |name, i| {
            if (std.mem.eql(u8, type_scope.getIdent(name), field_name_text)) return @intCast(i);
        }

        debugPanic("monotype invariant violated: missing record field index in solved type", .{});
    }

    fn bindPatternEnv(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        pattern_idx: CIR.Pattern.Idx,
        env: *BindingEnv,
    ) std.mem.Allocator.Error!void {
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
                for (self.ctx.typedCirModule(module_idx).slicePatterns(tag.args)) |arg_pat| {
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
        try self.recordPatternStructuralInfoFromSourceType(
            module_idx,
            type_scope,
            pattern_idx,
            source_ty,
            source_solved_var,
        );
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
                for (self.ctx.typedCirModule(module_idx).slicePatterns(tag.args)) |arg_pat| {
                    try self.bindPatternEnvFromTypeWithSolvedVar(
                        module_idx,
                        type_scope,
                        arg_pat,
                        self.requirePatternSourceType(module_idx, type_scope, arg_pat),
                        try self.requirePatternSolvedVar(module_idx, type_scope, arg_pat),
                        env,
                    );
                }
            },
            .record_destructure => |record| {
                for (self.ctx.typedCirModule(module_idx).sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const child_pattern_idx = self.ctx.typedCirModule(module_idx).getRecordDestruct(destruct_idx).kind.toPatternIdx();
                    try self.bindPatternEnvFromTypeWithSolvedVar(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        self.requirePatternSourceType(module_idx, type_scope, child_pattern_idx),
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                        env,
                    );
                }
            },
            .tuple => |tuple| {
                for (self.ctx.typedCirModule(module_idx).slicePatterns(tuple.patterns)) |child_pattern_idx| {
                    try self.bindPatternEnvFromTypeWithSolvedVar(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        self.requirePatternSourceType(module_idx, type_scope, child_pattern_idx),
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                        env,
                    );
                }
            },
            .list => |list| {
                for (self.ctx.typedCirModule(module_idx).slicePatterns(list.patterns)) |child_pattern_idx| {
                    try self.bindPatternEnvFromTypeWithSolvedVar(
                        module_idx,
                        type_scope,
                        child_pattern_idx,
                        self.requirePatternSourceType(module_idx, type_scope, child_pattern_idx),
                        try self.requirePatternSolvedVar(module_idx, type_scope, child_pattern_idx),
                        env,
                    );
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        try self.bindPatternEnvFromTypeWithSolvedVar(
                            module_idx,
                            type_scope,
                            rest_pattern_idx,
                            self.requirePatternSourceType(module_idx, type_scope, rest_pattern_idx),
                            try self.requirePatternSolvedVar(module_idx, type_scope, rest_pattern_idx),
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
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
        return switch (pattern) {
            .assign => |assign| .{
                .ty = try self.requirePatternType(module_idx, type_scope, pattern_idx),
                .symbol = try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            },
            .as => |as_pat| .{
                .ty = try self.requirePatternType(module_idx, type_scope, pattern_idx),
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
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
        return switch (pattern) {
            .assign => |assign| try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, assign.ident),
            .as => |as_pat| try self.ctx.getOrCreatePatternSymbol(module_idx, pattern_idx, as_pat.ident),
            .nominal => |nominal| try self.requirePatternSymbolOnly(module_idx, nominal.backing_pattern),
            .nominal_external => |nominal| try self.requirePatternSymbolOnly(module_idx, nominal.backing_pattern),
            .underscore => try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
            else => debugTodoPattern(pattern),
        };
    }

    fn requirePatternBinderWithType(
        self: *Lowerer,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.TypedSymbol {
        const pattern = self.ctx.typedCirModule(module_idx).pattern(pattern_idx).data;
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
            .underscore => .{
                .ty = ty,
                .symbol = try self.ctx.addSyntheticSymbol(base.Ident.Idx.NONE),
            },
            else => debugTodoPattern(pattern),
        };
    }

    fn specializeLocalFnSource(
        self: *Lowerer,
        source_ref: LocalFnSourceRef,
        caller_scope: *const TypeCloneScope,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const group = self.local_fn_groups.items[source_ref.group_index];
        const expected_arg_tys = try self.collectCurriedArgTypes(expected_ty);
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
        try type_scope.initCloneAll(self.allocator, self.ctx.typedCirModule(group.module_idx));
        defer type_scope.deinit();
        var declaration_env = try self.materializeFrozenBindingEnv(&type_scope, group.declaration_env);
        defer declaration_env.deinit();
        const scoped_expected_var = if (expected_var) |var_| blk: {
            const frozen = try self.freezeCheckerVarFromScope(caller_scope, var_);
            defer {
                var owned = frozen;
                owned.deinit(self.allocator);
            }
            break :blk try self.materializeFrozenCheckerVar(&type_scope, frozen);
        } else null;
        const letfn = try self.lowerLambdaLikeDefWithEnv(
            group.module_idx,
            &type_scope,
            declaration_env,
            specialized_symbol,
            source.expr_idx,
            false,
            source.seed_var,
            scoped_expected_var,
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
        _: *Lowerer,
        left: []const type_mod.TypeId,
        right: []const type_mod.TypeId,
    ) bool {
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
        if (env.get(.{ .module_idx = module_idx, .pattern_idx = @intFromEnum(pattern_idx) })) |entry| {
            if (entry.local_fn_source) |source| {
                return try self.specializeLocalFnSource(source, type_scope, expected_ty, expected_var);
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
                return try self.specializeTopLevelFromCallSite(
                    type_scope,
                    top_level_symbol,
                    top_level,
                    expected_ty,
                    expected_var orelse debugPanic(
                        "monotype specialization invariant violated: missing exact checker seed for local top-level specialization {d}",
                        .{top_level_symbol.raw()},
                    ),
                );
            }
            try self.ensureTopLevelValueDefEmitted(top_level_symbol);
        }

        return top_level_symbol;
    }

    fn lookupFunctionArgVarInStore(
        self: *const Lowerer,
        store: *const types.Store,
        fn_var: Var,
        arg_index: usize,
    ) ?Var {
        const resolved = store.resolveVar(fn_var);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                    const args = store.sliceVars(func.args);
                    if (arg_index >= args.len) break :blk null;
                    break :blk args[arg_index];
                },
                else => null,
            },
            .alias => |alias| self.lookupFunctionArgVarInStore(store, store.getAliasBackingVar(alias), arg_index),
            else => null,
        };
    }

    fn lookupCurriedFunctionArgVarInStore(
        self: *const Lowerer,
        store: *const types.Store,
        fn_var: ?Var,
        arg_index: usize,
    ) ?Var {
        const function_var = fn_var orelse return null;
        if (self.lookupFunctionArgVarInStore(store, function_var, 0) == null) {
            return null;
        }

        var remaining = arg_index;
        var current = function_var;
        while (true) {
            const resolved = store.resolveVar(current);
            const func = switch (resolved.desc.content) {
                .structure => |flat| switch (flat) {
                    .fn_pure, .fn_effectful, .fn_unbound => |fn_info| fn_info,
                    else => return null,
                },
                .alias => |alias| {
                    current = store.getAliasBackingVar(alias);
                    continue;
                },
                else => return null,
            };
            const args = store.sliceVars(func.args);
            if (remaining < args.len) return args[remaining];
            remaining -= args.len;
            const ret_var = func.ret;
            if (self.lookupFunctionArgVarInStore(store, ret_var, 0) == null) return null;
            current = ret_var;
        }
    }

    fn lookupCurriedFunctionResultVarInStore(
        self: *const Lowerer,
        store: *const types.Store,
        fn_var: ?Var,
    ) ?Var {
        const function_var = fn_var orelse return null;
        var current = function_var;
        while (true) {
            const ret_var = self.lookupFunctionRetVarInStore(store, current) orelse return null;
            if (self.lookupFunctionArgVarInStore(store, ret_var, 0) == null) return ret_var;
            current = ret_var;
        }
    }

    fn lookupSpecializedExprVar(
        self: *const Lowerer,
        module_idx: u32,
        env: BindingEnv,
        expr_idx: CIR.Expr.Idx,
    ) ?Var {
        const expr = self.ctx.typedCirModule(module_idx).expr(expr_idx).data;
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
            try self.scopeVar(type_scope, module_idx, self.ctx.typedCirModule(module_idx).exprType(expr_idx));
    }

    fn scopedSolvedExprResultVar(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
        expr: typed_cir.Expr,
    ) std.mem.Allocator.Error!Var {
        return self.lookupSpecializedExprVar(expr.module().module_idx, env, expr.idx) orelse
            try self.scopeVar(type_scope, expr.module().module_idx, expr.ty());
    }

    fn sourceTypeKey(
        self: *const Lowerer,
        source_module_idx: u32,
        source_var: Var,
    ) TypeCloneScope.TypeKey {
        const resolved = self.ctx.typedCirModule(source_module_idx).typeStoreConst().resolveVar(source_var);
        return .{
            .module_idx = source_module_idx,
            .var_ = resolved.var_,
        };
    }

    fn scopeVar(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        source_module_idx: u32,
        source_var: Var,
    ) std.mem.Allocator.Error!Var {
        const key = self.sourceTypeKey(source_module_idx, source_var);
        if (type_scope.source_var_map.get(key)) |scoped| return scoped;
        const source_module = self.ctx.typedCirModule(source_module_idx);
        const copied_root = try type_clone_source.copyVarFromModule(
            source_module_idx,
            source_module.typeStoreConst(),
            type_scope.typeStoreMut(),
            source_var,
            &type_scope.copied_source_var_map,
            source_module.identStoreConst(),
            type_scope.identStoreMut(),
            self.allocator,
        );
        const scoped_root = type_scope.instantiation_var_map.get(copied_root) orelse
            try type_scope.instantiator.instantiateVar(copied_root);
        try self.putOrAssertSourceVarMapping(type_scope, key, scoped_root);
        return scoped_root;
    }

    fn putOrAssertSourceVarMapping(
        _: *Lowerer,
        type_scope: *TypeCloneScope,
        key: TypeCloneScope.TypeKey,
        scoped_var: Var,
    ) std.mem.Allocator.Error!void {
        if (type_scope.source_var_map.get(key)) |existing| {
            if (existing != scoped_var) {
                debugPanic(
                    "monotype specialization invariant violated: conflicting scoped source var mapping for module {d}",
                    .{key.module_idx},
                );
            }
            return;
        }
        try type_scope.source_var_map.put(key, scoped_var);
    }

    fn formatVarTextFromStore(
        self: *Lowerer,
        store: *const types.Store,
        idents: *const base.Ident.Store,
        var_: Var,
    ) std.mem.Allocator.Error![]u8 {
        var writer = try types.TypeWriter.initFromParts(
            self.allocator,
            store,
            idents,
            null,
        );
        defer writer.deinit();
        writer.setDefaultNumeralsToDec(true);
        const text = try writer.writeGet(var_, .one_line);
        return self.allocator.dupe(u8, text);
    }

    fn formatTagUnionText(
        self: *Lowerer,
        store: *const types.Store,
        idents: *const base.Ident.Store,
        tags: types.Tag.SafeMultiList.Range,
        ext: Var,
    ) std.mem.Allocator.Error![]u8 {
        var buf = try std.array_list.Managed(u8).initCapacity(self.allocator, 64);
        defer buf.deinit();
        const writer = buf.writer();
        try writer.writeAll("[");
        const tag_slice = store.getTagsSlice(tags);
        for (tag_slice.items(.name), tag_slice.items(.args), 0..) |tag_name, args_range, tag_index| {
            if (tag_index != 0) try writer.writeAll(", ");
            try writer.writeAll(idents.getText(tag_name));
            const args = store.sliceVars(args_range);
            if (args.len != 0) {
                try writer.writeAll("(");
                for (args, 0..) |arg_var, arg_index| {
                    if (arg_index != 0) try writer.writeAll(", ");
                    const arg_text = try self.formatVarTextFromStore(store, idents, arg_var);
                    defer self.allocator.free(arg_text);
                    try writer.writeAll(arg_text);
                }
                try writer.writeAll(")");
            }
        }
        try writer.writeAll("]");
        switch (store.resolveVar(ext).desc.content) {
            .structure => |flat| switch (flat) {
                .empty_tag_union => {},
                else => {
                    const ext_text = try self.formatVarTextFromStore(store, idents, ext);
                    defer self.allocator.free(ext_text);
                    try writer.print(" + {s}", .{ext_text});
                },
            },
            else => {
                const ext_text = try self.formatVarTextFromStore(store, idents, ext);
                defer self.allocator.free(ext_text);
                try writer.print(" + {s}", .{ext_text});
            },
        }
        return buf.toOwnedSlice();
    }

    fn gatherFlattenedTagUnion(
        self: *Lowerer,
        store: *const types.Store,
        tags: types.Tag.SafeMultiList.Range,
        initial_ext: Var,
        out: *std.ArrayList(types.Tag),
    ) std.mem.Allocator.Error!Var {
        const tag_slice = store.getTagsSlice(tags);
        for (tag_slice.items(.name), tag_slice.items(.args)) |name, args| {
            try out.append(self.allocator, .{ .name = name, .args = args });
        }

        var ext = initial_ext;
        var iteration_count: usize = 0;
        while (true) : (iteration_count += 1) {
            if (iteration_count > 1024) {
                debugPanic(
                    "monotype specialization invariant violated: tag union extension chain did not terminate",
                    .{},
                );
            }

            const resolved = store.resolveVar(ext);
            switch (resolved.desc.content) {
                .alias => |alias| {
                    ext = store.getAliasBackingVar(alias);
                },
                .structure => |flat| switch (flat) {
                    .tag_union => |ext_tags| {
                        const ext_slice = store.getTagsSlice(ext_tags.tags);
                        for (ext_slice.items(.name), ext_slice.items(.args)) |name, args| {
                            try out.append(self.allocator, .{ .name = name, .args = args });
                        }
                        ext = ext_tags.ext;
                    },
                    else => return resolved.var_,
                },
                else => return resolved.var_,
            }
        }
    }

    fn materializeWorkspaceTagUnionResidualVar(
        _: *Lowerer,
        residual_tags: []const types.Tag,
        residual_ext: Var,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!Var {
        const workspace_store = type_scope.typeStoreMut();
        const content = try workspace_store.mkTagUnion(residual_tags, residual_ext);
        return try workspace_store.freshFromContentWithRank(content, types.Rank.generalized);
    }

    fn materializeWorkspaceRecordResidualVar(
        _: *Lowerer,
        residual_fields: []const types.RecordField,
        terminal: FlattenedRecordTerminal,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!Var {
        const workspace_store = type_scope.typeStoreMut();

        const content: types.Content = switch (terminal) {
            .unbound => .{ .structure = .{
                .record_unbound = try workspace_store.appendRecordFields(residual_fields),
            } },
            .empty_record, .var_ => blk: {
                const ext_var = switch (terminal) {
                    .empty_record => try workspace_store.freshFromContentWithRank(
                        .{ .structure = .empty_record },
                        types.Rank.generalized,
                    ),
                    .var_ => |var_| var_,
                    .unbound => unreachable,
                };
                break :blk .{ .structure = .{
                    .record = .{
                        .fields = try workspace_store.appendRecordFields(residual_fields),
                        .ext = ext_var,
                    },
                } };
            },
        };

        return try workspace_store.freshFromContentWithRank(content, types.Rank.generalized);
    }

    fn flattenedRecordTerminalAsVar(
        _: *Lowerer,
        terminal: FlattenedRecordTerminal,
        type_scope: *TypeCloneScope,
    ) std.mem.Allocator.Error!Var {
        const workspace_store = type_scope.typeStoreMut();
        return switch (terminal) {
            .var_ => |var_| var_,
            .empty_record => try workspace_store.freshFromContentWithRank(
                .{ .structure = .empty_record },
                types.Rank.generalized,
            ),
            .unbound => try workspace_store.freshFromContentWithRank(
                .{ .structure = .{
                    .record_unbound = try workspace_store.appendRecordFields(&.{}),
                } },
                types.Rank.generalized,
            ),
        };
    }

    const FlattenedRecordTerminal = union(enum) {
        empty_record,
        unbound,
        var_: Var,
    };

    fn alignFlattenedRecordTerminal(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        canonical_terminal: FlattenedRecordTerminal,
        aligned_terminal: FlattenedRecordTerminal,
    ) std.mem.Allocator.Error!void {
        switch (canonical_terminal) {
            .empty_record => switch (aligned_terminal) {
                .empty_record, .unbound => {},
                .var_ => |aligned_var| {
                    const resolved = type_scope.typeStoreConst().resolveVar(aligned_var);
                    switch (resolved.desc.content) {
                        .flex, .rigid, .err => {
                            try type_scope.typeStoreMut().dangerousSetVarDesc(resolved.var_, .{
                                .content = .{ .structure = .empty_record },
                                .rank = types.Rank.generalized,
                            });
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: empty-record/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    }
                },
            },
            .unbound => switch (aligned_terminal) {
                .unbound => {},
                .empty_record => debugPanic(
                    "monotype specialization invariant violated: open-record/content mismatch while aligning workspace vars",
                    .{},
                ),
                .var_ => |aligned_var| {
                    const resolved = type_scope.typeStoreConst().resolveVar(aligned_var);
                    switch (resolved.desc.content) {
                        .flex, .rigid, .err => {
                            const empty_fields = try type_scope.typeStoreMut().appendRecordFields(&.{});
                            try type_scope.typeStoreMut().dangerousSetVarDesc(resolved.var_, .{
                                .content = .{ .structure = .{ .record_unbound = empty_fields } },
                                .rank = types.Rank.generalized,
                            });
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: open-record/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    }
                },
            },
            .var_ => |canonical_var| switch (aligned_terminal) {
                .var_ => |aligned_var| try self.alignWorkspaceVarToCanonical(type_scope, canonical_var, aligned_var),
                .empty_record => {
                    const resolved = type_scope.typeStoreConst().resolveVar(canonical_var);
                    switch (resolved.desc.content) {
                        .flex, .rigid, .err => {
                            try type_scope.typeStoreMut().dangerousSetVarDesc(resolved.var_, .{
                                .content = .{ .structure = .empty_record },
                                .rank = types.Rank.generalized,
                            });
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: record extension/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    }
                },
                .unbound => {
                    const resolved = type_scope.typeStoreConst().resolveVar(canonical_var);
                    switch (resolved.desc.content) {
                        .flex, .rigid, .err => {
                            const empty_fields = try type_scope.typeStoreMut().appendRecordFields(&.{});
                            try type_scope.typeStoreMut().dangerousSetVarDesc(resolved.var_, .{
                                .content = .{ .structure = .{ .record_unbound = empty_fields } },
                                .rank = types.Rank.generalized,
                            });
                        },
                        else => debugPanic(
                            "monotype specialization invariant violated: record extension/content mismatch while aligning workspace vars",
                            .{},
                        ),
                    }
                },
            },
        }
    }

    fn gatherFlattenedRecordFields(
        self: *Lowerer,
        store: *const types.Store,
        fields: types.RecordField.SafeMultiList.Range,
        initial_ext: Var,
        out: *std.ArrayList(types.RecordField),
    ) std.mem.Allocator.Error!FlattenedRecordTerminal {
        const field_slice = store.getRecordFieldsSlice(fields);
        for (field_slice.items(.name), field_slice.items(.var_)) |name, var_| {
            try out.append(self.allocator, .{ .name = name, .var_ = var_ });
        }

        var ext = initial_ext;
        var iteration_count: usize = 0;
        while (true) : (iteration_count += 1) {
            if (iteration_count > 1024) {
                debugPanic(
                    "monotype specialization invariant violated: record extension chain did not terminate",
                    .{},
                );
            }

            const resolved = store.resolveVar(ext);
            switch (resolved.desc.content) {
                .alias => |alias| {
                    ext = store.getAliasBackingVar(alias);
                },
                .structure => |flat| switch (flat) {
                    .record => |ext_record| {
                        const ext_slice = store.getRecordFieldsSlice(ext_record.fields);
                        for (ext_slice.items(.name), ext_slice.items(.var_)) |name, var_| {
                            try out.append(self.allocator, .{ .name = name, .var_ = var_ });
                        }
                        ext = ext_record.ext;
                    },
                    .record_unbound => |ext_fields| {
                        const ext_slice = store.getRecordFieldsSlice(ext_fields);
                        for (ext_slice.items(.name), ext_slice.items(.var_)) |name, var_| {
                            try out.append(self.allocator, .{ .name = name, .var_ = var_ });
                        }
                        return .unbound;
                    },
                    .empty_record => return .empty_record,
                    else => return .{ .var_ = resolved.var_ },
                },
                else => return .{ .var_ = resolved.var_ },
            }
        }
    }

    fn gatherFlattenedRecordContent(
        self: *Lowerer,
        store: *const types.Store,
        _: Var,
        content: types.Content,
        out: *std.ArrayList(types.RecordField),
    ) std.mem.Allocator.Error!FlattenedRecordTerminal {
        return switch (content) {
            .structure => |flat| switch (flat) {
                .record => |record| self.gatherFlattenedRecordFields(store, record.fields, record.ext, out),
                .record_unbound => |fields| blk: {
                    const field_slice = store.getRecordFieldsSlice(fields);
                    for (field_slice.items(.name), field_slice.items(.var_)) |name, var_| {
                        try out.append(self.allocator, .{ .name = name, .var_ = var_ });
                    }
                    break :blk .unbound;
                },
                .empty_record => .empty_record,
                else => debugPanic(
                    "monotype specialization invariant violated: attempted to flatten non-record workspace content as record",
                    .{},
                ),
            },
            else => debugPanic(
                "monotype specialization invariant violated: attempted to flatten non-structural workspace content as record",
                .{},
            ),
        };
    }

    const WorkspaceDispatchReceiverIdentity = struct {
        module_idx: u32,
        type_name: []const u8,
    };

    fn resolveWorkspaceDispatchReceiverIdentity(
        self: *const Lowerer,
        type_scope: *const TypeCloneScope,
        receiver_var: Var,
    ) ?WorkspaceDispatchReceiverIdentity {
        const resolved = type_scope.typeStoreConst().resolveVar(receiver_var);
        return switch (resolved.desc.content) {
            .alias => |alias| .{
                .module_idx = self.ctx.findModuleIdxByName(type_scope.getIdent(alias.origin_module)),
                .type_name = type_scope.getIdent(alias.ident.ident_idx),
            },
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| .{
                    .module_idx = self.ctx.findModuleIdxByName(type_scope.getIdent(nominal.origin_module)),
                    .type_name = type_scope.getIdent(nominal.ident.ident_idx),
                },
                else => null,
            },
            else => null,
        };
    }

    fn resolveAttachedMethodTargetFromWorkspaceVar(
        self: *Lowerer,
        source_module_idx: u32,
        type_scope: *TypeCloneScope,
        receiver_var: Var,
        method_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!?ResolvedTarget {
        const source_module = self.ctx.typedCirModule(source_module_idx);
        const method_name_text = source_module.getIdent(method_name);
        var receiver_writer = try types.TypeWriter.initFromParts(
            self.allocator,
            type_scope.typeStoreConst(),
            type_scope.identStoreConst(),
            null,
        );
        defer receiver_writer.deinit();
        receiver_writer.setDefaultNumeralsToDec(true);
        const receiver_text = try receiver_writer.writeGet(receiver_var, .one_line);
        const receiver_copy = try self.allocator.dupe(u8, receiver_text);
        defer self.allocator.free(receiver_copy);
        const receiver = self.resolveWorkspaceDispatchReceiverIdentity(type_scope, receiver_var) orelse {
            return null;
        };
        const receiver_module = self.ctx.typedCirModule(receiver.module_idx);
        const resolved = receiver_module.resolveAttachedMethodTargetByText(
            receiver.type_name,
            method_name_text,
        ) orelse {
            return null;
        };
        return .{
            .module_idx = resolved.module_idx,
            .def_idx = resolved.def_idx,
        };
    }

    fn resolveAttachedMethodTargetFromSourceVar(
        self: *Lowerer,
        source_module_idx: u32,
        source_var: Var,
        method_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!?ResolvedTarget {
        const source_module = self.ctx.typedCirModule(source_module_idx);
        const method_name_text = source_module.getIdent(method_name);
        const receiver = self.resolveSourceDispatchReceiverIdentity(source_module_idx, source_var) orelse return null;
        const receiver_module = self.ctx.typedCirModule(receiver.module_idx);
        const resolved = receiver_module.resolveAttachedMethodTargetByText(
            receiver.type_name,
            method_name_text,
        ) orelse return null;
        return .{
            .module_idx = resolved.module_idx,
            .def_idx = resolved.def_idx,
        };
    }

    fn resolveSourceDispatchReceiverIdentity(
        self: *const Lowerer,
        source_module_idx: u32,
        source_var: Var,
    ) ?WorkspaceDispatchReceiverIdentity {
        const source_module = self.ctx.typedCirModule(source_module_idx);
        const source_store = source_module.typeStoreConst();
        const resolved = source_store.resolveVar(source_var);
        return switch (resolved.desc.content) {
            .alias => |alias| .{
                .module_idx = self.ctx.findModuleIdxByName(source_module.getIdent(alias.origin_module)),
                .type_name = source_module.getIdent(alias.ident.ident_idx),
            },
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| .{
                    .module_idx = self.ctx.findModuleIdxByName(source_module.getIdent(nominal.origin_module)),
                    .type_name = source_module.getIdent(nominal.ident.ident_idx),
                },
                else => null,
            },
            else => null,
        };
    }


    fn lookupFunctionRetVarInStore(
        self: *const Lowerer,
        store: *const types.Store,
        fn_var: ?Var,
    ) ?Var {
        const var_ = fn_var orelse return null;
        const resolved = store.resolveVar(var_);
        return switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| func.ret,
                else => null,
            },
            .alias => |alias| self.lookupFunctionRetVarInStore(store, store.getAliasBackingVar(alias)),
            else => null,
        };
    }

    fn assertWorkspaceVarTypesEqual(
        self: *Lowerer,
        module_idx: u32,
        type_scope: *TypeCloneScope,
        actual_var: Var,
        expected_var: Var,
        comptime context: []const u8,
    ) std.mem.Allocator.Error!void {
        const actual_ty = try self.instantiateVarType(module_idx, type_scope, actual_var);
        const expected_ty = try self.instantiateVarType(module_idx, type_scope, expected_var);
        if (!self.ctx.types.equalIds(actual_ty, expected_ty)) {
            var writer = try types.TypeWriter.initFromParts(
                self.allocator,
                type_scope.typeStoreConst(),
                type_scope.identStoreConst(),
                null,
            );
            defer writer.deinit();
            writer.setDefaultNumeralsToDec(true);
            const actual_text = try writer.writeGet(actual_var, .one_line);
            const actual_copy = try self.allocator.dupe(u8, actual_text);
            defer self.allocator.free(actual_copy);
            writer.reset();
            writer.setDefaultNumeralsToDec(true);
            const expected_text = try writer.writeGet(expected_var, .one_line);
            const expected_copy = try self.allocator.dupe(u8, expected_text);
            defer self.allocator.free(expected_copy);
            debugPanic(
                "monotype invariant violated: " ++ context ++ " type mismatch in module {d}; actual var {d} => {s}; expected var {d} => {s}",
                .{ module_idx, @intFromEnum(actual_var), actual_copy, @intFromEnum(expected_var), expected_copy },
            );
        }
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
        if (self.lookupFunctionArgVarInStore(type_scope.typeStoreConst(), function_var, 0) == null) {
            if (arg_index == 0) return self.makeUnitType();
            return debugPanic(
                "monotype invariant violated: missing function arg {d} in module {d}",
                .{ arg_index, module_idx },
            );
        }
        const arg_var = self.lookupCurriedFunctionArgVarInStore(type_scope.typeStoreConst(), function_var, arg_index) orelse
            debugPanic("monotype invariant violated: missing function arg {d} in module {d}", .{ arg_index, module_idx });
        return try self.instantiateVarType(module_idx, type_scope, arg_var);
    }

    fn functionArgCountInStore(
        self: *const Lowerer,
        store: *const types.Store,
        fn_var: Var,
    ) usize {
        if (self.lookupFunctionArgVarInStore(store, fn_var, 0) == null) {
            if (self.lookupFunctionRetVarInStore(store, fn_var) != null) return 1;
            debugPanic(
                "monotype invariant violated: attempted to read function arity from non-function content",
                .{},
            );
        }
        var count: usize = 0;
        var current = fn_var;
        while (true) {
            const resolved = store.resolveVar(current);
            const func = switch (resolved.desc.content) {
                .structure => |flat| switch (flat) {
                    .fn_pure, .fn_effectful, .fn_unbound => |fn_info| fn_info,
                    else => return count,
                },
                .alias => |alias| {
                    current = store.getAliasBackingVar(alias);
                    continue;
                },
                else => return count,
            };
            const args = store.sliceVars(func.args);
            count += args.len;
            const ret_var = func.ret;
            if (self.lookupFunctionArgVarInStore(store, ret_var, 0) == null) return count;
            current = ret_var;
        }
    }

    fn lookupOrSpecializeExternal(
        self: *Lowerer,
        current_module_idx: u32,
        type_scope: *TypeCloneScope,
        lookup: anytype,
        expected_ty: type_mod.TypeId,
        expected_var: ?Var,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const current_module = self.ctx.typedCirModule(current_module_idx);
        const module_idx = current_module.resolvedImportModule(lookup.module_idx) orelse debugPanic(
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
                return try self.specializeTopLevelFromCallSite(
                    type_scope,
                    symbol,
                    top_level,
                    expected_ty,
                    expected_var orelse debugPanic(
                        "monotype specialization invariant violated: missing exact checker seed for external top-level specialization {d}",
                        .{symbol.raw()},
                    ),
                );
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

    fn copyCheckerVarToScope(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        source_module_idx: u32,
        source_var: Var,
    ) std.mem.Allocator.Error!Var {
        return self.scopeVar(type_scope, source_module_idx, source_var);
    }

    fn buildCurriedFuncType(
        self: *Lowerer,
        arg_ids: []const type_mod.TypeId,
        ret_id: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.Content {
        if (arg_ids.len == 0) {
            const unit_ty = try self.ctx.types.addType(.{ .record = .{ .fields = &.{} } });
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

    fn findTopLevelDefByIdent(
        self: *const Lowerer,
        module_idx: u32,
        ident: base.Ident.Idx,
    ) ?CIR.Def.Idx {
        const typed_cir_module = self.ctx.typedCirModule(module_idx);
        for (typed_cir_module.allDefs()) |def_idx| {
            const def = typed_cir_module.def(def_idx);
            if (def.data.kind != .let) continue;
            switch (def.pattern.data) {
                .assign => |assign| {
                    if (assign.ident.eql(ident)) return def_idx;
                },
                else => {},
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
        _: u32,
        type_scope: *TypeCloneScope,
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
                break :blk try self.specializeTopLevelFromCallSite(
                    type_scope,
                    source_symbol,
                    top_level,
                    expected_ty,
                    expected_var orelse debugPanic(
                        "monotype specialization invariant violated: missing exact checker seed for resolved target specialization {d}",
                        .{source_symbol.raw()},
                    ),
                );
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

    fn freezeBindingEnv(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        env: BindingEnv,
    ) std.mem.Allocator.Error!FrozenBindingEnv {
        var out = FrozenBindingEnv.init(self.allocator);
        errdefer {
            var iter = out.valueIterator();
            while (iter.next()) |value| {
                value.deinit(self.allocator);
            }
            out.deinit();
        }

        var iter = env.iterator();
        while (iter.next()) |entry| {
            const frozen_typed = if (entry.value_ptr.typed) |typed| blk: {
                break :blk FrozenTypedBinding{
                    .symbol = typed.symbol,
                    .checker_var = if (typed.solved_var) |var_|
                        try self.freezeCheckerVarFromScope(type_scope, var_)
                    else
                        null,
                };
            } else null;

            try out.put(entry.key_ptr.*, .{
                .typed = frozen_typed,
                .local_fn_source = entry.value_ptr.local_fn_source,
                .local_fn_seed_var = if (entry.value_ptr.local_fn_seed_var) |var_|
                    try self.freezeCheckerVarFromScope(type_scope, var_)
                else
                    null,
            });
        }
        return out;
    }

    fn materializeFrozenBindingEnv(
        self: *Lowerer,
        type_scope: *TypeCloneScope,
        frozen_env: FrozenBindingEnv,
    ) std.mem.Allocator.Error!BindingEnv {
        var out = BindingEnv.init(self.allocator);
        errdefer out.deinit();

        var iter = frozen_env.iterator();
        while (iter.next()) |entry| {
            const typed = if (entry.value_ptr.typed) |frozen_typed| blk: {
                break :blk TypedBinding{
                    .symbol = frozen_typed.symbol,
                    .solved_var = if (frozen_typed.checker_var) |frozen_var|
                        try self.materializeFrozenCheckerVar(type_scope, frozen_var)
                    else
                        null,
                };
            } else null;

            try out.put(entry.key_ptr.*, .{
                .typed = typed,
                .local_fn_source = entry.value_ptr.local_fn_source,
                .local_fn_seed_var = if (entry.value_ptr.local_fn_seed_var) |frozen_var|
                    try self.materializeFrozenCheckerVar(type_scope, frozen_var)
                else
                    null,
            });
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

fn isRecursiveTopLevelDef(typed_cir_module: Ctx.Module, def_idx: CIR.Def.Idx) bool {
    const evaluation_order = typed_cir_module.evaluationOrder() orelse return false;
    for (evaluation_order.sccs) |scc| {
        for (scc.defs) |member| {
            if (member == def_idx) return scc.is_recursive;
        }
    }
    return false;
}

fn builtinNumPrim(typed_cir_module: Ctx.Module, ident: base.Ident.Idx) ?type_mod.Prim {
    const idents = typed_cir_module.commonIdents();
    if (ident.eql(idents.u8) or ident.eql(idents.u8_type)) return .u8;
    if (ident.eql(idents.i8) or ident.eql(idents.i8_type)) return .i8;
    if (ident.eql(idents.u16) or ident.eql(idents.u16_type)) return .u16;
    if (ident.eql(idents.i16) or ident.eql(idents.i16_type)) return .i16;
    if (ident.eql(idents.u32) or ident.eql(idents.u32_type)) return .u32;
    if (ident.eql(idents.i32) or ident.eql(idents.i32_type)) return .i32;
    if (ident.eql(idents.u64) or ident.eql(idents.u64_type)) return .u64;
    if (ident.eql(idents.i64) or ident.eql(idents.i64_type)) return .i64;
    if (ident.eql(idents.u128) or ident.eql(idents.u128_type)) return .u128;
    if (ident.eql(idents.i128) or ident.eql(idents.i128_type)) return .i128;
    if (ident.eql(idents.f32) or ident.eql(idents.f32_type)) return .f32;
    if (ident.eql(idents.f64) or ident.eql(idents.f64_type)) return .f64;
    if (ident.eql(idents.dec) or ident.eql(idents.dec_type)) return .dec;
    return null;
}

fn builtinNumPrimInStore(idents: *const base.Ident.Store, ident: base.Ident.Idx) ?type_mod.Prim {
    const text = idents.getText(ident);
    if (std.mem.eql(u8, text, "U8")) return .u8;
    if (std.mem.eql(u8, text, "I8")) return .i8;
    if (std.mem.eql(u8, text, "U16")) return .u16;
    if (std.mem.eql(u8, text, "I16")) return .i16;
    if (std.mem.eql(u8, text, "U32")) return .u32;
    if (std.mem.eql(u8, text, "I32")) return .i32;
    if (std.mem.eql(u8, text, "U64")) return .u64;
    if (std.mem.eql(u8, text, "I64")) return .i64;
    if (std.mem.eql(u8, text, "U128")) return .u128;
    if (std.mem.eql(u8, text, "I128")) return .i128;
    if (std.mem.eql(u8, text, "F32")) return .f32;
    if (std.mem.eql(u8, text, "F64")) return .f64;
    if (std.mem.eql(u8, text, "Dec")) return .dec;
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

fn isBuiltinBoolTagUnionSlice(
    typed_cir_module: Ctx.Module,
    exec_idents: *const base.Ident.Store,
    tags: []const type_mod.Tag,
) bool {
    if (tags.len != 2) return false;

    var saw_true = false;
    var saw_false = false;
    const idents = typed_cir_module.commonIdents();
    for (tags) |tag| {
        const args_len = tag.args.len;
        if (args_len != 0) return false;
        const tag_name = exec_idents.getText(tag.name);
        if (std.mem.eql(u8, tag_name, typed_cir_module.getIdent(idents.true_tag))) {
            saw_true = true;
            continue;
        }
        if (std.mem.eql(u8, tag_name, typed_cir_module.getIdent(idents.false_tag))) {
            saw_false = true;
            continue;
        }
        return false;
    }

    return saw_true and saw_false;
}

fn constraintsContainFromNumeral(
    type_store: *const types.Store,
    range: types.StaticDispatchConstraint.SafeList.Range,
) bool {
    for (type_store.sliceStaticDispatchConstraints(range)) |constraint| {
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

fn debugTodo(comptime msg: []const u8) noreturn {
    if (comptime builtin.target.os.tag == .freestanding) {
        @trap();
    }
    if (builtin.mode == .Debug) {
        std.debug.panic("TODO {s}", .{msg});
    }
    unreachable;
}

fn debugPanic(comptime fmt: []const u8, args: anytype) noreturn {
    if (comptime builtin.target.os.tag == .freestanding) {
        @trap();
    }
    if (builtin.mode == .Debug) {
        std.debug.panic(fmt, args);
    }
    unreachable;
}

fn debugTodoExpr(expr: CIR.Expr) noreturn {
    if (comptime builtin.target.os.tag == .freestanding) {
        @trap();
    }
    if (builtin.mode == .Debug) {
        std.debug.panic("TODO monotype.lowerExpr expr {s}", .{@tagName(expr)});
    }
    unreachable;
}

fn debugTodoStmt(stmt: CIR.Statement) noreturn {
    if (comptime builtin.target.os.tag == .freestanding) {
        @trap();
    }
    if (builtin.mode == .Debug) {
        std.debug.panic("TODO monotype.lowerStmt stmt {s}", .{@tagName(stmt)});
    }
    unreachable;
}

fn debugTodoPattern(pattern: CIR.Pattern) noreturn {
    if (comptime builtin.target.os.tag == .freestanding) {
        @trap();
    }
    if (builtin.mode == .Debug) {
        std.debug.panic("TODO monotype.lowerPattern {s}", .{@tagName(pattern)});
    }
    unreachable;
}

test "monotype lower tests" {
    std.testing.refAllDecls(@This());
}
