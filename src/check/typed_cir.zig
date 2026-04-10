const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const types = @import("types");
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const MethodKey = ModuleEnv.MethodKey;
const CommonIdents = ModuleEnv.CommonIdents;
const EvaluationOrder = can.DependencyGraph.EvaluationOrder;
const CompactWriter = collections.CompactWriter;

const CachedModule = struct {
    env: *ModuleEnv,
    buffer: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
};

const OwnedCheckedModule = struct {
    env: *ModuleEnv,
    owned_source: ?[]u8 = null,
};

const ModuleData = struct {
    env: *ModuleEnv,
    ownership: union(enum) {
        borrowed,
        owned_checked: OwnedCheckedModule,
        owned_cached: CachedModule,
    },

    fn initBorrowed(env: *ModuleEnv) ModuleData {
        return .{
            .env = env,
            .ownership = .borrowed,
        };
    }

    fn initOwnedChecked(env: *ModuleEnv, owned_source: ?[]u8) ModuleData {
        return .{
            .env = env,
            .ownership = .{ .owned_checked = .{
                .env = env,
                .owned_source = owned_source,
            } },
        };
    }

    fn initOwnedCached(env: *ModuleEnv, buffer: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8) ModuleData {
        return .{
            .env = env,
            .ownership = .{ .owned_cached = .{
                .env = env,
                .buffer = buffer,
            } },
        };
    }

    fn deinit(self: *ModuleData, allocator: Allocator) void {
        switch (self.ownership) {
            .borrowed => {},
            .owned_checked => |owned| {
                owned.env.deinit();
                if (owned.owned_source) |source| allocator.free(source);
                allocator.destroy(owned.env);
            },
            .owned_cached => |owned| {
                owned.env.deinitCachedModule();
                allocator.free(owned.buffer);
                allocator.destroy(owned.env);
            },
        }
    }
};

pub const Modules = struct {
    allocator: Allocator,
    modules: []ModuleData,

    pub const SourceModule = union(enum) {
        precompiled: *ModuleEnv,
        owned_checked: OwnedCheckedModule,
        owned_cached: CachedModule,

        fn initModuleData(self: @This(), allocator: Allocator) Allocator.Error!ModuleData {
            return switch (self) {
                .precompiled => |module_env| blk: {
                    try module_env.getIdentStore().enableRuntimeInserts(allocator);
                    break :blk ModuleData.initBorrowed(module_env);
                },
                .owned_checked => |owned| blk: {
                    try owned.env.getIdentStore().enableRuntimeInserts(allocator);
                    break :blk ModuleData.initOwnedChecked(owned.env, owned.owned_source);
                },
                .owned_cached => |owned| blk: {
                    try owned.env.getIdentStore().enableRuntimeInserts(allocator);
                    break :blk ModuleData.initOwnedCached(owned.env, owned.buffer);
                },
            };
        }
    };

    pub fn publish(allocator: Allocator, source_modules: []const SourceModule) Allocator.Error!Modules {
        const modules = try allocator.alloc(ModuleData, source_modules.len);
        errdefer allocator.free(modules);

        for (source_modules, 0..) |source_module, i| {
            modules[i] = try source_module.initModuleData(allocator);
        }

        return .{
            .allocator = allocator,
            .modules = modules,
        };
    }

    pub fn deinit(self: *Modules) void {
        for (self.modules) |*module_data| module_data.deinit(self.allocator);
        self.allocator.free(self.modules);
    }

    pub fn moduleCount(self: @This()) usize {
        return self.modules.len;
    }

    pub fn module(self: @This(), module_idx: u32) Module {
        return .{
            .allocator = self.allocator,
            .module_idx = module_idx,
            .data_store = @constCast(&self.modules[module_idx]),
        };
    }

    pub fn findModuleIdxByName(self: @This(), target_name: []const u8) u32 {
        for (0..self.moduleCount()) |idx| {
            if (std.mem.eql(u8, self.module(@intCast(idx)).name(), target_name)) return @intCast(idx);
        }

        std.debug.panic(
            "typed CIR invariant violated: missing target module {s}",
            .{target_name},
        );
    }

};

pub const Module = struct {
    pub const DispatchSite = union(enum) {
        resolved: types.ResolvedStaticDispatchSite,
        requirement: types.StaticDispatchSiteRequirement,
    };

    allocator: Allocator,
    module_idx: u32,
    data_store: *ModuleData,

    fn env(self: @This()) *ModuleEnv {
        return self.data_store.env;
    }

    pub fn allDefs(self: @This()) []const CIR.Def.Idx {
        return self.env().store.sliceDefs(self.env().all_defs);
    }

    pub fn nodeCount(self: @This()) usize {
        return self.env().store.nodes.len();
    }

    pub fn typeStoreConst(self: @This()) *const types.Store {
        return &self.env().types;
    }

    pub fn identStoreConst(self: @This()) *const Ident.Store {
        return self.env().getIdentStoreConst();
    }

    pub fn commonIdents(self: @This()) CommonIdents {
        return self.env().idents;
    }

    pub fn qualifiedModuleIdent(self: @This()) Ident.Idx {
        return self.env().qualified_module_ident;
    }

    pub fn evaluationOrder(self: @This()) ?*const EvaluationOrder {
        if (self.env().evaluation_order) |evaluation_order| return evaluation_order;
        return null;
    }

    pub fn findCommonIdent(self: @This(), text: []const u8) ?Ident.Idx {
        return self.identStoreConst().findByString(text);
    }

    pub fn getIdent(self: @This(), idx: Ident.Idx) []const u8 {
        return self.env().getIdent(idx);
    }

    pub fn getString(self: @This(), idx: StringLiteral.Idx) []const u8 {
        return self.env().getString(idx);
    }

    pub fn name(self: @This()) []const u8 {
        if (!self.env().qualified_module_ident.isNone()) {
            return self.getIdent(self.env().qualified_module_ident);
        }
        return self.getIdent(self.env().display_module_name_idx);
    }

    pub fn resolvedImportModule(self: @This(), import_idx: CIR.Import.Idx) ?u32 {
        return self.env().imports.getResolvedModule(import_idx);
    }

    pub fn lookupMethodIdentFromModule(
        self: @This(),
        source_module: Module,
        type_ident: Ident.Idx,
        method_ident: Ident.Idx,
    ) ?Ident.Idx {
        const type_name = source_module.getIdent(type_ident);
        const method_name = source_module.getIdent(method_ident);
        const local_type_ident = self.findCommonIdent(type_name) orelse return null;
        const local_method_ident = self.findCommonIdent(method_name) orelse return null;
        return self.env().method_idents.get(self.env().gpa, MethodKey{
            .type_ident = local_type_ident,
            .method_ident = local_method_ident,
        });
    }

    pub fn lookupMethodIdentByText(
        self: @This(),
        type_name: []const u8,
        method_name: []const u8,
    ) ?Ident.Idx {
        const local_type_ident = self.findCommonIdent(type_name) orelse return null;
        const local_method_ident = self.findCommonIdent(method_name) orelse return null;
        return self.env().method_idents.get(self.env().gpa, MethodKey{
            .type_ident = local_type_ident,
            .method_ident = local_method_ident,
        });
    }

    pub fn exposedNodeIndexById(self: @This(), ident_idx: Ident.Idx) ?u16 {
        return self.env().getExposedNodeIndexById(ident_idx);
    }

    pub fn nodeTag(self: @This(), idx: CIR.Node.Idx) CIR.Node.Tag {
        return self.env().store.nodes.get(idx).tag;
    }

    pub fn regionAt(self: @This(), idx: CIR.Node.Idx) base.Region {
        return self.env().store.getNodeRegion(idx);
    }

    pub fn sliceDefs(self: @This(), span: CIR.Def.Span) []const CIR.Def.Idx {
        return self.env().store.sliceDefs(span);
    }

    pub fn def(self: @This(), idx: CIR.Def.Idx) Def {
        const data = self.env().store.getDef(idx);
        return .{
            .owner = self,
            .idx = idx,
            .data = data,
            .pattern = self.pattern(data.pattern),
            .expr = self.expr(data.expr),
        };
    }

    pub fn defType(self: @This(), idx: CIR.Def.Idx) Var {
        const def_data = self.env().store.getDef(idx);
        return self.patternType(def_data.pattern);
    }

    pub fn exprType(self: @This(), idx: CIR.Expr.Idx) Var {
        _ = self;
        return ModuleEnv.varFrom(idx);
    }

    pub fn expr(self: @This(), idx: CIR.Expr.Idx) Expr {
        return .{
            .owner = self,
            .idx = idx,
            .data = self.env().store.getExpr(idx),
        };
    }

    pub fn patternType(self: @This(), idx: CIR.Pattern.Idx) Var {
        _ = self;
        return ModuleEnv.varFrom(idx);
    }

    pub fn pattern(self: @This(), idx: CIR.Pattern.Idx) Pattern {
        return .{
            .owner = self,
            .idx = idx,
            .data = self.env().store.getPattern(idx),
        };
    }

    pub fn typeAnnoType(self: @This(), idx: CIR.TypeAnno.Idx) Var {
        _ = self;
        return ModuleEnv.varFrom(idx);
    }

    pub fn staticDispatchSiteRequirement(
        self: @This(),
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
    ) ?types.StaticDispatchSiteRequirement {
        return self.typeStoreConst().findStaticDispatchSiteRequirement(
            self.exprType(expr_idx),
            method_name,
        );
    }

    pub fn resolvedStaticDispatchTarget(
        self: @This(),
        expr_idx: CIR.Expr.Idx,
    ) ?types.ResolvedStaticDispatchSite {
        return self.typeStoreConst().findResolvedStaticDispatchSite(self.exprType(expr_idx));
    }

    pub fn dispatchSite(
        self: @This(),
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
    ) ?DispatchSite {
        if (self.resolvedStaticDispatchTarget(expr_idx)) |resolved| {
            return .{ .resolved = resolved };
        }
        if (self.staticDispatchSiteRequirement(expr_idx, method_name)) |requirement| {
            return .{ .requirement = requirement };
        }
        return null;
    }

    pub fn getStatement(self: @This(), idx: CIR.Statement.Idx) CIR.Statement {
        return self.env().store.getStatement(idx);
    }

    pub fn getRecordField(self: @This(), idx: CIR.RecordField.Idx) CIR.RecordField {
        return self.env().store.getRecordField(idx);
    }

    pub fn getRecordDestruct(self: @This(), idx: CIR.Pattern.RecordDestruct.Idx) CIR.Pattern.RecordDestruct {
        return self.env().store.getRecordDestruct(idx);
    }

    pub fn getIfBranch(self: @This(), idx: CIR.Expr.IfBranch.Idx) CIR.Expr.IfBranch {
        return self.env().store.getIfBranch(idx);
    }

    pub fn getMatchBranch(self: @This(), idx: CIR.Expr.Match.Branch.Idx) CIR.Expr.Match.Branch {
        return self.env().store.getMatchBranch(idx);
    }

    pub fn getMatchBranchPattern(self: @This(), idx: CIR.Expr.Match.BranchPattern.Idx) CIR.Expr.Match.BranchPattern {
        return self.env().store.getMatchBranchPattern(idx);
    }

    pub fn sliceExpr(self: @This(), span: CIR.Expr.Span) []const CIR.Expr.Idx {
        return self.env().store.sliceExpr(span);
    }

    pub fn slicePatterns(self: @This(), span: CIR.Pattern.Span) []const CIR.Pattern.Idx {
        return self.env().store.slicePatterns(span);
    }

    pub fn sliceStatements(self: @This(), span: CIR.Statement.Span) []const CIR.Statement.Idx {
        return self.env().store.sliceStatements(span);
    }

    pub fn sliceRecordFields(self: @This(), span: CIR.RecordField.Span) []const CIR.RecordField.Idx {
        return self.env().store.sliceRecordFields(span);
    }

    pub fn sliceRecordDestructs(self: @This(), span: CIR.Pattern.RecordDestruct.Span) []const CIR.Pattern.RecordDestruct.Idx {
        return self.env().store.sliceRecordDestructs(span);
    }

    pub fn sliceIfBranches(self: @This(), span: CIR.Expr.IfBranch.Span) []const CIR.Expr.IfBranch.Idx {
        return self.env().store.sliceIfBranches(span);
    }

    pub fn matchBranchSlice(self: @This(), span: CIR.Expr.Match.Branch.Span) []const CIR.Expr.Match.Branch.Idx {
        return self.env().store.sliceMatchBranches(span);
    }

    pub fn sliceMatchBranchPatterns(self: @This(), span: CIR.Expr.Match.BranchPattern.Span) []const CIR.Expr.Match.BranchPattern.Idx {
        return self.env().store.sliceMatchBranchPatterns(span);
    }
};

pub const Def = struct {
    owner: Module,
    idx: CIR.Def.Idx,
    data: CIR.Def,
    pattern: Pattern,
    expr: Expr,

    pub fn module(self: @This()) Module {
        return self.owner;
    }
};

pub const Expr = struct {
    owner: Module,
    idx: CIR.Expr.Idx,
    data: CIR.Expr,

    pub fn module(self: @This()) Module {
        return self.owner;
    }

    pub fn ty(self: @This()) Var {
        return self.owner.exprType(self.idx);
    }
};

pub const Pattern = struct {
    owner: Module,
    idx: CIR.Pattern.Idx,
    data: CIR.Pattern,

    pub fn module(self: @This()) Module {
        return self.owner;
    }

    pub fn ty(self: @This()) Var {
        return self.owner.patternType(self.idx);
    }
};
