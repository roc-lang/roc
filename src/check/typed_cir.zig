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

    pub const ResolvedMethodTarget = struct {
        module_idx: u32,
        def_idx: CIR.Def.Idx,
    };

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

    pub fn init(allocator: Allocator, source_modules: []const SourceModule) Allocator.Error!Modules {
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

};

pub const Module = struct {
    allocator: Allocator,
    module_idx: u32,
    data_store: *ModuleData,

    pub const CurriedFnShape = struct {
        args: []Var,
        ret: Var,

        pub fn deinit(self: *@This(), allocator: Allocator) void {
            allocator.free(self.args);
        }
    };

    pub const DispatchReceiverIdentity = struct {
        origin_module: Ident.Idx,
        type_ident: Ident.Idx,
    };

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

    pub fn requiresTypes(self: @This()) []const ModuleEnv.RequiredType {
        return self.env().requires_types.items.items;
    }

    pub fn lookupMethodIdent(
        self: @This(),
        local_type_ident: Ident.Idx,
        local_method_ident: Ident.Idx,
    ) ?Ident.Idx {
        return self.env().method_idents.get(self.env().gpa, MethodKey{
            .type_ident = local_type_ident,
            .method_ident = local_method_ident,
        });
    }

    pub fn nodeTag(self: @This(), idx: CIR.Node.Idx) CIR.Node.Tag {
        return self.env().store.nodes.get(idx).tag;
    }

    pub fn regionAt(self: @This(), idx: CIR.Node.Idx) base.Region {
        return self.env().store.getNodeRegion(idx);
    }

    pub fn getSource(self: @This(), region: base.Region) []const u8 {
        return self.env().getSource(region);
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

    pub fn topLevelDefByIdent(self: @This(), ident: Ident.Idx) ?CIR.Def.Idx {
        for (self.allDefs()) |def_idx| {
            const def = self.def(def_idx);
            if (def.data.kind != .let) continue;
            switch (def.pattern.data) {
                .assign => |assign| if (assign.ident.eql(ident)) return def_idx,
                else => {},
            }
        }
        return null;
    }

    pub fn topLevelDefByText(self: @This(), text: []const u8) ?CIR.Def.Idx {
        const ident = self.findCommonIdent(text) orelse return null;
        return self.topLevelDefByIdent(ident);
    }

    pub fn exprType(_: @This(), idx: CIR.Expr.Idx) Var {
        return ModuleEnv.varFrom(idx);
    }

    pub fn exprNeedsInstantiation(self: @This(), idx: CIR.Expr.Idx) bool {
        return self.typeStoreConst().needsInstantiation(self.exprType(idx));
    }

    pub fn exprHasErrType(self: @This(), idx: CIR.Expr.Idx) bool {
        return self.typeStoreConst().resolveVar(self.exprType(idx)).desc.content == .err;
    }

    pub fn exprDefaultsToDec(self: @This(), idx: CIR.Expr.Idx) bool {
        const resolved = self.typeStoreConst().resolveVar(self.exprType(idx));
        return switch (resolved.desc.content) {
            .flex => |flex| blk: {
                const constraints = self.typeStoreConst().sliceStaticDispatchConstraints(flex.constraints);
                for (constraints) |constraint| {
                    if (constraint.origin == .from_numeral) break :blk true;
                }
                break :blk false;
            },
            else => false,
        };
    }

    pub fn curriedFnShape(self: @This(), fn_var: Var) Allocator.Error!CurriedFnShape {
        var args = std.ArrayList(Var).empty;
        errdefer args.deinit(self.allocator);
        const ret = try self.appendCurriedFnArgs(&args, fn_var);
        return .{
            .args = try args.toOwnedSlice(self.allocator),
            .ret = ret,
        };
    }

    pub fn sourceVarRoot(self: @This(), var_: Var) Var {
        return self.typeStoreConst().resolveVar(var_).var_;
    }

    pub fn dispatchReceiverIdentity(self: @This(), source_var: Var) ?DispatchReceiverIdentity {
        const resolved = self.typeStoreConst().resolveVar(source_var);
        return switch (resolved.desc.content) {
            .alias => |alias| .{
                .origin_module = alias.origin_module,
                .type_ident = alias.ident.ident_idx,
            },
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| .{
                    .origin_module = nominal.origin_module,
                    .type_ident = nominal.ident.ident_idx,
                },
                else => null,
            },
            else => null,
        };
    }

    pub fn expr(self: @This(), idx: CIR.Expr.Idx) Expr {
        return .{
            .owner = self,
            .idx = idx,
            .data = self.env().store.getExpr(idx),
        };
    }

    pub fn patternType(_: @This(), idx: CIR.Pattern.Idx) Var {
        return ModuleEnv.varFrom(idx);
    }

    pub fn pattern(self: @This(), idx: CIR.Pattern.Idx) Pattern {
        return .{
            .owner = self,
            .idx = idx,
            .data = self.env().store.getPattern(idx),
        };
    }

    pub fn typeAnnoType(_: @This(), idx: CIR.TypeAnno.Idx) Var {
        return ModuleEnv.varFrom(idx);
    }

    pub fn exprIdxFromTypeVar(_: @This(), var_: Var) ?CIR.Expr.Idx {
        return @enumFromInt(@intFromEnum(ModuleEnv.nodeIdxFrom(var_)));
    }

    pub fn resolveAttachedMethodTargetByIdents(
        self: @This(),
        local_type_ident: Ident.Idx,
        local_method_ident: Ident.Idx,
    ) ?Modules.ResolvedMethodTarget {
        const method_ident = self.lookupMethodIdent(local_type_ident, local_method_ident) orelse return null;
        const exposed = self.env().getExposedNodeIndexById(method_ident) orelse return null;
        return .{
            .module_idx = self.module_idx,
            .def_idx = @enumFromInt(@as(u32, @intCast(exposed))),
        };
    }

    pub fn resolveAttachedMethodTargetByText(
        self: @This(),
        type_name: []const u8,
        method_name: []const u8,
    ) ?Modules.ResolvedMethodTarget {
        const local_type_ident = self.findCommonIdent(type_name) orelse return null;
        const local_method_ident = self.findCommonIdent(method_name) orelse return null;
        return self.resolveAttachedMethodTargetByIdents(local_type_ident, local_method_ident);
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

    fn appendCurriedFnArgs(self: @This(), args: *std.ArrayList(Var), fn_var: Var) Allocator.Error!Var {
        const store = self.typeStoreConst();
        var current = fn_var;
        while (true) {
            const resolved = store.resolveVar(current);
            switch (resolved.desc.content) {
                .alias => |alias| {
                    current = store.getAliasBackingVar(alias);
                },
                .structure => |flat| switch (flat) {
                    .fn_pure, .fn_effectful, .fn_unbound => |func| {
                        try args.appendSlice(self.allocator, store.sliceVars(func.args));
                        const ret = func.ret;
                        const ret_resolved = store.resolveVar(ret);
                        switch (ret_resolved.desc.content) {
                            .alias => |alias| current = store.getAliasBackingVar(alias),
                            .structure => |ret_flat| switch (ret_flat) {
                                .fn_pure, .fn_effectful, .fn_unbound => current = ret,
                                else => return ret,
                            },
                            else => return ret,
                        }
                    },
                    else => std.debug.panic(
                        "typed_cir invariant violated: expected function type when building source function shape",
                        .{},
                    ),
                },
                else => std.debug.panic(
                    "typed_cir invariant violated: expected function type when building source function shape",
                    .{},
                ),
            }
        }
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
