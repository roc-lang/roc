const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const types = @import("types");
const Check = @import("Check.zig");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const ExposedItems = collections.ExposedItems;
const MethodIdents = ModuleEnv.MethodIdents;
const MethodKey = ModuleEnv.MethodKey;
const CommonIdents = ModuleEnv.CommonIdents;
const EvaluationOrder = can.DependencyGraph.EvaluationOrder;

const ModuleData = struct {
    all_defs: CIR.Def.Span,
    index_data: []u32,
    node_vars: []Var,
    node_tags: []CIR.Node.Tag,
    regions: []base.Region,

    defs: []CIR.Def,
    defs_present: []bool,

    exprs: []CIR.Expr,
    exprs_present: []bool,

    patterns: []CIR.Pattern,
    patterns_present: []bool,

    statements: []CIR.Statement,
    statements_present: []bool,

    record_fields: []CIR.RecordField,
    record_fields_present: []bool,

    record_destructs: []CIR.Pattern.RecordDestruct,
    record_destructs_present: []bool,

    if_branches: []CIR.Expr.IfBranch,
    if_branches_present: []bool,

    match_branches: []CIR.Expr.Match.Branch,
    match_branches_present: []bool,

    match_branch_patterns: []CIR.Expr.Match.BranchPattern,
    match_branch_patterns_present: []bool,

    type_store: types.Store,
    ident_store: Ident.Store,
    string_store: StringLiteral.Store,
    exposed_items: ExposedItems,
    resolved_import_modules: []CIR.Import.ResolvedModuleIdx,
    display_module_name_idx: Ident.Idx,
    qualified_module_ident: Ident.Idx,
    common_idents: CommonIdents,
    method_idents: MethodIdents,
    evaluation_order: ?EvaluationOrder,

    fn init(allocator: Allocator, env: *const ModuleEnv) Allocator.Error!ModuleData {
        const node_count: usize = env.store.nodes.len();

        var data: ModuleData = .{
            .all_defs = env.all_defs,
            .index_data = try allocator.dupe(u32, env.store.index_data.items.items),
            .node_vars = try allocator.alloc(Var, node_count),
            .node_tags = try allocator.alloc(CIR.Node.Tag, node_count),
            .regions = try allocator.alloc(base.Region, node_count),
            .defs = try allocator.alloc(CIR.Def, node_count),
            .defs_present = try allocator.alloc(bool, node_count),
            .exprs = try allocator.alloc(CIR.Expr, node_count),
            .exprs_present = try allocator.alloc(bool, node_count),
            .patterns = try allocator.alloc(CIR.Pattern, node_count),
            .patterns_present = try allocator.alloc(bool, node_count),
            .statements = try allocator.alloc(CIR.Statement, node_count),
            .statements_present = try allocator.alloc(bool, node_count),
            .record_fields = try allocator.alloc(CIR.RecordField, node_count),
            .record_fields_present = try allocator.alloc(bool, node_count),
            .record_destructs = try allocator.alloc(CIR.Pattern.RecordDestruct, node_count),
            .record_destructs_present = try allocator.alloc(bool, node_count),
            .if_branches = try allocator.alloc(CIR.Expr.IfBranch, node_count),
            .if_branches_present = try allocator.alloc(bool, node_count),
            .match_branches = try allocator.alloc(CIR.Expr.Match.Branch, node_count),
            .match_branches_present = try allocator.alloc(bool, node_count),
            .match_branch_patterns = try allocator.alloc(CIR.Expr.Match.BranchPattern, node_count),
            .match_branch_patterns_present = try allocator.alloc(bool, node_count),
            .type_store = try env.types.clone(allocator),
            .ident_store = try env.getIdentStoreConst().clone(allocator),
            .string_store = try env.common.strings.clone(allocator),
            .exposed_items = try env.common.exposed_items.clone(allocator),
            .resolved_import_modules = try allocator.dupe(CIR.Import.ResolvedModuleIdx, env.imports.resolved_modules.items.items),
            .display_module_name_idx = env.display_module_name_idx,
            .qualified_module_ident = env.qualified_module_ident,
            .common_idents = env.idents,
            .method_idents = try env.method_idents.clone(allocator),
            .evaluation_order = if (env.evaluation_order) |evaluation_order|
                try evaluation_order.clone(allocator)
            else
                null,
        };
        errdefer data.deinit(allocator);

        @memset(data.defs_present, false);
        @memset(data.exprs_present, false);
        @memset(data.patterns_present, false);
        @memset(data.statements_present, false);
        @memset(data.record_fields_present, false);
        @memset(data.record_destructs_present, false);
        @memset(data.if_branches_present, false);
        @memset(data.match_branches_present, false);
        @memset(data.match_branch_patterns_present, false);

        for (0..node_count) |raw_idx| {
            data.node_vars[raw_idx] = @enumFromInt(raw_idx);

            const node_idx: CIR.Node.Idx = @enumFromInt(raw_idx);
            const tag = env.store.nodes.get(node_idx).tag;
            data.node_tags[raw_idx] = tag;
            data.regions[raw_idx] = env.store.getRegionAt(node_idx);

            if (isDefTag(tag)) {
                const idx: CIR.Def.Idx = @enumFromInt(raw_idx);
                data.defs[raw_idx] = env.store.getDef(idx);
                data.defs_present[raw_idx] = true;
            }

            if (isExprTag(tag) or tag == .malformed) {
                const idx: CIR.Expr.Idx = @enumFromInt(raw_idx);
                data.exprs[raw_idx] = env.store.getExpr(idx);
                data.exprs_present[raw_idx] = true;
            }

            if (isPatternTag(tag) or tag == .malformed) {
                const idx: CIR.Pattern.Idx = @enumFromInt(raw_idx);
                data.patterns[raw_idx] = env.store.getPattern(idx);
                data.patterns_present[raw_idx] = true;
            }

            if (isStatementTag(tag) or tag == .malformed) {
                const idx: CIR.Statement.Idx = @enumFromInt(raw_idx);
                data.statements[raw_idx] = env.store.getStatement(idx);
                data.statements_present[raw_idx] = true;
            }

            if (isRecordFieldTag(tag)) {
                const idx: CIR.RecordField.Idx = @enumFromInt(raw_idx);
                data.record_fields[raw_idx] = env.store.getRecordField(idx);
                data.record_fields_present[raw_idx] = true;
            }

            if (isRecordDestructTag(tag)) {
                const idx: CIR.Pattern.RecordDestruct.Idx = @enumFromInt(raw_idx);
                data.record_destructs[raw_idx] = env.store.getRecordDestruct(idx);
                data.record_destructs_present[raw_idx] = true;
            }

            if (isIfBranchTag(tag)) {
                const idx: CIR.Expr.IfBranch.Idx = @enumFromInt(raw_idx);
                data.if_branches[raw_idx] = env.store.getIfBranch(idx);
                data.if_branches_present[raw_idx] = true;
            }

            if (isMatchBranchTag(tag)) {
                const idx: CIR.Expr.Match.Branch.Idx = @enumFromInt(raw_idx);
                data.match_branches[raw_idx] = env.store.getMatchBranch(idx);
                data.match_branches_present[raw_idx] = true;
            }

            if (isMatchBranchPatternTag(tag)) {
                const idx: CIR.Expr.Match.BranchPattern.Idx = @enumFromInt(raw_idx);
                data.match_branch_patterns[raw_idx] = env.store.getMatchBranchPattern(idx);
                data.match_branch_patterns_present[raw_idx] = true;
            }
        }

        return data;
    }

    fn deinit(self: *ModuleData, allocator: Allocator) void {
        if (self.evaluation_order) |*evaluation_order| evaluation_order.deinit();
        self.method_idents.deinit(allocator);
        allocator.free(self.resolved_import_modules);
        self.exposed_items.deinit(allocator);
        self.string_store.deinit(allocator);
        self.ident_store.deinit(allocator);
        self.type_store.deinit();
        allocator.free(self.match_branch_patterns_present);
        allocator.free(self.match_branch_patterns);
        allocator.free(self.match_branches_present);
        allocator.free(self.match_branches);
        allocator.free(self.if_branches_present);
        allocator.free(self.if_branches);
        allocator.free(self.record_destructs_present);
        allocator.free(self.record_destructs);
        allocator.free(self.record_fields_present);
        allocator.free(self.record_fields);
        allocator.free(self.statements_present);
        allocator.free(self.statements);
        allocator.free(self.patterns_present);
        allocator.free(self.patterns);
        allocator.free(self.exprs_present);
        allocator.free(self.exprs);
        allocator.free(self.defs_present);
        allocator.free(self.defs);
        allocator.free(self.node_vars);
        allocator.free(self.regions);
        allocator.free(self.node_tags);
        allocator.free(self.index_data);
    }
};

pub const Modules = struct {
    allocator: Allocator,
    modules: []ModuleData,

    pub const SourceModule = union(enum) {
        checked: *const Check,
        precompiled: *const ModuleEnv,

        fn env(self: @This()) *const ModuleEnv {
            return switch (self) {
                .checked => |checker| checker.cir,
                .precompiled => |module_env| module_env,
            };
        }
    };

    pub fn publish(allocator: Allocator, source_modules: []const SourceModule) Allocator.Error!Modules {
        const modules = try allocator.alloc(ModuleData, source_modules.len);
        errdefer allocator.free(modules);

        var built_count: usize = 0;
        errdefer {
            for (modules[0..built_count]) |*module_data| module_data.deinit(allocator);
        }

        for (source_modules, 0..) |source_module, i| {
            modules[i] = try ModuleData.init(allocator, source_module.env());
            built_count += 1;
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
            "MIR invariant violated: missing target module {s}",
            .{target_name},
        );
    }
};

pub const Module = struct {
    allocator: Allocator,
    module_idx: u32,
    data_store: *ModuleData,

    fn sliceFromSpan(self: @This(), comptime T: type, span: base.DataSpan) []const T {
        if (span.len == 0) return &.{};
        return @ptrCast(self.data_store.index_data[span.start..][0..span.len]);
    }

    pub fn allDefs(self: @This()) []const CIR.Def.Idx {
        return self.sliceDefs(self.data_store.all_defs);
    }

    pub fn nodeCount(self: @This()) usize {
        return self.data_store.node_tags.len;
    }

    pub fn typeStoreConst(self: @This()) *const types.Store {
        return &self.data_store.type_store;
    }

    pub fn typeStoreMut(self: @This()) *types.Store {
        return &self.data_store.type_store;
    }

    pub fn identStoreConst(self: @This()) *const Ident.Store {
        return &self.data_store.ident_store;
    }

    pub fn identStoreMut(self: @This()) *Ident.Store {
        return &self.data_store.ident_store;
    }

    pub fn commonIdents(self: @This()) CommonIdents {
        return self.data_store.common_idents;
    }

    pub fn qualifiedModuleIdent(self: @This()) Ident.Idx {
        return self.data_store.qualified_module_ident;
    }

    pub fn evaluationOrder(self: @This()) ?*const EvaluationOrder {
        if (self.data_store.evaluation_order) |*evaluation_order| return evaluation_order;
        return null;
    }

    pub fn findCommonIdent(self: @This(), text: []const u8) ?Ident.Idx {
        return self.identStoreConst().findByString(text);
    }

    pub fn getIdent(self: @This(), idx: Ident.Idx) []const u8 {
        return self.identStoreConst().getText(idx);
    }

    pub fn getString(self: @This(), idx: StringLiteral.Idx) []const u8 {
        return self.data_store.string_store.get(idx);
    }

    pub fn name(self: @This()) []const u8 {
        if (!self.data_store.qualified_module_ident.isNone()) {
            return self.getIdent(self.data_store.qualified_module_ident);
        }
        return self.getIdent(self.data_store.display_module_name_idx);
    }

    pub fn resolvedImportModule(self: @This(), import_idx: CIR.Import.Idx) ?u32 {
        const idx = @intFromEnum(import_idx);
        if (idx >= self.data_store.resolved_import_modules.len) return null;
        const resolved = self.data_store.resolved_import_modules[idx];
        if (resolved.isNone()) return null;
        return @intFromEnum(resolved);
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
        var method_idents = &self.data_store.method_idents;
        return method_idents.get(self.allocator, MethodKey{
            .type_ident = local_type_ident,
            .method_ident = local_method_ident,
        });
    }

    pub fn exposedNodeIndexById(self: @This(), ident_idx: Ident.Idx) ?u16 {
        return self.data_store.exposed_items.getNodeIndexById(self.allocator, @bitCast(ident_idx));
    }

    pub fn nodeTag(self: @This(), idx: CIR.Node.Idx) CIR.Node.Tag {
        return self.data_store.node_tags[@intFromEnum(idx)];
    }

    pub fn regionAt(self: @This(), idx: CIR.Node.Idx) base.Region {
        return self.data_store.regions[@intFromEnum(idx)];
    }

    pub fn sliceDefs(self: @This(), span: CIR.Def.Span) []const CIR.Def.Idx {
        return self.sliceFromSpan(CIR.Def.Idx, span.span);
    }

    pub fn def(self: @This(), idx: CIR.Def.Idx) Def {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.defs_present[raw]);
        const data = self.data_store.defs[raw];
        return .{
            .owner = self,
            .idx = idx,
            .data = data,
            .pattern = self.pattern(data.pattern),
            .expr = self.expr(data.expr),
        };
    }

    pub fn expr(self: @This(), idx: CIR.Expr.Idx) Expr {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.exprs_present[raw]);
        return .{
            .owner = self,
            .idx = idx,
            .data = self.data_store.exprs[raw],
            .solved_var = self.data_store.node_vars[raw],
        };
    }

    pub fn pattern(self: @This(), idx: CIR.Pattern.Idx) Pattern {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.patterns_present[raw]);
        return .{
            .owner = self,
            .idx = idx,
            .data = self.data_store.patterns[raw],
            .solved_var = self.data_store.node_vars[raw],
        };
    }

    pub fn typeAnnoVar(self: @This(), idx: CIR.TypeAnno.Idx) Var {
        return self.data_store.node_vars[@intFromEnum(idx)];
    }

    pub fn getStatement(self: @This(), idx: CIR.Statement.Idx) CIR.Statement {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.statements_present[raw]);
        return self.data_store.statements[raw];
    }

    pub fn getRecordField(self: @This(), idx: CIR.RecordField.Idx) CIR.RecordField {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.record_fields_present[raw]);
        return self.data_store.record_fields[raw];
    }

    pub fn getRecordDestruct(self: @This(), idx: CIR.Pattern.RecordDestruct.Idx) CIR.Pattern.RecordDestruct {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.record_destructs_present[raw]);
        return self.data_store.record_destructs[raw];
    }

    pub fn getIfBranch(self: @This(), idx: CIR.Expr.IfBranch.Idx) CIR.Expr.IfBranch {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.if_branches_present[raw]);
        return self.data_store.if_branches[raw];
    }

    pub fn getMatchBranch(self: @This(), idx: CIR.Expr.Match.Branch.Idx) CIR.Expr.Match.Branch {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.match_branches_present[raw]);
        return self.data_store.match_branches[raw];
    }

    pub fn getMatchBranchPattern(self: @This(), idx: CIR.Expr.Match.BranchPattern.Idx) CIR.Expr.Match.BranchPattern {
        const raw = @intFromEnum(idx);
        std.debug.assert(self.data_store.match_branch_patterns_present[raw]);
        return self.data_store.match_branch_patterns[raw];
    }

    pub fn sliceExpr(self: @This(), span: CIR.Expr.Span) []const CIR.Expr.Idx {
        return self.sliceFromSpan(CIR.Expr.Idx, span.span);
    }

    pub fn slicePatterns(self: @This(), span: CIR.Pattern.Span) []const CIR.Pattern.Idx {
        return self.sliceFromSpan(CIR.Pattern.Idx, span.span);
    }

    pub fn sliceStatements(self: @This(), span: CIR.Statement.Span) []const CIR.Statement.Idx {
        return self.sliceFromSpan(CIR.Statement.Idx, span.span);
    }

    pub fn sliceRecordFields(self: @This(), span: CIR.RecordField.Span) []const CIR.RecordField.Idx {
        return self.sliceFromSpan(CIR.RecordField.Idx, span.span);
    }

    pub fn sliceRecordDestructs(self: @This(), span: CIR.Pattern.RecordDestruct.Span) []const CIR.Pattern.RecordDestruct.Idx {
        return self.sliceFromSpan(CIR.Pattern.RecordDestruct.Idx, span.span);
    }

    pub fn sliceIfBranches(self: @This(), span: CIR.Expr.IfBranch.Span) []const CIR.Expr.IfBranch.Idx {
        return self.sliceFromSpan(CIR.Expr.IfBranch.Idx, span.span);
    }

    pub fn matchBranchSlice(self: @This(), span: CIR.Expr.Match.Branch.Span) []const CIR.Expr.Match.Branch.Idx {
        return self.sliceFromSpan(CIR.Expr.Match.Branch.Idx, span.span);
    }

    pub fn sliceMatchBranchPatterns(self: @This(), span: CIR.Expr.Match.BranchPattern.Span) []const CIR.Expr.Match.BranchPattern.Idx {
        return self.sliceFromSpan(CIR.Expr.Match.BranchPattern.Idx, span.span);
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
    solved_var: Var,

    pub fn module(self: @This()) Module {
        return self.owner;
    }
};

pub const Pattern = struct {
    owner: Module,
    idx: CIR.Pattern.Idx,
    data: CIR.Pattern,
    solved_var: Var,

    pub fn module(self: @This()) Module {
        return self.owner;
    }
};

fn isDefTag(tag: CIR.Node.Tag) bool {
    return tag == .def;
}

fn isRecordFieldTag(tag: CIR.Node.Tag) bool {
    return tag == .record_field;
}

fn isRecordDestructTag(tag: CIR.Node.Tag) bool {
    return tag == .record_destruct;
}

fn isIfBranchTag(tag: CIR.Node.Tag) bool {
    return tag == .if_branch;
}

fn isMatchBranchTag(tag: CIR.Node.Tag) bool {
    return tag == .match_branch;
}

fn isMatchBranchPatternTag(tag: CIR.Node.Tag) bool {
    return tag == .match_branch_pattern;
}

fn isStatementTag(tag: CIR.Node.Tag) bool {
    return switch (tag) {
        .statement_decl,
        .statement_var,
        .statement_reassign,
        .statement_crash,
        .statement_dbg,
        .statement_expr,
        .statement_expect,
        .statement_for,
        .statement_while,
        .statement_break,
        .statement_return,
        .statement_import,
        .statement_alias_decl,
        .statement_nominal_decl,
        .statement_type_anno,
        .statement_type_var_alias,
        => true,
        else => false,
    };
}

fn isExprTag(tag: CIR.Node.Tag) bool {
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
        .expr_static_dispatch,
        .expr_external_lookup,
        .expr_pending_lookup,
        .expr_required_lookup,
        .expr_dot_access,
        .expr_apply,
        .expr_string,
        .expr_string_segment,
        .expr_bytes_literal,
        .expr_num,
        .expr_frac_f32,
        .expr_frac_f64,
        .expr_dec,
        .expr_dec_small,
        .expr_typed_int,
        .expr_typed_frac,
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
        .expr_type_var_dispatch,
        => true,
        else => false,
    };
}

fn isPatternTag(tag: CIR.Node.Tag) bool {
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
        .pattern_underscore,
        => true,
        else => false,
    };
}
