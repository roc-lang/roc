//! One-time source-to-solved extraction of callable templates and root-owned
//! callable discovery.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

pub const ExprTemplateSource = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
};

pub const LocalPatternTemplateSource = struct {
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
};

pub const CallableTemplateId = enum(u32) {
    _,
};

pub const CallableTemplateKind = enum {
    top_level_def,
    lambda,
    closure,
    hosted_lambda,
};

pub const ExternalDefSource = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

pub const CallableTemplateSource = union(enum) {
    expr: ExprTemplateSource,
    local_pattern: LocalPatternTemplateSource,
    external_def: ExternalDefSource,
};

pub const CallableTemplateBinding = union(enum) {
    anonymous,
    pattern: CIR.Pattern.Idx,
};

pub const CallableTemplateOwner = union(enum) {
    root_scope,
    lexical_template: CallableTemplateId,
};

pub const CallableTemplate = struct {
    source: CallableTemplateSource,
    module_idx: u32,
    cir_expr: CIR.Expr.Idx,
    runtime_expr: CIR.Expr.Idx,
    arg_patterns: CIR.Pattern.Span,
    body_expr: CIR.Expr.Idx,
    low_level_op: ?CIR.Expr.LowLevel = null,
    type_root: types.Var,
    binding: CallableTemplateBinding,
    kind: CallableTemplateKind,
    owner: CallableTemplateOwner,
    source_region: Region = Region.zero(),
};

pub const CallableBoundaryInfo = struct {
    arg_patterns: CIR.Pattern.Span,
    body_expr: CIR.Expr.Idx,
};

pub const DefExprCallableBoundary = struct {
    expr_idx: CIR.Expr.Idx,
    kind: CallableTemplateKind,
};

pub const RootExprSpan = extern struct {
    start: u32,
    len: u32,

    pub fn empty() RootExprSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: RootExprSpan) bool {
        return self.len == 0;
    }
};

pub fn exprTemplateSource(module_idx: u32, expr_idx: CIR.Expr.Idx) ExprTemplateSource {
    return .{ .module_idx = module_idx, .expr_idx = expr_idx };
}

pub fn localPatternTemplateSource(module_idx: u32, pattern_idx: CIR.Pattern.Idx) LocalPatternTemplateSource {
    return .{ .module_idx = module_idx, .pattern_idx = pattern_idx };
}

pub fn externalDefTemplateSource(module_idx: u32, def_node_idx: u16) ExternalDefSource {
    return .{ .module_idx = module_idx, .def_idx = @enumFromInt(def_node_idx) };
}

pub fn callableBoundaryInfo(
    all_module_envs: []const *ModuleEnv,
    module_idx: u32,
    callable_expr_idx: CIR.Expr.Idx,
) ?CallableBoundaryInfo {
    const module_env = all_module_envs[module_idx];
    return switch (module_env.store.getExpr(callable_expr_idx)) {
        .e_lambda => |lambda_expr| .{
            .arg_patterns = lambda_expr.args,
            .body_expr = lambda_expr.body,
        },
        .e_closure => |closure_expr| blk: {
            const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
            if (lambda_expr != .e_lambda) break :blk null;
            break :blk .{
                .arg_patterns = lambda_expr.e_lambda.args,
                .body_expr = lambda_expr.e_lambda.body,
            };
        },
        .e_hosted_lambda => |hosted_expr| .{
            .arg_patterns = hosted_expr.args,
            .body_expr = hosted_expr.body,
        },
        else => null,
    };
}

pub fn defExprCallableBoundary(
    all_module_envs: []const *ModuleEnv,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?DefExprCallableBoundary {
    const module_env = all_module_envs[module_idx];
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_lambda => .{ .expr_idx = expr_idx, .kind = .lambda },
        .e_closure => .{ .expr_idx = expr_idx, .kind = .closure },
        .e_hosted_lambda => .{ .expr_idx = expr_idx, .kind = .hosted_lambda },
        .e_block => |block| defExprCallableBoundary(all_module_envs, module_idx, block.final_expr),
        .e_dbg => |dbg_expr| defExprCallableBoundary(all_module_envs, module_idx, dbg_expr.expr),
        .e_expect => |expect_expr| defExprCallableBoundary(all_module_envs, module_idx, expect_expr.body),
        .e_return => |return_expr| defExprCallableBoundary(all_module_envs, module_idx, return_expr.expr),
        .e_nominal => |nominal_expr| defExprCallableBoundary(all_module_envs, module_idx, nominal_expr.backing_expr),
        .e_nominal_external => |nominal_expr| defExprCallableBoundary(all_module_envs, module_idx, nominal_expr.backing_expr),
        else => null,
    };
}

fn moduleIndexForEnv(all_module_envs: []const *ModuleEnv, env: *const ModuleEnv) ?u32 {
    for (all_module_envs, 0..) |module_env_entry, idx| {
        if (module_env_entry == env) return @intCast(idx);
    }
    return null;
}

pub fn moduleOwnsIdent(env: *const ModuleEnv, ident: base.Ident.Idx) bool {
    const ident_store = env.getIdentStoreConst();
    const bytes = ident_store.interner.bytes.items.items;
    const start: usize = @intCast(ident.idx);
    if (start >= bytes.len) return false;

    const tail = bytes[start..];
    const end_rel = std.mem.indexOfScalar(u8, tail, 0) orelse return false;
    const text = tail[0..end_rel];

    const roundtrip = ident_store.findByString(text) orelse return false;
    return roundtrip.eql(ident);
}

pub fn getOwnedIdentText(env: *const ModuleEnv, ident: base.Ident.Idx) []const u8 {
    if (std.debug.runtime_safety) std.debug.assert(moduleOwnsIdent(env, ident));
    return env.getIdent(ident);
}

pub fn identTextAcrossModules(
    all_module_envs: []const *ModuleEnv,
    starting_env: *const ModuleEnv,
    ident: base.Ident.Idx,
) ?[]const u8 {
    if (moduleOwnsIdent(starting_env, ident)) return getOwnedIdentText(starting_env, ident);

    for (all_module_envs) |module_env_entry| {
        if (module_env_entry == starting_env) continue;
        if (moduleOwnsIdent(module_env_entry, ident)) return getOwnedIdentText(module_env_entry, ident);
    }

    return null;
}

pub fn findModuleForOriginMaybe(
    all_module_envs: []const *ModuleEnv,
    source_env: *const ModuleEnv,
    origin_module: base.Ident.Idx,
) ?u32 {
    const source_module_idx = moduleIndexForEnv(all_module_envs, source_env) orelse return null;
    if (origin_module.eql(source_env.qualified_module_ident)) return source_module_idx;

    const origin_name = identTextAcrossModules(all_module_envs, source_env, origin_module) orelse return null;
    for (all_module_envs, 0..) |module_env_entry, idx| {
        const origin_module_name = module_env_entry.getIdent(module_env_entry.qualified_module_ident);
        if (std.mem.eql(u8, origin_name, origin_module_name)) return @intCast(idx);
    }
    return null;
}

pub fn findModuleForOrigin(
    all_module_envs: []const *ModuleEnv,
    source_env: *const ModuleEnv,
    origin_module: base.Ident.Idx,
) u32 {
    const source_module_idx = moduleIndexForEnv(all_module_envs, source_env) orelse unreachable;
    return findModuleForOriginMaybe(all_module_envs, source_env, origin_module) orelse {
        const origin_name = identTextAcrossModules(all_module_envs, source_env, origin_module) orelse source_env.getIdent(origin_module);
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "TemplateCatalog invariant violated: could not resolve origin module '{s}' from source module {d}",
                .{ origin_name, source_module_idx },
            );
        }
        unreachable;
    };
}

pub const Result = struct {
    callable_templates: std.ArrayListUnmanaged(CallableTemplate),
    callable_template_ids_by_source: std.AutoHashMapUnmanaged(CallableTemplateSource, CallableTemplateId),
    root_expr_entries: std.ArrayListUnmanaged(CIR.Expr.Idx),
    root_expr_spans_by_module: std.AutoHashMapUnmanaged(u32, RootExprSpan),

    pub fn init() Result {
        return .{
            .callable_templates = .empty,
            .callable_template_ids_by_source = .empty,
            .root_expr_entries = .empty,
            .root_expr_spans_by_module = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.callable_templates.deinit(allocator);
        self.callable_template_ids_by_source.deinit(allocator);
        self.root_expr_entries.deinit(allocator);
        self.root_expr_spans_by_module.deinit(allocator);
    }

    pub fn getCallableTemplate(self: *const Result, callable_template_id: CallableTemplateId) *const CallableTemplate {
        return &self.callable_templates.items[@intFromEnum(callable_template_id)];
    }

    pub fn getLocalCallableTemplate(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(.{
            .local_pattern = .{
                .module_idx = module_idx,
                .pattern_idx = pattern_idx,
            },
        });
    }

    pub fn getExternalCallableTemplate(self: *const Result, module_idx: u32, def_node_idx: u16) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(.{
            .external_def = .{
                .module_idx = module_idx,
                .def_idx = @enumFromInt(def_node_idx),
            },
        });
    }

    pub fn requireExternalCallableTemplate(
        self: *const Result,
        module_idx: u32,
        def_node_idx: u16,
        comptime reason: []const u8,
    ) CallableTemplateId {
        if (self.getExternalCallableTemplate(module_idx, def_node_idx)) |template_id| {
            return template_id;
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "TemplateCatalog invariant violated: {s} required external callable template after template-catalog priming (module={d}, def_node={d})",
                .{ reason, module_idx, def_node_idx },
            );
        }
        unreachable;
    }

    pub fn getExprCallableTemplate(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(.{
            .expr = .{
                .module_idx = module_idx,
                .expr_idx = expr_idx,
            },
        });
    }

    pub fn lookupCallableTemplateBySource(self: *const Result, source: CallableTemplateSource) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(source);
    }

    pub fn getModuleRootExprs(self: *const Result, module_idx: u32) []const CIR.Expr.Idx {
        const span = self.root_expr_spans_by_module.get(module_idx) orelse return &.{};
        return self.root_expr_entries.items[span.start .. span.start + span.len];
    }

    pub fn recordModuleRootExprs(
        self: *Result,
        allocator: Allocator,
        module_idx: u32,
        exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        if (self.root_expr_spans_by_module.contains(module_idx)) return;
        const start: u32 = @intCast(self.root_expr_entries.items.len);
        try self.root_expr_entries.appendSlice(allocator, exprs);
        try self.root_expr_spans_by_module.put(allocator, module_idx, .{
            .start = start,
            .len = @intCast(exprs.len),
        });
    }

    pub fn recordCallableTemplateSource(
        self: *Result,
        allocator: Allocator,
        source: CallableTemplateSource,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        try self.callable_template_ids_by_source.put(allocator, source, template_id);
    }

    pub fn recordAdditionalCallableTemplateSource(
        self: *Result,
        allocator: Allocator,
        source: CallableTemplateSource,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        const existing = self.lookupCallableTemplateBySource(source);
        if (existing) |existing_template_id| {
            if (existing_template_id != template_id) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "TemplateCatalog invariant violated: conflicting callable template aliases for source={s} (existing={d}, new={d})",
                        .{ @tagName(source), @intFromEnum(existing_template_id), @intFromEnum(template_id) },
                    );
                }
                unreachable;
            }
            return;
        }

        try self.recordCallableTemplateSource(allocator, source, template_id);
    }

    pub fn registerCallableTemplate(
        self: *Result,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        closure_owner: CallableTemplateOwner,
        source: CallableTemplateSource,
        module_idx: u32,
        cir_expr: CIR.Expr.Idx,
        runtime_expr: CIR.Expr.Idx,
        type_root: types.Var,
        binding_pattern: ?CIR.Pattern.Idx,
        kind: CallableTemplateKind,
        source_region: Region,
    ) Allocator.Error!CallableTemplateId {
        const existing = self.lookupCallableTemplateBySource(source);
        if (existing) |template_id| return template_id;
        const module_env = all_module_envs[module_idx];
        const boundary = callableBoundaryInfo(all_module_envs, module_idx, cir_expr) orelse blk: {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "TemplateCatalog invariant violated: registering callable template for non-callable boundary expr {d} in module {d}",
                    .{ @intFromEnum(cir_expr), module_idx },
                );
            }
            break :blk unreachable;
        };

        const owner: CallableTemplateOwner = if (kind == .closure)
            closure_owner
        else
            .root_scope;

        const callable_template_id: CallableTemplateId = @enumFromInt(self.callable_templates.items.len);
        try self.callable_templates.append(allocator, CallableTemplate{
            .source = source,
            .module_idx = module_idx,
            .cir_expr = cir_expr,
            .runtime_expr = runtime_expr,
            .arg_patterns = boundary.arg_patterns,
            .body_expr = boundary.body_expr,
            .low_level_op = switch (module_env.store.getExpr(boundary.body_expr)) {
                .e_run_low_level => |run_low_level| run_low_level.op,
                else => null,
            },
            .type_root = type_root,
            .binding = if (binding_pattern) |pattern_idx|
                .{ .pattern = pattern_idx }
            else
                .anonymous,
            .kind = kind,
            .owner = owner,
            .source_region = source_region,
        });
        try self.recordCallableTemplateSource(allocator, source, callable_template_id);

        return callable_template_id;
    }

    pub fn preRegisterCallableTemplateForDefExpr(
        self: *Result,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        closure_owner: CallableTemplateOwner,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        type_root: types.Var,
        binding_pattern: ?CIR.Pattern.Idx,
        additional_sources: []const CallableTemplateSource,
    ) Allocator.Error!?CallableTemplateId {
        const boundary = defExprCallableBoundary(all_module_envs, module_idx, expr_idx) orelse return null;
        const module_env = all_module_envs[module_idx];
        const template_id = try self.registerCallableTemplate(
            allocator,
            all_module_envs,
            closure_owner,
            .{ .expr = exprTemplateSource(module_idx, boundary.expr_idx) },
            module_idx,
            boundary.expr_idx,
            boundary.expr_idx,
            type_root,
            binding_pattern,
            boundary.kind,
            module_env.store.getExprRegion(boundary.expr_idx),
        );
        try self.recordAdditionalCallableTemplateSource(
            allocator,
            .{ .expr = exprTemplateSource(module_idx, expr_idx) },
            template_id,
        );
        for (additional_sources) |source| {
            try self.recordAdditionalCallableTemplateSource(allocator, source, template_id);
        }
        return template_id;
    }

    pub fn primeModuleCallableDefs(
        self: *Result,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        module_idx: u32,
    ) Allocator.Error!void {
        if (self.root_expr_spans_by_module.contains(module_idx)) return;
        const module_env = all_module_envs[module_idx];
        const defs = module_env.store.sliceDefs(module_env.all_defs);
        const root_expr_entries_start: u32 = @intCast(self.root_expr_entries.items.len);

        for (defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            try self.root_expr_entries.append(allocator, def.expr);
            _ = try self.preRegisterCallableTemplateForDefExpr(
                allocator,
                all_module_envs,
                .root_scope,
                module_idx,
                def.expr,
                ModuleEnv.varFrom(def.pattern),
                def.pattern,
                &.{
                    .{ .external_def = externalDefTemplateSource(module_idx, @intCast(@intFromEnum(def_idx))) },
                    .{ .local_pattern = localPatternTemplateSource(module_idx, def.pattern) },
                },
            );
        }

        try self.root_expr_spans_by_module.put(allocator, module_idx, .{
            .start = root_expr_entries_start,
            .len = @intCast(defs.len),
        });
    }
};
