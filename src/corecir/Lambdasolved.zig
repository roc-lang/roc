//! Solved lambda-set semantics over context-monomorphic source IR.
//!
//! This stage owns callable-set membership and solved capture provenance.
//! It must not emit executable MIR or LIR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const cm = @import("ContextMono.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const CIR = can.CIR;

pub const CallableSourceNamespace = enum(u2) {
    local_pattern = 0,
    external_def = 1,
    expr = 2,
};

pub fn packCallableSourceKey(namespace: CallableSourceNamespace, module_idx: u32, local_id: u32) u64 {
    if (std.debug.runtime_safety) {
        std.debug.assert(module_idx <= std.math.maxInt(u31));
        std.debug.assert(local_id <= std.math.maxInt(u31));
    }

    return (@as(u64, @intFromEnum(namespace)) << 62) |
        (@as(u64, module_idx) << 31) |
        @as(u64, local_id);
}

pub fn packLocalPatternSourceKey(module_idx: u32, pattern_idx: CIR.Pattern.Idx) u64 {
    return packCallableSourceKey(.local_pattern, module_idx, @intFromEnum(pattern_idx));
}

pub fn packExternalDefSourceKey(module_idx: u32, def_node_idx: u16) u64 {
    return packCallableSourceKey(.external_def, module_idx, def_node_idx);
}

pub fn packExprSourceKey(module_idx: u32, expr_idx: CIR.Expr.Idx) u64 {
    return packCallableSourceKey(.expr, module_idx, @intFromEnum(expr_idx));
}

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

pub const CallableTemplate = struct {
    source_key: u64,
    module_idx: u32,
    cir_expr: CIR.Expr.Idx,
    runtime_expr: CIR.Expr.Idx,
    type_root: types.Var,
    binding_pattern: ?CIR.Pattern.Idx = null,
    kind: CallableTemplateKind = .top_level_def,
    lexical_owner_template: ?CallableTemplateId = null,
    external_def: ?ExternalDefSource = null,
    source_region: Region = Region.zero(),
};

pub const DeferredLocalCallable = struct {
    pattern_idx: CIR.Pattern.Idx,
    cir_expr: CIR.Expr.Idx,
    module_idx: u32,
    source_key: u64,
    type_root: types.Var,
};

pub const ExprSource = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
};

pub const SourceContext = cm.SourceContext;
pub const ContextExprKey = cm.ContextExprKey;
pub const ContextPatternKey = cm.ContextPatternKey;

pub const Result = struct {
    callable_templates: std.ArrayListUnmanaged(CallableTemplate),
    source_exprs: std.AutoHashMapUnmanaged(u64, ExprSource),
    callable_template_ids_by_source: std.AutoHashMapUnmanaged(u64, CallableTemplateId),
    deferred_local_callables: std.AutoHashMapUnmanaged(u64, DeferredLocalCallable),

    pub fn init(allocator: Allocator) !Result {
        _ = allocator;
        return .{
            .callable_templates = .empty,
            .source_exprs = .empty,
            .callable_template_ids_by_source = .empty,
            .deferred_local_callables = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.callable_templates.deinit(allocator);
        self.source_exprs.deinit(allocator);
        self.callable_template_ids_by_source.deinit(allocator);
        self.deferred_local_callables.deinit(allocator);
    }

    pub fn getCallableTemplate(self: *const Result, callable_template_id: CallableTemplateId) *const CallableTemplate {
        return &self.callable_templates.items[@intFromEnum(callable_template_id)];
    }

    pub fn getLocalCallableTemplate(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getExternalCallableTemplate(self: *const Result, module_idx: u32, def_node_idx: u16) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(packExternalDefSourceKey(module_idx, def_node_idx));
    }

    pub fn getExprCallableTemplate(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(packExprSourceKey(module_idx, expr_idx));
    }

    pub fn getDeferredLocalCallable(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?DeferredLocalCallable {
        return self.deferred_local_callables.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getPatternSourceExpr(
        self: *const Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ExprSource {
        return self.source_exprs.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

};
