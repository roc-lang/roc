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

/// One monomorphic callable specialization referenced by solved lambda sets.
pub const CallableInstId = enum(u32) {
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
    type_root: types.Var,
    binding: CallableTemplateBinding,
    kind: CallableTemplateKind,
    owner: CallableTemplateOwner,
    source_region: Region = Region.zero(),
};

pub const SourceContext = cm.SourceContext;
pub const ContextExprKey = cm.ContextExprKey;
pub const ContextPatternKey = cm.ContextPatternKey;

pub const Result = struct {
    callable_templates: std.ArrayListUnmanaged(CallableTemplate),
    callable_template_ids_by_source: std.AutoHashMapUnmanaged(CallableTemplateSource, CallableTemplateId),

    pub fn init(allocator: Allocator) !Result {
        _ = allocator;
        return .{
            .callable_templates = .empty,
            .callable_template_ids_by_source = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.callable_templates.deinit(allocator);
        self.callable_template_ids_by_source.deinit(allocator);
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

    pub fn getExprCallableTemplate(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(.{
            .expr = .{
                .module_idx = module_idx,
                .expr_idx = expr_idx,
            },
        });
    }

};
