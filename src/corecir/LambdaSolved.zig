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

pub const CallableTemplateId = enum(u32) {
    _,

    pub const none: CallableTemplateId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CallableTemplateId) bool {
        return self == none;
    }
};

pub const CallableTemplateKind = enum {
    top_level_def,
    lambda,
    closure,
    hosted_lambda,
};

pub const CallableTemplate = struct {
    source_key: u64,
    module_idx: u32,
    cir_expr: CIR.Expr.Idx,
    type_root: types.Var,
    binding_pattern: ?CIR.Pattern.Idx = null,
    kind: CallableTemplateKind = .top_level_def,
    lexical_owner_template: CallableTemplateId = .none,
    source_region: Region = Region.zero(),
};

pub const CapturePlanId = enum(u32) {
    _,

    pub const none: CapturePlanId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CapturePlanId) bool {
        return self == none;
    }
};

pub const LambdaSetMemberId = enum(u32) {
    _,

    pub const none: LambdaSetMemberId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: LambdaSetMemberId) bool {
        return self == none;
    }
};

pub const CaptureSource = union(enum) {
    lexical_pattern: struct {
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    },
    parent_capture: struct {
        capture_plan: CapturePlanId,
        capture_index: u16,
    },
    top_level_def: struct {
        module_idx: u32,
        def_idx: CIR.Def.Idx,
    },
    source_expr: struct {
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    },
    exact_callable: struct {
        member: LambdaSetMemberId,
    },
};

pub const CaptureEntry = struct {
    source: CaptureSource,
    monotype: cm.ResolvedMonotype,
};

pub const CaptureEntrySpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CaptureEntrySpan {
        return .{ .start = 0, .len = 0 };
    }
};

pub const CapturePlan = struct {
    entries: CaptureEntrySpan,
};

pub const SolvedCallableKind = enum {
    direct,
    closure,
    hosted,
};

pub const LambdaSetMember = struct {
    template: CallableTemplateId,
    context_id: cm.ContextId,
    fn_monotype: cm.ResolvedMonotype,
    capture_plan: CapturePlanId = .none,
    kind: SolvedCallableKind,
};

pub const LambdaSetId = enum(u32) {
    _,

    pub const none: LambdaSetId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: LambdaSetId) bool {
        return self == none;
    }
};

pub const LambdaSetMemberSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LambdaSetMemberSpan {
        return .{ .start = 0, .len = 0 };
    }
};

pub const LambdaSet = struct {
    members: LambdaSetMemberSpan,
};

pub const ContextExprKey = cm.ContextExprKey;
pub const ContextPatternKey = cm.ContextPatternKey;

pub const Result = struct {
    callable_templates: std.ArrayListUnmanaged(CallableTemplate),
    capture_entries: std.ArrayListUnmanaged(CaptureEntry),
    capture_plans: std.ArrayListUnmanaged(CapturePlan),
    lambda_set_members: std.ArrayListUnmanaged(LambdaSetMember),
    lambda_set_member_entries: std.ArrayListUnmanaged(LambdaSetMemberId),
    lambda_sets: std.ArrayListUnmanaged(LambdaSet),
    expr_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    call_site_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    lookup_expr_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    context_pattern_lambda_sets: std.AutoHashMapUnmanaged(ContextPatternKey, LambdaSetId),
    root_module_idx: u32,
    root_source_expr_idx: ?CIR.Expr.Idx,

    pub fn init(root_module_idx: u32, root_source_expr_idx: ?CIR.Expr.Idx) Result {
        return .{
            .callable_templates = .empty,
            .capture_entries = .empty,
            .capture_plans = .empty,
            .lambda_set_members = .empty,
            .lambda_set_member_entries = .empty,
            .lambda_sets = .empty,
            .expr_lambda_sets = .empty,
            .call_site_lambda_sets = .empty,
            .lookup_expr_lambda_sets = .empty,
            .context_pattern_lambda_sets = .empty,
            .root_module_idx = root_module_idx,
            .root_source_expr_idx = root_source_expr_idx,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.callable_templates.deinit(allocator);
        self.capture_entries.deinit(allocator);
        self.capture_plans.deinit(allocator);
        self.lambda_set_members.deinit(allocator);
        self.lambda_set_member_entries.deinit(allocator);
        self.lambda_sets.deinit(allocator);
        self.expr_lambda_sets.deinit(allocator);
        self.call_site_lambda_sets.deinit(allocator);
        self.lookup_expr_lambda_sets.deinit(allocator);
        self.context_pattern_lambda_sets.deinit(allocator);
    }
};
