//! Solved lambda-set semantics over context-monomorphic source IR.
//!
//! This stage owns callable-set membership and solved capture provenance.
//! It must not emit executable MIR or LIR.

const std = @import("std");
const can = @import("can");
const cm = @import("ContextMono.zig");
const TemplateCatalog = @import("TemplateCatalog.zig");

const CIR = can.CIR;
pub const ExprTemplateSource = TemplateCatalog.ExprTemplateSource;
pub const LocalPatternTemplateSource = TemplateCatalog.LocalPatternTemplateSource;
pub const CallableTemplateId = TemplateCatalog.CallableTemplateId;

/// One monomorphic callable specialization referenced by solved lambda sets.
pub const CallableInstId = enum(u32) {
    _,
};

pub const CallableTemplateKind = TemplateCatalog.CallableTemplateKind;
pub const ExternalDefSource = TemplateCatalog.ExternalDefSource;
pub const CallableTemplateSource = TemplateCatalog.CallableTemplateSource;
pub const CallableTemplateBinding = TemplateCatalog.CallableTemplateBinding;
pub const CallableTemplateOwner = TemplateCatalog.CallableTemplateOwner;
pub const CallableTemplate = TemplateCatalog.CallableTemplate;

pub const SourceContext = cm.SourceContext;
pub const ContextExprKey = cm.ContextExprKey;
pub const ContextPatternKey = cm.ContextPatternKey;

pub const CallResultCallableInstKey = struct {
    context_expr: ContextExprKey,
    callee_callable_inst_raw: u32,
};

pub const ValueDefResolutionState = struct {
    in_progress_exprs: std.AutoHashMapUnmanaged(ContextExprKey, void),

    pub fn init() ValueDefResolutionState {
        return .{ .in_progress_exprs = .empty };
    }

    pub fn deinit(self: *ValueDefResolutionState, allocator: std.mem.Allocator) void {
        self.in_progress_exprs.deinit(allocator);
    }

    pub fn clear(self: *ValueDefResolutionState) void {
        self.in_progress_exprs.clearRetainingCapacity();
    }
};

pub const CallResultResolutionState = struct {
    in_progress_calls: std.AutoHashMapUnmanaged(CallResultCallableInstKey, void),

    pub fn init() CallResultResolutionState {
        return .{ .in_progress_calls = .empty };
    }

    pub fn deinit(self: *CallResultResolutionState, allocator: std.mem.Allocator) void {
        self.in_progress_calls.deinit(allocator);
    }

    pub fn clear(self: *CallResultResolutionState) void {
        self.in_progress_calls.clearRetainingCapacity();
    }
};

pub const Result = struct {
    pub fn init(allocator: std.mem.Allocator) !Result {
        _ = allocator;
        return .{};
    }

    pub fn deinit(self: *Result, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }
};
