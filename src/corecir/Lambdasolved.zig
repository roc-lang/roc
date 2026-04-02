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
