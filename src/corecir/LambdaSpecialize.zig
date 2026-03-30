//! Specialization from solved lambda sets to executable structured callable IR.

const std = @import("std");
const CoreCIR = @import("CoreCIR.zig");
const ContextMono = @import("ContextMono.zig");
const LambdaSolved = @import("LambdaSolved.zig");
const SpecializedCIR = @import("SpecializedCIR.zig");
const Monotype = @import("../mir/Monotype.zig");

const Allocator = std.mem.Allocator;

/// Identifies one executable callable specialization chosen from solved
/// lambda-set semantics.
pub const CallableInstId = enum(u32) {
    _,

    pub const none: CallableInstId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CallableInstId) bool {
        return self == none;
    }
};

/// One structural projection applied before demanding executable callable
/// specializations from a higher-order parameter.
pub const CallableParamProjection = union(enum) {
    field: Monotype.Name,
    tuple_elem: u32,
};

pub const CallableParamProjectionSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableParamProjectionSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableParamProjectionSpan) bool {
        return self.len == 0;
    }
};

pub const CallableInstSetId = enum(u32) {
    _,

    pub const none: CallableInstSetId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CallableInstSetId) bool {
        return self == none;
    }
};

pub const CallableParamSpecEntry = struct {
    param_index: u16,
    projections: CallableParamProjectionSpan = .empty(),
    callable_inst_set_id: CallableInstSetId,
};

pub const CallableParamSpecSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableParamSpecSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableParamSpecSpan) bool {
        return self.len == 0;
    }
};

pub const CallableInst = struct {
    member: LambdaSolved.LambdaSetMemberId,
    subst: ContextMono.TypeSubstId,
    fn_monotype: ContextMono.ResolvedMonotype,
    defining_context_id: ContextMono.ContextId,
    callable_param_specs: CallableParamSpecSpan = .empty(),
};

pub const CallableInstSetSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableInstSetSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableInstSetSpan) bool {
        return self.len == 0;
    }
};

pub const CallableInstSet = struct {
    members: CallableInstSetSpan,
};

pub const StageResult = struct {
    corecir: CoreCIR.Program,
    context_mono: ContextMono.Result,
    lambda_solved: LambdaSolved.Result,
    specialized: SpecializedCIR.Program,

    pub fn deinit(self: *StageResult, allocator: Allocator) void {
        self.specialized.deinit(allocator);
        self.lambda_solved.deinit(allocator);
        self.context_mono.deinit(allocator);
        self.corecir.deinit(allocator);
    }
};
