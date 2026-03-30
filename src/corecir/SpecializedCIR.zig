//! Structured executable IR after lambda-set specialization.
//!
//! This is the input to strongest-form MIR lowering.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const cm = @import("ContextMono.zig");
const ls = @import("LambdaSolved.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const CIR = can.CIR;

pub const CallableDefId = enum(u32) {
    _,

    pub const none: CallableDefId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CallableDefId) bool {
        return self == none;
    }
};

pub const RuntimeCallableKind = enum {
    direct,
    closure,
};

pub const CaptureField = struct {
    source: ls.CaptureSource,
    monotype: cm.ResolvedMonotype,
};

pub const CaptureFieldSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CaptureFieldSpan {
        return .{ .start = 0, .len = 0 };
    }
};

pub const CallableDef = struct {
    source_member: ls.LambdaSetMemberId,
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    fn_monotype: cm.ResolvedMonotype,
    captures: CaptureFieldSpan = .empty(),
    callable_kind: RuntimeCallableKind,
    source_region: Region,
};

pub const Program = struct {
    callable_defs: std.ArrayListUnmanaged(CallableDef),
    capture_fields: std.ArrayListUnmanaged(CaptureField),
    root_module_idx: u32,
    root_source_expr_idx: ?CIR.Expr.Idx,

    pub fn init(root_module_idx: u32, root_source_expr_idx: ?CIR.Expr.Idx) Program {
        return .{
            .callable_defs = .empty,
            .capture_fields = .empty,
            .root_module_idx = root_module_idx,
            .root_source_expr_idx = root_source_expr_idx,
        };
    }

    pub fn deinit(self: *Program, allocator: Allocator) void {
        self.callable_defs.deinit(allocator);
        self.capture_fields.deinit(allocator);
    }
};
