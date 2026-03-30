//! Structured normalized CIR.
//!
//! This stage removes frontend sugar and normalizes structured source forms
//! before contextual monotype determination and lambda-set solving.

const std = @import("std");
const can = @import("can");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

pub const RootKind = enum {
    expr,
    named_def,
};

pub const Root = struct {
    module_idx: u32,
    kind: RootKind,
    expr_idx: CIR.Expr.Idx,
};

/// Normalized structured source program.
pub const Program = struct {
    all_module_envs: []const *const ModuleEnv,
    roots: []const Root,

    pub fn deinit(_: *Program, _: Allocator) void {}
};
