//! Cor-style executable monomorphic program.

const std = @import("std");

pub const Type = @import("type.zig");
pub const Ast = @import("ast.zig");
pub const Layouts = @import("layouts.zig");
pub const Plan = @import("plan.zig");
pub const ExecPlan = @import("exec_plan.zig");
pub const Emit = @import("emit.zig");
pub const Lower = @import("lower.zig");

test "lambdamono tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Type);
    std.testing.refAllDecls(Ast);
    std.testing.refAllDecls(Layouts);
    std.testing.refAllDecls(Plan);
    std.testing.refAllDecls(ExecPlan);
    std.testing.refAllDecls(Emit);
    std.testing.refAllDecls(Lower);
}
