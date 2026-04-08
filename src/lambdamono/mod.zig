//! Cor-style executable monomorphic program.

const std = @import("std");

pub const Type = @import("type.zig");
pub const Ast = @import("ast.zig");
pub const LayoutFacts = @import("layout_facts.zig");
pub const LowerType = @import("lower_type.zig");
pub const Specializations = @import("specializations.zig");
pub const Lower = @import("lower.zig");

test "lambdamono tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Type);
    std.testing.refAllDecls(Ast);
    std.testing.refAllDecls(LayoutFacts);
    std.testing.refAllDecls(LowerType);
    std.testing.refAllDecls(Specializations);
    std.testing.refAllDecls(Lower);
}
