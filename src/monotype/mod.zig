//! Cor-style monotype lowering from solved checker facts.

const std = @import("std");

pub const Type = @import("type.zig");
pub const Ast = @import("ast.zig");
pub const SemanticFacts = @import("semantic_facts.zig");
pub const Lower = @import("lower.zig");
pub const Specializations = @import("specializations.zig");

test "monotype tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Type);
    std.testing.refAllDecls(Ast);
    std.testing.refAllDecls(SemanticFacts);
    std.testing.refAllDecls(Lower);
    std.testing.refAllDecls(Specializations);
}
