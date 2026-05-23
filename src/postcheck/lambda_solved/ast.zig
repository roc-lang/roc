//! Lambda Solved IR.

const std = @import("std");
const Common = @import("../common.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Type = @import("type.zig");

/// Identifier for an expression type entry.
pub const ExprId = enum(u32) { _ };
/// Identifier for a solved function definition.
pub const DefId = enum(u32) { _ };

/// Lambda Solved function definition.
pub const Def = struct {
    symbol: Common.Symbol,
    ty: Type.TypeVarId,
    body: Lifted.ExprId,
};

/// Lambda Solved program plus the solved type store.
pub const Program = struct {
    allocator: std.mem.Allocator,
    lifted: Lifted.Program,
    types: Type.Store,
    defs: std.ArrayList(Def),
    local_tys: std.ArrayList(Type.TypeVarId),
    expr_tys: std.ArrayList(Type.TypeVarId),
    pat_tys: std.ArrayList(Type.TypeVarId),
    fn_tys: std.ArrayList(Type.TypeVarId),

    pub fn init(allocator: std.mem.Allocator, lifted: Lifted.Program) Program {
        return .{
            .allocator = allocator,
            .lifted = lifted,
            .types = Type.Store.init(allocator),
            .defs = .empty,
            .local_tys = .empty,
            .expr_tys = .empty,
            .pat_tys = .empty,
            .fn_tys = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.fn_tys.deinit(self.allocator);
        self.pat_tys.deinit(self.allocator);
        self.expr_tys.deinit(self.allocator);
        self.local_tys.deinit(self.allocator);
        self.defs.deinit(self.allocator);
        self.types.deinit();
        self.lifted.deinit();
    }
};

test "lambda solved ast declarations are referenced" {
    std.testing.refAllDecls(@This());
}
