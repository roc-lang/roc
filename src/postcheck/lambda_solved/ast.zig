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
    body: FnBody,
};

/// Body availability for a Lambda Solved function definition.
pub const FnBody = union(enum) {
    roc: Lifted.ExprId,
    hosted,
};

/// Runtime layout requested for a checked data value.
pub const LayoutRequest = struct {
    checked_type: @import("check").CheckedModule.CheckedTypeId,
    ty: Type.TypeVarId,
    fn_id: ?Lifted.FnId = null,
};

/// Runtime schema requested for a named runtime value shape.
pub const RuntimeSchemaRequest = struct {
    def: @import("../monotype/type.zig").TypeDef,
    ty: Type.TypeVarId,
};

/// Read-only Lambda Solved program view.
pub const ProgramView = struct {
    lifted: Lifted.ProgramView,
    types: Type.Store.View,
    defs: []const Def,
    local_tys: []const Type.TypeVarId,
    expr_tys: []const Type.TypeVarId,
    pat_tys: []const Type.TypeVarId,
    fn_tys: []const Type.TypeVarId,
    layout_requests: []const LayoutRequest,
    runtime_schema_requests: []const RuntimeSchemaRequest,
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
    layout_requests: std.ArrayList(LayoutRequest),
    runtime_schema_requests: std.ArrayList(RuntimeSchemaRequest),

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
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.fn_tys.deinit(self.allocator);
        self.pat_tys.deinit(self.allocator);
        self.expr_tys.deinit(self.allocator);
        self.local_tys.deinit(self.allocator);
        self.defs.deinit(self.allocator);
        self.types.deinit();
        self.lifted.deinit();
    }

    pub fn view(self: *const Program) ProgramView {
        return .{
            .lifted = self.lifted.view(),
            .types = self.types.view(),
            .defs = self.defs.items,
            .local_tys = self.local_tys.items,
            .expr_tys = self.expr_tys.items,
            .pat_tys = self.pat_tys.items,
            .fn_tys = self.fn_tys.items,
            .layout_requests = self.layout_requests.items,
            .runtime_schema_requests = self.runtime_schema_requests.items,
        };
    }
};

test "lambda solved ast declarations are referenced" {
    std.testing.refAllDecls(@This());
}
