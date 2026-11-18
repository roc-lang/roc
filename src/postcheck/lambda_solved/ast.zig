//! Lambda Solved IR.

const std = @import("std");
const collections = @import("collections");
const Common = @import("../common.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Type = @import("type.zig");
const Instances = @import("instances.zig");

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
    const_locator: ?@import("check").CheckedModule.ConstLocator = null,
};

/// Runtime schema requested for a named runtime value shape.
pub const RuntimeSchemaRequest = struct {
    def: @import("../monotype/type.zig").TypeDef,
    ty: Type.TypeVarId,
};

/// One instantiation of a binding group: the images, at one reference, of
/// the group's scheme nodes (`Scheme`). The first `group count` rows are
/// the groups' identity instantiations, whose images are the scheme nodes
/// themselves.
pub const Instantiation = struct {
    group: u32,
    /// The function whose body holds the reference this instantiation was
    /// made for, or whose body a member naming it was copied into; null for
    /// an identity instantiation.
    context: ?Lifted.FnId,
    /// Start of this instantiation's images in `Polymorphism.images`, one
    /// per scheme node of its group, in scheme order; null for an identity
    /// instantiation.
    images: ?u32,
};

/// A binding group's scheme: the nodes of its function types that every
/// reference copies (design.md "Lambda Set Generalization"). Empty for a
/// monomorphic group, whose references unify with its types directly.
pub const Scheme = struct {
    /// `Polymorphism.scheme_nodes[nodes_start..][0..nodes_len]`.
    nodes_start: u32 = 0,
    nodes_len: u32 = 0,
    /// `Polymorphism.fn_roots[fn_roots_start..][0..group size]`: the scheme
    /// position of each group function's type, in group order.
    fn_roots_start: u32 = 0,

    pub fn isGeneric(self: Scheme) bool {
        return self.nodes_len != 0;
    }
};

/// The generalization Lambda Solving produced, which instance discovery
/// (`instances.zig`) reads.
pub const Polymorphism = struct {
    /// One scheme per binding group, in group order.
    schemes: std.ArrayList(Scheme) = .empty,
    scheme_nodes: std.ArrayList(Type.TypeVarId) = .empty,
    fn_roots: std.ArrayList(u32) = .empty,
    instantiations: std.ArrayList(Instantiation) = .empty,
    images: std.ArrayList(Type.TypeVarId) = .empty,
    /// The instantiation each reference to a function of a generalized group
    /// (`fn_ref` or direct `call_proc`) was solved at.
    expr_insts: collections.DenseMapUnmanaged(Lifted.ExprId, Type.InstId) = .empty,
    /// The binding group of each lifted function.
    fn_groups: std.ArrayList(u32) = .empty,

    pub fn deinit(self: *Polymorphism, allocator: std.mem.Allocator) void {
        self.fn_groups.deinit(allocator);
        self.expr_insts.deinit(allocator);
        self.images.deinit(allocator);
        self.instantiations.deinit(allocator);
        self.fn_roots.deinit(allocator);
        self.scheme_nodes.deinit(allocator);
        self.schemes.deinit(allocator);
    }

    pub fn identity(group: u32) Type.InstId {
        return @fromBackingInt(group);
    }

    pub fn isIdentity(self: *const Polymorphism, inst: Type.InstId) bool {
        return @backingInt(inst) < self.schemes.items.len;
    }

    pub fn schemeNodes(self: *const Polymorphism, scheme: Scheme) []const Type.TypeVarId {
        return self.scheme_nodes.items[scheme.nodes_start..][0..scheme.nodes_len];
    }

    /// The images of a non-identity instantiation, in scheme order.
    pub fn instImages(self: *const Polymorphism, inst: Type.InstId) []const Type.TypeVarId {
        const row = self.instantiations.items[@backingInt(inst)];
        const start = row.images orelse unreachable; // identity instantiations have no image row; callers ask only for a generalized group's references
        return self.images.items[start..][0..self.schemes.items[row.group].nodes_len];
    }
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
    binding_groups: Lifted.BindingGroups,
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
    /// The lifted program's binding groups, callee-first, in the order the
    /// solver solved them (`Lifted.Program.bindingGroups`).
    binding_groups: Lifted.BindingGroups = .empty,
    /// Schemes and instantiations.
    poly: Polymorphism = .{},
    /// Which instantiations of each generalized group are one instance
    /// (`instances.zig`), computed by the solver's last step.
    instances: Instances.Table = .{},

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
        self.instances.deinit(self.allocator);
        self.poly.deinit(self.allocator);
        self.binding_groups.deinit(self.allocator);
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

    /// Drop every solved type and table, keeping the lifted program and its
    /// binding groups, so the program can be solved again from the start.
    pub fn resetSolution(self: *Program) void {
        self.types.deinit();
        self.types = Type.Store.init(self.allocator);
        self.defs.clearRetainingCapacity();
        self.local_tys.clearRetainingCapacity();
        self.expr_tys.clearRetainingCapacity();
        self.pat_tys.clearRetainingCapacity();
        self.fn_tys.clearRetainingCapacity();
        self.layout_requests.clearRetainingCapacity();
        self.runtime_schema_requests.clearRetainingCapacity();
        self.poly.deinit(self.allocator);
        self.poly = .{};
        self.instances.deinit(self.allocator);
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
            .binding_groups = self.binding_groups,
        };
    }
};

test "lambda solved ast declarations are referenced" {
    std.testing.refAllDecls(@This());
}
