//! Executable MIR logical layout publication records.

const std = @import("std");
const layout_mod = @import("layout");
const repr = @import("../lambda_solved/mod.zig").Representation;

const Ast = @import("ast.zig");
const Type = @import("type.zig");

pub const LayoutPublicationKey = struct {
    executable_ty: Type.TypeId,
};

pub const Layouts = struct {
    graph: layout_mod.Graph,
    type_layouts: std.AutoHashMap(Type.TypeId, layout_mod.GraphRef),

    pub fn init(allocator: std.mem.Allocator) Layouts {
        return .{
            .graph = layout_mod.Graph.init(allocator),
            .type_layouts = std.AutoHashMap(Type.TypeId, layout_mod.GraphRef).init(allocator),
        };
    }

    pub fn deinit(self: *Layouts) void {
        self.type_layouts.deinit();
        self.graph.deinit();
    }
};

pub const CallLoweringLayoutInputs = struct {
    callable_set_keys: []const repr.CanonicalCallableSetKey = &.{},
    erased_signature_keys: []const repr.ErasedFnSigKey = &.{},
    executable_values: []const Ast.ExecutableValueRef = &.{},
};

test "executable layout publication records are source-blind" {
    std.testing.refAllDecls(@This());
}
