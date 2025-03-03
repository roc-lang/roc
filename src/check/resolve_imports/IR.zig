//! A simplified mocking of the future `resolve_imports` compiler stage's artifacts,
//! which is roughly the artifacts of today's `roc_can` compiler stage.
const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Region = base.Region;
const Type = types.Type;
const CanIR = @import("../canonicalize/IR.zig");

const Self = @This();

env: *base.ModuleEnv,
regions: Region.List,
exprs: Expr.List,
destructs: DestructureDef.List,
function_bodies: FunctionDef.List,
function_args: FunctionDef.Arg.List,
type_indices: collections.SafeList(Type.Idx),
declarations: DeclarationTag.List,
host_exposed_annotations: std.AutoHashMap(usize, Type.Idx),

pub fn init(env: *base.ModuleEnv, gpa: std.mem.Allocator) Self {
    return Self{
        .env = env,
        .regions = Region.List.init(gpa),
        .exprs = Expr.List.init(gpa),
        .destructs = DestructureDef.List.init(gpa),
        .function_bodies = FunctionDef.List.init(gpa),
        .function_args = FunctionDef.Arg.List.init(gpa),
        .type_indices = collections.SafeList(Type.Idx).init(gpa),
        .declarations = DeclarationTag.List.init(gpa),
        .host_exposed_annotations = std.AutoHashMap(usize, Type.Idx).init(gpa),
    };
}

pub fn deinit(self: *Self) void {
    self.regions.deinit();
    self.exprs.deinit();
    self.destructs.deinit();
    self.function_bodies.deinit();
    self.function_args.deinit();
    self.type_indices.deinit();
    self.declarations.deinit();
    self.host_exposed_annotations.deinit();
}

pub const Expr = union(enum) {
    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
};

pub const Pattern = union(enum) {
    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
};

pub const DestructureDef = union(enum) {
    pub const List = collections.SafeList(@This());
};

pub const DeclarationTag = union(enum) {
    Value,
    Function: collections.SafeList(FunctionDef).Idx,
    Recursive: collections.SafeList(FunctionDef).Idx,
    TailRecursive: collections.SafeList(FunctionDef).Idx,
    Destructure: collections.SafeList(DestructureDef).Idx,
    MutualRecursion: struct {
        length: u16,
        cycle_mark: IllegalCycleMark,
    },

    pub const List = collections.SafeList(@This());
};

/// Marks whether a recursive let-cycle was determined to be illegal during solving.
pub const IllegalCycleMark = ?Type.Idx;

pub const FunctionDef = struct {
    closure_type: Type.Idx,
    return_type: Type.Idx,
    fx_type: Type.Idx,
    // early_returns: std.ArrayList(CanIR.EarlyReturn),
    arguments: Arg.Slice,

    pub const Arg = struct {
        type: Type.Idx,
        pattern: Pattern.Idx,
        region: Region,

        pub const List = collections.SafeMultiList(@This());
        pub const Slice = Arg.List.Slice;
    };

    const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
};
