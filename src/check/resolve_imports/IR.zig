//! A simplified mocking of the future `resolve_imports` compiler stage's artifacts,
//! which is roughly the artifacts of today's `roc_can` compiler stage.
const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Region = base.Region;
const TypeVar = types.TypeVar;
const CanIR = @import("../canonicalize/IR.zig");

const Self = @This();

// utable: UnificationTable,
// pub type_var_slices: Vec<TypeVarSubsSlice>,
env: *base.ModuleEnv,
regions: Region.List,
exprs: Expr.List,
destructs: DestructureDef.List,
function_bodies: FunctionDef.List,
function_args: FunctionDef.Arg.List,
type_vars: collections.SafeList(TypeVar),
declarations: DeclarationTag.List,
host_exposed_annotations: std.AutoHashMap(usize, TypeVar),

pub fn init(env: *base.ModuleEnv, allocator: std.mem.Allocator) Self {
    return Self{
        .env = env,
        .regions = Region.List.init(allocator),
        .exprs = Expr.List.init(allocator),
        .destructs = DestructureDef.List.init(allocator),
        .function_bodies = FunctionDef.List.init(allocator),
        .function_args = FunctionDef.Arg.List.init(allocator),
        .type_vars = collections.SafeList(TypeVar).init(allocator),
        .declarations = DeclarationTag.List.init(allocator),
        .host_exposed_annotations = std.AutoHashMap(usize, TypeVar).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.regions.deinit();
    self.exprs.deinit();
    self.destructs.deinit();
    self.function_bodies.deinit();
    self.function_args.deinit();
    self.type_vars.deinit();
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
pub const IllegalCycleMark = ?TypeVar;

pub const FunctionDef = struct {
    closure_type: TypeVar,
    return_type: TypeVar,
    fx_type: TypeVar,
    // early_returns: std.ArrayList(CanIR.EarlyReturn),
    arguments: Arg.Slice,

    pub const Arg = struct {
        type: TypeVar,
        pattern: Pattern.Idx,
        region: Region,

        pub const List = collections.SafeMultiList(@This());
        pub const Slice = Arg.List.Slice;
    };

    const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
};
