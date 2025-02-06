//! A simplified mocking of the future `resolve_imports` compiler stage's artifacts,
//! which is roughly the artifacts of today's `roc_can` compiler stage.

const std = @import("std");
const base = @import("../base.zig");
const cols = @import("../collections.zig");
const problem = @import("../problem.zig");

const TypeVar = base.TypeVar;
const Region = base.Region;

// created from `Declarations`
pub const IR = struct {
    env: base.ModuleEnv,
    declarations: cols.SafeList(DeclarationTag),
    regions: cols.SafeList(base.Region),

    // utable: UnificationTable,
    // pub type_var_slices: Vec<TypeVarSubsSlice>,
    type_vars: []TypeVar,
    idents: base.IdentStore,
    // symbols: Vec<Symbol>,
    // symbol_regions: Vec<Region>,

    host_exposed_annotations: std.AutoHashMap(usize, TypeVar),

    function_bodies: cols.SafeList(FunctionDef),
    function_regions: cols.SafeList(Region),
    expressions: []Expr,
    expression_regions: []Region,
    destructs: []DestructureDef,
};

// pub const TypeVar =

pub const TypeContent = union(enum) {};

pub const Pattern = union(enum) {};

pub const Expr = union(enum) {};

pub const DestructureDef = union(enum) {};

pub const DeclarationTag = union(enum) {
    Value,
    Function: cols.SafeList(FunctionDef).Id,
    Recursive: cols.SafeList(FunctionDef).Id,
    TailRecursive: cols.SafeList(FunctionDef).Id,
    Destructure: cols.SafeList(DestructureDef).Id,
    MutualRecursion: struct {
        length: u16,
        cycle_mark: IllegalCycleMark,
    },
};

/// Marks whether a recursive let-cycle was determined to be illegal during solving.
pub const IllegalCycleMark = ?TypeVar;

pub const EarlyReturn = struct {
    type_var: TypeVar,
    region: Region,
    kind: Kind,

    const Kind = enum {
        Return,
        Try,
    };
};

pub const FunctionDef = struct {
    closure_type: TypeVar,
    return_type: TypeVar,
    fx_type: TypeVar,
    early_returns: std.ArrayList(EarlyReturn),
    captured_symbols: usize, // Vec<(Symbol, TypeVar)>,
    arguments: usize, //Vec<(TypeVar, Pattern, Region)>,
};
