const region = @import("base/region.zig");
const symbol = @import("base/symbol.zig");
const module = @import("base/module.zig");
const package = @import("base/package.zig");
const primitive = @import("base/primitive.zig");
const env = @import("base/env.zig");

pub const Region = region.Region;
pub const Position = region.Position;
pub const LineAndColumn = region.LineAndColumn;

pub const Ident = symbol.Ident;
pub const IdentAttributes = symbol.IdentAttributes;
pub const IdentProblems = symbol.IdentProblems;
pub const IdentId = symbol.IdentId;
pub const IdentStore = symbol.IdentStore;
pub const Symbol = symbol.Symbol;
pub const SymbolStore = symbol.SymbolStore;

pub const Module = module.Module;
pub const ModuleId = module.ModuleId;
pub const ModuleStore = module.ModuleStore;

pub const Package = package.Package;
pub const PackageId = package.PackageId;
pub const PackageStore = package.PackageStore;

pub const Primitive = primitive.Primitive;
pub const Literal = primitive.Literal;

pub const ModuleEnv = env.ModuleEnv;
pub const GlobalEnv = env.GlobalEnv;

pub const Recursive = enum {
    NotRecursive,
    Recursive,
    TailRecursive,
};

// TODO: can this be smaller than u32?
/// Source of crash, and its runtime representation to roc_panic.
pub const CrashOrigin = enum(u32) {
    /// The crash is due to Roc, either via a builtin or type error.
    Roc = 0,
    /// The crash is user-defined.
    User = 1,
};

pub const LowLevel = .{};

// TODO: move to relevant stages
pub const TypeVar = struct { id: u32 };
