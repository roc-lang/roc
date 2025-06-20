//! Scope management for identifier resolution during canonicalization.

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const CIR = @import("CIR.zig");

const Ident = base.Ident;

const Scope = @This();

/// Maps an Ident to a Pattern in the Can IR
idents: std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
aliases: std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
is_function_boundary: bool,

/// Initialize the scope
pub fn init(is_function_boundary: bool) Scope {
    return Scope{
        .idents = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .aliases = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .is_function_boundary = is_function_boundary,
    };
}

/// Deinitialize the scope
pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
    self.idents.deinit(gpa);
    self.aliases.deinit(gpa);
}

/// Scope management types and structures
pub const Error = error{
    NotInScope,
    AlreadyInScope,
    ExitedTopScopeLevel,
    TopLevelVarError,
    VarAcrossFunctionBoundary,
};

/// Result of looking up an identifier
pub const LookupResult = union(enum) {
    found: CIR.Pattern.Idx,
    not_found: void,
};

/// Result of introducing an identifier
pub const IntroduceResult = union(enum) {
    success: void,
    shadowing_warning: CIR.Pattern.Idx, // The pattern that was shadowed
    top_level_var_error: void,
    var_across_function_boundary: CIR.Pattern.Idx,
};

/// Item kinds in a scope
pub const ItemKind = enum { ident, alias };

/// Get the appropriate map for the given item kind
pub fn items(scope: *Scope, comptime item_kind: ItemKind) *std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx) {
    return switch (item_kind) {
        .ident => &scope.idents,
        .alias => &scope.aliases,
    };
}

/// Get the appropriate map for the given item kind (const version)
pub fn itemsConst(scope: *const Scope, comptime item_kind: ItemKind) *const std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx) {
    return switch (item_kind) {
        .ident => &scope.idents,
        .alias => &scope.aliases,
    };
}

/// Put an item in the scope, panics on OOM
pub fn put(scope: *Scope, gpa: std.mem.Allocator, comptime item_kind: ItemKind, name: Ident.Idx, pattern: CIR.Pattern.Idx) void {
    scope.items(item_kind).put(gpa, name, pattern) catch |err| collections.utils.exitOnOom(err);
}
