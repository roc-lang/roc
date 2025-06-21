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
/// Maps type names to their type declaration statements
type_decls: std.AutoHashMapUnmanaged(Ident.Idx, CIR.Statement.Idx),
is_function_boundary: bool,

/// Initialize the scope
pub fn init(is_function_boundary: bool) Scope {
    return Scope{
        .idents = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .aliases = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .type_decls = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Statement.Idx){},
        .is_function_boundary = is_function_boundary,
    };
}

/// Deinitialize the scope
pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
    self.idents.deinit(gpa);
    self.aliases.deinit(gpa);
    self.type_decls.deinit(gpa);
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

/// Result of looking up a type declaration
pub const TypeLookupResult = union(enum) {
    found: CIR.Statement.Idx,
    not_found: void,
};

/// Result of introducing an identifier
pub const IntroduceResult = union(enum) {
    success: void,
    shadowing_warning: CIR.Pattern.Idx, // The pattern that was shadowed
    top_level_var_error: void,
    var_across_function_boundary: CIR.Pattern.Idx,
};

/// Result of introducing a type declaration
/// TODO: Add more specific error types for different redeclaration scenarios
pub const TypeIntroduceResult = union(enum) {
    success: void,
    shadowing_warning: CIR.Statement.Idx, // The type declaration that was shadowed
    redeclared_error: CIR.Statement.Idx, // The type declaration that was redeclared
};

/// Item kinds in a scope
pub const ItemKind = enum { ident, alias, type_decl };

/// Get the appropriate map for the given item kind
pub fn items(scope: *Scope, comptime item_kind: ItemKind) switch (item_kind) {
    .ident, .alias => *std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
    .type_decl => *std.AutoHashMapUnmanaged(Ident.Idx, CIR.Statement.Idx),
} {
    return switch (item_kind) {
        .ident => &scope.idents,
        .alias => &scope.aliases,
        .type_decl => &scope.type_decls,
    };
}

/// Get the appropriate map for the given item kind (const version)
pub fn itemsConst(scope: *const Scope, comptime item_kind: ItemKind) switch (item_kind) {
    .ident, .alias => *const std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
    .type_decl => *const std.AutoHashMapUnmanaged(Ident.Idx, CIR.Statement.Idx),
} {
    return switch (item_kind) {
        .ident => &scope.idents,
        .alias => &scope.aliases,
        .type_decl => &scope.type_decls,
    };
}

/// Put an item in the scope, panics on OOM
pub fn put(scope: *Scope, gpa: std.mem.Allocator, comptime item_kind: ItemKind, name: Ident.Idx, value: switch (item_kind) {
    .ident, .alias => CIR.Pattern.Idx,
    .type_decl => CIR.Statement.Idx,
}) void {
    scope.items(item_kind).put(gpa, name, value) catch |err| collections.utils.exitOnOom(err);
}

/// Introduce a type declaration into the scope
pub fn introduceTypeDecl(scope: *Scope, gpa: std.mem.Allocator, ident_store: *const base.Ident.Store, name: Ident.Idx, type_decl: CIR.Statement.Idx) TypeIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.type_decls.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
            // Type redeclaration is an error, not just a warning
            return TypeIntroduceResult{ .redeclared_error = entry.value_ptr.* };
        }
    }

    // TODO: Check for shadowing in parent scopes and issue warnings
    // TODO: Implement proper type shadowing detection across scope levels
    scope.put(gpa, .type_decl, name, type_decl);
    return TypeIntroduceResult{ .success = {} };
}

/// Lookup a type declaration in the scope hierarchy
/// TODO: Optimize lookup performance - currently O(n) due to text comparison
/// TODO: Consider caching or using a more efficient data structure for type lookup
/// TODO: Support for nominal vs structural type distinction (future := operator)
pub fn lookupTypeDecl(scope: *const Scope, ident_store: *const base.Ident.Store, name: Ident.Idx) TypeLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.type_decls.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
            return TypeLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return TypeLookupResult{ .not_found = {} };
}
