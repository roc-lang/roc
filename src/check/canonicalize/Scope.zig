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
/// Maps type variables to their type annotation indices
type_vars: std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx),
/// Maps module alias names to their full module names
module_aliases: std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx),
/// Maps exposed item names to their source modules and original names (for import resolution)
exposed_items: std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
is_function_boundary: bool,

/// Initialize the scope
pub fn init(is_function_boundary: bool) Scope {
    return Scope{
        .idents = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .aliases = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .type_decls = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Statement.Idx){},
        .type_vars = std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx){},
        .module_aliases = std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx){},
        .exposed_items = std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo){},
        .is_function_boundary = is_function_boundary,
    };
}

/// Deinitialize the scope
pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
    self.idents.deinit(gpa);
    self.aliases.deinit(gpa);
    self.type_decls.deinit(gpa);
    self.type_vars.deinit(gpa);
    self.module_aliases.deinit(gpa);
    self.exposed_items.deinit(gpa);
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

/// Result of looking up a type variable
pub const TypeVarLookupResult = union(enum) {
    found: CIR.TypeAnno.Idx,
    not_found: void,
};

/// Result of looking up a module alias
pub const ModuleAliasLookupResult = union(enum) {
    found: Ident.Idx,
    not_found: void,
};

/// Information about an exposed item
pub const ExposedItemInfo = struct {
    module_name: Ident.Idx,
    original_name: Ident.Idx,
};

/// Result of looking up an exposed item
pub const ExposedItemLookupResult = union(enum) {
    found: ExposedItemInfo,
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
pub const TypeIntroduceResult = union(enum) {
    success: void,
    shadowing_warning: CIR.Statement.Idx, // The type declaration that was shadowed
    redeclared_error: CIR.Statement.Idx, // The type declaration that was redeclared
    type_alias_redeclared: CIR.Statement.Idx, // The type alias that was redeclared
    nominal_type_redeclared: CIR.Statement.Idx, // The nominal type that was redeclared
    cross_scope_shadowing: CIR.Statement.Idx, // Type shadowed across different scopes
    parameter_conflict: struct {
        original_stmt: CIR.Statement.Idx,
        conflicting_parameter: base.Ident.Idx,
    },
};

/// Result of introducing a type variable
pub const TypeVarIntroduceResult = union(enum) {
    success: void,
    shadowing_warning: CIR.TypeAnno.Idx, // The type variable that was shadowed
    already_in_scope: CIR.TypeAnno.Idx, // The type variable already exists in this scope
};

/// Result of introducing a module alias
pub const ModuleAliasIntroduceResult = union(enum) {
    success: void,
    shadowing_warning: Ident.Idx, // The module alias that was shadowed
    already_in_scope: Ident.Idx, // The module alias already exists in this scope
};

/// Result of introducing an exposed item
pub const ExposedItemIntroduceResult = union(enum) {
    success: void,
    shadowing_warning: ExposedItemInfo, // The exposed item that was shadowed
    already_in_scope: ExposedItemInfo, // The exposed item already exists in this scope
};

/// Item kinds in a scope
pub const ItemKind = enum { ident, alias, type_decl, type_var, module_alias, exposed_item };

/// Get the appropriate map for the given item kind
pub fn items(scope: *Scope, comptime item_kind: ItemKind) switch (item_kind) {
    .ident, .alias => *std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
    .type_decl => *std.AutoHashMapUnmanaged(Ident.Idx, CIR.Statement.Idx),
    .type_var => *std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx),
    .module_alias => *std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx),
    .exposed_item => *std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
} {
    return switch (item_kind) {
        .ident => &scope.idents,
        .alias => &scope.aliases,
        .type_decl => &scope.type_decls,
        .type_var => &scope.type_vars,
        .module_alias => &scope.module_aliases,
        .exposed_item => &scope.exposed_items,
    };
}

/// Get the appropriate map for the given item kind (const version)
pub fn itemsConst(scope: *const Scope, comptime item_kind: ItemKind) switch (item_kind) {
    .ident, .alias => *const std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
    .type_decl => *const std.AutoHashMapUnmanaged(Ident.Idx, CIR.Statement.Idx),
    .type_var => *const std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx),
    .module_alias => *const std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx),
    .exposed_item => *const std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
} {
    return switch (item_kind) {
        .ident => &scope.idents,
        .alias => &scope.aliases,
        .type_decl => &scope.type_decls,
        .type_var => &scope.type_vars,
        .module_alias => &scope.module_aliases,
        .exposed_item => &scope.exposed_items,
    };
}

/// Put an item in the scope, panics on OOM
pub fn put(scope: *Scope, gpa: std.mem.Allocator, comptime item_kind: ItemKind, name: Ident.Idx, value: switch (item_kind) {
    .ident, .alias => CIR.Pattern.Idx,
    .type_decl => CIR.Statement.Idx,
    .type_var => CIR.TypeAnno.Idx,
    .module_alias => Ident.Idx,
    .exposed_item => ExposedItemInfo,
}) void {
    scope.items(item_kind).put(gpa, name, value) catch |err| collections.utils.exitOnOom(err);
}

/// Introduce a type declaration into the scope
pub fn introduceTypeDecl(
    scope: *Scope,
    gpa: std.mem.Allocator,
    ident_store: *const base.Ident.Store,
    name: Ident.Idx,
    type_decl: CIR.Statement.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?CIR.Statement.Idx,
) TypeIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.type_decls.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
            // Type redeclaration is an error, not just a warning
            return TypeIntroduceResult{ .redeclared_error = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes and issue warnings
    var shadowed_stmt: ?CIR.Statement.Idx = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_stmt = lookup_fn(name);
    }

    scope.put(gpa, .type_decl, name, type_decl);

    if (shadowed_stmt) |stmt| {
        return TypeIntroduceResult{ .shadowing_warning = stmt };
    }

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

/// Update an existing type declaration in the scope
/// This is used for recursive type declarations where we need to update
/// the statement index after canonicalizing the type annotation
pub fn updateTypeDecl(
    scope: *Scope,
    gpa: std.mem.Allocator,
    ident_store: *const base.Ident.Store,
    name: Ident.Idx,
    new_type_decl: CIR.Statement.Idx,
) void {
    // Find the existing entry by comparing text content
    var iter = scope.type_decls.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
            // Update the existing entry with the new statement index
            entry.value_ptr.* = new_type_decl;
            return;
        }
    }
    // If not found, add it as a new entry
    scope.put(gpa, .type_decl, name, new_type_decl);
}

/// Introduce a type variable into the scope
pub fn introduceTypeVar(
    scope: *Scope,
    gpa: std.mem.Allocator,
    ident_store: *const base.Ident.Store,
    name: Ident.Idx,
    type_var_anno: CIR.TypeAnno.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?CIR.TypeAnno.Idx,
) TypeVarIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.type_vars.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
            // Type variable already exists in this scope
            return TypeVarIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_type_var: ?CIR.TypeAnno.Idx = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_type_var = lookup_fn(name);
    }

    scope.put(gpa, .type_var, name, type_var_anno);

    if (shadowed_type_var) |anno| {
        return TypeVarIntroduceResult{ .shadowing_warning = anno };
    }

    return TypeVarIntroduceResult{ .success = {} };
}

/// Lookup a type variable in the scope hierarchy
pub fn lookupTypeVar(scope: *const Scope, ident_store: *const base.Ident.Store, name: Ident.Idx) TypeVarLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.type_vars.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
            return TypeVarLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return TypeVarLookupResult{ .not_found = {} };
}

/// Look up a module alias in this scope
pub fn lookupModuleAlias(scope: *const Scope, ident_store: *const base.Ident.Store, name: Ident.Idx) ModuleAliasLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.module_aliases.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
            return ModuleAliasLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return ModuleAliasLookupResult{ .not_found = {} };
}

/// Introduce a module alias into this scope
pub fn introduceModuleAlias(
    scope: *Scope,
    gpa: std.mem.Allocator,
    ident_store: *const base.Ident.Store,
    alias_name: Ident.Idx,
    module_name: Ident.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?Ident.Idx,
) ModuleAliasIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.module_aliases.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(alias_name, entry.key_ptr.*)) {
            // Module alias already exists in this scope
            return ModuleAliasIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_module: ?Ident.Idx = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_module = lookup_fn(alias_name);
    }

    scope.put(gpa, .module_alias, alias_name, module_name);

    if (shadowed_module) |module| {
        return ModuleAliasIntroduceResult{ .shadowing_warning = module };
    }

    return ModuleAliasIntroduceResult{ .success = {} };
}

/// Look up an exposed item in this scope
pub fn lookupExposedItem(scope: *const Scope, ident_store: *const base.Ident.Store, name: Ident.Idx) ExposedItemLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.exposed_items.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
            return ExposedItemLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return ExposedItemLookupResult{ .not_found = {} };
}

/// Introduce an exposed item into this scope
pub fn introduceExposedItem(
    scope: *Scope,
    gpa: std.mem.Allocator,
    ident_store: *const base.Ident.Store,
    item_name: Ident.Idx,
    item_info: ExposedItemInfo,
    parent_lookup_fn: ?fn (Ident.Idx) ?ExposedItemInfo,
) ExposedItemIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.exposed_items.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(item_name, entry.key_ptr.*)) {
            // Exposed item already exists in this scope
            return ExposedItemIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_info: ?ExposedItemInfo = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_info = lookup_fn(item_name);
    }

    scope.put(gpa, .exposed_item, item_name, item_info);

    if (shadowed_info) |info| {
        return ExposedItemIntroduceResult{ .shadowing_warning = info };
    }

    return ExposedItemIntroduceResult{ .success = {} };
}
