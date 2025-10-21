//! Scope management for identifier resolution during canonicalization.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const CIR = @import("CIR.zig");

const Ident = base.Ident;
const Region = base.Region;

const Scope = @This();

/// Represents a type binding for a type imported from an external module.
/// Contains all necessary information to resolve the type from the imported module.
pub const ExternalTypeBinding = struct {
    module_ident: Ident.Idx,
    original_ident: Ident.Idx,
    target_node_idx: ?u16,
    import_idx: ?CIR.Import.Idx,
    origin_region: Region,
};

/// A unified type binding that can represent either a locally declared type or an externally imported type.
/// This is the single source of truth for all type resolution in a scope.
pub const TypeBinding = union(enum) {
    local_nominal: CIR.Statement.Idx,
    local_alias: CIR.Statement.Idx,
    associated_nominal: CIR.Statement.Idx,
    external_nominal: ExternalTypeBinding,
};

/// Maps an Ident to a Pattern in the Can IR
idents: std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
aliases: std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
/// Canonical bindings for type names (local, auto-imported, and imported types)
type_bindings: std.AutoHashMapUnmanaged(Ident.Idx, TypeBinding),
/// Maps type variables to their type annotation indices
type_vars: std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx),
/// Maps module alias names to their full module names
module_aliases: std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx),
/// Maps exposed item names to their source modules and original names (for import resolution)
exposed_items: std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
/// Maps module names to their Import.Idx for modules imported in this scope
imported_modules: std.StringHashMapUnmanaged(CIR.Import.Idx),
is_function_boundary: bool,

/// Initialize the scope
pub fn init(is_function_boundary: bool) Scope {
    return Scope{
        .idents = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .aliases = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .type_bindings = std.AutoHashMapUnmanaged(Ident.Idx, TypeBinding){},
        .type_vars = std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx){},
        .module_aliases = std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx){},
        .exposed_items = std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo){},
        .imported_modules = std.StringHashMapUnmanaged(CIR.Import.Idx){},
        .is_function_boundary = is_function_boundary,
    };
}

/// Deinitialize the scope
pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
    self.idents.deinit(gpa);
    self.aliases.deinit(gpa);
    self.type_bindings.deinit(gpa);
    self.type_vars.deinit(gpa);
    self.module_aliases.deinit(gpa);
    self.exposed_items.deinit(gpa);
    self.imported_modules.deinit(gpa);
}

/// Scope management types and structures
pub const Error = error{
    NotInScope,
    AlreadyInScope,
    ExitedTopScopeLevel,
    TopLevelVarError,
    VarAcrossFunctionBoundary,
    OutOfMemory,
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

/// Result of looking up an imported module
pub const ImportedModuleLookupResult = union(enum) {
    found: CIR.Import.Idx,
    not_found: void,
};

/// Result of introducing an imported module
pub const ImportedModuleIntroduceResult = union(enum) {
    success: void,
    already_imported: CIR.Import.Idx, // The module was already imported in this scope
};

/// Item kinds in a scope
pub const ItemKind = enum { ident, alias, type_var, module_alias, exposed_item };

/// Get the appropriate map for the given item kind
pub fn items(scope: *Scope, comptime item_kind: ItemKind) switch (item_kind) {
    .ident, .alias => *std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
    .type_var => *std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx),
    .module_alias => *std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx),
    .exposed_item => *std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
} {
    return switch (item_kind) {
        .ident => &scope.idents,
        .alias => &scope.aliases,
        .type_var => &scope.type_vars,
        .module_alias => &scope.module_aliases,
        .exposed_item => &scope.exposed_items,
    };
}

/// Get the appropriate map for the given item kind (const version)
pub fn itemsConst(scope: *const Scope, comptime item_kind: ItemKind) switch (item_kind) {
    .ident, .alias => *const std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
    .type_var => *const std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx),
    .module_alias => *const std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx),
    .exposed_item => *const std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
} {
    return switch (item_kind) {
        .ident => &scope.idents,
        .alias => &scope.aliases,
        .type_var => &scope.type_vars,
        .module_alias => &scope.module_aliases,
        .exposed_item => &scope.exposed_items,
    };
}

/// Put an item in the scope, panics on OOM
pub fn put(scope: *Scope, gpa: std.mem.Allocator, comptime item_kind: ItemKind, name: Ident.Idx, value: switch (item_kind) {
    .ident, .alias => CIR.Pattern.Idx,
    .type_var => CIR.TypeAnno.Idx,
    .module_alias => Ident.Idx,
    .exposed_item => ExposedItemInfo,
}) std.mem.Allocator.Error!void {
    try scope.items(item_kind).put(gpa, name, value);
}

/// Introduce a type declaration into the scope
pub fn introduceTypeDecl(
    scope: *Scope,
    gpa: std.mem.Allocator,
    name: Ident.Idx,
    type_decl: CIR.Statement.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?CIR.Statement.Idx,
) std.mem.Allocator.Error!TypeIntroduceResult {
    // Check if type already exists in this scope
    if (scope.type_bindings.getPtr(name)) |existing| {
        return switch (existing.*) {
            .local_nominal => |stmt| TypeIntroduceResult{ .redeclared_error = stmt },
            .local_alias => |stmt| TypeIntroduceResult{ .type_alias_redeclared = stmt },
            .associated_nominal => |stmt| TypeIntroduceResult{ .nominal_type_redeclared = stmt },
            .external_nominal => TypeIntroduceResult{ .nominal_type_redeclared = type_decl },
        };
    }

    // Check for shadowing in parent scopes and issue warnings
    var shadowed_stmt: ?CIR.Statement.Idx = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_stmt = lookup_fn(name);
    }

    // Add type binding (single source of truth)
    try scope.type_bindings.put(gpa, name, TypeBinding{ .local_nominal = type_decl });

    if (shadowed_stmt) |stmt| {
        return TypeIntroduceResult{ .shadowing_warning = stmt };
    }

    return TypeIntroduceResult{ .success = {} };
}

/// Introduce an unqualified type alias (for associated types)
/// Maps an unqualified name to a fully qualified type declaration
pub fn introduceTypeAlias(
    scope: *Scope,
    gpa: std.mem.Allocator,
    unqualified_name: Ident.Idx,
    qualified_type_decl: CIR.Statement.Idx,
) !void {
    try scope.type_bindings.put(gpa, unqualified_name, TypeBinding{
        .associated_nominal = qualified_type_decl,
    });
}

/// Update an existing type declaration in the scope
/// This is used for recursive type declarations where we need to update
/// the statement index after canonicalizing the type annotation
pub fn updateTypeDecl(
    scope: *Scope,
    gpa: std.mem.Allocator,
    name: Ident.Idx,
    new_type_decl: CIR.Statement.Idx,
) std.mem.Allocator.Error!void {
    if (scope.type_bindings.getPtr(name)) |binding_ptr| {
        const current = binding_ptr.*;
        binding_ptr.* = switch (current) {
            .local_nominal => TypeBinding{ .local_nominal = new_type_decl },
            .local_alias => TypeBinding{ .local_alias = new_type_decl },
            .associated_nominal => TypeBinding{ .associated_nominal = new_type_decl },
            .external_nominal => current,
        };
    } else {
        try scope.type_bindings.put(gpa, name, TypeBinding{ .local_nominal = new_type_decl });
    }
}

/// Introduce a type variable into the scope
pub fn introduceTypeVar(
    scope: *Scope,
    gpa: std.mem.Allocator,
    name: Ident.Idx,
    type_var_anno: CIR.TypeAnno.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?CIR.TypeAnno.Idx,
) std.mem.Allocator.Error!TypeVarIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.type_vars.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            // Type variable already exists in this scope
            return TypeVarIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_type_var: ?CIR.TypeAnno.Idx = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_type_var = lookup_fn(name);
    }

    try scope.put(gpa, .type_var, name, type_var_anno);

    if (shadowed_type_var) |anno| {
        return TypeVarIntroduceResult{ .shadowing_warning = anno };
    }

    return TypeVarIntroduceResult{ .success = {} };
}

/// Lookup a type variable in the scope hierarchy
pub fn lookupTypeVar(scope: *const Scope, name: Ident.Idx) TypeVarLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.type_vars.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            return TypeVarLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return TypeVarLookupResult{ .not_found = {} };
}

/// Look up a module alias in this scope
pub fn lookupModuleAlias(scope: *const Scope, name: Ident.Idx) ModuleAliasLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.module_aliases.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            return ModuleAliasLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return ModuleAliasLookupResult{ .not_found = {} };
}

/// Introduce a module alias into this scope
pub fn introduceModuleAlias(
    scope: *Scope,
    gpa: std.mem.Allocator,
    alias_name: Ident.Idx,
    module_name: Ident.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?Ident.Idx,
) std.mem.Allocator.Error!ModuleAliasIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.module_aliases.iterator();
    while (iter.next()) |entry| {
        if (alias_name.idx == entry.key_ptr.idx) {
            // Module alias already exists in this scope
            return ModuleAliasIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_module: ?Ident.Idx = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_module = lookup_fn(alias_name);
    }

    try scope.put(gpa, .module_alias, alias_name, module_name);

    if (shadowed_module) |module| {
        return ModuleAliasIntroduceResult{ .shadowing_warning = module };
    }

    return ModuleAliasIntroduceResult{ .success = {} };
}

/// Look up an exposed item in this scope
pub fn lookupExposedItem(scope: *const Scope, name: Ident.Idx) ExposedItemLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.exposed_items.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            return ExposedItemLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return ExposedItemLookupResult{ .not_found = {} };
}

/// Introduce an exposed item into this scope
pub fn introduceExposedItem(
    scope: *Scope,
    gpa: std.mem.Allocator,
    item_name: Ident.Idx,
    item_info: ExposedItemInfo,
    parent_lookup_fn: ?fn (Ident.Idx) ?ExposedItemInfo,
) std.mem.Allocator.Error!ExposedItemIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.exposed_items.iterator();
    while (iter.next()) |entry| {
        if (item_name.idx == entry.key_ptr.idx) {
            // Exposed item already exists in this scope
            return ExposedItemIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_info: ?ExposedItemInfo = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_info = lookup_fn(item_name);
    }

    try scope.put(gpa, .exposed_item, item_name, item_info);

    if (shadowed_info) |info| {
        return ExposedItemIntroduceResult{ .shadowing_warning = info };
    }

    return ExposedItemIntroduceResult{ .success = {} };
}

/// Look up an imported module in this scope
pub fn lookupImportedModule(scope: *const Scope, module_name: []const u8) ImportedModuleLookupResult {
    if (scope.imported_modules.get(module_name)) |import_idx| {
        return ImportedModuleLookupResult{ .found = import_idx };
    }
    return ImportedModuleLookupResult{ .not_found = {} };
}

/// Introduce an imported module into this scope
pub fn introduceImportedModule(
    scope: *Scope,
    gpa: std.mem.Allocator,
    module_name: []const u8,
    import_idx: CIR.Import.Idx,
) std.mem.Allocator.Error!ImportedModuleIntroduceResult {
    if (scope.imported_modules.contains(module_name)) {
        return ImportedModuleIntroduceResult{ .already_imported = scope.imported_modules.get(module_name).? };
    }

    try scope.imported_modules.put(gpa, module_name, import_idx);
    return ImportedModuleIntroduceResult{ .success = {} };
}
