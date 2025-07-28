//! Scope management for identifier resolution during canonicalization.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const ModuleEnv = @import("compile").ModuleEnv;

const Ident = base.Ident;

const Scope = @This();

/// Maps VALUE identifiers to their Pattern in the Can IR
value_idents: std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.Pattern.Idx),
/// Maps MODULE identifiers to their Pattern in the Can IR
module_idents: std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.Pattern.Idx),
aliases: std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.Pattern.Idx),
/// Maps type names to their type declaration statements
type_decls: std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.Statement.Idx),
/// Maps type variables to their type annotation indices
type_vars: std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.TypeAnno.Idx),
/// Maps module alias names to their full module names
module_aliases: std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx),
/// Maps exposed VALUE names to their source modules and original names (for import resolution)
exposed_values: std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
/// Maps exposed TYPE names to their source modules and original names (for import resolution)
exposed_types: std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
/// Maps exposed MODULE names to their source modules and original names (for import resolution)
exposed_modules: std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
/// Maps module names to their Import.Idx for modules imported in this scope
imported_modules: std.StringHashMapUnmanaged(ModuleEnv.Import.Idx),
is_function_boundary: bool,

/// Initialize the scope
pub fn init(is_function_boundary: bool) Scope {
    return Scope{
        .value_idents = std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.Pattern.Idx){},
        .module_idents = std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.Pattern.Idx){},
        .aliases = std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.Pattern.Idx){},
        .type_decls = std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.Statement.Idx){},
        .type_vars = std.AutoHashMapUnmanaged(Ident.Idx, ModuleEnv.TypeAnno.Idx){},
        .module_aliases = std.AutoHashMapUnmanaged(Ident.Idx, Ident.Idx){},
        .exposed_values = std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo){},
        .exposed_types = std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo){},
        .exposed_modules = std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo){},
        .imported_modules = std.StringHashMapUnmanaged(ModuleEnv.Import.Idx){},
        .is_function_boundary = is_function_boundary,
    };
}

/// Deinitialize the scope
pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
    self.value_idents.deinit(gpa);
    self.module_idents.deinit(gpa);
    self.aliases.deinit(gpa);
    self.type_decls.deinit(gpa);
    self.type_vars.deinit(gpa);
    self.module_aliases.deinit(gpa);
    self.exposed_values.deinit(gpa);
    self.exposed_types.deinit(gpa);
    self.exposed_modules.deinit(gpa);
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
    found: ModuleEnv.Pattern.Idx,
    not_found: void,
};

/// Result of looking up a type declaration
pub const TypeLookupResult = union(enum) {
    found: ModuleEnv.Statement.Idx,
    not_found: void,
};

/// Result of looking up a type variable
pub const TypeVarLookupResult = union(enum) {
    found: ModuleEnv.TypeAnno.Idx,
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
    shadowing_warning: ModuleEnv.Pattern.Idx, // The pattern that was shadowed
    top_level_var_error: void,
    var_across_function_boundary: ModuleEnv.Pattern.Idx,
};

/// Result of introducing a type declaration
pub const TypeIntroduceResult = union(enum) {
    success: void,
    shadowing_warning: ModuleEnv.Statement.Idx, // The type declaration that was shadowed
    redeclared_error: ModuleEnv.Statement.Idx, // The type declaration that was redeclared
    type_alias_redeclared: ModuleEnv.Statement.Idx, // The type alias that was redeclared
    nominal_type_redeclared: ModuleEnv.Statement.Idx, // The nominal type that was redeclared
    cross_scope_shadowing: ModuleEnv.Statement.Idx, // Type shadowed across different scopes
    parameter_conflict: struct {
        original_stmt: ModuleEnv.Statement.Idx,
        conflicting_parameter: base.Ident.Idx,
    },
};

/// Result of introducing a type variable
pub const TypeVarIntroduceResult = union(enum) {
    success: void,
    shadowing_warning: ModuleEnv.TypeAnno.Idx, // The type variable that was shadowed
    already_in_scope: ModuleEnv.TypeAnno.Idx, // The type variable already exists in this scope
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
    found: ModuleEnv.Import.Idx,
    not_found: void,
};

/// Result of introducing an imported module
pub const ImportedModuleIntroduceResult = union(enum) {
    success: void,
    already_imported: ModuleEnv.Import.Idx, // The module was already imported in this scope
};

/// Introduce a type declaration into the scope
pub fn introduceTypeDecl(
    scope: *Scope,
    gpa: std.mem.Allocator,
    name: Ident.Idx,
    type_decl: ModuleEnv.Statement.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?ModuleEnv.Statement.Idx,
) std.mem.Allocator.Error!TypeIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.type_decls.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            // Type redeclaration is an error, not just a warning
            return TypeIntroduceResult{ .redeclared_error = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes and issue warnings
    var shadowed_stmt: ?ModuleEnv.Statement.Idx = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_stmt = lookup_fn(name);
    }

    try scope.type_decls.put(gpa, name, type_decl);

    if (shadowed_stmt) |stmt| {
        return TypeIntroduceResult{ .shadowing_warning = stmt };
    }

    return TypeIntroduceResult{ .success = {} };
}

/// Lookup a type declaration in the scope hierarchy
/// TODO: Optimize lookup performance - currently O(n) due to text comparison
/// TODO: Consider caching or using a more efficient data structure for type lookup
/// TODO: Support for nominal vs structural type distinction (future := operator)
pub fn lookupTypeDecl(scope: *const Scope, name: Ident.Idx) TypeLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.type_decls.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
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
    name: Ident.Idx,
    new_type_decl: ModuleEnv.Statement.Idx,
) std.mem.Allocator.Error!void {
    // Find the existing entry by comparing text content
    var iter = scope.type_decls.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            // Update the existing entry with the new statement index
            entry.value_ptr.* = new_type_decl;
            return;
        }
    }
    // If not found, add it as a new entry
    try scope.type_decls.put(gpa, name, new_type_decl);
}

/// Introduce a type variable into the scope
pub fn introduceTypeVar(
    scope: *Scope,
    gpa: std.mem.Allocator,
    name: Ident.Idx,
    type_var_anno: ModuleEnv.TypeAnno.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?ModuleEnv.TypeAnno.Idx,
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
    var shadowed_type_var: ?ModuleEnv.TypeAnno.Idx = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_type_var = lookup_fn(name);
    }

    try scope.type_vars.put(gpa, name, type_var_anno);

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

    try scope.module_aliases.put(gpa, alias_name, module_name);

    if (shadowed_module) |module| {
        return ModuleAliasIntroduceResult{ .shadowing_warning = module };
    }

    return ModuleAliasIntroduceResult{ .success = {} };
}

/// Look up an exposed value in this scope
pub fn lookupExposedValue(scope: *const Scope, name: Ident.Idx) ExposedItemLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.exposed_values.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            return ExposedItemLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return ExposedItemLookupResult{ .not_found = {} };
}

/// Look up an exposed type in this scope
pub fn lookupExposedType(scope: *const Scope, name: Ident.Idx) ExposedItemLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.exposed_types.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            return ExposedItemLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return ExposedItemLookupResult{ .not_found = {} };
}

/// Introduce an exposed value into this scope
pub fn introduceExposedValue(
    scope: *Scope,
    gpa: std.mem.Allocator,
    item_name: Ident.Idx,
    item_info: ExposedItemInfo,
    parent_lookup_fn: ?fn (Ident.Idx) ?ExposedItemInfo,
) std.mem.Allocator.Error!ExposedItemIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.exposed_values.iterator();
    while (iter.next()) |entry| {
        if (item_name.idx == entry.key_ptr.idx) {
            // Exposed value already exists in this scope
            return ExposedItemIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_info: ?ExposedItemInfo = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_info = lookup_fn(item_name);
    }

    try scope.exposed_values.put(gpa, item_name, item_info);

    if (shadowed_info) |info| {
        return ExposedItemIntroduceResult{ .shadowing_warning = info };
    }

    return ExposedItemIntroduceResult{ .success = {} };
}

/// Introduce an exposed type into this scope
pub fn introduceExposedType(
    scope: *Scope,
    gpa: std.mem.Allocator,
    item_name: Ident.Idx,
    item_info: ExposedItemInfo,
    parent_lookup_fn: ?fn (Ident.Idx) ?ExposedItemInfo,
) std.mem.Allocator.Error!ExposedItemIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.exposed_types.iterator();
    while (iter.next()) |entry| {
        if (item_name.idx == entry.key_ptr.idx) {
            // Exposed type already exists in this scope
            return ExposedItemIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_info: ?ExposedItemInfo = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_info = lookup_fn(item_name);
    }

    try scope.exposed_types.put(gpa, item_name, item_info);

    if (shadowed_info) |info| {
        return ExposedItemIntroduceResult{ .shadowing_warning = info };
    }

    return ExposedItemIntroduceResult{ .success = {} };
}

/// Look up an exposed module in this scope
pub fn lookupExposedModule(scope: *const Scope, name: Ident.Idx) ExposedItemLookupResult {
    // Search by comparing text content, not identifier index
    var iter = scope.exposed_modules.iterator();
    while (iter.next()) |entry| {
        if (name.idx == entry.key_ptr.idx) {
            return ExposedItemLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return ExposedItemLookupResult{ .not_found = {} };
}

/// Introduce an exposed module into this scope
pub fn introduceExposedModule(
    scope: *Scope,
    gpa: std.mem.Allocator,
    item_name: Ident.Idx,
    item_info: ExposedItemInfo,
    parent_lookup_fn: ?fn (Ident.Idx) ?ExposedItemInfo,
) std.mem.Allocator.Error!ExposedItemIntroduceResult {
    // Check if already exists in current scope by comparing text content
    var iter = scope.exposed_modules.iterator();
    while (iter.next()) |entry| {
        if (item_name.idx == entry.key_ptr.idx) {
            // Exposed module already exists in this scope
            return ExposedItemIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }
    // Check for shadowing in parent scopes
    var shadowed_info: ?ExposedItemInfo = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_info = lookup_fn(item_name);
    }
    try scope.exposed_modules.put(gpa, item_name, item_info);
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
    import_idx: ModuleEnv.Import.Idx,
) std.mem.Allocator.Error!ImportedModuleIntroduceResult {
    if (scope.imported_modules.contains(module_name)) {
        return ImportedModuleIntroduceResult{ .already_imported = scope.imported_modules.get(module_name).? };
    }

    try scope.imported_modules.put(gpa, module_name, import_idx);
    return ImportedModuleIntroduceResult{ .success = {} };
}
