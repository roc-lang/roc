//! Scope management for identifier resolution during canonicalization.

const std = @import("std");
const Allocator = std.mem.Allocator;
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
    target_node_idx: ?u32,
    import_idx: ?CIR.Import.Idx,
    origin_region: Region,
    /// True if the module was attempted to be imported but was not found.
    /// This allows us to emit a more specific diagnostic when the type is used.
    module_not_found: bool,
};

/// A unified type binding that can represent either a locally declared type or an externally imported type.
/// This is the single source of truth for all type resolution in a scope.
pub const TypeBinding = union(enum) {
    local_nominal: CIR.Statement.Idx,
    local_alias: CIR.Statement.Idx,
    associated_nominal: CIR.Statement.Idx,
    external_nominal: ExternalTypeBinding,
};

/// Information about a forward reference (referenced before defined)
pub const ForwardReference = struct {
    /// The pattern index created for this forward reference
    pattern_idx: CIR.Pattern.Idx,
    /// Regions where this identifier was referenced (for error reporting)
    reference_regions: std.ArrayList(Region),
};

/// Information about a type variable alias binding (for static dispatch on type vars)
/// Example: `Thing : thing` creates an alias allowing `Thing.method(arg)` to dispatch
/// based on what `thing` resolves to at runtime.
pub const TypeVarAliasBinding = struct {
    /// The name of the type variable being aliased (e.g., "thing")
    type_var_name: Ident.Idx,
    /// The type annotation index for the type variable
    type_var_anno: CIR.TypeAnno.Idx,
    /// The statement index for the s_type_var_alias statement
    statement_idx: CIR.Statement.Idx,
};

/// Maps an Ident to a Pattern in the Can IR
idents: std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
aliases: std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx),
/// Forward references: identifiers that have been referenced but not yet defined
forward_references: std.AutoHashMapUnmanaged(Ident.Idx, ForwardReference),
/// Canonical bindings for type names (local, auto-imported, and imported types)
type_bindings: std.AutoHashMapUnmanaged(Ident.Idx, TypeBinding),
/// Maps type variables to their type annotation indices
type_vars: std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx),
/// Maps uppercase alias names to type variable aliases (for static dispatch on type vars)
/// The key is the alias name (e.g., "Thing"), the value contains the type var info
type_var_aliases: std.AutoHashMapUnmanaged(Ident.Idx, TypeVarAliasBinding),
/// Maps module alias names to their full module info (name + whether package-qualified)
module_aliases: std.AutoHashMapUnmanaged(Ident.Idx, ModuleAliasInfo),
/// Maps exposed item names to their source modules and original names (for import resolution)
exposed_items: std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo),
/// Maps module identifiers to their Import.Idx for modules imported in this scope.
imported_modules: std.AutoHashMapUnmanaged(Ident.Idx, CIR.Import.Idx),
is_function_boundary: bool,
/// The type name associated with this scope (if this scope is for an associated block)
/// null for regular scopes, set for scopes created by processAssociatedBlock
associated_type_name: ?Ident.Idx,

/// Initialize the scope
pub fn init(is_function_boundary: bool) Scope {
    return Scope{
        .idents = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .aliases = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Pattern.Idx){},
        .forward_references = std.AutoHashMapUnmanaged(Ident.Idx, ForwardReference){},
        .type_bindings = std.AutoHashMapUnmanaged(Ident.Idx, TypeBinding){},
        .type_vars = std.AutoHashMapUnmanaged(Ident.Idx, CIR.TypeAnno.Idx){},
        .type_var_aliases = std.AutoHashMapUnmanaged(Ident.Idx, TypeVarAliasBinding){},
        .module_aliases = std.AutoHashMapUnmanaged(Ident.Idx, ModuleAliasInfo){},
        .exposed_items = std.AutoHashMapUnmanaged(Ident.Idx, ExposedItemInfo){},
        .imported_modules = std.AutoHashMapUnmanaged(Ident.Idx, CIR.Import.Idx){},
        .is_function_boundary = is_function_boundary,
        .associated_type_name = null,
    };
}

/// Deinitialize the scope
pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
    self.idents.deinit(gpa);
    self.aliases.deinit(gpa);

    // Deinit forward reference arraylists
    var forward_iter = self.forward_references.valueIterator();
    while (forward_iter.next()) |forward_ref| {
        forward_ref.reference_regions.deinit(gpa);
    }
    self.forward_references.deinit(gpa);

    self.type_bindings.deinit(gpa);
    self.type_vars.deinit(gpa);
    self.type_var_aliases.deinit(gpa);
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

/// Result of looking up a type variable alias
pub const TypeVarAliasLookupResult = union(enum) {
    found: TypeVarAliasBinding,
    not_found: void,
};

/// Result of introducing a type variable alias
pub const TypeVarAliasIntroduceResult = union(enum) {
    success: void,
    shadowing_warning: TypeVarAliasBinding, // The type var alias that was shadowed
    already_in_scope: TypeVarAliasBinding, // The type var alias already exists in this scope
};

/// Information about a module alias
pub const ModuleAliasInfo = struct {
    module_name: Ident.Idx,
    is_package_qualified: bool,
};

/// Result of looking up a module alias
pub const ModuleAliasLookupResult = union(enum) {
    found: ModuleAliasInfo,
    not_found: void,
};

/// Information about an exposed item
pub const ExposedItemInfo = struct {
    module_name: Ident.Idx,
    original_name: Ident.Idx,
    target: ?collections.ExposedItemTarget = null,
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
    var_reassignment_ok: CIR.Pattern.Idx, // Var reassignment - return existing pattern
};

/// A requested type-name binding introduction.
pub const TypeBindingInput = union(enum) {
    local_nominal: CIR.Statement.Idx,
    local_alias: CIR.Statement.Idx,
    associated_nominal: CIR.Statement.Idx,
    external_nominal: ExternalTypeBinding,
};

/// The outcome of applying type-name binding policy to a scope.
pub const TypeBindingDecision = union(enum) {
    inserted,
    inserted_shadowing_parent: TypeBinding,
    replaced_current_external: ExternalTypeBinding,
    idempotent_current,
    rejected_current_conflict: TypeBinding,
    redeclared_current: TypeBinding,
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
    shadowing_warning: ModuleAliasInfo, // The module alias that was shadowed
    already_in_scope: ModuleAliasInfo, // The module alias already exists in this scope
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
    .module_alias => *std.AutoHashMapUnmanaged(Ident.Idx, ModuleAliasInfo),
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
    .module_alias => *const std.AutoHashMapUnmanaged(Ident.Idx, ModuleAliasInfo),
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
    .module_alias => ModuleAliasInfo,
    .exposed_item => ExposedItemInfo,
}) std.mem.Allocator.Error!void {
    try scope.items(item_kind).put(gpa, name, value);
}

/// Return the statement behind a local type binding, if it has one.
pub fn typeBindingStatement(binding: TypeBinding) ?CIR.Statement.Idx {
    return switch (binding) {
        .local_nominal => |stmt| stmt,
        .local_alias => |stmt| stmt,
        .associated_nominal => |stmt| stmt,
        .external_nominal => null,
    };
}

/// Convert an introduction request to the binding that would be stored.
pub fn inputToBinding(input: TypeBindingInput) TypeBinding {
    return switch (input) {
        .local_nominal => |stmt| TypeBinding{ .local_nominal = stmt },
        .local_alias => |stmt| TypeBinding{ .local_alias = stmt },
        .associated_nominal => |stmt| TypeBinding{ .associated_nominal = stmt },
        .external_nominal => |external| TypeBinding{ .external_nominal = external },
    };
}

fn inputStatement(input: TypeBindingInput) ?CIR.Statement.Idx {
    return switch (input) {
        .local_nominal => |stmt| stmt,
        .local_alias => |stmt| stmt,
        .associated_nominal => |stmt| stmt,
        .external_nominal => null,
    };
}

/// Return whether two external bindings refer to the same imported type name.
pub fn sameExternal(a: ExternalTypeBinding, b: ExternalTypeBinding) bool {
    return a.module_ident.eql(b.module_ident) and a.original_ident.eql(b.original_ident);
}

fn currentCollisionDecision(existing: TypeBinding, incoming: TypeBindingInput) TypeBindingDecision {
    if (inputStatement(incoming)) |incoming_stmt| {
        if (typeBindingStatement(existing)) |existing_stmt| {
            if (existing_stmt == incoming_stmt) return .idempotent_current;
        }
    }

    return switch (incoming) {
        .external_nominal => |incoming_external| switch (existing) {
            .external_nominal => |existing_external| if (sameExternal(existing_external, incoming_external))
                .idempotent_current
            else
                TypeBindingDecision{ .rejected_current_conflict = existing },
            .local_nominal, .local_alias, .associated_nominal => TypeBindingDecision{ .rejected_current_conflict = existing },
        },
        .local_nominal, .local_alias => switch (existing) {
            .external_nominal => |existing_external| TypeBindingDecision{ .replaced_current_external = existing_external },
            .local_nominal, .local_alias, .associated_nominal => TypeBindingDecision{ .redeclared_current = existing },
        },
        .associated_nominal => TypeBindingDecision{ .redeclared_current = existing },
    };
}

/// Introduce or reject a type binding according to the canonical type-name policy.
pub fn introduceTypeBinding(
    gpa: Allocator,
    scopes: []Scope,
    scope_idx: usize,
    name: Ident.Idx,
    incoming: TypeBindingInput,
) Allocator.Error!TypeBindingDecision {
    std.debug.assert(scope_idx < scopes.len);

    const incoming_binding = inputToBinding(incoming);
    const scope = &scopes[scope_idx];

    if (scope.type_bindings.get(name)) |existing| {
        const decision = currentCollisionDecision(existing, incoming);
        switch (decision) {
            .replaced_current_external => {
                try scope.type_bindings.put(gpa, name, incoming_binding);
            },
            .inserted,
            .inserted_shadowing_parent,
            .idempotent_current,
            .rejected_current_conflict,
            .redeclared_current,
            => {},
        }
        return decision;
    }

    var shadowed_parent: ?TypeBinding = null;
    var parent_idx = scope_idx;
    while (parent_idx > 0) {
        parent_idx -= 1;
        if (scopes[parent_idx].type_bindings.get(name)) |parent_binding| {
            shadowed_parent = parent_binding;
            break;
        }
    }

    try scope.type_bindings.put(gpa, name, incoming_binding);
    if (shadowed_parent) |binding| {
        return TypeBindingDecision{ .inserted_shadowing_parent = binding };
    }
    return .inserted;
}

/// Introduce a type variable into the scope
pub fn introduceTypeVar(
    scope: *Scope,
    gpa: std.mem.Allocator,
    name: Ident.Idx,
    type_var_anno: CIR.TypeAnno.Idx,
    parent_lookup_fn: ?fn (Ident.Idx) ?CIR.TypeAnno.Idx,
) std.mem.Allocator.Error!TypeVarIntroduceResult {
    // Check if already exists in current scope.
    var iter = scope.type_vars.iterator();
    while (iter.next()) |entry| {
        if (name.eql(entry.key_ptr.*)) {
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
    // Search by identifier equality.
    var iter = scope.type_vars.iterator();
    while (iter.next()) |entry| {
        if (name.eql(entry.key_ptr.*)) {
            return TypeVarLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return TypeVarLookupResult{ .not_found = {} };
}

/// Look up a type variable alias in this scope (for static dispatch on type vars)
pub fn lookupTypeVarAlias(scope: *const Scope, name: Ident.Idx) TypeVarAliasLookupResult {
    // Search by identifier equality.
    var iter = scope.type_var_aliases.iterator();
    while (iter.next()) |entry| {
        if (name.eql(entry.key_ptr.*)) {
            return TypeVarAliasLookupResult{ .found = entry.value_ptr.* };
        }
    }
    return TypeVarAliasLookupResult{ .not_found = {} };
}

/// Introduce a type variable alias into this scope (for static dispatch on type vars)
pub fn introduceTypeVarAlias(
    scope: *Scope,
    gpa: std.mem.Allocator,
    alias_name: Ident.Idx,
    type_var_name: Ident.Idx,
    type_var_anno: CIR.TypeAnno.Idx,
    statement_idx: CIR.Statement.Idx,
    parent_lookup_fn: ?*const fn (Ident.Idx) ?TypeVarAliasBinding,
) std.mem.Allocator.Error!TypeVarAliasIntroduceResult {
    // Check if already exists in current scope.
    var iter = scope.type_var_aliases.iterator();
    while (iter.next()) |entry| {
        if (alias_name.eql(entry.key_ptr.*)) {
            // Type var alias already exists in this scope
            return TypeVarAliasIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_alias: ?TypeVarAliasBinding = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_alias = lookup_fn(alias_name);
    }

    const binding = TypeVarAliasBinding{
        .type_var_name = type_var_name,
        .type_var_anno = type_var_anno,
        .statement_idx = statement_idx,
    };

    try scope.type_var_aliases.put(gpa, alias_name, binding);

    if (shadowed_alias) |shadowed| {
        return TypeVarAliasIntroduceResult{ .shadowing_warning = shadowed };
    }

    return TypeVarAliasIntroduceResult{ .success = {} };
}

/// Look up a module alias in this scope
pub fn lookupModuleAlias(scope: *const Scope, name: Ident.Idx) ModuleAliasLookupResult {
    // Search by identifier equality.
    var iter = scope.module_aliases.iterator();
    while (iter.next()) |entry| {
        if (name.eql(entry.key_ptr.*)) {
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
    is_package_qualified: bool,
    parent_lookup_fn: ?fn (Ident.Idx) ?ModuleAliasInfo,
) std.mem.Allocator.Error!ModuleAliasIntroduceResult {
    // Check if already exists in current scope.
    var iter = scope.module_aliases.iterator();
    while (iter.next()) |entry| {
        if (alias_name.eql(entry.key_ptr.*)) {
            // Module alias already exists in this scope
            return ModuleAliasIntroduceResult{ .already_in_scope = entry.value_ptr.* };
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_module: ?ModuleAliasInfo = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_module = lookup_fn(alias_name);
    }

    const module_info = ModuleAliasInfo{
        .module_name = module_name,
        .is_package_qualified = is_package_qualified,
    };

    try scope.put(gpa, .module_alias, alias_name, module_info);

    if (shadowed_module) |info| {
        return ModuleAliasIntroduceResult{ .shadowing_warning = info };
    }

    return ModuleAliasIntroduceResult{ .success = {} };
}

/// Look up an exposed item in this scope
pub fn lookupExposedItem(scope: *const Scope, name: Ident.Idx) ExposedItemLookupResult {
    // Search by identifier equality.
    var iter = scope.exposed_items.iterator();
    while (iter.next()) |entry| {
        if (name.eql(entry.key_ptr.*)) {
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
    // Check if already exists in current scope.
    var iter = scope.exposed_items.iterator();
    while (iter.next()) |entry| {
        if (item_name.eql(entry.key_ptr.*)) {
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
pub fn lookupImportedModule(scope: *const Scope, module_name: Ident.Idx) ImportedModuleLookupResult {
    if (scope.imported_modules.get(module_name)) |import_idx| {
        return ImportedModuleLookupResult{ .found = import_idx };
    }
    return ImportedModuleLookupResult{ .not_found = {} };
}

/// Introduce an imported module into this scope
pub fn introduceImportedModule(
    scope: *Scope,
    gpa: std.mem.Allocator,
    module_name: Ident.Idx,
    import_idx: CIR.Import.Idx,
) std.mem.Allocator.Error!ImportedModuleIntroduceResult {
    if (scope.imported_modules.contains(module_name)) {
        return ImportedModuleIntroduceResult{ .already_imported = scope.imported_modules.get(module_name).? };
    }

    try scope.imported_modules.put(gpa, module_name, import_idx);
    return ImportedModuleIntroduceResult{ .success = {} };
}
