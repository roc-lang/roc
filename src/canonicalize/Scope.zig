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
    /// True when the compiler installed this binding for its own baked
    /// `Builtin` module, which every module gets without asking. A name a
    /// module's own `import` brings in takes precedence over it.
    is_compiler_builtin: bool = false,
    /// True when this binding is an import's own name, which denotes the
    /// declaration the import selects -- a type module's main type, or the
    /// declaration a package header makes public -- rather than a name
    /// inside the module.
    names_import_main_type: bool = false,
};

/// A unified type binding that can represent either a locally declared type or an externally imported type.
/// This is the single source of truth for all type resolution in a scope.
pub const TypeBinding = union(enum) {
    local_nominal: CIR.Statement.Idx,
    local_alias: CIR.Statement.Idx,
    local_where_alias: CIR.Statement.Idx,
    associated_nominal: CIR.Statement.Idx,
    external_nominal: ExternalTypeBinding,
};

/// Information about a forward reference (referenced before defined)
pub const ForwardReference = struct {
    /// The pattern index created for this forward reference
    pattern_idx: CIR.Pattern.Idx,
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

/// The first of this scope's identifier bindings in the canonicalizer's
/// identifier `Bindings`, or `no_binding`.
ident_head: u32,
/// The first of this scope's alias bindings in the canonicalizer's alias
/// `Bindings`, or `no_binding`.
alias_head: u32,
/// Canonical bindings for type names (local, auto-imported, and imported types)
type_bindings: std.AutoHashMapUnmanaged(Ident.Idx, TypeBinding),
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
        .ident_head = no_binding,
        .alias_head = no_binding,
        .type_bindings = std.AutoHashMapUnmanaged(Ident.Idx, TypeBinding){},
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
    self.type_bindings.deinit(gpa);
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
    /// True when this item is an import's own name, which denotes the
    /// declaration the import selects rather than a name inside the module.
    names_import_main_type: bool = false,
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
    /// Reassigning a var declared outside the `expect` whose body is being
    /// canonicalized.
    var_reassigned_in_expect: CIR.Pattern.Idx,
    var_reassignment_ok: CIR.Pattern.Idx, // Var reassignment - return existing pattern
};

/// A requested type-name binding introduction.
pub const TypeBindingInput = union(enum) {
    local_nominal: CIR.Statement.Idx,
    local_alias: CIR.Statement.Idx,
    local_where_alias: CIR.Statement.Idx,
    associated_nominal: CIR.Statement.Idx,
    external_nominal: ExternalTypeBinding,
};

/// The outcome of applying type-name binding policy to a scope.
pub const TypeBindingDecision = union(enum) {
    inserted,
    inserted_shadowing_parent: TypeBinding,
    replaced_current_external: ExternalTypeBinding,
    /// An `exposing [...]` item replaced the binding the same import's own
    /// alias made. Both name the same import, so this is not a collision.
    narrowed_import_alias,
    idempotent_current,
    rejected_current_conflict: TypeBinding,
    redeclared_current: TypeBinding,
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

/// The namespaces whose bindings live in a `Bindings` across the scope stack.
pub const ItemKind = enum { ident, alias };

/// The end of a binding list or chain.
pub const no_binding: u32 = std.math.maxInt(u32);

/// One namespace's bindings across the whole scope stack. A name's innermost
/// binding is direct-indexed by identifier, and the bindings it shadows in
/// enclosing scopes hang off it, innermost first. Each scope lists its own
/// bindings, newest first, from the head the scope keeps.
pub fn Bindings(comptime V: type) type {
    return struct {
        const Self = @This();

        pub const Binding = struct {
            name: Ident.Idx,
            value: V,
            /// Index of the scope that binds the name.
            scope: u32,
            /// The name's binding in the nearest enclosing scope that binds it.
            shadowed: u32,
            /// The scope's next older binding.
            next_in_scope: u32,
        };

        slots: std.ArrayList(Binding) = .empty,
        /// Slots of bindings whose scopes have exited, for reuse.
        free_slots: std.ArrayList(u32) = .empty,
        innermost: collections.DenseMap(Ident.Idx, u32),

        pub fn init(gpa: Allocator) Self {
            return .{ .innermost = .init(gpa) };
        }

        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.slots.deinit(gpa);
            self.free_slots.deinit(gpa);
            self.innermost.deinit();
        }

        /// The innermost binding of `name`, if any open scope binds it.
        pub fn innermostBinding(self: *const Self, name: Ident.Idx) ?Binding {
            const slot = self.innermost.get(name) orelse return null;
            return self.slots.items[slot];
        }

        /// The value `scope` itself binds `name` to, if it binds it.
        pub fn getInScope(self: *const Self, name: Ident.Idx, scope: u32) ?V {
            var slot = self.innermost.get(name) orelse return null;
            while (slot != no_binding) {
                const binding = self.slots.items[slot];
                if (binding.scope == scope) return binding.value;
                if (binding.scope < scope) return null;
                slot = binding.shadowed;
            }
            return null;
        }

        /// Bind `name` to `value` in `scope`, replacing the value of a binding
        /// `scope` already has for it. `head` is `scope`'s list head.
        pub fn put(self: *Self, gpa: Allocator, head: *u32, scope: u32, name: Ident.Idx, value: V) Allocator.Error!void {
            const gop = try self.innermost.getOrPut(name);
            // Find where the scope's binding goes in the name's chain, which
            // runs from inner scopes to outer ones.
            var newer: u32 = no_binding;
            var older: u32 = if (gop.found_existing) gop.value_ptr.* else no_binding;
            while (older != no_binding) {
                const binding = &self.slots.items[older];
                if (binding.scope == scope) {
                    binding.value = value;
                    return;
                }
                if (binding.scope < scope) break;
                newer = older;
                older = binding.shadowed;
            }

            errdefer if (!gop.found_existing) {
                _ = self.innermost.remove(name);
            };
            try self.free_slots.ensureTotalCapacity(gpa, self.slots.items.len + 1);
            const slot = self.free_slots.pop() orelse slot: {
                const fresh = std.math.cast(u32, self.slots.items.len) orelse return error.OutOfMemory;
                if (fresh == no_binding) return error.OutOfMemory;
                _ = try self.slots.addOne(gpa);
                break :slot fresh;
            };
            self.slots.items[slot] = .{
                .name = name,
                .value = value,
                .scope = scope,
                .shadowed = older,
                .next_in_scope = head.*,
            };
            head.* = slot;
            if (newer == no_binding) {
                gop.value_ptr.* = slot;
            } else {
                self.slots.items[newer].shadowed = slot;
            }
        }

        /// Walks one scope's bindings, newest first.
        pub const Iterator = struct {
            bindings: *const Self,
            slot: u32,

            pub fn next(self: *Iterator) ?Binding {
                if (self.slot == no_binding) return null;
                const binding = self.bindings.slots.items[self.slot];
                self.slot = binding.next_in_scope;
                return binding;
            }
        };

        /// The bindings of the scope whose list head is `head`.
        pub fn scopeIterator(self: *const Self, head: u32) Iterator {
            return .{ .bindings = self, .slot = head };
        }

        /// Drop the bindings of the innermost open scope, whose list head is
        /// `head`, uncovering the bindings they shadowed.
        pub fn exitScope(self: *Self, head: u32) void {
            var slot = head;
            while (slot != no_binding) {
                const binding = self.slots.items[slot];
                // Nothing is open inside the innermost scope, so each of its
                // bindings is its name's innermost.
                std.debug.assert(self.innermost.get(binding.name).? == slot);
                if (binding.shadowed == no_binding) {
                    _ = self.innermost.remove(binding.name);
                } else {
                    self.innermost.getPtr(binding.name).?.* = binding.shadowed;
                }
                self.free_slots.appendAssumeCapacity(slot);
                slot = binding.next_in_scope;
            }
        }
    };
}

/// Return the statement behind a local type binding, if it has one.
pub fn typeBindingStatement(binding: TypeBinding) ?CIR.Statement.Idx {
    return switch (binding) {
        inline .local_nominal, .local_alias, .local_where_alias, .associated_nominal => |stmt| stmt,
        .external_nominal => null,
    };
}

/// Convert an introduction request to the binding that would be stored.
pub fn inputToBinding(input: TypeBindingInput) TypeBinding {
    return switch (input) {
        .local_nominal => |stmt| TypeBinding{ .local_nominal = stmt },
        .local_alias => |stmt| TypeBinding{ .local_alias = stmt },
        .local_where_alias => |stmt| TypeBinding{ .local_where_alias = stmt },
        .associated_nominal => |stmt| TypeBinding{ .associated_nominal = stmt },
        .external_nominal => |external| TypeBinding{ .external_nominal = external },
    };
}

fn inputStatement(input: TypeBindingInput) ?CIR.Statement.Idx {
    return switch (input) {
        inline .local_nominal, .local_alias, .local_where_alias, .associated_nominal => |stmt| stmt,
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
            .external_nominal => |existing_external| if (existing_external.names_import_main_type and
                !incoming_external.names_import_main_type and
                existing_external.import_idx == incoming_external.import_idx)
                // An `exposing [...]` item names one declaration of the
                // import, so it takes precedence over the binding the
                // import's own alias made for the same import.
                TypeBindingDecision.narrowed_import_alias
            else if (sameExternal(existing_external, incoming_external))
                .idempotent_current
            else if (existing_external.is_compiler_builtin and !incoming_external.is_compiler_builtin)
                // A name this module imports takes precedence over the same
                // name the compiler auto-imports from its baked `Builtin`.
                TypeBindingDecision{ .replaced_current_external = existing_external }
            else
                TypeBindingDecision{ .rejected_current_conflict = existing },
            .local_nominal, .local_alias, .local_where_alias, .associated_nominal => TypeBindingDecision{ .rejected_current_conflict = existing },
        },
        .local_nominal, .local_alias, .local_where_alias => switch (existing) {
            .external_nominal => |existing_external| TypeBindingDecision{ .replaced_current_external = existing_external },
            .local_nominal, .local_alias, .local_where_alias, .associated_nominal => TypeBindingDecision{ .redeclared_current = existing },
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
            .replaced_current_external, .narrowed_import_alias => {
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

/// Look up a type variable alias in this scope (for static dispatch on type vars)
pub fn lookupTypeVarAlias(scope: *const Scope, name: Ident.Idx) TypeVarAliasLookupResult {
    if (scope.type_var_aliases.get(name)) |binding| {
        return TypeVarAliasLookupResult{ .found = binding };
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
    if (scope.type_var_aliases.get(alias_name)) |existing| {
        // Type var alias already exists in this scope
        return TypeVarAliasIntroduceResult{ .already_in_scope = existing };
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
    if (scope.module_aliases.get(name)) |info| {
        return ModuleAliasLookupResult{ .found = info };
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
    if (scope.module_aliases.get(alias_name)) |existing| {
        // Module alias already exists in this scope
        return ModuleAliasIntroduceResult{ .already_in_scope = existing };
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

    try scope.module_aliases.put(gpa, alias_name, module_info);

    if (shadowed_module) |info| {
        return ModuleAliasIntroduceResult{ .shadowing_warning = info };
    }

    return ModuleAliasIntroduceResult{ .success = {} };
}

/// Look up an exposed item in this scope
pub fn lookupExposedItem(scope: *const Scope, name: Ident.Idx) ExposedItemLookupResult {
    if (scope.exposed_items.get(name)) |info| {
        return ExposedItemLookupResult{ .found = info };
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
    if (scope.exposed_items.get(item_name)) |existing| {
        // Exposed item already exists in this scope
        return ExposedItemIntroduceResult{ .already_in_scope = existing };
    }

    // Check for shadowing in parent scopes
    var shadowed_info: ?ExposedItemInfo = null;
    if (parent_lookup_fn) |lookup_fn| {
        shadowed_info = lookup_fn(item_name);
    }

    try scope.exposed_items.put(gpa, item_name, item_info);

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
