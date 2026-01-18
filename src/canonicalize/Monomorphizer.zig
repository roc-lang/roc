//! Monomorphizer - Specializes polymorphic functions to concrete types
//!
//! This module implements the monomorphization pass which:
//! 1. Finds all polymorphic function definitions
//! 2. Identifies call sites with concrete types
//! 3. Creates specialized versions of functions for each concrete type
//!
//! Following the Cor approach, monomorphization happens AFTER type checking
//! but BEFORE code generation. This differs from the Rust compiler's approach
//! where lambda sets are resolved during type inference.
//!
//! ## Architecture Overview
//!
//! The monomorphizer works in three phases:
//!
//! ### Phase 1: Registration
//! Polymorphic function definitions are registered as `PartialProc`s via
//! `registerPartialProc`. These store the function body and type info but
//! haven't been specialized yet.
//!
//! ### Phase 2: Finding
//! When a call site is encountered, `requestSpecialization` checks if a
//! specialization is needed and adds it to `pending_specializations`.
//! For cross-module calls, use `requestExternalSpecialization`.
//!
//! ### Phase 3: Making
//! `processPendingSpecializations` processes the queue, creating specialized
//! versions of functions by duplicating their bodies with type substitutions.
//!
//! ## Two-Phase Specialization
//!
//! Monomorphization uses a two-phase approach to handle recursive types:
//!
//! 1. **Finding Phase**: Walk the code, discover all needed specializations.
//!    Add them to `pending_specializations` but don't create bodies yet.
//!
//! 2. **Making Phase**: Process pending specializations, duplicate function
//!    bodies with type substitutions. This may discover more specializations,
//!    which are added to pending and processed in the same phase.
//!
//! This separation prevents infinite loops with recursive types by allowing
//! forward references that are patched after the recursive type is fully resolved.
//!
//! ## Polymorphic Recursion Detection
//!
//! The monomorphizer includes fuel-based cutoffs to prevent infinite
//! specialization from polymorphic recursion patterns like:
//!
//! ```roc
//! f = \list -> f [list]  // Type grows: a -> List a -> List (List a) -> ...
//! ```
//!
//! When the same function appears 3+ times on the specialization stack with
//! different types, or when recursion depth exceeds `max_recursion_depth`,
//! specialization stops to prevent non-termination.
//!
//! ## Key Types
//!
//! - `PartialProc`: A function that can be specialized
//! - `PendingSpecialization`: A specialization request waiting to be processed
//! - `SpecializedProc`: A completed specialization with duplicated body
//! - `SpecializationKey`: (original_ident, type_hash) for lookup
//! - `ExternalSpecializationRequest`: Cross-module specialization request

const std = @import("std");
const base = @import("base");
const types = @import("types");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const Expr = CIR.Expr;
const Pattern = @import("Pattern.zig").Pattern;

const Instantiator = types.instantiate.Instantiator;

const Self = @This();

/// Key for looking up specializations
pub const SpecializationKey = struct {
    original_ident: base.Ident.Idx,
    type_hash: u64,

    pub fn eql(a: SpecializationKey, b: SpecializationKey) bool {
        return a.original_ident == b.original_ident and a.type_hash == b.type_hash;
    }
};

/// A function that hasn't been specialized yet (partial proc).
/// Stores the original function definition that can be specialized to
/// different concrete types.
pub const PartialProc = struct {
    /// The original function identifier
    original_ident: base.Ident.Idx,
    /// The function's body expression index
    body_expr: Expr.Idx,
    /// The function's argument patterns
    arg_patterns: CIR.Pattern.Span,
    /// The function's polymorphic type variable
    type_var: types.Var,
    /// Whether this is a top-level definition
    is_top_level: bool,
};

/// A pending specialization that needs to be made.
/// Created during the finding phase, processed during the making phase.
pub const PendingSpecialization = struct {
    /// The original function identifier
    original_ident: base.Ident.Idx,
    /// The concrete type to specialize for
    concrete_type: types.Var,
    /// The call site expression (for error reporting)
    call_site: ?Expr.Idx,
    /// Type substitutions: maps polymorphic vars to concrete vars
    type_substitutions: types.VarMap,
};

/// A completed specialization.
/// The result of processing a PendingSpecialization.
pub const SpecializedProc = struct {
    /// The specialized function name
    specialized_ident: base.Ident.Idx,
    /// The specialized function body (duplicated with concrete types)
    body_expr: Expr.Idx,
    /// The specialized argument patterns
    arg_patterns: CIR.Pattern.Span,
    /// The concrete type for this specialization
    concrete_type: types.Var,
    /// The original function this was specialized from
    original_ident: base.Ident.Idx,
};

/// The phase of monomorphization we're in
pub const Phase = enum {
    /// Discovering what specializations are needed
    finding,
    /// Actually creating specialized functions
    making,
};

/// A reference to an external module that needs specialization.
/// Used for cross-module monomorphization.
pub const ExternalSpecializationRequest = struct {
    /// The module that defines the function
    source_module: CIR.Import.Idx,
    /// The original function identifier in the source module
    original_ident: base.Ident.Idx,
    /// The concrete type to specialize for
    concrete_type: types.Var,
    /// The call site in this module (for error reporting)
    call_site: ?Expr.Idx,

    pub fn eql(a: ExternalSpecializationRequest, b: ExternalSpecializationRequest) bool {
        return a.source_module == b.source_module and
            a.original_ident == b.original_ident;
        // Note: concrete_type comparison would require type equality check
    }
};

/// Result of requesting an external specialization.
/// The actual specialization happens in the source module.
pub const ExternalSpecializationResult = struct {
    /// The specialized name in the external module
    specialized_ident: base.Ident.Idx,
    /// Whether this is a new request or was already known
    is_new: bool,
};

/// Key for looking up resolved external specializations.
pub const ExternalSpecKey = struct {
    source_module: CIR.Import.Idx,
    original_ident: base.Ident.Idx,
    type_hash: u64,
};

/// The allocator for intermediate allocations
allocator: std.mem.Allocator,

/// The module environment containing the CIR
module_env: *ModuleEnv,

/// The type store for looking up concrete types
types_store: *types.Store,

/// Map from original ident to partial proc (functions that can be specialized)
partial_procs: std.AutoHashMap(base.Ident.Idx, PartialProc),

/// Specializations that need to be made (queue processed in making phase)
pending_specializations: std.ArrayList(PendingSpecialization),

/// Completed specializations: (original, type_hash) -> specialized proc
specialized: std.AutoHashMap(SpecializationKey, SpecializedProc),

/// Map from (original_name, concrete_type_hash) -> specialized_name (for lookup)
specialization_names: std.AutoHashMap(SpecializationKey, base.Ident.Idx),

/// Specializations currently being made (to detect recursion)
in_progress: std.AutoHashMap(SpecializationKey, void),

/// Counter for generating unique specialization names
specialization_counter: u32,

/// Counter for generating unique synthetic argument names (e.g., #arg0, #arg1)
/// Used during argument normalization to convert complex call arguments into let-bindings
synthetic_arg_counter: u32,

/// Current phase
phase: Phase,

/// Current recursion depth for detecting polymorphic recursion
recursion_depth: u32,

/// Maximum recursion depth before using fallback (fuel-based cutoff)
max_recursion_depth: u32,

/// Stack of functions currently being specialized (for recursion detection)
specialization_stack: std.ArrayList(SpecializationKey),

/// External specializations requested from other modules
external_requests: std.ArrayList(ExternalSpecializationRequest),

/// Resolved external specializations: maps (source_module, original_ident, type_hash) to specialized_ident.
/// Populated by resolveExternalSpecialization, used by requestExternalSpecialization.
resolved_external_specs: std.AutoHashMap(ExternalSpecKey, base.Ident.Idx),

/// Optional closure transformer for resolving unspecialized lambda set entries.
/// When set, the monomorphizer will resolve unspecialized closures during specialization.
closure_transformer: ?*ClosureTransformer,

/// Optional map from method idents to their lambda sets for ambient function unification.
/// Used during unspecialized closure resolution.
impl_lambda_sets: ?*const std.AutoHashMap(base.Ident.Idx, *ClosureTransformer.LambdaSet),

/// Default maximum recursion depth for polymorphic recursion
pub const DEFAULT_MAX_RECURSION_DEPTH: u32 = 64;

/// Initialize the monomorphizer
pub fn init(
    allocator: std.mem.Allocator,
    module_env: *ModuleEnv,
    types_store: *types.Store,
) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .types_store = types_store,
        .partial_procs = std.AutoHashMap(base.Ident.Idx, PartialProc).init(allocator),
        .pending_specializations = std.ArrayList(PendingSpecialization).empty,
        .specialized = std.AutoHashMap(SpecializationKey, SpecializedProc).init(allocator),
        .specialization_names = std.AutoHashMap(SpecializationKey, base.Ident.Idx).init(allocator),
        .in_progress = std.AutoHashMap(SpecializationKey, void).init(allocator),
        .specialization_counter = 0,
        .synthetic_arg_counter = 0,
        .phase = .finding,
        .recursion_depth = 0,
        .max_recursion_depth = DEFAULT_MAX_RECURSION_DEPTH,
        .specialization_stack = std.ArrayList(SpecializationKey).empty,
        .external_requests = std.ArrayList(ExternalSpecializationRequest).empty,
        .resolved_external_specs = std.AutoHashMap(ExternalSpecKey, base.Ident.Idx).init(allocator),
        .closure_transformer = null,
        .impl_lambda_sets = null,
    };
}

/// Set the closure transformer for resolving unspecialized lambda set entries.
/// This must be called before `processPendingSpecializations` to enable
/// automatic resolution of static dispatch closures.
pub fn setClosureTransformer(
    self: *Self,
    transformer: *ClosureTransformer,
    impl_sets: ?*const std.AutoHashMap(base.Ident.Idx, *ClosureTransformer.LambdaSet),
) void {
    self.closure_transformer = transformer;
    self.impl_lambda_sets = impl_sets;
}

/// Free resources
pub fn deinit(self: *Self) void {
    self.partial_procs.deinit();

    // Free type substitution maps in pending specializations
    for (self.pending_specializations.items) |*pending| {
        pending.type_substitutions.deinit();
    }
    self.pending_specializations.deinit(self.allocator);

    self.specialized.deinit();
    self.specialization_names.deinit();
    self.in_progress.deinit();
    self.specialization_stack.deinit(self.allocator);
    self.external_requests.deinit(self.allocator);
    self.resolved_external_specs.deinit();
}

/// Result of looking up a static dispatch implementation for a concrete type.
/// Contains the information needed to create a ClosureInfo for dispatch.
pub const StaticDispatchLookup = struct {
    /// The identifier of the method implementation (e.g., "List.hash")
    method_ident: base.Ident.Idx,
    /// The type identifier this implementation is for (e.g., "List")
    type_ident: base.Ident.Idx,
    /// The node index for the method, if found in this module
    node_idx: ?u16,
};

/// Looks up the implementation of a static dispatch method for a concrete type.
///
/// Given a concrete type (e.g., List U64) and a method name (e.g., "hash"),
/// this looks up the qualified method identifier (e.g., "List.hash") and
/// finds the function definition that implements it.
///
/// This is used during monomorphization to resolve unspecialized closures
/// (static-dispatch-dependent lambda set entries) to concrete implementations.
///
/// Returns null if:
/// - The type is not resolved to a concrete type
/// - No implementation is registered for this type/method combination
pub fn lookupStaticDispatch(
    self: *Self,
    concrete_type: types.Var,
    method_name: base.Ident.Idx,
) ?StaticDispatchLookup {
    // Resolve the type to get the concrete structure
    const resolved = self.types_store.resolveVar(concrete_type);

    // Get the type identifier from the concrete type
    const type_ident: ?base.Ident.Idx = switch (resolved.desc.content) {
        .structure => |flat| switch (flat) {
            .nominal_type => |nom| nom.ident.ident_idx,
            // For built-in types, we need to look up by their canonical name
            .record, .record_unbound => self.module_env.common.findIdent("Record"),
            .tag_union, .empty_tag_union => self.module_env.common.findIdent("Tag"),
            .tuple => self.module_env.common.findIdent("Tuple"),
            .fn_pure, .fn_effectful, .fn_unbound => self.module_env.common.findIdent("Fn"),
            .empty_record => self.module_env.common.findIdent("Record"),
        },
        .alias => |alias| alias.ident.ident_idx,
        // Flex/rigid vars are not concrete - can't look up implementation
        .flex, .rigid => null,
        .err => null,
    };

    const resolved_type_ident = type_ident orelse return null;

    // Look up the method implementation
    const qualified_method = self.module_env.lookupMethodIdent(
        resolved_type_ident,
        method_name,
    ) orelse {
        // Try looking in external modules if not found locally
        // This handles the case where the type is defined in another module
        return null;
    };

    // Find the node index for this method (for later lookup)
    const node_idx = self.module_env.getExposedNodeIndexById(qualified_method);

    return StaticDispatchLookup{
        .method_ident = qualified_method,
        .type_ident = resolved_type_ident,
        .node_idx = node_idx,
    };
}

/// Get the concrete type name from a type variable for error messages
pub fn getConcreteTypeName(self: *Self, type_var: types.Var) ?[]const u8 {
    const resolved = self.types_store.resolveVar(type_var);
    return switch (resolved.desc.content) {
        .structure => |flat| switch (flat) {
            .nominal_type => |nom| self.module_env.getIdent(nom.ident.ident_idx),
            .record, .record_unbound, .empty_record => "Record",
            .tag_union, .empty_tag_union => "Tag",
            .tuple => "Tuple",
            .fn_pure, .fn_effectful, .fn_unbound => "Function",
        },
        .alias => |alias| self.module_env.getIdent(alias.ident.ident_idx),
        else => null,
    };
}

/// The ClosureTransformer module contains lambda set types
const ClosureTransformer = @import("ClosureTransformer.zig");

/// Result of resolving an unspecialized closure
pub const ResolvedClosure = struct {
    /// The original unspecialized closure that was resolved
    unspecialized: ClosureTransformer.UnspecializedClosure,
    /// The looked up static dispatch implementation
    impl_lookup: StaticDispatchLookup,
};

/// Resolve unspecialized closures in a lambda set.
///
/// This function is called during monomorphization when we have concrete type
/// information available. For each unspecialized closure (static dispatch reference),
/// it looks up the concrete implementation.
///
/// Returns a list of resolved closures. Closures that couldn't be resolved
/// (e.g., the type doesn't have the method) are left unresolved.
pub fn resolveUnspecializedClosures(
    self: *Self,
    lambda_set: *ClosureTransformer.LambdaSet,
    type_var: types.Var,
) !std.ArrayList(ResolvedClosure) {
    var resolved = std.ArrayList(ResolvedClosure).empty;
    errdefer resolved.deinit(self.allocator);

    // Iterate through unspecialized closures
    var i: usize = 0;
    while (i < lambda_set.unspecialized.items.len) {
        const unspec = lambda_set.unspecialized.items[i];

        // Look up the static dispatch implementation for this concrete type
        if (self.lookupStaticDispatch(type_var, unspec.member)) |impl| {
            // Found the implementation - add to resolved list
            try resolved.append(self.allocator, .{
                .unspecialized = unspec,
                .impl_lookup = impl,
            });

            // Remove from unspecialized list (swap with last element)
            _ = lambda_set.unspecialized.swapRemove(i);
            // Don't increment i since we removed an element
        } else {
            // Implementation not found for this type
            // This could mean the type doesn't have the method
            // Keep it in unspecialized for now (error will be reported later)
            i += 1;
        }
    }

    return resolved;
}

/// Resolve unspecialized closures using type substitutions.
///
/// This is an alternative entry point that uses a type substitution map
/// to resolve the type from the expression associated with each unspecialized closure.
pub fn resolveUnspecializedClosuresWithSubstitutions(
    self: *Self,
    lambda_set: *ClosureTransformer.LambdaSet,
    type_substitutions: *const types.VarMap,
) !std.ArrayList(ResolvedClosure) {
    var resolved = std.ArrayList(ResolvedClosure).empty;
    errdefer resolved.deinit(self.allocator);

    var i: usize = 0;
    while (i < lambda_set.unspecialized.items.len) {
        const unspec = lambda_set.unspecialized.items[i];

        // Get the type variable for this expression
        const expr = self.module_env.store.getExpr(unspec.member_expr);
        const type_var: ?types.Var = switch (expr) {
            .e_type_var_dispatch => |tvd| blk: {
                // Get the type var from the type var alias statement
                const stmt = self.module_env.store.getStatement(tvd.type_var_alias_stmt);
                const type_var_anno = stmt.s_type_var_alias.type_var_anno;
                const ct_var = ModuleEnv.varFrom(type_var_anno);

                // Check if this type var has a substitution
                if (type_substitutions.get(ct_var)) |concrete| {
                    break :blk concrete;
                }
                break :blk ct_var;
            },
            else => null,
        };

        if (type_var) |tv| {
            // Look up the static dispatch implementation for this concrete type
            if (self.lookupStaticDispatch(tv, unspec.member)) |impl| {
                // Found the implementation - add to resolved list
                try resolved.append(self.allocator, .{
                    .unspecialized = unspec,
                    .impl_lookup = impl,
                });

                // Remove from unspecialized list
                _ = lambda_set.unspecialized.swapRemove(i);
                // Don't increment i since we removed an element
                continue;
            }
        }

        // Implementation not found or type not concrete yet
        i += 1;
    }

    return resolved;
}

/// Add a resolved static dispatch implementation as a closure to a lambda set.
///
/// This is called after `resolveUnspecializedClosures` to add the resolved
/// implementations back into the lambda set as proper closures that can be
/// used for dispatch generation.
///
/// Note: For static dispatch implementations that are simple functions (not closures with
/// captures), this creates a "pure lambda" closure info with no captures.
pub fn addResolvedToLambdaSet(
    self: *Self,
    lambda_set: *ClosureTransformer.LambdaSet,
    resolved: ResolvedClosure,
) !void {
    // Create a ClosureInfo for the resolved static dispatch implementation.
    // Static dispatch implementations are typically top-level functions without captures,
    // so we create a simple closure with the method ident as the tag name.
    //
    // Get the actual method's expression instead of the e_type_var_dispatch expression.
    // If we have a node_idx, use it to get the method definition's expression.
    // Otherwise, fall back to the member_expr from the unspecialized closure.
    const lambda_body = if (resolved.impl_lookup.node_idx) |node_idx| blk: {
        const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
        const def = self.module_env.store.getDef(def_idx);
        break :blk def.expr;
    } else resolved.unspecialized.member_expr;

    const closure_info = ClosureTransformer.ClosureInfo{
        .tag_name = resolved.impl_lookup.method_ident,
        .lambda_body = lambda_body,
        .lambda_args = CIR.Pattern.Span{ .span = base.DataSpan.empty() },
        .capture_names = std.ArrayList(base.Ident.Idx).empty,
        .lifted_fn_pattern = null, // Will be set during further processing if needed
        .lifted_captures_pattern = null, // No captures for top-level static dispatch impls
    };

    try lambda_set.addClosure(self.allocator, closure_info);
}

/// Process all resolved closures and add them to the lambda set.
///
/// This is a convenience function that iterates over the resolved closures
/// from `resolveUnspecializedClosures` and adds them all to the lambda set.
pub fn addAllResolvedToLambdaSet(
    self: *Self,
    lambda_set: *ClosureTransformer.LambdaSet,
    resolved_list: *std.ArrayList(ResolvedClosure),
) !void {
    for (resolved_list.items) |resolved| {
        try self.addResolvedToLambdaSet(lambda_set, resolved);
    }
}

/// Resolve all unspecialized entries that depend on a type variable.
///
/// When a type variable becomes concrete during monomorphization, this function
/// finds all unspecialized lambda set entries that depend on it and resolves them.
///
/// The resolution follows a specific order:
/// - Entries are sorted by region DESCENDING (innermost/highest region first)
/// - This ensures that nested lambda sets are resolved before their enclosing ones
/// - This is critical for correct ambient function unification
///
/// Parameters:
/// - tracker: The UnspecializedByTypeVar tracker from the ClosureTransformer
/// - type_var: The polymorphic type variable that has become concrete
/// - concrete_type: The concrete type that type_var has been unified with
///
/// Returns the number of entries that were successfully resolved.
pub fn resolveEntriesForTypeVar(
    self: *Self,
    tracker: *ClosureTransformer.UnspecializedByTypeVar,
    type_var: types.Var,
    concrete_type: types.Var,
) !usize {
    const entry_refs = tracker.getEntriesForVar(type_var) orelse return 0;

    // If no entries, nothing to do
    if (entry_refs.len == 0) return 0;

    // Step 1: Flatten lambda sets that have multiple entries for the same concrete type.
    // This is required when a lambda set has multiple unspecialized entries (e.g., C:f:1 + C:f:2)
    // that all depend on the same type variable becoming concrete.
    // Each entry needs its own lambda set after flattening.
    //
    // See Rust implementation: specialize.rs:331-400
    var flattened_lambda_sets = std.ArrayList(*ClosureTransformer.LambdaSet).empty;
    defer flattened_lambda_sets.deinit(self.allocator);

    // Find unique lambda sets and flatten each one
    var seen_lambda_sets = std.AutoHashMap(*ClosureTransformer.LambdaSet, void).init(self.allocator);
    defer seen_lambda_sets.deinit();

    for (entry_refs) |entry_ref| {
        const gop = try seen_lambda_sets.getOrPut(entry_ref.lambda_set);
        if (!gop.found_existing) {
            // Flatten this lambda set if it has multiple entries for the concrete type
            var additional_sets = try entry_ref.lambda_set.flattenForConcreteType(
                self.allocator,
                concrete_type,
                self.types_store,
            );
            defer additional_sets.deinit(self.allocator);

            // Track the additional lambda sets for processing
            for (additional_sets.items) |new_set| {
                try flattened_lambda_sets.append(self.allocator, new_set);
            }
        }
    }

    // Step 2: Collect all entries to process (from original and flattened lambda sets)
    // We need to re-fetch entry_refs since flattening may have modified indices
    const updated_entry_refs = tracker.getEntriesForVar(type_var) orelse return 0;

    var all_entries = std.ArrayList(ClosureTransformer.UnspecializedEntryRef).empty;
    defer all_entries.deinit(self.allocator);

    // Add original entries
    for (updated_entry_refs) |entry_ref| {
        try all_entries.append(self.allocator, entry_ref);
    }

    // Add entries from flattened lambda sets
    for (flattened_lambda_sets.items) |lambda_set| {
        for (lambda_set.unspecialized.items, 0..) |_, i| {
            try all_entries.append(self.allocator, .{
                .lambda_set = lambda_set,
                .index = i,
            });
        }
    }

    // Sort by region DESCENDING (innermost first - higher region numbers first)
    std.mem.sort(ClosureTransformer.UnspecializedEntryRef, all_entries.items, {}, compareByRegionDesc);

    var resolved_count: usize = 0;

    // Step 3: Process each entry in region order
    for (all_entries.items) |entry_ref| {
        // Validate the entry is still valid (index might be stale after swapRemove)
        if (entry_ref.index >= entry_ref.lambda_set.unspecialized.items.len) {
            continue;
        }

        const unspec = entry_ref.lambda_set.unspecialized.items[entry_ref.index];

        // Look up the static dispatch implementation for the concrete type
        if (self.lookupStaticDispatch(concrete_type, unspec.member)) |impl| {
            // Resolve this entry
            const resolved = ResolvedClosure{
                .unspecialized = unspec,
                .impl_lookup = impl,
            };

            // Add to the lambda set as a closure
            try self.addResolvedToLambdaSet(entry_ref.lambda_set, resolved);

            // Remove from unspecialized list
            // Note: Using swapRemove changes indices, which is why we process all entries
            // in one pass and then remove the tracking entirely
            _ = entry_ref.lambda_set.unspecialized.swapRemove(entry_ref.index);

            resolved_count += 1;
        }
        // If not found, leave it in unspecialized for error reporting later
    }

    // Remove tracking for this type variable (entries are now resolved or will error)
    tracker.removeVar(type_var);

    return resolved_count;
}

/// Comparison function for sorting UnspecializedEntryRef by region descending.
fn compareByRegionDesc(
    _: void,
    a: ClosureTransformer.UnspecializedEntryRef,
    b: ClosureTransformer.UnspecializedEntryRef,
) bool {
    // Handle potential out-of-bounds indices gracefully
    const a_region = if (a.index < a.lambda_set.unspecialized.items.len)
        a.lambda_set.unspecialized.items[a.index].region
    else
        0;
    const b_region = if (b.index < b.lambda_set.unspecialized.items.len)
        b.lambda_set.unspecialized.items[b.index].region
    else
        0;
    return a_region > b_region; // Descending order
}

/// Unify ambient function types when resolving an unspecialized entry.
///
/// When an unspecialized lambda set entry is resolved to a concrete implementation,
/// we need to unify the ambient function type of the original lambda set with
/// the ambient function type of the implementation's lambda set.
///
/// This is critical for connecting type variables across lambda set boundaries:
/// - The original lambda set has an ambient function (the enclosing function)
/// - The resolved implementation also has its own ambient function
/// - Unifying them ensures that type variables in both contexts are connected
///
/// Parameters:
/// - original_lambda_set: The lambda set containing the unspecialized entry
/// - impl_lambda_set: The lambda set from the resolved implementation (may be null)
///
/// Note: If either lambda set lacks an ambient function, no unification occurs.
pub fn unifyAmbientFunctions(
    self: *Self,
    original_lambda_set: *ClosureTransformer.LambdaSet,
    impl_lambda_set: ?*ClosureTransformer.LambdaSet,
) void {
    const impl_set = impl_lambda_set orelse return;
    const original_ambient = original_lambda_set.ambient_function_var orelse return;
    const impl_ambient = impl_set.ambient_function_var orelse return;

    // Skip if they're the same variable
    if (original_ambient == impl_ambient) return;

    // Unify by linking the original ambient to point to the implementation ambient.
    // This connects the type variables from both contexts.
    //
    // We resolve both variables first to get their actual descriptors, then
    // create a union that preserves the implementation's type information.
    const original_data = self.types_store.resolveVarAndCompressPath(original_ambient);
    const impl_data = self.types_store.resolveVarAndCompressPath(impl_ambient);

    // Get the descriptor from the implementation (it has the concrete type info)
    const impl_desc = self.types_store.getDesc(impl_data.desc_idx);

    // Link original -> impl, using the impl's descriptor
    self.types_store.union_(original_data.var_, impl_data.var_, impl_desc);
}

/// Resolve unspecialized entries with ambient function unification.
///
/// This is an enhanced version of `resolveEntriesForTypeVar` that also performs
/// ambient function unification for each resolved entry.
///
/// The unification connects type variables across lambda set boundaries,
/// which is essential for correct type inference with nested closures.
///
/// Parameters:
/// - tracker: The UnspecializedByTypeVar tracker
/// - type_var: The polymorphic type variable that has become concrete
/// - concrete_type: The concrete type that type_var has been unified with
/// - impl_lambda_sets: Optional map from method ident to the implementation's lambda set.
///   If provided, ambient function unification will be performed.
pub fn resolveEntriesForTypeVarWithUnification(
    self: *Self,
    tracker: *ClosureTransformer.UnspecializedByTypeVar,
    type_var: types.Var,
    concrete_type: types.Var,
    impl_lambda_sets: ?*const std.AutoHashMap(base.Ident.Idx, *ClosureTransformer.LambdaSet),
) !usize {
    const entry_refs = tracker.getEntriesForVar(type_var) orelse return 0;

    if (entry_refs.len == 0) return 0;

    // Copy to slice for sorting
    const entries = try self.allocator.alloc(ClosureTransformer.UnspecializedEntryRef, entry_refs.len);
    defer self.allocator.free(entries);
    @memcpy(entries, entry_refs);

    // Sort by region DESCENDING (innermost first)
    std.mem.sort(ClosureTransformer.UnspecializedEntryRef, entries, {}, compareByRegionDesc);

    var resolved_count: usize = 0;

    for (entries) |entry_ref| {
        if (entry_ref.index >= entry_ref.lambda_set.unspecialized.items.len) {
            continue;
        }

        const unspec = entry_ref.lambda_set.unspecialized.items[entry_ref.index];

        if (self.lookupStaticDispatch(concrete_type, unspec.member)) |impl| {
            // Perform ambient function unification if we have the implementation's lambda set
            if (impl_lambda_sets) |ls_map| {
                if (ls_map.get(impl.method_ident)) |impl_ls| {
                    self.unifyAmbientFunctions(entry_ref.lambda_set, impl_ls);
                }
            }

            // Resolve this entry
            const resolved = ResolvedClosure{
                .unspecialized = unspec,
                .impl_lookup = impl,
            };

            try self.addResolvedToLambdaSet(entry_ref.lambda_set, resolved);
            _ = entry_ref.lambda_set.unspecialized.swapRemove(entry_ref.index);
            resolved_count += 1;
        }
    }

    tracker.removeVar(type_var);

    return resolved_count;
}

/// Register a polymorphic function definition as a partial proc.
/// This should be called for each function definition during the finding phase.
pub fn registerPartialProc(
    self: *Self,
    ident: base.Ident.Idx,
    body_expr: Expr.Idx,
    arg_patterns: CIR.Pattern.Span,
    type_var: types.Var,
    is_top_level: bool,
) !void {
    const partial = PartialProc{
        .original_ident = ident,
        .body_expr = body_expr,
        .arg_patterns = arg_patterns,
        .type_var = type_var,
        .is_top_level = is_top_level,
    };
    try self.partial_procs.put(ident, partial);
}

/// Check if a function is registered as a partial proc (can be specialized)
pub fn isPartialProc(self: *const Self, ident: base.Ident.Idx) bool {
    return self.partial_procs.contains(ident);
}

/// Request a specialization for a function at a specific concrete type.
/// This is called when we find a call site that uses a polymorphic function
/// with concrete types.
///
/// In the finding phase, this adds to pending_specializations.
/// In the making phase, this may recursively create the specialization.
pub fn requestSpecialization(
    self: *Self,
    original_ident: base.Ident.Idx,
    concrete_type: types.Var,
    call_site: ?Expr.Idx,
) !?base.Ident.Idx {
    const type_hash = self.structuralTypeHash(concrete_type);
    const key = SpecializationKey{
        .original_ident = original_ident,
        .type_hash = type_hash,
    };

    // Check if we already have this specialization
    if (self.specialization_names.get(key)) |specialized_name| {
        return specialized_name;
    }

    // Check if it's already pending - if so, look up or create the specialized name
    for (self.pending_specializations.items) |pending| {
        if (pending.original_ident == original_ident) {
            const pending_hash = self.structuralTypeHash(pending.concrete_type);
            if (pending_hash == type_hash) {
                // Already pending - create and return the specialized name so call sites
                // can reference it. The name will be reused when the spec is processed.
                const specialized_name = try self.createSpecializedName(original_ident, concrete_type);
                return specialized_name;
            }
        }
    }

    // Check if this function is registered as a partial proc
    if (!self.partial_procs.contains(original_ident)) {
        // Not a polymorphic function we know about - might be external
        return null;
    }

    // Create the specialized name now so call sites can reference it.
    // This ensures all references to the same specialization use the same name.
    const specialized_name = try self.createSpecializedName(original_ident, concrete_type);

    // Create type substitutions by unifying the polymorphic type with concrete
    const type_subs = types.VarMap.init(self.allocator);
    // For now, we'll populate this during body duplication

    // Add to pending
    try self.pending_specializations.append(self.allocator, PendingSpecialization{
        .original_ident = original_ident,
        .concrete_type = concrete_type,
        .call_site = call_site,
        .type_substitutions = type_subs,
    });

    return specialized_name;
}

/// Request a specialization from an external module.
/// This is used when a function call crosses module boundaries and the
/// function is polymorphic in the external module.
///
/// Returns the specialized name if already known, or adds to external_requests
/// for later resolution.
pub fn requestExternalSpecialization(
    self: *Self,
    source_module: CIR.Import.Idx,
    original_ident: base.Ident.Idx,
    concrete_type: types.Var,
    call_site: ?Expr.Idx,
) !ExternalSpecializationResult {
    // Check if we already have this request (same module, ident, and concrete type)
    const new_type_hash = self.structuralTypeHash(concrete_type);

    // First, check if this has already been resolved
    const ext_key = ExternalSpecKey{
        .source_module = source_module,
        .original_ident = original_ident,
        .type_hash = new_type_hash,
    };
    if (self.resolved_external_specs.get(ext_key)) |resolved_ident| {
        return ExternalSpecializationResult{
            .specialized_ident = resolved_ident,
            .is_new = false,
        };
    }

    // Check if we already have a pending request
    for (self.external_requests.items) |existing| {
        if (existing.source_module == source_module and
            existing.original_ident == original_ident)
        {
            // Compare concrete types using structural type hashing
            const existing_type_hash = self.structuralTypeHash(existing.concrete_type);
            if (existing_type_hash == new_type_hash) {
                // Pending but not yet resolved - create/lookup the specialized name
                // so all call sites use a consistent reference
                const specialized_name = try self.createSpecializedName(original_ident, concrete_type);
                return ExternalSpecializationResult{
                    .specialized_ident = specialized_name,
                    .is_new = false,
                };
            }
        }
    }

    // Create the specialized name now so all call sites use a consistent reference.
    // When the external module resolves this, it will use the same naming scheme.
    const specialized_name = try self.createSpecializedName(original_ident, concrete_type);

    // Add new external request
    try self.external_requests.append(self.allocator, ExternalSpecializationRequest{
        .source_module = source_module,
        .original_ident = original_ident,
        .concrete_type = concrete_type,
        .call_site = call_site,
    });

    return ExternalSpecializationResult{
        .specialized_ident = specialized_name,
        .is_new = true,
    };
}

/// Get all external specialization requests.
/// This should be called after the finding phase to get the list of
/// specializations needed from external modules.
pub fn getExternalRequests(self: *const Self) []const ExternalSpecializationRequest {
    return self.external_requests.items;
}

/// Verify that all external specialization requests have been resolved.
/// This should be called before code generation to ensure no unresolved
/// external specializations remain.
///
/// Returns a list of unresolved requests. If empty, all are resolved.
pub fn getUnresolvedExternalRequests(self: *Self) !std.ArrayList(ExternalSpecializationRequest) {
    var unresolved = std.ArrayList(ExternalSpecializationRequest).empty;

    for (self.external_requests.items) |request| {
        const type_hash = self.structuralTypeHash(request.concrete_type);
        const ext_key = ExternalSpecKey{
            .source_module = request.source_module,
            .original_ident = request.original_ident,
            .type_hash = type_hash,
        };

        if (!self.resolved_external_specs.contains(ext_key)) {
            try unresolved.append(self.allocator, request);
        }
    }

    return unresolved;
}

/// Check if all external specialization requests have been resolved.
/// Returns true if all are resolved, false otherwise.
pub fn allExternalSpecializationsResolved(self: *Self) bool {
    for (self.external_requests.items) |request| {
        const type_hash = self.structuralTypeHash(request.concrete_type);
        const ext_key = ExternalSpecKey{
            .source_module = request.source_module,
            .original_ident = request.original_ident,
            .type_hash = type_hash,
        };

        if (!self.resolved_external_specs.contains(ext_key)) {
            return false;
        }
    }
    return true;
}

/// Resolve an external specialization request with the actual specialized name.
/// Called when the external module provides the specialized function name.
pub fn resolveExternalSpecialization(
    self: *Self,
    source_module: CIR.Import.Idx,
    original_ident: base.Ident.Idx,
    specialized_ident: base.Ident.Idx,
    concrete_type: types.Var,
) !void {
    const type_hash = self.structuralTypeHash(concrete_type);

    // Store in the local specialization names for consistency
    const key = SpecializationKey{
        .original_ident = original_ident,
        .type_hash = type_hash,
    };
    try self.specialization_names.put(key, specialized_ident);

    // Store in the external specs map for cross-module lookups
    const ext_key = ExternalSpecKey{
        .source_module = source_module,
        .original_ident = original_ident,
        .type_hash = type_hash,
    };
    try self.resolved_external_specs.put(ext_key, specialized_ident);
}

/// Process all pending specializations.
/// This is the main entry point for the making phase.
pub fn processPendingSpecializations(self: *Self) !void {
    self.phase = .making;

    // Process until no more pending (may add more during processing)
    while (self.pending_specializations.items.len > 0) {
        // Pop from the end (LIFO order for better cache locality)
        var pending = self.pending_specializations.pop() orelse break;

        try self.makeSpecialization(&pending);

        // Clean up the type substitutions map
        pending.type_substitutions.deinit();
    }

    self.phase = .finding;
}

/// Create a single specialization from a pending request.
fn makeSpecialization(self: *Self, pending: *PendingSpecialization) !void {
    const type_hash = self.structuralTypeHash(pending.concrete_type);
    const key = SpecializationKey{
        .original_ident = pending.original_ident,
        .type_hash = type_hash,
    };

    // Check if already made (could have been created by a recursive call)
    if (self.specialized.contains(key)) {
        return;
    }

    // Check for infinite recursion
    if (self.in_progress.contains(key)) {
        // We're in the middle of making this specialization - this is a recursive call.
        // The forward reference will be resolved when we complete the outer specialization.
        return;
    }

    // Polymorphic recursion (infinite types) should be caught by the type checker,
    // so we should never hit the recursion depth limit here. If we do, it's a bug.
    std.debug.assert(self.recursion_depth < self.max_recursion_depth);

    // Get the partial proc
    const partial = self.partial_procs.get(pending.original_ident) orelse return;

    // Mark as in progress and track depth
    try self.in_progress.put(key, {});
    try self.specialization_stack.append(self.allocator, key);
    self.recursion_depth += 1;

    defer {
        _ = self.in_progress.remove(key);
        _ = self.specialization_stack.pop();
        self.recursion_depth -= 1;
    }

    // Create the specialized name
    const specialized_ident = try self.createSpecializedName(
        pending.original_ident,
        pending.concrete_type,
    );

    // Register the name mapping first (for recursive references)
    try self.specialization_names.put(key, specialized_ident);

    // Build type substitutions from the polymorphic type to the concrete type
    try self.buildTypeSubstitutions(
        partial.type_var,
        pending.concrete_type,
        &pending.type_substitutions,
    );

    // Resolve unspecialized lambda set entries for type variables that became concrete.
    // This is the key integration point where static dispatch closures get resolved.
    if (self.closure_transformer) |transformer| {
        // For each type variable that has a concrete mapping, resolve its unspecialized entries
        var iter = pending.type_substitutions.iterator();
        while (iter.next()) |entry| {
            const poly_var = entry.key_ptr.*;
            const concrete_var = entry.value_ptr.*;

            // Resolve unspecialized entries with ambient function unification
            _ = try self.resolveEntriesForTypeVarWithUnification(
                &transformer.unspec_by_type_var,
                poly_var,
                concrete_var,
                self.impl_lambda_sets,
            );
        }
    }

    // Duplicate the body with type substitutions
    const specialized_body = try self.duplicateBody(
        partial.body_expr,
        &pending.type_substitutions,
    );

    // Duplicate argument patterns
    const specialized_args = try self.duplicatePatternSpan(partial.arg_patterns);

    // Create the specialized proc
    const specialized = SpecializedProc{
        .specialized_ident = specialized_ident,
        .body_expr = specialized_body,
        .arg_patterns = specialized_args,
        .concrete_type = pending.concrete_type,
        .original_ident = pending.original_ident,
    };

    try self.specialized.put(key, specialized);
}

/// Build type substitutions by comparing polymorphic type with concrete type.
/// This populates the VarMap with mappings from polymorphic vars to concrete vars.
fn buildTypeSubstitutions(
    self: *Self,
    polymorphic_var: types.Var,
    concrete_var: types.Var,
    var_map: *types.VarMap,
) std.mem.Allocator.Error!void {
    const poly_resolved = self.types_store.resolveVar(polymorphic_var);
    const concrete_resolved = self.types_store.resolveVar(concrete_var);

    // If the polymorphic type is a flex or rigid, map it to the concrete type
    switch (poly_resolved.desc.content) {
        .flex, .rigid => {
            try var_map.put(poly_resolved.var_, concrete_resolved.var_);
            return;
        },
        else => {},
    }

    // Otherwise, recursively compare structure
    switch (poly_resolved.desc.content) {
        .structure => |poly_flat| {
            const concrete_content = concrete_resolved.desc.content;
            switch (concrete_content) {
                .structure => |concrete_flat| {
                    try self.buildFlatTypeSubstitutions(poly_flat, concrete_flat, var_map);
                },
                else => {},
            }
        },
        .alias => |poly_alias| {
            const concrete_content = concrete_resolved.desc.content;
            switch (concrete_content) {
                .alias => |concrete_alias| {
                    // Compare alias type arguments
                    var poly_iter = self.types_store.iterAliasArgs(poly_alias);
                    var concrete_iter = self.types_store.iterAliasArgs(concrete_alias);

                    while (poly_iter.next()) |poly_arg| {
                        if (concrete_iter.next()) |concrete_arg| {
                            try self.buildTypeSubstitutions(poly_arg, concrete_arg, var_map);
                        }
                    }

                    // Also compare backing vars
                    const poly_backing = self.types_store.getAliasBackingVar(poly_alias);
                    const concrete_backing = self.types_store.getAliasBackingVar(concrete_alias);
                    try self.buildTypeSubstitutions(poly_backing, concrete_backing, var_map);
                },
                else => {},
            }
        },
        else => {},
    }
}

/// Build substitutions for flat types
fn buildFlatTypeSubstitutions(
    self: *Self,
    poly_flat: types.FlatType,
    concrete_flat: types.FlatType,
    var_map: *types.VarMap,
) std.mem.Allocator.Error!void {
    switch (poly_flat) {
        .fn_pure, .fn_effectful, .fn_unbound => |poly_func| {
            const concrete_func = switch (concrete_flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |f| f,
                else => return,
            };

            // Compare arguments
            const poly_args = self.types_store.sliceVars(poly_func.args);
            const concrete_args = self.types_store.sliceVars(concrete_func.args);

            const min_len = @min(poly_args.len, concrete_args.len);
            for (0..min_len) |i| {
                try self.buildTypeSubstitutions(poly_args[i], concrete_args[i], var_map);
            }

            // Compare return types
            try self.buildTypeSubstitutions(poly_func.ret, concrete_func.ret, var_map);
        },
        .record => |poly_record| {
            const concrete_record = switch (concrete_flat) {
                .record => |r| r,
                else => return,
            };

            // Compare field types
            const poly_fields = self.types_store.getRecordFieldsSlice(poly_record.fields);
            const concrete_fields = self.types_store.getRecordFieldsSlice(concrete_record.fields);

            const min_len = @min(poly_fields.len, concrete_fields.len);
            for (0..min_len) |i| {
                try self.buildTypeSubstitutions(
                    poly_fields.items(.var_)[i],
                    concrete_fields.items(.var_)[i],
                    var_map,
                );
            }
        },
        .tuple => |poly_tuple| {
            const concrete_tuple = switch (concrete_flat) {
                .tuple => |t| t,
                else => return,
            };

            const poly_elems = self.types_store.sliceVars(poly_tuple.elems);
            const concrete_elems = self.types_store.sliceVars(concrete_tuple.elems);

            const min_len = @min(poly_elems.len, concrete_elems.len);
            for (0..min_len) |i| {
                try self.buildTypeSubstitutions(poly_elems[i], concrete_elems[i], var_map);
            }
        },
        .tag_union => |poly_union| {
            const concrete_union = switch (concrete_flat) {
                .tag_union => |u| u,
                else => return,
            };

            // Compare extension types
            try self.buildTypeSubstitutions(poly_union.ext, concrete_union.ext, var_map);
        },
        .nominal_type => |poly_nom| {
            const concrete_nom = switch (concrete_flat) {
                .nominal_type => |n| n,
                else => return,
            };

            // Compare type arguments
            const poly_vars = self.types_store.sliceVars(poly_nom.vars.nonempty);
            const concrete_vars = self.types_store.sliceVars(concrete_nom.vars.nonempty);

            const min_len = @min(poly_vars.len, concrete_vars.len);
            for (0..min_len) |i| {
                try self.buildTypeSubstitutions(poly_vars[i], concrete_vars[i], var_map);
            }
        },
        .record_unbound, .empty_record, .empty_tag_union => {},
    }
}

/// Instantiate a type variable using the type substitutions.
/// Returns the concrete type for a polymorphic type.
pub fn instantiateType(
    self: *Self,
    type_var: types.Var,
    var_map: *std.AutoHashMap(types.Var, types.Var),
) std.mem.Allocator.Error!types.Var {
    var instantiator = Instantiator{
        .store = self.types_store,
        .idents = self.module_env.getIdentStoreConst(),
        .var_map = var_map,
        .current_rank = types.Rank.outermost,
        .rigid_behavior = .fresh_flex,
    };

    return try instantiator.instantiateVar(type_var);
}

/// Duplicate a function body, substituting polymorphic types with concrete ones.
/// Also discovers and queues additional specializations needed.
fn duplicateBody(
    self: *Self,
    original_body: Expr.Idx,
    type_substitutions: *const types.VarMap,
) !Expr.Idx {
    return try self.duplicateExpr(original_body, type_substitutions);
}

/// Recursively duplicate an expression tree with type substitutions.
fn duplicateExpr(
    self: *Self,
    expr_idx: Expr.Idx,
    type_subs: *const types.VarMap,
) std.mem.Allocator.Error!Expr.Idx {
    const expr = self.module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_call => |call| {
            // Check if this is a call to a polymorphic function we should specialize
            var specialized_func: ?Expr.Idx = null;
            const func_expr = self.module_env.store.getExpr(call.func);
            switch (func_expr) {
                .e_lookup_local => |lookup| {
                    // Get the identifier for this pattern
                    const pattern = self.module_env.store.getPattern(lookup.pattern_idx);
                    switch (pattern) {
                        .assign => |assign| {
                            // Check if this is a call to a partial proc
                            if (self.partial_procs.get(assign.ident)) |partial_proc| {
                                // Get the concrete type by applying substitutions to the polymorphic type
                                const concrete_type = type_subs.get(partial_proc.type_var) orelse partial_proc.type_var;

                                // Request a specialization for this concrete type
                                if (try self.requestSpecialization(assign.ident, concrete_type, expr_idx)) |specialized_ident| {
                                    // Create a new pattern for the specialized function
                                    const new_pattern = try self.module_env.store.addPattern(
                                        Pattern{ .assign = .{ .ident = specialized_ident } },
                                        base.Region.zero(),
                                    );
                                    // Create a new lookup expression pointing to the specialized function
                                    specialized_func = try self.module_env.store.addExpr(Expr{
                                        .e_lookup_local = .{ .pattern_idx = new_pattern },
                                    }, base.Region.zero());
                                }
                                // If requestSpecialization returned null, the function is not a known
                                // partial proc (it might be external or already monomorphic).
                            }
                        },
                        else => {},
                    }
                },
                else => {},
            }

            // Use the specialized function if available, otherwise duplicate the original
            const new_func = specialized_func orelse try self.duplicateExpr(call.func, type_subs);

            // Normalize arguments: complex expressions become let-bindings, simple lookups stay as-is.
            // This ensures all call arguments are simple symbol references by the time RC runs,
            // matching the architecture of crates/compiler/mono/src/inc_dec.rs.
            const args = self.module_env.store.sliceExpr(call.args);
            const args_start = self.module_env.store.scratch.?.exprs.top();
            const stmts_start = self.module_env.store.scratchTop("statements");
            var needs_block = false;

            for (args) |arg_idx| {
                const new_arg = try self.duplicateExpr(arg_idx, type_subs);
                const arg_expr = self.module_env.store.getExpr(new_arg);

                // Check if this argument needs normalization (is not a simple lookup)
                const is_simple = switch (arg_expr) {
                    .e_lookup_local => true,
                    // Literals and other non-refcounted values don't need normalization
                    .e_num, .e_frac_f32, .e_frac_f64, .e_dec, .e_dec_small,
                    .e_typed_int, .e_typed_frac, .e_zero_argument_tag,
                    .e_empty_record => true,
                    else => false,
                };

                if (is_simple) {
                    try self.module_env.store.scratch.?.exprs.append(new_arg);
                } else {
                    // Create a synthetic let-binding for this complex argument
                    // Use # prefix which is invalid in Roc syntax (starts a comment)
                    var name_buf: [32]u8 = undefined;
                    const name = std.fmt.bufPrint(&name_buf, "#arg{d}", .{self.synthetic_arg_counter}) catch "#arg";
                    self.synthetic_arg_counter += 1;

                    // Create the synthetic identifier and pattern
                    const ident = base.Ident.for_text(name);
                    const ident_idx = try self.module_env.insertIdent(ident);
                    const pattern_idx = try self.module_env.store.addPattern(
                        Pattern{ .assign = .{ .ident = ident_idx } },
                        base.Region.zero(),
                    );

                    // Create the let-binding statement: #argN = complex_expr
                    const decl_stmt = try self.module_env.store.addStatement(
                        .{ .s_decl = .{
                            .pattern = pattern_idx,
                            .expr = new_arg,
                            .anno = null,
                        } },
                        base.Region.zero(),
                    );
                    try self.module_env.store.addScratchStatement(decl_stmt);
                    needs_block = true;

                    // Replace the argument with a lookup to the synthetic binding
                    const lookup_expr = try self.module_env.store.addExpr(
                        Expr{ .e_lookup_local = .{ .pattern_idx = pattern_idx } },
                        base.Region.zero(),
                    );
                    try self.module_env.store.scratch.?.exprs.append(lookup_expr);
                }
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);

            // Create the call expression
            const call_expr = try self.module_env.store.addExpr(Expr{
                .e_call = .{
                    .func = new_func,
                    .args = new_args_span,
                    .called_via = call.called_via,
                },
            }, base.Region.zero());

            // If we created synthetic bindings, wrap the call in a block
            if (needs_block) {
                const stmts_span = try self.module_env.store.statementSpanFrom(stmts_start);
                return try self.module_env.store.addExpr(
                    Expr{ .e_block = .{
                        .stmts = stmts_span,
                        .final_expr = call_expr,
                    } },
                    base.Region.zero(),
                );
            }

            return call_expr;
        },

        .e_lambda => |lambda| {
            // Duplicate the lambda body
            const new_body = try self.duplicateExpr(lambda.body, type_subs);

            // Duplicate argument patterns
            const new_args = try self.duplicatePatternSpan(lambda.args);

            if (new_body == lambda.body and new_args.span.start == lambda.args.span.start) {
                return expr_idx;
            }

            return try self.module_env.store.addExpr(Expr{
                .e_lambda = .{
                    .args = new_args,
                    .body = new_body,
                },
            }, base.Region.zero());
        },

        .e_closure => |closure| {
            // Duplicate the inner lambda
            const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
            switch (lambda_expr) {
                .e_lambda => |lambda| {
                    const new_body = try self.duplicateExpr(lambda.body, type_subs);

                    if (new_body == lambda.body) {
                        return expr_idx;
                    }

                    const new_lambda = try self.module_env.store.addExpr(Expr{
                        .e_lambda = .{
                            .args = lambda.args,
                            .body = new_body,
                        },
                    }, base.Region.zero());

                    return try self.module_env.store.addExpr(Expr{
                        .e_closure = .{
                            .lambda_idx = new_lambda,
                            .captures = closure.captures,
                            .tag_name = closure.tag_name,
                        },
                    }, base.Region.zero());
                },
                else => return expr_idx,
            }
        },

        .e_block => |block| {
            // Duplicate block statements and final expression
            const stmts = self.module_env.store.sliceStatements(block.stmts);
            const stmt_start = self.module_env.store.scratch.?.statements.top();

            for (stmts) |stmt_idx| {
                const stmt = self.module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        const new_expr = try self.duplicateExpr(decl.expr, type_subs);
                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl = .{
                                .pattern = decl.pattern,
                                .expr = new_expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    .s_decl_gen => |decl| {
                        const new_expr = try self.duplicateExpr(decl.expr, type_subs);
                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl_gen = .{
                                .pattern = decl.pattern,
                                .expr = new_expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    else => {
                        try self.module_env.store.scratch.?.statements.append(stmt_idx);
                    },
                }
            }

            const new_stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);
            const new_final = try self.duplicateExpr(block.final_expr, type_subs);

            return try self.module_env.store.addExpr(Expr{
                .e_block = .{
                    .stmts = new_stmts_span,
                    .final_expr = new_final,
                },
            }, base.Region.zero());
        },

        .e_if => |if_expr| {
            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
            const branch_start = self.module_env.store.scratch.?.if_branches.top();

            for (branches) |branch_idx| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                const new_cond = try self.duplicateExpr(branch.cond, type_subs);
                const new_body = try self.duplicateExpr(branch.body, type_subs);

                const new_branch_idx = try self.module_env.store.addIfBranch(
                    Expr.IfBranch{ .cond = new_cond, .body = new_body },
                    base.Region.zero(),
                );
                try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
            }

            const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);
            const new_else = try self.duplicateExpr(if_expr.final_else, type_subs);

            return try self.module_env.store.addExpr(Expr{
                .e_if = .{
                    .branches = new_branches_span,
                    .final_else = new_else,
                },
            }, base.Region.zero());
        },

        .e_binop => |binop| {
            const new_lhs = try self.duplicateExpr(binop.lhs, type_subs);
            const new_rhs = try self.duplicateExpr(binop.rhs, type_subs);

            if (new_lhs == binop.lhs and new_rhs == binop.rhs) {
                return expr_idx;
            }

            return try self.module_env.store.addExpr(Expr{
                .e_binop = .{
                    .op = binop.op,
                    .lhs = new_lhs,
                    .rhs = new_rhs,
                },
            }, base.Region.zero());
        },

        .e_list => |list| {
            const elems = self.module_env.store.sliceExpr(list.elems);
            const elems_start = self.module_env.store.scratch.?.exprs.top();

            for (elems) |elem_idx| {
                const new_elem = try self.duplicateExpr(elem_idx, type_subs);
                try self.module_env.store.scratch.?.exprs.append(new_elem);
            }

            const new_elems_span = try self.module_env.store.exprSpanFrom(elems_start);

            return try self.module_env.store.addExpr(Expr{
                .e_list = .{ .elems = new_elems_span },
            }, base.Region.zero());
        },

        .e_tuple => |tuple| {
            const elems = self.module_env.store.sliceExpr(tuple.elems);
            const elems_start = self.module_env.store.scratch.?.exprs.top();

            for (elems) |elem_idx| {
                const new_elem = try self.duplicateExpr(elem_idx, type_subs);
                try self.module_env.store.scratch.?.exprs.append(new_elem);
            }

            const new_elems_span = try self.module_env.store.exprSpanFrom(elems_start);

            return try self.module_env.store.addExpr(Expr{
                .e_tuple = .{ .elems = new_elems_span },
            }, base.Region.zero());
        },

        .e_record => |record| {
            const field_indices = self.module_env.store.sliceRecordFields(record.fields);
            const fields_start = self.module_env.store.scratch.?.record_fields.top();

            for (field_indices) |field_idx| {
                const field = self.module_env.store.getRecordField(field_idx);
                const new_value = try self.duplicateExpr(field.value, type_subs);

                const new_field = CIR.RecordField{
                    .name = field.name,
                    .value = new_value,
                };
                const new_field_idx = try self.module_env.store.addRecordField(new_field, base.Region.zero());
                try self.module_env.store.scratch.?.record_fields.append(new_field_idx);
            }

            const new_fields_span = try self.module_env.store.recordFieldSpanFrom(fields_start);
            const new_ext = if (record.ext) |ext| try self.duplicateExpr(ext, type_subs) else null;

            return try self.module_env.store.addExpr(Expr{
                .e_record = .{
                    .fields = new_fields_span,
                    .ext = new_ext,
                },
            }, base.Region.zero());
        },

        .e_tag => |tag| {
            const args = self.module_env.store.sliceExpr(tag.args);
            const args_start = self.module_env.store.scratch.?.exprs.top();

            for (args) |arg_idx| {
                const new_arg = try self.duplicateExpr(arg_idx, type_subs);
                try self.module_env.store.scratch.?.exprs.append(new_arg);
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);

            return try self.module_env.store.addExpr(Expr{
                .e_tag = .{
                    .name = tag.name,
                    .args = new_args_span,
                },
            }, base.Region.zero());
        },

        .e_unary_minus => |unary| {
            const new_expr = try self.duplicateExpr(unary.expr, type_subs);
            if (new_expr == unary.expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_unary_minus = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_unary_not => |unary| {
            const new_expr = try self.duplicateExpr(unary.expr, type_subs);
            if (new_expr == unary.expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_unary_not = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_dot_access => |dot| {
            const new_receiver = try self.duplicateExpr(dot.receiver, type_subs);
            const new_args = if (dot.args) |args_span| blk: {
                const args = self.module_env.store.sliceExpr(args_span);
                const args_start = self.module_env.store.scratch.?.exprs.top();

                for (args) |arg_idx| {
                    const new_arg = try self.duplicateExpr(arg_idx, type_subs);
                    try self.module_env.store.scratch.?.exprs.append(new_arg);
                }

                break :blk try self.module_env.store.exprSpanFrom(args_start);
            } else null;

            return try self.module_env.store.addExpr(Expr{
                .e_dot_access = .{
                    .receiver = new_receiver,
                    .field_name = dot.field_name,
                    .field_name_region = dot.field_name_region,
                    .args = new_args,
                },
            }, base.Region.zero());
        },

        .e_return => |ret| {
            const new_expr = try self.duplicateExpr(ret.expr, type_subs);
            if (new_expr == ret.expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_return = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_dbg => |dbg| {
            const new_expr = try self.duplicateExpr(dbg.expr, type_subs);
            if (new_expr == dbg.expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_dbg = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_expect => |expect| {
            const new_body = try self.duplicateExpr(expect.body, type_subs);
            if (new_body == expect.body) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_expect = .{ .body = new_body },
            }, base.Region.zero());
        },

        .e_match => |match| {
            const new_cond = try self.duplicateExpr(match.cond, type_subs);

            // Duplicate match branches
            const branch_indices = self.module_env.store.sliceMatchBranches(match.branches);
            const branch_start = self.module_env.store.scratchMatchBranchTop();

            for (branch_indices) |branch_idx| {
                const branch = self.module_env.store.getMatchBranch(branch_idx);

                // Duplicate the branch value (body)
                const new_value = try self.duplicateExpr(branch.value, type_subs);

                // Duplicate the guard if present
                const new_guard = if (branch.guard) |guard|
                    try self.duplicateExpr(guard, type_subs)
                else
                    null;

                // Duplicate branch patterns (these are BranchPattern entries, not regular patterns)
                const new_patterns = try self.duplicateBranchPatternSpan(branch.patterns);

                const new_branch = Expr.Match.Branch{
                    .patterns = new_patterns,
                    .value = new_value,
                    .guard = new_guard,
                    .redundant = branch.redundant,
                };
                const new_branch_idx = try self.module_env.store.addMatchBranch(new_branch, base.Region.zero());
                try self.module_env.store.addScratchMatchBranch(new_branch_idx);
            }

            const new_branches_span = try self.module_env.store.matchBranchSpanFrom(branch_start);

            return try self.module_env.store.addExpr(Expr{
                .e_match = .{
                    .cond = new_cond,
                    .branches = new_branches_span,
                    .exhaustive = match.exhaustive,
                    .is_try_suffix = match.is_try_suffix,
                },
            }, base.Region.zero());
        },

        .e_nominal => |nominal| {
            const new_backing = try self.duplicateExpr(nominal.backing_expr, type_subs);
            if (new_backing == nominal.backing_expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_nominal = .{
                    .nominal_type_decl = nominal.nominal_type_decl,
                    .backing_expr = new_backing,
                    .backing_type = nominal.backing_type,
                },
            }, base.Region.zero());
        },

        .e_nominal_external => |nominal| {
            const new_backing = try self.duplicateExpr(nominal.backing_expr, type_subs);
            if (new_backing == nominal.backing_expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_nominal_external = .{
                    .module_idx = nominal.module_idx,
                    .target_node_idx = nominal.target_node_idx,
                    .backing_expr = new_backing,
                    .backing_type = nominal.backing_type,
                },
            }, base.Region.zero());
        },

        .e_for => |for_expr| {
            const new_expr = try self.duplicateExpr(for_expr.expr, type_subs);
            const new_body = try self.duplicateExpr(for_expr.body, type_subs);
            return try self.module_env.store.addExpr(Expr{
                .e_for = .{
                    .patt = for_expr.patt,
                    .expr = new_expr,
                    .body = new_body,
                },
            }, base.Region.zero());
        },

        // Pass through simple expressions unchanged
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_typed_int,
        .e_typed_frac,
        .e_str_segment,
        .e_str,
        .e_lookup_local,
        .e_lookup_external,
        .e_empty_list,
        .e_empty_record,
        .e_zero_argument_tag,
        .e_runtime_error,
        .e_ellipsis,
        .e_anno_only,
        .e_lookup_required,
        .e_type_var_dispatch,
        .e_hosted_lambda,
        .e_low_level_lambda,
        .e_crash,
        // RC expressions are inserted after monomorphization
        .e_incref,
        .e_decref,
        .e_free,
        => return expr_idx,
    }
}

/// Duplicate a span of patterns with type substitutions.
/// Used for duplicating function argument patterns and match branch patterns.
fn duplicatePatternSpan(
    self: *Self,
    span: CIR.Pattern.Span,
) std.mem.Allocator.Error!CIR.Pattern.Span {
    const patterns = self.module_env.store.slicePatterns(span);
    if (patterns.len == 0) {
        return span; // Empty span - return as-is
    }

    const patterns_start = self.module_env.store.scratchPatternTop();

    for (patterns) |pattern_idx| {
        const new_pattern = try self.duplicatePattern(pattern_idx);
        try self.module_env.store.addScratchPattern(new_pattern);
    }

    return try self.module_env.store.patternSpanFrom(patterns_start);
}

/// Duplicate a single pattern.
fn duplicatePattern(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!CIR.Pattern.Idx {
    const pattern = self.module_env.store.getPattern(pattern_idx);

    switch (pattern) {
        .assign => |assign| {
            // Simple assignment pattern - create a new one
            return try self.module_env.store.addPattern(
                Pattern{ .assign = assign },
                base.Region.zero(),
            );
        },
        .underscore => {
            return try self.module_env.store.addPattern(
                Pattern.underscore,
                base.Region.zero(),
            );
        },
        .applied_tag => |tag| {
            // Duplicate nested patterns
            const args = self.module_env.store.slicePatterns(tag.args);
            if (args.len == 0) {
                return try self.module_env.store.addPattern(
                    Pattern{ .applied_tag = tag },
                    base.Region.zero(),
                );
            }

            const args_start = self.module_env.store.scratchPatternTop();
            for (args) |arg_idx| {
                const new_arg = try self.duplicatePattern(arg_idx);
                try self.module_env.store.addScratchPattern(new_arg);
            }
            const new_args = try self.module_env.store.patternSpanFrom(args_start);

            return try self.module_env.store.addPattern(
                Pattern{ .applied_tag = .{
                    .name = tag.name,
                    .args = new_args,
                } },
                base.Region.zero(),
            );
        },
        .record_destructure => |record| {
            // Duplicate record field patterns
            const fields = self.module_env.store.sliceRecordDestructs(record.destructs);
            if (fields.len == 0) {
                return try self.module_env.store.addPattern(
                    Pattern{ .record_destructure = record },
                    base.Region.zero(),
                );
            }

            const fields_start = self.module_env.store.scratchRecordDestructTop();
            for (fields) |field_idx| {
                const field = self.module_env.store.getRecordDestruct(field_idx);
                // Duplicate the nested pattern if present
                const new_kind: Pattern.RecordDestruct.Kind = switch (field.kind) {
                    .Required => |p_idx| .{ .Required = try self.duplicatePattern(p_idx) },
                    .SubPattern => |p_idx| .{ .SubPattern = try self.duplicatePattern(p_idx) },
                };
                const new_field = Pattern.RecordDestruct{
                    .label = field.label,
                    .ident = field.ident,
                    .kind = new_kind,
                };
                const new_field_idx = try self.module_env.store.addRecordDestruct(new_field, base.Region.zero());
                try self.module_env.store.addScratchRecordDestruct(new_field_idx);
            }
            const new_fields = try self.module_env.store.recordDestructSpanFrom(fields_start);

            return try self.module_env.store.addPattern(
                Pattern{ .record_destructure = .{
                    .destructs = new_fields,
                } },
                base.Region.zero(),
            );
        },
        .tuple => |tuple| {
            // Duplicate tuple element patterns
            const elems = self.module_env.store.slicePatterns(tuple.patterns);
            if (elems.len == 0) {
                return try self.module_env.store.addPattern(
                    Pattern{ .tuple = tuple },
                    base.Region.zero(),
                );
            }

            const elems_start = self.module_env.store.scratchPatternTop();
            for (elems) |elem_idx| {
                const new_elem = try self.duplicatePattern(elem_idx);
                try self.module_env.store.addScratchPattern(new_elem);
            }
            const new_elems = try self.module_env.store.patternSpanFrom(elems_start);

            return try self.module_env.store.addPattern(
                Pattern{ .tuple = .{
                    .patterns = new_elems,
                } },
                base.Region.zero(),
            );
        },
        .list => |list| {
            // Duplicate list element patterns
            const elems = self.module_env.store.slicePatterns(list.patterns);
            const elems_start = self.module_env.store.scratchPatternTop();
            for (elems) |elem_idx| {
                const new_elem = try self.duplicatePattern(elem_idx);
                try self.module_env.store.addScratchPattern(new_elem);
            }
            const new_elems = try self.module_env.store.patternSpanFrom(elems_start);

            // Duplicate the rest pattern if present
            const new_rest_info = if (list.rest_info) |rest| blk: {
                const new_rest_pattern = if (rest.pattern) |p| try self.duplicatePattern(p) else null;
                break :blk @as(?@TypeOf(list.rest_info.?), .{
                    .index = rest.index,
                    .pattern = new_rest_pattern,
                });
            } else null;

            return try self.module_env.store.addPattern(
                Pattern{ .list = .{
                    .patterns = new_elems,
                    .rest_info = new_rest_info,
                } },
                base.Region.zero(),
            );
        },
        .as => |as| {
            // Duplicate the inner pattern
            const new_inner = try self.duplicatePattern(as.pattern);
            return try self.module_env.store.addPattern(
                Pattern{ .as = .{
                    .pattern = new_inner,
                    .ident = as.ident,
                } },
                base.Region.zero(),
            );
        },
        .nominal => |nom| {
            // Duplicate the backing pattern
            const new_backing = try self.duplicatePattern(nom.backing_pattern);
            return try self.module_env.store.addPattern(
                Pattern{ .nominal = .{
                    .nominal_type_decl = nom.nominal_type_decl,
                    .backing_pattern = new_backing,
                    .backing_type = nom.backing_type,
                } },
                base.Region.zero(),
            );
        },
        .nominal_external => |nom| {
            // Duplicate the backing pattern
            const new_backing = try self.duplicatePattern(nom.backing_pattern);
            return try self.module_env.store.addPattern(
                Pattern{ .nominal_external = .{
                    .module_idx = nom.module_idx,
                    .target_node_idx = nom.target_node_idx,
                    .backing_pattern = new_backing,
                    .backing_type = nom.backing_type,
                } },
                base.Region.zero(),
            );
        },
        // Literal patterns - create a new one with the same value
        .str_literal,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {
            return try self.module_env.store.addPattern(pattern, base.Region.zero());
        },
    }
}

/// Duplicate a span of match branch patterns (BranchPattern.Span).
fn duplicateBranchPatternSpan(
    self: *Self,
    span: Expr.Match.BranchPattern.Span,
) std.mem.Allocator.Error!Expr.Match.BranchPattern.Span {
    const branch_patterns = self.module_env.store.sliceMatchBranchPatterns(span);
    if (branch_patterns.len == 0) {
        return span; // Empty span - return as-is
    }

    const patterns_start = self.module_env.store.scratchMatchBranchPatternTop();

    for (branch_patterns) |bp_idx| {
        const bp = self.module_env.store.getMatchBranchPattern(bp_idx);
        const new_pattern = try self.duplicatePattern(bp.pattern);
        const new_bp = Expr.Match.BranchPattern{
            .pattern = new_pattern,
            .degenerate = bp.degenerate,
        };
        const new_bp_idx = try self.module_env.store.addMatchBranchPattern(new_bp, base.Region.zero());
        try self.module_env.store.addScratchMatchBranchPattern(new_bp_idx);
    }

    return try self.module_env.store.matchBranchPatternSpanFrom(patterns_start);
}

/// Check if a type variable represents a polymorphic type
pub fn isPolymorphic(self: *Self, type_var: types.Var) bool {
    const resolved = self.types_store.resolveVar(type_var);
    return switch (resolved.desc.content) {
        .flex, .rigid => true,
        .structure, .alias, .err => false,
    };
}

/// Compute a structural hash for a type.
/// Two structurally equivalent types will have the same hash.
pub fn structuralTypeHash(self: *Self, type_var: types.Var) u64 {
    var hasher = std.hash.Wyhash.init(0);
    var seen = std.AutoHashMap(types.Var, void).init(self.allocator);
    defer seen.deinit();
    self.hashTypeRecursive(&hasher, type_var, &seen);
    return hasher.final();
}

fn hashTypeRecursive(
    self: *Self,
    hasher: *std.hash.Wyhash,
    type_var: types.Var,
    seen: *std.AutoHashMap(types.Var, void),
) void {
    // Check for cycles
    if (seen.contains(type_var)) {
        hasher.update("CYCLE");
        return;
    }
    seen.put(type_var, {}) catch return;

    const resolved = self.types_store.resolveVar(type_var);

    // Hash based on content
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            hasher.update(std.mem.asBytes(&@as(u8, @intFromEnum(flat_type))));

            switch (flat_type) {
                .nominal_type => |nom| {
                    hasher.update(std.mem.asBytes(&nom.ident.ident_idx));
                    // Hash type parameters
                    const vars = self.types_store.sliceVars(nom.vars.nonempty);
                    for (vars) |v| {
                        self.hashTypeRecursive(hasher, v, seen);
                    }
                },
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    // Hash function arguments and return type
                    const args = self.types_store.sliceVars(func.args);
                    for (args) |arg| {
                        self.hashTypeRecursive(hasher, arg, seen);
                    }
                    self.hashTypeRecursive(hasher, func.ret, seen);
                },
                .record => |record| {
                    // Hash record fields
                    const fields_slice = self.types_store.getRecordFieldsSlice(record.fields);
                    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
                        hasher.update(std.mem.asBytes(&name));
                        self.hashTypeRecursive(hasher, var_, seen);
                    }
                },
                .tag_union => |tag_union| {
                    // Hash tags
                    const tags_slice = self.types_store.getTagsSlice(tag_union.tags);
                    for (tags_slice.items(.name), tags_slice.items(.args)) |name, args| {
                        hasher.update(std.mem.asBytes(&name));
                        const tag_args = self.types_store.sliceVars(args);
                        for (tag_args) |arg| {
                            self.hashTypeRecursive(hasher, arg, seen);
                        }
                    }
                },
                .tuple => |tuple| {
                    const elems = self.types_store.sliceVars(tuple.elems);
                    for (elems) |elem| {
                        self.hashTypeRecursive(hasher, elem, seen);
                    }
                },
                .record_unbound => |fields_range| {
                    const fields_slice = self.types_store.getRecordFieldsSlice(fields_range);
                    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
                        hasher.update(std.mem.asBytes(&name));
                        self.hashTypeRecursive(hasher, var_, seen);
                    }
                },
                .empty_record => hasher.update("empty_record"),
                .empty_tag_union => hasher.update("empty_tag_union"),
            }
        },
        .flex => |flex| {
            hasher.update("flex");
            if (flex.name) |name| {
                hasher.update(std.mem.asBytes(&name));
            }
        },
        .rigid => |rigid| {
            hasher.update("rigid");
            hasher.update(std.mem.asBytes(&rigid.name));
        },
        .alias => |alias| {
            hasher.update("alias");
            hasher.update(std.mem.asBytes(&alias.ident.ident_idx));
        },
        .err => hasher.update("err"),
    }
}

/// Get a readable type name for specialization suffix
pub fn getTypeName(self: *Self, type_var: types.Var) []const u8 {
    const resolved = self.types_store.resolveVar(type_var);
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            switch (flat_type) {
                .nominal_type => |nom| {
                    return self.module_env.getIdent(nom.ident.ident_idx);
                },
                .fn_pure, .fn_effectful, .fn_unbound => return "Fn",
                .record, .record_unbound => return "Record",
                .tag_union => return "Tag",
                .tuple => return "Tuple",
                .empty_record => return "EmptyRecord",
                .empty_tag_union => return "EmptyTag",
            }
        },
        .flex, .rigid => return "a",
        .alias => |alias| return self.module_env.getIdent(alias.ident.ident_idx),
        .err => return "Err",
    }
}

/// Check if a type is a tag union type.
pub fn isTagUnion(self: *const Self, type_var: types.Var) bool {
    const mutable_self: *Self = @constCast(self);
    const resolved = mutable_self.types_store.resolveVar(type_var);
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            return switch (flat_type) {
                .tag_union, .empty_tag_union => true,
                else => false,
            };
        },
        else => return false,
    }
}

/// Get the tag names from a tag union type.
/// Returns null if the type is not a tag union.
pub fn getTagNames(self: *Self, type_var: types.Var) ?[]const base.Ident.Idx {
    const resolved = self.types_store.resolveVar(type_var);
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            switch (flat_type) {
                .tag_union => |tag_union| {
                    const tags_slice = self.types_store.getTagsSlice(tag_union.tags);
                    return tags_slice.items(.name);
                },
                .empty_tag_union => return &.{},
                else => return null,
            }
        },
        else => return null,
    }
}

/// Count the number of tags in a tag union type.
pub fn countTags(self: *Self, type_var: types.Var) usize {
    const resolved = self.types_store.resolveVar(type_var);
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            switch (flat_type) {
                .tag_union => |tag_union| {
                    return tag_union.tags.count;
                },
                .empty_tag_union => return 0,
                else => return 0,
            }
        },
        else => return 0,
    }
}

/// Create a specialized name for a function
pub fn createSpecializedName(
    self: *Self,
    original_name: base.Ident.Idx,
    type_var: types.Var,
) !base.Ident.Idx {
    const type_hash = self.structuralTypeHash(type_var);
    const key = SpecializationKey{
        .original_ident = original_name,
        .type_hash = type_hash,
    };

    // Check if we already have this specialization
    if (self.specialization_names.get(key)) |existing| {
        return existing;
    }

    // Create new specialized name: original_TypeName_N
    const original = self.module_env.getIdent(original_name);
    const type_name = self.getTypeName(type_var);
    self.specialization_counter += 1;

    const specialized = try std.fmt.allocPrint(
        self.allocator,
        "{s}_{s}_{d}",
        .{ original, type_name, self.specialization_counter },
    );
    defer self.allocator.free(specialized);

    const specialized_ident = try self.module_env.insertIdent(base.Ident.for_text(specialized));

    try self.specialization_names.put(key, specialized_ident);
    return specialized_ident;
}

/// Get the number of completed specializations
pub fn getSpecializationCount(self: *const Self) usize {
    return self.specialized.count();
}

/// Get the specialized name for a function at a concrete type, if it exists
pub fn getSpecializedName(
    self: *const Self,
    original_ident: base.Ident.Idx,
    type_var: types.Var,
) ?base.Ident.Idx {
    // Need mutable self for structuralTypeHash but we only read
    const mutable_self: *Self = @constCast(self);
    const type_hash = mutable_self.structuralTypeHash(type_var);
    const key = SpecializationKey{
        .original_ident = original_ident,
        .type_hash = type_hash,
    };
    return self.specialization_names.get(key);
}

/// Iterator for specialized procs
pub fn specializedIterator(self: *const Self) std.AutoHashMap(SpecializationKey, SpecializedProc).ValueIterator {
    return self.specialized.valueIterator();
}

const testing = std.testing;

test "monomorphizer: init and deinit" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    try testing.expectEqual(@as(u32, 0), mono.specialization_counter);
    try testing.expectEqual(Phase.finding, mono.phase);
}

test "monomorphizer: register partial proc" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Create a test identifier
    const test_ident = try module_env.insertIdent(base.Ident.for_text("identity"));

    // Create a dummy type variable
    const type_var = try module_env.types.fresh();

    // Register a partial proc
    // Note: body_expr is undefined since we're not testing body duplication here
    try mono.registerPartialProc(
        test_ident,
        undefined, // body_expr not needed for this test
        CIR.Pattern.Span{ .span = .{ .start = 0, .len = 0 } },
        type_var,
        true,
    );

    try testing.expect(mono.isPartialProc(test_ident));
}

test "monomorphizer: isPolymorphic" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Fresh flex var should be polymorphic
    const flex_var = try module_env.types.fresh();
    try testing.expect(mono.isPolymorphic(flex_var));
}

test "monomorphizer: specialization key equality" {
    const test_ident = base.Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = 1,
    };
    const key1 = SpecializationKey{
        .original_ident = test_ident,
        .type_hash = 12345,
    };
    const key2 = SpecializationKey{
        .original_ident = test_ident,
        .type_hash = 12345,
    };
    const key3 = SpecializationKey{
        .original_ident = test_ident,
        .type_hash = 67890,
    };

    try testing.expect(key1.eql(key2));
    try testing.expect(!key1.eql(key3));
}

test "monomorphizer: type hashing consistency" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Same type should produce same hash
    const type_var = try module_env.types.fresh();
    const hash1 = mono.structuralTypeHash(type_var);
    const hash2 = mono.structuralTypeHash(type_var);
    try testing.expectEqual(hash1, hash2);

    // Note: Fresh flex vars may hash the same since they have the same structure.
    // The important property is consistency - same var always produces same hash.
}

test "monomorphizer: recursion depth tracking" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Initially at depth 0
    try testing.expectEqual(@as(u32, 0), mono.recursion_depth);
    try testing.expectEqual(DEFAULT_MAX_RECURSION_DEPTH, mono.max_recursion_depth);

    // Stack should be empty
    try testing.expectEqual(@as(usize, 0), mono.specialization_stack.items.len);
}

test "monomorphizer: process empty pending" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Processing empty pending should work without error
    try mono.processPendingSpecializations();

    try testing.expectEqual(@as(usize, 0), mono.getSpecializationCount());
}

test "monomorphizer: static dispatch lookup returns null for flex vars" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Create a flex type var (polymorphic - not yet resolved)
    const flex_var = try module_env.types.fresh();

    // Create a method name
    const method_name = try module_env.insertIdent(base.Ident.for_text("hash"));

    // Looking up static dispatch for a flex var should return null
    // (can't dispatch until we know the concrete type)
    const result = mono.lookupStaticDispatch(flex_var, method_name);
    try testing.expect(result == null);
}

test "monomorphizer: getConcreteTypeName" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // For flex vars, should return null
    const flex_var = try module_env.types.fresh();
    const name = mono.getConcreteTypeName(flex_var);
    try testing.expect(name == null);
}

test "monomorphizer: set closure transformer integration" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Create a closure transformer
    var transformer = ClosureTransformer.init(allocator, module_env);
    defer transformer.deinit();

    // Initially closure_transformer is null
    try testing.expect(mono.closure_transformer == null);

    // Set the closure transformer
    mono.setClosureTransformer(&transformer, null);

    // Now closure_transformer should be set
    try testing.expect(mono.closure_transformer != null);
    try testing.expect(mono.closure_transformer.? == &transformer);

    // Processing empty pending with closure transformer should work
    try mono.processPendingSpecializations();
    try testing.expectEqual(@as(usize, 0), mono.getSpecializationCount());
}

test "monomorphizer: pattern duplication - assign pattern" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Create a simple assign pattern
    const ident = try module_env.insertIdent(base.Ident.for_text("x"));
    const pattern_idx = try module_env.store.addPattern(
        Pattern{ .assign = .{ .ident = ident } },
        base.Region.zero(),
    );

    // Duplicate the pattern
    const duplicated = try mono.duplicatePattern(pattern_idx);

    // Should have created a new pattern (different index)
    try testing.expect(duplicated != pattern_idx);

    // The duplicated pattern should have the same structure
    const orig_pattern = module_env.store.getPattern(pattern_idx);
    const dup_pattern = module_env.store.getPattern(duplicated);

    try testing.expect(orig_pattern == .assign);
    try testing.expect(dup_pattern == .assign);
    try testing.expectEqual(orig_pattern.assign.ident, dup_pattern.assign.ident);
}

test "monomorphizer: pattern duplication - underscore pattern" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Create an underscore pattern
    const pattern_idx = try module_env.store.addPattern(
        Pattern.underscore,
        base.Region.zero(),
    );

    // Duplicate the pattern
    const duplicated = try mono.duplicatePattern(pattern_idx);

    // Should have created a new pattern
    try testing.expect(duplicated != pattern_idx);

    // The duplicated pattern should be underscore
    const dup_pattern = module_env.store.getPattern(duplicated);
    try testing.expect(dup_pattern == .underscore);
}

test "monomorphizer: external specialization resolution stores and retrieves" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Create test identifiers
    const original_ident = try module_env.insertIdent(base.Ident.for_text("polymorphic_fn"));
    const specialized_ident = try module_env.insertIdent(base.Ident.for_text("polymorphic_fn_I64"));

    // Create a concrete type
    const concrete_type = try module_env.types.fresh();

    // Create a source module index (using an arbitrary test value)
    const source_module: CIR.Import.Idx = @enumFromInt(1);

    // First request should be new and return a generated specialized name
    const result1 = try mono.requestExternalSpecialization(source_module, original_ident, concrete_type, null);
    try testing.expect(result1.is_new);
    // The specialized_ident is now a generated name, not the original_ident
    try testing.expect(result1.specialized_ident.idx != original_ident.idx);

    // Store the generated name for comparison
    const generated_specialized_ident = result1.specialized_ident;

    // Second request (before resolution) should return the same generated name
    const result1b = try mono.requestExternalSpecialization(source_module, original_ident, concrete_type, null);
    try testing.expect(!result1b.is_new);
    try testing.expectEqual(generated_specialized_ident, result1b.specialized_ident);

    // Resolve the external specialization with the actual specialized name
    try mono.resolveExternalSpecialization(source_module, original_ident, specialized_ident, concrete_type);

    // After resolution, request should return the resolved specialized ident
    const result2 = try mono.requestExternalSpecialization(source_module, original_ident, concrete_type, null);
    try testing.expect(!result2.is_new);
    try testing.expectEqual(specialized_ident, result2.specialized_ident);
}

test "monomorphizer: ExternalSpecKey stores module info" {
    const key1 = ExternalSpecKey{
        .source_module = @enumFromInt(1),
        .original_ident = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = 42,
        },
        .type_hash = 12345,
    };

    const key2 = ExternalSpecKey{
        .source_module = @enumFromInt(2), // Different module
        .original_ident = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = 42,
        },
        .type_hash = 12345,
    };

    // Different modules should produce different keys
    const hash1 = std.hash.Wyhash.hash(0, std.mem.asBytes(&key1));
    const hash2 = std.hash.Wyhash.hash(0, std.mem.asBytes(&key2));
    try testing.expect(hash1 != hash2);
}

test "monomorphizer: allExternalSpecializationsResolved detects unresolved" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Initially all resolved (no requests)
    try testing.expect(mono.allExternalSpecializationsResolved());

    // Create test identifiers
    const original_ident = try module_env.insertIdent(base.Ident.for_text("external_fn"));
    const concrete_type = try module_env.types.fresh();
    const source_module: CIR.Import.Idx = @enumFromInt(1);

    // Request an external specialization (not resolved)
    _ = try mono.requestExternalSpecialization(source_module, original_ident, concrete_type, null);

    // Now should report unresolved
    try testing.expect(!mono.allExternalSpecializationsResolved());

    // Get unresolved requests
    var unresolved = try mono.getUnresolvedExternalRequests();
    defer unresolved.deinit(allocator);
    try testing.expectEqual(@as(usize, 1), unresolved.items.len);

    // Resolve it
    const specialized_ident = try module_env.insertIdent(base.Ident.for_text("external_fn_I64"));
    try mono.resolveExternalSpecialization(source_module, original_ident, specialized_ident, concrete_type);

    // Now all resolved
    try testing.expect(mono.allExternalSpecializationsResolved());
}
