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
        .phase = .finding,
        .recursion_depth = 0,
        .max_recursion_depth = DEFAULT_MAX_RECURSION_DEPTH,
        .specialization_stack = std.ArrayList(SpecializationKey).empty,
        .external_requests = std.ArrayList(ExternalSpecializationRequest).empty,
        .resolved_external_specs = std.AutoHashMap(ExternalSpecKey, base.Ident.Idx).init(allocator),
    };
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
