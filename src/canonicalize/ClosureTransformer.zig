//! Closure Transformer
//!
//! Transforms closures with captures into tagged values with explicit capture records.
//! This implements the Cor-style defunctionalization approach for lambda sets.
//!
//! ## Pipeline Overview
//!
//! The closure transformation works in conjunction with `LambdaLifter`:
//!
//! 1. **ClosureTransformer** (this module):
//!    - Transforms closures to tags with capture records (`#N_name({ ... })`)
//!    - Tracks lambda sets for variables that hold closures
//!    - Generates dispatch match expressions at call sites
//!
//! 2. **LambdaLifter** (see `LambdaLifter.zig`):
//!    - Extracts closure bodies to top-level function definitions
//!    - Transforms captured variable references to `captures.field_name` accesses
//!    - Dispatch calls these lifted functions with the captures record
//!
//! ## Transformation Example
//!
//! Input:
//! ```roc
//! x = 42
//! addX = |y| x + y
//! result = addX(10)
//! ```
//!
//! After ClosureTransformer + LambdaLifter (internal representation):
//! ```roc
//! c1_addX = |y, captures| captures.x + y
//!
//! x = 42
//! addX = #1_addX({ x: x })
//! result = match addX {
//!     #1_addX(captures) => c1_addX(10, captures)
//! }
//! ```
//!
//! When emitted by RocEmitter, `#` becomes `C`, so `#1_addX` prints as `C1_addX`.
//!
//! ## Implementation Notes
//!
//! - Closures become tags with capture records (using `#` prefix to avoid name clashes)
//! - The `#` prefix is reserved for comments in Roc source, so it can't collide with user tags
//! - Call sites to closures become match expressions that dispatch based on the lambda set
//! - Pure lambdas (no captures) in mixed contexts are wrapped as closure tags with empty records
//! - Top-level patterns are tracked to avoid unnecessary captures (they're always in scope)
//!
//! ## Closure Representation Strategies
//!
//! The transformer determines the optimal representation for a lambda set:
//!
//! - **TaggedUnion**: Multiple closures with different captures, stored as tagged union
//! - **CaptureStruct**: Single closure with captures, stored as struct
//! - **UnwrappedCapture**: Single closure with single capture, stored directly
//! - **EnumDispatch**: Multiple closures with NO captures, stored as enum tag only
//!
//! This follows the Cor-style defunctionalization approach where lambda sets are
//! resolved AFTER type checking.

const std = @import("std");
const base = @import("base");
const types = @import("types");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const Expr = CIR.Expr;
const Pattern = @import("Pattern.zig").Pattern;
const RecordField = CIR.RecordField;
const LambdaSetInference = @import("LambdaSetInference.zig");

const Self = @This();

/// Represents how a lambda set is stored at runtime.
/// This determines the dispatch strategy and memory layout.
pub const ClosureRepresentation = union(enum) {
    /// Multiple closures with different captures - stored as tagged union.
    /// Each closure variant has its own capture record type.
    /// Dispatch requires switching on the tag to determine which closure to call.
    tagged_union: TaggedUnionInfo,

    /// Single closure with captures - stored as struct containing captures.
    /// No dispatch needed since there's only one possible closure.
    capture_struct: CaptureStructInfo,

    /// Single closure with exactly one capture - stored directly (unwrapped).
    /// Avoids the overhead of a single-field struct.
    unwrapped_capture: UnwrappedCaptureInfo,

    /// Multiple closures with NO captures - stored as enum tag only.
    /// Very efficient: just a tag value, no capture data needed.
    enum_dispatch: EnumDispatchInfo,

    /// No closures in the lambda set (empty or pure functions only)
    empty,
};

/// Info for tagged union representation
pub const TaggedUnionInfo = struct {
    /// The closures in this union, each with their own capture types
    closures: []const ClosureInfo,
    /// The type variable for the union type (if known)
    union_type_var: ?types.Var,
};

/// Info for single closure with captures as struct
pub const CaptureStructInfo = struct {
    /// The single closure
    closure: ClosureInfo,
    /// The record field types for captures
    capture_field_types: []const types.Var,
};

/// Info for single closure with single unwrapped capture
pub const UnwrappedCaptureInfo = struct {
    /// The single closure
    closure: ClosureInfo,
    /// The type of the single capture
    capture_type: ?types.Var,
};

/// Info for enum dispatch (multiple closures, no captures)
pub const EnumDispatchInfo = struct {
    /// The closures (all should have empty captures)
    closures: []const ClosureInfo,
    /// Number of variants (for tag size calculation)
    variant_count: usize,
};

/// Information about a transformed closure
pub const ClosureInfo = struct {
    /// The tag name for this closure (e.g., `#1_addX` or globally unique `UserModule.#1_addX`)
    tag_name: base.Ident.Idx,
    /// Source module name (for cross-module dispatch identification)
    source_module: base.Ident.Idx,
    /// The lambda body expression
    lambda_body: Expr.Idx,
    /// The lambda arguments
    lambda_args: CIR.Pattern.Span,
    /// The capture names (for generating dispatch function patterns)
    capture_names: std.ArrayList(base.Ident.Idx),
    /// The pattern for the lifted function definition (for calling in dispatch)
    /// This is the pattern that binds the lifted function name (e.g., `closure_addX_1`)
    lifted_fn_pattern: ?CIR.Pattern.Idx,
    /// The captures pattern used in the lifted function (for passing to calls)
    lifted_captures_pattern: ?CIR.Pattern.Idx,
};

/// Unspecialized Closure - represents a static-dispatch-dependent closure.
///
/// These are closures that depend on static dispatch implementations which won't be
/// known until monomorphization. For example, when code uses `Thing.hash`, we don't
/// know which concrete `hash` implementation to dispatch to until we know the
/// concrete type that `Thing` resolves to.
///
/// These are resolved during monomorphization when concrete types are known.
pub const UnspecializedClosure = struct {
    /// The type variable this closure depends on.
    /// When this type variable becomes concrete, we can resolve which implementation to use.
    type_var: types.Var,

    /// The static dispatch method symbol (e.g., `hash`, `eq`, `map`)
    member: base.Ident.Idx,

    /// The expression that references the static dispatch method.
    /// Used to locate this in the IR for resolution.
    member_expr: Expr.Idx,

    /// Region number for ordering during resolution.
    /// When resolving nested static-dispatch-dependent closures, we process
    /// innermost first (higher region numbers first).
    region: u8,
};

/// Internal validation error for lambda set resolution failures.
/// These indicate compiler bugs, not user errors.
pub const ResolutionError = struct {
    /// The kind of resolution error
    kind: Kind,
    /// The expression where the error occurred
    expr: ?Expr.Idx,
    /// The method that couldn't be resolved
    method_name: ?base.Ident.Idx,
    /// Additional context (e.g., type name)
    context: ?base.Ident.Idx,

    pub const Kind = enum {
        /// No implementation found for the static dispatch method on this type
        missing_static_dispatch,
    };

    pub fn missingImpl(expr: Expr.Idx, method_name: base.Ident.Idx, type_name: ?base.Ident.Idx) ResolutionError {
        return .{
            .kind = .missing_static_dispatch,
            .expr = expr,
            .method_name = method_name,
            .context = type_name,
        };
    }
};

/// Result of validating lambda set resolution.
pub const ValidationResult = struct {
    /// All unspecialized closures are resolved
    is_valid: bool,
    /// Number of unresolved entries
    unresolved_count: usize,
    /// First error encountered (if any)
    first_error: ?ResolutionError,
};

/// Reference to an unspecialized entry in a lambda set.
/// Used by UnspecializedByTypeVar to track which entries depend on which type variables.
pub const UnspecializedEntryRef = struct {
    /// Pointer to the lambda set containing this entry
    lambda_set: *LambdaSet,
    /// Index into lambda_set.unspecialized
    index: usize,
};

/// Tracks unspecialized entries by the type variable they depend on.
///
/// This enables efficient lookup when a type variable becomes concrete during
/// monomorphization - we can quickly find all unspecialized entries that need
/// to be resolved for that type variable.
pub const UnspecializedByTypeVar = struct {
    /// Map: type_var -> list of (lambda_set_ptr, entry_index)
    entries: std.AutoHashMap(types.Var, std.ArrayList(UnspecializedEntryRef)),
    /// Allocator for internal structures
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) UnspecializedByTypeVar {
        return .{
            .entries = std.AutoHashMap(types.Var, std.ArrayList(UnspecializedEntryRef)).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *UnspecializedByTypeVar) void {
        var iter = self.entries.valueIterator();
        while (iter.next()) |list| {
            list.deinit(self.allocator);
        }
        self.entries.deinit();
    }

    /// Track that an unspecialized entry depends on a type variable.
    pub fn trackEntry(
        self: *UnspecializedByTypeVar,
        type_var: types.Var,
        ref: UnspecializedEntryRef,
    ) !void {
        const gop = try self.entries.getOrPut(type_var);
        if (!gop.found_existing) {
            gop.value_ptr.* = std.ArrayList(UnspecializedEntryRef).empty;
        }
        try gop.value_ptr.append(self.allocator, ref);
    }

    /// Get all unspecialized entries that depend on a type variable.
    /// Returns null if no entries depend on this type variable.
    pub fn getEntriesForVar(self: *const UnspecializedByTypeVar, type_var: types.Var) ?[]const UnspecializedEntryRef {
        return if (self.entries.get(type_var)) |list| list.items else null;
    }

    /// Remove tracking for a type variable (called after resolving all its entries).
    pub fn removeVar(self: *UnspecializedByTypeVar, type_var: types.Var) void {
        if (self.entries.fetchRemove(type_var)) |kv| {
            // The value is an ArrayList which owns its memory.
            // We need to make a mutable copy to deinit it.
            var list = kv.value;
            list.deinit(self.allocator);
        }
    }

    /// Check if a type variable has any tracked unspecialized entries.
    pub fn hasEntriesForVar(self: *const UnspecializedByTypeVar, type_var: types.Var) bool {
        return self.entries.contains(type_var);
    }

    /// Get the total count of tracked entries across all type variables.
    pub fn totalEntryCount(self: *const UnspecializedByTypeVar) usize {
        var count: usize = 0;
        var iter = self.entries.valueIterator();
        while (iter.next()) |list| {
            count += list.items.len;
        }
        return count;
    }
};

/// A lambda set - a collection of closures that could reach a given variable.
///
/// Lambda sets can contain both:
/// - Resolved closures: concrete closures with known implementations
/// - Unspecialized closures: static-dispatch-dependent closures resolved at mono time
pub const LambdaSet = struct {
    /// Resolved/concrete closures in this lambda set
    closures: std.ArrayList(ClosureInfo),

    /// Unspecialized closures (static-dispatch-dependent).
    /// These are resolved during monomorphization when concrete types are known.
    unspecialized: std.ArrayList(UnspecializedClosure),

    /// For recursive lambda sets (self-referential closures).
    /// Points to the closure that references itself.
    recursion_closure: ?*ClosureInfo,

    /// The ambient function type that directly encloses this lambda set.
    /// Used during monomorphization for ambient function unification:
    /// when resolving an unspecialized entry, we unify this function type
    /// with the ambient function of the resolved method's lambda set.
    /// This connects type variables across lambda set boundaries.
    ambient_function_var: ?types.Var,

    pub fn init() LambdaSet {
        return .{
            .closures = std.ArrayList(ClosureInfo).empty,
            .unspecialized = std.ArrayList(UnspecializedClosure).empty,
            .recursion_closure = null,
            .ambient_function_var = null,
        };
    }

    /// Note: This does NOT free capture_names since they are shared with the closures map.
    /// The capture_names are owned by the original closures and freed when the transformer is deinitialized.
    pub fn deinit(self: *LambdaSet, allocator: std.mem.Allocator) void {
        self.closures.deinit(allocator);
        self.unspecialized.deinit(allocator);
    }

    pub fn addClosure(self: *LambdaSet, allocator: std.mem.Allocator, info: ClosureInfo) !void {
        try self.closures.append(allocator, info);
    }

    /// Add an unspecialized (static-dispatch-dependent) closure to this lambda set.
    /// These will be resolved during monomorphization.
    pub fn addUnspecialized(self: *LambdaSet, allocator: std.mem.Allocator, unspec: UnspecializedClosure) !void {
        try self.unspecialized.append(allocator, unspec);
    }

    /// Check if this lambda set has any unresolved static-dispatch-dependent closures.
    pub fn hasUnspecialized(self: *const LambdaSet) bool {
        return self.unspecialized.items.len > 0;
    }

    /// Check if all closures are resolved (no unspecialized remaining).
    pub fn isFullyResolved(self: *const LambdaSet) bool {
        return self.unspecialized.items.len == 0;
    }

    pub fn merge(self: *LambdaSet, allocator: std.mem.Allocator, other: *const LambdaSet) !void {
        for (other.closures.items) |info| {
            try self.closures.append(allocator, info);
        }
        for (other.unspecialized.items) |unspec| {
            try self.unspecialized.append(allocator, unspec);
        }
        // Don't merge recursion_closure - that's specific to each lambda set
    }

    /// Create a deep copy of this LambdaSet with its own ArrayList
    pub fn clone(self: *const LambdaSet, allocator: std.mem.Allocator) !LambdaSet {
        var new_set = LambdaSet.init();
        for (self.closures.items) |info| {
            try new_set.closures.append(allocator, info);
        }
        for (self.unspecialized.items) |unspec| {
            try new_set.unspecialized.append(allocator, unspec);
        }
        new_set.recursion_closure = self.recursion_closure;
        new_set.ambient_function_var = self.ambient_function_var;
        return new_set;
    }

    /// Determine the optimal closure representation for this lambda set.
    /// This is used to generate efficient dispatch code.
    ///
    /// IMPORTANT: This should only be called after all unspecialized closures
    /// have been resolved during monomorphization.
    pub fn determineRepresentation(self: *const LambdaSet) ClosureRepresentation {
        // All static-dispatch-dependent closures must be resolved before we can
        // determine representation. This happens during monomorphization.
        std.debug.assert(self.unspecialized.items.len == 0);

        const closures = self.closures.items;

        // Empty lambda set
        if (closures.len == 0) {
            return .empty;
        }

        // Single closure case
        if (closures.len == 1) {
            const closure = closures[0];
            const capture_count = closure.capture_names.items.len;

            if (capture_count == 0) {
                // Single closure with no captures - still use empty representation
                // since it's effectively a function pointer
                return .empty;
            } else if (capture_count == 1) {
                // Single closure with single capture - can unwrap
                return .{
                    .unwrapped_capture = .{
                        .closure = closure,
                        .capture_type = null, // Type info filled in later if needed
                    },
                };
            } else {
                // Single closure with multiple captures - use struct
                return .{
                    .capture_struct = .{
                        .closure = closure,
                        .capture_field_types = &.{}, // Type info filled in later if needed
                    },
                };
            }
        }

        // Multiple closures - check if any have captures
        var any_has_captures = false;
        for (closures) |closure| {
            if (closure.capture_names.items.len > 0) {
                any_has_captures = true;
                break;
            }
        }

        if (!any_has_captures) {
            // Multiple closures, none have captures - use enum dispatch
            return .{ .enum_dispatch = .{
                .closures = closures,
                .variant_count = closures.len,
            } };
        }

        // Multiple closures with some having captures - use tagged union
        return .{
            .tagged_union = .{
                .closures = closures,
                .union_type_var = null, // Type info filled in later if needed
            },
        };
    }

    /// Check if this lambda set requires runtime dispatch.
    /// Returns false if there's at most one closure (no branching needed).
    pub fn requiresDispatch(self: *const LambdaSet) bool {
        return self.closures.items.len > 1;
    }

    /// Check if all closures in this set have no captures.
    /// If true, the lambda set can use enum dispatch (tag-only representation).
    pub fn allCapturesEmpty(self: *const LambdaSet) bool {
        for (self.closures.items) |closure| {
            if (closure.capture_names.items.len > 0) {
                return false;
            }
        }
        return true;
    }

    /// Count how many closures in this set are static-dispatch-resolved (have no captures
    /// and were likely resolved from static dispatch method references).
    ///
    /// Static dispatch implementations are typically top-level functions without captures,
    /// so they're very efficient to dispatch to (no closure environment needed).
    pub fn countStaticDispatchClosures(self: *const LambdaSet) usize {
        var count: usize = 0;
        for (self.closures.items) |closure| {
            // Static-dispatch-resolved closures typically have:
            // - No captures (empty capture_names)
            // - No lifted captures pattern
            // - May have a lifted fn pattern for dispatch
            if (closure.capture_names.items.len == 0 and
                closure.lifted_captures_pattern == null)
            {
                count += 1;
            }
        }
        return count;
    }

    /// Check if this lambda set is "pure" (all closures are resolved static dispatch
    /// or otherwise have no captures).
    ///
    /// Pure lambda sets can use the most efficient representation (enum dispatch)
    /// since no closure environments need to be stored or passed.
    pub fn isPureLambdaSet(self: *const LambdaSet) bool {
        return self.allCapturesEmpty();
    }

    /// Check if this lambda set has mixed captures (some closures have captures,
    /// some don't).
    ///
    /// Mixed lambda sets require tagged union representation where static-dispatch-resolved
    /// closures effectively have an empty capture record.
    pub fn hasMixedCaptures(self: *const LambdaSet) bool {
        var has_with_captures = false;
        var has_without_captures = false;

        for (self.closures.items) |closure| {
            if (closure.capture_names.items.len > 0) {
                has_with_captures = true;
            } else {
                has_without_captures = true;
            }

            if (has_with_captures and has_without_captures) {
                return true;
            }
        }
        return false;
    }

    /// Calculate the maximum number of captures across all closures.
    /// Useful for determining if unwrapped capture optimization can apply.
    pub fn maxCaptureCount(self: *const LambdaSet) usize {
        var max: usize = 0;
        for (self.closures.items) |closure| {
            if (closure.capture_names.items.len > max) {
                max = closure.capture_names.items.len;
            }
        }
        return max;
    }

    /// Determine if this lambda set should use a compact representation.
    ///
    /// Returns true if:
    /// - All closures have <= 1 capture (unwrapped capture possible)
    /// - Or all closures have no captures (enum dispatch possible)
    ///
    /// This is useful for optimization decisions.
    pub fn canUseCompactRepresentation(self: *const LambdaSet) bool {
        const max_caps = self.maxCaptureCount();
        return max_caps <= 1;
    }

    /// Flatten a lambda set that has multiple unspecialized entries for the same concrete type.
    ///
    /// When a lambda set has multiple unspecialized entries that all depend on the same
    /// type variable (which is now becoming concrete), we need to split it into multiple
    /// lambda sets, one for each entry.
    ///
    /// The algorithm (matching Rust specialize.rs:331-400):
    /// 1. Partition unspecialized entries into those matching the concrete type and those not
    /// 2. If only one entry matches, no flattening needed
    /// 3. If multiple match:
    ///    - First lambda set (this one): gets all solved closures + non-matching entries + first matching entry
    ///    - Additional lambda sets: each gets only one matching entry
    ///
    /// Parameters:
    /// - allocator: Allocator for new lambda sets
    /// - concrete_type_var: The type variable that has become concrete
    /// - types_store: Type store for checking type equivalence
    ///
    /// Returns: A list of additional lambda sets created by flattening (not including this one).
    ///          The original lambda set is modified in place.
    ///          Returns empty list if no flattening was needed.
    pub fn flattenForConcreteType(
        self: *LambdaSet,
        allocator: std.mem.Allocator,
        concrete_type_var: types.Var,
        types_store: *types.Store,
    ) !std.ArrayList(*LambdaSet) {
        var additional_sets = std.ArrayList(*LambdaSet).empty;

        // Partition unspecialized entries: those matching concrete_type_var vs those not
        var matching_indices = std.ArrayList(usize).empty;
        defer matching_indices.deinit(allocator);

        var non_matching_entries = std.ArrayList(UnspecializedClosure).empty;
        defer non_matching_entries.deinit(allocator);

        for (self.unspecialized.items, 0..) |entry, i| {
            // Check if this entry's type variable is equivalent to the concrete type
            const equiv_result = types_store.checkVarsEquiv(entry.type_var, concrete_type_var);
            switch (equiv_result) {
                .equiv => try matching_indices.append(allocator, i),
                .not_equiv => try non_matching_entries.append(allocator, entry),
            }
        }

        // If 0 or 1 entries match, no flattening needed
        if (matching_indices.items.len <= 1) {
            return additional_sets;
        }

        // Multiple entries match - need to flatten
        // First, collect all matching entries
        var matching_entries = try allocator.alloc(UnspecializedClosure, matching_indices.items.len);
        defer allocator.free(matching_entries);
        for (matching_indices.items, 0..) |idx, i| {
            matching_entries[i] = self.unspecialized.items[idx];
        }

        // Update the original lambda set:
        // - Keep all solved closures
        // - Add all non-matching entries
        // - Add the FIRST matching entry
        self.unspecialized.clearRetainingCapacity();
        for (non_matching_entries.items) |entry| {
            try self.unspecialized.append(allocator, entry);
        }
        if (matching_entries.len > 0) {
            try self.unspecialized.append(allocator, matching_entries[0]);
        }

        // Create additional lambda sets for remaining matching entries
        for (matching_entries[1..]) |entry| {
            const new_set = try allocator.create(LambdaSet);
            new_set.* = LambdaSet.init();
            // Copy solved closures and recursion info from original
            for (self.closures.items) |closure| {
                try new_set.closures.append(allocator, closure);
            }
            new_set.recursion_closure = self.recursion_closure;
            new_set.ambient_function_var = self.ambient_function_var;
            // Add only this one matching entry
            try new_set.unspecialized.append(allocator, entry);

            try additional_sets.append(allocator, new_set);
        }

        return additional_sets;
    }

    /// Count how many unspecialized entries depend on a specific type variable.
    /// Useful for determining if flattening is needed.
    pub fn countEntriesForTypeVar(
        self: *const LambdaSet,
        type_var: types.Var,
        types_store: *types.Store,
    ) usize {
        var count: usize = 0;
        for (self.unspecialized.items) |entry| {
            const equiv_result = types_store.checkVarsEquiv(entry.type_var, type_var);
            if (equiv_result == .equiv) {
                count += 1;
            }
        }
        return count;
    }
};

/// Information about a capture for layout optimization
pub const CaptureLayoutInfo = struct {
    pattern_idx: CIR.Pattern.Idx,
    name: base.Ident.Idx,
    lookup_expr: CIR.Expr.Idx,
};

/// Sort captures alphabetically by name for consistent ABI layout.
/// This ensures that the same captures always produce the same record layout,
/// regardless of the order they appear in the source code.
pub fn sortCapturesAlphabetically(
    captures: []CaptureLayoutInfo,
    idents: *const base.Ident.Store,
) void {
    std.sort.insertion(CaptureLayoutInfo, captures, idents, struct {
        pub fn lessThan(ident_store: *const base.Ident.Store, a: CaptureLayoutInfo, b: CaptureLayoutInfo) bool {
            const a_text = ident_store.getText(a.name);
            const b_text = ident_store.getText(b.name);
            return std.mem.order(u8, a_text, b_text) == .lt;
        }
    }.lessThan);
}

/// Remove duplicate captures (same pattern_idx).
/// Returns the deduplicated slice length.
pub fn deduplicateCaptures(captures: []CaptureLayoutInfo) usize {
    if (captures.len <= 1) return captures.len;

    var write_idx: usize = 1;
    for (1..captures.len) |read_idx| {
        var is_duplicate = false;
        for (0..write_idx) |check_idx| {
            if (captures[read_idx].pattern_idx == captures[check_idx].pattern_idx) {
                is_duplicate = true;
                break;
            }
        }
        if (!is_duplicate) {
            captures[write_idx] = captures[read_idx];
            write_idx += 1;
        }
    }
    return write_idx;
}

/// The allocator for intermediate allocations
allocator: std.mem.Allocator,

/// The module environment containing the CIR (mutable for adding new expressions)
module_env: *ModuleEnv,

/// Counter for generating unique closure names
closure_counter: u32,

/// Map from original closure expression to its transformation info
closures: std.AutoHashMap(Expr.Idx, ClosureInfo),

/// Map from pattern index to lambda set (for tracking which closures can reach a variable)
pattern_lambda_sets: std.AutoHashMap(CIR.Pattern.Idx, LambdaSet),

/// Map from lambda expression to its return lambda set (what closures it returns when called)
lambda_return_sets: std.AutoHashMap(Expr.Idx, LambdaSet),

/// Map from pattern to lambda return set (for looking up what a variable's lambda returns when called)
pattern_lambda_return_sets: std.AutoHashMap(CIR.Pattern.Idx, LambdaSet),

/// Set of top-level pattern indices (these don't need to be captured since they're always in scope)
top_level_patterns: std.AutoHashMap(CIR.Pattern.Idx, void),

/// Current region number for ordering unspecialized closures.
/// Incremented when entering nested lambda scopes.
/// Higher region = more deeply nested = should be resolved first.
current_region: u8,

/// Tracks unspecialized entries by the type variable they depend on.
/// This enables efficient lookup during monomorphization when a type variable
/// becomes concrete - we can quickly find all entries that need resolution.
unspec_by_type_var: UnspecializedByTypeVar,

/// Optional reference to cross-module lambda set inference results.
/// When provided, used for generating globally unique closure names.
inference: ?*LambdaSetInference,

/// Initialize the transformer
pub fn init(allocator: std.mem.Allocator, module_env: *ModuleEnv) Self {
    return initWithInference(allocator, module_env, null);
}

/// Initialize the transformer with optional cross-module inference results
pub fn initWithInference(allocator: std.mem.Allocator, module_env: *ModuleEnv, inference: ?*LambdaSetInference) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .closure_counter = 0,
        .closures = std.AutoHashMap(Expr.Idx, ClosureInfo).init(allocator),
        .pattern_lambda_sets = std.AutoHashMap(CIR.Pattern.Idx, LambdaSet).init(allocator),
        .lambda_return_sets = std.AutoHashMap(Expr.Idx, LambdaSet).init(allocator),
        .pattern_lambda_return_sets = std.AutoHashMap(CIR.Pattern.Idx, LambdaSet).init(allocator),
        .top_level_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(allocator),
        .current_region = 0,
        .unspec_by_type_var = UnspecializedByTypeVar.init(allocator),
        .inference = inference,
    };
}

/// Enter a new nested scope (e.g., lambda body), incrementing the region counter.
/// Returns the previous region value for restoration.
pub fn enterRegion(self: *Self) u8 {
    const prev = self.current_region;
    if (self.current_region < 255) {
        self.current_region += 1;
    }
    return prev;
}

/// Exit a nested scope, restoring the previous region value.
pub fn exitRegion(self: *Self, prev_region: u8) void {
    self.current_region = prev_region;
}

/// Info extracted from a static dispatch reference expression.
pub const StaticDispatchInfo = struct {
    /// The method name being called (e.g., "hash", "eq", "default")
    method_name: base.Ident.Idx,
    /// Reference to the type var alias statement (for type resolution)
    type_var_alias_stmt: CIR.Statement.Idx,
    /// Arguments to the method call
    args: CIR.Expr.Span,
};

/// Check if an expression is a static dispatch reference (e_type_var_dispatch).
/// Returns the static dispatch info if so, null otherwise.
pub fn isStaticDispatchRef(self: *const Self, expr_idx: Expr.Idx) ?StaticDispatchInfo {
    const expr = self.module_env.store.getExpr(expr_idx);
    return switch (expr) {
        .e_type_var_dispatch => |tvd| StaticDispatchInfo{
            .method_name = tvd.method_name,
            .type_var_alias_stmt = tvd.type_var_alias_stmt,
            .args = tvd.args,
        },
        else => null,
    };
}

/// Create an unspecialized closure entry for a static dispatch reference.
/// This records that we need to resolve this dispatch at monomorphization time.
pub fn createUnspecializedClosure(
    self: *const Self,
    member_info: StaticDispatchInfo,
    expr_idx: Expr.Idx,
) UnspecializedClosure {
    _ = member_info.args; // Will be used when generating dispatch

    // Extract the type variable from the type var alias statement
    const stmt = self.module_env.store.getStatement(member_info.type_var_alias_stmt);
    const type_var_anno = stmt.s_type_var_alias.type_var_anno;
    const type_var = ModuleEnv.varFrom(type_var_anno);

    return UnspecializedClosure{
        .type_var = type_var,
        .member = member_info.method_name,
        .member_expr = expr_idx,
        .region = self.current_region,
    };
}

/// Get the type variable from an expression, if it's a type var dispatch.
/// Returns null for other expression types.
pub fn getTypeVarFromExpr(self: *const Self, expr_idx: Expr.Idx) ?types.Var {
    const expr = self.module_env.store.getExpr(expr_idx);
    return switch (expr) {
        .e_type_var_dispatch => |tvd| blk: {
            // Get the type var from the type var alias statement
            const stmt = self.module_env.store.getStatement(tvd.type_var_alias_stmt);
            const type_var_anno = stmt.s_type_var_alias.type_var_anno;
            break :blk ModuleEnv.varFrom(type_var_anno);
        },
        else => null,
    };
}

/// Add an unspecialized entry to a lambda set and track it by type variable.
///
/// This is the main entry point for adding unspecialized closures during
/// closure transformation. It:
/// 1. Adds the entry to the lambda set's unspecialized list
/// 2. Tracks the entry in unspec_by_type_var for efficient lookup during monomorphization
///
/// The type variable is now stored in the UnspecializedClosure itself.
///
/// Returns error if allocation fails.
pub fn addUnspecializedWithTracking(
    self: *Self,
    lambda_set: *LambdaSet,
    unspec: UnspecializedClosure,
) !void {
    // Record the index before adding
    const index = lambda_set.unspecialized.items.len;

    // Add to the lambda set
    try lambda_set.unspecialized.append(self.allocator, unspec);

    // Track this entry by its type variable
    try self.unspec_by_type_var.trackEntry(unspec.type_var, .{
        .lambda_set = lambda_set,
        .index = index,
    });
}

/// Add an unspecialized entry with tracking, creating the entry from dispatch info.
/// Convenience method that combines createUnspecializedClosure and addUnspecializedWithTracking.
pub fn addUnspecializedFromDispatchInfo(
    self: *Self,
    lambda_set: *LambdaSet,
    dispatch_info: StaticDispatchInfo,
    expr_idx: Expr.Idx,
) !void {
    const unspec = self.createUnspecializedClosure(dispatch_info, expr_idx);
    try self.addUnspecializedWithTracking(lambda_set, unspec);
}

/// Mark a pattern as a top-level definition (doesn't need to be captured)
pub fn markTopLevel(self: *Self, pattern_idx: CIR.Pattern.Idx) !void {
    try self.top_level_patterns.put(pattern_idx, {});
}

/// Check if a pattern is a top-level definition
pub fn isTopLevel(self: *const Self, pattern_idx: CIR.Pattern.Idx) bool {
    return self.top_level_patterns.contains(pattern_idx);
}

/// Validate that all lambda sets have been fully resolved.
///
/// This should be called after monomorphization to ensure no unspecialized
/// closures remain. Any remaining unspecialized entries indicate a failure
/// to resolve static dispatch implementations.
///
/// Returns a ValidationResult indicating whether validation passed and
/// providing error details if it failed.
pub fn validateAllResolved(self: *const Self) ValidationResult {
    var total_unresolved: usize = 0;
    var first_error: ?ResolutionError = null;

    // Check pattern lambda sets
    var pattern_iter = self.pattern_lambda_sets.valueIterator();
    while (pattern_iter.next()) |lambda_set| {
        if (lambda_set.unspecialized.items.len > 0) {
            total_unresolved += lambda_set.unspecialized.items.len;
            if (first_error == null) {
                const unspec = lambda_set.unspecialized.items[0];
                first_error = ResolutionError.missingImpl(
                    unspec.member_expr,
                    unspec.member,
                    null,
                );
            }
        }
    }

    // Check lambda return sets
    var return_iter = self.lambda_return_sets.valueIterator();
    while (return_iter.next()) |lambda_set| {
        if (lambda_set.unspecialized.items.len > 0) {
            total_unresolved += lambda_set.unspecialized.items.len;
            if (first_error == null) {
                const unspec = lambda_set.unspecialized.items[0];
                first_error = ResolutionError.missingImpl(
                    unspec.member_expr,
                    unspec.member,
                    null,
                );
            }
        }
    }

    // Check pattern lambda return sets
    var pattern_return_iter = self.pattern_lambda_return_sets.valueIterator();
    while (pattern_return_iter.next()) |lambda_set| {
        if (lambda_set.unspecialized.items.len > 0) {
            total_unresolved += lambda_set.unspecialized.items.len;
            if (first_error == null) {
                const unspec = lambda_set.unspecialized.items[0];
                first_error = ResolutionError.missingImpl(
                    unspec.member_expr,
                    unspec.member,
                    null,
                );
            }
        }
    }

    return .{
        .is_valid = total_unresolved == 0,
        .unresolved_count = total_unresolved,
        .first_error = first_error,
    };
}

/// Detect if a closure body contains a reference to the closure itself.
///
/// This is used to identify recursive closures, which require special handling
/// during dispatch generation (the recursion must be tracked to prevent
/// infinite lambda set expansion).
///
/// Returns true if the body contains a self-reference.
pub fn detectRecursion(
    self: *const Self,
    closure_pattern: CIR.Pattern.Idx,
    body_expr: Expr.Idx,
) bool {
    return self.exprContainsPatternRef(body_expr, closure_pattern);
}

/// Check if an expression contains a reference to the given pattern.
/// Used for recursive closure detection.
fn exprContainsPatternRef(
    self: *const Self,
    expr_idx: Expr.Idx,
    target_pattern: CIR.Pattern.Idx,
) bool {
    const expr = self.module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_lookup_local => |lookup| {
            return lookup.pattern_idx == target_pattern;
        },
        .e_call => |call| {
            // Check function expression
            if (self.exprContainsPatternRef(call.func, target_pattern)) {
                return true;
            }
            // Check arguments
            const args = self.module_env.store.sliceExpr(call.args);
            for (args) |arg_idx| {
                if (self.exprContainsPatternRef(arg_idx, target_pattern)) {
                    return true;
                }
            }
            return false;
        },
        .e_lambda => |lambda| {
            return self.exprContainsPatternRef(lambda.body, target_pattern);
        },
        .e_closure => |closure| {
            const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
            if (lambda_expr == .e_lambda) {
                return self.exprContainsPatternRef(lambda_expr.e_lambda.body, target_pattern);
            }
            return false;
        },
        .e_block => |block| {
            // Check statements
            const stmts = self.module_env.store.sliceStatements(block.stmts);
            for (stmts) |stmt_idx| {
                const stmt = self.module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        if (self.exprContainsPatternRef(decl.expr, target_pattern)) {
                            return true;
                        }
                    },
                    else => {},
                }
            }
            // Check final expression
            return self.exprContainsPatternRef(block.final_expr, target_pattern);
        },
        .e_if => |if_expr| {
            // Check all branches
            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                if (self.exprContainsPatternRef(branch.cond, target_pattern) or
                    self.exprContainsPatternRef(branch.body, target_pattern))
                {
                    return true;
                }
            }
            return self.exprContainsPatternRef(if_expr.final_else, target_pattern);
        },
        .e_binop => |binop| {
            return self.exprContainsPatternRef(binop.lhs, target_pattern) or
                self.exprContainsPatternRef(binop.rhs, target_pattern);
        },
        .e_list => |list| {
            const elems = self.module_env.store.sliceExpr(list.elems);
            for (elems) |elem_idx| {
                if (self.exprContainsPatternRef(elem_idx, target_pattern)) {
                    return true;
                }
            }
            return false;
        },
        .e_tuple => |tuple| {
            const elems = self.module_env.store.sliceExpr(tuple.elems);
            for (elems) |elem_idx| {
                if (self.exprContainsPatternRef(elem_idx, target_pattern)) {
                    return true;
                }
            }
            return false;
        },
        .e_record => |record| {
            const fields = self.module_env.store.sliceRecordFields(record.fields);
            for (fields) |field_idx| {
                const field = self.module_env.store.getRecordField(field_idx);
                if (self.exprContainsPatternRef(field.value, target_pattern)) {
                    return true;
                }
            }
            if (record.ext) |ext| {
                return self.exprContainsPatternRef(ext, target_pattern);
            }
            return false;
        },
        .e_tag => |tag| {
            const args = self.module_env.store.sliceExpr(tag.args);
            for (args) |arg_idx| {
                if (self.exprContainsPatternRef(arg_idx, target_pattern)) {
                    return true;
                }
            }
            return false;
        },
        .e_unary_minus => |unary| {
            return self.exprContainsPatternRef(unary.expr, target_pattern);
        },
        .e_unary_not => |unary| {
            return self.exprContainsPatternRef(unary.expr, target_pattern);
        },
        .e_dot_access => |dot| {
            if (self.exprContainsPatternRef(dot.receiver, target_pattern)) {
                return true;
            }
            if (dot.args) |args_span| {
                const args = self.module_env.store.sliceExpr(args_span);
                for (args) |arg_idx| {
                    if (self.exprContainsPatternRef(arg_idx, target_pattern)) {
                        return true;
                    }
                }
            }
            return false;
        },
        .e_tuple_access => |tuple_access| {
            return self.exprContainsPatternRef(tuple_access.tuple, target_pattern);
        },
        .e_match => |match| {
            if (self.exprContainsPatternRef(match.cond, target_pattern)) {
                return true;
            }
            const branches = self.module_env.store.sliceMatchBranches(match.branches);
            for (branches) |branch_idx| {
                const branch = self.module_env.store.getMatchBranch(branch_idx);
                if (self.exprContainsPatternRef(branch.value, target_pattern)) {
                    return true;
                }
                if (branch.guard) |guard| {
                    if (self.exprContainsPatternRef(guard, target_pattern)) {
                        return true;
                    }
                }
            }
            return false;
        },
        .e_return => |ret| {
            return self.exprContainsPatternRef(ret.expr, target_pattern);
        },
        .e_dbg => |dbg| {
            return self.exprContainsPatternRef(dbg.expr, target_pattern);
        },
        .e_expect => |expect| {
            return self.exprContainsPatternRef(expect.body, target_pattern);
        },
        .e_nominal => |nominal| {
            return self.exprContainsPatternRef(nominal.backing_expr, target_pattern);
        },
        .e_nominal_external => |nominal| {
            return self.exprContainsPatternRef(nominal.backing_expr, target_pattern);
        },
        .e_for => |for_expr| {
            return self.exprContainsPatternRef(for_expr.expr, target_pattern) or
                self.exprContainsPatternRef(for_expr.body, target_pattern);
        },
        .e_type_var_dispatch => |tvd| {
            const args = self.module_env.store.sliceExpr(tvd.args);
            for (args) |arg_idx| {
                if (self.exprContainsPatternRef(arg_idx, target_pattern)) {
                    return true;
                }
            }
            return false;
        },
        // Leaf expressions that can't contain references
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_typed_int,
        .e_typed_frac,
        .e_str_segment,
        .e_str,
        .e_lookup_external,
        .e_lookup_pending,
        .e_empty_list,
        .e_empty_record,
        .e_zero_argument_tag,
        .e_runtime_error,
        .e_ellipsis,
        .e_anno_only,
        .e_lookup_required,
        .e_hosted_lambda,
        .e_low_level_lambda,
        .e_crash,
        => return false,
    }
}

/// Mark a lambda set as containing a recursive closure.
///
/// When a closure references itself (directly or indirectly), its lambda set
/// needs special handling during code generation.
pub fn markLambdaSetRecursive(
    lambda_set: *LambdaSet,
    closure_info: *ClosureInfo,
) void {
    lambda_set.recursion_closure = closure_info;
}

/// Check if a lambda set is recursive.
pub fn isLambdaSetRecursive(lambda_set: *const LambdaSet) bool {
    return lambda_set.recursion_closure != null;
}

/// Detect recursion in a closure and mark the lambda set if recursive.
///
/// This combines `detectRecursion` and `markLambdaSetRecursive` into a single
/// operation that:
/// 1. Checks if the closure body contains a self-reference
/// 2. If so, marks the lambda set as recursive
///
/// This should be called after a closure is added to a lambda set.
///
/// Returns true if the closure was found to be recursive.
pub fn detectAndMarkRecursion(
    self: *const Self,
    lambda_set: *LambdaSet,
    closure_info: *ClosureInfo,
    closure_pattern: CIR.Pattern.Idx,
) bool {
    if (self.detectRecursion(closure_pattern, closure_info.lambda_body)) {
        markLambdaSetRecursive(lambda_set, closure_info);
        return true;
    }
    return false;
}

/// Check all closures in a lambda set for recursion.
///
/// This iterates through all closures in the lambda set and marks the set
/// as recursive if any closure references itself.
///
/// Note: This modifies `closures.items` to get mutable pointers to ClosureInfo.
pub fn detectRecursionInLambdaSet(
    self: *const Self,
    lambda_set: *LambdaSet,
) bool {
    for (lambda_set.closures.items, 0..) |*closure_info, i| {
        // For each closure, check if it references itself
        // We need the pattern that binds this closure, which is stored in lifted_fn_pattern
        if (closure_info.lifted_fn_pattern) |fn_pattern| {
            if (self.detectRecursion(fn_pattern, closure_info.lambda_body)) {
                markLambdaSetRecursive(lambda_set, &lambda_set.closures.items[i]);
                return true;
            }
        }
    }
    return false;
}

/// Check if a lambda set is empty (no closures can reach the variable).
pub fn isLambdaSetEmpty(lambda_set: *const LambdaSet) bool {
    return lambda_set.closures.items.len == 0 and
        lambda_set.unspecialized.items.len == 0;
}

/// Free resources
pub fn deinit(self: *Self) void {
    // Free capture name lists
    var closure_iter = self.closures.valueIterator();
    while (closure_iter.next()) |info| {
        info.capture_names.deinit(self.allocator);
    }
    self.closures.deinit();

    // Free lambda sets (they own their ClosureInfo copies)
    // Note: LambdaSet.deinit frees both closures and unspecialized arrays,
    // but does NOT free individual capture_names since they share data with closures map
    var lambda_set_iter = self.pattern_lambda_sets.valueIterator();
    while (lambda_set_iter.next()) |lambda_set| {
        lambda_set.deinit(self.allocator);
    }
    self.pattern_lambda_sets.deinit();

    // Free lambda return sets
    var return_set_iter = self.lambda_return_sets.valueIterator();
    while (return_set_iter.next()) |lambda_set| {
        lambda_set.deinit(self.allocator);
    }
    self.lambda_return_sets.deinit();

    // Free pattern lambda return sets
    var pattern_return_iter = self.pattern_lambda_return_sets.valueIterator();
    while (pattern_return_iter.next()) |lambda_set| {
        lambda_set.deinit(self.allocator);
    }
    self.pattern_lambda_return_sets.deinit();

    // Free top-level patterns set
    self.top_level_patterns.deinit();

    // Free unspecialized entry tracking
    self.unspec_by_type_var.deinit();
}

/// Generate a unique tag name for a closure.
///
/// When cross-module inference is available, generates globally unique names
/// like "UserModule.#1_addX" that are valid across module boundaries.
///
/// Without inference, generates local names like "#1_addX", "#2_addX" when a hint
/// is provided, or "#1", "#2" when no hint is available. The `#` prefix is used
/// because it's reserved for comments in Roc source code, so these names cannot
/// collide with user-defined tags. RocEmitter transforms `#` to `C` when
/// printing, so `#1_foo` becomes `C1_foo` in emitted code.
pub fn generateClosureTagName(self: *Self, hint: ?base.Ident.Idx) !base.Ident.Idx {
    self.closure_counter += 1;

    // Build the local part of the name
    var local_name: []const u8 = undefined;
    var local_name_allocated: bool = false;
    defer if (local_name_allocated) self.allocator.free(local_name);

    if (hint) |h| {
        const hint_name = self.module_env.getIdent(h);
        local_name = try std.fmt.allocPrint(
            self.allocator,
            "#{d}_{s}",
            .{ self.closure_counter, hint_name },
        );
        local_name_allocated = true;
    } else {
        local_name = try std.fmt.allocPrint(
            self.allocator,
            "#{d}",
            .{self.closure_counter},
        );
        local_name_allocated = true;
    }

    // If we have cross-module inference, generate a globally unique name
    if (self.inference != null) {
        const module_name = self.module_env.module_name;
        const global_name = try std.fmt.allocPrint(
            self.allocator,
            "{s}.{s}",
            .{ module_name, local_name },
        );
        defer self.allocator.free(global_name);
        return try self.module_env.insertIdent(base.Ident.for_text(global_name));
    }

    // Without inference, use local name
    return try self.module_env.insertIdent(base.Ident.for_text(local_name));
}

/// Generate the lowercase function name from a closure tag name.
/// E.g., "#1_foo" -> "c1_foo" (replaces # with lowercase c)
fn generateLiftedFunctionName(self: *Self, tag_name: base.Ident.Idx) !base.Ident.Idx {
    const tag_str = self.module_env.getIdent(tag_name);

    // Allocate a copy with # replaced by lowercase 'c'
    var fn_name = try self.allocator.alloc(u8, tag_str.len);
    defer self.allocator.free(fn_name);
    @memcpy(fn_name, tag_str);

    // Replace leading # with lowercase 'c' for function name
    if (fn_name.len > 0 and fn_name[0] == '#') {
        fn_name[0] = 'c';
    }

    return try self.module_env.insertIdent(base.Ident.for_text(fn_name));
}

/// Create patterns for calling a lifted function in dispatch.
/// Returns the pattern for the lifted function name and the captures pattern.
/// The actual lifted function body is created by LambdaLifter.
fn createLiftedFunctionPatterns(
    self: *Self,
    tag_name: base.Ident.Idx,
    has_captures: bool,
) !struct { fn_pattern: CIR.Pattern.Idx, captures_pattern: ?CIR.Pattern.Idx } {
    // Generate the lowercase function name
    const fn_name = try self.generateLiftedFunctionName(tag_name);

    // Create the pattern for the function (used in dispatch lookups)
    const fn_pattern = try self.module_env.store.addPattern(
        Pattern{ .assign = .{ .ident = fn_name } },
        base.Region.zero(),
    );

    // Build the captures parameter pattern if there are captures
    var captures_pattern: ?CIR.Pattern.Idx = null;
    if (has_captures) {
        // Create a "captures" identifier pattern (used in dispatch match branches)
        const captures_ident = try self.module_env.insertIdent(base.Ident.for_text("captures"));
        captures_pattern = try self.module_env.store.addPattern(
            Pattern{ .assign = .{ .ident = captures_ident } },
            base.Region.zero(),
        );
    }

    return .{ .fn_pattern = fn_pattern, .captures_pattern = captures_pattern };
}

/// Generate a dispatch match expression for a closure call.
///
/// Phase 5: Generates calls to lifted functions instead of inlining bodies.
/// Transforms a call like `f(10)` where `f` is a closure into:
/// ```roc
/// match f {
///     #1_f(captures) => c1_f(10, captures)
/// }
/// ```
fn generateDispatchMatch(
    self: *Self,
    closure_var_expr: Expr.Idx,
    closure_info: ClosureInfo,
    call_args: []const Expr.Idx,
) !Expr.Idx {
    // Step 1: Create the captures pattern for matching
    // Use the lifted_captures_pattern if there are captures, otherwise use empty record
    const captures_pattern = if (closure_info.lifted_captures_pattern) |cap_pat|
        cap_pat
    else blk: {
        // No captures - create an empty record destructure pattern
        const destructs_span = try self.module_env.store.recordDestructSpanFrom(
            self.module_env.store.scratchRecordDestructTop(),
        );
        break :blk try self.module_env.store.addPattern(
            Pattern{ .record_destructure = .{ .destructs = destructs_span } },
            base.Region.zero(),
        );
    };

    // Step 2: Create the applied_tag pattern: `#1_f(captures)`
    const pattern_args_start = self.module_env.store.scratchPatternTop();
    try self.module_env.store.addScratchPattern(captures_pattern);
    const pattern_args_span = try self.module_env.store.patternSpanFrom(pattern_args_start);

    const tag_pattern = try self.module_env.store.addPattern(
        Pattern{ .applied_tag = .{
            .name = closure_info.tag_name,
            .args = pattern_args_span,
        } },
        base.Region.zero(),
    );

    // Step 3: Create the body - a call to the lifted function
    // Generate: closure_f_1(arg1, arg2, ..., captures)
    const body_expr = if (closure_info.lifted_fn_pattern) |fn_pattern| blk: {
        // Create a lookup to the lifted function
        const fn_lookup = try self.module_env.store.addExpr(Expr{
            .e_lookup_local = .{ .pattern_idx = fn_pattern },
        }, base.Region.zero());

        // Build the argument list: (original call args..., captures)
        const args_start = self.module_env.store.scratch.?.exprs.top();

        // Add original call arguments
        for (call_args) |arg| {
            try self.module_env.store.scratch.?.exprs.append(arg);
        }

        // Add captures lookup if there are captures
        if (closure_info.lifted_captures_pattern) |cap_pat| {
            const captures_lookup = try self.module_env.store.addExpr(Expr{
                .e_lookup_local = .{ .pattern_idx = cap_pat },
            }, base.Region.zero());
            try self.module_env.store.scratch.?.exprs.append(captures_lookup);
        }

        const args_span = try self.module_env.store.exprSpanFrom(args_start);

        // Create the call expression
        break :blk try self.module_env.store.addExpr(Expr{
            .e_call = .{
                .func = fn_lookup,
                .args = args_span,
                .called_via = .apply,
            },
        }, base.Region.zero());
    } else blk: {
        // Fallback: no lifted function pattern (shouldn't happen)
        // Transform the lambda body to handle nested closures (old behavior)
        break :blk try self.transformExpr(closure_info.lambda_body);
    };

    // Step 4: Create the match branch
    const branch_pattern_start = self.module_env.store.scratchMatchBranchPatternTop();
    const branch_pattern = try self.module_env.store.addMatchBranchPattern(
        Expr.Match.BranchPattern{
            .pattern = tag_pattern,
            .degenerate = false,
        },
        base.Region.zero(),
    );
    try self.module_env.store.addScratchMatchBranchPattern(branch_pattern);
    const branch_patterns_span = try self.module_env.store.matchBranchPatternSpanFrom(branch_pattern_start);

    // Create a fresh type variable for the redundant field
    const redundant_var = try self.module_env.types.fresh();

    const branch = Expr.Match.Branch{
        .patterns = branch_patterns_span,
        .value = body_expr,
        .guard = null,
        .redundant = redundant_var,
    };
    const branch_idx = try self.module_env.store.addMatchBranch(branch, base.Region.zero());

    // Step 5: Create the match expression
    const branch_start = self.module_env.store.scratchMatchBranchTop();
    try self.module_env.store.addScratchMatchBranch(branch_idx);
    const branches_span = try self.module_env.store.matchBranchSpanFrom(branch_start);

    // Create a fresh type variable for exhaustiveness
    const exhaustive_var = try self.module_env.types.fresh();

    return try self.module_env.store.addExpr(Expr{
        .e_match = .{
            .cond = closure_var_expr,
            .branches = branches_span,
            .exhaustive = exhaustive_var,
            .is_try_suffix = false,
        },
    }, base.Region.zero());
}

/// Generate a dispatch match expression for a call to a variable with multiple possible closures.
///
/// Phase 5: Generates calls to lifted functions instead of inlining bodies.
/// Transforms a call like `f(10)` where `f` could be one of several closures into:
/// ```roc
/// match f {
///     #1_add1(captures) => c1_add1(10, captures),
///     #2_mul2({}) => c2_mul2(10),
/// }
/// ```
fn generateLambdaSetDispatchMatch(
    self: *Self,
    closure_var_expr: Expr.Idx,
    lambda_set: *const LambdaSet,
    call_args: []const Expr.Idx,
) !Expr.Idx {
    // All unspecialized closures must be resolved before generating dispatch.
    // Unspecialized closures (static-dispatch-dependent entries) are resolved during
    // monomorphization when concrete types are known.
    std.debug.assert(lambda_set.isFullyResolved());

    // If there's only one closure, use the simpler single-closure dispatch
    if (lambda_set.closures.items.len == 1) {
        return try self.generateDispatchMatch(closure_var_expr, lambda_set.closures.items[0], call_args);
    }

    // Start collecting match branches
    const branch_start = self.module_env.store.scratchMatchBranchTop();

    // Generate a branch for each closure in the lambda set
    for (lambda_set.closures.items) |closure_info| {
        // Step 1: Create the captures pattern for matching
        const captures_pattern = if (closure_info.lifted_captures_pattern) |cap_pat|
            cap_pat
        else blk: {
            // No captures - create an empty record destructure pattern
            const destructs_span = try self.module_env.store.recordDestructSpanFrom(
                self.module_env.store.scratchRecordDestructTop(),
            );
            break :blk try self.module_env.store.addPattern(
                Pattern{ .record_destructure = .{ .destructs = destructs_span } },
                base.Region.zero(),
            );
        };

        // Step 2: Create applied_tag pattern
        const pattern_args_start = self.module_env.store.scratchPatternTop();
        try self.module_env.store.addScratchPattern(captures_pattern);
        const pattern_args_span = try self.module_env.store.patternSpanFrom(pattern_args_start);

        const tag_pattern = try self.module_env.store.addPattern(
            Pattern{ .applied_tag = .{
                .name = closure_info.tag_name,
                .args = pattern_args_span,
            } },
            base.Region.zero(),
        );

        // Step 3: Create the body - call to lifted function
        const body_expr = if (closure_info.lifted_fn_pattern) |fn_pattern| blk: {
            // Create a lookup to the lifted function
            const fn_lookup = try self.module_env.store.addExpr(Expr{
                .e_lookup_local = .{ .pattern_idx = fn_pattern },
            }, base.Region.zero());

            // Build the argument list: (original call args..., captures)
            const args_start = self.module_env.store.scratch.?.exprs.top();

            // Add original call arguments
            for (call_args) |arg| {
                try self.module_env.store.scratch.?.exprs.append(arg);
            }

            // Add captures lookup if there are captures
            if (closure_info.lifted_captures_pattern) |cap_pat| {
                const captures_lookup = try self.module_env.store.addExpr(Expr{
                    .e_lookup_local = .{ .pattern_idx = cap_pat },
                }, base.Region.zero());
                try self.module_env.store.scratch.?.exprs.append(captures_lookup);
            }

            const args_span = try self.module_env.store.exprSpanFrom(args_start);

            // Create the call expression
            break :blk try self.module_env.store.addExpr(Expr{
                .e_call = .{
                    .func = fn_lookup,
                    .args = args_span,
                    .called_via = .apply,
                },
            }, base.Region.zero());
        } else blk: {
            // Fallback: no lifted function pattern (shouldn't happen)
            break :blk try self.transformExpr(closure_info.lambda_body);
        };

        // Step 4: Create match branch pattern
        const branch_pattern_start = self.module_env.store.scratchMatchBranchPatternTop();
        const branch_pattern = try self.module_env.store.addMatchBranchPattern(
            Expr.Match.BranchPattern{
                .pattern = tag_pattern,
                .degenerate = false,
            },
            base.Region.zero(),
        );
        try self.module_env.store.addScratchMatchBranchPattern(branch_pattern);
        const branch_patterns_span = try self.module_env.store.matchBranchPatternSpanFrom(branch_pattern_start);

        const redundant_var = try self.module_env.types.fresh();

        const branch = Expr.Match.Branch{
            .patterns = branch_patterns_span,
            .value = body_expr,
            .guard = null,
            .redundant = redundant_var,
        };
        const branch_idx = try self.module_env.store.addMatchBranch(branch, base.Region.zero());
        try self.module_env.store.addScratchMatchBranch(branch_idx);
    }

    const branches_span = try self.module_env.store.matchBranchSpanFrom(branch_start);

    const exhaustive_var = try self.module_env.types.fresh();

    return try self.module_env.store.addExpr(Expr{
        .e_match = .{
            .cond = closure_var_expr,
            .branches = branches_span,
            .exhaustive = exhaustive_var,
            .is_try_suffix = false,
        },
    }, base.Region.zero());
}

/// Result of transforming an expression that may contain closures
pub const TransformResult = struct {
    expr: Expr.Idx,
    lambda_set: ?LambdaSet,
};

/// Transform an expression and collect any closures into a lambda set.
/// This handles direct closures, if expressions with closures, etc.
pub fn transformExprWithLambdaSet(
    self: *Self,
    expr_idx: Expr.Idx,
    name_hint: ?base.Ident.Idx,
) std.mem.Allocator.Error!TransformResult {
    const expr = self.module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_closure => {
            // Closure with captures - transform to tag and create lambda set
            const transformed = try self.transformClosure(expr_idx, name_hint);
            if (self.closures.get(expr_idx)) |closure_info| {
                var lambda_set = LambdaSet.init();
                try lambda_set.addClosure(self.allocator, closure_info);
                return .{ .expr = transformed, .lambda_set = lambda_set };
            }
            // If no closure_info, it was converted to a pure lambda (all captures were top-level)
            return .{ .expr = transformed, .lambda_set = null };
        },
        .e_lambda => |lambda| {
            // Pure lambda (no captures) - transform body and track what it returns
            // Use transformExprWithLambdaSet to collect the body's lambda set
            const body_result = try self.transformExprWithLambdaSet(lambda.body, name_hint);

            // If body is unchanged, return original lambda
            if (body_result.expr == lambda.body) {
                return .{ .expr = expr_idx, .lambda_set = null };
            }

            // Create new lambda with transformed body
            const new_lambda = try self.module_env.store.addExpr(Expr{
                .e_lambda = .{
                    .args = lambda.args,
                    .body = body_result.expr,
                },
            }, base.Region.zero());

            // If the body returns closures, track this so calls to this lambda return those closures
            if (body_result.lambda_set) |body_lambda_set| {
                // Clone the lambda_set for storage since we're consuming the original
                const cloned = try body_lambda_set.clone(self.allocator);
                try self.lambda_return_sets.put(new_lambda, cloned);
                // Free the original since we're returning null (not passing ownership to caller)
                var to_free = body_lambda_set;
                to_free.deinit(self.allocator);
            }

            return .{ .expr = new_lambda, .lambda_set = null };
        },
        .e_if => |if_expr| {
            // If expression - collect closures from all branches
            // We need to handle the case where some branches are closures and others are pure lambdas
            var lambda_set = LambdaSet.init();

            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);

            // First pass: transform all branches and collect lambda sets
            // Store transformed branches temporarily
            const BranchInfo = struct { cond: Expr.Idx, body: Expr.Idx, has_lambda_set: bool };
            var transformed_branches = std.ArrayList(BranchInfo).empty;
            defer transformed_branches.deinit(self.allocator);

            var any_branch_has_lambda_set = false;

            for (branches) |branch_idx| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                const new_cond = try self.transformExpr(branch.cond);

                // Transform branch body and collect its lambda set
                var body_result = try self.transformExprWithLambdaSet(branch.body, name_hint);
                if (body_result.lambda_set) |*branch_lambda_set| {
                    try lambda_set.merge(self.allocator, branch_lambda_set);
                    branch_lambda_set.deinit(self.allocator);
                    any_branch_has_lambda_set = true;
                }

                try transformed_branches.append(self.allocator, .{
                    .cond = new_cond,
                    .body = body_result.expr,
                    .has_lambda_set = body_result.lambda_set != null,
                });
            }

            // Transform else branch and collect its lambda set
            var else_result = try self.transformExprWithLambdaSet(if_expr.final_else, name_hint);
            const else_has_lambda_set = else_result.lambda_set != null;
            if (else_result.lambda_set) |*else_lambda_set| {
                try lambda_set.merge(self.allocator, else_lambda_set);
                else_lambda_set.deinit(self.allocator);
                any_branch_has_lambda_set = true;
            }

            // Second pass: if any branch has a lambda set, convert pure lambdas to closure tags
            const branch_start = self.module_env.store.scratch.?.if_branches.top();

            if (any_branch_has_lambda_set) {
                for (transformed_branches.items) |*tb| {
                    var final_body = tb.body;
                    if (!tb.has_lambda_set) {
                        // Check if this is a pure lambda that needs to be converted to a closure tag
                        const body_expr = self.module_env.store.getExpr(tb.body);
                        if (body_expr == .e_lambda) {
                            // Convert pure lambda to closure tag with empty captures
                            const lambda = body_expr.e_lambda;
                            const tag_name = try self.generateClosureTagName(name_hint);

                            // Create empty record for captures
                            const empty_record = try self.module_env.store.addExpr(Expr.e_empty_record, base.Region.zero());

                            // Create tag expression with empty record
                            const args_start = self.module_env.store.scratch.?.exprs.top();
                            try self.module_env.store.scratch.?.exprs.append(empty_record);
                            const args_span = try self.module_env.store.exprSpanFrom(args_start);

                            final_body = try self.module_env.store.addExpr(Expr{
                                .e_tag = .{
                                    .name = tag_name,
                                    .args = args_span,
                                },
                            }, base.Region.zero());

                            // Create patterns for calling the lifted function in dispatch
                            const lifted_patterns = try self.createLiftedFunctionPatterns(tag_name, false);

                            // Add to lambda set with the lambda's info
                            try lambda_set.addClosure(self.allocator, ClosureInfo{
                                .tag_name = tag_name,
                                .source_module = self.module_env.module_name_idx,
                                .lambda_body = lambda.body,
                                .lambda_args = lambda.args,
                                .capture_names = std.ArrayList(base.Ident.Idx).empty,
                                .lifted_fn_pattern = lifted_patterns.fn_pattern,
                                .lifted_captures_pattern = lifted_patterns.captures_pattern,
                            });
                        }
                    }

                    const new_branch_idx = try self.module_env.store.addIfBranch(
                        Expr.IfBranch{ .cond = tb.cond, .body = final_body },
                        base.Region.zero(),
                    );
                    try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
                }

                // Handle else branch - convert pure lambda if needed
                var final_else = else_result.expr;
                if (!else_has_lambda_set) {
                    const else_expr = self.module_env.store.getExpr(else_result.expr);
                    if (else_expr == .e_lambda) {
                        const lambda = else_expr.e_lambda;
                        const tag_name = try self.generateClosureTagName(name_hint);

                        // Create empty record for captures
                        const empty_record = try self.module_env.store.addExpr(Expr.e_empty_record, base.Region.zero());

                        // Create tag expression with empty record
                        const args_start = self.module_env.store.scratch.?.exprs.top();
                        try self.module_env.store.scratch.?.exprs.append(empty_record);
                        const args_span = try self.module_env.store.exprSpanFrom(args_start);

                        final_else = try self.module_env.store.addExpr(Expr{
                            .e_tag = .{
                                .name = tag_name,
                                .args = args_span,
                            },
                        }, base.Region.zero());

                        // Create patterns for calling the lifted function in dispatch
                        const lifted_patterns = try self.createLiftedFunctionPatterns(tag_name, false);

                        // Add to lambda set with the lambda's info
                        try lambda_set.addClosure(self.allocator, ClosureInfo{
                            .tag_name = tag_name,
                            .source_module = self.module_env.module_name_idx,
                            .lambda_body = lambda.body,
                            .lambda_args = lambda.args,
                            .capture_names = std.ArrayList(base.Ident.Idx).empty,
                            .lifted_fn_pattern = lifted_patterns.fn_pattern,
                            .lifted_captures_pattern = lifted_patterns.captures_pattern,
                        });
                    }
                }

                const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);

                const new_if = try self.module_env.store.addExpr(Expr{
                    .e_if = .{
                        .branches = new_branches_span,
                        .final_else = final_else,
                    },
                }, base.Region.zero());

                return .{ .expr = new_if, .lambda_set = lambda_set };
            } else {
                // No closures in any branch, just create the if expression normally
                for (transformed_branches.items) |tb| {
                    const new_branch_idx = try self.module_env.store.addIfBranch(
                        Expr.IfBranch{ .cond = tb.cond, .body = tb.body },
                        base.Region.zero(),
                    );
                    try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
                }

                const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);

                const new_if = try self.module_env.store.addExpr(Expr{
                    .e_if = .{
                        .branches = new_branches_span,
                        .final_else = else_result.expr,
                    },
                }, base.Region.zero());

                return .{ .expr = new_if, .lambda_set = null };
            }
        },
        .e_call => |call| {
            // Call expression - transform and check if calling a function that returns closures
            const transformed = try self.transformExpr(expr_idx);

            // Check if the function is a local variable that returns closures when called
            const func_expr = self.module_env.store.getExpr(call.func);
            switch (func_expr) {
                .e_lookup_local => |lookup| {
                    // Check if this pattern has a lambda return set (calling a lambda that returns closures)
                    if (self.pattern_lambda_return_sets.get(lookup.pattern_idx)) |return_set| {
                        // Clone the return set for this call's result
                        const cloned = try return_set.clone(self.allocator);
                        return .{ .expr = transformed, .lambda_set = cloned };
                    }
                },
                else => {},
            }

            return .{ .expr = transformed, .lambda_set = null };
        },
        .e_type_var_dispatch => |tvd| {
            // Static dispatch reference - this is an unspecialized closure.
            // We record it as an unspecialized entry that will be resolved
            // during monomorphization when the concrete type is known.
            //
            // For example, `Thing.hash` becomes an unspecialized closure until
            // we know what concrete type `Thing` resolves to.
            const dispatch_info = StaticDispatchInfo{
                .method_name = tvd.method_name,
                .type_var_alias_stmt = tvd.type_var_alias_stmt,
                .args = tvd.args,
            };

            // Create a lambda set with this unspecialized closure
            var lambda_set = LambdaSet.init();
            const unspec = self.createUnspecializedClosure(dispatch_info, expr_idx);
            try lambda_set.addUnspecialized(self.allocator, unspec);

            // Transform arguments if any
            const args = self.module_env.store.sliceExpr(tvd.args);
            if (args.len > 0) {
                const args_start = self.module_env.store.scratch.?.exprs.top();
                for (args) |arg_idx| {
                    const new_arg = try self.transformExpr(arg_idx);
                    try self.module_env.store.scratch.?.exprs.append(new_arg);
                }
                const new_args_span = try self.module_env.store.exprSpanFrom(args_start);

                // Create new e_type_var_dispatch with transformed args
                const new_expr = try self.module_env.store.addExpr(Expr{
                    .e_type_var_dispatch = .{
                        .type_var_alias_stmt = tvd.type_var_alias_stmt,
                        .method_name = tvd.method_name,
                        .args = new_args_span,
                    },
                }, base.Region.zero());

                return .{ .expr = new_expr, .lambda_set = lambda_set };
            }

            return .{ .expr = expr_idx, .lambda_set = lambda_set };
        },
        else => {
            // Other expressions - just transform without lambda set
            const transformed = try self.transformExpr(expr_idx);
            return .{ .expr = transformed, .lambda_set = null };
        },
    }
}

/// Transform a closure expression into a tag with capture record.
/// Returns the new expression index.
pub fn transformClosure(
    self: *Self,
    closure_expr_idx: Expr.Idx,
    binding_name_hint: ?base.Ident.Idx,
) !Expr.Idx {
    const expr = self.module_env.store.getExpr(closure_expr_idx);

    switch (expr) {
        .e_closure => |closure| {
            // Get the lambda body and args
            const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
            const lambda = switch (lambda_expr) {
                .e_lambda => |l| l,
                else => return closure_expr_idx, // Not a lambda, return as-is
            };

            // Get captures
            const captures = self.module_env.store.sliceCaptures(closure.captures);

            // Build capture record fields, filtering out top-level patterns
            // (top-level constants don't need to be captured since they're always in scope)
            const scratch_top = self.module_env.store.scratch.?.record_fields.top();

            var capture_names = std.ArrayList(base.Ident.Idx).empty;
            var non_toplevel_capture_count: usize = 0;

            for (captures) |capture_idx| {
                const capture = self.module_env.store.getCapture(capture_idx);

                // Skip top-level patterns - they don't need to be captured
                if (self.isTopLevel(capture.pattern_idx)) {
                    continue;
                }

                non_toplevel_capture_count += 1;

                // Create a lookup expression for the captured variable
                // Use store.addExpr directly to avoid region sync checks during transformation
                const lookup_expr = try self.module_env.store.addExpr(Expr{
                    .e_lookup_local = .{ .pattern_idx = capture.pattern_idx },
                }, base.Region.zero());

                // Create record field: { capture_name: capture_value }
                const field = RecordField{
                    .name = capture.name,
                    .value = lookup_expr,
                };
                const field_idx = try self.module_env.store.addRecordField(field, base.Region.zero());
                try self.module_env.store.scratch.?.record_fields.append(field_idx);
                try capture_names.append(self.allocator, capture.name);
            }

            // If all captures were top-level, this is effectively a pure lambda
            // But we still need to transform its body (it might contain nested closures)
            if (non_toplevel_capture_count == 0) {
                capture_names.deinit(self.allocator);

                // Transform the lambda's body using transformExprWithLambdaSet to track
                // what closures the body returns (important for nested closures)
                const body_result = try self.transformExprWithLambdaSet(lambda.body, binding_name_hint);
                if (body_result.expr == lambda.body) {
                    // Body unchanged, return original lambda
                    return closure.lambda_idx;
                }

                // Create new lambda with transformed body
                const new_lambda = try self.module_env.store.addExpr(Expr{
                    .e_lambda = .{
                        .args = lambda.args,
                        .body = body_result.expr,
                    },
                }, base.Region.zero());

                // If the body returns closures, track this so calls to this lambda return those closures
                if (body_result.lambda_set) |body_lambda_set| {
                    try self.lambda_return_sets.put(new_lambda, body_lambda_set);
                }

                return new_lambda;
            }

            // Generate tag name (only if we have real captures)
            const tag_name = try self.generateClosureTagName(binding_name_hint);

            // Create the record expression
            const fields_span = try self.module_env.store.recordFieldSpanFrom(scratch_top);

            const record_expr = try self.module_env.store.addExpr(Expr{
                .e_record = .{ .fields = fields_span, .ext = null },
            }, base.Region.zero());

            // Create the tag expression: `tagName(captureRecord)
            // First, add the record as an argument
            const args_start = self.module_env.store.scratch.?.exprs.top();
            try self.module_env.store.scratch.?.exprs.append(record_expr);
            const args_span = try self.module_env.store.exprSpanFrom(args_start);

            const tag_expr = try self.module_env.store.addExpr(Expr{
                .e_tag = .{
                    .name = tag_name,
                    .args = args_span,
                },
            }, base.Region.zero());

            // Create patterns for calling the lifted function in dispatch
            const lifted_patterns = try self.createLiftedFunctionPatterns(
                tag_name,
                capture_names.items.len > 0,
            );

            // Store closure info for dispatch function generation
            try self.closures.put(closure_expr_idx, ClosureInfo{
                .tag_name = tag_name,
                .source_module = self.module_env.module_name_idx,
                .lambda_body = lambda.body,
                .lambda_args = lambda.args,
                .capture_names = capture_names,
                .lifted_fn_pattern = lifted_patterns.fn_pattern,
                .lifted_captures_pattern = lifted_patterns.captures_pattern,
            });

            return tag_expr;
        },
        .e_lambda => {
            // Pure lambda (no captures) - leave unchanged, no transformation needed
            return closure_expr_idx;
        },
        else => return closure_expr_idx, // Not a closure, return as-is
    }
}

/// Transform an entire expression tree, handling closures and their call sites.
/// This is the main entry point for the transformation.
pub fn transformExpr(self: *Self, expr_idx: Expr.Idx) std.mem.Allocator.Error!Expr.Idx {
    const expr = self.module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_closure => {
            // Transform closure to tag
            return try self.transformClosure(expr_idx, null);
        },
        .e_lambda => |lambda| {
            // Pure lambda (no captures) - transform the body to handle nested closures
            const transformed_body = try self.transformExpr(lambda.body);

            // If body is unchanged, return original lambda
            if (transformed_body == lambda.body) {
                return expr_idx;
            }

            // Create new lambda with transformed body
            return try self.module_env.store.addExpr(Expr{
                .e_lambda = .{
                    .args = lambda.args,
                    .body = transformed_body,
                },
            }, base.Region.zero());
        },
        .e_block => |block| {
            // Transform block: handle statements and final expression
            const stmts = self.module_env.store.sliceStatements(block.stmts);

            // Create new statements with transformed expressions
            const stmt_start = self.module_env.store.scratch.?.statements.top();

            for (stmts) |stmt_idx| {
                const stmt = self.module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        // Get binding name hint from pattern
                        const pattern = self.module_env.store.getPattern(decl.pattern);
                        const name_hint: ?base.Ident.Idx = switch (pattern) {
                            .assign => |a| a.ident,
                            else => null,
                        };

                        // Transform expression and collect lambda set
                        const result = try self.transformExprWithLambdaSet(decl.expr, name_hint);

                        // Track this pattern's lambda set if it has closures
                        if (result.lambda_set) |lambda_set| {
                            try self.pattern_lambda_sets.put(decl.pattern, lambda_set);

                            // Detect recursive closures: check if any closure in the lambda set
                            // references the binding pattern in its body
                            if (self.pattern_lambda_sets.getPtr(decl.pattern)) |stored_lambda_set| {
                                for (stored_lambda_set.closures.items, 0..) |*closure_info, i| {
                                    // Check if this closure references the binding pattern
                                    if (self.detectRecursion(decl.pattern, closure_info.lambda_body)) {
                                        markLambdaSetRecursive(stored_lambda_set, &stored_lambda_set.closures.items[i]);
                                        break; // Only one closure needs to be marked recursive
                                    }
                                }
                            }
                        }

                        // Create new statement with transformed expression
                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl = .{
                                .pattern = decl.pattern,
                                .expr = result.expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    else => {
                        // Copy statement as-is
                        try self.module_env.store.scratch.?.statements.append(stmt_idx);
                    },
                }
            }

            const new_stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);

            // Transform final expression
            const new_final = try self.transformExpr(block.final_expr);

            // Create new block
            return try self.module_env.store.addExpr(Expr{
                .e_block = .{
                    .stmts = new_stmts_span,
                    .final_expr = new_final,
                },
            }, base.Region.zero());
        },
        .e_call => |call| {
            // First transform arguments recursively
            const args = self.module_env.store.sliceExpr(call.args);
            const args_start = self.module_env.store.scratch.?.exprs.top();

            for (args) |arg_idx| {
                const new_arg = try self.transformExpr(arg_idx);
                try self.module_env.store.scratch.?.exprs.append(new_arg);
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);
            const transformed_args = self.module_env.store.sliceExpr(new_args_span);

            // Check if the function is a local variable that holds a closure
            const func_expr = self.module_env.store.getExpr(call.func);
            switch (func_expr) {
                .e_lookup_local => |lookup| {
                    // Check if this pattern has a lambda set (one or more closures)
                    if (self.pattern_lambda_sets.getPtr(lookup.pattern_idx)) |lambda_set| {
                        // Empty lambda set is a compiler bug - no closures can flow to this variable
                        std.debug.assert(!isLambdaSetEmpty(lambda_set));
                        // Generate a dispatch match expression for all possible closures
                        return try self.generateLambdaSetDispatchMatch(
                            call.func,
                            lambda_set,
                            transformed_args,
                        );
                    }
                },
                else => {},
            }

            // Not a closure call, transform normally
            const new_func = try self.transformExpr(call.func);

            return try self.module_env.store.addExpr(Expr{
                .e_call = .{
                    .func = new_func,
                    .args = new_args_span,
                    .called_via = call.called_via,
                },
            }, base.Region.zero());
        },
        .e_if => |if_expr| {
            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
            const branch_start = self.module_env.store.scratch.?.if_branches.top();

            for (branches) |branch_idx| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                const new_cond = try self.transformExpr(branch.cond);
                const new_body = try self.transformExpr(branch.body);

                const new_branch_idx = try self.module_env.store.addIfBranch(
                    Expr.IfBranch{ .cond = new_cond, .body = new_body },
                    base.Region.zero(),
                );
                try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
            }

            const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);
            const new_else = try self.transformExpr(if_expr.final_else);

            return try self.module_env.store.addExpr(Expr{
                .e_if = .{
                    .branches = new_branches_span,
                    .final_else = new_else,
                },
            }, base.Region.zero());
        },
        .e_binop => |binop| {
            const new_lhs = try self.transformExpr(binop.lhs);
            const new_rhs = try self.transformExpr(binop.rhs);

            // Return original if unchanged
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
        .e_lookup_pending,
        .e_empty_list,
        .e_empty_record,
        .e_zero_argument_tag,
        .e_runtime_error,
        .e_ellipsis,
        .e_anno_only,
        .e_lookup_required,
        .e_hosted_lambda,
        .e_low_level_lambda,
        => return expr_idx,

        .e_type_var_dispatch => |tvd| {
            // Transform arguments of the static dispatch
            const args = self.module_env.store.sliceExpr(tvd.args);
            if (args.len == 0) {
                return expr_idx;
            }

            const args_start = self.module_env.store.scratch.?.exprs.top();
            var any_changed = false;

            for (args) |arg_idx| {
                const new_arg = try self.transformExpr(arg_idx);
                if (new_arg != arg_idx) {
                    any_changed = true;
                }
                try self.module_env.store.scratch.?.exprs.append(new_arg);
            }

            if (!any_changed) {
                return expr_idx;
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);

            return try self.module_env.store.addExpr(Expr{
                .e_type_var_dispatch = .{
                    .type_var_alias_stmt = tvd.type_var_alias_stmt,
                    .method_name = tvd.method_name,
                    .args = new_args_span,
                },
            }, base.Region.zero());
        },

        .e_list => |list| {
            const elems = self.module_env.store.sliceExpr(list.elems);
            const elems_start = self.module_env.store.scratch.?.exprs.top();

            for (elems) |elem_idx| {
                const new_elem = try self.transformExpr(elem_idx);
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
                const new_elem = try self.transformExpr(elem_idx);
                try self.module_env.store.scratch.?.exprs.append(new_elem);
            }

            const new_elems_span = try self.module_env.store.exprSpanFrom(elems_start);

            return try self.module_env.store.addExpr(Expr{
                .e_tuple = .{ .elems = new_elems_span },
            }, base.Region.zero());
        },
        .e_tuple_access => |tuple_access| {
            const new_tuple = try self.transformExpr(tuple_access.tuple);

            if (new_tuple == tuple_access.tuple) {
                return expr_idx;
            }

            return try self.module_env.store.addExpr(Expr{
                .e_tuple_access = .{
                    .tuple = new_tuple,
                    .elem_index = tuple_access.elem_index,
                },
            }, base.Region.zero());
        },
        .e_record => |record| {
            const field_indices = self.module_env.store.sliceRecordFields(record.fields);
            const fields_start = self.module_env.store.scratch.?.record_fields.top();

            for (field_indices) |field_idx| {
                const field = self.module_env.store.getRecordField(field_idx);
                const new_value = try self.transformExpr(field.value);

                const new_field = RecordField{
                    .name = field.name,
                    .value = new_value,
                };
                const new_field_idx = try self.module_env.store.addRecordField(new_field, base.Region.zero());
                try self.module_env.store.scratch.?.record_fields.append(new_field_idx);
            }

            const new_fields_span = try self.module_env.store.recordFieldSpanFrom(fields_start);

            const new_ext = if (record.ext) |ext| try self.transformExpr(ext) else null;

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
                const new_arg = try self.transformExpr(arg_idx);
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
            const new_expr = try self.transformExpr(unary.expr);
            return try self.module_env.store.addExpr(Expr{
                .e_unary_minus = .{ .expr = new_expr },
            }, base.Region.zero());
        },
        .e_unary_not => |unary| {
            const new_expr = try self.transformExpr(unary.expr);
            return try self.module_env.store.addExpr(Expr{
                .e_unary_not = .{ .expr = new_expr },
            }, base.Region.zero());
        },
        .e_dot_access => |dot| {
            const new_receiver = try self.transformExpr(dot.receiver);
            const new_args = if (dot.args) |args_span| blk: {
                const args = self.module_env.store.sliceExpr(args_span);
                const args_start = self.module_env.store.scratch.?.exprs.top();

                for (args) |arg_idx| {
                    const new_arg = try self.transformExpr(arg_idx);
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
        .e_crash => return expr_idx,
        .e_dbg => |dbg| {
            const new_expr = try self.transformExpr(dbg.expr);
            return try self.module_env.store.addExpr(Expr{
                .e_dbg = .{
                    .expr = new_expr,
                },
            }, base.Region.zero());
        },
        .e_expect => |expect| {
            const new_body = try self.transformExpr(expect.body);
            return try self.module_env.store.addExpr(Expr{
                .e_expect = .{
                    .body = new_body,
                },
            }, base.Region.zero());
        },
        .e_return => |ret| {
            const new_expr = try self.transformExpr(ret.expr);
            return try self.module_env.store.addExpr(Expr{
                .e_return = .{ .expr = new_expr, .lambda = ret.lambda, .context = ret.context },
            }, base.Region.zero());
        },
        .e_match => |match| {
            const new_cond = try self.transformExpr(match.cond);

            // Transform all branch bodies and guards
            const branch_indices = self.module_env.store.sliceMatchBranches(match.branches);
            const branch_start = self.module_env.store.scratchMatchBranchTop();

            for (branch_indices) |branch_idx| {
                const branch = self.module_env.store.getMatchBranch(branch_idx);

                // Transform the branch value (body)
                const new_value = try self.transformExpr(branch.value);

                // Transform the guard if present
                const new_guard = if (branch.guard) |guard|
                    try self.transformExpr(guard)
                else
                    null;

                // Create new branch with transformed value and guard
                const new_branch = Expr.Match.Branch{
                    .patterns = branch.patterns,
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
            const new_backing = try self.transformExpr(nominal.backing_expr);
            return try self.module_env.store.addExpr(Expr{
                .e_nominal = .{
                    .nominal_type_decl = nominal.nominal_type_decl,
                    .backing_expr = new_backing,
                    .backing_type = nominal.backing_type,
                },
            }, base.Region.zero());
        },
        .e_nominal_external => |nominal| {
            const new_backing = try self.transformExpr(nominal.backing_expr);
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
            const new_expr = try self.transformExpr(for_expr.expr);
            const new_body = try self.transformExpr(for_expr.body);
            return try self.module_env.store.addExpr(Expr{
                .e_for = .{
                    .patt = for_expr.patt,
                    .expr = new_expr,
                    .body = new_body,
                },
            }, base.Region.zero());
        },
    }
}

// Tests

const testing = std.testing;

test "ClosureTransformer: init and deinit" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    try testing.expectEqual(@as(u32, 0), transformer.closure_counter);
}

test "ClosureTransformer: generateClosureTagName with hint" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    // Create a hint identifier
    const hint = try module_env.insertIdent(base.Ident.for_text("addX"));

    const tag_name = try transformer.generateClosureTagName(hint);
    const tag_str = module_env.getIdent(tag_name);

    try testing.expectEqualStrings("#1_addX", tag_str);
}

test "ClosureTransformer: generateClosureTagName without hint" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    const tag_name = try transformer.generateClosureTagName(null);
    const tag_str = module_env.getIdent(tag_name);

    try testing.expectEqualStrings("#1", tag_str);
}

test "ClosureTransformer: region tracking" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    // Initial region should be 0
    try testing.expectEqual(@as(u8, 0), transformer.current_region);

    // Enter nested scopes
    const region0 = transformer.enterRegion();
    try testing.expectEqual(@as(u8, 0), region0);
    try testing.expectEqual(@as(u8, 1), transformer.current_region);

    const region1 = transformer.enterRegion();
    try testing.expectEqual(@as(u8, 1), region1);
    try testing.expectEqual(@as(u8, 2), transformer.current_region);

    // Exit scopes
    transformer.exitRegion(region1);
    try testing.expectEqual(@as(u8, 1), transformer.current_region);

    transformer.exitRegion(region0);
    try testing.expectEqual(@as(u8, 0), transformer.current_region);
}

test "LambdaSet: unspecialized closures" {
    const allocator = testing.allocator;

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    // Initially empty
    try testing.expect(!lambda_set.hasUnspecialized());
    try testing.expect(lambda_set.isFullyResolved());

    // Add an unspecialized closure
    const unspec = UnspecializedClosure{
        .type_var = @enumFromInt(100), // Dummy type var for test
        .member = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = 42,
        },
        .member_expr = @enumFromInt(1),
        .region = 1,
    };
    try lambda_set.addUnspecialized(allocator, unspec);

    // Now has unspecialized
    try testing.expect(lambda_set.hasUnspecialized());
    try testing.expect(!lambda_set.isFullyResolved());
    try testing.expectEqual(@as(usize, 1), lambda_set.unspecialized.items.len);
}

test "LambdaSet: merge with unspecialized" {
    const allocator = testing.allocator;

    var set1 = LambdaSet.init();
    defer set1.deinit(allocator);

    var set2 = LambdaSet.init();
    defer set2.deinit(allocator);

    // Add unspecialized to set1
    const unspec1 = UnspecializedClosure{
        .type_var = @enumFromInt(100), // Dummy type var for test
        .member = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = 10,
        },
        .member_expr = @enumFromInt(1),
        .region = 0,
    };
    try set1.addUnspecialized(allocator, unspec1);

    // Add unspecialized to set2
    const unspec2 = UnspecializedClosure{
        .type_var = @enumFromInt(101), // Dummy type var for test
        .member = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = 20,
        },
        .member_expr = @enumFromInt(2),
        .region = 1,
    };
    try set2.addUnspecialized(allocator, unspec2);

    // Merge set2 into set1
    try set1.merge(allocator, &set2);

    // set1 should have both unspecialized closures
    try testing.expectEqual(@as(usize, 2), set1.unspecialized.items.len);
    try testing.expect(set1.hasUnspecialized());
}

test "LambdaSet: isEmpty" {
    const allocator = testing.allocator;

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    // Empty lambda set
    try testing.expect(isLambdaSetEmpty(&lambda_set));

    // Add an unspecialized closure
    const unspec = UnspecializedClosure{
        .type_var = @enumFromInt(100), // Dummy type var for test
        .member = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = 1,
        },
        .member_expr = @enumFromInt(1),
        .region = 0,
    };
    try lambda_set.addUnspecialized(allocator, unspec);

    // No longer empty
    try testing.expect(!isLambdaSetEmpty(&lambda_set));
}

test "LambdaSet: isPureLambdaSet with no closures" {
    const allocator = testing.allocator;

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    // Empty is pure (vacuously true)
    try testing.expect(lambda_set.isPureLambdaSet());
}

test "LambdaSet: maxCaptureCount" {
    const allocator = testing.allocator;

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    // Empty lambda set has max 0
    try testing.expectEqual(@as(usize, 0), lambda_set.maxCaptureCount());
}

test "LambdaSet: canUseCompactRepresentation" {
    const allocator = testing.allocator;

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    // Empty can use compact
    try testing.expect(lambda_set.canUseCompactRepresentation());
}

test "UnspecializedByTypeVar: init and deinit" {
    const allocator = testing.allocator;

    var tracker = UnspecializedByTypeVar.init(allocator);
    defer tracker.deinit();

    // Initially empty
    try testing.expectEqual(@as(usize, 0), tracker.totalEntryCount());
}

test "UnspecializedByTypeVar: track and retrieve" {
    const allocator = testing.allocator;

    var tracker = UnspecializedByTypeVar.init(allocator);
    defer tracker.deinit();

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    // Create a type variable (index 42)
    const type_var: types.Var = @enumFromInt(42);

    // Track an entry
    try tracker.trackEntry(type_var, .{ .lambda_set = &lambda_set, .index = 0 });

    // Should be retrievable
    const entries = tracker.getEntriesForVar(type_var);
    try testing.expect(entries != null);
    try testing.expectEqual(@as(usize, 1), entries.?.len);
    try testing.expectEqual(&lambda_set, entries.?[0].lambda_set);
    try testing.expectEqual(@as(usize, 0), entries.?[0].index);

    // Total count should be 1
    try testing.expectEqual(@as(usize, 1), tracker.totalEntryCount());
}

test "UnspecializedByTypeVar: multiple entries per type var" {
    const allocator = testing.allocator;

    var tracker = UnspecializedByTypeVar.init(allocator);
    defer tracker.deinit();

    var lambda_set1 = LambdaSet.init();
    defer lambda_set1.deinit(allocator);
    var lambda_set2 = LambdaSet.init();
    defer lambda_set2.deinit(allocator);

    const type_var: types.Var = @enumFromInt(42);

    // Track multiple entries for the same type var
    try tracker.trackEntry(type_var, .{ .lambda_set = &lambda_set1, .index = 0 });
    try tracker.trackEntry(type_var, .{ .lambda_set = &lambda_set2, .index = 1 });

    const entries = tracker.getEntriesForVar(type_var);
    try testing.expect(entries != null);
    try testing.expectEqual(@as(usize, 2), entries.?.len);
    try testing.expectEqual(@as(usize, 2), tracker.totalEntryCount());
}

test "UnspecializedByTypeVar: multiple type vars" {
    const allocator = testing.allocator;

    var tracker = UnspecializedByTypeVar.init(allocator);
    defer tracker.deinit();

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    const type_var1: types.Var = @enumFromInt(42);
    const type_var2: types.Var = @enumFromInt(99);

    // Track entries for different type vars
    try tracker.trackEntry(type_var1, .{ .lambda_set = &lambda_set, .index = 0 });
    try tracker.trackEntry(type_var2, .{ .lambda_set = &lambda_set, .index = 1 });

    // Both should be retrievable
    try testing.expect(tracker.hasEntriesForVar(type_var1));
    try testing.expect(tracker.hasEntriesForVar(type_var2));
    try testing.expectEqual(@as(usize, 2), tracker.totalEntryCount());
}

test "UnspecializedByTypeVar: remove var" {
    const allocator = testing.allocator;

    var tracker = UnspecializedByTypeVar.init(allocator);
    defer tracker.deinit();

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    const type_var: types.Var = @enumFromInt(42);

    try tracker.trackEntry(type_var, .{ .lambda_set = &lambda_set, .index = 0 });
    try testing.expect(tracker.hasEntriesForVar(type_var));

    // Remove the type var
    tracker.removeVar(type_var);

    // Should no longer be present
    try testing.expect(!tracker.hasEntriesForVar(type_var));
    try testing.expect(tracker.getEntriesForVar(type_var) == null);
    try testing.expectEqual(@as(usize, 0), tracker.totalEntryCount());
}

test "UnspecializedByTypeVar: non-existent type var" {
    const allocator = testing.allocator;

    var tracker = UnspecializedByTypeVar.init(allocator);
    defer tracker.deinit();

    const type_var: types.Var = @enumFromInt(42);

    // Non-existent type var returns null
    try testing.expect(tracker.getEntriesForVar(type_var) == null);
    try testing.expect(!tracker.hasEntriesForVar(type_var));
}

test "LambdaSet: ambient_function_var initialization" {
    const allocator = testing.allocator;

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    // Initially null
    try testing.expect(lambda_set.ambient_function_var == null);

    // Can set it
    const func_var: types.Var = @enumFromInt(123);
    lambda_set.ambient_function_var = func_var;
    try testing.expectEqual(func_var, lambda_set.ambient_function_var.?);
}

test "LambdaSet: clone preserves ambient_function_var" {
    const allocator = testing.allocator;

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    const func_var: types.Var = @enumFromInt(456);
    lambda_set.ambient_function_var = func_var;

    var cloned = try lambda_set.clone(allocator);
    defer cloned.deinit(allocator);

    try testing.expectEqual(func_var, cloned.ambient_function_var.?);
}

test "ClosureTransformer: validateAllResolved detects unresolved" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    // Initially valid (no lambda sets with unspecialized closures)
    const initial_result = transformer.validateAllResolved();
    try testing.expect(initial_result.is_valid);
    try testing.expectEqual(@as(usize, 0), initial_result.unresolved_count);

    // Create a pattern index for testing
    const pattern_idx: CIR.Pattern.Idx = @enumFromInt(1);

    // Create a lambda set with an unspecialized closure
    // Note: We don't defer deinit on lambda_set because it gets moved into
    // pattern_lambda_sets which is cleaned up by transformer.deinit()
    var lambda_set = LambdaSet.init();
    const unspec = UnspecializedClosure{
        .type_var = @enumFromInt(100), // Dummy type var for test
        .member = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = 42,
        },
        .member_expr = @enumFromInt(1),
        .region = 0,
    };
    try lambda_set.addUnspecialized(allocator, unspec);

    // Add to pattern_lambda_sets (note: this transfers ownership)
    try transformer.pattern_lambda_sets.put(pattern_idx, lambda_set);

    // Now validation should fail
    const result = transformer.validateAllResolved();
    try testing.expect(!result.is_valid);
    try testing.expectEqual(@as(usize, 1), result.unresolved_count);
    try testing.expect(result.first_error != null);
    try testing.expectEqual(ResolutionError.Kind.missing_static_dispatch, result.first_error.?.kind);
}
