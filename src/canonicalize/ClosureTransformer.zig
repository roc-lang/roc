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
//!    - Transforms closures to tags with capture records (`Closure_name_N({ ... })`)
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
//! After ClosureTransformer + LambdaLifter:
//! ```roc
//! closure_addX_1 = |y, captures| captures.x + y
//!
//! x = 42
//! addX = Closure_addX_1({ x: x })
//! result = match addX {
//!     Closure_addX_1(captures) => closure_addX_1(10, captures)
//! }
//! ```
//!
//! ## Implementation Notes
//!
//! - Closures become tags with capture records (using `Closure_` prefix to avoid name clashes)
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
    /// The tag name for this closure (e.g., `Closure_addX_1`)
    tag_name: base.Ident.Idx,
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

/// Unspecialized Closure - represents an ability-polymorphic closure.
///
/// These are closures that depend on ability implementations which won't be
/// known until monomorphization. For example, when code uses `hash` from the
/// Hash ability, we don't know which concrete `hash` implementation to dispatch
/// to until we know the concrete type.
///
/// These are resolved during monomorphization when concrete types are known.
pub const UnspecializedClosure = struct {
    /// The ability member symbol (e.g., `hash` from Hash ability)
    member: base.Ident.Idx,

    /// The expression that references the ability member.
    /// Used to locate this in the IR for resolution.
    member_expr: Expr.Idx,

    /// Region number for ordering during resolution.
    /// When resolving nested ability-polymorphic closures, we process
    /// innermost first (higher region numbers first).
    region: u8,
};

/// Information about a concrete type for cross-module requests.
/// This allows external modules to know what concrete type to use when
/// resolving an ability implementation.
pub const ConcreteTypeInfo = struct {
    /// The type identifier (e.g., "List", "Dict")
    type_ident: ?base.Ident.Idx,
    /// Type arguments if applicable (for generic types like List U64)
    type_args_hash: u64,
};

/// External Lambda Set Request - for cross-module lambda set resolution.
///
/// When a call crosses module boundaries and involves an ability-polymorphic
/// closure, we need to request the ability implementation from the external
/// module. This struct captures that request.
pub const ExternalLambdaSetRequest = struct {
    /// The module containing the ability implementation
    source_module: CIR.Import.Idx,

    /// The ability member being requested (e.g., `hash`, `eq`)
    ability_member: base.Ident.Idx,

    /// The concrete type for which we need the implementation
    concrete_type_info: ConcreteTypeInfo,

    /// The original unspecialized closure this request came from
    original_unspec: UnspecializedClosure,

    pub fn eql(a: ExternalLambdaSetRequest, b: ExternalLambdaSetRequest) bool {
        return a.source_module == b.source_module and
            a.ability_member == b.ability_member and
            a.concrete_type_info.type_args_hash == b.concrete_type_info.type_args_hash;
    }
};

/// Result of an external lambda set resolution.
/// Returned when the external module provides the closure info.
pub const ExternalLambdaSetResult = struct {
    /// The closure info for the resolved ability implementation
    closure_info: ClosureInfo,
    /// Whether this is a new resolution or was already cached
    is_new: bool,
};

/// Exported closure information for module interfaces.
///
/// This struct contains information about closures that external modules
/// might need when resolving their own lambda sets. It's part of the
/// module's exported interface.
pub const ExportedClosureInfo = struct {
    /// The closure's tag name (e.g., "Closure_hash_1")
    closure_name: base.Ident.Idx,

    /// The lifted function pattern for dispatch
    lifted_fn_pattern: ?CIR.Pattern.Idx,

    /// Ability member this implements (if any)
    implements_ability: ?AbilityImpl,

    pub const AbilityImpl = struct {
        /// The ability identifier (e.g., "Hash", "Eq")
        ability: base.Ident.Idx,
        /// The member within the ability (e.g., "hash", "eq")
        member: base.Ident.Idx,
        /// The type this implementation is for (e.g., "List", "Dict")
        for_type: base.Ident.Idx,
    };
};

/// A lambda set - a collection of closures that could reach a given variable.
///
/// Lambda sets can contain both:
/// - Resolved closures: concrete closures with known implementations
/// - Unspecialized closures: ability-polymorphic closures resolved at mono time
pub const LambdaSet = struct {
    /// Resolved/concrete closures in this lambda set
    closures: std.ArrayList(ClosureInfo),

    /// Unspecialized closures (ability-polymorphic).
    /// These are resolved during monomorphization when concrete types are known.
    unspecialized: std.ArrayList(UnspecializedClosure),

    /// For recursive lambda sets (self-referential closures).
    /// Points to the closure that references itself.
    recursion_closure: ?*ClosureInfo,

    pub fn init() LambdaSet {
        return .{
            .closures = std.ArrayList(ClosureInfo).empty,
            .unspecialized = std.ArrayList(UnspecializedClosure).empty,
            .recursion_closure = null,
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

    /// Add an unspecialized (ability-polymorphic) closure to this lambda set.
    /// These will be resolved during monomorphization.
    pub fn addUnspecialized(self: *LambdaSet, allocator: std.mem.Allocator, unspec: UnspecializedClosure) !void {
        try self.unspecialized.append(allocator, unspec);
    }

    /// Check if this lambda set has any unresolved ability-polymorphic closures.
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
        return new_set;
    }

    /// Determine the optimal closure representation for this lambda set.
    /// This is used to generate efficient dispatch code.
    ///
    /// IMPORTANT: This should only be called after all unspecialized closures
    /// have been resolved during monomorphization.
    pub fn determineRepresentation(self: *const LambdaSet) ClosureRepresentation {
        // All ability-polymorphic closures must be resolved before we can
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

    /// Count how many closures in this set are ability-derived (have no captures
    /// and were likely resolved from ability member references).
    ///
    /// Ability implementations are typically top-level functions without captures,
    /// so they're very efficient to dispatch to (no closure environment needed).
    pub fn countAbilityDerivedClosures(self: *const LambdaSet) usize {
        var count: usize = 0;
        for (self.closures.items) |closure| {
            // Ability-derived closures typically have:
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

    /// Check if this lambda set is "pure" (all closures are ability-derived
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
    /// Mixed lambda sets require tagged union representation where ability-derived
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

/// External lambda set requests - for cross-module resolution.
/// These are requests for ability implementations from other modules.
external_lambda_set_requests: std.ArrayList(ExternalLambdaSetRequest),

/// Exported closure information for module interface.
/// These are closures that can be used by other modules.
exported_closures: std.ArrayList(ExportedClosureInfo),

/// Initialize the transformer
pub fn init(allocator: std.mem.Allocator, module_env: *ModuleEnv) Self {
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
        .external_lambda_set_requests = std.ArrayList(ExternalLambdaSetRequest).empty,
        .exported_closures = std.ArrayList(ExportedClosureInfo).empty,
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

/// Info extracted from an ability member reference expression.
pub const AbilityMemberInfo = struct {
    /// The method name being called (e.g., "hash", "eq", "default")
    method_name: base.Ident.Idx,
    /// Reference to the type var alias statement (for type resolution)
    type_var_alias_stmt: CIR.Statement.Idx,
    /// Arguments to the method call
    args: CIR.Expr.Span,
};

/// Check if an expression is an ability member reference (e_type_var_dispatch).
/// Returns the ability member info if so, null otherwise.
pub fn isAbilityMemberRef(self: *const Self, expr_idx: Expr.Idx) ?AbilityMemberInfo {
    const expr = self.module_env.store.getExpr(expr_idx);
    return switch (expr) {
        .e_type_var_dispatch => |tvd| AbilityMemberInfo{
            .method_name = tvd.method_name,
            .type_var_alias_stmt = tvd.type_var_alias_stmt,
            .args = tvd.args,
        },
        else => null,
    };
}

/// Create an unspecialized closure entry for an ability member reference.
/// This records that we need to resolve this ability dispatch at monomorphization time.
pub fn createUnspecializedClosure(
    self: *const Self,
    member_info: AbilityMemberInfo,
    expr_idx: Expr.Idx,
) UnspecializedClosure {
    _ = member_info.type_var_alias_stmt; // Will be used in phase 2 for type resolution
    _ = member_info.args; // Will be used when generating dispatch
    return UnspecializedClosure{
        .member = member_info.method_name,
        .member_expr = expr_idx,
        .region = self.current_region,
    };
}

/// Mark a pattern as a top-level definition (doesn't need to be captured)
pub fn markTopLevel(self: *Self, pattern_idx: CIR.Pattern.Idx) !void {
    try self.top_level_patterns.put(pattern_idx, {});
}

/// Check if a pattern is a top-level definition
pub fn isTopLevel(self: *const Self, pattern_idx: CIR.Pattern.Idx) bool {
    return self.top_level_patterns.contains(pattern_idx);
}

/// Request an ability implementation from an external module.
///
/// Called when transforming code that references an ability member where
/// the implementation is defined in another module.
///
/// Returns true if this is a new request, false if already requested.
pub fn requestExternalLambdaSet(
    self: *Self,
    source_module: CIR.Import.Idx,
    ability_member: base.Ident.Idx,
    concrete_type_info: ConcreteTypeInfo,
    original_unspec: UnspecializedClosure,
) !bool {
    // Check if we already have this request
    for (self.external_lambda_set_requests.items) |existing| {
        if (existing.eql(.{
            .source_module = source_module,
            .ability_member = ability_member,
            .concrete_type_info = concrete_type_info,
            .original_unspec = original_unspec,
        })) {
            return false; // Already requested
        }
    }

    // Add new request
    try self.external_lambda_set_requests.append(self.allocator, .{
        .source_module = source_module,
        .ability_member = ability_member,
        .concrete_type_info = concrete_type_info,
        .original_unspec = original_unspec,
    });

    return true;
}

/// Get all pending external lambda set requests.
///
/// These should be processed after the transformation pass to resolve
/// cross-module ability implementations.
pub fn getExternalLambdaSetRequests(self: *const Self) []const ExternalLambdaSetRequest {
    return self.external_lambda_set_requests.items;
}

/// Resolve an external lambda set request with the closure info from the external module.
///
/// Called when the external module provides the ability implementation closure.
/// Returns the resolved result which includes the closure info.
pub fn resolveExternalLambdaSet(
    self: *Self,
    request: ExternalLambdaSetRequest,
    closure_info: ClosureInfo,
) ExternalLambdaSetResult {
    // Find and remove the request from our pending list
    var found = false;
    var i: usize = 0;
    while (i < self.external_lambda_set_requests.items.len) {
        if (self.external_lambda_set_requests.items[i].eql(request)) {
            _ = self.external_lambda_set_requests.swapRemove(i);
            found = true;
            break;
        }
        i += 1;
    }

    return .{
        .closure_info = closure_info,
        .is_new = found,
    };
}

/// Export a closure for the module interface.
///
/// Called for closures that implement abilities and should be accessible
/// from other modules.
pub fn exportClosure(
    self: *Self,
    closure_name: base.Ident.Idx,
    lifted_fn_pattern: ?CIR.Pattern.Idx,
    implements_ability: ?ExportedClosureInfo.AbilityImpl,
) !void {
    try self.exported_closures.append(self.allocator, .{
        .closure_name = closure_name,
        .lifted_fn_pattern = lifted_fn_pattern,
        .implements_ability = implements_ability,
    });
}

/// Get all exported closures for the module interface.
///
/// These are closures that other modules can reference when resolving
/// their ability-polymorphic lambda sets.
pub fn getExportedClosures(self: *const Self) []const ExportedClosureInfo {
    return self.exported_closures.items;
}

/// Find an exported closure by ability implementation.
///
/// Used when an external module requests a specific ability implementation
/// for a concrete type.
pub fn findExportedClosureByAbility(
    self: *const Self,
    ability: base.Ident.Idx,
    member: base.Ident.Idx,
    for_type: base.Ident.Idx,
) ?ExportedClosureInfo {
    for (self.exported_closures.items) |exported| {
        if (exported.implements_ability) |impl| {
            if (impl.ability == ability and
                impl.member == member and
                impl.for_type == for_type)
            {
                return exported;
            }
        }
    }
    return null;
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
                    .s_decl_gen => |decl| {
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
        .e_str_segment,
        .e_str,
        .e_lookup_external,
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

/// Handle an empty lambda set at a call site.
///
/// This returns the original call site expression for cases where a closure
/// is called but the lambda set is empty. This shouldn't happen in valid
/// programs - it indicates either a compiler bug or a program that's guaranteed
/// to fail at runtime.
///
/// In the future, this could generate a runtime error expression with proper
/// diagnostic information.
pub fn handleEmptyLambdaSet(
    call_site: Expr.Idx,
) Expr.Idx {
    // Return the original call site - this is an error case that shouldn't
    // happen in valid programs. Type checking should catch this.
    return call_site;
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
    var lambda_set_iter = self.pattern_lambda_sets.valueIterator();
    while (lambda_set_iter.next()) |lambda_set| {
        // Don't free individual capture_names here since they share data with closures map
        lambda_set.closures.deinit(self.allocator);
    }
    self.pattern_lambda_sets.deinit();

    // Free lambda return sets
    var return_set_iter = self.lambda_return_sets.valueIterator();
    while (return_set_iter.next()) |lambda_set| {
        lambda_set.closures.deinit(self.allocator);
    }
    self.lambda_return_sets.deinit();

    // Free pattern lambda return sets
    var pattern_return_iter = self.pattern_lambda_return_sets.valueIterator();
    while (pattern_return_iter.next()) |lambda_set| {
        lambda_set.closures.deinit(self.allocator);
    }
    self.pattern_lambda_return_sets.deinit();

    // Free top-level patterns set
    self.top_level_patterns.deinit();

    // Free external lambda set requests
    self.external_lambda_set_requests.deinit(self.allocator);

    // Free exported closures
    self.exported_closures.deinit(self.allocator);
}

/// Generate a unique tag name for a closure
pub fn generateClosureTagName(self: *Self, hint: ?base.Ident.Idx) !base.Ident.Idx {
    self.closure_counter += 1;

    // If we have a hint (e.g., from the variable name), use it with counter for uniqueness
    if (hint) |h| {
        const hint_name = self.module_env.getIdent(h);
        // Use Closure_ prefix (capitalized) to create valid Roc tag names that won't clash with userspace tags
        // Include counter to ensure uniqueness, e.g., "myFunc" becomes "Closure_myFunc_1", "Closure_myFunc_2", etc.
        const tag_name = try std.fmt.allocPrint(
            self.allocator,
            "Closure_{s}_{d}",
            .{ hint_name, self.closure_counter },
        );
        defer self.allocator.free(tag_name);
        return try self.module_env.insertIdent(base.Ident.for_text(tag_name));
    }

    // Otherwise generate a numeric name
    const tag_name = try std.fmt.allocPrint(
        self.allocator,
        "Closure_{d}",
        .{self.closure_counter},
    );
    defer self.allocator.free(tag_name);
    return try self.module_env.insertIdent(base.Ident.for_text(tag_name));
}

/// Generate the lowercase function name from a closure tag name.
/// E.g., "Closure_addX_1" -> "closure_addX_1"
fn generateLiftedFunctionName(self: *Self, tag_name: base.Ident.Idx) !base.Ident.Idx {
    const tag_str = self.module_env.getIdent(tag_name);

    // Allocate a copy with first char lowercased
    var fn_name = try self.allocator.alloc(u8, tag_str.len);
    defer self.allocator.free(fn_name);
    @memcpy(fn_name, tag_str);

    if (fn_name.len > 0 and fn_name[0] >= 'A' and fn_name[0] <= 'Z') {
        fn_name[0] = fn_name[0] + ('a' - 'A');
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
///     Closure_f_1(captures) => closure_f_1(10, captures)
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

    // Step 2: Create the applied_tag pattern: `Closure_f_1(captures)`
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
        },
    }, base.Region.zero());
}

/// Generate a dispatch match expression for a call to a variable with multiple possible closures.
///
/// Phase 5: Generates calls to lifted functions instead of inlining bodies.
/// Transforms a call like `f(10)` where `f` could be one of several closures into:
/// ```roc
/// match f {
///     Closure_add1_1(captures) => closure_add1_1(10, captures),
///     Closure_mul2_2({}) => closure_mul2_2(10),
/// }
/// ```
fn generateLambdaSetDispatchMatch(
    self: *Self,
    closure_var_expr: Expr.Idx,
    lambda_set: *const LambdaSet,
    call_args: []const Expr.Idx,
) !Expr.Idx {
    // All unspecialized closures must be resolved before generating dispatch.
    // Unspecialized closures (ability-polymorphic entries) are resolved during
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
            // Ability member reference - this is an unspecialized closure.
            // We record it as an unspecialized entry that will be resolved
            // during monomorphization when the concrete type is known.
            //
            // For example, `hash` from `Hash has hash : a -> U64 | a has Hash`
            // becomes an unspecialized closure until we know what type `a` is.
            const member_info = AbilityMemberInfo{
                .method_name = tvd.method_name,
                .type_var_alias_stmt = tvd.type_var_alias_stmt,
                .args = tvd.args,
            };

            // Create a lambda set with this unspecialized closure
            var lambda_set = LambdaSet.init();
            const unspec = self.createUnspecializedClosure(member_info, expr_idx);
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
                    .s_decl_gen => |decl| {
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
                        }

                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl_gen = .{
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
        .e_hosted_lambda,
        .e_low_level_lambda,
        => return expr_idx,

        .e_type_var_dispatch => |tvd| {
            // Transform arguments of the ability member dispatch
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
                .e_return = .{ .expr = new_expr },
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

    try testing.expectEqualStrings("Closure_addX_1", tag_str);
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

    try testing.expectEqualStrings("Closure_1", tag_str);
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

test "ClosureTransformer: external lambda set requests" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    // Create test identifiers
    const ability_member = try module_env.insertIdent(base.Ident.for_text("hash"));

    // Create a dummy unspecialized closure
    const unspec = UnspecializedClosure{
        .member = ability_member,
        .member_expr = @enumFromInt(1),
        .region = 0,
    };

    // Request an external lambda set
    // source_module is only used for equality comparison in tests, never dereferenced
    const source_module: CIR.Import.Idx = undefined;
    const concrete_type_info = ConcreteTypeInfo{
        .type_ident = null,
        .type_args_hash = 12345,
    };

    const is_new = try transformer.requestExternalLambdaSet(
        source_module,
        ability_member,
        concrete_type_info,
        unspec,
    );
    try testing.expect(is_new);

    // Get the pending requests
    const requests = transformer.getExternalLambdaSetRequests();
    try testing.expectEqual(@as(usize, 1), requests.len);
    try testing.expect(requests[0].ability_member == ability_member);

    // Duplicate request should return false
    const is_new2 = try transformer.requestExternalLambdaSet(
        source_module,
        ability_member,
        concrete_type_info,
        unspec,
    );
    try testing.expect(!is_new2);
}

test "ClosureTransformer: export closure" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = Self.init(allocator, module_env);
    defer transformer.deinit();

    // Create test identifiers
    const closure_name = try module_env.insertIdent(base.Ident.for_text("Closure_hash_1"));
    const ability = try module_env.insertIdent(base.Ident.for_text("Hash"));
    const member = try module_env.insertIdent(base.Ident.for_text("hash"));
    const for_type = try module_env.insertIdent(base.Ident.for_text("List"));

    // Export a closure
    try transformer.exportClosure(
        closure_name,
        null,
        ExportedClosureInfo.AbilityImpl{
            .ability = ability,
            .member = member,
            .for_type = for_type,
        },
    );

    // Check exported closures
    const exported = transformer.getExportedClosures();
    try testing.expectEqual(@as(usize, 1), exported.len);
    try testing.expect(exported[0].closure_name == closure_name);

    // Find by ability
    const found = transformer.findExportedClosureByAbility(ability, member, for_type);
    try testing.expect(found != null);
    try testing.expect(found.?.closure_name == closure_name);
}

test "LambdaSet: isEmpty" {
    const allocator = testing.allocator;

    var lambda_set = LambdaSet.init();
    defer lambda_set.deinit(allocator);

    // Empty lambda set
    try testing.expect(isLambdaSetEmpty(&lambda_set));

    // Add an unspecialized closure
    const unspec = UnspecializedClosure{
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
