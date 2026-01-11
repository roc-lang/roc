//! Exhaustiveness and redundancy checking for pattern matching.
//!
//! This module implements Maranget's algorithm from "Warnings for Pattern Matching" (2007)
//! to detect:
//! - Non-exhaustive match expressions (missing cases)
//! - Redundant patterns (unreachable branches)
//! - Unmatchable patterns (patterns on uninhabited types)
//!
//! ## Architecture
//!
//! The implementation uses a two-phase approach:
//!
//! 1. **Pattern Conversion**: CIR patterns are converted to an intermediate representation
//!    (`UnresolvedPattern`) that captures the pattern structure but may not yet know the
//!    full union type (e.g., we see tag name "Ok" but don't know all alternatives yet).
//!
//! 2. **Type Resolution & Checking**: During checking, patterns are resolved on-demand
//!    with full type information using `checkExhaustiveSketched` and `isUsefulSketched`.
//!
//! The algorithm matches record patterns by field name (not positionally) and correctly
//! handles uninhabited types, open unions, and extension chains.
//!
//! ## References
//!
//! - [Warnings for Pattern Matching](http://moscova.inria.fr/~maranget/papers/warn/warn.pdf)
//! - Original Rust implementation in `crates/compiler/exhaustive/`

const std = @import("std");
const base = @import("base");
const Can = @import("can");
const types = @import("types");

const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const TypeStore = types.Store;
const Var = types.Var;

/// Builtin type identifiers needed for special-casing in exhaustiveness checking.
/// These types have special backing representations that would incorrectly appear uninhabited.
pub const BuiltinIdents = struct {
    /// The Builtin module identifier
    builtin_module: Ident.Idx,
    /// Numeric type identifiers (Builtin.Num.*)
    u8_type: Ident.Idx,
    i8_type: Ident.Idx,
    u16_type: Ident.Idx,
    i16_type: Ident.Idx,
    u32_type: Ident.Idx,
    i32_type: Ident.Idx,
    u64_type: Ident.Idx,
    i64_type: Ident.Idx,
    u128_type: Ident.Idx,
    i128_type: Ident.Idx,
    f32_type: Ident.Idx,
    f64_type: Ident.Idx,
    dec_type: Ident.Idx,

    /// Check if a nominal type is a builtin numeric type.
    /// Numeric types have [] as backing but are inhabited primitives.
    pub fn isBuiltinNumericType(self: BuiltinIdents, nominal: types.NominalType) bool {
        // First check if it's from the Builtin module
        if (nominal.origin_module != self.builtin_module) {
            return false;
        }
        // Then check if it's one of the numeric types
        const ident = nominal.ident.ident_idx;
        return ident == self.u8_type or
            ident == self.i8_type or
            ident == self.u16_type or
            ident == self.i16_type or
            ident == self.u32_type or
            ident == self.i32_type or
            ident == self.u64_type or
            ident == self.i64_type or
            ident == self.u128_type or
            ident == self.i128_type or
            ident == self.f32_type or
            ident == self.f64_type or
            ident == self.dec_type;
    }
};

/// 1-based index for user-facing error messages.
/// Provides ordinal formatting like "1st", "2nd", "3rd", etc.
pub const HumanIndex = struct {
    value: u32, // 0-based internally

    pub fn fromZeroBased(index: u32) HumanIndex {
        return .{ .value = index };
    }

    /// Returns the 1-based index number
    pub fn toHuman(self: HumanIndex) u32 {
        return self.value + 1;
    }

    /// Returns ordinal string: "1st", "2nd", "3rd", "4th", etc.
    pub fn ordinal(self: HumanIndex, allocator: std.mem.Allocator) ![]const u8 {
        const n = self.toHuman();
        const suffix = switch (n % 100) {
            11, 12, 13 => "th",
            else => switch (n % 10) {
                1 => "st",
                2 => "nd",
                3 => "rd",
                else => "th",
            },
        };
        return std.fmt.allocPrint(allocator, "{d}{s}", .{ n, suffix });
    }
};

/// A pattern for exhaustiveness checking.
/// This is a simplified representation focused only on what matters for coverage analysis.
pub const Pattern = union(enum) {
    /// Matches anything (wildcard, identifier binding).
    /// When generated as a "missing pattern", carries the type for inhabitedness checking.
    anything: ?Var,
    /// Matches a specific literal value
    literal: Literal,
    /// Matches a constructor (tag) with nested patterns for arguments
    ctor: Ctor,
    /// Matches a list with specific arity
    list: List,

    pub const Ctor = struct {
        /// The union this constructor belongs to
        union_info: Union,
        /// Which tag in the union this is
        tag_id: TagId,
        /// Patterns for constructor arguments
        args: []const Pattern,
    };

    pub const List = struct {
        arity: ListArity,
        /// Patterns for list elements
        elements: []const Pattern,
    };

    /// Deep-copy to a new allocator
    pub fn dupe(self: Pattern, allocator: std.mem.Allocator) error{OutOfMemory}!Pattern {
        return switch (self) {
            .anything => |maybe_type| .{ .anything = maybe_type },
            .literal => |lit| .{ .literal = lit },
            .ctor => |c| .{ .ctor = .{
                .union_info = try c.union_info.dupe(allocator),
                .tag_id = c.tag_id,
                .args = try dupePatterns(allocator, c.args),
            } },
            .list => |l| .{ .list = .{
                .arity = l.arity,
                .elements = try dupePatterns(allocator, l.elements),
            } },
        };
    }

    fn dupePatterns(allocator: std.mem.Allocator, patterns: []const Pattern) error{OutOfMemory}![]const Pattern {
        const result = try allocator.alloc(Pattern, patterns.len);
        for (patterns, 0..) |pat, i| {
            result[i] = try pat.dupe(allocator);
        }
        return result;
    }

    /// Check if this pattern can ever match a value (is inhabited).
    /// A pattern is uninhabited if it matches a type with no possible values,
    /// such as an empty tag union or a constructor with uninhabited arguments.
    pub fn isInhabited(self: Pattern, type_store: *TypeStore, builtin_idents: BuiltinIdents) error{OutOfMemory}!bool {
        return switch (self) {
            .anything => |maybe_type| {
                if (maybe_type) |type_var| {
                    // Check if the type is inhabited using our comprehensive check
                    return isTypeInhabited(type_store, builtin_idents, type_var);
                }
                // Wildcards without type info should only occur in intermediate patterns
                // during matrix specialization, which should never be checked for inhabitedness.
                // Missing patterns should always have type info from ColumnTypes.
                unreachable;
            },

            // Literals are always inhabited (match their specific value)
            .literal => true,

            .ctor => |c| {
                // An empty closed union is uninhabited
                if (c.union_info.alternatives.len == 0 and !c.union_info.has_flex_extension) {
                    return false;
                }

                // All arguments must be inhabited for the pattern to be inhabited
                for (c.args) |arg| {
                    if (!try arg.isInhabited(type_store, builtin_idents)) return false;
                }
                return true;
            },

            .list => |l| {
                // All elements must be inhabited
                for (l.elements) |elem| {
                    if (!try elem.isInhabited(type_store, builtin_idents)) return false;
                }
                return true;
            },
        };
    }
};

/// Identifies a tag within a union
pub const TagId = enum(u16) {
    /// The first/only constructor in a single-constructor type (records, tuples, etc.)
    only = 0,
    _,

    pub fn toInt(self: TagId) u16 {
        return @intFromEnum(self);
    }
};

/// Represents all possible constructors for a type
pub const Union = struct {
    /// All possible constructors
    alternatives: []const CtorInfo,
    /// How to render this in error messages
    render_as: RenderAs,
    /// True if the extension is a flex var (type not fully constrained).
    /// This means wildcards shouldn't be marked redundant since more tags might exist.
    has_flex_extension: bool = false,

    /// Deep-copy to a new allocator
    pub fn dupe(self: Union, allocator: std.mem.Allocator) error{OutOfMemory}!Union {
        return .{
            .alternatives = try allocator.dupe(CtorInfo, self.alternatives),
            .render_as = self.render_as,
            .has_flex_extension = self.has_flex_extension,
        };
    }
};

/// Information about a single constructor
pub const CtorInfo = struct {
    name: CtorName,
    tag_id: TagId,
    arity: usize,
};

/// Name of a constructor - either a tag or an opaque type
pub const CtorName = union(enum) {
    tag: Ident.Idx,
    opaque_type: Ident.Idx,
};

/// How to render a union in error messages
pub const RenderAs = union(enum) {
    /// Tag union
    tag,
    /// Opaque type
    opaque_type,
    /// Record with field names in order
    record: []const Ident.Idx,
    /// Tuple
    tuple,
    /// Guard synthetic constructor
    guard,
};

/// The arity of a list pattern
pub const ListArity = union(enum) {
    /// Matches exactly N elements: [a, b, c]
    exact: usize,
    /// Matches N or more elements: [a, b, .., c]
    /// Fields are (prefix_len, suffix_len)
    slice: Slice,

    pub const Slice = struct {
        prefix: usize,
        suffix: usize,
    };

    pub fn minLen(self: ListArity) usize {
        return switch (self) {
            .exact => |n| n,
            .slice => |s| s.prefix + s.suffix,
        };
    }

    /// Does this arity cover all lengths that `other` covers?
    pub fn coversAritiesOf(self: ListArity, other: ListArity) bool {
        return self.coversLength(other.minLen());
    }

    pub fn coversLength(self: ListArity, length: usize) bool {
        return switch (self) {
            .exact => |n| n == length,
            .slice => |s| s.prefix + s.suffix <= length,
        };
    }
};

/// Literal values that can appear in patterns
pub const Literal = union(enum) {
    int: i128,
    uint: u128,
    bit: bool,
    byte: u8,
    float: u64, // stored as bits
    decimal: i128, // stored as i128 (Dec representation)
    str: StringLiteral.Idx,

    pub fn eql(a: Literal, b: Literal) bool {
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        if (tag_a != tag_b) return false;

        return switch (a) {
            .int => |ai| ai == b.int,
            .uint => |au| au == b.uint,
            .bit => |ab| ab == b.bit,
            .byte => |aby| aby == b.byte,
            .float => |af| af == b.float,
            .decimal => |ad| ad == b.decimal,
            // StringLiteral.Store deduplicates strings, so identical strings
            // receive the same index. Direct index comparison is correct.
            .str => |as| as == b.str,
        };
    }
};

/// Severity of exhaustiveness errors
pub const Severity = enum {
    /// Will crash at runtime if reached
    runtime_error,
    /// Suspicious but won't crash
    warning,
};

/// Errors detected during exhaustiveness checking
pub const Error = union(enum) {
    /// Match expression doesn't cover all cases
    incomplete: Incomplete,
    /// A branch can never be reached
    redundant: Redundant,
    /// A pattern can never match (e.g., matching uninhabited type)
    unmatchable: Unmatchable,

    pub fn severity(self: Error) Severity {
        return switch (self) {
            .incomplete => .runtime_error,
            .redundant, .unmatchable => .warning,
        };
    }

    pub fn region(self: Error) Region {
        return switch (self) {
            .incomplete => |e| e.region,
            .redundant => |e| e.branch_region,
            .unmatchable => |e| e.branch_region,
        };
    }

    pub const Incomplete = struct {
        region: Region,
        context: Context,
        missing_patterns: []const Pattern,
    };

    pub const Redundant = struct {
        overall_region: Region,
        branch_region: Region,
        index: HumanIndex,
    };

    pub const Unmatchable = struct {
        overall_region: Region,
        branch_region: Region,
        index: HumanIndex,
    };
};

/// Context where exhaustiveness checking happens
pub const Context = enum {
    /// Pattern in function argument
    bad_arg,
    /// Pattern in destructuring
    bad_destruct,
    /// Pattern in match/when expression
    bad_case,
};

// CIR Pattern Conversion
//
// These types and functions convert CIR patterns to an intermediate representation
// suitable for exhaustiveness checking. At this stage, we may not yet know the full
// union type for tag patterns (that comes during type resolution).

const CIR = Can.CIR;
const NodeStore = Can.CIR.NodeStore;
const CirPattern = Can.CIR.Pattern;

/// A pattern converted from CIR, potentially before full type information is available.
/// Tag patterns store the tag name but may not yet know all alternatives in the union.
pub const UnresolvedPattern = union(enum) {
    /// Matches anything (wildcard, identifier binding)
    anything,
    /// Matches a specific literal value
    literal: Literal,
    /// A constructor whose union type is not yet known (tag name only)
    ctor: struct {
        tag_name: Ident.Idx,
        args: []const UnresolvedPattern,
    },
    /// A constructor whose union type IS known (e.g., records, tuples, guards)
    known_ctor: struct {
        union_info: Union,
        tag_id: TagId,
        args: []const UnresolvedPattern,
    },
    /// Matches a list with specific arity
    list: struct {
        arity: ListArity,
        elements: []const UnresolvedPattern,
    },
};

/// A row in the pattern matrix before type resolution
pub const UnresolvedRow = struct {
    /// The patterns for this row (usually just one, but could be more for multiple scrutinees)
    patterns: []const UnresolvedPattern,
    /// The source region of this pattern
    region: Region,
    /// Whether this branch has a guard condition
    guard: Guard,
    /// Index of the branch this pattern came from (for tracking redundancy)
    branch_index: u32,
};

/// Whether a branch has a guard condition
pub const Guard = enum {
    has_guard,
    no_guard,
};

/// Collection of pattern rows for a match expression
pub const UnresolvedRows = struct {
    /// All rows (one per pattern in the match)
    rows: []const UnresolvedRow,
    /// The overall region of the match expression
    overall_region: Region,
};

/// Convert a CIR pattern to an unresolved pattern for exhaustiveness checking.
///
/// This extracts the structure of the pattern. Tag patterns are left with just
/// their tag name; the full union type will be resolved during type checking.
pub fn convertPattern(
    allocator: std.mem.Allocator,
    store: *const NodeStore,
    pattern_idx: CirPattern.Idx,
) error{OutOfMemory}!UnresolvedPattern {
    const pattern = store.getPattern(pattern_idx);

    return switch (pattern) {
        // Simple binding patterns match anything
        .assign, .underscore => .anything,

        // As patterns: convert the inner pattern
        .as => |p| convertPattern(allocator, store, p.pattern),

        // Tag application: unknown union type, will be resolved later
        .applied_tag => |p| {
            const arg_indices = store.slicePatterns(p.args);
            const args = try allocator.alloc(UnresolvedPattern, arg_indices.len);
            for (arg_indices, 0..) |arg_idx, i| {
                args[i] = try convertPattern(allocator, store, arg_idx);
            }
            return .{ .ctor = .{
                .tag_name = p.name,
                .args = args,
            } };
        },

        // List patterns
        .list => |p| {
            const elem_indices = store.slicePatterns(p.patterns);
            const elements = try allocator.alloc(UnresolvedPattern, elem_indices.len);
            for (elem_indices, 0..) |elem_idx, i| {
                elements[i] = try convertPattern(allocator, store, elem_idx);
            }

            const arity: ListArity = if (p.rest_info) |rest| blk: {
                // Has rest pattern like [a, .., b]
                const prefix_len = rest.index;
                const suffix_len = elem_indices.len - rest.index;
                break :blk .{ .slice = .{
                    .prefix = prefix_len,
                    .suffix = suffix_len,
                } };
            } else .{ .exact = elem_indices.len };

            return .{ .list = .{
                .arity = arity,
                .elements = elements,
            } };
        },

        // Record destructure: single-constructor type
        .record_destructure => |p| {
            const destructs = store.sliceRecordDestructs(p.destructs);
            const args = try allocator.alloc(UnresolvedPattern, destructs.len);
            const field_names = try allocator.alloc(Ident.Idx, destructs.len);

            for (destructs, 0..) |destruct_idx, i| {
                const destruct = store.getRecordDestruct(destruct_idx);
                field_names[i] = destruct.label;
                const sub_pattern_idx = destruct.kind.toPatternIdx();
                args[i] = try convertPattern(allocator, store, sub_pattern_idx);
            }

            const alternatives = try allocator.alloc(CtorInfo, 1);
            alternatives[0] = .{
                .name = .{ .tag = Ident.Idx.NONE },
                .tag_id = .only,
                .arity = destructs.len,
            };

            return .{ .known_ctor = .{
                .union_info = .{
                    .alternatives = alternatives,
                    .render_as = .{ .record = field_names },
                },
                .tag_id = .only,
                .args = args,
            } };
        },

        // Tuple patterns: single-constructor type
        .tuple => |p| {
            const elem_indices = store.slicePatterns(p.patterns);
            const args = try allocator.alloc(UnresolvedPattern, elem_indices.len);
            for (elem_indices, 0..) |elem_idx, i| {
                args[i] = try convertPattern(allocator, store, elem_idx);
            }

            const alternatives = try allocator.alloc(CtorInfo, 1);
            alternatives[0] = .{
                .name = .{ .tag = Ident.Idx.NONE },
                .tag_id = .only,
                .arity = elem_indices.len,
            };

            return .{ .known_ctor = .{
                .union_info = .{
                    .alternatives = alternatives,
                    .render_as = .tuple,
                },
                .tag_id = .only,
                .args = args,
            } };
        },

        // Numeric literals
        .num_literal => |p| {
            switch (p.value.kind) {
                .i128 => return .{ .literal = .{ .int = p.value.toI128() } },
                .u128 => return .{ .literal = .{ .uint = @bitCast(p.value.bytes) } },
            }
        },

        // Decimal literals
        .small_dec_literal => |p| {
            return .{ .literal = .{ .decimal = p.value.toRocDec().num } };
        },
        .dec_literal => |p| {
            return .{ .literal = .{ .decimal = p.value.num } };
        },

        // Float literals
        .frac_f32_literal => |p| {
            return .{ .literal = .{ .float = @bitCast(@as(f64, p.value)) } };
        },
        .frac_f64_literal => |p| {
            return .{ .literal = .{ .float = @bitCast(p.value) } };
        },

        // String literals
        .str_literal => |p| {
            return .{ .literal = .{ .str = p.literal } };
        },

        // Nominal patterns: convert the backing pattern
        .nominal => |p| convertPattern(allocator, store, p.backing_pattern),
        .nominal_external => |p| convertPattern(allocator, store, p.backing_pattern),

        // Runtime errors match anything since we won't reach them
        .runtime_error => .anything,
    };
}

/// Convert all branches of a match expression to unresolved pattern rows.
///
/// Each branch can have multiple patterns (OR patterns like `A | B => ...`),
/// so we create one row per pattern, not per branch.
pub fn convertMatchBranches(
    allocator: std.mem.Allocator,
    store: *const NodeStore,
    branches_span: CIR.Expr.Match.Branch.Span,
    overall_region: Region,
) error{OutOfMemory}!UnresolvedRows {
    const branch_indices = store.matchBranchSlice(branches_span);

    // First pass: count rows and check if any branch has a guard
    var total_patterns: usize = 0;
    var any_has_guard = false;

    for (branch_indices) |branch_idx| {
        const branch = store.getMatchBranch(branch_idx);
        const branch_patterns = store.sliceMatchBranchPatterns(branch.patterns);
        total_patterns += branch_patterns.len;
        if (branch.guard != null) {
            any_has_guard = true;
        }
    }

    // Allocate space for all rows
    const rows = try allocator.alloc(UnresolvedRow, total_patterns);
    var row_idx: usize = 0;

    for (branch_indices, 0..) |branch_idx, branch_i| {
        const branch = store.getMatchBranch(branch_idx);
        const branch_patterns = store.sliceMatchBranchPatterns(branch.patterns);
        const has_guard = branch.guard != null;

        for (branch_patterns) |bp_idx| {
            const bp = store.getMatchBranchPattern(bp_idx);
            const pattern_region = store.getPatternRegion(bp.pattern);

            const converted = try convertPattern(allocator, store, bp.pattern);

            // If any branch has a guard, wrap all patterns in a Guard constructor
            const final_pattern = if (any_has_guard) blk: {
                // Guarded branches match `True`, non-guarded match anything
                const guard_pattern: UnresolvedPattern = if (has_guard)
                    .{ .literal = .{ .bit = true } }
                else
                    .anything;

                const guard_args = try allocator.alloc(UnresolvedPattern, 2);
                guard_args[0] = guard_pattern;
                guard_args[1] = converted;

                const alternatives = try allocator.alloc(CtorInfo, 1);
                alternatives[0] = .{
                    .name = .{ .tag = Ident.Idx.NONE },
                    .tag_id = .only,
                    .arity = 2,
                };

                break :blk UnresolvedPattern{ .known_ctor = .{
                    .union_info = .{
                        .alternatives = alternatives,
                        .render_as = .guard,
                    },
                    .tag_id = .only,
                    .args = guard_args,
                } };
            } else converted;

            const pattern_slice = try allocator.alloc(UnresolvedPattern, 1);
            pattern_slice[0] = final_pattern;

            rows[row_idx] = .{
                .patterns = pattern_slice,
                .region = pattern_region,
                .guard = if (has_guard) .has_guard else .no_guard,
                .branch_index = @intCast(branch_i),
            };
            row_idx += 1;
        }
    }

    return .{
        .rows = rows,
        .overall_region = overall_region,
    };
}

// Pattern Reification
//
// These functions resolve unresolved patterns to concrete patterns using type information.
// In the 1-phase design, reification happens on-demand during usefulness checking,
// and type errors are propagated immediately rather than silently skipped.

/// Errors that can occur during pattern reification.
/// These indicate type mismatches that prevent exhaustiveness checking.
pub const ReifyError = error{
    OutOfMemory,
    /// Type couldn't be resolved (e.g., polymorphic type with unknown structure)
    TypeError,
};

// Helper types and functions for pattern resolution

const UnionResult = union(enum) {
    success: Union,
    not_a_union,
};

/// Extract union information from a type variable.
/// Filters out uninhabited constructors at construction time.
fn getUnionFromType(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
) error{OutOfMemory}!UnionResult {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    // Try to unwrap as a tag union
    if (content.unwrapTagUnion()) |tag_union| {
        return try buildUnionFromTagUnion(allocator, type_store, tag_union);
    }

    // Try to follow aliases and other type wrappers
    switch (content) {
        .alias => |alias| {
            const backing_var = type_store.getAliasBackingVar(alias);
            return getUnionFromType(allocator, type_store, builtin_idents, backing_var);
        },
        // Polymorphic types (flex/rigid vars) cannot be treated as unions because
        // we don't know what constructors they have. This is correct behavior -
        // the caller should handle this by skipping exhaustiveness checking.
        .flex, .rigid => return .not_a_union,
        // Structure might contain tag union or nominal type info
        .structure => |flat_type| {
            switch (flat_type) {
                .tag_union => |tag_union| {
                    return try buildUnionFromTagUnion(allocator, type_store, tag_union);
                },
                // Nominal types (like Try, Result) are user-defined types that wrap other types
                // We need to unwrap them to find the underlying tag union
                .nominal_type => |nominal| {
                    const backing_var = type_store.getNominalBackingVar(nominal);
                    return getUnionFromType(allocator, type_store, builtin_idents, backing_var);
                },
                else => return .not_a_union,
            }
        },
        else => {},
    }

    // Not a tag union
    return .not_a_union;
}

/// Build a Union structure from a TagUnion type.
/// Includes all constructors so patterns can be matched.
/// Inhabitedness checking is done separately via isSketchedPatternInhabited.
///
/// IMPORTANT: This function follows extension chains to gather ALL tags.
/// Tag unions from unification may have tags split across the main union
/// and its extension chain (e.g., [Normal, ..ext] where ext = [HasEmpty, ..]).
fn buildUnionFromTagUnion(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    tag_union: types.TagUnion,
) error{OutOfMemory}!UnionResult {
    // Gather all tags by following the extension chain
    var all_tags: std.ArrayList(GatheredTag) = .empty;
    defer all_tags.deinit(allocator);

    var is_open = false;
    var has_flex = false;

    // Start with the initial tag union
    var current_tags = tag_union.tags;
    var current_ext = tag_union.ext;

    // Track seen extension variables to detect cycles
    var seen_exts = std.AutoHashMap(Var, void).init(allocator);
    defer seen_exts.deinit();

    // Follow extension chain to collect all tags
    while (true) {
        // Add tags from current level
        const tags_slice = type_store.getTagsSlice(current_tags);
        const tag_names = tags_slice.items(.name);
        const tag_args = tags_slice.items(.args);

        for (tag_names, tag_args) |name, args_range| {
            try all_tags.append(allocator, .{ .name = name, .args = args_range });
        }

        // Resolve the extension variable
        const ext_resolved = type_store.resolveVar(current_ext);
        const ext_var = ext_resolved.var_;

        // Cycle detection: have we seen this variable before?
        const gop = try seen_exts.getOrPut(ext_var);
        if (gop.found_existing) {
            // Cycle detected - treat as closed union and stop
            // This shouldn't happen in well-formed types, but prevents infinite loops
            break;
        }

        // Check what the extension is
        const ext_content = ext_resolved.desc.content;

        switch (ext_content) {
            .flex => {
                // Flex extension = open union, stop here
                is_open = true;
                has_flex = true;
                break;
            },
            .rigid => {
                // Rigid extension = open union (for exhaustiveness), stop here
                is_open = true;
                has_flex = false;
                break;
            },
            .structure => |flat_type| {
                switch (flat_type) {
                    .tag_union => |ext_tu| {
                        // Extension is another tag union - continue following
                        current_tags = ext_tu.tags;
                        current_ext = ext_tu.ext;
                    },
                    .empty_tag_union => {
                        // Closed union - stop here
                        is_open = false;
                        has_flex = false;
                        break;
                    },
                    else => {
                        // Other structure types = closed for our purposes
                        is_open = false;
                        has_flex = false;
                        break;
                    },
                }
            },
            .alias => |alias| {
                // Follow alias to its backing var
                current_ext = type_store.getAliasBackingVar(alias);
                // Don't break - continue with the resolved alias
            },
            else => {
                // Other content types = treat as closed
                is_open = false;
                has_flex = false;
                break;
            },
        }
    }

    // Allocate alternatives (add one extra for open unions)
    const num_alts = all_tags.items.len + @as(usize, if (is_open) 1 else 0);
    const alternatives = try allocator.alloc(CtorInfo, num_alts);

    for (all_tags.items, 0..) |tag, i| {
        const arg_vars = type_store.sliceVars(tag.args);
        alternatives[i] = .{
            .name = .{ .tag = tag.name },
            .tag_id = @enumFromInt(i),
            .arity = arg_vars.len,
        };
    }

    // Add synthetic #Open constructor for open unions
    if (is_open) {
        alternatives[all_tags.items.len] = .{
            .name = .{ .tag = Ident.Idx.NONE }, // Represents "#Open"
            .tag_id = @enumFromInt(all_tags.items.len),
            .arity = 0,
        };
    }

    return .{ .success = .{
        .alternatives = alternatives,
        .render_as = .tag,
        .has_flex_extension = has_flex,
    } };
}

/// A tag gathered during extension chain traversal
const GatheredTag = struct {
    name: Ident.Idx,
    args: types.Var.SafeList.Range,
};

// Inhabitedness Checking
//
// A type is "inhabited" if it has at least one possible value.
// This is critical for exhaustiveness checking: patterns on uninhabited types
// should not contribute to coverage, and uninhabited constructors should not
// require matching.
//
// Core API:
// - `isTypeInhabited`: Check if a single type is inhabited (THE primary entry point)
// - `areAllTypesInhabited`: Check if all types in a slice are inhabited (AND semantics)
//
// Pattern.isInhabited() delegates to isTypeInhabited for wildcard patterns.
//
// Based on the algorithm from the Rust implementation in crates/compiler/types/src/subs.rs.

/// Work item for the work-list based inhabitedness algorithm.
/// This enables purely iterative checking without recursion, preventing
/// stack overflow on deeply nested types.
const WorkItem = union(enum) {
    /// Check if this type is inhabited, push result onto results stack
    check_type: Var,

    /// Pop N results, AND them together, push combined result.
    /// All must be true for result to be true.
    /// count=0 pushes true (empty AND is vacuously true).
    and_combine: u32,

    /// Pop N results, OR them together, push combined result.
    /// Any must be true for result to be true.
    /// count=0 pushes false (empty OR has no witnesses).
    or_combine: u32,

    /// Pop result; if false and extension is open (flex/rigid), push true; else push original.
    /// The Var is the extension variable to check.
    check_open_extension: Var,
};

/// Check if a type is inhabited (has at least one possible value).
///
/// A type is uninhabited if:
/// - It's an empty tag union with no flex extension
/// - It's a tag union where ALL variants have at least one uninhabited argument
/// - It's a record/tuple with any uninhabited field
/// - It's a nominal type whose backing type is uninhabited
///
/// A type is inhabited if:
/// - It's a flex/rigid variable (unconstrained, could be anything)
/// - It's a recursion var (these only appear in valid recursive types)
/// - It's a builtin primitive type (Builtin.Num.*, Builtin.Str, etc.)
/// - It's a function type
/// - It has at least one constructor with all inhabited arguments
///
/// This implementation uses a work-list algorithm to avoid stack overflow on
/// deeply nested types. See docs/exhaustiveness/004_worklist_inhabitedness_algorithm.md
fn isTypeInhabited(type_store: *TypeStore, builtin_idents: BuiltinIdents, type_var: Var) error{OutOfMemory}!bool {
    // Use a seen set to detect cycles in recursive types
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(type_store.gpa);

    const gpa = type_store.gpa;

    // Work-list of items to process (LIFO order)
    var work_list: std.ArrayList(WorkItem) = .empty;
    defer work_list.deinit(gpa);

    // Stack of boolean results from completed checks
    var results: std.ArrayList(bool) = .empty;
    defer results.deinit(gpa);

    // Start with the initial type
    try work_list.append(gpa, .{ .check_type = type_var });

    while (work_list.pop()) |item| {
        switch (item) {
            .check_type => |var_to_check| {
                const resolved = type_store.resolveVar(var_to_check);
                const resolved_var = resolved.var_;
                const content = resolved.desc.content;

                // Cycle detection: if we've seen this resolved variable before,
                // treat it as inhabited. Cycles in recursive types are considered
                // inhabited (if we got here, there must be a non-recursive path).
                const gop = try seen.getOrPut(gpa, resolved_var);
                if (gop.found_existing) {
                    try results.append(gpa, true);
                    continue;
                }

                switch (content) {
                    // Flex and rigid variables are unconstrained - assume inhabited
                    .flex, .rigid => try results.append(gpa, true),

                    // Error types are treated as inhabited (we don't want to cascade errors)
                    .err => try results.append(gpa, true),

                    // Aliases - check the backing type
                    .alias => |alias| {
                        const backing_var = type_store.getAliasBackingVar(alias);
                        try work_list.append(gpa, .{ .check_type = backing_var });
                    },

                    .structure => |flat_type| switch (flat_type) {
                        // Empty tag union is uninhabited
                        .empty_tag_union => try results.append(gpa, false),

                        // Empty record is inhabited (the unit type)
                        .empty_record => try results.append(gpa, true),

                        // Tag unions: need OR semantics across tags, AND semantics within each tag's args
                        .tag_union => |tag_union| {
                            try pushTagUnionWork(gpa, type_store, &work_list, tag_union);
                        },

                        // Nominal types - check for builtin primitives which have special backing.
                        .nominal_type => |nominal| {
                            // Check if this is a builtin number type (Builtin.Num.*)
                            // These have [] as backing type but are inhabited primitives.
                            if (builtin_idents.isBuiltinNumericType(nominal)) {
                                try results.append(gpa, true);
                            } else {
                                // For other nominal types, check the backing var
                                const backing_var = type_store.getNominalBackingVar(nominal);
                                try work_list.append(gpa, .{ .check_type = backing_var });
                            }
                        },

                        // Records - all fields must be inhabited (AND semantics)
                        .record => |record| {
                            const fields_slice = type_store.getRecordFieldsSlice(record.fields);
                            const field_vars = fields_slice.items(.var_);
                            try pushAndWork(gpa, &work_list, field_vars);
                        },

                        .record_unbound => |fields| {
                            const fields_slice = type_store.getRecordFieldsSlice(fields);
                            const field_vars = fields_slice.items(.var_);
                            try pushAndWork(gpa, &work_list, field_vars);
                        },

                        // Tuples - all elements must be inhabited (AND semantics)
                        .tuple => |tuple| {
                            const elem_vars = type_store.sliceVars(tuple.elems);
                            try pushAndWork(gpa, &work_list, elem_vars);
                        },

                        // Functions are always inhabited (they're values)
                        .fn_pure, .fn_effectful, .fn_unbound => try results.append(gpa, true),
                    },
                }
            },

            .and_combine => |count| {
                // Pop N results, AND them together
                // Empty AND is vacuously true
                var combined: bool = true;
                for (0..count) |_| {
                    if (!(results.pop() orelse true)) {
                        combined = false;
                    }
                }
                try results.append(gpa, combined);
            },

            .or_combine => |count| {
                // Pop N results, OR them together
                // Empty OR has no witnesses (false)
                var combined: bool = false;
                for (0..count) |_| {
                    if (results.pop() orelse false) {
                        combined = true;
                    }
                }
                try results.append(gpa, combined);
            },

            .check_open_extension => |ext_var| {
                // Pop the current result; if false and extension is open, the union is still inhabited
                const current = results.pop() orelse false;
                if (current) {
                    // Already inhabited, no need to check extension
                    try results.append(gpa, true);
                } else {
                    // Check if extension is open (flex/rigid)
                    const is_open = try isExtensionOpen(type_store, ext_var);
                    try results.append(gpa, is_open);
                }
            },
        }
    }

    // Final result should be on top of the results stack
    return if (results.items.len > 0) results.items[0] else true;
}

/// Push work items for AND semantics: all types must be inhabited.
/// Pushes AndCombine(N) followed by CheckType for each var.
fn pushAndWork(gpa: std.mem.Allocator, work_list: *std.ArrayList(WorkItem), vars: []const Var) !void {
    const count: u32 = @intCast(vars.len);
    if (count == 0) {
        // Empty AND is true - push result directly
        try work_list.append(gpa, .{ .and_combine = 0 });
    } else {
        // Push combine instruction first (will be processed last due to LIFO)
        try work_list.append(gpa, .{ .and_combine = count });
        // Push all type checks (will be processed first)
        for (vars) |v| {
            try work_list.append(gpa, .{ .check_type = v });
        }
    }
}

/// Push work items for a tag union: OR semantics across tags, AND within each tag's args.
/// Also handles extension chain following.
fn pushTagUnionWork(gpa: std.mem.Allocator, type_store: *TypeStore, work_list: *std.ArrayList(WorkItem), initial_tag_union: types.TagUnion) !void {
    // First, collect all tags by following the extension chain
    var all_tag_args: std.ArrayList(Var.SafeList.Range) = .empty;
    defer all_tag_args.deinit(gpa);

    var final_ext = initial_tag_union.ext;

    // Track seen extension variables to detect cycles
    var seen_exts: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen_exts.deinit(gpa);

    var current_tags = initial_tag_union.tags;
    var current_ext = initial_tag_union.ext;

    // Follow extension chain to collect all tags
    while (true) {
        const tags_slice = type_store.getTagsSlice(current_tags);
        const tag_args = tags_slice.items(.args);

        // Add all tags at this level
        for (tag_args) |args_range| {
            try all_tag_args.append(gpa, args_range);
        }

        // Check the extension
        const ext_resolved = type_store.resolveVar(current_ext);
        const ext_var = ext_resolved.var_;
        final_ext = current_ext;

        // Cycle detection for extension chain
        const gop = try seen_exts.getOrPut(gpa, ext_var);
        if (gop.found_existing) {
            break;
        }

        const ext_content = ext_resolved.desc.content;

        switch (ext_content) {
            .flex, .rigid => {
                // Open extension - we'll handle this in check_open_extension
                break;
            },
            .structure => |flat_type| switch (flat_type) {
                .tag_union => |ext_tu| {
                    // Continue following extension chain
                    current_tags = ext_tu.tags;
                    current_ext = ext_tu.ext;
                },
                .empty_tag_union => {
                    // Closed union - no more tags
                    break;
                },
                else => break,
            },
            .alias => |alias| {
                // Follow alias
                current_ext = type_store.getAliasBackingVar(alias);
            },
            else => break,
        }
    }

    const num_tags: u32 = @intCast(all_tag_args.items.len);

    if (num_tags == 0) {
        // No tags - result depends only on whether extension is open
        // Push false, then check_open_extension will override if extension is open
        try work_list.append(gpa, .{ .check_open_extension = final_ext });
        try work_list.append(gpa, .{ .or_combine = 0 }); // Empty OR = false
    } else {
        // Push items in reverse order (LIFO):
        // 1. check_open_extension (processed last)
        // 2. or_combine (processed after all tags)
        // 3. For each tag: and_combine + check_type for each arg (processed first)
        try work_list.append(gpa, .{ .check_open_extension = final_ext });
        try work_list.append(gpa, .{ .or_combine = num_tags });

        // Push work for each tag (AND semantics for args within each tag)
        for (all_tag_args.items) |args_range| {
            const arg_vars = type_store.sliceVars(args_range);
            const arg_count: u32 = @intCast(arg_vars.len);

            // Each tag contributes one result (AND of its args)
            try work_list.append(gpa, .{ .and_combine = arg_count });
            for (arg_vars) |arg_var| {
                try work_list.append(gpa, .{ .check_type = arg_var });
            }
        }
    }
}

/// Check if an extension variable represents an open extension (flex or rigid).
/// Uses cycle detection to safely traverse extension chains.
fn isExtensionOpen(type_store: *TypeStore, ext_var: Var) error{OutOfMemory}!bool {
    const gpa = type_store.gpa;

    // Track seen extension variables to detect cycles
    var seen_exts: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen_exts.deinit(gpa);

    // Follow the extension chain to find the actual extension type
    var current_ext = ext_var;

    while (true) {
        const ext_resolved = type_store.resolveVar(current_ext);
        const resolved_var = ext_resolved.var_;

        // Cycle detection: have we seen this variable before?
        const gop = try seen_exts.getOrPut(gpa, resolved_var);
        if (gop.found_existing) {
            // Cycle detected - treat as closed to be safe
            return false;
        }

        const ext_content = ext_resolved.desc.content;

        switch (ext_content) {
            .flex, .rigid => return true,
            .structure => |flat_type| switch (flat_type) {
                .tag_union => |ext_tu| {
                    current_ext = ext_tu.ext;
                },
                .empty_tag_union => return false,
                else => return false,
            },
            .alias => |alias| {
                current_ext = type_store.getAliasBackingVar(alias);
            },
            else => return false,
        }
    }
}

/// Check if all types in a slice are inhabited.
/// Returns false if ANY type is uninhabited (AND semantics).
fn areAllTypesInhabited(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_vars: []const Var,
) error{OutOfMemory}!bool {
    for (type_vars) |type_var| {
        if (!try isTypeInhabited(type_store, builtin_idents, type_var)) {
            return false;
        }
    }
    return true;
}

/// Check if an UnresolvedPattern (sketched pattern) is inhabited.
/// This requires type information to resolve the pattern's constructor types.
///
/// A sketched pattern is uninhabited if:
/// - It's a constructor whose argument types are uninhabited
/// - It's a wildcard matching an uninhabited type
fn isSketchedPatternInhabited(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    patterns: []const UnresolvedPattern,
    column_types: ColumnTypes,
) ReifyError!bool {
    if (patterns.len == 0) return true;
    if (column_types.types.len == 0) return true;

    const first = patterns[0];
    const first_col_type = column_types.types[0];

    switch (first) {
        .ctor => |c| {
            // Look up the union type to get tag_id and argument types
            const union_result = try getUnionFromType(allocator, type_store, builtin_idents, first_col_type);
            const union_info = switch (union_result) {
                .success => |u| u,
                .not_a_union => return error.TypeError,
            };
            const tag_id = findTagId(union_info, c.tag_name) orelse return error.TypeError;

            // Get the constructor's argument types
            const arg_types = getCtorArgTypes(allocator, type_store, first_col_type, tag_id);

            // Check if any argument type is uninhabited
            for (arg_types, 0..) |arg_type, i| {
                if (!try isTypeInhabited(type_store, builtin_idents, arg_type)) {
                    return false; // Uninhabited argument = uninhabited pattern
                }
                // Also recursively check nested patterns
                if (i < c.args.len) {
                    const arg_col_types = try allocator.alloc(Var, 1);
                    arg_col_types[0] = arg_type;
                    const nested_patterns = try allocator.alloc(UnresolvedPattern, 1);
                    nested_patterns[0] = c.args[i];
                    if (!try isSketchedPatternInhabited(allocator, type_store, builtin_idents, nested_patterns, .{
                        .types = arg_col_types,
                        .type_store = type_store,
                        .builtin_idents = builtin_idents,
                    })) {
                        return false;
                    }
                }
            }
            return true;
        },
        // known_ctor is for records - records are always inhabited (unless they have uninhabited fields,
        // but that's checked via their field types, not the pattern structure)
        .known_ctor => return true,
        .anything => {
            // Wildcard - check if the type itself is uninhabited
            return isTypeInhabited(type_store, builtin_idents, first_col_type);
        },
        .literal => return true, // Literals are always inhabited
        .list => |l| {
            // Empty list pattern is always inhabited
            // Non-empty list patterns are uninhabited if the element type is uninhabited
            const min_len = l.arity.minLen();
            if (min_len == 0) return true; // Empty list is always inhabited

            // Check if element type is inhabited
            const elem_type = getListElemType(type_store, first_col_type) orelse return true;
            return isTypeInhabited(type_store, builtin_idents, elem_type);
        },
    }
}

/// Check if an extension variable represents an open union.
///
/// An open union is one where additional constructors may exist beyond those
/// explicitly listed. This occurs when the extension is:
/// - A flex var: The type is not yet fully constrained, more tags could be added
/// - A rigid var: The user explicitly said "and potentially more tags"
/// - A nested tag union: More tags exist in the extension
///
/// Open unions require a wildcard pattern or explicit `#Open` constructor to be exhaustive.
fn isOpenExtension(type_store: *TypeStore, ext: Var) bool {
    const resolved = type_store.resolveVar(ext);
    const content = resolved.desc.content;

    return switch (content) {
        // Both flex and rigid extensions mean the union is open:
        // - Flex: type not fully constrained, could unify with more tags
        // - Rigid: user explicitly marked it as open (e.g., [A, B]a)
        .flex, .rigid => true,
        // Empty tag union means it's closed - no additional tags possible
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union => false,
            // A tag union extension (nested tags) means more tags exist
            .tag_union => true,
            else => false,
        },
        // Recursion vars, aliases - resolve further
        .alias => |alias| {
            const backing = type_store.getAliasBackingVar(alias);
            return isOpenExtension(type_store, backing);
        },
        .recursion_var => |rec| {
            return isOpenExtension(type_store, rec.structure);
        },
        else => false,
    };
}

/// Find the tag_id for a tag name within a union.
/// Compares only the string index, not attributes, since the same tag name
/// from a pattern vs a type definition may have different attributes.
fn findTagId(union_info: Union, tag_name: Ident.Idx) ?TagId {
    for (union_info.alternatives) |alt| {
        const alt_ident = switch (alt.name) {
            .tag => |t| if (t == Ident.Idx.NONE) continue else t,
            .opaque_type => |o| o,
        };
        // Compare just the idx (interned string index), not the full Ident.Idx
        // which includes attributes that may differ between pattern and type
        if (alt_ident.idx == tag_name.idx) {
            // Return the stored tag_id, not the array position.
            // The tag_id preserves the original index for getCtorArgTypes.
            return alt.tag_id;
        }
    }
    return null;
}

/// Collect unique type parameter variables from a backing type structure.
/// Type parameters are flex or rigid vars that appear in the type.
/// Returns them in order of first encounter (declaration order for well-formed types).
///
/// This is used for type parameter substitution in nominal types:
/// given `Try a e : [Ok(a), Err(e)]` and `Try(I64, Str)`, we need to know
/// that `a` is the first parameter and `e` is the second.
fn collectTypeParamsFromBackingType(
    type_store: *TypeStore,
    backing_var: Var,
) error{OutOfMemory}![]const Var {
    const gpa = type_store.gpa;

    var params: std.ArrayList(Var) = .empty;
    errdefer params.deinit(gpa);

    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(gpa);

    var stack: std.ArrayList(Var) = .empty;
    defer stack.deinit(gpa);

    try stack.append(gpa, backing_var);

    while (stack.pop()) |var_| {
        const resolved = type_store.resolveVar(var_);
        const root_var = resolved.var_;

        // Skip if already seen
        const gop = try seen.getOrPut(gpa, root_var);
        if (gop.found_existing) continue;

        switch (resolved.desc.content) {
            .flex, .rigid => {
                // This is a type parameter - add to our list
                try params.append(gpa, root_var);
            },
            .structure => |flat_type| {
                switch (flat_type) {
                    .tag_union => |tu| {
                        // Add tag args in order (first tag's args, then second tag's args, etc.)
                        const tags_slice = type_store.getTagsSlice(tu.tags);
                        const all_args = tags_slice.items(.args);

                        // Process tags in reverse order so they come out in correct order from stack
                        var tag_idx = all_args.len;
                        while (tag_idx > 0) {
                            tag_idx -= 1;
                            const args = type_store.sliceVars(all_args[tag_idx]);
                            // Process args in reverse order
                            var arg_idx = args.len;
                            while (arg_idx > 0) {
                                arg_idx -= 1;
                                try stack.append(gpa, args[arg_idx]);
                            }
                        }
                        // Extension last
                        try stack.append(gpa, tu.ext);
                    },
                    .tuple => |tuple| {
                        const elems = type_store.sliceVars(tuple.elems);
                        var i = elems.len;
                        while (i > 0) {
                            i -= 1;
                            try stack.append(gpa, elems[i]);
                        }
                    },
                    .record => |record| {
                        const fields = getRecordFieldTypes(type_store, record.fields);
                        var i = fields.len;
                        while (i > 0) {
                            i -= 1;
                            try stack.append(gpa, fields[i]);
                        }
                    },
                    .record_unbound => |fields| {
                        const field_types = getRecordFieldTypes(type_store, fields);
                        var i = field_types.len;
                        while (i > 0) {
                            i -= 1;
                            try stack.append(gpa, field_types[i]);
                        }
                    },
                    .fn_pure, .fn_effectful, .fn_unbound => |func| {
                        // Add return first, then args in order
                        const args = type_store.sliceVars(func.args);
                        var i = args.len;
                        while (i > 0) {
                            i -= 1;
                            try stack.append(gpa, args[i]);
                        }
                        try stack.append(gpa, func.ret);
                    },
                    .nominal_type => |nominal| {
                        // For nested nominal types, traverse the backing type
                        try stack.append(gpa, type_store.getNominalBackingVar(nominal));
                    },
                    .empty_record, .empty_tag_union => {},
                }
            },
            .alias => |alias| {
                try stack.append(gpa, type_store.getAliasBackingVar(alias));
            },
            else => {},
        }
    }

    return try params.toOwnedSlice(gpa);
}

/// Get the argument types for a constructor.
/// For nominal types with type arguments (like Try(A, B)), we need to return
/// the actual type arguments, not the backing type's unsubstituted type params.
///
/// IMPORTANT: This function follows extension chains to find the tag at the given index.
/// Tag unions from unification may have tags split across the main union
/// and its extension chain (e.g., [Normal, ..ext] where ext = [HasEmpty, ..]).
fn getCtorArgTypes(allocator: std.mem.Allocator, type_store: *TypeStore, type_var: Var, tag_id: TagId) []const Var {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    if (content.unwrapTagUnion()) |tag_union| {
        // Follow extension chain to find the tag at the given index
        var current_tags = tag_union.tags;
        var current_ext = tag_union.ext;
        var current_offset: usize = 0;
        const target_idx = @intFromEnum(tag_id);

        // Track seen extension variables to detect cycles
        var seen_exts = std.AutoHashMap(Var, void).init(type_store.gpa);
        defer seen_exts.deinit();

        while (true) {
            const tags_slice = type_store.getTagsSlice(current_tags);
            const tag_args = tags_slice.items(.args);

            // Check if the target index is in this level
            if (target_idx < current_offset + tag_args.len) {
                const local_idx = target_idx - current_offset;
                return type_store.sliceVars(tag_args[local_idx]);
            }

            // Move to the extension
            current_offset += tag_args.len;
            const ext_resolved = type_store.resolveVar(current_ext);
            const ext_var = ext_resolved.var_;

            // Cycle detection: have we seen this variable before?
            const gop = seen_exts.getOrPut(ext_var) catch {
                // OOM during cycle detection - return empty to avoid crash.
                // This is conservative: caller will see wrong arity but won't crash.
                return &[_]Var{};
            };
            if (gop.found_existing) {
                // Cycle detected - tag not found
                break;
            }

            const ext_content = ext_resolved.desc.content;

            switch (ext_content) {
                .structure => |flat_type| switch (flat_type) {
                    .tag_union => |ext_tu| {
                        current_tags = ext_tu.tags;
                        current_ext = ext_tu.ext;
                    },
                    else => break,
                },
                .alias => |alias| {
                    current_ext = type_store.getAliasBackingVar(alias);
                },
                else => break,
            }
        }
    }

    // Follow aliases and nominal types
    switch (content) {
        .alias => |alias| {
            const backing_var = type_store.getAliasBackingVar(alias);
            return getCtorArgTypes(allocator, type_store, backing_var, tag_id);
        },
        .structure => |flat_type| switch (flat_type) {
            .nominal_type => |nominal| {
                // For parametric nominal types like Try(I64, Str), we need to:
                // 1. Get the backing type's constructor args (which may be type parameters)
                // 2. Substitute any type parameters with the nominal type's arguments
                const backing_var = type_store.getNominalBackingVar(nominal);
                const nom_args = type_store.sliceNominalArgs(nominal);
                const backing_args = getCtorArgTypes(allocator, type_store, backing_var, tag_id);

                // If no nominal args or no backing args, nothing to substitute
                if (nom_args.len == 0 or backing_args.len == 0) {
                    return backing_args;
                }

                // Check if any backing args need substitution (are still type parameters)
                var needs_substitution = false;
                for (backing_args) |arg| {
                    const arg_resolved = type_store.resolveVar(arg);
                    if (arg_resolved.desc.content == .flex or arg_resolved.desc.content == .rigid) {
                        needs_substitution = true;
                        break;
                    }
                }

                if (!needs_substitution) {
                    return backing_args;
                }

                // Collect type parameters from the backing type to build substitution map
                const type_params = collectTypeParamsFromBackingType(type_store, backing_var) catch {
                    // OOM - return unsubstituted args
                    return backing_args;
                };
                defer type_store.gpa.free(type_params);

                // Build substitution: param[i] -> nom_args[i]
                // Only substitute if we have the same number of params and args
                if (type_params.len != nom_args.len) {
                    return backing_args;
                }

                // Allocate result with substituted vars (uses arena allocator, freed at end of check)
                const result = allocator.alloc(Var, backing_args.len) catch {
                    return backing_args;
                };

                for (backing_args, 0..) |arg, i| {
                    const arg_resolved = type_store.resolveVar(arg);
                    const arg_root = arg_resolved.var_;

                    // Check if this is a type parameter that should be substituted
                    if (arg_resolved.desc.content == .flex or arg_resolved.desc.content == .rigid) {
                        // Find which parameter index this is
                        var found_idx: ?usize = null;
                        for (type_params, 0..) |param, param_idx| {
                            if (type_store.resolveVar(param).var_ == arg_root) {
                                found_idx = param_idx;
                                break;
                            }
                        }

                        if (found_idx) |idx| {
                            result[i] = nom_args[idx];
                        } else {
                            result[i] = arg;
                        }
                    } else {
                        result[i] = arg;
                    }
                }

                return result;
            },
            .tuple => |tuple| {
                // Tuples are single-constructor types, return the element types
                return type_store.sliceVars(tuple.elems);
            },
            .record => |record| {
                // Records are single-constructor types, return the field types
                return getRecordFieldTypes(type_store, record.fields);
            },
            .record_unbound => |fields| {
                // Unbound records also have field types
                return getRecordFieldTypes(type_store, fields);
            },
            else => {},
        },
        else => {},
    }

    return &[_]Var{};
}

/// Get the field types from a Record type.
/// Returns the Var for each field in the record.
fn getRecordFieldTypes(type_store: *TypeStore, fields: types.RecordField.SafeMultiList.Range) []const Var {
    const fields_slice = type_store.getRecordFieldsSlice(fields);
    return fields_slice.items(.var_);
}

/// Look up a record field's type by its name.
/// Returns null if the field doesn't exist in the record type.
/// Handles record, record_unbound, and follows aliases/recursion vars.
/// Uses iterative approach to avoid stack overflow on deeply nested types.
fn getRecordFieldTypeByName(type_store: *TypeStore, record_type: Var, field_name: Ident.Idx) ?Var {
    var current_type = record_type;

    // Track seen variables to detect cycles in recursive types
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(type_store.gpa);

    while (true) {
        const resolved = type_store.resolveVar(current_type);
        const resolved_var = resolved.var_;
        const content = resolved.desc.content;

        // Cycle detection: if we've seen this resolved variable before, stop
        const gop = seen.getOrPut(type_store.gpa, resolved_var) catch return null;
        if (gop.found_existing) {
            return null; // Cycle detected - field not found
        }

        switch (content) {
            .structure => |flat_type| switch (flat_type) {
                .record => |record| {
                    const fields_slice = type_store.getRecordFieldsSlice(record.fields);
                    const field_names = fields_slice.items(.name);
                    const field_vars = fields_slice.items(.var_);

                    for (field_names, field_vars) |name, var_| {
                        // Compare by idx (interned string index), not the full Ident.Idx
                        if (name.idx == field_name.idx) {
                            return var_;
                        }
                    }
                    // Field not found in this record - check extension
                    current_type = record.ext;
                    continue;
                },
                .record_unbound => |fields| {
                    const fields_slice = type_store.getRecordFieldsSlice(fields);
                    const field_names = fields_slice.items(.name);
                    const field_vars = fields_slice.items(.var_);

                    for (field_names, field_vars) |name, var_| {
                        if (name.idx == field_name.idx) {
                            return var_;
                        }
                    }
                    return null;
                },
                .empty_record => return null,
                else => return null,
            },
            .alias => |alias| {
                current_type = type_store.getAliasBackingVar(alias);
                continue;
            },
            else => return null,
        }
    }
}

/// Get the element type from a List type.
fn getListElemType(type_store: *TypeStore, type_var: Var) ?Var {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    // List is a nominal type with one type argument (the element type)
    if (content.unwrapNominalType()) |nominal| {
        const args = type_store.sliceNominalArgs(nominal);
        if (args.len == 1) {
            return args[0];
        }
    }

    // Follow aliases
    switch (content) {
        .alias => |alias| {
            const backing_var = type_store.getAliasBackingVar(alias);
            return getListElemType(type_store, backing_var);
        },
        else => {},
    }

    return null;
}

// Exhaustiveness Algorithm
//
// Implementation of Maranget's algorithm for checking pattern exhaustiveness.
// The key insight is that we maintain a "pattern matrix" where:
// - Each row represents one branch's patterns
// - Each column represents one position in the scrutinee
//
// We recursively specialize the matrix by constructors and check if all cases are covered.

/// Type information for each column in the pattern matrix.
/// Used to determine inhabitedness of wildcard patterns.
pub const ColumnTypes = struct {
    /// Type variable for each column
    types: []const Var,
    /// Reference to type store for lookups
    type_store: *TypeStore,
    /// Builtin type identifiers for special-casing
    builtin_idents: BuiltinIdents,

    /// Get the number of columns
    pub fn len(self: ColumnTypes) usize {
        return self.types.len;
    }

    /// Get the argument types when specializing by a constructor.
    /// The first column type must be a resolved type (not a flex/rigid var) that
    /// contains the constructor being specialized by.
    ///
    /// Returns error.TypeError if the payload types don't match the expected arity.
    /// This can happen for records where the pattern destructures fewer fields
    /// than the actual record type has. This is a known limitation of the reified
    /// pattern algorithm that treats record fields positionally instead of by name.
    ///
    /// When this happens, exhaustiveness checking is skipped for the match expression.
    /// The sketched pattern path (`specializeByRecordPattern`) handles records correctly
    /// by matching fields by name. See module-level docs for more details.
    pub fn specializeByConstructor(
        self: ColumnTypes,
        allocator: std.mem.Allocator,
        tag_id: TagId,
        expected_arity: usize,
    ) error{ OutOfMemory, TypeError }!ColumnTypes {
        // Column types must be available. If not, it indicates a compiler bug.
        std.debug.assert(self.types.len > 0);

        // Look up the tag's payload types from types[0]
        const payload_types = getCtorArgTypes(allocator, self.type_store, self.types[0], tag_id);

        // For tag unions, the arity should match exactly.
        // For records, the pattern might destructure fewer fields than the actual type has.
        // Currently, we don't handle records by field name, so return TypeError to skip
        // exhaustiveness checking in that case.
        if (payload_types.len != expected_arity) {
            return error.TypeError;
        }

        // New types: [payload_types..., self.types[1...]...]
        const new_types = try allocator.alloc(Var, payload_types.len + self.types.len - 1);
        @memcpy(new_types[0..payload_types.len], payload_types);
        if (self.types.len > 1) {
            @memcpy(new_types[payload_types.len..], self.types[1..]);
        }

        return .{ .types = new_types, .type_store = self.type_store, .builtin_idents = self.builtin_idents };
    }

    /// Specialize column types for a record pattern.
    /// Unlike tag unions (positional), records are matched by field name.
    /// `field_names` are the names of the fields being destructured.
    /// Returns the types for those specific fields in the given order.
    pub fn specializeByRecordPattern(
        self: ColumnTypes,
        allocator: std.mem.Allocator,
        field_names: []const Ident.Idx,
    ) error{ OutOfMemory, TypeError }!ColumnTypes {
        std.debug.assert(self.types.len > 0);

        const record_type = self.types[0];
        const field_types = try allocator.alloc(Var, field_names.len);

        for (field_names, 0..) |name, i| {
            field_types[i] = getRecordFieldTypeByName(self.type_store, record_type, name) orelse
                return error.TypeError;
        }

        // New types: [field_types..., self.types[1...]...]
        const new_types = try allocator.alloc(Var, field_types.len + self.types.len - 1);
        @memcpy(new_types[0..field_types.len], field_types);
        if (self.types.len > 1) {
            @memcpy(new_types[field_types.len..], self.types[1..]);
        }

        return .{ .types = new_types, .type_store = self.type_store, .builtin_idents = self.builtin_idents };
    }

    /// Remove the first column type
    pub fn dropFirst(self: ColumnTypes) ColumnTypes {
        if (self.types.len == 0) {
            return .{ .types = &[_]Var{}, .type_store = self.type_store, .builtin_idents = self.builtin_idents };
        }
        return .{ .types = self.types[1..], .type_store = self.type_store, .builtin_idents = self.builtin_idents };
    }

    /// Expand for list specialization
    pub fn specializeForList(
        self: ColumnTypes,
        allocator: std.mem.Allocator,
        elem_count: usize,
    ) !ColumnTypes {
        if (self.types.len == 0) {
            return .{ .types = &[_]Var{}, .type_store = self.type_store, .builtin_idents = self.builtin_idents };
        }

        const elem_type = getListElemType(self.type_store, self.types[0]) orelse self.types[0];

        const new_types = try allocator.alloc(Var, elem_count + self.types.len - 1);
        for (0..elem_count) |i| {
            new_types[i] = elem_type;
        }
        if (self.types.len > 1) {
            @memcpy(new_types[elem_count..], self.types[1..]);
        }

        return .{ .types = new_types, .type_store = self.type_store, .builtin_idents = self.builtin_idents };
    }
};

/// Build the list of list arities we need to check for exhaustiveness.
/// This handles the complexity of variable-length list patterns.
fn buildListCtorsForChecking(
    allocator: std.mem.Allocator,
    pattern_arities: []const ListArity,
) ![]const ListArity {
    // Find the maximum lengths we need to consider
    var max_exact_len: usize = 0;
    var has_slice = false;
    var max_slice_min: usize = 0;

    for (pattern_arities) |arity| {
        switch (arity) {
            .exact => |len| {
                max_exact_len = @max(max_exact_len, len);
            },
            .slice => |s| {
                has_slice = true;
                max_slice_min = @max(max_slice_min, s.prefix + s.suffix);
            },
        }
    }

    if (!has_slice) {
        // Only exact patterns - check each length from 0 to max+1
        var result: std.ArrayList(ListArity) = .empty;
        for (0..max_exact_len + 2) |len| {
            try result.append(allocator, .{ .exact = len });
        }
        return try result.toOwnedSlice(allocator);
    }

    // Has slice patterns - check each length from 0 to the point where slices take over
    var result: std.ArrayList(ListArity) = .empty;
    const check_until = @max(max_exact_len + 1, max_slice_min);

    for (0..check_until) |len| {
        try result.append(allocator, .{ .exact = len });
    }

    // Add one slice pattern to cover all remaining lengths
    try result.append(allocator, .{ .slice = .{
        .prefix = check_until,
        .suffix = 0,
    } });

    return try result.toOwnedSlice(allocator);
}

// Sketched Pattern Path
//
// This implementation works with UnresolvedPattern directly, resolving types
// on-demand during checking. This is the primary implementation used by checkMatch.
//
// The sketched path handles records correctly by matching fields by name rather
// than position. This allows patterns like `{ name, age }` and `{ age, name }` to
// be properly compared even though they list fields in different orders.

/// A matrix of sketched (unresolved) patterns for exhaustiveness checking.
/// Patterns are reified on-demand when type information is needed.
pub const SketchedMatrix = struct {
    rows: []const []const UnresolvedPattern,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, rows: []const []const UnresolvedPattern) SketchedMatrix {
        return .{ .rows = rows, .allocator = allocator };
    }

    pub fn isEmpty(self: SketchedMatrix) bool {
        return self.rows.len == 0;
    }

    /// Get the first column of patterns
    pub fn firstColumn(self: SketchedMatrix) error{OutOfMemory}![]const UnresolvedPattern {
        if (self.rows.len == 0) return &[_]UnresolvedPattern{};
        const col = try self.allocator.alloc(UnresolvedPattern, self.rows.len);
        for (self.rows, 0..) |row, i| {
            col[i] = if (row.len > 0) row[0] else .anything;
        }
        return col;
    }
};

/// Result of collecting constructors from the first column of a sketched matrix.
/// When reifying fails, returns an error.
const CollectedCtorsSketched = union(enum) {
    /// Only wildcards/anything - pattern is not exhaustive by itself
    non_exhaustive_wildcards,
    /// Specific tag constructors found
    ctors: struct {
        found: []const TagId,
        union_info: Union,
    },
    /// List patterns found
    lists: []const ListArity,
    /// Literal patterns found (cannot be exhaustive for infinite domains)
    literals,
};

/// Collect constructors from the first column of a sketched matrix.
/// Reifies constructor patterns on-demand to get union information.
fn collectCtorsSketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    matrix: SketchedMatrix,
    first_col_type: Var,
) ReifyError!CollectedCtorsSketched {
    if (matrix.isEmpty()) return .non_exhaustive_wildcards;

    const first_col = try matrix.firstColumn();
    if (first_col.len == 0) return .non_exhaustive_wildcards;

    // Determine what kind of patterns we have
    var found_ctor = false;
    var found_list = false;
    var found_literal = false;
    var found_wildcard = false;
    var union_info: ?Union = null;

    // For records, collect all unique field names from all patterns
    var all_record_fields: std.ArrayList(Ident.Idx) = .empty;
    var is_record = false;

    for (first_col) |pat| {
        switch (pat) {
            .ctor => |c| {
                found_ctor = true;
                if (union_info == null) {
                    const union_result = try getUnionFromType(allocator, type_store, builtin_idents, first_col_type);
                    switch (union_result) {
                        .success => |u| union_info = u,
                        .not_a_union => return error.TypeError,
                    }
                    // Verify tag exists
                    if (findTagId(union_info.?, c.tag_name) == null) {
                        return error.TypeError;
                    }
                }
            },
            .known_ctor => |kc| {
                found_ctor = true;
                if (union_info == null) {
                    union_info = kc.union_info;
                }
                // Collect record fields
                switch (kc.union_info.render_as) {
                    .record => |fields| {
                        is_record = true;
                        for (fields) |field| {
                            // Add if not already present
                            var already_present = false;
                            for (all_record_fields.items) |existing| {
                                if (existing.idx == field.idx) {
                                    already_present = true;
                                    break;
                                }
                            }
                            if (!already_present) {
                                try all_record_fields.append(allocator, field);
                            }
                        }
                    },
                    else => {},
                }
            },
            .list => {
                found_list = true;
            },
            .literal => {
                found_literal = true;
            },
            .anything => {
                found_wildcard = true;
            },
        }
    }

    if (found_ctor) {
        // Collect all unique tag IDs
        var tag_set = std.AutoHashMap(TagId, void).init(allocator);
        for (first_col) |pat| {
            switch (pat) {
                .ctor => |c| {
                    const tag_id = findTagId(union_info.?, c.tag_name) orelse return error.TypeError;
                    try tag_set.put(tag_id, {});
                },
                .known_ctor => |kc| {
                    try tag_set.put(kc.tag_id, {});
                },
                else => {},
            }
        }

        var found_tags: std.ArrayList(TagId) = .empty;
        var it = tag_set.keyIterator();
        while (it.next()) |key| {
            try found_tags.append(allocator, key.*);
        }

        // For records, update union_info to include all fields
        var result_union_info = union_info.?;
        if (is_record and all_record_fields.items.len > 0) {
            const all_fields = try all_record_fields.toOwnedSlice(allocator);
            result_union_info.render_as = .{ .record = all_fields };
            // Update the alternative's arity to match total fields
            if (result_union_info.alternatives.len == 1) {
                const new_alts = try allocator.alloc(CtorInfo, 1);
                new_alts[0] = .{
                    .tag_id = result_union_info.alternatives[0].tag_id,
                    .arity = all_fields.len,
                    .name = result_union_info.alternatives[0].name,
                };
                result_union_info.alternatives = new_alts;
            }
        }

        return .{ .ctors = .{
            .found = try found_tags.toOwnedSlice(allocator),
            .union_info = result_union_info,
        } };
    }

    if (found_list) {
        // When we have both list patterns and wildcards, we still need to check
        // all list arities. The wildcards will be expanded during specialization
        // (specializeByListAritySketched handles wildcards by expanding them).
        // Previously this returned .non_exhaustive_wildcards when wildcards were
        // present, which caused false non-exhaustive errors because wildcards
        // covering all list arities weren't being considered.
        var arities: std.ArrayList(ListArity) = .empty;
        for (first_col) |pat| {
            if (pat == .list) {
                try arities.append(allocator, pat.list.arity);
            }
        }
        return .{ .lists = try arities.toOwnedSlice(allocator) };
    }

    if (found_literal) {
        if (found_wildcard) {
            return .non_exhaustive_wildcards;
        }
        return .literals;
    }

    return .non_exhaustive_wildcards;
}

/// Specialize a sketched matrix by a constructor.
/// Keeps patterns in unresolved form until needed.
/// For records, handles field name matching so patterns with different field sets
/// are properly aligned to the target field order.
fn specializeByConstructorSketched(
    allocator: std.mem.Allocator,
    matrix: SketchedMatrix,
    tag_id: TagId,
    arity: usize,
    union_info: Union,
) error{OutOfMemory}!SketchedMatrix {
    var new_rows: std.ArrayList([]const UnresolvedPattern) = .empty;

    // For records, get the target field names we're specializing by
    const target_fields: ?[]const Ident.Idx = switch (union_info.render_as) {
        .record => |fields| fields,
        else => null,
    };

    for (matrix.rows) |row| {
        if (row.len == 0) continue;

        const first = row[0];
        const rest = row[1..];

        switch (first) {
            .ctor => |c| {
                const pat_tag_id = findTagId(union_info, c.tag_name) orelse continue;
                if (@intFromEnum(pat_tag_id) == @intFromEnum(tag_id)) {
                    const new_row = try allocator.alloc(UnresolvedPattern, c.args.len + rest.len);
                    @memcpy(new_row[0..c.args.len], c.args);
                    @memcpy(new_row[c.args.len..], rest);
                    try new_rows.append(allocator, new_row);
                }
            },
            .known_ctor => |kc| {
                if (@intFromEnum(kc.tag_id) == @intFromEnum(tag_id)) {
                    // For records, we need to match fields by name, not position.
                    // Different patterns may destructure different fields.
                    if (target_fields) |targets| {
                        // Get this pattern's field names
                        const pat_fields: []const Ident.Idx = switch (kc.union_info.render_as) {
                            .record => |fields| fields,
                            else => &[_]Ident.Idx{}, // Shouldn't happen for records
                        };

                        // Build the new row with fields aligned to target order
                        const new_row = try allocator.alloc(UnresolvedPattern, arity + rest.len);

                        for (targets, 0..) |target_field, i| {
                            // Find this field in the pattern's field list
                            var found = false;
                            for (pat_fields, 0..) |pat_field, j| {
                                if (pat_field.idx == target_field.idx) {
                                    // Found the field - use the pattern's arg
                                    new_row[i] = if (j < kc.args.len) kc.args[j] else .anything;
                                    found = true;
                                    break;
                                }
                            }
                            if (!found) {
                                // Pattern doesn't destructure this field - use wildcard
                                new_row[i] = .anything;
                            }
                        }

                        @memcpy(new_row[arity..], rest);
                        try new_rows.append(allocator, new_row);
                    } else {
                        // Non-record: use positional matching
                        const new_row = try allocator.alloc(UnresolvedPattern, kc.args.len + rest.len);
                        @memcpy(new_row[0..kc.args.len], kc.args);
                        @memcpy(new_row[kc.args.len..], rest);
                        try new_rows.append(allocator, new_row);
                    }
                }
            },
            .anything => {
                const new_row = try allocator.alloc(UnresolvedPattern, arity + rest.len);
                for (0..arity) |i| {
                    new_row[i] = .anything;
                }
                @memcpy(new_row[arity..], rest);
                try new_rows.append(allocator, new_row);
            },
            else => {},
        }
    }

    return SketchedMatrix.init(allocator, try new_rows.toOwnedSlice(allocator));
}

/// Specialize the sketched matrix for wildcard - keep only rows starting with wildcard
fn specializeByAnythingSketched(allocator: std.mem.Allocator, matrix: SketchedMatrix) error{OutOfMemory}!SketchedMatrix {
    var new_rows: std.ArrayList([]const UnresolvedPattern) = .empty;

    for (matrix.rows) |row| {
        if (row.len == 0) continue;
        if (row[0] == .anything) {
            try new_rows.append(allocator, row[1..]);
        }
    }

    return SketchedMatrix.init(allocator, try new_rows.toOwnedSlice(allocator));
}

/// Specialize the sketched matrix by a list arity.
fn specializeByListAritySketched(
    allocator: std.mem.Allocator,
    matrix: SketchedMatrix,
    arity: ListArity,
) error{OutOfMemory}!SketchedMatrix {
    var new_rows: std.ArrayList([]const UnresolvedPattern) = .empty;

    const target_len = arity.minLen();

    for (matrix.rows) |row| {
        if (row.len == 0) continue;

        const first = row[0];
        const rest = row[1..];

        switch (first) {
            .list => |l| {
                if (l.arity.coversLength(target_len)) {
                    const new_row = try allocator.alloc(UnresolvedPattern, target_len + rest.len);

                    switch (l.arity) {
                        .exact => {
                            @memcpy(new_row[0..l.elements.len], l.elements);
                        },
                        .slice => |s| {
                            @memcpy(new_row[0..s.prefix], l.elements[0..s.prefix]);
                            const middle_len = target_len - s.prefix - s.suffix;
                            for (s.prefix..s.prefix + middle_len) |i| {
                                new_row[i] = .anything;
                            }
                            if (s.suffix > 0) {
                                const suffix_start = l.elements.len - s.suffix;
                                @memcpy(new_row[s.prefix + middle_len .. target_len], l.elements[suffix_start..]);
                            }
                        },
                    }

                    @memcpy(new_row[target_len..], rest);
                    try new_rows.append(allocator, new_row);
                }
            },
            .anything => {
                const new_row = try allocator.alloc(UnresolvedPattern, target_len + rest.len);
                for (0..target_len) |i| {
                    new_row[i] = .anything;
                }
                @memcpy(new_row[target_len..], rest);
                try new_rows.append(allocator, new_row);
            },
            else => {},
        }
    }

    return SketchedMatrix.init(allocator, try new_rows.toOwnedSlice(allocator));
}

/// Check if a sketched pattern matrix is exhaustive.
/// Reifies patterns on-demand when type information is needed.
/// Returns missing patterns as reified Pattern for error messages.
pub fn checkExhaustiveSketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    matrix: SketchedMatrix,
    column_types: ColumnTypes,
) ReifyError![]const Pattern {
    const n = column_types.len();

    // Base case: empty matrix with columns to fill = not exhaustive
    if (matrix.isEmpty()) {
        if (n == 0) {
            return &[_]Pattern{};
        }
        // Return typed wildcards as missing pattern
        const missing = try allocator.alloc(Pattern, n);
        for (column_types.types, 0..) |col_type, i| {
            missing[i] = .{ .anything = col_type };
        }
        return missing;
    }

    if (n == 0) {
        return &[_]Pattern{};
    }

    // Column types must be available for exhaustiveness checking.
    // If this assertion fails, it indicates a compiler bug - likely incomplete type inference.
    std.debug.assert(column_types.types.len > 0);
    const first_col_type = column_types.types[0];
    const ctors = try collectCtorsSketched(allocator, type_store, builtin_idents, matrix, first_col_type);

    return switch (ctors) {
        .non_exhaustive_wildcards => {
            const new_matrix = try specializeByAnythingSketched(allocator, matrix);
            const rest_types = column_types.dropFirst();
            const rest = try checkExhaustiveSketched(allocator, type_store, builtin_idents, new_matrix, rest_types);

            if (rest.len == 0) return &[_]Pattern{};

            const result = try allocator.alloc(Pattern, 1 + rest.len);
            result[0] = .{ .anything = column_types.types[0] };
            @memcpy(result[1..], rest);
            return result;
        },

        .ctors => |ctor_info| {
            const num_found = ctor_info.found.len;
            const num_alts = ctor_info.union_info.alternatives.len;

            if (num_found < num_alts) {
                // Check missing constructors
                for (ctor_info.union_info.alternatives) |alt| {
                    var found = false;
                    for (ctor_info.found) |found_id| {
                        if (@intFromEnum(alt.tag_id) == @intFromEnum(found_id)) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        // Skip uninhabited constructors - they don't need to be matched
                        // because no values of that constructor can exist.
                        const arg_types = getCtorArgTypes(allocator, type_store, first_col_type, alt.tag_id);
                        if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
                            continue;
                        }

                        const specialized = try specializeByConstructorSketched(
                            allocator,
                            matrix,
                            alt.tag_id,
                            alt.arity,
                            ctor_info.union_info,
                        );

                        // Use field-name-based lookup for records, positional for everything else
                        const specialized_types = switch (ctor_info.union_info.render_as) {
                            .record => |field_names| try column_types.specializeByRecordPattern(allocator, field_names),
                            else => try column_types.specializeByConstructor(allocator, alt.tag_id, alt.arity),
                        };
                        const inner_missing = try checkExhaustiveSketched(allocator, type_store, builtin_idents, specialized, specialized_types);

                        // For arity-0 constructors with no matching rows, the specialized matrix
                        // is empty with 0 columns, which returns empty inner_missing. But we still
                        // need to report the constructor as missing.
                        // Note: We skip the #Open synthetic tag (arity-0 with no name) - it represents
                        // "possibly more constructors" and is handled by the union's has_flex_extension flag.
                        const is_open_synthetic = alt.name.tag.isNone();
                        const is_missing = inner_missing.len > 0 or (alt.arity == 0 and specialized.isEmpty() and !is_open_synthetic);
                        if (is_missing) {
                            const missing_pattern = Pattern{ .ctor = .{
                                .union_info = ctor_info.union_info,
                                .tag_id = alt.tag_id,
                                .args = inner_missing,
                            } };
                            if (try missing_pattern.isInhabited(column_types.type_store, column_types.builtin_idents)) {
                                const result = try allocator.alloc(Pattern, 1);
                                result[0] = missing_pattern;
                                return result;
                            }
                        }
                    }
                }
                return &[_]Pattern{};
            }

            // All constructors covered - check each recursively
            for (ctor_info.union_info.alternatives) |alt| {
                // Skip uninhabited constructors
                const arg_types = getCtorArgTypes(allocator, type_store, first_col_type, alt.tag_id);
                if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
                    continue;
                }

                const specialized = try specializeByConstructorSketched(
                    allocator,
                    matrix,
                    alt.tag_id,
                    alt.arity,
                    ctor_info.union_info,
                );

                // Use field-name-based lookup for records, positional for everything else
                const specialized_types = switch (ctor_info.union_info.render_as) {
                    .record => |field_names| try column_types.specializeByRecordPattern(allocator, field_names),
                    else => try column_types.specializeByConstructor(allocator, alt.tag_id, alt.arity),
                };
                const missing = try checkExhaustiveSketched(allocator, type_store, builtin_idents, specialized, specialized_types);

                if (missing.len > 0) {
                    const args = try allocator.alloc(Pattern, alt.arity);
                    for (0..alt.arity) |i| {
                        if (i < missing.len) {
                            args[i] = missing[i];
                        } else {
                            const arg_type = if (i < specialized_types.types.len) specialized_types.types[i] else null;
                            args[i] = .{ .anything = arg_type };
                        }
                    }

                    const missing_pattern = Pattern{ .ctor = .{
                        .union_info = ctor_info.union_info,
                        .tag_id = alt.tag_id,
                        .args = args,
                    } };

                    if (try missing_pattern.isInhabited(column_types.type_store, column_types.builtin_idents)) {
                        const result = try allocator.alloc(Pattern, 1);
                        result[0] = missing_pattern;
                        return result;
                    }
                }
            }

            return &[_]Pattern{};
        },

        .lists => |arities| {
            const ctors_to_check = try buildListCtorsForChecking(allocator, arities);

            // Check if list elements are inhabited. If not, only the empty list exists.
            const elem_type = if (column_types.types.len > 0)
                getListElemType(type_store, column_types.types[0])
            else
                null;
            const elem_inhabited = if (elem_type) |et|
                try isTypeInhabited(type_store, builtin_idents, et)
            else
                true; // No type info, assume inhabited

            for (ctors_to_check) |list_arity| {
                const min_len = list_arity.minLen();

                // Skip non-empty list arities if elements are uninhabited
                if (min_len > 0 and !elem_inhabited) {
                    continue;
                }

                const specialized = try specializeByListAritySketched(allocator, matrix, list_arity);
                const specialized_types = try column_types.specializeForList(allocator, min_len);
                const missing = try checkExhaustiveSketched(allocator, type_store, builtin_idents, specialized, specialized_types);

                if (missing.len > 0) {
                    const elements = try allocator.alloc(Pattern, min_len);
                    for (0..min_len) |i| {
                        if (i < missing.len) {
                            elements[i] = missing[i];
                        } else {
                            const elem_type_for_pat = if (i < specialized_types.types.len) specialized_types.types[i] else null;
                            elements[i] = .{ .anything = elem_type_for_pat };
                        }
                    }

                    const result = try allocator.alloc(Pattern, 1);
                    result[0] = .{ .list = .{
                        .arity = list_arity,
                        .elements = elements,
                    } };
                    return result;
                }
            }

            return &[_]Pattern{};
        },

        .literals => {
            const result = try allocator.alloc(Pattern, 1);
            const first_type = if (column_types.types.len > 0) column_types.types[0] else null;
            result[0] = .{ .anything = first_type };
            return result;
        },
    };
}

/// Check if a new sketched pattern row is "useful" given existing sketched rows.
/// Reifies patterns on-demand when type information is needed.
pub fn isUsefulSketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    existing_matrix: SketchedMatrix,
    new_row: []const UnresolvedPattern,
    column_types: ColumnTypes,
) ReifyError!bool {
    // Empty matrix = new row is definitely useful, UNLESS the pattern is uninhabited
    if (existing_matrix.isEmpty()) {
        // Check if the pattern is on an uninhabited type
        // For ctor patterns, check if any argument type is uninhabited
        return isSketchedPatternInhabited(allocator, type_store, builtin_idents, new_row, column_types);
    }

    // No more patterns to check = not useful (existing rows cover everything)
    if (new_row.len == 0) return false;

    // Column types must match the pattern row length.
    // If this assertion fails, it indicates a compiler bug - likely incomplete type inference.
    std.debug.assert(column_types.types.len >= new_row.len);

    const first = new_row[0];
    const rest = new_row[1..];
    const first_col_type = column_types.types[0];

    return switch (first) {
        .ctor => |c| {
            const union_result = try getUnionFromType(allocator, type_store, builtin_idents, first_col_type);
            const union_info = switch (union_result) {
                .success => |u| u,
                .not_a_union => return error.TypeError,
            };
            const tag_id = findTagId(union_info, c.tag_name) orelse return error.TypeError;

            const specialized = try specializeByConstructorSketched(
                allocator,
                existing_matrix,
                tag_id,
                c.args.len,
                union_info,
            );
            const specialized_types = try column_types.specializeByConstructor(allocator, tag_id, c.args.len);

            const extended_row = try allocator.alloc(UnresolvedPattern, c.args.len + rest.len);
            @memcpy(extended_row[0..c.args.len], c.args);
            @memcpy(extended_row[c.args.len..], rest);

            return isUsefulSketched(allocator, type_store, builtin_idents, specialized, extended_row, specialized_types);
        },

        .known_ctor => |kc| {
            // For records, we need to merge field sets from matrix + current pattern
            var merged_union_info = kc.union_info;
            switch (kc.union_info.render_as) {
                .record => |current_fields| {
                    // Collect all unique fields from matrix patterns + current pattern
                    var all_fields: std.ArrayList(Ident.Idx) = .empty;

                    // Add current pattern's fields
                    for (current_fields) |field| {
                        try all_fields.append(allocator, field);
                    }

                    // Add fields from matrix patterns
                    const first_col = try existing_matrix.firstColumn();
                    for (first_col) |pat| {
                        switch (pat) {
                            .known_ctor => |mat_kc| {
                                switch (mat_kc.union_info.render_as) {
                                    .record => |mat_fields| {
                                        for (mat_fields) |field| {
                                            var already_present = false;
                                            for (all_fields.items) |existing| {
                                                if (existing.idx == field.idx) {
                                                    already_present = true;
                                                    break;
                                                }
                                            }
                                            if (!already_present) {
                                                try all_fields.append(allocator, field);
                                            }
                                        }
                                    },
                                    else => {},
                                }
                            },
                            else => {},
                        }
                    }

                    // Update union_info with all fields
                    const all_fields_slice = try all_fields.toOwnedSlice(allocator);
                    merged_union_info.render_as = .{ .record = all_fields_slice };
                    if (merged_union_info.alternatives.len == 1) {
                        const new_alts = try allocator.alloc(CtorInfo, 1);
                        new_alts[0] = .{
                            .tag_id = merged_union_info.alternatives[0].tag_id,
                            .arity = all_fields_slice.len,
                            .name = merged_union_info.alternatives[0].name,
                        };
                        merged_union_info.alternatives = new_alts;
                    }
                },
                else => {},
            }

            const arity = if (merged_union_info.alternatives.len > 0)
                merged_union_info.alternatives[0].arity
            else
                kc.args.len;

            const specialized = try specializeByConstructorSketched(
                allocator,
                existing_matrix,
                kc.tag_id,
                arity,
                merged_union_info,
            );

            // Use field-name-based lookup for records, positional for everything else
            const specialized_types = switch (merged_union_info.render_as) {
                .record => |field_names| try column_types.specializeByRecordPattern(allocator, field_names),
                else => try column_types.specializeByConstructor(allocator, kc.tag_id, kc.args.len),
            };

            // Expand current pattern's args to match merged field set
            const extended_row = switch (merged_union_info.render_as) {
                .record => |merged_fields| blk: {
                    const row = try allocator.alloc(UnresolvedPattern, arity + rest.len);
                    const current_fields = switch (kc.union_info.render_as) {
                        .record => |f| f,
                        else => &[_]Ident.Idx{},
                    };

                    // Map current pattern's args to merged field positions
                    for (merged_fields, 0..) |merged_field, i| {
                        var found = false;
                        for (current_fields, 0..) |cur_field, j| {
                            if (cur_field.idx == merged_field.idx) {
                                row[i] = if (j < kc.args.len) kc.args[j] else .anything;
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            row[i] = .anything;
                        }
                    }
                    @memcpy(row[arity..], rest);
                    break :blk row;
                },
                else => blk: {
                    const row = try allocator.alloc(UnresolvedPattern, kc.args.len + rest.len);
                    @memcpy(row[0..kc.args.len], kc.args);
                    @memcpy(row[kc.args.len..], rest);
                    break :blk row;
                },
            };

            return isUsefulSketched(allocator, type_store, builtin_idents, specialized, extended_row, specialized_types);
        },

        .anything => {
            // Check if matrix is complete (covers all constructors)
            const ctors = try collectCtorsSketched(allocator, type_store, builtin_idents, existing_matrix, first_col_type);

            switch (ctors) {
                .non_exhaustive_wildcards => {
                    const specialized = try specializeByAnythingSketched(allocator, existing_matrix);
                    const rest_types = column_types.dropFirst();
                    return isUsefulSketched(allocator, type_store, builtin_idents, specialized, rest, rest_types);
                },

                .ctors => |ctor_info| {
                    // Optimization: For flex extensions, wildcards are always useful.
                    // See comment in isUseful for detailed explanation.
                    if (ctor_info.union_info.has_flex_extension) {
                        return true;
                    }

                    const num_found = ctor_info.found.len;
                    const num_alts = ctor_info.union_info.alternatives.len;

                    if (num_found < num_alts) {
                        // Not all constructors covered - but check if missing constructors are all uninhabited
                        var any_missing_inhabited = false;
                        for (ctor_info.union_info.alternatives) |alt| {
                            var found = false;
                            for (ctor_info.found) |found_id| {
                                if (@intFromEnum(alt.tag_id) == @intFromEnum(found_id)) {
                                    found = true;
                                    break;
                                }
                            }
                            if (!found) {
                                // This constructor is missing - check if it's inhabited
                                const arg_types = getCtorArgTypes(allocator, type_store, first_col_type, alt.tag_id);
                                var ctor_uninhabited = false;
                                for (arg_types) |arg_type| {
                                    if (!try isTypeInhabited(type_store, builtin_idents, arg_type)) {
                                        ctor_uninhabited = true;
                                        break;
                                    }
                                }
                                if (!ctor_uninhabited) {
                                    any_missing_inhabited = true;
                                    break;
                                }
                            }
                        }

                        if (!any_missing_inhabited) {
                            // All missing constructors are uninhabited - wildcard is not useful
                            return false;
                        }

                        const specialized = try specializeByAnythingSketched(allocator, existing_matrix);
                        const rest_types = column_types.dropFirst();
                        return isUsefulSketched(allocator, type_store, builtin_idents, specialized, rest, rest_types);
                    }

                    // All constructors covered - check each one
                    for (ctor_info.union_info.alternatives) |alt| {
                        // Skip uninhabited constructors
                        const arg_types = getCtorArgTypes(allocator, type_store, first_col_type, alt.tag_id);
                        if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
                            continue;
                        }

                        const specialized = try specializeByConstructorSketched(
                            allocator,
                            existing_matrix,
                            alt.tag_id,
                            alt.arity,
                            ctor_info.union_info,
                        );

                        // Use field-name-based lookup for records, positional for everything else
                        const specialized_types = switch (ctor_info.union_info.render_as) {
                            .record => |field_names| try column_types.specializeByRecordPattern(allocator, field_names),
                            else => try column_types.specializeByConstructor(allocator, alt.tag_id, alt.arity),
                        };

                        const extended = try allocator.alloc(UnresolvedPattern, alt.arity + rest.len);
                        for (0..alt.arity) |i| {
                            extended[i] = .anything;
                        }
                        @memcpy(extended[alt.arity..], rest);

                        if (try isUsefulSketched(allocator, type_store, builtin_idents, specialized, extended, specialized_types)) {
                            return true;
                        }
                    }
                    return false;
                },

                .lists => |arities| {
                    const ctors_to_check = try buildListCtorsForChecking(allocator, arities);

                    // Check if list elements are inhabited. If not, only the empty list exists.
                    const elem_type = if (column_types.types.len > 0)
                        getListElemType(type_store, column_types.types[0])
                    else
                        null;
                    const elem_inhabited = if (elem_type) |et|
                        try isTypeInhabited(type_store, builtin_idents, et)
                    else
                        true; // No type info, assume inhabited

                    for (ctors_to_check) |list_arity| {
                        const min_len = list_arity.minLen();

                        // Skip non-empty list arities if elements are uninhabited
                        if (min_len > 0 and !elem_inhabited) {
                            continue;
                        }

                        const specialized = try specializeByListAritySketched(allocator, existing_matrix, list_arity);
                        const specialized_types = try column_types.specializeForList(allocator, min_len);

                        const extended = try allocator.alloc(UnresolvedPattern, min_len + rest.len);
                        for (0..min_len) |i| {
                            extended[i] = .anything;
                        }
                        @memcpy(extended[min_len..], rest);

                        if (try isUsefulSketched(allocator, type_store, builtin_idents, specialized, extended, specialized_types)) {
                            return true;
                        }
                    }
                    return false;
                },

                .literals => {
                    return true;
                },
            }
        },

        .literal => |lit| {
            var matching_rows: std.ArrayList([]const UnresolvedPattern) = .empty;

            for (existing_matrix.rows) |row| {
                if (row.len == 0) continue;
                const row_first = row[0];

                const matches = switch (row_first) {
                    .literal => |l| Literal.eql(l, lit),
                    .anything => true,
                    else => false,
                };

                if (matches) {
                    try matching_rows.append(allocator, row[1..]);
                }
            }

            const filtered = SketchedMatrix.init(allocator, try matching_rows.toOwnedSlice(allocator));
            const rest_types = column_types.dropFirst();
            return isUsefulSketched(allocator, type_store, builtin_idents, filtered, rest, rest_types);
        },

        .list => |l| {
            // Check if list elements are inhabited. If not, only the empty list exists.
            const elem_type = if (column_types.types.len > 0)
                getListElemType(type_store, column_types.types[0])
            else
                null;
            const elem_inhabited = if (elem_type) |et|
                try isTypeInhabited(type_store, builtin_idents, et)
            else
                true; // No type info, assume inhabited

            switch (l.arity) {
                .exact => {
                    // If this pattern requires elements but elements are uninhabited,
                    // this pattern can never match, so it's not useful (redundant).
                    if (l.elements.len > 0 and !elem_inhabited) {
                        return false;
                    }

                    const specialized = try specializeByListAritySketched(allocator, existing_matrix, l.arity);
                    const specialized_types = try column_types.specializeForList(allocator, l.elements.len);

                    const extended_row = try allocator.alloc(UnresolvedPattern, l.elements.len + rest.len);
                    @memcpy(extended_row[0..l.elements.len], l.elements);
                    @memcpy(extended_row[l.elements.len..], rest);

                    return isUsefulSketched(allocator, type_store, builtin_idents, specialized, extended_row, specialized_types);
                },
                .slice => |s| {
                    // Slice patterns always match at least one non-empty list (prefix + suffix elements),
                    // so if elements are uninhabited, slice patterns are never useful.
                    if (!elem_inhabited) {
                        return false;
                    }

                    const first_col = try existing_matrix.firstColumn();
                    var arities_list: std.ArrayList(ListArity) = .empty;
                    for (first_col) |p| {
                        if (p == .list) {
                            try arities_list.append(allocator, p.list.arity);
                        }
                    }
                    try arities_list.append(allocator, l.arity);

                    const check_arities = try buildListCtorsForChecking(allocator, arities_list.items);

                    for (check_arities) |check_arity| {
                        const len = check_arity.minLen();
                        if (!l.arity.coversLength(len)) continue;

                        const specialized = try specializeByListAritySketched(allocator, existing_matrix, check_arity);
                        const specialized_types = try column_types.specializeForList(allocator, len);

                        const extended_row = try allocator.alloc(UnresolvedPattern, len + rest.len);
                        @memcpy(extended_row[0..s.prefix], l.elements[0..s.prefix]);
                        const middle_len = len - s.prefix - s.suffix;
                        for (s.prefix..s.prefix + middle_len) |i| {
                            extended_row[i] = .anything;
                        }
                        if (s.suffix > 0) {
                            const suffix_start = l.elements.len - s.suffix;
                            @memcpy(extended_row[s.prefix + middle_len .. len], l.elements[suffix_start..]);
                        }
                        @memcpy(extended_row[len..], rest);

                        if (try isUsefulSketched(allocator, type_store, builtin_idents, specialized, extended_row, specialized_types)) {
                            return true;
                        }
                    }
                    return false;
                },
            }
        },
    };
}

/// Result of checking rows for redundancy using sketched patterns
pub const RedundancyResultSketched = struct {
    /// Non-redundant rows (useful patterns)
    non_redundant_rows: []const []const UnresolvedPattern,
    /// Indices of redundant branches (covered by previous patterns)
    redundant_indices: []const u32,
    /// Regions of redundant branches
    redundant_regions: []const Region,
    /// Indices of unmatchable branches (patterns on uninhabited types)
    unmatchable_indices: []const u32,
    /// Regions of unmatchable branches
    unmatchable_regions: []const Region,
};

/// Process sketched pattern rows and identify redundant and unmatchable patterns.
/// Uses on-demand reification for type checking.
///
/// A pattern is **unmatchable** if it's on an uninhabited type (e.g., `Err(_)` on `Try(I64, [])`).
/// A pattern is **redundant** if it's covered by previous patterns (e.g., `_` after `Ok(_)` and `Err(_)`).
pub fn checkRedundancySketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    rows: []const UnresolvedRow,
    column_types: ColumnTypes,
) ReifyError!RedundancyResultSketched {
    var non_redundant: std.ArrayList([]const UnresolvedPattern) = .empty;
    var redundant_indices: std.ArrayList(u32) = .empty;
    var redundant_regions: std.ArrayList(Region) = .empty;
    var unmatchable_indices: std.ArrayList(u32) = .empty;
    var unmatchable_regions: std.ArrayList(Region) = .empty;

    for (rows) |row| {
        // First check if the pattern is on an uninhabited type (unmatchable)
        const is_inhabited = try isSketchedPatternInhabited(
            allocator,
            type_store,
            builtin_idents,
            row.patterns,
            column_types,
        );

        if (!is_inhabited) {
            // Pattern matches an uninhabited type - it's unmatchable
            try unmatchable_indices.append(allocator, row.branch_index);
            try unmatchable_regions.append(allocator, row.region);
            continue;
        }

        // Pattern is on an inhabited type - check if it's useful (not redundant)
        // Rows with guards are always considered useful (guard might fail at runtime)
        const matrix = SketchedMatrix.init(allocator, non_redundant.items);
        const is_useful = row.guard == .has_guard or
            try isUsefulSketched(allocator, type_store, builtin_idents, matrix, row.patterns, column_types);

        if (is_useful) {
            try non_redundant.append(allocator, row.patterns);
        } else {
            try redundant_indices.append(allocator, row.branch_index);
            try redundant_regions.append(allocator, row.region);
        }
    }

    return .{
        .non_redundant_rows = try non_redundant.toOwnedSlice(allocator),
        .redundant_indices = try redundant_indices.toOwnedSlice(allocator),
        .redundant_regions = try redundant_regions.toOwnedSlice(allocator),
        .unmatchable_indices = try unmatchable_indices.toOwnedSlice(allocator),
        .unmatchable_regions = try unmatchable_regions.toOwnedSlice(allocator),
    };
}

// High-level Integration API
//
// These functions provide a simpler interface for the type checker to call.

/// Result of exhaustiveness and redundancy checking
pub const CheckResult = struct {
    /// Whether the match is exhaustive
    is_exhaustive: bool,
    /// Missing patterns if not exhaustive (for error messages)
    missing_patterns: []const Pattern,
    /// Indices of redundant branches (covered by previous patterns)
    redundant_indices: []const u32,
    /// Regions of redundant branches
    redundant_regions: []const Region,
    /// Indices of unmatchable branches (patterns on uninhabited types)
    unmatchable_indices: []const u32,
    /// Regions of unmatchable branches
    unmatchable_regions: []const Region,

    /// Free all allocated memory in the result
    pub fn deinit(self: CheckResult, allocator: std.mem.Allocator) void {
        for (self.missing_patterns) |pat| {
            freePattern(allocator, pat);
        }
        allocator.free(self.missing_patterns);
        allocator.free(self.redundant_indices);
        allocator.free(self.redundant_regions);
        allocator.free(self.unmatchable_indices);
        allocator.free(self.unmatchable_regions);
    }

    fn freePattern(allocator: std.mem.Allocator, pattern: Pattern) void {
        switch (pattern) {
            .anything, .literal => {},
            .ctor => |c| {
                for (c.args) |arg| {
                    freePattern(allocator, arg);
                }
                allocator.free(c.args);
                allocator.free(c.union_info.alternatives);
            },
            .list => |l| {
                for (l.elements) |elem| {
                    freePattern(allocator, elem);
                }
                allocator.free(l.elements);
            },
        }
    }
};

/// Perform full exhaustiveness and redundancy checking on a match expression.
///
/// This is the main entry point for the type checker.
/// Uses 1-phase on-demand reification: patterns are converted to UnresolvedPattern
/// and reified on-demand during checking when type information is needed.
///
/// Returns `error.TypeError` when a pattern cannot be resolved due to type issues
/// (e.g., polymorphic types, type mismatches). The caller should handle this by
/// skipping exhaustiveness error reporting for that match expression.
pub fn checkMatch(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    node_store: *const NodeStore,
    builtin_idents: BuiltinIdents,
    branches_span: CIR.Expr.Match.Branch.Span,
    scrutinee_type: Var,
    overall_region: Region,
) ReifyError!CheckResult {
    // Use an arena for all intermediate allocations to avoid leaks
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    // The scrutinee type must be fully resolved for exhaustiveness checking.
    // If it's still a flex/rigid var, we can't determine what constructors exist.
    const resolved_scrutinee = type_store.resolveVar(scrutinee_type);
    switch (resolved_scrutinee.desc.content) {
        .flex, .rigid => return error.TypeError,
        else => {},
    }

    // Phase 1: Convert CIR patterns to sketched (unresolved) patterns
    const sketched = try convertMatchBranches(
        arena_alloc,
        node_store,
        branches_span,
        overall_region,
    );

    // Create initial column types for on-demand reification
    const initial_types = try arena_alloc.alloc(Var, 1);
    initial_types[0] = scrutinee_type;
    const column_types = ColumnTypes{
        .types = initial_types,
        .type_store = type_store,
        .builtin_idents = builtin_idents,
    };

    // Phase 2: Check redundancy with on-demand reification
    // Patterns are reified as needed when type information is required
    const redundancy = try checkRedundancySketched(
        arena_alloc,
        type_store,
        builtin_idents,
        sketched.rows,
        column_types,
    );

    // Phase 3: Check exhaustiveness on non-redundant patterns
    const sketched_matrix = SketchedMatrix.init(arena_alloc, redundancy.non_redundant_rows);
    const missing = try checkExhaustiveSketched(
        arena_alloc,
        type_store,
        builtin_idents,
        sketched_matrix,
        column_types,
    );

    // Copy results to the original allocator before freeing the arena
    const result_patterns = try allocator.alloc(Pattern, missing.len);
    for (missing, 0..) |pat, i| {
        result_patterns[i] = try pat.dupe(allocator);
    }

    return .{
        .is_exhaustive = missing.len == 0,
        .missing_patterns = result_patterns,
        .redundant_indices = try allocator.dupe(u32, redundancy.redundant_indices),
        .redundant_regions = try allocator.dupe(Region, redundancy.redundant_regions),
        .unmatchable_indices = try allocator.dupe(u32, redundancy.unmatchable_indices),
        .unmatchable_regions = try allocator.dupe(Region, redundancy.unmatchable_regions),
    };
}

/// Format a pattern for display in error messages.
pub fn formatPattern(
    allocator: std.mem.Allocator,
    ident_store: *const Ident.Store,
    string_store: *const StringLiteral.Store,
    pattern: Pattern,
) error{OutOfMemory}![]const u8 {
    var buf: std.ArrayList(u8) = .empty;

    try formatPatternInto(&buf, allocator, ident_store, string_store, pattern);

    return try buf.toOwnedSlice(allocator);
}

fn formatPatternInto(
    buf: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    ident_store: *const Ident.Store,
    string_store: *const StringLiteral.Store,
    pattern: Pattern,
) error{OutOfMemory}!void {
    switch (pattern) {
        .anything => try buf.appendSlice(allocator, "_"),

        .literal => |lit| switch (lit) {
            .int => |i| {
                var tmp: [40]u8 = undefined;
                const str = std.fmt.bufPrint(&tmp, "{d}", .{i}) catch "<int>";
                try buf.appendSlice(allocator, str);
            },
            .uint => |u| {
                var tmp: [40]u8 = undefined;
                const str = std.fmt.bufPrint(&tmp, "{d}", .{u}) catch "<uint>";
                try buf.appendSlice(allocator, str);
            },
            .bit => |b| try buf.appendSlice(allocator, if (b) "Bool.true" else "Bool.false"),
            .byte => |b| {
                var tmp: [4]u8 = undefined;
                const str = std.fmt.bufPrint(&tmp, "{d}", .{b}) catch "<byte>";
                try buf.appendSlice(allocator, str);
            },
            .float => |f| {
                const float_val: f64 = @bitCast(f);
                var tmp: [32]u8 = undefined;
                const str = std.fmt.bufPrint(&tmp, "{d}", .{float_val}) catch "<float>";
                try buf.appendSlice(allocator, str);
            },
            .decimal => |d| {
                var tmp: [40]u8 = undefined;
                const str = std.fmt.bufPrint(&tmp, "{d}", .{d}) catch "<decimal>";
                try buf.appendSlice(allocator, str);
            },
            .str => |idx| {
                try buf.appendSlice(allocator, "\"");
                const text = string_store.get(idx);
                try buf.appendSlice(allocator, text);
                try buf.appendSlice(allocator, "\"");
            },
        },

        .ctor => |c| {
            switch (c.union_info.render_as) {
                .tag => {
                    const alt = c.union_info.alternatives[c.tag_id.toInt()];
                    switch (alt.name) {
                        .tag => |t| {
                            if (t == Ident.Idx.NONE) {
                                // This is the #Open synthetic tag - show as wildcard
                                try buf.appendSlice(allocator, "_");
                                return;
                            }
                            try buf.appendSlice(allocator, ident_store.getText(t));
                        },
                        .opaque_type => |o| {
                            try buf.appendSlice(allocator, ident_store.getText(o));
                        },
                    }
                    // Add arguments
                    if (c.args.len > 0) {
                        for (c.args) |arg| {
                            try buf.appendSlice(allocator, " ");
                            try formatPatternInto(buf, allocator, ident_store, string_store, arg);
                        }
                    }
                },

                .record => |field_names| {
                    try buf.appendSlice(allocator, "{ ");
                    for (c.args, 0..) |arg, i| {
                        if (i > 0) try buf.appendSlice(allocator, ", ");
                        if (i < field_names.len) {
                            try buf.appendSlice(allocator, ident_store.getText(field_names[i]));
                        } else {
                            try buf.appendSlice(allocator, "_");
                        }
                        try buf.appendSlice(allocator, ": ");
                        try formatPatternInto(buf, allocator, ident_store, string_store, arg);
                    }
                    try buf.appendSlice(allocator, " }");
                },

                .tuple => {
                    try buf.appendSlice(allocator, "(");
                    for (c.args, 0..) |arg, i| {
                        if (i > 0) try buf.appendSlice(allocator, ", ");
                        try formatPatternInto(buf, allocator, ident_store, string_store, arg);
                    }
                    try buf.appendSlice(allocator, ")");
                },

                .guard => {
                    // Unwrap the guard - show the actual pattern (second arg)
                    if (c.args.len >= 2) {
                        try formatPatternInto(buf, allocator, ident_store, string_store, c.args[1]);
                        try buf.appendSlice(allocator, " (with guard)");
                    }
                },

                .opaque_type => {
                    const alt = c.union_info.alternatives[c.tag_id.toInt()];
                    switch (alt.name) {
                        .opaque_type => |o| {
                            try buf.appendSlice(allocator, ident_store.getText(o));
                        },
                        .tag => |t| {
                            try buf.appendSlice(allocator, ident_store.getText(t));
                        },
                    }
                    if (c.args.len > 0) {
                        try buf.appendSlice(allocator, " ");
                        try formatPatternInto(buf, allocator, ident_store, string_store, c.args[0]);
                    }
                },
            }
        },

        .list => |l| {
            try buf.appendSlice(allocator, "[");
            switch (l.arity) {
                .exact => {
                    for (l.elements, 0..) |elem, i| {
                        if (i > 0) try buf.appendSlice(allocator, ", ");
                        try formatPatternInto(buf, allocator, ident_store, string_store, elem);
                    }
                },
                .slice => |s| {
                    // Format as [prefix.., suffix]
                    for (0..s.prefix) |i| {
                        if (i > 0) try buf.appendSlice(allocator, ", ");
                        try formatPatternInto(buf, allocator, ident_store, string_store, l.elements[i]);
                    }
                    if (s.prefix > 0 and s.suffix > 0) {
                        try buf.appendSlice(allocator, ", .., ");
                    } else if (s.prefix > 0) {
                        try buf.appendSlice(allocator, ", ..");
                    } else if (s.suffix > 0) {
                        try buf.appendSlice(allocator, ".., ");
                    } else {
                        try buf.appendSlice(allocator, "..");
                    }
                    const suffix_start = l.elements.len - s.suffix;
                    for (suffix_start..l.elements.len) |i| {
                        if (i > suffix_start) try buf.appendSlice(allocator, ", ");
                        try formatPatternInto(buf, allocator, ident_store, string_store, l.elements[i]);
                    }
                },
            }
            try buf.appendSlice(allocator, "]");
        },
    }
}
