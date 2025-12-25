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
//!    (`CirPattern`) that captures the pattern structure but may not yet know the full
//!    union type (e.g., we see tag name "Ok" but don't know all alternatives yet).
//!
//! 2. **Type Resolution & Checking**: During type checking, patterns are resolved with
//!    full type information, then checked using a pattern matrix algorithm.
//!
//! ## References
//!
//! - [Warnings for Pattern Matching](http://moscova.inria.fr/~maranget/papers/warn/warn.pdf)
//! - Original Rust implementation in `crates/compiler/exhaustive/`

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const Can = @import("can");
const types = @import("types");

const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const TypeStore = types.Store;
const Var = types.Var;
const Content = types.Content;

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
    pub fn isInhabited(self: Pattern, type_store: *TypeStore) bool {
        return switch (self) {
            .anything => |maybe_type| {
                if (maybe_type) |type_var| {
                    // Check if the type is empty (uninhabited)
                    return !isTypeEmpty(type_store, type_var);
                }
                // TODO: Track type info for all wildcards so we can check inhabitedness.
                return true;
            },

            // Literals are always inhabited (match their specific value)
            .literal => true,

            .ctor => |c| {
                // An empty union is uninhabited - no constructors exist
                if (c.union_info.alternatives.len == 0 and !c.union_info.has_flex_extension) {
                    return false;
                }

                // Check if this constructor exists in the union
                var found = false;
                for (c.union_info.alternatives) |alt| {
                    if (alt.tag_id == c.tag_id) {
                        found = true;
                        break;
                    }
                }
                if (!found and !c.union_info.has_flex_extension) {
                    // Constructor was removed from union (shouldn't happen in practice)
                    return false;
                }

                // All arguments must be inhabited for the pattern to be inhabited
                for (c.args) |arg| {
                    if (!arg.isInhabited(type_store)) return false;
                }
                return true;
            },

            .list => |l| {
                // All elements must be inhabited
                for (l.elements) |elem| {
                    if (!elem.isInhabited(type_store)) return false;
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

    /// Check if this union type is inhabited (has at least one possible value).
    /// An empty tag union with no flex extension is uninhabited.
    pub fn isInhabited(self: Union) bool {
        // If there's a flex extension, more constructors might exist
        if (self.has_flex_extension) return true;
        // Otherwise, need at least one alternative
        return self.alternatives.len > 0;
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
            // TODO: StringLiteral.Store doesn't deduplicate, so different indices
            // could represent the same string. We should compare the actual string
            // contents to properly detect redundant string patterns.
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

/// Result of reifying pattern rows
pub const ReifiedRows = struct {
    /// Rows that are not redundant (patterns are fully resolved)
    rows: []const []const Pattern,
    /// Indices of redundant branches (detected during reification)
    redundant_indices: []const u32,
    /// Errors found during reification
    errors: []const Error,
    /// The overall region of the match expression
    overall_region: Region,
    /// True if any constructor patterns couldn't be fully resolved
    /// (e.g., polymorphic types where the union structure isn't known).
    /// TODO: We should handle polymorphic types properly instead of skipping checks.
    has_unresolved_ctor: bool,
};

/// Reify an unresolved pattern to a concrete pattern using type information.
///
/// This resolves tag patterns by looking up the full union type and determining
/// the tag_id for the constructor.
///
/// Returns `error.TypeError` when the type cannot be resolved (e.g., polymorphic types).
/// This allows the caller to gracefully skip exhaustiveness checking rather than
/// producing incorrect results.
pub fn reifyPattern(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    ident_store: *const Ident.Store,
    unresolved: UnresolvedPattern,
    type_var: Var,
) ReifyError!Pattern {
    return switch (unresolved) {
        .anything => .{ .anything = null },

        .literal => |lit| .{ .literal = lit },

        .known_ctor => |kc| {
            // Union is already known, just reify the args
            const args = try allocator.alloc(Pattern, kc.args.len);
            const arg_types = getCtorArgTypes(type_store, type_var, kc.tag_id);

            for (kc.args, 0..) |arg, i| {
                const arg_type = if (i < arg_types.len) arg_types[i] else type_var;

                // Check if arg_type is a flex/rigid var (polymorphic, unresolved)
                const resolved_arg = type_store.resolveVar(arg_type);
                const is_unresolved = switch (resolved_arg.desc.content) {
                    .flex, .rigid => true,
                    else => false,
                };

                if (is_unresolved and arg != .anything) {
                    // Arg type is polymorphic but pattern expects something specific.
                    // We can't reliably do exhaustiveness checking.
                    return error.TypeError;
                } else {
                    args[i] = try reifyPattern(allocator, type_store, ident_store, arg, arg_type);
                }
            }

            return .{ .ctor = .{
                .union_info = kc.union_info,
                .tag_id = kc.tag_id,
                .args = args,
            } };
        },

        .ctor => |c| {
            // Need to look up the union type from the type variable
            const union_result = try getUnionFromType(allocator, type_store, ident_store, type_var);

            switch (union_result) {
                .success => |union_info| {
                    const tag_id = findTagId(union_info, c.tag_name) orelse {
                        // Tag not found in union - type error
                        return error.TypeError;
                    };

                    const args = try allocator.alloc(Pattern, c.args.len);
                    const arg_types = getCtorArgTypes(type_store, type_var, tag_id);

                    for (c.args, 0..) |arg, i| {
                        const arg_type = if (i < arg_types.len) arg_types[i] else type_var;

                        // Check if arg_type is a flex/rigid var (polymorphic, unresolved)
                        const resolved_arg = type_store.resolveVar(arg_type);
                        const is_unresolved = switch (resolved_arg.desc.content) {
                            .flex, .rigid => true,
                            else => false,
                        };

                        if (is_unresolved and arg != .anything) {
                            // Arg type is polymorphic but pattern expects something specific.
                            // We can't reliably do exhaustiveness checking.
                            return error.TypeError;
                        } else {
                            args[i] = try reifyPattern(allocator, type_store, ident_store, arg, arg_type);
                        }
                    }

                    return .{ .ctor = .{
                        .union_info = union_info,
                        .tag_id = tag_id,
                        .args = args,
                    } };
                },
                .not_a_union => {
                    // Type is not a union - can't do exhaustiveness checking
                    return error.TypeError;
                },
            }
        },

        .list => |l| {
            const elem_type = getListElemType(type_store, type_var) orelse type_var;

            const elements = try allocator.alloc(Pattern, l.elements.len);
            for (l.elements, 0..) |elem, i| {
                elements[i] = try reifyPattern(allocator, type_store, ident_store, elem, elem_type);
            }

            return .{ .list = .{
                .arity = l.arity,
                .elements = elements,
            } };
        },
    };
}

/// Reify all unresolved pattern rows to concrete patterns.
///
/// This also performs redundancy checking as patterns are added.
/// If any pattern fails to reify due to a TypeError, has_unresolved_ctor is set to true,
/// which will cause checking to be skipped for this match expression.
pub fn reifyRows(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    ident_store: *const Ident.Store,
    unresolved: UnresolvedRows,
    scrutinee_type: Var,
) error{OutOfMemory}!ReifiedRows {
    var reified_rows: std.ArrayList([]const Pattern) = .empty;
    var redundant_indices: std.ArrayList(u32) = .empty;
    var errors: std.ArrayList(Error) = .empty;
    var has_unresolved_ctor = false;

    for (unresolved.rows) |row| {
        // Reify this row's patterns
        const reified = try allocator.alloc(Pattern, row.patterns.len);
        for (row.patterns, 0..) |pat, i| {
            reified[i] = reifyPattern(allocator, type_store, ident_store, pat, scrutinee_type) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.TypeError => blk: {
                    // Type error during reification - pattern couldn't be resolved.
                    // Mark as unresolved and use a wildcard as placeholder.
                    has_unresolved_ctor = true;
                    break :blk .{ .anything = null };
                },
            };
        }

        try reified_rows.append(allocator, reified);
    }

    return .{
        .rows = try reified_rows.toOwnedSlice(allocator),
        .redundant_indices = try redundant_indices.toOwnedSlice(allocator),
        .errors = try errors.toOwnedSlice(allocator),
        .overall_region = unresolved.overall_region,
        .has_unresolved_ctor = has_unresolved_ctor,
    };
}

// Helper types and functions for reification

const UnionResult = union(enum) {
    success: Union,
    not_a_union,
};

/// Extract union information from a type variable.
fn getUnionFromType(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    ident_store: *const Ident.Store,
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
            return getUnionFromType(allocator, type_store, ident_store, backing_var);
        },
        .recursion_var => |rec| {
            return getUnionFromType(allocator, type_store, ident_store, rec.structure);
        },
        // TODO: Handle polymorphic types properly instead of treating as not a union.
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
                    return getUnionFromType(allocator, type_store, ident_store, backing_var);
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
fn buildUnionFromTagUnion(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    tag_union: types.TagUnion,
) error{OutOfMemory}!UnionResult {
    const tags_slice = type_store.getTagsSlice(tag_union.tags);
    const tag_names = tags_slice.items(.name);
    const tag_args = tags_slice.items(.args);

    // Check if it's an open union (has extension variable)
    const is_open = isOpenExtension(type_store, tag_union.ext);

    // Allocate alternatives (add one extra for open unions)
    const num_alts = tag_names.len + @as(usize, if (is_open) 1 else 0);
    const alternatives = try allocator.alloc(CtorInfo, num_alts);

    for (tag_names, tag_args, 0..) |name, args_range, i| {
        const arg_vars = type_store.sliceVars(args_range);
        alternatives[i] = .{
            .name = .{ .tag = name },
            .tag_id = @enumFromInt(i),
            .arity = arg_vars.len,
        };
    }

    // Add synthetic #Open constructor for open unions
    if (is_open) {
        alternatives[tag_names.len] = .{
            .name = .{ .tag = Ident.Idx.NONE }, // Represents "#Open"
            .tag_id = @enumFromInt(tag_names.len),
            .arity = 0,
        };
    }

    // Check if extension is a flex var (type not fully constrained)
    const has_flex = hasFlexExtension(type_store, tag_union.ext);

    return .{ .success = .{
        .alternatives = alternatives,
        .render_as = .tag,
        .has_flex_extension = has_flex,
    } };
}

/// Check if an extension variable is a flex var (unconstrained).
/// Used to determine if wildcards should be considered redundant.
fn hasFlexExtension(type_store: *TypeStore, ext: Var) bool {
    const resolved = type_store.resolveVar(ext);
    const content = resolved.desc.content;
    return content == .flex;
}

/// Check if a type is an empty tag union (uninhabited).
/// Returns true if the type is definitely uninhabited (empty closed tag union).
/// Returns false (inhabited) for any other type or if uncertain.
fn isTypeEmpty(type_store: *TypeStore, type_var: Var) bool {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    // Check for empty_tag_union structure directly
    switch (content) {
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union => return true,
            .tag_union => |tag_union| {
                const tags_slice = type_store.getTagsSlice(tag_union.tags);
                if (tags_slice.len == 0) {
                    // Empty but check extension
                    if (isOpenExtension(type_store, tag_union.ext) or hasFlexExtension(type_store, tag_union.ext)) {
                        return false; // Might have more tags
                    }
                    return true; // Definitely empty
                }
                return false; // Has tags, not empty
            },
            .nominal_type => {
                // TODO: Properly check emptiness for nominal types with type parameters.
                // Currently emptiness for types like Try(I64, []) is propagated through
                // ColumnTypes, but we should directly inspect type arguments here.
                return false;
            },
            else => return false, // Other structures are not empty
        },
        .alias => |alias| {
            const backing_var = type_store.getAliasBackingVar(alias);
            return isTypeEmpty(type_store, backing_var);
        },
        .recursion_var => |rec| {
            return isTypeEmpty(type_store, rec.structure);
        },
        else => return false, // Default: not empty
    }
}

/// Check if an extension variable represents an open union.
/// TODO: Properly handle flex vs rigid extension semantics for exhaustiveness.
/// Currently rigid vars are treated as open, flex vars as closed.
fn isOpenExtension(type_store: *TypeStore, ext: Var) bool {
    const resolved = type_store.resolveVar(ext);
    const content = resolved.desc.content;

    return switch (content) {
        .rigid => true,
        .flex => false,
        // Empty tag union means it's closed
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
    for (union_info.alternatives, 0..) |alt, i| {
        const alt_ident = switch (alt.name) {
            .tag => |t| if (t == Ident.Idx.NONE) continue else t,
            .opaque_type => |o| o,
        };
        // Compare just the idx (interned string index), not the full Ident.Idx
        // which includes attributes that may differ between pattern and type
        if (alt_ident.idx == tag_name.idx) {
            return @enumFromInt(i);
        }
    }
    return null;
}

/// Get the argument types for a constructor.
/// For nominal types with type arguments (like Try(A, B)), we need to return
/// the actual type arguments, not the backing type's unsubstituted type params.
fn getCtorArgTypes(type_store: *TypeStore, type_var: Var, tag_id: TagId) []const Var {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    if (content.unwrapTagUnion()) |tag_union| {
        const tags_slice = type_store.getTagsSlice(tag_union.tags);
        const tag_args = tags_slice.items(.args);

        const idx = @intFromEnum(tag_id);
        if (idx < tag_args.len) {
            return type_store.sliceVars(tag_args[idx]);
        }
    }

    // Follow aliases and nominal types
    switch (content) {
        .alias => |alias| {
            const backing_var = type_store.getAliasBackingVar(alias);
            return getCtorArgTypes(type_store, backing_var, tag_id);
        },
        .structure => |flat_type| switch (flat_type) {
            .nominal_type => |nominal| {
                // For parametric nominal types like Try(A, B), get args from backing type
                const nom_args = type_store.sliceNominalArgs(nominal);
                const backing_var = type_store.getNominalBackingVar(nominal);
                const backing_args = getCtorArgTypes(type_store, backing_var, tag_id);

                // TODO: Properly substitute type parameters instead of this heuristic.
                if (backing_args.len == 1 and nom_args.len > 0) {
                    const first_arg_resolved = type_store.resolveVar(backing_args[0]);
                    switch (first_arg_resolved.desc.content) {
                        .rigid, .flex => {
                            return nom_args[0..1];
                        },
                        else => {},
                    }
                }
                return backing_args;
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
fn getRecordFieldTypeByName(type_store: *TypeStore, record_type: Var, field_name: Ident.Idx) ?Var {
    const resolved = type_store.resolveVar(record_type);
    const content = resolved.desc.content;

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
                return getRecordFieldTypeByName(type_store, record.ext, field_name);
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
            const backing_var = type_store.getAliasBackingVar(alias);
            return getRecordFieldTypeByName(type_store, backing_var, field_name);
        },
        .recursion_var => |rec| {
            return getRecordFieldTypeByName(type_store, rec.structure, field_name);
        },
        else => return null,
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

/// A matrix of patterns for exhaustiveness checking.
/// Each row represents one branch, each column represents one scrutinee position.
pub const PatternMatrix = struct {
    rows: []const []const Pattern,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, rows: []const []const Pattern) PatternMatrix {
        return .{ .rows = rows, .allocator = allocator };
    }

    pub fn isEmpty(self: PatternMatrix) bool {
        return self.rows.len == 0;
    }

    /// Get the first column of patterns
    pub fn firstColumn(self: PatternMatrix) ![]const Pattern {
        if (self.rows.len == 0) return &[_]Pattern{};
        const col = try self.allocator.alloc(Pattern, self.rows.len);
        for (self.rows, 0..) |row, i| {
            col[i] = if (row.len > 0) row[0] else .{ .anything = null };
        }
        return col;
    }
};

/// Type information for each column in the pattern matrix.
/// Used to determine inhabitedness of wildcard patterns.
pub const ColumnTypes = struct {
    /// Type variable for each column
    types: []const Var,
    /// Reference to type store for lookups
    type_store: *TypeStore,

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
    /// than the actual record type has - a known limitation of the current algorithm
    /// that treats records positionally instead of by field name.
    pub fn specializeByConstructor(
        self: ColumnTypes,
        allocator: std.mem.Allocator,
        tag_id: TagId,
        expected_arity: usize,
    ) error{ OutOfMemory, TypeError }!ColumnTypes {
        // Column types must be available. If not, it indicates a compiler bug.
        std.debug.assert(self.types.len > 0);

        // Look up the tag's payload types from types[0]
        const payload_types = getCtorArgTypes(self.type_store, self.types[0], tag_id);

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

        return .{ .types = new_types, .type_store = self.type_store };
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

        return .{ .types = new_types, .type_store = self.type_store };
    }

    /// Remove the first column type
    pub fn dropFirst(self: ColumnTypes) ColumnTypes {
        if (self.types.len == 0) {
            return .{ .types = &[_]Var{}, .type_store = self.type_store };
        }
        return .{ .types = self.types[1..], .type_store = self.type_store };
    }

    /// Expand for list specialization
    pub fn specializeForList(
        self: ColumnTypes,
        allocator: std.mem.Allocator,
        elem_count: usize,
    ) !ColumnTypes {
        if (self.types.len == 0) {
            return .{ .types = &[_]Var{}, .type_store = self.type_store };
        }

        const elem_type = getListElemType(self.type_store, self.types[0]) orelse self.types[0];

        const new_types = try allocator.alloc(Var, elem_count + self.types.len - 1);
        for (0..elem_count) |i| {
            new_types[i] = elem_type;
        }
        if (self.types.len > 1) {
            @memcpy(new_types[elem_count..], self.types[1..]);
        }

        return .{ .types = new_types, .type_store = self.type_store };
    }
};

/// Result of collecting constructors from the first column
const CollectedCtors = union(enum) {
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

/// Collect constructors from the first column of the matrix.
fn collectCtors(allocator: std.mem.Allocator, matrix: PatternMatrix) !CollectedCtors {
    if (matrix.isEmpty()) return .non_exhaustive_wildcards;

    const first_col = try matrix.firstColumn();
    if (first_col.len == 0) return .non_exhaustive_wildcards;

    // Determine what kind of patterns we have
    var found_ctor = false;
    var found_list = false;
    var found_literal = false;
    var found_wildcard = false;
    var union_info: ?Union = null;

    for (first_col) |pat| {
        switch (pat) {
            .ctor => |c| {
                found_ctor = true;
                if (union_info == null) {
                    union_info = c.union_info;
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
            if (pat == .ctor) {
                try tag_set.put(pat.ctor.tag_id, {});
            }
        }

        var found_tags: std.ArrayList(TagId) = .empty;
        var it = tag_set.keyIterator();
        while (it.next()) |key| {
            try found_tags.append(allocator, key.*);
        }

        return .{ .ctors = .{
            .found = try found_tags.toOwnedSlice(allocator),
            .union_info = union_info.?,
        } };
    }

    if (found_list) {
        // If there's also a wildcard, it covers all remaining list lengths
        if (found_wildcard) {
            return .non_exhaustive_wildcards;
        }
        var arities: std.ArrayList(ListArity) = .empty;
        for (first_col) |pat| {
            if (pat == .list) {
                try arities.append(allocator, pat.list.arity);
            }
        }
        return .{ .lists = try arities.toOwnedSlice(allocator) };
    }

    if (found_literal) {
        // If there's also a wildcard, it covers all remaining literal values
        // So we should treat this as having wildcards rather than incomplete literals
        if (found_wildcard) {
            return .non_exhaustive_wildcards;
        }
        return .literals;
    }

    return .non_exhaustive_wildcards;
}

/// Specialize the matrix by a constructor.
/// Keeps only rows that match this constructor, expanding their arguments.
fn specializeByConstructor(
    allocator: std.mem.Allocator,
    matrix: PatternMatrix,
    tag_id: TagId,
    arity: usize,
) !PatternMatrix {
    var new_rows: std.ArrayList([]const Pattern) = .empty;

    for (matrix.rows) |row| {
        if (row.len == 0) continue;

        const first = row[0];
        const rest = row[1..];

        switch (first) {
            .ctor => |c| {
                if (@intFromEnum(c.tag_id) == @intFromEnum(tag_id)) {
                    // This row matches - expand constructor args
                    const new_row = try allocator.alloc(Pattern, c.args.len + rest.len);
                    @memcpy(new_row[0..c.args.len], c.args);
                    @memcpy(new_row[c.args.len..], rest);
                    try new_rows.append(allocator, new_row);
                }
                // Otherwise row doesn't match, skip it
            },

            .anything => {
                // Wildcard matches everything - expand with wildcards for args
                const new_row = try allocator.alloc(Pattern, arity + rest.len);
                for (0..arity) |i| {
                    new_row[i] = .{ .anything = null };
                }
                @memcpy(new_row[arity..], rest);
                try new_rows.append(allocator, new_row);
            },

            else => {}, // Literals and lists don't match constructors
        }
    }

    return PatternMatrix.init(allocator, try new_rows.toOwnedSlice(allocator));
}

/// Specialize the matrix for wildcard - keep only rows starting with wildcard
fn specializeByAnything(allocator: std.mem.Allocator, matrix: PatternMatrix) !PatternMatrix {
    var new_rows: std.ArrayList([]const Pattern) = .empty;

    for (matrix.rows) |row| {
        if (row.len == 0) continue;
        if (row[0] == .anything) {
            try new_rows.append(allocator, row[1..]);
        }
    }

    return PatternMatrix.init(allocator, try new_rows.toOwnedSlice(allocator));
}

/// Specialize the matrix by a list arity.
fn specializeByListArity(
    allocator: std.mem.Allocator,
    matrix: PatternMatrix,
    arity: ListArity,
) !PatternMatrix {
    var new_rows: std.ArrayList([]const Pattern) = .empty;

    const target_len = arity.minLen();

    for (matrix.rows) |row| {
        if (row.len == 0) continue;

        const first = row[0];
        const rest = row[1..];

        switch (first) {
            .list => |l| {
                if (l.arity.coversLength(target_len)) {
                    // This list pattern covers the target length
                    // For exact patterns, elements.len == target_len
                    // For slice patterns, we need to expand the "rest" part with wildcards
                    const new_row = try allocator.alloc(Pattern, target_len + rest.len);

                    switch (l.arity) {
                        .exact => {
                            // Exact match - just copy elements
                            @memcpy(new_row[0..l.elements.len], l.elements);
                        },
                        .slice => |s| {
                            // Slice pattern [prefix.., suffix] - expand middle with wildcards
                            // Copy prefix elements
                            @memcpy(new_row[0..s.prefix], l.elements[0..s.prefix]);
                            // Fill middle with wildcards
                            const middle_len = target_len - s.prefix - s.suffix;
                            for (s.prefix..s.prefix + middle_len) |i| {
                                new_row[i] = .{ .anything = null };
                            }
                            // Copy suffix elements
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
                // Wildcard matches all list lengths
                const new_row = try allocator.alloc(Pattern, target_len + rest.len);
                for (0..target_len) |i| {
                    new_row[i] = .{ .anything = null };
                }
                @memcpy(new_row[target_len..], rest);
                try new_rows.append(allocator, new_row);
            },

            else => {},
        }
    }

    return PatternMatrix.init(allocator, try new_rows.toOwnedSlice(allocator));
}

/// Check if the pattern matrix is exhaustive.
/// Returns a list of missing patterns (empty if exhaustive).
pub fn checkExhaustive(
    allocator: std.mem.Allocator,
    matrix: PatternMatrix,
    column_types: ColumnTypes,
) ![]const Pattern {
    const n = column_types.len();

    // Base case: empty matrix with columns to fill = not exhaustive
    if (matrix.isEmpty()) {
        if (n == 0) {
            // No more columns to check - we're exhaustive
            return &[_]Pattern{};
        }
        // Empty matrix but columns remain - return typed wildcards as missing pattern
        const missing = try allocator.alloc(Pattern, n);
        for (column_types.types, 0..) |col_type, i| {
            missing[i] = .{ .anything = col_type };
        }
        return missing;
    }

    // No more columns = we found a match, exhaustive for this path
    if (n == 0) {
        return &[_]Pattern{};
    }

    const ctors = try collectCtors(allocator, matrix);

    return switch (ctors) {
        .non_exhaustive_wildcards => {
            // Only wildcards in first column - recurse on rest
            const new_matrix = try specializeByAnything(allocator, matrix);
            const rest_types = column_types.dropFirst();
            const rest = try checkExhaustive(allocator, new_matrix, rest_types);

            if (rest.len == 0) return &[_]Pattern{};

            // Prepend typed wildcard to missing patterns
            const result = try allocator.alloc(Pattern, 1 + rest.len);
            const first_type = if (column_types.types.len > 0) column_types.types[0] else null;
            result[0] = .{ .anything = first_type };
            @memcpy(result[1..], rest);
            return result;
        },

        .ctors => |ctor_info| {
            const num_found = ctor_info.found.len;
            const num_alts = ctor_info.union_info.alternatives.len;

            if (num_found < num_alts) {
                // Not all constructors covered explicitly - check if wildcards cover the rest
                // For each missing constructor, specialize and check if exhaustive
                for (ctor_info.union_info.alternatives) |alt| {
                    var found = false;
                    for (ctor_info.found) |found_id| {
                        if (@intFromEnum(alt.tag_id) == @intFromEnum(found_id)) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        // This constructor isn't explicitly matched - check if wildcards cover it
                        const specialized = try specializeByConstructor(
                            allocator,
                            matrix,
                            alt.tag_id,
                            alt.arity,
                        );
                        const specialized_types = try column_types.specializeByConstructor(allocator, alt.tag_id, alt.arity);
                        const inner_missing = try checkExhaustive(allocator, specialized, specialized_types);

                        if (inner_missing.len > 0) {
                            // Wildcards don't cover this - check if the pattern is inhabited
                            const missing_pattern = Pattern{ .ctor = .{
                                .union_info = ctor_info.union_info,
                                .tag_id = alt.tag_id,
                                .args = inner_missing,
                            } };
                            // Only report as missing if the pattern is inhabited
                            // (e.g., Err on a Try with empty error type is uninhabited)
                            if (missing_pattern.isInhabited(column_types.type_store)) {
                                const result = try allocator.alloc(Pattern, 1);
                                result[0] = missing_pattern;
                                return result;
                            }
                            // Pattern is uninhabited, continue checking others
                        }
                        // else: wildcards cover this constructor, continue checking others
                    }
                }
                // All missing constructors are covered by wildcards
                return &[_]Pattern{};
            }

            // All constructors covered - check each one recursively
            for (ctor_info.union_info.alternatives) |alt| {
                const specialized = try specializeByConstructor(
                    allocator,
                    matrix,
                    alt.tag_id,
                    alt.arity,
                );

                const specialized_types = try column_types.specializeByConstructor(allocator, alt.tag_id, alt.arity);
                const missing = try checkExhaustive(allocator, specialized, specialized_types);

                if (missing.len > 0) {
                    // Found a missing pattern in this constructor's arguments
                    // Wrap it in this constructor
                    const args = try allocator.alloc(Pattern, alt.arity);
                    for (0..alt.arity) |i| {
                        if (i < missing.len) {
                            args[i] = missing[i];
                        } else {
                            // Get type for this argument if available
                            const arg_type = if (i < specialized_types.types.len) specialized_types.types[i] else null;
                            args[i] = .{ .anything = arg_type };
                        }
                    }

                    const missing_pattern = Pattern{ .ctor = .{
                        .union_info = ctor_info.union_info,
                        .tag_id = alt.tag_id,
                        .args = args,
                    } };

                    // Only report as missing if the pattern is inhabited
                    if (missing_pattern.isInhabited(column_types.type_store)) {
                        const result = try allocator.alloc(Pattern, 1);
                        result[0] = missing_pattern;
                        return result;
                    }
                    // Pattern is uninhabited, continue checking other constructors
                }
            }

            // All paths exhaustive
            return &[_]Pattern{};
        },

        .lists => |arities| {
            // For list patterns, we need to check various lengths
            // Build the list of lengths we need to check
            const ctors_to_check = try buildListCtorsForChecking(allocator, arities);

            for (ctors_to_check) |list_arity| {
                const specialized = try specializeByListArity(allocator, matrix, list_arity);
                const min_len = list_arity.minLen();
                const specialized_types = try column_types.specializeForList(allocator, min_len);
                const missing = try checkExhaustive(allocator, specialized, specialized_types);

                if (missing.len > 0) {
                    // Found a missing pattern
                    const elements = try allocator.alloc(Pattern, min_len);
                    for (0..min_len) |i| {
                        if (i < missing.len) {
                            elements[i] = missing[i];
                        } else {
                            // Get element type if available
                            const elem_type = if (i < specialized_types.types.len) specialized_types.types[i] else null;
                            elements[i] = .{ .anything = elem_type };
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
            // Literals have infinite domains (except Bool which is handled as ctor)
            // So literal-only patterns are never exhaustive
            const result = try allocator.alloc(Pattern, 1);
            const first_type = if (column_types.types.len > 0) column_types.types[0] else null;
            result[0] = .{ .anything = first_type };
            return result;
        },
    };
}

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

/// Main entry point: check exhaustiveness and return an error if not exhaustive.
pub fn check(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    region: Region,
    context: Context,
    rows: []const []const Pattern,
    scrutinee_type: Var,
) !?Error {
    const matrix = PatternMatrix.init(allocator, rows);

    // Initial column types: just the scrutinee type
    const initial_types = try allocator.alloc(Var, 1);
    initial_types[0] = scrutinee_type;
    const column_types = ColumnTypes{
        .types = initial_types,
        .type_store = type_store,
    };

    const missing = try checkExhaustive(allocator, matrix, column_types);

    if (missing.len > 0) {
        return .{ .incomplete = .{
            .region = region,
            .context = context,
            .missing_patterns = missing,
        } };
    }

    return null;
}

// Redundancy Checking
//
// A pattern is "useful" if it can match something that existing patterns don't.
// If a pattern is not useful, it's redundant (unreachable).

/// Check if a new pattern row is "useful" given existing rows.
/// A pattern is useful if it can match something the existing patterns don't.
/// If not useful, it's redundant.
pub fn isUseful(
    allocator: std.mem.Allocator,
    existing_matrix: PatternMatrix,
    new_row: []const Pattern,
    column_types: ColumnTypes,
) !bool {
    // Empty matrix = new row is definitely useful
    if (existing_matrix.isEmpty()) return true;

    // No more patterns to check = not useful (existing rows cover everything)
    if (new_row.len == 0) return false;

    const first = new_row[0];
    const rest = new_row[1..];

    return switch (first) {
        .ctor => |c| {
            // Specialize matrix by this constructor
            const specialized = try specializeByConstructor(
                allocator,
                existing_matrix,
                c.tag_id,
                c.args.len,
            );
            const specialized_types = try column_types.specializeByConstructor(allocator, c.tag_id, c.args.len);

            // Check if args + rest is useful in specialized matrix
            const extended_row = try allocator.alloc(Pattern, c.args.len + rest.len);
            @memcpy(extended_row[0..c.args.len], c.args);
            @memcpy(extended_row[c.args.len..], rest);

            return isUseful(allocator, specialized, extended_row, specialized_types);
        },

        .anything => {
            // Check if matrix is complete (covers all constructors)
            const ctors = try collectCtors(allocator, existing_matrix);

            switch (ctors) {
                .non_exhaustive_wildcards => {
                    // Not complete - check if any existing wildcard already covers this
                    const specialized = try specializeByAnything(allocator, existing_matrix);
                    const rest_types = column_types.dropFirst();
                    return isUseful(allocator, specialized, rest, rest_types);
                },

                .ctors => |ctor_info| {
                    // TODO: Properly handle flex extensions instead of assuming wildcards useful.
                    if (ctor_info.union_info.has_flex_extension) {
                        return true;
                    }

                    const num_found = ctor_info.found.len;
                    const num_alts = ctor_info.union_info.alternatives.len;

                    if (num_found < num_alts) {
                        // Not all constructors covered - wildcard might be useful
                        // Check the default (wildcard) path
                        const specialized = try specializeByAnything(allocator, existing_matrix);
                        const rest_types = column_types.dropFirst();
                        return isUseful(allocator, specialized, rest, rest_types);
                    }

                    // All constructors covered - check each one
                    for (ctor_info.union_info.alternatives) |alt| {
                        const specialized = try specializeByConstructor(
                            allocator,
                            existing_matrix,
                            alt.tag_id,
                            alt.arity,
                        );
                        const specialized_types = try column_types.specializeByConstructor(allocator, alt.tag_id, alt.arity);

                        const extended = try allocator.alloc(Pattern, alt.arity + rest.len);
                        for (0..alt.arity) |i| {
                            extended[i] = .{ .anything = null };
                        }
                        @memcpy(extended[alt.arity..], rest);

                        if (try isUseful(allocator, specialized, extended, specialized_types)) {
                            return true;
                        }
                    }
                    return false;
                },

                .lists => |arities| {
                    // For list patterns, check if wildcard is useful for any length
                    const ctors_to_check = try buildListCtorsForChecking(allocator, arities);

                    for (ctors_to_check) |list_arity| {
                        const specialized = try specializeByListArity(allocator, existing_matrix, list_arity);
                        const min_len = list_arity.minLen();
                        const specialized_types = try column_types.specializeForList(allocator, min_len);

                        const extended = try allocator.alloc(Pattern, min_len + rest.len);
                        for (0..min_len) |i| {
                            extended[i] = .{ .anything = null };
                        }
                        @memcpy(extended[min_len..], rest);

                        if (try isUseful(allocator, specialized, extended, specialized_types)) {
                            return true;
                        }
                    }
                    return false;
                },

                .literals => {
                    // Literals have infinite domains, so wildcard is always useful
                    return true;
                },
            }
        },

        .literal => |lit| {
            // Keep rows that match this literal or are wildcards
            var matching_rows: std.ArrayList([]const Pattern) = .empty;

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

            const filtered = PatternMatrix.init(
                allocator,
                try matching_rows.toOwnedSlice(allocator),
            );
            const rest_types = column_types.dropFirst();
            return isUseful(allocator, filtered, rest, rest_types);
        },

        .list => |l| {
            // For list patterns, we need to check usefulness at various lengths
            // that the pattern covers. A slice pattern like [1, ..] covers
            // lengths 1, 2, 3, etc. so we need to check if it's useful at any of those.
            switch (l.arity) {
                .exact => {
                    // Exact pattern - only check at this one length
                    const specialized = try specializeByListArity(allocator, existing_matrix, l.arity);
                    const specialized_types = try column_types.specializeForList(allocator, l.elements.len);

                    const extended_row = try allocator.alloc(Pattern, l.elements.len + rest.len);
                    @memcpy(extended_row[0..l.elements.len], l.elements);
                    @memcpy(extended_row[l.elements.len..], rest);

                    return isUseful(allocator, specialized, extended_row, specialized_types);
                },
                .slice => |s| {
                    // Slice pattern - need to check at multiple lengths
                    // Collect arities from existing matrix to know what lengths to check
                    const first_col = try existing_matrix.firstColumn();
                    var arities_list: std.ArrayList(ListArity) = .empty;
                    for (first_col) |p| {
                        if (p == .list) {
                            try arities_list.append(allocator, p.list.arity);
                        }
                    }
                    // Also add our own arity
                    try arities_list.append(allocator, l.arity);

                    const check_arities = try buildListCtorsForChecking(allocator, arities_list.items);

                    for (check_arities) |check_arity| {
                        // Only check lengths that our pattern covers
                        const len = check_arity.minLen();
                        if (!l.arity.coversLength(len)) continue;

                        const specialized = try specializeByListArity(allocator, existing_matrix, check_arity);
                        const specialized_types = try column_types.specializeForList(allocator, len);

                        // Expand our pattern to this length
                        const extended_row = try allocator.alloc(Pattern, len + rest.len);
                        // Copy prefix elements
                        @memcpy(extended_row[0..s.prefix], l.elements[0..s.prefix]);
                        // Fill middle with wildcards
                        const middle_len = len - s.prefix - s.suffix;
                        for (s.prefix..s.prefix + middle_len) |i| {
                            extended_row[i] = .{ .anything = null };
                        }
                        // Copy suffix elements
                        if (s.suffix > 0) {
                            const suffix_start = l.elements.len - s.suffix;
                            @memcpy(extended_row[s.prefix + middle_len .. len], l.elements[suffix_start..]);
                        }
                        @memcpy(extended_row[len..], rest);

                        if (try isUseful(allocator, specialized, extended_row, specialized_types)) {
                            return true;
                        }
                    }
                    return false;
                },
            }
        },
    };
}

/// Result of checking rows for redundancy
pub const RedundancyResult = struct {
    /// Non-redundant rows (useful patterns)
    non_redundant_rows: []const []const Pattern,
    /// Indices of redundant branches
    redundant_indices: []const u32,
    /// Regions of redundant branches
    redundant_regions: []const Region,
};

/// Process pattern rows and identify redundant patterns.
/// Returns non-redundant rows and information about which rows were redundant.
pub fn checkRedundancy(
    allocator: std.mem.Allocator,
    rows: []const UnresolvedRow,
    reified_patterns: []const []const Pattern,
    column_types: ColumnTypes,
) !RedundancyResult {
    var non_redundant: std.ArrayList([]const Pattern) = .empty;
    var redundant_indices: std.ArrayList(u32) = .empty;
    var redundant_regions: std.ArrayList(Region) = .empty;

    for (rows, reified_patterns) |row, patterns| {
        // Rows with guards are always considered useful (guard might fail at runtime)
        const is_useful = row.guard == .has_guard or
            try isUseful(allocator, PatternMatrix.init(allocator, non_redundant.items), patterns, column_types);

        if (is_useful) {
            try non_redundant.append(allocator, patterns);
        } else {
            try redundant_indices.append(allocator, row.branch_index);
            try redundant_regions.append(allocator, row.region);
        }
    }

    return .{
        .non_redundant_rows = try non_redundant.toOwnedSlice(allocator),
        .redundant_indices = try redundant_indices.toOwnedSlice(allocator),
        .redundant_regions = try redundant_regions.toOwnedSlice(allocator),
    };
}

// =============================================================================
// 1-Phase On-Demand Reification
//
// These functions work with UnresolvedPattern directly, reifying on-demand
// during usefulness checking. This allows type errors to propagate immediately
// rather than being silently skipped.
// =============================================================================

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
    ident_store: *const Ident.Store,
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
                    const union_result = try getUnionFromType(allocator, type_store, ident_store, first_col_type);
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
        if (found_wildcard) {
            return .non_exhaustive_wildcards;
        }
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
    ident_store: *const Ident.Store,
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
    const ctors = try collectCtorsSketched(allocator, type_store, ident_store, matrix, first_col_type);

    return switch (ctors) {
        .non_exhaustive_wildcards => {
            const new_matrix = try specializeByAnythingSketched(allocator, matrix);
            const rest_types = column_types.dropFirst();
            const rest = try checkExhaustiveSketched(allocator, type_store, ident_store, new_matrix, rest_types);

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
                        const inner_missing = try checkExhaustiveSketched(allocator, type_store, ident_store, specialized, specialized_types);

                        if (inner_missing.len > 0) {
                            const missing_pattern = Pattern{ .ctor = .{
                                .union_info = ctor_info.union_info,
                                .tag_id = alt.tag_id,
                                .args = inner_missing,
                            } };
                            if (missing_pattern.isInhabited(column_types.type_store)) {
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
                const missing = try checkExhaustiveSketched(allocator, type_store, ident_store, specialized, specialized_types);

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

                    if (missing_pattern.isInhabited(column_types.type_store)) {
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

            for (ctors_to_check) |list_arity| {
                const specialized = try specializeByListAritySketched(allocator, matrix, list_arity);
                const min_len = list_arity.minLen();
                const specialized_types = try column_types.specializeForList(allocator, min_len);
                const missing = try checkExhaustiveSketched(allocator, type_store, ident_store, specialized, specialized_types);

                if (missing.len > 0) {
                    const elements = try allocator.alloc(Pattern, min_len);
                    for (0..min_len) |i| {
                        if (i < missing.len) {
                            elements[i] = missing[i];
                        } else {
                            const elem_type = if (i < specialized_types.types.len) specialized_types.types[i] else null;
                            elements[i] = .{ .anything = elem_type };
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
    ident_store: *const Ident.Store,
    existing_matrix: SketchedMatrix,
    new_row: []const UnresolvedPattern,
    column_types: ColumnTypes,
) ReifyError!bool {
    // Empty matrix = new row is definitely useful
    if (existing_matrix.isEmpty()) return true;

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
            const union_result = try getUnionFromType(allocator, type_store, ident_store, first_col_type);
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

            return isUsefulSketched(allocator, type_store, ident_store, specialized, extended_row, specialized_types);
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

            return isUsefulSketched(allocator, type_store, ident_store, specialized, extended_row, specialized_types);
        },

        .anything => {
            // Check if matrix is complete (covers all constructors)
            const ctors = try collectCtorsSketched(allocator, type_store, ident_store, existing_matrix, first_col_type);

            switch (ctors) {
                .non_exhaustive_wildcards => {
                    const specialized = try specializeByAnythingSketched(allocator, existing_matrix);
                    const rest_types = column_types.dropFirst();
                    return isUsefulSketched(allocator, type_store, ident_store, specialized, rest, rest_types);
                },

                .ctors => |ctor_info| {
                    if (ctor_info.union_info.has_flex_extension) {
                        return true;
                    }

                    const num_found = ctor_info.found.len;
                    const num_alts = ctor_info.union_info.alternatives.len;

                    if (num_found < num_alts) {
                        const specialized = try specializeByAnythingSketched(allocator, existing_matrix);
                        const rest_types = column_types.dropFirst();
                        return isUsefulSketched(allocator, type_store, ident_store, specialized, rest, rest_types);
                    }

                    // All constructors covered - check each one
                    for (ctor_info.union_info.alternatives) |alt| {
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

                        if (try isUsefulSketched(allocator, type_store, ident_store, specialized, extended, specialized_types)) {
                            return true;
                        }
                    }
                    return false;
                },

                .lists => |arities| {
                    const ctors_to_check = try buildListCtorsForChecking(allocator, arities);

                    for (ctors_to_check) |list_arity| {
                        const specialized = try specializeByListAritySketched(allocator, existing_matrix, list_arity);
                        const min_len = list_arity.minLen();
                        const specialized_types = try column_types.specializeForList(allocator, min_len);

                        const extended = try allocator.alloc(UnresolvedPattern, min_len + rest.len);
                        for (0..min_len) |i| {
                            extended[i] = .anything;
                        }
                        @memcpy(extended[min_len..], rest);

                        if (try isUsefulSketched(allocator, type_store, ident_store, specialized, extended, specialized_types)) {
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
            return isUsefulSketched(allocator, type_store, ident_store, filtered, rest, rest_types);
        },

        .list => |l| {
            switch (l.arity) {
                .exact => {
                    const specialized = try specializeByListAritySketched(allocator, existing_matrix, l.arity);
                    const specialized_types = try column_types.specializeForList(allocator, l.elements.len);

                    const extended_row = try allocator.alloc(UnresolvedPattern, l.elements.len + rest.len);
                    @memcpy(extended_row[0..l.elements.len], l.elements);
                    @memcpy(extended_row[l.elements.len..], rest);

                    return isUsefulSketched(allocator, type_store, ident_store, specialized, extended_row, specialized_types);
                },
                .slice => |s| {
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

                        if (try isUsefulSketched(allocator, type_store, ident_store, specialized, extended_row, specialized_types)) {
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
    /// Indices of redundant branches
    redundant_indices: []const u32,
    /// Regions of redundant branches
    redundant_regions: []const Region,
};

/// Process sketched pattern rows and identify redundant patterns.
/// Uses on-demand reification for type checking.
pub fn checkRedundancySketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    ident_store: *const Ident.Store,
    rows: []const UnresolvedRow,
    column_types: ColumnTypes,
) ReifyError!RedundancyResultSketched {
    var non_redundant: std.ArrayList([]const UnresolvedPattern) = .empty;
    var redundant_indices: std.ArrayList(u32) = .empty;
    var redundant_regions: std.ArrayList(Region) = .empty;

    for (rows) |row| {
        // Rows with guards are always considered useful (guard might fail at runtime)
        const matrix = SketchedMatrix.init(allocator, non_redundant.items);
        const is_useful = row.guard == .has_guard or
            try isUsefulSketched(allocator, type_store, ident_store, matrix, row.patterns, column_types);

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
    };
}

// =============================================================================
// High-level Integration API
// =============================================================================
//
// These functions provide a simpler interface for the type checker to call.

/// Result of exhaustiveness and redundancy checking
pub const CheckResult = struct {
    /// Whether the match is exhaustive
    is_exhaustive: bool,
    /// Missing patterns if not exhaustive (for error messages)
    missing_patterns: []const Pattern,
    /// Indices of redundant branches
    redundant_indices: []const u32,
    /// Regions of redundant branches
    redundant_regions: []const Region,

    /// Free all allocated memory in the result
    pub fn deinit(self: CheckResult, allocator: std.mem.Allocator) void {
        for (self.missing_patterns) |pat| {
            freePattern(allocator, pat);
        }
        allocator.free(self.missing_patterns);
        allocator.free(self.redundant_indices);
        allocator.free(self.redundant_regions);
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
    ident_store: *const Ident.Store,
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
    };

    // Phase 2: Check redundancy with on-demand reification
    // Patterns are reified as needed when type information is required
    const redundancy = try checkRedundancySketched(
        arena_alloc,
        type_store,
        ident_store,
        sketched.rows,
        column_types,
    );

    // Phase 3: Check exhaustiveness on non-redundant patterns
    const sketched_matrix = SketchedMatrix.init(arena_alloc, redundancy.non_redundant_rows);
    const missing = try checkExhaustiveSketched(
        arena_alloc,
        type_store,
        ident_store,
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
