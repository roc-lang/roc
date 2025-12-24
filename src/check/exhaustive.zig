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

const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;

/// A pattern for exhaustiveness checking.
/// This is a simplified representation focused only on what matters for coverage analysis.
pub const Pattern = union(enum) {
    /// Matches anything (wildcard, identifier binding)
    anything,
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
};

/// Identifies a tag within a union
pub const TagId = enum(u16) {
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
pub const RenderAs = enum {
    tag,
    opaque_type,
    record,
    tuple,
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
            // Compare indices directly. Note: since StringLiteral.Store doesn't
            // deduplicate, different indices could represent the same string.
            // This means some redundant patterns may not be detected, but we
            // won't produce false positives.
            .str => |as| as == b.str,
        };
    }
};

/// Errors detected during exhaustiveness checking
pub const Error = union(enum) {
    /// Match expression doesn't cover all cases
    incomplete: Incomplete,
    /// A branch can never be reached
    redundant: Redundant,
    /// A pattern can never match (e.g., matching uninhabited type)
    unmatchable: Unmatchable,

    pub const Incomplete = struct {
        region: Region,
        context: Context,
        missing_patterns: []const Pattern,
    };

    pub const Redundant = struct {
        overall_region: Region,
        branch_region: Region,
        index: u32, // 0-based branch index
    };

    pub const Unmatchable = struct {
        overall_region: Region,
        branch_region: Region,
        index: u32,
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

            for (destructs, 0..) |destruct_idx, i| {
                const destruct = store.getRecordDestruct(destruct_idx);
                const sub_pattern_idx = destruct.kind.toPatternIdx();
                args[i] = try convertPattern(allocator, store, sub_pattern_idx);
            }

            const alternatives = try allocator.alloc(CtorInfo, 1);
            alternatives[0] = .{
                .name = .{ .tag = Ident.Idx.NONE },
                .tag_id = @enumFromInt(0),
                .arity = destructs.len,
            };

            return .{ .known_ctor = .{
                .union_info = .{
                    .alternatives = alternatives,
                    .render_as = .record,
                },
                .tag_id = @enumFromInt(0),
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
                .tag_id = @enumFromInt(0),
                .arity = elem_indices.len,
            };

            return .{ .known_ctor = .{
                .union_info = .{
                    .alternatives = alternatives,
                    .render_as = .tuple,
                },
                .tag_id = @enumFromInt(0),
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
                    .tag_id = @enumFromInt(0),
                    .arity = 2,
                };

                break :blk UnresolvedPattern{ .known_ctor = .{
                    .union_info = .{
                        .alternatives = alternatives,
                        .render_as = .guard,
                    },
                    .tag_id = @enumFromInt(0),
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
