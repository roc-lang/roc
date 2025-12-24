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
//! 1. **Sketching**: During canonicalization, patterns are converted to "sketched"
//!    patterns without full type information.
//!
//! 2. **Reification & Checking**: During type checking, sketched patterns are "reified"
//!    with full type information, then checked using a pattern matrix algorithm.
//!
//! ## References
//!
//! - [Warnings for Pattern Matching](http://moscova.inria.fr/~maranget/papers/warn/warn.pdf)
//! - Original Rust implementation in `crates/compiler/exhaustive/`

const std = @import("std");
const collections = @import("collections");
const base = @import("base");

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
