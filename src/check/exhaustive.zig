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
const collections = @import("collections");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const base = @import("base");
const builtins = @import("builtins");
const i128h = builtins.compiler_rt_128;
const Can = @import("can");
const types = @import("types");
const reporting = @import("reporting");
const problem = @import("problem.zig");

const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const TypeStore = types.Store;
const Var = types.Var;

fn exhaustiveInvariant(comptime message: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(message, args);
    }
    unreachable;
}

/// Builtin type identifiers needed for special-casing in exhaustiveness checking.
/// These types have special backing representations that would incorrectly appear uninhabited.
/// Memo of declaration openings performed during one exhaustiveness entry
/// point: one instantiated backing per (declaration, resolved arg roots).
/// Without it, walkers that follow a recursive nominal's backing would open a
/// FRESH copy at every level (each containing a new recursive application),
/// so their seen-sets never converge.
pub const NominalOpenCache = struct {
    entries: std.ArrayListUnmanaged(Entry) = .empty,
    /// Resolved argument roots of every entry, contiguous per entry.
    args: std.ArrayListUnmanaged(Var) = .empty,
    /// Entry index per (declaration, resolved argument roots), hashed and
    /// compared through `entries` and `args`. The walkers never unify, so a
    /// root resolved when its entry was recorded is still that argument's
    /// root for the rest of the entry point.
    index: std.HashMapUnmanaged(u32, void, EntryContext, std.hash_map.default_max_load_percentage) = .empty,
    allocator: std.mem.Allocator,

    const Entry = struct {
        decl: types.NominalDecl.Idx,
        args_start: u32,
        args_len: u32,
        opened: Var,
    };

    /// One opening request: a declaration and its resolved argument roots.
    const Key = struct {
        decl: types.NominalDecl.Idx,
        args: []const Var,
    };

    fn hashKey(key: Key) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.decl);
        for (key.args) |arg| std.hash.autoHash(&hasher, arg);
        return hasher.final();
    }

    fn entryKey(self: *const NominalOpenCache, entry_index: u32) Key {
        const entry = self.entries.items[entry_index];
        return .{
            .decl = entry.decl,
            .args = self.args.items[entry.args_start..][0..entry.args_len],
        };
    }

    fn keysEql(lhs: Key, rhs: Key) bool {
        if (lhs.decl != rhs.decl) return false;
        if (lhs.args.len != rhs.args.len) return false;
        for (lhs.args, rhs.args) |lhs_arg, rhs_arg| {
            if (lhs_arg != rhs_arg) return false;
        }
        return true;
    }

    const EntryContext = struct {
        cache: *const NominalOpenCache,

        pub fn hash(self: EntryContext, entry_index: u32) u64 {
            return hashKey(self.cache.entryKey(entry_index));
        }

        pub fn eql(self: EntryContext, lhs: u32, rhs: u32) bool {
            return keysEql(self.cache.entryKey(lhs), self.cache.entryKey(rhs));
        }
    };

    const KeyContext = struct {
        cache: *const NominalOpenCache,

        pub fn hash(_: KeyContext, key: Key) u64 {
            return hashKey(key);
        }

        pub fn eql(self: KeyContext, key: Key, entry_index: u32) bool {
            return keysEql(key, self.cache.entryKey(entry_index));
        }
    };

    /// An empty cache; owns nothing until an opening is recorded.
    pub fn init(allocator: std.mem.Allocator) NominalOpenCache {
        return .{ .allocator = allocator };
    }

    /// Free the cache's entry and argument lists and its index.
    pub fn deinit(self: *NominalOpenCache) void {
        self.index.deinit(self.allocator);
        self.entries.deinit(self.allocator);
        self.args.deinit(self.allocator);
    }

    /// The recorded opening for a declaration applied to these resolved
    /// argument roots, if one exists.
    fn lookup(self: *const NominalOpenCache, key: Key) ?Var {
        const entry_index = self.index.getKeyAdapted(key, KeyContext{ .cache = self }) orelse return null;
        return self.entries.items[entry_index].opened;
    }

    /// Record an opening whose resolved argument roots already occupy
    /// `args[args_start..]`.
    fn record(self: *NominalOpenCache, decl: types.NominalDecl.Idx, args_start: u32, opened: Var) std.mem.Allocator.Error!void {
        const entry_index: u32 = @intCast(self.entries.items.len);
        try self.entries.append(self.allocator, .{
            .decl = decl,
            .args_start = args_start,
            .args_len = @intCast(self.args.items.len - args_start),
            .opened = opened,
        });
        errdefer _ = self.entries.pop();
        try self.index.putNoClobberContext(self.allocator, entry_index, {}, EntryContext{ .cache = self });
    }
};

/// Identifiers and per-entry-point state the exhaustiveness analysis needs
/// from its caller: builtin type names for classifying nominal types, the
/// module's identifier store, and the nominal opening memo.
pub const BuiltinIdents = struct {
    /// The module's identifier store (needed by the declaration-backed
    /// opening operation, which instantiates backing templates).
    idents: *const Ident.Store,
    /// Opening memo for this exhaustiveness entry point (see
    /// `NominalOpenCache`).
    open_cache: *NominalOpenCache,
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
    /// Unqualified numeric type identifiers (U8, I8, etc)
    u8: Ident.Idx,
    i8: Ident.Idx,
    u16: Ident.Idx,
    i16: Ident.Idx,
    u32: Ident.Idx,
    i32: Ident.Idx,
    u64: Ident.Idx,
    i64: Ident.Idx,
    u128: Ident.Idx,
    i128: Ident.Idx,
    f32: Ident.Idx,
    f64: Ident.Idx,
    dec: Ident.Idx,
    /// List type identifiers (unqualified and Builtin-qualified)
    list: Ident.Idx,
    builtin_list: Ident.Idx,

    /// Check if a nominal type is the builtin List type.
    pub fn isBuiltinListType(self: BuiltinIdents, nominal: types.NominalType) bool {
        if (!nominal.originIsBuiltin()) return false;
        const ident = nominal.ident.ident_idx;
        return ident.eql(self.list) or ident.eql(self.builtin_list);
    }

    /// Check if a nominal type is a builtin numeric type.
    /// Numeric types have [] as backing but are inhabited primitives.
    pub fn isBuiltinNumericType(self: BuiltinIdents, nominal: types.NominalType) bool {
        return self.isBuiltinNumericIdent(nominal.ident.ident_idx);
    }

    /// Check if an ident refers to a builtin numeric type.
    pub fn isBuiltinNumericIdent(self: BuiltinIdents, ident: Ident.Idx) bool {
        // Numeric types are builtin and have reserved names; treat them as builtin
        // regardless of origin module to avoid false "uninhabited" errors.
        return ident.eql(self.u8_type) or
            ident.eql(self.i8_type) or
            ident.eql(self.u16_type) or
            ident.eql(self.i16_type) or
            ident.eql(self.u32_type) or
            ident.eql(self.i32_type) or
            ident.eql(self.u64_type) or
            ident.eql(self.i64_type) or
            ident.eql(self.u128_type) or
            ident.eql(self.i128_type) or
            ident.eql(self.f32_type) or
            ident.eql(self.f64_type) or
            ident.eql(self.dec_type) or
            ident.eql(self.u8) or
            ident.eql(self.i8) or
            ident.eql(self.u16) or
            ident.eql(self.i16) or
            ident.eql(self.u32) or
            ident.eql(self.i32) or
            ident.eql(self.u64) or
            ident.eql(self.i64) or
            ident.eql(self.u128) or
            ident.eql(self.i128) or
            ident.eql(self.f32) or
            ident.eql(self.f64) or
            ident.eql(self.dec);
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
    pub fn ordinal(self: HumanIndex, allocator: std.mem.Allocator) Allocator.Error![]const u8 {
        const n = self.toHuman();
        const suffix = switch (n % 100) {
            11, 12, 13 => "th",
            else => switch (n % 10) {
                // spellchecker:off
                1 => "st",
                2 => "nd",
                3 => "rd",
                else => "th",
                // spellchecker:on
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

    /// Check if this pattern can ever match a value (is inhabited).
    /// A pattern is uninhabited if it matches a type with no possible values,
    /// such as an empty tag union or a constructor with uninhabited arguments.
    pub fn isInhabited(self: Pattern, type_store: *TypeStore, builtin_idents: BuiltinIdents) error{OutOfMemory}!bool {
        return self.isInhabitedWithKnownEmpty(type_store, builtin_idents, &.{});
    }

    /// Every node of the pattern must be inhabited; nodes are checked in
    /// source order from an explicit work list.
    pub fn isInhabitedWithKnownEmpty(
        self: Pattern,
        type_store: *TypeStore,
        builtin_idents: BuiltinIdents,
        known_empty_vars: []const Var,
    ) error{OutOfMemory}!bool {
        var pending: std.ArrayList(Pattern) = .empty;
        defer pending.deinit(type_store.gpa);
        try pending.append(type_store.gpa, self);
        while (pending.pop()) |pattern| {
            const children: []const Pattern = switch (pattern) {
                .anything => |maybe_type| {
                    // Wildcards without type info should only occur in intermediate patterns
                    // during matrix specialization, which should never be checked for inhabitedness.
                    // Missing patterns should always have type info from ColumnTypes.
                    const type_var = maybe_type orelse unreachable;
                    if (!try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, type_var, known_empty_vars)) return false;
                    continue;
                },

                // Literals are always inhabited (match their specific value)
                .literal => continue,

                .ctor => |c| blk: {
                    // An empty closed union is uninhabited
                    if (c.union_info.alternatives.len == 0 and !c.union_info.has_flex_extension) {
                        return false;
                    }
                    // All arguments must be inhabited for the pattern to be inhabited
                    break :blk c.args;
                },

                // All elements must be inhabited
                .list => |l| l.elements,
            };
            const start = pending.items.len;
            try pending.appendSlice(type_store.gpa, children);
            std.mem.reverse(Pattern, pending.items[start..]);
        }
        return true;
    }
};

/// Identifies a tag within a union
pub const TagId = enum(u32) {
    /// The first/only constructor in a single-constructor type (records, tuples, etc.)
    only = 0,
    _,

    pub fn toInt(self: TagId) u32 {
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
    /// Record fields in specialization order, paired with the exact types the
    /// checker judged for their nested sub-patterns. For an optional field,
    /// this is the binder's `Try(payload, [MissingField])`, not the raw payload
    /// stored on the scrutinee row.
    record: RecordColumns,
    /// Tuple
    tuple,
    /// Guard synthetic constructor
    guard,
};

/// Parallel record-column metadata used while specializing nested patterns.
pub const RecordColumns = struct {
    names: []const Ident.Idx,
    types: []const Var,
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
    /// A literal whose exact digits live in the module env's numeral table,
    /// identified by an id `NumeralKeyInterner` assigns per distinct digit
    /// spelling: branches repeating the same spelling compare equal (so the
    /// duplicate-branch warning fires), while spelling variants of one value
    /// (`1.5` vs `1.50`) stay distinct—sound for usefulness, since a
    /// literal never covers another pattern.
    exact_numeral: u32,

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
            .exact_numeral => |an| an == b.exact_numeral,
        };
    }
};

/// Severity of exhaustiveness errors. The canonical severity enum lives in
/// `reporting.Severity`; exhaustiveness only ever produces `runtime_error`
/// (an incomplete match crashes if reached) or `warning`.
pub const Severity = reporting.Severity;

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
    /// Matches a refutable string interpolation predicate.
    str_pattern,
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

/// Assigns each exact-path numeral literal pattern a value-identity id from
/// its recorded digit facts: identical spellings intern to the same id, so
/// `Literal.eql` detects duplicate branches without carrying digit slices.
/// Identity is the verbatim digit string (`1.5` ≠ `1.50`), matching
/// `NumeralInfo.keyBytes`' deliberate no-normalization design. A pattern with
/// no recorded digits gets a fresh unique id—the conservative pre-interner
/// occurrence identity.
pub const NumeralKeyInterner = struct {
    module_env: *const Can.ModuleEnv,
    map: std.StringHashMapUnmanaged(u32) = .empty,
    next_id: u32 = 0,

    /// `allocator` must be the same arena the surrounding check uses: interned
    /// key bytes live in it until the check completes, and are never freed
    /// individually.
    fn idFor(
        self: *NumeralKeyInterner,
        allocator: std.mem.Allocator,
        pattern_idx: CirPattern.Idx,
    ) error{OutOfMemory}!u32 {
        const literal = self.module_env.numeralLiteralForNode(Can.ModuleEnv.nodeIdxFrom(pattern_idx)) orelse
            return self.freshId();
        const exact = self.module_env.exactNumeral(literal);

        // Unambiguous key: length-prefixed `before` digits, then `after`
        // digits, then scale and sign/fractional flags.
        var key: std.ArrayList(u8) = .empty;
        try key.ensureTotalCapacity(allocator, 4 + exact.before.len + exact.after.len + 5);
        key.appendSliceAssumeCapacity(&u32LeBytes(@intCast(exact.before.len)));
        key.appendSliceAssumeCapacity(exact.before);
        key.appendSliceAssumeCapacity(exact.after);
        key.appendSliceAssumeCapacity(&u32LeBytes(exact.scale));
        key.appendAssumeCapacity(@as(u8, @intFromBool(exact.is_negative)) |
            (@as(u8, @intFromBool(exact.is_fractional)) << 1));

        const entry = try self.map.getOrPut(allocator, key.items);
        if (!entry.found_existing) entry.value_ptr.* = self.freshId();
        return entry.value_ptr.*;
    }

    fn freshId(self: *NumeralKeyInterner) u32 {
        const id = self.next_id;
        self.next_id += 1;
        return id;
    }

    fn u32LeBytes(value: u32) [4]u8 {
        return .{
            @truncate(value),
            @truncate(value >> 8),
            @truncate(value >> 16),
            @truncate(value >> 24),
        };
    }
};

/// Convert a CIR pattern to an unresolved pattern for exhaustiveness checking.
///
/// This extracts the structure of the pattern. Tag patterns are left with just
/// their tag name; the full union type will be resolved during type checking.
/// Subpatterns are converted from an explicit work list, in source order, each
/// into the slot its parent allocated for it.
pub fn convertPattern(
    allocator: std.mem.Allocator,
    store: *const NodeStore,
    numeral_keys: *NumeralKeyInterner,
    pattern_idx: CirPattern.Idx,
) error{OutOfMemory}!UnresolvedPattern {
    var result: UnresolvedPattern = undefined;
    var pending: std.ArrayList(ConvertPatternItem) = .empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, .{ .pattern = pattern_idx, .dest = &result });
    while (pending.pop()) |item| {
        const start = pending.items.len;
        try convertPatternNode(allocator, store, numeral_keys, item, &pending);
        std.mem.reverse(ConvertPatternItem, pending.items[start..]);
    }
    return result;
}

const ConvertPatternItem = struct {
    pattern: CirPattern.Idx,
    dest: *UnresolvedPattern,
};

/// Convert one pattern node into `item.dest`, queueing each subpattern for
/// the slot allocated for it.
fn convertPatternNode(
    allocator: std.mem.Allocator,
    store: *const NodeStore,
    numeral_keys: *NumeralKeyInterner,
    item: ConvertPatternItem,
    pending: *std.ArrayList(ConvertPatternItem),
) error{OutOfMemory}!void {
    const pattern_idx = item.pattern;
    const pattern = store.getPattern(pattern_idx);

    item.dest.* = node: switch (pattern) {
        // Simple binding patterns match anything
        .assign, .var_assign, .underscore => .anything,

        // As patterns: convert the inner pattern
        .as => |p| return try pending.append(allocator, .{ .pattern = p.pattern, .dest = item.dest }),

        // Tag application: unknown union type, will be resolved later
        .applied_tag => |p| {
            const arg_indices = store.slicePatterns(p.args);
            const args = try allocator.alloc(UnresolvedPattern, arg_indices.len);
            for (arg_indices, 0..) |arg_idx, i| {
                try pending.append(allocator, .{ .pattern = arg_idx, .dest = &args[i] });
            }
            break :node .{ .ctor = .{
                .tag_name = p.name,
                .args = args,
            } };
        },

        // List patterns
        .list => |p| {
            const elem_indices = store.slicePatterns(p.patterns);
            const elements = try allocator.alloc(UnresolvedPattern, elem_indices.len);
            for (elem_indices, 0..) |elem_idx, i| {
                try pending.append(allocator, .{ .pattern = elem_idx, .dest = &elements[i] });
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

            break :node .{ .list = .{
                .arity = arity,
                .elements = elements,
            } };
        },

        // Record destructure: single-constructor type
        .record_destructure => |p| {
            const destructs = store.sliceRecordDestructs(p.destructs);
            const args = try allocator.alloc(UnresolvedPattern, destructs.len);
            const field_names = try allocator.alloc(Ident.Idx, destructs.len);
            const field_types = try allocator.alloc(Var, destructs.len);

            for (destructs, 0..) |destruct_idx, i| {
                const destruct = store.getRecordDestruct(destruct_idx);
                field_names[i] = destruct.label;
                const sub_pattern_idx = destruct.kind.toPatternIdx();
                field_types[i] = Can.ModuleEnv.varFrom(sub_pattern_idx);
                try pending.append(allocator, .{ .pattern = sub_pattern_idx, .dest = &args[i] });
            }

            const alternatives = try allocator.alloc(CtorInfo, 1);
            alternatives[0] = .{
                .name = .{ .tag = Ident.Idx.NONE },
                .tag_id = .only,
                .arity = destructs.len,
            };

            break :node .{ .known_ctor = .{
                .union_info = .{
                    .alternatives = alternatives,
                    .render_as = .{ .record = .{ .names = field_names, .types = field_types } },
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
                try pending.append(allocator, .{ .pattern = elem_idx, .dest = &args[i] });
            }

            const alternatives = try allocator.alloc(CtorInfo, 1);
            alternatives[0] = .{
                .name = .{ .tag = Ident.Idx.NONE },
                .tag_id = .only,
                .arity = elem_indices.len,
            };

            break :node .{ .known_ctor = .{
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
                .i128 => break :node .{ .literal = .{ .int = p.value.toI128() } },
                .u128 => break :node .{ .literal = .{ .uint = @bitCast(p.value.bytes) } },
            }
        },

        .num_from_numeral_literal => {
            break :node .{ .literal = .{ .exact_numeral = try numeral_keys.idFor(allocator, pattern_idx) } };
        },

        // Decimal literals
        .small_dec_literal => |p| {
            break :node .{ .literal = .{ .decimal = p.value.toRocDec().num } };
        },
        .dec_literal => |p| {
            break :node .{ .literal = .{ .decimal = p.value.num } };
        },

        // Float literals
        .frac_f32_literal => |p| {
            break :node .{ .literal = .{ .float = @bitCast(@as(f64, p.value)) } };
        },
        .frac_f64_literal => |p| {
            break :node .{ .literal = .{ .float = @bitCast(p.value) } };
        },

        // String literals
        .str_literal => |p| {
            break :node .{ .literal = .{ .str = p.literal } };
        },
        .str_interpolation => {
            break :node .str_pattern;
        },

        // Nominal patterns: convert the backing pattern
        .nominal => |p| return try pending.append(allocator, .{ .pattern = p.backing_pattern, .dest = item.dest }),
        .nominal_external => |p| return try pending.append(allocator, .{ .pattern = p.backing_pattern, .dest = item.dest }),

        // Runtime errors match anything since we won't reach them
        .runtime_error => .anything,
        .deferred_import_ref => exhaustiveInvariant("deferred import reference pattern reached exhaustiveness checking", .{}),
    };
}

/// Convert all branches of a match expression to unresolved pattern rows.
///
/// Each branch can have multiple patterns (OR patterns like `A | B => ...`),
/// so we create one row per pattern, not per branch.
pub fn convertMatchBranches(
    allocator: std.mem.Allocator,
    store: *const NodeStore,
    numeral_keys: *NumeralKeyInterner,
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

            const converted = try convertPattern(allocator, store, numeral_keys, bp.pattern);

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

// Pattern Resolution
//
// These functions resolve unresolved patterns to concrete patterns using type information.
// In the 1-phase design, resolution happens on-demand during usefulness checking,
// and type errors are propagated immediately rather than silently skipped.

/// Errors that can occur during pattern resolution.
/// These indicate type mismatches that prevent exhaustiveness checking.
pub const PatternResolveError = error{
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
/// The explicit declaration-backed opening operation (issue #9983) for
/// exhaustiveness analysis: instantiate the nominal application's backing
/// template with its actual args substituted for the declaration's formals.
/// The instantiated copy is analysis-only scratch in the type store (the
/// checker backfills regions for it after each exhaustiveness entry point).
/// Returns null only for invalid declarations whose error was already reported.
fn openNominalBacking(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    nominal: types.NominalType,
) error{OutOfMemory}!?Var {
    const decl_idx = type_store.lookupNominalDecl(nominal) orelse
        exhaustiveInvariant("exhaustiveness nominal opening referenced a missing declaration", .{});
    const decl = type_store.getNominalDecl(decl_idx);
    if (!decl.isValid()) return null;

    const args = type_store.sliceNominalArgs(nominal);
    const cache = builtin_idents.open_cache;

    // One opening per (declaration, resolved arg roots) per entry point, so
    // recursive backings converge onto a fixed graph the walkers' seen-sets
    // can terminate on. The resolved roots are staged at the end of the
    // cache's argument list so a hit costs no separate allocation.
    const args_start: u32 = @intCast(cache.args.items.len);
    for (args) |arg| {
        try cache.args.append(cache.allocator, type_store.resolveVar(arg).var_);
    }
    const key: NominalOpenCache.Key = .{
        .decl = decl_idx,
        .args = cache.args.items[args_start..],
    };
    if (cache.lookup(key)) |opened| {
        cache.args.shrinkRetainingCapacity(args_start);
        return opened;
    }

    var var_map = collections.DenseMap(Var, Var).init(type_store.gpa);
    defer var_map.deinit();
    const opened = try types.instantiate.instantiateNominalBacking(
        type_store,
        builtin_idents.idents,
        &var_map,
        decl,
        args,
        .outermost,
        .instantiation,
    );

    try cache.record(decl_idx, args_start, opened);

    return opened;
}

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
                    const backing_var = (try openNominalBacking(type_store, builtin_idents, nominal)) orelse return .not_a_union;
                    return getUnionFromType(allocator, type_store, builtin_idents, backing_var);
                },
                .record,
                .tuple,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                .empty_tag_union,
                => return .not_a_union,
            }
        },
        .field_presence, .err => {},
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
                    .record,
                    .tuple,
                    .nominal_type,
                    .fn_pure,
                    .fn_effectful,
                    .fn_unbound,
                    .empty_record,
                    => {
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
            .field_presence, .err => {
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
// - `isTypeInhabitedWithKnownEmpty`: Check if a single type is inhabited.
// - `areAllTypesInhabitedWithKnownEmpty`: Check if all types in a slice are inhabited (AND semantics).
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

    /// Leave a nominal declaration after checking its instantiated backing.
    /// This makes non-regular recursive applications converge even when each
    /// application has fresh type argument roots.
    leave_nominal: types.NominalDecl.Idx,
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
fn varIsKnownEmpty(type_store: *TypeStore, known_empty_vars: []const Var, type_var: Var) bool {
    const resolved_var = type_store.resolveVar(type_var).var_;
    for (known_empty_vars) |known_empty_var| {
        const resolved_known_empty = type_store.resolveVar(known_empty_var).var_;
        if (@intFromEnum(resolved_var) == @intFromEnum(resolved_known_empty)) return true;
    }
    return false;
}

fn isTypeInhabitedWithKnownEmpty(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
    known_empty_vars: []const Var,
) error{OutOfMemory}!bool {
    // Use a seen set to detect cycles in recursive types
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(type_store.gpa);

    var active_nominals: std.AutoHashMapUnmanaged(types.NominalDecl.Idx, void) = .empty;
    defer active_nominals.deinit(type_store.gpa);

    const gpa = type_store.gpa;

    // Work-list of items to process (LIFO order)
    var work_list: std.ArrayList(WorkItem) = .empty;
    defer work_list.deinit(gpa);

    // Stack of boolean results from completed checks
    var results: std.ArrayList(bool) = .empty;
    defer results.deinit(gpa);

    var record_field_vars: std.ArrayList(Var) = .empty;
    defer record_field_vars.deinit(gpa);

    // Start with the initial type
    try work_list.append(gpa, .{ .check_type = type_var });

    while (work_list.pop()) |item| {
        switch (item) {
            .check_type => |var_to_check| {
                const resolved = type_store.resolveVar(var_to_check);
                const resolved_var = resolved.var_;
                const content = resolved.desc.content;

                if (varIsKnownEmpty(type_store, known_empty_vars, resolved_var)) {
                    try results.append(gpa, false);
                    continue;
                }

                // Cycle detection: if we've seen this resolved variable before,
                // treat it as inhabited. Cycles in recursive types are considered
                // inhabited (if we got here, there must be a non-recursive path).
                const gop = try seen.getOrPut(gpa, resolved_var);
                if (gop.found_existing) {
                    try results.append(gpa, true);
                    continue;
                }

                switch (content) {
                    // Flex and rigid variables are unconstrained - assume
                    // inhabited. A presence fact is atomic and never a value
                    // type reached here; treat it as inhabited for safety.
                    .flex, .rigid, .field_presence => try results.append(gpa, true),

                    // Error types are treated as inhabited (we don't want to cascade errors)
                    .err => try results.append(gpa, true),

                    // Aliases - check the backing type
                    .alias => |alias| {
                        if (builtin_idents.isBuiltinNumericIdent(alias.ident.ident_idx)) {
                            try results.append(gpa, true);
                            continue;
                        }
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
                                const decl_idx = type_store.lookupNominalDecl(nominal) orelse
                                    exhaustiveInvariant("inhabitedness referenced a missing nominal declaration", .{});
                                const active = try active_nominals.getOrPut(gpa, decl_idx);
                                if (active.found_existing) {
                                    // Match the variable-cycle rule above: a recursive
                                    // path is treated as inhabited.
                                    try results.append(gpa, true);
                                } else if (try openNominalBacking(type_store, builtin_idents, nominal)) |backing_var| {
                                    try work_list.append(gpa, .{ .leave_nominal = decl_idx });
                                    try work_list.append(gpa, .{ .check_type = backing_var });
                                } else {
                                    _ = active_nominals.remove(decl_idx);
                                    // Unresolvable/invalid declaration: treat as inhabited
                                    try results.append(gpa, true);
                                }
                            }
                        },

                        // Records - all fields must be inhabited (AND semantics)
                        .record => |record| {
                            const presences = type_store.getRecordFieldsSlice(record.fields).items(.presence);
                            try pushRecordAndWork(gpa, type_store, &work_list, &record_field_vars, presences);
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
                } else if (varIsKnownEmpty(type_store, known_empty_vars, ext_var)) {
                    try results.append(gpa, false);
                } else {
                    // Check if extension is open (flex/rigid)
                    const is_open = try isExtensionOpen(type_store, ext_var);
                    try results.append(gpa, is_open);
                }
            },

            .leave_nominal => |decl_idx| {
                const removed = active_nominals.remove(decl_idx);
                std.debug.assert(removed);
            },
        }
    }

    // Final result should be on top of the results stack
    return if (results.items.len > 0) results.items[0] else true;
}

fn isUnresolvedUnboundFlex(flex: types.Flex) bool {
    return flex.constraints.len() == 0;
}

fn isUnresolvedUnboundRigid(rigid: types.Rigid) bool {
    return rigid.constraints.len() == 0 and rigid.name.attributes.ignored;
}

fn appendUniqueVar(gpa: std.mem.Allocator, out: *std.ArrayList(Var), var_: Var) Allocator.Error!void {
    const resolved_var = var_;
    for (out.items) |existing| {
        if (@intFromEnum(existing) == @intFromEnum(resolved_var)) return;
    }
    try out.append(gpa, resolved_var);
}

/// Check constructor payload inhabitedness.
///
/// This differs from general type inhabitedness only for unresolved unbound
/// variables: as a constructor payload, one means no value of that payload type
/// has been constructed, so the constructor is not constructible unless a later
/// constraint has already resolved it to a concrete type.
pub fn isCtorPayloadTypeInhabited(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
) error{OutOfMemory}!bool {
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(type_store.gpa);
    return isCtorPayloadTypeInhabitedHelp(type_store, builtin_idents, type_var, &seen);
}

fn isCtorPayloadTypeInhabitedHelp(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
    seen: *std.AutoHashMapUnmanaged(Var, void),
) error{OutOfMemory}!bool {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    switch (content) {
        .flex => |flex| return !isUnresolvedUnboundFlex(flex),
        .rigid => |rigid| return !isUnresolvedUnboundRigid(rigid),
        .field_presence, .err => return true,
        .alias, .structure => {},
    }

    const gop = try seen.getOrPut(type_store.gpa, resolved.var_);
    if (gop.found_existing) return true;

    return switch (content) {
        .flex, .rigid, .err, .field_presence => unreachable,
        .alias => |alias| blk: {
            if (builtin_idents.isBuiltinNumericIdent(alias.ident.ident_idx)) {
                break :blk true;
            }
            break :blk try isCtorPayloadTypeInhabitedHelp(
                type_store,
                builtin_idents,
                type_store.getAliasBackingVar(alias),
                seen,
            );
        },
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union => false,
            .empty_record => true,
            .tag_union => |tag_union| try isCtorPayloadTagUnionInhabited(
                type_store,
                builtin_idents,
                tag_union,
                seen,
            ),
            .nominal_type => |nominal| blk: {
                if (builtin_idents.isBuiltinNumericType(nominal)) {
                    break :blk true;
                }
                const backing_var = (try openNominalBacking(type_store, builtin_idents, nominal)) orelse break :blk true;
                break :blk try isCtorPayloadTypeInhabitedHelp(
                    type_store,
                    builtin_idents,
                    backing_var,
                    seen,
                );
            },
            .record => |record| blk: {
                for (0..record.fields.count) |offset| {
                    const field_presence = type_store.getRecordFieldAt(record.fields, @intCast(offset)).presence;
                    if (!fieldIsAlwaysPresent(type_store, field_presence)) continue;
                    const field_var = field_presence.typeVar();
                    if (!try isCtorPayloadTypeInhabitedHelp(type_store, builtin_idents, field_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (0..tuple.elems.count) |offset| {
                    const elem_var = type_store.getVarAt(tuple.elems, @intCast(offset));
                    if (!try isCtorPayloadTypeInhabitedHelp(type_store, builtin_idents, elem_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .fn_pure, .fn_effectful, .fn_unbound => true,
        },
    };
}

fn isCtorPayloadTagUnionInhabited(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    initial_tag_union: types.TagUnion,
    seen: *std.AutoHashMapUnmanaged(Var, void),
) error{OutOfMemory}!bool {
    var seen_exts: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen_exts.deinit(type_store.gpa);

    var current_tags = initial_tag_union.tags;
    var current_ext = initial_tag_union.ext;

    while (true) {
        for (0..current_tags.count) |tag_offset| {
            const args_range = type_store.getTagAt(current_tags, @intCast(tag_offset)).args;
            var all_args_inhabited = true;
            for (0..args_range.count) |arg_offset| {
                const arg_var = type_store.getVarAt(args_range, @intCast(arg_offset));
                if (!try isCtorPayloadTypeInhabitedHelp(type_store, builtin_idents, arg_var, seen)) {
                    all_args_inhabited = false;
                    break;
                }
            }
            if (all_args_inhabited) return true;
        }

        const ext_resolved = type_store.resolveVar(current_ext);
        const gop = try seen_exts.getOrPut(type_store.gpa, ext_resolved.var_);
        if (gop.found_existing) return false;

        switch (ext_resolved.desc.content) {
            .flex => |flex| return !isUnresolvedUnboundFlex(flex),
            .rigid => |rigid| return !isUnresolvedUnboundRigid(rigid),
            .structure => |flat_type| switch (flat_type) {
                .tag_union => |ext_tag_union| {
                    current_tags = ext_tag_union.tags;
                    current_ext = ext_tag_union.ext;
                },
                .empty_tag_union => return false,
                .record,
                .tuple,
                .nominal_type,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                => return false,
            },
            .alias => |alias| {
                current_ext = type_store.getAliasBackingVar(alias);
            },
            .field_presence, .err => return false,
        }
    }
}

/// Collects unresolved unbound type variables that make a constructor payload uninhabited.
pub fn collectCtorPayloadBlockers(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
    out: *std.ArrayList(Var),
) error{OutOfMemory}!void {
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(type_store.gpa);
    try collectCtorPayloadBlockersHelp(type_store, builtin_idents, type_var, out, &seen);
}

fn collectCtorPayloadBlockersHelp(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
    out: *std.ArrayList(Var),
    seen: *std.AutoHashMapUnmanaged(Var, void),
) error{OutOfMemory}!void {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    switch (content) {
        .flex => |flex| {
            if (isUnresolvedUnboundFlex(flex)) {
                try appendUniqueVar(type_store.gpa, out, resolved.var_);
            }
            return;
        },
        .rigid => |rigid| {
            if (isUnresolvedUnboundRigid(rigid)) {
                try appendUniqueVar(type_store.gpa, out, resolved.var_);
            }
            return;
        },
        .field_presence, .err => return,
        .alias, .structure => {},
    }

    const gop = try seen.getOrPut(type_store.gpa, resolved.var_);
    if (gop.found_existing) return;

    switch (content) {
        .flex, .rigid, .err, .field_presence => unreachable,
        .alias => |alias| {
            if (builtin_idents.isBuiltinNumericIdent(alias.ident.ident_idx)) return;
            try collectCtorPayloadBlockersHelp(
                type_store,
                builtin_idents,
                type_store.getAliasBackingVar(alias),
                out,
                seen,
            );
        },
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union, .empty_record => {},
            .tag_union => |tag_union| try collectCtorPayloadTagUnionBlockers(
                type_store,
                builtin_idents,
                tag_union,
                out,
                seen,
            ),
            .nominal_type => |nominal| {
                if (builtin_idents.isBuiltinNumericType(nominal)) return;
                const backing_var = (try openNominalBacking(type_store, builtin_idents, nominal)) orelse return;
                try collectCtorPayloadBlockersHelp(
                    type_store,
                    builtin_idents,
                    backing_var,
                    out,
                    seen,
                );
            },
            .record => |record| {
                for (0..record.fields.count) |offset| {
                    const field_presence = type_store.getRecordFieldAt(record.fields, @intCast(offset)).presence;
                    if (!fieldIsAlwaysPresent(type_store, field_presence)) continue;
                    const field_var = field_presence.typeVar();
                    if (!try isCtorPayloadTypeInhabited(type_store, builtin_idents, field_var)) {
                        try collectCtorPayloadBlockersHelp(type_store, builtin_idents, field_var, out, seen);
                    }
                }
            },
            .tuple => |tuple| {
                for (0..tuple.elems.count) |offset| {
                    const elem_var = type_store.getVarAt(tuple.elems, @intCast(offset));
                    if (!try isCtorPayloadTypeInhabited(type_store, builtin_idents, elem_var)) {
                        try collectCtorPayloadBlockersHelp(type_store, builtin_idents, elem_var, out, seen);
                    }
                }
            },
            .fn_pure, .fn_effectful, .fn_unbound => {},
        },
    }
}

fn collectCtorPayloadTagUnionBlockers(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    initial_tag_union: types.TagUnion,
    out: *std.ArrayList(Var),
    seen: *std.AutoHashMapUnmanaged(Var, void),
) error{OutOfMemory}!void {
    var seen_exts: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen_exts.deinit(type_store.gpa);

    var current_tags = initial_tag_union.tags;
    var current_ext = initial_tag_union.ext;

    while (true) {
        for (0..current_tags.count) |tag_offset| {
            const args_range = type_store.getTagAt(current_tags, @intCast(tag_offset)).args;
            var all_args_inhabited = true;
            for (0..args_range.count) |arg_offset| {
                const arg_var = type_store.getVarAt(args_range, @intCast(arg_offset));
                if (!try isCtorPayloadTypeInhabited(type_store, builtin_idents, arg_var)) {
                    all_args_inhabited = false;
                    break;
                }
            }
            if (!all_args_inhabited) {
                for (0..args_range.count) |arg_offset| {
                    const arg_var = type_store.getVarAt(args_range, @intCast(arg_offset));
                    if (!try isCtorPayloadTypeInhabited(type_store, builtin_idents, arg_var)) {
                        try collectCtorPayloadBlockersHelp(type_store, builtin_idents, arg_var, out, seen);
                    }
                }
            }
        }

        const ext_resolved = type_store.resolveVar(current_ext);
        const gop = try seen_exts.getOrPut(type_store.gpa, ext_resolved.var_);
        if (gop.found_existing) return;

        switch (ext_resolved.desc.content) {
            .flex => |flex| {
                if (isUnresolvedUnboundFlex(flex)) {
                    try appendUniqueVar(type_store.gpa, out, ext_resolved.var_);
                }
                return;
            },
            .rigid => |rigid| {
                if (isUnresolvedUnboundRigid(rigid)) {
                    try appendUniqueVar(type_store.gpa, out, ext_resolved.var_);
                }
                return;
            },
            .structure => |flat_type| switch (flat_type) {
                .tag_union => |ext_tag_union| {
                    current_tags = ext_tag_union.tags;
                    current_ext = ext_tag_union.ext;
                },
                .empty_tag_union => return,
                .record,
                .tuple,
                .nominal_type,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                => return,
            },
            .alias => |alias| {
                current_ext = type_store.getAliasBackingVar(alias);
            },
            .field_presence, .err => return,
        }
    }
}

fn isKnownAbsentCtorPayloadTypeInhabited(
    allocator: Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
) error{OutOfMemory}!bool {
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(type_store.gpa);

    return isKnownAbsentCtorPayloadTypeInhabitedHelp(allocator, type_store, builtin_idents, type_var, &seen);
}

fn isKnownAbsentCtorPayloadTypeInhabitedHelp(
    allocator: Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
    seen: *std.AutoHashMapUnmanaged(Var, void),
) error{OutOfMemory}!bool {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    switch (content) {
        .flex => return false,
        .rigid => |rigid| return !rigid.name.attributes.ignored,
        .field_presence, .err => return true,
        .alias, .structure => {},
    }

    const gop = try seen.getOrPut(type_store.gpa, resolved.var_);
    if (gop.found_existing) return true;

    return switch (content) {
        .flex, .rigid, .err, .field_presence => unreachable,
        .alias => |alias| blk: {
            if (builtin_idents.isBuiltinNumericIdent(alias.ident.ident_idx)) break :blk true;
            break :blk try isKnownAbsentCtorPayloadTypeInhabitedHelp(
                allocator,
                type_store,
                builtin_idents,
                type_store.getAliasBackingVar(alias),
                seen,
            );
        },
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union => false,
            .empty_record => true,
            .tag_union, .nominal_type => blk: {
                if (flat_type == .nominal_type and builtin_idents.isBuiltinNumericType(flat_type.nominal_type)) {
                    break :blk true;
                }

                const union_result = try getUnionFromType(allocator, type_store, builtin_idents, type_var);
                const union_info = switch (union_result) {
                    .success => |union_info| union_info,
                    .not_a_union => break :blk false,
                };

                for (union_info.alternatives) |alt| {
                    const arg_types = try getCtorArgTypes(type_store, builtin_idents, type_var, alt.tag_id);
                    var all_args_inhabited = true;
                    for (0..arg_types.len()) |offset| {
                        const arg_var = arg_types.get(type_store, offset);
                        if (!try isKnownAbsentCtorPayloadTypeInhabitedHelp(
                            allocator,
                            type_store,
                            builtin_idents,
                            arg_var,
                            seen,
                        )) {
                            all_args_inhabited = false;
                            break;
                        }
                    }
                    if (all_args_inhabited) break :blk true;
                }

                break :blk false;
            },
            .record => |record| blk: {
                for (0..record.fields.count) |offset| {
                    const field_presence = type_store.getRecordFieldAt(record.fields, @intCast(offset)).presence;
                    if (!fieldIsAlwaysPresent(type_store, field_presence)) continue;
                    const field_var = field_presence.typeVar();
                    if (!try isKnownAbsentCtorPayloadTypeInhabitedHelp(
                        allocator,
                        type_store,
                        builtin_idents,
                        field_var,
                        seen,
                    )) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (0..tuple.elems.count) |offset| {
                    const elem_var = type_store.getVarAt(tuple.elems, @intCast(offset));
                    if (!try isKnownAbsentCtorPayloadTypeInhabitedHelp(
                        allocator,
                        type_store,
                        builtin_idents,
                        elem_var,
                        seen,
                    )) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .fn_pure, .fn_effectful, .fn_unbound => true,
        },
    };
}

fn collectKnownAbsentCtorPayloadBlockers(
    allocator: Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
    out: *std.ArrayList(Var),
) error{OutOfMemory}!void {
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(type_store.gpa);

    try collectKnownAbsentCtorPayloadBlockersHelp(allocator, type_store, builtin_idents, type_var, out, &seen);
}

fn collectKnownAbsentCtorPayloadBlockersHelp(
    allocator: Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_var: Var,
    out: *std.ArrayList(Var),
    seen: *std.AutoHashMapUnmanaged(Var, void),
) error{OutOfMemory}!void {
    const resolved = type_store.resolveVar(type_var);
    const content = resolved.desc.content;

    switch (content) {
        .flex => {
            try appendUniqueVar(type_store.gpa, out, resolved.var_);
            return;
        },
        .rigid => |rigid| {
            if (rigid.name.attributes.ignored) {
                try appendUniqueVar(type_store.gpa, out, resolved.var_);
            }
            return;
        },
        .field_presence, .err => return,
        .alias, .structure => {},
    }

    const gop = try seen.getOrPut(type_store.gpa, resolved.var_);
    if (gop.found_existing) return;

    switch (content) {
        .flex, .rigid, .err, .field_presence => unreachable,
        .alias => |alias| {
            if (builtin_idents.isBuiltinNumericIdent(alias.ident.ident_idx)) return;
            try collectKnownAbsentCtorPayloadBlockersHelp(
                allocator,
                type_store,
                builtin_idents,
                type_store.getAliasBackingVar(alias),
                out,
                seen,
            );
        },
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union, .empty_record => {},
            .tag_union, .nominal_type => {
                if (flat_type == .nominal_type and builtin_idents.isBuiltinNumericType(flat_type.nominal_type)) {
                    return;
                }

                const union_result = try getUnionFromType(allocator, type_store, builtin_idents, type_var);
                const union_info = switch (union_result) {
                    .success => |union_info| union_info,
                    .not_a_union => return,
                };

                for (union_info.alternatives) |alt| {
                    const arg_types = try getCtorArgTypes(type_store, builtin_idents, type_var, alt.tag_id);
                    var all_args_inhabited = true;
                    for (0..arg_types.len()) |offset| {
                        const arg_var = arg_types.get(type_store, offset);
                        if (!try isKnownAbsentCtorPayloadTypeInhabitedHelp(
                            allocator,
                            type_store,
                            builtin_idents,
                            arg_var,
                            seen,
                        )) {
                            all_args_inhabited = false;
                            break;
                        }
                    }

                    if (!all_args_inhabited) {
                        for (0..arg_types.len()) |offset| {
                            const arg_var = arg_types.get(type_store, offset);
                            if (!try isKnownAbsentCtorPayloadTypeInhabited(
                                allocator,
                                type_store,
                                builtin_idents,
                                arg_var,
                            )) {
                                try collectKnownAbsentCtorPayloadBlockersHelp(
                                    allocator,
                                    type_store,
                                    builtin_idents,
                                    arg_var,
                                    out,
                                    seen,
                                );
                            }
                        }
                    }
                }
            },
            .record => |record| {
                for (0..record.fields.count) |offset| {
                    const field_presence = type_store.getRecordFieldAt(record.fields, @intCast(offset)).presence;
                    if (!fieldIsAlwaysPresent(type_store, field_presence)) continue;
                    const field_var = field_presence.typeVar();
                    if (!try isKnownAbsentCtorPayloadTypeInhabited(allocator, type_store, builtin_idents, field_var)) {
                        try collectKnownAbsentCtorPayloadBlockersHelp(allocator, type_store, builtin_idents, field_var, out, seen);
                    }
                }
            },
            .tuple => |tuple| {
                for (0..tuple.elems.count) |offset| {
                    const elem_var = type_store.getVarAt(tuple.elems, @intCast(offset));
                    if (!try isKnownAbsentCtorPayloadTypeInhabited(allocator, type_store, builtin_idents, elem_var)) {
                        try collectKnownAbsentCtorPayloadBlockersHelp(allocator, type_store, builtin_idents, elem_var, out, seen);
                    }
                }
            },
            .fn_pure, .fn_effectful, .fn_unbound => {},
        },
    }
}

/// Push work items for AND semantics: all types must be inhabited.
/// Pushes AndCombine(N) followed by CheckType for each var.
fn pushAndWork(gpa: std.mem.Allocator, work_list: *std.ArrayList(WorkItem), vars: []const Var) Allocator.Error!void {
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

/// An optional field may be absent, so the record is inhabited no matter what
/// that field's payload type is: only fields that are always present join the
/// AND. A defaulted field is always present (its default is materialized at
/// every omission site), and a presence that has not resolved to a concrete
/// kind is treated as present so inhabitedness stays conservative.
fn fieldIsAlwaysPresent(type_store: *TypeStore, presence: types.RecordField.Presence) bool {
    return switch (presence.decode()) {
        .required => true,
        .unknown => |unknown| switch (type_store.resolveVar(unknown.presence).desc.content) {
            .field_presence => |kind| switch (kind) {
                .optional => false,
                .required, .defaulted => true,
            },
            .flex, .rigid, .alias, .structure, .err => true,
        },
    };
}

fn pushRecordAndWork(
    gpa: std.mem.Allocator,
    type_store: *TypeStore,
    work_list: *std.ArrayList(WorkItem),
    field_vars: *std.ArrayList(Var),
    presences: []const types.RecordField.Presence,
) Allocator.Error!void {
    field_vars.clearRetainingCapacity();
    try field_vars.ensureTotalCapacity(gpa, presences.len);
    for (presences) |presence| {
        if (!fieldIsAlwaysPresent(type_store, presence)) continue;
        field_vars.appendAssumeCapacity(presence.typeVar());
    }
    try pushAndWork(gpa, work_list, field_vars.items);
}

/// Push work items for a tag union: OR semantics across tags, AND within each tag's args.
/// Also handles extension chain following.
fn pushTagUnionWork(gpa: std.mem.Allocator, type_store: *TypeStore, work_list: *std.ArrayList(WorkItem), initial_tag_union: types.TagUnion) Allocator.Error!void {
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
            // Cycle detected - treat as closed to be safe
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
                .record,
                .tuple,
                .nominal_type,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                => break,
            },
            .alias => |alias| {
                // Follow alias
                current_ext = type_store.getAliasBackingVar(alias);
            },
            .field_presence, .err => break,
        }
    }

    const num_tags: u32 = @intCast(all_tag_args.items.len);

    if (num_tags == 0) {
        // No tags - result depends only on whether extension is open
        // Push false, then check_open_extension records true when the extension is open
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
                .record,
                .tuple,
                .nominal_type,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                => return false,
            },
            .alias => |alias| {
                current_ext = type_store.getAliasBackingVar(alias);
            },
            .field_presence, .err => return false,
        }
    }
}

fn areAllCtorArgTypesInhabitedWithKnownEmpty(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    arg_types: CtorArgTypes,
    known_empty_vars: []const Var,
) error{OutOfMemory}!bool {
    for (0..arg_types.len()) |offset| {
        const arg_type = arg_types.get(type_store, offset);
        if (!try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, arg_type, known_empty_vars)) {
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
    payload_vars_to_close: ?*std.ArrayList(Var),
) PatternResolveError!bool {
    if (patterns.len == 0) return true;
    if (column_types.types.len == 0) return true;

    const known_empty_vars = if (payload_vars_to_close) |vars| vars.items else &.{};

    // Nested constructor arguments are checked in source order from an
    // explicit work list: each argument's type first, then its own pattern.
    var pending: std.ArrayList(SketchedInhabitedItem) = .empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, .{ .pattern = .{ .pattern = patterns[0], .type_var = column_types.types[0] } });

    while (pending.pop()) |item| {
        switch (item) {
            .arg_type => |arg_type| {
                if (!try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, arg_type, known_empty_vars)) {
                    return false; // Uninhabited argument = uninhabited pattern
                }
            },
            .pattern => |at| switch (at.pattern) {
                .ctor => |c| {
                    // Look up the union type to get tag_id and argument types
                    const union_result = try getUnionFromType(allocator, type_store, builtin_idents, at.type_var);
                    const union_info = switch (union_result) {
                        .success => |u| u,
                        .not_a_union => return error.TypeError,
                    };
                    const tag_id = findTagId(union_info, c.tag_name) orelse return error.TypeError;

                    // Get the constructor's argument types
                    const arg_types = try getCtorArgTypes(type_store, builtin_idents, at.type_var, tag_id);

                    // Check if any argument type is uninhabited, and also
                    // check nested patterns
                    const start = pending.items.len;
                    for (0..arg_types.len()) |i| {
                        const arg_type = arg_types.get(type_store, i);
                        try pending.append(allocator, .{ .arg_type = arg_type });
                        if (i < c.args.len) {
                            try pending.append(allocator, .{ .pattern = .{ .pattern = c.args[i], .type_var = arg_type } });
                        }
                    }
                    std.mem.reverse(SketchedInhabitedItem, pending.items[start..]);
                },
                // known_ctor is for records - records are always inhabited (unless they have uninhabited fields,
                // but that's checked via their field types, not the pattern structure)
                .known_ctor => {},
                .anything => {
                    // Wildcard - check if the type itself is uninhabited
                    if (!try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, at.type_var, known_empty_vars)) return false;
                },
                .literal, .str_pattern => {}, // Literals and string predicates are always inhabited
                .list => |l| {
                    // Empty list pattern is always inhabited
                    // Non-empty list patterns are uninhabited if the element type is uninhabited
                    if (l.arity.minLen() == 0) continue;

                    // Check if element type is inhabited
                    const elem_type = try getListElemType(type_store, builtin_idents, at.type_var);
                    if (!try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, elem_type, known_empty_vars)) return false;
                },
            },
        }
    }
    return true;
}

const SketchedInhabitedItem = union(enum) {
    arg_type: Var,
    pattern: struct { pattern: UnresolvedPattern, type_var: Var },
};

/// Find the tag_id for a tag name within a union.
/// Uses Ident.Idx equality directly.
fn findTagId(union_info: Union, tag_name: Ident.Idx) ?TagId {
    for (union_info.alternatives) |alt| {
        const alt_ident = switch (alt.name) {
            .tag => |t| if (t.eql(Ident.Idx.NONE)) continue else t,
            .opaque_type => |o| o,
        };
        if (alt_ident.eql(tag_name)) {
            // Return the stored tag_id, not the array position.
            // The tag_id preserves the original index for getCtorArgTypes.
            return alt.tag_id;
        }
    }
    return null;
}

/// Constructor ids of a union by constructor name, so a constructor pattern
/// resolves its id in constant time however wide the union is.
const TagIdsByName = std.AutoHashMapUnmanaged(u29, TagId);

fn tagIdsByName(allocator: std.mem.Allocator, union_info: Union) error{OutOfMemory}!TagIdsByName {
    var tag_ids: TagIdsByName = .empty;
    try tag_ids.ensureTotalCapacity(allocator, @intCast(union_info.alternatives.len));
    for (union_info.alternatives) |alt| {
        const alt_ident = ctorNameIdent(alt.name) orelse continue;
        const gop = tag_ids.getOrPutAssumeCapacity(alt_ident.idx);
        if (!gop.found_existing) gop.value_ptr.* = alt.tag_id;
    }
    return tag_ids;
}

/// Record fields collected across patterns, each field once in first-seen
/// order.
const RecordFieldUnion = struct {
    names: std.ArrayList(Ident.Idx) = .empty,
    types: std.ArrayList(Var) = .empty,
    seen: std.AutoHashMapUnmanaged(u29, void) = .empty,

    fn add(self: *RecordFieldUnion, allocator: std.mem.Allocator, name: Ident.Idx, ty: Var) error{OutOfMemory}!void {
        const gop = try self.seen.getOrPut(allocator, name.idx);
        if (gop.found_existing) return;
        try self.names.append(allocator, name);
        try self.types.append(allocator, ty);
    }
};

fn ctorNameIdent(ctor_name: CtorName) ?Ident.Idx {
    return switch (ctor_name) {
        .tag => |tag| if (tag.eql(Ident.Idx.NONE)) null else tag,
        .opaque_type => |opaque_type| opaque_type,
    };
}

fn identSetContains(idents: []const Ident.Idx, ident: Ident.Idx) bool {
    for (idents) |existing| {
        if (existing.eql(ident)) return true;
    }
    return false;
}

/// Collect uninhabited payload vars for constructors that are absent from a
/// syntactically-known constructed tag set.
///
/// This intentionally resolves each constructor's payload through the concrete
/// target type, not just the nominal backing type, so `Try(Str, e)` reports `e`
/// as the blocker for `Err(e)` rather than the backing type parameter.
pub fn collectAbsentCtorPayloadBlockersForConstructedTags(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    target_var: Var,
    constructed_tags: []const Ident.Idx,
    out: *std.ArrayList(Var),
) error{OutOfMemory}!void {
    const union_result = try getUnionFromType(allocator, type_store, builtin_idents, target_var);
    const union_info = switch (union_result) {
        .success => |union_info| union_info,
        .not_a_union => return,
    };

    for (union_info.alternatives) |alt| {
        const name = ctorNameIdent(alt.name) orelse continue;
        if (identSetContains(constructed_tags, name)) continue;

        const arg_types = try getCtorArgTypes(type_store, builtin_idents, target_var, alt.tag_id);
        for (0..arg_types.len()) |offset| {
            const arg_type = arg_types.get(type_store, offset);
            if (!try isKnownAbsentCtorPayloadTypeInhabited(allocator, type_store, builtin_idents, arg_type)) {
                try collectKnownAbsentCtorPayloadBlockers(allocator, type_store, builtin_idents, arg_type, out);
            }
        }
    }
}

/// Get the argument types for a constructor.
/// For nominal types with type arguments (like Try(A, B)), we need to return
/// the actual type arguments, not the backing type's unsubstituted type params.
///
/// IMPORTANT: This function follows extension chains to find the tag at the given index.
/// Tag unions from unification may have tags split across the main union
/// and its extension chain (e.g., [Normal, ..ext] where ext = [HasEmpty, ..]).
const CtorArgTypes = union(enum) {
    vars: Var.SafeList.Range,
    record_fields: types.RecordField.SafeMultiList.Range,
    none,

    fn len(self: CtorArgTypes) usize {
        return switch (self) {
            .vars => |range| range.count,
            .record_fields => |range| range.count,
            .none => 0,
        };
    }

    /// Read one argument through the stable range each time. Opening a nominal
    /// can append to the type store and relocate its side arrays, so recursive
    /// exhaustiveness code must not retain borrowed slices into those arrays.
    fn get(self: CtorArgTypes, type_store: *TypeStore, offset: usize) Var {
        return switch (self) {
            .vars => |range| type_store.getVarAt(range, @intCast(offset)),
            .record_fields => |range| type_store.getRecordFieldAt(range, @intCast(offset)).presence.typeVar(),
            .none => unreachable,
        };
    }
};

fn getCtorArgTypes(type_store: *TypeStore, builtin_idents: BuiltinIdents, type_var: Var, tag_id: TagId) std.mem.Allocator.Error!CtorArgTypes {
    // Aliases and nominal backings are followed in a loop.
    var current = type_var;
    while (true) {
        const resolved = type_store.resolveVar(current);
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
                // Check if the target index is in this level
                if (target_idx < current_offset + current_tags.count) {
                    const local_idx = target_idx - current_offset;
                    return .{ .vars = type_store.getTagAt(current_tags, @intCast(local_idx)).args };
                }

                // Move to the extension
                current_offset += current_tags.count;
                const ext_resolved = type_store.resolveVar(current_ext);
                const ext_var = ext_resolved.var_;

                // Cycle detection: have we seen this variable before?
                const gop = try seen_exts.getOrPut(ext_var);
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
                        .record,
                        .tuple,
                        .nominal_type,
                        .fn_pure,
                        .fn_effectful,
                        .fn_unbound,
                        .empty_record,
                        .empty_tag_union,
                        => break,
                    },
                    .alias => |alias| {
                        current_ext = type_store.getAliasBackingVar(alias);
                    },
                    .flex, .rigid, .field_presence, .err => break,
                }
            }
        }

        // Follow aliases and nominal types
        switch (content) {
            .alias => |alias| {
                current = type_store.getAliasBackingVar(alias);
                continue;
            },
            .structure => |flat_type| switch (flat_type) {
                .nominal_type => |nominal| {
                    // The opening operation instantiates the declaration's
                    // backing template with the application's actual args already
                    // substituted for its formals, so the constructor args it
                    // yields are the concrete payload types—no positional
                    // substitution needed.
                    current = (try openNominalBacking(type_store, builtin_idents, nominal)) orelse return .none;
                    continue;
                },
                .tuple => |tuple| {
                    // Tuples are single-constructor types, return the element types
                    return .{ .vars = tuple.elems };
                },
                .record => |record| {
                    // Records are single-constructor types, return the field types
                    return .{ .record_fields = record.fields };
                },
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => {},
            },
            .flex, .rigid, .field_presence, .err => {},
        }

        return .none;
    }
}

/// Get the element type of a column matched by list patterns.
///
/// Returns error.TypeError when the column's type is not the builtin List,
/// which means the list patterns did not type-check against the scrutinee.
fn getListElemType(type_store: *TypeStore, builtin_idents: BuiltinIdents, type_var: Var) PatternResolveError!Var {
    var current = type_var;
    while (true) {
        const content = type_store.resolveVar(current).desc.content;
        if (content.unwrapNominalType()) |nominal| {
            if (!builtin_idents.isBuiltinListType(nominal)) return error.TypeError;
            const args = type_store.sliceNominalArgs(nominal);
            if (args.len != 1) return error.TypeError;
            return args[0];
        }
        switch (content) {
            .alias => |alias| current = type_store.getAliasBackingVar(alias),
            .flex, .rigid, .field_presence, .structure, .err => return error.TypeError,
        }
    }
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
    /// than the actual record type has. This is a known limitation of the resolved
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
        const payload_types = try getCtorArgTypes(self.type_store, self.builtin_idents, self.types[0], tag_id);

        // For tag unions, the arity should match exactly.
        // For records, the pattern might destructure fewer fields than the actual type has.
        // Currently, we don't handle records by field name, so return TypeError to skip
        // exhaustiveness checking in that case.
        if (payload_types.len() != expected_arity) {
            return error.TypeError;
        }

        // New types: [payload_types..., self.types[1...]...]
        const new_types = try allocator.alloc(Var, payload_types.len() + self.types.len - 1);
        for (0..payload_types.len()) |offset| {
            new_types[offset] = payload_types.get(self.type_store, offset);
        }
        if (self.types.len > 1) {
            @memcpy(new_types[payload_types.len()..], self.types[1..]);
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
        record: RecordColumns,
    ) error{OutOfMemory}!ColumnTypes {
        std.debug.assert(self.types.len > 0);
        std.debug.assert(record.names.len == record.types.len);

        // The checker already judged each record field's sub-pattern against
        // its binder type. Consume that exact type here: optional destructures
        // bind `Try(payload, [MissingField])`, which cannot be reconstructed
        // from the scrutinee row's raw payload type.
        const new_types = try allocator.alloc(Var, record.types.len + self.types.len - 1);
        @memcpy(new_types[0..record.types.len], record.types);
        if (self.types.len > 1) {
            @memcpy(new_types[record.types.len..], self.types[1..]);
        }

        return .{ .types = new_types, .type_store = self.type_store, .builtin_idents = self.builtin_idents };
    }

    /// Specialize a synthetic guard constructor.
    ///
    /// Guard wrappers are not real scrutinee constructors. They add one column
    /// for the guard condition and one column for the original pattern.
    pub fn specializeByGuard(self: ColumnTypes, allocator: std.mem.Allocator) error{OutOfMemory}!ColumnTypes {
        std.debug.assert(self.types.len > 0);

        const new_types = try allocator.alloc(Var, self.types.len + 1);
        new_types[0] = self.types[0];
        new_types[1] = self.types[0];
        if (self.types.len > 1) {
            @memcpy(new_types[2..], self.types[1..]);
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

    /// Expand the first column (a list of `elem_type`) into `elem_count` element columns.
    pub fn specializeForList(
        self: ColumnTypes,
        allocator: std.mem.Allocator,
        elem_type: Var,
        elem_count: usize,
    ) Allocator.Error!ColumnTypes {
        std.debug.assert(self.types.len > 0);

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
) Allocator.Error![]const ListArity {
    // Find the maximum lengths we need to consider
    var max_exact_len: usize = 0;
    var has_slice = false;
    var max_prefix: usize = 0;
    var max_suffix: usize = 0;

    for (pattern_arities) |arity| {
        switch (arity) {
            .exact => |len| {
                max_exact_len = @max(max_exact_len, len);
            },
            .slice => |s| {
                has_slice = true;
                max_prefix = @max(max_prefix, s.prefix);
                max_suffix = @max(max_suffix, s.suffix);
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

    // Has slice patterns - check each length from 0 to the point where slices take over.
    //
    // The final slice stands for every list of at least `check_until` elements.
    // Its element columns must be the same positions at every such length, so
    // its prefix holds every pattern's prefix and its suffix holds every
    // pattern's suffix without the two overlapping. That requires
    // `check_until >= max_prefix + max_suffix`; with any less, a prefix element
    // of one pattern and a suffix element of another would share a column.
    var result: std.ArrayList(ListArity) = .empty;
    const check_until = @max(max_exact_len + 1, max_prefix + max_suffix);

    for (0..check_until) |len| {
        try result.append(allocator, .{ .exact = len });
    }

    // Add one slice pattern to cover all remaining lengths
    try result.append(allocator, .{ .slice = .{
        .prefix = check_until - max_suffix,
        .suffix = max_suffix,
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
/// Patterns are resolved on-demand when type information is needed.
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
/// When resolving fails, returns an error.
const CollectedCtorsSketched = union(enum) {
    /// Only wildcards/anything - pattern is not exhaustive by itself
    non_exhaustive_wildcards,
    /// Specific tag constructors found
    ctors: struct {
        found: []const TagId,
        /// The same constructors as `found`, for constant-time membership.
        found_set: collections.DenseMap(TagId, void),
        union_info: Union,
        /// `union_info`'s constructor ids by name.
        tag_ids: TagIdsByName,
        has_wildcards: bool,
    },
    /// List patterns found
    lists: []const ListArity,
    /// Literal patterns found (cannot be exhaustive for infinite domains)
    literals,
};

/// Collect constructors from the first column of a sketched matrix.
/// Resolves constructor patterns on-demand to get union information.
fn collectCtorsSketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    matrix: SketchedMatrix,
    first_col_type: Var,
) PatternResolveError!CollectedCtorsSketched {
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
    var all_record_fields: RecordFieldUnion = .{};
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
                    .record => |record| {
                        is_record = true;
                        for (record.names, record.types) |field, field_type| {
                            try all_record_fields.add(allocator, field, field_type);
                        }
                    },
                    .tag, .opaque_type, .tuple, .guard => {},
                }
            },
            .list => {
                found_list = true;
            },
            .literal, .str_pattern => {
                found_literal = true;
            },
            .anything => {
                found_wildcard = true;
            },
        }
    }

    if (found_ctor) {
        const tag_ids = try tagIdsByName(allocator, union_info.?);
        // Collect all unique tag IDs
        var tag_set = collections.DenseMap(TagId, void).init(allocator);
        for (first_col) |pat| {
            switch (pat) {
                .ctor => |c| {
                    const tag_id = tag_ids.get(c.tag_name.idx) orelse return error.TypeError;
                    try tag_set.put(tag_id, {});
                },
                .known_ctor => |kc| {
                    try tag_set.put(kc.tag_id, {});
                },
                .anything, .literal, .str_pattern, .list => {},
            }
        }

        var found_tags: std.ArrayList(TagId) = .empty;
        var it = tag_set.keyIterator();
        while (it.next()) |key| {
            try found_tags.append(allocator, key.*);
        }

        // For records, update union_info to include all fields
        var result_union_info = union_info.?;
        if (is_record and all_record_fields.names.items.len > 0) {
            const all_fields = try all_record_fields.names.toOwnedSlice(allocator);
            const all_types = try all_record_fields.types.toOwnedSlice(allocator);
            result_union_info.render_as = .{ .record = .{ .names = all_fields, .types = all_types } };
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
            .found_set = tag_set,
            .union_info = result_union_info,
            .tag_ids = tag_ids,
            .has_wildcards = found_wildcard,
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
        for (first_col) |p| {
            if (p == .list) {
                try arities.append(allocator, p.list.arity);
            }
        }
        try arities.append(allocator, .{ .slice = .{
            .prefix = 0,
            .suffix = 0,
        } });

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
    tag_ids_by_name: *const TagIdsByName,
) error{OutOfMemory}!SketchedMatrix {
    var new_rows: std.ArrayList([]const UnresolvedPattern) = .empty;

    // For records, get the target field names we're specializing by
    const target_fields: ?[]const Ident.Idx = switch (union_info.render_as) {
        .record => |record| record.names,
        .tag, .opaque_type, .tuple, .guard => null,
    };

    for (matrix.rows) |row| {
        if (row.len == 0) continue;

        const first = row[0];
        const rest = row[1..];

        switch (first) {
            .ctor => |c| {
                const pat_tag_id = tag_ids_by_name.get(c.tag_name.idx) orelse continue;
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
                            .record => |record| record.names,
                            .tag, .opaque_type, .tuple, .guard => &[_]Ident.Idx{}, // Shouldn't happen for records
                        };

                        // Build the new row with fields aligned to target order
                        const new_row = try allocator.alloc(UnresolvedPattern, arity + rest.len);

                        var pat_positions: std.AutoHashMapUnmanaged(u29, usize) = .empty;
                        defer pat_positions.deinit(allocator);
                        try pat_positions.ensureTotalCapacity(allocator, @intCast(pat_fields.len));
                        for (pat_fields, 0..) |pat_field, j| {
                            const gop = pat_positions.getOrPutAssumeCapacity(pat_field.idx);
                            if (!gop.found_existing) gop.value_ptr.* = j;
                        }
                        for (targets, 0..) |target_field, i| {
                            // A field the pattern doesn't destructure is a wildcard.
                            const j = pat_positions.get(target_field.idx) orelse {
                                new_row[i] = .anything;
                                continue;
                            };
                            new_row[i] = if (j < kc.args.len) kc.args[j] else .anything;
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
            .literal, .str_pattern, .list => {},
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
            .literal, .str_pattern, .ctor, .known_ctor => {},
        }
    }

    return SketchedMatrix.init(allocator, try new_rows.toOwnedSlice(allocator));
}

/// Walk a tag union's ext var chain and collect any flex ext vars.
fn collectFlexExtVars(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    type_var: Var,
    out: *std.ArrayList(Var),
) std.mem.Allocator.Error!void {
    const resolved = type_store.resolveVar(type_var);
    const tag_union = resolved.desc.content.unwrapTagUnion() orelse return;

    var current_ext = tag_union.ext;
    while (true) {
        const ext_resolved = type_store.resolveVar(current_ext);
        switch (ext_resolved.desc.content) {
            .flex => {
                try out.append(allocator, ext_resolved.var_);
                break;
            },
            .structure => |ft| switch (ft) {
                .tag_union => |ext_tu| current_ext = ext_tu.ext,
                .empty_tag_union => break,
                .record,
                .tuple,
                .nominal_type,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                => break,
            },
            .alias => |alias| {
                current_ext = type_store.getAliasBackingVar(alias);
            },
            .rigid, .field_presence, .err => break,
        }
    }
}

/// A missing row reported by `checkExhaustiveSketched`, split back into the
/// columns its first column was specialized into and the columns after it.
const SplitMissingRow = struct {
    /// Patterns for the sub-columns of the first column (constructor
    /// arguments or list elements).
    head_args: []const Pattern,
    /// Patterns for the remaining columns after the first.
    rest: []const Pattern,
};

/// Split the missing row of a matrix whose first column was specialized into
/// `head_arity` sub-columns. `column_count` is the column count before
/// specialization. A missing row holds exactly one pattern per column, so the
/// specialized row has `head_arity + column_count - 1` patterns.
fn splitMissingRow(specialized_missing: []const Pattern, head_arity: usize, column_count: usize) SplitMissingRow {
    std.debug.assert(column_count > 0);
    std.debug.assert(specialized_missing.len == head_arity + column_count - 1);
    return .{
        .head_args = specialized_missing[0..head_arity],
        .rest = specialized_missing[head_arity..],
    };
}

/// Build a missing row from the pattern for its first column and the patterns
/// for the remaining columns.
fn missingRowWithHead(allocator: std.mem.Allocator, head: Pattern, rest: []const Pattern) error{OutOfMemory}![]const Pattern {
    const row = try allocator.alloc(Pattern, 1 + rest.len);
    row[0] = head;
    @memcpy(row[1..], rest);
    return row;
}

/// The state every step of one exhaustiveness check shares.
const ExhaustiveCtx = struct {
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    ext_vars_to_close: *std.ArrayList(Var),
    ext_vars_to_keep_open: *std.ArrayList(Var),
    payload_vars_to_close: *std.ArrayList(Var),
};

/// One specialized matrix to check.
const ExhaustiveCall = struct {
    matrix: SketchedMatrix,
    column_types: ColumnTypes,
    close_open_extension: bool,
};

/// A check waiting on the missing patterns of the specialized matrix it
/// queued.
const ExhaustiveFrame = union(enum) {
    /// The first column was all wildcards: a missing row of the remaining
    /// columns gains a `_` of this type in front.
    prepend_anything: ?Var,
    ctors: ExhaustiveCtorsFrame,
    lists: ExhaustiveListsFrame,
};

/// Constructors of the first column checked one at a time. `.all` checks
/// every inhabited alternative's payloads; `.missing` checks only the
/// alternatives no row names.
const ExhaustiveCtorsFrame = struct {
    mode: enum { all, missing },
    matrix: SketchedMatrix,
    column_types: ColumnTypes,
    alternatives: []const CtorInfo,
    union_info: Union,
    tag_ids_by_name: TagIdsByName,
    first_col_type: Var,
    found_set: collections.DenseMap(TagId, void),
    close_open_extension: bool,
    index: usize = 0,
    alt: CtorInfo = undefined,
    specialized: SketchedMatrix = undefined,
    specialized_types: ColumnTypes = undefined,
};

/// List arities of the first column checked one at a time.
const ExhaustiveListsFrame = struct {
    matrix: SketchedMatrix,
    column_types: ColumnTypes,
    ctors_to_check: []const ListArity,
    elem_type: Var,
    elem_inhabited: bool,
    index: usize = 0,
    list_arity: ListArity = undefined,
    specialized: SketchedMatrix = undefined,
    specialized_types: ColumnTypes = undefined,
};

const ExhaustiveStep = union(enum) {
    done: []const Pattern,
    call: ExhaustiveCall,
};

/// Check if a sketched pattern matrix is exhaustive.
/// Resolves patterns on-demand when type information is needed.
/// Returns missing patterns as resolved Pattern for error messages.
///
/// Each specialization is a queued call and each check waiting on one is an
/// explicit frame, so nesting depth never becomes native call depth.
pub fn checkExhaustiveSketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    matrix: SketchedMatrix,
    column_types: ColumnTypes,
    ext_vars_to_close: *std.ArrayList(Var),
    ext_vars_to_keep_open: *std.ArrayList(Var),
    payload_vars_to_close: *std.ArrayList(Var),
    close_open_extension: bool,
) PatternResolveError![]const Pattern {
    const ctx: ExhaustiveCtx = .{
        .allocator = allocator,
        .type_store = type_store,
        .builtin_idents = builtin_idents,
        .ext_vars_to_close = ext_vars_to_close,
        .ext_vars_to_keep_open = ext_vars_to_keep_open,
        .payload_vars_to_close = payload_vars_to_close,
    };
    var frames: std.ArrayList(ExhaustiveFrame) = .empty;
    defer frames.deinit(allocator);

    var call: ExhaustiveCall = .{ .matrix = matrix, .column_types = column_types, .close_open_extension = close_open_extension };
    next_call: while (true) {
        var input: ?[]const Pattern = switch (try exhaustiveEnter(ctx, &frames, call)) {
            .done => |missing| missing,
            .call => |child| {
                call = child;
                continue :next_call;
            },
            .pushed => null,
        };
        while (frames.items.len > 0) {
            switch (try exhaustiveResume(ctx, &frames.items[frames.items.len - 1], input)) {
                .done => |missing| {
                    _ = frames.pop();
                    input = missing;
                },
                .call => |child| {
                    call = child;
                    continue :next_call;
                },
            }
        }
        return input.?;
    }
}

/// Start checking one matrix: answered at once, or a frame pushed that waits
/// on specialized matrices (`.call` names its first one; `.pushed` lets the
/// frame choose).
fn exhaustiveEnter(
    ctx: ExhaustiveCtx,
    frames: *std.ArrayList(ExhaustiveFrame),
    call: ExhaustiveCall,
) PatternResolveError!union(enum) { done: []const Pattern, call: ExhaustiveCall, pushed } {
    const allocator = ctx.allocator;
    const type_store = ctx.type_store;
    const builtin_idents = ctx.builtin_idents;
    const matrix = call.matrix;
    const column_types = call.column_types;
    const close_open_extension = call.close_open_extension;
    const n = column_types.len();

    // Base case: empty matrix with columns to fill = not exhaustive
    if (matrix.isEmpty()) {
        if (n == 0) {
            return .{ .done = &[_]Pattern{} };
        }
        // Return typed wildcards as missing pattern
        const missing = try allocator.alloc(Pattern, n);
        for (column_types.types, 0..) |col_type, i| {
            missing[i] = .{ .anything = col_type };
        }
        return .{ .done = missing };
    }

    if (n == 0) {
        return .{ .done = &[_]Pattern{} };
    }

    // Column types must be available for exhaustiveness checking.
    // If this assertion fails, it indicates a compiler bug - likely incomplete type inference.
    std.debug.assert(column_types.types.len > 0);
    const first_col_type = column_types.types[0];
    const ctors = try collectCtorsSketched(allocator, type_store, builtin_idents, matrix, first_col_type);

    switch (ctors) {
        .non_exhaustive_wildcards => {
            // All patterns are wildcards at this column. If the column type is an
            // open tag union, mark its ext var as keep-open so we don't close it
            // even if a different specialized branch collected it for closing.
            try collectFlexExtVars(allocator, type_store, first_col_type, ctx.ext_vars_to_keep_open);

            const new_matrix = try specializeByAnythingSketched(allocator, matrix);
            try frames.append(allocator, .{ .prepend_anything = column_types.types[0] });
            return .{ .call = .{ .matrix = new_matrix, .column_types = column_types.dropFirst(), .close_open_extension = false } };
        },

        .ctors => |ctor_info| {
            const num_found = ctor_info.found.len;
            const num_alts = ctor_info.union_info.alternatives.len;

            // If the union is open and has wildcards, mark its ext var as
            // keep-open to prevent a different specialized branch from closing it.
            if (ctor_info.union_info.has_flex_extension and ctor_info.has_wildcards) {
                try collectFlexExtVars(allocator, type_store, first_col_type, ctx.ext_vars_to_keep_open);
            }

            // Detect exhaustive open unions: all real tags covered, no wildcards.
            // The only "missing" constructor is the synthetic #Open.
            // Record the ext var for closing and recurse into payloads.
            std.debug.assert(num_found <= num_alts);
            const has_open_synthetic = if (num_alts > 0) switch (ctor_info.union_info.alternatives[num_alts - 1].name) {
                .tag => |tag| tag.isNone(),
                .opaque_type => false,
            } else false;
            const real_alternatives = if (has_open_synthetic)
                ctor_info.union_info.alternatives[0 .. num_alts - 1]
            else
                ctor_info.union_info.alternatives;
            var all_inhabited_real_ctors_found = true;
            if (ctor_info.union_info.has_flex_extension or close_open_extension) {
                for (real_alternatives) |alt| {
                    const arg_types = try getCtorArgTypes(type_store, builtin_idents, first_col_type, alt.tag_id);
                    if (!try areAllCtorArgTypesInhabitedWithKnownEmpty(type_store, builtin_idents, arg_types, ctx.payload_vars_to_close.items)) {
                        continue;
                    }

                    if (!ctor_info.found_set.contains(alt.tag_id)) {
                        all_inhabited_real_ctors_found = false;
                        break;
                    }
                }
            }

            const frame: ExhaustiveCtorsFrame = if ((ctor_info.union_info.has_flex_extension or close_open_extension) and
                has_open_synthetic and
                !ctor_info.has_wildcards and
                all_inhabited_real_ctors_found)
            blk: {
                if (ctor_info.union_info.has_flex_extension) {
                    try collectFlexExtVars(allocator, type_store, first_col_type, ctx.ext_vars_to_close);
                }

                // Check all real constructors' payloads (skip #Open synthetic).
                break :blk .{
                    .mode = .all,
                    .matrix = matrix,
                    .column_types = column_types,
                    .alternatives = real_alternatives,
                    .union_info = ctor_info.union_info,
                    .tag_ids_by_name = ctor_info.tag_ids,
                    .first_col_type = first_col_type,
                    .found_set = ctor_info.found_set,
                    .close_open_extension = close_open_extension,
                };
            } else .{
                // Check missing constructors, or when all constructors are
                // covered, check each one's payloads.
                .mode = if (num_found < num_alts) .missing else .all,
                .matrix = matrix,
                .column_types = column_types,
                .alternatives = ctor_info.union_info.alternatives,
                .union_info = ctor_info.union_info,
                .tag_ids_by_name = ctor_info.tag_ids,
                .first_col_type = first_col_type,
                .found_set = ctor_info.found_set,
                .close_open_extension = close_open_extension,
            };
            try frames.append(allocator, .{ .ctors = frame });
            return .pushed;
        },

        .lists => |arities| {
            const ctors_to_check = try buildListCtorsForChecking(allocator, arities);

            // Check if list elements are inhabited. If not, only the empty list exists.
            const elem_type = try getListElemType(type_store, builtin_idents, column_types.types[0]);
            const elem_inhabited = try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, elem_type, ctx.payload_vars_to_close.items);

            try frames.append(allocator, .{ .lists = .{
                .matrix = matrix,
                .column_types = column_types,
                .ctors_to_check = ctors_to_check,
                .elem_type = elem_type,
                .elem_inhabited = elem_inhabited,
            } });
            return .pushed;
        },

        .literals => {
            // Literal domains are infinite, so a value outside the listed literals
            // reaches no row; every column of that value is unconstrained.
            const missing = try allocator.alloc(Pattern, column_types.types.len);
            for (column_types.types, 0..) |col_type, col_index| {
                missing[col_index] = .{ .anything = col_type };
            }
            return .{ .done = missing };
        },
    }
}

/// Deliver the missing patterns of the frame's in-flight specialized matrix
/// (null when the frame was just pushed), then either finish the frame or
/// name its next specialized matrix.
fn exhaustiveResume(
    ctx: ExhaustiveCtx,
    frame: *ExhaustiveFrame,
    input: ?[]const Pattern,
) PatternResolveError!ExhaustiveStep {
    const allocator = ctx.allocator;
    const type_store = ctx.type_store;
    const builtin_idents = ctx.builtin_idents;
    switch (frame.*) {
        .prepend_anything => |first_type| {
            const rest = input.?;
            if (rest.len == 0) return .{ .done = &[_]Pattern{} };

            const result = try allocator.alloc(Pattern, 1 + rest.len);
            result[0] = .{ .anything = first_type };
            @memcpy(result[1..], rest);
            return .{ .done = result };
        },

        .ctors => |*ctors| {
            if (input) |missing| {
                const alt = ctors.alt;
                switch (ctors.mode) {
                    .all => if (missing.len > 0) {
                        const split = splitMissingRow(missing, alt.arity, ctors.column_types.len());
                        const missing_pattern = Pattern{ .ctor = .{
                            .union_info = ctors.union_info,
                            .tag_id = alt.tag_id,
                            .args = split.head_args,
                        } };

                        if (try missing_pattern.isInhabitedWithKnownEmpty(ctors.column_types.type_store, ctors.column_types.builtin_idents, ctx.payload_vars_to_close.items)) {
                            return .{ .done = try missingRowWithHead(allocator, missing_pattern, split.rest) };
                        }
                    },
                    .missing => {
                        // For arity-0 constructors in the last column with no matching rows, the
                        // specialized matrix is empty with 0 columns, which returns empty
                        // inner_missing. But we still need to report the constructor as missing.
                        // The #Open synthetic tag (arity-0 with no name) represents "possibly more
                        // constructors". For flex extensions, it's skipped because the union will be
                        // closed. For rigid extensions (from annotations), #Open represents real
                        // unknown tags and must be reported as missing.
                        const is_open_synthetic = alt.name.tag.isNone();
                        const skip_open = is_open_synthetic and (ctors.union_info.has_flex_extension or ctors.close_open_extension);
                        const is_missing = missing.len > 0 or (alt.arity == 0 and ctors.specialized.isEmpty() and !skip_open);
                        if (is_missing) {
                            const split = splitMissingRow(missing, alt.arity, ctors.column_types.len());
                            const missing_pattern = Pattern{ .ctor = .{
                                .union_info = ctors.union_info,
                                .tag_id = alt.tag_id,
                                .args = split.head_args,
                            } };
                            if (try missing_pattern.isInhabitedWithKnownEmpty(ctors.column_types.type_store, ctors.column_types.builtin_idents, ctx.payload_vars_to_close.items)) {
                                return .{ .done = try missingRowWithHead(allocator, missing_pattern, split.rest) };
                            }
                        }
                    },
                }
            }

            while (ctors.index < ctors.alternatives.len) {
                const alt = ctors.alternatives[ctors.index];
                ctors.index += 1;
                if (ctors.mode == .missing and ctors.found_set.contains(alt.tag_id)) continue;

                // Skip uninhabited constructors - they don't need to be matched
                // because no values of that constructor can exist.
                const arg_types = try getCtorArgTypes(type_store, builtin_idents, ctors.first_col_type, alt.tag_id);
                if (!try areAllCtorArgTypesInhabitedWithKnownEmpty(type_store, builtin_idents, arg_types, ctx.payload_vars_to_close.items)) {
                    continue;
                }

                const specialized = try specializeByConstructorSketched(
                    allocator,
                    ctors.matrix,
                    alt.tag_id,
                    alt.arity,
                    ctors.union_info,
                    &ctors.tag_ids_by_name,
                );

                // Use field-name-based lookup for records, positional for everything else
                const specialized_types = switch (ctors.union_info.render_as) {
                    .record => |record| try ctors.column_types.specializeByRecordPattern(allocator, record),
                    .guard => try ctors.column_types.specializeByGuard(allocator),
                    .tag, .opaque_type, .tuple => try ctors.column_types.specializeByConstructor(allocator, alt.tag_id, alt.arity),
                };
                ctors.alt = alt;
                ctors.specialized = specialized;
                ctors.specialized_types = specialized_types;
                return .{ .call = .{ .matrix = specialized, .column_types = specialized_types, .close_open_extension = false } };
            }
            return .{ .done = &[_]Pattern{} };
        },

        .lists => |*lists| {
            if (input) |missing| {
                const min_len = lists.list_arity.minLen();
                // For length-0 lists (empty list) in the last column with no matching rows,
                // the specialized matrix is empty with 0 columns, which returns empty
                // missing. But we still need to report the empty list as missing.
                const is_missing = missing.len > 0 or (min_len == 0 and lists.specialized.isEmpty());
                if (is_missing) {
                    const split = splitMissingRow(missing, min_len, lists.column_types.len());
                    return .{ .done = try missingRowWithHead(allocator, .{ .list = .{
                        .arity = lists.list_arity,
                        .elements = split.head_args,
                    } }, split.rest) };
                }
            }

            while (lists.index < lists.ctors_to_check.len) {
                const list_arity = lists.ctors_to_check[lists.index];
                lists.index += 1;
                const min_len = list_arity.minLen();

                // Skip non-empty list arities if elements are uninhabited
                if (min_len > 0 and !lists.elem_inhabited) {
                    continue;
                }

                lists.list_arity = list_arity;
                lists.specialized = try specializeByListAritySketched(allocator, lists.matrix, list_arity);
                lists.specialized_types = try lists.column_types.specializeForList(allocator, lists.elem_type, min_len);
                return .{ .call = .{ .matrix = lists.specialized, .column_types = lists.specialized_types, .close_open_extension = false } };
            }
            return .{ .done = &[_]Pattern{} };
        },
    }
}

/// One row whose usefulness against a matrix is asked.
const UsefulCall = struct {
    matrix: SketchedMatrix,
    row: []const UnresolvedPattern,
    column_types: ColumnTypes,
    close_open_extension: bool,
};

/// A row that is useful exactly when one of its specializations is, tried
/// one at a time.
const UsefulFrame = struct {
    matrix: SketchedMatrix,
    rest: []const UnresolvedPattern,
    column_types: ColumnTypes,
    first_col_type: Var,
    index: usize = 0,
    kind: union(enum) {
        /// A wildcard against fully covered constructors: every inhabited
        /// constructor, its arguments wildcards.
        ctors: struct { union_info: Union, tag_ids: TagIdsByName },
        /// A wildcard against list patterns: every list arity to check.
        lists: struct { arities: []const ListArity, elem_type: Var, elem_inhabited: bool },
        /// A slice pattern: every arity it covers.
        slice: struct { arities: []const ListArity, elements: []const UnresolvedPattern, arity: ListArity, slice: ListArity.Slice, elem_type: Var },
    },
};

/// Check if a new sketched pattern row is "useful" given existing sketched rows.
/// Resolves patterns on-demand when type information is needed.
///
/// A step that narrows to one specialization continues in the loop; one that
/// tries several keeps an explicit frame, so nesting depth never becomes
/// native call depth.
pub fn isUsefulSketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    existing_matrix: SketchedMatrix,
    new_row: []const UnresolvedPattern,
    column_types: ColumnTypes,
    payload_vars_to_close: *std.ArrayList(Var),
    close_open_extension: bool,
) PatternResolveError!bool {
    var frames: std.ArrayList(UsefulFrame) = .empty;
    defer frames.deinit(allocator);

    var call: UsefulCall = .{
        .matrix = existing_matrix,
        .row = new_row,
        .column_types = column_types,
        .close_open_extension = close_open_extension,
    };
    next_call: while (true) {
        var input: ?bool = switch (try usefulEnter(allocator, type_store, builtin_idents, &frames, call, payload_vars_to_close)) {
            .done => |useful| useful,
            .call => |child| {
                call = child;
                continue :next_call;
            },
            .pushed => null,
        };
        while (frames.items.len > 0) {
            if (input orelse false) {
                _ = frames.pop();
                continue;
            }
            if (try usefulNext(allocator, type_store, builtin_idents, &frames.items[frames.items.len - 1], payload_vars_to_close)) |child| {
                call = child;
                continue :next_call;
            }
            _ = frames.pop();
            input = false;
        }
        return input.?;
    }
}

/// The next specialization of the frame's row, or null when none is left.
fn usefulNext(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    frame: *UsefulFrame,
    payload_vars_to_close: *std.ArrayList(Var),
) PatternResolveError!?UsefulCall {
    const rest = frame.rest;
    switch (frame.kind) {
        .ctors => |*ctors| while (frame.index < ctors.union_info.alternatives.len) {
            const union_info = ctors.union_info;
            const alt = union_info.alternatives[frame.index];
            frame.index += 1;
            // Skip uninhabited constructors
            const arg_types = try getCtorArgTypes(type_store, builtin_idents, frame.first_col_type, alt.tag_id);
            if (!try areAllCtorArgTypesInhabitedWithKnownEmpty(type_store, builtin_idents, arg_types, payload_vars_to_close.items)) {
                continue;
            }

            const specialized = try specializeByConstructorSketched(
                allocator,
                frame.matrix,
                alt.tag_id,
                alt.arity,
                union_info,
                &ctors.tag_ids,
            );

            // Use field-name-based lookup for records, positional for everything else
            const specialized_types = switch (union_info.render_as) {
                .record => |record| try frame.column_types.specializeByRecordPattern(allocator, record),
                .guard => try frame.column_types.specializeByGuard(allocator),
                .tag, .opaque_type, .tuple => try frame.column_types.specializeByConstructor(allocator, alt.tag_id, alt.arity),
            };

            const extended = try allocator.alloc(UnresolvedPattern, alt.arity + rest.len);
            for (0..alt.arity) |i| {
                extended[i] = .anything;
            }
            @memcpy(extended[alt.arity..], rest);

            return .{ .matrix = specialized, .row = extended, .column_types = specialized_types, .close_open_extension = false };
        },

        .lists => |lists| while (frame.index < lists.arities.len) {
            const list_arity = lists.arities[frame.index];
            frame.index += 1;
            const min_len = list_arity.minLen();

            // Skip non-empty list arities if elements are uninhabited
            if (min_len > 0 and !lists.elem_inhabited) {
                continue;
            }

            const specialized = try specializeByListAritySketched(allocator, frame.matrix, list_arity);
            const specialized_types = try frame.column_types.specializeForList(allocator, lists.elem_type, min_len);

            const extended = try allocator.alloc(UnresolvedPattern, min_len + rest.len);
            for (0..min_len) |i| {
                extended[i] = .anything;
            }
            @memcpy(extended[min_len..], rest);

            return .{ .matrix = specialized, .row = extended, .column_types = specialized_types, .close_open_extension = false };
        },

        .slice => |slice| while (frame.index < slice.arities.len) {
            const check_arity = slice.arities[frame.index];
            frame.index += 1;
            const len = check_arity.minLen();
            if (!slice.arity.coversLength(len)) continue;
            const s = slice.slice;

            const specialized = try specializeByListAritySketched(allocator, frame.matrix, check_arity);
            const specialized_types = try frame.column_types.specializeForList(allocator, slice.elem_type, len);

            const extended_row = try allocator.alloc(UnresolvedPattern, len + rest.len);
            @memcpy(extended_row[0..s.prefix], slice.elements[0..s.prefix]);
            const middle_len = len - s.prefix - s.suffix;
            for (s.prefix..s.prefix + middle_len) |i| {
                extended_row[i] = .anything;
            }
            if (s.suffix > 0) {
                const suffix_start = slice.elements.len - s.suffix;
                @memcpy(extended_row[s.prefix + middle_len .. len], slice.elements[suffix_start..]);
            }
            @memcpy(extended_row[len..], rest);

            return .{ .matrix = specialized, .row = extended_row, .column_types = specialized_types, .close_open_extension = false };
        },
    }
    return null;
}

/// Decide one row's usefulness at once, narrow it to one specialization
/// (`.call`), or push a frame that tries several (`.pushed`).
fn usefulEnter(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    frames: *std.ArrayList(UsefulFrame),
    call: UsefulCall,
    payload_vars_to_close: *std.ArrayList(Var),
) PatternResolveError!union(enum) { done: bool, call: UsefulCall, pushed } {
    const existing_matrix = call.matrix;
    const new_row = call.row;
    const column_types = call.column_types;
    const close_open_extension = call.close_open_extension;

    // Empty matrix = new row is definitely useful, UNLESS the pattern is uninhabited
    if (existing_matrix.isEmpty()) {
        // Check if the pattern is on an uninhabited type
        // For ctor patterns, check if any argument type is uninhabited
        return .{ .done = try isSketchedPatternInhabited(allocator, type_store, builtin_idents, new_row, column_types, payload_vars_to_close) };
    }

    // No more patterns to check = not useful (existing rows cover everything)
    if (new_row.len == 0) return .{ .done = false };

    // Column types must match the pattern row length.
    // If this assertion fails, it indicates a compiler bug - likely incomplete type inference.
    std.debug.assert(column_types.types.len >= new_row.len);

    const first = new_row[0];
    const rest = new_row[1..];
    const first_col_type = column_types.types[0];

    switch (first) {
        .ctor => |c| {
            const union_result = try getUnionFromType(allocator, type_store, builtin_idents, first_col_type);
            const union_info = switch (union_result) {
                .success => |u| u,
                .not_a_union => return error.TypeError,
            };
            const tag_ids = try tagIdsByName(allocator, union_info);
            const tag_id = tag_ids.get(c.tag_name.idx) orelse return error.TypeError;

            const specialized = try specializeByConstructorSketched(
                allocator,
                existing_matrix,
                tag_id,
                c.args.len,
                union_info,
                &tag_ids,
            );
            const specialized_types = try column_types.specializeByConstructor(allocator, tag_id, c.args.len);

            const extended_row = try allocator.alloc(UnresolvedPattern, c.args.len + rest.len);
            @memcpy(extended_row[0..c.args.len], c.args);
            @memcpy(extended_row[c.args.len..], rest);

            return .{ .call = .{ .matrix = specialized, .row = extended_row, .column_types = specialized_types, .close_open_extension = false } };
        },

        .known_ctor => |kc| {
            // For records, we need to merge field sets from matrix + current pattern
            var merged_union_info = kc.union_info;
            switch (kc.union_info.render_as) {
                .record => |current_record| {
                    // Collect all unique fields from matrix patterns + current pattern
                    var all_fields: RecordFieldUnion = .{};

                    // Add current pattern's fields
                    for (current_record.names, current_record.types) |field, field_type| {
                        try all_fields.add(allocator, field, field_type);
                    }

                    // Add fields from matrix patterns
                    const first_col = try existing_matrix.firstColumn();
                    for (first_col) |pat| {
                        switch (pat) {
                            .known_ctor => |mat_kc| {
                                switch (mat_kc.union_info.render_as) {
                                    .record => |mat_record| {
                                        for (mat_record.names, mat_record.types) |field, field_type| {
                                            try all_fields.add(allocator, field, field_type);
                                        }
                                    },
                                    .tag, .opaque_type, .tuple, .guard => {},
                                }
                            },
                            .anything, .literal, .str_pattern, .ctor, .list => {},
                        }
                    }

                    // Update union_info with all fields
                    const all_fields_slice = try all_fields.names.toOwnedSlice(allocator);
                    const all_types_slice = try all_fields.types.toOwnedSlice(allocator);
                    merged_union_info.render_as = .{ .record = .{ .names = all_fields_slice, .types = all_types_slice } };
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
                .tag, .opaque_type, .tuple, .guard => {},
            }

            const arity = if (merged_union_info.alternatives.len > 0)
                merged_union_info.alternatives[0].arity
            else
                kc.args.len;

            const merged_tag_ids = try tagIdsByName(allocator, merged_union_info);
            const specialized = try specializeByConstructorSketched(
                allocator,
                existing_matrix,
                kc.tag_id,
                arity,
                merged_union_info,
                &merged_tag_ids,
            );

            // Use field-name-based lookup for records, positional for everything else
            const specialized_types = switch (merged_union_info.render_as) {
                .record => |record| try column_types.specializeByRecordPattern(allocator, record),
                .guard => try column_types.specializeByGuard(allocator),
                .tag, .opaque_type, .tuple => try column_types.specializeByConstructor(allocator, kc.tag_id, kc.args.len),
            };

            // Expand current pattern's args to match merged field set
            const extended_row = switch (merged_union_info.render_as) {
                .record => |merged_record| blk: {
                    const row = try allocator.alloc(UnresolvedPattern, arity + rest.len);
                    const current_fields = switch (kc.union_info.render_as) {
                        .record => |record| record.names,
                        .tag, .opaque_type, .tuple, .guard => &[_]Ident.Idx{},
                    };

                    // Map current pattern's args to merged field positions
                    for (merged_record.names, 0..) |merged_field, i| {
                        var found = false;
                        for (current_fields, 0..) |cur_field, j| {
                            if (cur_field.eql(merged_field)) {
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
                .tag, .opaque_type, .tuple, .guard => blk: {
                    const row = try allocator.alloc(UnresolvedPattern, kc.args.len + rest.len);
                    @memcpy(row[0..kc.args.len], kc.args);
                    @memcpy(row[kc.args.len..], rest);
                    break :blk row;
                },
            };

            return .{ .call = .{ .matrix = specialized, .row = extended_row, .column_types = specialized_types, .close_open_extension = false } };
        },

        .anything => {
            // Check if matrix is complete (covers all constructors)
            const ctors = try collectCtorsSketched(allocator, type_store, builtin_idents, existing_matrix, first_col_type);

            switch (ctors) {
                .non_exhaustive_wildcards => {
                    const specialized = try specializeByAnythingSketched(allocator, existing_matrix);
                    const rest_types = column_types.dropFirst();
                    return .{ .call = .{ .matrix = specialized, .row = rest, .column_types = rest_types, .close_open_extension = false } };
                },

                .ctors => |ctor_info| {
                    // Optimization: For flex extensions, wildcards are always useful.
                    // See comment in isUseful for detailed explanation.
                    if (ctor_info.union_info.has_flex_extension and !close_open_extension) {
                        return .{ .done = true };
                    }

                    const num_found = ctor_info.found.len;
                    const num_alts = ctor_info.union_info.alternatives.len;

                    if (num_found < num_alts) {
                        // Not all constructors covered - but check if missing constructors are all uninhabited
                        var any_missing_inhabited = false;
                        for (ctor_info.union_info.alternatives) |alt| {
                            if (!ctor_info.found_set.contains(alt.tag_id)) {
                                if (close_open_extension) {
                                    const is_open_synthetic = switch (alt.name) {
                                        .tag => |tag| tag.isNone(),
                                        .opaque_type => false,
                                    };
                                    if (is_open_synthetic) continue;
                                }

                                // This constructor is missing - check if it's inhabited
                                const arg_types = try getCtorArgTypes(type_store, builtin_idents, first_col_type, alt.tag_id);
                                var ctor_uninhabited = false;
                                for (0..arg_types.len()) |offset| {
                                    const arg_type = arg_types.get(type_store, offset);
                                    if (!try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, arg_type, payload_vars_to_close.items)) {
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
                            return .{ .done = false };
                        }

                        const specialized = try specializeByAnythingSketched(allocator, existing_matrix);
                        const rest_types = column_types.dropFirst();
                        return .{ .call = .{ .matrix = specialized, .row = rest, .column_types = rest_types, .close_open_extension = false } };
                    }

                    // All constructors covered - check each one
                    try frames.append(allocator, .{
                        .matrix = existing_matrix,
                        .rest = rest,
                        .column_types = column_types,
                        .first_col_type = first_col_type,
                        .kind = .{ .ctors = .{ .union_info = ctor_info.union_info, .tag_ids = ctor_info.tag_ids } },
                    });
                    return .pushed;
                },

                .lists => |arities| {
                    const ctors_to_check = try buildListCtorsForChecking(allocator, arities);

                    // Check if list elements are inhabited. If not, only the empty list exists.
                    const elem_type = try getListElemType(type_store, builtin_idents, column_types.types[0]);
                    const elem_inhabited = try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, elem_type, payload_vars_to_close.items);

                    try frames.append(allocator, .{
                        .matrix = existing_matrix,
                        .rest = rest,
                        .column_types = column_types,
                        .first_col_type = first_col_type,
                        .kind = .{ .lists = .{ .arities = ctors_to_check, .elem_type = elem_type, .elem_inhabited = elem_inhabited } },
                    });
                    return .pushed;
                },

                .literals => {
                    return .{ .done = true };
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
                    .str_pattern, .ctor, .known_ctor, .list => false,
                };

                if (matches) {
                    try matching_rows.append(allocator, row[1..]);
                }
            }

            const filtered = SketchedMatrix.init(allocator, try matching_rows.toOwnedSlice(allocator));
            const rest_types = column_types.dropFirst();
            return .{ .call = .{ .matrix = filtered, .row = rest, .column_types = rest_types, .close_open_extension = false } };
        },

        .str_pattern => {
            var matching_rows: std.ArrayList([]const UnresolvedPattern) = .empty;

            for (existing_matrix.rows) |row| {
                if (row.len == 0) continue;
                if (row[0] == .anything) {
                    try matching_rows.append(allocator, row[1..]);
                }
            }

            const filtered = SketchedMatrix.init(allocator, try matching_rows.toOwnedSlice(allocator));
            const rest_types = column_types.dropFirst();
            return .{ .call = .{ .matrix = filtered, .row = rest, .column_types = rest_types, .close_open_extension = false } };
        },

        .list => |l| {
            // Check if list elements are inhabited. If not, only the empty list exists.
            const elem_type = try getListElemType(type_store, builtin_idents, column_types.types[0]);
            const elem_inhabited = try isTypeInhabitedWithKnownEmpty(type_store, builtin_idents, elem_type, payload_vars_to_close.items);

            switch (l.arity) {
                .exact => {
                    // If this pattern requires elements but elements are uninhabited,
                    // this pattern can never match, so it's not useful (redundant).
                    if (l.elements.len > 0 and !elem_inhabited) {
                        return .{ .done = false };
                    }

                    const specialized = try specializeByListAritySketched(allocator, existing_matrix, l.arity);
                    const specialized_types = try column_types.specializeForList(allocator, elem_type, l.elements.len);

                    const extended_row = try allocator.alloc(UnresolvedPattern, l.elements.len + rest.len);
                    @memcpy(extended_row[0..l.elements.len], l.elements);
                    @memcpy(extended_row[l.elements.len..], rest);

                    return .{ .call = .{ .matrix = specialized, .row = extended_row, .column_types = specialized_types, .close_open_extension = false } };
                },
                .slice => |s| {
                    // Slice patterns always match at least one non-empty list (prefix + suffix elements),
                    // so if elements are uninhabited, slice patterns are never useful.
                    if (!elem_inhabited) {
                        return .{ .done = false };
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

                    try frames.append(allocator, .{
                        .matrix = existing_matrix,
                        .rest = rest,
                        .column_types = column_types,
                        .first_col_type = first_col_type,
                        .kind = .{ .slice = .{ .arities = check_arities, .elements = l.elements, .arity = l.arity, .slice = s, .elem_type = elem_type } },
                    });
                    return .pushed;
                },
            }
        },
    }
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
/// Uses on-demand resolution for type checking.
///
/// A pattern is **unmatchable** if it's on an uninhabited type (e.g., `Err(_)` on `Try(I64, [])`).
/// A pattern is **redundant** if it's covered by previous patterns (e.g., `_` after `Ok(_)` and `Err(_)`).
pub fn checkRedundancySketched(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    rows: []const UnresolvedRow,
    column_types: ColumnTypes,
    payload_vars_to_close: *std.ArrayList(Var),
    close_open_extension: bool,
) PatternResolveError!RedundancyResultSketched {
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
            payload_vars_to_close,
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
            try isUsefulSketched(allocator, type_store, builtin_idents, matrix, row.patterns, column_types, payload_vars_to_close, close_open_extension);

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
    /// Owns every slice below, and every pattern the missing patterns nest.
    arena: base.SingleThreadArena,
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
    /// Flex ext vars to close via unification with empty_tag_union.
    /// These are tag union positions where all constructors were exhaustively
    /// covered without wildcards.
    ext_vars_to_close: []const Var,
    /// Unresolved constructor payload vars to close via unification with
    /// empty_tag_union. Exhaustiveness relied on these payloads being
    /// unconstructible.
    payload_vars_to_close: []const Var,

    /// Free all allocated memory in the result
    pub fn deinit(self: CheckResult) void {
        self.arena.deinit();
    }
};

/// Perform full exhaustiveness and redundancy checking on a match expression.
///
/// This is the main entry point for the type checker.
/// Uses 1-phase on-demand resolution: patterns are converted to UnresolvedPattern
/// and resolved on-demand during checking when type information is needed.
///
/// Returns `error.TypeError` when a pattern cannot be resolved due to type issues
/// (e.g., polymorphic types, type mismatches). The caller should handle this by
/// skipping exhaustiveness error reporting for that match expression.
pub fn checkMatch(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    module_env: *const Can.ModuleEnv,
    node_store: *const NodeStore,
    builtin_idents: BuiltinIdents,
    branches_span: CIR.Expr.Match.Branch.Span,
    scrutinee_type: Var,
    overall_region: Region,
    known_empty_payload_vars: []const Var,
    scrutinee_constructors_known: bool,
) PatternResolveError!CheckResult {
    // Every allocation, results included, lives in one arena that the result
    // owns.
    var arena = base.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const arena_alloc = arena.allocator();

    // Phase 1: Convert CIR patterns to sketched (unresolved) patterns
    var numeral_keys = NumeralKeyInterner{ .module_env = module_env };
    const sketched = try convertMatchBranches(
        arena_alloc,
        node_store,
        &numeral_keys,
        branches_span,
        overall_region,
    );

    // Create initial column types for on-demand resolution
    const initial_types = try arena_alloc.alloc(Var, 1);
    initial_types[0] = scrutinee_type;
    const column_types = ColumnTypes{
        .types = initial_types,
        .type_store = type_store,
        .builtin_idents = builtin_idents,
    };

    // Phase 2: Check redundancy with on-demand resolution
    // Patterns are resolved as needed when type information is required
    var payload_vars_to_close: std.ArrayList(Var) = .empty;
    for (known_empty_payload_vars) |payload_var| {
        try appendUniqueVar(arena_alloc, &payload_vars_to_close, type_store.resolveVar(payload_var).var_);
    }

    const redundancy = try checkRedundancySketched(
        arena_alloc,
        type_store,
        builtin_idents,
        sketched.rows,
        column_types,
        &payload_vars_to_close,
        scrutinee_constructors_known,
    );

    // Phase 3: Check exhaustiveness on non-redundant patterns
    const sketched_matrix = SketchedMatrix.init(arena_alloc, redundancy.non_redundant_rows);
    var ext_vars_to_close: std.ArrayList(Var) = .empty;
    var ext_vars_to_keep_open: std.ArrayList(Var) = .empty;
    const missing = try checkExhaustiveSketched(
        arena_alloc,
        type_store,
        builtin_idents,
        sketched_matrix,
        column_types,
        &ext_vars_to_close,
        &ext_vars_to_keep_open,
        &payload_vars_to_close,
        scrutinee_constructors_known,
    );

    // Filter: remove any ext vars that should be kept open.
    // This handles cases where a specialized branch (e.g., one match arm) sees
    // all tags without wildcards, but a different branch has wildcards for the
    // same type position—meaning the union should stay open.
    var filtered_close: std.ArrayList(Var) = .empty;
    for (ext_vars_to_close.items) |close_var| {
        var dominated = false;
        for (ext_vars_to_keep_open.items) |keep_var| {
            if (@intFromEnum(close_var) == @intFromEnum(keep_var)) {
                dominated = true;
                break;
            }
        }
        if (!dominated) {
            try filtered_close.append(arena_alloc, close_var);
        }
    }

    return .{
        .arena = arena,
        .is_exhaustive = missing.len == 0,
        .missing_patterns = missing,
        .redundant_indices = redundancy.redundant_indices,
        .redundant_regions = redundancy.redundant_regions,
        .unmatchable_indices = redundancy.unmatchable_indices,
        .unmatchable_regions = redundancy.unmatchable_regions,
        .ext_vars_to_close = filtered_close.items,
        .payload_vars_to_close = payload_vars_to_close.items,
    };
}

/// Perform exhaustiveness checking for a single destructuring pattern.
///
/// This uses the same matrix algorithm as match expressions, with a single row
/// corresponding to the destructure pattern.
pub fn checkDestructure(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    module_env: *const Can.ModuleEnv,
    node_store: *const NodeStore,
    builtin_idents: BuiltinIdents,
    pattern_idx: CirPattern.Idx,
    scrutinee_type: Var,
    known_empty_payload_vars: []const Var,
    scrutinee_constructors_known: bool,
) PatternResolveError!CheckResult {
    var arena = base.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const arena_alloc = arena.allocator();

    var numeral_keys = NumeralKeyInterner{ .module_env = module_env };
    const converted = try convertPattern(arena_alloc, node_store, &numeral_keys, pattern_idx);
    const pattern_slice = try arena_alloc.alloc(UnresolvedPattern, 1);
    pattern_slice[0] = converted;

    const rows = try arena_alloc.alloc(UnresolvedRow, 1);
    rows[0] = .{
        .patterns = pattern_slice,
        .region = node_store.getPatternRegion(pattern_idx),
        .guard = .no_guard,
        .branch_index = 0,
    };

    const initial_types = try arena_alloc.alloc(Var, 1);
    initial_types[0] = scrutinee_type;
    const column_types = ColumnTypes{
        .types = initial_types,
        .type_store = type_store,
        .builtin_idents = builtin_idents,
    };

    var payload_vars_to_close: std.ArrayList(Var) = .empty;
    for (known_empty_payload_vars) |payload_var| {
        try appendUniqueVar(arena_alloc, &payload_vars_to_close, type_store.resolveVar(payload_var).var_);
    }

    const redundancy = try checkRedundancySketched(
        arena_alloc,
        type_store,
        builtin_idents,
        rows,
        column_types,
        &payload_vars_to_close,
        scrutinee_constructors_known,
    );

    const sketched_matrix = SketchedMatrix.init(arena_alloc, redundancy.non_redundant_rows);
    var ext_vars_to_close: std.ArrayList(Var) = .empty;
    var ext_vars_to_keep_open: std.ArrayList(Var) = .empty;
    const missing = try checkExhaustiveSketched(
        arena_alloc,
        type_store,
        builtin_idents,
        sketched_matrix,
        column_types,
        &ext_vars_to_close,
        &ext_vars_to_keep_open,
        &payload_vars_to_close,
        scrutinee_constructors_known,
    );

    var filtered_close: std.ArrayList(Var) = .empty;
    for (ext_vars_to_close.items) |close_var| {
        var dominated = false;
        for (ext_vars_to_keep_open.items) |keep_var| {
            if (@intFromEnum(close_var) == @intFromEnum(keep_var)) {
                dominated = true;
                break;
            }
        }
        if (!dominated) {
            try filtered_close.append(arena_alloc, close_var);
        }
    }

    return .{
        .arena = arena,
        .is_exhaustive = missing.len == 0,
        .missing_patterns = missing,
        .redundant_indices = redundancy.redundant_indices,
        .redundant_regions = redundancy.redundant_regions,
        .unmatchable_indices = redundancy.unmatchable_indices,
        .unmatchable_regions = redundancy.unmatchable_regions,
        .ext_vars_to_close = filtered_close.items,
        .payload_vars_to_close = payload_vars_to_close.items,
    };
}

/// Format a pattern for display in error messages.
const ByteList = std.array_list.Managed(u8);
const ByteListRange = problem.ExtraStringIdx;

/// Format a pattern as a string, into the provided buffer
/// Returns a rank of the inserted text
pub fn formatPattern(
    buf: *ByteList,
    ident_store: *const Ident.Store,
    string_store: *const StringLiteral.Store,
    pattern: Pattern,
) error{OutOfMemory}!ByteListRange {
    const start = buf.items.len;

    var unmanaged = buf.moveToUnmanaged();
    errdefer buf.* = unmanaged.toManaged(buf.allocator);

    var writer_alloc = std.Io.Writer.Allocating.fromArrayList(buf.allocator, &unmanaged);
    var scratch = try ByteList.initCapacity(buf.allocator, 400);
    defer scratch.deinit();

    formatPatternInto(&writer_alloc.writer, ident_store, string_store, &scratch, pattern) catch return error.OutOfMemory;

    unmanaged = writer_alloc.toArrayList();
    buf.* = unmanaged.toManaged(buf.allocator);

    const end = buf.items.len;

    return ByteListRange{
        .start = start,
        .count = end - start,
    };
}

/// Format a pattern as a string, into the provided writer. Nested patterns
/// are written from an explicit work list of literal text and subpatterns.
fn formatPatternInto(
    writer: *std.Io.Writer,
    ident_store: *const Ident.Store,
    string_store: *const StringLiteral.Store,
    scratch: *ByteList,
    pattern: Pattern,
) error{ OutOfMemory, WriteFailed }!void {
    const gpa = scratch.allocator;
    var pending: std.ArrayList(FormatPatternItem) = .empty;
    defer pending.deinit(gpa);
    try pending.append(gpa, .{ .pattern = pattern });
    while (pending.pop()) |item| {
        switch (item) {
            .text => |text| try writer.writeAll(text),
            .pattern => |next| {
                const start = pending.items.len;
                try formatPatternNode(writer, ident_store, string_store, scratch, next, &pending);
                std.mem.reverse(FormatPatternItem, pending.items[start..]);
            },
        }
    }
}

const FormatPatternItem = union(enum) {
    text: []const u8,
    pattern: Pattern,
};

/// Write one pattern node's leaf text, or queue its pieces in writing order.
fn formatPatternNode(
    writer: *std.Io.Writer,
    ident_store: *const Ident.Store,
    string_store: *const StringLiteral.Store,
    scratch: *ByteList,
    pattern: Pattern,
    pending: *std.ArrayList(FormatPatternItem),
) error{ OutOfMemory, WriteFailed }!void {
    const gpa = scratch.allocator;
    switch (pattern) {
        .anything => try writer.writeAll("_"),

        .literal => |lit| switch (lit) {
            .int => |i| {
                try scratch.resize(40);
                try writer.writeAll(i128h.i128_to_str(scratch.items, i).str);
            },
            .uint => |u| {
                try scratch.resize(40);
                try writer.writeAll(i128h.u128_to_str(scratch.items, u).str);
            },
            .bit => |b| try writer.writeAll(if (b) "Bool.true" else "Bool.false"),
            .byte => |b| try writer.print("{}", .{b}),
            .float => |f| {
                const float_val: f64 = @bitCast(f);
                try scratch.resize(400);
                try writer.writeAll(i128h.f64_to_str(scratch.items, float_val));
            },
            .decimal => |d| {
                try scratch.resize(40);
                try writer.writeAll(i128h.i128_to_str(scratch.items, d).str);
            },
            .str => |idx| {
                try writer.writeAll("\"");
                const text = string_store.get(idx);
                try writer.writeAll(text);
                try writer.writeAll("\"");
            },
            // The exact digits live outside this reporter's reach; the region
            // highlight carries the literal's spelling.
            .exact_numeral => try writer.writeAll("<numeric literal>"),
        },

        .ctor => |c| {
            switch (c.union_info.render_as) {
                .tag => {
                    const alt = c.union_info.alternatives[c.tag_id.toInt()];
                    switch (alt.name) {
                        .tag => |t| {
                            if (t.eql(Ident.Idx.NONE)) {
                                // This is the #Open synthetic tag - show as wildcard
                                try writer.writeAll("_");
                                return;
                            }
                            try writer.writeAll(ident_store.getText(t));
                        },
                        .opaque_type => |o| {
                            try writer.writeAll(ident_store.getText(o));
                        },
                    }
                    // Add arguments
                    for (c.args) |arg| {
                        try pending.append(gpa, .{ .text = " " });
                        try pending.append(gpa, .{ .pattern = arg });
                    }
                },

                .record => |record| {
                    try writer.writeAll("{ ");
                    for (c.args, 0..) |arg, i| {
                        if (i > 0) try pending.append(gpa, .{ .text = ", " });
                        try pending.append(gpa, .{ .text = if (i < record.names.len) ident_store.getText(record.names[i]) else "_" });
                        try pending.append(gpa, .{ .text = ": " });
                        try pending.append(gpa, .{ .pattern = arg });
                    }
                    try pending.append(gpa, .{ .text = " }" });
                },

                .tuple => {
                    try writer.writeAll("(");
                    for (c.args, 0..) |arg, i| {
                        if (i > 0) try pending.append(gpa, .{ .text = ", " });
                        try pending.append(gpa, .{ .pattern = arg });
                    }
                    try pending.append(gpa, .{ .text = ")" });
                },

                .guard => {
                    // Unwrap the guard - show the actual pattern (second arg)
                    if (c.args.len >= 2) {
                        try pending.append(gpa, .{ .pattern = c.args[1] });
                        try pending.append(gpa, .{ .text = " (with guard)" });
                    }
                },

                .opaque_type => {
                    const alt = c.union_info.alternatives[c.tag_id.toInt()];
                    switch (alt.name) {
                        .opaque_type => |o| {
                            try writer.writeAll(ident_store.getText(o));
                        },
                        .tag => |t| {
                            try writer.writeAll(ident_store.getText(t));
                        },
                    }
                    if (c.args.len > 0) {
                        try pending.append(gpa, .{ .text = " " });
                        try pending.append(gpa, .{ .pattern = c.args[0] });
                    }
                },
            }
        },

        .list => |l| {
            try writer.writeAll("[");
            switch (l.arity) {
                .exact => {
                    for (l.elements, 0..) |elem, i| {
                        if (i > 0) try pending.append(gpa, .{ .text = ", " });
                        try pending.append(gpa, .{ .pattern = elem });
                    }
                },
                .slice => |s| {
                    // Format as [prefix.., suffix]
                    for (0..s.prefix) |i| {
                        if (i > 0) try pending.append(gpa, .{ .text = ", " });
                        try pending.append(gpa, .{ .pattern = l.elements[i] });
                    }
                    try pending.append(gpa, .{ .text = if (s.prefix > 0 and s.suffix > 0)
                        ", .., "
                    else if (s.prefix > 0)
                        ", .."
                    else if (s.suffix > 0)
                        ".., "
                    else
                        ".." });
                    const suffix_start = l.elements.len - s.suffix;
                    for (suffix_start..l.elements.len) |i| {
                        if (i > suffix_start) try pending.append(gpa, .{ .text = ", " });
                        try pending.append(gpa, .{ .pattern = l.elements[i] });
                    }
                },
            }
            try pending.append(gpa, .{ .text = "]" });
        },
    }
}

test "record inhabitedness retains its field-var scratch buffer" {
    var counting = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    const gpa = counting.allocator();
    const presences = [_]types.RecordField.Presence{
        .required(@enumFromInt(1)),
        .required(@enumFromInt(2)),
    };
    var work_list: std.ArrayList(WorkItem) = .empty;
    defer work_list.deinit(gpa);
    var scratch: std.ArrayList(Var) = .empty;
    defer scratch.deinit(gpa);

    // Built on the testing allocator so it stays out of `counting`'s tallies.
    var type_store = try TypeStore.initCapacity(std.testing.allocator, 4, 0);
    defer type_store.deinit();

    try pushRecordAndWork(gpa, &type_store, &work_list, &scratch, &presences);
    try std.testing.expect(scratch.capacity >= presences.len);

    work_list.clearRetainingCapacity();
    const allocations = counting.allocations;
    const resizes = counting.resize_index;
    try pushRecordAndWork(gpa, &type_store, &work_list, &scratch, &presences);

    try std.testing.expectEqual(allocations, counting.allocations);
    try std.testing.expectEqual(resizes, counting.resize_index);
}
