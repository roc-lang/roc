//! This module defines the core data structures for representing types in the compiler's
//! Hindley-Milner type inference system. It includes:
//!
//! - `Var`: unique type variable identifiers
//! - `Descriptor`: the rank, mark, and structure of a type
//! - `Content`: the semantic meaning of a type (flex var, alias, function, record, etc.)
//! - `FlatType`: the 'flat' shape of a type (tuples, numbers, tag unions, etc.)
//! - `Alias`: nominal or structural type aliases
//! - `Func`, `Record`, `TagUnion`: structured type forms
//!
//! Special care is taken to keep memory layouts small and efficient. When modifying
//! these types, please consider their size impact and unification performance.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const Ident = base.Ident;
const MkSafeList = collections.SafeList;
const MkSafeMultiList = collections.SafeMultiList;

test {
    // If your changes caused this number to go down, great! Please update it to the lower number.
    // If it went up, please make sure your changes are absolutely required!
    try std.testing.expectEqual(36, @sizeOf(Descriptor));
    try std.testing.expectEqual(28, @sizeOf(Content));
    try std.testing.expectEqual(12, @sizeOf(Alias));
    try std.testing.expectEqual(24, @sizeOf(FlatType));
    try std.testing.expectEqual(12, @sizeOf(Record));
    try std.testing.expectEqual(20, @sizeOf(NominalType)); // Increased from 16 due to is_opaque field
    try std.testing.expectEqual(72, @sizeOf(StaticDispatchConstraint)); // Includes recursion_info + num_literal fields
    try std.testing.expectEqual(16, @sizeOf(Func));
}

/// A type variable
pub const Var = enum(u32) {
    _,

    /// A safe list of type variables
    pub const SafeList = MkSafeList(Var);

    /// Debug representation of a type variable, panics on allocation failure
    pub fn allocPrint(self: Var, gpa: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        return try std.fmt.allocPrint(gpa, "#{d}", .{@intFromEnum(self)});
    }
};

/// A mapping from polymorphic type variables to concrete type variables
pub const VarMap = std.hash_map.HashMap(Var, Var, std.hash_map.AutoContext(Var), 80);

/// TypeScope represents nested type scopes for resolving polymorphic type variables.
/// Each HashMap in the list represents a scope level, mapping polymorphic type variables
/// to their resolved monomorphic equivalents.
pub const TypeScope = struct {
    scopes: std.array_list.Managed(VarMap),

    pub fn init(allocator: std.mem.Allocator) TypeScope {
        return .{
            .scopes = std.array_list.Managed(VarMap).init(allocator),
        };
    }

    pub fn deinit(self: *TypeScope) void {
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit();
    }

    /// Look up a type variable in all nested scopes, returning the mapped variable if found
    pub fn lookup(self: *const TypeScope, var_to_find: Var) ?Var {
        for (self.scopes.items) |*scope| {
            if (scope.get(var_to_find)) |mapped_var| {
                return mapped_var;
            }
        }
        return null;
    }
};

/// A type descriptor
pub const Descriptor = struct {
    content: Content,
    rank: Rank,
    mark: Mark,
};

/// In general, the rank tracks the number of let-bindings a variable is "under".
/// Top-level definitions have rank 1. A let inside a top-level definition gets rank 2, and so on.
///
/// An example:
/// ```
/// foo = 3
///
/// plus_five = |arg| {
///    x = 5
///    arg + x
/// }
/// ```
/// Here the rank of `foo` is 1 because it is at the top level and the rank of `x` is 2 because it is under or inside `plus_five`.
///
/// Imported variables get rank 2.
///
/// Rank 0 is special, it is used for variables that are generalized (generic).
///
/// Keeping track of ranks makes type inference faster.
///
pub const Rank = enum(u8) {
    /// When the corresponding type is generic, like in `List.len`.
    generalized = 0,
    top_level = 1,
    _,

    /// Get the lowest rank
    pub fn min(a: Rank, b: Rank) Rank {
        return @enumFromInt(@min(@intFromEnum(a), @intFromEnum(b)));
    }

    /// Get the lowest rank
    pub fn max(a: Rank, b: Rank) Rank {
        return @enumFromInt(@max(@intFromEnum(a), @intFromEnum(b)));
    }

    /// Get the next rank
    pub fn next(a: Rank) Rank {
        return @enumFromInt(@intFromEnum(a) + 1);
    }

    /// Get the prev rank
    pub fn prev(a: Rank) Rank {
        return @enumFromInt(@intFromEnum(a) - 1);
    }
};

/// A type variable mark
///
/// Marks are temporary annotations used during various phases of type inference
/// and type checking to track state.
///
/// Some places `Mark` is used:
/// * Marking variables as visited in occurs checks to avoid redundant work
/// * Marking variables for generalizing during solving
pub const Mark = enum(u32) {
    const Self = @This();

    visited = 0,
    none = 1,
    _,

    /// Get the next mark
    pub fn next(self: Self) Self {
        return @enumFromInt(@intFromEnum(self) + 1);
    }
};

// content //

/// Represents what the a type *is*
pub const Content = union(enum) {
    const Self = @This();

    flex: Flex,
    rigid: Rigid,
    alias: Alias,
    structure: FlatType,
    recursion_var: RecursionVar,
    err,

    // helpers //

    /// Unwrap a record or return null
    pub fn unwrapRecord(content: Self) ?Record {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .record => |record| {
                        return record;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a tag union or return null
    pub fn unwrapTagUnion(content: Self) ?TagUnion {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .tag_union => |tag_union| {
                        return tag_union;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a nominal type or return null
    pub fn unwrapNominalType(content: Self) ?NominalType {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .nominal_type => |nominal_type| {
                        return nominal_type;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a function (pure, eff, or unbound) and return it
    pub fn unwrapFunc(content: Self) ?Func {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .fn_pure => |func| return func,
                    .fn_effectful => |func| return func,
                    .fn_unbound => |func| return func,
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a function (pure, eff, or unbound) and return it
    pub fn unwrapFuncFull(content: Self) ?struct { func: Func, ext: enum { unbound, pure, effectful } } {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .fn_pure => |func| return .{ .func = func, .ext = .pure },
                    .fn_effectful => |func| return .{ .func = func, .ext = .effectful },
                    .fn_unbound => |func| return .{ .func = func, .ext = .unbound },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a recursion var or return null
    pub fn unwrapRecursionVar(content: Self) ?RecursionVar {
        switch (content) {
            .recursion_var => |rec_var| return rec_var,
            else => return null,
        }
    }

    /// Check if content is a recursion var
    pub fn isRecursionVar(content: Self) bool {
        return switch (content) {
            .recursion_var => true,
            else => false,
        };
    }
};

// flex //

/// A flex var, with optional static dispatch constraints
pub const Flex = struct {
    name: ?Ident.Idx,
    constraints: StaticDispatchConstraint.SafeList.Range,

    pub fn init() Flex {
        return .{
            .name = null,
            .constraints = StaticDispatchConstraint.SafeList.Range.empty(),
        };
    }

    pub fn withName(self: Flex, name: ?Ident.Idx) Flex {
        return .{
            .name = name,
            .constraints = self.constraints,
        };
    }

    pub fn withConstraints(self: Flex, constraints: StaticDispatchConstraint.SafeList.Range) Flex {
        return .{
            .name = self.name,
            .constraints = constraints,
        };
    }
};

// rigid //

/// A rigid var, with optional static dispatch constraints
pub const Rigid = struct {
    name: Ident.Idx,
    constraints: StaticDispatchConstraint.SafeList.Range,

    pub fn init(name: Ident.Idx) Rigid {
        return .{
            .name = name,
            .constraints = StaticDispatchConstraint.SafeList.Range.empty(),
        };
    }

    pub fn withConstraints(self: Rigid, constraints: StaticDispatchConstraint.SafeList.Range) Rigid {
        return .{
            .name = self.name,
            .constraints = constraints,
        };
    }
};

// recursion var //

/// A recursion variable marks a point in a type where recursion occurs.
/// This is used to implement **equirecursive unification** for static dispatch constraints.
///
/// ## The Problem
///
/// When a type has recursive constraints (e.g., `a.plus : a, Int -> a`), the return type `a`
/// would normally require checking the same constraints infinitely:
/// - `a` has constraint `a.plus : a, _ -> ret`
/// - `ret` also needs constraint `ret.plus : ret, _ -> ret2`
/// - `ret2` also needs constraint `ret2.plus : ret2, _ -> ret3`
/// - ...infinitely
///
/// This occurs in expressions like `(|x| x.plus(5))(7)` where numeric operations return
/// numeric types that themselves support the same operations.
///
/// ## The Solution: Equirecursive Unification
///
/// Instead of infinitely expanding the constraint chain, we:
/// 1. **Detect recursion** during constraint checking (via constraint_check_stack in Check.zig)
/// 2. **Create a RecursionVar** that points back to the original structure
/// 3. **Unify equirecursively**: Two types unify if they're structurally equal up to their recursion point
///
/// ## How It Works
///
/// A RecursionVar creates a **circular reference**:
/// ```
/// type_var -> RecursionVar { structure: type_var }
/// ```
///
/// During unification (see unify.zig):
/// - When we encounter a RecursionVar, we unfold one level and unify with its structure
/// - The existing cycle detection in `unifyGuarded` (via `checkVarsEquiv`) prevents infinite recursion
/// - Two RecursionVars unify if their structures unify
///
/// During type display (see TypeWriter.zig):
/// - RecursionVar displays as its structure type
/// - The existing `seen` tracking detects cycles and displays "..." to indicate recursion
///
/// ## Example
///
/// For `(|x| x.plus(5))(7)`:
/// - Without RecursionVar: Infinite loop during constraint checking
/// - With RecursionVar: Creates `a` where `a = RecursionVar { structure: a }`, terminates successfully
pub const RecursionVar = struct {
    /// The type variable containing the actual structure this recursion var points to
    structure: Var,
    /// Optional name for debugging and pretty-printing
    name: ?Ident.Idx,
};

// alias //

/// A named alias to a different type
pub const Alias = struct {
    ident: TypeIdent,
    vars: Var.SafeList.NonEmptyRange,
};

/// Represents an ident of a type
/// TODO: Should this be something like CanIdent???
pub const TypeIdent = struct {
    const Self = @This();

    ident_idx: Ident.Idx,
    // TODO: Add module ident?

    pub fn eql(store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.order(store, a, b) == .eq;
    }

    /// Get the ordering of how a compares to b
    pub fn order(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.ident_idx);
        const b_text = store.getText(b.ident_idx);
        return std.mem.order(u8, a_text, b_text);
    }
};

// flat types //

/// Represents type without indirection, it's the concrete form that a type
/// takes after resolving type variables and aliases.
pub const FlatType = union(enum) {
    record: Record,
    record_unbound: RecordField.SafeMultiList.Range,
    tuple: Tuple,
    nominal_type: NominalType,
    fn_pure: Func,
    fn_effectful: Func,
    fn_unbound: Func,
    empty_record,
    tag_union: TagUnion,
    empty_tag_union,
};

// tuples //

/// Represents a tuple
pub const Tuple = struct {
    elems: Var.SafeList.Range,
};

// number types (used by layout and canonicalization) //

/// Integer types - used by layout.zig
pub const Int = struct {
    /// The exact precision of an Int
    pub const Precision = enum(u4) {
        u8 = 0,
        i8 = 1,
        u16 = 2,
        i16 = 3,
        u32 = 4,
        i32 = 5,
        u64 = 6,
        i64 = 7,
        u128 = 8,
        i128 = 9,

        /// Size in bytes
        pub fn size(self: @This()) u32 {
            // int values always have the same size as their alignment
            return @as(u32, @intCast(self.alignment().toByteUnits()));
        }

        /// Alignment
        pub fn alignment(self: @This()) std.mem.Alignment {
            // Both self and std.mem.Alignment are stored as log2(alignment) integers,
            // although we have to divide self by 2 to get to that exact representation.
            return @enumFromInt(@intFromEnum(self) / 2);
        }
    };

    /// The lowest number of bits that can represent the decimal value of an Int literal, *excluding* its sign.
    /// (By design, the sign is stored separately in IntRequirements.)
    pub const BitsNeeded = enum(u4) {
        @"7" = 0, // 7-bit integers (that is, `I8` - which uses 1 bit for the sign) are the smallest we support
        @"8" = 1,
        @"9_to_15" = 2,
        @"16" = 3,
        @"17_to_31" = 4,
        @"32" = 5,
        @"33_to_63" = 6,
        @"64" = 7,
        @"65_to_127" = 8,
        @"128" = 9,

        /// Calculate the BitsNeeded for a given u128 value
        pub fn fromValue(val: u128) BitsNeeded {
            if (val == 0) return .@"7";

            // Count leading zeros to determine how many bits are needed
            const leading_zeros = @clz(val);
            const bits_used = 128 - leading_zeros;

            // Map bits used to our enum values
            return switch (bits_used) {
                0...7 => .@"7",
                8 => .@"8",
                9...15 => .@"9_to_15",
                16 => .@"16",
                17...31 => .@"17_to_31",
                32 => .@"32",
                33...63 => .@"33_to_63",
                64 => .@"64",
                65...127 => .@"65_to_127",
                128 => .@"128",
                else => unreachable,
            };
        }

        /// Convert the BitsNeeded enum to the actual number of bits
        pub fn toBits(self: BitsNeeded) u8 {
            return switch (self) {
                .@"7" => 7,
                .@"8" => 8,
                .@"9_to_15" => 9,
                .@"16" => 16,
                .@"17_to_31" => 17,
                .@"32" => 32,
                .@"33_to_63" => 33,
                .@"64" => 64,
                .@"65_to_127" => 65,
                .@"128" => 128,
            };
        }
    };
};

/// Floating-point types - used by layout.zig
pub const Frac = struct {
    pub const Precision = enum(u3) {
        f32 = 2,
        f64 = 3,
        dec = 4,

        /// Size in bytes
        pub fn size(self: @This()) u32 {
            // frac values always have the same size as their alignment
            return @as(u32, @intCast(self.alignment().toByteUnits()));
        }

        /// Alignment
        pub fn alignment(self: @This()) std.mem.Alignment {
            // Map precision values to log2(alignment):
            // f32 (2) -> 4 bytes -> log2(4) = 2
            // f64 (3) -> 8 bytes -> log2(8) = 3
            // dec (4) -> 16 bytes -> log2(16) = 4
            return @enumFromInt(@intFromEnum(self));
        }
    };

    /// The requirements of a particular Frac literal: which types can represent it in memory.
    /// We don't bother tracking whether it can fit in F64, because:
    /// - If it can fit in F32 without precision loss compared to F64, then it can definitely fit in F64 as well.
    /// - If it can't fit in F32 or Dec, then clearly must have fit in F64, or else we would have errored out.
    /// - If it can't fit in F32 but it can fit in Dec, then it can fit (with precision loss) in F64, which is fine.
    ///
    /// Examples:
    ///
    ///     3.14 - fits in f32, f64, and dec
    ///     1e40 - fits only in f64 (exceeds f32's max of ~3.4e38, and is out of dec's range)
    ///     0.1 - fits in f32, f64, and dec (though f32 and f64 use binary approximation)
    ///     NaN - fits in f32 and f64, but not dec
    ///     1.23456789012345 - may fit in f64 and dec, but not f32 (precision loss)
    pub const Requirements = packed struct {
        fits_in_f32: bool,
        fits_in_dec: bool,
    };
};

/// Requirements for integer literals - used by CIR.zig for type inference
pub const IntRequirements = struct {
    // Whether the literal was negative, and therefore only unifies with signed ints
    sign_needed: bool,

    // The lowest number of bits that can represent the decimal value of the Int literal *excluding* its sign.
    bits_needed: u8,

    // True if the literal is an exact power of two (e.g., 1, 2, 4, ..., 2^k) on a boundary of a signed int.
    // This is crucial to allow the single negative boundary value −2^(N−1) when bits_needed == N.
    // When unifying multiple literals, we AND this flag to remain conservative.
    is_minimum_signed: bool,

    pub fn init() @This() {
        return .{
            .sign_needed = false,
            .bits_needed = 0,
            .is_minimum_signed = false,
        };
    }

    /// Unifies two IntRequirements, returning the most restrictive combination
    pub fn unify(self: IntRequirements, other: IntRequirements) IntRequirements {
        return IntRequirements{
            .sign_needed = self.sign_needed or other.sign_needed,
            .bits_needed = @max(self.bits_needed, other.bits_needed),
            .is_minimum_signed = self.is_minimum_signed and other.is_minimum_signed,
        };
    }

    /// Create Requirements from a u128 value and whether it's negated
    pub fn fromIntLiteral(val: u128, is_negated: bool) IntRequirements {
        const bits_need = Int.BitsNeeded.fromValue(val);
        return IntRequirements{
            .sign_needed = is_negated,
            .bits_needed = bits_need.toBits(),
            .is_minimum_signed = is_negated and IntRequirements.isMinimumSigned(val),
        };
    }

    /// Check if a value is a minimum signed value.
    /// These need special consideration
    pub fn isMinimumSigned(val: u128) bool {
        return switch (val) {
            @as(u128, @intCast(std.math.maxInt(i8))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i16))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i32))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i64))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i128))) + 1 => true,
            else => false,
        };
    }
};

/// Requirements for floating-point literals - used by CIR.zig for type inference
pub const FracRequirements = struct {
    fits_in_f32: bool,
    fits_in_dec: bool,

    pub fn init() @This() {
        return .{ .fits_in_f32 = true, .fits_in_dec = true };
    }

    /// Unifies two FracRequirements, returning the intersection of capabilities
    pub fn unify(self: FracRequirements, other: FracRequirements) FracRequirements {
        return FracRequirements{
            .fits_in_f32 = self.fits_in_f32 and other.fits_in_f32,
            .fits_in_dec = self.fits_in_dec and other.fits_in_dec,
        };
    }
};

/// Parse a number literal with an optional type suffix (e.g., "123u8", "45.67f64")
/// Used by Can.zig for canonicalization
pub fn parseNumeralWithSuffix(text: []const u8) struct { num_text: []const u8, suffix: ?[]const u8 } {
    var split_index: usize = text.len;
    var is_hex_or_bin = false;
    var start_index: usize = 0;

    // Check for negative prefix
    var prefix_offset: usize = 0;
    if (text.len > 0 and text[0] == '-') {
        prefix_offset = 1;
    }

    if (text.len > prefix_offset + 2 and text[prefix_offset] == '0') {
        switch (text[prefix_offset + 1]) {
            'x', 'X', 'b', 'B', 'o', 'O' => {
                is_hex_or_bin = true;
                start_index = prefix_offset + 2; // Skip the "0x", "0b", or "0o" prefix
            },
            else => {},
        }
    }

    for (text[start_index..], start_index..) |char, i| {
        if (char >= 'a' and char <= 'z') {
            // If we find a letter, check if it's a valid hex digit in a hex literal.
            if (is_hex_or_bin and (char >= 'a' and char <= 'f')) {
                // This is part of the hex number, continue.
                continue;
            }

            // This is the start of a suffix.
            split_index = i;
            break;
        }
    }

    if (split_index == text.len) {
        return .{ .num_text = text, .suffix = null };
    } else {
        return .{
            .num_text = text[0..split_index],
            .suffix = text[split_index..],
        };
    }
}

// nominal types //

/// A nominal user-defined type
pub const NominalType = struct {
    ident: TypeIdent,
    vars: Var.SafeList.NonEmptyRange,
    /// The full module path where this nominal type was originally defined
    /// (e.g., "Json.Decode" or "mypackage.Data.Person")
    origin_module: Ident.Idx,
    /// True if this type was declared with :: (opaque), false if declared with := (nominal)
    is_opaque: bool,

    /// Checks if backing types can unify directly with this nominal type
    pub fn canLiftInner(self: NominalType, cur_module_idx: Ident.Idx) bool {
        if (self.is_opaque) {
            // If opaque, then can only lift inner type if the current module is
            // the same
            return self.origin_module == cur_module_idx;
        }

        // If not opaque, then the inner type can always be lifted
        return true;
    }
};

// functions //

/// Represents a function
pub const Func = struct {
    args: Var.SafeList.Range,
    ret: Var,
    needs_instantiation: bool,
};

// records //

/// Represents a record
pub const Record = struct {
    fields: RecordField.SafeMultiList.Range,
    ext: Var,

    const Self = @This();
};

/// A field on a record
pub const RecordField = struct {
    const Self = @This();

    /// The name of the field
    name: Ident.Idx,
    /// The type of the field's value
    var_: Var,

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByName(ident_store, a, b) == .lt;
    }

    /// Get the ordering of how a compares to b
    pub fn orderByName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.name);
        const b_text = store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }

    /// A safe multi list of record fields
    pub const SafeMultiList = MkSafeMultiList(Self);

    /// A safe list of record fields
    pub const SafeList = MkSafeList(Self);
};

/// Two record fields
pub const TwoRecordFields = struct {
    a: RecordField,
    b: RecordField,

    /// A safe list of tag union fields
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tag union fields
    pub const SafeMultiList = MkSafeMultiList(@This());
};

// tag unions //

/// Represents a tag union
pub const TagUnion = struct {
    tags: Tag.SafeMultiList.Range,
    ext: Var,
};

/// A tag entry in a tag union row
pub const Tag = struct {
    /// The name of the tag (e.g. "Ok", "Err")
    name: Ident.Idx,

    /// A list of argument types for the tag (0 = no payload)
    args: Var.SafeList.Range,

    const Self = @This();

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByName(ident_store, a, b) == .lt;
    }

    /// Get the ordering of how a compares to b
    pub fn orderByName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.name);
        const b_text = store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }

    /// A safe list of tags
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tags
    pub const SafeMultiList = MkSafeMultiList(@This());
};

/// Two tag union fields
pub const TwoTags = struct {
    a: Tag,
    b: Tag,

    /// A safe list of tag union fields
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tag union fields
    pub const SafeMultiList = MkSafeMultiList(@This());
};

// content //

/// Information about a numeric literal for from_numeral constraint checking
///
/// Stores the parsed numeric value and metadata needed to validate conversion
/// to a specific numeric type at compile-time.
pub const NumeralInfo = struct {
    /// The parsed numeric value stored as raw bytes
    /// For fractional literals, this is scaled by 10^18 (Dec representation)
    bytes: [16]u8,

    /// Whether the original literal was stored as u128 (for large unsigned values)
    is_u128: bool,

    /// Whether the literal was negative
    is_negative: bool,

    /// Whether the literal had a decimal point
    is_fractional: bool,

    /// Source region for error reporting
    region: base.Region,

    /// Get the value as i128 (may overflow for large u128 values)
    pub fn toI128(self: NumeralInfo) i128 {
        return @bitCast(self.bytes);
    }

    /// Get the value as u128
    pub fn toU128(self: NumeralInfo) u128 {
        return @bitCast(self.bytes);
    }

    /// Create from an i128 value
    pub fn fromI128(val: i128, is_negative: bool, is_fractional: bool, region: base.Region) NumeralInfo {
        return .{
            .bytes = @bitCast(val),
            .is_u128 = false,
            .is_negative = is_negative,
            .is_fractional = is_fractional,
            .region = region,
        };
    }

    /// Create from a u128 value
    pub fn fromU128(val: u128, is_fractional: bool, region: base.Region) NumeralInfo {
        return .{
            .bytes = @bitCast(val),
            .is_u128 = true,
            .is_negative = false, // u128 values are never negative
            .is_fractional = is_fractional,
            .region = region,
        };
    }
};

/// Information about a recursive static dispatch constraint
///
/// When we detect that a constraint refers to itself (e.g., through a chain
/// of constraints), we create a RecursionVar to prevent infinite loops and
/// store metadata about the recursion here.
pub const RecursionInfo = struct {
    /// The recursion variable created to represent this recursive constraint
    recursion_var: Var,

    /// The depth in the constraint check stack where recursion was detected
    /// This helps with debugging and understanding the recursion structure
    depth: usize,
};

/// Represents a static dispatch constraints on a variable
///
/// sort  : List(a) -> List(a) where [a.ord : a -> Ord]
///                                   ^^^^^^^^^^^^^^^
pub const StaticDispatchConstraint = struct {
    const Self = @This();

    /// the dispatch fn name
    fn_name: Ident.Idx,
    /// the dispatch fn var, a function
    fn_var: Var,
    /// the origin of this constraint (operator, method call, or where clause)
    origin: Origin,
    /// Optional recursion information if this constraint is recursive
    recursion_info: ?RecursionInfo = null,
    /// Optional numeric literal info for from_numeral constraints
    num_literal: ?NumeralInfo = null,

    /// Tracks where a static dispatch constraint originated from
    pub const Origin = enum(u4) {
        desugared_binop, // From binary operator desugaring (e.g., +, -, *, etc.)
        desugared_unaryop, // From uniary operator desugaring (e.g., !)
        method_call, // From .method() syntax
        where_clause, // From where clause in type annotation
        from_numeral, // From numeric literal conversion
    };

    /// A safe list of static dispatch constraints
    pub const SafeList = MkSafeList(Self);

    /// A safe multi list of static dispatch constraints
    pub const SafeMultiList = MkSafeMultiList(Self);

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByFnNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByFnName(ident_store, a, b) == .lt;
    }

    /// Get the ordering of how a compares to b
    pub fn orderByFnName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.fn_name);
        const b_text = store.getText(b.fn_name);
        return std.mem.order(u8, a_text, b_text);
    }
};

/// Two record fields
pub const TwoStaticDispatchConstraints = struct {
    a: StaticDispatchConstraint,
    b: StaticDispatchConstraint,

    /// A safe list of tag union fields
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tag union fields
    pub const SafeMultiList = MkSafeMultiList(@This());
};

/// Polarity of a type, or roughly, what side of an arrow it appears on.
pub const Polarity = enum {
    /// A type that appears in negative/input position
    neg,
    /// A type that appears in positive/output position
    pos,

    pub const lhs = Polarity.neg;
    pub const rhs = Polarity.pos;
};
