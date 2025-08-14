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
    try std.testing.expectEqual(32, @sizeOf(Descriptor));
    try std.testing.expectEqual(24, @sizeOf(Content));
    try std.testing.expectEqual(12, @sizeOf(Alias));
    try std.testing.expectEqual(20, @sizeOf(FlatType));
    try std.testing.expectEqual(12, @sizeOf(Record));
    try std.testing.expectEqual(16, @sizeOf(NominalType));
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
    scopes: std.ArrayList(VarMap),

    pub fn init(allocator: std.mem.Allocator) TypeScope {
        return .{
            .scopes = std.ArrayList(VarMap).init(allocator),
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
pub const Descriptor = struct { content: Content, rank: Rank, mark: Mark };

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
pub const Rank = enum(u4) {
    /// When the corresponding type is generic, like in `List.len`.
    generalized = 0,

    top_level = 1,
    _,

    /// Get the lowest rank
    pub fn min(a: Rank, b: Rank) Rank {
        return @enumFromInt(@min(@intFromEnum(a), @intFromEnum(b)));
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

    flex_var: ?Ident.Idx,
    rigid_var: Ident.Idx,
    alias: Alias,
    structure: FlatType,
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
    str,
    box: Var,
    list: Var,
    list_unbound,
    record_unbound: RecordField.SafeMultiList.Range,
    record_poly: struct { record: Record, var_: Var },
    tuple: Tuple,
    num: Num,
    nominal_type: NominalType,
    fn_pure: Func,
    fn_effectful: Func,
    fn_unbound: Func,
    record: Record,
    empty_record,
    tag_union: TagUnion,
    empty_tag_union,
};

// tuples //

/// Represents a tuple
pub const Tuple = struct {
    elems: Var.SafeList.Range,
};

// numbers //

/// Represents numeric types in the type system.
///
/// Numbers are extremely common, so we special-case their representation
/// for both performance and memory efficiency. While Roc exposes numbers
/// as opaque types with phantom type variables (eg, `Num(a)`), we avoid
/// representing them using fully generic applications unless absolutely
/// necessary.
///
/// In most cases—when a specific number type is known (like `U8`) — we store
/// a compact, canonical form directly. This avoids the need for multiple
/// indirections, such as separate type variables and layered aliases.
///
/// When a polymorphic number is required (eg in the type signature of
/// a generic function over `Num(a)`), we allow full representation via
/// `num_poly`, `int_poly`, or `frac_poly`. However, during unification,
/// if a polymorphic number is unified with a compact one, the compact
/// form always wins: we discard the polymorphic wrapper and store the
/// concrete, memory-efficient version instead.
pub const Num = union(enum) {
    num_unbound: IntRequirements,
    int_unbound: IntRequirements,
    frac_unbound: FracRequirements,
    num_poly: struct { var_: Var, requirements: IntRequirements },
    int_poly: struct { var_: Var, requirements: IntRequirements },
    frac_poly: struct { var_: Var, requirements: FracRequirements },
    int_precision: Int.Precision, // TODO instead of storing this, can we just always store a num_compact instead?
    frac_precision: Frac.Precision, // TODO instead of storing this, can we just always store a num_compact instead?
    num_compact: Compact,

    /// Represents a compact number
    pub const Compact = union(enum) {
        const Self = @This();

        int: Int.Precision,
        frac: Frac.Precision,

        pub fn placeholder() Compact {
            return Compact{ .int = .u8 };
        }
    };

    /// Represents the type constraints of an integer literal: the minimum number of bits required
    /// to store it in memory, and whether a sign is required because it's a negative integer.
    ///
    /// Here's an example:
    ///
    ///     if foo() 500 else -500
    ///
    /// The `500` literal has a requirement of 9 bits and it's not negative. The `-500` literal also
    /// requires 9 bits but it *is* negative, which means we need a signed integer. Since both require
    /// 9 bits, they don't fit in I8, and therefore only type annotations of I16, I32, I64, or I128
    /// will avoid an "integer literal too large" error.
    ///
    /// Here's an example which demonstrates why we need to track sign separately:
    ///
    ///     if foo() 100 else 200
    ///
    /// The `100` literal has a requirement of 7 bits, and the `200` literal requires 8. If we didn't
    /// track sign, we might write down that the `100` literal "fits in `I8`" - which is a true claim,
    /// but it doesn't record whether the `I8` being signed is necessary. So when we get `200` in the mix,
    /// we have an ambiguity that causes a problem:
    /// * If we assume the sign was required, then if this is annotated `U8`, that would incorrectly give an error. `U8` should be fine here!
    /// * If we assume the sign is not required, then if it had been `-100` instead, then a `U8` annotation would incorrectly *not* give an error.
    ///
    /// Putting all of this together, we need to track the required number of bits and the sign separately.
    pub const IntRequirements = struct {
        // Whether the literal was negative, and therefore only unifies with signed ints
        sign_needed: bool,

        // The lowest number of bits that can represent the decimal value of the Int literal  *excluding* its sign.
        bits_needed: u8,

        /// Unifies two IntRequirements, returning the most restrictive combination
        pub fn unify(self: IntRequirements, other: IntRequirements) IntRequirements {
            return IntRequirements{
                .sign_needed = self.sign_needed or other.sign_needed,
                .bits_needed = @max(self.bits_needed, other.bits_needed),
            };
        }
    };

    /// Represents the type constraints of a number literal that has a decimal point: whether it fits
    /// in F32 and/or Dec.
    ///
    /// We don't bather tracking whether it can fit in F64, because:
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
    pub const FracRequirements = struct {
        fits_in_f32: bool,
        fits_in_dec: bool,

        /// Unifies two FracRequirements, returning the intersection of capabilities
        pub fn unify(self: FracRequirements, other: FracRequirements) FracRequirements {
            return FracRequirements{
                .fits_in_f32 = self.fits_in_f32 and other.fits_in_f32,
                .fits_in_dec = self.fits_in_dec and other.fits_in_dec,
            };
        }
    };

    /// The Frac data type
    pub const Frac = struct {
        /// The precision of a Frac
        pub const Precision = enum(u3) {
            f32 = 2,
            f64 = 3,
            dec = 4,

            /// Default precision for Frac(a), e.g. if you put `1.1` into `roc repl`.
            pub const default = Num.Frac.Precision.dec;

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
        /// We don't bather tracking whether it can fit in F64, because:
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

    /// The Int data type
    pub const Int = struct {
        /// The requirements of a particular integer literal: the minimum number of bits required
        /// to store it, and whether a sign is required because it's a negative integer.
        /// Here's an example:
        ///
        ///     if foo() 500 else -500
        ///
        /// The `500` literal has a requirement of 9 bits and it's not negative. The `-500` literal also
        /// requires 9 bits but it *is* negative, which means we need a signed integer. Since both require
        /// 9 bits, they don't fit in I8, and therefore only type annotations of I16, I32, I64, or I128
        /// will avoid an "integer literal too large" error.
        ///
        /// Here's an example which demonstrates why we need to track sign separately:
        ///
        ///     if foo() 100 else 200
        ///
        /// The `100` literal has a requirement of 7 bits, and the `200` literal requires 8. If we didn't
        /// track sign, we might write down that the `100` literal "fits in `I8`" - which is a true claim,
        /// but it doesn't record whether the `I8` being signed is necessary. So when we get `200` in the mix,
        /// we have an ambiguity that causes a problem:
        /// * If we assume the sign was required, then if this is annotated `U8`, that would incorrectly give an error. `U8` should be fine here!
        /// * If we assume the sign is not required, then if it had been `-100` instead, then a `U8` annotation would incorrectly *not* give an error.
        ///
        /// Putting all of this together, we need to track the required number of bits and the sign separately.
        pub const Requirements = packed struct {
            sign_needed: bool,
            bits_needed: BitsNeeded,

            /// Create Requirements from a u128 value and whether it's negated
            pub fn fromIntLiteral(val: u128, is_negated: bool) Requirements {
                return Requirements{
                    .sign_needed = is_negated,
                    .bits_needed = BitsNeeded.fromValue(val),
                };
            }
        };

        /// The lowest number of bits that can represent the decimal value of an Int literal, *excluding* its sign.
        /// (By design, the sign is sored separately in Requirements.)
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

            /// Default precision for Int(a), e.g. if you put `0x1` into `roc repl`.
            ///
            /// Note that numbers default to integers, so this is also what you'll
            /// get if you put `1` into `roc repl`.
            pub const default = Num.Int.Precision.i128;

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
    };

    /// a frac f32
    pub const frac_f32: Num = Num{ .num_compact = Compact{ .frac = .f32 } };

    /// a frac f64
    pub const frac_f64: Num = Num{ .num_compact = Compact{ .frac = .f64 } };

    /// a frac dec
    pub const frac_dec: Num = Num{ .num_compact = Compact{ .frac = .dec } };

    /// an int u8
    pub const int_u8: Num = Num{ .num_compact = Compact{ .int = .u8 } };

    /// an int i8
    pub const int_i8: Num = Num{ .num_compact = Compact{ .int = .i8 } };

    /// an int u16
    pub const int_u16: Num = Num{ .num_compact = Compact{ .int = .u16 } };

    /// an int i16
    pub const int_i16: Num = Num{ .num_compact = Compact{ .int = .i16 } };

    /// an int u32
    pub const int_u32: Num = Num{ .num_compact = Compact{ .int = .u32 } };

    /// an int i32
    pub const int_i32: Num = Num{ .num_compact = Compact{ .int = .i32 } };

    /// an int u64
    pub const int_u64: Num = Num{ .num_compact = Compact{ .int = .u64 } };

    /// an int i64
    pub const int_i64: Num = Num{ .num_compact = Compact{ .int = .i64 } };

    /// an int u128
    pub const int_u128: Num = Num{ .num_compact = Compact{ .int = .u128 } };

    /// an int i128
    pub const int_i128: Num = Num{ .num_compact = Compact{ .int = .i128 } };

    pub fn parseNumLiteralWithSuffix(text: []const u8) struct { num_text: []const u8, suffix: ?[]const u8 } {
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
};

// nominal types //

/// A nominal user-defined type
pub const NominalType = struct {
    ident: TypeIdent,
    vars: Var.SafeList.NonEmptyRange,
    /// The full module path where this nominal type was originally defined
    /// (e.g., "Json.Decode" or "mypackage.Data.Person")
    origin_module: Ident.Idx,
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

test "Precision.size() and alignment()" {

    // u8 and i8 should have size and alignment of 1
    try std.testing.expectEqual(1, Num.Int.Precision.u8.size());
    try std.testing.expectEqual(1, Num.Int.Precision.i8.size());
    try std.testing.expectEqual(1, Num.Int.Precision.u8.alignment().toByteUnits());
    try std.testing.expectEqual(1, Num.Int.Precision.i8.alignment().toByteUnits());

    // u16 and i16 should have size and alignment of 2
    try std.testing.expectEqual(2, Num.Int.Precision.u16.size());
    try std.testing.expectEqual(2, Num.Int.Precision.i16.size());
    try std.testing.expectEqual(2, Num.Int.Precision.u16.alignment().toByteUnits());
    try std.testing.expectEqual(2, Num.Int.Precision.i16.alignment().toByteUnits());

    // u32 and i32 should have size and alignment of 4
    try std.testing.expectEqual(4, Num.Int.Precision.u32.size());
    try std.testing.expectEqual(4, Num.Int.Precision.i32.size());
    try std.testing.expectEqual(4, Num.Int.Precision.u32.alignment().toByteUnits());
    try std.testing.expectEqual(4, Num.Int.Precision.i32.alignment().toByteUnits());

    // u64 and i64 should have size and alignment of 8
    try std.testing.expectEqual(8, Num.Int.Precision.u64.size());
    try std.testing.expectEqual(8, Num.Int.Precision.i64.size());
    try std.testing.expectEqual(8, Num.Int.Precision.u64.alignment().toByteUnits());
    try std.testing.expectEqual(8, Num.Int.Precision.i64.alignment().toByteUnits());

    // u128 and i128 should have size and alignment of 16
    try std.testing.expectEqual(16, Num.Int.Precision.u128.size());
    try std.testing.expectEqual(16, Num.Int.Precision.i128.size());
    try std.testing.expectEqual(16, Num.Int.Precision.u128.alignment().toByteUnits());
    try std.testing.expectEqual(16, Num.Int.Precision.i128.alignment().toByteUnits());

    // f32 should have size and alignment of 4
    try std.testing.expectEqual(4, Num.Frac.Precision.f32.size());
    try std.testing.expectEqual(4, Num.Frac.Precision.f32.alignment().toByteUnits());

    // f64 should have size and alignment of 8
    try std.testing.expectEqual(8, Num.Frac.Precision.f64.size());
    try std.testing.expectEqual(8, Num.Frac.Precision.f64.alignment().toByteUnits());

    // dec should have size and alignment of 16
    try std.testing.expectEqual(16, Num.Frac.Precision.dec.size());
    try std.testing.expectEqual(16, Num.Frac.Precision.dec.alignment().toByteUnits());
}

test "BitsNeeded.fromValue calculates correct bits for various values" {
    const BitsNeeded = Num.Int.BitsNeeded;

    // Test minimum signed value adjustments
    try std.testing.expectEqual(BitsNeeded.@"7", BitsNeeded.fromValue(127)); // -128 adjusted
    try std.testing.expectEqual(BitsNeeded.@"8", BitsNeeded.fromValue(128)); // -128 not adjusted

    // Test other values
    try std.testing.expectEqual(BitsNeeded.@"7", BitsNeeded.fromValue(0));
    try std.testing.expectEqual(BitsNeeded.@"7", BitsNeeded.fromValue(1));
    try std.testing.expectEqual(BitsNeeded.@"7", BitsNeeded.fromValue(127));
    try std.testing.expectEqual(BitsNeeded.@"8", BitsNeeded.fromValue(255));
    try std.testing.expectEqual(BitsNeeded.@"9_to_15", BitsNeeded.fromValue(256));
    try std.testing.expectEqual(BitsNeeded.@"16", BitsNeeded.fromValue(65535));
    try std.testing.expectEqual(BitsNeeded.@"17_to_31", BitsNeeded.fromValue(65536));

    // Test that toBits returns expected values
    try std.testing.expectEqual(@as(u8, 7), BitsNeeded.@"7".toBits());
    try std.testing.expectEqual(@as(u8, 8), BitsNeeded.@"8".toBits());
    try std.testing.expectEqual(@as(u8, 9), BitsNeeded.@"9_to_15".toBits());
    try std.testing.expectEqual(@as(u8, 16), BitsNeeded.@"16".toBits());
    try std.testing.expectEqual(@as(u8, 17), BitsNeeded.@"17_to_31".toBits());
    try std.testing.expectEqual(@as(u8, 32), BitsNeeded.@"32".toBits());
    try std.testing.expectEqual(@as(u8, 33), BitsNeeded.@"33_to_63".toBits());
    try std.testing.expectEqual(@as(u8, 64), BitsNeeded.@"64".toBits());
    try std.testing.expectEqual(@as(u8, 65), BitsNeeded.@"65_to_127".toBits());
    try std.testing.expectEqual(@as(u8, 128), BitsNeeded.@"128".toBits());
}
