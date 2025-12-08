//! Common IR types and utilities
//! This module contains type definitions and utilities used across the canonicalization IR.

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("reporting");
const builtins = @import("builtins");

const CompactWriter = collections.CompactWriter;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const TypeVar = types_mod.Var;

// Re-export these from other modules for convenience
pub const NodeStore = @import("NodeStore.zig");
pub const Node = @import("Node.zig");
pub const Expr = @import("Expression.zig").Expr;
pub const Pattern = @import("Pattern.zig").Pattern;
pub const Statement = @import("Statement.zig").Statement;
pub const TypeAnno = @import("TypeAnnotation.zig").TypeAnno;
pub const Diagnostic = @import("Diagnostic.zig").Diagnostic;

/// Indices of builtin type declarations within the Builtin module.
/// Loaded once at startup from builtin_indices.bin (generated at build time).
/// Contains both statement indices (positions within Builtin.bin) and ident indices
/// (interned identifiers for comparison without string lookups).
pub const BuiltinIndices = struct {
    // Statement indices - positions within the Builtin module
    bool_type: Statement.Idx,
    try_type: Statement.Idx,
    dict_type: Statement.Idx,
    set_type: Statement.Idx,
    str_type: Statement.Idx,
    list_type: Statement.Idx,
    box_type: Statement.Idx,
    utf8_problem_type: Statement.Idx,
    u8_type: Statement.Idx,
    i8_type: Statement.Idx,
    u16_type: Statement.Idx,
    i16_type: Statement.Idx,
    u32_type: Statement.Idx,
    i32_type: Statement.Idx,
    u64_type: Statement.Idx,
    i64_type: Statement.Idx,
    u128_type: Statement.Idx,
    i128_type: Statement.Idx,
    dec_type: Statement.Idx,
    f32_type: Statement.Idx,
    f64_type: Statement.Idx,
    numeral_type: Statement.Idx,

    // Ident indices - simple unqualified names (e.g., "Bool", "U8")
    bool_ident: Ident.Idx,
    try_ident: Ident.Idx,
    dict_ident: Ident.Idx,
    set_ident: Ident.Idx,
    str_ident: Ident.Idx,
    list_ident: Ident.Idx,
    box_ident: Ident.Idx,
    utf8_problem_ident: Ident.Idx,
    u8_ident: Ident.Idx,
    i8_ident: Ident.Idx,
    u16_ident: Ident.Idx,
    i16_ident: Ident.Idx,
    u32_ident: Ident.Idx,
    i32_ident: Ident.Idx,
    u64_ident: Ident.Idx,
    i64_ident: Ident.Idx,
    u128_ident: Ident.Idx,
    i128_ident: Ident.Idx,
    dec_ident: Ident.Idx,
    f32_ident: Ident.Idx,
    f64_ident: Ident.Idx,
    numeral_ident: Ident.Idx,
    // Tag idents for Try type
    ok_ident: Ident.Idx,
    err_ident: Ident.Idx,

    /// Convert a nominal type's ident to a NumKind, if it's a builtin numeric type.
    /// This allows direct ident comparison instead of string comparison for type identification.
    pub fn numKindFromIdent(self: BuiltinIndices, ident: Ident.Idx) ?NumKind {
        if (ident == self.u8_ident) return .u8;
        if (ident == self.i8_ident) return .i8;
        if (ident == self.u16_ident) return .u16;
        if (ident == self.i16_ident) return .i16;
        if (ident == self.u32_ident) return .u32;
        if (ident == self.i32_ident) return .i32;
        if (ident == self.u64_ident) return .u64;
        if (ident == self.i64_ident) return .i64;
        if (ident == self.u128_ident) return .u128;
        if (ident == self.i128_ident) return .i128;
        if (ident == self.f32_ident) return .f32;
        if (ident == self.f64_ident) return .f64;
        if (ident == self.dec_ident) return .dec;
        return null;
    }
};

// Type definitions for module compilation

/// Represents a definition (binding of a pattern to an expression) in the CIR
pub const Def = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    pattern: Pattern.Idx,
    expr: Expr.Idx,
    annotation: ?Annotation.Idx,
    kind: Kind,

    pub const Kind = union(enum) {
        /// A def that introduces identifiers
        let: void,
        /// A standalone statement with an fx variable
        stmt: TypeVar,
        /// Ignored result, must be effectful
        ignored: TypeVar,

        pub fn decode(encoded: [2]u32) Kind {
            if (encoded[0] == 0) {
                return .let;
            } else if (encoded[0] == 1) {
                return .{ .stmt = @as(TypeVar, @enumFromInt(encoded[1])) };
            } else {
                return .{ .ignored = @as(TypeVar, @enumFromInt(encoded[1])) };
            }
        }

        pub fn encode(self: Kind) [2]u32 {
            switch (self) {
                .let => return .{ 0, 0 },
                .stmt => |ty_var| return .{ 1, @intFromEnum(ty_var) },
                .ignored => |ty_var| return .{ 2, @intFromEnum(ty_var) },
            }
        }
    };

    pub fn pushToSExprTree(self: *const Def, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        const name: []const u8 = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };
        try tree.pushStaticAtom(name);

        const attrs = tree.beginNode();

        try cir.store.getPattern(self.pattern).pushToSExprTree(cir, tree, self.pattern);
        try cir.store.getExpr(self.expr).pushToSExprTree(cir, tree, self.expr);

        if (self.annotation) |annotation_idx| {
            try cir.store.getAnnotation(annotation_idx).pushToSExprTree(cir, tree, annotation_idx);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents a type header (e.g., 'Maybe a' or 'Try err ok') in type annotations
pub const TypeHeader = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { start: u32, len: u32 };

    /// The fully qualified name for lookups (e.g., "Builtin.Bool" or "MyModule.Foo.Bar")
    name: base.Ident.Idx,
    /// The name relative to the module, without the module prefix (e.g., "Bool" or "Foo.Bar").
    /// This is what should be used for NominalType.ident to avoid redundancy with origin_module.
    relative_name: base.Ident.Idx,
    args: TypeAnno.Span,

    pub fn pushToSExprTree(self: *const TypeHeader, cir: anytype, tree: anytype, idx: TypeHeader.Idx) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("ty-header");

        // Get the region for this TypeHeader
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
        const region = cir.store.getRegionAt(node_idx);
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        const name_str = cir.getIdent(self.name);
        try tree.pushStringPair("name", name_str);

        const attrs = tree.beginNode();

        if (self.args.span.len > 0) {
            const args_begin = tree.beginNode();
            try tree.pushStaticAtom("ty-args");
            const args_attrs = tree.beginNode();
            for (cir.store.sliceTypeAnnos(self.args)) |anno_idx| {
                try cir.store.getTypeAnno(anno_idx).pushToSExprTree(cir, tree, anno_idx);
            }
            try tree.endNode(args_begin, args_attrs);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents a where clause constraint in type definitions
pub const WhereClause = union(enum) {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    w_method: struct {
        var_: TypeAnno.Idx,
        method_name: base.Ident.Idx,
        args: TypeAnno.Span,
        ret: TypeAnno.Idx,
    },
    w_alias: struct {
        var_: TypeAnno.Idx,
        alias_name: base.Ident.Idx,
    },
    w_malformed: struct {
        diagnostic: Diagnostic.Idx,
    },

    pub fn pushToSExprTree(self: *const WhereClause, cir: anytype, tree: anytype, idx: WhereClause.Idx) !void {
        switch (self.*) {
            .w_method => |method| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("method");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                // Add module-of and ident information
                try cir.store.getTypeAnno(method.var_).pushToSExprTree(cir, tree, method.var_);

                const method_name_str = cir.getIdent(method.method_name);
                try tree.pushStringPair("name", method_name_str);

                const attrs = tree.beginNode();

                // Add actual argument types
                const args_begin = tree.beginNode();
                try tree.pushStaticAtom("args");
                const args_attrs = tree.beginNode();
                for (cir.store.sliceTypeAnnos(method.args)) |arg_idx| {
                    try cir.store.getTypeAnno(arg_idx).pushToSExprTree(cir, tree, arg_idx);
                }
                try tree.endNode(args_begin, args_attrs);

                // Add actual return type
                try cir.store.getTypeAnno(method.ret).pushToSExprTree(cir, tree, method.ret);
                try tree.endNode(begin, attrs);
            },
            .w_alias => |alias| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("alias");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                try cir.store.getTypeAnno(alias.var_).pushToSExprTree(cir, tree, alias.var_);

                const alias_name_str = cir.getIdent(alias.alias_name);
                try tree.pushStringPair("name", alias_name_str);

                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .w_malformed => {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("malformed");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }
};

/// Represents a type annotation associated with definitions
pub const Annotation = struct {
    pub const Idx = enum(u32) { _ };

    anno: TypeAnno.Idx,
    where: ?WhereClause.Span,

    pub fn pushToSExprTree(self: *const @This(), env: anytype, tree: *SExprTree, idx: Annotation.Idx) !void {
        const annotation = self.*;

        const begin = tree.beginNode();
        try tree.pushStaticAtom("annotation");
        const attrs = tree.beginNode();

        // Get the region for this Annotation
        const region = env.store.getAnnotationRegion(idx);
        try env.appendRegionInfoToSExprTreeFromRegion(tree, region);

        // Append annotation
        try env.store.getTypeAnno(annotation.anno).pushToSExprTree(env, tree, self.anno);

        // Append where clause
        if (annotation.where) |where_span| {
            const where_begin = tree.beginNode();
            try tree.pushStaticAtom("where");
            const where_attrs = tree.beginNode();
            const where_clauses = env.store.sliceWhereClauses(where_span);
            for (where_clauses) |clause_idx| {
                const clause = env.store.getWhereClause(clause_idx);
                try clause.pushToSExprTree(env, tree, clause_idx);
            }
            try tree.endNode(where_begin, where_attrs);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents an item exposed by a module's interface
pub const ExposedItem = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    name: base.Ident.Idx,
    alias: ?base.Ident.Idx,
    is_wildcard: bool,

    pub fn pushToSExprTree(self: *const ExposedItem, _: anytype, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("exposed");

        const name_str = cir.getIdent(self.name);
        try tree.pushStringPair("name", name_str);

        if (self.alias) |alias_idx| {
            const alias_str = cir.getIdent(alias_idx);
            try tree.pushStringPair("alias", alias_str);
        }

        try tree.pushBoolPair("wildcard", self.is_wildcard);

        const attrs = tree.beginNode();
        try tree.endNode(begin, attrs);
    }
};

/// Represents a field in a record pattern for pattern matching
pub const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { start: u32, len: u32 };
};

/// Represents an arbitrary precision smallish decimal value
pub const SmallDecValue = struct {
    numerator: i16,
    denominator_power_of_ten: u8,

    /// Convert a small dec to f64 (use for size comparisons)
    /// TODO: Review, claude generated
    pub fn toF64(self: @This()) f64 {
        const numerator_f64 = @as(f64, @floatFromInt(self.numerator));
        const divisor = std.math.pow(f64, 10, @as(f64, @floatFromInt(self.denominator_power_of_ten)));
        return numerator_f64 / divisor;
    }

    /// Calculate the int requirements of a SmallDecValue
    pub fn toFracRequirements(self: SmallDecValue) types_mod.FracRequirements {
        const f64_val = self.toF64();
        return types_mod.FracRequirements{
            .fits_in_f32 = fitsInF32(f64_val),
            .fits_in_dec = fitsInDec(f64_val),
        };
    }

    test "SmallDecValue.toF64 - basic cases" {
        // Test integer values
        {
            const val = SmallDecValue{ .numerator = 42, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, 42.0), val.toF64());
        }

        // Test decimal values
        {
            const val = SmallDecValue{ .numerator = 314, .denominator_power_of_ten = 2 };
            try std.testing.expectApproxEqAbs(@as(f64, 3.14), val.toF64(), 0.0001);
        }

        // Test negative values
        {
            const val = SmallDecValue{ .numerator = -500, .denominator_power_of_ten = 3 };
            try std.testing.expectApproxEqAbs(@as(f64, -0.5), val.toF64(), 0.0001);
        }

        // Test very small values
        {
            const val = SmallDecValue{ .numerator = 1, .denominator_power_of_ten = 10 };
            try std.testing.expectApproxEqAbs(@as(f64, 1e-10), val.toF64(), 1e-15);
        }

        // Test maximum numerator
        {
            const val = SmallDecValue{ .numerator = 32767, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, 32767.0), val.toF64());
        }

        // Test minimum numerator
        {
            const val = SmallDecValue{ .numerator = -32768, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, -32768.0), val.toF64());
        }
    }

    test "SmallDecValue.toFracRequirements - fits in all types" {
        // Small integer - fits in everything
        {
            const val = SmallDecValue{ .numerator = 100, .denominator_power_of_ten = 0 };
            const req = val.toFracRequirements();
            try std.testing.expect(req.fits_in_f32);
            try std.testing.expect(req.fits_in_dec);
        }

        // Pi approximation - fits in everything
        {
            const val = SmallDecValue{ .numerator = 31416, .denominator_power_of_ten = 4 };
            const req = val.toFracRequirements();
            try std.testing.expect(req.fits_in_f32);
            try std.testing.expect(req.fits_in_dec);
        }
    }
};

/// Check if the given f64 fits in f32 range (ignoring precision loss)
pub fn fitsInF32(f64_val: f64) bool {
    // Check if it's within the range that f32 can represent.
    // This includes normal, subnormal, and zero values.
    // (This is a magnitude check, so take the abs value to check
    // positive and negative at the same time.)
    const abs_val = @abs(f64_val);
    return abs_val == 0.0 or (abs_val >= std.math.floatTrueMin(f32) and abs_val <= std.math.floatMax(f32));
}

/// Check if a float value can be represented accurately in RocDec
pub fn fitsInDec(value: f64) bool {
    // RocDec uses i128 with 18 decimal places
    const max_dec_value = 170141183460469231731.0;
    const min_dec_value = -170141183460469231731.0;

    return value >= min_dec_value and value <= max_dec_value;
}

/// Represents an arbitrary precision integer value
pub const IntValue = struct {
    bytes: [16]u8,
    kind: IntKind,

    pub const IntKind = enum {
        i128,
        u128,
    };

    pub fn toI128(self: IntValue) i128 {
        return @bitCast(self.bytes);
    }

    pub fn bufPrint(self: IntValue, buf: []u8) ![]u8 {
        switch (self.kind) {
            .i128 => {
                const val: i128 = @bitCast(self.bytes);
                return std.fmt.bufPrint(buf, "{d}", .{val});
            },
            .u128 => {
                const val: u128 = @bitCast(self.bytes);
                return std.fmt.bufPrint(buf, "{d}", .{val});
            },
        }
    }

    /// Calculate the int requirements of an IntValue
    /// TODO: Review, claude generated
    pub fn toIntRequirements(self: IntValue) types_mod.IntRequirements {
        var is_negated = false;
        var u128_val: u128 = undefined;

        switch (self.kind) {
            .i128 => {
                const val: i128 = @bitCast(self.bytes);
                is_negated = val < 0;
                u128_val = if (val < 0) @abs(val) else @intCast(val);
            },
            .u128 => {
                const val: u128 = @bitCast(self.bytes);
                is_negated = false;
                u128_val = val;
            },
        }

        // Special handling for minimum signed values
        // These are the exact minimum values for each signed integer type.
        // They need special handling because their absolute value is one more
        // than the maximum positive value of the same signed type.
        // For example: i8 range is -128 to 127, so abs(-128) = 128 doesn't fit in i8's positive range
        const is_minimum_signed = is_negated and switch (u128_val) {
            @as(u128, @intCast(std.math.maxInt(i8))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i16))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i32))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i64))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i128))) + 1 => true,
            else => false,
        };

        // For minimum signed values, subtract 1 from the magnitude
        // This makes the bit calculation work correctly with the "n-1 bits for magnitude" rule
        const adjusted_val = if (is_minimum_signed) u128_val - 1 else u128_val;
        const bits_needed = types_mod.Int.BitsNeeded.fromValue(adjusted_val);
        return types_mod.IntRequirements{
            .sign_needed = is_negated and u128_val != 0, // -0 doesn't need a sign
            .bits_needed = bits_needed.toBits(),
            .is_minimum_signed = is_minimum_signed,
        };
    }

    /// Calculate the frac requirements of an IntValue
    /// TODO: Review, claude generated
    /// Calculate the frac requirements of an IntValue
    pub fn toFracRequirements(self: IntValue) types_mod.FracRequirements {
        // Convert to f64 for checking
        const f64_val: f64 = switch (self.kind) {
            .i128 => @floatFromInt(@as(i128, @bitCast(self.bytes))),
            .u128 => blk: {
                const val = @as(u128, @bitCast(self.bytes));
                if (val > @as(u128, 1) << 64) {
                    break :blk std.math.inf(f64);
                }
                break :blk @floatFromInt(val);
            },
        };

        // For integers, check both range AND exact representability in f32
        const fits_in_f32 = blk: {
            // Check range
            if (!fitsInF32(f64_val)) {
                break :blk false;
            }

            // Additionally check exact representability for integers
            // F32 can exactly represent integers only up to 2^24
            const f32_max_exact_int = 16777216.0; // 2^24
            break :blk @abs(f64_val) <= f32_max_exact_int;
        };

        const fits_in_dec = fitsInDec(f64_val);

        return types_mod.FracRequirements{
            .fits_in_f32 = fits_in_f32,
            .fits_in_dec = fits_in_dec,
        };
    }
};

/// Canonical information about a number
pub const NumKind = enum {
    // If this number has no restrictions
    num_unbound,

    // If this number is an int with no restrictions
    int_unbound,

    // If the number has an explicit suffix
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
};

/// Base-256 digit storage for Numeral values.
/// Used to construct Roc Numeral values during compile-time evaluation.
///
/// Numeral in Roc stores:
/// - is_negative: Bool (whether there was a minus sign)
/// - digits_before_pt: List(U8) (base-256 digits before decimal point)
/// - digits_after_pt: List(U8) (base-256 digits after decimal point)
///
/// Example: "356.517" becomes:
/// - is_negative = false
/// - digits_before_pt = [1, 100] (because 356 = 1*256 + 100)
/// - digits_after_pt = [2, 5] (because 517 = 2*256 + 5)
pub const NumeralDigits = struct {
    /// Index into the shared digit byte array in ModuleEnv
    digits_start: u32,
    /// Number of bytes for digits_before_pt
    before_pt_len: u16,
    /// Number of bytes for digits_after_pt
    after_pt_len: u16,
    /// Whether the literal had a minus sign
    is_negative: bool,

    /// Get the total length of stored digits
    pub fn totalLen(self: NumeralDigits) u32 {
        return @as(u32, self.before_pt_len) + @as(u32, self.after_pt_len);
    }

    /// Extract digits_before_pt from the shared byte array
    pub fn getDigitsBeforePt(self: NumeralDigits, digit_bytes: []const u8) []const u8 {
        return digit_bytes[self.digits_start..][0..self.before_pt_len];
    }

    /// Extract digits_after_pt from the shared byte array
    pub fn getDigitsAfterPt(self: NumeralDigits, digit_bytes: []const u8) []const u8 {
        const after_start = self.digits_start + self.before_pt_len;
        return digit_bytes[after_start..][0..self.after_pt_len];
    }

    /// Format the base-256 encoded numeral back to a human-readable decimal string.
    /// Writes to the provided buffer and returns a slice of the written content.
    /// Buffer should be at least 128 bytes to handle most numbers.
    pub fn formatDecimal(self: NumeralDigits, digit_bytes: []const u8, buf: []u8) []const u8 {
        return formatBase256ToDecimal(
            self.is_negative,
            self.getDigitsBeforePt(digit_bytes),
            self.getDigitsAfterPt(digit_bytes),
            buf,
        );
    }
};

/// Format base-256 encoded digits to a human-readable decimal string.
/// This is useful for error messages where we need to show the user what number
/// was invalid (e.g., "The number 999999999 is not a valid U8").
///
/// Parameters:
/// - is_negative: whether the number had a minus sign
/// - digits_before_pt: base-256 encoded integer part
/// - digits_after_pt: base-256 encoded fractional part
/// - buf: output buffer (should be at least 128 bytes)
///
/// Returns a slice of buf containing the formatted decimal string.
pub fn formatBase256ToDecimal(
    is_negative: bool,
    digits_before_pt: []const u8,
    digits_after_pt: []const u8,
    buf: []u8,
) []const u8 {
    var writer = std.io.fixedBufferStream(buf);
    const w = writer.writer();

    // Write sign if negative
    if (is_negative) w.writeAll("-") catch {};

    // Convert base-256 integer part to decimal
    var value: u128 = 0;
    for (digits_before_pt) |digit| {
        value = value * 256 + digit;
    }
    w.print("{d}", .{value}) catch {};

    // Format fractional part if present and non-zero
    if (digits_after_pt.len > 0) {
        var has_nonzero = false;
        for (digits_after_pt) |d| {
            if (d != 0) {
                has_nonzero = true;
                break;
            }
        }
        if (has_nonzero) {
            w.writeAll(".") catch {};
            // Convert base-256 fractional digits to decimal
            var frac: f64 = 0;
            var mult: f64 = 1.0 / 256.0;
            for (digits_after_pt) |digit| {
                frac += @as(f64, @floatFromInt(digit)) * mult;
                mult /= 256.0;
            }
            // Print fractional part (removing leading "0.")
            var frac_buf: [32]u8 = undefined;
            const frac_str = std.fmt.bufPrint(&frac_buf, "{d:.6}", .{frac}) catch "0";
            if (frac_str.len > 2 and std.mem.startsWith(u8, frac_str, "0.")) {
                w.writeAll(frac_str[2..]) catch {};
            }
        }
    }

    return buf[0..writer.pos];
}

// RocDec type definition (for missing export)
// Must match the structure of builtins.RocDec
pub const RocDec = builtins.dec.RocDec;

/// Converts a RocDec to an i128 integer
pub fn toI128(self: RocDec) i128 {
    return self.num;
}

/// Creates a RocDec from an f64 value, returns null if conversion fails
pub fn fromF64(f: f64) ?RocDec {
    // Simple conversion - the real implementation is in builtins/dec.zig
    const scaled = @as(i128, @intFromFloat(f * 1_000_000_000_000_000_000.0));
    return RocDec{ .num = scaled };
}

/// Represents an import statement in a module
pub const Import = struct {
    pub const Idx = enum(u32) {
        first = 0,
        _,
    };

    /// Sentinel value indicating unresolved import (max u32)
    pub const UNRESOLVED_MODULE: u32 = std.math.maxInt(u32);

    pub const Store = struct {
        /// Map from interned string idx to Import.Idx for deduplication
        map: std.AutoHashMapUnmanaged(base.StringLiteral.Idx, Import.Idx) = .{},
        /// List of interned string IDs indexed by Import.Idx
        imports: collections.SafeList(base.StringLiteral.Idx) = .{},
        /// List of interned ident IDs indexed by Import.Idx (parallel to imports)
        /// Used for efficient index-based lookups instead of string comparison
        import_idents: collections.SafeList(base.Ident.Idx) = .{},
        /// Resolved module indices, parallel to imports list
        /// Each entry is either a valid module index or UNRESOLVED_MODULE
        resolved_modules: collections.SafeList(u32) = .{},

        pub fn init() Store {
            return .{};
        }

        pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
            self.map.deinit(allocator);
            self.imports.deinit(allocator);
            self.import_idents.deinit(allocator);
            self.resolved_modules.deinit(allocator);
        }

        /// Get or create an Import.Idx for the given module name.
        /// The module name is first checked against existing imports by comparing strings.
        /// New imports are initially unresolved (UNRESOLVED_MODULE).
        /// If ident_idx is provided, it will be stored for index-based lookups.
        pub fn getOrPut(self: *Store, allocator: std.mem.Allocator, strings: *base.StringLiteral.Store, module_name: []const u8) !Import.Idx {
            return self.getOrPutWithIdent(allocator, strings, module_name, null);
        }

        /// Get or create an Import.Idx for the given module name, with an associated ident.
        /// The module name is first checked against existing imports by comparing strings.
        /// New imports are initially unresolved (UNRESOLVED_MODULE).
        /// If ident_idx is provided, it will be stored for index-based lookups.
        pub fn getOrPutWithIdent(self: *Store, allocator: std.mem.Allocator, strings: *base.StringLiteral.Store, module_name: []const u8, ident_idx: ?base.Ident.Idx) !Import.Idx {
            // First check if we already have this module name by comparing strings
            for (self.imports.items.items, 0..) |existing_string_idx, i| {
                const existing_name = strings.get(existing_string_idx);
                if (std.mem.eql(u8, existing_name, module_name)) {
                    // Found existing import with same name
                    // Update ident if provided and not already set
                    if (ident_idx) |ident| {
                        if (i < self.import_idents.len()) {
                            const current = self.import_idents.items.items[i];
                            if (current.isNone()) {
                                self.import_idents.items.items[i] = ident;
                            }
                        }
                    }
                    return @as(Import.Idx, @enumFromInt(i));
                }
            }

            // Not found - create new import
            const string_idx = try strings.insert(allocator, module_name);
            const idx = @as(Import.Idx, @enumFromInt(self.imports.len()));

            // Add to both the list and the map, with unresolved module initially
            _ = try self.imports.append(allocator, string_idx);
            _ = try self.import_idents.append(allocator, ident_idx orelse base.Ident.Idx.NONE);
            _ = try self.resolved_modules.append(allocator, Import.UNRESOLVED_MODULE);
            try self.map.put(allocator, string_idx, idx);

            return idx;
        }

        /// Get the ident index for an import, or null if not set
        pub fn getIdentIdx(self: *const Store, import_idx: Import.Idx) ?base.Ident.Idx {
            const idx = @intFromEnum(import_idx);
            if (idx >= self.import_idents.len()) return null;
            const ident = self.import_idents.items.items[idx];
            if (ident.isNone()) return null;
            return ident;
        }

        /// Get the resolved module index for an import, or null if unresolved
        pub fn getResolvedModule(self: *const Store, import_idx: Import.Idx) ?u32 {
            const idx = @intFromEnum(import_idx);
            if (idx >= self.resolved_modules.len()) return null;
            const resolved = self.resolved_modules.items.items[idx];
            if (resolved == Import.UNRESOLVED_MODULE) return null;
            return resolved;
        }

        /// Set the resolved module index for an import
        pub fn setResolvedModule(self: *Store, import_idx: Import.Idx, module_idx: u32) void {
            const idx = @intFromEnum(import_idx);
            if (idx < self.resolved_modules.len()) {
                self.resolved_modules.items.items[idx] = module_idx;
            }
        }

        /// Resolve all imports by matching import names to module names in the provided array.
        /// This sets the resolved_modules index for each import that matches a module.
        ///
        /// Parameters:
        /// - env: The module environment containing the string store for import names
        /// - available_modules: Array of module environments to match against
        ///
        /// For each import, this finds the module in available_modules whose module_name
        /// matches the import name and sets the resolved index accordingly.
        pub fn resolveImports(self: *Store, env: anytype, available_modules: []const *const @import("ModuleEnv.zig")) void {
            const import_count: usize = @intCast(self.imports.len());
            for (0..import_count) |i| {
                const import_idx: Import.Idx = @enumFromInt(i);
                const str_idx = self.imports.items.items[i];
                const import_name = env.common.getString(str_idx);

                // Find matching module in available_modules by comparing module names
                for (available_modules, 0..) |module_env, module_idx| {
                    if (std.mem.eql(u8, module_env.module_name, import_name)) {
                        self.setResolvedModule(import_idx, @intCast(module_idx));
                        break;
                    }
                }
            }
        }

        /// Serialize this Store to the given CompactWriter. The resulting Store
        /// in the writer's buffer will have offsets instead of pointers. Calling any
        /// methods on it or dereferencing its internal "pointers" (which are now
        /// offsets) is illegal behavior!
        pub fn serialize(
            self: *const Store,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!*const Store {
            // First, write the Store struct itself
            const offset_self = try writer.appendAlloc(allocator, Store);

            // Then serialize the sub-structures and update the struct
            offset_self.* = .{
                .map = .{}, // Map will be empty after deserialization (only used for deduplication during insertion)
                .imports = (try self.imports.serialize(allocator, writer)).*,
                .import_idents = (try self.import_idents.serialize(allocator, writer)).*,
                .resolved_modules = (try self.resolved_modules.serialize(allocator, writer)).*,
            };

            return @constCast(offset_self);
        }

        /// Add the given offset to the memory addresses of all pointers in `self`.
        pub fn relocate(self: *Store, offset: isize) void {
            self.imports.relocate(offset);
            self.import_idents.relocate(offset);
            self.resolved_modules.relocate(offset);
        }

        /// Uses extern struct to guarantee consistent field layout across optimization levels.
        pub const Serialized = extern struct {
            // Placeholder to match Store size - not serialized
            // Reserve space for hashmap (3 pointers for unmanaged hashmap internals)
            map: [3]u64,
            imports: collections.SafeList(base.StringLiteral.Idx).Serialized,
            import_idents: collections.SafeList(base.Ident.Idx).Serialized,
            resolved_modules: collections.SafeList(u32).Serialized,

            /// Serialize a Store into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                store: *const Store,
                allocator: std.mem.Allocator,
                writer: *CompactWriter,
            ) std.mem.Allocator.Error!void {
                // Serialize the imports SafeList
                try self.imports.serialize(&store.imports, allocator, writer);
                // Serialize the import_idents SafeList
                try self.import_idents.serialize(&store.import_idents, allocator, writer);
                // Serialize the resolved_modules SafeList
                try self.resolved_modules.serialize(&store.resolved_modules, allocator, writer);

                // Set map to all zeros; the space needs to be here,
                // but the map will be rebuilt during deserialization.
                self.map = .{ 0, 0, 0 };
                // Note: The map is not serialized as it's only used for deduplication during insertion
            }

            /// Deserialize this Serialized struct into a Store
            pub fn deserialize(self: *Serialized, offset: i64, allocator: std.mem.Allocator) std.mem.Allocator.Error!*Store {
                // Overwrite ourself with the deserialized version, and return our pointer after casting it to Store.
                const store = @as(*Store, @ptrFromInt(@intFromPtr(self)));

                store.* = .{
                    .map = .{}, // Will be repopulated below
                    .imports = self.imports.deserialize(offset).*,
                    .import_idents = self.import_idents.deserialize(offset).*,
                    .resolved_modules = self.resolved_modules.deserialize(offset).*,
                };

                // Pre-allocate the exact capacity needed for the map
                const import_count = store.imports.items.items.len;
                try store.map.ensureTotalCapacity(allocator, @intCast(import_count));

                // Repopulate the map - we know there's enough capacity since we
                // are deserializing from a Serialized struct
                for (store.imports.items.items, 0..) |string_idx, i| {
                    const import_idx = @as(Import.Idx, @enumFromInt(i));
                    store.map.putAssumeCapacityNoClobber(string_idx, import_idx);
                }

                return store;
            }
        };
    };
};

/// Represents a field in a record expression
pub const RecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    name: base.Ident.Idx,
    value: Expr.Idx,

    pub fn pushToSExprTree(self: *const RecordField, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("field");
        try tree.pushStringPair("name", cir.getIdent(self.name));
        const attrs = tree.beginNode();
        try cir.store.getExpr(self.value).pushToSExprTree(cir, tree, self.value);
        try tree.endNode(begin, attrs);
    }
};

/// Represents an external declaration from another module
pub const ExternalDecl = struct {
    /// Fully qualified name (e.g., "json.Json.utf8")
    qualified_name: base.Ident.Idx,
    /// Module this decl comes from (e.g., "json.Json")
    module_name: base.Ident.Idx,
    /// Local name within that module (e.g., "utf8")
    local_name: base.Ident.Idx,
    /// Type variable for this declaration
    type_var: TypeVar,
    /// Kind of external declaration
    kind: enum { value, type },
    /// Region where this was referenced
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };
    /// A safe list of external declarations
    pub const SafeList = collections.SafeList(ExternalDecl);

    pub fn pushToSExprTree(self: *const ExternalDecl, cir: anytype, tree: anytype) !void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);

        // Add fully qualified name
        try tree.pushStringPair("ident", cir.getIdent(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }

    pub fn pushToSExprTreeWithRegion(self: *const ExternalDecl, cir: anytype, tree: anytype, region: Region) !void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        // Add fully qualified name
        try tree.pushStringPair("ident", cir.getIdent(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }
};

// Real Report type from the reporting module
pub const Report = reporting.Report;

/// Checks if a type is castable for index type conversions
pub fn isCastable(comptime T: type) bool {
    return switch (T) {
        Expr.Idx,
        Pattern.Idx,
        Statement.Idx,
        TypeAnno.Idx,
        Def.Idx,
        TypeHeader.Idx,
        RecordField.Idx,
        Pattern.RecordDestruct.Idx,
        Expr.IfBranch.Idx,
        Expr.Match.Branch.Idx,
        WhereClause.Idx,
        Annotation.Idx,
        TypeAnno.RecordField.Idx,
        ExposedItem.Idx,
        Expr.Match.BranchPattern.Idx,
        PatternRecordField.Idx,
        Node.Idx,
        TypeVar,
        => true,
        else => false,
    };
}

/// Safely casts between compatible index types
pub fn castIdx(comptime From: type, comptime To: type, idx: From) To {
    return @as(To, @enumFromInt(@intFromEnum(idx)));
}
