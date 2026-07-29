//! The single authority for literal-defaulting decisions.
//!
//! "Given this literal's (or literal group's) constraints, what type does it
//! default to?" is answered here and nowhere else. The checker's defaulting
//! passes, the canonical-type-key builder, the checked artifact's
//! default-phase scan, and monotype solving/lowering all call in here, so the
//! decisions cannot drift apart between stages — there is no cross-module
//! "must agree" contract left to maintain, because there is only one
//! implementation to agree with.

const std = @import("std");
const base = @import("base");
const types = @import("types.zig");
const numeral = @import("numeral.zig");

const Ident = base.Ident;
const StaticDispatchConstraint = types.StaticDispatchConstraint;
const LiteralKind = StaticDispatchConstraint.LiteralKind;

/// The builtin type a defaulted literal lands on when no candidate probing
/// applies (or when probing found no satisfier): numerals default to Dec,
/// quotes and interpolations to Str.
pub const DefaultTarget = enum {
    dec,
    str,
};

/// The method-name idents of the literal-conversion hooks. Callers resolve
/// these once from their ident store; the decision of what they mean lives
/// here.
pub const LiteralMethodIdents = struct {
    from_numeral: Ident.Idx,
    from_quote: Ident.Idx,
    from_interpolation: Ident.Idx,
};

/// The literal kind one constraint carries: a `from_literal` origin carries
/// its kind directly, and a `where_clause` contract naming a literal-conversion
/// hook carries that hook's kind. The two must be interchangeable here: a
/// `where` clause like `b.from_numeral : Numeral -> ...` asserts exactly what a
/// literal's own conversion constraint asserts (b is creatable from a numeral
/// literal), and it is the only way an annotation can express that constraint —
/// so if it were not defaulting-eligible, annotating a def with its own
/// inferred type would strip callers of defaulting and change whether their
/// programs check.
pub fn constraintLiteralKind(
    idents: LiteralMethodIdents,
    constraint: StaticDispatchConstraint,
) ?LiteralKind {
    return switch (constraint.origin) {
        .from_literal => |lit| lit,
        .where_clause => if (constraint.fn_name.eql(idents.from_numeral))
            .numeral
        else if (constraint.fn_name.eql(idents.from_quote))
            .quote
        else if (constraint.fn_name.eql(idents.from_interpolation))
            .interpolation
        else
            null,
        .desugared_binop,
        .desugared_unaryop,
        .method_call,
        => null,
    };
}

/// The literal kind a flex var defaults as when it carries literal-conversion
/// constraints (see `constraintLiteralKind`), by fixed precedence:
/// numeral > quote > interpolation.
///
/// A var can carry several kinds at once (a flex/flex merge like
/// `if c 1 else "s"` unions the two literals' constraint sets). Such a var can
/// never type-check, but the kind chosen here decides which head default is
/// attempted (Dec vs Str) and hence which literal-kind diagnostic fires — so
/// it must not depend on constraint storage order (which unify side each
/// literal arrived on), or mirror-image programs would get different keys and
/// diagnostics.
pub fn dominantKind(
    idents: LiteralMethodIdents,
    constraints: []const StaticDispatchConstraint,
) ?LiteralKind {
    var has_quote = false;
    var has_interpolation = false;
    for (constraints) |constraint| {
        switch (constraintLiteralKind(idents, constraint) orelse continue) {
            .numeral => return .numeral,
            .quote => has_quote = true,
            .interpolation => has_interpolation = true,
        }
    }
    return if (has_quote) .quote else if (has_interpolation) .interpolation else null;
}

/// The default target type for a literal kind. Exhaustive over `LiteralKind`:
/// adding a kind fails to compile until its default is chosen here.
pub fn defaultTargetForKind(kind: LiteralKind) DefaultTarget {
    return switch (kind) {
        .numeral => .dec,
        .quote, .interpolation => .str,
    };
}

/// Candidate order for numeral defaulting (first satisfier wins). `Dec` (the
/// canonical default) heads the list; then integers — signed before unsigned,
/// `I64` first (Roc's historical integer default), wider before narrower so a
/// tie never lands on a type that overflows sooner than it must; floats last.
/// Order past `Dec` only matters when the constraints refute `Dec` yet accept
/// several candidates — a pinned concrete arg or return admits exactly one
/// regardless of order. That single-admission claim holds because builtin
/// numeric methods are homogeneous (`T, T -> T`, e.g. `Dec.plus : Dec, Dec ->
/// Dec`): every signature position is the dispatcher type itself, so pinning
/// ANY position pins the candidate.
pub const numeral_default_candidates = [_]numeral.Target{ .dec, .i64, .u64, .i128, .u128, .i32, .u32, .i16, .u16, .i8, .u8, .f64, .f32 };

/// When a still-open literal-ish var's default is applied. A var that
/// checking leaves open (a generalized literal no instantiation pinned) is
/// published with one of the `mono_*` phases; monotype solving materializes
/// the corresponding `DefaultTarget` when the var is still unresolved at
/// specialization time.
pub const NumericDefaultPhase = enum(u8) {
    /// The default was applied during checking; the var must not survive to
    /// publication in this state.
    checking_finalized,
    /// Defaults to Dec if still unresolved at monomorphic specialization.
    mono_specialization,
    /// Defaults to Str if still unresolved at monomorphic specialization
    /// (from_quote / from_interpolation literals).
    mono_specialization_str,
};

/// The target a mono-specialization default phase materializes, or null for
/// `checking_finalized` (which must never reach a consumer of this mapping —
/// callers treat null as an invariant violation with their own reporting).
pub fn defaultTargetForPhase(phase: NumericDefaultPhase) ?DefaultTarget {
    return switch (phase) {
        .checking_finalized => null,
        .mono_specialization => .dec,
        .mono_specialization_str => .str,
    };
}

/// The method-name idents that make an arithmetic constraint defaultable.
/// Callers resolve these once from their ident store; the decision itself
/// lives here.
pub const ArithmeticMethodIdents = struct {
    plus: Ident.Idx,
    minus: Ident.Idx,
    times: Ident.Idx,
    div_by: Ident.Idx,
    div_trunc_by: Ident.Idx,
    rem_by: Ident.Idx,
    negate: Ident.Idx,
};

/// Whether a constraint is a desugared arithmetic operation whose receiver
/// may default (to Dec) when nothing else pins it: `a + b` on an otherwise
/// unconstrained var behaves like a numeric literal for defaulting purposes.
pub fn isDefaultableArithmeticConstraint(
    idents: ArithmeticMethodIdents,
    constraint: StaticDispatchConstraint,
) bool {
    return switch (constraint.origin) {
        .desugared_binop => constraint.fn_name.eql(idents.plus) or
            constraint.fn_name.eql(idents.minus) or
            constraint.fn_name.eql(idents.times) or
            constraint.fn_name.eql(idents.div_by) or
            constraint.fn_name.eql(idents.div_trunc_by) or
            constraint.fn_name.eql(idents.rem_by),
        .desugared_unaryop => constraint.fn_name.eql(idents.negate),
        .from_literal,
        .method_call,
        .where_clause,
        => false,
    };
}

/// The default phase for a var that checking publishes still-open: numeral
/// literals and defaultable arithmetic default to Dec at specialization,
/// quotes/interpolations to Str, anything else has no default.
pub fn numericDefaultPhase(
    arithmetic_idents: ArithmeticMethodIdents,
    literal_idents: LiteralMethodIdents,
    constraints: []const StaticDispatchConstraint,
) ?NumericDefaultPhase {
    const kind = dominantKind(literal_idents, constraints);
    if (kind == .numeral) return .mono_specialization;
    for (constraints) |constraint| {
        if (isDefaultableArithmeticConstraint(arithmetic_idents, constraint)) return .mono_specialization;
    }
    if (kind == .quote or kind == .interpolation) return .mono_specialization_str;
    return null;
}

fn testIdent(idx: u29) Ident.Idx {
    return .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = idx };
}

const test_literal_idents = LiteralMethodIdents{
    .from_numeral = testIdent(1),
    .from_quote = testIdent(2),
    .from_interpolation = testIdent(3),
};

test "dominant kind precedence is numeral > quote > interpolation" {
    const region = base.Region.zero();
    const numeral_constraint = StaticDispatchConstraint{
        .fn_name = Ident.Idx.NONE,
        .fn_var = undefined,
        .origin = .{ .from_literal = .{ .numeral = types.NumeralInfo.testOnlyInt(1, false, region) } },
    };
    const quote_constraint = StaticDispatchConstraint{
        .fn_name = Ident.Idx.NONE,
        .fn_var = undefined,
        .origin = .{ .from_literal = .quote },
    };
    const interpolation_constraint = StaticDispatchConstraint{
        .fn_name = Ident.Idx.NONE,
        .fn_var = undefined,
        .origin = .{ .from_literal = .interpolation },
    };
    const method_constraint = StaticDispatchConstraint{
        .fn_name = Ident.Idx.NONE,
        .fn_var = undefined,
        .origin = .method_call,
    };

    try std.testing.expectEqual(@as(?LiteralKind, null), dominantKind(test_literal_idents, &.{method_constraint}));
    try std.testing.expectEqual(@as(?LiteralKind, .interpolation), dominantKind(test_literal_idents, &.{ method_constraint, interpolation_constraint }));
    try std.testing.expectEqual(@as(?LiteralKind, .quote), dominantKind(test_literal_idents, &.{ interpolation_constraint, quote_constraint }));
    // Order-independent: the numeral wins from either side.
    try std.testing.expectEqual(@as(?LiteralKind, .numeral), dominantKind(test_literal_idents, &.{ quote_constraint, numeral_constraint }));
    try std.testing.expectEqual(@as(?LiteralKind, .numeral), dominantKind(test_literal_idents, &.{ numeral_constraint, quote_constraint }));
}

test "where-clause literal-conversion hooks carry their hook's literal kind" {
    const where_from_numeral = StaticDispatchConstraint{
        .fn_name = test_literal_idents.from_numeral,
        .fn_var = undefined,
        .origin = .{ .where_clause = .{} },
    };
    const where_from_quote = StaticDispatchConstraint{
        .fn_name = test_literal_idents.from_quote,
        .fn_var = undefined,
        .origin = .{ .where_clause = .{} },
    };
    const where_from_interpolation = StaticDispatchConstraint{
        .fn_name = test_literal_idents.from_interpolation,
        .fn_var = undefined,
        .origin = .{ .where_clause = .{ .body_required = true } },
    };
    const where_plus = StaticDispatchConstraint{
        .fn_name = testIdent(4),
        .fn_var = undefined,
        .origin = .{ .where_clause = .{} },
    };
    // A method-call constraint named like a hook is NOT a literal conversion:
    // only the annotation's `where` contract and the literal's own origin are.
    const method_call_from_numeral = StaticDispatchConstraint{
        .fn_name = test_literal_idents.from_numeral,
        .fn_var = undefined,
        .origin = .method_call,
    };

    try std.testing.expectEqual(@as(?LiteralKind, .numeral), constraintLiteralKind(test_literal_idents, where_from_numeral));
    try std.testing.expectEqual(@as(?LiteralKind, .quote), constraintLiteralKind(test_literal_idents, where_from_quote));
    try std.testing.expectEqual(@as(?LiteralKind, .interpolation), constraintLiteralKind(test_literal_idents, where_from_interpolation));
    try std.testing.expectEqual(@as(?LiteralKind, null), constraintLiteralKind(test_literal_idents, where_plus));
    try std.testing.expectEqual(@as(?LiteralKind, null), constraintLiteralKind(test_literal_idents, method_call_from_numeral));

    try std.testing.expectEqual(@as(?LiteralKind, .numeral), dominantKind(test_literal_idents, &.{ where_plus, where_from_numeral }));
    try std.testing.expectEqual(@as(?LiteralKind, null), dominantKind(test_literal_idents, &.{where_plus}));
}

test "default targets are Dec for numerals and Str for string kinds" {
    try std.testing.expectEqual(DefaultTarget.dec, defaultTargetForKind(.numeral));
    try std.testing.expectEqual(DefaultTarget.str, defaultTargetForKind(.quote));
    try std.testing.expectEqual(DefaultTarget.str, defaultTargetForKind(.interpolation));
    try std.testing.expectEqual(@as(?DefaultTarget, .dec), defaultTargetForPhase(.mono_specialization));
    try std.testing.expectEqual(@as(?DefaultTarget, .str), defaultTargetForPhase(.mono_specialization_str));
    try std.testing.expectEqual(@as(?DefaultTarget, null), defaultTargetForPhase(.checking_finalized));
    try std.testing.expectEqual(numeral.Target.dec, numeral_default_candidates[0]);
}
