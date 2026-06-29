//! Two-pass differential harness for the iterative checker conversion.
//!
//! Type-checks the same module twice in independent `Check` instances — once
//! forced fully-recursive (`force_recursive = true`), once with the iterative
//! driver (`force_recursive = false`) — and asserts the inferred def types are
//! identical and the diagnostic COUNT is unchanged. Per the fidelity rule,
//! inferred types must be preserved exactly while diagnostics may reorder, so
//! we compare types exactly but diagnostics by count (catching added/dropped
//! diagnostics without flagging pure reordering). The snapshot suite remains
//! the primary oracle for full diagnostic content/ordering. This is the
//! REQUIRED results-preservation guarantee for the recursion-to-work-stack
//! migration.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

/// Type-check `source` twice — once forced fully-recursive, once with the
/// iterative driver — and assert identical rendered def types and an unchanged
/// diagnostic count (reordering is allowed per the fidelity rule).
pub fn expectIterMatchesRecursive(source: []const u8) !void {
    var rec = try TestEnv.initWithMode("Diff", source, true);
    defer rec.deinit();
    var itr = try TestEnv.initWithMode("Diff", source, false);
    defer itr.deinit();

    const rec_types = try rec.renderAllDefTypes();
    defer rec.gpa.free(rec_types);
    const itr_types = try itr.renderAllDefTypes();
    defer itr.gpa.free(itr_types);
    try std.testing.expectEqualStrings(rec_types, itr_types);

    const rec_diags = try rec.module_env.getDiagnostics();
    defer rec.gpa.free(rec_diags);
    const itr_diags = try itr.module_env.getDiagnostics();
    defer itr.gpa.free(itr_diags);
    try std.testing.expectEqual(rec_diags.len, itr_diags.len);
}

test "differential: simple module matches across recursive/iterative" {
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 1
        \\    y = (x, 2)
        \\    y.0
        \\}
    );
}

test "differential: tuple access matches across recursive/iterative" {
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    t = (1, "two", 3)
        \\    a = t.0
        \\    b = t.1
        \\    (a, b)
        \\}
    );
}

test "differential: deeply nested blocks match across recursive/iterative" {
    // The block `final_expr` spine — the recursion flattened by the e_block
    // migration. Statement lists are empty, so only nesting via final_expr.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    {
        \\        {
        \\            {
        \\                42
        \\            }
        \\        }
        \\    }
        \\}
    );
}

test "differential: leaf kinds — int/dec/frac/str/empty literals" {
    // Exercises e_num (typed via annotation), e_dec_small, e_frac_f64,
    // e_str_segment, e_empty_list, e_empty_record, e_bytes_literal.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    n : U64
        \\    n = 42
        \\    d = 3.14
        \\    f = 1.5f64
        \\    s = "hello"
        \\    el = []
        \\    er = {}
        \\    by = "abc".Utf8
        \\    (n, d, f, s, el, er, by)
        \\}
    );
}

test "differential: leaf kinds — typed/suffixed numeric literals" {
    // Exercises e_typed_int, e_typed_frac, e_num with suffix, e_dec.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    a = 100u8
        \\    b = 7i32
        \\    c = 2.5dec
        \\    d = 9.0f32
        \\    (a, b, c, d)
        \\}
    );
}

test "differential: leaf kinds — local lookups and zero-arg tag" {
    // Exercises e_lookup_local (both the not-processed top-level path via the
    // reference to `helper`, and ordinary local binding lookups) and
    // e_zero_argument_tag.
    try expectIterMatchesRecursive(
        \\helper = 5
        \\
        \\main! = |_args| {
        \\    x = helper
        \\    y = x
        \\    tag = None
        \\    (y, tag)
        \\}
    );
}

test "differential: leaf kinds — crash and ellipsis diverging exprs" {
    // Exercises e_crash and e_ellipsis (both produce a free flex var).
    try expectIterMatchesRecursive(
        \\foo = |_x| crash "boom"
        \\
        \\bar = |_x| ...
        \\
        \\main! = |_args| (foo, bar)
    );
}

test "differential: leaf kinds — recursive local lookup (processing path)" {
    // Exercises e_lookup_local's `.processing` recursive-function path.
    try expectIterMatchesRecursive(
        \\countdown : U64 -> U64
        \\countdown = |n| if n == 0 0 else countdown(n - 1)
        \\
        \\main! = |_args| countdown(5)
    );
}

test "differential: blocks with statements and nested-block bindings match" {
    // Exercises the statement loop (s_decl with a block RHS) plus a final-expr
    // block, so both the statement path and the spine are covered.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = {
        \\        y = 1
        \\        z = (y, y)
        \\        z.1
        \\    }
        \\    w = x + 1
        \\    {
        \\        a = w
        \\        a
        \\    }
        \\}
    );
}

test "differential: binop and unary kinds match" {
    // Exercises e_binop (+, *, >), e_unary_minus (-a), e_unary_not (!).
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    a = 1 + 2 * 3
        \\    b = -a
        \\    c = !(a > 0)
        \\    (a, b, c)
        \\}
    );
}

test "differential: field access matches" {
    // Exercises e_field_access (record field projection).
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    r = { x: 1, y: 2 }
        \\    (r.x, r.y)
        \\}
    );
}

test "differential: closure (capture wrapper around lambda) matches" {
    // Exercises e_closure forwarding call-arg status to its inner lambda.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    f = |x| x + 1
        \\    g = |y| y * 2
        \\    (f(2), g(3))
        \\}
    );
}

test "differential: explicit return matches" {
    // Exercises e_return (early return with an expected return type).
    try expectIterMatchesRecursive(
        \\get : U64 -> U64
        \\get = |n| {
        \\    if n > 0 {
        \\        return n
        \\    }
        \\    0
        \\}
        \\
        \\main! = |_args| get(3)
    );
}

test "differential: dbg matches" {
    // Exercises e_dbg (evaluates inner expr, returns {}, own does_fx false).
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 5
        \\    dbg x
        \\    x
        \\}
    );
}

test "differential: expect and structural equality match" {
    // Exercises e_expect (statement) and the `==` equality node it wraps.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 1
        \\    expect x == 1
        \\    x
        \\}
    );
}

test "differential: nominal type construction matches" {
    // Exercises e_nominal (constructing a value of a nominal tag-union type).
    try expectIterMatchesRecursive(
        \\Color := [Red, Green, Blue]
        \\
        \\main! = |_args| Color.Red
    );
}
