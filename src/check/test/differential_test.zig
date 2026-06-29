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

test "differential: list literal (variable-arity) matches" {
    // Exercises e_list: a non-empty list whose elements are unified pairwise
    // against the first element's var (the deferred interleaved-unify path).
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    xs = [1, 2, 3, 4]
        \\    ys = ["a", "b"]
        \\    (xs, ys)
        \\}
    );
}

test "differential: list with element type mismatch matches" {
    // Exercises e_list's error path (unifyInContext fails on a later element),
    // confirming the deferred unify loop produces the same break-on-error
    // behavior and the same diagnostic count.
    try expectIterMatchesRecursive(
        \\main! = |_args| [1, "two", 3]
    );
}

test "differential: tuple (variable-arity, mixed element types) matches" {
    // Exercises e_tuple with heterogeneous element types.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    t = (1, "two", 3.0, [4, 5])
        \\    t
        \\}
    );
}

test "differential: tag application (variable-arity args) matches" {
    // Exercises e_tag with one and multiple arguments.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    a = Some(1)
        \\    b = Pair(1, "two")
        \\    (a, b)
        \\}
    );
}

test "differential: method call / dispatch call matches" {
    // Exercises e_method_call (which the checker rewrites to e_dispatch_call)
    // via receiver.method(arg) syntax, including the call-arg flag per argument.
    try expectIterMatchesRecursive(
        \\func = |x, y| {
        \\    add_x = |a| a.plus(x)
        \\    add_y = |b| b.plus(y)
        \\    add_x(5).plus(add_y(5))
        \\}
        \\
        \\main! = |_args| func(10, 20)
    );
}

test "differential: e_call (apply) — interleaved func/args migration matches" {
    // Exercises the migrated e_call apply path: a generalized immediately-invoked
    // lambda (`(|x| ...)(arg)`, forcing the instantiate-on-generalized branch in
    // the `call_after_func` resume step), a top-level multi-arg function call
    // (rigid-shared args unified pairwise), a nested call passed as an argument
    // (per-arg `checking_call_arg` re-assertion), and an effectful call (the
    // effectful-func `does_fx` accumulator). Covers func + arg child scheduling,
    // did_err reconstruction, and the post-args constraint-publishing body.
    try expectIterMatchesRecursive(
        \\add = |a, b| a + b
        \\
        \\pick = |x, y| if x > y x else y
        \\
        \\main! = |_args| {
        \\    iife = (|n| n + 1)(10)
        \\    summed = add(1, 2)
        \\    picked = pick(summed, iife)
        \\    nested = add(add(3, 4), 5)
        \\    (iife, summed, picked, nested)
        \\}
    );
}

test "differential: e_call with bare-lambda (unmigrated) arg keeps call-arg flag" {
    // Regression guard: a function-literal call argument that canonicalizes to a
    // bare `e_lambda` (no captures, so NOT a migrated kind) takes the iterative
    // driver's escape hatch. It must still be checked with `checking_call_arg`
    // set, exactly like the recursive arm's per-arg flag — otherwise the lambda
    // is wrongly generalized and the result stays polymorphic (`a`) instead of
    // defaulting (`Dec`). The literal-defaulting outcome differs unless the flag
    // is honored, so this exercises the escape-hatch `call_arg` re-assertion.
    try expectIterMatchesRecursive(
        \\apply2 = |f, x| f(x)
        \\result = apply2(|n| n + 1, 10)
    );
}

test "differential: for-loop over range (type dispatch) matches" {
    // Exercises the type-dispatch call path (e_type_method_call /
    // e_type_dispatch_call) used to drive an inclusive-range `for` iterator,
    // plus e_run_low_level lowering reached through the builtin range protocol.
    try expectIterMatchesRecursive(
        \\total : U64
        \\total = {
        \\    var sum_ = 0
        \\    for i in 1..=5 {
        \\        sum_ = sum_ + i
        \\    }
        \\    sum_
        \\}
        \\
        \\main! = |_args| total
    );
}

test "differential: for-loop over list (interleaved pattern/iterable/body) matches" {
    // Exercises e_for as an interleaving kind: the loop pattern is checked
    // inline in `.enter`, the `iterable` (a list) is scheduled as a child, the
    // `for_after_iterable` resume step builds the iter/next dispatch constraints
    // from the pattern's item_var and the iterable's var, and the `body` is
    // scheduled before `.exit` unifies the loop expr with `{}`.
    try expectIterMatchesRecursive(
        \\sum_list : List(U64) -> U64
        \\sum_list = |items| {
        \\    var total = 0
        \\    for x in items {
        \\        total = total + x
        \\    }
        \\    total
        \\}
        \\
        \\main! = |_args| sum_list([1, 2, 3])
    );
}

test "differential: string interpolation (interleaved parts) matches" {
    // Exercises e_interpolation: the `first` segment, an interpolated value
    // child (`x`), and a following segment child, with the between-child
    // str-var unifies and the iterator-protocol constraint built in `.exit`.
    try expectIterMatchesRecursive(
        \\greet = |name| {
        \\    x = name
        \\    "a${x}b"
        \\}
        \\
        \\main! = |_args| greet("world")
    );
}

test "differential: string interpolation with multiple parts matches" {
    // Two interpolated segments drive the per-pair resume loop more than once,
    // exercising the cursor advance across `interp_after_part_value` /
    // `interp_after_part_segment`.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    a = 1
        \\    b = 2
        \\    "x=${a.to_str()} y=${b.to_str()}!"
        \\}
    );
}

test "differential: if/else (single branch, no expected type) matches" {
    // No annotation: the no-expected pairwise branch path (branch_var ref +
    // final-else pairwise unify).
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 1
        \\    if x > 0 "pos" else "nonpos"
        \\}
    );
}

test "differential: if/else-if/else (multi-branch pairwise) matches" {
    // Three branch conds + final else: exercises the cursor loop and the
    // per-branch pairwise unify with running last_if_branch contexts.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 5
        \\    if x > 10 {
        \\        "big"
        \\    } else if x > 3 {
        \\        "mid"
        \\    } else if x > 0 {
        \\        "small"
        \\    } else {
        \\        "nonpos"
        \\    }
        \\}
    );
}

test "differential: if/else with expected return annotation (accumulator path) matches" {
    // The annotation supplies expected.branch_result, so the branch_acc
    // accumulator path (checkBranchBodyAgainstExpected + closing
    // unify(if, acc) + unify(if, expected_ret)) is exercised.
    try expectIterMatchesRecursive(
        \\classify : I64 -> Str
        \\classify = |x| if x > 0 "pos" else if x == 0 "zero" else "neg"
        \\
        \\main! = |_args| classify(7)
    );
}

test "differential: if branches feeding numeric meet (no annotation) matches" {
    // Branch bodies are numeric literals that must meet to a common type via
    // the pairwise path; exercises branch-body unification rather than Str.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 2
        \\    n = if x > 1 100 else if x > 0 10 else 1
        \\    n
        \\}
    );
}

test "differential: if branch body type mismatch (error-recovery break) matches" {
    // Second branch's body type disagrees with the first: drives the
    // no-expected pairwise unify failure + error-recovery break that poisons
    // remaining branch bodies to .err.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 1
        \\    if x > 2 "a" else if x > 1 2 else "c"
        \\}
    );
}

test "differential: nested if in branch body matches" {
    // An if expression nested inside another if's branch body: the inner if is
    // scheduled as a child frame under the outer if's body slot, exercising the
    // recursion-flattening across nested conditionals.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 3
        \\    y = 4
        \\    if x > 0 {
        \\        if y > 0 "both" else "x-only"
        \\    } else {
        \\        "neither"
        \\    }
        \\}
    );
}

test "differential: match on tag union (helper-delegating) matches" {
    // Exercises the e_match arm: cond child, multiple branches each with their
    // own hoist scope, per-pattern checkPattern + cond/pattern unify, and the
    // between-branch pairwise body unify (no expected return type → pairwise
    // path with the first branch's body var).
    try expectIterMatchesRecursive(
        \\Color : [Red, Green, Blue]
        \\
        \\name : Color -> Str
        \\name = |c|
        \\    match c {
        \\        Red => "red"
        \\        Green => "green"
        \\        Blue => "blue"
        \\    }
        \\
        \\main! = |_args| name(Red)
    );
}

test "differential: match with payload binding and guard matches" {
    // Branch patterns bind a payload var (checkPattern bindings + cond unify)
    // and a guard (checkExprWithHoistSelectionSuppressed + freshBool unify),
    // exercising the suppression raise/lower across the scheduled guard/body.
    try expectIterMatchesRecursive(
        \\classify : [Some(I64), None] -> Str
        \\classify = |opt|
        \\    match opt {
        \\        Some(n) if n > 0 => "positive"
        \\        Some(_) => "nonpos"
        \\        None => "none"
        \\    }
        \\
        \\main! = |_args| classify(Some(3))
    );
}

test "differential: match with expected return annotation (accumulator path) matches" {
    // The annotation supplies expected.branch_result, so the branch_acc
    // accumulator path (checkBranchBodyAgainstExpected) is exercised instead of
    // the pairwise path.
    try expectIterMatchesRecursive(
        \\to_num : [A, B, C] -> I64
        \\to_num = |t|
        \\    match t {
        \\        A => 1
        \\        B => 2
        \\        C => 3
        \\    }
        \\
        \\main! = |_args| to_num(B)
    );
}

test "differential: match nested inside if branch body matches" {
    // A match nested under an if branch body: the match runs as a re-entrant
    // checkExpr under the iterative if-body frame, flattening recursion across
    // the two interleaving kinds.
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 1
        \\    if x > 0 {
        \\        match x {
        \\            0 => "zero"
        \\            _ => "nonzero"
        \\        }
        \\    } else {
        \\        "neg"
        \\    }
        \\}
    );
}
