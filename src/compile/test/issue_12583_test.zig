//! Regression test for https://github.com/roc-lang/roc/issues/12583.
//!
//! At `--opt speed`, passing a `Bool` literal to an argument the callee only
//! stores (never inspects) must not make SpecConstr inline the callee and its
//! callees at every call site. The program with `Bool` literals lowers to a
//! post-check program about the size of the same program with computed
//! `Bool`s of the same values.
const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

fn app(comptime false_arg: []const u8, comptime true_arg: []const u8) []const u8 {
    return
    \\Command : [Insert(U64), Flag(Bool)]
    \\
    \\apply : List(U64), Command -> List(U64)
    \\apply = |xs, cmd| match cmd {
    \\    Insert(n) => xs.append(n)
    \\    Flag(on) => if on xs.append(1) else xs
    \\}
    \\
    \\State : { xs : List(U64), name : Str, on : Bool, items : List(Str) }
    \\
    \\step_in : State, Str, Command -> State
    \\step_in = |s, id, command| match s.items.find_first(|f| f == id) {
    \\    Ok(_) => {
    \\        ys = apply(s.xs, command)
    \\        z0 = ys.map(|x| x + 1).keep_if(|x| x % 2 != 0)
    \\        z1 = z0.map(|x| x + 2).keep_if(|x| x % 3 != 0)
    \\        z2 = z1.map(|x| x + 3).keep_if(|x| x % 4 != 0)
    \\        z3 = z2.map(|x| x + 4).keep_if(|x| x % 5 != 0)
    \\        z4 = z3.map(|x| x + 5).keep_if(|x| x % 6 != 0)
    \\        z5 = z4.map(|x| x + 6).keep_if(|x| x % 7 != 0)
    \\        z6 = z5.map(|x| x + 7).keep_if(|x| x % 8 != 0)
    \\        z7 = z6.map(|x| x + 8).keep_if(|x| x % 9 != 0)
    \\        { ..s, xs: z7 }
    \\    }
    \\    Err(_) => s
    \\}
    \\
    \\step_now : State, Str, Command -> State
    \\step_now = |s, id, command| step_in({ ..s, xs: s.xs.append(1) }, id, command)
    \\
    \\select : State, Str, Bool -> State
    \\select = |s, id, on|
    \\    if id == s.name {
    \\        { ..s, on: on }
    \\    } else {
    \\        old = s.name
    \\        s1 = { ..s, name: id, on: on }
    \\        s2 = if old.is_empty() s1 else step_now(s1, old, Flag(Bool.False))
    \\        if id.is_empty() s2 else step_now(s2, id, Flag(Bool.True))
    \\    }
    \\
    \\main! = |args| {
    \\    n = args.len()
    \\    s0 = { xs: [n, n + 1, n + 2], name: "", on: Bool.False, items: ["f0", "f1", "f2"] }
    \\    s1 = select(s0, "f0", 
    ++ false_arg ++ ")\n    s2 = select(s1, \"f1\", " ++ true_arg ++
        ")\n    s3 = select(s2, \"f2\", " ++ false_arg ++
        ")\n    s4 = select(s3, \"f3\", " ++ true_arg ++
        ")\n    s5 = select(s4, \"f4\", " ++ false_arg ++
        ")\n    s6 = select(s5, \"f5\", " ++ true_arg ++
        ")\n    s7 = select(s6, \"f6\", " ++ false_arg ++
        ")\n    s8 = select(s7, \"f7\", " ++ true_arg ++
        ")\n    s9 = select(s8, \"f8\", " ++ false_arg ++
        ")\n    s10 = select(s9, \"f9\", " ++ true_arg ++
        ")\n    s11 = select(s10, \"f10\", " ++ false_arg ++
        ")\n    s12 = select(s11, \"f11\", " ++ true_arg ++ ")\n" ++
        \\    echo!(s12.xs.len().to_str())
        \\    Ok({})
        \\}
    ;
}

const literal_bool_app = app("Bool.False", "Bool.True");
const computed_bool_app = app("n > 99", "n < 99");

/// Lower the app with the post-check settings `roc build` uses at its
/// default `--opt speed` and report the post-check program size.
fn liftedExprCount(app_body: []const u8) harness.LowerToLirHarnessError!usize {
    var lifted_exprs: usize = 0;
    try harness.expectLowersToLirWithOptions(app_body, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .lifted_expr_count_out = &lifted_exprs,
    });
    return lifted_exprs;
}

test "issue 12583: a Bool literal the callee only stores does not inline the callee at every call site" {
    const literal_count = try liftedExprCount(literal_bool_app);
    const computed_count = try liftedExprCount(computed_bool_app);
    // `select` never inspects its `Bool`, so a literal there is no more
    // reason to specialize or inline `select` than a computed `Bool` is.
    if (literal_count > computed_count * 2) {
        std.debug.print(
            "post-check program held {d} expressions with Bool literals and {d} with computed Bools\n",
            .{ literal_count, computed_count },
        );
        return error.PostCheckProgramGrewFromUninspectedLiteral;
    }
}
