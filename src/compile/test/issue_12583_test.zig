//! Regression test for https://github.com/roc-lang/roc/issues/12583.
//!
//! `select` only stores its `Bool` argument. Passing it a `Bool` literal must
//! not make an optimized build copy `select` (and the large `step_in` it
//! reaches) into every call site: the literal-argument program must lower to
//! no more LIR than the same program passing computed `Bool`s, which differs
//! only by twelve extra comparisons.

const std = @import("std");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

const declarations =
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
;

const literal_main =
    \\main! = |args| {
    \\    n = args.len()
    \\    s0 = { xs: [n, n + 1, n + 2], name: "", on: Bool.False, items: ["f0", "f1", "f2"] }
    \\    s1 = select(s0, "f0", Bool.False)
    \\    s2 = select(s1, "f1", Bool.True)
    \\    s3 = select(s2, "f2", Bool.False)
    \\    s4 = select(s3, "f3", Bool.True)
    \\    s5 = select(s4, "f4", Bool.False)
    \\    s6 = select(s5, "f5", Bool.True)
    \\    s7 = select(s6, "f6", Bool.False)
    \\    s8 = select(s7, "f7", Bool.True)
    \\    s9 = select(s8, "f8", Bool.False)
    \\    s10 = select(s9, "f9", Bool.True)
    \\    s11 = select(s10, "f10", Bool.False)
    \\    s12 = select(s11, "f11", Bool.True)
    \\    echo!(s12.xs.len().to_str())
    \\    Ok({})
    \\}
;

const computed_main =
    \\main! = |args| {
    \\    n = args.len()
    \\    s0 = { xs: [n, n + 1, n + 2], name: "", on: Bool.False, items: ["f0", "f1", "f2"] }
    \\    s1 = select(s0, "f0", n > 99)
    \\    s2 = select(s1, "f1", n < 99)
    \\    s3 = select(s2, "f2", n > 99)
    \\    s4 = select(s3, "f3", n < 99)
    \\    s5 = select(s4, "f4", n > 99)
    \\    s6 = select(s5, "f5", n < 99)
    \\    s7 = select(s6, "f6", n > 99)
    \\    s8 = select(s7, "f7", n < 99)
    \\    s9 = select(s8, "f8", n > 99)
    \\    s10 = select(s9, "f9", n < 99)
    \\    s11 = select(s10, "f10", n > 99)
    \\    s12 = select(s11, "f11", n < 99)
    \\    echo!(s12.xs.len().to_str())
    \\    Ok({})
    \\}
;

const optimized_options: harness.LirLoweringOptions = .{
    .inline_mode = .wrappers,
    .spec_constr_clone_inlining = .all_calls,
    .consume_dead_boxes = true,
    .list_in_place_map = true,
    .prove_ranges = true,
};

var last_stmt_count: usize = 0;

fn recordStmtCount(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    last_stmt_count = store.getCFStmts().len;
}

fn lowerAndCountStmts(app_body: []const u8) harness.LowerToLirHarnessError!usize {
    last_stmt_count = 0;
    try harness.expectLirInspectionWithOptions(app_body, optimized_options, recordStmtCount);
    return last_stmt_count;
}

test "issue 12583: a Bool literal that select only stores does not inline select at every call site" {
    const literal_stmts = try lowerAndCountStmts(declarations ++ literal_main);
    const computed_stmts = try lowerAndCountStmts(declarations ++ computed_main);
    try std.testing.expect(computed_stmts > 0);
    std.testing.expect(literal_stmts <= computed_stmts) catch |err| {
        std.debug.print(
            "Bool-literal program lowered to {d} LIR statements; computed-Bool program to {d}\n",
            .{ literal_stmts, computed_stmts },
        );
        return err;
    };
}
