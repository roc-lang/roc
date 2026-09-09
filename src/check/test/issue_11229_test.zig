//! Regression tests for https://github.com/roc-lang/roc/issues/11229.
//! Expected record-update context must not create an independent dispatch use.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

fn assertLetBoundMatchesInlined(let_bound_src: []const u8, inlined_src: []const u8) TestEnv.TestEnvError!void {
    const gpa = std.testing.allocator;
    var inlined_env = try TestEnv.init("Test", inlined_src);
    defer inlined_env.deinit();
    try inlined_env.assertNoErrors();
    const expected = try inlined_env.allocDefType(gpa, "f");
    defer gpa.free(expected);

    var let_bound_env = try TestEnv.init("Test", let_bound_src);
    defer let_bound_env.deinit();
    try let_bound_env.assertDefType("f", expected);
}

test "issue 11229 let-bound binop chain feeding a record update resolves to U32" {
    const src =
        \\trigger = |state| {
        \\    val = state.num * 10 + 5.U32
        \\    { ..state, num: val }
        \\}
        \\
        \\updated : U32
        \\updated = trigger({ num: 1.U32 }).num
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertDefType("updated", "U32");
}

test "issue 11229 let-bound binop chain feeding a record update stays open" {
    try assertLetBoundMatchesInlined(
        \\f = |state| {
        \\    val = state.num * 10 + 5.U32
        \\    { ..state, num: val }
        \\}
    ,
        \\f = |state| { ..state, num: state.num * 10 + 5.U32 }
    );
}

test "issue 11229 let-bound binop chain with pinned operands stays open" {
    try assertLetBoundMatchesInlined(
        \\f = |state| {
        \\    val = state.num * 10.U32 + 5.U32
        \\    { ..state, num: val }
        \\}
    ,
        \\f = |state| { ..state, num: state.num * 10.U32 + 5.U32 }
    );
}

test "issue 11229 updating another field does not default an arithmetic receiver" {
    try assertLetBoundMatchesInlined(
        \\f = |state| {
        \\    val = state.num * 10.U32 + 5.U32
        \\    { ..state, items: [val] }
        \\}
    ,
        \\f = |state| { ..state, items: [state.num * 10.U32 + 5.U32] }
    );
}

test "issue 11229 record update remains usable with heterogeneous user arithmetic" {
    const src =
        \\Counter := { count: U32 }.{
        \\    times : Counter, U32 -> Counter
        \\    times = |c, n| { count: c.count * n }
        \\    plus : Counter, U32 -> Counter
        \\    plus = |c, n| { count: c.count + n }
        \\}
        \\trigger = |state| {
        \\    val = state.num * 10.U32 + 5.U32
        \\    { ..state, num: val }
        \\}
        \\updated : Counter
        \\updated = trigger({ num: Counter.{ count: 1 } }).num
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertDefType("updated", "Counter");
}

test "issue 11229 borrowed nominal update field guides nested defaulted constructions" {
    const src =
        \\Item := { a: U32, b: U32 ?? 7 }
        \\State(a) := { items: List(a), num: U32 }
        \\StateAlias : State(Item)
        \\update : StateAlias -> StateAlias
        \\update = |state| {
        \\    val = state.num * 10.U32 + 5.U32
        \\    { ..state, num: val, items: [Item.{ a: 1 }, Item.{ a: 2, b: 3 }] }
        \\}
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11229 borrowed record extension guides optional nested constructions" {
    const src =
        \\Tail : { items: List({ a ?: U32 }), num: U32 }
        \\State(tail) : { label: Str, ..tail }
        \\update : State(Tail) -> State(Tail)
        \\update = |state| { ..state, items: [{ a: 1 }, {}] }
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11229 expected shape preserves shared annotated variables" {
    const src =
        \\update : { items: List({ left: a, right: a }) } -> { items: List({ left: a, right: a }) }
        \\update = |state| { ..state, items: [{ left: 1.U32, right: "bad" }] }
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertFirstTypeError("Type Mismatch");
}

test "issue 11229 expected shape does not erase real arithmetic obligations" {
    const src =
        \\trigger = |state| {
        \\    val = state.num * 10.U32 + 5.U32
        \\    { ..state, num: val }
        \\}
        \\updated = trigger({ num: "bad" })
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertFirstTypeError("Missing Method");
}
