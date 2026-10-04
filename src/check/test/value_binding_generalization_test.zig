//! Whether a value binding generalizes is decided by its right-hand side
//! alone, never by its annotation (design.md "Value Bindings Generalize By
//! Expression"). An annotation that introduces a type variable on a binding
//! that does not generalize is rejected, and the binding is checked as an
//! unannotated one.
const TestEnv = @import("TestEnv.zig");

const rejected_title = "Value Is Not Polymorphic";

fn expectRejected(comptime source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError(rejected_title);
}

fn expectAccepted(comptime source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

// Rejected: the annotation claims a polymorphism the binding does not have.

test "value binding generalization: an annotated empty list is not polymorphic" {
    try expectRejected(
        \\empty : List(a)
        \\empty = []
    );
}

test "value binding generalization: an annotated eta-reduced call result is not polymorphic" {
    try expectRejected(
        \\mk = |{}| |x| x
        \\
        \\id : a -> a
        \\id = mk({})
    );
}

test "value binding generalization: an annotated conditional between lambdas is not polymorphic" {
    try expectRejected(
        \\pick : Bool -> Str
        \\pick = |c| {
        \\    f : a -> a
        \\    f = if c |x| x else |x| x
        \\    f("x")
        \\}
    );
}

test "value binding generalization: an annotated tuple containing a type variable is not polymorphic" {
    try expectRejected(
        \\pair : (List(a), U8)
        \\pair = ([], 1)
    );
}

test "value binding generalization: an annotated record containing a type variable is not polymorphic" {
    try expectRejected(
        \\rec : { items : List(a), count : U8 }
        \\rec = { items: [], count: 1 }
    );
}

test "value binding generalization: a local annotation introducing a fresh type variable is not polymorphic" {
    try expectRejected(
        \\count : U8 -> U64
        \\count = |_n| {
        \\    xs : List(b)
        \\    xs = []
        \\    List.len(xs)
        \\}
    );
}

test "value binding generalization: an annotated where clause on a value is not polymorphic" {
    try expectRejected(
        \\items : List(a) where [a.to_str : a -> Str]
        \\items = []
    );
}

test "value binding generalization: an explicit open tag union on a value is rejected" {
    try expectRejected(
        \\color : [Red, Green, ..]
        \\color = Red
    );
}

test "value binding generalization: an annotated number literal is not polymorphic" {
    try expectRejected(
        \\n : a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
        \\n = 5
    );
}

test "value binding generalization: the report quotes the annotation and suggests a thunk" {
    var test_env = try TestEnv.init("Test",
        \\empty : List(a)
        \\empty = []
    );
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Value Is Not Polymorphic**
        \\The type annotation on `empty` says it can be used at many types, but `empty` is not a function, so it can only have one type.
        \\```roc
        \\empty : List(a)
        \\```
        \\^^^^^^^^^^^^^^^
        \\
        \\
        \\If you want me to infer its type, write `_` in place of each type variable, or write a concrete type.
        \\
        \\If you want to use it at many types, make it a function that takes `{}`:
        \\    empty : {} -> List(a)
        \\    empty = |{}| []
        \\Then call it as `empty({})` wherever you use it.
        \\
        \\
    );
}

test "value binding generalization: a rejected binding keeps the type its right-hand side infers" {
    var test_env = try TestEnv.init("Test",
        \\empty : List(a)
        \\empty = []
        \\
        \\bytes = List.append(empty, 1.U8)
    );
    defer test_env.deinit();
    try test_env.assertTypeErrorTitles(&.{rejected_title});
    try test_env.assertDefTypeOptions("empty", "List(U8)", .{ .allow_type_errors = true });
}

test "value binding generalization: a rejected binding used at two types also reports the mismatch" {
    var test_env = try TestEnv.init("Test",
        \\empty : List(a)
        \\empty = []
        \\
        \\both = (List.append(empty, 1.U8), List.append(empty, "s"))
    );
    defer test_env.deinit();
    try test_env.assertTypeErrorTitles(&.{ rejected_title, "Type Mismatch" });
}

test "value binding generalization: an unannotated value used at two types is a mismatch" {
    var test_env = try TestEnv.init("Test",
        \\empty = []
        \\
        \\both = (List.append(empty, 1.U8), List.append(empty, "s"))
    );
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

// Accepted: the binding generalizes by its right-hand side, or the annotation
// introduces no type variable.

test "value binding generalization: an annotated lambda is used at two types" {
    try expectAccepted(
        \\id : a -> a
        \\id = |x| x
        \\
        \\both = (id("x"), id(1.U8))
    );
}

test "value binding generalization: an annotated value alias of a function is used at two types" {
    try expectAccepted(
        \\id : a -> a
        \\id = |x| x
        \\
        \\same : a -> a
        \\same = id
        \\
        \\both = (same("x"), same(1.U8))
    );
}

test "value binding generalization: a concrete annotation on a value is accepted" {
    try expectAccepted(
        \\empty : List(U8)
        \\empty = []
    );
}

test "value binding generalization: an inference hole on a value is accepted" {
    try expectAccepted(
        \\empty : List(_)
        \\empty = []
        \\
        \\bytes = List.append(empty, 1.U8)
    );
}

test "value binding generalization: an implicitly open tag union on a value is accepted" {
    try expectAccepted(
        \\color : [Red, Green]
        \\color = Red
    );
}

test "value binding generalization: a local annotation naming an enclosing type variable is accepted" {
    try expectAccepted(
        \\wrap : a -> List(a)
        \\wrap = |x| {
        \\    empty : List(a)
        \\    empty = []
        \\    List.append(empty, x)
        \\}
        \\
        \\both = (wrap("x"), wrap(1.U8))
    );
}

test "value binding generalization: a thunk is used at two types" {
    try expectAccepted(
        \\empty : {} -> List(a)
        \\empty = |{}| []
        \\
        \\both = (List.append(empty({}), 1.U8), List.append(empty({}), "s"))
    );
}

test "value binding generalization: the issue 12016 program checks" {
    try expectAccepted(
        \\format : Bool -> Str
        \\format = |trim_input| {
        \\    config = { format: if trim_input |s| s.trim() else |s| s }
        \\    (config.format)("  hi  ")
        \\}
    );
}

test "value binding generalization: the report names no type the annotation did not write" {
    var test_env = try TestEnv.init("Test",
        \\n : a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
        \\n = 5
    );
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Value Is Not Polymorphic**
        \\The type annotation on `n` says it can be used at many types, but `n` is not a function, so it can only have one type.
        \\```roc
        \\n : a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
        \\```
        \\^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\
        \\If you want me to infer its type, write `_` in place of each type variable, or write a concrete type.
        \\
        \\If you want to use it at many types, make it a function that takes `{}`:
        \\    n : {} -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
        \\    n = |{}| 5
        \\Then call it as `n({})` wherever you use it.
        \\
        \\
    );
}
