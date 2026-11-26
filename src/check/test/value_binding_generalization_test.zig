//! Whether a value binding generalizes is decided by its right-hand side and
//! its position, never by its annotation (design.md "Value Bindings
//! Generalize By Expression", "Polymorphic Constants"). A top-level constant
//! generalizes over every type variable that carries no requirement; a local
//! value binding does not generalize. An annotation that introduces a type
//! variable a binding cannot quantify is rejected, and the binding is checked
//! as an unannotated one.
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

// Accepted: a top-level constant quantifies every variable its annotation
// introduces that carries no requirement.

test "value binding generalization: an annotated empty list is polymorphic" {
    try expectAccepted(
        \\empty : List(a)
        \\empty = []
        \\
        \\both = (List.append(empty, 1.U8), List.append(empty, "s"))
    );
}

test "value binding generalization: an annotated eta-reduced call result is polymorphic" {
    try expectAccepted(
        \\mk = |{}| |x| x
        \\
        \\id : a -> a
        \\id = mk({})
        \\
        \\both = (id(1.U8), id("s"))
    );
}

test "value binding generalization: an annotated tuple containing a type variable is polymorphic" {
    try expectAccepted(
        \\pair : (List(a), U8)
        \\pair = ([], 1)
        \\
        \\both = (List.append(pair.0, 1.U8), List.append(pair.0, "s"))
    );
}

test "value binding generalization: an annotated record containing a type variable is polymorphic" {
    try expectAccepted(
        \\rec : { items : List(a), count : U8 }
        \\rec = { items: [], count: 1 }
        \\
        \\both = (List.append(rec.items, 1.U8), List.append(rec.items, "s"))
    );
}

test "value binding generalization: an unannotated constant used at two types is polymorphic" {
    try expectAccepted(
        \\empty = []
        \\
        \\both = (List.append(empty, 1.U8), List.append(empty, "s"))
    );
}

test "value binding generalization: a constant's unannotated tag row is used at two unions" {
    try expectAccepted(
        \\color = Purple
        \\
        \\pastel : [Purple, Mauve] -> Str
        \\pastel = |_| "pastel"
        \\
        \\vibrant : [Purple, Orange] -> Str
        \\vibrant = |_| "vibrant"
        \\
        \\both = (pastel(color), vibrant(color))
    );
}

test "value binding generalization: a constant whose variable carries a requirement stays weak" {
    // `5`'s type carries a literal-conversion requirement, so `five` is weak
    // and its two uses at different number types are a mismatch.
    var test_env = try TestEnv.init("Test",
        \\five = 5
        \\
        \\both = (five + 1.U8, five + 1.U16)
    );
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "value binding generalization: a constant generalizes its unconstrained variables and keeps its constrained ones weak" {
    // The list's item type carries no requirement and generalizes; the
    // number's type carries one and is one weak type shared by every use.
    try expectAccepted(
        \\pair = ([], 5)
        \\
        \\both = (List.append(pair.0, 1.U8), List.append(pair.0, "s"), pair.1 + 1.U16)
    );
}

test "value binding generalization: a placeholder constant is polymorphic" {
    try expectAccepted(
        \\parse : Str -> Try(a, [Bad])
        \\parse = ...
        \\
        \\combine : a, b -> (a, b)
        \\combine = crash "todo"
    );
}

// Rejected: the annotation claims a polymorphism the binding does not have.

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

test "value binding generalization: an explicit open tag union on a local value is rejected" {
    try expectRejected(
        \\name : U8 -> Str
        \\name = |_n| {
        \\    color : [Red, Green, ..]
        \\    color = Red
        \\    match color {
        \\        Red => "red"
        \\        _ => "other"
        \\    }
        \\}
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
        \\count : U8 -> U64
        \\count = |_n| {
        \\    empty : List(a)
        \\    empty = []
        \\    List.len(empty)
        \\}
    );
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Value Is Not Polymorphic**
        \\The type annotation on `empty` says it can be used at many types, but `empty` isn't defined as a function (like `|x| ...`), so it can only have one type.
        \\```roc
        \\    empty : List(a)
        \\```
        \\    ^^^^^^^^^^^^^^^
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

fn expectThunkSuggestion(comptime source: []const u8, comptime suggestion: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeProblemMsgContains(rejected_title, suggestion);
}

test "value binding generalization: the suggested thunk drops an output-position `..`" {
    try expectThunkSuggestion(
        \\f : U8 -> U8
        \\f = |n| {
        \\    color : [Red, Green, ..]
        \\    color = Red
        \\    n
        \\}
    ,
        \\    color : {} -> [Red, Green]
        \\    color = |{}| Red
    );
}

test "value binding generalization: the suggested thunk drops a `..` in a covariant type argument" {
    try expectThunkSuggestion(
        \\f : U8 -> U8
        \\f = |n| {
        \\    colors : List([Red, Green, ..])
        \\    colors = [Red]
        \\    n
        \\}
    ,
        \\    colors : {} -> List([Red, Green])
    );
}

test "value binding generalization: the suggested thunk drops every output-position `..`" {
    try expectThunkSuggestion(
        \\f : U8 -> U8
        \\f = |n| {
        \\    pair : ([A([X, ..]), B, ..], { c : [C, ..] })
        \\    pair = (A(X), { c: C })
        \\    n
        \\}
    ,
        \\    pair : {} -> ([A([X]), B], { c : [C] })
    );
}

test "value binding generalization: the suggested thunk keeps an input-position `..`" {
    try expectThunkSuggestion(
        \\mk = |{}| |_| Done
        \\
        \\f : U8 -> U8
        \\f = |n| {
        \\    show : [A, ..] -> [Done, ..]
        \\    show = mk({})
        \\    n
        \\}
    ,
        \\    show : {} -> ([A, ..] -> [Done])
    );
}

test "value binding generalization: the suggested thunk keeps a `..` in a contravariant type argument" {
    try expectThunkSuggestion(
        \\Handler(e) : e -> Str
        \\
        \\mk = |{}| |_| "x"
        \\
        \\f : U8 -> U8
        \\f = |n| {
        \\    handle : Handler([A, ..])
        \\    handle = mk({})
        \\    n
        \\}
    ,
        \\    handle : {} -> Handler([A, ..])
    );
}

test "value binding generalization: a rejected list used at two types reports one error" {
    try expectRejected(
        \\empty : List(a) where [a.to_str : a -> Str]
        \\empty = []
        \\
        \\both = (List.append(empty, 1.U8), List.append(empty, "s"))
    );
}

test "value binding generalization: a rejected function value called at two types reports one error" {
    try expectRejected(
        \\mk = |{}| |x| x
        \\
        \\id : a -> a where [a.to_str : a -> Str]
        \\id = mk({})
        \\
        \\both = (id(1.U8), id("s"))
        \\
        \\shown = |{}| {
        \\    _n = id(2.U8)
        \\    s = id("t")
        \\    "${s}"
        \\}
    );
}

test "value binding generalization: a rejected local binding used at two types reports one error" {
    try expectRejected(
        \\both : U8 -> (U64, U64)
        \\both = |_n| {
        \\    xs : List(b)
        \\    xs = []
        \\    (List.len(List.append(xs, 1.U8)), List.len(List.append(xs, "s")))
        \\}
    );
}

test "value binding generalization: a rejected top-level binding used from two functions reports one error" {
    try expectRejected(
        \\empty : List(a) where [a.to_str : a -> Str]
        \\empty = []
        \\
        \\bytes = |{}| List.append(empty, 1.U8)
        \\
        \\strs = |{}| List.append(empty, "s")
        \\
        \\count = |{}| List.len(empty).to_str()
    );
}

test "value binding generalization: a rejected binding used in a conditional branch reports one error" {
    try expectRejected(
        \\empty : List(a) where [a.to_str : a -> Str]
        \\empty = []
        \\
        \\pick : Bool -> U64
        \\pick = |c| if c List.len(List.append(empty, 1.U8)) else 0
    );
}

test "value binding generalization: an unannotated local value used at two types is a mismatch" {
    var test_env = try TestEnv.init("Test",
        \\both = |_n| {
        \\    empty = []
        \\    (List.append(empty, 1.U8), List.append(empty, "s"))
        \\}
    );
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 13298: a polymorphic constant function is returned at an annotation's rigid variable" {
    // `k`'s variable carries no requirement, so `k` is polymorphic: `f`
    // returns `k` at its own `b`, and `g` uses it at `U64`.
    try expectAccepted(
        \\k = if Bool.True |x| x else |x| x
        \\f : {} -> (b -> b)
        \\f = |_| k
        \\g : U64 -> U64
        \\g = |n| k(n)
    );
}

test "issue 13298: returning a weak local function cannot bind it to the annotation's rigid variable" {
    var env = try TestEnv.init("Test",
        \\f : U64 -> (b -> b)
        \\f = |n| {
        \\    k = if n > 0 |x| x else |x| x
        \\    _ = k(n)
        \\    k
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "issue 13298: a polymorphic constant record returns its callback at an annotation rigid" {
    try expectAccepted(
        \\k = if Bool.True { next: |x| x } else { next: |x| x }
        \\f : {} -> { next: b -> b }
        \\f = |_| k
        \\g : U64 -> U64
        \\g = |n| {
        \\    next = k.next
        \\    next(n)
        \\}
    );
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
        \\The type annotation on `n` says it can be used at many types, but `n` isn't defined as a function (like `|x| ...`), so it can only have one type.
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

// Stubs: a placeholder body for an annotated function type is not a function
// either; the report suggests writing it inside a lambda of the annotation's
// arity.

test "value binding generalization: an ellipsis stub for a function annotation suggests a lambda stub" {
    var test_env = try TestEnv.init("Test",
        \\run : U8 -> U8
        \\run = |n| {
        \\    parse : Str -> Try(a, [Bad])
        \\    parse = ...
        \\    n
        \\}
    );
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Value Is Not Polymorphic**
        \\The type annotation on `parse` says it can be used at many types, but `parse` isn't defined as a function (like `|x| ...`), so it can only have one type.
        \\```roc
        \\    parse : Str -> Try(a, [Bad])
        \\```
        \\    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\
        \\If this function is not implemented yet, write its placeholder body inside a function:
        \\    parse = |_| ...
        \\
        \\
        \\
    );
}

test "value binding generalization: a crash stub keeps its message and matches a multi-argument arity" {
    var test_env = try TestEnv.init("Test",
        \\run : U8 -> U8
        \\run = |n| {
        \\    combine : a, b -> (a, b)
        \\    combine = crash "todo"
        \\    n
        \\}
    );
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Value Is Not Polymorphic**
        \\The type annotation on `combine` says it can be used at many types, but `combine` isn't defined as a function (like `|x| ...`), so it can only have one type.
        \\```roc
        \\    combine : a, b -> (a, b)
        \\```
        \\    ^^^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\
        \\If this function is not implemented yet, write its placeholder body inside a function:
        \\    combine = |_, _| crash "todo"
        \\
        \\
        \\
    );
}

test "value binding generalization: a stub for a non-function annotation gets the ordinary advice" {
    var test_env = try TestEnv.init("Test",
        \\run : U8 -> U8
        \\run = |n| {
        \\    none : List(a)
        \\    none = ...
        \\    n
        \\}
    );
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Value Is Not Polymorphic**
        \\The type annotation on `none` says it can be used at many types, but `none` isn't defined as a function (like `|x| ...`), so it can only have one type.
        \\```roc
        \\    none : List(a)
        \\```
        \\    ^^^^^^^^^^^^^^
        \\
        \\
        \\If you want me to infer its type, write `_` in place of each type variable, or write a concrete type.
        \\
        \\If you want to use it at many types, make it a function that takes `{}`:
        \\    none : {} -> List(a)
        \\    none = |{}| ...
        \\Then call it as `none({})` wherever you use it.
        \\
        \\
    );
}

test "value binding generalization: a lambda stub generalizes" {
    try expectAccepted(
        \\parse : Str -> Try(a, [Bad])
        \\parse = |_| ...
    );
}

// Polymorphic recursion: a constant read inside a generic recursion is
// instantiated over the recursion's own variables, so an accepted recursion
// reaches finitely many instances of it, and a recursion at a growing type is
// rejected whatever constants it reads (design.md "Polymorphic Constants").

test "value binding generalization: a polymorphic constant read inside an accepted recursion" {
    try expectAccepted(
        \\empty : List(_a)
        \\empty = []
        \\
        \\count : U64, a -> U64
        \\count = |n, x| if n == 0 List.len(List.append(empty, x)) else count(n - 1, x)
        \\
        \\both = (count(2, 1.U8), count(3, "s"))
    );
}

test "value binding generalization: a recursion at a growing type that reads a polymorphic constant is rejected" {
    var test_env = try TestEnv.init("Test",
        \\empty : List(_a)
        \\empty = []
        \\
        \\depth : U64, a -> U64
        \\depth = |n, x| if n == 0 List.len(List.append(empty, x)) else 1 + depth(n - 1, [x])
        \\
        \\used = depth(2, 1.U8)
    );
    defer test_env.deinit();
    try test_env.assertOneTypeError("Polymorphic Recursion");
}
