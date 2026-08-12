//! Tests for cross-module type checking functionality.

const std = @import("std");
const Check = @import("../Check.zig");
const TestEnv = @import("./TestEnv.zig");

const testing = std.testing;

test "cross-module - check type - monomorphic function passes" {
    const source_a =
        \\main! : Str -> Str
        \\main! = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    // Str is auto-imported from Builtin module, so it prints as "Str"
    try test_env_a.assertLastDefType("Str -> Str");

    const source_b =
        \\import A
        \\
        \\main : Str
        \\main = A.main!("hello")
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    // Str is auto-imported from Builtin module, so it prints as "Str"
    try test_env_b.assertLastDefType("Str");
}

test "cross-module - check type - monomorphic function fails" {
    const source_a =
        \\main! : Str -> Str
        \\main! = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("Str -> Str");

    const source_b =
        \\import A
        \\
        \\main : U8
        \\main = A.main!(1)
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertOneTypeError("Type Mismatch");
}

test "cross-module - check type - polymorphic function passes" {
    const source_a =
        \\main! : a -> a
        \\main! = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("a -> a");

    const source_b =
        \\import A
        \\
        \\main : Str
        \\main = A.main!("hello")
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    // Str is auto-imported from Builtin module, so it prints as "Str"
    try test_env_b.assertLastDefType("Str");
}

test "cross-module - check type - polymorphic function with multiple uses passes" {
    const source_a =
        \\main! : a -> a
        \\main! = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("a -> a");

    const source_b =
        \\import A
        \\
        \\main : U64
        \\main = {
        \\  a =  A.main!(10)
        \\  b =  A.main!(15)
        \\  _c =  A.main!("Hello")
        \\  a + b
        \\}
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("U64");
}

test "cross-module - settled weak-receiver scheme remains polymorphic" {
    const source_a =
        \\top_str = "a,b,c"
        \\main! = |g| top_str.split_on(",").map(g)
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertDefType("main!", "(Str -> b) -> List(b)");

    // Module publication happens after A's weak receiver has settled. Its
    // discharged root scheme must retain the same independent quantified
    // result variable even though checker-local pending requirements are gone.
    const source_b =
        \\import A
        \\lengths = A.main!(|s| s.count_utf8_bytes())
        \\selves = A.main!(|s| s)
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("lengths", "List(U64)");
    try test_env_b.assertDefType("selves", "List(Str)");
}

test "cross-module - nested capturing closure publishes its dispatch relation" {
    const source_a =
        \\main! : a -> a where [a.plus : a, Dec -> a]
        \\main! = |x| {
        \\  add_x = |y| x + y
        \\  add_x(10)
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertDefType("main!", "a -> a where [a.plus : a, Dec -> a]");

    const source_b =
        \\import A
        \\answer = A.main!(42)
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("answer", "Dec");

    const source_c =
        \\import A
        \\bad = A.main!("not a number")
    ;
    var test_env_c = try TestEnv.initWithImport("C", source_c, "A", &test_env_a);
    defer test_env_c.deinit();
    try test_env_c.assertOneTypeError("Missing Method");
}

test "cross-module - optional record - one optional value shared by two exports unifies" {
    // `other = orig` shares `orig`'s type; both exports carry the same
    // structural `optional` field kind, so their imported copies unify
    // (design.md "Field Kinds (All-Dynamic Optional Fields)").
    const source_a =
        \\orig! : { world ?: U8 }
        \\orig! = { world: 5 }
        \\
        \\other! = orig!
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("{ world ?: U8 }");

    const source_b =
        \\import A
        \\
        \\lst = [A.orig!, A.other!]
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("List({ world ?: U8 })");
}

test "cross-module - optional record - two same-shape optional annotations are one type" {
    // A field kind is structural, not a hidden per-definition witness: two
    // `?:` annotations of the same shape are the same type, so separately
    // annotated values mix freely (design.md "Field Kinds (All-Dynamic
    // Optional Fields)").
    const source_a =
        \\orig! : { world ?: U8 }
        \\orig! = { world: 5 }
        \\
        \\separate! : { world ?: U8 }
        \\separate! = { world: 5 }
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\lst = [A.orig!, A.separate!]
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("List({ world ?: U8 })");
}

test "cross-module - optional record - imported optional value still requires optional access" {
    // The `optional` kind crosses the module boundary: a direct `.world`
    // read of the imported value is rejected, `.?world` binds.
    const source_a =
        \\orig! : { world ?: U8 }
        \\orig! = { world: 5 }
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_direct =
        \\import A
        \\
        \\bad = A.orig!.world
    ;
    var test_env_direct = try TestEnv.initWithImport("B", source_direct, "A", &test_env_a);
    defer test_env_direct.deinit();
    try test_env_direct.assertOneTypeError("Type Mismatch");

    const source_optional =
        \\import A
        \\
        \\good = A.orig!.?world
    ;
    var test_env_optional = try TestEnv.initWithImport("C", source_optional, "A", &test_env_a);
    defer test_env_optional.deinit();
    try test_env_optional.assertLastDefType("Try(U8, [MissingField])");
}

test "cross-module - optional record - imported type declaration keeps the optional kind" {
    // The `optional` kind is pinned by the DECLARATION's annotation and
    // crosses the module boundary with the type: B constructs at A's alias
    // omitting the optional field (opt-in width absorption), and reading it
    // demands `.?` (design.md "Field Kinds (All-Dynamic Optional Fields)").
    const source_a =
        \\Thing : { world ?: U8 }
        \\
        \\mk! : Thing
        \\mk! = {}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("Thing");

    const source_b =
        \\import A
        \\
        \\use = A.mk!.?world
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("Try(U8, [MissingField])");
}

test "cross-module - defaulted record - imported alias default crosses the boundary (review M3)" {
    // The DefaultId's origin half is env-local and must rebase across the
    // store boundary (design.md "Defaulted Fields"): B constructs at A's
    // alias, omitting the defaulted field.
    const source_a =
        \\Cfg : { count : U8 ?? 10, name : Str }
        \\
        \\get_count! : Cfg -> U8
        \\get_count! = |c| c.count
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("Cfg -> U8");

    const source_b =
        \\import A
        \\
        \\use_it = A.get_count!({ name: "b" })
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("U8");
}

test "cross-module - defaulted record - textually identical local default does not merge (review M3)" {
    // One written default is one identity: B declaring its own `?? 10` is a
    // DIFFERENT default than A's even though the text matches, so the two
    // record types mismatch (design.md "Defaulted Fields").
    const source_a =
        \\Cfg : { count : U8 ?? 10 }
        \\
        \\mk! : Cfg
        \\mk! = {}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\local : { count : U8 ?? 10 }
        \\local = {}
        \\
        \\lst = [A.mk!, local]
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertOneTypeError("Type Mismatch");
}

test "cross-module - check type - static dispatch" {
    const source_a =
        \\A := [A(Str)].{
        \\  to_str : A -> Str
        \\  to_str = |A.A(val)| val
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertDefType("A.to_str", "A -> Str");

    const source_b =
        \\import A
        \\
        \\a_val = A.A("hello")
        \\
        \\main = a_val.to_str()
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("a_val", "A");
    try test_env_b.assertDefType("main", "Str");
}

test "cross-module - check type - static dispatch - no annotation & indirection" {
    const source_a =
        \\A := [A(Str)].{
        \\  to_str = |A.A(val)| val
        \\  to_str2 = |x| x.to_str()
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertDefType("A.to_str", "A -> Str");
    try test_env_a.assertDefType("A.to_str2", "a -> b where [a.to_str : a -> b]");

    const source_b =
        \\import A
        \\
        \\val1 = A.A("hello")
        \\val2 = A.A("world")
        \\
        \\main = (val1.to_str(), val1.to_str2(), val2.to_str2())
        \\
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("val1", "A");
    try test_env_b.assertDefType("val2", "A");
    try test_env_b.assertDefType("main", "(Str, Str, Str)");
}

test "cross-module - check type - method call on imported associated constant" {
    const source_html =
        \\Html := { foo : U8 }.{
        \\  href : Html, Str -> Html
        \\  href = |html, _val| html
        \\
        \\  a : Html
        \\  a = { foo: 4 }
        \\}
    ;
    var test_env_html = try TestEnv.init("Html", source_html);
    defer test_env_html.deinit();

    const source_main =
        \\import Html
        \\
        \\main : Html
        \\main = Html.a.href("./other-page.html")
    ;
    var test_env_main = try TestEnv.initWithImport("Main", source_main, "Html", &test_env_html);
    defer test_env_main.deinit();
    try test_env_main.assertDefType("main", "Html");
}

test "cross-module - check type - opaque types 1" {
    const source_a =
        \\A :: [A(Str)].{}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\a_val : A.A 
        \\a_val = A("hello")
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertFirstTypeError("Type Mismatch");
}

test "cross-module - check type - opaque types 2" {
    const source_a =
        \\A :: [A(Str)].{}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\a_val = A.A("hello")
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertFirstTypeError("Cannot Use Opaque Nominal Type");
}

test "displayNameIsBetter - shorter names are preferred" {
    // Tests the core comparison logic used when multiple imports provide different
    // display names for the same type (e.g., `import Foo as F` and `import Foo as Foo`).
    // The shortest name wins for error message display. For equal lengths, the
    // lexicographically smaller name wins (deterministic regardless of import order).
    const displayNameIsBetter = Check.displayNameIsBetter;

    // Shorter is better
    try testing.expect(displayNameIsBetter("T", "Type"));
    try testing.expect(displayNameIsBetter("AB", "ABC"));
    try testing.expect(!displayNameIsBetter("Type", "T"));
    try testing.expect(!displayNameIsBetter("ABC", "AB"));

    // Equal length: lexicographically smaller wins
    try testing.expect(displayNameIsBetter("Abc", "Bbc")); // 'A' < 'B'
    try testing.expect(displayNameIsBetter("Aac", "Abc")); // 'a' < 'b' at position 1
    try testing.expect(!displayNameIsBetter("Bbc", "Abc"));
    try testing.expect(!displayNameIsBetter("Abc", "Aac"));

    // Identical strings: no replacement
    try testing.expect(!displayNameIsBetter("Same", "Same"));
    try testing.expect(!displayNameIsBetter("", ""));
}

test "cross-module - check type - nested module access" {
    // Test access to nested modules across files (issue #9074)
    // When a module Outer contains a nested module Inner with a value inner,
    // accessing Outer.Inner.inner from another file that imports Outer should work.
    const source_outer =
        \\Outer := [].{
        \\    outer : I64
        \\    outer = 20
        \\    Inner := [].{
        \\        inner : I64
        \\        inner = 10
        \\    }
        \\}
    ;
    var test_env_outer = try TestEnv.init("Outer", source_outer);
    defer test_env_outer.deinit();
    try test_env_outer.assertDefType("Outer.outer", "I64");
    try test_env_outer.assertDefType("Outer.Inner.inner", "I64");

    const source_main =
        \\import Outer
        \\
        \\test1 : I64
        \\test1 = Outer.outer
        \\test2 : I64
        \\test2 = Outer.Inner.inner
        \\main : I64
        \\main = test1 + test2
    ;
    var test_env_main = try TestEnv.initWithImport("Main", source_main, "Outer", &test_env_outer);
    defer test_env_main.deinit();
    try test_env_main.assertDefType("test1", "I64");
    try test_env_main.assertDefType("test2", "I64");
    try test_env_main.assertDefType("main", "I64");
}

test "cross-module - ambiguous instantiation of an imported where-clause scheme reports in the instantiating module" {
    // Module A exposes a legitimate polymorphic scheme in the `Iter.collect`
    // shape: the constrained var appears in return position (any caller can
    // pin it by using the result) and the body dispatches the contract, so
    // the constraint is body-forced. A itself is clean.
    const source_a =
        \\A := [A].{
        \\    make : List(U64) -> output where [output.from_list : List(U64) -> output]
        \\    make = |xs| {
        \\        Output : output
        \\        Output.from_list(xs)
        \\    }
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertNoErrors();

    // Module B instantiates the scheme and throws the result away, so nothing
    // can ever pin the copied receiver: the body-forced contract is a genuine
    // dead end (the issue 9815/9819 shape, across a module boundary).
    // Constraint provenance is module-scoped (cleared on import), so the
    // diagnostic must land on B's own instantiating expression.
    const source_b =
        \\import A
        \\
        \\use_it = || {
        \\    _ = A.make([1, 2, 3])
        \\    {}
        \\}
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();

    try test_env_b.assertOneTypeError("Missing Method");
}
