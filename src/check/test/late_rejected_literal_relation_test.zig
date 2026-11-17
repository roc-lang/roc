//! A literal whose conversion a later relation rejects is reported once, at
//! the literal. A where-clause requirement that a scheme use copied, and that
//! the literal's value reaches through a name, belongs to the call that owns
//! the use; that call reads a value the rejection made erroneous, so the
//! requirement's failure is a cascade and is not reported. A requirement a
//! valid value reaches is still reported.

const TestEnv = @import("./TestEnv.zig");

const apply_with_where_clause =
    \\apply : p -> Str where [p.to_str : p -> Str]
    \\apply = |x| x.to_str()
    \\
    \\
;

fn expectReports(source: []const u8, expected: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertTypeErrorMsgs(expected);
}

const z_rejected_by_list_len =
    \\**Type Mismatch**
    \\This number is being used where a non-number type is needed.
    \\```roc
    \\    z = 1
    \\```
    \\        ^
    \\
    \\The type was determined to be non-numeric here:
    \\```roc
    \\    _n = List.len(z)
    \\```
    \\                  ^
    \\
    \\Other code expects this to have the type:
    \\
    \\    List(_item)
    \\
    \\
;

test "a call argument's literal rejected by a later use reports only the literal" {
    try expectReports(apply_with_where_clause ++
        \\shown = {
        \\    z = 1
        \\    s = apply(z)
        \\    _n = List.len(z)
        \\    s
        \\}
    , &.{z_rejected_by_list_len});
}

test "a loop binder's literal rejected by a later use reports only the literal" {
    try expectReports(apply_with_where_clause ++
        \\shown = {
        \\    var $r = ""
        \\    for y in [1, 2] {
        \\        $r = apply(y)
        \\        _n = List.len(y)
        \\    }
        \\    $r
        \\}
    , &.{
        \\**Type Mismatch**
        \\This number is being used where a non-number type is needed.
        \\```roc
        \\    for y in [1, 2] {
        \\```
        \\              ^
        \\
        \\The type was determined to be non-numeric here:
        \\```roc
        \\        _n = List.len(y)
        \\```
        \\                      ^
        \\
        \\Other code expects this to have the type:
        \\
        \\    List(_item)
        \\
        \\
    });
}

test "a literal rejected before the where-clause relation reports only the literal" {
    try expectReports(apply_with_where_clause ++
        \\shown = {
        \\    z = 1
        \\    _n = List.len(z)
        \\    apply(z)
        \\}
    , &.{z_rejected_by_list_len});
}

test "a method argument's literal rejected by a later use reports only the literal" {
    try expectReports(
        \\Wrap := [W].{
        \\    show : Wrap, p -> Str where [p.to_str : p -> Str]
        \\    show = |_w, x| x.to_str()
        \\}
        \\
        \\shown = {
        \\    z = 1
        \\    s = Wrap.W.show(z)
        \\    _n = List.len(z)
        \\    s
        \\}
    , &.{z_rejected_by_list_len});
}

test "a record field's literal rejected by a later use reports only the literal" {
    try expectReports(
        \\apply_r : { a : p } -> Str where [p.to_str : p -> Str]
        \\apply_r = |r| r.a.to_str()
        \\
        \\shown = {
        \\    z = 1
        \\    s = apply_r({ a: z })
        \\    _n = List.len(z)
        \\    s
        \\}
    , &.{z_rejected_by_list_len});
}

test "a list item's literal rejected by a later use reports only the literal" {
    try expectReports(
        \\apply_l : List(p) -> U64 where [p.to_str : p -> Str]
        \\apply_l = |xs| xs.len()
        \\
        \\shown = {
        \\    z = 1
        \\    s = apply_l([z])
        \\    _n = List.len(z)
        \\    s
        \\}
    , &.{z_rejected_by_list_len});
}

test "a tuple item's literal rejected by a later use reports only the literal" {
    try expectReports(
        \\apply_t : (p, Str) -> Str where [p.to_str : p -> Str]
        \\apply_t = |(a, _b)| a.to_str()
        \\
        \\shown = {
        \\    z = 1
        \\    s = apply_t((z, "x"))
        \\    _n = List.len(z)
        \\    s
        \\}
    , &.{z_rejected_by_list_len});
}

test "a tag payload's literal rejected by a later use reports only the literal" {
    try expectReports(
        \\apply_g : [G(p)] -> Str where [p.to_str : p -> Str]
        \\apply_g = |G(a)| a.to_str()
        \\
        \\shown = {
        \\    z = 1
        \\    s = apply_g(G(z))
        \\    _n = List.len(z)
        \\    s
        \\}
    , &.{z_rejected_by_list_len});
}

test "a numeral a later use gives a type without from_numeral reports only the literal" {
    try expectReports(
        \\Foo := [F].{}
        \\
        \\takes_foo : Foo -> {}
        \\takes_foo = |_f| {}
        \\
        \\
    ++ apply_with_where_clause ++
        \\shown = {
        \\    z = 1
        \\    s = apply(z)
        \\    _u = takes_foo(z)
        \\    s
        \\}
    , &.{
        \\**Type Mismatch**
        \\This number is being used where a non-number type is needed.
        \\```roc
        \\    z = 1
        \\```
        \\        ^
        \\
        \\The type was determined to be non-numeric here:
        \\```roc
        \\    _u = takes_foo(z)
        \\```
        \\                   ^
        \\
        \\Other code expects this to have the type:
        \\
        \\    Foo
        \\
        \\
    });
}

test "a string literal a later use gives a type without from_quote reports only the literal" {
    try expectReports(
        \\Foo := [F].{}
        \\
        \\takes_foo : Foo -> {}
        \\takes_foo = |_f| {}
        \\
        \\
    ++ apply_with_where_clause ++
        \\shown = {
        \\    z = "a"
        \\    s = apply(z)
        \\    _u = takes_foo(z)
        \\    s
        \\}
    , &.{
        \\**Type Mismatch**
        \\This string literal is being used where a non-string type is needed.
        \\```roc
        \\    z = "a"
        \\```
        \\        ^^^
        \\
        \\The type was determined to be:
        \\
        \\    Foo
        \\
        \\
    });
}

const missing_to_str_on_list_u8_tail =
    \\
    \\The value's type, which does not have a method named `to_str`, is:
    \\
    \\    List(U8)
    \\
    \\**Hint:** For this to work, the type would need to have a method named `to_str` associated with it in the type's declaration.
    \\
    \\This use needs that because of this code in the definition it uses:
    \\```roc
    \\apply = |x| x.to_str()
    \\```
    \\            ^^^^^^^^^^
    \\
    \\
    \\
;

test "a valid value beside the rejected literal still reports its missing method" {
    try expectReports(apply_with_where_clause ++
        \\shown = {
        \\    z = 1
        \\    w : List(U8)
        \\    w = []
        \\    s = Str.concat(apply(z), apply(w))
        \\    _n = List.len(z)
        \\    s
        \\}
    , &.{
        \\**Missing Method**
        \\A `where` clause requires the `to_str` method here, but the type being used doesn't have that method.
        \\```roc
        \\    s = Str.concat(apply(z), apply(w))
        \\```
        \\                             ^^^^^^^^
        \\
        ++ missing_to_str_on_list_u8_tail,
        z_rejected_by_list_len,
    });
}

test "a valid branch beside the rejected literal still reports its missing method" {
    try expectReports(apply_with_where_clause ++
        \\pick : Bool -> Str
        \\pick = |c| {
        \\    z = 1
        \\    w : List(U8)
        \\    w = []
        \\    apply(if (c) (z) else (w))
        \\}
    , &.{
        \\**Type Mismatch**
        \\This number is being used where a non-number type is needed.
        \\```roc
        \\    z = 1
        \\```
        \\        ^
        \\
        \\The type was determined to be non-numeric here:
        \\```roc
        \\    apply(if (c) (z) else (w))
        \\```
        \\                           ^
        \\
        \\Other code expects this to have the type:
        \\
        \\    List(U8)
        \\
        \\
        ,
        \\**Missing Method**
        \\A `where` clause requires the `to_str` method here, but the type being used doesn't have that method.
        \\```roc
        \\    apply(if (c) (z) else (w))
        \\```
        \\    ^^^^^^^^^^^^^^^^^^^^^^^^^^
        \\
        ++ missing_to_str_on_list_u8_tail,
    });
}
