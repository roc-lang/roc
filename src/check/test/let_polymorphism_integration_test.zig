//! Integration tests for let-polymorphism that parse, canonicalize, and type-check
//! actual code to ensure polymorphic values work correctly in practice.

const TestEnv = @import("./TestEnv.zig");

test "direct polymorphic identity usage" {
    const source =
        \\{
        \\    id = |x| x
        \\    a = id(1)
        \\    b = id("x")
        \\    { a, b }
        \\}
    ;
    // The field 'a' has the same type as the dispatcher for from_numeral, so they should share the same name
    // Note: 'c' is used because 'a' and 'b' are already identifiers in the code
    try typeCheck(
        source,
        "{ a: Dec, b: Str }",
    );
}

test "higher-order function with polymorphic identity" {
    const source =
        \\{
        \\    id = |x| x
        \\    f = |g, v| g(v)
        \\    a = f(id, 1)
        \\    b = f(id, "x")
        \\    { a, b }
        \\}
    ;
    try typeCheck(
        source,
        "{ a: Dec, b: Str }",
    );
}

test "let-polymorphism with function composition" {
    const source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add_one = |x| x + 1
        \\    compose(double, add_one)
        \\}
    ;
    try typeCheck(
        source,
        "Dec -> Dec",
    );
}

test "polymorphic empty list" {
    const source =
        \\{
        \\    empty = []
        \\    nums = [1, 2, 3]
        \\    strs = ["a", "b", "c"]
        \\    { empty, nums, strs }
        \\}
    ;
    try typeCheck(
        source,
        "{ empty: List(_a), nums: List(Dec), strs: List(Str) }",
    );
}

test "polymorphic cons function" {
    const source =
        \\{
        \\    cons = |x, xs| List.concat([x], xs)
        \\    list1 = cons(1, [2, 3])
        \\    list2 = cons("a", ["b", "c"])
        \\    { list1, list2 }
        \\}
    ;
    try typeCheck(
        source,
        "{ list1: List(Dec), list2: List(Str) }",
    );
}

test "polymorphic record constructor" {
    const source =
        \\{
        \\    make_pair = |x, y| { first: x, second: y }
        \\    pair1 = make_pair(1, "a")
        \\    pair2 = make_pair("b", 42)
        \\    pair3 = make_pair(True, False)
        \\    { pair1, pair2, pair3 }
        \\}
    ;
    try typeCheck(
        source,
        "{ pair1: { first: Dec, second: Str }, pair2: { first: Str, second: Dec }, pair3: { first: [True], second: [False] } }",
    );
}

test "polymorphic identity with various numeric types" {
    const source =
        \\{
        \\    id = |x| x
        \\    int_val = id(42)
        \\    float_val = id(3.14)
        \\    bool_val = id(True)
        \\    { int_val, float_val, bool_val }
        \\}
    ;
    try typeCheck(
        source,
        "{ bool_val: [True], float_val: Dec, int_val: Dec }",
    );
}

test "nested polymorphic data structures" {
    const source =
        \\{
        \\    make_box = |x| { value: x }
        \\    box1 = make_box(42)
        \\    box2 = make_box("hello")
        \\    nested = make_box(make_box(100))
        \\    { box1, box2, nested }
        \\}
    ;
    try typeCheck(
        source,
        "{ box1: { value: Dec }, box2: { value: Str }, nested: { value: { value: Dec } } }",
    );
}

test "polymorphic function in let binding" {
    const source =
        \\{
        \\    result = {
        \\        id = |x| x
        \\        a = id(1)
        \\        b = id("test")
        \\        { a, b }
        \\    }
        \\    result
        \\}
    ;
    try typeCheck(
        source,
        "{ a: Dec, b: Str }",
    );
}

test "polymorphic swap function" {
    const source =
        \\{
        \\    swap = |pair| { first: pair.second, second: pair.first }
        \\    pair1 = { first: 1, second: "a" }
        \\    pair2 = { first: True, second: 42 }
        \\    swapped1 = swap(pair1)
        \\    swapped2 = swap(pair2)
        \\    { swapped1, swapped2 }
        \\}
    ;
    try typeCheck(
        source,
        "{ swapped1: { first: Str, second: Dec }, swapped2: { first: Dec, second: [True] } }",
    );
}

test "polymorphic option type simulation" {
    const source =
        \\{
        \\    none = { tag: "None" }
        \\    some = |x| { tag: "Some", value: x }
        \\    opt1 = some(42)
        \\    opt2 = some("hello")
        \\    opt3 = none
        \\    { opt1, opt2, opt3 }
        \\}
    ;
    try typeCheck(
        source,
        "{ opt1: { tag: Str, value: Dec }, opt2: { tag: Str, value: Str }, opt3: { tag: Str } }",
    );
}

test "polymorphic const function" {
    const source =
        \\{
        \\    const = |x| |_| x
        \\    always5 = const(5)
        \\    alwaysHello = const("hello")
        \\    num = always5(99)
        \\    str = alwaysHello(True)
        \\    { num, str }
        \\}
    ;
    try typeCheck(
        source,
        "{ num: Dec, str: Str }",
    );
}

test "polymorphic pipe function" {
    const source =
        \\{
        \\    pipe = |x, f| f(x)
        \\    double = |n| n * 2
        \\    length = |_s| 5
        \\    num_result = pipe(21, double)
        \\    str_result = pipe("hello", length)
        \\    { num_result, str_result }
        \\}
    ;
    try typeCheck(
        source,
        "{ num_result: Dec, str_result: Dec }",
    );
}

test "polymorphic identity with if-else (eval regression)" {
    // Exact source from the failing eval test "polymorphic identity function"
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;
    try typeCheck(
        source,
        "Str",
    );
}

test "polymorphic identity with if-else - all intermediate types" {
    // Same as above but returning a record so we can assert on every intermediate type
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    result = if (num > 0) str else ""
        \\    { num, str, result }
        \\}
    ;
    try typeCheck(
        source,
        "{ num: Dec, result: Str, str: Str }",
    );
}

test "a top-level value whose type keeps an unpinned part can be inspected" {
    // `empty = []` is a weak binding whose item type nothing pins and which
    // carries no method requirement. Passing it to `Str.inspect` is accepted
    // in every spelling: directly, inside interpolation, through a wrapper
    // function, and through a field.
    const source =
        \\my_inspect = |v| Str.inspect(v)
        \\
        \\empty = []
        \\ok_empty = Ok([])
        \\pair = ([], Ok([]))
        \\rec = { items: [] }
        \\first = List.first([])
        \\dict = Dict.empty()
        \\set = Set.empty()
        \\
        \\shown = Str.inspect(empty)
        \\
        \\expect Str.inspect(empty) == "[]"
        \\expect "${Str.inspect(empty)}" == "[]"
        \\expect my_inspect(empty) == "[]"
        \\expect Str.inspect(rec.items) == "[]"
        \\expect Str.inspect(ok_empty) == "Ok([])"
        \\expect Str.inspect(pair) == "([], Ok([]))"
        \\expect Str.inspect(rec) == "{ items: [] }"
        \\expect Str.inspect(first) == "Err(ListWasEmpty)"
        \\expect Str.inspect(dict) == "Dict.from_list([])"
        \\expect Str.inspect(set) == "Set.from_list([])"
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("empty", "List(_a)");
    try test_env.assertDefType("shown", "Str");
}

test "a nominal top-level value with an unpinned type argument can be inspected" {
    const source =
        \\Wrap(a) := [W(List(a))].{
        \\    to_inspect : Wrap(a) -> Str
        \\    to_inspect = |Wrap.(W(items))| "Wrap<${Str.inspect(items)}>"
        \\}
        \\
        \\Phantom(a) := [P(U64)]
        \\
        \\wrapped = Wrap.W([])
        \\phantom = Phantom.P(1)
        \\
        \\expect Str.inspect(wrapped) == "Wrap<[]>"
        \\expect Str.inspect(phantom) == "P(1)"
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "comparing a polymorphic constant requires the method of the comparison's own instance" {
    // `empty`'s item type carries no requirement, so `empty` quantifies it
    // (design.md "Polymorphic Constants"): the comparison's `is_eq` is a
    // requirement of its own instance, which nothing else fixes and so is
    // decided, not of `empty`.
    const source =
        \\empty = []
        \\
        \\expect empty == []
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "inspecting and comparing a polymorphic constant are checked at their own instances" {
    const source =
        \\empty = []
        \\
        \\expect Str.inspect(empty) == "[]"
        \\expect empty == []
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
fn typeCheck(comptime source_expr: []const u8, expected_type: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.initExpr("Test", source_expr);
    defer test_env.deinit();
    return test_env.assertLastDefType(expected_type);
}
