//! Regression tests for issue 10804.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/10804
//
// A type whose associated block requests a derived codec is encodable only when
// every component of its backing shape is encodable too. `Data` declares no
// `encoder_for`, so serializing a `Chart` must be reported as a missing method
// on `Data`. Accepting it hands Monotype a shape whose field has no encoder
// target, which its method registry cannot lower.

test "issue 10804: derived encoder_for on an alias rejects a field type without encoder_for" {
    const source =
        \\Data := [Url(Str)]
        \\Chart :: { data : Data }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Chart.{ data: Data.Url("foo.json") })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

test "issue 10804: derived encoder_for on a nominal rejects a field type without encoder_for" {
    const source =
        \\Data := [Url(Str)]
        \\Chart := { data : Data }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Chart.{ data: Data.Url("foo.json") })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

test "issue 10804: derived parser_for rejects a field type without parser_for" {
    const source =
        \\Data := [Url(Str)]
        \\Chart :: { data : Data }.{
        \\  parser_for : _
        \\}
        \\parse_chart : Str -> Try(Chart, _)
        \\parse_chart = |s| Json.parse(s)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

test "issue 10804: derived encoder_for rejects a tag payload type without encoder_for" {
    const source =
        \\Data := [Url(Str)]
        \\Chart := [Chart(Data)].{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Chart.Chart(Data.Url("foo.json")))
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

test "issue 10804: derived encoder_for reaches a field type nested two shapes deep" {
    const source =
        \\Data := [Url(Str)]
        \\Inner :: { data : Data }
        \\Chart :: { inner : Inner }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Chart.{ inner: { data: Data.Url("foo.json") } })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

// A derived codec's backing shape is instantiated fresh at every use, so a
// recursive type reaches its own declaration through vars the walk has never
// seen. Validation has to recognize the declaration itself, or these two
// modules never finish checking.

test "issue 10804: derived codecs terminate on a recursive type" {
    const source =
        \\Tree := [Leaf(Str), Node(List(Tree))].{
        \\  encoder_for : _
        \\  parser_for : _
        \\}
        \\out = Json.to_str(Tree.Node([Tree.Leaf("a")]))
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 10804: derived codecs terminate on mutually recursive types" {
    const source =
        \\A := { b : List(B) }.{
        \\  encoder_for : _
        \\}
        \\B := { a : List(A), name : Str }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(A.{ b: [B.{ a: [], name: "x" }] })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

// Two applications of one declaration are separate obligations: cutting the
// walk at an open declaration must not let the second one skip validation.
test "issue 10804: a second application of the same declaration is still validated" {
    const source =
        \\Data := [Url(Str)]
        \\Wrap(a) :: { x : a }.{
        \\  encoder_for : _
        \\}
        \\Chart :: { p : Wrap(Str), q : Wrap(Data) }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Chart.{ p: { x: "s" }, q: { x: Data.Url("u") } })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

// The walk stops at an application it has already accounted for, so it has to
// tell two applications of one declaration apart even when one is inside the
// other.
test "issue 10804: derived encoder_for reaches through a nested application of the same declaration" {
    const source =
        \\Data := [Url(Str)]
        \\Wrap(a) :: { x : a }.{
        \\  encoder_for : _
        \\}
        \\Chart :: { p : Wrap(Wrap(Data)) }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Chart.{ p: { x: { x: Data.Url("u") } } })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

// A declaration that applies itself at an argument built from its own formal
// has no last level, so its codec would need a different shape at each one.
// That is a property of the declaration, and the walk has to report it rather
// than follow the applications forever.
test "issue 10804: derived encoder_for rejects a declaration that grows its own formal" {
    const source =
        \\Nest(a) := [Done, More(Nest(List(a)))].{
        \\  encoder_for : _
        \\}
        \\to_json : Nest(Str) -> Str
        \\to_json = |n| Json.to_str(n)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

// A formal passed straight through leaves the application the same size, so
// applying a declaration to a sibling's larger argument is not growth.
test "issue 10804: two applications of one declaration may differ in size" {
    const source =
        \\Wrap(a) :: { x : a }.{
        \\  encoder_for : _
        \\}
        \\Chart :: { p : Wrap(Str), q : Wrap(List(Str)) }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Chart.{ p: { x: "s" }, q: { x: ["t"] } })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

// The recursive occurrence's argument is a fixed type rather than one built
// from the formal, so the applications reach a last level even though that
// argument happens to be larger than the one the outer level was given.
test "issue 10804: a self-application at a fixed larger argument still checks" {
    const source =
        \\SelfE(a) := { x : a, y : Try(SelfE({ p ?: Str }), [Null]) }.{
        \\  encoder_for : _
        \\}
        \\to_json : SelfE(Str) -> Str
        \\to_json = |n| Json.to_str(n)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

// The recursive occurrence here carries a concrete argument, which
// instantiation mints fresh at every level, so recognizing it takes comparing
// the arguments' shapes rather than the vars carrying them.
test "issue 10804: a declaration that applies itself at a concrete argument checks once" {
    const source =
        \\SelfC(a) := { x : a, y : Try(SelfC(Str), [Null]) }.{
        \\  encoder_for : _
        \\}
        \\to_json : SelfC(Str) -> Str
        \\to_json = |n| Json.to_str(n)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

// Nesting a declaration inside itself gives two applications with different
// arguments, and both have to be walked: cutting on the declaration alone
// would skip the inner one's components.
test "issue 10804: a declaration nested inside itself checks both applications" {
    const source =
        \\Data := [Url(Str)].{
        \\  encoder_for : _
        \\}
        \\Pair(a, b) :: { first : a, second : b }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Pair.{ first: "x", second: Pair.{ first: "y", second: Data.Url("u") } })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

// A derived codec over a formal has nothing to say about that formal where the
// type is still polymorphic; the obligation belongs to each concrete use, and
// rejecting it here would reject working generic code.
test "issue 10804: a derived encoder_for over a formal checks where it is polymorphic" {
    const source =
        \\Wrap(a) :: { x : a }.{
        \\  encoder_for : _
        \\}
        \\to_json = |w| Json.to_str(w)
        \\out = to_json(Wrap.{ x: "s" })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 10804: an annotated polymorphic derived encoder_for checks" {
    const source =
        \\Wrap(a) :: { x : a }.{
        \\  encoder_for : _
        \\}
        \\to_json : Wrap(a) -> Str
        \\to_json = |w| Json.to_str(w)
        \\out = to_json(Wrap.{ x: "s" })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 10804: a polymorphic derived parser_for checks at a concrete use" {
    const source =
        \\Wrap(a) :: { x : a }.{
        \\  parser_for : _
        \\  encoder_for : _
        \\}
        \\parse_it : Str -> Try(Wrap(Str), _)
        \\parse_it = |s| Json.parse(s)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

// A nominal's backing walks as a shape, so the optional fields inside it are
// judged by the record it belongs to rather than by where that nominal sits.
test "issue 10804: an optional field inside a nested derived codec checks" {
    const source =
        \\Inner :: { a : Str, b ?: Str }.{
        \\  parser_for : _
        \\  encoder_for : _
        \\}
        \\Outer :: { inner : Inner }.{
        \\  parser_for : _
        \\  encoder_for : _
        \\}
        \\parse_it : Str -> Try(Outer, _)
        \\parse_it = |s| Json.parse(s)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

// The marker that says "this codec is the compiler's own" lives in the module
// that declared the type, so the walk reads it out of another module's env
// whenever the shape crosses a boundary.
test "issue 10804: an imported type's derived encoder_for still validates its components" {
    const source_a =
        \\Data := [Url(Str)]
        \\Chart :: { data : Data }.{
        \\  encoder_for : _
        \\}
        \\make : Str -> Chart
        \\make = |url| { data: Data.Url(url) }
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertNoErrors();

    const source_b =
        \\import A
        \\
        \\out = Json.to_str(A.make("foo.json"))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();

    try test_env_b.assertOneTypeError("Missing Method");
}

// Control: the same shape, except the field's type declares its own derived
// `encoder_for`. Component validation must accept this one.
test "issue 10804: derived encoder_for accepts a field type that declares encoder_for" {
    const source =
        \\Data := [Url(Str)].{
        \\  encoder_for : _
        \\}
        \\Chart :: { data : Data }.{
        \\  encoder_for : _
        \\}
        \\out = Json.to_str(Chart.{ data: Data.Url("foo.json") })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}
