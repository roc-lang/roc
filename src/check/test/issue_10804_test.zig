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
