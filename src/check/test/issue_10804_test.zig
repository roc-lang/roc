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
