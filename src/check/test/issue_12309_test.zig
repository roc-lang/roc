//! Regression tests for https://github.com/roc-lang/roc/issues/12309.
//!
//! A nominal or opaque type with no `encoder_for` is not encodable, whatever
//! its backing type. Encoding such a value inside a record field, a tuple, or
//! a tag payload is reported as a missing method, exactly like encoding the
//! bare value.
const TestEnv = @import("TestEnv.zig");

test "issue 12309: opaque over Str in a record field is missing encoder_for" {
    const source =
        \\Local :: Str
        \\out = Json.to_str({ token: Local.("hunter2") })
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 12309: nominal over Str in a record field is missing encoder_for" {
    const source =
        \\Local := Str
        \\out = Json.to_str({ token: Local.("hunter2") })
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 12309: opaque over U64 in a tuple is missing encoder_for" {
    const source =
        \\W :: U64
        \\out = Json.to_str((W.(5), 1.U64))
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 12309: opaque over Str in a tag payload is missing encoder_for" {
    const source =
        \\W :: Str
        \\v : [T(W)]
        \\v = T(W.("x"))
        \\out = Json.to_str(v)
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 12309: opaque over Bool in a record field is missing encoder_for" {
    const source =
        \\W :: Bool
        \\out = Json.to_str({ v: W.(Bool.True) })
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 12309: derived encoder_for over Str in a record field is accepted" {
    const source =
        \\Username := Str.{ encoder_for : _ }
        \\out = Json.to_str({ name: Username.("a") })
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12309: derived encoder_for over Bool in a record field is accepted" {
    const source =
        \\Flag :: Bool.{ encoder_for : _ }
        \\out = Json.to_str({ v: Flag.(Bool.True) })
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12309: derived encoder_for over F64 in a record field keeps the error row" {
    const source =
        \\Latitude := F64.{ encoder_for : _ }
        \\out = Json.to_str({ lat: Latitude.(51.5) })
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "issue 12309: derived encoder_for over F64 is accepted by a fallible encoder" {
    const source =
        \\Latitude := F64.{ encoder_for : _ }
        \\out = Json.to_str_try({ lat: Latitude.(51.5) })
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}
