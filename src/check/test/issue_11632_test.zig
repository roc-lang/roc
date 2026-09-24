//! Regression tests for https://github.com/roc-lang/roc/issues/11632.
//! Codec derivation must let all use sites constrain inferred tag rows.

const TestEnv = @import("./TestEnv.zig");

test "issue 11632: derived parser permits later match tags" {
    const source =
        \\expect {
        \\    w = Json.parse("\"B\"")
        \\    match w {
        \\        Ok(A(s)) => s == ""
        \\        Ok(B) => True
        \\        Err(_) => False
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 11632: derived parser permits later match tags in reverse order" {
    const source =
        \\expect {
        \\    w = Json.parse("{\"A\":\"\"}")
        \\    match w {
        \\        Ok(B) => False
        \\        Ok(A(s)) => s == ""
        \\        Err(_) => False
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 11632: derived encoder permits later match tags" {
    const source =
        \\expect {
        \\    v = if 1 == 1 A else B
        \\    s = Json.to_str(v)
        \\    extra = match v {
        \\        A => 1
        \\        B => 2
        \\        C => 3
        \\    }
        \\    s == "\"A\"" and extra == 1
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 11632: derived encoder rejects a rigid open tag row" {
    const source =
        \\encode : [A, B, ..tags] -> Str
        \\encode = |value| Json.to_str(value)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

test "issue 11632: nested inferred tag rows remain open through later uses" {
    const source =
        \\expect {
        \\    value = { tags: [A] }
        \\    encoded = Json.to_str(value)
        \\    all_a = List.all(value.tags, |tag| match tag {
        \\        A => True
        \\        B => False
        \\    })
        \\    encoded == "{\"tags\":[\"A\"]}" and all_a
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 11632: fresh caller openness is not definition annotation authority" {
    const source =
        \\make : {} -> [A, B]
        \\make = |_| A
        \\expect {
        \\    value = make({})
        \\    encoded = Json.to_str(value)
        \\    match value {
        \\        A => encoded == "\"A\""
        \\        B => False
        \\        C => False
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}
