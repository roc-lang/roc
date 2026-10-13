//! Regression tests for https://github.com/roc-lang/roc/issues/12229.
//!
//! A derived codec whose boundary shape holds a structural tag union with a
//! list, record or tag union payload below a second record level must lower
//! through Monotype: each generated-call role the derived body asks for must
//! be found from the checked codec contract, including the format's
//! `parse_list_start` call for the tag payload.

const harness = @import("lower_to_lir_harness.zig");

test "issue 12229: derived parser for a nested record field holding a tag union with a list payload lowers to Monotype" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    parsed : Try({ v : { w : [A(List(Str))] } }, _)
        \\    parsed = Json.parse(Str.join_with(args, ""))
        \\    match parsed {
        \\        Ok(_) => echo!("ok")
        \\        Err(_) => echo!("err")
        \\    }
        \\    Ok({})
        \\}
    , .{ .monotype_only = true });
}

test "issue 12229: derived parser for an optional record field holding a tag union with a record payload lowers to Monotype" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    parsed : Try({ w : Try([A({ n : Str })], [Missing]) }, _)
        \\    parsed = Json.parse(Str.join_with(args, ""))
        \\    match parsed {
        \\        Ok(_) => echo!("ok")
        \\        Err(_) => echo!("err")
        \\    }
        \\    Ok({})
        \\}
    , .{ .monotype_only = true });
}

test "issue 12229: derived parser for list items holding a tag union with a tag union payload lowers to Monotype" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    parsed : Try(List({ steps : [A([B(Str)])] }), _)
        \\    parsed = Json.parse(Str.join_with(args, ""))
        \\    match parsed {
        \\        Ok(_) => echo!("ok")
        \\        Err(_) => echo!("err")
        \\    }
        \\    Ok({})
        \\}
    , .{ .monotype_only = true });
}

test "issue 12229: derived encoder for a nested record field holding a tag union with a list payload lowers to Monotype" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    value : { v : { w : [A(List(Str))] } }
        \\    value = { v: { w: A(args) } }
        \\    echo!(Json.to_str(value))
        \\    Ok({})
        \\}
    , .{ .monotype_only = true });
}
