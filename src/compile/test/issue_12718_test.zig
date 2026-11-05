//! Regression test for https://github.com/roc-lang/roc/issues/12718.
//!
//! At `--opt speed`, `Json.parse` into a record with a `Dict` field must
//! lower to LIR that passes ARC certification. SpecConstr's direct-call
//! inlining (#12689) left the dictionary loop's entry reading a value whose
//! binding it had not kept, which a release build read as garbage.
const harness = @import("lower_to_lir_harness.zig");

const app =
    \\main! = |args| {
    \\    json = match args.first() {
    \\        Ok(text) => text
    \\        Err(_) => "{}"
    \\    }
    \\    parsed : Try({ a : Dict(Str, Str) }, _)
    \\    parsed = Json.parse(json)
    \\    match parsed {
    \\        Ok(r) => echo!(r.a.len().to_str())
    \\        Err(_) => echo!("err")
    \\    }
    \\    Ok({})
    \\}
;

test "issue 12718: Json.parse into a record with a Dict field lowers at --opt speed" {
    try harness.expectLowersToLirWithOptions(app, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
    });
}
