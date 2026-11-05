//! Regression test for https://github.com/roc-lang/roc/issues/12718.
//!
//! At `--opt speed`, `Json.parse` into a record with a `Dict` field must
//! lower to LIR that passes ARC certification. SpecConstr's loop exit
//! selection bound the record parser's `uninitialized_payload` initializers
//! to fresh locals, so each payload slot entered its loop as a plain read of
//! a local nothing wrote and lost its presence guard; ARC retained the slot,
//! and a release build read garbage. Direct-call inlining (#12689) first
//! brought this parser into that shape.
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
