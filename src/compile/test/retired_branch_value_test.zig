//! A branch value that an earlier branch's mismatch leaves unrelated to the
//! result is erroneous alone: when it reads a binding, the binding's class
//! keeps its type for every other use of it.

const harness = @import("lower_to_lir_harness.zig");

test "a retired match branch reading a nominal payload binder leaves other matches on that payload intact" {
    try harness.expectLowersToLirWithOptions(
        \\T := [A5, A7(Str), A8({ r0 : Bool, r1 : U64 })].{}
        \\
        \\v12 = match T.A8({ r0: True, r1: 1 }) {
        \\    A8(v15) => v15.r0
        \\    _ => False
        \\}
        \\
        \\v16 = match T.A7("t") {
        \\    A5 => { r0: (True, 97), r1: 16 }
        \\    A7(_) => { r0: False, r1: 1 }
        \\    A8(v22) => v22
        \\}
        \\
        \\main! = |_args| {
        \\    _ = v12
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}
