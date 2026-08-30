//! Regression test for issue #10935.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 10935: while-loop var read by a record update after two reassigning arms lowers to LIR" {
    // Repro for https://github.com/roc-lang/roc/issues/10935.
    //
    // The `while` loop's var is the join parameter of the loop header. Two
    // match arms reassign it and jump back to the header; the default arm
    // reads it to build a record update. ARC certification must accept that
    // read, because the reassigning arms are the only statements that release
    // the loop var and neither of them reaches the default arm.
    try expectLowersToLir(
        \\State : { bytes : List(U8) }
        \\
        \\skip_blank : State -> State
        \\skip_blank = |state| {
        \\    var $bytes = state.bytes
        \\    while True {
        \\        match $bytes {
        \\            [' ', .. as rest] => {
        \\                $bytes = rest
        \\            }
        \\            ['#', .. as rest] => {
        \\                $bytes = rest
        \\            }
        \\            _ => return { ..state, bytes: $bytes }
        \\        }
        \\    }
        \\    state
        \\}
        \\
        \\main! = |_| {
        \\    result = skip_blank({ bytes: ['#', 'a'] })
        \\    echo!(result.bytes.len().to_str())
        \\    Ok({})
        \\}
    );
}
