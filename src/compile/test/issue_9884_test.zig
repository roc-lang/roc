//! Regression test for issue #9884.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 9884: tuple alias in annotated merge function lowers to LIR" {
    try expectLowersToLir(
        \\EawRange : (Str, Str, Str)
        \\
        \\merge_op : { merged : List(EawRange), merging : EawRange }, EawRange -> { merged : List(EawRange), merging : EawRange }
        \\merge_op = |{ merged, merging }, current_value| {
        \\    if merging.0 == current_value.0 {
        \\        {
        \\            merged,
        \\            merging: (merging.0, merging.1, current_value.2),
        \\        }
        \\    } else {
        \\        {
        \\            merged: merged.append(merging),
        \\            merging: current_value,
        \\        }
        \\    }
        \\}
        \\
        \\main! = |_| {
        \\    Ok({})
        \\}
    );
}
