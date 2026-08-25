//! Regression test for loop exit selection carrying local binder state.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "selected loop exit carrying substituted local binder state lowers to LIR" {
    try expectLowersToLir(
        \\walk_closure = |s, v| {
        \\    v_num = match U64.from_str(v) { Ok(n) => n, _ => 0 }
        \\    match s {
        \\        (Unknown(Undefined), _) => Continue((Unknown(Value(v_num)), 0))
        \\        (Unknown(Value(prev)), _) =>
        \\            if v_num > prev {
        \\                Continue((Ascending(v_num), 0))
        \\            } else {
        \\                Break((Unknown(Undefined), 1))
        \\            }
        \\        (Ascending(_), _) => Break((Unknown(Undefined), 1))
        \\    }
        \\}
        \\
        \\main! = |_| {
        \\    (s, _) = List.fold_until(Str.split_on("", ""), (Unknown(Undefined), 0), walk_closure)
        \\    match s {
        \\        Unknown(_) => Ok({})
        \\        Ascending(_) => Ok({})
        \\    }
        \\}
    );
}
