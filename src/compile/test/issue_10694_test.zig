//! Regression test for issue #10694.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 10694: open dispatch result stays graph-backed until final sealing" {
    try expectLowersToLir(
        \\limit : U64
        \\limit = 16
        \\
        \\read : Str -> Try(I64, [BadInput])
        \\read = |s| if Str.is_empty(s) { Err(BadInput) } else { Ok(16) }
        \\
        \\decode : Str -> Try(I64, _)
        \\decode = |body| {
        \\    n = read(body) ? |_e| BadRead
        \\    limit_i64 = limit.to_i64_try() ?? 0
        \\    if n != limit_i64 { Err(Mismatch) } else { Ok(n) }
        \\}
        \\
        \\main! = |args| {
        \\    body = match args {
        \\        [first, ..] => first
        \\        [] => "x"
        \\    }
        \\    match decode(body) {
        \\        Ok(n) => echo!(n.to_str())
        \\        Err(_) => echo!("err")
        \\    }
        \\    Ok({})
        \\}
    );
}
