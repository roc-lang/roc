//! Where-clause result rows are producers for match reachability diagnostics.
const TestEnv = @import("TestEnv.zig");

test "issue 13105: a where clause produces a tag through a generic helper" {
    var env = try TestEnv.init("Repro",
        \\Item := {}.{
        \\    read : Str -> Try(Item, [Bad, Gone])
        \\    read = |s| if (s == "") Err(Gone) else Err(Bad)
        \\}
        \\
        \\read_any : Str -> Try(a, [Bad, ..errs]) where [a.read : Str -> Try(a, [Bad, ..errs])]
        \\read_any = |s| {
        \\    Shape : a
        \\    Shape.read(s)
        \\}
        \\
        \\describe : Str -> Try(a, Str) where [a.read : Str -> Try(a, [Bad, Gone])]
        \\describe = |s|
        \\    match read_any(s) {
        \\        Ok(v) => Ok(v)
        \\        Err(Gone) => Err("gone")
        \\        Err(Bad) => Err("bad")
        \\    }
        \\
        \\expect {
        \\    r : Try(Item, Str)
        \\    r = describe("")
        \\    match r {
        \\        Err(m) => m == "gone"
        \\        Ok(_) => Bool.False
        \\    }
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13105: a tag no where clause produces is still reported through a generic helper" {
    var env = try TestEnv.init("Repro",
        \\Item := {}.{
        \\    read : Str -> Try(Item, [Bad])
        \\    read = |_| Err(Bad)
        \\}
        \\
        \\read_any : Str -> Try(a, [Bad, ..errs]) where [a.read : Str -> Try(a, [Bad, ..errs])]
        \\read_any = |s| {
        \\    Shape : a
        \\    Shape.read(s)
        \\}
        \\
        \\describe : Str -> Try(Item, Str)
        \\describe = |s| {
        \\    r : Try(Item, _)
        \\    r = read_any(s)
        \\    match r {
        \\        Ok(v) => Ok(v)
        \\        Err(Gone) => Err("gone")
        \\        Err(Bad) => Err("bad")
        \\    }
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeWarning("Unmatchable Pattern");
}
