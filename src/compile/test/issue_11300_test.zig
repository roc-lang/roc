//! Regression test for issue #11300: forwarding a record of closures must
//! preserve method evidence when the callee orders its requirements differently.

const std = @import("std");
const postcheck = @import("postcheck");
const harness = @import("lower_to_lir_harness.zig");

test "issue 11300: a forwarded record closure that only returns Err lowers with a method call on its Ok payload" {
    try harness.expectLowersToLir(
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args|
        \\    outer({
        \\        run: || Err(CommandNotFound),
        \\        get: |_name| "x",
        \\    }).map_err(|_| Exit(1)).map_ok(|_| {})
        \\
        \\outer = |hooks| {
        \\    _ = hooks.get
        \\    middle(hooks)
        \\}
        \\
        \\middle = |hooks| {
        \\    get = hooks.get
        \\    _ = get("name")
        \\    run = hooks.run
        \\    output = run()?
        \\    parse(output.trim())
        \\}
        \\
        \\parse : Str -> Try(I32, _)
        \\parse = |_s| Ok(1)
        \\
    );
}

test "issue 11300: multiple forwarding edges preserve reordered Boolean method evidence" {
    try harness.expectLowersToLir(
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args|
        \\    outer({
        \\        run: || Err(CommandNotFound),
        \\        get: |_name| "x",
        \\    }).map_err(|_| Exit(1)).map_ok(|_| {})
        \\
        \\outer = |hooks| {
        \\    _ = hooks.get
        \\    forward(hooks)
        \\}
        \\
        \\forward = |hooks| {
        \\    _ = hooks.run
        \\    middle(hooks)
        \\}
        \\
        \\middle = |hooks| {
        \\    get = hooks.get
        \\    _ = get("name")
        \\    run = hooks.run
        \\    output = run()?
        \\    parse(output.is_empty())
        \\}
        \\
        \\parse : Bool -> Try(I32, _)
        \\parse = |_s| Ok(1)
    );
}

test "issue 11300: imported stored functions preserve symbolic and concrete method evidence" {
    var dir = std.testing.tmpDir(.{});
    defer dir.cleanup();
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : Str => Str }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": run! }
        \\run! = |input| main!(input)
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "Hooks.roc",
        .data =
        \\module [stored]
        \\
        \\stored = { call: outer }
        \\
        \\outer = |hooks| {
        \\    _ = hooks.get
        \\    middle(hooks)
        \\}
        \\
        \\middle = |hooks| {
        \\    get = hooks.get
        \\    _ = get("name")
        \\    run = hooks.run
        \\    output = run()?
        \\    parse(output.trim())
        \\}
        \\
        \\parse : Str -> Try(I32, _)
        \\parse = |_s| Ok(1)
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\import Hooks
        \\
        \\main! = |input| {
        \\    call = Hooks.stored.call
        \\    failed = call({ run: || Err(CommandNotFound), get: |_name| input })
        \\    succeeded = call({ run: || Ok(input), get: |_name| input })
        \\    match (failed, succeeded) {
        \\        (Err(CommandNotFound), Ok(1)) => input
        \\        _ => "unexpected"
        \\    }
        \\}
        ,
    });
    const path = try dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
    defer std.testing.allocator.free(path);
    try harness.expectAppPathLowersToLir(path);
}

test "issue 11300: repeated symbolic forwarding reuses specialization bodies" {
    const prefix =
        \\outer = |hooks| {
        \\    _ = hooks.get
        \\    middle(hooks)
        \\}
        \\middle = |hooks| {
        \\    get = hooks.get
        \\    _ = get("name")
        \\    run = hooks.run
        \\    output = run()?
        \\    parse(output.trim())
        \\}
        \\parse : Str -> Try(I32, _)
        \\parse = |_s| Ok(1)
        \\main! = |_args| {
        \\    hooks = { run: || Err(CommandNotFound), get: |_name| "x" }
        \\    _ = outer(hooks)
        \\
    ;
    const suffix =
        \\    Ok({})
        \\}
    ;
    var once: postcheck.Monotype.Lower.Diagnostics = .{};
    var repeated: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(prefix ++ suffix, .{
        .monotype_only = true,
        .monotype_diagnostics_out = &once,
    });
    try harness.expectLowersToLirWithOptions(prefix ++ "    _ = outer(hooks)\n" ++ suffix, .{
        .monotype_only = true,
        .monotype_diagnostics_out = &repeated,
    });
    // Open requests reserve independent graph cells before their completed
    // interfaces can share a body. Count body lowering, not provisional misses.
    try std.testing.expect(once.body.deferred_template_bodies_lowered > 0);
    try std.testing.expectEqual(once.specialization.nested_misses, repeated.specialization.nested_misses);
    try std.testing.expectEqual(once.body.deferred_template_bodies_lowered, repeated.body.deferred_template_bodies_lowered);
    try std.testing.expectEqual(once.body.caller_owned_template_bodies_lowered, repeated.body.caller_owned_template_bodies_lowered);
    try std.testing.expectEqual(@as(u64, 0), repeated.specialization.evidence_missing);
}
