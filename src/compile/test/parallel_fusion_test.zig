//! Real-worker determinism for constructor and iterator continuation fusion.

const harness = @import("lower_to_lir_harness.zig");

test "LIR pass workers deterministically fuse branch-return Try consumers" {
    // Leave ordinary calls for the solved inline plan, so this exercises
    // late LIR fusion rather than SpecConstr's earlier value propagation.
    try harness.expectLirPassParallelismDeterministicLir(.{ .app_body =
        \\choose : U64 -> Try(U64, [Unavailable])
        \\choose = |n| if n == 0 Err(Unavailable) else Ok(n)
        \\
        \\consume : U64 -> U64
        \\consume = |n| {
        \\    value = match choose(n) {
        \\        Ok(value) => value + 1
        \\        Err(Unavailable) => 0
        \\    }
        \\    value * 2 + n
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    count = List.len(args)
        \\    echo!(Str.inspect(consume(count) + consume(count + 1)))
        \\    Ok({})
        \\}
    }, .{ .inline_mode = .wrappers, .spec_constr_clone_inlining = .iterator_fusion, .proc_debug_names = true, .dump_proc_identities = true }, &.{.tag_fusion});
}

test "LIR pass workers deterministically forward generated iterator joins in none mode" {
    // A numeric iterator keeps the forwarded continuations ownership-neutral;
    // the runtime bound prevents specialization from erasing the loop.
    try harness.expectLirPassParallelismDeterministicLir(.{ .app_body =
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    total = 0.U64.until(List.len(args)).map(|n| n * 2).fold(0, |sum, n| sum + n)
        \\    echo!(Str.inspect(total))
        \\    Ok({})
        \\}
    }, .{ .inline_mode = .none, .proc_debug_names = true, .dump_proc_identities = true }, &.{.forwarding_join});
}
