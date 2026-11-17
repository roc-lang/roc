//! A stored function holds a value for each enclosing-frame binder the
//! specialization that was evaluated reads. A binder its source body names
//! only where that specialization's types leave the code unreachable (here,
//! the `Err` arm of a `Try` whose error type has no values) has no stored
//! value, and the procedures the restored body builds capture none.
//! Repro for https://github.com/roc-lang/roc/issues/13056
const base = @import("base");
const harness = @import("lower_to_lir_harness.zig");

const strategies = [_]base.SpecializationStrategy{ .lss, .boxy };

fn expectLowers(comptime app_body: []const u8) harness.LowerToLirHarnessError!void {
    for (strategies) |strategy| {
        try harness.expectLowersToLirWithOptions(app_body, .{ .specialization_strategy = strategy });
    }
}

const preparation =
    \\Prep(state, error) : { name : Str, build : (U64 -> Try(state, error)), fail : (error -> Str) }
    \\
    \\prep : Str, (U64 -> Try(state, error)) -> Prep(state, error)
    \\prep = |name, build| { name, build, fail: |_error| name }
    \\
    \\owner : Prep({}, [])
    \\owner = prep("o", |_| Ok({}))
    \\
;

const main =
    \\
    \\main! = |args| {
    \\    echo!(stored(args.len())("x"))
    \\    Ok({})
    \\}
;

test "issue 13056: a closure built by a stored function omits a capture only an unreachable arm reads" {
    try expectLowers(preparation ++
        \\reg : Prep(state, error) -> (U64 -> (Str -> Str))
        \\reg = |p| {
        \\    name = p.name
        \\    build = p.build
        \\    fail = p.fail
        \\    |n| {
        \\        built = build(n)
        \\        |asked| match built {
        \\            Ok(_) => Str.concat(name, asked)
        \\            Err(error) => fail(error)
        \\        }
        \\    }
        \\}
        \\
        \\stored : U64 -> (Str -> Str)
        \\stored = reg(owner)
        \\
    ++ main);
}

test "issue 13056: a closure passed as an argument by a stored function omits a capture only an unreachable arm reads" {
    try expectLowers(preparation ++
        \\kept : Str, (Str -> Str) -> (Str -> Str)
        \\kept = |name, run| |asked| Str.concat(name, run(asked))
        \\
        \\reg : Prep(state, error) -> (U64 -> (Str -> Str))
        \\reg = |p| {
        \\    name = p.name
        \\    build = p.build
        \\    fail = p.fail
        \\    |n| {
        \\        built = build(n)
        \\        kept(
        \\            name,
        \\            |asked| match built {
        \\                Ok(_) => Str.concat(name, asked)
        \\                Err(error) => fail(error)
        \\            },
        \\        )
        \\    }
        \\}
        \\
        \\stored : U64 -> (Str -> Str)
        \\stored = reg(owner)
        \\
    ++ main);
}

test "issue 13056: a stored function whose only capture an unreachable arm reads stores none" {
    try expectLowers(preparation ++
        \\reg : Prep(state, error) -> (Try(state, error) -> (Str -> Str))
        \\reg = |p| {
        \\    fail = p.fail
        \\    |built| |asked| match built {
        \\        Ok(_) => asked
        \\        Err(error) => fail(error)
        \\    }
        \\}
        \\
        \\stored : Try({}, []) -> (Str -> Str)
        \\stored = reg(owner)
        \\
        \\main! = |args| {
        \\    echo!(stored(Ok({}))(args.len().to_str()))
        \\    Ok({})
        \\}
    );
}

// The error type reaches neither the stored function's type nor a capture it
// stores, so only the specialization that built the function says the `Err`
// arm is unreachable.
test "issue 13056: the specialization that built a stored function decides its restored body's unreachable arm" {
    try expectLowers(preparation ++
        \\reg : Prep(state, error) -> (U64 -> (Str -> Str))
        \\reg = |p| {
        \\    fail = p.fail
        \\    |_n| {
        \\        built : Try({}, error)
        \\        built = Ok({})
        \\        |asked| match built {
        \\            Ok(_) => asked
        \\            Err(error) => fail(error)
        \\        }
        \\    }
        \\}
        \\
        \\stored : U64 -> (Str -> Str)
        \\stored = reg(owner)
        \\
    ++ main);
}

test "issue 13056: the specialization that built a capturing stored function decides its restored body's unreachable arm" {
    try expectLowers(preparation ++
        \\reg : Prep(state, error) -> (U64 -> (Str -> Str))
        \\reg = |p| {
        \\    fail = p.fail
        \\    name = p.name
        \\    |_n| {
        \\        built : Try({}, error)
        \\        built = Ok({})
        \\        |asked| match built {
        \\            Ok(_) => Str.concat(name, asked)
        \\            Err(error) => fail(error)
        \\        }
        \\    }
        \\}
        \\
        \\stored : U64 -> (Str -> Str)
        \\stored = reg(owner)
        \\
    ++ main);
}

test "issue 13056: a local procedure of a stored function omits a capture only an unreachable arm reads" {
    try expectLowers(preparation ++
        \\reg : Prep(state, error) -> (U64 -> (Str -> Str))
        \\reg = |p| {
        \\    name = p.name
        \\    build = p.build
        \\    fail = p.fail
        \\    |n| {
        \\        answer = |built, asked| match built {
        \\            Ok(_) => Str.concat(name, asked)
        \\            Err(error) => fail(error)
        \\        }
        \\        first = answer(build(n), "a")
        \\        |asked| Str.concat(first, answer(build(n), asked))
        \\    }
        \\}
        \\
        \\stored : U64 -> (Str -> Str)
        \\stored = reg(owner)
        \\
    ++ main);
}
