//! Effect propagation tests for direct calls and delayed static dispatch.

const TestEnv = @import("./TestEnv.zig");

fn expectNoErrors(comptime source: []const u8) !void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

fn expectOneTypeError(comptime source: []const u8, comptime title: []const u8) !void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError(title);
}

fn expectFirstTypeError(comptime source: []const u8, comptime title: []const u8) !void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError(title);
}

test "effect propagation - direct top-level effectful call reports top-level effect" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\tick! : {} => U64
        \\tick! = |_| 1
        \\
        \\top = tick!({})
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - static-dispatch top-level effectful call reports top-level effect" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\Eff := [Eff].{
        \\    tick! : Eff => U64
        \\    tick! = |_| 1
        \\}
        \\
        \\top = (Eff.Eff).tick!()
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - pure function annotation rejects direct effectful call" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\tick! : {} => U64
        \\tick! = |_| 1
        \\
        \\direct : {} -> U64
        \\direct = |x| tick!(x)
    , "TYPE MISMATCH");
}

test "effect propagation - pure function annotation rejects effectful method call" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\Eff := [Eff].{
        \\    tick! : Eff => U64
        \\    tick! = |_| 1
        \\}
        \\
        \\direct : Eff -> U64
        \\direct = |x| x.tick!()
    , "TYPE MISMATCH");
}

test "effect propagation - effectful function annotation accepts effectful method call" {
    try expectNoErrors(
        \\package [] {}
        \\
        \\Eff := [Eff].{
        \\    tick! : Eff => U64
        \\    tick! = |_| 1
        \\}
        \\
        \\direct : Eff => U64
        \\direct = |x| x.tick!()
    );
}

test "effect propagation - pure where clause rejects effectful implementation method" {
    try expectFirstTypeError(
        \\package [] {}
        \\
        \\uses_tick : a -> U64 where [a.tick! : a -> U64]
        \\uses_tick = |x| x.tick!()
        \\
        \\Eff := [Eff].{
        \\    tick! : Eff => U64
        \\    tick! = |_| 1
        \\}
        \\
        \\value = uses_tick(Eff.Eff)
    , "TYPE MISMATCH");
}

test "effect propagation - effectful where clause makes caller effectful" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\uses_tick : a => U64 where [a.tick! : a => U64]
        \\uses_tick = |x| x.tick!()
        \\
        \\Eff := [Eff].{
        \\    tick! : Eff => U64
        \\    tick! = |_| 1
        \\}
        \\
        \\top = uses_tick(Eff.Eff)
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - expect rejects direct effectful call" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\tick! : {} => U64
        \\tick! = |_| 1
        \\
        \\expect tick!({}) == 1
    , "EFFECTFUL EXPECT");
}

test "effect propagation - expect rejects delayed effectful dispatch" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\Eff := [Eff].{
        \\    tick! : Eff => U64
        \\    tick! = |_| 1
        \\}
        \\
        \\expect (Eff.Eff).tick!() == 1
    , "EFFECTFUL EXPECT");
}

test "effect propagation - effectful binop dispatch reports top-level effect" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\Num := [Num].{
        \\    plus : Num, Num => Num
        \\    plus = |_, _| Num
        \\}
        \\
        \\top = Num.Num + Num.Num
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - effectful unary dispatch reports top-level effect" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\Num := [Num].{
        \\    negate : Num => Num
        \\    negate = |_| Num
        \\}
        \\
        \\top = -Num.Num
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - imported effectful nominal method reports top-level effect" {
    var imported = try TestEnv.init("A",
        \\A := [A].{
        \\    tick! : A => U64
        \\    tick! = |_| 1
        \\}
    );
    defer imported.deinit();
    try imported.assertNoErrors();
    try imported.assertDefType("A.tick!", "A => U64");

    var test_env = try TestEnv.initWithImport("Test",
        \\import A
        \\
        \\value = A.A
        \\
        \\top = value.tick!()
    , "A", &imported);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - effectful type method reports top-level effect" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\Thing := [Thing].{
        \\    make! : {} => Thing
        \\    make! = |_| Thing
        \\}
        \\
        \\top = Thing.make!({})
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - effectful interpolation dispatch reports top-level effect" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\Text := [Text].{
        \\    from_interpolation : Str, Iter((U64, Str)) => Text
        \\    from_interpolation = |_, _| Text
        \\}
        \\
        \\top : Text
        \\top = "score ${1.U64}"
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - effectful iterator dispatch reports top-level effect" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\Bag := [Bag].{
        \\    iter : Bag => Iter(U64)
        \\    iter = |_| Iter.single(1.U64)
        \\}
        \\
        \\top = {
        \\    var total = 0.U64
        \\    for item in Bag.Bag {
        \\        total = total + item
        \\    }
        \\    total
        \\}
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - local function alias preserves effectfulness" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\tick! : {} => U64
        \\tick! = |_| 1
        \\
        \\alias = tick!
        \\
        \\top = alias({})
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - imported function alias preserves effectfulness" {
    var imported = try TestEnv.init("A",
        \\tick! : {} => U64
        \\tick! = |_| 1
    );
    defer imported.deinit();
    try imported.assertNoErrors();

    var test_env = try TestEnv.initWithImport("Test",
        \\import A
        \\
        \\alias = A.tick!
        \\
        \\top = alias({})
    , "A", &imported);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - higher-order pure function parameter stays pure" {
    try expectNoErrors(
        \\package [] {}
        \\
        \\call : ({} -> U64) -> U64
        \\call = |fn| fn({})
        \\
        \\top = call(|_| 1)
    );
}

test "effect propagation - higher-order effectful function parameter is effectful" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\tick! : {} => U64
        \\tick! = |_| 1
        \\
        \\call : ({} => U64) => U64
        \\call = |fn| fn({})
        \\
        \\top = call(tick!)
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - closure creation with effectful body is not effectful" {
    try expectNoErrors(
        \\package [] {}
        \\
        \\tick! : {} => U64
        \\tick! = |_| 1
        \\
        \\closure = || tick!({})
    );
}

test "effect propagation - closure call with effectful body is effectful" {
    try expectOneTypeError(
        \\package [] {}
        \\
        \\tick! : {} => U64
        \\tick! = |_| 1
        \\
        \\closure = || tick!({})
        \\
        \\top = closure()
    , "EFFECTFUL TOP-LEVEL VALUE");
}

test "effect propagation - dbg expect and crash are not effectful calls" {
    try expectNoErrors(
        \\package [] {}
        \\
        \\with_dbg : U64 -> {}
        \\with_dbg = |n| dbg n
        \\
        \\with_expect : U64 -> {}
        \\with_expect = |n| {
        \\    expect n == n
        \\}
        \\
        \\with_crash : U64 -> U64
        \\with_crash = |n| if n == 0 { crash "zero" } else { n }
    );
}
