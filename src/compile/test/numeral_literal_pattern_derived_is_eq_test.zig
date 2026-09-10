//! Literal-pattern equality must consume checked structural or callable evidence.
const harness = @import("lower_to_lir_harness.zig");

test "numeral literal pattern on a nominal record with a generated is_eq lowers to LIR" {
    try harness.expectLowersToLir(
        \\Px := { n : U32 }.{
        \\    is_eq : _
        \\    from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| match U32.from_numeral(numeral) {
        \\        Ok(n) => Ok({ n: n })
        \\        Err(err) => Err(err)
        \\    }
        \\}
        \\
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    p : Px
        \\    p = 380
        \\    match p {
        \\        380 => Ok({})
        \\        _ => Err(Exit(1))
        \\    }
        \\}
    );
}

test "literal pattern checked equality lowers through boxy" {
    try harness.expectLowersToLirWithOptions(
        \\Px := { n : U32 }.{
        \\    is_eq : _
        \\    from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| match U32.from_numeral(numeral) {
        \\        Ok(n) => Ok({ n: n })
        \\        Err(err) => Err(err)
        \\    }
        \\}
        \\
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    p : Px
        \\    p = 380
        \\    match p {
        \\        380 => Ok({})
        \\        _ => Err(Exit(1))
        \\    }
        \\}
    , .{ .specialization_strategy = .boxy });
}

test "literal pattern checked equality: generic evidence lowers under both strategies" {
    const source =
        \\Px := { n : U32 }.{
        \\    is_eq : _
        \\    from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| match U32.from_numeral(numeral) {
        \\        Ok(n) => Ok({ n: n })
        \\        Err(err) => Err(err)
        \\    }
        \\}
        \\matches : a -> Bool where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool]
        \\matches = |p| match p {
        \\    380 => True
        \\    _ => False
        \\}
        \\main! = |args| {
        \\    _ = args
        \\    _ = (matches(380.Px), matches(381.Px), matches(380.U32), matches(381.U32))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "literal pattern checked equality: derived tag union lowers under both strategies" {
    const source =
        \\Px := [Val(U32)].{
        \\    is_eq : _
        \\    from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| match U32.from_numeral(numeral) {
        \\        Ok(n) => Ok(Val(n))
        \\        Err(err) => Err(err)
        \\    }
        \\}
        \\matches : a -> Bool where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool]
        \\matches = |p| match p {
        \\    380 => True
        \\    _ => False
        \\}
        \\main! = |args| {
        \\    _ = args
        \\    _ = (matches(380.Px), matches(381.Px))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "literal pattern checked equality: nested heap values and guard fallthrough lowers under both strategies" {
    const source =
        \\Px := { n : List(U32) }.{
        \\    is_eq : _
        \\    from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| match U32.from_numeral(numeral) {
        \\        Ok(n) => Ok({ n: [n] })
        \\        Err(err) => Err(err)
        \\    }
        \\}
        \\matches : List(Px), Bool -> U8
        \\matches = |ps, flag| match ps {
        \\    [380, ..] if flag => 1
        \\    [380, ..] => 2
        \\    _ => 3
        \\}
        \\main! = |args| {
        \\    _ = args
        \\    _ = (matches([380.Px], True), matches([380.Px], False), matches([381.Px], True), matches([], True))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "literal pattern checked equality: derived quote lowers under both strategies" {
    const source =
        \\Word := { text : Str }.{
        \\    is_eq : _
        \\    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
        \\    from_quote = |text| Ok({ text: text })
        \\}
        \\matches : Word -> Bool
        \\matches = |p| match p {
        \\    "hello" => True
        \\    _ => False
        \\}
        \\main! = |args| {
        \\    _ = args
        \\    _ = (matches("hello".Word), matches("world".Word))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "literal pattern checked equality: recursive nominal lowers under both strategies" {
    const source =
        \\Px := [End, Next(Px)].{
        \\    is_eq : _
        \\    from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
        \\    from_numeral = |_numeral| Ok(Next(End))
        \\}
        \\matches : Px -> Bool
        \\matches = |p| match p {
        \\    0 => True
        \\    _ => False
        \\}
        \\main! = |args| {
        \\    _ = args
        \\    _ = (matches(Px.Next(Px.End)), matches(Px.End))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "literal pattern checked equality: multiple guards short circuit lowers under both strategies" {
    const source =
        \\Px := { n: U32 }.{
        \\    from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| match U32.from_numeral(numeral) {
        \\        Ok(n) => Ok({ n: n })
        \\        Err(err) => Err(err)
        \\    }
        \\    is_eq : Px, Px -> Bool
        \\    is_eq = |a, b| if a.n == 999 { crash "later equality must short circuit" } else { a.n == b.n }
        \\}
        \\matches : Px, Px, Bool -> U8
        \\matches = |a, b, flag| match (a, b) {
        \\    (380, 381) if flag => 1
        \\    _ => 2
        \\}
        \\main! = |args| {
        \\    _ = args
        \\    _ = (matches(380.Px, 381.Px, True), matches(380.Px, 381.Px, False), matches(380.Px, 382.Px, True), matches(379.Px, 999.Px, True))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}
