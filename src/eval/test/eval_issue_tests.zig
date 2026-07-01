//! Focused regression repros for GitHub issues.

const TestCase = @import("parallel_runner.zig").TestCase;

const issue806RightmostField = "value.b.b.b.b.b.b.b.b.b.b.b.b";

const issue806LargeAggregateSource =
    \\R1 : { a : U64, b : U64 }
    \\R2 : { a : R1, b : R1 }
    \\R3 : { a : R2, b : R2 }
    \\R4 : { a : R3, b : R3 }
    \\R5 : { a : R4, b : R4 }
    \\R6 : { a : R5, b : R5 }
    \\R7 : { a : R6, b : R6 }
    \\R8 : { a : R7, b : R7 }
    \\R9 : { a : R8, b : R8 }
    \\R10 : { a : R9, b : R9 }
    \\R11 : { a : R10, b : R10 }
    \\R12 : { a : R11, b : R11 }
    \\
    \\Wrapped := [Payload(R12)]
    \\
    \\make1 : U64 -> R1
    \\make1 = |n| { a: n, b: n + 1 }
    \\
    \\make2 : U64 -> R2
    \\make2 = |n| { a: make1(n), b: make1(n + 2) }
    \\
    \\make3 : U64 -> R3
    \\make3 = |n| { a: make2(n), b: make2(n + 4) }
    \\
    \\make4 : U64 -> R4
    \\make4 = |n| { a: make3(n), b: make3(n + 8) }
    \\
    \\make5 : U64 -> R5
    \\make5 = |n| { a: make4(n), b: make4(n + 16) }
    \\
    \\make6 : U64 -> R6
    \\make6 = |n| { a: make5(n), b: make5(n + 32) }
    \\
    \\make7 : U64 -> R7
    \\make7 = |n| { a: make6(n), b: make6(n + 64) }
    \\
    \\make8 : U64 -> R8
    \\make8 = |n| { a: make7(n), b: make7(n + 128) }
    \\
    \\make9 : U64 -> R9
    \\make9 = |n| { a: make8(n), b: make8(n + 256) }
    \\
    \\make10 : U64 -> R10
    \\make10 = |n| { a: make9(n), b: make9(n + 512) }
    \\
    \\make11 : U64 -> R11
    \\make11 = |n| { a: make10(n), b: make10(n + 1024) }
    \\
    \\make12 : U64 -> R12
    \\make12 = |n| { a: make11(n), b: make11(n + 2048) }
    \\
    \\wrap : U64 -> Wrapped
    \\wrap = |n| Payload(make12(n))
    \\
    \\main = match wrap(1) {
++ "    Payload(value) => " ++ issue806RightmostField ++ "\n" ++
    \\}
;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        .name = "issue 806: large aggregate evaluates across interpreter dev wasm and llvm",
        .source_kind = .module,
        .source = issue806LargeAggregateSource,
        .expected = .{ .inspect_str = "4096" },
    },
    .{
        // https://github.com/roc-lang/roc/issues/9796
        // Two passing top-level expects that both instantiate a parser result
        // alias through a type module must finalize independently without
        // corrupting the checked data consumed by the next expect root.
        .name = "issue 9796: multiple parser expects with forward alias both finalize",
        .source_kind = .module,
        .source =
        \\Parser(input, a) :: { fun : input -> Parser.ParseResult(input, a) }.{
        \\    ParseResult(input, a) : Try({ val : a, input : input }, [ParsingFailure(Str)])
        \\
        \\    build_primitive_parser : (input -> ParseResult(input, a)) -> Parser(input, a)
        \\    build_primitive_parser = |fun| { { fun } }
        \\
        \\    parse_partial : Parser(input, a), input -> ParseResult(input, a)
        \\    parse_partial = |{ fun }, input| {
        \\        fun(input)
        \\    }
        \\
        \\    chomp_until : a -> Parser(List(a), List(a)) where [a.is_eq : a, a -> Bool]
        \\    chomp_until = |char| {
        \\        build_primitive_parser(
        \\            |input| {
        \\                match input.find_first_index(|x| { x == char }) {
        \\                    Ok(index) => {
        \\                        val = input.sublist({ start: 0, len: index })
        \\                        Ok({ val, input: input.drop_first(index) })
        \\                    }
        \\                    Err(_) => Err(ParsingFailure("character not found"))
        \\                }
        \\            }
        \\        )
        \\    }
        \\}
        \\
        \\expect {
        \\    input = "# H\nR".to_utf8()
        \\    result = Parser.parse_partial(Parser.chomp_until('\n'), input)
        \\    result == Ok({ val: ['#', ' ', 'H'], input: ['\n', 'R'] })
        \\}
        \\
        \\expect {
        \\    match Parser.parse_partial(Parser.chomp_until('\n'), []) {
        \\        Ok(_) => Bool.False
        \\        Err(ParsingFailure(_)) => Bool.True
        \\    }
        \\}
        \\
        \\main : U8
        \\main = 0
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        // https://github.com/roc-lang/roc/issues/9686
        // A type alias (`Score : U64`) defined in an imported type module is used
        // as a List element type. The comparator passed to List.sort_with desugars
        // `a < b`/`a > b` to is_lt/is_gt static dispatches on `Score`. Across the
        // module boundary the alias must be unwrapped to its U64 backing for static
        // dispatch; otherwise the comparator fails to resolve / selects the wrong
        // target and the list is not sorted (the issue's wrong `[70, 75, 40]`).
        // Inline (single module) the alias collapses to U64 and sorts correctly.
        // Correct top-3 descending of the input is [100, 90, 75].
        .name = "issue 9686: imported type-module alias element sorts correctly",
        .source_kind = .module,
        .imports = &.{.{
            .name = "HighScores",
            .source =
            \\HighScores :: {}.{
            \\    Score : U64
            \\
            \\    personal_top_three : List(Score) -> List(Score)
            \\    personal_top_three = |scores| {
            \\        scores->sort_desc().take_first(3)
            \\    }
            \\}
            \\
            \\sort_desc = |list| {
            \\    list.sort_with(
            \\        |a, b| if a < b {
            \\            GT
            \\        } else if a > b {
            \\            LT
            \\        } else {
            \\            EQ
            \\        },
            \\    )
            \\}
            ,
        }},
        .source =
        \\import HighScores
        \\
        \\main = HighScores.personal_top_three([10, 30, 90, 30, 100, 20, 10, 0, 30, 40, 40, 75, 70])
        ,
        .expected = .{ .inspect_str = "[100, 90, 75]" },
    },
    .{
        .name = "issue 8949: wasm evaluates to_str after boxed closure allocation",
        .source_kind = .module,
        .source =
        \\State : { count : I64 }
        \\
        \\main = {
        \\    initialState : State
        \\    initialState = { count: 42 }
        \\    _updater = Box.box(|state| { count: state.count + 1 })
        \\    countStr = initialState.count.to_str()
        \\
        \\    "Count: ${countStr}"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Count: 42\"" },
    },
    .{
        .name = "issue 9389: non-existent list method reports a problem instead of crashing",
        .source_kind = .module,
        .source =
        \\Min :: [].{
        \\    foo : List(a) -> List(a)
        \\    foo = |list| list.reverse()
        \\}
        \\
        \\main = Min.foo([0])
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9390: var initialized from polymorphic list.len reads back as U64",
        .source_kind = .module,
        .source =
        \\foo : List(a) -> U64
        \\foo = |list| {
        \\    var $idx = list.len()
        \\    $idx
        \\}
        \\
        \\main = foo([0])
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "issue 9390: var initialized from polymorphic list.len controls one loop iteration",
        .source_kind = .module,
        .source =
        \\foo : List(a) -> U64
        \\foo = |list| {
        \\    var $idx = list.len()
        \\    var $iterations = 0
        \\    while $idx > 0 {
        \\        $idx = $idx - 1
        \\        $iterations = $iterations + 1
        \\    }
        \\    $iterations
        \\}
        \\
        \\main = foo([0])
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "issue 9391: fractional method receiver specializes to F64 argument",
        .source_kind = .module,
        .source =
        \\f : U64 -> F64
        \\f = |_n| 0.001
        \\
        \\main = 0.001.is_lte(f(3))
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9393: greater-than-or-equal on numeric list element terminates",
        .source_kind = .module,
        .source =
        \\uppercase_help : List(I64) -> List(I64)
        \\uppercase_help = |bytes| {
        \\    first = bytes.get(0).ok_or(0)
        \\    if is_lowercase(first) bytes else bytes
        \\}
        \\
        \\is_lowercase = |c| c >= 97
        \\
        \\main = [100]->uppercase_help
        ,
        .expected = .{ .inspect_str = "[100]" },
    },
    .{
        .name = "issue 9395: List.all static dispatch on list literal terminates",
        .source_kind = .module,
        .source =
        \\bar : U8 -> Bool
        \\bar = |_c| Bool.True
        \\
        \\main = [0].all(bar)
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9395: List.any static dispatch on list literal terminates",
        .source_kind = .module,
        .source =
        \\bar : U8 -> Bool
        \\bar = |_c| Bool.True
        \\
        \\main = [0].any(bar)
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9396: undefined identifier in type-module method body reports a problem",
        .source_kind = .module,
        .source =
        \\Min :: [].{
        \\    foo : U8 -> Bool
        \\    foo = |_bar| U8.is_zero(baz)
        \\}
        \\
        \\main = Min.foo(0)
        ,
        .expected = .{ .problem = {} },
    },
    .{
        // Calling `Foo.inspect(Baz)` leaves the nominal type parameter `a`
        // unconstrained: `Baz` carries no `a`, so the `where [a.inspect : a -> Str]`
        // clause has nothing to bind `a` to. Earlier this panicked in mono
        // specialization ("constrained variable matched multiple checked method
        // owners"). The body never invokes the constraint, so the owner is
        // irrelevant and the call simply evaluates to "ok".
        .name = "issue 9502: static dispatch on no-payload variant of polymorphic nominal evaluates",
        .source_kind = .module,
        .source =
        \\Foo(a) := [Bar(a), Baz].{
        \\    inspect : Foo(a) -> Str where [a.inspect : a -> Str]
        \\    inspect = |_foo| "ok"
        \\}
        \\
        \\main = Foo.inspect(Baz)
        ,
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        // A nominal record whose declared field order ({ id, balance }) differs
        // from alphabetical order ({ balance, id }). It is constructed in the
        // imported module and its fields read in the main module, so an
        // inconsistent cross-module layout would read the wrong bytes.
        .name = "nominal record imported across modules reads correct fields",
        .source_kind = .module,
        .imports = &.{.{
            .name = "Acct",
            .source =
            \\Acct := { id : U8, balance : U32 }.{
            \\    sample : Acct
            \\    sample = { id : 7, balance : 99 }
            \\}
            \\
            ,
        }},
        .source =
        \\import Acct exposing [Acct]
        \\
        \\describe : Acct -> U32
        \\describe = |{ id, balance }| id.to_u32() * 1000 + balance
        \\
        \\main = describe(Acct.sample)
        ,
        .expected = .{ .inspect_str = "7099" },
    },
    .{
        // A nominal record with unnamed padding fields (mirroring a C struct's
        // explicit padding). Construction provides only the named fields and the
        // padding bytes stay reserved; reading the named fields back must use the
        // offsets that account for the padding (a@0, b@4), not packed offsets.
        .name = "nominal record with unnamed padding reads correct fields",
        .source_kind = .module,
        .source =
        \\Padded := { a : U8, _ : U8, _ : U8, _ : U8, b : U32 }
        \\
        \\sample : Padded
        \\sample = { a : 7, b : 99 }
        \\
        \\describe : Padded -> U32
        \\describe = |{ a, b }| a.to_u32() * 1000 + b
        \\
        \\main = describe(sample)
        ,
        .expected = .{ .inspect_str = "7099" },
    },
    .{
        // An unnamed padding field whose type is refcounted (`Str`). Its bytes
        // are uninitialized garbage and must never be refcounted, compared, or
        // inspected — only its size is reserved. If the padding spacer were
        // treated as a live Str, dropping the value would decref garbage and
        // crash, so this exercises the refcount/equality padding skip on every
        // backend.
        .name = "nominal record with refcounted-typed padding is never refcounted",
        .source_kind = .module,
        .source =
        \\Padded := { a : U8, _ : Str, b : U32 }
        \\
        \\sample : Padded
        \\sample = { a : 7, b : 99 }
        \\
        \\describe : Padded -> U32
        \\describe = |{ a, b }| a.to_u32() * 1000 + b
        \\
        \\main = describe(sample)
        ,
        .expected = .{ .inspect_str = "7099" },
    },
    .{
        // https://github.com/roc-lang/roc/issues/9725
        // An anonymous record must work as a Dict key. Dict.insert requires
        // `k.to_hash`, which the checker derives structurally for records (as it
        // already does for `is_eq`). Inserting under a record key then reading it
        // back with an equal record key must round-trip: the structural hash makes
        // both keys land in the same bucket and structural is_eq confirms the match.
        .name = "issue 9725: record as a Dict key round-trips",
        .source_kind = .module,
        .source =
        \\main = Dict.empty().insert({ a: 1, b: 2 }, 99).get({ a: 1, b: 2 })
        ,
        .expected = .{ .inspect_str = "Ok(99.0)" },
    },
    .{
        // A small (SSO) string key exercises the wasm backend's str hashing path
        // (hasher_write_str), which must decode small-string inline storage the
        // same way every other backend does, so cross-backend hashes stay equal
        // and the key round-trips on every backend (incl. wasm).
        .name = "issue 9725: small-string Dict key round-trips",
        .source_kind = .module,
        .source =
        \\main = Dict.empty().insert("ab", 99).get("ab")
        ,
        .expected = .{ .inspect_str = "Ok(99.0)" },
    },
    .{
        // The structural `to_hash` derivation generalizes beyond records: a tuple
        // key must work the same way, hashing each element in order.
        .name = "issue 9725: tuple as a Dict key round-trips",
        .source_kind = .module,
        .source =
        \\main = Dict.empty().insert((1, 2), 99).get((1, 2))
        ,
        .expected = .{ .inspect_str = "Ok(99.0)" },
    },
    .{
        // A record key whose field is itself a structural type (a tuple). The
        // derived `to_hash` must recurse into the nested tuple to hash all of it.
        .name = "issue 9725: record with nested tuple as a Dict key round-trips",
        .source_kind = .module,
        .source =
        \\main = Dict.empty().insert({ pair: (1, 2), tag: 3 }, 99).get({ pair: (1, 2), tag: 3 })
        ,
        .expected = .{ .inspect_str = "Ok(99.0)" },
    },
    .{
        // The structural `to_hash` derivation also covers tag unions: the
        // discriminant is hashed first, then the active variant's payloads. Two
        // tags with payloads exercise both the discriminant and the payload paths.
        .name = "issue 9725: tag-union value as a Dict key round-trips",
        .source_kind = .module,
        .source =
        \\Key : [A(U64), B(U64)]
        \\
        \\main = {
        \\    key_a : Key
        \\    key_a = A(1)
        \\    key_b : Key
        \\    key_b = B(1)
        \\    d = Dict.empty().insert(key_a, 11).insert(key_b, 22)
        \\    d.get(B(1))
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(22.0)" },
    },
    .{
        // https://github.com/roc-lang/roc/issues/9783
        // The exact mathematical result is 16129, which does not fit in I8.
        .name = "issue 9783: signed times crashes on overflow",
        .source = "I8.times(I8.highest, I8.highest)",
        .expected = .{ .crash = {} },
    },
    .{
        // https://github.com/roc-lang/roc/issues/9812
        // The exact mathematical result is 128, which does not fit in I8.
        .name = "issue 9812: signed negate crashes on lowest value",
        .source = "I8.negate(I8.lowest)",
        .expected = .{ .crash = {} },
    },
    .{
        // https://github.com/roc-lang/roc/issues/9813
        // The exact mathematical result is 128, which does not fit in I8.
        .name = "issue 9813: signed abs crashes on lowest value",
        .source = "I8.abs(I8.lowest)",
        .expected = .{ .crash = {} },
    },
    .{
        // https://github.com/roc-lang/roc/issues/9814
        // The exact mathematical result is 128, which does not fit in I8.
        .name = "issue 9814: signed div_by crashes on lowest divided by negative one",
        .source = "I8.div_by(I8.lowest, -1)",
        .expected = .{ .crash = {} },
    },
    .{
        // The same signed division edge has a defined remainder of zero.
        .name = "issue 9814: signed rem and mod by negative one return zero",
        .source = "(I8.rem_by(I8.lowest, -1), I8.mod_by(I8.lowest, -1))",
        .expected = .{ .inspect_str = "(0, 0)" },
    },
};
