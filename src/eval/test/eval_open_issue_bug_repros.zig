//! Regression repros for open GitHub issues older than one week.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Open-issue regression test cases that run through the eval test harness.
pub const tests = [_]TestCase{
    .{
        .name = "issue 9353: string interpolation with annotation-only value reports a problem",
        .source_kind = .module,
        .source =
        \\x : Str
        \\
        \\main = "value: ${x}"
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9353: string interpolation with closure reports a problem",
        .source_kind = .module,
        .source =
        \\main = "value: ${|x| x}"
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9348: static dispatch on built-in generic type accepts named lambda",
        .source_kind = .module,
        .source =
        \\main = {
        \\    f = |x| x * 2
        \\    [1, 2, 3].map(f) == [2, 4, 6]
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9328: dbg does not make pure function effectful",
        .source_kind = .module,
        .source =
        \\User := [
        \\    LoggedIn(Str),
        \\    Guest,
        \\].{
        \\    welcome : User -> {}
        \\    welcome = |self| match self {
        \\        Guest => dbg "Welcome Guest"
        \\        LoggedIn(name) => dbg Str.concat("Welcome Guest", name)
        \\    }
        \\}
        \\
        \\main = User.welcome(Guest)
        ,
        .expected = .{ .inspect_str = "{}" },
    },
    .{
        .name = "issue 9252: List.get on zero-sized values returns Ok value",
        .source_kind = .module,
        .source =
        \\main = [{}].get(0) == Ok({})
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9244: missing numeric method without annotation reports a problem",
        .source_kind = .module,
        .source =
        \\main = {
        \\    x = 3
        \\    x.to_u64()
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9230: duplicate type annotation reports a problem",
        .source_kind = .module,
        .source =
        \\hey : Str
        \\hey : Str
        \\hey = "yo"
        \\
        \\main = hey
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9228: List(U8).get returns the requested element",
        .source_kind = .module,
        .source =
        \\main = {
        \\    nums : List(U8)
        \\    nums = [10.U8, 100.U8]
        \\    nums.get(1).ok_or(0)
        \\}
        ,
        .expected = .{ .inspect_str = "100" },
    },
    .{
        .name = "issue 9210: tuple index access inside lambda preserves tuple type",
        .source_kind = .module,
        .source =
        \\first_of_tuple = |tup| tup.0
        \\
        \\main = first_of_tuple(("first", "last"))
        ,
        .expected = .{ .inspect_str = "\"first\"" },
    },
    .{
        .name = "issue 9209: multiple list item retrievals keep receiver type",
        .source_kind = .module,
        .source =
        \\main = {
        \\    list = ["foo", "bar"]
        \\    foo = list.first().ok_or("")
        \\    bar = list.last().ok_or("")
        \\    foo.concat(bar)
        \\}
        ,
        .expected = .{ .inspect_str = "\"foobar\"" },
    },
    .{
        .name = "issue 9175: invalid boolean lhs reports a problem",
        .source_kind = .module,
        .source =
        \\x = 0 and o
        \\
        \\main = x
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9170: record with 33 numeric fields type-checks without allocator corruption",
        .source_kind = .module,
        .source =
        \\r = {
        \\    f0: 0.U8, f1: 0.U8, f2: 0.U8, f3: 0.U8, f4: 0.U8,
        \\    f5: 0.U8, f6: 0.U8, f7: 0.U8, f8: 0.U8, f9: 0.U8,
        \\    f10: 0.U8, f11: 0.U8, f12: 0.U8, f13: 0.U8, f14: 0.U8,
        \\    f15: 0.U8, f16: 0.U8, f17: 0.U8, f18: 0.U8, f19: 0.U8,
        \\    f20: 0.U8, f21: 0.U8, f22: 0.U8, f23: 0.U8, f24: 0.U8,
        \\    f25: 0.U8, f26: 0.U8, f27: 0.U8, f28: 0.U8, f29: 0.U8,
        \\    f30: 0.U8, f31: 0.U8, f32: 0.U8,
        \\}
        \\
        \\main = r.f32
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "issue 9132: type-module closure can return tag union with multiple payloads",
        .source_kind = .module,
        .source =
        \\Wrapper(input, val) := { run : input -> [Ok(val, input), Err(Str)] }.{
        \\    apply : Wrapper(input, val), input -> [Ok(val, input), Err(Str)]
        \\    apply = |wrapper, inp| (wrapper.run)(inp)
        \\
        \\    succeed : val -> Wrapper(input, val)
        \\    succeed = |value| { run: |inp| Ok(value, inp) }
        \\
        \\    map : Wrapper(input, a), (a -> b) -> Wrapper(input, b)
        \\    map = |wrapper, transform| {
        \\        run: |inp|
        \\            match apply(wrapper, inp) {
        \\                Err(msg) => Err(msg)
        \\                Ok(val, rest) => Ok(transform(val), rest)
        \\            }
        \\    }
        \\}
        \\
        \\mapped = Wrapper.map(Wrapper.succeed("a"), |x| Str.concat(x, "!"))
        \\
        \\main = Wrapper.apply(mapped, "input") == Ok("a!", "input")
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9123: same-module nominal method works in string interpolation",
        .source_kind = .module,
        .source =
        \\Color := [Red, Green, Blue].{
        \\    as_str : Color -> Str
        \\    as_str = |color| match color {
        \\        Red => "Red"
        \\        Green => "Green"
        \\        Blue => "Blue"
        \\    }
        \\}
        \\
        \\main = {
        \\    red : Color
        \\    red = Red
        \\    "My favourite color is ${red.as_str()}"
        \\}
        ,
        .expected = .{ .inspect_str = "\"My favourite color is Red\"" },
    },
    .{
        .name = "issue 9119: cross-file fold calls work during comptime test evaluation",
        .source_kind = .module,
        .imports = &.{.{
            .name = "Bug3Module",
            .source =
            \\module [count_chars]
            \\
            \\count_chars : Str -> U32
            \\count_chars = |str| {
            \\    bytes = Str.to_utf8(str)
            \\    List.fold(bytes, 0, |acc, _byte| acc + 1)
            \\}
            ,
        }},
        .source =
        \\import Bug3Module
        \\
        \\Value := [VAtom(Str), ..].{
        \\    parse_and_count : Str -> Value
        \\    parse_and_count = |s| {
        \\        count = Bug3Module.count_chars(s)
        \\        VAtom("len=${count.to_str()}")
        \\    }
        \\}
        \\
        \\main = match Value.parse_and_count("hello") {
        \\    VAtom("len=5") => True
        \\    _ => False
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9117: type-method local bindings keep other parameters in scope",
        .source_kind = .module,
        .source =
        \\Foo := {}.{
        \\    method : U32, U16 -> Str
        \\    method = |a, b| {
        \\        a64 = a.to_u64()
        \\        b64 = b.to_u64()
        \\        (a64 * b64).to_str()
        \\    }
        \\}
        \\
        \\main = Foo.method(5, 10)
        ,
        .expected = .{ .inspect_str = "\"50\"" },
    },
    .{
        .name = "issue 9114: imported nominal tag union method maps right value",
        .source_kind = .module,
        .imports = &.{.{
            .name = "Either",
            .source =
            \\module [Either]
            \\
            \\Either(left, right) := [Left(left), Right(right)].{
            \\    map_right = |either, f| {
            \\        match either {
            \\            Left(l) => Left(l)
            \\            Right(r) => Right(f(r))
            \\        }
            \\    }
            \\}
            ,
        }},
        .source =
        \\import Either
        \\
        \\create_right : U64 -> Either.Either(Str, U64)
        \\create_right = |a| Right(a)
        \\
        \\double = |a| a * 2
        \\
        \\main = match create_right(23).map_right(double) {
        \\    Right(46) => True
        \\    _ => False
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9080: while loop with default numeric type terminates",
        .source_kind = .module,
        .source =
        \\main = {
        \\    max = 10
        \\    var $i = 0
        \\    while $i < max {
        \\        $i = $i + 1
        \\    }
        \\    $i
        \\}
        ,
        .expected = .{ .inspect_str = "10.0" },
    },
    .{
        .name = "issue 9054: recursive compress with equality-bound annotation evaluates",
        .source_kind = .module,
        .source =
        \\compress : List(a) -> List(a) where [a.is_eq : a, a -> Bool]
        \\compress = |l| {
        \\    match l {
        \\        [] => []
        \\        [e] => [e]
        \\        [e1, e2, .. as rest] => {
        \\            rest_compression = compress(List.concat([e2], rest))
        \\            if e1 == e2 { rest_compression } else { List.concat([e1], rest_compression) }
        \\        }
        \\    }
        \\}
        \\
        \\main = compress([1.U8, 2.U8])
        ,
        .expected = .{ .inspect_str = "[1, 2]" },
    },
    .{
        .name = "issue 9053: recursive flatten test does not overflow compiler stack",
        .source_kind = .module,
        .source =
        \\Node(a) := [One(a), Many(List(Node(a)))]
        \\
        \\flatten : List(Node(a)) -> List(a)
        \\flatten = |input| {
        \\    flatten_aux = |l, acc| {
        \\        match l {
        \\            [] => acc
        \\            [One(e), .. as rest] => flatten_aux(rest, List.append(acc, e))
        \\            [Many(e), .. as rest] => flatten_aux(rest, flatten_aux(e, acc))
        \\        }
        \\    }
        \\    flatten_aux(input, [])
        \\}
        \\
        \\main = flatten([One("a"), Many([One("b"), Many([One("c"), One("d")]), One("e")])]) == ["a", "b", "c", "d", "e"]
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9052: malformed tag arithmetic reports a problem",
        .source_kind = .module,
        .source =
        \\p = F(0, 0 - B)
        \\
        \\main = p
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9020: local closure with repeated question operators in fold evaluates",
        .source_kind = .module,
        .source =
        \\sum : List(U64) -> U64
        \\sum = |nums| nums.len()
        \\
        \\main = {
        \\    f = |digits| Ok(List.first(digits)? + List.first(digits)?)
        \\    vs = ["a"].map(|s| s.to_utf8().map(|c| c.to_u64()))
        \\    vs.fold(
        \\        [],
        \\        |acc, v| {
        \\            match f(v) {
        \\                Ok(u) => acc.append(u)
        \\                Err(_) => acc
        \\            }
        \\        },
        \\    )->sum()
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "issue 9019: static dispatch rejects functions whose first arg is not receiver type",
        .source_kind = .module,
        .source =
        \\main = {
        \\    lst = 1.until(10)
        \\    lst.repeat(3)
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9018: List(a) annotation accepts List(List(U64)) argument",
        .source_kind = .module,
        .source =
        \\keep_oks : List(a), (a -> Try(b, e)) -> List(b)
        \\keep_oks = |vs, f| {
        \\    vs.fold(
        \\        [],
        \\        |acc, v| {
        \\            match f(v) {
        \\                Ok(u) => acc.append(u)
        \\                Err(_) => acc
        \\            }
        \\        },
        \\    )
        \\}
        \\
        \\extract_digits : Str -> List(U64)
        \\extract_digits = |s| s.to_utf8().map(|c| c.to_u64())
        \\
        \\main = keep_oks(["a", "b"].map(extract_digits), |digits| Ok(digits)) == [[97], [98]]
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9011: recursive parser returns nested value without refcount panic",
        .source_kind = .module,
        .source =
        \\TokenContents : [
        \\    EqualToken,
        \\    ModuloToken,
        \\    UIntDigitsToken(U64),
        \\    IdentToken(Str),
        \\    EndOfFileToken,
        \\]
        \\
        \\TokenizerResult : (TokenContents, U64)
        \\
        \\get_next_token! : U64 => TokenizerResult
        \\get_next_token! = |index| {
        \\    tokens : List(TokenContents)
        \\    tokens = [IdentToken("i"), ModuloToken, UIntDigitsToken(15), EqualToken, UIntDigitsToken(0)]
        \\    match List.get(tokens, index) {
        \\        Ok(value) => (value, index + 1)
        \\        Err(_) => (EndOfFileToken, index)
        \\    }
        \\}
        \\
        \\ValueCombinationMethod := [
        \\    BooleanAnd, BooleanOr, BooleanNot,
        \\    IsEqual, IsNotEqual, IsGreaterThan, IsLessThan, IsGreaterThanOrEqual, IsLessThanOrEqual,
        \\    Multiply, Divide, Modulo,
        \\    Add, Subtract,
        \\]
        \\
        \\Value := [
        \\    UInt(U64),
        \\    CombinedValue({ combination_method: ValueCombinationMethod }),
        \\]
        \\
        \\parse_value! : TokenizerResult => Try((TokenContents, Value, U64), Str)
        \\parse_value! = |result| {
        \\    (_, index) = result
        \\    var $index = index
        \\    (token2, $index) = get_next_token!($index)
        \\    combination_method1 : ValueCombinationMethod
        \\    combination_method1 = match token2 {
        \\        ModuloToken => Modulo
        \\        _ => {
        \\            return Ok((token2, UInt(3), $index))
        \\        }
        \\    }
        \\    (token3, _value2, $index) = parse_value!(get_next_token!($index))?
        \\    value : Value
        \\    value = CombinedValue({ combination_method: combination_method1 })
        \\    Ok((token3, value, $index))
        \\}
        \\
        \\main = match parse_value!(get_next_token!(0)) {
        \\    Ok(_) => True
        \\    Err(_) => False
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9010: closed tag union error merges into open try error",
        .source_kind = .module,
        .source =
        \\bar : {} -> Try(U32, [Bar(U8)])
        \\bar = |{}| Err(Bar(5))
        \\
        \\foo : {} -> Try(U32, [Foo(U8), ..])
        \\foo = |{}| {
        \\    a = bar({})?
        \\    Ok(a + 5)
        \\}
        \\
        \\main = foo({}) == Err(Bar(5))
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9005: invalid tag arithmetic reports a problem",
        .source_kind = .module,
        .source =
        \\b = 6 + L
        \\
        \\main = b
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9002: malformed open record comparison reports a problem",
        .source_kind = .module,
        .source =
        \\R0000 : { x : F }
        \\a00a000 : R -> R0000
        \\a00a000 = { ..0, x0 }
        \\x = a00a000({}) == 0 / {}
        \\
        \\main = x
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9000: invalid field access on scientific literal reports a problem",
        .source_kind = .module,
        .source =
        \\y = 1E10.e
        \\
        \\main = y
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 8937: recursive pack expect terminates",
        .source_kind = .module,
        .source =
        \\pack : List(a) -> List(List(a)) where [a.is_eq : a, a -> Bool]
        \\pack = |input| {
        \\    pack_aux = |l, acc| {
        \\        match l {
        \\            [] => acc
        \\            [e, .. as rest] => {
        \\                rest_pack = pack_aux(rest, [])
        \\                match rest_pack {
        \\                    [] => [[e]]
        \\                    [p, .. as rprest] => {
        \\                        match p {
        \\                            [rpe, ..] => {
        \\                                if e == rpe {
        \\                                    List.concat([List.append(p, e)], rprest)
        \\                                } else {
        \\                                    List.concat([[e]], rest_pack)
        \\                                }
        \\                            }
        \\                            _ => rest_pack
        \\                        }
        \\                    }
        \\                    _ => rest_pack
        \\                }
        \\            }
        \\        }
        \\    }
        \\    pack_aux(input, [])
        \\}
        \\
        \\main = pack(["a", "a", "b", "c", "c"]) == [["a", "a"], ["b"], ["c", "c"]]
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 8890: while loop with repeated List.get has reasonable small-case behavior",
        .source_kind = .module,
        .source =
        \\find_substring : List(U8), List(U8) -> Try(U64, [NotFound])
        \\find_substring = |haystack, needle| {
        \\    needle_len = List.len(needle)
        \\    haystack_len = List.len(haystack)
        \\    pos_init : U64
        \\    pos_init = 0
        \\    var $pos = pos_init
        \\    var $found = Bool.False
        \\    limit = haystack_len - needle_len + 1
        \\    while !$found and $pos < limit {
        \\        if matches_at_pos(haystack, needle, $pos) {
        \\            $found = Bool.True
        \\        } else {
        \\            $pos = $pos + 1
        \\        }
        \\    }
        \\    if $found { Ok($pos) } else { Err(NotFound) }
        \\}
        \\
        \\matches_at_pos : List(U8), List(U8), U64 -> Bool
        \\matches_at_pos = |haystack, needle, pos| {
        \\    needle_len = List.len(needle)
        \\    var $matches = Bool.True
        \\    idx_init : U64
        \\    idx_init = 0
        \\    var $idx = idx_init
        \\    while $matches and $idx < needle_len {
        \\        match (List.get(haystack, pos + $idx), List.get(needle, $idx)) {
        \\            (Ok(h), Ok(n)) => if h != n { $matches = Bool.False }
        \\            _ => { $matches = Bool.False }
        \\        }
        \\        $idx = $idx + 1
        \\    }
        \\    $matches
        \\}
        \\
        \\main = find_substring("abcdef".to_utf8(), "cd".to_utf8()) == Ok(2)
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 8885: unary not on number reports a problem",
        .source_kind = .module,
        .source =
        \\main = !3
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 8540: fold over tuple accumulator and I64.mod_by evaluates",
        .source_kind = .module,
        .source =
        \\input = "L68\nL30"
        \\
        \\rotate : (I64, I64), Str -> (I64, I64)
        \\rotate = |(position, zeroes), _line| {
        \\    (position.mod_by(100), zeroes)
        \\}
        \\
        \\main = input
        \\    .split_on("\n")
        \\    .fold((50, 0), rotate)
        ,
        .expected = .{ .inspect_str = "(50, 0)" },
    },
};
