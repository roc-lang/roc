app [main!] { pf: platform "./platform/main.roc" }

Word := { code: U64, spelling: Str }.{
    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text|
        if text == "never" {
            crash "an unreachable pattern conversion was evaluated"
        } else if text == "low" or text == "alias-low" {
            Ok({ code: 1, spelling: text })
        } else if text == "high" or text == "alias-high" {
            Ok({ code: 2, spelling: text })
        } else {
            Ok({ code: 3, spelling: text })
        }

    # Different spellings are equal; backing-record equality is incorrect here.
    is_eq : Word, Word -> Bool
    is_eq = |a, b|
        if a.spelling == "poison" {
            crash "a later pattern equality was evaluated"
        } else {
            a.code == b.code
        }
}

rank : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
rank = |value| match value {
    "low" => 1
    "high" => 2
    _ => 3
}

forward = |value| rank(value)

make = || "low"

capture = |flag| {
    run = |value| if flag rank(value) else 3
    run
}

guarded = |a, b, flag| match (a, b) {
    ("low", "high") if flag => 1
    ("low", "high") => 2
    _ => 3
}

short_circuit = |value| match value {
    "low" => 1
    "never" => 2
    _ => 3
}

expect rank("alias-low".Word) == 1
expect rank("alias-high".Word) == 2
expect rank("other".Word) == 3
expect forward("alias-low".Word) == 1
expect capture(True)("alias-low".Word) == 1

main! : List(Str) => Try({}, [Exit(I8)])
main! = |args| {
    low = "alias-low".Word
    high = "alias-high".Word
    other = "other".Word
    poison = "poison".Word
    made : Word
    made = make()
    made_str : Str
    made_str = make()
    saved = capture(True)
    first : U64
    first = guarded(low, high, True)
    second : U64
    second = guarded(low, high, False)
    rest : U64
    rest = guarded(other, poison, True)
    short : U64
    short = short_circuit(low)
    low_str = rank(if args.is_empty() "low".Str else "other".Str)
    high_str = rank("high".Str)
    other_str = rank("other".Str)

    if rank(low) != 1 or rank(high) != 2 or rank(other) != 3 {
        Err(Exit(1))
    } else if forward(low) != 1 or saved(low) != 1 or rank(made) != 1 or made_str != "low" {
        Err(Exit(2))
    } else if low_str != 1 or high_str != 2 or other_str != 3 {
        Err(Exit(3))
    } else if first != 1 or second != 2 or rest != 3 or short != 1 {
        Err(Exit(4))
    } else {
        Ok({})
    }
}
