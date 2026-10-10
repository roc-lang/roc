# Generic forwarding, recursion, and captured literal evidence at two types.
Word := { text: Str }.{
    is_eq : Word, Word -> Bool
    is_eq = |a, b| a.text == b.text

    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        Ok({ text: Str.concat(text, "!") })
    }
}

rank : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
rank = |value| match value {
    "low" => 1
    _ => 2
}

Other := { text: Str }.{
    is_eq : Other, Other -> Bool
    is_eq = |a, b| a.text == b.text

    from_quote : Str -> Try(Other, [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        Ok({ text: Str.concat(text, "?") })
    }
}

forward : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
forward = |value| rank(value)

make_rank : a -> (a -> U64) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
make_rank = |_| |value| forward(value)

repeat_rank : a, U64 -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
repeat_rank = |value, remaining| if remaining == 0 {
    forward(value)
} else {
    repeat_rank(value, remaining - 1)
}

main! = |args| {
    w = if args.len() > 100 "x".Word else "low!".Word
    o = if args.len() > 100 "x".Other else "low!".Other
    first = make_rank(w)
    second = make_rank(o)
    echo!(Str.inspect(first(w)))
    echo!(Str.inspect(second(o)))
    echo!(Str.inspect(repeat_rank(w, 3)))
    echo!(Str.inspect(repeat_rank(o, 3)))
    Ok({})
}
