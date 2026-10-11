# `wrap` is specialized once for its compile-time call and once for its
# runtime call, and both specializations convert the same literal to the same
# type, so they share one literal root. The root's value holds a closure, which
# the runtime program must name the same way as the compile-time program that
# evaluated it.
Word := [Word(Str -> Str)].{
    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text| Ok(Word(|x| Str.concat(text, x)))

    apply : Word, Str -> Str
    apply = |Word(f), x| f(x)
}

wrap : Str -> Str
wrap = |body| {
    w : Word
    w = "pre-"
    w.apply(body)
}

main! = |args| {
    echo!(wrap("a"))
    echo!(wrap(Str.concat("b", Str.inspect(List.len(args)))))
    Ok({})
}
