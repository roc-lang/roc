# A literal's type parameter that has no constraints, sits outside every row
# extension, and that no binding's type reaches is fixed by nothing, so the
# literal converts at `[]` there. A parameter a binding's scheme quantifies, a
# constrained one, and a row extension keep their own rules.
Sig(ctx) :: [Const(Str)].{
    from_quote : Str -> Try(Sig(ctx), [BadQuotedBytes(Str)])
    from_quote = |raw| Ok(Const(raw))

    text : Sig(ctx) -> Str
    text = |s| match s {
        Const(t) => t
    }

    same : Sig(ctx), Sig(ctx) -> Str
    same = |a, _| Sig.text(a)

    with_value : Sig(n), n -> Str where [n.to_str : n -> Str]
    with_value = |s, n| "${Sig.text(s)} ${n.to_str()}"

    with_tag : Sig(t), t -> Str
    with_tag = |s, _| Sig.text(s)
}

# Grounded: the constant's literal parameter is fixed by nothing.
constant : Str
constant = Sig.text("constant")

# Grounded: the parameter is internal to the function body.
in_body : {} -> Str
in_body = |_| Sig.text("in body")

# Not grounded: `pick`'s scheme quantifies the literal's parameter, so it
# stays polymorphic and both calls below check.
pick = |source| Sig.same("picked", source)

# Not grounded: the numeral constrains the parameter, which defaults to a
# number.
numbered : Str
numbered = Sig.with_value("numbered", 5)

# Not grounded: the parameter is a tag row, whose extension keeps its own
# default.
tagged : Str
tagged = Sig.with_tag("tagged", Red)

main! = |_args| {
    u64_source : Sig(U64)
    u64_source = "u64"
    str_source : Sig(Str)
    str_source = "str"
    echo!(constant)
    echo!(in_body({}))
    echo!(pick(u64_source))
    echo!(pick(str_source))
    echo!(numbered)
    echo!(tagged)
    Ok({})
}
