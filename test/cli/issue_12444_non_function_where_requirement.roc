# repro for https://github.com/roc-lang/roc/issues/12444
# A where clause can only require methods. `a.highest : a` is not a function,
# so it is reported where it is declared rather than turning every call of
# `fixed` into an unexplained compiler error.
Fmt := {}.{
    fixed : U64 -> (a -> Str) where [a.highest : a, a.to_str : a -> Str]
    fixed = |_dp|
        |n| n.to_str()
}

expect Fmt.fixed(2)(12.3456) == "12.3456"
