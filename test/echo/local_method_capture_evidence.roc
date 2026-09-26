# A method of a type declared inside `main!` that captures `main!`'s local,
# used as where-clause evidence.
getit : a -> Str where [a.get : a -> Str]
getit = |x| x.get()

main! = |_args| {
    prefix = "loc"
    Loc := [L].{
        get : Loc -> Str
        get = |_| prefix
    }

    echo!("${getit(Loc.L)}\n")
    Ok({})
}
