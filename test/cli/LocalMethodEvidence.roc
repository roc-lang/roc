## Methods of a nominal type declared inside a function body, used as
## where-clause evidence. The type and its methods exist only in the block
## that declares them, so a closed expression referring to the type is not
## hoisted out of that block into a compile-time root.
LocalMethodEvidence := {}

getit : a -> Str where [a.get : a -> Str]
getit = |x| x.get()

plain : Str -> Bool
plain = |prefix| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "p"
    }

    getit(Loc.L) == prefix
}

whole : Str -> Str
whole = |suffix| {
    z = {
        Loc := [L].{
            get : Loc -> Str
            get = |_| "w"
        }
        getit(Loc.L)
    }
    Str.concat(z, suffix)
}

expect plain("p")
expect !plain("q")
expect whole("x") == "wx"
