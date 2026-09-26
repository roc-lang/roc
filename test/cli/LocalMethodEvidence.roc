## Methods of a nominal type declared inside a function body, used as
## where-clause evidence. The methods exist only in the block that declares
## them, so an expression whose checked evidence names one is not hoisted out
## of that block into a compile-time root, however the value reached it.
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

flow : Str -> Bool
flow = |prefix| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "p"
    }

    l : Loc
    l = Loc.L
    x = getit(l)
    x == prefix
}

escaping : Str -> Str
escaping = |suffix| {
    z = {
        Loc := [L].{
            get : Loc -> Str
            get = |_| "e"
        }
        Loc.L
    }
    Str.concat(getit(z), suffix)
}

expect plain("p")
expect !plain("q")
expect whole("x") == "wx"
expect flow("p")
expect !flow("q")
expect escaping("s") == "es"
