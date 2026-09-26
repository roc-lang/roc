## Methods of a nominal type declared inside a function body that capture the
## function's locals, used as where-clause evidence and dispatched directly.
## Each call of the enclosing function sees its own captures.
LocalMethodCaptureEvidence := {}

getit : a -> Str where [a.get : a -> Str]
getit = |x| x.get()

capturing : Str -> Bool
capturing = |prefix| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| prefix
    }

    getit(Loc.L) == "p"
}

echoed : Str -> Str
echoed = |prefix| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| prefix
    }

    getit(Loc.L)
}

direct : Str -> Bool
direct = |prefix| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| prefix
    }

    l : Loc
    l = Loc.L
    l.get() == "p"
}

captured_flow : Str -> Str
captured_flow = |prefix| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| prefix
    }

    l : Loc
    l = Loc.L
    getit(l)
}

expect capturing("p")
expect !capturing("q")
expect echoed("p") == "p" and echoed("q") == "q"
expect direct("p")
expect captured_flow("p") == "p" and captured_flow("q") == "q"
