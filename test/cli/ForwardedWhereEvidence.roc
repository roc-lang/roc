## A where-clause requirement forwarded through two generic functions and
## discharged by a top-level type's method. Debug builds cross-check the
## forwarded chain target against the receiver owner's method.
ForwardedWhereEvidence := {}

Top := [T].{
    get : Top -> Str
    get = |_| "top"
}

inner : a -> Str where [a.get : a -> Str]
inner = |x| x.get()

outer : a -> Str where [a.get : a -> Str]
outer = |x| inner(x)

run : Str -> Str
run = |p| Str.concat(outer(Top.T), p)

expect run("!") == "top!"
