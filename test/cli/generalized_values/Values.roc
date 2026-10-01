# Generalized values imported by GeneralizedValueObservations.roc and
# GeneralizedValueCrash.roc: every importer specialization is compile-time
# evaluated in the importing program (design.md "Specialization-Owned
# Top-Level Values").
Values := [].{
    grow : U64, List(a) -> List(a)
    grow = |n, acc| if n == 0 { acc } else { grow(n - 1, acc) }

    made : List(a)
    made = {
        dbg "evaluating made"
        grow(3, [])
    }

    checked : List(a)
    checked = {
        expect List.len(grow(2, [])) == 1
        []
    }

    none : List(Str)
    none = []

    # Crashes on the path compile-time evaluation takes, but has a path that
    # returns a value, so it is still evaluated per specialization (a value
    # whose body always crashes is evaluated at its use instead).
    boom : List(a)
    boom = if List.is_empty(none) { crash "no list today" } else { [] }
}
