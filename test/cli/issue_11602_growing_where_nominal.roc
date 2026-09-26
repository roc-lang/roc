# Repro for https://github.com/roc-lang/roc/issues/11602
#
# `Thing` is invalid because its recursion grows its argument, and `Thing`
# appears in a where alias's method signature. The compiler must report the
# invalid recursive type; it must not panic while publishing the checked
# artifact.
Thing(a) :: [A(Thing(List(a)))]

coll.ToThings(a) : where [coll.to_things : coll -> Thing(a)]

map : input -> Str where [input.ToThings(a)]
map = |_input| ""
