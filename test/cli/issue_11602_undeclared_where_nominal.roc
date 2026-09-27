# Repro for https://github.com/roc-lang/roc/issues/11602
#
# `b` is an undeclared type variable in `Thing`'s backing type, and `Thing`
# appears in a where-clause method constraint. The compiler must report the
# undeclared type variable (like `roc check` does without the where clause);
# it must not panic while publishing the checked artifact.
coll.ToThings(a) : where [coll.to_things : coll -> Thing(a)]

Thing(a) :: b

map : input -> Thing(a) where [input.ToThings(a)]
map = |input| input.to_things()
