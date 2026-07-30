# META
~~~ini
description=Structural containers use their components' equality and hash methods
type=repl
~~~
# SOURCE
~~~roc
» d1 = Dict.from_list([("Bob", 3.1), ("John", 4.2)])
» d2 = Dict.from_list([("John", 4.2), ("Bob", 3.1)])
» (d1, 0) == (d2, 0)
» Owes(d1) == Owes(d2)
» [d1] == [d2]
» { items: Set.from_list([1, 2]) } == { items: Set.from_list([2, 1]) }
» Dict.empty().insert({ owes: d1 }, "found").get({ owes: d2 })
» Dict.empty().insert(Set.from_list([1, 2]), "found").get(Set.from_list([2, 1]))
~~~
# OUTPUT
assigned `d1`
---
assigned `d2`
---
True
---
True
---
True
---
True
---
Ok("found")
---
Ok("found")
# PROBLEMS
NIL
