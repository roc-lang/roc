# META
~~~ini
description=Dict.fold_until folds until the step returns Break, and full folds otherwise
type=repl
~~~
# SOURCE
~~~roc
» d = Dict.empty().insert("alice", 10.I64).insert("bob", 20).insert("charlie", 30)
» d.fold_until(0, |acc, _k, v| if acc + v >= 20 { Break(acc + v) } else { Continue(acc + v) })
» d.fold_until(0, |acc, _k, v| Continue(acc + v))
» d.len()
~~~
# OUTPUT
assigned `d`
---
30
---
60
---
3
# PROBLEMS
NIL
