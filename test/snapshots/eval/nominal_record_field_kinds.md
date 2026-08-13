# META
~~~ini
description=Nominal-backed records resolve optional and defaulted field kinds through their backing row: construct, omit, update, read, and a generic nominal
type=repl
~~~
# SOURCE
~~~roc
» Wrap := { a : U8, b ?: U8, c : U8 ?? 10 }
» w = Wrap.{ a: 1, b: 2 }
» w
» u = { ..w, b: 9 }
» u
» u.?b ?? 99
» omitted = Wrap.{ a: 3 }
» omitted
» Pair(t) := { first : t, second ?: t }
» p = Pair.{ first: "one" }
» p
~~~
# OUTPUT
assigned `Wrap`
---
assigned `w`
---
{ a: 1, b: 2, c: 10 }
---
assigned `u`
---
{ a: 1, b: 9, c: 10 }
---
9
---
assigned `omitted`
---
{ a: 3, b: <missing>, c: 10 }
---
assigned `Pair`
---
assigned `p`
---
{ first: "one", second: <missing> }
# PROBLEMS
NIL
