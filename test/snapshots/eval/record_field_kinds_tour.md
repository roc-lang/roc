# META
~~~ini
description=Runtime tour of required, optional, and defaulted record fields: read, default, update, destructure, and inspect
type=repl
~~~
# SOURCE
~~~roc
» Rec : { req : U8, def : U8 ?? 10, opt ?: U8 }
» r : Rec
» r = { req: 1 }
» r
» r.?opt ?? 99
» r.def
» s = { ..r, opt: 7 }
» s
» s.?opt ?? 99
» take_opt : Rec -> U8
» take_opt = |rec| {
    { opt, .. } = rec
    opt ?? 0
}
» take_opt(s)
~~~
# OUTPUT
assigned `Rec`
---

---
assigned `r`
---
{ def: 10, opt: <missing>, req: 1 }
---
99
---
10
---
assigned `s`
---
{ def: 10, opt: 7, req: 1 }
---
7
---

---
assigned `take_opt`
---
7
# PROBLEMS
NIL
