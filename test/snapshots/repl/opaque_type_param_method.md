# META
~~~ini
description=Opaque type with type params - method call should resolve params correctly
type=repl
~~~
# SOURCE
~~~roc
» Wrapper(a) := { inner : a }
» unwrap : Wrapper(a) -> a
» unwrap = |w| w.inner
» unwrap({ inner: "hello" })
~~~
# OUTPUT
assigned `Wrapper`
---

---
assigned `unwrap`
---
"hello"
# PROBLEMS
NIL
