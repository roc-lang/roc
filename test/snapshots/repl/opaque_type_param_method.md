# META
~~~ini
description=Opaque type with type params - method call should resolve params correctly
type=repl
skip=true
# TODO: REPL does not support standalone type annotations yet.
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
assigned `unwrap`
---
assigned `unwrap`
---
"hello"
# PROBLEMS
NIL
