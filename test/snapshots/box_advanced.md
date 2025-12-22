# META
~~~ini
description=Advanced Box usage: nested boxes, with functions, refcounting
type=repl
~~~
# SOURCE
~~~roc
» Box.unbox(Box.unbox(Box.box(Box.box(42))))
» (|x| x * 2)(Box.unbox(Box.box(21)))
» Str.concat(Box.unbox(Box.box("shared")), Box.unbox(Box.box("shared")))
~~~
# OUTPUT
42
---
42
---
"sharedshared"
# PROBLEMS
NIL
