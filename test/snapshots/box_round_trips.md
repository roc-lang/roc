# META
~~~ini
description=Box.box and Box.unbox round-trips with various types
type=repl
~~~
# SOURCE
~~~roc
» Box.unbox(Box.box(42))
» Box.unbox(Box.box(123))
» Box.unbox(Box.box("hello"))
» Box.unbox(Box.box([1, 2, 3]))
» Box.unbox(Box.box({ x: 1, y: 2 }))
» Box.unbox(Box.box((1, "two", 3.0)))
» Box.unbox(Box.box({}))
~~~
# OUTPUT
42.0
---
123.0
---
"hello"
---
[1.0, 2.0, 3.0]
---
{ x: 1.0, y: 2.0 }
---
(1.0, "two", 3.0)
---
{}
# PROBLEMS
NIL
