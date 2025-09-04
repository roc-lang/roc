# META
~~~ini
description=Record field access
type=repl
~~~
# SOURCE
~~~roc
» {}.foo
» {foo: "Hello"}.foo
» {foo: "Hello", bar: "World"}.bar
~~~
# OUTPUT
Evaluation error: error.ZeroSizedType
---
"Hello"
---
"World"
# PROBLEMS
NIL
