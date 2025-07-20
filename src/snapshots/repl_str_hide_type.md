# META
~~~ini
description=String values should not display type annotations in REPL
type=repl
~~~
# SOURCE
~~~roc
» "hello"
» "world"
» "Hello, World!"
~~~
# OUTPUT
Evaluation error: error.LayoutError
---
Evaluation error: error.LayoutError
---
Evaluation error: error.LayoutError
# PROBLEMS
NIL
