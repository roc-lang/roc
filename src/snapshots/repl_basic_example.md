# META
~~~ini
description=two strings
type=repl
~~~
# SOURCE
~~~roc
» 1 + 1
» 0.1 + 0.2
» "Hello, World!"
» []
~~~
# EXPECTED
2
---
Evaluation error: error.LayoutError
---
Evaluation error: error.LayoutError
---
[] : List(_elem)
# PROBLEMS
NIL
