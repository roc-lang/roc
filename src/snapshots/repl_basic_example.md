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
# OUTPUT
2
---
Evaluation error: error.LayoutError
---
"Hello, World!"
---
<list_of_zst>
# PROBLEMS
NIL
