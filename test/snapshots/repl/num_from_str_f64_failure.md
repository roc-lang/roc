# META
~~~ini
description=F64.from_str failure cases
type=repl
~~~
# SOURCE
~~~roc
» F64.from_str("hello")
» F64.from_str("")
» F64.from_str("1.2.3")
~~~
# OUTPUT
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
# PROBLEMS
NIL
