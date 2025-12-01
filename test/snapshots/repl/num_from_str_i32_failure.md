# META
~~~ini
description=I32.from_str failure cases
type=repl
~~~
# SOURCE
~~~roc
» I32.from_str("2147483648")
» I32.from_str("-2147483649")
» I32.from_str("hello")
» I32.from_str("")
» I32.from_str("12.5")
~~~
# OUTPUT
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
# PROBLEMS
NIL
