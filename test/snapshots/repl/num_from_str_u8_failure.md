# META
~~~ini
description=U8.from_str failure cases
type=repl
~~~
# SOURCE
~~~roc
» U8.from_str("256")
» U8.from_str("-1")
» U8.from_str("hello")
» U8.from_str("")
» U8.from_str("12.5")
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
