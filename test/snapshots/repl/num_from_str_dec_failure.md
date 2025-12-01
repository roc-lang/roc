# META
~~~ini
description=Dec.from_str failure cases
type=repl
~~~
# SOURCE
~~~roc
» Dec.from_str("hello")
» Dec.from_str("")
» Dec.from_str("1.2.3")
~~~
# OUTPUT
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
# PROBLEMS
NIL
