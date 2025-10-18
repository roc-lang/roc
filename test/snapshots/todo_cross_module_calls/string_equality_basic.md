# META
~~~ini
description=Basic string equality comparisons
type=repl
~~~
# SOURCE
~~~roc
» "hello" == "hello"
» "hello" == "world"
» "hello" != "world"
» "hello" != "hello"
» "" == ""
» "" == "not empty"
» "same length" == "different.."
~~~
# OUTPUT
True
---
False
---
True
---
False
---
True
---
False
---
False
# PROBLEMS
NIL
