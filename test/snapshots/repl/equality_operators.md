# META
~~~ini
description=Test == and != operators with numbers, booleans, and strings
type=repl
~~~
# SOURCE
~~~roc
» 1 == 1
» 1 == 2
» 1 != 1
» 1 != 2
» Bool.True == Bool.True
» Bool.True == Bool.False
» Bool.True != Bool.True
» Bool.True != Bool.False
» "hello" == "hello"
» "hello" == "world"
» "hello" != "hello"
» "hello" != "world"
~~~
# OUTPUT
True
---
False
---
False
---
True
---
True
---
False
---
False
---
True
---
True
---
False
---
False
---
True
# PROBLEMS
NIL
