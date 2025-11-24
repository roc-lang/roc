# META
~~~ini
description=Test == and != operators with numbers, booleans, strings, and lists
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
» [1, 2, 3] == [1, 2, 3]
» [1, 2, 3] == [1, 2, 4]
» [1, 2, 3] != [1, 2, 3]
» [1, 2, 3] != [1, 2, 4]
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
