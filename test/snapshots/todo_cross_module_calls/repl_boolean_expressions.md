# META
~~~ini
description=Boolean expressions and operations
type=repl
~~~
# SOURCE
~~~roc
» Bool.true # incorrect, tags must be UPPERCASE
» Bool.false
» Bool.True
» Bool.False
» !Bool.True
» !Bool.False
» Bool.True and Bool.False
» !Bool.True or !Bool.True
~~~
# OUTPUT
Crash: Compile-time error encountered at runtime
---
Crash: Compile-time error encountered at runtime
---
True
---
False
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
