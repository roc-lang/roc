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
Evaluation error: error.Crash
---
Evaluation error: error.Crash
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
# PROBLEMS
NIL
