# META
~~~ini
description=Modulo operation for integer types
type=repl
~~~
# SOURCE
~~~roc
» U8.mod_by(10, 3)
» U8.mod_by(10, 5)
» I8.mod_by(10, 3)
» I8.mod_by(-10, 3)
» I8.mod_by(10, -3)
» I8.mod_by(-10, -3)
» U64.mod_by(1000000, 7)
» I64.mod_by(100, 10)
~~~
# OUTPUT
1
---
0
---
1
---
2
---
-2
---
-1
---
1
---
0
# PROBLEMS
NIL
