# META
~~~ini
description=abs_diff method on all number types
type=repl
~~~
# SOURCE
~~~roc
» 10.abs_diff(3)
» 3.abs_diff(10)
» 100.abs_diff(25)
» (3.5).abs_diff(1.2)
» (1.2).abs_diff(3.5)
~~~
# OUTPUT
7
---
7
---
75
---
2.3
---
2.3
# PROBLEMS
NIL
