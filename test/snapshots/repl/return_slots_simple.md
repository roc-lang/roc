# META
~~~ini
description=Simple test for return slots debugging
type=repl
~~~
# SOURCE
~~~roc
» (|x| x)(5)
» (|x| x + 1)(5)
~~~
# OUTPUT
5
---
6
# PROBLEMS
NIL
