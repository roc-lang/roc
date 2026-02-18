# META
~~~ini
description=String ordering operations should fail gracefully (not supported)
type=repl
~~~
# SOURCE
~~~roc
» "apple" > "banana"
» "zoo" < "aardvark"
» "equal" >= "equal"
» "first" <= "second"
~~~
# OUTPUT
TYPE MISMATCH
---
TYPE MISMATCH
---
TYPE MISMATCH
---
TYPE MISMATCH
# PROBLEMS
NIL
