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
Evaluation error: error.StringOrderingNotSupported
---
Evaluation error: error.StringOrderingNotSupported
---
Evaluation error: error.StringOrderingNotSupported
---
Evaluation error: error.StringOrderingNotSupported
# PROBLEMS
NIL
