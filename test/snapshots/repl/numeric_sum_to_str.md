# META
~~~ini
description=Numeric sum then convert to I16 string
type=repl
~~~
# SOURCE
~~~roc
» a = 4
» b = 5
» sum = a + b
» I16.to_str(sum)
~~~
# OUTPUT
assigned `a`
---
assigned `b`
---
assigned `sum`
---
"9"
# PROBLEMS
NIL
