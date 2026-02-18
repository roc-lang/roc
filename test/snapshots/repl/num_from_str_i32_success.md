# META
~~~ini
description=I32.from_str success cases
type=repl
~~~
# SOURCE
~~~roc
» I32.from_str("0")
» I32.from_str("42")
» I32.from_str("-42")
» I32.from_str("2147483647")
» I32.from_str("-2147483648")
~~~
# OUTPUT
Ok(0)
---
Ok(42)
---
Ok(-42)
---
Ok(2147483647)
---
Ok(-2147483648)
# PROBLEMS
NIL
