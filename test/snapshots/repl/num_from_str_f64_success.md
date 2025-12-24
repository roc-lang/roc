# META
~~~ini
description=F64.from_str success cases
type=repl
~~~
# SOURCE
~~~roc
» F64.from_str("0")
» F64.from_str("3.14159")
» F64.from_str("-2.5")
» F64.from_str("1e10")
» F64.from_str("-1.5e-3")
~~~
# OUTPUT
Ok(0)
---
Ok(3.14159)
---
Ok(-2.5)
---
Ok(10000000000)
---
Ok(-0.0015)
# PROBLEMS
NIL
