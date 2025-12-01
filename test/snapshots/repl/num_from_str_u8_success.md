# META
~~~ini
description=U8.from_str success cases
type=repl
~~~
# SOURCE
~~~roc
» U8.from_str("0")
» U8.from_str("42")
» U8.from_str("255")
~~~
# OUTPUT
Ok(0)
---
Ok(42)
---
Ok(255)
# PROBLEMS
NIL
