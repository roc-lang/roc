# META
~~~ini
description=Dec.from_str success cases
type=repl
~~~
# SOURCE
~~~roc
» Dec.from_str("0")
» Dec.from_str("123.456")
» Dec.from_str("-99.99")
» Dec.from_str("1000000")
~~~
# OUTPUT
Ok(0)
---
Ok(123.456)
---
Ok(-99.99)
---
Ok(1000000)
# PROBLEMS
NIL
