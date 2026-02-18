# META
~~~ini
description=from_str negative literals return Err for unsigned integers
type=repl
~~~
# SOURCE
~~~roc
» U8.from_str("-1")
» U8.from_str("-128")
» U16.from_str("-1")
» U16.from_str("-1000")
» U32.from_str("-1")
» U32.from_str("-999999")
» U64.from_str("-1")
» U64.from_str("-12345678901234")
» U128.from_str("-1")
» U128.from_str("-99999999999999999999")
~~~
# OUTPUT
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
---
Err(BadNumStr)
# PROBLEMS
NIL
