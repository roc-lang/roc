# META
~~~ini
description=from_str for all integer types (I8, U16, I16, U32, U64)
type=repl
~~~
# SOURCE
~~~roc
» I8.from_str("127")
» I8.from_str("-128")
» I8.from_str("128")
» U16.from_str("65535")
» U16.from_str("65536")
» I16.from_str("32767")
» I16.from_str("-32768")
» I16.from_str("32768")
» U32.from_str("4294967295")
» U32.from_str("4294967296")
» U64.from_str("18446744073709551615")
» U64.from_str("18446744073709551616")
~~~
# OUTPUT
Ok(127)
---
Ok(-128)
---
Err(BadNumStr)
---
Ok(65535)
---
Err(BadNumStr)
---
Ok(32767)
---
Ok(-32768)
---
Err(BadNumStr)
---
Ok(4294967295)
---
Err(BadNumStr)
---
Ok(18446744073709551615)
---
Err(BadNumStr)
# PROBLEMS
NIL
