# META
~~~ini
description=from_str for various numeric types (F32, I64, U128, I128)
type=repl
~~~
# SOURCE
~~~roc
» F32.from_str("3.14")
» F32.from_str("invalid")
» I64.from_str("-9223372036854775808")
» I64.from_str("9223372036854775807")
» I64.from_str("9223372036854775808")
» U128.from_str("0")
» U128.from_str("12345678901234567890")
» I128.from_str("-12345678901234567890")
» I128.from_str("12345678901234567890")
~~~
# OUTPUT
Ok(3.140000104904175)
---
Err(BadNumStr)
---
Ok(-9223372036854775808)
---
Ok(9223372036854775807)
---
Err(BadNumStr)
---
Ok(0)
---
Ok(12345678901234567890)
---
Ok(-12345678901234567890)
---
Ok(12345678901234567890)
# PROBLEMS
NIL
