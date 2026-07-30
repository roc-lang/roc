# META
~~~ini
description=round_to/floor_to/ceiling_to _try variants return Ok in range and Err(OutOfRange) otherwise, across Dec/F32/F64
type=repl
~~~
# SOURCE
~~~roc
» Dec.round_to_i32_try(3.5)
» F32.floor_to_i16_try(-2.7)
» F64.ceiling_to_i8_try(3.2)
» F64.round_to_u8_try(-1.0)
» Dec.ceiling_to_u16_try(-2.0)
» F32.floor_to_i8_try(1000.0)
» F64.round_to_u8_try(999.0)
» F64.round_to_i8_try(F64.nan)
» F32.round_to_i8_try(F32.infinity)
~~~
# OUTPUT
Ok(4)
---
Ok(-3)
---
Ok(4)
---
Err(OutOfRange)
---
Err(OutOfRange)
---
Err(OutOfRange)
---
Err(OutOfRange)
---
Err(OutOfRange)
---
Err(OutOfRange)
# PROBLEMS
NIL
