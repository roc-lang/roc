# META
~~~ini
description=Signed `to_i<n>` - a noop that returns the value
type=repl
~~~
# SOURCE
~~~roc
» I8.to_i8(I8.highest) == I8.highest
» I16.to_i16(I16.highest) == I16.highest
» I32.to_i32(I32.highest) == I32.highest
» I64.to_i64(I64.highest) == I64.highest
» I128.to_i128(I128.highest) == I128.highest
» I8.to_i8_wrap(I8.highest) == I8.highest
» I16.to_i16_wrap(I16.highest) == I16.highest
» I32.to_i32_wrap(I32.highest) == I32.highest
» I64.to_i64_wrap(I64.highest) == I64.highest
» I128.to_i128_wrap(I128.highest) == I128.highest
» I8.to_i8_try(I8.highest) == Ok(I8.highest)
» I16.to_i16_try(I16.highest) == Ok(I16.highest)
» I32.to_i32_try(I32.highest) == Ok(I32.highest)
» I64.to_i64_try(I64.highest) == Ok(I64.highest)
» I128.to_i128_try(I128.highest) == Ok(I128.highest)
~~~
# OUTPUT
True
---
True
---
True
---
True
---
True
---
True
---
True
---
True
---
True
---
True
---
True
---
True
---
True
---
True
---
True
# PROBLEMS
NIL
