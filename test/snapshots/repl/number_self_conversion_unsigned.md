# META
~~~ini
description=Unsigned `to_u<n>` - a noop that returns the value
type=repl
~~~
# SOURCE
~~~roc
» U8.to_u8(U8.highest) == U8.highest
» U16.to_u16(U16.highest) == U16.highest
» U32.to_u32(U32.highest) == U32.highest
» U64.to_u64(U64.highest) == U64.highest
» U128.to_u128(U128.highest) == U128.highest
» U8.to_u8_wrap(U8.highest) == U8.highest
» U16.to_u16_wrap(U16.highest) == U16.highest
» U32.to_u32_wrap(U32.highest) == U32.highest
» U64.to_u64_wrap(U64.highest) == U64.highest
» U128.to_u128_wrap(U128.highest) == U128.highest
» U8.to_u8_try(U8.highest) == Ok(U8.highest)
» U16.to_u16_try(U16.highest) == Ok(U16.highest)
» U32.to_u32_try(U32.highest) == Ok(U32.highest)
» U64.to_u64_try(U64.highest) == Ok(U64.highest)
» U128.to_u128_try(U128.highest) == Ok(U128.highest)
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
