# META
~~~ini
description=abs method on all signed integer and float types
type=repl
~~~
# SOURCE
~~~roc
» I8.abs(-5)
» I8.abs(5)
» I8.abs(-127)
» I16.abs(-100)
» I16.abs(100)
» I16.abs(-32767)
» I32.abs(-42)
» I32.abs(42)
» I32.abs(-2147483647)
» I64.abs(-999)
» I64.abs(999)
» I64.abs(-9223372036854775807)
» I128.abs(-12345)
» I128.abs(12345)
» (-3.14).abs()
» (3.14).abs()
» (-2.718).abs()
» (2.718).abs()
» (-1.5).abs()
» (1.5).abs()
~~~
# OUTPUT
5
---
5
---
127
---
100
---
100
---
32767
---
42
---
42
---
2147483647
---
999
---
999
---
9223372036854775807
---
12345
---
12345
---
3.14
---
3.14
---
2.718
---
2.718
---
1.5
---
1.5
# PROBLEMS
NIL
