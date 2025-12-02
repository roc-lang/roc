# META
~~~ini
description=abs_diff method on all numeric types
type=repl
~~~
# SOURCE
~~~roc
» U8.abs_diff(10, 3)
» U8.abs_diff(3, 10)
» U16.abs_diff(1000, 500)
» U16.abs_diff(500, 1000)
» U32.abs_diff(100000, 25000)
» U32.abs_diff(25000, 100000)
» U64.abs_diff(1000000, 250000)
» U64.abs_diff(250000, 1000000)
» U128.abs_diff(999999999, 111111111)
» U128.abs_diff(111111111, 999999999)
» I8.abs_diff(-5, 10)
» I8.abs_diff(10, -5)
» I16.abs_diff(-100, 50)
» I16.abs_diff(50, -100)
» I32.abs_diff(-1000, 500)
» I32.abs_diff(500, -1000)
» I64.abs_diff(-10000, 5000)
» I64.abs_diff(5000, -10000)
» I128.abs_diff(-100000, 50000)
» I128.abs_diff(50000, -100000)
» (3.5).abs_diff(1.2)
» (1.2).abs_diff(3.5)
» (5.5).abs_diff(2.0)
» (2.0).abs_diff(5.5)
~~~
# OUTPUT
7
---
7
---
500
---
500
---
75000
---
75000
---
750000
---
750000
---
888888888
---
888888888
---
15
---
15
---
150
---
150
---
1500
---
1500
---
15000
---
15000
---
150000
---
150000
---
2.3
---
2.3
---
3.5
---
3.5
# PROBLEMS
NIL
