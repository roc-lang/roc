# META
~~~ini
description=String comparison edge cases and longer strings
type=repl
~~~
# SOURCE
~~~roc
» longStr1 = "This is a very long string that should test the comparison of larger strings that might not fit in small string optimization"
» longStr2 = "This is a very long string that should test the comparison of larger strings that might not fit in small string optimization"
» longStr3 = "This is a very long string that should test the comparison of larger strings that might not fit in small string optimizatioN"
» longStr1 == longStr2
» longStr1 == longStr3
» whitespace1 = "hello "
» whitespace2 = "hello"
» whitespace1 == whitespace2
» specialChars1 = "hello\nworld"
» specialChars2 = "hello\nworld"
» specialChars1 == specialChars2
~~~
# OUTPUT
assigned `longStr1`
---
assigned `longStr2`
---
assigned `longStr3`
---
True
---
False
---
assigned `whitespace1`
---
assigned `whitespace2`
---
False
---
assigned `specialChars1`
---
assigned `specialChars2`
---
True
# PROBLEMS
NIL
