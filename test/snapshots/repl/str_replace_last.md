# META
~~~ini
description=Str.replace_last replaces only the last occurrence of a substring
type=repl
~~~
# SOURCE
~~~roc
» "a,b,a".replace_last(",", " and ")
» "abc".replace_last("x", "y")
» "abc".replace_last("", "X")
» source = "alpha, beta, gamma, delta — this is a long heap string that will not fit inline"
» Str.replace_last(source, ", ", " / ")
» source
~~~
# OUTPUT
"a,b and a"
---
"abc"
---
"abc"
---
assigned `source`
---
"alpha, beta, gamma / delta — this is a long heap string that will not fit inline"
---
"alpha, beta, gamma, delta — this is a long heap string that will not fit inline"
# PROBLEMS
NIL
