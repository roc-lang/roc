# META
~~~ini
description=Str.replace_first replaces only the first occurrence of a substring
type=repl
~~~
# SOURCE
~~~roc
» "a,b,a".replace_first(",", " and ")
» "abc".replace_first("x", "y")
» "abc".replace_first("", "X")
» source = "alpha, beta, gamma, delta — this is a long heap string that will not fit inline"
» Str.replace_first(source, ", ", " / ")
» source
~~~
# OUTPUT
"a and b,a"
---
"abc"
---
"abc"
---
assigned `source`
---
"alpha / beta, gamma, delta — this is a long heap string that will not fit inline"
---
"alpha, beta, gamma, delta — this is a long heap string that will not fit inline"
# PROBLEMS
NIL
