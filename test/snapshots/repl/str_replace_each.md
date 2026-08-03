# META
~~~ini
description=Str.replace_each replaces every occurrence of a substring
type=repl
~~~
# SOURCE
~~~roc
» "a,b,c,d".replace_each(",", " and ")
» "a,,b".replace_each(",", "-")
» "a,b".replace_each(",", "x,y")
» "abc".replace_each("x", "y")
» "abc".replace_each("", "X")
» source = "alpha, beta, gamma, delta — this is a long heap string that will not fit inline"
» Str.replace_each(source, ", ", " / ")
» source
~~~
# OUTPUT
"a and b and c and d"
---
"a--b"
---
"ax,yb"
---
"abc"
---
"abc"
---
assigned `source`
---
"alpha / beta / gamma / delta — this is a long heap string that will not fit inline"
---
"alpha, beta, gamma, delta — this is a long heap string that will not fit inline"
# PROBLEMS
NIL
