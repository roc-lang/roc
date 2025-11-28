# META
~~~ini
description=Str.split_on should split a string on a delimiter
type=repl
~~~
# SOURCE
~~~roc
» List.len(Str.split_on("hello world", " "))
» List.len(Str.split_on("a,b,c", ","))
» List.len(Str.split_on("no match", "x"))
» List.len(Str.split_on("", ","))
~~~
# OUTPUT
2
---
3
---
1
---
1
# PROBLEMS
NIL
