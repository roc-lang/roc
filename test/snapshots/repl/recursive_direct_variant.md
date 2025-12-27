# META
~~~ini
description=Recursive type with direct self-reference in tag variant should not crash
type=repl
~~~
# SOURCE
~~~roc
» Tree := [Text(Str), Wrapper(Tree)]
» inner : Tree = Text("hello")
» outer : Tree = Wrapper(inner)
» Str.inspect(outer)
~~~
# OUTPUT
defined type
---
Tree
---
Tree
---
Tree
# PROBLEMS
NIL
