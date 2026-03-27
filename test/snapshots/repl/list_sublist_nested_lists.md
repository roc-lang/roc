# META
~~~ini
description=List.sublist returns a continuous subsection of the original list
type=repl
~~~
# SOURCE
~~~roc
» List.sublist([[0, 1], [2, 3], [4, 5], [6, 7], [8, 9]], {start: 2, len: 9})
~~~
# OUTPUT
[[4.0, 5.0], [6.0, 7.0], [8.0, 9.0]]
# PROBLEMS
NIL
