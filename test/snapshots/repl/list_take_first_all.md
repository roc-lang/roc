# META
~~~ini
description=List.take_first returns entire list when more elements requested than in original list
type=repl
~~~
# SOURCE
~~~roc
» List.take_first([0, 1, 2, 3, 4, 5, 6], 300)
~~~
# OUTPUT
[0, 1, 2, 3, 4, 5, 6]
# PROBLEMS
NIL
