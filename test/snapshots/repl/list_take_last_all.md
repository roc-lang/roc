# META
~~~ini
description=List.take_last returns entire list when requesting more elements than in original list
type=repl
~~~
# SOURCE
~~~roc
» List.take_last([0, 1, 2, 3, 4, 5, 6], 200)
~~~
# OUTPUT
[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
# PROBLEMS
NIL
