# META
~~~ini
description=List.find_last returns Ok with the last matching element
type=repl
~~~
# SOURCE
~~~roc
» List.find_last([1, 2, 3, 4], |x| x < 4)
~~~
# OUTPUT
Ok(3.0)
# PROBLEMS
NIL
