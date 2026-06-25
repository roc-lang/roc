# META
~~~ini
description=List.find_last returns Err(NotFound) when no element matches the predicate
type=repl
~~~
# SOURCE
~~~roc
» List.find_last([1, 2, 3], |x| x > 99)
~~~
# OUTPUT
Err(NotFound)
# PROBLEMS
NIL
