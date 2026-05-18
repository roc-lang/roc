# META
~~~ini
description=List.find_first_index returns Err(NotFound) when no element matches the predicate
type=repl
~~~
# SOURCE
~~~roc
» List.find_first_index([1, 2, 3], |x| x > 99)
~~~
# OUTPUT
Err(NotFound)
# PROBLEMS
NIL
