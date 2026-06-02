# META
~~~ini
description=List.find_first_index returns Ok with the index of the first matching element
type=repl
~~~
# SOURCE
~~~roc
» List.find_first_index([10, 20, 30, 40], |x| x > 15)
~~~
# OUTPUT
Ok(1)
# PROBLEMS
NIL
