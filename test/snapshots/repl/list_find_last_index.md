# META
~~~ini
description=List.find_last_index returns Ok with the index of the last matching element
type=repl
~~~
# SOURCE
~~~roc
» List.find_last_index([10, 20, 30, 40], |x| x < 35)
~~~
# OUTPUT
Ok(2)
# PROBLEMS
NIL
