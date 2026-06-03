# META
~~~ini
description=List.find_first returns Ok with the first matching element
type=repl
~~~
# SOURCE
~~~roc
» List.find_first([1, 2, 3, 4], |x| x > 2)
~~~
# OUTPUT
Ok(3.0)
# PROBLEMS
NIL
