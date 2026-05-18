# META
~~~ini
description=List.replace returns the new list paired with the value that was replaced
type=repl
~~~
# SOURCE
~~~roc
» List.replace([10, 20, 30], 1, 99)
~~~
# OUTPUT
{ list: [10.0, 99.0, 30.0], value: 20.0 }
# PROBLEMS
NIL
