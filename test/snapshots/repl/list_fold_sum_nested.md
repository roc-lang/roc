# META
~~~ini
description=List.len on List.fold result - tests nested function calls
type=repl
~~~
# SOURCE
~~~roc
» List.len(List.fold([1, 2, 3, 4, 5], [0], |acc, _| acc))
~~~
# OUTPUT
1
# PROBLEMS
NIL
