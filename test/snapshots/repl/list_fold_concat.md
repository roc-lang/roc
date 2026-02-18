# META
~~~ini
description=List.fold with concat - tests nested function calls
type=repl
~~~
# SOURCE
~~~roc
» List.len(List.fold([1, 2, 3], [], |acc, x| List.concat(acc, [x])))
~~~
# OUTPUT
3
# PROBLEMS
NIL
