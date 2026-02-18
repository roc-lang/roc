# META
~~~ini
description=List.fold with record accumulator - tests record state in fold
type=repl
~~~
# SOURCE
~~~roc
» List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
~~~
# OUTPUT
{ count: 3, sum: 6 }
# PROBLEMS
NIL
