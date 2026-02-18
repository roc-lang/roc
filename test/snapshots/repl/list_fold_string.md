# META
~~~ini
description=List.fold with string accumulator should concatenate correctly
type=repl
~~~
# SOURCE
~~~roc
» ["a", "b", "c"].fold("", |acc, s| Str.concat(acc, s))
~~~
# OUTPUT
"abc"
# PROBLEMS
NIL
