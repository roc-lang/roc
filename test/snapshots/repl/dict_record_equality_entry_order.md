# META
~~~ini
description=Record fields use Dict equality independent of entry order (https://github.com/roc-lang/roc/issues/10049)
type=repl
~~~
# SOURCE
~~~roc
» { owes: Dict.from_list([("Bob", 3.1), ("John", 4.2)]) } == { owes: Dict.from_list([("John", 4.2), ("Bob", 3.1)]) }
~~~
# OUTPUT
True
# PROBLEMS
NIL
