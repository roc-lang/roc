# META
~~~ini
description=List.sublist returns a continuous subsection of the original list
type=repl
~~~
# SOURCE
~~~roc
» List.sublist(["abcdefghijklmnopqrstuvwxyz", "a long, heap allocated str", "0123456789.1011121314151617", "abcdefghijklmnopqrstuvwxyz", "a long, heap allocated str", "0123456789.1011121314151617", "abcdefghijklmnopqrstuvwxyz", "a long, heap allocated str", "0123456789.1011121314151617"], {start: 2, len: 3})
~~~
# OUTPUT
["0123456789.1011121314151617", "abcdefghijklmnopqrstuvwxyz", "a long, heap allocated str"]
# PROBLEMS
NIL
