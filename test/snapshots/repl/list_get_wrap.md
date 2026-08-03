# META
~~~ini
description=List.get_wrap indexes into a list with the index wrapping modulo the length
type=repl
~~~
# SOURCE
~~~roc
» ["a", "b", "c", "d"].get_wrap(5)
» ["a", "b", "c", "d"].get_wrap(1)
» ["a", "b"].take_first(0).get_wrap(0)
» words = ["one long heap-allocated string that will not fit inline", "two long heap-allocated string that will not fit inline"]
» words.get_wrap(3)
» words
~~~
# OUTPUT
Ok("b")
---
Ok("b")
---
Err(ListWasEmpty)
---
assigned `words`
---
Ok("two long heap-allocated string that will not fit inline")
---
["one long heap-allocated string that will not fit inline", "two long heap-allocated string that will not fit inline"]
# PROBLEMS
NIL
