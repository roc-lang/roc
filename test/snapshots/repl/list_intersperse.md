# META
~~~ini
description=List.intersperse
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3].intersperse(0)
» [42].intersperse(0)
» List.intersperse([], 0)
» words = ["a string long enough to be heap-allocated instead of stored inline", "another heap-allocated string in the source list"]
» sep = "a heap-allocated separator that gets duplicated between the elements"
» words.intersperse(sep)
» words
» sep
~~~
# OUTPUT
[1.0, 0.0, 2.0, 0.0, 3.0]
---
[42.0]
---
[]
---
assigned `words`
---
assigned `sep`
---
["a string long enough to be heap-allocated instead of stored inline", "a heap-allocated separator that gets duplicated between the elements", "another heap-allocated string in the source list"]
---
["a string long enough to be heap-allocated instead of stored inline", "another heap-allocated string in the source list"]
---
"a heap-allocated separator that gets duplicated between the elements"
# PROBLEMS
NIL
