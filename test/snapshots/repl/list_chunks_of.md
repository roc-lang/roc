# META
~~~ini
description=List.chunks_of
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3, 4, 5].chunks_of(2)
» [1, 2, 3].chunks_of(10)
» [1, 2, 3].chunks_of(1)
» [1, 2, 3].chunks_of(0)
» List.chunks_of([], 2)
» [1, 2, 3, 4].chunks_of(2)
» [1, 2, 3].chunks_of(18446744073709551615)
» words = ["a string long enough to be heap-allocated instead of stored inline", "another heap-allocated string in the source", "a third heap-allocated string to fill out the chunks", "a fourth heap-allocated string in the source list"]
» words.chunks_of(2)
» words
~~~
# OUTPUT
[[1.0, 2.0], [3.0, 4.0], [5.0]]
---
[[1.0, 2.0, 3.0]]
---
[[1.0], [2.0], [3.0]]
---
[]
---
[]
---
[[1.0, 2.0], [3.0, 4.0]]
---
[[1.0, 2.0, 3.0]]
---
assigned `words`
---
[["a string long enough to be heap-allocated instead of stored inline", "another heap-allocated string in the source"], ["a third heap-allocated string to fill out the chunks", "a fourth heap-allocated string in the source list"]]
---
["a string long enough to be heap-allocated instead of stored inline", "another heap-allocated string in the source", "a third heap-allocated string to fill out the chunks", "a fourth heap-allocated string in the source list"]
# PROBLEMS
NIL
