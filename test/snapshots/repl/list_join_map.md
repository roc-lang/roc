# META
~~~ini
description=List.join_map
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3].join_map(|n| [n, n * 10])
» [1, 2, 3].join_map(|n| if n == 2 { [] } else { [n] })
» [1, 2, 3].join_map(|_| [])
» [5].join_map(|n| [n, n, n])
» List.join_map([], |n| [n, n])
» words = ["alpha long enough to be heap-allocated instead of stored inline", "beta also long enough to require a heap allocation"]
» words.join_map(|w| [w, w])
» words
~~~
# OUTPUT
[1.0, 10.0, 2.0, 20.0, 3.0, 30.0]
---
[1.0, 3.0]
---
[]
---
[5.0, 5.0, 5.0]
---
[]
---
assigned `words`
---
["alpha long enough to be heap-allocated instead of stored inline", "alpha long enough to be heap-allocated instead of stored inline", "beta also long enough to require a heap allocation", "beta also long enough to require a heap allocation"]
---
["alpha long enough to be heap-allocated instead of stored inline", "beta also long enough to require a heap allocation"]
# PROBLEMS
NIL
