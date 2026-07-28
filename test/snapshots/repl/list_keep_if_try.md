# META
~~~ini
description=List.keep_if_try filters with a fallible predicate
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3, 4].keep_if_try(|n| Ok(n > 2))
» [1, 2, 3].keep_if_try(|n| if n < 3 { Ok(n.is_even()) } else { Err(Stop) })
» [1.I64, 2, 3, 4].keep_if_try!(|n| Ok(n > 2))
» List.keep_if_try([], |_| Ok(Bool.True))
» strings = ["a heap-allocated string long enough to not be stored inline"]
» List.keep_if_try(strings, |_| Ok(Bool.True))
» strings
~~~
# OUTPUT
Ok([3.0, 4.0])
---
Err(Stop)
---
Ok([3, 4])
---
Ok([])
---
assigned `strings`
---
Ok(["a heap-allocated string long enough to not be stored inline"])
---
["a heap-allocated string long enough to not be stored inline"]
# PROBLEMS
NIL
