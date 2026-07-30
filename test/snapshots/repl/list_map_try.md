# META
~~~ini
description=List.map_try maps until the first Err
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3].map_try(|n| Ok(n * 10))
» [1, 2, 3, 4].map_try(|n| if n < 3 { Ok(n) } else { Err(TooBig) })
» [1.I64, 2].map_try!(|n| Ok(n * 10))
» List.map_try([], |n| Ok(n))
» strings = ["a heap-allocated string long enough to not be stored inline"]
» List.map_try(strings, |s| Ok(s))
» strings
~~~
# OUTPUT
Ok([10.0, 20.0, 30.0])
---
Err(TooBig)
---
Ok([10, 20])
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
