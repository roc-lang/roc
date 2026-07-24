# META
~~~ini
description=List.keep_errs keeps the Err values
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3, 4].keep_errs(|n| if n.is_even() { Ok(n) } else { Err(n) })
» [1.I64, 2, 3].keep_errs(|n| Ok(n))
» mixed = ["a long heap-allocated string number one that will not fit inline", "a long heap-allocated string number two that will not fit inline"]
» List.keep_errs(mixed, |s| if s.starts_with("a long heap-allocated string number one") { Ok(s) } else { Err(s) })
» mixed
~~~
# OUTPUT
[1, 3]
---
[]
---
assigned `mixed`
---
["a long heap-allocated string number two that will not fit inline"]
---
["a long heap-allocated string number one that will not fit inline", "a long heap-allocated string number two that will not fit inline"]
# PROBLEMS
NIL
