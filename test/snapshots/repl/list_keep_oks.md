# META
~~~ini
description=List.keep_oks keeps the Ok values
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3, 4].keep_oks(|n| if n.is_even() { Ok(n) } else { Err({}) })
» [1.I64, 2, 3].keep_oks(|_| Err({}))
» mixed = ["a long heap-allocated string number one that will not fit inline", "a long heap-allocated string number two that will not fit inline"]
» List.keep_oks(mixed, |s| if s.starts_with("a long heap-allocated string number one") { Ok(s) } else { Err({}) })
» mixed
~~~
# OUTPUT
[2, 4]
---
[]
---
assigned `mixed`
---
["a long heap-allocated string number one that will not fit inline"]
---
["a long heap-allocated string number one that will not fit inline", "a long heap-allocated string number two that will not fit inline"]
# PROBLEMS
NIL
