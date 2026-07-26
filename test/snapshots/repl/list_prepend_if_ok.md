# META
~~~ini
description=List.prepend_if_ok adds the Ok payload to the front of a list, or leaves it unchanged on Err
type=repl
~~~
# SOURCE
~~~roc
» List.prepend_if_ok([2, 3], Ok(1))
» List.prepend_if_ok([2, 3], Err(NotFound))
» list = ["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"]
» List.prepend_if_ok(list, Ok("one more heap-allocated string to exercise element refcounting"))
» list
~~~
# OUTPUT
[1.0, 2.0, 3.0]
---
[2.0, 3.0]
---
assigned `list`
---
["one more heap-allocated string to exercise element refcounting", "a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"]
---
["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"]
# PROBLEMS
NIL
