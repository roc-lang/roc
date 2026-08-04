# META
~~~ini
description=Str.split_last splits a string on the last occurrence of a delimiter
type=repl
~~~
# SOURCE
~~~roc
» "a:b:c".split_last(":")
» "a:b:".split_last(":")
» "nomatch".split_last(":")
» source = "key=value=tail — a long heap-allocated string that will not fit inline"
» Str.split_last(source, "=")
» source
~~~
# OUTPUT
Ok({ after: "c", before: "a:b" })
---
Ok({ after: "", before: "a:b" })
---
Err(NotFound)
---
assigned `source`
---
Ok({ after: "tail — a long heap-allocated string that will not fit inline", before: "key=value" })
---
"key=value=tail — a long heap-allocated string that will not fit inline"
# PROBLEMS
NIL
