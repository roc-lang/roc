# META
~~~ini
description=Str.split_first splits a string on the first occurrence of a delimiter
type=repl
~~~
# SOURCE
~~~roc
» "foo: bar".split_first(":")
» "nomatch".split_first(":")
» "a:b:c".split_first(":")
» source = "key=value=tail — a long heap-allocated string that will not fit inline"
» Str.split_first(source, "=")
» source
~~~
# OUTPUT
Ok({ after: " bar", before: "foo" })
---
Err(NotFound)
---
Ok({ after: "b:c", before: "a" })
---
assigned `source`
---
Ok({ after: "value=tail — a long heap-allocated string that will not fit inline", before: "key" })
---
"key=value=tail — a long heap-allocated string that will not fit inline"
# PROBLEMS
NIL
