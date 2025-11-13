# META
~~~ini
description=Str.concat should work with empty and non-empty strings
type=repl
~~~
# SOURCE
~~~roc
» Str.concat("", "")
» Str.concat("", "a")
» Str.concat("foo", "")
» Str.concat("foo", "bar")
~~~
# OUTPUT
""
---
"a"
---
"foo"
---
"foobar"
# PROBLEMS
NIL
