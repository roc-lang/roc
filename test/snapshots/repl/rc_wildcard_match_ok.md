# META
~~~ini
description=RC: wildcard match on Ok Result (no payload binding)
type=repl
~~~
# SOURCE
~~~roc
» match Str.from_utf8([72, 105]) { Ok(_) => "matched", Err(_) => "fail" }
~~~
# OUTPUT
"matched"
# PROBLEMS
NIL
