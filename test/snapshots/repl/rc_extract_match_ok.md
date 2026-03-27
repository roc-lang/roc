# META
~~~ini
description=RC: extracting match on Ok Result (payload bound to s)
type=repl
~~~
# SOURCE
~~~roc
» match Str.from_utf8([72, 105]) { Ok(s) => s, Err(_) => "fail" }
~~~
# OUTPUT
"Hi"
# PROBLEMS
NIL
