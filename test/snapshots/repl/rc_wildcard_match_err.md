# META
~~~ini
description=RC: wildcard match on Err Result (complex payload layout)
type=repl
~~~
# SOURCE
~~~roc
» match Str.from_utf8([255]) { Ok(_) => "fail", Err(_) => "got error" }
~~~
# OUTPUT
"got error"
# PROBLEMS
NIL
