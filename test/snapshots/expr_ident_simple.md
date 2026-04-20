# META
~~~ini
description=Simple identifier lookup canonicalization
type=expr
~~~
# SOURCE
~~~roc
foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-ident (raw "foo"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
