# META
~~~ini
description=Test typed integer with undeclared type
type=expr
~~~
# SOURCE
~~~roc
42:NonexistentType
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "42"))
~~~
# FORMATTED
~~~roc
42
~~~
# CANONICALIZE
~~~clojure
(e-num (value "42"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
