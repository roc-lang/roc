# META
~~~ini
description=Mono test: integer literal
type=mono
~~~
# SOURCE
~~~roc
42
~~~
# MONO
~~~roc
42
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "42"))
~~~
# CANONICALIZE
~~~clojure
(e-num (value "42"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
