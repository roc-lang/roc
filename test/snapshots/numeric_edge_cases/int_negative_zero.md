# META
~~~ini
description=Negative integer zero
type=expr
~~~
# SOURCE
~~~roc
-0
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
(e-int (raw "-0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "0"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
