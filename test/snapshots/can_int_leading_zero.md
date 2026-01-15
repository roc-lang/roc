# META
~~~ini
description=Test integer with leading zero (no octal prefix)
type=expr
~~~
# SOURCE
~~~roc
012
~~~
# EXPECTED
LEADING ZERO - :0:0:0:0
# PROBLEMS
**LEADING ZERO**
Numbers cannot have leading zeros.



# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "012"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "12"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
