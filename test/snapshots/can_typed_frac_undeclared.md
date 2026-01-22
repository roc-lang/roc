# META
~~~ini
description=Test typed fractional with undeclared type
type=expr
~~~
# SOURCE
~~~roc
3.14:UnknownType
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "3.14"))
~~~
# FORMATTED
~~~roc
3.14
~~~
# CANONICALIZE
~~~clojure
(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
