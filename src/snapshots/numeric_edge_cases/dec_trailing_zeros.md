# META
~~~ini
description=Decimal with trailing zeros
type=expr
~~~
# SOURCE
~~~roc
1.2000
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-7 (raw "1.2000"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-7 (num-var 74) (numerator "12000") (denominator-power-of-ten "4") (value "1.2") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Frac(*)"))
~~~