# META
~~~ini
description=unary_negation_arg
type=expr
~~~
# SOURCE
~~~roc
whee  12 -foo
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_negation_arg.md:1:1:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:7-1:9),OpUnaryMinus(1:10-1:11),LowerIdent(1:11-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "whee"))
~~~
# FORMATTED
~~~roc
whee
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
