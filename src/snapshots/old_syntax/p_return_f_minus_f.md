# META
~~~ini
description=p_return_f_minus_f
type=expr
~~~
# SOURCE
~~~roc
p
return#
 f
 -f
~~~
# EXPECTED
UNDEFINED VARIABLE - p_return_f_minus_f.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
KwReturn(2:1-2:7),Newline(2:8-2:8),
LowerIdent(3:2-3:3),Newline(1:1-1:1),
OpUnaryMinus(4:2-4:3),LowerIdent(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "p"))
~~~
# FORMATTED
~~~roc
p
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
