# META
~~~ini
description=as_in_func_type_args
type=expr
~~~
# SOURCE
~~~roc
e:J
  as H->A
r
~~~
# EXPECTED
UNDEFINED VARIABLE - as_in_func_type_args.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
KwAs(2:3-2:5),UpperIdent(2:6-2:7),OpArrow(2:7-2:9),UpperIdent(2:9-2:10),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "e"))
~~~
# FORMATTED
~~~roc
e
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
