# META
~~~ini
description=value_def_confusion
type=expr
~~~
# SOURCE
~~~roc
a:F
F
:h
abc
~~~
# EXPECTED
UNDEFINED VARIABLE - value_def_confusion.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
UpperIdent(2:1-2:2),Newline(1:1-1:1),
OpColon(3:1-3:2),LowerIdent(3:2-3:3),Newline(1:1-1:1),
LowerIdent(4:1-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "a"))
~~~
# FORMATTED
~~~roc
a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
