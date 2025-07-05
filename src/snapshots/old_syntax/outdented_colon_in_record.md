# META
~~~ini
description=outdented_colon_in_record
type=expr
~~~
# SOURCE
~~~roc
x = foo {
bar
:
blah
}
x
~~~
# EXPECTED
UNDEFINED VARIABLE - outdented_colon_in_record.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),LowerIdent(1:5-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:1-2:4),Newline(1:1-1:1),
OpColon(3:1-3:2),Newline(1:1-1:1),
LowerIdent(4:1-4:5),Newline(1:1-1:1),
CloseCurly(5:1-5:2),Newline(1:1-1:1),
LowerIdent(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "x"))
~~~
# FORMATTED
~~~roc
x
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
