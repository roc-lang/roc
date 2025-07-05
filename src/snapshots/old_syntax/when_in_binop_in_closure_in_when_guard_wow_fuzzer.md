# META
~~~ini
description=when_in_binop_in_closure_in_when_guard_wow_fuzzer
type=expr
~~~
# SOURCE
~~~roc
when f
is
3 if\t->m%when f
is z->e
 z->m
~~~
# EXPECTED
UNDEFINED VARIABLE - when_in_binop_in_closure_in_when_guard_wow_fuzzer.md:1:1:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),Newline(1:1-1:1),
LowerIdent(2:1-2:3),Newline(1:1-1:1),
Int(3:1-3:2),KwIf(3:3-3:5),OpBackslash(3:5-3:6),LowerIdent(3:6-3:7),OpArrow(3:7-3:9),LowerIdent(3:9-3:10),OpPercent(3:10-3:11),LowerIdent(3:11-3:15),LowerIdent(3:16-3:17),Newline(1:1-1:1),
LowerIdent(4:1-4:3),LowerIdent(4:4-4:5),OpArrow(4:5-4:7),LowerIdent(4:7-4:8),Newline(1:1-1:1),
LowerIdent(5:2-5:3),OpArrow(5:3-5:5),LowerIdent(5:5-5:6),EndOfFile(5:6-5:6),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
