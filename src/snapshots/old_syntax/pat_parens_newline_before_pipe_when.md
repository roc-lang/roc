# META
~~~ini
description=pat_parens_newline_before_pipe_when
type=expr
~~~
# SOURCE
~~~roc
when 0
is S#
 (H
)|B->e
~~~
# EXPECTED
UNDEFINED VARIABLE - pat_parens_newline_before_pipe_when.md:1:1:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),Newline(1:1-1:1),
LowerIdent(2:1-2:3),UpperIdent(2:4-2:5),Newline(2:6-2:6),
OpenRound(3:2-3:3),UpperIdent(3:3-3:4),Newline(1:1-1:1),
CloseRound(4:1-4:2),OpBar(4:2-4:3),UpperIdent(4:3-4:4),OpArrow(4:4-4:6),LowerIdent(4:6-4:7),EndOfFile(4:7-4:7),
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
