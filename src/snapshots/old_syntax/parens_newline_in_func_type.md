# META
~~~ini
description=parens_newline_in_func_type
type=expr
~~~
# SOURCE
~~~roc
C:(
h)->a
C
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),OpArrow(2:3-2:5),LowerIdent(2:5-2:6),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "C"))
~~~
# FORMATTED
~~~roc
C
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (ext-var 73) (name "C") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[C]*"))
~~~
