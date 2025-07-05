# META
~~~ini
description=sneaky_implements_in_opaque_fn_type
type=expr
~~~
# SOURCE
~~~roc
N:=e->(implements)
I
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColonEqual(1:2-1:4),LowerIdent(1:4-1:5),OpArrow(1:5-1:7),NoSpaceOpenRound(1:7-1:8),KwImplements(1:8-1:18),CloseRound(1:18-1:19),Newline(1:1-1:1),
UpperIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "N"))
~~~
# FORMATTED
~~~roc
N
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "N"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[N]*"))
~~~
