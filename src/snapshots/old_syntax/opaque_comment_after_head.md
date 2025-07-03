# META
~~~ini
description=opaque_comment_after_head
type=expr
~~~
# SOURCE
~~~roc
A#
 p:=a
A
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:3-1:3),
LowerIdent(2:2-2:3),OpColonEqual(2:3-2:5),LowerIdent(2:5-2:6),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "A"))
~~~
# FORMATTED
~~~roc
A
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "A") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[A]a"))
~~~
