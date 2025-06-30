# META
~~~ini
description=annotation_apply_newlines
type=expr
~~~
# SOURCE
~~~roc
A
 p:
e
A
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:2-2:3),OpColon(2:3-2:4),Newline(1:1-1:1),
LowerIdent(3:1-3:2),Newline(1:1-1:1),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
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
(e-tag @1.1-1.2 (ext-var 73) (name "A") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[A]*"))
~~~
