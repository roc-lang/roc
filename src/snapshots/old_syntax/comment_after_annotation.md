# META
~~~ini
description=comment_after_annotation
type=expr
~~~
# SOURCE
~~~roc
F:e#


q
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:5-1:5),
Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "F"))
~~~
# FORMATTED
~~~roc
F
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (ext-var 73) (name "F") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[F]*"))
~~~
