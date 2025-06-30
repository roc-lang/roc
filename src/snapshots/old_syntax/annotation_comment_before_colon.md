# META
~~~ini
description=annotation_comment_before_colon
type=expr
~~~
# SOURCE
~~~roc
A
 e#g
:A
AA
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:2-2:3),Newline(2:4-2:5),
OpColon(3:1-3:2),UpperIdent(3:2-3:3),Newline(1:1-1:1),
UpperIdent(4:1-4:3),EndOfFile(4:3-4:3),
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
