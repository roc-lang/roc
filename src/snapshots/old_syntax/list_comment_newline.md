# META
~~~ini
description=list_comment_newline
type=expr
~~~
# SOURCE
~~~roc
[L#
,

]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),UpperIdent(1:2-1:3),Newline(1:4-1:4),
Comma(2:1-2:2),Newline(1:1-1:1),
Newline(1:1-1:1),
CloseSquare(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-list @1.1-4.2
	(e-tag @1.2-1.3 (raw "L")))
~~~
# FORMATTED
~~~roc
[
	L,

]
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-4.2 (elem-var 74) (id 75)
	(elems
		(e-tag @1.2-1.3 (ext-var 73) (name "L") (args "TODO"))))
~~~
# TYPES
~~~clojure
(expr (id 75) (type "List([L]*)"))
~~~
