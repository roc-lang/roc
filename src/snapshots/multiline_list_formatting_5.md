# META
~~~ini
description=multiline_list_formatting (5)
type=expr
~~~
# SOURCE
~~~roc
[1, 2, # Foo
  3]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),Int(1:5-1:6),Comma(1:6-1:7),Newline(1:9-1:13),
Int(2:3-2:4),CloseSquare(2:4-2:5),EndOfFile(2:5-2:5),
~~~
# PARSE
~~~clojure
(e-list @1-1-2-5
	(e-int @1-2-1-3 (raw "1"))
	(e-int @1-5-1-6 (raw "2"))
	(e-int @2-3-2-4 (raw "3")))
~~~
# FORMATTED
~~~roc
[
	1,
	2, # Foo
	3,
]
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-2-5 (elem-var 81) (id 82)
	(elems
		(e-int @1-2-1-3 (num-var 74) (sign-needed "false") (bits-needed "7") (value "1"))
		(e-int @1-5-1-6 (num-var 77) (sign-needed "false") (bits-needed "7") (value "2"))
		(e-int @2-3-2-4 (num-var 80) (sign-needed "false") (bits-needed "7") (value "3"))))
~~~
# TYPES
~~~clojure
(expr (id 82) (type "List(Num(Int(*)))"))
~~~