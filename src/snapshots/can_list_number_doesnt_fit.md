# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1u8, 2u8, 300]
~~~
# EXPECTED
invalid_num_literal - can_list_number_doesnt_fit.md:1:2:1:5
invalid_num_literal - can_list_number_doesnt_fit.md:1:7:1:10
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:5),Comma(1:5-1:6),Int(1:7-1:10),Comma(1:10-1:11),Int(1:12-1:15),CloseSquare(1:15-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.16
	(e-int @1.2-1.5 (raw "1u8"))
	(e-int @1.7-1.10 (raw "2u8"))
	(e-int @1.12-1.15 (raw "300")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.16
	(elems
		(e-runtime-error (tag "invalid_num_literal"))
		(e-runtime-error (tag "invalid_num_literal"))
		(e-int @1.12-1.15 (value "300"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.16 (type "List(Error)"))
~~~
