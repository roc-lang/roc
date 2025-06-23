# META
~~~ini
description=List with integer literals
type=expr
~~~
# SOURCE
~~~roc
[1, 2, 3]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),Int(1:5-1:6),Comma(1:6-1:7),Int(1:8-1:9),CloseSquare(1:9-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(list (1:1-1:10)
	(int (1:2-1:3) "1")
	(int (1:5-1:6) "2")
	(int (1:8-1:9) "3"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_list (1:1-1:10)
	(elem_var 81)
	(elems
		(e_int (1:2-1:3)
			(int_var 73)
			(precision_var 72)
			(literal "1")
			(value "TODO")
			(bound "u8"))
		(e_int (1:5-1:6)
			(int_var 76)
			(precision_var 75)
			(literal "2")
			(value "TODO")
			(bound "u8"))
		(e_int (1:8-1:9)
			(int_var 79)
			(precision_var 78)
			(literal "3")
			(value "TODO")
			(bound "u8"))))
~~~
# TYPES
~~~clojure
(expr 82 (type "List(Num(Int(*)))"))
~~~