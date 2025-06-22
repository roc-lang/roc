# META
~~~ini
description=multiline_list_formatting (2)
type=expr
~~~
# SOURCE
~~~roc
[
	1,
	2,
	3,
]
~~~
# PROBLEMS
~~~txt
NIL
~~~
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Newline(1:1-1:1),
Int(2:2-2:3),Comma(2:3-2:4),Newline(1:1-1:1),
Int(3:2-3:3),Comma(3:3-3:4),Newline(1:1-1:1),
Int(4:2-4:3),Comma(4:3-4:4),Newline(1:1-1:1),
CloseSquare(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(list (1:1-5:2)
	(int (2:2-2:3) "1")
	(int (3:2-3:3) "2")
	(int (4:2-4:3) "3"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_list (1:1-5:2)
	(elem_var 21)
	(elems
		(e_int (2:2-2:3)
			(int_var 13)
			(precision_var 12)
			(literal "1")
			(value "TODO")
			(bound "u8"))
		(e_int (3:2-3:3)
			(int_var 16)
			(precision_var 15)
			(literal "2")
			(value "TODO")
			(bound "u8"))
		(e_int (4:2-4:3)
			(int_var 19)
			(precision_var 18)
			(literal "3")
			(value "TODO")
			(bound "u8"))))
~~~
# TYPES
~~~clojure
(expr 22 (type "List(Num(Int(*)))"))
~~~