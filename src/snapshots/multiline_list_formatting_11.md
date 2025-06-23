# META
~~~ini
description=multiline_list_formatting (11)
type=expr
~~~
# SOURCE
~~~roc
[
	[1],
	[2],
	[
		3,
		4,
	],
	[5],
]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Newline(1:1-1:1),
OpenSquare(2:2-2:3),Int(2:3-2:4),CloseSquare(2:4-2:5),Comma(2:5-2:6),Newline(1:1-1:1),
OpenSquare(3:2-3:3),Int(3:3-3:4),CloseSquare(3:4-3:5),Comma(3:5-3:6),Newline(1:1-1:1),
OpenSquare(4:2-4:3),Newline(1:1-1:1),
Int(5:3-5:4),Comma(5:4-5:5),Newline(1:1-1:1),
Int(6:3-6:4),Comma(6:4-6:5),Newline(1:1-1:1),
CloseSquare(7:2-7:3),Comma(7:3-7:4),Newline(1:1-1:1),
OpenSquare(8:2-8:3),Int(8:3-8:4),CloseSquare(8:4-8:5),Comma(8:5-8:6),Newline(1:1-1:1),
CloseSquare(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(list (1:1-9:2)
	(list (2:2-2:5) (int (2:3-2:4) "1"))
	(list (3:2-3:5) (int (3:3-3:4) "2"))
	(list (4:2-7:3)
		(int (5:3-5:4) "3")
		(int (6:3-6:4) "4"))
	(list (8:2-8:5) (int (8:3-8:4) "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_list (1:1-9:2)
	(elem_var 35)
	(elems
		(e_list (2:2-2:5)
			(elem_var 15)
			(elems
				(e_int (2:3-2:4)
					(int_var 14)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "1"))))
		(e_list (3:2-3:5)
			(elem_var 20)
			(elems
				(e_int (3:3-3:4)
					(int_var 19)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "2"))))
		(e_list (4:2-7:3)
			(elem_var 28)
			(elems
				(e_int (5:3-5:4)
					(int_var 24)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "3"))
				(e_int (6:3-6:4)
					(int_var 27)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "4"))))
		(e_list (8:2-8:5)
			(elem_var 33)
			(elems
				(e_int (8:3-8:4)
					(int_var 32)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "5"))))))
~~~
# TYPES
~~~clojure
(expr 36 (type "List(List(Num(Int(*))))"))
~~~