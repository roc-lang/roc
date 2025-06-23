# META
~~~ini
description=Nested list literals
type=expr
~~~
# SOURCE
~~~roc
[[1, 2], [3, 4], [5]]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),OpenSquare(1:2-1:3),Int(1:3-1:4),Comma(1:4-1:5),Int(1:6-1:7),CloseSquare(1:7-1:8),Comma(1:8-1:9),OpenSquare(1:10-1:11),Int(1:11-1:12),Comma(1:12-1:13),Int(1:14-1:15),CloseSquare(1:15-1:16),Comma(1:16-1:17),OpenSquare(1:18-1:19),Int(1:19-1:20),CloseSquare(1:20-1:21),CloseSquare(1:21-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(list (1:1-1:22)
	(list (1:2-1:8)
		(int (1:3-1:4) "1")
		(int (1:6-1:7) "2"))
	(list (1:10-1:16)
		(int (1:11-1:12) "3")
		(int (1:14-1:15) "4"))
	(list (1:18-1:21) (int (1:19-1:20) "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_list (1:1-1:22)
	(elem_var 33)
	(elems
		(e_list (1:2-1:8)
			(elem_var 18)
			(elems
				(e_int (1:3-1:4)
					(int_var 14)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "1"))
				(e_int (1:6-1:7)
					(int_var 17)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "2"))))
		(e_list (1:10-1:16)
			(elem_var 26)
			(elems
				(e_int (1:11-1:12)
					(int_var 22)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "3"))
				(e_int (1:14-1:15)
					(int_var 25)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "4"))))
		(e_list (1:18-1:21)
			(elem_var 31)
			(elems
				(e_int (1:19-1:20)
					(int_var 30)
					(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
					(value "5"))))))
~~~
# TYPES
~~~clojure
(expr 34 (type "List(List(Num(Int(*))))"))
~~~