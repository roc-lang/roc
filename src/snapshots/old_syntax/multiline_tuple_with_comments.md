# META
~~~ini
description=multiline_tuple_with_comments
type=expr
~~~
# SOURCE
~~~roc
(
    #before 1
    1
    #after 1
    ,
    #before 2
    2
    #after 2
    ,
    #before 3
    3
    # after 3
)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
Newline(2:6-2:14),
Int(3:5-3:6),Newline(1:1-1:1),
Newline(4:6-4:13),
Comma(5:5-5:6),Newline(1:1-1:1),
Newline(6:6-6:14),
Int(7:5-7:6),Newline(1:1-1:1),
Newline(8:6-8:13),
Comma(9:5-9:6),Newline(1:1-1:1),
Newline(10:6-10:14),
Int(11:5-11:6),Newline(1:1-1:1),
Newline(12:6-12:14),
CloseRound(13:1-13:2),EndOfFile(13:2-13:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-13.2
	(e-int @3.5-3.6 (raw "1"))
	(e-int @7.5-7.6 (raw "2"))
	(e-int @11.5-11.6 (raw "3")))
~~~
# FORMATTED
~~~roc
(
	# before 1
	1,
	# before 2
	2,
	# before 3
	3,
	# after 3
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-13.2
	(elems
		(e-int @3.5-3.6 (value "1"))
		(e-int @7.5-7.6 (value "2"))
		(e-int @11.5-11.6 (value "3"))))
~~~
# TYPES
~~~clojure
(expr @1.1-13.2 (type "(Num(*), Num(*), Num(*))"))
~~~
