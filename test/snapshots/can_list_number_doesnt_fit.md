# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1.U8, 2.U8, 300]
~~~
# EXPECTED
INVALID NUMBER - can_list_number_doesnt_fit.md:1:14:1:17
# PROBLEMS

┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  [1.U8, 2.U8, 300]                                                         │
 │               ‾‾‾                                                          │
 └──────────────────────────────────────── can_list_number_doesnt_fit.md:1:14 ┘

    The inferred type is:

        U8

# TOKENS
~~~zig
OpenSquare,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-typed-int (raw "1") (type "U8"))
	(e-typed-int (raw "2") (type "U8"))
	(e-int (raw "300")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-typed-int (value "1") (type "U8"))
		(e-typed-int (value "2") (type "U8"))
		(e-num (value "300"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Error)"))
~~~
