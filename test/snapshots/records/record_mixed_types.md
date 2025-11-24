# META
~~~ini
description=Record with mixed field types
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, active: True, scores: [95, 87, 92], balance: 1250.75 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,LowerIdent,OpColon,Float,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name")
		(e-string
			(e-string-part (raw "Alice"))))
	(field (field "age")
		(e-int (raw "30")))
	(field (field "active")
		(e-tag (raw "True")))
	(field (field "scores")
		(e-list
			(e-int (raw "95"))
			(e-int (raw "87"))
			(e-int (raw "92"))))
	(field (field "balance")
		(e-frac (raw "1250.75"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "name")
			(e-string
				(e-literal (string "Alice"))))
		(field (name "age")
			(e-num (value "30")))
		(field (name "active")
			(e-tag (name "True")))
		(field (name "scores")
			(e-list
				(elems
					(e-num (value "95"))
					(e-num (value "87"))
					(e-num (value "92")))))
		(field (name "balance")
			(e-frac-dec (value "1250.75")))))
~~~
# TYPES
~~~clojure
(expr (type "{ active: [True]_others, age: a, balance: b, name: Str, scores: List(c) } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
~~~
