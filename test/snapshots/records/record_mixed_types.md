# META
~~~ini
description=Record with mixed field types
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
~~~
# EXPECTED
DOES NOT EXIST - record_mixed_types.md:1:35:1:44
# PROBLEMS
**DOES NOT EXIST**
`Bool.true` does not exist.

**record_mixed_types.md:1:35:1:44:**
```roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
```
                                  ^^^^^^^^^


# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceDotLowerIdent,Comma,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,LowerIdent,OpColon,Float,CloseCurly,
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
		(e-ident (raw "Bool.true")))
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
			(e-runtime-error (tag "qualified_ident_does_not_exist")))
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
(expr (type "{ active: Error, age: Num(_size), balance: Num(Frac(_size2)), name: Error, scores: List(Num(_size3)) }"))
~~~
