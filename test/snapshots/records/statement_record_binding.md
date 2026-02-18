# META
~~~ini
description=Record in let binding statement
type=statement
~~~
# SOURCE
~~~roc
person = { name: "Alice", age: 30, email: "alice@example.com" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-decl
	(p-ident (raw "person"))
	(e-record
		(field (field "name")
			(e-string
				(e-string-part (raw "Alice"))))
		(field (field "age")
			(e-int (raw "30")))
		(field (field "email")
			(e-string
				(e-string-part (raw "alice@example.com"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-let
		(p-assign (ident "person"))
		(e-record
			(fields
				(field (name "name")
					(e-string
						(e-literal (string "Alice"))))
				(field (name "age")
					(e-num (value "30")))
				(field (name "email")
					(e-string
						(e-literal (string "alice@example.com"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
