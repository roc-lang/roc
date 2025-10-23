# META
~~~ini
description=Chained record updater expressions
type=snippet
~~~
# SOURCE
~~~roc
person = { name: "Alice", age: 30, city: "Boston" }
updated_one = { ..person, age: 31 }
updated2 = { ..updated_one, city: "New York" }
final = { ..updated2, name: "Alice Smith", age: 32 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "person"))
			(e-record
				(field (field "name")
					(e-string
						(e-string-part (raw "Alice"))))
				(field (field "age")
					(e-int (raw "30")))
				(field (field "city")
					(e-string
						(e-string-part (raw "Boston"))))))
		(s-decl
			(p-ident (raw "updated_one"))
			(e-record
				(ext
					(e-ident (raw "person")))
				(field (field "age")
					(e-int (raw "31")))))
		(s-decl
			(p-ident (raw "updated2"))
			(e-record
				(ext
					(e-ident (raw "updated_one")))
				(field (field "city")
					(e-string
						(e-string-part (raw "New York"))))))
		(s-decl
			(p-ident (raw "final"))
			(e-record
				(ext
					(e-ident (raw "updated2")))
				(field (field "name")
					(e-string
						(e-string-part (raw "Alice Smith"))))
				(field (field "age")
					(e-int (raw "32")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "person"))
		(e-record
			(fields
				(field (name "name")
					(e-string
						(e-literal (string "Alice"))))
				(field (name "age")
					(e-num (value "30")))
				(field (name "city")
					(e-string
						(e-literal (string "Boston")))))))
	(d-let
		(p-assign (ident "updated_one"))
		(e-record
			(ext
				(e-lookup-local
					(p-assign (ident "person"))))
			(fields
				(field (name "age")
					(e-num (value "31"))))))
	(d-let
		(p-assign (ident "updated2"))
		(e-record
			(ext
				(e-lookup-local
					(p-assign (ident "updated_one"))))
			(fields
				(field (name "city")
					(e-string
						(e-literal (string "New York")))))))
	(d-let
		(p-assign (ident "final"))
		(e-record
			(ext
				(e-lookup-local
					(p-assign (ident "updated2"))))
			(fields
				(field (name "name")
					(e-string
						(e-literal (string "Alice Smith"))))
				(field (name "age")
					(e-num (value "32")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ age: Num(_size), city: Str, name: Str }"))
		(patt (type "{ age: Num(_size), { age: Num(_size2), city: Str, name: Str } }"))
		(patt (type "{ city: Str, age: Num(_size), { age: Num(_size2), city: Str, name: Str } }"))
		(patt (type "{ age: Num(_size), name: Str, city: Str, age: Num(_size2), { age: Num(_size3), city: Str, name: Str } }")))
	(expressions
		(expr (type "{ age: Num(_size), city: Str, name: Str }"))
		(expr (type "{ age: Num(_size), { age: Num(_size2), city: Str, name: Str } }"))
		(expr (type "{ city: Str, age: Num(_size), { age: Num(_size2), city: Str, name: Str } }"))
		(expr (type "{ age: Num(_size), name: Str, city: Str, age: Num(_size2), { age: Num(_size3), city: Str, name: Str } }"))))
~~~
