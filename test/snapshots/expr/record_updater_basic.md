# META
~~~ini
description=Basic record updater with field override
type=snippet
~~~
# SOURCE
~~~roc
person = { name: "Alice", age: 30 }
updated = { ..person, age: 31 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
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
					(e-int (raw "30")))))
		(s-decl
			(p-ident (raw "updated"))
			(e-record
				(ext
					(e-ident (raw "person")))
				(field (field "age")
					(e-int (raw "31")))))))
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
					(e-num (value "30"))))))
	(d-let
		(p-assign (ident "updated"))
		(e-record
			(ext
				(e-lookup-local
					(p-assign (ident "person"))))
			(fields
				(field (name "age")
					(e-num (value "31")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ age: Num(_size), name: Str }"))
		(patt (type "{ age: Num(_size), { age: Num(_size2), name: Str } }")))
	(expressions
		(expr (type "{ age: Num(_size), name: Str }"))
		(expr (type "{ age: Num(_size), { age: Num(_size2), name: Str } }"))))
~~~
