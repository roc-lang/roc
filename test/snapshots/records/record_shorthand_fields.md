# META
~~~ini
description=Record construction using shorthand field syntax
type=expr
~~~
# SOURCE
~~~roc
{ name, age, email, active }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name"))
	(field (field "age"))
	(field (field "email"))
	(field (field "active")))
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
			(e-lookup-local
				(p-assign (ident "name"))))
		(field (name "age")
			(e-lookup-local
				(p-assign (ident "age"))))
		(field (name "email")
			(e-lookup-local
				(p-assign (ident "email"))))
		(field (name "active")
			(e-lookup-local
				(p-assign (ident "active"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ active: _field, age: _field2, email: _field3, name: _field4 }"))
~~~
