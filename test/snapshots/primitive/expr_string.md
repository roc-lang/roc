# META
~~~ini
description=A primitive
type=snippet
~~~
# SOURCE
~~~roc
name = "luc"
foo = "hello ${name}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "name"))
			(e-string
				(e-string-part (raw "luc"))))
		(s-decl
			(p-ident (raw "foo"))
			(e-string
				(e-string-part (raw "hello "))
				(e-ident (raw "name"))
				(e-string-part (raw ""))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "name"))
		(e-string
			(e-literal (string "luc"))))
	(d-let
		(p-assign (ident "foo"))
		(e-string
			(e-literal (string "hello "))
			(e-lookup-local
				(p-assign (ident "name")))
			(e-literal (string "")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
