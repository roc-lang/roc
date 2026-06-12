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
		(e-block
			(s-let
				(p-assign (ident "#interp_0"))
				(e-lookup-local
					(p-assign (ident "name"))))
			(e-dispatch-call (method "from_interpolation") (constraint-fn-var 167)
				(receiver
					(e-string
						(e-literal (string "hello "))))
				(args
					(e-dispatch-call (method "prepended") (constraint-fn-var 125)
						(receiver
							(e-dispatch-call (method "iter") (constraint-fn-var 61)
								(receiver
									(e-empty_list))
								(args)))
						(args
							(e-tuple
								(elems
									(e-lookup-local
										(p-assign (ident "#interp_0")))
									(e-string
										(e-literal (string ""))))))))))))
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
