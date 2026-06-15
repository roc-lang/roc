# META
~~~ini
description=Interpolated string literals desugar to from_interpolation
type=file
~~~
# SOURCE
~~~roc
greet : Str -> Str
greet = |name| "Hello, ${name}!"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "greet")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "greet"))
			(e-lambda
				(args
					(p-ident (raw "name")))
				(e-string
					(e-string-part (raw "Hello, "))
					(e-ident (raw "name"))
					(e-string-part (raw "!")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "greet"))
		(e-lambda
			(args
				(p-assign (ident "name")))
			(e-block
				(s-let
					(p-assign (ident "#interp_0"))
					(e-lookup-local
						(p-assign (ident "name"))))
				(e-interpolation (constraint-fn-var 93)
					(first
						(e-literal (string "Hello, ")))
					(parts
						(e-lookup-local
							(p-assign (ident "#interp_0")))
						(e-literal (string "!"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Str")))
	(expressions
		(expr (type "Str -> Str"))))
~~~
