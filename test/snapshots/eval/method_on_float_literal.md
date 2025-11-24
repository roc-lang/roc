# META
~~~ini
description=Method call directly on float literal
type=snippet
~~~
# SOURCE
~~~roc
age : Str
age = 12.34.to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "age")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "age"))
			(e-field-access
				(e-frac (raw "12.34"))
				(e-apply
					(e-ident (raw "to_str")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "age"))
		(e-dot-access (field "to_str")
			(receiver
				(e-dec-small (numerator "1234") (denominator-power-of-ten "2") (value "12.34")))
			(args))
		(annotation
			(ty-lookup (name "Str") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))))
~~~
