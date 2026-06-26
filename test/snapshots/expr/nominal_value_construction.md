# META
~~~ini
description=Nominal value construction with Type.(value) syntax
type=snippet
~~~
# SOURCE
~~~roc
Distance := U64

d : Distance
d = Distance.(26)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Distance")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "d")
			(ty (name "Distance")))
		(s-decl
			(p-ident (raw "d"))
			(e-nominal-apply
				(mapper (e-tag (raw "Distance")))
				(e-int (raw "26"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "d"))
		(e-nominal (nominal "Distance")
			(e-num (value "26")))
		(annotation
			(ty-lookup (name "Distance") (local))))
	(s-nominal-decl
		(ty-header (name "Distance"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Distance")))
	(type_decls
		(nominal (type "Distance")
			(ty-header (name "Distance"))))
	(expressions
		(expr (type "Distance"))))
~~~
