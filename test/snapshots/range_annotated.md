# META
~~~ini
description=Annotation on a range pins the bound type through Iter(num)
type=snippet
~~~
# SOURCE
~~~roc
r : Iter(U8)
r = 0..<10
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,Int,OpDoubleDotLessThan,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "r")
			(ty-apply
				(ty (name "Iter"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "r"))
			(e-binop (op "..<")
				(e-int (raw "0"))
				(e-int (raw "10"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-call (constraint-fn-var 264)
			(e-lookup-external
				(builtin))
			(e-num (value "0"))
			(e-num (value "10")))
		(annotation
			(ty-apply (name "Iter") (builtin)
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Iter(U8)")))
	(expressions
		(expr (type "Iter(U8)"))))
~~~
