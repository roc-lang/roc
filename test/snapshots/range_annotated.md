# META
~~~ini
description=Annotation on a range pins the bound type through Range(num)
type=snippet
~~~
# SOURCE
~~~roc
r : Range(U8)
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
	(type-mod)
	(statements
		(s-type-anno (name "r")
			(ty-apply
				(ty (name "Range"))
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
		(e-dispatch-call (method "range_exclusive_to") (constraint-fn-var 236)
			(receiver
				(e-num (value "0")))
			(args
				(e-num (value "10"))))
		(annotation
			(ty-apply (name "Range") (builtin)
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Range(U8)")))
	(expressions
		(expr (type "Range(U8)"))))
~~~
