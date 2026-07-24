# META
~~~ini
description=Calling a function-valued field of a nominal record with explicit field-application syntax (issue 10152)
type=snippet
~~~
# SOURCE
~~~roc
Field := { f : I64 -> I64 }

run : Field -> I64
run = |rec| (rec.f)(1)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Field")
				(args))
			(ty-record
				(anno-record-field (name "f")
					(ty-fn
						(ty (name "I64"))
						(ty (name "I64"))))))
		(s-type-anno (name "run")
			(ty-fn
				(ty (name "Field"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args
					(p-ident (raw "rec")))
				(e-apply
					(e-tuple
						(e-field-access
							(e-ident (raw "rec"))
							(e-ident (raw "f"))))
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-assign (ident "rec")))
			(e-call (constraint-fn-var 215)
				(e-field-access (field "f")
					(receiver
						(e-lookup-local
							(p-assign (ident "rec")))))
				(e-num (value "1"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Field") (local))
				(ty-lookup (name "I64") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Field"))
		(ty-record
			(field (field "f")
				(ty-fn (effectful false)
					(ty-lookup (name "I64") (builtin))
					(ty-lookup (name "I64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Field -> I64")))
	(type_decls
		(nominal (type "Field")
			(ty-header (name "Field"))))
	(expressions
		(expr (type "Field -> I64"))))
~~~
