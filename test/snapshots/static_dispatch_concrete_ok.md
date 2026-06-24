# META
~~~ini
description=Static dispatch and equality on concretely-typed values still check cleanly (issue 9485 soundness guard)
type=file
~~~
# SOURCE
~~~roc
conv = |x| x.to_i128()

direct = (5.U8).to_i128()

through_helper = conv(5.U8)

eq = { a: 1.U8 } == { a: 2.U8 }

id = |x| x

poly_no_dispatch = id("hi")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,OpenRound,Int,NoSpaceDotUpperIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,CloseCurly,OpEquals,OpenCurly,LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "conv"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-method-call (method ".to_i128")
					(receiver
						(e-ident (raw "x")))
					(args))))
		(s-decl
			(p-ident (raw "direct"))
			(e-method-call (method ".to_i128")
				(receiver
					(e-tuple
						(e-typed-int (raw "5") (type "U8"))))
				(args)))
		(s-decl
			(p-ident (raw "through_helper"))
			(e-apply
				(e-ident (raw "conv"))
				(e-typed-int (raw "5") (type "U8"))))
		(s-decl
			(p-ident (raw "eq"))
			(e-binop (op "==")
				(e-record
					(field (field "a")
						(e-typed-int (raw "1") (type "U8"))))
				(e-record
					(field (field "a")
						(e-typed-int (raw "2") (type "U8"))))))
		(s-decl
			(p-ident (raw "id"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "poly_no_dispatch"))
			(e-apply
				(e-ident (raw "id"))
				(e-string
					(e-string-part (raw "hi")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "conv"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dispatch-call (method "to_i128") (constraint-fn-var 43)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args))))
	(d-let
		(p-assign (ident "direct"))
		(e-dispatch-call (method "to_i128") (constraint-fn-var 80)
			(receiver
				(e-typed-int (value "5") (type "U8")))
			(args)))
	(d-let
		(p-assign (ident "through_helper"))
		(e-call (constraint-fn-var 135)
			(e-lookup-local
				(p-assign (ident "conv")))
			(e-typed-int (value "5") (type "U8"))))
	(d-let
		(p-assign (ident "eq"))
		(e-structural-eq (negated "false")
			(lhs
				(e-record
					(fields
						(field (name "a")
							(e-typed-int (value "1") (type "U8"))))))
			(rhs
				(e-record
					(fields
						(field (name "a")
							(e-typed-int (value "2") (type "U8"))))))))
	(d-let
		(p-assign (ident "id"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "poly_no_dispatch"))
		(e-call (constraint-fn-var 248)
			(e-lookup-local
				(p-assign (ident "id")))
			(e-string
				(e-literal (string "hi"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "b -> c where [b.to_i128 : b -> c]"))
		(patt (type "I128"))
		(patt (type "I128"))
		(patt (type "Bool"))
		(patt (type "b -> b"))
		(patt (type "Str")))
	(expressions
		(expr (type "b -> c where [b.to_i128 : b -> c]"))
		(expr (type "I128"))
		(expr (type "I128"))
		(expr (type "Bool"))
		(expr (type "b -> b"))
		(expr (type "Str"))))
~~~
