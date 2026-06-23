# META
~~~ini
description=Mono test: re-emission keeps parens around right-nested same-precedence operands and doubled negation
type=mono
~~~
# SOURCE
~~~roc
sub_nested = 1 - (2 - 3)
div_nested = 8 / (4 / 2)
mixed_nested = 1 - (2 + 3)
neg_nested = -(-(sub_nested))
~~~
# MONO
~~~roc
sub_nested : Dec
sub_nested = 1 - (2 - 3)

div_nested : Dec
div_nested = 8 / (4 / 2)

mixed_nested : Dec
mixed_nested = 1 - (2 + 3)

neg_nested : Dec
neg_nested = -(-sub_nested)
~~~
# FORMATTED
~~~roc
sub_nested = 1 - (2 - 3)

div_nested = 8 / (4 / 2)

mixed_nested = 1 - (2 + 3)

neg_nested = -(-(sub_nested))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpBinaryMinus,OpenRound,Int,OpBinaryMinus,Int,CloseRound,
LowerIdent,OpAssign,Int,OpSlash,OpenRound,Int,OpSlash,Int,CloseRound,
LowerIdent,OpAssign,Int,OpBinaryMinus,OpenRound,Int,OpPlus,Int,CloseRound,
LowerIdent,OpAssign,OpUnaryMinus,NoSpaceOpenRound,OpUnaryMinus,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "sub_nested"))
			(e-binop (op "-")
				(e-int (raw "1"))
				(e-tuple
					(e-binop (op "-")
						(e-int (raw "2"))
						(e-int (raw "3"))))))
		(s-decl
			(p-ident (raw "div_nested"))
			(e-binop (op "/")
				(e-int (raw "8"))
				(e-tuple
					(e-binop (op "/")
						(e-int (raw "4"))
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "mixed_nested"))
			(e-binop (op "-")
				(e-int (raw "1"))
				(e-tuple
					(e-binop (op "+")
						(e-int (raw "2"))
						(e-int (raw "3"))))))
		(s-decl
			(p-ident (raw "neg_nested"))
			(unary "-"
				(e-tuple
					(unary "-"
						(e-tuple
							(e-ident (raw "sub_nested")))))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sub_nested"))
		(e-dispatch-call (method "minus") (constraint-fn-var 133)
			(receiver
				(e-num (value "1")))
			(args
				(e-dispatch-call (method "minus") (constraint-fn-var 131)
					(receiver
						(e-num (value "2")))
					(args
						(e-num (value "3")))))))
	(d-let
		(p-assign (ident "div_nested"))
		(e-dispatch-call (method "div_by") (constraint-fn-var 236)
			(receiver
				(e-num (value "8")))
			(args
				(e-dispatch-call (method "div_by") (constraint-fn-var 234)
					(receiver
						(e-num (value "4")))
					(args
						(e-num (value "2")))))))
	(d-let
		(p-assign (ident "mixed_nested"))
		(e-dispatch-call (method "minus") (constraint-fn-var 339)
			(receiver
				(e-num (value "1")))
			(args
				(e-dispatch-call (method "plus") (constraint-fn-var 337)
					(receiver
						(e-num (value "2")))
					(args
						(e-num (value "3")))))))
	(d-let
		(p-assign (ident "neg_nested"))
		(e-dispatch-call (method "negate") (constraint-fn-var 343)
			(receiver
				(e-dispatch-call (method "negate") (constraint-fn-var 341)
					(receiver
						(e-lookup-local
							(p-assign (ident "sub_nested"))))
					(args)))
			(args))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))))
~~~
