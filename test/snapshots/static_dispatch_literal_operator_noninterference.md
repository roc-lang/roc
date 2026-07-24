# META
~~~ini
description=Numeric-literal-only and equality-only programs never trigger the ambiguity judgment: defaulting and structural equality pin their receivers first
type=file
~~~
# SOURCE
~~~roc
nums = [1, 2, 3]

sum = 1 + 2

diff = 5 - sum

lists_equal = [1] == [1]

tags_equal = Try.Ok(1) == Try.Ok(1)

negated = -sum
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
LowerIdent,OpAssign,Int,OpPlus,Int,
LowerIdent,OpAssign,Int,OpBinaryMinus,LowerIdent,
LowerIdent,OpAssign,OpenSquare,Int,CloseSquare,OpEquals,OpenSquare,Int,CloseSquare,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,OpUnaryMinus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "nums"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-decl
			(p-ident (raw "sum"))
			(e-binop (op "+")
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "diff"))
			(e-binop (op "-")
				(e-int (raw "5"))
				(e-ident (raw "sum"))))
		(s-decl
			(p-ident (raw "lists_equal"))
			(e-binop (op "==")
				(e-list
					(e-int (raw "1")))
				(e-list
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "tags_equal"))
			(e-binop (op "==")
				(e-apply
					(e-tag (raw "Try.Ok"))
					(e-int (raw "1")))
				(e-apply
					(e-tag (raw "Try.Ok"))
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "negated"))
			(unary "-"
				(e-ident (raw "sum"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "nums"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3")))))
	(d-let
		(p-assign (ident "sum"))
		(e-dispatch-call (method "plus") (constraint-fn-var 242)
			(receiver
				(e-num (value "1")))
			(args
				(e-num (value "2")))))
	(d-let
		(p-assign (ident "diff"))
		(e-dispatch-call (method "minus") (constraint-fn-var 251)
			(receiver
				(e-num (value "5")))
			(args
				(e-lookup-local
					(p-assign (ident "sum"))))))
	(d-let
		(p-assign (ident "lists_equal"))
		(e-method-eq (negated "false")
			(lhs
				(e-list
					(elems
						(e-num (value "1")))))
			(rhs
				(e-list
					(elems
						(e-num (value "1")))))))
	(d-let
		(p-assign (ident "tags_equal"))
		(e-method-eq (negated "false")
			(lhs
				(e-nominal-external
					(builtin)
					(e-tag (name "Ok")
						(args
							(e-num (value "1"))))))
			(rhs
				(e-nominal-external
					(builtin)
					(e-tag (name "Ok")
						(args
							(e-num (value "1"))))))))
	(d-let
		(p-assign (ident "negated"))
		(e-dispatch-call (method "negate") (constraint-fn-var 334)
			(receiver
				(e-lookup-local
					(p-assign (ident "sum"))))
			(args))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Dec)"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Bool"))
		(patt (type "Bool"))
		(patt (type "Dec")))
	(expressions
		(expr (type "List(Dec)"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Bool"))
		(expr (type "Bool"))
		(expr (type "Dec"))))
~~~
