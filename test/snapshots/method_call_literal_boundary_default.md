# META
~~~ini
description=Open numeric literals unreachable from a generalized def's signature are defaulted at the generalization boundary (with a warning), and the deferred dispatch cascade pins the signature
type=snippet
~~~
# SOURCE
~~~roc
func = |x, y| {
	add_x = |a| a.plus(x)
	add_y = |b| b.plus(y)
	add_x(5).plus(add_y(5))
}

result = func(10, 20)
~~~
# EXPECTED
LITERAL DEFAULTED - method_call_literal_boundary_default.md:4:8:4:9
LITERAL DEFAULTED - method_call_literal_boundary_default.md:4:22:4:23
# PROBLEMS

┌───────────────────┐
│ LITERAL DEFAULTED ├─ Nothing in this definition's type determines the ──────┐
└┬──────────────────┘  type of this number literal, so it was given the       │
 │                     default type `Dec` instead.                            │
 │                                                                            │
 │  add_x(5).plus(add_y(5))                                                   │
 │        ‾                                                                   │
 └─────────────────────────────── method_call_literal_boundary_default.md:4:8 ┘

    Hint: To use a different numeric type here, add a suffix or a type
    annotation.


┌───────────────────┐
│ LITERAL DEFAULTED ├─ Nothing in this definition's type determines the ──────┐
└┬──────────────────┘  type of this number literal, so it was given the       │
 │                     default type `Dec` instead.                            │
 │                                                                            │
 │  add_x(5).plus(add_y(5))                                                   │
 │                      ‾                                                     │
 └────────────────────────────── method_call_literal_boundary_default.md:4:22 ┘

    Hint: To use a different numeric type here, add a suffix or a type
    annotation.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "func"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "add_x"))
							(e-lambda
								(args
									(p-ident (raw "a")))
								(e-method-call (method ".plus")
									(receiver
										(e-ident (raw "a")))
									(args
										(e-ident (raw "x"))))))
						(s-decl
							(p-ident (raw "add_y"))
							(e-lambda
								(args
									(p-ident (raw "b")))
								(e-method-call (method ".plus")
									(receiver
										(e-ident (raw "b")))
									(args
										(e-ident (raw "y"))))))
						(e-method-call (method ".plus")
							(receiver
								(e-apply
									(e-ident (raw "add_x"))
									(e-int (raw "5"))))
							(args
								(e-apply
									(e-ident (raw "add_y"))
									(e-int (raw "5")))))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "10"))
				(e-int (raw "20"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-block
				(s-let
					(p-assign (ident "add_x"))
					(e-closure
						(captures
							(capture (ident "x")))
						(e-lambda
							(args
								(p-assign (ident "a")))
							(e-dispatch-call (method "plus") (constraint-fn-var 46)
								(receiver
									(e-lookup-local
										(p-assign (ident "a"))))
								(args
									(e-lookup-local
										(p-assign (ident "x"))))))))
				(s-let
					(p-assign (ident "add_y"))
					(e-closure
						(captures
							(capture (ident "y")))
						(e-lambda
							(args
								(p-assign (ident "b")))
							(e-dispatch-call (method "plus") (constraint-fn-var 48)
								(receiver
									(e-lookup-local
										(p-assign (ident "b"))))
								(args
									(e-lookup-local
										(p-assign (ident "y"))))))))
				(e-dispatch-call (method "plus") (constraint-fn-var 126)
					(receiver
						(e-call (constraint-fn-var 87)
							(e-lookup-local
								(p-assign (ident "add_x")))
							(e-num (value "5"))))
					(args
						(e-call (constraint-fn-var 125)
							(e-lookup-local
								(p-assign (ident "add_y")))
							(e-num (value "5"))))))))
	(d-let
		(p-assign (ident "result"))
		(e-call (constraint-fn-var 380)
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "10"))
			(e-num (value "20")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec, Dec -> Dec"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec, Dec -> Dec"))
		(expr (type "Dec"))))
~~~
