# META
~~~ini
description=A local definition used before it is defined (sequential scoping)
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
|_| {
    g = |x| f(x)
    f = |x| x + 1
    g(1)
}
~~~
# EXPECTED
USED BEFORE DEFINITION - local_let_forward_reference.md:2:13:2:14
# PROBLEMS

┌────────────────────────┐
│ USED BEFORE DEFINITION ├─ The name `f` is used before it is defined. ───────┐
└┬───────────────────────┘                                                    │
 │                                                                            │
 │  g = |x| f(x)                                                              │
 │          ‾                                                                 │
 └─────────────────────────────────────── local_let_forward_reference.md:2:13 ┘

    Local definitions are evaluated in order: a definition can refer to itself
    or to definitions written before it, but not to definitions written later
    in the same block. Move `f` above this use, or move both to the top level.

# TOKENS
~~~zig
OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-underscore))
	(e-block
		(statements
			(s-decl
				(p-ident (raw "g"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-apply
						(e-ident (raw "f"))
						(e-ident (raw "x")))))
			(s-decl
				(p-ident (raw "f"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-int (raw "1")))))
			(e-apply
				(e-ident (raw "g"))
				(e-int (raw "1"))))))
~~~
# FORMATTED
~~~roc
|_| {
	g = |x| f(x)
	f = |x| x + 1
	g(1)
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-underscore))
	(e-block
		(s-let
			(p-assign (ident "g"))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-call
					(e-runtime-error (tag "local_reference_before_definition"))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(s-let
			(p-assign (ident "f"))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-dispatch-call (method "plus") (constraint-fn-var 61)
					(receiver
						(e-lookup-local
							(p-assign (ident "x"))))
					(args
						(e-num (value "1"))))))
		(e-call (constraint-fn-var 99)
			(e-lookup-local
				(p-assign (ident "g")))
			(e-num (value "1")))))
~~~
# TYPES
~~~clojure
(expr (type "_arg -> Error"))
~~~
