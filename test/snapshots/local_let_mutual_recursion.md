# META
~~~ini
description=Mutual recursion between local definitions is not allowed (sequential scoping)
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
|_| {
    is_even = |n| if (n == 0) Bool.True else is_odd(n - 1)
    is_odd = |n| if (n == 0) Bool.False else is_even(n - 1)
    is_even(4)
}
~~~
# EXPECTED
MUTUALLY RECURSIVE LOCAL DEFINITIONS - local_let_mutual_recursion.md:2:46:2:52
# PROBLEMS

┌──────────────────────────────────────┐
│ MUTUALLY RECURSIVE LOCAL DEFINITIONS ├─ The local definitions `is_even` ────┐
└┬─────────────────────────────────────┘  and `is_odd` are mutually           │
 │                                        recursive, which isn't supported    │
 │                                        for local definitions.              │
 │                                                                            │
 │  is_even = |n| if (n == 0) Bool.True else is_odd(n - 1)                    │
 │                                           ‾‾‾‾‾‾                           │
 └──────────────────────────────────────── local_let_mutual_recursion.md:2:46 ┘

    Local definitions are evaluated in order and can only refer to themselves
    or to earlier definitions. Move these mutually recursive definitions to the
    top level, where mutual recursion is supported.

# TOKENS
~~~zig
OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,OpenRound,LowerIdent,OpEquals,Int,CloseRound,UpperIdent,NoSpaceDotUpperIdent,KwElse,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,OpenRound,LowerIdent,OpEquals,Int,CloseRound,UpperIdent,NoSpaceDotUpperIdent,KwElse,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
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
				(p-ident (raw "is_even"))
				(e-lambda
					(args
						(p-ident (raw "n")))
					(e-if-then-else
						(e-tuple
							(e-binop (op "==")
								(e-ident (raw "n"))
								(e-int (raw "0"))))
						(e-tag (raw "Bool.True"))
						(e-apply
							(e-ident (raw "is_odd"))
							(e-binop (op "-")
								(e-ident (raw "n"))
								(e-int (raw "1")))))))
			(s-decl
				(p-ident (raw "is_odd"))
				(e-lambda
					(args
						(p-ident (raw "n")))
					(e-if-then-else
						(e-tuple
							(e-binop (op "==")
								(e-ident (raw "n"))
								(e-int (raw "0"))))
						(e-tag (raw "Bool.False"))
						(e-apply
							(e-ident (raw "is_even"))
							(e-binop (op "-")
								(e-ident (raw "n"))
								(e-int (raw "1")))))))
			(e-apply
				(e-ident (raw "is_even"))
				(e-int (raw "4"))))))
~~~
# FORMATTED
~~~roc
|_| {
	is_even = |n| if (n == 0) Bool.True else is_odd(n - 1)
	is_odd = |n| if (n == 0) Bool.False else is_even(n - 1)
	is_even(4)
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-underscore))
	(e-block
		(s-let
			(p-assign (ident "is_even"))
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "n"))))
								(rhs
									(e-num (value "0"))))
							(e-nominal-external
								(builtin)
								(e-tag (name "True")))))
					(if-else
						(e-call
							(e-runtime-error (tag "local_reference_before_definition"))
							(e-dispatch-call (method "minus") (constraint-fn-var 128)
								(receiver
									(e-lookup-local
										(p-assign (ident "n"))))
								(args
									(e-num (value "1")))))))))
		(s-let
			(p-assign (ident "is_odd"))
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "n"))))
								(rhs
									(e-num (value "0"))))
							(e-nominal-external
								(builtin)
								(e-tag (name "False")))))
					(if-else
						(e-call (constraint-fn-var 223)
							(e-lookup-local
								(p-assign (ident "is_even")))
							(e-dispatch-call (method "minus") (constraint-fn-var 221)
								(receiver
									(e-lookup-local
										(p-assign (ident "n"))))
								(args
									(e-num (value "1")))))))))
		(e-call (constraint-fn-var 269)
			(e-lookup-local
				(p-assign (ident "is_even")))
			(e-num (value "4")))))
~~~
# TYPES
~~~clojure
(expr (type "_arg -> Error"))
~~~
