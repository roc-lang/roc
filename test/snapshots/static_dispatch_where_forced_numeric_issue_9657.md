# META
~~~ini
description=A where-clause receiver forced to a numeric grounding that lacks the method is rejected at check time instead of panicking post-check (issue 9657)
type=file
~~~
# SOURCE
~~~roc
make_map : (a -> b) -> (I64 -> I64) where [a.decode : I64 -> a, b.encode : b -> I64]
make_map = |f| {
    wrapped : I64 -> I64
    wrapped = |input| {
        A : a
        value : a
        value = A.decode(input)

        output : b
        output = f(value)

        output.encode()
    }

    wrapped
}

use_it = {
    transform = make_map(|n| n + 1)
    transform(41)
}
~~~
# EXPECTED
MISSING METHOD - static_dispatch_where_forced_numeric_issue_9657.md:12:9:12:24
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `encode` on ────┐
└┬───────────────┘  an unresolved type variable, but unresolved type          │
 │                  variables have no methods.                                │
 │                                                                            │
 │  output.encode()                                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                           │
 └─────────────────── static_dispatch_where_forced_numeric_issue_9657.md:12:9 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.

# TOKENS
~~~zig
LowerIdent,OpColon,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,OpenRound,UpperIdent,OpArrow,UpperIdent,CloseRound,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "make_map")
			(ty-fn
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-fn
					(ty (name "I64"))
					(ty (name "I64"))))
			(where
				(method (mod-of "a") (name "decode")
					(args
						(ty (name "I64")))
					(ty-var (raw "a")))
				(method (mod-of "b") (name "encode")
					(args
						(ty-var (raw "b")))
					(ty (name "I64")))))
		(s-decl
			(p-ident (raw "make_map"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-block
					(statements
						(s-type-anno (name "wrapped")
							(ty-fn
								(ty (name "I64"))
								(ty (name "I64"))))
						(s-decl
							(p-ident (raw "wrapped"))
							(e-lambda
								(args
									(p-ident (raw "input")))
								(e-block
									(statements
										(s-type-decl
											(header (name "A")
												(args))
											(ty-var (raw "a")))
										(s-type-anno (name "value")
											(ty-var (raw "a")))
										(s-decl
											(p-ident (raw "value"))
											(e-apply
												(e-ident (raw "A.decode"))
												(e-ident (raw "input"))))
										(s-type-anno (name "output")
											(ty-var (raw "b")))
										(s-decl
											(p-ident (raw "output"))
											(e-apply
												(e-ident (raw "f"))
												(e-ident (raw "value"))))
										(e-method-call (method ".encode")
											(receiver
												(e-ident (raw "output")))
											(args))))))
						(e-ident (raw "wrapped"))))))
		(s-decl
			(p-ident (raw "use_it"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "transform"))
						(e-apply
							(e-ident (raw "make_map"))
							(e-lambda
								(args
									(p-ident (raw "n")))
								(e-binop (op "+")
									(e-ident (raw "n"))
									(e-int (raw "1"))))))
					(e-apply
						(e-ident (raw "transform"))
						(e-int (raw "41"))))))))
~~~
# FORMATTED
~~~roc
make_map : (a -> b) -> (I64 -> I64) where [a.decode : I64 -> a, b.encode : b -> I64]
make_map = |f| {
	wrapped : I64 -> I64
	wrapped = |input| {
		A : a
		value : a
		value = A.decode(input)

		output : b
		output = f(value)

		output.encode()
	}

	wrapped
}

use_it = {
	transform = make_map(|n| n + 1)
	transform(41)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "make_map"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-block
				(s-let
					(p-assign (ident "wrapped"))
					(e-closure
						(captures
							(capture (ident "f")))
						(e-lambda
							(args
								(p-assign (ident "input")))
							(e-block
								(s-type-var-alias (alias "A") (type-var "a")
									(ty-rigid-var (name "a")))
								(s-let
									(p-assign (ident "value"))
									(e-type-dispatch-call (method "decode") (type-dispatch-stmt 28) (constraint-fn-var 278)
										(args
											(e-lookup-local
												(p-assign (ident "input"))))))
								(s-let
									(p-assign (ident "output"))
									(e-call (constraint-fn-var 284)
										(e-lookup-local
											(p-assign (ident "f")))
										(e-lookup-local
											(p-assign (ident "value")))))
								(e-runtime-error (tag "erroneous_value_expr"))))))
				(e-lookup-local
					(p-assign (ident "wrapped")))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "a"))
						(ty-rigid-var (name "b"))))
				(ty-parens
					(ty-fn (effectful false)
						(ty-lookup (name "I64") (builtin))
						(ty-lookup (name "I64") (builtin)))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "decode")
					(args
						(ty-lookup (name "I64") (builtin)))
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "b"))) (name "encode")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
					(ty-lookup (name "I64") (builtin))))))
	(d-let
		(p-assign (ident "use_it"))
		(e-block
			(s-let
				(p-assign (ident "transform"))
				(e-call (constraint-fn-var 302)
					(e-lookup-local
						(p-assign (ident "make_map")))
					(e-lambda
						(args
							(p-assign (ident "n")))
						(e-dispatch-call (method "plus") (constraint-fn-var 300)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "1")))))))
			(e-call (constraint-fn-var 310)
				(e-lookup-local
					(p-assign (ident "transform")))
				(e-num (value "41"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a -> b) -> (I64 -> I64) where [a.decode : I64 -> a, b.encode : b -> I64]"))
		(patt (type "I64")))
	(expressions
		(expr (type "(a -> b) -> (I64 -> I64) where [a.decode : I64 -> a, b.encode : b -> I64]"))
		(expr (type "I64"))))
~~~
