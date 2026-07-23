# META
~~~ini
description=A recursive call through the group's own `=>` annotation must not trigger the effectful-name warning; a call to an effectful function of a finished group still does
type=file
~~~
# SOURCE
~~~roc
recurse : U64 => U64
recurse = |n|
    1 + recurse(n + 1)

pingIt : U64 => U64
pingIt = |n| if n == 0 1 else pongIt(n - 1)

pongIt : U64 => U64
pongIt = |n| if n == 0 2 else pingIt(n - 1)

caller = |n| recurse(n)

main! = |_| {
    _ = caller(0)
    Ok({})
}
~~~
# EXPECTED
EFFECTFUL FUNCTION NAME - effectful_annotation_recursive_no_name_warning.md:11:1:11:7
# PROBLEMS

┌─────────────────────────┐
│ EFFECTFUL FUNCTION NAME ├─ This function performs an effect, so its name ───┐
└┬────────────────────────┘  must end in `!`.                                 │
 │                                                                            │
 │  caller = |n| recurse(n)                                                   │
 │  ‾‾‾‾‾‾                                                                    │
 └──────────────────── effectful_annotation_recursive_no_name_warning.md:11:1 ┘

    Add a trailing `!` to this function name.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
Int,OpPlus,LowerIdent,NoSpaceOpenRound,LowerIdent,OpPlus,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,Int,Int,KwElse,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,Int,Int,KwElse,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "recurse")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "recurse"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-binop (op "+")
					(e-int (raw "1"))
					(e-apply
						(e-ident (raw "recurse"))
						(e-binop (op "+")
							(e-ident (raw "n"))
							(e-int (raw "1")))))))
		(s-type-anno (name "pingIt")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "pingIt"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "n"))
						(e-int (raw "0")))
					(e-int (raw "1"))
					(e-apply
						(e-ident (raw "pongIt"))
						(e-binop (op "-")
							(e-ident (raw "n"))
							(e-int (raw "1")))))))
		(s-type-anno (name "pongIt")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "pongIt"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "n"))
						(e-int (raw "0")))
					(e-int (raw "2"))
					(e-apply
						(e-ident (raw "pingIt"))
						(e-binop (op "-")
							(e-ident (raw "n"))
							(e-int (raw "1")))))))
		(s-decl
			(p-ident (raw "caller"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-apply
					(e-ident (raw "recurse"))
					(e-ident (raw "n")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "caller"))
								(e-int (raw "0"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
recurse : U64 => U64
recurse = |n|
	1 + recurse(n + 1)

pingIt : U64 => U64
pingIt = |n| if n == 0 1 else pongIt(n - 1)

pongIt : U64 => U64
pongIt = |n| if n == 0 2 else pingIt(n - 1)

caller = |n| recurse(n)

main! = |_| {
	_ = caller(0)
	Ok({})
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "recurse"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-dispatch-call (method "plus") (constraint-fn-var 316)
				(receiver
					(e-num (value "1")))
				(args
					(e-call (constraint-fn-var 315)
						(e-lookup-local
							(p-assign (ident "recurse")))
						(e-dispatch-call (method "plus") (constraint-fn-var 295)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "1"))))))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "pingIt"))
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
						(e-num (value "1"))))
				(if-else
					(e-call (constraint-fn-var 406)
						(e-lookup-local
							(p-assign (ident "pongIt")))
						(e-dispatch-call (method "minus") (constraint-fn-var 386)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "1"))))))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "pongIt"))
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
						(e-num (value "2"))))
				(if-else
					(e-call (constraint-fn-var 491)
						(e-lookup-local
							(p-assign (ident "pingIt")))
						(e-dispatch-call (method "minus") (constraint-fn-var 471)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "1"))))))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "caller"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-call (constraint-fn-var 506)
				(e-lookup-local
					(p-assign (ident "recurse")))
				(e-lookup-local
					(p-assign (ident "n"))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 514)
						(e-lookup-local
							(p-assign (ident "caller")))
						(e-num (value "0"))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "U64 => U64"))
		(patt (type "U64 => U64"))
		(patt (type "U64 => U64"))
		(patt (type "U64 => U64"))
		(patt (type "_arg => [Ok({}), ..]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "U64 => U64"))
		(expr (type "U64 => U64"))
		(expr (type "U64 => U64"))
		(expr (type "U64 => U64"))
		(expr (type "_arg => [Ok({}), ..]"))))
~~~
