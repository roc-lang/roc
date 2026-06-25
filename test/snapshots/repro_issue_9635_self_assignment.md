# META
~~~ini
description=repro for https://github.com/roc-lang/roc/issues/9635 — top-level value referenced multiple times on RHS of a nested binding is wrongly flagged as self-assignment
type=file
~~~
# SOURCE
~~~roc
make = || {
    total = base + base + base
    total
}

base = 10

main! = |_args| Ok({})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "make"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-ident (raw "total"))
							(e-binop (op "+")
								(e-binop (op "+")
									(e-ident (raw "base"))
									(e-ident (raw "base")))
								(e-ident (raw "base"))))
						(e-ident (raw "total"))))))
		(s-decl
			(p-ident (raw "base"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-apply
					(e-tag (raw "Ok"))
					(e-record))))))
~~~
# FORMATTED
~~~roc
make = || {
	total = base + base + base
	total
}

base = 10

main! = |_args| Ok({})
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
		(p-assign (ident "make"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-assign (ident "total"))
					(e-dispatch-call (method "plus") (constraint-fn-var 84)
						(receiver
							(e-dispatch-call (method "plus") (constraint-fn-var 82)
								(receiver
									(e-lookup-local
										(p-assign (ident "base"))))
								(args
									(e-lookup-local
										(p-assign (ident "base"))))))
						(args
							(e-lookup-local
								(p-assign (ident "base"))))))
				(e-lookup-local
					(p-assign (ident "total"))))))
	(d-let
		(p-assign (ident "base"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-tag (name "Ok")
				(args
					(e-empty_record))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "({}) -> Dec"))
		(patt (type "Dec"))
		(patt (type "_arg -> [Ok({}), ..]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "({}) -> Dec"))
		(expr (type "Dec"))
		(expr (type "_arg -> [Ok({}), ..]"))))
~~~
