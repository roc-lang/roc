# META
~~~ini
description=repro for https://github.com/roc-lang/roc/issues/9725 — a record must be usable as a Dict key (structural to_hash), like it is as a Set element
type=file
~~~
# SOURCE
~~~roc
main! = |_args| {
    dbg Dict.empty().insert({a: 1, b: 2}, 3)
    Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
KwDbg,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,Comma,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(s-dbg
							(e-method-call (method ".insert")
								(receiver
									(e-apply
										(e-ident (raw "Dict.empty"))))
								(args
									(e-record
										(field (field "a")
											(e-int (raw "1")))
										(field (field "b")
											(e-int (raw "2"))))
									(e-int (raw "3")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
main! = |_args| {
	dbg Dict.empty().insert({ a: 1, b: 2 }, 3)
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
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-dbg
					(e-dispatch-call (method "insert") (constraint-fn-var 206)
						(receiver
							(e-call (constraint-fn-var 105)
								(e-lookup-external
									(builtin))))
						(args
							(e-record
								(fields
									(field (name "a")
										(e-num (value "1")))
									(field (name "b")
										(e-num (value "2")))))
							(e-num (value "3")))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "_arg -> [Ok({}), ..]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "_arg -> [Ok({}), ..]"))))
~~~
