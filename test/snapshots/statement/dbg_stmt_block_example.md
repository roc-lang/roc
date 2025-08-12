# META
~~~ini
description=Debug expression in a block context both statement and expression versions
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = |num| {
    # statement - prints out the value of num convertert to a string
    dbg num.to_str()

    # expression - prints out the value of num and then returns it
    dbg(num)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),OpBar(3:7-3:8),LowerIdent(3:8-3:11),OpBar(3:11-3:12),OpenCurly(3:13-3:14),
KwDbg(5:5-5:8),LowerIdent(5:9-5:12),NoSpaceDotLowerIdent(5:12-5:19),NoSpaceOpenRound(5:19-5:20),CloseRound(5:20-5:21),
KwDbg(8:5-8:8),NoSpaceOpenRound(8:8-8:9),LowerIdent(8:9-8:12),CloseRound(8:12-8:13),
CloseCurly(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(file @1.1-9.2
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
		(s-decl @3.1-9.2
			(p-ident @3.1-3.4 (raw "foo"))
			(e-lambda @3.7-9.2
				(args
					(p-ident @3.8-3.11 (raw "num")))
				(e-block @3.13-9.2
					(statements
						(s-dbg @5.5-5.21
							(e-field-access @5.9-5.21
								(e-ident @5.9-5.12 (raw "num"))
								(e-apply @5.12-5.21
									(e-ident @5.12-5.19 (raw "to_str")))))
						(s-dbg @8.5-8.13
							(e-tuple @8.8-8.13
								(e-ident @8.9-8.12 (raw "num"))))))))))
~~~
# FORMATTED
~~~roc
module [foo]

foo = |num| {
	# statement - prints out the value of num convertert to a string
	dbg num.to_str()

	# expression - prints out the value of num and then returns it
	dbg (num)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-lambda @3.7-9.2
			(args
				(p-assign @3.8-3.11 (ident "num")))
			(e-block @3.13-9.2
				(s-dbg @5.5-5.21
					(e-dot-access @5.9-5.21 (field "to_str")
						(receiver
							(e-lookup-local @5.9-5.12
								(p-assign @3.8-3.11 (ident "num"))))
						(args)))
				(e-dbg @8.5-8.13
					(e-lookup-local @8.9-8.12
						(p-assign @3.8-3.11 (ident "num"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "_arg -> _ret")))
	(expressions
		(expr @3.7-9.2 (type "_arg -> _ret"))))
~~~
