# META
~~~ini
description=Debug expression in a block context both statement and expression versions
type=snippet
~~~
# SOURCE
~~~roc
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
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),OpBar(1:7-1:8),LowerIdent(1:8-1:11),OpBar(1:11-1:12),OpenCurly(1:13-1:14),
KwDbg(3:5-3:8),LowerIdent(3:9-3:12),NoSpaceDotLowerIdent(3:12-3:19),NoSpaceOpenRound(3:19-3:20),CloseRound(3:20-3:21),
KwDbg(6:5-6:8),NoSpaceOpenRound(6:8-6:9),LowerIdent(6:9-6:12),CloseRound(6:12-6:13),
CloseCurly(7:1-7:2),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.2
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-7.2
			(p-ident @1.1-1.4 (raw "foo"))
			(e-lambda @1.7-7.2
				(args
					(p-ident @1.8-1.11 (raw "num")))
				(e-block @1.13-7.2
					(statements
						(s-dbg @3.5-3.21
							(e-field-access @3.9-3.21
								(e-ident @3.9-3.12 (raw "num"))
								(e-apply @3.12-3.21
									(e-ident @3.12-3.19 (raw "to_str")))))
						(s-dbg @6.5-6.13
							(e-tuple @6.8-6.13
								(e-ident @6.9-6.12 (raw "num"))))))))))
~~~
# FORMATTED
~~~roc
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
		(p-assign @1.1-1.4 (ident "foo"))
		(e-lambda @1.7-7.2
			(args
				(p-assign @1.8-1.11 (ident "num")))
			(e-block @1.13-7.2
				(s-dbg @3.5-3.21
					(e-dot-access @3.9-3.21 (field "to_str")
						(receiver
							(e-lookup-local @3.9-3.12
								(p-assign @1.8-1.11 (ident "num"))))
						(args)))
				(e-dbg @6.5-6.13
					(e-lookup-local @6.9-6.12
						(p-assign @1.8-1.11 (ident "num"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "a -> a")))
	(expressions
		(expr @1.7-7.2 (type "a -> a"))))
~~~
