# META
~~~ini
description=Debug expression in a block context both statement and expression versions
type=file:DbgStmtBlockExample.roc
~~~
# SOURCE
~~~roc
DbgStmtBlockExample := {}

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
UpperIdent(1:1-1:20),OpColonEqual(1:21-1:23),OpenCurly(1:24-1:25),CloseCurly(1:25-1:26),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),OpBar(3:7-3:8),LowerIdent(3:8-3:11),OpBar(3:11-3:12),OpenCurly(3:13-3:14),
KwDbg(5:5-5:8),LowerIdent(5:9-5:12),NoSpaceDotLowerIdent(5:12-5:19),NoSpaceOpenRound(5:19-5:20),CloseRound(5:20-5:21),
KwDbg(8:5-8:8),NoSpaceOpenRound(8:8-8:9),LowerIdent(8:9-8:12),CloseRound(8:12-8:13),
CloseCurly(9:1-9:2),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.2
	(type-module @1.1-1.20)
	(statements
		(s-type-decl @1.1-1.26
			(header @1.1-1.20 (name "DbgStmtBlockExample")
				(args))
			(ty-record @1.24-1.26))
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
DbgStmtBlockExample := {}

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
						(p-assign @3.8-3.11 (ident "num")))))))
	(s-nominal-decl @1.1-1.26
		(ty-header @1.1-1.20 (name "DbgStmtBlockExample"))
		(ty-record @1.24-1.26)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "a -> a")))
	(type_decls
		(nominal @1.1-1.26 (type "DbgStmtBlockExample")
			(ty-header @1.1-1.20 (name "DbgStmtBlockExample"))))
	(expressions
		(expr @3.7-9.2 (type "a -> a"))))
~~~
