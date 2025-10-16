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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwDbg,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
KwDbg,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-block
					(statements
						(s-dbg
							(e-field-access
								(e-ident (raw "num"))
								(e-apply
									(e-ident (raw "to_str")))))
						(s-dbg
							(e-tuple
								(e-ident (raw "num"))))))))))
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
		(p-assign (ident "foo"))
		(e-lambda
			(args
				(p-assign (ident "num")))
			(e-block
				(s-dbg
					(e-dot-access (field "to_str")
						(receiver
							(e-lookup-local
								(p-assign (ident "num"))))
						(args)))
				(e-dbg
					(e-lookup-local
						(p-assign (ident "num"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a")))
	(expressions
		(expr (type "a -> a"))))
~~~
