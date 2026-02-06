# META
~~~ini
description=Mono test: closure returns closure with captured variable, verifying lifted patterns
type=mono
~~~
# SOURCE
~~~roc
# A function that returns a closure capturing a variable
# This tests that lifted function patterns are properly created
make_adder = |x| |y| x + y

# Use the closure maker
add_five = make_adder(5.I64)
result = add_five(10.I64)
~~~
# MONO
~~~roc
c1_make_adder = |y, captures| captures.x + y

make_adder = |x| C1_make_adder({ x: x })

add_five = make_adder(5.I64)

result = match add_five {
	C1_make_adder(captures) => c1_make_adder(10.I64, captures)
}
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "make_adder"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-lambda
					(args
						(p-ident (raw "y")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-ident (raw "y"))))))
		(s-decl
			(p-ident (raw "add_five"))
			(e-apply
				(e-ident (raw "make_adder"))
				(e-typed-int (raw "5") (type ".I64"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_five"))
				(e-typed-int (raw "10") (type ".I64"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "make_adder"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-tag (name "#1_make_adder")
				(args
					(e-record
						(fields
							(field (name "x")
								(e-lookup-local
									(p-assign (ident "x"))))))))))
	(d-let
		(p-assign (ident "add_five"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_adder")))
			(e-typed-int (value "5") (type "I64"))))
	(d-let
		(p-assign (ident "result"))
		(e-match
			(match
				(cond
					(e-lookup-local
						(p-assign (ident "add_five"))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-call
								(e-lookup-local
									(p-assign (ident "c1_make_adder")))
								(e-typed-int (value "10") (type "I64"))
								(e-lookup-local
									(p-assign (ident "captures")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[#1_make_adder({ ..a, x: <RecursiveType> }), ..b] -> (_arg -> [#1_make_adder({ ..a, x: <RecursiveType> }), ..b])"))
		(patt (type "I64 -> I64"))
		(patt (type "I64")))
	(expressions
		(expr (type "[#1_make_adder({ ..a, x: <RecursiveType> }), ..b] -> [#1_make_adder({ ..a, x: [#1_make_adder(<RecursiveType>), ..b] }), ..b]"))
		(expr (type "[#1_make_adder({ .., x: <RecursiveType> }), ..]"))
		(expr (type "[]"))))
~~~
