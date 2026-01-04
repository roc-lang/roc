# META
~~~ini
description=Mono test: nested closures with captures at top-level
type=mono
~~~
# SOURCE
~~~roc
x = 10
make_adder = |y| |z| x + y + z
add_five = make_adder(5)
result = add_five(3)
~~~
# MONO
~~~roc
c1_make_adder = |z, captures| x + captures.y + z

x = 10

make_adder = |y| C1_make_adder({ y: y })

add_five = make_adder(5)

result = match add_five {
	C1_make_adder(captures) => c1_make_adder(3, captures)
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
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "make_adder"))
			(e-lambda
				(args
					(p-ident (raw "y")))
				(e-lambda
					(args
						(p-ident (raw "z")))
					(e-binop (op "+")
						(e-binop (op "+")
							(e-ident (raw "x"))
							(e-ident (raw "y")))
						(e-ident (raw "z"))))))
		(s-decl
			(p-ident (raw "add_five"))
			(e-apply
				(e-ident (raw "make_adder"))
				(e-int (raw "5"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_five"))
				(e-int (raw "3"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "make_adder"))
		(e-lambda
			(args
				(p-assign (ident "y")))
			(e-tag (name "#1_make_adder")
				(args
					(e-record
						(fields
							(field (name "y")
								(e-lookup-local
									(p-assign (ident "y"))))))))))
	(d-let
		(p-assign (ident "add_five"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_adder")))
			(e-num (value "5"))))
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
								(e-num (value "3"))
								(e-lookup-local
									(p-assign (ident "captures")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> [#1_make_adder({ .._others, y: a }), .._others]"))
		(patt (type "a -> (a -> (a -> [#1_make_adder({ .._others, y: a }), .._others]))"))
		(patt (type "a -> b where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral([#1_make_adder({ ..c, y: d }), ..e])]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, a -> b]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral([#1_make_adder({ .._others, y: _field }), .._others])])]")))
	(expressions
		(expr (type "a -> [#1_make_adder({ .._others, y: a }), .._others]"))
		(expr (type "a -> [#1_make_adder({ .._others, y: a }), .._others]"))
		(expr (type "[#1_make_adder({ .._others, y: _field }), .._others]"))
		(expr (type "(a -> [#1_make_adder({ ..b, y: a }), ..c]), a -> (a -> [#1_make_adder({ ..b, y: a }), ..c])"))))
~~~
