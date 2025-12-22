# META
~~~ini
description=Mono test: block with let binding at top-level
type=mono
~~~
# SOURCE
~~~roc
result = {
    x = 42
    x
}
~~~
# MONO
~~~roc
result : Dec = 42
~~~
# FORMATTED
~~~roc
result = {
	x = 42
	x
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "x"))
						(e-int (raw "42")))
					(e-ident (raw "x")))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-num (value "42"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
