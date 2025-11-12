# META
~~~ini
description=Headerless file with main function
type=file
~~~
# SOURCE
~~~roc
x = 5
main! = |_| x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-ident (raw "x"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "x")))
			(e-lambda
				(args
					(p-underscore))
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a where [_b.from_int_digits : _arg -> _ret]"))
		(patt (type "_arg -> _ret where [_a.from_int_digits : _arg2 -> _ret2]")))
	(expressions
		(expr (type "_a where [_b.from_int_digits : _arg -> _ret]"))
		(expr (type "_arg -> _ret where [_a.from_int_digits : _arg2 -> _ret2]"))))
~~~
