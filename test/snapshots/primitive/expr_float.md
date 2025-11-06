# META
~~~ini
description=A primitive
type=snippet
~~~
# SOURCE
~~~roc
foo = 12.34
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-frac (raw "12.34")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-dec-small (numerator "1234") (denominator-power-of-ten "2") (value "12.34"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(num where [num.from_dec_digits : { before_dot : List(U8), after_dot : List(U8) } -> Try(num, [OutOfRange])])")))
	(expressions
		(expr (type "Num(num where [num.from_dec_digits : { before_dot : List(U8), after_dot : List(U8) } -> Try(num, [OutOfRange])])"))))
~~~
