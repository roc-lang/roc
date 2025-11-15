# META
~~~ini
description=Float literal type inference
type=snippet
~~~
# SOURCE
~~~roc
x = 3.14
y = 1.23e45
z = 0.5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-frac (raw "3.14")))
		(s-decl
			(p-ident (raw "y"))
			(e-frac (raw "1.23e45")))
		(s-decl
			(p-ident (raw "z"))
			(e-frac (raw "0.5")))))
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
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(d-let
		(p-assign (ident "y"))
		(e-frac-f64 (value "1.23e45")))
	(d-let
		(p-assign (ident "z"))
		(e-dec-small (numerator "5") (denominator-power-of-ten "1") (value "0.5"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_size where [_a.from_dec_digits : _arg -> _ret]"))
		(patt (type "_size where [_a.from_dec_digits : _arg -> _ret]"))
		(patt (type "_size where [_a.from_dec_digits : _arg -> _ret]")))
	(expressions
		(expr (type "_size where [_a.from_dec_digits : _arg -> _ret]"))
		(expr (type "_size where [_a.from_dec_digits : _arg -> _ret]"))
		(expr (type "_size where [_a.from_dec_digits : _arg -> _ret]"))))
~~~
