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
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),Float(1:7-1:12),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.12
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-1.12
			(p-ident @1.1-1.4 (raw "foo"))
			(e-frac @1.7-1.12 (raw "12.34")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.4 (ident "foo"))
		(e-dec-small @1.7-1.12 (numerator "1234") (denominator-power-of-ten "2") (value "12.34"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "Num(Frac(_size))")))
	(expressions
		(expr @1.7-1.12 (type "Num(Frac(_size))"))))
~~~
