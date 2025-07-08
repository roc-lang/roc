# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = 12.34
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),Float(2:7-2:12),EndOfFile(2:12-2:12),
~~~
# PARSE
~~~clojure
(file @1.1-2.12
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-decl @2.1-2.12
			(p-ident @2.1-2.4 (raw "foo"))
			(e-frac @2.7-2.12 (raw "12.34")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.4 (ident "foo"))
		(e-dec-small @2.7-2.12 (numerator "1234") (denominator-power-of-ten "2") (value "12.34"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.12 (type "Frac(*)")))
	(expressions
		(expr (type "Frac(*)"))))
~~~
