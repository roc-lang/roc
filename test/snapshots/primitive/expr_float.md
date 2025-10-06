# META
~~~ini
description=A primitive
type=file:ExprFloat.roc
~~~
# SOURCE
~~~roc
ExprFloat := {}

foo = 12.34
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:10),OpColonEqual(1:11-1:13),OpenCurly(1:14-1:15),CloseCurly(1:15-1:16),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),Float(3:7-3:12),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.12
	(type-module @1.1-1.10)
	(statements
		(s-type-decl @1.1-1.16
			(header @1.1-1.10 (name "ExprFloat")
				(args))
			(ty-record @1.14-1.16))
		(s-decl @3.1-3.12
			(p-ident @3.1-3.4 (raw "foo"))
			(e-frac @3.7-3.12 (raw "12.34")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-dec-small @3.7-3.12 (numerator "1234") (denominator-power-of-ten "2") (value "12.34")))
	(s-nominal-decl @1.1-1.16
		(ty-header @1.1-1.10 (name "ExprFloat"))
		(ty-record @1.14-1.16)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Num(Frac(_size))")))
	(type_decls
		(nominal @1.1-1.16 (type "ExprFloat")
			(ty-header @1.1-1.10 (name "ExprFloat"))))
	(expressions
		(expr @3.7-3.12 (type "Num(Frac(_size))"))))
~~~
