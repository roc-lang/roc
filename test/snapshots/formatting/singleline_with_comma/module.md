# META
~~~ini
description=Singleline with comma formatting module
type=snippet
~~~
# SOURCE
~~~roc
a = 'a'
b = 'a'
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,SingleQuote,
LowerIdent,OpAssign,SingleQuote,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-single-quote (raw "'a'")))
		(s-decl
			(p-ident (raw "b"))
			(e-single-quote (raw "'a'")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "97")))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "97"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(num where [num.from_int_digits : List(U8) -> Try(num, [OutOfRange])])"))
		(patt (type "Num(num where [num.from_int_digits : List(U8) -> Try(num, [OutOfRange])])")))
	(expressions
		(expr (type "Num(num where [num.from_int_digits : List(U8) -> Try(num, [OutOfRange])])"))
		(expr (type "Num(num where [num.from_int_digits : List(U8) -> Try(num, [OutOfRange])])"))))
~~~
