# META
~~~ini
description=Multiline formatting module
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
		(patt (type "Num(Int(_size))"))
		(patt (type "Num(Int(_size))")))
	(expressions
		(expr (type "Num(Int(_size))"))
		(expr (type "Num(Int(_size))"))))
~~~
