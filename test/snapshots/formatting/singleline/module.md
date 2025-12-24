# META
~~~ini
description=Singleline formatting module
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
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))))
~~~
