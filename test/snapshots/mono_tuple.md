# META
~~~ini
description=Mono test: tuple at top-level
type=mono
~~~
# SOURCE
~~~roc
pair = (1, 2)
~~~
# MONO
~~~roc
pair : (Dec, Dec) = (1, 2)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "pair"))
			(e-tuple
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "pair"))
		(e-tuple
			(elems
				(e-num (value "1"))
				(e-num (value "2"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a, b) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "(a, b) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))))
~~~
