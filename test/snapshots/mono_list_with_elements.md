# META
~~~ini
description=Mono test: list with elements at top-level
type=mono
~~~
# SOURCE
~~~roc
numbers = [1, 2, 3]
~~~
# MONO
~~~roc
numbers : Bool
numbers = [1, 2, 3]
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
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "numbers"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "numbers"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "Bool"))))
~~~
