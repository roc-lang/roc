# META
~~~ini
description=Calling a float literal directly (type error)
type=snippet
~~~
# SOURCE
~~~roc
x = 12.34()
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - call_float_literal.md:1:5:1:10
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_numeral` on a type that doesn't support methods:
**call_float_literal.md:1:5:1:10:**
```roc
x = 12.34()
```
    ^^^^^

This type doesn't support methods:
    _({}) -> _ret_



# TOKENS
~~~zig
LowerIdent,OpAssign,Float,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-frac (raw "12.34"))))))
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
		(e-call
			(e-dec-small (numerator "1234") (denominator-power-of-ten "2") (value "12.34")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
