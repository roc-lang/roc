# META
~~~ini
description=Rejects same-name method requirements whose outer call arities cannot share one method scheme
type=file
~~~
# SOURCE
~~~roc
f = |value| (value.convert(), value.convert(1))
~~~
# EXPECTED
TYPE MISMATCH - conflicting_same_method_arities.md:1:31:1:36
# PROBLEMS
── ✗ type mismatch ───────────────────── conflicting_same_method_arities.md:1:31

This expression is used in an unexpected way.

f = |value| (value.convert(), value.convert(1))
                              ^^^^^

It has the type:

    a where [a.convert : a -> _ret]

But you are trying to use it as:

    _a
      where [
        _b.convert : c, d -> _ret,
        c.convert : c -> _ret2,
        d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),
      ]

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,Comma,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-tuple
					(e-method-call (method ".convert")
						(receiver
							(e-ident (raw "value")))
						(args))
					(e-method-call (method ".convert")
						(receiver
							(e-ident (raw "value")))
						(args
							(e-int (raw "1")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> (_field, _field2)")))
	(expressions
		(expr (type "Error -> (_field, _field2)"))))
~~~
