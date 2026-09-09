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
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 31) (end 1 36))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "conflicting_same_method_arities.md") (start 1 31) (end 1 36) (annotation error) (line-text "f = |value| (value.convert(), value.convert(1))"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a where [a.convert : a -> _ret]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "_a")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    _b.convert : c, d -> _ret,")
			(line-break)
			(indent 1)
			(text "    c.convert : c -> _ret2,")
			(line-break)
			(indent 1)
			(text "    d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end))))
~~~
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
