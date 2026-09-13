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
MISSING METHOD - call_float_literal.md:1:5:1:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 5) (end 1 10))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "call_float_literal.md") (start 1 5) (end 1 10) (annotation error) (line-text "x = 12.34()"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "({}) -> _ret")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Float,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
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
		(e-call (constraint-fn-var 213)
			(e-runtime-error (tag "erroneous_value_expr")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
