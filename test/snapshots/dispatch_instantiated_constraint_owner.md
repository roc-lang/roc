# META
~~~ini
description=An instantiated method constraint is rejected at its instantiating use, not inside the generalized helper
type=snippet
~~~
# SOURCE
~~~roc
g = |r| r.is_ok()
f = |a| g(a).x
expect f([1].first())
~~~
# EXPECTED
TYPE MISMATCH - dispatch_instantiated_constraint_owner.md:3:10:3:21
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 10) (end 3 21))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "is_ok")
			(reflow " ")
			(reflow "method on")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "has an incompatible type."))
		(document
			(source-region (file "dispatch_instantiated_constraint_owner.md") (start 3 10) (end 3 21) (annotation error) (line-text "expect f([1].first())"))
			(line-break)
			(reflow "The method")
			(reflow " ")
			(annotated code "is_ok")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(_ok, [ListWasEmpty, ..]) -> Bool")
			(line-break)
			(indent 1)
			(text "  where [_ok.from_numeral : Numeral -> Try(_ok, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But I need it to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(_ok, [ListWasEmpty, ..]) -> { x: _field, .. }")
			(line-break)
			(indent 1)
			(text "  where [_ok.from_numeral : Numeral -> Try(_ok, [InvalidNumeral(Str)])]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,CloseSquare,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-ident (raw "r")))
				(e-method-call (method ".is_ok")
					(receiver
						(e-ident (raw "r")))
					(args))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-field-access
					(receiver
						(e-apply
							(e-ident (raw "g"))
							(e-ident (raw "a"))))
					(segment (mode "required") (field "x")))))
		(s-expect
			(e-apply
				(e-ident (raw "f"))
				(e-method-call (method ".first")
					(receiver
						(e-list
							(e-int (raw "1"))))
					(args))))))
~~~
# FORMATTED
~~~roc
g = |r| r.is_ok()

f = |a| g(a).x
expect f([1].first())
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-assign (ident "r")))
			(e-dispatch-call (method "is_ok") (constraint-fn-var 222)
				(receiver
					(e-lookup-local
						(p-assign (ident "r"))))
				(args))))
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-field-access
				(receiver
					(e-call (constraint-fn-var 228)
						(e-lookup-local
							(p-assign (ident "g")))
						(e-lookup-local
							(p-assign (ident "a")))))
				(segments
					(segment (name "x") (mode "required"))))))
	(s-expect
		(e-call (constraint-fn-var 260)
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-dispatch-call (method "first") (constraint-fn-var 246)
				(receiver
					(e-list
						(elems
							(e-num (value "1")))))
				(args)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "b -> c where [b.is_ok : b -> c]"))
		(patt (type "b -> c where [b.is_ok : b -> { x: c, .. }]")))
	(expressions
		(expr (type "b -> c where [b.is_ok : b -> c]"))
		(expr (type "b -> c where [b.is_ok : b -> { x: c, .. }]"))))
~~~
