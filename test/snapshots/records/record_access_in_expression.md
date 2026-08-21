# META
~~~ini
description=Record field access used in expressions (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.age + 5
~~~
# EXPECTED
POLYMORPHIC VALUE - record_access_in_expression.md:1:1:1:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Polymorphic Value")
		(region (start 1 1) (end 1 15))
		(headline
			(reflow "This top-level value still has an unresolved polymorphic type."))
		(document
			(source-region (file "record_access_in_expression.md") (start 1 1) (end 1 15) (annotation error) (line-text "person.age + 5"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "a where [a.plus : a, Dec -> a]")
			(line-break)
			(reflow "Add an annotation or use this value in a way that fixes its concrete type."))))
~~~
# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "+")
	(e-field-access
		(receiver
			(e-ident (raw "person")))
		(segment (mode "required") (field "age")))
	(e-int (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "plus") (constraint-fn-var 215)
	(receiver
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "age") (mode "required")))))
	(args
		(e-num (value "5"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, Dec -> a]"))
~~~
