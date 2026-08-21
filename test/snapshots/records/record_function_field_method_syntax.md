# META
~~~ini
description=Method call syntax is not reinterpreted as calling a record field
type=snippet
~~~
# SOURCE
~~~roc
r = { f: |x| x }

result = r.f(1)
~~~
# EXPECTED
MISSING METHOD - record_function_field_method_syntax.md:3:12:3:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 3 12) (end 3 13))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "f")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "record_function_field_method_syntax.md") (start 3 12) (end 3 13) (annotation error) (line-text "result = r.f(1)"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "f")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ f: a -> a }")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-record
				(field (field "f")
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-ident (raw "x"))))))
		(s-decl
			(p-ident (raw "result"))
			(e-method-call (method ".f")
				(receiver
					(e-ident (raw "r")))
				(args
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-record
			(fields
				(field (name "f")
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "x"))))))))
	(d-let
		(p-assign (ident "result"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ f: a -> a }"))
		(patt (type "_a")))
	(expressions
		(expr (type "{ f: a -> a }"))
		(expr (type "_a"))))
~~~
