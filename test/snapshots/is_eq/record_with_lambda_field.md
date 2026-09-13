# META
~~~ini
description=Record with lambda field doesn't support equality - shows which field is ineligible
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: "30", process: |x| x } == { name: "Bob", age: "25", process: |y| y }
~~~
# EXPECTED
TYPE DOES NOT SUPPORT EQUALITY - record_with_lambda_field.md:1:1:1:91
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Does Not Support Equality")
		(region (start 1 1) (end 1 91))
		(headline
			(reflow "This expression is doing an equality check on a type that doesn't support equality."))
		(document
			(source-region (file "record_with_lambda_field.md") (start 1 1) (end 1 91) (annotation error) (line-text "{ name: \"Alice\", age: \"30\", process: |x| x } == { name: \"Bob\", age: \"25\", process: |y| y }"))
			(line-break)
			(reflow "The type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ age: a, name: b, process: c -> c }")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]),")
			(line-break)
			(indent 1)
			(text "    b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)]),")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "This record does not support equality because these fields have types that don't support ")
			(annotated emphasis "is_eq")
			(reflow ":")
			(line-break)
			(line-break)
			(text "    ")
			(annotated emphasis "process")
			(text ": ")
			(annotated type "a -> a")
			(line-break)
			(text "        ")
			(reflow "Function equality is not supported.")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Anonymous records only have an ")
			(annotated emphasis "is_eq")
			(reflow " method if all of their fields have ")
			(annotated emphasis "is_eq")
			(reflow " methods.")
			(line-break))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,CloseCurly,OpEquals,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "==")
	(e-record
		(field (field "name")
			(e-string
				(e-string-part (raw "Alice"))))
		(field (field "age")
			(e-string
				(e-string-part (raw "30"))))
		(field (field "process")
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x")))))
	(e-record
		(field (field "name")
			(e-string
				(e-string-part (raw "Bob"))))
		(field (field "age")
			(e-string
				(e-string-part (raw "25"))))
		(field (field "process")
			(e-lambda
				(args
					(p-ident (raw "y")))
				(e-ident (raw "y"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "erroneous_value_expr"))
~~~
# TYPES
~~~clojure
(expr (type "Bool"))
~~~
