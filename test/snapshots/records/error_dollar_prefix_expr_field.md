# META
~~~ini
description=Dollar-prefixed expression record field names are rejected
type=expr
~~~
# SOURCE
~~~roc
{ $name: "Ada" }
~~~
# EXPECTED
INVALID RECORD FIELD NAME - error_dollar_prefix_expr_field.md:1:3:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Record Field Name")
		(region (start 1 3) (end 1 8))
		(headline
			(reflow "Record field names cannot start with a dollar sign."))
		(document
			(reflow "Names that start with ")
			(annotated code "$")
			(reflow " are reassignable variables declared with the ")
			(annotated code "var")
			(reflow " keyword, so they cannot be used as record field names.")
			(line-break)
			(line-break)
			(source-region (file "error_dollar_prefix_expr_field.md") (start 1 3) (end 1 8) (annotation error) (line-text "{ $name: \"Ada\" }")))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "$name")
		(e-string
			(e-string-part (raw "Ada")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "$name")
			(e-string
				(e-literal (string "Ada"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ $name: Str }"))
~~~
