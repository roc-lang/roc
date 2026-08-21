# META
~~~ini
description=Dollar-prefixed record field names are rejected
type=expr
~~~
# SOURCE
~~~roc
{ $field : "value" }
~~~
# EXPECTED
INVALID RECORD FIELD NAME - dollar_prefix_field_name.md:1:3:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Record Field Name")
		(region (start 1 3) (end 1 9))
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
			(source-region (file "dollar_prefix_field_name.md") (start 1 3) (end 1 9) (annotation error) (line-text "{ $field : \"value\" }")))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "$field")
		(e-string
			(e-string-part (raw "value")))))
~~~
# FORMATTED
~~~roc
{ $field: "value" }
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "$field")
			(e-string
				(e-literal (string "value"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ $field: Str }"))
~~~
