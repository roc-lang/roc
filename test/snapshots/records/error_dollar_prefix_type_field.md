# META
~~~ini
description=Dollar-prefixed type record field names are rejected
type=statement
~~~
# SOURCE
~~~roc
Person : { $name : Str }
~~~
# EXPECTED
INVALID RECORD FIELD NAME - error_dollar_prefix_type_field.md:1:12:1:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Record Field Name")
		(region (start 1 12) (end 1 17))
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
			(source-region (file "error_dollar_prefix_type_field.md") (start 1 12) (end 1 17) (annotation error) (line-text "Person : { $name : Str }")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-type-decl
	(header (name "Person")
		(args))
	(ty-record
		(anno-record-field (name "$name")
			(ty (name "Str")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Person"))
		(ty-record
			(field (field "$name")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Person")
			(ty-header (name "Person"))))
	(expressions))
~~~
