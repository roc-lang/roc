# META
~~~ini
description=Unnamed field in a structural record type is rejected
type=snippet
~~~
# SOURCE
~~~roc
Bad : { x : U8, _ : U8 }
~~~
# EXPECTED
UNNAMED FIELD NOT ALLOWED IN STRUCTURAL RECORD - unnamed_field_in_structural_record_error.md:1:17:1:23
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unnamed Field Not Allowed In Structural Record")
		(region (start 1 17) (end 1 23))
		(headline
			(reflow "Unnamed fields (written ")
			(annotated code "_")
			(reflow " or ")
			(annotated code "_name")
			(reflow ") are only allowed in nominal record type declarations, not in structural record types."))
		(document
			(source-region (file "unnamed_field_in_structural_record_error.md") (start 1 17) (end 1 23) (annotation error) (line-text "Bad : { x : U8, _ : U8 }"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Unnamed fields reserve layout padding for a nominal type (declared with ")
			(annotated code ":=")
			(reflow "). Give the field a name, or move it into a nominal type declaration."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,Underscore,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Bad")
				(args))
			(ty-record
				(anno-record-field (name "x")
					(ty (name "U8")))
				(anno-record-field (name "_")
					(ty (name "U8")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Bad"))
		(ty-record
			(field (field "x")
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Bad")
			(ty-header (name "Bad"))))
	(expressions))
~~~
