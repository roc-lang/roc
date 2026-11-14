# META
~~~ini
description=A record update whose list field has elements missing a field reports that field of the update, naming the missing element field
type=file
~~~
# SOURCE
~~~roc
Model := { files : List({ url : Str, size : U64 }) }

empty : Model
empty = { files: [] }

model : Model
model = { ..empty, files: [{ url: "u" }] }

plain : { files : List({ url : Str, size : U64 }) } -> { files : List({ url : Str, size : U64 }) }
plain = |base| { ..base, files: [{ url: "u" }] }
~~~
# EXPECTED
TYPE MISMATCH - repro_issue_12968_record_update_list_field.md:7:27:7:41
TYPE MISMATCH - repro_issue_12968_record_update_list_field.md:10:33:10:47
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 7 27) (end 7 41))
		(headline
			(reflow "The type of the field")
			(reflow " ")
			(annotated code "files")
			(reflow " ")
			(reflow "is incompatible."))
		(document
			(source-region (file "repro_issue_12968_record_update_list_field.md") (start 7 27) (end 7 41) (annotation error) (line-text "model = { ..empty, files: [{ url: \"u\" }] }"))
			(line-break)
			(reflow "You are trying to update the")
			(reflow " ")
			(annotated code "files")
			(reflow " ")
			(reflow "field to be the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List({ url: Str })")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the")
			(reflow " ")
			(annotated code "empty")
			(reflow " ")
			(reflow "record needs it to be")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List({ size: U64, url: Str })")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated underline "Note:")
			(reflow " ")
			(reflow "You cannot change the type of a record field with the record update syntax.")
			(reflow " ")
			(reflow "You can do that by create a new record, copying over the unchanged fields, then transforming")
			(reflow " ")
			(annotated code "files")
			(reflow " ")
			(reflow "to be the new type.")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This record is missing the field:")
			(reflow " ")
			(annotated code "size")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 10 33) (end 10 47))
		(headline
			(reflow "The type of the field")
			(reflow " ")
			(annotated code "files")
			(reflow " ")
			(reflow "is incompatible."))
		(document
			(source-region (file "repro_issue_12968_record_update_list_field.md") (start 10 33) (end 10 47) (annotation error) (line-text "plain = |base| { ..base, files: [{ url: \"u\" }] }"))
			(line-break)
			(reflow "You are trying to update the")
			(reflow " ")
			(annotated code "files")
			(reflow " ")
			(reflow "field to be the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List({ url: Str })")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the")
			(reflow " ")
			(annotated code "base")
			(reflow " ")
			(reflow "record needs it to be")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List({ size: U64, url: Str })")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated underline "Note:")
			(reflow " ")
			(reflow "You cannot change the type of a record field with the record update syntax.")
			(reflow " ")
			(reflow "You can do that by create a new record, copying over the unchanged fields, then transforming")
			(reflow " ")
			(annotated code "files")
			(reflow " ")
			(reflow "to be the new type.")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This record is missing the field:")
			(reflow " ")
			(annotated code "size"))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,CloseRound,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseSquare,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,CloseRound,CloseCurly,OpArrow,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseSquare,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Model")
				(args))
			(ty-record
				(anno-record-field (name "files")
					(ty-apply
						(ty (name "List"))
						(ty-record
							(anno-record-field (name "url")
								(ty (name "Str")))
							(anno-record-field (name "size")
								(ty (name "U64"))))))))
		(s-type-anno (name "empty")
			(ty (name "Model")))
		(s-decl
			(p-ident (raw "empty"))
			(e-record
				(field (field "files")
					(e-list))))
		(s-type-anno (name "model")
			(ty (name "Model")))
		(s-decl
			(p-ident (raw "model"))
			(e-record
				(ext
					(e-ident (raw "empty")))
				(field (field "files")
					(e-list
						(e-record
							(field (field "url")
								(e-string
									(e-string-part (raw "u")))))))))
		(s-type-anno (name "plain")
			(ty-fn
				(ty-record
					(anno-record-field (name "files")
						(ty-apply
							(ty (name "List"))
							(ty-record
								(anno-record-field (name "url")
									(ty (name "Str")))
								(anno-record-field (name "size")
									(ty (name "U64")))))))
				(ty-record
					(anno-record-field (name "files")
						(ty-apply
							(ty (name "List"))
							(ty-record
								(anno-record-field (name "url")
									(ty (name "Str")))
								(anno-record-field (name "size")
									(ty (name "U64")))))))))
		(s-decl
			(p-ident (raw "plain"))
			(e-lambda
				(args
					(p-ident (raw "base")))
				(e-record
					(ext
						(e-ident (raw "base")))
					(field (field "files")
						(e-list
							(e-record
								(field (field "url")
									(e-string
										(e-string-part (raw "u"))))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "empty"))
		(e-record
			(fields
				(field (name "files")
					(e-empty_list))))
		(annotation
			(ty-lookup (name "Model") (local))))
	(d-let
		(p-assign (ident "model"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "Model") (local))))
	(d-let
		(p-assign (ident "plain"))
		(e-lambda
			(args
				(p-assign (ident "base")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "files")
						(ty-apply (name "List") (builtin)
							(ty-record
								(field (field "url")
									(ty-lookup (name "Str") (builtin)))
								(field (field "size")
									(ty-lookup (name "U64") (builtin)))))))
				(ty-record
					(field (field "files")
						(ty-apply (name "List") (builtin)
							(ty-record
								(field (field "url")
									(ty-lookup (name "Str") (builtin)))
								(field (field "size")
									(ty-lookup (name "U64") (builtin))))))))))
	(s-nominal-decl
		(ty-header (name "Model"))
		(ty-record
			(field (field "files")
				(ty-apply (name "List") (builtin)
					(ty-record
						(field (field "url")
							(ty-lookup (name "Str") (builtin)))
						(field (field "size")
							(ty-lookup (name "U64") (builtin)))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Model"))
		(patt (type "Model"))
		(patt (type "{ files: List({ size: U64, url: Str }) } -> { files: List({ size: U64, url: Str }) }")))
	(type_decls
		(nominal (type "Model")
			(ty-header (name "Model"))))
	(expressions
		(expr (type "Model"))
		(expr (type "Model"))
		(expr (type "{ files: List({ size: U64, url: Str }) } -> { files: List({ size: U64, url: Str }) }"))))
~~~
