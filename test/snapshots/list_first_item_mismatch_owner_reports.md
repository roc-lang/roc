# META
~~~ini
description=A list literal whose first item does not fit its expected item type is reported by whatever gave the list its expected type: an annotation, a call argument, a record field, or a tuple slot
type=file
~~~
# SOURCE
~~~roc
File : { url : Str, size : U64 }

annotated : List(File)
annotated = [{ url: "u" }]

total : List(File) -> U64
total = |files| files.len()

argument : U64
argument = total([{ url: "u" }])

field : { files : List(File) }
field = { files: [{ url: "u" }] }

slot : (List(File), U64)
slot = ([{ url: "u" }], 1)
~~~
# EXPECTED
TYPE MISMATCH - list_first_item_mismatch_owner_reports.md:4:13:4:27
TYPE MISMATCH - list_first_item_mismatch_owner_reports.md:10:12:10:12
TYPE MISMATCH - list_first_item_mismatch_owner_reports.md:13:9:13:34
TYPE MISMATCH - list_first_item_mismatch_owner_reports.md:16:8:16:27
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 13) (end 4 27))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "list_first_item_mismatch_owner_reports.md") (start 4 13) (end 4 27) (annotation error) (line-text "annotated = [{ url: \"u\" }]"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List({ url: Str })")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(File)")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This record is missing the field:")
			(reflow " ")
			(annotated code "size")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 10 18) (end 10 32))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "list_first_item_mismatch_owner_reports.md") (start 10 12) (end 10 33) (annotation dim) (line-text "argument = total([{ url: \"u\" }])"))
				(underline (start 10 18) (end 10 32) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List({ url: Str })")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "total")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(File)")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This record is missing the field:")
			(reflow " ")
			(annotated code "size")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 13 9) (end 13 34))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "list_first_item_mismatch_owner_reports.md") (start 13 9) (end 13 34) (annotation error) (line-text "field = { files: [{ url: \"u\" }] }"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ files: List({ url: Str }) }")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ files: List(File) }")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This record is missing the field:")
			(reflow " ")
			(annotated code "size")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 16 8) (end 16 27))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "list_first_item_mismatch_owner_reports.md") (start 16 8) (end 16 27) (annotation error) (line-text "slot = ([{ url: \"u\" }], 1)"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(List({ url: Str }), U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(List(File), U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This record is missing the field:")
			(reflow " ")
			(annotated code "size"))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseSquare,CloseRound,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpenSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseSquare,CloseCurly,
LowerIdent,OpColon,OpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,OpenSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseSquare,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "File")
				(args))
			(ty-record
				(anno-record-field (name "url")
					(ty (name "Str")))
				(anno-record-field (name "size")
					(ty (name "U64")))))
		(s-type-anno (name "annotated")
			(ty-apply
				(ty (name "List"))
				(ty (name "File"))))
		(s-decl
			(p-ident (raw "annotated"))
			(e-list
				(e-record
					(field (field "url")
						(e-string
							(e-string-part (raw "u")))))))
		(s-type-anno (name "total")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "File")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "total"))
			(e-lambda
				(args
					(p-ident (raw "files")))
				(e-method-call (method ".len")
					(receiver
						(e-ident (raw "files")))
					(args))))
		(s-type-anno (name "argument")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "argument"))
			(e-apply
				(e-ident (raw "total"))
				(e-list
					(e-record
						(field (field "url")
							(e-string
								(e-string-part (raw "u"))))))))
		(s-type-anno (name "field")
			(ty-record
				(anno-record-field (name "files")
					(ty-apply
						(ty (name "List"))
						(ty (name "File"))))))
		(s-decl
			(p-ident (raw "field"))
			(e-record
				(field (field "files")
					(e-list
						(e-record
							(field (field "url")
								(e-string
									(e-string-part (raw "u")))))))))
		(s-type-anno (name "slot")
			(ty-tuple
				(ty-apply
					(ty (name "List"))
					(ty (name "File")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "slot"))
			(e-tuple
				(e-list
					(e-record
						(field (field "url")
							(e-string
								(e-string-part (raw "u"))))))
				(e-int (raw "1"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "annotated"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "File") (local)))))
	(d-let
		(p-assign (ident "total"))
		(e-lambda
			(args
				(p-assign (ident "files")))
			(e-dispatch-call (method "len") (constraint-fn-var 382)
				(receiver
					(e-lookup-local
						(p-assign (ident "files"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "File") (local)))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "argument"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "field"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-record
				(field (field "files")
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "File") (local)))))))
	(d-let
		(p-assign (ident "slot"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-tuple
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "File") (local)))
				(ty-lookup (name "U64") (builtin)))))
	(s-alias-decl
		(ty-header (name "File"))
		(ty-record
			(field (field "url")
				(ty-lookup (name "Str") (builtin)))
			(field (field "size")
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(File)"))
		(patt (type "List(File) -> U64"))
		(patt (type "U64"))
		(patt (type "{ files: List(File) }"))
		(patt (type "(List(File), U64)")))
	(type_decls
		(alias (type "File")
			(ty-header (name "File"))))
	(expressions
		(expr (type "List(File)"))
		(expr (type "List(File) -> U64"))
		(expr (type "U64"))
		(expr (type "{ files: List(File) }"))
		(expr (type "(List(File), U64)"))))
~~~
