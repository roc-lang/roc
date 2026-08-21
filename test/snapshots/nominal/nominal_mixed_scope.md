# META
~~~ini
description=Example of mixed local and external nominal types in same scope
type=snippet
~~~
# SOURCE
~~~roc
LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
    }
}
~~~
# EXPECTED
IMPORT MUST BE TOP LEVEL - nominal_mixed_scope.md:7:5:7:11
UNDECLARED TYPE - nominal_mixed_scope.md:7:12:7:17
UNDECLARED TYPE - nominal_mixed_scope.md:10:9:10:12
UNDECLARED TYPE - nominal_mixed_scope.md:11:9:11:12
UNDECLARED TYPE - nominal_mixed_scope.md:12:9:12:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Import Must Be Top Level")
		(region (start 7 5) (end 7 11))
		(headline
			(reflow "I was parsing an import, but imports are only allowed at the top level."))
		(document
			(reflow "Move this import after the mod header and before declarations or executable statements.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "import Json")
			(line-break)
			(line-break)
			(indent 1)
			(text "main = 1")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "import")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "nominal_mixed_scope.md") (start 7 5) (end 7 11) (annotation error) (line-text "    import Color.RGB"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 7 12) (end 7 17))
		(headline
			(reflow "The type ")
			(annotated code "Color")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "nominal_mixed_scope.md") (start 7 12) (end 7 17) (annotation error) (line-text "    import Color.RGB"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 10 9) (end 10 12))
		(headline
			(reflow "The type ")
			(annotated code "RGB")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "nominal_mixed_scope.md") (start 10 9) (end 10 12) (annotation error) (line-text "        RGB.Red => LocalStatus.Pending"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 11 9) (end 11 12))
		(headline
			(reflow "The type ")
			(annotated code "RGB")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "nominal_mixed_scope.md") (start 11 9) (end 11 12) (annotation error) (line-text "        RGB.Green => LocalStatus.Complete"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 12 9) (end 12 12))
		(headline
			(reflow "The type ")
			(annotated code "RGB")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "nominal_mixed_scope.md") (start 12 9) (end 12 12) (annotation error) (line-text "        RGB.Blue => LocalStatus.Pending")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,Underscore,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwImport,UpperIdent,NoSpaceDotUpperIdent,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "LocalStatus")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Pending"))
					(ty (name "Complete")))))
		(s-type-anno (name "processColor")
			(ty-fn
				(_)
				(ty (name "LocalStatus"))))
		(s-decl
			(p-ident (raw "processColor"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-block
					(statements
						(s-malformed (tag "import_must_be_top_level"))
						(e-tag (raw "Color.RGB"))
						(e-match
							(e-ident (raw "color"))
							(branches
								(branch
									(p-tag (raw ".Red"))
									(e-tag (raw "LocalStatus.Pending")))
								(branch
									(p-tag (raw ".Green"))
									(e-tag (raw "LocalStatus.Complete")))
								(branch
									(p-tag (raw ".Blue"))
									(e-tag (raw "LocalStatus.Pending")))))))))))
~~~
# FORMATTED
~~~roc
LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

	# bring RGB into scope
		Color.RGB

	match color {
		RGB.Red => LocalStatus.Pending
		RGB.Green => LocalStatus.Complete
		RGB.Blue => LocalStatus.Pending
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processColor"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-underscore)
				(ty-lookup (name "LocalStatus") (local)))))
	(s-nominal-decl
		(ty-header (name "LocalStatus"))
		(ty-tag-union
			(ty-tag-name (name "Pending"))
			(ty-tag-name (name "Complete")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> LocalStatus")))
	(type_decls
		(nominal (type "LocalStatus")
			(ty-header (name "LocalStatus"))))
	(expressions
		(expr (type "_arg -> LocalStatus"))))
~~~
