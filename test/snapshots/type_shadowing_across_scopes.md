# META
~~~ini
description=Type shadowing across scopes should produce warning
type=snippet
~~~
# SOURCE
~~~roc
Try(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
    "processed"

# In a nested mod scope, redeclare Try
InnerMod : {
    Try : [Success, Failure]
}
~~~
# EXPECTED
EXPECTED TYPE FIELD - type_shadowing_across_scopes.md:9:5:9:8
EXPECTED RECORD TYPE SEPARATOR - type_shadowing_across_scopes.md:9:21:9:28
UNEXPECTED STATEMENT - type_shadowing_across_scopes.md:9:28:9:29
UNEXPECTED STATEMENT - type_shadowing_across_scopes.md:10:1:10:2
BUILTIN TYPE SHADOWED - type_shadowing_across_scopes.md:1:1:1:28
UNUSED VARIABLE - type_shadowing_across_scopes.md:4:16:4:20
MALFORMED TYPE - type_shadowing_across_scopes.md:9:21:9:28
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Type Field")
		(region (start 9 5) (end 9 8))
		(headline
			(reflow "I was parsing a record type, and I expected a field name."))
		(document
			(reflow "Record type fields start with lowercase names, ")
			(annotated code "_")
			(reflow ", or named underscores, followed by ")
			(annotated code ":")
			(reflow " and the field type.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ name : Str, age : U64 }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "Try")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "type_shadowing_across_scopes.md") (start 9 5) (end 9 8) (annotation error) (line-text "    Try : [Success, Failure]"))))
	(report
		(severity runtime_error)
		(title "Expected Record Type Separator")
		(region (start 9 21) (end 9 28))
		(headline
			(reflow "I was parsing a record type, and I expected `,` or `}`."))
		(document
			(reflow "Separate record type fields with commas and close the record type with ")
			(annotated code "}")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ name : Str, age : U64 }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "Failure")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "type_shadowing_across_scopes.md") (start 9 21) (end 9 28) (annotation error) (line-text "    Try : [Success, Failure]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 9 28) (end 9 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "type_shadowing_across_scopes.md") (start 9 28) (end 9 29) (annotation error) (line-text "    Try : [Success, Failure]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 10 1) (end 10 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "type_shadowing_across_scopes.md") (start 10 1) (end 10 2) (annotation error) (line-text "}"))))
	(report
		(severity warning)
		(title "Builtin Type Shadowed")
		(region (start 1 1) (end 1 28))
		(headline
			(text "The type ")
			(annotated symbol-unqualified "Try")
			(text " shadows a builtin type."))
		(document
			(reflow "This may make the builtin type inaccessible in this scope.")
			(line-break)
			(source-region (file "type_shadowing_across_scopes.md") (start 1 1) (end 1 28) (annotation warning) (line-text "Try(a, b) : [Ok(a), Err(b)]"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 4 16) (end 4 20))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "data")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_data")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "type_shadowing_across_scopes.md") (start 4 16) (end 4 20) (annotation error) (line-text "processData = |data|"))))
	(report
		(severity runtime_error)
		(title "Malformed Type")
		(region (start 9 21) (end 9 28))
		(headline
			(reflow "This type annotation is malformed or contains invalid syntax."))
		(document
			(source-region (file "type_shadowing_across_scopes.md") (start 9 21) (end 9 28) (annotation error) (line-text "    Try : [Success, Failure]")))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
StringStart,StringPart,StringEnd,
UpperIdent,OpColon,OpenCurly,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Try")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a")))
					(ty-apply
						(ty (name "Err"))
						(ty-var (raw "b"))))))
		(s-type-anno (name "processData")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "processData"))
			(e-lambda
				(args
					(p-ident (raw "data")))
				(e-string
					(e-string-part (raw "processed")))))
		(s-type-decl
			(header (name "InnerMod")
				(args))
			(ty-malformed (tag "expected_ty_close_curly_or_comma")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
Try(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
	"processed"

# In a nested mod scope, redeclare Try
InnerMod :

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processData"))
		(e-lambda
			(args
				(p-assign (ident "data")))
			(e-string
				(e-literal (string "processed"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "Try")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "Err")
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "InnerMod"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Str")))
	(type_decls
		(alias (type "Try(a, b)")
			(ty-header (name "Try")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Error")
			(ty-header (name "InnerMod"))))
	(expressions
		(expr (type "Str -> Str"))))
~~~
