# META
~~~ini
description=Error types should propagate through aliases when underscores are used
type=snippet
~~~
# SOURCE
~~~roc
BadBase := _

BadDerived := BadBase

value : BadDerived
value = "test"

GoodBase := Str

GoodDerived := GoodBase

goodValue : GoodDerived
goodValue = "test"
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - underscore_error_propagation.md:1:1:1:1
TYPE MISMATCH - underscore_error_propagation.md:6:9:6:15
TYPE MISMATCH - underscore_error_propagation.md:13:13:13:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "underscore_error_propagation.md") (start 1 1) (end 1 1) (annotation error) (line-text "BadBase := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 9) (end 6 15))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "underscore_error_propagation.md") (start 6 9) (end 6 15) (annotation error) (line-text "value = \"test\""))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "BadDerived")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 13 13) (end 13 19))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "underscore_error_propagation.md") (start 13 13) (end 13 19) (annotation error) (line-text "goodValue = \"test\""))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "GoodDerived")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,Underscore,
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
UpperIdent,OpColonEqual,UpperIdent,
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "BadBase")
				(args))
			(_))
		(s-type-decl
			(header (name "BadDerived")
				(args))
			(ty (name "BadBase")))
		(s-type-anno (name "value")
			(ty (name "BadDerived")))
		(s-decl
			(p-ident (raw "value"))
			(e-string
				(e-string-part (raw "test"))))
		(s-type-decl
			(header (name "GoodBase")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "GoodDerived")
				(args))
			(ty (name "GoodBase")))
		(s-type-anno (name "goodValue")
			(ty (name "GoodDerived")))
		(s-decl
			(p-ident (raw "goodValue"))
			(e-string
				(e-string-part (raw "test"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "value"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "BadDerived") (local))))
	(d-let
		(p-assign (ident "goodValue"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "GoodDerived") (local))))
	(s-nominal-decl
		(ty-header (name "BadBase"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "BadDerived"))
		(ty-lookup (name "BadBase") (local)))
	(s-nominal-decl
		(ty-header (name "GoodBase"))
		(ty-lookup (name "Str") (builtin)))
	(s-nominal-decl
		(ty-header (name "GoodDerived"))
		(ty-lookup (name "GoodBase") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "BadDerived"))
		(patt (type "GoodDerived")))
	(type_decls
		(nominal (type "BadBase")
			(ty-header (name "BadBase")))
		(nominal (type "BadDerived")
			(ty-header (name "BadDerived")))
		(nominal (type "GoodBase")
			(ty-header (name "GoodBase")))
		(nominal (type "GoodDerived")
			(ty-header (name "GoodDerived"))))
	(expressions
		(expr (type "BadDerived"))
		(expr (type "GoodDerived"))))
~~~
