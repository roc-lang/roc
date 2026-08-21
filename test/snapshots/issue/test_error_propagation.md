# META
~~~ini
description=Test error propagation - aliases that reference error types should not propagate errors
type=snippet
~~~
# SOURCE
~~~roc
BadBase := _

GoodAlias := BadBase

value : GoodAlias
value = "test"
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - test_error_propagation.md:1:1:1:1
TYPE MISMATCH - test_error_propagation.md:6:9:6:15
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
			(source-region (file "test_error_propagation.md") (start 1 1) (end 1 1) (annotation error) (line-text "BadBase := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 9) (end 6 15))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "test_error_propagation.md") (start 6 9) (end 6 15) (annotation error) (line-text "value = \"test\""))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "GoodAlias")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,Underscore,
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
			(header (name "GoodAlias")
				(args))
			(ty (name "BadBase")))
		(s-type-anno (name "value")
			(ty (name "GoodAlias")))
		(s-decl
			(p-ident (raw "value"))
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
			(ty-lookup (name "GoodAlias") (local))))
	(s-nominal-decl
		(ty-header (name "BadBase"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "GoodAlias"))
		(ty-lookup (name "BadBase") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "GoodAlias")))
	(type_decls
		(nominal (type "BadBase")
			(ty-header (name "BadBase")))
		(nominal (type "GoodAlias")
			(ty-header (name "GoodAlias"))))
	(expressions
		(expr (type "GoodAlias"))))
~~~
