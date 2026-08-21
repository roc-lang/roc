# META
~~~ini
description=Test if usage affects error type conversion
type=snippet
~~~
# SOURCE
~~~roc
UnusedType := _

UsedType := _

value : UsedType
value = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
TYPE MISMATCH - usage_test.md:6:9:6:11
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
			(source-region (file "usage_test.md") (start 1 1) (end 1 1) (annotation error) (line-text "UnusedType := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "usage_test.md") (start 1 1) (end 1 1) (annotation error) (line-text "UnusedType := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 9) (end 6 11))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "usage_test.md") (start 6 9) (end 6 11) (annotation error) (line-text "value = 42"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "UsedType")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,Underscore,
UpperIdent,OpColonEqual,Underscore,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "UnusedType")
				(args))
			(_))
		(s-type-decl
			(header (name "UsedType")
				(args))
			(_))
		(s-type-anno (name "value")
			(ty (name "UsedType")))
		(s-decl
			(p-ident (raw "value"))
			(e-int (raw "42")))))
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
			(ty-lookup (name "UsedType") (local))))
	(s-nominal-decl
		(ty-header (name "UnusedType"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "UsedType"))
		(ty-underscore)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "UsedType")))
	(type_decls
		(nominal (type "UnusedType")
			(ty-header (name "UnusedType")))
		(nominal (type "UsedType")
			(ty-header (name "UsedType"))))
	(expressions
		(expr (type "UsedType"))))
~~~
