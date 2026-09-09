# META
~~~ini
description=Simple test for single underscore type becoming error type
type=snippet
~~~
# SOURCE
~~~roc
BadType := _

foo : BadType
foo = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - simple_underscore_error.md:1:1:1:1
TYPE MISMATCH - simple_underscore_error.md:4:7:4:9
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
			(source-region (file "simple_underscore_error.md") (start 1 1) (end 1 1) (annotation error) (line-text "BadType := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 7) (end 4 9))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "simple_underscore_error.md") (start 4 7) (end 4 9) (annotation error) (line-text "foo = 42"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "BadType")
			(annotation-end))))
~~~
# TOKENS
~~~zig
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
			(header (name "BadType")
				(args))
			(_))
		(s-type-anno (name "foo")
			(ty (name "BadType")))
		(s-decl
			(p-ident (raw "foo"))
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
		(p-assign (ident "foo"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "BadType") (local))))
	(s-nominal-decl
		(ty-header (name "BadType"))
		(ty-underscore)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "BadType")))
	(type_decls
		(nominal (type "BadType")
			(ty-header (name "BadType"))))
	(expressions
		(expr (type "BadType"))))
~~~
